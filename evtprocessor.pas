unit EvtProcessor;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, db, EventsProcessorVars, httpsend, fpjson, jsonparser;

 const ID_Bursary = 4;
 type TEvent = record
                ID: integer;
                Date: TDateTime;
                Priority: integer;
                Resource: shortstring;
                EventType: shortstring;
                ID_Resource: integer;
                ID_LinkedResource: integer;
               end;
      TEvents = array[0..99] of TEvent;

      TEventCallBack = procedure(Sender: TObject; ID_Event: integer) of object;
      TLogCallBack = procedure(Sender: TObject; message: string) of object;

 const TResourceMap:       array[0..7, 0..1] of shortstring = (('Alerts', 'SELECT A.* FROM "Alerts" A LEFT JOIN "Markets" M ON (A."ID_Market" = M."ID") WHERE (A."ID" = %d) AND (M."ID_Bursary" = %d)'),
                                                               ('Messages', 'SELECT M.* FROM "Messages" M WHERE (M."ID" = %d) AND (M."ID_Bursary" = %d)'),
                                                               ('Assets', 'SELECT A.* FROM "Assets" A LEFT JOIN "Markets" M ON (A."ID_Market" = M."ID") WHERE (A."ID" = %d) AND (M."ID_Bursary" = %d)'),
                                                               ('AssetSessions', 'SELECT A.* FROM "AssetSessions" A LEFT JOIN "Rings" R ON (A."ID_Ring" = R."ID") LEFT JOIN "Markets" M ON (R."ID_Market" = M."ID") WHERE (A."ID" = %d) AND (M."ID_Bursary" = %d)'),
                                                               ('Orders', 'SELECT O.* FROM "Orders" O LEFT JOIN "Rings" R ON (O."ID_Ring" = R."ID") LEFT JOIN "Markets" M ON (R."ID_Market" = M."ID") WHERE (O."ID" = %d) AND (M."ID_Bursary" = %d)'),
                                                               ('DeltaT', 'SELECT A.* FROM "Assets" A LEFT JOIN "Markets" M ON (A."ID_Market" = M."ID") WHERE (A."ID" = %d) AND (M."ID_Bursary" = %d)'),
                                                               ('DeltaT1', 'SELECT A.* FROM "Assets" A LEFT JOIN "Markets" M ON (A."ID_Market" = M."ID") WHERE (A."ID" = %d) AND (M."ID_Bursary" = %d)'),
                                                               ('Journal', 'SELECT J.* FROM "Journal" J WHERE (J."ID" = %d) AND (J."ID_Bursary" = %d)'));
       TLinkedResourceMap: array[0..2, 0..1] of shortstring = (('AssetSessions', 'Assets'),
                                                               ('DeltaT', 'Orders'),
                                                               ('DeltaT1', 'Orders'));

 type TEventProcessor = class
                        protected
                         FEvents: TEvents;
                         FEventsLength: integer;
                         FLastID: integer;

                         FOnAppendEvent: TEventCallBack;
                         FOnUpdateEvent: TEventCallBack;
                         FOnRemoveEvent: TEventCallBack;
                         FOnLogMessage: TLogCallBack;

                         HttpCookies: shortstring;

                         function GetEvent(Index: integer): TEvent;

                         function IndexOfEvent(ID: integer): integer;
                         function AddEvent(ID: integer; Date: TDateTime; Priority: integer; Resource: shortstring; EventType: shortstring; ID_Resource: integer; ID_LinkedResource: integer): integer;
                         function ConvertJSON(DataType: TFieldType; Value: Variant): shortstring;
                         function ExtractEventObject(ds: TDataSet): string;
                         function ExtractEventMessage(idx: integer = 0): string;
                         function RPCPost(const URL: shortstring; const Data: TStream): Boolean;
                         function SendEventToRPC(message: string; var response: string; var lag: integer): boolean;
                         function CheckRPCResponse(response: string): boolean;
                         function ProcessEvent(idx: integer = 0): boolean;
                         procedure RemoveEvent(idx: integer = 0; CheckEvent: boolean = true);
                        public
                         constructor Create;
                         procedure CheckEvents;

                         property EventsCount: integer read FEventsLength;
                         property Events[Index: integer]: TEvent read GetEvent;
                         property OnAppendEvent: TEventCallBack read FOnAppendEvent write FOnAppendEvent;
                         property OnUpdateEvent: TEventCallBack read FOnUpdateEvent write FOnUpdateEvent;
                         property OnRemoveEvent: TEventCallBack read FOnRemoveEvent write FOnRemoveEvent;
                         property OnLogMessage: TLogCallBack read FOnLogMessage write FOnLogMessage;
                        end;

implementation

 constructor TEventProcessor.Create;
  begin
   FEventsLength:= 0;
   FLastID:= 0;
   FOnAppendEvent:= nil;
   FOnUpdateEvent:= nil;
   FOnRemoveEvent:= nil;
  end;

 function TEventProcessor.GetEvent(Index: integer): TEvent;
  begin
   FillChar(result, sizeof(result), 0);

   if (Index >= 0) and (Index < FEventsLength) then result:= FEvents[Index];
  end;

 function TEventProcessor.IndexOfEvent(ID: integer): integer;
  var b: boolean;
      i: integer;
  begin
   b:= false;
   i:= -1;
   while not b and (i < FEventsLength - 1) do
    begin
     i += 1;
     if FEvents[i].ID = ID then b:= true;
    end;

   if b then result:= i else result:= -1;
  end;

 function TEventProcessor.AddEvent(ID: integer; Date: TDateTime; Priority: integer; Resource: shortstring; EventType: shortstring; ID_Resource: integer; ID_LinkedResource: integer): integer;
  begin
   result:= -1;
   if FEventsLength = 100 then Exit;

   if IndexOfEvent(ID) > -1 then Exit;

   result:= FEventsLength;
   FEventsLength += 1;

   FEvents[result].ID:= ID;
   FEvents[result].Date:= Date;
   FEvents[result].Priority:= Priority;
   FEvents[result].Resource:= Resource;
   FEvents[result].EventType:= EventType;
   FEvents[result].ID_Resource:= ID_Resource;
   FEvents[result].ID_LinkedResource:= ID_LinkedResource;

   if Assigned(FOnAppendEvent) then FOnAppendEvent(self, ID);
  end;

 function TEventProcessor.ConvertJSON(DataType: TFieldType; Value: Variant): shortstring;
  var tfs: TFormatSettings;
  begin
   tfs.DecimalSeparator:= '.';
   tfs.DateSeparator:= '-';
   tfs.TimeSeparator:= ':';
   tfs.LongDateFormat:= 'yyyy-mm-dd hh:nn:ss';
   tfs.ShortDateFormat:= 'yyyy-mm-dd hh:nn:ss';

   if Value = Null then result:= 'null'
   else case DataType of
    ftAutoInc:  result:= IntToStr(integer(Value));
    ftBoolean:  if boolean(Value) then result:= 'true' else result:= 'false';
    ftInteger,
    ftWord:     result:= IntToStr(integer(Value));
    ftFloat:    result:= FloatToStr(double(Value), tfs);
    ftDateTime: result:= '"' + FormatDateTime('yyyy-mm-dd''T''hh:nn:ss', TDateTime(value), []) {DateTimeToStr(TDateTime(Value), tfs)} + '"';
    ftString:   result:= '"' + string(Value) + '"';
   end;
  end;

 function TEventProcessor.ExtractEventObject(ds: TDataSet): string;
  var i: integer;
      FieldName: shortstring;
      DataType: TFieldType;
  begin
   result:= '';
   if ds = nil then Exit;

   for i:= 0 to ds.FieldDefs.Count - 1 do
    begin
     FieldName:= ds.FieldDefs[i].Name;
     DataType:= ds.FieldDefs[i].DataType;

     if result <> '' then result += ', ' + #13#10;
     result += '"' + FieldName + '": ' + ConvertJSON(DataType, ds.FieldByName(FieldName).Value);
    end;
  end;

 function TEventProcessor.ExtractEventMessage(idx: integer = 0): string;
  var ds: TDataSet;
      i: integer;
      str_object, str_linkedobject: string;
      b, isSQL: boolean;
      str_Resource, str_LinkedResource: shortstring;
  begin
   result:= '';
   str_object:= '';
   str_linkedobject:= '';
   if (idx < 0) or (idx >= FEventsLength) then Exit;

   str_Resource:= FEvents[idx].Resource;
   b:= false;
   i:= -1;
   while not b and (i < High(TResourceMap)) do
    begin
     i += 1;
     if TResourceMap[i][0] = FEvents[idx].Resource then begin str_Resource:= TResourceMap[i][1]; b:= true; end;
    end;

   isSQL:= pos(' ', trim(str_Resource)) > 0;

   str_LinkedResource:= '';
   b:= false;
   i:= -1;
   while not b and (i < High(TResourceMap)) do
    begin
     i += 1;
     if TLinkedResourceMap[i][0] = FEvents[idx].Resource then begin str_LinkedResource:= TLinkedResourceMap[i][1]; b:= true; end;
    end;

   //  obtain first the direct resourced object
   ds:= nil;
   try
    if isSQL then ds:= ds_brm['brm'].Query(Format(str_Resource, [FEvents[idx].ID_Resource, ID_Bursary]))
    else ds:= ds_brm['brm'].Query(Format('SELECT * FROM "%s" WHERE "ID" = %d', [str_Resource, FEvents[idx].ID_Resource]));

    if ds = nil then Exit;
    if ds.IsEmpty then begin if Assigned(FOnLogMessage) then FOnLogMessage(self, Format('Object missing or on different Bursary on Event ID %d', [FEvents[idx].ID])); Exit; end;

    str_object:= ExtractEventObject(ds);
   finally
    FreeAndNil(ds);
   end;

   //  obtain the linked resourced object
   if str_linkedobject <> '' then
    begin
     ds:= nil;
     try
      ds:= ds_brm['brm'].Query(Format('SELECT * FROM "%s" WHERE "ID" = %d', [str_LinkedResource, FEvents[idx].ID_LinkedResource]));
      if ds = nil then Exit;
      if ds.IsEmpty then begin if Assigned(FOnLogMessage) then FOnLogMessage(self, Format('Object missing on Event ID %d', [FEvents[idx].ID])); Exit; end;

      str_linkedobject:= ExtractEventObject(ds);
     finally
      FreeAndNil(ds);
     end;
    end;

   result:= '"Event": ' + #13#10 +
            '{ ' + #13#10 +
            '  "ID": '                + ConvertJSON(ftInteger, FEvents[idx].ID) + ', ' + #13#10 +
            '  "Date": '              + ConvertJSON(ftDateTime, FEvents[idx].Date) + ', ' + #13#10 +
            '  "Priority": '          + ConvertJSON(ftInteger, FEvents[idx].Priority) + ', ' + #13#10 +
            '  "Resource": '          + ConvertJSON(ftString, FEvents[idx].Resource) + ', ' + #13#10 +
            '  "EventType": '         + ConvertJSON(ftString, FEvents[idx].EventType) + ', ' + #13#10 +
            '  "ID_Resource": '       + ConvertJSON(ftInteger, FEvents[idx].ID_Resource) + ', ' + #13#10 +
            '  "ID_LinkedResource": ' + ConvertJSON(ftInteger, FEvents[idx].ID_LinkedResource) + #13#10 +
            '}';
   if str_object <> ''       then result += ', ' + #13#10 + '"Object": '       + #13#10 + '{ ' + #13#10 + str_object       + #13#10 + ' } ';
   if str_linkedobject <> '' then result += ', ' + #13#10 + '"LinkedObject": ' + #13#10 + '{ ' + #13#10 + str_linkedobject + #13#10 + ' } ';
   result:= '{' + result + '}';
  end;

 function TEventProcessor.RPCPost(const URL: shortstring; const Data: TStream): Boolean;
  var HTTP: THTTPSend;
  begin
   HTTP:= THTTPSend.Create;
   try
    HTTP.Cookies.Text:= HttpCookies;
    HTTP.Document.CopyFrom(Data, 0);
    HTTP.MimeType := 'Application/octet-stream';
    Result := HTTP.HTTPMethod('POST', URL);
    Data.Size := 0;
    if Result then
     begin
      Data.Seek(0, soFromBeginning);
      Data.CopyFrom(HTTP.Document, 0);
      HttpCookies:= HTTP.Cookies.Text;
     end;
   finally
    HTTP.Free;
   end;
 end;

 function TEventProcessor.SendEventToRPC(message: string; var response: string; var lag: integer): boolean;
  var str_request: string;
      ss: TStringStream;
      t_start, t_end: TDateTime;
  begin
   result:= false;
   response:= '';
   if Trim(Settings.RPC) = '' then Exit;

   str_request:= message;
   if str_request = '' then str_request:= #13#10;

   ss:= nil;
   try
    ss:= TStringStream.Create(str_request);
    ss.Seek(0, soFromBeginning);

    t_start:= Now;
    result:= RPCPost(Settings.RPC, ss);
    t_end:= Now;

    ss.Seek(0, soFromBeginning);
    response:= ss.DataString;

    lag:= round((t_end - t_start) * 86400000); //  com time in msec
   finally
    ss.Free;
   end;
  end;

 function TEventProcessor.CheckRPCResponse(response: string): boolean;
  var jData: TJSONData;
      jObject: TJSONObject;
      res_Success: boolean;
      res_ErrorCode: integer;
      res_ResultType: shortstring;
      res_Result: string;
  begin
   result:= false;
   jData:= nil;
   jObject:= nil;

   try
    jData:= GetJSON(response);
    if jData = nil then begin if Assigned(FOnLogMessage) then FOnLogMessage(self, 'wrong returned format'); Exit; end;

    //  look at response properties
    jObject:= TJSONObject(jData);
    if jObject.Get('Success') = Null then begin if Assigned(FOnLogMessage) then FOnLogMessage(self, 'missing Success field'); Exit; end;
    if jObject.Get('ErrorCode') = Null then begin if Assigned(FOnLogMessage) then FOnLogMessage(self, 'missing ErrorCode field'); Exit; end;
    if jObject.Get('ResultType') = Null then begin if Assigned(FOnLogMessage) then FOnLogMessage(self, 'missing ResultType field'); Exit; end;
    if jObject.Get('Result') = Null then begin if Assigned(FOnLogMessage) then FOnLogMessage(self, 'missing Result field'); Exit; end;

    try
     res_Success:= jObject.Get('Success');
     res_ErrorCode:= jObject.Get('ErrorCode');
     res_ResultType:= jObject.Get('ResultType');
     res_Result:= jObject.Get('Result');
    except
     if Assigned(FOnLogMessage) then FOnLogMessage(self, 'error retrieving result field value');
     Exit;
    end;

    if not res_Success then begin if Assigned(FOnLogMessage) then FOnLogMessage(self, res_Result); Exit; end;

    result:= true;
   finally
    FreeAndNil(jData);
   end;
  end;

 function TEventProcessor.ProcessEvent(idx: integer = 0): boolean;
  var str_object, str_response: string;
      lag: integer;
      str_log: string;
  begin
   result:= false;
   if (idx < 0) or (idx >= FEventsLength) then Exit;

   FLastID:= FEvents[idx].ID;
   str_object:= ExtractEventMessage(idx);

   if Trim(str_object) = '' then
    begin
     RemoveEvent(idx, false);
     Exit;
    end;

   str_log:= '%s: Sent event ID %d in %d msec. ';
   result:= SendEventToRPC(str_object, str_response, lag);
   if result then str_log:= Format(str_log, [TimeToStr(Now), FLastID, lag]) + Format('RPC replied %s', [str_response])
   else str_log:= Format(str_log, [TimeToStr(Now), FLastID, lag]) + 'noreply';
   if Assigned(FOnLogMessage) then FOnLogMessage(self, str_log);

   if result then result:= CheckRPCResponse(str_response);
  end;

 procedure TEventProcessor.RemoveEvent(idx: integer = 0; CheckEvent: boolean = true);
  var i, ID: integer;
  begin
   if (idx < 0) or (idx >= FEventsLength) then Exit;

   ID:= FEvents[idx].ID;

   if CheckEvent then ds_brm['brm'].Execute(Format('UPDATE "Events" SET "Checked" = 1 WHERE "ID" = %d', [ID]));
   for i:= idx to FEventsLength - 2 do FEvents[i]:= FEvents[i + 1];

   FEventsLength -= 1;

   if Assigned(FOnRemoveEvent) then FOnRemoveEvent(self, ID);
  end;

 procedure TEventProcessor.CheckEvents;
  var ds: TDataset;
      ID_Event, Priority, ID_Resource, ID_LinkedResource: integer;
      Date_Event: TDateTime;
      Resource, EventType: shortstring;
  begin
   ds:= nil;
   try
    ds:= ds_brm['brm'].Query(Format('SELECT TOP 10 * FROM "Events" WHERE ("Checked" = 0) AND ("Date" >= CONVERT(VARCHAR(20), GetDate(), 102)) AND ("ID" > %d) ORDER BY "ID" ASC', [FLastID]));
    if ds = nil then Exit;
    //if not ds.IsEmpty then lbl_TimeOfRequest.Caption:= DateTimeToStr(ds.FieldByName('DBNow').AsDateTime);

    ds.First;
    while not ds.EOF do
     begin
      ID_Event:= ds.FieldByName('ID').AsInteger;
      Date_Event:= ds.FieldByName('Date').AsDateTime;
      Priority:= ds.FieldByName('Priority').AsInteger;
      Resource:= ds.FieldByName('Resource').AsString;
      EventType:= ds.FieldByName('EventType').AsString;
      ID_Resource:= ds.FieldByName('ID_Resource').AsInteger;
      ID_LinkedResource:= ds.FieldByName('ID_LinkedResource').AsInteger;

      AddEvent(ID_Event, Date_Event, Priority, Resource, EventType, ID_Resource, ID_LinkedResource);

      ds.Next;
     end;

   finally
    FreeAndNil(ds);
   end;

   //  check the list of already loaded events (just the first one)
   repeat
    if FEventsLength = 0 then Exit;
    if ProcessEvent then begin RemoveEvent; Exit; end;
   until FEventsLength = 0;
  end;

end.


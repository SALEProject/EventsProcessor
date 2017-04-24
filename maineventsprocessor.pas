unit MainEventsProcessor;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
 StdCtrls, Spin, ComCtrls, db, EventsProcessorVars, EvtProcessor;

type

 { Tfrm_MainForm }

 Tfrm_MainForm = class(TForm)
  ed_Log: TMemo;
  Label1: TLabel;
  ed_Interval: TSpinEdit;
  Label2: TLabel;
  Label3: TLabel;
  lv_Events: TListView;
  Timer: TTimer;
  procedure ed_IntervalChange(Sender: TObject);
  procedure FormCreate(Sender: TObject);
  procedure TimerTimer(Sender: TObject);
 private
  { private declarations }
 public
  { public declarations }
  f_millisecond: double;
  EventProcessor: TEventProcessor;

  procedure RefreshEventList;
  procedure OnAppendEvent(Sender: TObject; ID_Event: integer);
  procedure OnRemoveEvent(Sender: TObject; ID_Event: integer);
  procedure OnLogMessage(Sender: TObject; Message: string);
 end;

var
 frm_MainForm: Tfrm_MainForm;

implementation

{$R *.lfm}

{ Tfrm_MainForm }

procedure Tfrm_MainForm.FormCreate(Sender: TObject);
var SettingsFileName: shortstring;
begin
 SettingsFileName:= ParamStr(0);
 SettingsFileName:= IncludeTrailingPathDelimiter(ExtractFilePath(SettingsFileName)) +
                    ExtractFileNameOnly(SettingsFileName) + '.xml';
 if FileExists(SettingsFileName) then Settings:= LoadEventsProcessorSettings(SettingsFileName)
 else Application.Terminate;

 InitDS;

 ed_Interval.Value:= Timer.Interval;

 f_millisecond:= 1 / 86400 / 1000;
 //InitSchedule;
 EventProcessor:= TEventProcessor.Create;
 EventProcessor.OnAppendEvent:= @OnAppendEvent;
 EventProcessor.OnRemoveEvent:= @OnRemoveEvent;
 EventProcessor.OnLogMessage:= @OnLogMessage;

 ed_Log.Clear;
end;

procedure Tfrm_MainForm.ed_IntervalChange(Sender: TObject);
var val: integer;
begin
 val:= ed_Interval.Value;
 if val < 50 then val:= 50;
 Timer.Interval:= val;
end;

var inTimer: boolean;
procedure Tfrm_MainForm.TimerTimer(Sender: TObject);
begin
 if inTimer then Exit;
 inTimer:= true;

 try
  EventProcessor.CheckEvents;
 finally
  inTimer:= false;
 end;

 //RefreshEventList;
end;

procedure Tfrm_MainForm.RefreshEventList;
 var i: integer;
     item: TListItem;
 begin
  lv_Events.Clear;

  for i:= 0 to EventProcessor.EventsCount - 1 do
   begin
    item:= lv_Events.Items.Add;
    item.Caption:= IntToStr(EventProcessor.Events[i].ID);
    item.SubItems.Add(DateTimeToStr(EventProcessor.Events[i].Date));
    item.SubItems.Add(IntToStr(EventProcessor.Events[i].Priority));
    item.SubItems.Add(EventProcessor.Events[i].Resource);
    item.SubItems.Add(EventProcessor.Events[i].EventType);
    item.SubItems.Add(IntToStr(EventProcessor.Events[i].ID_Resource));
    item.SubItems.Add(IntToStr(EventProcessor.Events[i].ID_LinkedResource));
   end;
 end;

 procedure Tfrm_MainForm.OnAppendEvent(Sender: TObject; ID_Event: integer);
  var i: integer;
      b: boolean;
      item: TListItem;
  begin
   b:= false;
   i:= -1;
   while not b and (i < EventProcessor.EventsCount - 1) do
    begin
     i += 1;
     if EventProcessor.Events[i].ID = ID_Event then b:= true;
    end;

   if b then
    begin
     item:= lv_Events.Items.Add;
     item.Caption:= IntToStr(ID_Event);
     item.SubItems.Add(DateTimeToStr(EventProcessor.Events[i].Date));
     item.SubItems.Add(IntToStr(EventProcessor.Events[i].Priority));
     item.SubItems.Add(EventProcessor.Events[i].Resource);
     item.SubItems.Add(EventProcessor.Events[i].EventType);
     item.SubItems.Add(IntToStr(EventProcessor.Events[i].ID_Resource));
     item.SubItems.Add(IntToStr(EventProcessor.Events[i].ID_LinkedResource));
    end;
  end;

 procedure Tfrm_MainForm.OnRemoveEvent(Sender: TObject; ID_Event: integer);
  var i: integer;
      b: boolean;
  begin
   b:= false;
   i:= -1;
   while not b and (i < lv_Events.Items.Count - 1) do
    begin
     i += 1;
     if StrToIntDef(lv_Events.Items[i].Caption, 0) = ID_Event then b:= true;
    end;

   if b then lv_Events.Items[i].Delete;
  end;

 procedure Tfrm_MainForm.OnLogMessage(Sender: TObject; Message: string);
  begin
   ed_Log.Lines.Add(Message);
  end;

end.


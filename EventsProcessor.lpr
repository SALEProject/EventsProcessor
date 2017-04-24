program EventsProcessor;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}{$IFDEF UseCThreads}
 cthreads,
 {$ENDIF}{$ENDIF}
 Interfaces, // this includes the LCL widgetset
 Forms, bitMSSQLDataConnector, MainEventsProcessor, EventsProcessorVars,
 EvtProcessor
 { you can add units after this };

{$R *.res}

begin
 //SetHeapTraceOutput('heap.trc');
 RequireDerivedFormResource := True;
 Application.Initialize;
 Application.CreateForm(Tfrm_MainForm, frm_MainForm);
 Application.Run;
end.


{
icon link:
  Application icon created by
  <a href="https://www.flaticon.com/de/autoren/freepik" title="Freepik">Freepik</a>
  from <a href="https://www.flaticon.com/de/" title="Flaticon"> www.flaticon.com</a>
}

program rcc;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, rccMain; //, rccCodes;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

unit rccSetupRemoteControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  StdCtrls,
  rccGlobal;

type

  { TSetupRemoteControlForm }

  TSetupRemoteControlForm = class(TForm)
    ButtonPanel: TButtonPanel;
    edName: TEdit;
    Label1: TLabel;
    lblName: TLabel;
    KeysMemo: TMemo;
    Panel1: TPanel;
  private
    function GetKeys: TStrings;
    function GetRCName: String;
    procedure SetKeys(const AValue: TStrings);
    procedure SetRCName(const AValue: String);
  public
    property Keys: TStrings read GetKeys write SetKeys;
    property RemoteControlName: String read GetRCName write SetRCName;
  end;

var
  SetupRemoteControlForm: TSetupRemoteControlForm;

implementation

{$R *.lfm}

function TSetupRemoteControlForm.GetKeys: TStrings;
begin
  Result := KeysMemo.Lines;
end;

function TSetupRemoteControlForm.GetRCName: String;
begin
  Result := edName.Text;
end;

procedure TSetupRemoteControlForm.SetKeys(const AValue: TStrings);
begin
  KeysMemo.Lines.Assign(AValue);
end;

procedure TSetupRemoteControlForm.SetRCName(const AValue: String);
begin
  edName.Text := AValue;
end;

end.


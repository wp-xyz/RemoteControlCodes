unit rccSerialPortSettings;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, ExtCtrls, Forms, ButtonPanel, Serial,
  rccGlobal;

type
  { TSerPortForm }
  TSerPortForm = class(TForm)
    ButtonPanel: TButtonPanel;
    cmbBaudRate : TComboBox;
    cmbPort : TComboBox;
    cmbStopBits : TComboBox;
    cmbParity : TComboBox;
    cmbDatabits : TComboBox;
    lblBaudRate : TLabel;
    lblParity : TLabel;
    lblDatabits : TLabel;
    lblStopBits : TLabel;
    lblPort : TLabel;
    MainPanel : TPanel;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    { private declarations }
    function  GetBaudrate: integer;
    function  GetDatabits: integer;
    function  GetParity: TParityType;
    function  GetPort: string;
    function  GetStopBits: Integer;
    procedure SetBaudrate(value: integer);
    procedure SetDatabits(Value: integer);
    procedure SetParity(Value: TParityType);
    procedure SetPort(Value: string);
    procedure SetStopBits(Value: Integer);
    function ValidParams(out AControl: TWinControl; out AMsg: String): Boolean;
  public
    { public declarations }
    function GetSerialParams: TSerialParams;
    procedure SetSerialParams(const AParams: TSerialParams);
  end;

var
  SerPortForm : TSerPortForm;


implementation

{$R *.lfm}

uses
  Dialogs, TypInfo,
  rccSerialDevice;


{ TSerPortForm }

procedure TSerPortForm.FormCreate(Sender: TObject);
begin
  cmbPort.Items.CommaText := GetSerialPortNames;
end;

function TSerPortForm.GetSerialParams: TSerialParams;
begin
  Result.Port := GetPort;
  Result.BaudRate := GetBaudRate;
  Result.DataBits := GetDataBits;
  Result.StopBits := GetStopBits;
  Result.Parity := GetParity;
end;

procedure TSerPortForm.SetSerialParams(const AParams: TSerialParams);
begin
  SetPort(AParams.Port);
  SetBaudRate(AParams.BaudRate);
  SetDataBits(AParams.DataBits);
  SetStopBits(AParams.StopBits);
  SetParity(AParams.Parity);
end;

function TSerPortForm.GetBaudrate: integer;
begin
  result := StrToInt(cmbBaudRate.Text);
end;

function TSerPortForm.GetDatabits: integer;
begin
  result := StrToInt(cmbDatabits.Text);
end;

function TSerPortForm.GetParity: TParityType;
begin
  Result := TParityType(cmbParity.ItemIndex);
end;

function TSerPortForm.GetPort: string;
begin
  if cmbPort.ItemIndex = -1 then
    result := ''
  else
    result := cmbPort.Items[cmbPort.ItemIndex];
end;

function TSerPortForm.GetStopBits: Integer;
begin
  result := cmbStopBits.ItemIndex + 1;
end;

procedure TSerPortForm.OKButtonClick(Sender: TObject);
var
  C: TWinControl;
  msg: String;
begin
  if not ValidParams(C, msg) then
  begin
    C.SetFocus;
    MessageDlg(msg, mtError, [mbOK], 0);
    ModalResult := mrNone;
  end;
end;

procedure TSerPortForm.SetBaudrate(Value: integer);
begin
  cmbBaudRate.ItemIndex := cmbBaudRate.Items.IndexOf(IntToStr(Value));
  if cmbBaudRate.ItemIndex = -1 then
    raise Exception.CreateFmt('%d is not a valid baud rate.', [Value]);
end;

procedure TSerPortForm.SetDataBits(Value: integer);
begin
  cmbDatabits.ItemIndex := cmbDatabits.Items.IndexOf(IntToStr(Value));
  if cmbDatabits.ItemIndex = -1 then
    raise Exception.Create('Illegal databits value');
end;

procedure TSerPortForm.SetParity(value: TParityType);
begin
  cmbParity.ItemIndex := ord(Value);
end;

procedure TSerPortForm.SetPort(Value: string);
var
  i : integer;
begin
  if Value = '' then
    cmbPort.ItemIndex := -1
  else
    cmbPort.ItemIndex := cmbPort.Items.IndexOf(Uppercase(Value));
end;

procedure TSerPortForm.SetStopBits(Value: Integer);
begin
  cmbStopBits.ItemIndex := cmbStopBits.Items.IndexOf(IntToStr(Value));
  if cmbStopBits.ItemIndex = -1 then
    raise Exception.Create('Illegal stopbits value.');
end;

function TSerPortForm.ValidParams(out AControl: TWinControl; out AMsg: String): Boolean;
begin
  Result := False;

  if cmbPort.ItemIndex = -1 then
  begin
    AMsg := 'No port selected.';
    AControl := cmbPort;
    exit;
  end;

  Result := true;
end;

end.


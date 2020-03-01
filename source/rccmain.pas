unit rccMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, XMLConf, Types, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ExtCtrls, ActnList, Menus, ComCtrls,
  TAGraph, TASeries, TASources, TACustomSource,
  mrumanager,
  rccGlobal, rccCodes, rccSerialDevice;

type

  { TMainForm }

  TMainForm = class(TForm)
    acFileOpen: TAction;
    acFileSave: TAction;
    acFileSaveAs: TAction;
    acFileQuit: TAction;
    acDeleteCode: TAction;
    acSerialPort: TAction;
    acSetupKeys: TAction;
    acClearLog: TAction;
    acClearCodes: TAction;
    acClearCodesAndKeys: TAction;
    acCodesHexPascal: TAction;
    acCodesHexC: TAction;
    acCodesDecimal: TAction;
    acSend: TAction;
    acCodesChart: TAction;
    acConnect: TAction;
    acAbout: TAction;
    ActionList: TActionList;
    Chart: TChart;
    edName: TEdit;
    Grid: TDrawGrid;
    Images: TImageList;
    lblName: TLabel;
    MainMenu: TMainMenu;
    Memo: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    mnuHelp: TMenuItem;
    mnuFileOpenRecent: TMenuItem;
    mnuClearLog: TMenuItem;
    MRUMenuManager: TMRUMenuManager;
    N4: TMenuItem;
    mnuClearCodes: TMenuItem;
    mnuClearCodesAndKeys: TMenuItem;
    N3: TMenuItem;
    mnuSetupRemoteControl: TMenuItem;
    N2: TMenuItem;
    mnuDeleteCode: TMenuItem;
    mnuFileOpen: TMenuItem;
    mnuFileSave: TMenuItem;
    mnuFileQuit: TMenuItem;
    mnuCodesStart: TMenuItem;
    mnuCodesStop: TMenuItem;
    mnuFileSaveAs: TMenuItem;
    mnuSettings: TMenuItem;
    mnuSettingsSerial: TMenuItem;
    mnuCodes: TMenuItem;
    N1: TMenuItem;
    mnuFile: TMenuItem;
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    RawDataSeries: TAreaSeries;
    RecentFilePopup: TPopupMenu;
    SaveDialog: TSaveDialog;
    CodeTimer: TTimer;
    pgCodes: TTabSheet;
    pgLog: TTabSheet;
    ChartSplitter: TSplitter;
    ToolBar1: TToolBar;
    tbOpen: TToolButton;
    tbQuit: TToolButton;
    tbSave: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    RawCodesChartSource: TUserDefinedChartSource;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    procedure acAboutExecute(Sender: TObject);
    procedure acClearCodesAndKeysExecute(Sender: TObject);
    procedure acClearCodesExecute(Sender: TObject);
    procedure acClearLogExecute(Sender: TObject);
    procedure acCodesChartExecute(Sender: TObject);
    procedure acCodesFormatExecute(Sender: TObject);
    procedure acConnectExecute(Sender: TObject);
    procedure acDeleteCodeExecute(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acFileQuitExecute(Sender: TObject);
    procedure acFileSaveAsExecute(Sender: TObject);
    procedure acFileSaveExecute(Sender: TObject);
    procedure acSendExecute(Sender: TObject);
    procedure acSerialPortExecute(Sender: TObject);
    procedure acSetupKeysExecute(Sender: TObject);
    procedure CodeTimerTimer(Sender: TObject);
    procedure edNameChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure GridGetCellHint(Sender: TObject; ACol, ARow: Integer;
      var HintText: String);
    procedure GridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    (*
    procedure RawCodesChartSourceGetChartDataItem(
      ASource: TUserDefinedChartSource; AIndex: Integer;
      var AItem: TChartDataItem);
      *)
    procedure RecentFileHandler(Sender: TObject; const AFileName: String
      );
  private
    FCodeList: TRemoteControlCodeList;
    //FCodeFormatMask: String;

    FSerialDevice: TSerialDevice;
    FRunning: Boolean;
    FFileName: String;
    FDirty: Boolean;
    FRecentFilesManager: TMRUMenuManager;
    function Connect: Boolean;
    procedure Disconnect;
    procedure SerialConnectHandler(Sender: TObject);
    procedure SerialDataHandler(Sender: TObject; const AData: RawByteString);
    procedure SerialDisconnectHandler(Sender: TObject);
    {$IFDEF SYNASER}
    procedure SerialErrorHandler(Sender: TObject; AStatus: Integer);
    {$ENDIF}
    procedure StartTransfer;
    procedure StopTransfer;
    procedure TransmitCode;

    procedure CodesChanged;
    procedure LoadCodes(const AFileName: String);
    procedure SaveCodes(const AFileName: String);
    procedure UpdateActionStates(ARow: Integer);
    procedure UpdateCaption;
    procedure UpdateCodesFormat;
    procedure UpdateGrid;

    function GetCellText(ACol, ARow: Integer): String;
    function GetCurrentCode: TRemoteControlCode;
    function GetRowCode(ARow: Integer): TRemoteControlCode;

    procedure Plot(ACode: TRemoteControlCode);

    procedure ReadIni;
    procedure WriteIni;
  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLType, IniFiles, TypInfo, Serial,
  rccUtils, rccSerialPortSettings, rccSetupRemoteControl, rccAbout;


{ TMainForm }

procedure TMainForm.acClearCodesExecute(Sender: TObject);
begin
  FCodeList.ClearCodes;
  CodesChanged;
end;

procedure TMainForm.acAboutExecute(Sender: TObject);
var
  F: TAboutForm;
begin
  F := TAboutForm.Create(nil);
  try
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TMainForm.acClearCodesAndKeysExecute(Sender: TObject);
begin
  FCodeList.Clear;
  Grid.RowCount := 2;
  Grid.Row := 1;
  CodesChanged;
end;

procedure TMainForm.acClearLogExecute(Sender: TObject);
begin
  Memo.Clear;
end;

procedure TMainForm.acCodesChartExecute(Sender: TObject);
begin
  Chart.Visible := acCodesChart.Checked;
  ChartSplitter.Visible := Chart.Visible;
  ChartSplitter.Top := 0;
  Plot(GetCurrentCode);
end;

procedure TMainForm.acCodesFormatExecute(Sender: TObject);
begin
  UpdateCodesFormat;
end;

procedure TMainForm.acConnectExecute(Sender: TObject);
begin
  if FRunning then
    StopTransfer
  else
    StartTransfer;
end;

procedure TMainForm.acDeleteCodeExecute(Sender: TObject);
var
  code: TRemoteControlCode;
begin
  code := GetCurrentCode;
  if code <> nil then
  begin
    code.Clear;
    CodesChanged;
    FDirty := true;
  end;
end;

procedure TMainForm.acFileOpenExecute(Sender: TObject);
begin
  with OpenDialog do
  begin
    InitialDir := ExtractFileDir(FFileName);
    if Execute then
    begin
      LoadCodes(FileName);
      FFileName := FileName;
      UpdateCaption;
    end;
  end;
end;

procedure TMainForm.acFileQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.acFileSaveAsExecute(Sender: TObject);
begin
  with SaveDialog do
  begin
    FileName := '';
    InitialDir := ExtractFileDir(FFileName);
    if Execute then begin
      SaveCodes(FileName);
      FFileName := FileName;
      UpdateCaption;
    end;
  end;
end;

procedure TMainForm.acFileSaveExecute(Sender: TObject);
begin
  if FFileName = '' then
    acFileSaveAsExecute(nil)
  else
    SaveCodes(FFileName);
end;

procedure TMainForm.acSendExecute(Sender: TObject);
begin
  TransmitCode;
end;

procedure TMainForm.acSerialPortExecute(Sender: TObject);
var
  F: TSerPortForm;
begin
  F := TSerPortForm.Create(nil);
  try
    F.Position := poMainFormCenter;
    F.SetSerialParams(SerialParams);
    if F.ShowModal = mrOK then
      SerialParams := F.GetSerialParams;
  finally
    F.Free;
  end;
end;

procedure TMainForm.acSetupKeysExecute(Sender: TObject);
var
  F: TSetupRemoteControlForm;
  L: TStrings;
begin
  F := TSetupRemoteControlForm.Create(nil);
  L := TStringList.Create;
  try
    FCodeList.ListKeys(L);
    F.Keys := L;
    F.RemoteControlName := edName.Text;
    F.Position := poMainFormCenter;
    if F.ShowModal = mrOK then
    begin
      edName.Text := F.RemoteControlName;
      FCodeList.PopulateFromKeyList(F.Keys);
      Grid.RowCount := FCodeList.Count + Grid.FixedRows;
      CodesChanged;
    end;
  finally
    L.Free;
    F.Free;
  end;
end;

procedure TMainForm.CodesChanged;
var
  code: TRemoteControlCode;
begin
  UpdateGrid;
  if (FCodeList.Count > 0) and (Grid.Row <= Grid.FixedRows) then
    Grid.Row := Grid.FixedRows;

  code := GetCurrentCode;
  if (code <> nil) and Chart.Visible then
    Plot(code);
end;

procedure TMainForm.CodeTimerTimer(Sender: TObject);
begin
  CodeTimer.Enabled := false;
end;

procedure TMainForm.edNameChange(Sender: TObject);
begin
  FDirty := true;
  UpdateCaption;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  WriteIni;
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  res: Integer;
begin
  if FDirty then
  begin
    res := MessageDlg('Codes are not saved. Discard?', mtConfirmation, [mbYes, mbNo], 0);
    CanClose := (res = mrYes);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FCodeList := TRemoteControlCodeList.Create;
  FCodeList.Add(TRemoteControlCode.Create(''));

  FRecentFilesManager := TMRUMenuManager.Create(self);
  FRecentFilesManager.IniFileName := CalcIniFileName;
  FRecentFilesManager.IniSection := 'Recent files';
  FRecentFilesManager.MenuItem := mnuFileOpenRecent;
  FRecentFilesManager.MenuCaptionMask := '%0x - %1s';
  FRecentFilesManager.MaxRecent := 16;
  FRecentFilesManager.PopupMenu := RecentFilePopup;
  FRecentFilesManager.OnRecentFile := @RecentFileHandler;

  ReadIni;
  UpdateCaption;
  CodesChanged;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FCodeList.Free;
end;

function TMainForm.GetCellText(ACol, ARow: Integer): String;
var
  code: TRemoteControlCode;
begin
  Result := '';
  if (FCodeList.Count = 0) or (ARow = 0) then
    exit;
  code := FCodeList[ARow - Grid.FixedRows];
  if code = nil then
    exit;
  case ACol of
    0: Result := code.KeyName;
    1: Result := code.CodeToString;
    2: Result := code.CodeTypeName;
    3: Result := code.BitsToString;
    4: Result := code.RawToString;
  end;
end;

function TMainForm.GetCurrentCode: TRemoteControlCode;
begin
  Result := GetRowCode(Grid.Row);
end;

function TMainForm.GetRowCode(ARow: Integer): TRemoteControlCode;
var
  idx: Integer;
begin
  idx := ARow - Grid.FixedRows;
  if (idx > -1) and (idx < FCodeList.Count) then
    Result := FCodeList[idx]
  else
    Result := nil;
end;

procedure TMainForm.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  txt: String;
begin
  txt := GetCellText(ACol, ARow);
  Grid.Canvas.TextRect(ARect, ARect.Left + varCellPadding, ARect.Top + varCellPadding, txt);
end;

procedure TMainForm.GridGetCellHint(Sender: TObject; ACol, ARow: Integer;
  var HintText: String);
var
  s: String;
begin
  s := GetCellText(ACol, ARow);
  if Grid.Canvas.TextWidth(s) > Grid.ColWidths[aCol] then
    HintText := WrapText(s, 128)
  else
    HintText := '';
end;

procedure TMainForm.GridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  code: TRemoteControlCode;
begin
  if Key = VK_DELETE then
  begin
    code := GetCurrentCode;
    if Assigned(code) then
    begin
      code.Clear;
      CodesChanged;
    end;
  end;
end;

procedure TMainForm.GridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  if Chart.Visible then
    Plot(GetRowCode(ARow));
  UpdateActionStates(ARow);
end;

function TMainForm.Connect: Boolean;
const
  TIMEOUT = 1000;
var
  p : integer;
begin
  Result := false;

  if pos(SerialParams.Port, GetSerialPortNames) = 0 then begin
    MessageDlg(
      Format('Port %s does not exist on this PC.', [SerialParams.Port]),
      mtError, [mbOK], 0
    );
    exit;
  end;

  FreeAndNil(FSerialDevice);
  FSerialDevice := TSerialDevice.Create(TArduinoThread);
  FSerialDevice.OnConnect := @SerialConnectHandler;
  FSerialDevice.OnDisconnect := @SerialDisconnectHandler;
  FSerialDevice.OnData := @SerialDataHandler;
  {$IFDEF SYNASER}
  FSerialDevice.OnError := @SerialErrorHandler;
  {$ENDIF}
  FSerialDevice.Connect(
    SerialParams.Port,
    SerialParams.Baudrate,
    SerialParams.Databits,
    SerialParams.Parity,
    SerialParams.StopBits,
    TIMEOUT
  );

  Result := FSerialDevice.Connected;
  if not Result then
    Memo.Lines.Add('Could not connect to serial port.');
end;

procedure TMainForm.Disconnect;
begin
  if (FSerialDevice <> nil) and FSerialDevice.Connected then
  begin
    FSerialDevice.Disconnect;
  end;
end;

procedure TMainForm.LoadCodes(const AFileName: String);
var
  xml: TXMLConfig;
  n: Integer;
  i: Integer;
  rcCode: TRemoteControlCode;
  key: String;
  keyName: String;
  codeValue: LongInt;
  codeType: string;
  codeBits: Integer;
  codeRaw: String;
  s: String;
begin
  xml := TXMLConfig.Create(nil);
  try
    xml.RootName := RCF_ROOT;
    xml.LoadFromFile(AFileName);

    caption := xml.Rootname;

    if xml.RootName <> RCF_ROOT then
    begin
      MessageDlg('Invalid rcf file', mtError, [mbOK], 0);
      exit;
    end;

    edName.Text := xml.GetValue('General/Name', '');
    FCodeList.Clear;

    n := xml.GetValue('Keys/Count', 0);
    for i := 1 to n do
    begin
      key := 'Keys/Key' + IntToStr(i);
      keyname := xml.GetValue(key + '/Name', '');
      if keyname = '' then
        Continue;
      s := xml.GetValue(key + '/Code', '');
      if s <> '' then codeValue := StrToInt(s) else codeValue := 0;
      codeType := xml.GetValue(key + '/CodeType', '');
      s := xml.GetValue(key + '/CodeBits', '');
      if s <> '' then codebits := StrToInt(s) else codeBits := 0;
      codeRaw := xml.GetValue(key + '/Raw', '');

      rcCode := TRemoteControlCode.Create(keyname);
      rcCode.Code := codeValue;
      rcCode.SetCodeTypeFromString(codeType);
      rcCode.Bits := codeBits;
      rcCode.Raw := rcCode.StringToRaw(codeRaw);
      FCodeList.Add(rcCode)
    end;
    Grid.RowCount := FCodeList.Count + Grid.FixedRows;
    CodesChanged;

    FRecentFilesManager.AddToRecent(AFileName);
  finally
    xml.Free;
  end;

  FDirty := false;
  UpdateCaption;
end;

procedure TMainForm.SerialConnectHandler(Sender: TObject);
begin
  if Memo.Lines.Count > 0 then
    Memo.Lines.Add('');
  Memo.Lines.Add('Connected (Serial handle = %d)', [FSerialDevice.SerialHandle]);
end;

procedure TMainForm.SerialDataHandler(Sender: TObject; const AData: RawByteString);
var
  valuestr: String;
  sa: TStringArray;
  code: TRemoteControlCode;
begin
  // Display string in the memo
  Memo.Lines.Add('Receiving: ' + AData);

  // When received string does not begin with '!' we have a message from the
  // Arduino which is not a code. Don't process any further.
  if AData[1] <> '!' then
    exit;

  // Avoid catching follow-up codes
  if CodeTimer.Enabled then
    exit;
  CodeTimer.Enabled := true;

  // Remove leading code identifier '!'
  valueStr := Copy(AData, 2, MaxInt);
  if valueStr = '' then
    exit;

  code := GetCurrentCode;

  // Split received string and add to current grid row
  // 0: type, 1:bits, 2:code, 3:raw
  sa := valueStr.Split(';');
  code.SetCodeTypeFromString(sa[0]);
  code.Bits := StrToInt(sa[1]);
  code.Code := StrToInt('$' + sa[2]);
  code.Raw := code.StringToRaw(sa[3]);

  CodesChanged;
  if Grid.Row < Grid.RowCount then
    Grid.Row := Grid.Row + 1;

  // mark code list as being "dirty", i.e. user will be prompted to save it.
  if not FDirty then
  begin
    FDirty := true;
    UpdateCaption;
  end;
  UpdateActionStates(Grid.Row);
end;

procedure TMainForm.SerialDisconnectHandler(Sender: TObject);
begin
  Memo.Lines.Add('Disconnected');
end;

{$IFDEF SYNASER}
procedure TMainForm.OnErrorHandler(Sender: TObject; AStatus: Integer);
var
  s: String;
begin
  if AStatus = ErrTimeOut then
    exit;
  s := Format('Port error %d (%s)', [AStatus, GetSerialDeviceErrorText(AStatus)]);
  Memo.Lines.Add(s);
end;
{$ENDIF}

procedure TMainForm.Plot(ACode: TRemoteControlCode);
var
  i: Integer;
  x, y: double;
  isMark: Boolean;
begin
  RawDataSeries.BeginUpdate;
  try
    RawDataSeries.Clear;

    if (ACode = nil) or (Length(ACode.Raw) = 0) then
      exit;

    x := 0;
    RawDataSeries.AddXY(x, 0);
    for i := 0 to High(ACode.Raw) do
    begin
      isMark := (i mod 2 = 0);
      y := Integer(isMark);
      RawDataSeries.AddXY(x, y);
      x := x + ACode.Raw[i] * 1E-3;  // --> ms
      RawDataSeries.AddXY(x, y);
    end;
  finally
    RawDataSeries.EndUpdate;
  end;
end;

procedure TMainForm.ReadIni;
var
  ini: TCustomIniFile;
  L, T, W, H: Integer;
  R: TRect;
  s: String;
begin
  ini := CreateAppIni;
  try
    L := ini.ReadInteger('MainForm', 'Left', Left);
    T := ini.ReadInteger('MainForm', 'Top', Top);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    R := Monitor.WorkareaRect;
    if W > R.Right - R.Left then W := R.Right - R.Left;
    if H > R.Bottom - R.Top then H := R.Bottom - R.Top;
    if L < R.Left then L := R.Left;
    if T < R.Top then T := R.Top;
    if L + W > R.Right then L := R.Right - W;
    if T + H > R.Bottom then T := R.Bottom - H;
    SetBounds(L, T, W, H);

    Chart.Height := ini.ReadInteger('MainForm', 'Chart.Height', Chart.Height);
    acCodesChart.Checked := ini.ReadBool('MainForm', 'Chart.Visible', Chart.Visible);
    acCodesChartExecute(nil);

    SerialParams.Port := ini.ReadString('SerialParams', 'Port', SerialParams.Port);
    SerialParams.BaudRate := ini.ReadInteger('SerialParams', 'BaudRate', SerialParams.BaudRate);
    SerialParams.DataBits := ini.ReadInteger('SerialParams', 'DataBits', SerialParams.DataBits);
    SerialParams.StopBits := ini.ReadInteger('SerialParams', 'StopBits', SerialParams.StopBits);
    s := ini.ReadString('SerialParams', 'Parity', Copy(GetEnumName(TypeInfo(TParityType), ord(SerialParams.Parity)), 1, 1));
    if s <> '' then
      case UpperCase(s)[1] of
        'N': SerialParams.Parity := NoneParity;
        'O': SerialParams.Parity := OddParity;
        'E': SerialParams.Parity := EvenParity;
      end;

  finally
    ini.Free;
  end;
end;

procedure TMainForm.RecentFileHandler(Sender: TObject;
  const AFileName: String);
begin
  LoadCodes(AFileName);
end;

procedure TMainForm.SaveCodes(const AFileName: String);
var
  xml: TXMLConfig;
  i: Integer;
  res: Integer;
  key: String;
  code: TRemoteControlCode;
begin
  if AFileName = '' then
  begin
    MessageDlg('No filename specified', mtError, [mbOK], 0);
    exit;
  end;

  if edName.Text = '' then
  begin
    res := MessageDlg('Specifiy the name of the remote control.', mtInformation, [mbYes, mbNo], 0);
    if res = mrYes then
      exit;
  end;

  xml := TXMLConfig.Create(nil);
  try
    xml.RootName := RCF_ROOT;
    xml.SetValue('General/Name', edName.Text);
    for i := 0 to FCodeList.Count-1 do
    begin
      code := FCodeList[i];
      key := 'Keys/Key' + IntToStr(i+1);
      xml.SetValue(key + '/Name', code.KeyName);
      if code.IsEmpty then
        Continue;
      xml.SetValue(key + '/Code', code.CodeToString);
      xml.SetValue(key + '/CodeType', code.CodeTypeName);
      xml.SetValue(key + '/CodeBits', code.Bits);
      xml.SetValue(key + '/Raw', code.RawToString);
    end;
    xml.SetValue('Keys/Count', FCodeList.Count);
    xml.SaveToFile(AFileName);
    xml.Flush;
  finally
    xml.Free;
  end;

  FDirty := false;
  UpdateCaption;
end;

procedure TMainForm.StartTransfer;
begin
  if not FRunning then begin
    FRunning := Connect;
    if FRunning then begin
      acConnect.Caption := 'Disconnect';
      UpdateActionStates(Grid.Row);
    end;
  end;
end;

procedure TMainForm.StopTransfer;
begin
  if FRunning then begin
    Disconnect;
    FRunning := false;
    CodeTimer.Enabled := false;
    acConnect.Caption := 'Connect';
    UpdateActionStates(Grid.Row);
  end;
end;

procedure TMainForm.TransmitCode;
const
  n = 3;
  REPEAT_DELAY = 20;
var
  code: TRemoteControlCode;
  nRaw: Word;
  i: Integer;
  s: String;
begin
  code := GetCurrentCode;
  if (code = nil) or (code.Bits = 0) or (code.KeyName = '') or (code.Code = 0) then
  begin
    Memo.Lines.Add('Cannot send: incomplete data.');
    exit;
  end;

  if (code.Code = $FF) then
    // repeat code
  else
  if (code.CodeType > ctUNUSED) then
  begin
    // Sends decoded data
    // - code type
    // - code value
    // - bits
    Memo.Lines.Add('Sending %s code %s (%d bits)', [code.CodeTypeName, code.CodeToString, code.Bits]);
    s := Format('%d,%d,%d ', [byte(code.CodeType), code.Code, code.Bits]);
    // Send code n times
    for i := 1 to n do
      FSerialDevice.Transmit(s[1], Length(s));
  end else
  if (code.CodeType = ctUNKNOWN) then
  begin
    // Sends raw data:
    // - code type (-1)
    // - count of marks and spaces
    // - times of marks and spaces in µs.
    nRaw := Length(code.Raw);
    s := IntToStr(byte(code.CodeType)) + ',' + IntToStr(nRaw);
    for i := 0 to nRaw-1 do
      s := s + ',' + IntToStr(code.Raw[i]);
    Memo.Lines.Add('Sending %d raw mark/space times', [nRaw]);
    // Send code n times
    for i := 1 to n do begin
      FSerialDevice.Transmit(s[1], Length(s));
      Sleep(REPEAT_DELAY);
    end;
  end;
end;

procedure TMainForm.UpdateActionStates(ARow: Integer);
var
  code: TRemoteControlCode;
begin
  code := GetCurrentCode;
  acSend.Enabled := (code <> nil) and FRunning;
end;

procedure TMainForm.UpdateCaption;
var
  s: String;
begin
  if FFileName = '' then
    s := APP_CAPTION
  else
    s := Format('%s - %s', [APP_CAPTION, ExtractFileName(FFileName)]);
  if FDirty then
    Caption := s + ' [*]'
  else
    Caption := s;
end;

procedure TMainForm.UpdateCodesFormat;
var
  r: Integer;
  code: Integer;
begin
  if acCodesHexPascal.Checked then
    CodeFormatMask := '$%x'
  else if acCodesHexC.Checked then
    CodeFormatMask := '0x%x'
  else if acCodesDecimal.Checked then
    CodeFormatMask := '%d'
  else
    raise Exception.Create('Unknown code format');

  UpdateGrid;
end;

procedure TMainForm.UpdateGrid;
begin
  Grid.Invalidate;
end;

procedure TMainForm.WriteIni;
var
  ini: TCustomIniFile;
  s: String;
begin
  ini := CreateAppIni;
  try
    ini.WriteInteger('MainForm', 'Left', Left);
    ini.WriteInteger('MainForm', 'Top', Top);
    ini.WriteInteger('MainForm', 'Width', Width);
    ini.WriteInteger('MainForm', 'Height', Height);
    ini.WriteInteger('MainForm', 'Chart.Height', Chart.Height);
    ini.WriteBool('MainForm', 'Chart.Visible', acCodesChart.Checked);

    ini.WriteString('SerialParams', 'Port', SerialParams.Port);
    ini.WriteInteger('SerialParams', 'BaudRate', SerialParams.BaudRate);
    ini.WriteInteger('SerialParams', 'DataBits', SerialParams.DataBits);
    ini.WriteInteger('SerialParams', 'StopBits', SerialParams.StopBits);
    s := GetEnumName(TypeInfo(TParityType), ord(SerialParams.Parity));
    ini.WriteString('SerialParams', 'Parity', copy(s, 1, 1));
  finally
    ini.Free;
  end;
end;


end.


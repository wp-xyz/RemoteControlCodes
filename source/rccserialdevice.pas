unit rccSerialDevice;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Serial;

type

  { SerialThread }

  TSerialThread = class;

  TSerialDataEvent = procedure(Sender: TObject; const AData: RawByteString) of object;
  TSerialErrorEvent = procedure(Sender: TObject; AStatus: Integer) of object;

  TSerialThread = class(TThread)
  private
    FTimeOut: Integer;
    FOnData: TSerialDataEvent;
    FOnError: TSerialErrorEvent;
    function GetBufferSize: Integer;
    procedure SetBufferSize(AValue: Integer);

  protected
    FBuffer: RawByteString;
    FSerialHandle: LongInt;
    FStatus: Integer;
    procedure DoDataAvail;
    procedure DoError;
    property BufferSize: integer read GetBufferSize write SetBufferSize;
    property SerialHandle: LongInt read FSerialHandle;

  public
    constructor Create(CreateSuspended: Boolean;
      const StackSize: SizeUInt = DefaultStackSize);
    destructor Destroy; override;
    function SetPortParams(const APort: String; ABaudRate, ADataBits, AStopBits: Integer;
      AParity: TParityType): boolean;

    property Terminated;
    property TimeOut: Integer read FTimeOut write FTimeOut default 500;
    property OnData: TSerialDataEvent read FOnData write FOnData;
    property OnError: TSerialErrorEvent read FOnError write FOnError;
  end;

  TSerialThreadClass = class of TSerialThread;


  { TSerialDevice }

  TSerialDevice = class
  private
    FThreadClass: TSerialThreadClass;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnData: TSerialDataEvent;
    FOnError: TSerialErrorEvent;
    function GetSerialHandle: LongInt;
    procedure SetOnData(AValue: TSerialDataEvent);
    procedure SetOnError(AValue: TSerialErrorEvent);
  protected
    FThread: TSerialThread;
    procedure ThreadTerminateHandler(Sender: TObject);
  public
    constructor Create(AThreadClass: TSerialThreadClass);
    destructor Destroy; override;
    procedure Connect(APort:String; ABaudRate, ADataBits: Integer;
      AParity: TParityType; AStopBits: Integer; ATimeOut: Integer);
    function Connected: Boolean;
    procedure Disconnect;
    procedure Transmit(var ABuffer; ABufferSize: Integer);
    property SerialHandle: LongInt read GetSerialHandle;
    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnData: TSerialDataEvent read FOnData write SetOnData;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
    property OnError: TSerialErrorEvent read FOnError write SetOnError;
  end;


  { TArduinoThread }

  TArduinoThread = class(TSerialThread)
  protected
    procedure Execute; override;
  end;

  function GetSerialPortNames: string;


implementation

{$IFDEF MSWINDOWS}
uses
  Registry;
{$ENDIF}

operator = (const A, B: TMethod): Boolean; overload; inline;
begin
  Result := (A.Code = B.Code) and (A.Data = B.Data);
end;

function GetSerialPortNames: string;
{$IFDEF MSWINDOWS}
var
  reg: TRegistry;
  l, v: TStringList;
  n: integer;
begin
  l := TStringList.Create;
  v := TStringList.Create;
  reg := TRegistry.Create;
  try
    reg.Access := KEY_READ;
    reg.RootKey := HKEY_LOCAL_MACHINE;
    reg.OpenKey('\HARDWARE\DEVICEMAP\SERIALCOMM', false);
    reg.GetValueNames(l);
    for n := 0 to l.Count - 1 do
      v.Add(PChar(reg.ReadString(l[n])));
    Result := v.CommaText;
  finally
    reg.Free;
    l.Free;
    v.Free;
  end;
end;
{$ELSE}
  function Check(AMask: String): String;
  begin
  var
    sr : TSearchRec;
  begin
    Result := '';
    if FindFirst(AMask, $FFFFFFFF, sr) = 0 then
    repeat
      if (sr.Attr and $FFFFFFFF) = Sr.Attr then
        Result := Result + '/dev/' + sr.Name + ',';
    until FindNext(sr) <> 0;
    FindClose(sr);
  end;

begin
  Result := Check('/dev/ttyS*') + Check('/dev/ttyUSB*') + Check('/dev/ttyAM*');
  if Result <> '' then
    Delete(Result, Length(Result), 1);
end;
{$ENDIF}


{ TSerialThread }

constructor TSerialThread.Create(CreateSuspended: Boolean;
  const StackSize: SizeUInt = DefaultStackSize);
begin
  inherited Create(CreateSuspended, StackSize);
  FTimeout := 500;
end;

destructor TSerialThread.Destroy;
begin
  SerSync(FSerialhandle); { flush out any remaining before closure }
  SerFlushOutput(FSerialhandle); { discard any remaining output }
  SerClose(FSerialhandle);

  BufferSize := 0;
  inherited Destroy;
end;

procedure TSerialThread.DoDataAvail;
begin
  if Assigned(FOnData) then
    FOnData(self, FBuffer);
end;

procedure TSerialThread.DoError;
begin
  if Assigned(FOnError) then
    FOnError(self, FStatus);
end;

function TSerialThread.GetBufferSize: Integer;
begin
  result := Length(FBuffer);
end;

procedure TSerialThread.SetBufferSize(AValue: Integer);
begin
  SetLength(FBuffer, AValue);
end;

function TSerialThread.SetPortParams(const APort:String; ABaudRate, ADataBits, AStopBits: Integer;
  AParity: TParityType): Boolean;
var
  flags: TSerialFlags = [];
begin
  FSerialHandle := SerOpen(APort);
  if FSerialHandle <> 0 then
  begin
    SerSetParams(FSerialHandle, ABaudRate, ADataBits, AParity, AStopBits, flags);
    Result := true;
  end else
    Result := false;
end;


{ TSerialDevice }

constructor TSerialDevice.Create(AThreadClass: TSerialThreadClass);
begin
  inherited Create;
  FThreadClass := AThreadClass;
end;

destructor TSerialDevice.Destroy;
begin
  if FThread <> nil then
    FThread.Terminate;
  inherited Destroy;
end;

procedure TSerialDevice.Connect(APort:String; ABaudRate, ADataBits: Integer;
  AParity: TParityType; AStopBits: Integer; ATimeOut: Integer);
var
  status: boolean;
begin
  if FThread <> nil then
    FThread.Terminate;

  FThread := FThreadClass.Create(true);
  FThread.FreeOnTerminate := true;
  FThread.OnData := FOnData;
  FThread.OnError := FOnError;
  FThread.OnTerminate := @ThreadTerminateHandler;
  FThread.TimeOut := ATimeOut;
  status := FThread.SetPortParams(APort, ABaudRate, ADataBits, AStopBits, AParity);

  if status then
  begin
    if Assigned(FOnConnect) then
      FOnConnect(self);
    FThread.Start;
  end else
  begin
    if Assigned(FOnError) then
      FOnError(self, 1);
  end;
end;

function TSerialDevice.Connected: Boolean;
begin
  Result := FThread <> nil;
end;

procedure TSerialDevice.Disconnect;
begin
  if FThread <> nil then begin
    FThread.Terminate;
    if Assigned(FOnDisconnect) then
      FOnDisconnect(self);
  end;
end;

function TSerialDevice.GetSerialHandle: LongInt;
begin
  if Assigned(FThread) then
    Result := FThread.SerialHandle
  else
    Result := 0;
end;

procedure TSerialDevice.SetOnData(AValue: TSerialDataEvent);
begin
  if TMethod(FOnData) = TMethod(AValue) then exit;
  FOnData := AValue;
  if Assigned(FThread) then FThread.OnData := AValue;;
end;

procedure TSerialDevice.SetOnError(AValue: TSerialErrorEvent);
begin
  if TMethod(FOnError) = TMethod(AValue) then exit;
  FOnError := AValue;
  if Assigned(FThread) then FThread.OnError := AValue;
end;

procedure TSerialDevice.ThreadTerminateHandler(Sender: TObject);
begin
  FThread := nil;
end;

procedure TSerialDevice.Transmit(var ABuffer; ABufferSize: Integer);
begin
  if Connected then
    SerWrite(FThread.SerialHandle, ABuffer, ABufferSize);
end;


{ TArduinoThread }

procedure TArduinoThread.Execute;
type
  TBuffer = string[255];
const
  TERMINATOR = #13#10;
var
  i: Integer;
  n: Integer;
  buf: TBuffer;
begin
  buf := Default(TBuffer);
  while not Terminated do
  begin
    n := SerRead(SerialHandle, buf[1], 256);
    if n > 0 then
    begin
      SetLength(buf, n);
      i := Length(FBuffer);
      FBuffer := FBuffer + buf;
    end;
    i := pos(TERMINATOR, FBuffer);
    if i > 0 then
    begin
      buf := Copy(FBuffer, i + Length(TERMINATOR), MaxInt);
      SetLength(FBuffer, i - 1);
      Synchronize(@DoDataAvail);
      FBuffer := buf;
    end;
  end;
end;


end.


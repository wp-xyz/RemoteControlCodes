unit rccCodes;

{$MODE objfpc}{$H+}
{.$DEFINE DEBUG_WAV_WRITER}

interface

uses
  Classes, Contnrs, SysUtils, rccGlobal;

type
  TRemoteControlCode = class
    KeyName: String;
    CodeType: TCodeType;
    Code: LongInt;
    Bits: Byte;
    Raw: TMarkSpaceArray;
    constructor Create(AKeyName: String);
    procedure Assign(RCCode: TRemoteControlCode);
    function BitsToString: String;
    function CodeToString: String;
    function CodeTypeName: String;
    class function CodeTypeName(ACodeType: TCodeType): String;
    procedure Clear;
    function IsEmpty: Boolean;
    function RawToString: String;
    procedure SaveCodeAsWAV(const AFileName: String); overload;
    procedure SaveCodeAsWAV(const AStream: TStream); overload;
    procedure SetCodeTypeFromString(const AString: String);
    function StringToRaw(const AString: String): TMarkSpaceArray;
  end;

  TRemoteControlCodeList = class(TFPObjectList)
  private
    function GetItem(AIndex: Integer): TRemoteControlCode;
    procedure SetItem(AIndex: Integer; const AValue: TRemoteControlCode);
  public
    procedure ClearCodes;
    function IsEmpty: Boolean;
    procedure ListKeys(AList: TStrings);
    procedure PopulateFromKeyList(AList: TStrings);
    procedure SaveCodesAsWav(ADirectory: String);
    property Items[AIndex: Integer]: TRemoteControlCode read GetItem write SetItem; default;
  end;


implementation

uses
  TypInfo, LazFileUtils,
  fpWavFormat, fpWavWriter;

{ TRemoteControlCode }

constructor TRemoteControlCode.Create(AKeyName: String);
begin
  KeyName := AKeyName;
  CodeType := ctUNUSED;
  Code := 0;
  Bits := 0;
  SetLength(Raw, 0);
end;

procedure TRemoteControlCode.Assign(RCCode: TRemoteControlCode);
begin
   KeyName := RCCode.KeyName;
   CodeType := RCCode.CodeType;
   Code := RCCode.Code;
   Bits := RCCode.Bits;
   SetLength(Raw, Length(RCCode.Raw));
   Move(RCCode.Raw[0], Raw[0], Length(RCCode.Raw)*SizeOf(word));
end;

function TRemoteControlCode.BitsToString: String;
begin
  if Bits = 0 then
    Result := ''
  else
    Result := IntToStr(Bits);
end;

procedure TRemoteControlCode.Clear;
begin
   CodeType := ctUNUSED;
   Bits := 0;
   SetLength(Raw, 0);
end;

function TRemoteControlCode.CodeToString: String;
begin
  if CodeType = ctUNUSED then
    Result := ''
  else
    Result := Format(CodeFormatMask, [Code]);
end;

function TRemoteControlCode.CodeTypeName: String;
begin
  Result := CodeTypeName(CodeType);
end;

class function TRemoteControlCode.CodeTypeName(ACodeType: TCodeType): String;
begin
  if ACodeType = ctUnUsed then
    Result := ''
  else
  begin
    Result := GetEnumName(TypeInfo(TCodeType), integer(ACodeType));
    Delete(Result, 1, 2);
  end;
end;

function TRemoteControlCode.IsEmpty: Boolean;
begin
   Result := (CodeType = ctUNUSED) and (Bits <= 0) and (Length(Raw) = 0);
end;

function TRemoteControlCode.RawToString: String;
const
  MARK_SPACE:array[0..1] of String[1] = ('m', 's');
var
  i: Integer;
begin
   if (Raw = nil) or (Length(Raw) = 0) then
     Result := ''
   else
   begin
     Result := Format('m%d', [Raw[0]]);
     for i := 1 to High(Raw) do
       Result := Format('%s %s%d', [Result, MARK_SPACE[i mod 2], Raw[i]]);
   end;
end;

procedure TRemoteControlCode.SaveCodeAsWAV(const AFileName: String);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(AFileName, fmCreate + fmShareDenyNone);
  try
    SaveCodeAsWAV(stream);
  finally
    stream.Free;
  end;
end;

procedure TRemoteControlCode.SaveCodeAsWAV(const AStream: TStream);
const
  SAMPLE_RATE = 44100;  // Hz
  MODULATION_FREQUENCY = 38000.0;  // Hz
  MAX_SIGNAL = $7FFF;
  {$IFDEF DEBUG_WAV_WRITER}
  SIGNAL_FREQUENCY = 1000.0;  // Hz
  SIGNAL_DURATION = 1.0;     // s
  {$ENDIF}
var
  writer: TWavWriter;
  t, dt: double;
  tRaw: Double;
  i: Integer;
  isMark: Boolean;
  buf: Int16;
  done: Boolean;
begin
  //dt := 2.0 / SAMPLE_RATE * 1E3;  // ms   // We get two samples per time step
  dt := 1.0 / SAMPLE_RATE;   // s;  we get two samples per time step
  writer := TWavWriter.Create;
  try
    with writer do
    begin
      StoreToStream(AStream);
      fmt.ChunkHeader.ID := AUDIO_CHUNK_ID_fmt;
      fmt.Format := AUDIO_FORMAT_PCM;
      fmt.Channels := 2;
      fmt.SampleRate := SAMPLE_RATE;
      fmt.BitsPerSample := 16;
      fmt.BlockAlign := fmt.Channels * fmt.BitsPerSample div 8;
      fmt.ByteRate := fmt.Channels * fmt.SampleRate * fmt.BitsPerSample div 8;

      t := 0.0;
      done := false;
      i := 0;
      {$IFDEF DEBUG_WAV_WRITER}
      while not done do begin
        buf := round(MAX_SIGNAL * sin(t * TWO_PI * SIGNAL_FREQUENCY));
        WriteBuf(buf, SizeOf(buf));
        buf := round(-MAX_SIGNAL * sin(t * TWO_PI * SIGNAL_FREQUENCY));
        WriteBuf(buf, SizeOf(buf));
        t := t + dt;
        if t > SIGNAL_DURATION then
          done := true;
      end;
      {$ELSE}
      isMark := true;
      tRaw := Raw[0] * 1E-6;  // Raw is in µs, we need seconds.
      while not done do begin
        if isMark then
        begin
          // We will modulate the signal by the half of the 38 kHz (19 kHz)
          // in order to move the signal into the frequency range of the line
          // output.
          // The two channels will have the same signal, but in anti-phase.
          // Thus, LED1 will emit when channel 1 is positive, and LED2 will
          // emit when channel 2 is positive.
          // This way the emitted light modulation will have double frequency
          // of the modulation applied here, i.e. 38 kHz as needed by the
          // TV's IR detector.

          // channel 1
          buf := round(MAX_SIGNAL * sin(t * pi*MODULATION_FREQUENCY));  // 19 kHz
          WriteBuf(buf, SizeOf(buf));

          // channel 2
          buf := round(-MAX_SIGNAL * sin(t * pi*MODULATION_FREQUENCY));  // Note the '-'
          WriteBuf(buf, SizeOf(buf));
        end else
        begin
          // Space --> no signal
          buf := 0;
          // channel 1
          WriteBuf(buf, SizeOf(buf));
          // channel 2
          WriteBuf(buf, Sizeof(buf));
        end;

        t := t + dt;
        if t > tRaw then
        begin
          isMark := not isMark;
          inc(i);
          if i = Length(Raw) then
            isMark := not isMark;
          if i < Length(Raw) then
            tRaw := tRaw + Raw[i] * 1E-6
          else
            done := true;
        end;
      end;
      {$ENDIF}
    end;
  finally
    writer.Free;
  end;
end;

procedure TRemoteControlCode.SetCodeTypeFromString(const AString: String);
var
  ct: TCodeType;
begin
   for ct in TCodeType do
   begin
     if CodeTypeName(ct) = AString then begin
       CodeType := ct;
       exit;
     end;
   end;
end;

function TRemoteControlCode.StringToRaw(const AString: String): TMarkSpaceArray;
var
  L: TStrings;
  s: String;
  i: Integer;
begin
  SetLength(Result, 0);
  if AString = '' then
    exit;

  L := TStringList.Create;
  try
    L.Delimiter := ' ';
    L.DelimitedText := AString;
    if L[0] = '' then L.Delete(0);
    SetLength(Result, L.Count);
    for i := 0 to L.Count - 1 do
    begin
      s := L[i];
      Delete(s, 1, 1);  // remove leading 'm'/'s'
      Result[i] := StrToInt(s);
    end;
  finally
    L.Free;
  end;
end;


{ TRemoteControlCodeList }

procedure TRemoteControlCodeList.ClearCodes;
var
  i: Integer;
begin
   for i := 0 to Count-1 do
     Items[i].Clear;
end;

function TRemoteControlCodeList.GetItem(AIndex: Integer): TRemoteControlCode;
begin
  Result := TRemoteControlCode(inherited Items[AIndex]);
end;

function TRemoteControlCodeList.IsEmpty: Boolean;
var
  i: Integer;
begin
  Result := false;
  for i := 0 to Count-1 do
    if (Items[i].KeyName <> '') and (Length(Items[i].Raw) > 0) then
      exit;
  Result := true;
end;

procedure TRemoteControlCodeList.ListKeys(AList: TStrings);
var
  i: Integer;
begin
  Assert(AList <> nil);
  AList.Clear;
  AList.BeginUpdate;
  try
    for i:=0 to Count-1 do
      AList.Add(Items[i].KeyName);
  finally
    AList.EndUpdate;
  end;
end;

procedure TRemoteControlCodeList.PopulateFromKeyList(AList: TStrings);
var
  i: Integer;
begin
  Assert(AList <> nil);
  Clear;
  for i:=0 to AList.Count - 1 do begin
    Add(TRemoteControlCode.Create(AList[i]));
  end;
end;

procedure TRemoteControlCodeList.SaveCodesAsWav(ADirectory: String);
var
  i, j: Integer;
  fn: String;
begin
  for i := 0 to Count-1 do
    if (Items[i].KeyName <> '') and (Length(Items[i].Raw) > 0) then
    begin
      fn := Items[i].KeyName;
      for j := Length(fn) downto 1 do
        case fn[j] of
          ':', '/', '\': fn[j] := '_';
          '+': begin
                 System.Delete(fn, j, 1);
                 System.Insert('plus', fn, j);
               end;
          '-': begin
                 System.Delete(fn, j, 1);
                 System.Insert('minus', fn, j);
               end;
        end;
      Items[i].SaveCodeAsWav(AppendPathDelim(ADirectory) + fn + '.wav');
      {$IFDEF DEBUG_WAV_WRITER}
      break;
      {$ENDIF}
    end;
end;

procedure TRemoteControlCodeList.SetItem(AIndex: Integer;
  const AValue: TRemoteControlCode);
begin
  TRemoteControlCode(inherited Items[AIndex]).Assign(AValue);
end;

end.


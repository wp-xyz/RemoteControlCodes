unit rccCodes;

{$mode objfpc}{$H+}

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
    procedure SetCodeTypeFromString(const AString: String);
    function StringToRaw(const AString: String): TMarkSpaceArray;
  end;

  TRemoteControlCodeList = class(TFPObjectList)
  private
    function GetItem(AIndex: Integer): TRemoteControlCode;
    procedure SetItem(AIndex: Integer; const AValue: TRemoteControlCode);
  public
    procedure ClearCodes;
    procedure ListKeys(AList: TStrings);
    procedure PopulateFromKeyList(AList: TStrings);
    property Items[AIndex: Integer]: TRemoteControlCode read GetItem write SetItem; default;
  end;


implementation

uses
  TypInfo;

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
   if Length(Raw) = 0 then
     Result := ''
   else
   begin
     Result := Format('m%d', [Raw[0]]);
     for i := 1 to High(Raw) do
       Result := Format('%s %s%d', [Result, MARK_SPACE[i mod 2], Raw[i]]);
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

procedure TRemoteControlCodeList.SetItem(AIndex: Integer;
  const AValue: TRemoteControlCode);
begin
  TRemoteControlCode(inherited Items[AIndex]).Assign(AValue);
end;

end.


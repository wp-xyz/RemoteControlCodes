unit rccUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

function CalcIniFileName: String;
function CreateAppIni: TCustomIniFile;
//function StopBitsToStr(ASynaserValue: Integer): String;
//function StrToStopBits(AString: String): Integer;


implementation

function CalcIniFileName: String;
begin
  Result := ChangeFileExt(GetAppConfigFile(false), '.ini');
end;

function CreateAppIni: TCustomIniFile;
begin
  Result := TIniFile.Create(CalcIniFileName);
end;
  {
function StopBitsToStr(ASynaserValue: Integer): String;
begin
  case ASynaserValue of
    0: Result := '1';
    1: Result := '1.5';
    2: Result := '2';
  end;
end;

function StrToStopBits(AString: String): Integer;
begin
  case AString of
    '1'   : Result := 0;
    '1.5' : Result := 1;
    '2'   : Result := 2;
    else   raise Exception.Create('Unknown stopbit string');
  end;
end;
   }
end.


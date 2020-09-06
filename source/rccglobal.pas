unit rccGlobal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Serial;

type
  TCodeType = (
    ctUNKNOWN = -1, ctUNUSED = 0, ctRC5, ctRC6, ctNEC, ctSONY, ctPANASONIC,
    ctJVC, ctSAMSUNG, ctWHYNTER, ctAIWA_RC_T501, ctLG, ctSANYO, ctMITSUBISHI,
    ctDISH, ctSHARP, ctDENON, ctPRONTO
  );

  TMarkSpaceArray = array of word;

  TSerialParams = record
    Port: String;
    BaudRate: Integer;     // see synaser
    DataBits: Integer;     // 4, 5, 6, 7, 8
    StopBits: Integer;     // 1=1 stop bit, 2=2 stop bits -- no support of 1.5 stop bits
    Parity: TParityType;   
  end;

var
  SerialParams: TSerialParams = (
    Port: '';
    BaudRate: 9600;
    DataBits: 8;
    StopBits: 1;
    Parity: NoneParity;
  );

  CodeFormatMask: String = '$%x';

const
  APP_CAPTION = 'Remote Control Codes';
  RCF_ROOT = 'REMOTE_CONTROL';

  TWO_PI = 2.0 * pi;


implementation

end.


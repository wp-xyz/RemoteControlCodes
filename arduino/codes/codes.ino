#include <IRremote.h>
#include <IRremoteInt.h>

/*  Reads the codes of an IR remote control. 
 *
 *   Use the serial monitor to see the codes as hex value; displays also 
 *   the manufacturer.
 *   
 *   Requires a TSOP 4838 IR remove control receiver IC. 
 *   Output of the IR receiver (pin 1) is connected to digital input pin 7 (RECV_PIN).
 *   Sends the following information to the serial port:
 *     - manufacturer (SONY, NEC etc)
 *     - bit length of code
 *     - code, in hex
 */

const int LED_PIN = 12;        // pin for debugging LED
const int RECV_PIN = 7;        // any pin is allowed
//const int SEND_PIN = 3;      // hard-coded in IRremote library for ATMega328 --> change for others
const int BAUD_RATE = 9600;   // Baudrate used for serial communication
const char SEPARATOR = ';';
const char START_CHAR = '!';
const int DEBUG_MODE = 0;

int codeLen;                    // length of the code
unsigned int rawCodes[RAWBUF];  // buffer for mark/space durations

IRsend irsend;
IRrecv irrecv(RECV_PIN);
decode_results results;

//-------------------------------------------------------------------------
//  Arduino setup
//-------------------------------------------------------------------------
void setup(){
  pinMode(RECV_PIN, INPUT);
  pinMode(LED_PIN, OUTPUT);
  Serial.begin(BAUD_RATE);
  while (!Serial) {
    ; // wait for serial port to connect. Needed for native USB only
  }  
  Serial.setTimeout(1);
  irrecv.enableIRIn();
}

/*
//-------------------------------------------------------------------------
// prints 8-bit data in hex with leading zeroes
//-------------------------------------------------------------------------
void PrintHex(uint8_t *data, uint8_t len) 
{
  Serial.print("0x");
  for (int i=0; i<len; i++) {
    if (data[i]<0x10) {
      Serial.print("0");
    }
    Serial.print(data[i], HEX);
    Serial.print(" ");
  }
}
*/

//-------------------------------------------------------------------------
// Send IR code received from serial port to device. 
//-------------------------------------------------------------------------
byte send_code_type = -1;
byte send_bits = 0;
unsigned long send_code = 0;

void process_send() { 
  delay(10);                             // wait for all data to arrive
  send_code_type = Serial.parseInt();    // the first integer received is the code_type (SONY, RC5, etc)

  if (DEBUG_MODE == 1) {
    Serial.print("code_type ");
    Serial.println(send_code_type);
  }
 
  if (send_code_type == byte(UNKNOWN)) {
    codeLen = Serial.parseInt();

    if (DEBUG_MODE == 1) {
      Serial.print("codeLen");
      Serial.println(codeLen);
    }
    
    for (int i=0; i<codeLen; i++) {
      if (i % 8) {
        delay(10);
      }
      rawCodes[i] = Serial.parseInt();
    }
    irsend.sendRaw(rawCodes, codeLen, 38);  // Assume 38 kHz
  } else {
    send_code = Serial.parseInt();
    send_bits = Serial.parseInt();

    if (DEBUG_MODE == 1) {
      Serial.print("code_bits ");
      Serial.println(send_bits);
      Serial.print("code ");
      Serial.print(send_code, HEX);
      Serial.println("");
    }

    // send code three times to IR controller
    for (int i=1; i<=3; i++) {
      switch (send_code_type) {
        case RC5:
          irsend.sendRC5(send_code, send_bits);
          break;
        case RC6:
          irsend.sendRC6(send_code, send_bits);
          break;
        case NEC:
          irsend.sendNEC(send_code, send_bits);
          break;
        case SONY:
          irsend.sendSony(send_code, send_bits);
          break;
        case PANASONIC:
          irsend.sendPanasonic(send_code, send_bits);
          break;
        case JVC:
          irsend.sendJVC(send_code, send_bits, true); 
          break;
        case SAMSUNG:
          irsend.sendSAMSUNG(send_code, send_bits);
          break;
        case WHYNTER:
          irsend.sendWhynter(send_code, send_bits);
          break;
        case AIWA_RC_T501:
          irsend.sendAiwaRCT501(send_code); //, send_bits);
          break;
        case LG:
          irsend.sendLG(send_code, send_bits);
          break;
        //case SANYO:
        //  irsend.sendSANYO(send_code, send_bits);
        //  break;
        //case MITSUBISHI:
        //  irsend.sendMITSUBISHI(send_code, send_bits);
        //  break;
        case DISH:
          irsend.sendDISH(send_code, send_bits);
          break;
        case SHARP:
          irsend.sendSharpRaw(send_code, send_bits);
          break;
        case DENON:
          irsend.sendDenon(send_code, send_bits);
          break;
        //case PRONTO:
        //  irsend.sendPronto(send_code, send_bits);
        //  break;
        default:
          break;
      }   
    }
  }  

  delay(50);
  
  // Remove remaining separators for the serial buffer. The buffer must be empty.
  while (Serial.available() > 0) { 
    Serial.read();
  }
}

//---------------------------------------------------------------------
//  Writes decoded IR code to serial port
//---------------------------------------------------------------------
void process_receive(decode_results *results) {
  Serial.print(START_CHAR);
  switch (results->decode_type){
    case NEC: 
      Serial.print("NEC"); 
      break;
    case SONY: 
      Serial.print("SONY"); 
      break;
    case RC5: 
      Serial.print("RC5"); 
      break;
    case RC6: 
      Serial.print("RC6"); 
      break;
    case DISH: 
      Serial.print("DISH"); 
      break;
    case SHARP: 
      Serial.print("SHARP"); 
      break;
    case JVC: 
      Serial.print("JVC"); 
      break;
    case SANYO: 
      Serial.print("SANYO"); 
      break;
    case MITSUBISHI: 
      Serial.print("MITSUBISHI"); 
      break;
    case SAMSUNG: 
      Serial.print("SAMSUNG"); 
      break;
    case LG: 
      Serial.print("LG"); 
      break;
    case WHYNTER: 
      Serial.print("WHYNTER"); 
      break;
    case AIWA_RC_T501: 
      Serial.print("AIWA_RC_T501"); 
      break;
    case PANASONIC: 
      Serial.print("PANASONIC"); 
      break;
    case DENON: 
      Serial.print("DENON"); 
      break;
    default:
      case UNKNOWN:
        Serial.print("UNKNOWN");
        break;
  }    
   
  Serial.print(SEPARATOR);
  Serial.print(results->bits);
  Serial.print(SEPARATOR);
  Serial.print(results->value, HEX);
  Serial.print(SEPARATOR);

  // Send the raw space/mark times in microseconds to the serial port
  // To store raw codes:
  // - Drop first value (gap)
  // - Convert from ticks to microseconds
  // - Tweak marks shorter, and spaces longer to cancel out IR receiver distortion
  codeLen = results->rawlen - 1;
  for (int i = 1; i <= codeLen; i++) {
    if (i % 2) {
      // Mark
      rawCodes[i - 1] = results->rawbuf[i]*USECPERTICK - MARK_EXCESS;
      Serial.print(" m");
    } 
    else {
      // Space
      rawCodes[i - 1] = results->rawbuf[i]*USECPERTICK + MARK_EXCESS;
      Serial.print(" s");
    }
    Serial.print(rawCodes[i - 1], DEC);
  }
  Serial.println("");
}

//-------------------------------------------------------------------
//                         Arduino loop
//-------------------------------------------------------------------
void loop(){
  // send IO pulses only when there are data in serial buffer
  if (Serial.available() > 0) {
    process_send();
    irrecv.enableIRIn();  // Re-enable receiver
  }  

  // send received IR codes to serial port
  if (irrecv.decode(&results)) {
    process_receive(&results);  // send code to serial
    irrecv.resume();            // resume receiver
  }
}

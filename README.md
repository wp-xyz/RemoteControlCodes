# RemoteControlCodes
This application stores the codes sent by an IR remote control to electronic devices (TV, VCR etc). It cooperates with an Arduino microcontroller which receives and decodes the IR signal using a TSOP4838 IR receiver. Code information is communicated to the program by the serial port.

In addition, the application can send the stored codes to the electronic device using an IR LED connected to a digitial output of the Arduino.

## Setting up the Arduino ##
Needed parts:
* Arduino microcontroller (Arduino Nano)
* TSOP4838 infra-red receiver
* Infrared LED
* 220 ohms resistor

![Circuit
](https://github.com/wp-xyz/RemoteControlCodes/blob/master/arduino/arduino%20circuit/circuit.png)

Connect the Arduino to the PC via an USB cable and load the `codes.ino` sketch (in folder `arduino/codes`) to the Arduino.

## Usage of the program
* Open "Codes" > "Set up remote control". Define the name of the remote control and enter the labels of the remote control key
for which you want to store the IR codes. After pressing "OK" each key name gets a row in a grid of the main form.
* Active the serial port by pressing the "Connect" button.
* Select the key in the first grid row. Point the remote control towards the IR receiver on the Arduino and press the first key. The code will be added to other cells in the corresponding grid row.
 * column "Code": hexadecimal value of the code received from the IR remote control.
 * column "Type: manufacturer
 * column "Bits": number of bits used on the code. 
 * column "Raw": list of the mark and space times of the pulses sent by the IR remote control. Times are in microseconds. When "Code" > "Plot marks/spaces" is checked a plot of the pulse sequence is shown at the bottom of the window.
* After the code has been received the next row in the grid is highlightes. Press on the associated remote control key to continue.
* The button "Send" creates the original pulse sequence received and sends it to the IR LED attached the Arduino board. This way the Arduino can be used as a "spare" remote control.

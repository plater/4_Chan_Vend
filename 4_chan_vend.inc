    LIST

;Header File For 4 Channel Vending program utilized for biltong vending
;Defines for inputs, outputs and display. PIC 18F452
    NOLIST

    IFNDEF __18F4520
    MESSG "Processor-header file mismatch.  Verify selected processor."
    ENDIF
;******************************************************************************
;Initialization bits
;-------------------------------------------------------------------------------
initializeA	    equ	0xFF	;All bits input
adoff		    equ	0x06	;All A/D inputs digital
adsensor	    equ	0x0E	;If vend sense needs A/D

;EEPROM addresses and constants
;-------------------------------------------------------------------------------
readee		equ	0x01	    ;EECON1 setup for read
price1		equ	0x00	    ;R0 to R255, 3 bcd digits
price2		equ	0x01	    ;R0 to R255, 3 bcd digits
price3		equ	0x02	    ;R0 to R255, 3 bcd digits
price4		equ	0x03	    ;R0 to R255, 3 bcd digits
hopvalue	equ	0x04	    ;Hopper Coin Value R1,R2,R5 or R10
coinvalue	equ	0x05	    ;Value added for every coin pulse
venderr		equ	0x06	    ;When bit is high disable motor
coinsin		equ	0x07	    ;lsb of Rands in
coinsin1	equ	0x08	    ;msb of Rands in
coinsin2	equ	0x09	    ;Upper byte of Rands in
coinsinv	equ	0x0A	    ;lsb of clearable Rands in
coinsinv1	equ	0x0B	    ;msb of clearable Rands in
coinsout	equ	0x0C	    ;Coins out of hopper low byte
coinsout1	equ	0x0D	    ;Coins out of hopper high byte
vends1		equ	0x0E	    ;Total vends chan 1 up to 255 vends
vends2		equ	0x0F	    ;Total vends chan 2
vends3		equ	0x10	    ;Total vends chan 3
vends4		equ	0x11	    ;Total vends chan 4

;Coin Mech Inputs on RA + Coin mech enable on RC7
;------------------------------------------------------------------------------
cmenable	    equ	0x80	;Coin mech enable on RC7 low = disabled
coinpulse	    equ	0x10	;Coin and note reader bit RA4
optics		    equ	0x01	;Vend out IR sensor bit RA0
service		    equ	0x02	;Service switch bit RA1
hopperin	    equ	0x08	;Hopper coin out switch RA3
sparecmi	    equ	0x20	;Spare input (or output) on coin mech plug
spareRE0	    equ	0x01	;Spare RE0 line? also /RD line for PSP bus
opticon		    equ 0x80	;LED on RD7 Not used.

;Vend motors RC0 to RC3,RC6
;-------------------------------------------------------------------------------
vend1		    equ 0x01	;Vend motor one on RC0
vend2		    equ 0x02	;Vend motor two on RC1
vend3		    equ 0x04	;Vend motor three on RC2
vend4		    equ 0x08	;Vend motor four on RC3
hopper		    equ	0x40	;Hopper motor on RC6

;Button Inputs on RC4/5 and RD2/3
;-------------------------------------------------------------------------------
button1		    equ	0x04	;Button one on RD2 - motor RC2
button2		    equ	0x08	;Button two on RD3 - motor RC3
button3		    equ	0x10	;Button three on RC4 - motor RC0
button4		    equ	0x20	;Button four on RC5 - motor RC1
buttonon	    equ	0x40	;Button pullup on RC6

;Button lights on RD0/1 and RD4/5
;-------------------------------------------------------------------------------
butlight1	    equ	0x01	;Button light one on RD0
butlight2	    equ	0x02	;Button light two on RD1
butlight3	    equ	0x10	;Button light three on RD4
butlight4	    equ	0x20	;Button light four on RD5

;Display defines, RE1 = read, RE2 = RS, RB4 = E (enable) RB0 to RB3 db
;-------------------------------------------------------------------------------
portbread	    equ	0xEF	;Set port b to read
portbwrite	    equ	0xE0	;Set port b to write
dispinsw	    equ	0x00	;Write instruction
dispinsr	    equ	0x02	;Read address and busy flag
dispdatw	    equ	0x04	;Write db
dispdatr	    equ	0x06	;Read db
dispclr		    equ	0x01	;Write 20h to entire display
dispaddr	    equ	0xA8	;Upper 8 digit address
dsphome		    equ	0x02	;Select leftmost digit
dispnormal	    equ	0x06	;Static, increment address on write/read
dispshright	    equ 0x05	;Shift display right, increment address on write/read
dispshleft	    equ 0x07	;Shift display left, increment address on write/read
dispon		    equ 0x0C	;Display on, cursor off and blink off
dispoff		    equ 0x08	;Display off
dispsr		    equ 0x1B	;Display shift right
dispsl		    equ 0x18	;Display shift left
dispfunc	    equ 0x20	;4 bit opperation, single line, 5x8 character

dispdig1	    equ 0x80	;Address display digit 1
dispdig2	    equ 0x81	;Address display digit 2
dispdig3	    equ 0x82	;Address display digit 3
dispdig4	    equ 0x83	;Address display digit 4
dispdig5	    equ 0x84	;Address display digit 5
dispdig6	    equ 0x85	;Address display digit 6
dispdig7	    equ 0x86	;Address display digit 7
dispdig8	    equ 0x87	;Address display digit 8
dispdig9	    equ 0xA8	;Address display digit 9
dispdig10	    equ 0xA9	;Address display digit 10
dispdig11	    equ 0xAA	;Address display digit 11
dispdig12	    equ 0xAB	;Address display digit 12
dispdig13	    equ 0xAC	;Address display digit 13
dispdig14	    equ 0xAD	;Address display digit 14
dispdig15	    equ 0xAE	;Address display digit 15
dispdig16	    equ 0xAF	;Address display digit 16

;Values for multybyte hex conversion
tenmil		    equ	0x80
tenmil1		    equ	0x96
tenmil2		    equ	0x98
onemil		    equ	0x40
onemil1		    equ	0x42
onemil2		    equ	0x0F
hundthow	    equ	0xA0
hundthow1	    equ	0x86
hundthow2	    equ	0x01
tenthow		    equ	0x10
tenthow1	    equ	0x27
thousand	    equ	0xE8
thousand1	    equ	0x03
hundred		    equ	0x64

;Display string addresses, update from listing file
Testing		    equ 0x10
Testingl	    equ 0x00
Insertcoin	    equ	0x10
Insertcoinl	    equ 0x24
Credit		    equ	0x10
Creditl		    equ 0x40
Pushlitbutton	    equ	0x10
Pushlitbuttonl	    equ 0x54
Tovend		    equ	0x10
Tovendl		    equ 0x76
Auditmode	    equ	0x10
Auditmodel	    equ 0x8E
Price		    equ	0x10
Pricel		    equ 0xAC
Vends		    equ	0x10
Vendsl		    equ 0xBE
Cash		    equ	0x10
Cashl		    equ 0xCE
Testmode	    equ	0x11
Testmodel	    equ 0x00
Vendtest	    equ	0x11
Vendtestl	    equ 0x16
Coinnotetest	    equ	0x11
Coinnotetestl	    equ 0x2E
Outofservice	    equ 0x11
Outofservicel	    equ	0x50
Nochange	    equ	0x11
Nochangel	    equ	0x72
Buyagain	    equ	0x11
Buyagainl	    equ	0x94
Cleartotals	    equ	0x11
Cleartotalsl	    equ 0xB2
Sensortest	    equ	0x11
Sensortestl	    equ	0xD0
Pleasewait	    equ	0x12
Pleasewaitl	    equ	0x00
Setprice	    equ	0x12
Setpricel	    equ	0x1C
Empty		    equ	0x12
Emptyl		    equ	0x3A
Audit		    equ	0x12
Auditl		    equ	0x5A
Totals		    equ	0x12
Totalsl		    equ	0x78
Hopvalued	    equ	0x12
Hopvaluedl	    equ	0x9A
Coinout		    equ	0x12
Coinoutl	    equ	0xB8

;Timer values
;-------------------------------------------------------------------------------
timeronm	    equ	0x08	;Bit 7 is the enable no prescale 32, 16 bit mode T0CON
timeronh	    equ	0x07	;Bit 7 is the enable bit prescale 256, 16 bit mode T0CON
timeron2	    equ	0x05	;Bit 7 is the enable bit prescale 64, 16 bit mode T0CON
timeron		    equ	0x04	;Bit 7 is the enable bit prescale 32, 16 bit mode T0CON
timeron4	    equ	0x01	;Prescale 4 16 bit mode
inttimer	    equ	0xA4	;Enable timer 0 interupt INTCON
fortyms		    equ	0x62	;Load timer for 40mSec
sec1.6		    equ	0x3C	;1.6Sec preset for timer
sec1.6l		    equ	0xB0


;Memory defines
;    CBLOCK	    0x90
;      loopr
;   ENDC

    LIST



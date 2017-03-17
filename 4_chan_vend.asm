;******************************************************************************
;                                                                             *
;******************************************************************************
;                                                                             *
;    Filename:      4_chan_vend.ASM                                           *
;    Date:          08/05/2016                                                *
;    File Version:  0.1                                                       *
;                                                                             *
;    Author:        Dave Plater                                               *
;    Copyright:     Dave Plater 17 March 2017                                 *
;                                                                             *
;******************************************************************************
;                                                                             *
;    Files Required: P18F4520.INC 4_chan_vend.inc                             *
;                                                                             *
;******************************************************************************
    list c=200
    LIST P=18F4520		;directive to define processor
        #include "4_chan_vend.inc"
	#include "p18f4520.inc"		;processor specific variable definitions
	errorlevel 0
;******************************************************************************
; PIC18F4520 Configuration Bit Settings

; ASM source line config statements



; CONFIG1H
  CONFIG  OSC = XT              ; Oscillator Selection bits (XT oscillator)
  CONFIG  FCMEN = OFF           ; Fail-Safe Clock Monitor Enable bit (Fail-Safe Clock Monitor disabled)
  CONFIG  IESO = OFF            ; Internal/External Oscillator Switchover bit (Oscillator Switchover mode disabled)

; CONFIG2L
  CONFIG  PWRT = ON             ; Power-up Timer Enable bit (PWRT enabled)
  CONFIG  BOREN = ON            ; Brown-out Reset Enable bits (Brown-out Reset enabled and controlled by software (SBOREN is enabled))
  CONFIG  BORV = 3              ; Brown Out Reset Voltage bits (Minimum setting)

; CONFIG2H
  CONFIG  WDT = OFF             ; Watchdog Timer Enable bit (WDT disabled (control is placed on the SWDTEN bit))
  CONFIG  WDTPS = 32768         ; Watchdog Timer Postscale Select bits (1:32768)

; CONFIG3H
  CONFIG  CCP2MX = PORTC        ; CCP2 MUX bit (CCP2 input/output is multiplexed with RC1)
  CONFIG  PBADEN = OFF          ; PORTB A/D Enable bit (PORTB<4:0> pins are configured as digital I/O on Reset)
  CONFIG  LPT1OSC = OFF         ; Low-Power Timer1 Oscillator Enable bit (Timer1 configured for higher power operation)
  CONFIG  MCLRE = ON            ; MCLR Pin Enable bit (MCLR pin enabled; RE3 input pin disabled)

; CONFIG4L
  CONFIG  STVREN = ON           ; Stack Full/Underflow Reset Enable bit (Stack full/underflow will cause Reset)
  CONFIG  LVP = OFF             ; Single-Supply ICSP Enable bit (Single-Supply ICSP disabled)
  CONFIG  XINST = OFF           ; Extended Instruction Set Enable bit (Instruction set extension and Indexed Addressing mode disabled (Legacy mode))

; CONFIG5L
  CONFIG  CP0 = OFF             ; Code Protection bit (Block 0 (000800-001FFFh) not code-protected)
  CONFIG  CP1 = OFF             ; Code Protection bit (Block 1 (002000-003FFFh) not code-protected)
  CONFIG  CP2 = OFF             ; Code Protection bit (Block 2 (004000-005FFFh) not code-protected)
  CONFIG  CP3 = OFF             ; Code Protection bit (Block 3 (006000-007FFFh) not code-protected)

; CONFIG5H
  CONFIG  CPB = OFF             ; Boot Block Code Protection bit (Boot block (000000-0007FFh) not code-protected)
  CONFIG  CPD = OFF             ; Data EEPROM Code Protection bit (Data EEPROM not code-protected)

; CONFIG6L
  CONFIG  WRT0 = OFF            ; Write Protection bit (Block 0 (000800-001FFFh) not write-protected)
  CONFIG  WRT1 = OFF            ; Write Protection bit (Block 1 (002000-003FFFh) not write-protected)
  CONFIG  WRT2 = OFF            ; Write Protection bit (Block 2 (004000-005FFFh) not write-protected)
  CONFIG  WRT3 = OFF            ; Write Protection bit (Block 3 (006000-007FFFh) not write-protected)

; CONFIG6H
  CONFIG  WRTC = OFF            ; Configuration Register Write Protection bit (Configuration registers (300000-3000FFh) not write-protected)
  CONFIG  WRTB = OFF            ; Boot Block Write Protection bit (Boot block (000000-0007FFh) not write-protected)
  CONFIG  WRTD = OFF            ; Data EEPROM Write Protection bit (Data EEPROM not write-protected)

; CONFIG7L
  CONFIG  EBTR0 = OFF           ; Table Read Protection bit (Block 0 (000800-001FFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR1 = OFF           ; Table Read Protection bit (Block 1 (002000-003FFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR2 = OFF           ; Table Read Protection bit (Block 2 (004000-005FFFh) not protected from table reads executed in other blocks)
  CONFIG  EBTR3 = OFF           ; Table Read Protection bit (Block 3 (006000-007FFFh) not protected from table reads executed in other blocks)

; CONFIG7H
  CONFIG  EBTRB = OFF           ; Boot Block Table Read Protection bit (Boot block (000000-0007FFh) not protected from table reads executed in other blocks)


;
;******************************************************************************
;EEPROM data
; Data to be programmed into the Data EEPROM is defined here

		ORG	0xf00000
		data	0x0505		;Price2 - Price1
		data	0x0505		;Price4 - Price3
		data	0x0105		;Coin pulse - Hopper coin value
		data	0x0000		;Coins in byte 0 - Venderr
		data	0x0000		;Coins in byte 2 - Coins in byte 1
		data	0x0000		;Coinsr in byte 1 - Coinsr in byte 0
		data	0x0000		;Coins out1 - Coins out0
		data	0x0000		;vends2 - vends1
		data	0x0000		;vends4 - vends3
		data	0x0000

;Configuration Memory 0x200000 to 0x200007

		data	"4_Chan01"

;******************************************************************************
;Variable definitions
; These variables are only needed if low priority interrupts are used.
; More variables may be needed to store other special function registers used
; in the interrupt routines.

		CBLOCK	0x000
		price1pv	;Initialized price1
		price2pv	;Initialized price2
		price3pv	;Initialized price3
		price4pv	;Initialized price4
		hopvaluepv	;Subtract this value for every coin out.
		coinvaluepv	;Add this value for every coin pulse.
		venderrpv	;Bit0=mot1 bit1=mot2 bit2=mot3 bit3=mot4 bit4=hopper
		coinsinpv	;Non volatile coinsin low byte
		coinsin1pv	;Non volatile coinsin middle byte
		coinsin2pv	;Non volatile coinsin high byte
		coinsinvpv	;Volatile coinsin low byte
		coinsinv1pv	;Volatile coinsin high byte
		coinsoutpv	;Non volatile coinsin low byte
		coinsout1pv	;Non volatile coinsin middle byte
		credit		;Credit in R1 increments
		creditt		;Temporary credit for display and hopper
		vendenable	;Bit0=mot1 bit1=mot2 bit2=mot3 bit3=mot4 set means vend
		vendcost	;Store cost of current vend
		vendflags	;bit 0=still credit bit 1=error bit 2=display incoin bit 3=nochange
				;bit4=credisplay bit5=Buy another?
		sensor		;Digital value of sensor volts
		sensflags	;bit 0=1 sensor interupt bit 1=1 timer end
		timcount	;Timer interupt counter
		timcounth
		dspaddr		;Display digit address holder
		WREG_TEMP	;variable used for context saving
		STATUS_TEMP	;variable used for context saving
		minprice	;Lowest vend price
		maxprice	;Turn off coin mech when this is reached
		winterupt	;Bit 0 = display service Bit 1 = eeprom service
		dispbyte	;Display data to be written, msnibble first ls second, clear after write
		dispwait	;Display data read, bit 7 = busy flag, high = busy
		mcstatus	;bit0=inscoin, bit1=maxprice bit2=maxcred bit3=coindisabled
				;bit4=no change
		togd		;Toggle display counter
		hexdisp		;Least significant hex display byte
		hexdisp1	;middle hex display byte
		hexdisp2	;Most significant hex display byte
		buttons		;Button decoder
		motors		;Vend motor enable bits
		loopr
		loopv		;Loop storage
		busy		;Busy flag storage
		nibble1		;lowest nibble for bcd display
		nibble2
		nibble3		;msn for credit display
		nibble4
		nibble5		;For 5th display digit
		nibble6
		nibble7
		nibble8		;Highest nibbles used only for total cash
		loopnop		;Display delay loop counter
		loopwsv		;Save WREG here for Display delay loop counter
		wait0		;counter registers
		wait1
		wait2
		servswitch	;Service switch bits bit 0 = 1 service switch pressed
		DATA_EE_DATA
		DATA_EE_ADDR
		ENDC

;_______________________________________________________________________________
;Macro definitions
pushwr	macro
	movff	WREG, WREG_TEMP	    ;Save wreg in wreg_temp
	endm
popwr	macro
	movff	WREG_TEMP, WREG	    ;Restore wreg
	endm
pushst	macro
	movff	WREG, WREG_TEMP	    ;Save wreg in wreg_temp
	endm
popst	macro
	movff	WREG_TEMP, WREG	    ;Restore wreg
	endm
clnib3	macro
	clrf	nibble1
	clrf	nibble2
	clrf	nibble3
	endm
clnib5	macro
	clrf	nibble1
	clrf	nibble2
	clrf	nibble3
	clrf	nibble4
	clrf	nibble5
	endm
clnib8	macro
	clrf	nibble1
	clrf	nibble2
	clrf	nibble3
	clrf	nibble4
	clrf	nibble5
	clrf	nibble6
	clrf	nibble7
	clrf	nibble8
	endm

clrhex	macro
	clrf	hexdisp
	clrf	hexdisp1
	clrf	hexdisp2
	endm

clrsnc	macro
	clrf	sensorcount
	movlw	0xEE
	movwf	sensorcount1
	movlw	0x86
	movwf	sensorcount2
	endm
presnc	macro
	movlw	0xEE
	movwf	sensorcount1
	endm
;	clrf	sensorcount3
;	clrf	sensorcount4		;7A 12 00

;******************************************************************************
;Reset vector
; This code will start executing when a reset occurs.

		ORG	0x0000

u		goto	Main		;go to start of main code

;******************************************************************************
;High priority interrupt vector
; This code will start executing when a high priority interrupt occurs or
; when any interrupt occurs if interrupt priorities are not enabled.

		ORG	0x0008

		goto	HighInt		;go to high priority interrupt routine

;******************************************************************************
;Low priority interrupt vector and routine
; This code will start executing when a low priority interrupt occurs.
; This code can be removed if low priority interrupts are not used.

		ORG	0x0018
		goto	HighInt		;go to high priority interrupt routine


;******************************************************************************
;High priority interrupt routine
;winterupt Bit 0 = display service Bit 1 = eeprom service

HighInt:	bsf	sensflags,1
highint1:	retfie	FAST

;******************************************************************************
;Write to display contents of displayd address in displaya
;******************************************************************************
;Start of main program
; The main program code is placed here.


Main:           Call	setports
                Call	initdisplay
		nop
		call	initram
		call	setmaxprice
		bsf	vendflags,2	    ;Display insert coin
		call	timer2s
Maincoine:	call	pricecheck
Maincoin1:	call	togdisplay	    ;Alternate between nochange and insert coin
Maincoin:	btfsc	vendflags,2	    ;Bit 2 set = display insert coin
		call	displinsert
		btfsc	vendflags,3	    ;Vendflags bit 3 = display no change
		call	displnochange
		btfss	PORTA, 1
		call	setup
		call	pricedisplay
		call	chancheck
		call	senserd
		btfsc	sensflags,0	    ;If sensor interupted stop
		call	sensorblock	    ;Sensor blocked, shut down machine
		bsf	LATC,7		    ;Turn on coin mech
		bcf	LATA,2		    ;Turn on note reader
		btfss	PORTA, 4	    ;Check for coin in
		call	coinin		    ;Check for coins
		btfss	PORTA, 5	    ;Check for note in
		call	notein		    ;Check for notes
		tstfsz	credit		    ;No credit then loop
		bra	vendloop
		btfsc	venderrpv,4	    ;Check if hopper is shut down
		bra	Maincoin1
		bra	Maincoin
vendloop:	call	pricecheck	    ;See if there's cash for vend.
		tstfsz	vendenable	    ;Are the vend flags set
		call	vend		    ;If button pushed vend channel
		btfsc	vendflags,0	    ;Bit 0=1 if vend complete and credit left
		call	refund		    ;Start the change loop
		btfss	PORTA, 1
		call	setup
		tstfsz	credit
		bra	venloop
		bsf	vendflags,2
		bra	Maincoine
venloop:	movff	credit,WREG
		call	maxpriceck
		btfss	PORTA, 4	    ;Check for coin in
		call	coinin		    ;Check for coins or notes
		call	togdisplayv	    ;Alternate between nochange and credit display
		btfsc	vendflags,3	    ;bit 3=nochange
		call	displnochange
		btfss	vendflags,4	    ;More credit display it
		bra	vendloop
		call	credisplay	    ;Update the display for fresh credit
		bra	vendloop

maxpriceck:	subwf	maxprice,0	    ;Compare credit to maximum price
		bc	onmech		    
offmech:	bcf	LATC,7		    ;Turn off coin & note reader
		bsf	LATA,2
		return
onmech:		bsf	LATC,7		    ;Turn on coin & note reader
		bcf	LATA,2
		return

togdisplayv:	btfss	INTCON,2	    ;If bit 2 = 1 timer finished
		return
		call	timer1.6s	    ;Reset timer
		btfss	venderrpv,4	    ;Check if hopper empty
		bra	togdispv2
		incf	togd
		btfss	togd,0
		bra	togdispv1	    ;Display credit
		bcf	vendflags,4	    ;Alternate display bit 4 = credit display
		bsf	vendflags,3	    ;Bit 3 high = display no change
		return			    ;Display "No Change"
togdispv1:	bsf	vendflags,4	    ;Alternate display bit 4 = credit display
		bcf	vendflags,3	    ;If bit 3 high = display no change
		return
togdispv2:	bcf	vendflags,3
		return

togdisplay:	btfss	INTCON,2	    ;If bit 2 = 1 timer finished
		return
		call	timer1.6s	    ;Reset timer
		incf	togd
		btfss	venderrpv,4
		return
		btfsc	togd,0
		bra	togdisp1	    ;Display "Insert Coin"
		bcf	vendflags,2	    ;Alternate display bit 2 = insert coin
		bsf	vendflags,3	    ;Bit 3 high = display no change
		return			    ;Display "No Change"
togdisp1:	bsf	vendflags,2	    ;Alternate display bit 2 = insert coin
		bcf	vendflags,3	    ;Bit 3 high = display no change
		return



chancheck:	movlw	0xF0		    ;Check if any channels still work
		iorwf	venderrpv,0	    ;Or in venderrpv to WREG
		comf	WREG
		bz	chancheck1
		return
chancheck1:	call	displempty	    ;All channels empty.
chancheck2:	btfss	PORTA, 1
		call	setup
		bra	chancheck2


pricecheck:	clrf	vendenable
		movff	price1pv,WREG
		cpfslt	credit		    ;Skip if credit less than price
		bsf	vendenable,0	    ;Set the enable flag
		movff	price2pv,WREG
		cpfslt	credit
		bsf	vendenable,1
		movff	price3pv,WREG
		cpfslt	credit
		bsf	vendenable,2
		movff	price4pv,WREG
		cpfslt	credit
		bsf	vendenable,3
		movff	venderrpv,WREG
		comf	WREG
		andwf	vendenable, 1	    ;Unset buttons with error
		btfss	vendenable,0
		bcf	LATD,0
		btfss	vendenable,1
		bcf	LATD,1
		btfss	vendenable,2
		bcf	LATD,4
		btfss	vendenable,3
		bcf	LATD,5
		return

vend:		clrf	motors
		btfsc	vendenable,0
		call	svend1
		btfsc	vendenable,1
		call	svend2
		btfsc	vendenable,2
		call	svend3
		btfsc	vendenable,3
		call	svend4
		return

svend1:		bsf	LATD,0		    ;Button light one on RD0
		movff	price1pv,vendcost
		btfsc	PORTD, 2	    ;Button one on RD2 - motor RC2
		return
		clrf	LATD
		bsf	LATD,0		    ;Button light one on RD0
		bsf	motors,0	    ;Turn on motor flag
		bra	startvend

svend2:		bsf	LATD,1		    ;Button light two on RD1
		movff	price2pv,vendcost
		btfsc	PORTD, 3	    ;Button two on RD3 - motor RC3
		return
		clrf	LATD
		bsf	LATD,1		    ;Button light two on RD1
		bsf	motors,1
		bra	startvend

svend3:		bsf	LATD,4		    ;Button light three on RD4
		movff	price3pv,vendcost
		btfsc	PORTC, 4	    ;Button three on RC4 - motor RC0
		return
		clrf	LATD
		bsf	LATD,4		    ;Button light three on RD4
		bsf	motors,2
		bra	startvend

svend4:		bsf	LATD,5		    ;Button light one on RD5
		movff	price4pv,vendcost
		btfsc	PORTC, 5	    ;Button four on RC5 - motor RC1
		return
		clrf	LATD
		bsf	LATD,5		    ;Button light one on RD5
		bsf	motors,3
		bra	startvend

sensorerr:	clrf	vendcost	    ;Sensor error shut down machine
		movlw	0x0F
		iorwf	venderrpv	    ;Set disable all channels
		bsf	vendflags,1	    ;error set flag bit 1 100% refund
errorvend1:	movlw	venderr
		movwf	EEADR		    ;Address of venderr storage
		movff	venderrpv,EEDATA    ;Write error flags
		call	eewrite		    ;Write updated value to eeprom
		bsf	vendflags,0	    ;credit leftover set vendflags bit 0
		bsf	vendflags,4	    ;More credit display it
		clrf	LATD
		clrf	togd
		return

errorvend:	movff	motors,WREG
		iorwf	venderrpv	    ;Combine error into volatile venderr flags
		bra	errorvend1

startvend:	bcf	LATC,7		    ;Turn off coin & note reader
		bsf	LATA,2
		call	displwait	    ;Display "Wait"
		call	storecoins	    ;Update eeprom coins in
	        clrf	vendflags	    ;Reset vendflags at start of vend.
		bsf	vendflags,4	    ;More credit display it
		bcf	vendflags,3	    ;Don't display No Change
		call	senserd		    ;Read optic sensor
		btfsc	sensflags,0	    ;If bit 0 = 0 continue with vend
		bra	sensorerr	    ;Sensor blocked
		movff	motors, PORTC	    ;Start to vend
		call	delay.1s	    ;Wait for 100mS for motor start
		call	timer1.6s	    ;Start the timer, monitor sensflags bit 1
startvend1:	call	senserd		    ;Read optic sensor
		btfsc	sensflags,0	    ;Bit 0 = 1 when beam is broken
		bra	vendend
		btfsc	INTCON,2	    ;If bit 2 set in INTCON timer finished
		bra	motorsoff	    ;Turn off motor wait for drop
		bra	startvend1	    ;Loop until beam broken or timer finished
motorsoff:	clrf	LATD		    ;Switch off button light
		tstfsz	LATC		    ;If motor is off and nothing dropped we have a problem
		bra	motoroff
		bra	errorvend
motoroff:	clrf	LATC		    ;Switch motors off
		call	timer2s		    ;Start the timer again
		bra	startvend1	    ;Wait for product to drop
vendend:	clrf	LATC		    ;Turn off motor at end of sucessfull vend
		movff	vendcost,WREG
		subwf	credit
		bc	vendend1
		clrf	credit		    ;Negative result clear credit
vendend1:	tstfsz	credit
		bsf	vendflags,0	    ;credit leftover set vendflags bit 0
		clrf	vendenable	    ;Reset all vendenable flags
		call	updatevend	    ;Update vend counters
		clrf	togd
		return

refund:		btfsc	venderrpv,4	    ;If bit 4 is set hopper error
		return
;		btfsc	vendflags,1	    ;Bit 1 set = error vend 100% refund
		bra	refundall
		clrf	vendflags	    ;Reset vendflags
		call	pricecheck	    ;Check if enough funds
		tstfsz	vendenable	    ;Is there enough cash to vend?
		bra	refund0		    ;Wait for fresh vend alternate display
		bra	refundall	    ;Just pay the money
refund0:	movlw	0x09
		movwf	loopv		    ;Alternate display 9 times
refund1:	call	timer1s		    ;Start 1 second timer
		call	credisplay	    ;Display credits
refund2:	call	vend		    ;Wait for fresh vend
		tstfsz	vendflags	    ;Check for successful vends
		bra	refund		    ;Start the whole process again
		btfsc	INTCON,2	    ;If bit 1 = 1 timer finished
		bra	step1
		bra	refund2		    ;Loop until timer finished
step1:		decf	loopv		    ;Decrement counter
		bz	refundall	    ;Just pay the money
		call	timer1s		    ;Start 1 second timer
		call	displpushbutton	    ;Display "Push Button"
refund3:	call	vend		    ;Wait for fresh vend
		tstfsz	vendflags
		bra	refund		    ;Start the whole process again
		btfsc	INTCON,2	    ;If bit 1 = 1 timer finished
		bra	step2
		bra	refund3		    ;Loop until timer finished
step2:		decf	loopv		    ;Decrement counter
		bz	refundall	    ;Just pay the money
		call	timer1s		    ;Start 1 second timer
		call	displbuymore	    ;Display "Buy Again?"
refund4:	call	vend		    ;Wait for fresh vend
		tstfsz	vendflags	    ;If vendflags are set a vend has taken place
		bra	refund		    ;Start the whole process again
		btfsc	INTCON,2	    ;If bit 1 = 1 timer finished
		bra	step3
		bra	refund4		    ;Loop until timer finished
step3:		decf	loopv
		bnz	refund1
refundall:	movff	hopvaluepv,WREG	    ;Value of hopper coin
		clrf	vendflags	    ;Reset vendflags
		bsf	vendflags,4	    ;Display credit
		clrf	creditt		    ;Creditt = number of coins to vend
		cpfslt	credit		    ;Not enough credit left for one coin
		bra	payout		    ;credit is => hopper coin.
		return			    ;credit is < hopper coin
payout:		incf	creditt		    ;Payout at least one coin
		movff	hopvaluepv,WREG	    ;Value of hopper coin
		subwf	credit		    ;Subtract one coin from credit
		bz	payout0		    ;Pay out time
		cpfslt	credit		    ;Not enough credit left for one more coin
		bra	payout
payout0:	;btfsc	PORTA,3		    ;Test if hopper switch works
;		bra	hoperror	    ;Hopper switch problem
		bsf	PORTC,6		    ;Hopper motor on RC6 on
		btfsc	PORTA,3		    ;Test if coin in switch
		bra	payout3		    ;Hopper switch open coin going through
payout1:	call	delay.1s	    ;Debounce
		call	timer1.6s	    ;1.6 second time out
payout2:	btfsc	PORTA,3		    ;Test if hopper switch open
		bra	payout3		    ;Hopper switch open coin going through
		btfsc	INTCON,2	    ;If bit 2 set in INTCON timer finished
		bra	hoperror	    ;Hopper timeout
		bra	payout2		    ;Carry on looping wait for coin
payout3:	call	delay.1s	    ;debounce
payout4:	call	timer1s		    ;Allow 1 Second for coin to pass
payout5:	btfss	PORTA,3		    ;Test if hopper switch closed
		bra	coinpast	    ;Coin exit
		btfsc	INTCON,2	    ;If bit 2 set in INTCON timer finished
		bra	hoperror	    ;Hopper timeout coin jam
		bra	payout5		    ;Carry on looping
coinpast:	infsnz	coinsoutpv	    ;Increment coins out counter
		incf	coinsout1pv	    ;Increment high byte
		decfsz	creditt		    ;Paying finished if creditt = 0
		bra	payout1
		bcf	PORTC,6		    ;Hopper motor on RC6 off
		movlw	coinsout	    ;Address of coins out storage
		movwf	EEADR		    ;Address of coins out storage
		movff	coinsoutpv,EEDATA   ;coins out lower byte
		call	eewrite		    ;Write updated value to eeprom
		incf	EEADR
		movff	coinsout1pv,EEDATA   ;Coins out upper byte
		call	eewrite
		clrf	togd
		tstfsz	credit
		bsf	vendflags,4	    ;More credit display it
		return

hoperror:	bsf	venderrpv,4	    ;Bit 4 = hopper shut down
		bcf	PORTC,6		    ;Hopper motor on RC6 off
		movff	hopvaluepv,WREG
		tstfsz	creditt
		bra	hoperrora	    ;add coins to credit
hoperrore:	movlw	venderr
		movwf	EEADR
		movff	venderrpv,EEDATA    ;Shut down hopper for ever
		call	eewrite
		clrf	togd
		return
hoperrora:	addwf	credit
		decfsz	creditt
		bra	hoperrora
		bra	hoperrore

updatevend:	movlw	vends1		    ;Address of vends3 storage
		btfsc	motors,0	    ;Motors 0 = vends3
		bra	updatevend1
		movlw	vends2		    ;Address of vends4 storage
		btfsc	motors,1	    ;Motors 1 = vends4
		bra	updatevend1
		movlw	vends3		    ;Address of vends1 storage
		btfsc	motors,2	    ;Motors 2 = vends1
		bra	updatevend1
		movlw	vends4		    ;Address of vends2 storage
updatevend1:	movwf	EEADR		    ;Address of vends storage
		call	eeread
		incf	EEDATA		    ;Increment number of vends
		call	eewrite		    ;Write updated value to eeprom
		return

storecoins:	movlw	coinsin		    ;Update non volatile eeprom coinsin
		movwf	EEADR
		movff	coinsinpv,EEDATA
		call	eewrite
		movlw	coinsin1
		movwf	EEADR
		movff	coinsin1pv,EEDATA
		call	eewrite
		movlw	coinsin2
		movwf	EEADR
		movff	coinsin2pv,EEDATA
		call	eewrite
		movlw	coinsinv	    ;Update volatile eeprom coinsin
		movwf	EEADR
		movff	coinsinvpv,EEDATA
		call	eewrite
		movlw	coinsinv1
		movwf	EEADR
		movff	coinsinv1pv,EEDATA
		call	eewrite
		return

servin:		clrf	servswitch
		btfsc	PORTA, 1
		return			    ;Service switch not pressed
		bsf	servswitch,0
servin1:	btfss	PORTA, 1	    ;Debounce and make service switch usable
		bra	servin1
		call	delay.1s
		btfss	PORTA, 1
		bra	servin1
		return

setup:		btfss	PORTA, 1
		bra	setup
		bcf	LATC,7		    ;Turn off coin & note mechs
		movlw	venderr
		call	clearee
		call	displauditmode
		call	delay1s
setup1:		call	butin
		clrf	WREG
		btfsc	buttons, 0
		call	priceset
		btfsc	WREG,0
		bra	setup
		btfsc	buttons,1
		call	sensdisplay
		btfsc	WREG,1
		bra	setup
		btfsc	buttons,2
		call	auditmode
		btfsc	WREG,2
		bra	setup
		btfsc	buttons,3
		call	sethop
		btfsc	WREG,5
		bra	setup
		call	servin
		btfss	servswitch,0
		bra	setup1
		reset

sethop:		call	displhop
		clrhex
		movlw	hopvalue
		call	eeread
		movff	EEDATA,hexdisp
		call	disphex
sethop1:	call	butin
		clrf	WREG
		btfsc	buttons,0
		call	dechop
		btfsc	buttons,1
		call	inchop
		tstfsz	WREG
		bra	sethop
		btfss	buttons,2
		bra	sethop1
		call	delay1s
		retlw	0x20

dechop:		call	delay.2s
		call	butin
		tstfsz	buttons
		bra	dechop
		decf	EEDATA
		call	eewrite
		retlw	0x01

inchop:		call	delay.2s
		call	butin
		tstfsz	buttons
		bra	inchop
		incf	EEDATA
		call	eewrite
		retlw	0x02

auditmode:	call	displaudit
		call	delay1s
auditmode1:	call	butin
		clrf	WREG
		btfsc	buttons,0
		call	displaytot	    ;Display clearable totals
		btfsc	WREG,0
		bra	auditmode
		btfsc	buttons,1
		call	displaytotc	    ;Display clearable totals
		btfsc	WREG,1
		bra	auditmode
		btfsc	buttons,2
		call	displayvends	    ;Display vend totals
		btfsc	WREG,2
		bra	auditmode
		btfss	buttons,3
		bra	auditmode1
auditmodee:	call	butin
		call	delay1s
		retlw	0x04

displaytot:	clrhex
		movff	coinsin2pv,hexdisp2
		movff	coinsin1pv,hexdisp1
		movff	coinsinpv,hexdisp
		call	displcash
		call	disphex
displaytot1:	call	butinf
		tstfsz	buttons
		bra	displaytot1
		call	delay1s
		retlw	0x01

displaytotc:	call	displtotals
		call	delay.5s
displaytotc1:	call	butin
		clrf	WREG
		btfsc	buttons,0
		call	cleartot
		btfsc	WREG,2
		bra	displaytotc
		btfsc	buttons,1
		call	totadisp
		btfsc	WREG,0
		bra	displaytotc
		btfsc	buttons,2
		call	coutdisp
		btfsc	WREG,3		    ;If WREG not zero refresh display
		bra	displaytotc
		btfss	buttons,3	    ;Button 4 exit
		bra	displaytotc1
		call	delay1s
		clrf	buttons
		retlw	0x02

totadisp:	clrf	hexdisp2
		movlw	coinsinv
		call	eeread
		movff	EEDATA,hexdisp
		movlw	coinsinv1
		call	eeread
		movff	EEDATA,hexdisp1
		call	displcash
		call	disphex
totadispe:	call	butinf
		call	delay.5s
		tstfsz	buttons
		bra	totadispe
		call	delay1s
		retlw	0x01

coutdisp:	call	displcoinsout
		clrf	hexdisp2
		movlw	coinsout
		call	eeread
		movff	EEDATA,hexdisp
		movlw	coinsout1
		call	eeread
		movff	EEDATA,hexdisp1
		call	disphex
coutdispe:	call	butinf
		call	delay.5s
		tstfsz	buttons
		bra	coutdispe
		call	delay1s
		retlw	0x08

displayvends:	call	displvends
		call	delay.5s
displayvends1:	call	butinf
displayvends2:	movlw	"1"
		movwf	dispbyte
		movlw	vends1
		btfsc	buttons,0
		bra	vendisplay
		movlw	"2"
		movwf	dispbyte
		movlw	vends2
		btfsc	buttons,1
		bra	vendisplay
		movlw	"3"
		movwf	dispbyte
		movlw	vends3
		btfsc	buttons,2
		bra	vendisplay
		movlw	"4"
		movwf	dispbyte
		movlw	vends4
		btfsc	buttons,3
		bra	vendisplay
		call	servin
		btfss	servswitch,0
		bra	displayvends1
		call	delay1s
		retlw	0x04
vendisplay:	movwf	EEADR
		call	eeread
		clrhex
		movff	EEDATA,hexdisp
		movff	dispbyte,WREG
		call	charwrite
		movlw	" "
		call	charwrite
		call	disphex
vendispexit:	call	butinf
		tstfsz	buttons
		bra	vendispexit
		call	delay.1s
		bra	displayvends

cleartot:	call	displcleartot
cleartot1:	call	butin
		btfsc	buttons,2
		retlw	0x04
		btfss	buttons,1
		bra	cleartot1
		movlw	coinsinv
		call	clearee
		movlw	coinsinv1
		call	clearee
		movlw	coinsinv
		call	clearee
		movlw	coinsout
		call	clearee
		movlw	coinsout1
		call	clearee
		movlw	vends1
		call	clearee
		movlw	vends2
		call	clearee
		movlw	vends3
		call	clearee
		movlw	vends4
		call	clearee
		call	delay1s
		retlw	0x04

clearee:	movwf	EEADR
		clrf	EEDATA
		call	eewrite
		return

priceset:	call	displsetp
		clrf	vendcost
priceset1:	call	servin
		btfsc	servswitch,0
		retlw	0x01
		call	butin
		tstfsz	buttons
		bra	priceset1
priceset2:	call	delay.1s
		call	butin
		btfsc	buttons,0
		bra	setp1
		btfsc	buttons,1
		bra	setp2
		btfsc	buttons,2
		bra	setp3
		btfss	buttons,3
		bra	priceset1
setp4:		movlw	price4		    ;eeprom = 0xf00000
		bra	setprice
setp3:		movlw	price3
		bra	setprice
setp2:		movlw	price2
		bra	setprice
setp1:		movlw	price1
setprice:	movwf	EEADR
		call	eeread
		movff	EEDATA,vendcost
		call	delay.2s
		call	displayprice
setprice1:	call	butin
		btfsc	buttons,0
		call	decprice
		btfsc	buttons, 1
		call	incprice
		btfss	buttons,3
		bra	setprice1
		call	delay.5s
setpexit:	call	butin
		btfsc	buttons,3
		bra	setpexit
		call	delay.2s
		bra	priceset

decprice:	call	delay.2s
		call	butin
		tstfsz	buttons
		bra	decprice
		decf	EEDATA
		call	eewrite
		movff	EEDATA,vendcost
		call	displayprice
		return

incprice:	call	delay.2s
		call	butin
		tstfsz	buttons
		bra	incprice
		incf	EEDATA
		call	eewrite
		movff	EEDATA,vendcost
		call	displayprice
		return

butinf:		clrf	buttons		    ;Fast butin
		btfss	PORTD, 2	    ;Check button 1
		bsf	buttons, 0	    ;Set 1st button bit
		btfss	PORTD, 3	    ;Check button 2
		bsf	buttons, 1	    ;Set 2nd button bit
		btfss	PORTC, 4	    ;Check button 3
		bsf	buttons, 2	    ;Set 3rd button bit
		btfss	PORTC, 5	    ;Check button 4
		bsf	buttons, 3	    ;Set 4th button bit
		return


butin:		clrf	buttons
		btfss	PORTD, 2	    ;Check button 1
		bsf	buttons, 0	    ;Set 1st button bit
		btfss	PORTD, 3	    ;Check button 2
		bsf	buttons, 1	    ;Set 2nd button bit
		btfss	PORTC, 4	    ;Check button 3
		bsf	buttons, 2	    ;Set 3rd button bit
		btfss	PORTC, 5	    ;Check button 4
		bsf	buttons, 3	    ;Set 4th button bit
		tstfsz	buttons
		bra	butin1
		retlw	0x00		    ;Nothing pressed
butin1:		movff	PORTD,WREG	    ;store in WREG
		andlw	0x0C		    ;Mask off bits 2 & 3
		iorwf	PORTC,0		    ;combine PORTC
		comf	WREG		    ;Invert bits
		andlw	0x3C		    ;Mask off bits 2,3,4 & 5
		bnz	butin1
		retlw	0x00

pricedisplay:	call	butinf
		tstfsz	buttons
		bra	pdspgo
		return
pdspgo:		btfsc	buttons,0
		bra	price1dsp
		btfsc	buttons,1
		bra	price2dsp
		btfsc	buttons,2
		bra	price3dsp
		btfsc	buttons,3
		bra	price4dsp
		return			    ;False alarm
price1dsp:	movff	price1pv,vendcost
		btfss	venderrpv,0
		bra	displayprice
		bra	displayoos
price2dsp:	movff	price2pv,vendcost
		btfss	venderrpv,1
		bra	displayprice
		bra	displayoos
price3dsp:	movff	price3pv,vendcost
		btfss	venderrpv,2
		bra	displayprice
		bra	displayoos
price4dsp:	movff	price4pv,vendcost
		btfss	venderrpv,3
		bra	displayprice
		bra	displayoos
displayprice:	clrhex
		call	displprice
		movff	vendcost,hexdisp
		call	disphex
dsprice:	call	butin
		bsf	vendflags,2
		bcf	vendflags,3
		return

displayoos:	call	displempty
		bra	dsprice

credisplay:	call	displcredit
		bcf	vendflags,4	    ;Clear the display bit.
		clnib3
		movff	credit, WREG
		movwf	creditt		    ;Move to working register
credisp1:	movlw	0x09		    ;9 is the highest for 1 digit
		cpfsgt	creditt
		bra	credit1nb	    ;Display lowest nibble
		movlw	.99		    ;99 in decimal = 2 digits
		cpfsgt	creditt		    ;Up to 99 needs two digits
		bra	credit2nb	    ;Display two nibbles
		movlw	.100		    ;Use 100 for conversion
crednb3:	incf	nibble3		    ;nibble2 contains hundreds digit
		subwf	creditt, 1	    ;Subtract 100 from creditt for upper nibble
		bz	credisp1	    ;Exactly R100?
		cpfslt	creditt
		bra	crednb3
		bra	credisp1	    ;Repeat until less than 100
credit2nb:	movlw	.10		    ;10s
		subwf	creditt, 0	    ;Is there exactly R10?
		movlw	.10
		bz	crednb2
		cpfsgt	creditt		    ;if creditt is less than 10 nibble2 stays 0
		bra	credisp1
crednb2:	subwf	creditt		    ;
		incf	nibble2
		bra	credisp1
credit1nb:	movff	creditt, nibble1
		tstfsz	nibble3		    ;Display nibble3?
		bra	nib3disp	    ;display 3 nibbles
		tstfsz	nibble2		    ;If nibble2 is nz and nibble1 is display nibble1
		bra	nib2disp
nib1disp:	movlw	0x30
		iorwf	nibble1,0
		call	charwrite
		movff	credit,creditt
		return
nib3disp:	movlw	0x30
		iorwf	nibble3,0
		call	charwrite
nib2disp:	movlw	0x30
		iorwf	nibble2,0
		call	charwrite
		bra	nib1disp

cointest:	call	displcointest
cointest1:	btfsc	PORTA, 4	    ;Read RA4 skip if clear
		bra	cointest3
		call	coinin
		call	credisplay
cointest3:	btfsc	PORTA, 1	    ;Read service switch
		bra	cointest1
		call	delay5ms	    ;Debounce
cointest2:	btfsc	PORTA, 1	    ;Read service switch
		bra	cointest3
		call	delay5ms	    ;Debounce
		btfsc	PORTA, 1	    ;Read service switch
		bra	cointest2	    ;False alarm
		call	displtest
		return

coinin:		btfsc	PORTA, 4	    ;If it's not low then there's problem
		return
coinin1:	btfss	PORTA, 4	    ;Loop while credit in is low
		bra	coinin1
		movff	coinvaluepv,WREG
		addwf	credit		    ;Increment credit store.
		addwf	coinsinpv
		bnc	coinin2
		incf	coinsin1pv
		bnc	coinin2
		incf	coinsin2pv
coinin2:	addwf	coinsinvpv
		bnc	coinin3
		incf	coinsinv1pv
coinin3:	bsf	vendflags,4	    ;Display credit
		bcf	vendflags,3	    ;Don't display no change
		clrf	togd
		call	timer5ms
		return

notein:		btfsc	PORTA, 5	    ;If it's not low then there's problem
		return
notein1:	btfss	PORTA, 5	    ;Loop while credit in is low
		bra	notein1
		call	timernote	    ;Set 150mS timer and disable coins.
		movlw	0x0A		    ;Note reader gives R10 pulses
		addwf	credit		    ;Increment credit store.
		addwf	coinsinpv
		bnc	notein2
		incf	coinsin1pv
		bnc	notein2
		incf	coinsin2pv
notein2:	addwf	coinsinvpv
		bnc	notein3
		incf	coinsinv1pv
notein3:	bsf	vendflags,4	    ;Display credit
		bcf	vendflags,3	    ;Don't display no change
		clrf	togd
notein4:	btfss	PORTA, 5
		goto	notein
		btfss	INTCON,2	    ;If bit 2 = 1 timer finished
		goto	notein4		    ;Wait for note pulse for 150mSec
		call	timer1s
		return

displtest:	call	dispclear
		movlw	Testing		    ;Upper string address
		movwf	TBLPTRH
		movlw	Testingl	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displtotals:	call	dispclear
		movlw	Totals		    ;Upper string address
		movwf	TBLPTRH
		movlw	Totalsl		    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displempty:	call	dispclear
		movlw	Empty		    ;Upper string address
		movwf	TBLPTRH
		movlw	Emptyl		    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displinsert:	call	dispclear
		bcf	vendflags,2	    ;Clear bit 2 and display
		movlw	Insertcoin	    ;Upper string address
		movwf	TBLPTRH
		movlw	Insertcoinl	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displcredit:	call	dispclear
		movlw	Credit	        ;Upper string address
		movwf	TBLPTRH
		movlw	Creditl		    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displpushbutton:call	dispclear
		movlw	Pushlitbutton       ;Upper string address
		movwf	TBLPTRH
		movlw	Pushlitbuttonl	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displtovend:	call	dispclear
		movlw	Tovend		    ;Upper string address
		movwf	TBLPTRH
		movlw	Tovendl		    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displbuymore:	call	dispclear
		movlw	Buyagain	    ;Upper string address
		movwf	TBLPTRH
		movlw	Buyagainl	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displauditmode:	call	dispclear	    ;Actually service mode
		movlw	Auditmode	    ;Upper string address
		movwf	TBLPTRH
		movlw	Auditmodel	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displaudit:	call	dispclear	    ;Actually service mode
		movlw	Audit		    ;Upper string address
		movwf	TBLPTRH
		movlw	Auditl		    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displprice:	call	dispclear
		movlw	Price		    ;Upper string address
		movwf	TBLPTRH
		movlw	Pricel		    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displvends:	call	dispclear
		movlw	Vends		    ;Upper string address
		movwf	TBLPTRH
		movlw	Vendsl		    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displcash:	call	dispclear
		movlw	Cash		    ;Upper string address
		movwf	TBLPTRH
		movlw	Cashl		    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		movlw	"R"
		call	charwrite
		return

displcoinsout:	call	dispclear
		movlw	Coinout		    ;Upper string address
		movwf	TBLPTRH
		movlw	Coinoutl	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displtestmode:	call	dispclear
		movlw	Testmode	    ;Upper string address
		movwf	TBLPTRH
		movlw	Testmodel	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displvendtest:	call	dispclear
		movlw	Vendtest	    ;Upper string address
		movwf	TBLPTRH
		movlw	Vendtestl	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displcointest:	call	dispclear
		movlw	Coinnotetest	    ;Upper string address
		movwf	TBLPTRH
		movlw	Coinnotetestl	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

disploutofser:	call	dispclear
		movlw	Outofservice	    ;Upper string address
		movwf	TBLPTRH
		movlw	Outofservicel	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displnochange:	call	dispclear
		bcf	vendflags,3	    ;Clear bit 3 and display
		movlw	Nochange	    ;Upper string address
		movwf	TBLPTRH
		movlw	Nochangel	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displcleartot:	call	dispclear
		movlw	Cleartotals	    ;Upper string address
		movwf	TBLPTRH
		movlw	Cleartotalsl	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displwait:	call	dispclear
		movlw	Pleasewait	    ;Upper string address
		movwf	TBLPTRH
		movlw	Pleasewaitl	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displhop:	call	dispclear	    ;Display hooper coin value
		movlw	Hopvalued	    ;Upper string address
		movwf	TBLPTRH
		movlw	Hopvaluedl	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

displsetp:	call	dispclear	    ;Price set mode
		movlw	Setprice	    ;Upper string address
		movwf	TBLPTRH
		movlw	Setpricel	    ;Lower string address
		movwf	TBLPTRL		    ;Lower string address
		call	dispstring
		return

sensorblock:	call	disploutofser
sensorblk:	bcf	PORTC,7		    ;Turn off coin & note reader
		btfss	PORTA, 1
		call	setup
		call	senserd
		btfsc	sensflags,0
		bra	sensorblk	    ;Loop until sensor is cleared
		clrf	vendflags
		bsf	vendflags,2
		call	delay5ms
		return

senserd:	call	getsensor	    ;Read sensor and set or unset sensor bit
		movlw	0xFF		    ;sensor interupted is < FE
		bsf	sensflags,0	    ;Sensor is interupted or wire is broken
		cpfseq	sensor		    ;Sensor interupted if equal
		bcf	sensflags,0	    ;Sensor is on and not interrupted
		return

getsensor:	bsf     ADCON0,GO	    ;Start A/D conversion
getsensor1:	btfss   PIR1,ADIF	    ;Wait for conversion to complete
		goto    getsensor1
		clrf	PIR1
		rrcf	ADRESH		    ;Rotate lsb of ADRESH to carry
		rrcf	ADRESL		    ;Add lsb of ADRESH to ADRESHL
		rrcf	ADRESH		    ;Rotate lsb of ADRESH to carry
		rrcf	ADRESL		    ;Add lsb of ADRESH to ADRESHL
		movff	ADRESL, sensor
		return

sensdisplay:	call	dispclear
sensorloop:	call	butin
		btfsc	buttons,0
		retlw	0x02
		call	disphome
		call	getsensor	    ;Read A/D converter sensor voltage
		clrhex
		movff	sensor,hexdisp
		call	disphex
		call	delay.1s
		bra	sensorloop

disphexw:	call	displwait
disphex:	clnib8			    ;Display 3 hex bytes as decimal. hexdisp, hexdisp1, hexdisp2
		bcf	STATUS,C	    ;Clear carry flag
disphex0:	btg	STATUS,C	    ;Toggle the carry flag for stupid subwfb
		tstfsz	hexdisp2	    ;Is there data in the most significant byte
		bra	disphex8
		tstfsz	hexdisp1	    ;Is there data in the middle byte
		bra	disphex5
		bra	disphex3	    ;Just convert one byte
disphex8:	movlw	.10		    ;Convert three bytes
		subwfb	hexdisp
		clrf	WREG
		subwfb	hexdisp1
		subwfb	hexdisp2
disphex81:	call	incnibb		    ;9 is maximum count clear it
		bra	disphex0
disphex5:	movlw	.10
		subwfb	hexdisp
		clrf	WREG
		subwfb	hexdisp1
		bra	disphex81
disphex3:	movlw	.9
		cpfsgt	hexdisp
		bra	hex1nib
		movlw	.10
		subwfb	hexdisp
		call	incnibb		    ;9 is maximum count clear it
		bra	disphex0
hex1nib:	bnc	hex1nib1
;		incf	hexdisp		    ;Finish off
hex1nib1:	movff	hexdisp,nibble1
		movlw	.10
		cpfslt	nibble1
		call	incnibb1
dsphexnib:	tstfsz	nibble8
		bra	digitwrite8
		tstfsz	nibble7
		bra	digitwrite7
		tstfsz	nibble6
		bra	digitwrite6
		tstfsz	nibble5
		bra	digitwrite5
		tstfsz	nibble4
		bra	digitwrite4
		tstfsz	nibble3
		bra	digitwrite3
		tstfsz	nibble2
		bra	digitwrite2
digitwrite1:	movff	nibble1,WREG
digitwrite:	iorlw	0x30		    ;Last digit to display
		call	charwrite
		return
digitwrite8:	movff	nibble8,WREG
		call	digitwrite
digitwrite7:	movff	nibble7,WREG
		call	digitwrite
digitwrite6:	movff	nibble6,WREG
		call	digitwrite
digitwrite5:	movff	nibble5,WREG
		call	digitwrite
digitwrite4:	movff	nibble4,WREG
		call	digitwrite
digitwrite3:	movff	nibble3,WREG
		call	digitwrite
digitwrite2:	movff	nibble2,WREG
		call	digitwrite
		bra	digitwrite1

incnibb1:	clrf	nibble1
incnibb:	pushwr
		pushst
		movlw	.10
		incf	nibble2
		cpfseq	nibble2
		bra	incnibend
		clrf	nibble2
		incf	nibble3
		cpfseq	nibble3
		bra	incnibend
		clrf	nibble3
		incf	nibble4
		cpfseq	nibble4
		bra	incnibend
		clrf	nibble4
		incf	nibble5
		cpfseq	nibble5
		bra	incnibend
		clrf	nibble5
		incf	nibble6
		cpfseq	nibble6
		bra	incnibend
		clrf	nibble6
		incf	nibble7
		cpfseq	nibble7
		bra	incnibend
		clrf	nibble7
		incf	nibble8
		cpfseq	nibble8
		bra	incnibend
		clrf	nibble8
incnibend:	popwr
		popst
		return

setports:       movlw	adsensor	   ;Switch AN0 to analogue.
		movwf	ADCON1
		movlw   0xC1		   ;RC, A/D enabled
		movwf	ADCON0
		movlw	0x84
		movwf	ADCON2
                clrf    TRISA
                comf    TRISA              ;All port a to input
		bcf	TRISA,2		   ;RA2 for note reader enable
                clrf    LATB               ;Set display bits to zero, RB4 - enable off
		movlw	0xe0
		movwf   TRISB		   ;Load port b's direction write
                clrf    LATB               ;Set display bits to zero, RB4 - enable off
                movlw   0x30               ;All port c except RC5 and RC4 are outputs
                clrf    LATC               ;Set motors and button lights off
                movwf   TRISC
                clrf    LATC               ;Set motors and button lights off
                movlw   0x0c               ;All port d except RD2 and RD3 are outputs
                movwf   TRISD
                clrf    LATD               ;All outputs off
		bsf	LATD,6		   ;Set button pullup on
		movlw	0x09		   ;All port e in except 1 = display r/w 2 = RS
		movwf	TRISE
		clrf	LATE		   ;Setup RS = 0 R/W = 0 Write instruction
		bcf	PORTC,7		    ;Turn off coin & note reader
		clrf	RCON
		clrf	INTCON
		clrf	INTCON2
		clrf	INTCON3
		clrf	PIR1
		clrf	PIR2
		clrf	PIE1
		clrf	PIE2
		clrf	IPR1
		clrf	IPR2
		clrf	EECON1
		return

rbusy:		pushwr
		movlw	0x02
		movwf	LATE		    ;Set read instruction
		movff	TRISB, WREG
		iorlw	0x0f		    ;Set B direction to input
		movff	WREG, TRISB
		bsf	LATB, 4		    ;set E on
		call	dlynop
		movff	PORTB, busy	    ;Bit 3 in busy is the busy flag
		bcf	LATB, 4
		call	dlynop
		swapf	busy,0		    ;Swap nibbles result in WREG
		andlw	0xf0
		movff	WREG, busy
		bsf	LATB, 4
		call	dlynop
		movwf	PORTB		    ;Lower nibble to busy
		bcf	LATB, 4
		call	dlynop
		andlw	0x0f
		iorwf	busy, 1
		clrf	TRISB		    ;Restore direction to write.
		popwr
		return

dlynop:		movff	WREG, loopwsv	    ;Set LCD delay here
		movlw	0x14
		movwf	loopnop
dlynop1:	decfsz	loopnop
		bra	dlynop1
		movff	loopwsv, WREG

eeread:		movwf	EEADR		    ;Address of price 1 storage
		BCF	EECON1, EEPGD	    ; Point to DATA memory
		BCF	EECON1, CFGS	    ; Access program FLASH or Data EEPROM memory
		BSF	EECON1, RD	    ; EEPROM Read
		return

initram:	movlw	price1
		call	eeread
		movff	EEDATA, price1pv    ;Store price 1 in global register
		movlw	price2
		call	eeread
		movff	EEDATA, price2pv    ;Store price 2 in global register
		movlw	price3
		call	eeread
		movff	EEDATA, price3pv    ;Store price 3 in global register
		movlw	price4
		call	eeread
		movff	EEDATA, price4pv    ;Store price 4 in global register
		movlw	hopvalue
		call	eeread
		movff	EEDATA, hopvaluepv  ;Store coin value in global register
		movlw	coinvalue
		call	eeread
		movff	EEDATA, coinvaluepv  ;Store coin value in global register
		movlw	venderr
		call	eeread
		movff	EEDATA, venderrpv   ;Store error bits in global register
		movlw	coinsin
		call	eeread
		movff	EEDATA, coinsinpv   ;Store coins in in global register
		movlw	coinsin1
		call	eeread
		movff	EEDATA, coinsin1pv   ;Store coins in in global register
		movlw	coinsin2
		call	eeread
		movff	EEDATA, coinsin2pv  ;Store coins in in global register
		movlw	coinsinv
		call	eeread
		movff	EEDATA, coinsinvpv  ;Store coins in in global register
		movlw	coinsinv1
		call	eeread
		movff	EEDATA, coinsinv1pv ;Store coins in in global register
		movlw	coinsout
		call	eeread
		movff	EEDATA, coinsoutpv  ;Store coins in in global register
		movlw	coinsout1
		call	eeread
		movff	EEDATA, coinsout1pv ;Store coins in in global register
		call	clreg		    ;Clear sensor flags
		return

setmaxprice:	movff	price1pv,WREG
		cpfslt	price2pv	    ;If WREG > price2pv test next
		movff	price2pv,WREG	    ;price 2 is higher compare next
		cpfslt	price3pv
		movff	price3pv,WREG
		cpfslt	price4pv
		movff	price4pv,WREG
setmaxend:	movwf	maxprice
		decf	maxprice
		return

clreg:		clrf	WREG
		clrf	credit		;Credit in R1 increments
		clrf	creditt		;Temporary credit for display and hopper
		clrf	minprice	;Lowest vend price
		clrf	maxprice	;Turn off coin mech when this is reached
		clrf	vendenable	;Bit0=mot1 bit1=mot2 bit2=mot3 bit3=mot4 set means vend
		clrf	vendcost	;Store cost of current vend
		clrf	vendflags	;bit 0=still credit bit 1=error bit 2=display incoin bit 3=nochange bit4=credisplay
		clrf	sensor		;Digital value of sensor volts
		clrf	sensflags	;bit 0=1 sensor interupt bit 1=1 timer end
		clrf	dspaddr		;Display digit address holder
		clrf	T0CON
		clrf	T1CON
		clrf	T2CON
		clrf	T3CON
		return


initdisplay:	Call    delay5ms
                movlw   0x04                ;Repeat 8 bit init three times
                movwf   loopr
initdsp8:       movlw   0x03                ;Msb is E on set 4 bit mode,high nibble
		call	dispw1
		call	delay5ms
		decf	loopr
		bnz	initdsp8
init8busy:	call	rbusy
		btfsc	busy, 7
		bra	init8busy
		movlw	0x02
		call	dispw1		    ;After this instruction is 4 bit mode
		nop
init4busy:	call	rbusy
		btfsc	busy, 7		    ;Read the busy flag
		bra	init4busy
initdisp:	movlw   0x02                ;Msb is enable on lower nibble is control
		call	dispw1
		nop
		movlw	0x08
		call	dispw1
		nop
                call    dispset		    ;4 bit type can only set once
		nop
initdisp4:	call	displayoff
		nop
		call    dispclear
		nop
		call	dispentry
		nop
                call    displayon
		nop
                return

dispset:	call	rbusy		    ;Read instruction for busy flag
		incf	loopr		    ;Address 0x87
		btfsc	busy, 7		    ;Bit 7 = busy flag
		bra	dispset		    ;Reread if busy flag set
		movlw	dispfunc	    ;4 bit mode, 1 line, 5x8 character
		call	dispctlwr	    ;Write instruction
		return

displayoff:	call	rbusy		    ;Read instruction for busy flag
		btfsc	busy, 7		    ;Bit 7 = busy flag
		bra	displayoff	    ;Reread if busy flag set
		movlw	0x08		    ;Display off instruction 0x08
		call	dispctlwr	    ;Write instruction
		return

displayon:	call	rbusy		    ;Read instruction for busy flag
		btfsc	busy, 7		    ;Bit 7 = busy flag
		bra	displayon	    ;Reread if busy flag set
		movlw	dispon		    ;Display on instruction 0x0C
		call	dispctlwr	    ;Write instruction
		return

dispentry:	call	rbusy		    ;Read instruction for busy flag
		btfsc	busy, 7		    ;Bit 7 = busy flag
		bra	dispentry	    ;Reread if busy flag set
		movlw	dispnormal	    ;Increment, right, noshift
		call	dispctlwr	    ;Write instruction
		return

dispclear:	call	rbusy		    ;Read instruction for busy flag
		btfsc	busy, 7		    ;Bit 7 = busy flag
		bra	dispclear	    ;Reread if busy flag set
		clrf	dspaddr		    ;digit address location to zero
		movlw	dispclr		    ;Display clear instruction
		call	dispctlwr	    ;Write instruction
		return

disphome:	call	rbusy		    ;Read instruction for busy flag
		btfsc	busy, 7		    ;Bit 7 = busy flag
		bra	disphome	    ;Reread if busy flag set
		movlw	dsphome		    ;Display home instruction
		call	dispctlwr	    ;Write instruction
		return

displine2:	call	rbusy		    ;Read instruction for busy flag
		btfsc	busy, 7		    ;Bit 7 = busy flag
		bra	displine2	    ;Reread if busy flag set
		movlw	dispaddr	    ;Display address upper 8 digits
		movwf	dspaddr		    ;Current digit address location
		call	dispctlwr	    ;Write instruction
		return

displine1:	call	dispctlrd	    ;Read instruction for busy flag
		btfsc	busy, 7		    ;Bit 7 = busy flag
		bra	displine1	    ;Reread if busy flag set
		movlw	0x80	    	    ;Display first digit address
		call	dispctlwr	    ;Write instruction
		return

dispstring:	tblrd*+
		movff	TABLAT, loopr	    ;Move string length to loopr
dispchar1:	incf	TBLPTRL
		tblrd*+
		movff	TABLAT, WREG
		call	charwrite
		decf	loopr
		bnz	dispchar1
		nop
		return

charwrite:	call	rbusy		    ;Write 1 WREG char to dspaddr digit
		btfsc	busy, 7		    ;Bit 7 = busy flag
		bra	charwrite	    ;Reread if busy flag set
		movff	WREG, dispbyte
		call	dispdtrwr
		movlw	0x08
		cpfseq	dspaddr
		return
		call	displine2	    ;Skip to upper 8 digits		;
		return

dispinitial:	movlw	"A"
		movff	WREG, dispbyte	    ;Store in dispbyte
		call	disphome
dispiniti:	call	rbusy		    ;Read instruction for busy flag
		btfsc	busy, 7		    ;Bit 7 = busy flag
		bra	dispiniti	    ;Reread if busy flag set
		movff	dispbyte, WREG
		call	dispdtrwr
		incf	dispbyte
		movlw	"I"
		xorwf	dispbyte,0
		bnz	dispini2
		call	displine2	    ;Damned display writes 8 char only
dispini2:	movlw	"P"
		cpfsgt	dispbyte
		bra	dispiniti
		return

dispw1:		movwf	LATB		    ;Write nibble in WREG to display
                bsf	LATB, 4		    ;Enable on
		call	dlynop
		bcf	LATB, 4		    ;Enable off
		call	dlynop
		return

dispctlwr:                                  ;Write control, WREG contains the value to be written
		movff	WREG, WREG_TEMP	    ;save working register
		movlw	0x00		    ;Write instruction R/W 0 & RS=0 0x0
		bra	displaywr
dispdtrwr:	movff	WREG, WREG_TEMP	    ;save working register
		incf	dspaddr		    ;Next digit address
		movlw	0x04		    ;Write data R/W low & RS high 0x04
displaywr:	movwf	LATE		    ;Set RS high or low for write
		movff	WREG_TEMP, WREG	    ;Restore WREG
		swapf	WREG		    ;exchange nibbles
		bcf	WREG, 4		    ;set E bit low
		call	dispw1		    ;Write msn first
		movff	WREG_TEMP, WREG	    ;Restore WREG
		andlw	0x0f		    ;Clear upper 4 bits
		call	dispw1
		nop
		return

dispctlrd:	pushwr			    ;Read control, busy flag = bit 7 Return in WREG
		movlw	0x02		    ;Read instruction R/W high & RS low
		bra	displayrd
dispdtrrd:	movlw	0x06		    ;Read data R/W high & RS high
displayrd:	movwf	LATE		    ;Set up for read
		movlw	0xef		    ;Set port b lower nibble to read
		movwf	TRISB
		bsf	LATB, 4		    ;Set E high
		call	dlynop
		movff	PORTB, WREG	    ;Transfer hi nibble to WREG low nibble
		bcf	LATB, 4		    ;Set E low
		swapf	WREG		    ;Move low nibble to upper
		andlw	0xf0		    ;Set lower nibble to zero
		movff	WREG, busy	    ;Store in WREG_TEMP
		bsf	LATB, 4		    ;Set E high for lower nibble
		call	dlynop
		movff	PORTB, WREG	    ;Transfer low nibble to WREG low nibble
		bcf	LATB, 4		    ;Set E low
		andlw	0x0f		    ;Clear upper bits
		iorwf	busy		    ;Combine upper nibble into WREG_TEMP
		movlw	0xe0
		movwf	TRISB		    ;Revert portb to output
		clrf	LATB
		popwr			    ;Return result in busy
		return

delay5ms:	call	timer5ms
delay5ms1:	btfss	INTCON,2
		bra	delay5ms1
		return

delay1s:	call	timer1s
delay1s1:	btfss	INTCON,2
		bra	delay1s1
		return

delay.5s:	call	timer.5s
delay.5s1:	btfss	INTCON,2
		bra	delay.5s1
		return

delay.2s:	call	timer.2s
delay.2s1:	btfss	INTCON,2
		bra	delay.2s1
		return

delay.1s:	call	timer.1s
delay.1s1:	btfss	INTCON,2
		bra	delay.1s1
		return

timernote:	bcf	LATC,7		    ;Disable coin mech
		movlw	0x6D		    ;150mSec timer
		movwf	TMR0H
		movlw	0X84
		movwf	TMR0L
		movlw	timeron4	    ;Use x4 prescale
		goto	settimer1
		

timer16s:	clrf	TMR0L		    ;2.097152 seconds
		clrf	TMR0H		    ;
		goto	settimerh

timer4s:	clrf	TMR0L		    ;4,194304 seconds
		clrf	TMR0H		    ;
		goto	settimer2

timer1s:	movlw	0x80		    ;1048mSec preset.
		movwf	TMR0H
		movlw	0X00
		movwf	TMR0L
		goto	settimer

timer1.6s:	movlw	sec1.6		    ;1600mSec preset.
		movwf	TMR0H
		movlw	sec1.6l
		movwf	TMR0L
		goto	settimer

timer2s:	movlw	0x80		    ;1048mSec preset.
		movwf	TMR0H
		movlw	0X00
		movwf	TMR0L
		goto	settimer2

timer.5s:	movlw	0xC0		    ;524mSec preset.
		movwf	TMR0H
		movlw	0X00
		movwf	TMR0L
		goto	settimer

timer.2s:	movlw	0xE6		    ;209.644mSec preset.
		movwf	TMR0H
		movlw	0X61
		movwf	TMR0L
		goto	settimer

timer.1s:	movlw	0xF3		    ;104.832mSec preset.
		movwf	TMR0H
		movlw	0X34
		movwf	TMR0L
		goto	settimer

timer5ms:	movlw	0xEC		    ;5mSec preset.
		movwf	TMR0H
		movlw	0X78
		movwf	TMR0L
		goto	settimerm

settimerm:	movlw	timeronm
		bra	settimer1	    ;No prescale, 16 bit mode
settimer2:	movlw	timeron2	    ;Divide by 64
	        bra	settimer1
settimerh:	movlw	timeronh	    ;Divide by 256
	        bra	settimer1
settimer:	movlw	timeron		    ;Divide by 32
settimer1:	movwf	T0CON		    ;
		clrf	INTCON
		movlw	0x20
		movwf	INTCON
		movlw	0x04
		movwf	INTCON2
		bsf	T0CON,7		    ;Start the timer, monitor INTCON, TMR0IF
		return

eewrite:	;MOVLW	DATA_EE_ADDR	    ;Taken from PIC data sheet
		;MOVWF	EEADR		    ;Values already set up
		;MOVLW	DATA_EE_DATA	    ;
		;MOVWF	EEDATA		    ;Data Memory Value to write
		BCF	EECON1, EEPGD	    ;Point to EEPROM DATA memory
		BCF	EECON1, CFGS	    ;Access program FLASH or Data EEPROM memory
		BSF	EECON1, WREN	    ;Enable writes
		BCF	INTCON, GIE	    ;Disable interrupts not used but safe
		MOVLW	0x55
		MOVWF	EECON2		    ;Write 55h
		MOVLW	0xAA
		MOVWF	EECON2		    ;Write AAh
		BSF	EECON1, WR	    ;Set WR bit to begin write
;		BSF	INTCON, GIE	    ;Enable interrupts, not used ATM
		nop
		nop
		nop
		nop
eewrite1:	btfsc	EECON1, WR	    ;Poll EEIF bit until set for end of write
		bra	eewrite1
;		BCF	INTCON, GIE	    ;Disable interrupts not used but safe
		BCF	EECON1, WREN	    ;Disable writes on write complete (EEIF set)
;		bcf	IPR2, EEIF	    ;Clear EEIF bit in IPR2
		return

;Display strings
;-------------------------------------------------------------------------------
		code_pack 0x1000
test:	data	0x10,"Testing=01234567"
	data	0x0C," Insert Cash"
	data	0x08,"Credit R"
	data	0x0F,"Push Lit Button"
	data	0x0A,"   To Vend"
	data	0x0D," Service Mode"
	data	0x07,"Price R"
	data	0x05,"Vends "
	data	0x05,"Cash "
	data	0x0C,"TotalCash"
	data	0x07," Test"
	data	0x0B,"Test Mode"	;0xD02
	data	0x0A," Vend Test"
	data	0x0F," Coin/Note Test"
	data	0x0F," Out Of Service"
	data	0x0F,"No Change Given"
	data	0x0D," Buy Another?"
	data	0x0D," Clear Totals"
	data	0x0D,"  Sensor Test"
	data	0x0C,"  Pad  "		;Pad to 0xE00
	data	0x0C," Please Wait"
	data	0x0D,"   Set Prices"
	data	0x0E,"  Out Of Stock"
	data	0x0D,"   Audit Mode"
	data	0x0F," Display Totals"
	data	0x0D,"Hopper Coin R"
	data	0x0A,"Coins Out "

;End of program

	END
 




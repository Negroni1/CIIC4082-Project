.segment "HEADER"           ; Assembler Directive (.), HEADER
.byte $4e, $45, $53, $1a    ; Magic string that always begins an iNES header
.byte $02                   ; Number of 16KB PRG-ROM banks
.byte $01                   ; Number of 8KB CHR-ROM banks
.byte %00000001             ; Vertical mirroring, no save RAM, no mapper
.byte %00000000             ; No special-case flags set, no mapper
.byte $00                   ; No PRG-RAM present
.byte $00                   ; NTSC format

.segment "ZEROPAGE"
	frogX: 					.res 1	; Reserve 1 byte for the sprite position in X
	frogY: 					.res 1	; Reserve 1 byte for the sprite position in X
	frogDirection: 			.res 1	; Reserve 1 byte for the sprite direction
	animationState:			.res 1	; Reserve 1 byte for the animation state for the FSM
	tick:					.res 1	; Reserve 1 byte for the a tick counter (frame counter)
	frogOffSet:				.res 1	; Reserve 1 byte for the frog off-set, to be able to add multiple frogs on the screen (on different memory)
	directionOffSet:		.res 1	; Reserve 1 byte for the frog direction off-set, to able to have multiple frogs w/ different direction/orientation

.segment "STARTUP"

.segment "CODE"
.proc irq_handler       ; IRQ (Interrup Request) can be triggered by the NES’ sound processor or from certain types of cartridge hardware.
    RTI                 ; Return from Interrupt
.endproc

.proc nmi_handler       ; NMI (“Non-Maskable Interrupt”) occurs when the PPU starts preparing the next frame of graphics, 60 times per second.
    LDA #$00            ; Load 00 into A
    STA $2003           ; Store 00 into the OAM Address, is used to set where in OAM we want to write to; for all of our projects (and for most commercial games), this will always be $00, the beginning of the OAM block
                        ; This tells the PPU to prepare for a transfer to OAM starting at byte zero
    LDA #$02            ; Load 02 into A
    STA $4014           ; Store 02 into OAMDMA, initiates the transfer of an entire page of memory into OAM
                        ; This tells the PPU to initiate a high-speed transfer of the 256 bytes from $0200-$02ff into OAM
    LDA #$00            ; A = 0
    STA $2005           ; PPU Scroll = 0
    STA $2005           

	JSR update			; Call Update function
	JSR draw			; Call Draw function

    RTI                 ; Return from Interrupt
.endproc

.proc reset_handler     ; Set up the system when it is first turned on
    SEI                 ; Set Interrupt ignore bit, thus anything that would trigger an IRQ event does nothing instead
    CLD                 ; Clear Decimal mode bit
    LDX #$40    
    STX $4017
    LDX #$FF
    TXS
    INX                 ; X = $00
    STX $2000           ; PPUCTRL = 0, we turn off NMIs
    STX $2001           ; PPUMASK = 0, disable rendering to the screen during startup
    STX $4010           ; 4010 = 0, turns off DMC IRQs, to ensure that we don’t draw random garbage to the screen
    BIT $2002
	vblankwait:             ; fetches the PPU’s status from PPUSTATUS, until PPU is ready
		BIT $2002
		BPL vblankwait

	vblankwait2:
		BIT $2002
		BPL vblankwait2

	LDA #$00				; A = 0
	STA frogX				; Set frog X to 0
	STA frogY				; Set frog Y to 0
	STA animationState		; Set animation state to 0
	STA tick				; Set tick to 0
	STA frogOffSet			; Set frog memory allocation offset to 0
	STA directionOffSet		; Set frog direction offset to 0
    STA frogDirection   	; Set frog Direction to 0

    JMP main				; Go to main
.endproc

.proc update 	; ---------------------+
	LDA tick	; Load tick
	CLC			; Clear carry flag
	ADC #$01   	; Add 1 to tick
	STA tick	; Store new tick
.endproc

.proc draw ; ------------------------------------------------------------------------------------------+
	; SAVE REGISTER INTO THE STACK
	PHP						; Push the Processor Status Register onto the stack
	PHA						; Push the Accumulator Register onto the stack
	TXA						; Transfer X into the Accumulator
	PHA						; Push X (the accumulator register) onto the stack
	TYA						; Transfer Y into the Accumulator
	PHA						; Push Y (the accumulator register) onto the stack

	; DRAW FROG LEFT
	LDA #$01          		
    STA frogDirection   	; Set frogDirection to 01 (Left)
    LDA #$70           		
    STA frogX				; Set frog X position to 70
    LDA #$60          		
    STA frogY				; Set frog Y position to 60
	JSR drawAnimation		; Call drawAnimation

	; DRAW FROG DOWN
	LDA #$0D          		
    STA frogDirection   	; Set frogDirection to 0D (Down)
	LDA #$80           		
    STA frogX				; Set frog X position to 80
    LDA #$60           		
    STA frogY				; Set frog Y position to 60
	JSR drawAnimation		; Call drawAnimation

	; DRAW FROG UP
	LDA #$19
    STA frogDirection   	; Set frogDirection to 19 (Up)
	LDA #$70
    STA frogX				; Set frog X position to 70
    LDA #$70 
    STA frogY				; Set frog Y position to 70
	JSR drawAnimation		; Call drawAnimation

	; DRAW FROG DOWN
	LDA #$25          	
    STA frogDirection		; Set frogDirection to 25 (Right)
	LDA #$80
    STA frogX				; Set frog X position to 80
    LDA #$70
    STA frogY				; Set frog Y position to 70
	JSR drawAnimation		; Call drawAnimation

	; RESTORE REGISTERS & RETURN
	PLA						; Pull Y of the stack and place it into the accumulator	register
	TAY						; Restore/Transfer the accumulator into Y
	PLA						; Pull X of the stack and place it into the accumulator	register
	TAX						; Restore/Transfer the accumulator into X
	PLA						; Pull the top value of the stack and place it into the accumulator register
	PLP						; Pull the top value of the stack and place it into the  processor status register
	RTS						; Return from subroutine
.endproc

.proc drawAnimation ; ------------------------------------------------------------------------------------------+
	; SAVE REGISTER INTO THE STACK
	PHP							; Push the Processor Status Register onto the stack
	PHA							; Push the Accumulator Register onto the stack
	TXA							; Transfer X into the Accumulator
	PHA							; Push X (the accumulator register) onto the stack
	TYA							; Transfer Y into the Accumulator
	PHA							; Push Y (the accumulator register) onto the stack

	LDA tick					; Load current tick
	CMP #$1E
	BEQ changeAnimation			; Go to changeAnimation if tick is 30
	JMP continue				; Else continue (go to drawFrog)
	changeAnimation:
		LDA #$00
		STA tick				; Set tick to 0
		LDA animationState
		CLC
		ADC #$01
		STA animationState 		; Add 1 to the animation state
		LDA animationState
		CMP #$01
		BEQ stateIncrement		; If animation state is 1 go to state increment
		CMP #$03
		BEQ stateIncrement		; If animation state is 3 go to state increment
		CMP #$05
		BEQ stateDecrement		; If animation state is 5 go to state decrement
		JMP continue

		stateIncrement:
			LDA directionOffSet
			CLC
			ADC #$04     
			STA directionOffSet	; Increase direction offset by four (1 tile)
			LDA animationState
			CLC
			ADC #$01
			STA animationState 	; Add 1 to animation state (transition state, with for another tick)
			JMP continue

		stateDecrement:
			LDA directionOffSet
			CLC
			SBC #$07   
			STA directionOffSet	; Set direction offset to original direction
			LDA #$00
			STA animationState 	; Set state to first (original state)
			JMP continue
    
	continue:
		JSR drawFrog       ; Draw the frog

    ; RESTORE REGISTERS & RETURN
	PLA						; Pull Y of the stack and place it into the accumulator	register
	TAY						; Restore/Transfer the accumulator into Y
	PLA						; Pull X of the stack and place it into the accumulator	register
	TAX						; Restore/Transfer the accumulator into X
	PLA						; Pull the top value of the stack and place it into the accumulator register
	PLP						; Pull the top value of the stack and place it into the  processor status register
	RTS						; Return from subroutine
.endproc

.proc drawFrog	; ------------------------------------------------------------------------------------------+
	; SAVE REGISTER INTO THE STACK
	PHP						; Push the Processor Status Register onto the stack
	PHA						; Push the Accumulator Register onto the stack
	TXA						; Transfer X into the Accumulator
	PHA						; Push X (the accumulator register) onto the stack
	TYA						; Transfer Y into the Accumulator
	PHA						; Push Y (the accumulator register) onto the stack

	LDA frogDirection
	CLC
	ADC directionOffSet
	STA frogDirection		; Add direction offset to the frog direction

	LDX frogOffSet			; Load frog offset to X

	; DRAW UPPER LEFT TILE
	LDA frogY
	STA $0200, X        
	LDA frogDirection
	STA $0201, X      
	LDA #$00
	STA $0202, X        
	LDA frogX
	STA $0203, X  

	; DRAW UPPER RIGHT TILE
	LDA frogY 
	STA $0204, X    
	LDA frogDirection
	CLC
	ADC #$01     
	STA $0205, X      
	LDA #$00
	STA $0206, X         
	LDA frogX
	CLC
	ADC #$08
	STA $0207, X 

	; DRAW LOWER LEFT TILE
	LDA frogY		
	CLC
	ADC #$08
	STA $0208, X        
	LDA frogDirection
	CLC
	ADC #$02
	STA $0209, X      
	LDA #$01
	STA $020A, X         
	LDA frogX
	STA $020B, X

	; DRAW LOWER RIGHT TILE
	LDA frogY				
	CLC
	ADC #$08
	STA $020C, X        
	LDA frogDirection
	CLC
	ADC #$03
	STA $020D, X      
	LDA #$01
	STA $020E, X         
	LDA frogX				
	CLC
	ADC #$08
	STA $020F, X

	LDA frogOffSet
	CLC
	ADC #$10
	STA frogOffSet 			; Increase frog offset by 16  

	; RESTORE REGISTERS & RETURN
	PLA						; Pull Y of the stack and place it into the accumulator	register
	TAY						; Restore/Transfer the accumulator into Y
	PLA						; Pull X of the stack and place it into the accumulator	register
	TAX						; Restore/Transfer the accumulator into X
	PLA						; Pull the top value of the stack and place it into the accumulator register
	PLP						; Pull the top value of the stack and place it into the  processor status register
	RTS						; Return from subroutine
.endproc

.proc main ; -------------------------------------------------------------------------+
    LDX $2002           ; Load PPU Status into X
    LDX #$3f            ; Load 3f into X
    STX $2006           ; Store 3f in PPU Address
    LDX #$00            ; Load 00 into X
    STX $2006           ; Store 00 in PPU Address, such that we have 3f00 (the first color of the first palette) in PPU Address

    ; WRITE PALETTES -----------------------------------------------------------------+
    load_palettes:
        LDA palettes, X     ; Load palettes into X
        STA $2007           ; Store X into PPU data
        INX                 ; Increase X
        CPX #$18            ; Compare X, If X > 24 (6 patterns tables)
        BNE load_palettes   ; 
    
	vblankwait:         	; wait for another vblank before continuing
        BIT $2002       	; 2002 = PPU status
        BPL vblankwait
        LDA #%10010000  	; turn on NMIs, sprites use first pattern table
        STA $2000       	; Store A in PPU Control
        LDA #%00011110  	; turn on screen
        STA $2001       	; Store A in PPU Mask

    forever:
        JMP forever
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"               ; read-only data
    palettes:
		; BACKGROUND PALETTE -----------------------------------------------------------+
		.byte $2B, $07, $17, $3A    ; green brown, brown, light green
		.byte $2B, $0F, $0B, $3A    ; green, dark green, light green
		.byte $2B, $1C, $2C, $3C    ; green blue, blue, light blue
		.byte $2B, $1C, $17, $3A    ; green blue, brown, light green
		; SPRITE PALETTE  --------------------------------------------------------------+
		.byte $2B, $0f, $3A, $20    ; green, black, green, white
		.byte $2B, $0f, $1B, $3B    ; green, black, green, green

.segment "CHARS"
.incbin "graphics.chr"
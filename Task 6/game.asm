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

	temp: 						.res 1
	scroll_offset:				.res 1
	level_change:				.res 1
	background_offset_low:		.res 1
	background_offset_up:		.res 1
	background_tile_offset:		.res 1
	tick_counter:				.res 1
	counter_1:					.res 1
	counter_2:					.res 1
	counter_3:					.res 1
	counter_offset:				.res 1
	stageClearState:			.res 1
	timer_stage1_counter1:		.res 1
	timer_stage1_counter2:		.res 1
	timer_stage1_counter3:		.res 1
	counter_Y:					.res 1
	counter_X:					.res 1

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

	JSR update	
	JSR drawCounter
	JSR drawGameOver
	JSR drawStageClear
	; JSR draw
 
	LDA scroll_offset
	STA $2005
	LDA #$00
	STA $2005
	LDX #$00

	LDA level_change
	CMP #$01
	BNE continue
	JSR drawBackground
	continue:
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

	LDX #$00
	STX temp
	STX scroll_offset
	STX level_change
	STX background_offset_low
	STX background_tile_offset
	STX counter_offset
	STX stageClearState
	STX counter_Y
	LDX #$20
	STX background_offset_up
	LDX #$01
	STX counter_3
	LDX #$02
	STX counter_2
	LDX #$03
	STX counter_1
	LDX #$D0
	STX counter_X

	LDA #$00				; A = 0
	STA frogX				; Set frog X to 0
	STA frogY				; Set frog Y to 0
	STA animationState		; Set animation state to 0
	STA tick				; Set tick to 0
	STA frogOffSet			; Set frog memory allocation offset to 0
	STA directionOffSet		; Set frog direction offset to 0
    STA frogDirection   	; Set frog Direction to 0
	
	
    JMP main
.endproc
.proc draw ; ------------------------------------------------------------------------------------------+
	; SAVE REGISTER INTO THE STACK
	PHP						; Push the Processor Status Register onto the stack
	PHA						; Push the Accumulator Register onto the stack
	TXA						; Transfer X into the Accumulator
	PHA						; Push X (the accumulator register) onto the stack
	TYA						; Transfer Y into the Accumulator
	PHA						; Push Y (the accumulator register) onto the stack

	; DRAW FROG DOWN
	LDA #$0D          		
    STA frogDirection   	; Set frogDirection to 0D (Down)
	LDA #$70          		
    STA frogX				; Set frog X position to 80
    LDA #$20           		
    STA frogY				; Set frog Y position to 60
	JSR drawFrog		; Call drawAnimation


	; RESTORE REGISTERS & RETURN
	PLA						; Pull Y of the stack and place it into the accumulator	register
	TAY						; Restore/Transfer the accumulator into Y
	PLA						; Pull X of the stack and place it into the accumulator	register
	TAX						; Restore/Transfer the accumulator into X
	PLA						; Pull the top value of the stack and place it into the accumulator register
	PLP						; Pull the top value of the stack and place it into the  processor status register
	RTS						; Return from subroutine
.endproc
.proc update
	; SAVE REGISTER INTO THE STACK
	PHP						; Push the Processor Status Register onto the stack
	PHA						; Push the Accumulator Register onto the stack
	TXA						; Transfer X into the Accumulator
	PHA						; Push X (the accumulator register) onto the stack
	TYA						; Transfer Y into the Accumulator
	PHA						; Push Y (the accumulator register) onto the stack

	LDA tick_counter
	CMP #$3C
	BEQ change_counter
	CLC
	ADC #$01
	STA tick_counter
	JMP controller
	change_counter:
		LDA stageClearState
		CMP #$02
		BEQ controller
		LDA #$00
		STA tick_counter
		LDA counter_1
		CMP #$00
		BEQ change_counter2
		LDA counter_3
		DEC counter_1
		JMP controller
		change_counter2:
			LDA counter_2
			CMP #$00
			BEQ check
			JMP dec_
			check:
			LDA counter_3
			CMP #$00
			BEQ to_continue	
			dec_:
			LDA #$09
			STA counter_1
			LDA counter_2
			CMP #$00
			BEQ change_counter3
			DEC counter_2
			JMP controller
			change_counter3:
				LDA counter_3
				CMP #$00
				BEQ continue
				LDA #$09
				STA counter_2
				LDA counter_3
				CMP #$00
				BEQ continue
				DEC counter_3
			JMP controller
			to_continue:
				JMP continue

	controller:
		LDA $4016   ; Read controller 1
		STA temp    ; Store state in temp variable
		LDA #$01
		STA $4016   ; Strobe controller
		LDA #$00
		STA $4016   ; Clear strobe, begin reading button states

		LDY #$08    ; Prepare to read 8 buttons
		LDA #$00    ; Clear A to start

		read_buttons:
			LDA $4016       ; Read button state
			LSR             ; Shift right, moving the button's state into carry
			ROL temp        ; Rotate Left through Carry to move button state into temp
			DEY
			BNE read_buttons

		; Check Right (bit 7 of temp)
		LDA temp
		AND #%00000001  ; Isolate Right button
		BEQ arrow_left  ; If 0, button not pressed, check next
		LDA scroll_offset
		CMP #$FF
		BEQ continue
		CLC
		ADC #$01
		STA scroll_offset
		JMP continue

		arrow_left:
			LDA temp
			AND #%00000010  ; Isolate Left button
			BEQ button_start    ; If 0, button not pressed, check next
			LDA scroll_offset
			CMP #$00
			BEQ continue
			DEC scroll_offset
			JMP continue

		button_start:
			LDA temp
			AND #%00010000
			BEQ continue
			; TODO: CHAGE MAP
			LDA level_change
			EOR #%01
			STA level_change
			LDA #$00
			STA scroll_offset

			LDA stageClearState
			CLC
			ADC #$01
			STA stageClearState

			LDA stageClearState
			CMP #$02
			BEQ continue

			LDX #$00
			STX counter_3
			LDX #$06
			STX counter_2
			LDX #$00
			STX counter_1
	
	continue:
	
	; RESTORE REGISTERS & RETURN
	PLA						; Pull Y of the stack and place it into the accumulator	register
	TAY						; Restore/Transfer the accumulator into Y
	PLA						; Pull X of the stack and place it into the accumulator	register
	TAX						; Restore/Transfer the accumulator into X
	PLA						; Pull the top value of the stack and place it into the accumulator register
	PLP						; Pull the top value of the stack and place it into the  processor status register
	RTS						; Return from subroutine
.endproc


.proc drawStageClear
	; SAVE REGISTER INTO THE STACK
	PHP						; Push the Processor Status Register onto the stack
	PHA						; Push the Accumulator Register onto the stack
	TXA						; Transfer X into the Accumulator
	PHA						; Push X (the accumulator register) onto the stack
	TYA						; Transfer Y into the Accumulator
	PHA						; Push Y (the accumulator register) onto the stack

	LDA stageClearState
	CMP #$02
	BNE exit

	LDX #$88
	STX counter_X
	STX counter_Y

	LDX counter_1
	CLC
	ADC timer_stage1_counter1
	

	LDX #$00
	stage_clear:
		LDA stageClear,X
		STA $0200,X
		INX
		CPX #$38
		BNE stage_clear
	
	; LDA #$0D          		
    ; STA frogDirection   	; Set frogDirection to 0D (Down)
	; LDA #$70          		
    ; STA frogX				; Set frog X position to 80
    ; LDA #$20           		
    ; STA frogY				; Set frog Y position to 60
	; JSR drawAnimation		; Call drawAnimation


	exit:

	; RESTORE REGISTERS & RETURN
	PLA						; Pull Y of the stack and place it into the accumulator	register
	TAY						; Restore/Transfer the accumulator into Y
	PLA						; Pull X of the stack and place it into the accumulator	register
	TAX						; Restore/Transfer the accumulator into X
	PLA						; Pull the top value of the stack and place it into the accumulator register
	PLP						; Pull the top value of the stack and place it into the  processor status register
	RTS						; Return from subroutine
.endproc

.proc drawGameOver
	; SAVE REGISTER INTO THE STACK
	PHP						; Push the Processor Status Register onto the stack
	PHA						; Push the Accumulator Register onto the stack
	TXA						; Transfer X into the Accumulator
	PHA						; Push X (the accumulator register) onto the stack
	TYA						; Transfer Y into the Accumulator
	PHA						; Push Y (the accumulator register) onto the stack

	LDA #$00
	CLC
	ADC counter_1
	CLC	
	ADC counter_2
	CLC
	ADC counter_3
	CMP #$00
	BNE exit


	LDX #$00
	game_over:
		LDA gameOver,X
		STA $0200,X
		INX
		CPX #$20
		BNE game_over

	exit:
	; RESTORE REGISTERS & RETURN
	PLA						; Pull Y of the stack and place it into the accumulator	register
	TAY						; Restore/Transfer the accumulator into Y
	PLA						; Pull X of the stack and place it into the accumulator	register
	TAX						; Restore/Transfer the accumulator into X
	PLA						; Pull the top value of the stack and place it into the accumulator register
	PLP						; Pull the top value of the stack and place it into the  processor status register
	RTS						; Return from subroutine
.endproc

.proc drawCounter
	; SAVE REGISTER INTO THE STACK
	PHP						; Push the Processor Status Register onto the stack
	PHA						; Push the Accumulator Register onto the stack
	TXA						; Transfer X into the Accumulator
	PHA						; Push X (the accumulator register) onto the stack
	TYA						; Transfer Y into the Accumulator
	PHA						; Push Y (the accumulator register) onto the stack

	LDX counter_offset
	
	LDA counter_Y
	STA $0200, X    
	LDA counter_3
	CLC
	ADC #$40
	STA $0201, X     
	LDA #$02
	STA $0202, X         
	LDA counter_X
	STA $0203, X  

	LDA counter_Y
	STA $0204, X         
	LDA counter_2
	CLC
	ADC #$40
	STA $0205, X      
	LDA #$02
	STA $0206, X         
	LDA counter_X
	CLC
	ADC #$08
	STA $0207, X 


	LDA counter_Y
	STA $0208, X       
	LDA counter_1
	CLC
	ADC #$40
	STA $0209, X      
	LDA #$02
	STA $020A, X         
	LDA counter_X
	CLC
	ADC #$10
	STA $020B, X  

	LDA counter_offset
	CLC
	ADC #$10
	STA counter_offset 			; Increase frog offset by 16  
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
.proc drawBackground
    ; Save register states to the stack
    PHP
    PHA
    TXA
    PHA
    TYA
    PHA
    ; WRITE BACKGROUND DATA ---------------------------------------------------------------+
	LDX 00
	STX background_tile_offset
	; ; BACKGROUND 1 ----------------------------------------------------------------------+
	; ; ; UPPER ---------------------------------------------------------------------------+
	LDA level_change
	CMP #$01
	BNE bg
	vblankwait_off:         ; wait for another vblank before continuing
        BIT $2002       ; 2002 = PPU status
        BPL vblankwait_off
		LDA #%00000000  ; turn on NMIs, sprites use first pattern table
		STA $2000       ; Store A in PPU Control
		LDA #%00000000  ; turn on screen
		STA $2001       ; Store A in PPU Mask

		LDA #$20
		STA background_offset_up
		LDA #$00
		STA background_offset_low

	bg:
	LDX #$00
	STX background_tile_offset
	background1:
		LDA background_offset_up
		STA $2006
		LDA background_offset_low
		STA $2006							; Upper Tiles address

		; LDX #$00
		LDX background_tile_offset
		load_background1Upper:           	; Iterate through the BACKGROUND to draw UPPER TILES
			; UPPER LEFT
			LDA level_change
			CMP #$01
			BNE screen1_up
			LDA background_screen3, X 
			JMP screen1_con
			screen1_up:
			LDA background_screen1, X 
			screen1_con:
			STA $2007           			; Store X into PPUDATA
			; UPPER RIGHT							; Lower Tiles Address
			CLC
			ADC #$01
			STA $2007						; Store X into 2001
			INX
			CPX #$10            			; Compare X, If X == 255 stop the loop
			BNE load_background1Upper
		
		; ; ; LOWER ---------------------------------------------------------------------------+		LDA background_offset_low
		LDA background_offset_low
		CLC
		ADC #$20
		STA background_offset_low

		LDA background_offset_up
		STA $2006
		LDA background_offset_low
		STA $2006							; Lower Tiles Address

		LDX background_tile_offset
		load_background1_lower:           	; Iterate through the BACKGROUND to draw LOWER TILES
			; LOWER LEFT
			LDA level_change
			CMP #$01
			BNE screen1_low
			LDA background_screen3, X 
			JMP screen1_con_low
			screen1_low:
			LDA background_screen1, X 
			screen1_con_low:
			CLC
			ADC #$02						; Sum 2 such that it takes the lower left
			STA $2007           			; Store it into PPUDATA
			; LOWER RIGHT
			CLC
			ADC #$01						
			STA $2007						; Store it into PPUDATA
			INX
			CPX #$10            			; Compare X, If X == 255 stop the loop
			BNE load_background1_lower
		LDA background_tile_offset
		CLC
		ADC #$10
		STA background_tile_offset
		LDA background_offset_low
		CMP #$E0
		BEQ increase_up
		JMP continue
		increase_up:
			LDA background_offset_up
			CLC
			ADC #$01
			STA background_offset_up
			LDA #$00
			STA background_offset_low
			JMP background1
		continue:
		LDA background_offset_low
		CLC
		ADC #$20
		STA background_offset_low

		LDA background_offset_up
		CMP #$24
		BEQ exit1
		JMP background1

	exit1:

	; BACKGROUND 2  -----------------------------------------------------------------------+
	LDX #$00
	STX background_tile_offset
	STX background_offset_low
	LDX #$24
	STX background_offset_up

	background2:
		LDA background_offset_up
		STA $2006
		LDA background_offset_low
		STA $2006							; Upper Tiles address

		; LDX #$00
		LDX background_tile_offset
		load_background2Upper:           	; Iterate through the BACKGROUND to draw UPPER TILES
			; UPPER LEFT
			LDA level_change
			CMP #$01
			BNE screen2_up
			LDA background_screen4, X 
			JMP screen2_con
			screen2_up:
			LDA background_screen2, X 
			screen2_con:
			STA $2007           			; Store X into PPUDATA
			; UPPER RIGHT							; Lower Tiles Address
			CLC
			ADC #$01
			STA $2007						; Store X into 2001
			INX
			CPX #$10            			; Compare X, If X == 255 stop the loop
			BNE load_background2Upper
		; ; ; LOWER ---------------------------------------------------------------------------+		LDA background_offset_low
		LDA background_offset_low
		CLC
		ADC #$20
		STA background_offset_low

		LDA background_offset_up
		STA $2006
		LDA background_offset_low
		STA $2006							; Lower Tiles Address

		LDX background_tile_offset
		load_background2_lower:           	; Iterate through the BACKGROUND to draw LOWER TILES
			; LOWER LEFT
			LDA level_change
			CMP #$01
			BNE screen2_low
			LDA background_screen4, X 
			JMP screen2_con_low
			screen2_low:
			LDA background_screen2, X 
			screen2_con_low:
			CLC
			ADC #$02						; Sum 2 such that it takes the lower left
			STA $2007           			; Store it into PPUDATA
			; LOWER RIGHT
			CLC
			ADC #$01						
			STA $2007						; Store it into PPUDATA
			INX
			CPX #$10            			; Compare X, If X == 255 stop the loop
			BNE load_background2_lower
		; JMP exit2
		LDA background_tile_offset
		CLC
		ADC #$10
		STA background_tile_offset

		LDA background_offset_low
		CMP #$E0
		BEQ increase_up2
		; BEQ exit2
		JMP continue2
		increase_up2:
			LDA background_offset_up
			CLC
			ADC #$01
			STA background_offset_up
			LDA #$00
			STA background_offset_low
			JMP background2
		continue2:
		LDA background_offset_low
		CLC
		ADC #$20
		STA background_offset_low

		LDA background_offset_up
		CMP #$27
		BEQ check_exit
		JMP background2
		check_exit:
			LDA background_offset_low
			CMP #$C0
			BEQ exit2
			JMP background2
	exit2:
		
	; ATTRIBUTE TABLE BACKGROUND 1
	LDA $2002
	LDA #$23
	STA $2006
	LDA #$C0
	STA $2006

	LDX #$00
	load_attribute: 
		LDA level_change
		CMP #$01
		BNE attribute1
		LDA attribute_screen3, X 
		JMP attribute1_con
		attribute1:
		LDA attribute_screen1, X       			; Iterate through the attributes to draw them
		attribute1_con:
		STA $2007         				; Store X into PPUDATA
        INX                 			; Increase X
        CPX #$40            			; Compare X, If X > 192 (12 16bit sprites) stop the loop
        BNE load_attribute

	LDA $2002
	LDA #$27
	STA $2006
	LDA #$C0
	STA $2006

	LDX #$00
	load_attribute2:           ; Iterate through the sprites to draw them
		LDA level_change
		CMP #$01
		BNE attribute2
		LDA attribute_screen4, X 
		JMP attribute2_con
		attribute2:
		LDA attribute_screen2, X       			; Iterate through the attributes to draw them
		attribute2_con:
        STA $2007         ; Store X into 0200
        INX                 ; Increase X
        CPX #$40            ; Compare X, If X > 192 (12 16bit sprites) stop the loop
        BNE load_attribute2

    vblankwait:         ; wait for another vblank before continuing
        BIT $2002       ; 2002 = PPU status
        BPL vblankwait
		LDA #%10010000  ; turn on NMIs, sprites use first pattern table
		STA $2000       ; Store A in PPU Control
		LDA #%00011110  ; turn on screen
		STA $2001       ; Store A in PPU Mask
	LDA #$00
	STA level_change

	PLA
    TAY
    PLA
    TAX
    PLA
    PLP
    RTS
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
        CPX #$20            ; Compare X, If X > 24 (6 patterns tables)
        BNE load_palettes   ; 

	JSR drawBackground
    forever:
        JMP forever
.endproc

.segment "VECTORS"
.addr nmi_handler, reset_handler, irq_handler

.segment "RODATA"               ; read-only data
    palettes:
		; BACKGROUND PALETTE -----------------------------------------------------------+
		.byte $2C, $3A, $17, $2B    ; green brown, brown, light green
		.byte $2C, $0F, $1C, $3B    ; green, dark green, light green
		.byte $2C, $1C, $3C, $2B    ; green blue, blue, light blue
		.byte $2C, $1C, $17, $2B    ; green blue, brown, light green
		.byte $2C, $0f, $28, $20    ; green, black, green, white
		.byte $2C, $0f, $17, $3B    ; green, black, green, green
		.byte $2C, $0f, $25, $20
		.byte $2C, $00, $00, $00

	gameOver:
		.byte $68, $60, $02, $60 	; G
		.byte $68, $61, $02, $70 	; A
		.byte $68, $62, $02, $80 	; M
		.byte $68, $63, $02, $90 	; E
		.byte $78, $65, $02, $60 	; O
		.byte $78, $66, $02, $70 	; V
		.byte $78, $67, $02, $80 	; E
		.byte $78, $68, $02, $90 	; R

	stageClear:
		.byte $68, $50, $02, $60 	; S
		.byte $68, $51, $02, $70 	; T
		.byte $68, $52, $02, $80 	; A
		.byte $68, $53, $02, $90 	; G
		.byte $68, $54, $02, $A0 	; E
		.byte $78, $56, $02, $60 	; C
		.byte $78, $57, $02, $70 	; L
		.byte $78, $58, $02, $80 	; E
		.byte $78, $59, $02, $90 	; A
		.byte $78, $5A, $02, $A0 	; R
		.byte $88, $51, $02, $60	; T
		.byte $88, $69, $02, $68	; I
		.byte $88, $62, $02, $70	; M
		.byte $88, $63, $02, $78	; E


background_screen1:
	.byte $05, $09, $09, $09, $09, $09, $09, $09, $09, $09, $0D, $01, $05, $09, $09, $0D, $11, $1D, $1D, $1D, $15, $1D, $1D, $1D, $1D, $15, $19, $01, $11, $15, $15, $19, $11, $1D, $15, $1D, $1D, $1D, $15, $1D, $1D, $1D, $19, $01, $11, $15, $15, $19, $21, $25, $29, $1D, $2D, $25, $25, $25, $29, $1D, $19, $01, $11, $15, $15, $19, $45, $01, $11, $1D, $19, $01, $05, $09, $35, $1D, $39, $09, $35, $15, $15, $19, $01, $53, $11, $1D, $19, $01, $11, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $19, $01, $01, $11, $1D, $19, $01, $11, $1D, $2D, $25, $29, $1D, $2D, $25, $25, $31, $09, $09, $35, $1D, $19, $01, $11, $1D, $19, $3D, $11, $1D, $19, $01, $01, $01, $1D, $1D, $1D, $1D, $19, $01, $11, $1D, $19, $41, $11, $1D, $19, $01, $01, $01, $25, $25, $25, $25, $31, $01, $11, $1D, $39, $09, $35, $1D, $39, $09, $09, $09, $01, $45, $01, $01, $49, $01, $11, $1D, $15, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $01, $4D, $51, $55, $01, $01, $21, $25, $29, $1D, $2D, $25, $25, $25, $25, $25, $05, $09, $09, $09, $09, $09, $09, $09, $35, $1D, $39, $09, $09, $09, $09, $0D, $11, $1D, $15, $1D, $1D, $15, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $19, $21, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $31, $BA, $AA, $AA, $AA, $FB, $FA, $FA, $AA, $28, $AA, $AA, $AA, $AA, $AA, $AF, $AF	; .byte $11,$1d,$1d,$1d,$15,$1d,$1d,$1d,$1d,$15,$19,$01,$11,$15,$15,$19

attribute_screen1:
	.byte $ba,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$fb,$fa,$fa,$fa,$fa,$aa,$aa,$aa
	.byte $28,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$af,$ab,$af,$af
	.byte $ff,$ff,$ba,$aa,$aa,$aa,$aa,$aa,$22,$0a,$a8,$fa,$ba,$fa,$fa,$fa
	.byte $ba,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f

background_screen2:
.byte $01, $01, $05, $09, $0D, $01, $01, $05, $09, $09, $09, $09, $09, $09, $09, $0D, $45, $01, $11, $1D, $19, $01, $01, $11, $15, $15, $15, $15, $15, $15, $15, $19, $01, $01, $11, $1D, $19, $01, $45, $11, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $19, $01, $01, $11, $1D, $19, $01, $01, $11, $1D, $2D, $25, $25, $25, $29, $1D, $19, $45, $01, $11, $1D, $19, $01, $01, $11, $1D, $19, $45, $01, $4D, $11, $1D, $19, $01, $53, $11, $1D, $19, $01, $01, $11, $1D, $19, $01, $49, $01, $11, $1D, $19, $4C, $01, $11, $1D, $39, $09, $09, $35, $1D, $19, $01, $01, $01, $11, $1D, $19, $55, $01, $11, $1D, $1D, $1D, $1D, $1D, $1D, $19, $53, $55, $01, $11, $1D, $19, $01, $01, $11, $1D, $2D, $25, $29, $1D, $2D, $31, $01, $01, $01, $11, $1D, $19, $09, $09, $35, $1D, $19, $01, $11, $1D, $19, $01, $01, $47, $01, $11, $1D, $19, $1D, $1D, $1D, $1D, $19, $01, $11, $1D, $19, $01, $49, $01, $01, $11, $1D, $19, $25, $25, $25, $25, $31, $01, $11, $1D, $19, $01, $01, $01, $01, $11, $1D, $19, $01, $45, $01, $4D, $01, $55, $11, $1D, $39, $09, $09, $09, $0D, $11, $15, $19, $01, $01, $49, $01, $45, $01, $11, $1D, $1D, $1D, $1D, $1D, $19, $11, $59, $19, $01, $01, $01, $01, $01, $01, $21, $25, $25, $25, $25, $25, $31, $21, $25, $31, $8A, $AA, $AA, $AA, $82, $AA, $EA, $FA, $28, $AA, $AA, $A8, $88, $AA, $AA, $AA
attribute_screen2:
	.byte $8a,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$82,$ea,$aa,$88,$ea,$fa,$fa,$aa
	.byte $28,$aa,$aa,$a8,$aa,$20,$a8,$aa,$88,$aa,$aa,$aa,$aa,$08,$aa,$aa
	.byte $aa,$ae,$af,$bb,$af,$22,$ca,$aa,$fe,$fa,$ba,$ab,$aa,$28,$ee,$aa
	.byte $a2,$82,$82,$aa,$aa,$aa,$ee,$aa,$0a,$0a,$0a,$0f,$0f,$0f,$0f,$0f

background_screen3:
.byte $61, $65, $69, $6D, $79, $7D, $89, $89, $89, $89, $89, $79, $7D, $89, $91, $5D, $61, $65, $69, $71, $81, $85, $8D, $8D, $8D, $8D, $8D, $81, $85, $8D, $B1, $5D, $61, $65, $99, $75, $75, $75, $75, $75, $75, $75, $75, $75, $75, $A1, $B1, $5D, $61, $65, $99, $9D, $9D, $9D, $9D, $9D, $9D, $9D, $9D, $9D, $99, $69, $B1, $5D, $61, $65, $69, $B9, $A9, $A9, $A9, $A9, $A9, $A9, $A9, $BD, $65, $69, $B1, $5D, $61, $65, $69, $B1, $5D, $C9, $89, $79, $7D, $89, $91, $61, $65, $69, $B1, $5D, $61, $65, $69, $B1, $5D, $61, $8D, $81, $85, $8D, $B1, $61, $65, $69, $B1, $5D, $61, $65, $69, $B1, $5D, $61, $CD, $75, $75, $A1, $B1, $61, $65, $69, $6D, $89, $61, $65, $69, $B1, $5D, $61, $65, $99, $99, $69, $D1, $D5, $65, $69, $71, $8D, $61, $65, $69, $B1, $5D, $61, $65, $99, $99, $69, $B1, $61, $65, $99, $75, $75, $61, $65, $69, $6D, $89, $C1, $65, $99, $99, $69, $B1, $61, $A5, $9D, $9D, $9D, $61, $65, $69, $71, $8D, $C5, $65, $99, $99, $69, $B1, $95, $A9, $A9, $A9, $A9, $61, $65, $99, $75, $75, $75, $99, $99, $99, $69, $B1, $5D, $5D, $5D, $5D, $5D, $61, $65, $9D, $9D, $9D, $9D, $9D, $9D, $9D, $B5, $B1, $5D, $5D, $5D, $5D, $5D, $95, $A9, $A9, $A9, $A9, $A9, $A9, $A9, $A9, $A9, $AD, $5D, $5D, $5D, $5D, $5D, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55
attribute_screen3:
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$05,$05,$05,$05,$05,$05,$05,$05
background_screen4:
.byte $5D, $C9, $89, $89, $79, $7D, $89, $89, $91, $C9, $89, $89, $79, $7D, $89, $91, $5D, $61, $8D, $8D, $81, $85, $8D, $8D, $B1, $61, $8D, $8D, $81, $85, $8D, $B1, $5D, $61, $CD, $75, $75, $75, $75, $A1, $B1, $61, $CD, $75, $75, $75, $A1, $B1, $5D, $61, $65, $99, $9D, $9D, $99, $69, $B1, $61, $65, $99, $9D, $9D, $B5, $B1, $5D, $61, $65, $69, $B9, $BD, $65, $69, $B1, $61, $65, $69, $B9, $A9, $A9, $AD, $5D, $61, $65, $69, $B1, $61, $65, $69, $B1, $61, $65, $69, $B1, $5D, $5D, $5D, $5D, $61, $65, $69, $B1, $61, $65, $69, $B1, $61, $65, $69, $B1, $5D, $5D, $5D, $89, $C1, $65, $69, $B1, $61, $65, $69, $B1, $61, $65, $69, $B1, $5D, $5D, $5D, $8D, $C5, $65, $69, $B1, $61, $65, $69, $B1, $61, $65, $69, $B1, $5D, $5D, $5D, $75, $75, $99, $69, $B1, $61, $65, $69, $B1, $61, $65, $69, $B1, $5D, $5D, $5D, $9D, $9D, $99, $69, $B1, $61, $65, $69, $6D, $C1, $65, $69, $B1, $5D, $5D, $5D, $A9, $BD, $65, $69, $B1, $61, $65, $69, $71, $C5, $65, $69, $B1, $5D, $5D, $5D, $5D, $61, $65, $69, $D1, $D5, $99, $99, $75, $75, $99, $69, $B1, $5D, $5D, $5D, $5D, $61, $A5, $B5, $B1, $61, $A5, $9D, $9D, $9D, $9D, $B5, $B1, $5D, $5D, $5D, $5D, $95, $A9, $A9, $AD, $95, $A9, $A9, $A9, $A9, $A9, $A9, $AD, $5D, $5D, $5D
attribute_screen4:
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$05,$05,$05,$05,$05,$05,$05,$05


.segment "CHARS"
.incbin "graphics.chr"
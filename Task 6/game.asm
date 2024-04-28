.segment "HEADER"           ; Assembler Directive (.), HEADER
.byte $4e, $45, $53, $1a    ; Magic string that always begins an iNES header
.byte $02                   ; Number of 16KB PRG-ROM banks
.byte $01                   ; Number of 8KB CHR-ROM banks
.byte %00000001             ; Vertical mirroring, no save RAM, no mapper
.byte %00000000             ; No special-case flags set, no mapper
.byte $00                   ; No PRG-RAM present
.byte $00                   ; NTSC format

.segment "ZEROPAGE"
	frogX: 						.res 1	; Reserve 1 byte for the sprite position in X
	frogY: 						.res 1	; Reserve 1 byte for the sprite position in X
	frogDirection: 				.res 1	; Reserve 1 byte for the sprite direction
	animationState:				.res 1	; Reserve 1 byte for the animation state for the FSM
	tick:						.res 1	; Reserve 1 byte for the a tick counter (frame counter)
	frogOffSet:					.res 1	; Reserve 1 byte for the frog off-set, to be able to add multiple frogs on the screen (on different memory)
	directionOffSet:			.res 1	; Reserve 1 byte for the frog direction off-set, to able to have multiple frogs w/ different direction/orientation
	hasMoved:					.res 1	; Reserve 1 byte for storing wether the frog moved last frame
	controllerInput:			.res 1	; Reserve 1 byte for the controller input
	scroll_offset:				.res 1
	level_change:				.res 1
	background_offset_low:		.res 1
	background_offset_up:		.res 1
	background_tile_offset:		.res 1
	tileX:						.res 1
	tileY:						.res 1
	TileIndex: 					.res 1
	temp: 						.res 1
	collision_table: 			.res 1
	collision_change: 			.res 1
	tempFrogX: 					.res 1
	tempFrogY: 					.res 1
	byteIndex: 					.res 1
	bitIndex: 					.res 1
	baseTile: 					.res 1

.segment "BSS"
	collision_map: 				.res 240     ; 960 tiles / 8 tiles per byte = 120 bytes * 2 nametables = 240 bytes
	tick_counter: 				.res 1
	counter_1:					.res 1
	counter_2:					.res 1
	counter_3:					.res 1
	counter_offset:				.res 1
	stageClearState:			.res 1
	total_count1: 				.res 1
	total_count2:				.res 1
	total_coutn2:				.res 1
	counter_X:					.res 1
	counter_Y:					.res 1
	gameOverState:				.res 1
	tick_over:					.res 1


.segment "STARTUP"

.segment "CODE"
.proc irq_handler       ; IRQ (Interrup Request) can be triggered by the NES’ sound processor or from certain types of cartridge hardware.
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
	vblankwait:           ; fetches the PPU’s status from PPUSTATUS, until PPU is ready
		BIT $2002
		BPL vblankwait

	vblankwait2:
		BIT $2002
		BPL vblankwait2

	LDA #$08							; A = 0
	STA frogX							; Set frog X to 0

	LDA #$80							; A = 136
	STA frogY							; Set frog Y to 136

	LDA $2002

	LDA #$00							; A = 0
	STA animationState		; Set animation state to 0
	STA tick							; Set tick to 0
	STA frogOffSet				; Set frog memory allocation offset to 0
	LDA collision_table
	STA directionOffSet		; Set frog direction offset to 0
	STA scroll_offset
	STA counter_offset
	STA stageClearState
	STA counter_Y
  	STA $2005           ; PPU Scroll = 0
  	STA $2005  
  	STA controllerInput   						; Set controllerInput to 0
 	STA hasMoved  			 	; Set hasMoved to 0
	STA tileX
	STA tileY
	STA TileIndex
	STA level_change
	STA gameOverState
	STA tick_over
	
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


	LDA #$25							; A = 25
	STA frogDirection   	; Set frog Direction to Right (25)

	LDA #1
	STA collision_change
	JSR load_collisions

  JMP main							; Go to main
.endproc

.proc nmi_handler       ; NMI (“Non-Maskable Interrupt”) occurs when the PPU starts preparing the next frame of graphics, 60 times per second.
	JMP handler
	RTI
	handler:
	; Initialize OAM for DMA transfer
	LDA #$00
	STA $2003           ; Set OAM start address for DMA
	LDA #$02
	STA $4014           ; Start DMA transfer from $0200-$02FF to OAM

  	JSR ClearSprites    ; Call ClearSprites to clear sprite data at the beginning of the frame
	JSR drawGameOver
	LDA gameOverState
	CMP #$01
	BEQ continue
	JSR drawCounter
	JSR drawStageClear
	JSR changeStage

	LDA #$00
  	STA hasMoved        ; Reset hasMoved at the start of each frame

	JSR update					; Call Update function         
	LDA level_change
	CMP #$01
	BNE continue
	JSR drawBackground
	continue:
    RTI  

.endproc

.proc ClearSprites
    LDX #$00            ; Start with the first sprite
	clear_loop:
    LDA #$FF            ; Load A with $FF, a Y position off-screen
    STA $0200, X        ; Set the sprite's Y position off-screen, taking $0200 as the start of OAM
    INX                 ; Move to the next byte in OAM
    INX                 ; Skip over the attribute byte
    INX                 ; Skip over the X position byte
    INX                 ; Advance to the next sprite's Y position
    CPX #$00            ; Check if X has rolled over, indicating all 64 sprites were processed
    BNE clear_loop
    RTS                 ; Return from subroutine
.endproc

.proc update
	; SAVE REGISTER INTO THE STACK
	PHP						; Push the Processor Status Register onto the stack
	PHA						; Push the Accumulator Register onto the stack
	TXA						; Transfer X into the Accumulator
	PHA						; Push X (the accumulator register) onto the stack
	TYA						; Transfer Y into the Accumulator
	PHA						; Push Y (the accumulator register) onto the stack

	LDA tick    ; Load tick
	CLC         ; Clear carry flag
	ADC #$01    ; Add 1 to tick
	STA tick    ; Store new tick

	LDA tick_counter
	CMP #$3C
	BEQ change_counter
	CLC
	ADC #$01
	STA tick_counter
	JMP continue
	change_counter:
		LDA stageClearState
		CMP #$02
		BEQ continue
		LDA #$00
		STA tick_counter
		LDA counter_1
		CMP #$00
		BEQ change_counter2
		LDA counter_3
		DEC counter_1
		JMP continue
		change_counter2:
			LDA counter_2
			CMP #$00
			BEQ check
			JMP dec_
			check:
			LDA counter_3
			CMP #$00
			BEQ continue	
			dec_:
			LDA #$09
			STA counter_1
			LDA counter_2
			CMP #$00
			BEQ change_counter3
			DEC counter_2
			JMP continue
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
	continue:
	; LDA #$00
	; STA tileX
	; STA tileY
	; STA TileIndex

	LDA collision_change
	CMP #0
	BEQ continue_update

	LDA #0
	STA collision_change
	JSR load_collisions

	continue_update:
	JSR HandleMovement
  
	JSR draw        ; Call Draw function to reflect changes
  
	; Wait for VBlank to end before restoring registers (prevents changes during rendering)
	WaitEndVBlank:
    BIT $2002
    BMI WaitEndVBlank      ; Wait until VBlank is over

	LDA $2002

	LDA scroll_offset
	STA $2005
	LDA #$00
	STA $2005
	LDX #$00

			; LDA #$00
			; STA $2006  ; Reset PPU address high byte
			; STA $2006  ; Reset PPU address low byte

	; RESTORE REGISTERS & RETURN
	PLA						; Pull Y of the stack and place it into the accumulator	register
	TAY						; Restore/Transfer the accumulator into Y
	PLA						; Pull X of the stack and place it into the accumulator	register
	TAX						; Restore/Transfer the accumulator into X
	PLA						; Pull the top value of the stack and place it into the accumulator register
	PLP						; Pull the top value of the stack and place it into the  processor status register
	RTS						; Return from subroutine
.endproc

.proc changeStage
	; SAVE REGISTER INTO THE STACK
	PHP						; Push the Processor Status Register onto the stack
	PHA						; Push the Accumulator Register onto the stack
	TXA						; Transfer X into the Accumulator
	PHA						; Push X (the accumulator register) onto the stack
	TYA						; Transfer Y into the Accumulator
	PHA						; Push Y (the accumulator register) onto the stack

	LDA frogX
	CMP #$F4
	BNE continue
	checkY:
		LDA frogY
		CMP #$CE
		BNE continue
		LDA level_change
		CMP #$01
		BEQ continue
		LDA #$01
		STA level_change
		STA stageClearState
		LDA #$10
		STA frogY
		LDa #$08
		STA frogX
		
		LDX #$00
		STX counter_3
		LDX #$01
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

	LDA #$01
	STA gameOverState
	LDX #$00
	game_over:
		LDA gameOver,X
		STA $0200,X
		INX
		CPX #$20
		BNE game_over
	; LDA #$00
	; STA tick
	LDA tick_over
	CLC
	ADC #$01
	STA tick_over
	CMP #$1E
	BEQ stage1
	CMP #$3C
	BEQ stage2
	CMP #$78
	BEQ stage3
	CMP #$96
	BEQ stage4
	CMP #$b4
	BEQ frog_out
	JMP exit
	stage1:
		LDA #$25
		STA frogDirection
		JSR drawFrog
		JMP exit
	stage2:
		LDA #$19
		STA frogDirection
		JSR drawFrog
		JMP exit
	stage3:
		LDA #$01
		STA frogDirection
		JSR drawFrog
		JMP exit
	stage4:
		LDA #$0D
		STA frogDirection
		JSR drawFrog
		LDA #$b4
		STA tick_over
		JMP exit
	frog_out:
		LDA frogY
		CLC
		ADC #$01
		STA frogY
		CMP #$FF
		JSR drawFrog
		BNE frog_out
	exit:
		JSR drawFrog
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

	LDA counter_Y
	STA $0200
	CLC
	ADC #$10    
	LDA counter_3
	CLC
	ADC #$40
	STA $0201    
	LDA #$02
	STA $0202         
	LDA counter_X
	STA $0203

	
	LDA counter_Y
	STA $0200   
	LDA counter_3
	CLC
	ADC #$40
	STA $020
	CLC
	ADC #$10     
	LDA #$02
	STA $0202
	CLC
	ADC #$10         
	LDA counter_X
	STA $0203
	CLC
	ADC #$10  

	LDA counter_Y
	STA $0204
	CLC
	ADC #$10          
	LDA counter_2
	CLC
	ADC #$40
	STA $0205
	CLC
	ADC #$10     
	LDA #$02
	STA $0206
	CLC
	ADC #$10         
	LDA counter_X
	CLC
	ADC #$08
	STA $0207
	CLC
	ADC #$10  


	LDA counter_Y
	STA $0208
	CLC
	ADC #$10       
	LDA counter_1
	CLC
	ADC #$40
	STA $0209
	CLC
	ADC #$10      
	LDA #$02
	STA $020A
	CLC
	ADC #$10          
	LDA counter_X
	CLC
	ADC #$10
	STA $020B
	CLC
	ADC #$10   
 
	; RESTORE REGISTERS & RETURN
	PLA						; Pull Y of the stack and place it into the accumulator	register
	TAY						; Restore/Transfer the accumulator into Y
	PLA						; Pull X of the stack and place it into the accumulator	register
	TAX						; Restore/Transfer the accumulator into X
	PLA						; Pull the top value of the stack and place it into the accumulator register
	PLP						; Pull the top value of the stack and place it into the  processor status register
	RTS						; Return from subroutine
.endproc

.proc HandleMovement
	; Read and process controller input
	LDA $4016   ; Read controller 1
	STA controllerInput    ; Store state in controllerInput variable
	LDA #$01
	STA $4016   ; Strobe controller
	LDA #$00
	STA $4016   ; Clear strobe, begin reading button states

	LDY #$08    ; Prepare to read 8 buttons
	LDA #$00    ; Clear A to start

	read_buttons:
			LDA $4016       ; Read button state
			LSR             ; Shift right, moving the button's state into carry
			ROL controllerInput        ; Rotate Left through Carry to move button state into controllerInput
			DEY
			BNE read_buttons

			; Check Right
			LDA controllerInput
			AND #%00000001  ; Isolate Right button
			BEQ check_left  ; If not pressed, check next
			
			check_right:
				LDA #$25
				STA frogDirection ; Set frogDirection to 25 (Right)

				; LDA frogX
				; CMP #$FF
				; BEQ near_done
				
				LDA frogX
				INC frogX       ; Move right

				LDA scroll_offset
				CMP #$FF    ; Check if scroll_offset is greater than 255
				BEQ SkipIncrement  ; Skip if incrementing leads to overflow
				CLC         ; Clear carry for addition
				ADC #$01    ; Increment scroll_offset
				STA scroll_offset

				LDA scroll_offset
				CMP #$6F
				BMI ColRightUnchanged

				LDA collision_table
				CMP #1
				BEQ ColRightUnchanged

				LDA #1
				STA collision_table		; Using second table
				STA collision_change
				JSR load_collisions

				ColRightUnchanged:
				JSR CheckCollisionsOnSprite
				CMP #$01
				BEQ right_collision_with_scroll
				
				SkipIncrement:
				JSR CheckCollisionsOnSprite
				CMP #$01
				BEQ right_collision
				JMP update_done
				
				right_collision_with_scroll:
				LDA scroll_offset
				DEC scroll_offset
				right_collision:
				DEC frogX
				JMP update_done
				

			check_left:
				LDA controllerInput
				AND #%00000010  ; Isolate Left button
				BEQ check_up    ; If 0, button not pressed, check next
				
				LDA #$01
				STA frogDirection ; Set frogDirection to 01 (Left)

				LDA frogX
				CMP #$00
				BEQ near_done
				
				LDA frogX 
				DEC frogX

				LDA scroll_offset
				CMP #$00    ; Check if scroll_offset is 0
				BEQ SkipDecrement  ; Skip if decrementing leads to overflow
				SEC         ; Set carry for subtraction
				SBC #$01    ; Decrease scroll_offset
				STA scroll_offset

				LDA scroll_offset
				CMP #$7D
				BPL ColLeftUnchanged

				LDA collision_table
				CMP #0
				BEQ ColLeftUnchanged

				LDA #0
				STA collision_table		; Using first table
				LDA #1
				STA collision_change

				ColLeftUnchanged:
				JSR CheckCollisionsOnSprite
				CMP #$01
				BEQ left_collision_with_scroll

				SkipDecrement:
				JSR CheckCollisionsOnSprite
				CMP #$01
				BEQ left_collision
				JMP update_done
				
				left_collision_with_scroll:
				LDA scroll_offset
				INC scroll_offset
				left_collision:
				INC frogX
				JMP update_done

			near_done:
				JMP update_done

			check_up:
				LDA controllerInput
				AND #%00001000  ; Isolate Up button
				BEQ check_down    ; If 0, button not pressed, check next
				
				LDA #$19
				STA frogDirection ; Set frogDirection to 19 (Up)

				LDA frogY
				CMP #$00
				BEQ update_done
				
				LDA frogY 
				DEC frogY

				JSR CheckCollisionsOnSprite
				CMP #$01
				BEQ up_collision
				JMP update_done
				
				up_collision:
				INC frogY
				JMP update_done

			check_down:
				LDA controllerInput
				AND #%00000100  ; Isolate Down button
				BEQ button_start
				
				LDA #$0D
				STA frogDirection ; Set frogDirection to 0D (Down)

				LDA frogY
				CMP #$E8
				BEQ update_done
				
				LDA frogY 
				INC frogY

				JSR CheckCollisionsOnSprite
				CMP #$01
				BEQ down_collision
				JMP update_done
				
				down_collision:
				DEC frogY
				JMP update_done
		button_start:

			no_movement:
					LDA #$00
					STA hasMoved ; Set hasMoved to 0
					STA directionOffSet
					; STA tileX
					; STA tileY
					; STA TileIndex
					LDA #$0D
					STA frogDirection
					JMP update_done

	update_done:
		RTS
.endproc

.proc CheckCollisionsOnSprite ; ------------------------------------------------------------------------------------------+
    ; Top-Left Corner
		LDA frogX
		CLC
		ADC scroll_offset
		AND #$FF
		STA tempFrogX
    LDA frogY
    STA tempFrogY

		STA tempFrogY
    JSR CalculateTileIndex
    JSR CheckCollision
    BNE HandleCollision

		; Top-Right corner
		LDA frogX
		CLC
		ADC scroll_offset
		AND #$FF
    ADC #$08      ; Add 8 pixels to check the right side of the sprite
    STA tempFrogX
    LDA frogY
    STA tempFrogY
    JSR CalculateTileIndex
    JSR CheckCollision
    BNE HandleCollision

    ; Bottom-Left corner
		LDA frogX
		CLC
		ADC scroll_offset
		AND #$FF
    STA tempFrogX
    LDA tempFrogY
    CLC
    ADC #$08      ; Add 8 pixels to check the bottom side of the sprite
    STA tempFrogY
    JSR CalculateTileIndex
    JSR CheckCollision
    BNE HandleCollision

    ; Bottom-Right corner
    LDA tempFrogX
    ADC #$08      ; Add 8 pixels to check the right side
    STA tempFrogX
    JSR CalculateTileIndex
    JSR CheckCollision
    BNE HandleCollision

    ; No collision detected
		LDA #$00
    RTS

		HandleCollision:
    LDA #$01
    RTS

.endproc

.proc CalculateTileIndex ; ------------------------------------------------------------------------------------------+
		; Formula for Byte Index: X/8/8 + Y/8 * 4 (collision map width)
		LDA tempFrogX
		LSR A					; TileX / 2
		LSR A					; TileX / 4
		LSR A					; TileX / 8
		LSR A					; TileX / 16
		LSR A					; TileX / 32
		LSR A					; TileX / 64
		STA $00

		LDA tempFrogY
		LSR A					; TileY / 2
		LSR A					; TileY / 4
		LSR A					; TileY / 8
		
		ASL A					; (TileY/8) * 2
		ASL A					; (TileY/8) * 4
		ASL A					; (TileY/8) * 8
		
		CLC
		ADC $00				; ((TileY/8) * 4) + (TileX / 64)
		STA byteIndex

		; Formula for Bit Index: X/8 AND %0111
		LDA tempFrogX
		LSR A					; TileX / 2
		LSR A					; TileX / 4
		LSR A					; TileX / 8
		AND #%0111		; Bitmask index
		STA bitIndex

		RTS	
.endproc

; .proc CheckCollision
;     LDY byteIndex       ; Load the index for the collision map byte
;     LDA col_map, Y      ; Load the byte from the collision map

;     LDY bitIndex        ; Load the index for the bitmask
;     LDA BitMask, Y      ; Load the bitmask from the BitMask array
    
;     AND #$FF            ; Correct usage, applies the previously loaded bitmask directly to A
;     STA TileIndex       ; Store the result which indicates collision if non-zero
;     RTS
; .endproc

.proc CheckCollision ; ------------------------------------------------------------------------------------------+
	LDY byteIndex
	LDX bitIndex

	LDA collision_map, Y
	AND BitMask, X
	STA TileIndex
	RTS
.endproc

.proc draw ; ------------------------------------------------------------------------------------------+
	; SAVE REGISTER INTO THE STACK
	PHP						; Push the Processor Status Register onto the stack
	PHA						; Push the Accumulator Register onto the stack
	TXA						; Transfer X into the Accumulator
	PHA						; Push X (the accumulator register) onto the stack
	TYA						; Transfer Y into the Accumulator
	PHA						; Push Y (the accumulator register) onto the stack

	; vblankwait:           ; fetches the PPU’s status from PPUSTATUS, until PPU is ready
	; 	BIT $2002
	; 	BPL vblankwait

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

.proc drawAnimation
    ; Save register states to the stack
    PHP
    PHA
    TXA
    PHA
    TYA
    PHA

    LDA hasMoved           ; Check if the frog has moved
    CMP #$00
    BEQ maintainAnimation  ; If the frog hasn't moved, maintain current animation

	updateAnimation:
    LDA tick
    CMP #$0A               ; Compare tick to 10
    BCC continue           ; If less than 10, skip to drawing the current frame

    ; Animation state logic when tick >= 10
    LDA #$00
    STA tick               ; Reset tick
    LDA animationState
    CLC
    ADC #$01               ; Increment animation state
    STA animationState

		LDA directionOffSet
		CLC
		ADC #$04     
		STA directionOffSet	; Increase direction offset by four (1 tile)

    LDA animationState
    CMP #$03               ; 4 = total animation states
    BCC continue           ; If animationState < 4, proceed to drawing

    ; Reset animation state to 0 if it reaches the limit
    LDA #$00
    STA animationState
	STA directionOffSet

	maintainAnimation:
    LDA #$00
    STA animationState

	continue:
    JSR drawFrog           ; Draw the frog with the current animation state

    ; Restore register states from the stack
    PLA
    TAY
    PLA
    TAX
    PLA
    PLP
    RTS
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
	LDA gameOverState
	CMP #$01
	BEQ if_tile1
	LDA #$00
	JMP con_tile1
	if_tile1:
	LDA #$03
	con_tile1:
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
	LDA gameOverState
	CMP #$01
	BEQ if_tile2
	LDA #$00
	JMP con_tile2
	if_tile2:
	LDA #$03
	con_tile2:
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
	LDA gameOverState
	CMP #$01
	BEQ if_tile3
	LDA #$01
	JMP con_tile3
	if_tile3:
	LDA #$03
	con_tile3:
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
	LDA gameOverState
	CMP #$01
	BEQ if_tile4
	LDA #$01
	JMP con_tile4
	if_tile4:
	LDA #$03
	con_tile4:
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
	LDX #$00
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
			LDA background_screen3, X 
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
	
	LDA scroll_offset
	STA $2005
	LDA #$00
	STA $2005

	PLA
    TAY
    PLA
    TAX
    PLA
    PLP
    RTS
.endproc


; .proc BuildCollisionMap
;     LDX #0  ; Index for initializing collision_map

; 	InitializeCollisionMap:
; 			LDA #0
; 			STA collision_map, X
; 			INX
; 			CPX #$F0  ; Total bytes for both nametables
; 			BNE InitializeCollisionMap

; 			LDX #0  ; Reset for building the map from nametable data

; 	BuildMapLoop:
; 			CPX #$F0  ; Check against the total entries expected
; 			BCS FinishedBuildingMap

; 			; Load base tile index from nametable
; 			LDA background_screen1, X
; 			STA baseTile
; 			INX

; 			LDY #0  ; Y will be used to iterate through the 2x2 tile matrix
; 	ProcessTile:
; 			TYA            ; Transfer Y to A for calculations
; 			CLC
; 			ADC baseTile   ; Add Y to baseTile to get the current tile index
; 			TAX            ; Transfer result to X for CheckWalkable

; 			; Perform walkability check
; 			LDA #0         ; Reset X for walkable addresses loop
; 	CheckWalkable:
; 			LDA walkable_addresses, X
; 			CMP #$FF       ; Check if end of walkable tile list
; 			BEQ TileNotFound
; 			CMP baseTile   ; Compare with the base tile index
; 			BEQ FoundTile  ; If found, mark as walkable

; 			INX            ; Move to next walkable address
; 			JMP CheckWalkable

; 	FoundTile:
; 			; Set the bit in the collision map to indicate walkable
; 			LDA #$01
; 			STA TileIndex  ; Assume TileIndex is where we store if it's walkable

; 			; Set the correct bit in collision_map
; 			TXA            ; Reload the current tile index to A
; 			LSR A
; 			LSR A
; 			LSR A          ; Divide index by 8 to get byte offset
; 			STA temp

; 			TXA            ; Reload tile index to A
; 			AND #7         ; Get bit within byte
; 			TAX            ; Use X for bit shifts

; 			LDA #$01
; 	ShiftBitmask:
; 			DEX            ; Decrement X
; 			BEQ DoneShifting ; Exit loop when X is 0
; 			ASL A          ; Shift left to move '1' to the correct bit position
; 			JMP ShiftBitmask

; 	DoneShifting:
; 			LDY temp   ; Load the byte offset into Y
; 			LDA collision_map, Y ; Load the current value from the collision_map
; 			ORA collision_map, Y          ; OR the bitmask with the current map value to set the bit

; 			JMP NextTile   ; Continue to the next tile

; 	TileNotFound:
; 			; Handle tile not found case if needed
; 			JMP NextTile

; 	NextTile:
; 			INY
; 			CPY #4
; 			BCC ProcessTile

; 			JMP BuildMapLoop

; 	FinishedBuildingMap:
; 			RTS
; .endproc



; .proc createCollisionMap ; -------------------------------------------------------------------------+
; 	LDX #$00         				; Start of the table
; 	; CheckWalkable:
; 	; 		LDA walkable_addresses, X
; 	; 		CMP #$FF     				; $FF marks the end of walkable tile list
; 	; 		BEQ TileNotFound
; 	; 		CMP TileIndex 			; Compare with the fetched tile index
; 	; 		BEQ FoundTile  			; Branch if it is a walkable tile
; 	; 		INX
; 	; 		JMP CheckWalkable
; .endproc

; .proc load_collisions ; -------------------------------------------------------------------------+
; 		; Assume collision_map is in zero page or another RAM location
;     LDX #0  ; Start index for the loop
; 	copy_loop:
;     LDA col_map, X  ; Load data from col_map (in ROM)
;     STA collision_map, X  ; Store it in collision_map (in RAM)
;     INX
;     CPX #240
; 		BNE copy_loop
; .endproc

.proc load_collisions ; -------------------------------------------------------------------------+
	copy_loop:
    LDA col_map, X  ; Load data from col_map (in ROM)
    STA collision_map, X  ; Store it in collision_map (in RAM)
    INX
    CPX #240
		BNE copy_loop
	
	LDA collision_table
	CMP #1
	BEQ collisionMap2

	collisionMap1:
		LDA frogX
		CMP #8
		BEQ start

		LDA frogX
		SEC
		SBC #8
		STA frogX

		LDA scroll_offset
		SEC
		SBC #8
		STA scroll_offset
		start:
    LDX #0  ; Start index for the loop
	copy_loop1:
    LDA col_map, X  ; Load data from col_map (in ROM)
    STA collision_map, X  ; Store it in collision_map (in RAM)
    INX
    CPX #240
		BNE copy_loop1
		RTS

	collisionMap2:
	LDA frogX
	CLC
	ADC #8
	STA frogX

	LDA scroll_offset
	CLC
	ADC #8
	STA scroll_offset

	LDX #0  ; Start index for the loop
	copy_loop2:
    LDA col_map2, X  ; Load data from col_map (in ROM)
    STA collision_map, X  ; Store it in collision_map (in RAM)
    INX
    CPX #240
		BNE copy_loop2
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
		
		lda #$3F
		sta $2006
		lda #0
		sta $2006
		sta $2006
		sta $2006

		JSR drawBackground	; Call drawBackground

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
	.byte $2C, $3A, $17, $2B    ; green brown, brown, light green
	.byte $2C, $0F, $1C, $3B    ; green, dark green, light green
	.byte $2C, $1C, $3C, $2B    ; green blue, blue, light blue
	.byte $2C, $1C, $17, $2B    ; green blue, brown, light green
	.byte $2C, $0f, $28, $20    ; green, black, green, white
	.byte $2C, $0f, $17, $3B    ; green, black, green, green
	.byte $2C, $0f, $25, $20
	.byte $2C, $0f, $3D, $30
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
	.byte $05, $09, $09, $09, $09, $09, $09, $09, $09, $09, $0D, $01, $05, $09, $09, $0D, $11, $D9, $D9, $D9, $15, $D9, $D9, $D9, $D9, $15, $19, $01, $11, $15, $15, $19, $11, $D9, $15, $D9, $D9, $D9, $15, $D9, $D9, $D9, $19, $01, $11, $15, $15, $19, $21, $25, $29, $D9, $2D, $25, $25, $25, $29, $D9, $19, $01, $11, $15, $15, $19, $45, $01, $11, $D9, $19, $01, $05, $09, $35, $D9, $39, $09, $35, $15, $15, $19, $01, $53, $11, $D9, $19, $01, $11, $D9, $D9, $D9, $D9, $D9, $D9, $D9, $D9, $19, $01, $01, $11, $D9, $19, $01, $11, $D9, $2D, $25, $29, $D9, $2D, $25, $25, $31, $09, $09, $35, $D9, $19, $01, $11, $D9, $19, $3D, $11, $D9, $19, $01, $01, $01, $D9, $D9, $D9, $D9, $19, $01, $11, $D9, $19, $41, $11, $D9, $19, $01, $01, $01, $25, $25, $25, $25, $31, $01, $11, $D9, $39, $09, $35, $D9, $39, $09, $09, $09, $01, $45, $01, $01, $49, $01, $11, $D9, $15, $D9, $D9, $D9, $D9, $D9, $D9, $D9, $01, $4D, $51, $55, $01, $01, $21, $25, $29, $D9, $2D, $25, $25, $25, $25, $25, $05, $09, $09, $09, $09, $09, $09, $09, $35, $D9, $39, $09, $09, $09, $09, $0D, $11, $D9, $15, $D9, $D9, $15, $D9, $D9, $D9, $D9, $D9, $D9, $D9, $D9, $D9, $19, $21, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $31, $BA, $AA, $AA, $AA, $FB, $FA, $FA, $AA, $28, $AA, $AA, $AA, $AA, $AA, $AF, $AF	; .byte $11,$D9,$D9,$D9,$15,$D9,$D9,$D9,$D9,$15,$19,$01,$11,$15,$15,$19

attribute_screen1:
	.byte $ba,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$fb,$fa,$fa,$fa,$fa,$aa,$aa,$aa
	.byte $28,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$af,$ab,$af,$af
	.byte $ff,$ff,$ba,$aa,$aa,$aa,$aa,$aa,$22,$0a,$a8,$fa,$ba,$fa,$fa,$fa
	.byte $ba,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f

background_screen2:
.byte $01, $01, $05, $09, $0D, $01, $01, $05, $09, $09, $09, $09, $09, $09, $09, $0D, $45, $01, $11, $D9, $19, $01, $01, $11, $15, $15, $15, $15, $15, $15, $15, $19, $01, $01, $11, $D9, $19, $01, $45, $11, $D9, $D9, $D9, $D9, $D9, $D9, $D9, $19, $01, $01, $11, $D9, $19, $01, $01, $11, $D9, $2D, $25, $25, $25, $29, $D9, $19, $45, $01, $11, $D9, $19, $01, $01, $11, $D9, $19, $45, $01, $4D, $11, $D9, $19, $01, $53, $11, $D9, $19, $01, $01, $11, $D9, $19, $01, $49, $01, $11, $D9, $19, $4C, $01, $11, $D9, $39, $09, $09, $35, $D9, $19, $01, $01, $01, $11, $D9, $19, $55, $01, $11, $D9, $D9, $D9, $D9, $D9, $D9, $19, $53, $55, $01, $11, $D9, $19, $01, $01, $11, $D9, $2D, $25, $29, $D9, $2D, $31, $01, $01, $01, $11, $D9, $19, $09, $09, $35, $D9, $19, $01, $11, $D9, $19, $01, $01, $47, $01, $11, $D9, $19, $D9, $D9, $D9, $D9, $19, $01, $11, $D9, $19, $01, $49, $01, $01, $11, $D9, $19, $25, $25, $25, $25, $31, $01, $11, $D9, $19, $01, $01, $01, $01, $11, $D9, $19, $01, $45, $01, $4D, $01, $55, $11, $D9, $39, $09, $09, $09, $0D, $11, $15, $19, $01, $01, $49, $01, $45, $01, $11, $D9, $D9, $D9, $D9, $D9, $19, $11, $59, $19, $01, $01, $01, $01, $01, $01, $21, $25, $25, $25, $25, $25, $31, $21, $25, $31, $8A, $AA, $AA, $AA, $82, $AA, $EA, $FA, $28, $AA, $AA, $A8, $88, $AA, $AA, $AA
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


walkable_addresses:
	.byte $05, $12, $14, $15, $16, $17, $18, $19, $1B, $D9, $1E, $1F, $20, $2A, $2C, $2D, $2F, $36, $38, $39, $3B, $D9, $DA, $DB, $DC, $FF

col_map:
	.byte %11111111, %11111111, %11111111, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %11111111, %11111111, %11111111, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %10000000, %00000000, %00001111, %10000011, %00000000, %00000000, %00000000, %00000000
	.byte %10000000, %00000000, %00001111, %10000011, %00000000, %00000000, %00000000, %00000000
	.byte %10000000, %00000000, %00001111, %10000011, %00000000, %00000000, %00000000, %00000000
	.byte %10000000, %00000000, %00001111, %10000011, %00000000, %00000000, %00000000, %00000000
	.byte %11111000, %11111111, %10001111, %10000011, %00000000, %00000000, %00000000, %00000000
	.byte %11111000, %11111111, %10001111, %10000011, %00000000, %00000000, %00000000, %00000000
	.byte %11111000, %11111111, %10001111, %10000011, %00000000, %00000000, %00000000, %00000000
	.byte %11111000, %11111111, %10001111, %10000011, %00000000, %00000000, %00000000, %00000000
	.byte %11111000, %11111000, %00000000, %00000011, %00000000, %00000000, %00000000, %00000000
	.byte %11111000, %11111000, %00000000, %00000011, %00000000, %00000000, %00000000, %00000000
	.byte %11111000, %11111000, %11111000, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %11111000, %11111000, %11111000, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %11111000, %11111000, %11111000, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %11111000, %11111000, %11111000, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %11111000, %11111000, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %00000000, %11111000, %11111000, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %11111111, %11111000, %11111000, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %11111111, %11111000, %11111000, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %11111111, %11111000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
	.byte %11111111, %11111000, %00000000, %00000000, %00000000, %00000000, %00000000, %00000000
	.byte %11111111, %11111111, %10001111, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %11111111, %11111111, %10001111, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %11111111, %11111111, %10001111, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %11111111, %11111111, %10001111, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %10000000, %00000000, %00000000, %00000011, %00000000, %00000000, %00000000, %00000000
	.byte %10000000, %00000000, %00000000, %00000011, %00000000, %00000000, %00000000, %00000000
	.byte %11111111, %11111111, %11111111, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte %11111111, %11111111, %11111111, %11111111, %00000000, %00000000, %00000000, %00000000

	col_map2:
	.byte  %11111111, %11111111, %11111111, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte  %11111111, %11111111, %11111111, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111110, %00000000, %00000011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111110, %00000000, %00000011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111110, %00000000, %00000011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111110, %00000000, %00000011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111110, %00111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111110, %00111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111110, %00111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111110, %00111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111110, %00111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111110, %00111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111110, %00111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111110, %00111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %00000000, %00111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %00000000, %00111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111000, %11111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111000, %11111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111000, %11111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111000, %11111000, %11111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %00000000, %11111000, %11111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %00000000, %11111000, %11111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111111, %11111000, %11111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111111, %11111000, %11111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111111, %11111000, %11111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111111, %11111000, %11111111, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111111, %11111000, %00000000, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111111, %11111000, %00000000, %11100011, %00000000, %00000000, %00000000, %00000000
	.byte  %11111111, %11111111, %11111111, %11111111, %00000000, %00000000, %00000000, %00000000
	.byte  %11111111, %11111111, %11111111, %11111111, %00000000, %00000000, %00000000, %00000000

BitMask:
	.byte %10000000
	.byte %01000000
	.byte %00100000
	.byte %00010000
	.byte %00001000
	.byte %00000100
	.byte %00000010
	.byte %00000001

.segment "CHARS"
.incbin "graphics.chr"
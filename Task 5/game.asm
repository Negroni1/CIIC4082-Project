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
	; directionOffSet:		.res 1	; Reserve 1 byte for the frog direction off-set, to able to have multiple frogs w/ different direction/orientation
	hasMoved:		.res 1	; Reserve 1 byte for storing wether the frog moved last frame
	controllerInput:		.res 1	; Reserve 1 byte for the controller input
	scroll_offset:				.res 1
	level_change:				.res 1
	background_offset_low:		.res 1
	background_offset_up:		.res 1
	background_tile_offset:		.res 1
	tileX:	.res 1
	tileY:	.res 1
	TileIndex: .res 1
	Tile: .res 1
	tileDataPtr: .res 2
	chrTilePtr: .res 2

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

  JSR ClearSprites    ; Call ClearSprites to clear sprite data at the beginning of the frame
	
	LDA #$00
  STA hasMoved        ; Reset hasMoved at the start of each frame
	JSR update					; Call Update function         

	LDA scroll_offset
	STA $2005
	LDA #$00
	STA $2005
	LDX #$00

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

	LDA #$08							; A = 8
	STA frogX							; Set frog X to 8

	LDA #$80							; A = 136
	STA frogY							; Set frog Y to 136

	LDA #$00							; A = 0
	STA animationState		; Set animation state to 0
	STA tick							; Set tick to 0
	STA frogOffSet				; Set frog memory allocation offset to 0
	; STA directionOffSet		; Set frog direction offset to 0
  STA $2005           ; PPU Scroll = 0
  STA $2005  
  STA controllerInput   						; Set controllerInput to 0
  STA hasMoved  			 	; Set hasMoved to 0
	STA tileX
	STA tileY
	STA TileIndex
  STA chrTilePtr        ; Low byte of CHR tile pointer
  STA tileDataPtr       ; Low byte of RAM tile data pointer

  LDA #$10
  STA chrTilePtr+1      ; High byte of CHR tile pointer ($1000)

  LDA #$03
  STA tileDataPtr+1     ; High byte of RAM tile data pointer ($0300)
	
	LDA #$20
	STA background_offset_up

	LDA #$25							; A = 25
	STA frogDirection   	; Set frog Direction to Right (25)

  JMP main							; Go to main
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

		; Ensure VBlank has started
	WaitVBlank:
    BIT $2002               ; Read PPU status to reset the address latch
    BPL WaitVBlank          ; Loop until VBlank starts (bit 7 is set)

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

    		; Check Right (bit 7 of controllerInput)
    		LDA controllerInput
   		 	AND #%00000001  ; Isolate Right button
   		 	BEQ check_left  ; If 0, button not pressed, check next
    		
    		LDA #$25
    		STA frogDirection ; Set frogDirection to 25 (Right)

				JSR checkWalkable
				CMP #$00
				BEQ near_done

				LDA frogX
				CMP #$F8
				BEQ SkipIncrementRight
    		INC frogX       ; Move right
				
				SkipIncrementRight:
				LDA #$01
				STA hasMoved ; Set hasMoved to 1

    		JMP update_done

				check_left:
    				LDA controllerInput
    				AND #%00000010  ; Isolate Left button
    				BEQ check_up    ; If 0, button not pressed, check next
    				
						LDA #$01
						STA frogDirection ; Set frogDirection to 01 (Left)

						JSR checkWalkable
						CMP #$00
						BEQ update_done

						LDA frogX
						CMP #$00
						BEQ SkipDecrementLeft
    				DEC frogX       ; Move left
						
						SkipDecrementLeft:
						LDA #$01
						STA hasMoved ; Set hasMoved to 1

    				JMP update_done

				check_up:
    				LDA controllerInput
    				AND #%00001000  ; Isolate Up button
    				BEQ check_down  ; If 0, button not pressed, check next
    				
						LDA #$19
    				STA frogDirection ; Set frogDirection to 19 (Up)

						JSR checkWalkable
						CMP #$00
						BEQ update_done

						LDA frogY
						CMP #$00
						BEQ SkipDecrementUp
    				DEC frogY       ; Move up

						SkipDecrementUp:
						LDA #$01
						STA hasMoved ; Set hasMoved to 1

    				JMP update_done

				near_done:
					JMP update_done

				check_down:
    				LDA controllerInput
   				  AND #%00000100  ; Isolate Down button
    				BEQ no_movement ; If 0, button not pressed, no movement was done
    				
    				LDA #$0D
    				STA frogDirection ; Set frogDirection to 0D (Down)

						JSR checkWalkable
						CMP #$00
						BEQ update_done

						LDA frogY
						CMP #$E8
						BEQ SkipIncrementDown
    				INC frogY       ; Move down

						SkipIncrementDown:
						LDA #$01
						STA hasMoved ; Set hasMoved to 1
						
						JMP update_done
					
				no_movement:
						LDA #$00
						STA hasMoved ; Set hasMoved to 0
						; STA directionOffSet
						LDA #$0D
						STA frogDirection
						JMP update_done					

		update_done:
    		JSR draw        ; Call Draw function to reflect changes
  
	; Wait for VBlank to end before restoring registers (prevents changes during rendering)
	WaitEndVBlank:
    BIT $2002
    BMI WaitEndVBlank      ; Wait until VBlank is over

	; RESTORE REGISTERS & RETURN
	PLA						; Pull Y of the stack and place it into the accumulator	register
	TAY						; Restore/Transfer the accumulator into Y
	PLA						; Pull X of the stack and place it into the accumulator	register
	TAX						; Restore/Transfer the accumulator into X
	PLA						; Pull the top value of the stack and place it into the accumulator register
	PLP						; Pull the top value of the stack and place it into the  processor status register
	RTS						; Return from subroutine
.endproc

.proc readTile ; ------------------------------------------------------------------------------------------+
	; SAVE REGISTER INTO THE STACK
	PHP						; Push the Processor Status Register onto the stack
	PHA						; Push the Accumulator Register onto the stack
	TXA						; Transfer X into the Accumulator
	PHA						; Push X (the accumulator register) onto the stack
	TYA						; Transfer Y into the Accumulator
	PHA						; Push Y (the accumulator register) onto the stack

;	Directional Branching
	LDA frogX
	STA tileX
	LDA frogY
	STA tileY

	LDA frogDirection
	CMP #$01
	BEQ Left_Tile
	CMP #$0D
	BEQ Down_Tile
	CMP #$19
	BEQ Up_Tile
	CMP #$25
	BEQ Right_Tile
	JMP Calculate_Index

	Left_Tile:
			LDA frogX
			CMP #$00
			BEQ NearExit    ; Exit if at boundary
			SEC         ; Set carry for subtraction
			SBC #$01    ; Decrement frogX
			STA tileX
			JMP HandleScrollDecrement

	Right_Tile:
			LDA frogX
			CMP #$F8    ; Check if frogX is less than 248
			BEQ NearExit
			CLC         ; Clear carry for addition
			ADC #$01    ; Increment frogX
			STA tileX
			JMP HandleScrollIncrement

	Up_Tile:
			LDA frogY
			CMP #$00
			BEQ Exit
			SEC         ; Set carry for subtraction
			SBC #$01    ; Decrement frogY
			STA tileY
			JMP Calculate_Index  ; Only need to recalculate indexes

	Down_Tile:
			LDA frogY
			CMP #$E8    ; Check if frogY is less than 232
			BEQ Exit
			CLC         ; Clear carry for addition
			ADC #$01    ; Increment frogY
			STA tileY
			JMP Calculate_Index  ; Only need to recalculate indexes

	NearExit:
			JMP Exit  ; Use unconditional jump to reach the actual exit point

	HandleScrollDecrement:
    LDA scroll_offset
    CMP #$00    ; Check if scroll_offset is lower than 0
    BEQ SkipDecrement  ; Skip if decrementing leads to underflow
    SEC         ; Set carry for subtraction
    SBC #$01    ; Decrement scroll_offset
    STA scroll_offset    ; Update the global state if everything is fine
    JMP Calculate_Index  ; Use the updated value for calculation
	SkipDecrement:
    JMP Exit

	HandleScrollIncrement:
    LDA scroll_offset
    CMP #$FF    ; Check if scroll_offset is greater than 255
    BEQ SkipIncrement  ; Skip if incrementing leads to overflow
    CLC         ; Clear carry for addition
    ADC #$01    ; Increment scroll_offset
    STA scroll_offset    ; Update the global state if everything is fine
    JMP Calculate_Index  ; Use the updated value for calculation
	SkipIncrement:
    JMP Exit

	Calculate_Index:
			; [Adjust tileX with the horizontal scroll offset]
			LDA tileX
			CLC
			ADC scroll_offset
			STA tileX

			; [Tile Coordinate Calculation]
			LSR A
			LSR A
			LSR A
			STA tileX
			LDA tileY
			LSR A
			LSR A
			LSR A
			STA tileY

			; [PPU Address Calculation]
			LDA tileY
			ASL A
			ASL A
			ASL A
			STA $00

			LDA tileX
			CLC
			ADC $00
			STA $2006
			LDA #$20
			ADC #0
			STA $2006+1

			; [Read Tile Index]
			LDA $2007
			STA TileIndex

	Exit:
    ; [Restore Registers]
    PLA
    TAY
    PLA
    TAX
    PLA
    PLP
    RTS
.endproc
	; ; Move Left
	; Left_Tile:
	; 	LDA frogX
  ;   CMP #$00     ; Check if frogX is greater than 0
  ;   BEQ SkipDecrementLeft ; Skip decrement if at boundary
  ;   SEC
  ;   SBC #$01
	; 	SkipDecrementLeft:
  ;   	STA tileX
	; 	STA tileX  ; Store the result in tileX

	; 	LDA frogY
	; 	STA tileY  ; Copy Y as is

	; 	LDA scroll_offset
	; 	CMP #$00     ; Check if frogX is greater than 0
  ;   BEQ SkipOffsetDecrement ; Skip decrement if at boundary
	; 	DEC scroll_offset
	; 	JSR Calculate_Index
	; 	LDA scroll_offset
	; 	INC scroll_offset

	; 	SkipOffsetDecrement:
	; 		JSR Calculate_Index

	; 	JMP Exit

	; ; Move Down
	; Down_Tile:
	; 		LDA frogY
	; 		CMP #$00     ; Check if frogX is greater than 0
  ;  		BEQ SkipIncrementDown ; Skip decrement if at boundary
	; 		CLC        ; Clear carry for addition
	; 		ADC #$01   ; Add 1 to A, considering carry
	; 		SkipIncrementDown:
  ;   		STA tileY
	; 		STA tileY  ; Store the result in tileY
	; 		LDA frogX
	; 		STA tileX  ; Copy X as is
	; 		JSR Calculate_Index
	; 		JMP Exit

	; ; Move Up
	; Up_Tile:
	; 		LDA frogY
	; 		CMP #$E8     ; Check if frogX is lower than 232 (29 tiles)
  ;   	BEQ SkipDecrementUp ; Skip decrement if at boundary
	; 		SEC        ; Set carry for subtraction
	; 		SBC #$01   ; Subtract 1 from A, considering carry
	; 		SkipDecrementUp:
  ;   		STA tileY
	; 		STA tileY  ; Store the result in tileY
	; 		LDA frogX
	; 		STA tileX  ; Copy X as is
	; 		JSR Calculate_Index
	; 		JMP Exit

	; ; Move Right
	; Right_Tile:
	; 		LDA frogX
	; 		CMP #$F8     ; Check if frogX is lower than 248
  ;   	BEQ SkipIncrementRight ; Skip decrement if at boundary
	; 		CLC        ; Clear carry for addition
	; 		ADC #$01   ; Add 1 to A, considering carry

	; 		SkipIncrementRight:
  ;   		STA tileY
	; 		STA tileX  ; Store the result in tileX
	; 		LDA frogY
	; 		STA tileY  ; Copy Y as is

	; 		LDA scroll_offset
	; 		CMP #$FF     ; Check if scroll_offset is greater than 256
  ;   	BEQ SkipOffsetIncrement ; Skip decrement if at boundary
	; 		INC scroll_offset
	; 		JSR Calculate_Index
	; 		LDA scroll_offset
	; 		DEC scroll_offset

	; 		SkipOffsetIncrement:
	; 			JSR Calculate_Index
	; 		JMP Exit

	; Calculate_Index:
	; 		; Adjust tileX with the horizontal scroll offset
	; 		LDA tileX
	; 		CLC
	; 		ADC scroll_offset      ; Add scroll offset to tileX
	; 		STA tileX              ; Store back to tileX

	; 		; Convert X to tile coordinate
	; 		LSR A
	; 		LSR A
	; 		LSR A                 ; tileX / 8 to get the tile X coordinate
	; 		STA tileX

	; 		; Convert Y to tile coordinate
	; 		LDA tileY
	; 		LSR A
	; 		LSR A
	; 		LSR A                 ; tileY / 8 to get the tile Y coordinate
	; 		STA tileY

	; 		; Calculate Name Table address
	; 		; $2000 is the start of the Name Table
	; 		LDA tileY
	; 		ASL A
	; 		ASL A
	; 		ASL A                 ; TileY * 8 (row offset in Name Table)
	; 		STA $00               ; Store temporarily in zero-page address $00

	; 		LDA tileX
	; 		CLC
	; 		ADC $00               ; Add TileY * 8 (from $00) and TileX
	; 		STA tileX             ; Store result back to tileX for further calculation

	; 		; Assume $2000 is already set in the high byte, need to adjust the actual address
	; 		LDA tileX
	; 		CLC
	; 		ADC #$00              ; Adjust low byte of address if necessary due to addition carry
	; 		STA $2006             ; Store the lower byte of address

	; 		LDA #$20              ; High byte of the base name table address
	; 		ADC #0                ; Add carry from previous addition
	; 		STA $2006+1           ; Store the upper byte of address


	; 		; Set the PPU Address to read
	; 		LDA 2006+1
	; 		STA $2006
	; 		LDA 2006
	; 		STA $2006

	; 		; Read the tile index from PPU Data port
	; 		LDA $2007       ; First read after setting address gets the tile index
	; 		STA TileIndex
	; 		RTS

	; Exit:
	; 	; RESTORE REGISTERS & RETURN
	; 	PLA						; Pull Y of the stack and place it into the accumulator	register
	; 	TAY						; Restore/Transfer the accumulator into Y
	; 	PLA						; Pull X of the stack and place it into the accumulator	register
	; 	TAX						; Restore/Transfer the accumulator into X
	; 	PLA						; Pull the top value of the stack and place it into the accumulator register
	; 	PLP						; Pull the top value of the stack and place it into the  processor status register
	; 	RTS						; Return from subroutine
; .endproc

.proc checkWalkable ; ------------------------------------------------------------------------------------------+
	JSR readTile

	; Now, TileIndex needs to be compared with each tile in the CHR ROM
	LDX #0            ; X will index each tile in the CHR file, starting at $1000
	FindTileLoop:
		; Point to the next tile in CHR ROM (16 bytes per tile)
		LDA #$10
		STA chrTilePtr+1  ; High byte of CHR base address
		TXA
		CLC
		ADC #$00          ; Low byte start at $1000, offset by X
		STA chrTilePtr    ; Set low byte of CHR tile pointer

		LDY #$00          ; Y will index each byte within a tile
	CompareTileData:
		; Assuming we have fetched the tile data from PPU to a RAM location pointed by tileDataPtr
		LDA (tileDataPtr), Y  ; Load byte from RAM tile data
		CMP (chrTilePtr), Y   ; Compare with byte from CHR tile data
		BEQ NextByte          ; If match, check next byte
		INX
		CPX #$F0             ; Check 240 tiles (assuming tiles go from $1000 to $1FF0)
		BNE FindTileLoop     ; Continue if not all tiles checked
		JMP TileNotFound

	NextByte:
		INY
		CPY #$10             ; 16 bytes per tile
		BNE CompareTileData  ; If not all bytes checked, keep comparing
		; All bytes match, tile found
		STX TileIndex   ; Store the index of the matching tile
		JMP FoundTile

	FoundTile:
		LDA TileIndex
		AND #%00001111  			; Isolate Tile Index in CHR
		STA TileIndex
		JSR CheckWalkable
		LDA #$01 							; Return 1
		RTS

	TileNotFound:
		LDA #$00 							; Return 0
		RTS

	LDX #$00         				; Start of the table
	CheckWalkable:
			LDA walkable_addresses, X
			CMP #$FF     				; Let's say $FF marks the end of your walkable tile list
			BEQ TileNotFound
			CMP TileIndex 			; Compare with the fetched tile index
			BEQ FoundTile  			; Branch if it is a walkable tile
			INX
			JMP CheckWalkable
.endproc

.proc draw ; ------------------------------------------------------------------------------------------+
	; SAVE REGISTER INTO THE STACK
	PHP						; Push the Processor Status Register onto the stack
	PHA						; Push the Accumulator Register onto the stack
	TXA						; Transfer X into the Accumulator
	PHA						; Push X (the accumulator register) onto the stack
	TYA						; Transfer Y into the Accumulator
	PHA						; Push Y (the accumulator register) onto the stack

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

.proc drawAnimation	; ------------------------------------------------------------------------------------------+
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

		; LDA directionOffSet
		; CLC
		; ADC #$04     
		; STA directionOffSet	; Increase direction offset by four (1 tile)

    LDA animationState
    CMP #$03               ; 4 = total animation states
    BCC continue           ; If animationState < 4, proceed to drawing

    ; Reset animation state to 0 if it reaches the limit
    LDA #$00
    STA animationState
		; STA directionOffSet

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
	; CLC
	; ADC directionOffSet
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
        CPX #$18            ; Compare X, If X > 24 (6 patterns tables)
        BNE load_palettes   ;

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
		.byte $2C, $00, $00, $00
		.byte $2C, $00, $00, $00
		.byte $2C, $00, $00, $00
		.byte $2C, $00, $00, $00

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
.byte $61, $65, $69, $6D, $79, $7D, $89, $89, $89, $89, $89, $79, $7D, $89, $91, $5D, $61, $65, $69, $71, $81, $85, $8D, $8D, $8D, $8D, $8D, $81, $85, $8D, $B1, $5D, $61, $65, $99, $75, $75, $75, $75, $75, $75, $75, $75, $75, $75, $A1, $B1, $5D, $61, $65, $99, $9D, $9D, $9D, $9D, $9D, $9D, $9D, $9D, $9D, $99, $69, $B1, $5D, $61, $65, $69, $B9, $A9, $A9, $A9, $A9, $A9, $A9, $A9, $BD, $65, $69, $B1, $5D, $61, $65, $69, $B1, $5D, $C9, $89, $79, $7D, $89, $91, $61, $65, $69, $B1, $5D, $61, $65, $69, $B1, $5D, $61, $8D, $81, $85, $8D, $B1, $61, $65, $69, $B1, $5D, $61, $65, $69, $B1, $5D, $61, $CD, $75, $75, $A1, $B1, $61, $65, $69, $6D, $89, $61, $65, $69, $B1, $5D, $61, $65, $99, $99, $69, $D1, $D5, $65, $69, $71, $8D, $61, $65, $69, $B1, $5D, $61, $65, $99, $99, $69, $B1, $61, $65, $99, $75, $75, $61, $65, $69, $6D, $89, $C1, $65, $99, $99, $69, $B1, $61, $A5, $9D, $9D, $9D, $61, $65, $69, $71, $8D, $C5, $65, $99, $99, $69, $B1, $95, $A9, $A9, $A9, $A9, $61, $65, $99, $75, $75, $75, $99, $99, $99, $69, $B1, $5D, $5D, $5D, $5D, $5D, $61, $65, $9D, $9D, $9D, $9D, $9D, $9D, $9D, $B5, $B1, $5D, $5D, $5D, $5D, $5D, $95, $A9, $A9, $A9, $A9, $A9, $A9, $A9, $A9, $A9, $AD, $5D, $5D, $5D, $5D, $5D, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55, $55
attribute_screen4:
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55,$55
	.byte $55,$55,$55,$55,$55,$55,$55,$55,$05,$05,$05,$05,$05,$05,$05,$05

walkable_addresses:
	.byte $05, $12, $14, $15, $16, $17, $18, $19, $1B, $1D, $1E, $1F, $20, $2A, $2C, $2D, $2F, $36, $38, $39, $3B, $FF

.segment "CHARS"
.incbin "graphics.chr"
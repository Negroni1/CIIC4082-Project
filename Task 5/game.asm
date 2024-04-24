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
	hasMoved:		.res 1	; Reserve 1 byte for storing wether the frog moved last frame
	temp:		.res 1	; Reserve 1 byte for a temporal variable
	scroll_offset:				.res 1
	level_change:				.res 1
	background_offset_low:		.res 1
	background_offset_up:		.res 1
	background_tile_offset:		.res 1
	tileX:	.res 1
	tileY:	.res 1
	TileIndex: .res 1

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
	STA directionOffSet		; Set frog direction offset to 0
  STA $2005           ; PPU Scroll = 0
  STA $2005  
  STA temp   						; Set temp to 0
  STA hasMoved  			 	; Set hasMoved to 0
	STA tileX
	STA tileY
	STA TileIndex
	
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
   		 	BEQ check_left  ; If 0, button not pressed, check next
    		
    		LDA #$25
    		STA frogDirection ; Set frogDirection to 25 (Right)

				JSR checkWalkable
				CMP #$00
				BEQ update_done

				LDA frogX
    		INC frogX       ; Move right
				LDA #$01
				STA hasMoved ; Set hasMoved to 1

				LDA scroll_offset
				CLC
				ADC #$01
				STA scroll_offset
    		JMP update_done

				check_left:
    				LDA temp
    				AND #%00000010  ; Isolate Left button
    				BEQ check_up    ; If 0, button not pressed, check next
    				
						STA frogDirection ; Set frogDirection to 01 (Left)
						STA hasMoved ; Set hasMoved to 1

						JSR checkWalkable
						CMP #$00
						BEQ update_done

						LDA frogX
    				DEC frogX       ; Move left
    				LDA #$01
						LDA scroll_offset
						DEC scroll_offset
    				JMP update_done

				check_up:
    				LDA temp
    				AND #%00001000  ; Isolate Up button
    				BEQ check_down  ; If 0, button not pressed, check next
    				
						LDA #$19
    				STA frogDirection ; Set frogDirection to 19 (Up)

						JSR checkWalkable
						CMP #$00
						BEQ update_done

						LDA frogY
    				DEC frogY       ; Move up
						LDA #$01
						STA hasMoved ; Set hasMoved to 1
    				JMP update_done

				check_down:
    				LDA temp
   				  AND #%00000100  ; Isolate Down button
    				BEQ no_movement ; If 0, button not pressed, no movement was done
    				
    				LDA #$0D
    				STA frogDirection ; Set frogDirection to 0D (Down)

						JSR checkWalkable
						CMP #$00
						BEQ update_done

						LDA frogY
    				INC frogY       ; Move down
						LDA #$01
						STA hasMoved ; Set hasMoved to 1
					JMP update_done
					
				no_movement:
						LDA #$00
						STA hasMoved ; Set hasMoved to 0
						STA directionOffSet
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

; Compare direction and branch
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

; Move Left
Left_Tile:
    LDA frogX
    SEC        ; Set carry for subtraction
    SBC #$01   ; Subtract 1 from A, considering carry
    STA tileX  ; Store the result in tileX
    LDA frogY
    STA tileY  ; Copy Y as is
    JMP Calculate_Index

; Move Down
Down_Tile:
    LDA frogY
    SEC        ; Set carry for subtraction
    SBC #$01   ; Subtract 1 from A, considering carry
    STA tileY  ; Store the result in tileY
    LDA frogX
    STA tileX  ; Copy X as is
    JMP Calculate_Index

; Move Up
Up_Tile:
    LDA frogY
    CLC        ; Clear carry for addition
    ADC #$01   ; Add 1 to A, considering carry
    STA tileY  ; Store the result in tileY
    LDA frogX
    STA tileX  ; Copy X as is
    JMP Calculate_Index

; Move Right
Right_Tile:
    LDA frogX
    CLC        ; Clear carry for addition
    ADC #$01   ; Add 1 to A, considering carry
    STA tileX  ; Store the result in tileX
    LDA frogY
    STA tileY  ; Copy Y as is
    JMP Calculate_Index

Calculate_Index:
    ; Adjust tileX with the horizontal scroll offset
    LDA tileX
    CLC
    ADC scroll_offset      ; Add scroll offset to tileX
    STA tileX              ; Store back to tileX

    ; Convert X to tile coordinate
    LSR A
    LSR A
    LSR A                 ; tileX / 8 to get the tile X coordinate
    STA tileX

    ; Convert Y to tile coordinate
    LDA tileY
    LSR A
    LSR A
    LSR A                 ; tileY / 8 to get the tile Y coordinate
    STA tileY

    ; Calculate Name Table address
    ; $2000 is the start of the Name Table
    LDA tileY
    ASL A
    ASL A
    ASL A                 ; TileY * 8 (row offset in Name Table)
    STA $00               ; Store temporarily in zero-page address $00

    LDA tileX
    CLC
    ADC $00               ; Add TileY * 8 (from $00) and TileX
    STA tileX             ; Store result back to tileX for further calculation

    ; Assume $2000 is already set in the high byte, need to adjust the actual address
    LDA tileX
    CLC
    ADC #$00              ; Adjust low byte of address if necessary due to addition carry
    STA $2006             ; Store the lower byte of address

    LDA #$20              ; High byte of the base name table address
    ADC #0                ; Add carry from previous addition
    STA $2006+1           ; Store the upper byte of address


	; Set the PPU Address to read
	LDA 2006+1
	STA $2006
	LDA 2006
	STA $2006

	; Read the tile index from PPU Data port
	LDA $2007       ; First read after setting address gets the tile index
	STA TileIndex

	; RESTORE REGISTERS & RETURN
	PLA						; Pull Y of the stack and place it into the accumulator	register
	TAY						; Restore/Transfer the accumulator into Y
	PLA						; Pull X of the stack and place it into the accumulator	register
	TAX						; Restore/Transfer the accumulator into X
	PLA						; Pull the top value of the stack and place it into the accumulator register
	PLP						; Pull the top value of the stack and place it into the  processor status register
	RTS						; Return from subroutine
.endproc

.proc checkWalkable ; ------------------------------------------------------------------------------------------+
	JSR readTile
	
	LDX #$00         ; Start of the table
	CheckWalkable:
			LDA walkable_addresses, X
			CMP #$FF     ; Let's say $FF marks the end of your walkable tile list
			BEQ NotWalkable
			CMP TileIndex ; Compare with the fetched tile index
			BEQ Walkable  ; Branch if it is a walkable tile
			INX
			JMP CheckWalkable

	Walkable:
			LDA #$01
			RTS

	NotWalkable:
			LDA #$00
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


.proc drawBackground	; ------------------------------------------------------------------------------------------+
	; WRITE BACKGROUND DATA ---------------------------------------------------------------+
	; SAVE REGISTER INTO THE STACK
	PHP						; Push the Processor Status Register onto the stack
	PHA						; Push the Accumulator Register onto the stack
	TXA						; Transfer X into the Accumulator
	PHA						; Push X (the accumulator register) onto the stack
	TYA						; Transfer Y into the Accumulator
	PHA						; Push Y (the accumulator register) onto the stack
	
	; ; BACKGROUND 1 ----------------------------------------------------------------------+
	; ; ; UPPER ---------------------------------------------------------------------------+
	LDX 00
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
			LDA background_screen1, X 
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
			LDA background_screen1, X       ; Load the background tile into X
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

	; ATTRIBUTE TABLE BACKGROUND 1
	LDA $2002
	LDA #$23
	STA $2006
	LDA #$C0
	STA $2006

	LDX #$00
	load_attribute: 
		LDA attribute_screen1, X       			; Iterate through the attributes to draw them
		STA $2007         				; Store X into PPUDATA
        INX                 			; Increase X
        CPX #$40            			; Compare X, If X > 192 (12 16bit sprites) stop the loop
        BNE load_attribute

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
		; .byte $2C, $00, $00, $00
		; .byte $2C, $00, $00, $00
		; .byte $2C, $00, $00, $00
		; SPRITE PALETTE  --------------------------------------------------------------+
		.byte $2B, $0f, $3A, $20    ; green, black, green, white
		.byte $2B, $0f, $1B, $3B    ; green, black, green, green

background_screen1:
	.byte $05, $09, $09, $09, $09, $09, $09, $09, $09, $09, $0D, $01, $05, $09, $09, $0D, $11, $1D, $1D, $1D, $15, $1D, $1D, $1D, $1D, $15, $19, $01, $11, $15, $15, $19, $11, $1D, $15, $1D, $1D, $1D, $15, $1D, $1D, $1D, $19, $01, $11, $15, $15, $19, $21, $25, $29, $1D, $2D, $25, $25, $25, $29, $1D, $19, $01, $11, $15, $15, $19, $45, $01, $11, $1D, $19, $01, $05, $09, $35, $1D, $39, $09, $35, $15, $15, $19, $01, $53, $11, $1D, $19, $01, $11, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $19, $01, $01, $11, $1D, $19, $01, $11, $1D, $2D, $25, $29, $1D, $2D, $25, $25, $31, $09, $09, $35, $1D, $19, $01, $11, $1D, $19, $3D, $11, $1D, $19, $01, $01, $01, $1D, $1D, $1D, $1D, $19, $01, $11, $1D, $19, $41, $11, $1D, $19, $01, $01, $01, $25, $25, $25, $25, $31, $01, $11, $1D, $39, $09, $35, $1D, $39, $09, $09, $09, $01, $45, $01, $01, $49, $01, $11, $1D, $15, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $01, $4D, $51, $55, $01, $01, $21, $25, $29, $1D, $2D, $25, $25, $25, $25, $25, $05, $09, $09, $09, $09, $09, $09, $09, $35, $1D, $39, $09, $09, $09, $09, $0D, $11, $1D, $15, $1D, $1D, $15, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $1D, $19, $21, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $25, $31, $BA, $AA, $AA, $AA, $FB, $FA, $FA, $AA, $28, $AA, $AA, $AA, $AA, $AA, $AF, $AF	; .byte $11,$1d,$1d,$1d,$15,$1d,$1d,$1d,$1d,$15,$19,$01,$11,$15,$15,$19

attribute_screen1:
	.byte $ba,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$fb,$fa,$fa,$fa,$fa,$aa,$aa,$aa
	.byte $28,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$af,$ab,$af,$af
	.byte $ff,$ff,$ba,$aa,$aa,$aa,$aa,$aa,$22,$0a,$a8,$fa,$ba,$fa,$fa,$fa
	.byte $ba,$aa,$aa,$aa,$aa,$aa,$aa,$aa,$0f,$0f,$0f,$0f,$0f,$0f,$0f,$0f

walkable_addresses:
	.byte $05, $00, $12, $14, $15, $16, $17, $18, $19, $1B, $1D, $1E, $1F, $20, $2A, $2C, $2D, $2F, $36, $38, $39, $3B, $FF

.segment "CHARS"
.incbin "graphics.chr"
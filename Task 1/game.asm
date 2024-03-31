.segment "HEADER"           ; Assembler Directive (.), HEADER
.byte $4e, $45, $53, $1a    ; Magic string that always begins an iNES header
.byte $02                   ; Number of 16KB PRG-ROM banks
.byte $01                   ; Number of 8KB CHR-ROM banks
.byte %00000001             ; Vertical mirroring, no save RAM, no mapper
.byte %00000000             ; No special-case flags set, no mapper
.byte $00                   ; No PRG-RAM present
.byte $00                   ; NTSC format

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
    JMP main
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

    ; WRITE SPRITE DATA ---------------------------------------------------------------+
    LDX #$00                ; Set X to 0
    load_sprites:           ; Iterate through the sprites to draw them
        LDA sprites,X       ; Load the sprites into X
        STA $0200,X         ; Store X into 0200
        INX                 ; Increase X
        CPX #$CA            ; Compare X, If X > 192 (12 16bit sprites) stop the loop
        BNE load_sprites
    
    ; BACKGROUND TILES ---------------------------------------------------------------+
	; (TODO) Loop
    LDA $2002
	LDA #$22
	STA $2006
	LDA #$0C
	STA $2006
	LDX #$04
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$0D
	STA $2006
	LDX #$05
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$0E
	STA $2006
	LDX #$0D
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$0F
	STA $2006
	LDX #$0C
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$10
	STA $2006
	LDX #$09
	STX $2007

	LDA $2002
	LDA #$22
	STA $2006
	LDA #$11
	STA $2006
	LDX #$0A
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$12
	STA $2006
	LDX #$02
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$2C
	STA $2006
	LDX #$03
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$2D
	STA $2006
	LDX #$0C
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$2D
	STA $2006
	LDX #$07
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$2E
	STA $2006
	LDX #$0E
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$2F
	STA $2006
	LDX #$0F
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$30
	STA $2006
	LDX #$10
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$31
	STA $2006
	LDX #$11
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$32
	STA $2006
	LDX #$03
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$33
	STA $2006
	LDX #$01
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$4C
	STA $2006
	LDX #$32
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$4D
	STA $2006
	LDX #$33
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$4E
	STA $2006
	LDX #$13
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$4F
	STA $2006
	LDX #$14
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$50
	STA $2006
	LDX #$15
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$51
	STA $2006
	LDX #$16
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$52
	STA $2006
	LDX #$17
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$53
	STA $2006
	LDX #$18
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$6C
	STA $2006
	LDX #$19
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$6D
	STA $2006
	LDX #$1A
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$6E
	STA $2006
	LDX #$2C
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$6F
	STA $2006
	LDX #$2C
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$70
	STA $2006
	LDX #$2D
	STX $2007

    LDA $2002
	LDA #$22
	STA $2006
	LDA #$71
	STA $2006
	LDX #$35
	STX $2007
    
    LDA $2002
	LDA #$22
	STA $2006
	LDA #$72
	STA $2006
	LDX #$36
	STX $2007

	LDA $2002
	LDA #$22
	STA $2006
	LDA #$73
	STA $2006
	LDX #$39
	STX $2007

    ; ATTRIBUTE TABLE --------------------------------------------------------------+
	LDA $2002
	LDA #$23
	STA $2006
	LDA #$E3
	STA $2006
	LDA #%01011110
	STA $2007

    LDA $2002
	LDA #$23
	STA $2006
	LDA #$E4
	STA $2006
	LDA #%01010010
	STA $2007

    vblankwait:         ; wait for another vblank before continuing
        BIT $2002       ; 2002 = PPU status
        BPL vblankwait
        LDA #%10010000  ; turn on NMIs, sprites use first pattern table
        STA $2000       ; Store A in PPU Control
        LDA #%00011110  ; turn on screen
        STA $2001       ; Store A in PPU Mask

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

    sprites:
    ; LEFT  ---------------------------------------------------------------------------------------------------+
		; STANDING
		.byte $40, $01, 00, $60    	; y = 40, tile number = 01, Special attribute flags - palette = 00, x = 60
		.byte $40, $02, 00, $68    	; y = 40, tile number = 02, Special attribute flags - palette = 00, x = 68
		.byte $48, $03, 01, $60    	; y = 48, tile number = 03, Special attribute flags - palette = 01, x = 60
		.byte $48, $04, 01, $68    	; y = 48, tile number = 04, Special attribute flags - palette = 01, x = 68
    	; JUMPING 1
		.byte $40, $05, 00, $70    	; y = 40, tile number = 05, Special attribute flags - palette = 00, x = 70
		.byte $40, $06, 00, $78    	; y = 40, tile number = 06, Special attribute flags - palette = 00, x = 78
		.byte $48, $07, 01, $70    	; y = 48, tile number = 07, Special attribute flags - palette = 01, x = 70
		.byte $48, $08, 01, $78    	; y = 48, tile number = 08, Special attribute flags - palette = 01, x = 78
    	; JUMPING 2
		.byte $40, $09, 00, $80    	; y = 40, tile number = 09, Special attribute flags - palette = 00, x = 80
		.byte $40, $0A, 00, $88    	; y = 40, tile number = 0A, Special attribute flags - palette = 00, x = 88
		.byte $48, $0B, 01, $80    	; y = 48, tile number = 0B, Special attribute flags - palette = 01, x = 80
		.byte $48, $0C, 01, $88    	; y = 48, tile number = 0C, Special attribute flags - palette = 01, x = 88

    ; RIGTH  --------------------------------------------------------------------------------------------------+
		; STANDING
		.byte $50, $25, $00, $60    ; y = 50, tile number = 25, Special attribute flags - palette = 00, x = 60
		.byte $50, $26, $00, $68    ; y = 50, tile number = 26, Special attribute flags - palette = 00, x = 68
		.byte $58, $27, $01, $60    ; y = 50, tile number = 27, Special attribute flags - palette = 01, x = 60
		.byte $58, $28, $01, $68    ; y = 50, tile number = 28, Special attribute flags - palette = 01, x = 68
		; JUMPING 1
		.byte $50, $29, $00, $70    ; y = 50, tile number = 29, Special attribute flags - palette = 00, x = 70
		.byte $50, $2A, $00, $78    ; y = 50, tile number = 2A, Special attribute flags - palette = 00, x = 78
		.byte $58, $2B, $01, $70    ; y = 58, tile number = 2B, Special attribute flags - palette = 01, x = 70
		.byte $58, $2C, $01, $78    ; y = 58, tile number = 2C, Special attribute flags - palette = 01, x = 78
		; JUMPING 
		.byte $50, $2D, $00, $80    ; y = 50, tile number = 2D, Special attribute flags - palette = 00, x = 80
		.byte $50, $2E, $00, $88    ; y = 50, tile number = 2E, Special attribute flags - palette = 00, x = 88
		.byte $58, $2F, $01, $80    ; y = 58, tile number = 2F, Special attribute flags - palette = 01, x = 80
		.byte $58, $30, $01, $88    ; y = 58, tile number = 30, Special attribute flags - palette = 01, x = 88
    
	; DOWN  ---------------------------------------------------------------------------------------------------+
		; STANING
		.byte $60, $0D, 00, $60    	; y = 60, tile number = 0D, Special attribute flags - palette = 00, x = 60
		.byte $60, $0E, 00, $68    	; y = 60, tile number = 0E, Special attribute flags - palette = 00, x = 68
		.byte $68, $0F, 01, $60    	; y = 68, tile number = 0F, Special attribute flags - palette = 01, x = 60
		.byte $68, $10, 01, $68    	; y = 68, tile number = 10, Special attribute flags - palette = 01, x = 68
		; JUMPING 1 
		.byte $60, $11, 00, $70    	; y = 60, tile number = 11, Special attribute flags - palette = 00, x = 70
		.byte $60, $12, 00, $78    	; y = 60, tile number = 12, Special attribute flags - palette = 00, x = 78
		.byte $68, $13, 01, $70    	; y = 68, tile number = 13, Special attribute flags - palette = 01, x = 70
		.byte $68, $14, 01, $78    	; y = 68, tile number = 14, Special attribute flags - palette = 01, x = 78
		; JUMPING 2
		.byte $60, $15, 00, $80    	; y = 60, tile number = 15, Special attribute flags - palette = 00, x = 80
		.byte $60, $16, 00, $88    	; y = 60, tile number = 16, Special attribute flags - palette = 00, x = 88
		.byte $68, $17, 01, $80    	; y = 68, tile number = 17, Special attribute flags - palette = 01, x = 80
		.byte $68, $18, 01, $88    	; y = 68, tile number = 18, Special attribute flags - palette = 01, x = 88

	; UP  -----------------------------------------------------------------------------------------------------+
		; STANDING
		.byte $70, $19, 00, $60    	; y = 70, tile number = 19, Special attribute flags - palette = 00, x = 60
		.byte $70, $1A, 00, $68    	; y = 70, tile number = 1A, Special attribute flags - palette = 00, x = 68
		.byte $78, $1B, 01, $60    	; y = 70, tile number = 1B, Special attribute flags - palette = 01, x = 60
		.byte $78, $1C, 01, $68    	; y = 70, tile number = 1C, Special attribute flags - palette = 01, x = 68
		; JUMPING 1
		.byte $70, $1D, 00, $70    	; y = 70, tile number = 1D, Special attribute flags - palette = 00, x = 70
		.byte $70, $1E, 00, $78    	; y = 70, tile number = 1E, Special attribute flags - palette = 00, x = 78
		.byte $78, $1F, 01, $70    	; y = 70, tile number = 1F, Special attribute flags - palette = 01, x = 70
		.byte $78, $20, 01, $78    	; y = 70, tile number = 20, Special attribute flags - palette = 01, x = 78
		; JUMPING 2	
		.byte $70, $21, 00, $80    	; y = 70, tile number = 21, Special attribute flags - palette = 00, x = 80
		.byte $70, $22, 00, $88    	; y = 70, tile number = 22, Special attribute flags - palette = 00, x = 88
		.byte $78, $23, 01, $80    	; y = 70, tile number = 23, Special attribute flags - palette = 01, x = 80
		.byte $78, $24, 01, $88    	; y = 70, tile number = 24, Special attribute flags - palette = 01, x = 88

.segment "CHARS"
.incbin "graphics.chr"
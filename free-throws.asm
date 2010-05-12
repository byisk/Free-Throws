	processor 6502
	include vcs.h
	include macro.h
	org $F000

; Here will be defined bytes of memory for variables
Section = $80; If 0 then it runs Interaction section, if 1 it runs Playfield

Start
	CLEAN_START

	lda #$00
	sta COLUBK
	lda #$90
	sta COLUP0
	lda #$40
	sta COLUPF
	ldx #1
	stx CTRLPF  ; Make playfield reflected
	ldx #2
	stx Section

;VSYNC time
MainLoop
	lda #2
	sta VSYNC
	sta WSYNC
	sta WSYNC
	sta WSYNC
	lda #43
	sta TIM64T
	lda #0
	sta VSYNC

	lda INPT4
	bmi TheGame
	lda Section
	cmp #1
	beq MakePlayfield
MakeInteraction
	dec Section
	jmp TheGame
MakePlayfield
	inc Section
TheGame

; There below is a code used for drawing playfield. There will be an animation showing a shooting a ball in the future. Frame section.

	lda Section
	cmp #1
	beq Interaction
	jmp Playfield

Interaction
	lda INTIM
	bne Interaction
	ldx #0
	stx PF0
	sta WSYNC
	sta HMOVE
	sta WSYNC
	sta VBLANK
	sta WSYNC
	ldy #97
	jsr ScanLoop
	ldx #%11000000
	stx PF2
	ldy #8
	jsr ScanLoop
	ldy #16
	ldx #2
	stx ENAM0
	ldx #%00110000
	stx NUSIZ0
	jsr ScanLoop
	ldy #8
	ldx #0
	stx ENAM0
	jsr ScanLoop
	ldx #0
	stx PF2
	ldy #97
	jsr ScanLoop
	jmp OverScan

Playfield
	lda INTIM
	bne Playfield
	ldx #%00100000
	stx PF0
	ldx #%00000010
	stx PF1
	sta WSYNC
;	ldy #70
	sta WSYNC
	sta VBLANK
	sta WSYNC
;	jsr ScanLoop
	ldx #%11111100
	stx PF2
	ldy #6
	jsr ScanLoop
	ldx #%10000000
	stx PF2
	ldy #4
	jsr ScanLoop
	ldx #%11000000
	stx PF2
	ldy #8
	jsr ScanLoop
	ldx #%01000000
	stx PF2
	ldy #16
;	ldx #2
;	stx ENAM0
;	ldx #%00110000
;	stx NUSIZ0
	jsr ScanLoop
	ldx #%11000000
	stx PF2
	ldy #8
	jsr ScanLoop
	ldx #0
;	stx ENAM0
	stx PF2
	ldy #140
	jsr ScanLoop
	ldx #%00000001
	stx PF1
	ldx #$FF
	stx PF2
	ldy #8
	jsr ScanLoop
	ldx #0
	stx PF1
	stx PF2
	ldy #38
	jsr ScanLoop

	jmp OverScan

; There below will be a code used for interaction with a player. There will be a BL enabled moving horizontal and vertical and waiting for player's interaction (pressing the FIRE button).

	

ScanLoop
	sta WSYNC
	dey
	bne ScanLoop
	rts
	

OverScan
	lda #2
	sta WSYNC
	sta VBLANK
	ldx #30
OverScanWait
	sta WSYNC
	dex
	bne OverScanWait
	jmp  MainLoop

	org $FFFC
	.word Start
	.word Start


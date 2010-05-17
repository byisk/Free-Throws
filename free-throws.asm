	processor 6502
	include vcs.h
	include macro.h
	org $F000

; Here will be defined bytes of memory for variables
Section = $80; If 0 then it runs Interaction section, if 1 it runs Playfield
Speed = $81; This is used to control the Speed and Direction of the ball. HMM0 reads a value from that variable.

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
;	ldx #$80
;	stx Speed

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

CheckDifficulty
	lda SWCHB
	and #%01000000
	beq P0SetAmateur
	jmp P0SetPro

P0SetAmateur
	ldx #$C0
	stx Speed
	jmp TestButtonPress

P0SetPro
	ldx #$A0
	stx Speed

TestButtonPress
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

; There below is a code used for drawing playfield. There will be an animation showing a shooting a ball in the future. Frame section, WSYNC section.

	lda Section
	cmp #1
	beq Interaction
	jmp Playfield

Interaction
	lda INTIM
	bne Interaction
	ldx #0
	stx PF0
	ldx Speed
	stx HMM0
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
	ldx #%01000000
	stx PF2
	ldy #4
	jsr ScanLoop
	ldy #8
	ldx #2
	stx ENAM0
	ldx #%00100000
	stx NUSIZ0
	jsr ScanLoop
	ldy #4
	ldx #0
	stx ENAM0
	jsr ScanLoop
	ldx #%11000000
	stx PF2
	ldy #8
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

; There below is a ScanLoop mechanism used as a subroutine, because the playfield consist of more than one piece, so it's more efficent to do this that way.

ScanLoop
	sta WSYNC
	dey
	bne ScanLoop
	rts

; OverScan section. The end of a whole frame.

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



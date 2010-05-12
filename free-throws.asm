	processor 6502
	include vcs.h
	include macro.h
	org $F000

; Here will be defined bytes of memory for variables

Start
	CLEAN_START

	lda #$00
	sta COLUBK
	lda #$40
	sta COLUPF
	ldx #$1
	stx CTRLPF  ; Make playfield reflected

;VSYNC time
MainLoop
	lda  #2
	sta  VSYNC
	sta  WSYNC
	sta  WSYNC
	sta  WSYNC
	lda  #43
	sta  TIM64T
	lda  #0
	sta  VSYNC

WaitForVblankEnd
	lda INTIM
	bne WaitForVblankEnd

	ldy #90
	sta WSYNC
	sta VBLANK
	sta WSYNC
	jsr ScanLoop
	ldx #%11100000
	stx PF2
	ldy #48
	jsr ScanLoop
	ldx #0
	stx PF2
	ldy #90
	jsr ScanLoop
	jmp OverScan

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


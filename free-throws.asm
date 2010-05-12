	processor 6502
	include vcs.h
	include macro.h
	org $F000

; Here will be defined bytes of memory for variables

Start
	CLEAN_START

	lda #$00
	sta COLUBK

;VSYNC time
MainLoop
	lda  #2
	sta  VSYNC
	sta  WSYNC
	sta  WSYNC
	sta  WSYNC
	lda  #43
	sta  TIM64T
	lda #0
	sta  VSYNC

WaitForVblankEnd
	lda INTIM
	bne WaitForVblankEnd
	ldy #227
	sta WSYNC
	sta VBLANK

	sta WSYNC

ScanLoop
	sta WSYNC
	dey		;decrement scanline counter
	bne ScanLoop	;lather rinse repeat


;overscan same as last time
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


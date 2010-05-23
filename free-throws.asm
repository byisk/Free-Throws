	processor 6502
	include vcs.h
	include macro.h
	org $F000

; Here will be defined bytes of memory for variables
Section = $80; If 1 then it runs Interaction section, if 2 it runs Playfield
SpeedRight = $81; This is used to control the Speed and Direction of the ball. HMM0 reads a value from that variable.
SpeedLeft = $82; The same function as above.
Direction = $83; should the value is set to 1, the move happens in the right direction; if 2 - left direction.
HorizontalMoveCounter = $84; It is decreased every horizontal move (HMOVE) to check if the ball reached the edge of the screen.
CheckDifficultyBlocker = $85; Prevents SWCHB from being checked. Difficulty is checked only when the ball reaches the edge of the
; screen. Because of this, HorizontalMoveCounter isn't rewritten every frame, only when the ball reaches the edge of the screen.
; a '1' means blocker isn't set, a '2' means it is set.
HorizontalVerticalSwitch = $86; It switches between horizontal and vertical moving.
CheckDrawVertical = $87; It checks if the vertical ball was initially set-up.
VerticalPosition = $88; The value of this variable says where to put the player from the top.
DrawInteractionPlayfield = $89; It is decreased to zero when drawing in loop.

Start
	CLEAN_START

	lda #$00
	sta COLUBK
	lda #$90
	sta COLUP0
	lda #$70
	sta COLUP1
	lda #$40
	sta COLUPF
	ldx #1
	stx CheckDifficultyBlocker ; Doesn't block CheckDifficulty section by default.
	stx CTRLPF  ; Make playfield reflected.
	stx Direction ; Move the ball in the right direction first.
	stx Section ; Show Interaction section before Playfield section.
	stx HorizontalVerticalSwitch
	stx CheckDrawVertical

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

	lda CheckDifficultyBlocker
	cmp #1
	beq CheckDifficulty
	jmp TestButtonPress

CheckDifficulty
	lda SWCHB
	and #%01000000
	beq P0SetAmateur
	jmp P0SetPro

P0SetAmateur
	ldx #$F0
	stx SpeedRight
	ldx #$10
	stx SpeedLeft
	ldx #68
	stx HorizontalMoveCounter
	inc CheckDifficultyBlocker	
	jmp TestButtonPress

P0SetPro
	ldx #$E0
	stx SpeedRight
	ldx #$20
	stx SpeedLeft
	ldx #34
	stx HorizontalMoveCounter
	inc CheckDifficultyBlocker

TestButtonPress
	lda INPT4
	bmi TheGame
	inc HorizontalVerticalSwitch
;	lda Section
;	cmp #1
;	beq MakePlayfield
;MakeInteraction
;	dec Section
;	jmp TheGame
;MakePlayfield
;	inc Section
TheGame

; There below is a code used for drawing the playfield. There will be an animation showing a shooting a ball in the future. Frame section, WSYNC section.

	lda Section
	cmp #1
	beq Interaction
	jmp Playfield

Interaction ; Interaction section

CheckDirection
	lda Direction
	cmp #1
	beq MoveRight

MoveLeft
	ldx SpeedLeft
	dec HorizontalMoveCounter
	bne MakeMove
	dec Direction
	dec CheckDifficultyBlocker
	jmp MakeMove

MoveRight
	ldx SpeedRight
	dec HorizontalMoveCounter
	bne MakeMove
	inc Direction
	dec CheckDifficultyBlocker

MakeMove
	lda HorizontalVerticalSwitch
	cmp #1
	bne DrawVerticalBall
	stx HMM0
	sta WSYNC
	sta HMOVE ; I diveded SpeedLeft and SpeedRight by two and insted of it, I put sta HMOVE two times to give more precision.
	sta HMOVE
	jmp MakeMoveLoop

DrawVerticalBall
	lda CheckDrawVertical
	cmp #1
	bne MakeVerticalMove
	ldx #%00001111
	stx GRP1

; Positioning the ball to the center.

	ldx #$90
	stx HMP1
	sta WSYNC
	sta HMOVE
	sta HMOVE
	sta HMOVE
	sta HMOVE
	sta HMOVE
	inc CheckDrawVertical
	jmp MakeMoveLoop

MakeVerticalMove
	; Here will be something awesome soon.

; There below is a loop waiting for a VBLANK end (TIM64T counts 64*43=2752 and 2752 diveded by 76 machine cycles gives
; 36.2 so it's almost 37 and Vertical Blank lasts 37 scanlines indeed.

MakeMoveLoop
	lda INTIM
	bne MakeMoveLoop
	sta WSYNC
	sta VBLANK
	sta WSYNC
	ldy VerticalPosition
	ldx #0
	stx PF0
	ldx #97
	jsr Loop0
	ldx #%11000000
	stx PF2
	ldx #8
	jsr Loop0
	ldx #%01000000
	stx PF2
	ldx #4
	jsr Loop0
	ldx #2
	stx ENAM0
	ldx #%00100000
	stx NUSIZ0
	ldx #8
	jsr Loop0
	ldx #0
	stx ENAM0
	ldx #4
	jsr Loop0
	ldx #%11000000
	stx PF2
	ldx #8
	jsr Loop0
	ldx #0
	stx PF2
	ldx #97
	jsr Loop0
	jmp OverScan

Loop0
	stx DrawInteractionPlayfield
Loop1
	sta WSYNC
;	jsr DrawPlayer1
	dec DrawInteractionPlayfield
	bne Loop1
	rts

Playfield ; Playfield section
	ldx #%00100000 ; Draw borders of the playground for a whole frame. The rest of drawing is done in DrawPlayfield
	stx PF0 ; becausie PF values are changed line-by-line during a ScanLine.
	ldx #%00000010
	stx PF1
	
; The same code as above (MakeMove) has to count when to finish VBLANK.

DrawPlayfield
	lda INTIM
	bne DrawPlayfield
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





;*******************************************************
;*             This code is based on the               *
;*                                                     *
;*      AsmOne example coded by Rune Gram-Madsen       *
;*                                                     *
;*******************************************************

ScreenHeight = 256	;200 for NTSC
KB2_SIZE = 128

J	BSR.S	STARTUP
	BEQ.S	.ERROR		; An error ?

        BSR	start
	BRA.S	CLOSEDOWN	; Closedown

.ERROR:	RTS

STARTUP:
	MOVE.L	A7,ERROR_STACK(A3)	; Save stack pointer if an error
	BSR	TASK_FIND
	BSR	INTULIB_OPEN
	BSR	GRAPHLIB_OPEN
	BSR	SCREEN_OPEN
	BSR	WINDOW_OPEN
	BSR	KEYB_INIT
	BSR	COLORS_SET
	MOVEQ	#-1,D0		; Set ok value
	RTS

STARTUP_ERROR:
	MOVE.L	ERROR_STACK(A3),A7	; Restore old stackpointer
	MOVEQ	#0,D0		; Set error value
	RTS			; Return to main to main routine

CLOSEDOWN:
	BSR	KEYB_EXIT
	BSR	WINDOW_CLOSE
	BSR	SCREEN_CLOSE
	BSR	GRAPHLIB_CLOSE
	BRA	INTULIB_CLOSE

TASK_FIND:
	SUB.L	A1,A1		; a1 = 0 Our task
	MOVE.L	4.W,A6
	JSR	FINDTASK(A6)
	MOVE.L	D0,TASK_PTR(A3)	; Store the pointer for our task
	RTS

COLORS_SET:
	MOVE.L	SCREEN_HANDLE(A3),A0
	LEA.L	44(A0),A0		; Get the screens viewport

	MOVE.L	GRAPHICS_BASE(A3),A6
	LEA.L	COLORS(A3),A1		; Pointer to the color list
	MOVEQ	#8,D0			; 8 colors to set
	JMP	LOADRGB4(A6)		; Set the colors

KEYB_INIT:
	MOVE.L	4.W,A6
	LEA	CONSOLE_NAME(A3),A0	; Pointer to "Console.Device"
	LEA	IO_REQUEST(A3),A1	; Io Buffer
	MOVEQ	#-1,D0			; Flags
	MOVEQ	#0,D1			; Unit
	JSR	OPENDEVICE(A6)		
	TST.L	D0			; An error
	BNE.S	STARTUP_ERROR

	MOVE.L	IO_REQUEST+20,CONSOLE_DEVICE(A3)	; Get console device
	MOVE.L	WINDOW_HANDLE(A3),A0
	MOVE.L	$56(A0),KEY_PORT(A3)	; Get this windows keyport
	RTS

KEYB_EXIT:
	LEA	IO_REQUEST(A3),A1
	MOVE.L	4.W,A6
	JMP	CLOSEDEVICE(A6)

INTULIB_OPEN:
	MOVE.L	4.W,A6
	LEA	INTUITION_NAME(A3),A1	; Pointer to "intuition.library"
	JSR	OldOpenLibrary(A6)
	MOVE.L	D0,INTUITION_BASE(A3)	; Store pointer
	BEQ	STARTUP_ERROR		; If error jump
	RTS

INTULIB_CLOSE:
	MOVE.L	4.W,A6
	MOVE.L	INTUITION_BASE(A3),A1
	JMP	CloseLibrary(A6)

GRAPHLIB_OPEN:
	MOVE.L	4.W,A6
	LEA	GRAPHICS_NAME(A3),A1	; Pointer to "graphics.library"
	JSR	OldOpenLibrary(A6)
	MOVE.L	D0,GRAPHICS_BASE(A3)
	BEQ	STARTUP_ERROR
	RTS

GRAPHLIB_CLOSE:
	MOVE.L	4.W,A6
	MOVE.L	GRAPHICS_BASE(A3),A1
	JMP	CloseLibrary(A6)

SCREEN_OPEN:
	LEA.L	SCREEN_DEFS(A3),A0
	MOVE.L	INTUITION_BASE(A3),A6
	JSR	OPENSCREEN(A6)
	MOVE.L	D0,SCREEN_HANDLE(A3)
	BEQ	STARTUP_ERROR

	MOVE.L	D0,A0
	;MOVEQ #0,D0
	;jsr ShowTitle(a6)
        ;movea.l SCREEN_HANDLE(A3),A0  ;;???
	LEA.L	$C0(A0),A0		; Get bitplane pointers
	LEA.L	BITPLANE1_PTR(A3),A1
	MOVE.L	(A0)+,(A1)+		; Bitplane 1
	MOVE.L	(A0)+,(A1)+		; Bitplane 2
	;MOVE.L	(A0)+,(A1)+		; Bitplane 3
	;MOVE.L	(A0)+,(A1)+		; Bitplane 4
	RTS

SCREEN_CLOSE:
	MOVE.L	SCREEN_HANDLE(A3),A0
	MOVE.L	INTUITION_BASE(A3),A6
	JMP	CLOSESCREEN(A6)

WINDOW_OPEN:
	MOVE.L	INTUITION_BASE(A3),A6	; Pointer to intuition library
	LEA	WINDOW_DEFS(A3),A0	; Pointer to window definitions
	JSR	OPENWINDOW(A6)
	MOVE.L	D0,WINDOW_HANDLE(A3)	; Store window handle
	BEQ	STARTUP_ERROR		; Error jump
	MOVE.L	TASK_PTR(A3),A0		; Get task pointer
	MOVE.L	$B8(A0),TASK_OLDWINDOW(A3)	; Store the old window
	MOVE.L	D0,$B8(A0)		; Make Reguesters turn up on this
	RTS				; Window

WINDOW_CLOSE:
	MOVE.L	TASK_PTR(A3),A0		; Get task ptr
	MOVE.L	TASK_OLDWINDOW(A3),$B8(A0)	; Restore old window
	MOVE.L	INTUITION_BASE(A3),A6
	MOVE.L	WINDOW_HANDLE(A3),A0
	JMP	CLOSEWINDOW(A6)

KEYB_STILLKEYSINBUFFER:
        move.w KEYB_OUTBUFFER(A3),d1  ; Increase out pointer
        addq.w #1,d1
        cmpi.w #KB2_SIZE,d1
        bne .l1

        moveq #0,d1
.l1:	move.w d1,KEYB_OUTBUFFER(A3)
        LEA	KEYB_BUFFER(A3),A0
	MOVE.B	(A0,D0.W),D0		; Get the oldest key
	RTS

KEYB_GETKEYS:
	MOVE.L	KEY_PORT(A3),A0	; Our key port
	MOVE.L	4.W,A6
	JSR	GETMSG(A6)	; Get the message
	MOVE.L	D0,KEY_MSG(A3)
	BEQ.S	KEYB_GETKEYS	; No message, jump again

KEYB_GETKEYS0:
	MOVE.L	D0,A4		; Msg now in A4
	MOVE.L	20(A4),D3	; Get message type

	MOVE.L	D3,D1
	AND.L	#RAWKEY,D1	; Was it a raw key ??
	;BEQ	KEYB_ANSWER	; If no just answer

	MOVE.W	24(A4),D4	; Key code
	BTST	#7,D4		; Bit 7 - Key release
	BNE	KEYB_ANSWER		; We dont need them

	MOVE.W	26(A4),D5	; QUALIFIER
	MOVE.L	28(A4),D6	; IADDRESS
	MOVE.W	D4,IECODE(A3)	; TRANSFER CODE
	MOVE.W	D5,IEQUAL(A3)	; QUALIFIERS
	MOVE.L	D6,IEADDR(A3)	; AND POINTER TO OLD KEYS

;---  Convert to ascii  ---
	LEA	MY_EVENT(A3),A0	; Pointer to event structure
	LEA	KEY_BUFFER(A3),A1	; Convert buffer
	MOVEQ	#80,D1		; Max 80 characters
	SUB.L	A2,A2		; A2 = 0 Keymap - Default
	MOVE.L	CONSOLE_DEVICE(A3),A6
	JSR	RAWKEYCONVERT(A6) ; Convert the rawkey into Ascii

;---  Copy keys to buffer  ---
; d0 = number of chars in the convert buffer
	SUBQ.W	#1,D0
	BMI.S	KEYB_ANSWER		; No chars ??
	LEA	KEY_BUFFER(A3),A1
	LEA	KEYB_BUFFER(A3),A0
	MOVE.W	KEYB_INBUFFER(A3),D1
.LOOP:	MOVE.B	(A1)+,(A0,D1.W)		; Copy the keys to the normal
	ADDQ.B	#1,D1			;  buffer.
        cmpi.w #KB2_SIZE,D1
        bne .l1

        moveq #0,d1
.l1:	DBF	D0,.LOOP
	MOVE.W	D1,KEYB_INBUFFER(A3)

;******* ANSWER KEYPRESS *******
KEYB_ANSWER:
	MOVE.L	KEY_MSG(A3),A1
	MOVE.L	4.W,A6
	JSR	REPLYMSG(A6)
        moveq #0,d0
        rts


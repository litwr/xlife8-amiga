     include "amiga.mac"

execbase = 4
AddPort = -354 ;create port
RemPort = -360 ;remove port
DoIo = -456 ;perform I/O
SendIo  = -462 ;start I/O
CurrentDir = -126

BUFSZ = 10000   ;must be greater or equal to filesize!

run:
    movea.l 4.w,a6
    lea dosname(pc),a1
    jsr OldOpenLibrary(a6)
    move.l d0,doslib
    sub.l a1,a1		; a1 = 0 Our task
	move.l 4.w,a6
	jsr FindTask(a6)
    move.l d0,a4
    tst.l $ac(a4)     ;pr_CLI: CLI or Workbench?
    bne .fromCLI

    lea.l $5c(a4),a0    ;WBench message
    jsr WaitPort(a6)
    lea.l $5c(a4),a0
    jsr GetMsg(a6)
    move.l d0,wbmsg
    movea.l d0,a0
    movea.l 36(a0),a0   ;Arglist
    move.l (a0),d1     ;Arglist[0]->Lock
    movea.l doslib(pc),a6
    jsr CurrentDir(a6)

.fromCLI:
    movea.l doslib(pc),a6
    move.l #manpage,d1
    move.l #MODE_OLD,d2
    jsr Open(a6)
    tst.l d0
    beq .doserror

    lea.l buffer(pc),a5
    move.l d0,filehl
    move.l d0,d1
    move.l a5,d2
    move.l #BUFSZ,d3
    jsr Read(a6)
    tst.l d0
    bgt .cont
    
    move.l filehl(pc),d1
    jsr Close(a6)
.doserror:
    bsr error\.w
.closedos:
    movea.l 4.w,a6
    move.l doslib(pc),a1
    jmp CloseLibrary(a6)

.cont:
    add.l a5,d0
    move.l d0,feof
    bsr .closedos

    movea.l 4.w,a6
    lea.l intname(pc),a1
    jsr OldOpenLibrary(a6)
    move.l d0,intbase

    movea.l d0,a6
    lea.l windowdef(pc),a0
    jsr OpenWindow(a6)
    move.l d0,windowhd

    movea.l 4.w,a6
    sub.l a1,a1
    jsr FindTask(a6)
    move.l d0,readreply+$10   ;set sigtask

    lea.l readreply(pc),a1
    jsr AddPort(a6)
    lea.l writerep(pc),a1
    jsr AddPort(a6)

    lea.l readio(pc),a1
    move.l windowhd(pc),readio+$28
    move.l #48,readio+$24
    clr.l d0
    clr.l d1
    lea.l devicename(pc),a0
    jsr OpenDevice(a6)
    tst.l d0
    bne error

    move.l readio+$14,writeio+$14  ;DEVICE
    move.l readio+$18,writeio+$18  ;UNIT
    bsr coninit
go:
    bsr iniconread
    moveq #21,d7
.loop:
    bsr conoutline
    bsr conoutnl
    dbra d7,.loop
    bsr conoutline
.loop2:
    movea.l 4.w,a6
    movea.l windowhd(pc),a0
    movea.l 86(a0),a0
    jsr GetMsg(a6)
    tst.l d0
    bne.s wevent

    lea.l readreply(pc),a0
    jsr GetMsg(a6)
    tst.l d0
    beq.s .loop2

    cmpi.b #$d,rbuffer
    bne.s no1

    bsr conoutnl
    bsr conoutline
    bsr iniconread
    bra.s .loop2
no1:
    cmp.b #' ',rbuffer
    bne.s no2

    bsr conoutnl
    bra.s go

no2:
    cmp.b #'q',rbuffer
    beq.s ende

    cmp.b #'Q',rbuffer
    beq.s ende
    bra.s go
    
wevent:
    movea.l d0,a0
    MOVE.L 20(a0),d3	;Get message type
    and.l #$200,d3      ;CLOSEWINDOW
    beq go

ende:
    lea.l readreply(pc),a1
    jsr RemPort(a6)
    lea.l readio(pc),a1
    jsr CloseDevice(a6)
    lea.l writerep(pc),a1
    jsr RemPort(a6)
error:
    movea.l intbase(pc),a6
    movea.l windowhd(pc),a0
    jsr CloseWindow(a6)
    movea.l a6,a1
    movea.l 4.w,a6
    jsr CloseLibrary(a6)
.w: tst.l wbmsg
    beq.s .e

    movea.l 4.w,a6
    jsr Forbid(a6)
    movea.l wbmsg(pc),a1
    jmp ReplyMsg(a6)
.e: rts

conoutline:     ;a5=buffer
    cmpa.l feof(pc),a5
    bcs.s .cont

    tst.l (sp)+
.loop2:
    moveq #7,d7
.loop3:
    mulu #100,d0
    dbra d7,.loop3
    bsr iniconread
    lea.l readreply(pc),a0
    jsr GetMsg(a6)
    tst.l d0
    beq.s .loop2
    bra.s ende

.cont:
    moveq #-1,d6
    movea.l a5,a0
.loop:
    addq.l #1,d6
    cmpi.b #$a,(a0)+
    bne.s .loop

    cmpa.l feof(pc),a0
    ble.s .cont2

    suba.l feof(pc),a0
    sub.l a0,d6
    movea.l feof(pc),a0
.cont2:
    lea.l writeio(pc),a1
    move.l a5,40(a1)
    movea.l a0,a5
    move.l d6,36(a1)   ;length
    bra.s conoutnl\.e

conoutnl:
    lea.l writeio(pc),a1
    move.l #nl,40(a1)
    move.l #2,36(a1)   ;length
.e: move.w #3,28(a1)   ;command: WRITE
    movea.l 4.w,a6
    move.l #writerep,14(a1)  ;set reply port
    jmp DoIo(a6)

coninit:
    lea.l writeio(pc),a1
    move.l #conini,40(a1)
    move.l #8,36(a1)   ;length
    bra.s conoutnl\.e

iniconread:
    movea.l 4.w,a6 ;start console input
    lea.l readio(pc),a1
    move.w #2,28(a1) ;command: READ
    move.l #rbuffer,40(a1)
    move.l #1,36(a1)  ;length
    move.l #readreply,14(a1) ;set reply port
    jmp SendIo(a6)

doslib dc.l 0
intbase dc.l 0
windowhd dc.l 0
filehl dc.l 0
readreply blk.l 8,0
writerep blk.l 8,0
feof dc.l 0

readio:
message blk.b 20,0
io blk.b 12,0
ioreq blk.b 16,0

writeio:
    blk.b 20,0
    blk.b 12,0
    blk.b 16,0

windowdef:
    dc.w 0,0 ;position
    dc.w 640,200 /size
    dc.b 0,1 /colors
    dc.l $200 ;IDCMP flags: CLOSEWINDOW
    dc.l $100e ;window flags: ACTIVATE+WINDOWDRAG+WINDOWDEPTH+WINDOWCLOSE
    dc.l 0 ;nogadgets
    dc.l 0 ;no menu check
    dc.l windname
    dc.l 0   ;screen
    dc.l 0  ;no bit map
    dc.w 640,200 ;min. size
    dc.w 640,200 ;max. size
    dc.w 1 ;screen type 1 = WBENCHSCREEN
wbmsg: dc.l 0

dosname  dc.b "dos.library",0
intname dc.b 'intuition.library',0
windname dc.b "More for Xlife-8 manpage",0
devicename dc.b 'console.device',0
manpage dc.b 'manpage.txt',0
nl dc.b $d,$a
conini dc.b $9b,$30,$20,$70,$9B,$3F,$37,$6C
rbuffer blk.b 80,0
buffer blk.b BUFSZ


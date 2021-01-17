copyr:   bsr totext
         move.l GRAPHICS_BASE(a3),a6
         movepenq 0,6
         color 2
         lea.l curpathsv(a3),a1
         bsr makepath2
         lea.l copyleft(a3),a0
         bsr fn2path\.loop
         move.l #curpathsv,d1
         move.l #MODE_OLD,d2
         move.l doslib(a3),a6
         jsr Open(a6)
         tst.l d0
         beq readtent\.e1     ;-> rts

         moveq #40,d7
         moveq #6,d6
         move.l d0,filehl(a3)     ;fh-save
.loop:   move.l filehl(a3),d1
         move.l #x0,d2
         moveq #1,d3
         move.l doslib(a3),a6
         jsr Read(a6)
         subq.l #1,d0
         bne .exit

         move.l #700,d0
.delay:  mulu #10,d1
         dbra d0,.delay

         cmpi.b #10,x0(a3)
         beq.s .nl

         moveq #1,d0
         lea.l x0(a3),a0
         movea.l RASTER_PORT(a3),a1
         move.l GRAPHICS_BASE(a3),a6
         jsr Text(a6)
         subq.b #1,d7
         bne.s .loop

.nl:     moveq #40,d7
         addq.l #8,d6
         moveq #0,d0
         move.l d6,d1
         move.l GRAPHICS_BASE(a3),a6
         movea.l RASTER_PORT(a3),a1
         jsr Move(a6)
         bra.s .loop

.exit:   bsr getkey
         bra loadpat\.exit2

setcolors:
         lea.l curpathsv(a3),a1
         bsr makepath2
         lea.l cf(a3),a0
         bsr fn2path\.loop
         move.l #curpathsv,d1
         move.l #MODE_OLD,d2
         move.l doslib(a3),a6
         jsr Open(a6)
         tst.l d0
         beq savepat\.error

         move.l d0,filehl(a3)     ;fh-save
         move.l d0,d1
         move.l #lightgreen,d2
         moveq #18,d3
         jsr Read(a6)
         cmpi.l #18,d0
         bne.s savecf\.e

         movea.l VIEW_PORT(a3),a0
         movea.l GRAPHICS_BASE(A3),a6
         lea.l COLORS(a3),a1   ; Pointer to the color list
         moveq #8,d0           ; 8 colors to set
         jsr LoadRGB4(a6)      ; Set the colors
         bra loadpat\.exit2

savecf:  lea.l curpathsv(a3),a1
         bsr makepath2
         lea.l cf(a3),a0
         bsr fn2path\.loop
         move.l #curpathsv,d1
         move.l #MODE_NEWFILE,d2
         movea.l doslib(a3),a6
         jsr Open(a6)
         tst.l d0
         beq savepat\.error

         move.l d0,filehl(a3)     ;fh-save
         move.l d0,d1
         move.l #lightgreen,d2
         moveq #18,d3
         jsr Write(a6)
         cmpi.l #18,d0
         beq loadpat\.exit2

.e:      bsr savepat\.error
         bra loadpat\.exit2

readtent:move.l filehl(a3),d1
         move.l #iobseg,d2
         move.l #iobseg_end-iobseg,d3
         jsr Read(a6)
         lsr.l d0
         move.l d0,tsz(a3)
         sub.l d0,filesz(a3)
.e1:     rts

loadpat: bsr makepath
         bsr fn2path
         move.l #curpath,d1       ;pointer to a pattern path
         move.l doslib(a3),a6     ;DOS base address
         move.l #-2,d2         ;'read' mode
         jsr Lock(a6)          ;find file
         beq readtent\.e1

         move.l d0,tmplock(a3)     ;lock-save
         move.l d0,d1
         move.l #iobseg,d2   ;pointer to FilelnfoBlock
         jsr Examine(a6)
         move.l iobseg+124,d1
         lsr.l d1
         swap d1
         tst.w d1
         bne readtent\.e1

         swap d1
         subq.l #3,d1
         bls readtent\.e1

         move.l d1,filesz(a3)
         move.l tmplock(a3),d1
         jsr UnLock(a6)
         move.l #curpath,d1
         move.l #MODE_OLD,d2
         move.l doslib(a3),a6
         jsr Open(a6)
         tst.l d0
         beq readtent\.e1

         move.l d0,filehl(a3)
         move.l live(a3),d6  ;save live&born
         move.l filehl(a3),d1
         move.l #x0,d2
         moveq.l #6,d3
         jsr Read(a6)
         cmpi.l #6,d0
         bne .l1

         move.b live+1(a3),d0
         or.b born+1(a3),d0
         cmpi.b #2,d0
         bcc .l1

         move.b born(a3),d0
         andi.b #1,d0
         beq .l2

.l1:     move.l d6,live(a3)
         bra .exit2

.l2:     move.w live(a3),d0    ;LSB -> MSB
         rol.w #8,d0
         move.w d0,live(a3)
         move.w born(a3),d0
         rol.w #8,d0
         move.w d0,born(a3)
         move.l d6,-(sp)
         bsr readtent   ;should adjust filesz
         bsr showrect
         move.l doslib(a3),a6
         move.l (sp)+,d6
         cmpi.b #27,d0
         beq .l1

         bsr fillrt
         bsr puttent
         tst.l filesz(a3)
         beq .exit2

.loop:   move.l filehl(a3),d1
         moveq.l #2,d3
         move.l #x0,d2
         jsr Read(a6)
         cmpi.l #2,d0
         bne .l1

         bsr putpixel
         subq.l #1,filesz(a3)
         bne .loop

.exit2:  move.l filehl(a3),d1
         move.l doslib(a3),a6
         jmp Close(a6)

printd0l:
         lea.l temp(a3),a1
         move.l d0,(a1)
         move.b #'4',d1
         sub.l #4000000000,d0
         bcc .l2

.l1:     subq.b #1,d1
         add.l #1000000000,d0
         bcs .l2

         subq.b #1,d1
         add.l #1000000000,d0
         bcc printd0_e

.l2:     move.l d0,(a1)
         lea.l ulformat(a3),a0
         move.b d1,(a0)
         bra printd0_e

printd0: lea.l temp(a3),a1
         move.w d0,(a1)
printd0_e:
         lea.l stuffChar(pc),a2
         move.l #-1,charCount(a3)
         move.l a3,-(sp)
         lea.l stringbuf(a3),a3
         movea.l 4.w,a6
         jsr RawDoFmt(a6)
         move.l (sp)+,a3
         move.l charCount(a3),d0
         lea.l stringbuf(a3),a0
         movea.l RASTER_PORT(a3),a1
         move.l GRAPHICS_BASE(a3),a6
         jmp Text(a6)

fnextcheck:
         moveq #1,d1
         tst.l iobseg+4
         bpl.s .exit          ;directory?

         movea.l #iobseg+8,a0
.l1:     tst.b (a0)+
         bne.s .l1

         move.l a0,d0
         sub.l #iobseg+8+1+4,d0   ;filename length
         bls.s .exit

         cmpi.b #".",-5(a0)
         bne.s .exit

         cmpi.b #"8",-4(a0)
         bne.s .exit

         move.b -3(a0),d1
         ori.b #32,d1
         cmpi.b #"x",d1
         bne.s .exit

         move.b -2(a0),d1
         ori.b #32,d1
         cmpi.b #"l",d1
         bne.s .exit

         clr.l d1
         move.b d1,-5(a0)
.exit:   rts

showdir: bsr totext
         clr.l d5   ;relative counter
         clr.l d6   ;absolute counter
         bsr makepath
         move.l #curpath,d1       ;pointer to a pattern path
         move.l doslib(a3),a6     ;DOS base address
         move.l #-2,d2         ;'read' mode
         jsr Lock(a6)          ;find file
         tst.l d0              ;found?
         bne .cont              ;no!
         rts

.cont:   move.l d0,tmplock(a3)     ;lock-save
         move.l d0,d1
         move.l #iobseg,d2   ;pointer to FilelnfoBlock
         jsr    Examine(a6)    ;get disk name
         tst.l d0              ;OK?
         beq showfree            ;no (rarely occurs)
         bra .outpuff           ;else output name

.loop:   ;* read filename
         move.l doslib(a3),a6     ;DOS base address
         move.l tmplock(a3),d1     ;key in D1
         move.l #iobseg,d2   ;pointer to FileInfoBlock
         jsr ExNext(a6)        ;find next file
         tst.l d0              ;found?
         beq showfree          ;no: done

.outpuff:bsr fnextcheck
         tst.b d1
         bne.s .loop

         lea.l svfn(a3),a4   ;check against a pattern in svfn
         lea.l iobseg+8,a5
         move.w d0,-(sp)
         bsr parse500
         move.l d0,d1
         move.w (sp)+,d0
         tst.b d1
         beq.s .loop

         cmpi.b #11+1,d0    ;we can show only 11 first chars of fn
         bcs.s .l2

         moveq #11,d0
.l2:     move.l GRAPHICS_BASE(a3),a6
         move.w d0,-(sp)
         clr.w d0
         btst #0,d5
         beq.s .l3

         move.w #20*8,d0
.l3:     move.w d5,d1
         lsr.w #1,d1
         addq.w #2,d1
         lsl.w #3,d1
         subq.w #2,d1
         movea.l RASTER_PORT(a3),a1
         jsr Move(a6)
         color 1    ;red
         move.w d6,d0
         lea.l nformat(a3),a0
         bsr printd0
         color 3    ;black
         move.w (sp)+,d0
         lea.l iobseg+8,a0
         movea.l RASTER_PORT(a3),a1
         ;movea.l GRAPHICS_BASE(a3),a6
         jsr Text(a6)
         print ' '
         invvideo
         move.w iobseg+124+2,d0
         lea.l sformat(a3),a0
         bsr printd0
         normvideo
         addq.l #1,d6
         cmp.w #1000,d6
         beq.s showfree

         addq.l #1,d5
         cmp.w #2*(25-2),d5
         bne .loop

         move.l #40,d1      ;40/50 sec (PAL), 40/60 sec (NTSC)
         move.l doslib(a3),a6     ;DOS base address
         jsr Delay(a6)
         bsr clrscn
         clr.l d5
         bra .loop

showfree:move.l tmplock(a3),d1   ;after showdir
         move.l #iobseg,d2
         jsr Info(a6)
         tst.l d0
         beq .l2

         lea.l iobseg+12,a0
         moveq.l #0,d0
         move.l (a0)+,d1  ;total blocks
         sub.l (a0)+,d1   ;used blocks
         bcs .l2

         move.l (a0),d0   ;block size in bytes
         beq .l2

         exg d1,d0
         moveq.l #0,d2
.l1:     addq.l #1,d2
         lsr #1,d1
         bcc .l1

         sub.l #11,d2
         beq .l2
         bmi .l3

         lsl.l d2,d0
         bra .l2

.l3:     neg.l d2
         lsr.l d2,d0
.l2:     move.l GRAPHICS_BASE(a3),a6
         movea.l RASTER_PORT(a3),a1
         move.l d0,-(sp)
         clr.w d0
         move.w d5,d1
         addq.w #1,d1
         lsr.w d1
         addq.w #2,d1
         lsl.w #3,d1
         subq.w #2,d1
         jsr Move(a6)
         lea.l lformat(a3),a0
         move.l (sp)+,d0
         bsr printd0l
         print 'K free'
         color 3  ;black

         move.l tmplock(a3),d1
         move.l doslib(a3),a6
         jmp UnLock(a6)

findfn:  ;fn# in D0
         move.l d0,d5
         moveq.l #-1,d6   ;absolute counter
         bsr makepath
         move.l #curpath,d1       ;pointer to a pattern path
         move.l doslib(a3),a6     ;DOS base address
         move.l #-2,d2         ;'read' mode
         jsr Lock(a6)          ;find file
         tst.l d0              ;found?
         bne .l1               ;no!
         rts

.l1:     move.l d0,tmplock(a3)     ;lock-save
         move.l d0,d1
         move.l #iobseg,d2   ;pointer to FilelnfoBlock
         jsr Examine(a6)     ;get disk name
         tst.l d0              ;OK?
         beq .close            ;no (rarely occurs)
         bra .setout           ;else set name

.loop:   ;* read filename
         move.l doslib(a3),a6     ;DOS base address
         move.l tmplock(a3),d1     ;key in D1
         move.l #iobseg,d2   ;pointer to FileInfoBlock
         jsr ExNext(a6)        ;find next file
         tst.l d0              ;found?
         beq .close         ;no: done

.setout: bsr fnextcheck
         tst.b d1
         bne .loop

         move.l a0,-(sp)
         lea.l svfn(a3),a4   ;check against a pattern in svfn
         lea.l iobseg+8,a5
         bsr parse500
         move.l (sp)+,a0
         tst.b d0
         beq .loop

         move.b #".",-5(a0)
         addq.l #1,d6
         cmp.l d6,d5
         bne .loop

         lea.l fn(a3),a1
         lea.l iobseg+8,a0
.copy:   move.b (a0)+,(a1)+
         bne .copy

.close:  move.l tmplock(a3),d1
         jmp UnLock(a6)

savepat: lea.l curpathsv(a3),a1
         bsr makepath\.sv
         lea.l svfn(a3),a0
         subq.l #1,a1
         move.b #'/',(a1)+
.loop:   move.b (a0)+,(a1)+
         bne .loop

         move.l #curpathsv,d1
         move.l #MODE_NEWFILE,d2
         move.l doslib(a3),a6
         jsr Open(a6)
         tst.l d0
         beq .error

         move.l d0,filehl(a3)
         move.l d0,d1
         move.l #x0,d2
         moveq #6,d3
         move.l live(a3),d0  ;msb -> lsb
         move.l d0,d7
         rol.w #8,d0
         swap d0
         rol.w #8,d0
         swap d0
         move.l d0,live(a3)
         jsr Write(a6)
         move.l d7,live(a3)

         lea.l tiles(a3),a4
         ;mov dx,[boxsz_xmin]  ;dl - xmin, dh - ymin
         move.b boxsz_xmin(a3),d3
         move.b boxsz_ymin(a3),d4
         ;xor cx,cx  ;cl - currow, ch - curcol
         clr.l d5
         clr.l d6
.loop0:  ;xor bx,bx
         clr.l d1
.loop2:  ;mov al,[si+bx]
         move.b (a4,d1),d0
         bne .cont1

.loop4:  ;inc bx
         addq.l #1,d1
         ;cmp bl,8
         cmpi.b #8,d1
         bne .loop2

         ;add si,tilesize
         lea.l tilesize(a4),a4
         ;inc ch
         addq.b #1,d6
         ;cmp ch,hormax
         cmpi.b #hormax,d6
         bne .loop0

         ;xor ch,ch
         clr.l d6
         ;inc cx
         addq.l #1,d5
         ;cmp cl,vermax
         cmpi.b #vermax,d5
         bne .loop0
         bra loadpat\.exit2

.error:  jsr IoErr(a6)
         tst.b errst(a3)
         bne.s .c1
         rts

.c1:     move.l d0,temp(a3)
         lea.l ioerrmsg(a3),a0
         lea.l temp(a3),a1
         lea.l stuffChar(pc),a2
         move.l #-1,charCount(a3)
         move.l a3,-(sp)
         lea.l stringbuf(a3),a3
         movea.l 4.w,a6
         jsr RawDoFmt(a6)
         move.l (sp)+,a3
         bsr totext
         move.l GRAPHICS_BASE(a3),a6
         movepenq 0,6
         color 1
         movea.l RASTER_PORT(a3),a1
         move.l charCount(a3),d0
         lea.l stringbuf(a3),a0
         jsr Text(a6)
         bra getkey

.cont1:  ;mov ah,0ffh
         move.b #$ff,d7
.loop3:  ;inc ah
         addq.b #1,d7
         ;shl al,1
         lsl.b d0
         bcs .cont4
         beq .loop4
         bra .loop3

.cont4:  movem.w d0/d1/d3/d4/d5/d6/d7,-(sp)
         ;mov al,ch
         move.b d6,d0
         ;shl al,1
         ;shl al,1
         ;shl al,1
         lsl.b #3,d0
         ;add al,ah
         add.b d7,d0
         ;sub al,dl
         sub.b d3,d0
         ;mov [x0],al
         move.b d0,x0(a3)
         ;mov al,cl
         move.b d5,d0
         ;shl al,1
         ;shl al,1
         ;shl al,1
         lsl.b #3,d0
         ;add al,bl
         add.b d1,d0
         ;sub al,dh
         sub.b d4,d0
         ;mov [y0],al
         move.b d0,y0(a3)
         ;push bx
         ;push cx
         ;push dx
         ;mov ah,40h
         ;mov bx,[filehl]
         ;mov cx,2
         ;mov dx,x0
         ;int 21h
         move.l filehl(a3),d1
         move.l #x0,d2   ;TODO: check MSB!
         moveq #2,d3
         jsr Write(a6)
         ;pop dx
         ;pop cx
         ;pop bx
         movem.w (sp)+,d0/d1/d3/d4/d5/d6/d7
         bra .loop3


showcomm:tst.b fn(a3)
         beq tograph

         bsr crsrclr
         bsr makepath
         bsr fn2path
         lea.l -4(a1),a1
         move.b #'T',(a1)+
         move.b #'X',(a1)+
         move.b #'T',(a1)
         move.l #curpath,d1       ;pointer to a pattern path
         movea.l doslib(a3),a6     ;DOS base address
         move.l #MODE_OLD,d2
         jsr Open(a6)
         tst.l d0
         beq .error

         move.l d0,filehl(a3)
         bsr totext
         movea.l GRAPHICS_BASE(a3),a6
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,6
         color 2
         moveq #0,d6   ;x-pos
         moveq #6,d7   ;y-pos

.loop:   move.l filehl(a3),d1
         move.l #stringbuf,d2
         moveq.l #1,d3
         move.l doslib(a3),a6
         jsr Read(a6)
         ;tst.l d0
         ;bmi .fin

         cmpi.l #1,d0
         bne .fin

         move.b stringbuf(a3),d0
         cmpi.b #$d,d0
         beq .loop

         cmpi.b #$a,d0
         bne .c1

.nl:     cmpi.w #25*8-2,d7
         bne .pr

         subq.w #8,d7
         movea.l BITPLANE1_PTR(a3),a0
         movea.l BITPLANE2_PTR(a3),a2
         move.w #nextline*48-1,d1
         lea.l nextline*8(a0),a1
         lea.l nextline*8(a2),a4
.l1:     move.l (a1)+,(a0)+
         move.l (a4)+,(a2)+
         dbra d1,.l1

         move.w #nextline*2-1,d1
.l2:     clr.l (a0)+
         clr.l (a2)+
         dbra d1,.l2

         move.l #20,d1      ;20/50 sec (PAL), 20/60 sec (NTSC)
         move.l doslib(a3),a6     ;DOS base address
         jsr Delay(a6)
         ;movem.l d6/d7,-(sp)
         bsr getkey2
         ;movem.l (sp)+,d6/d7
         tst.b d0
         beq.s .pr

         bsr getkey3
.pr:     clr.l d6
         addq.w #8,d7
         move.w d7,d1
         clr.w d0
         movea.l GRAPHICS_BASE(a3),a6
         movea.l RASTER_PORT(a3),a1
         jsr Move(a6)
         bra .loop

.c1:     cmpi.b #8,d0   ;tab
         bne .c2

         move.b #32,stringbuf(a3)
.c2:     lea.l stringbuf(a3),a0
         moveq #1,d0
         movea.l GRAPHICS_BASE(a3),a6
         movea.l RASTER_PORT(a3),a1
         jsr Text(a6)
         addq.w #8,d6
         cmpi.l #40*8,d6
         beq .nl
         bra .loop

.fin:    move.l filehl(a3),d1
         move.l doslib(a3),a6
         jsr Close(a6)
.exit:   bsr getkey3
         bra tograph

.error:  bsr totext
         movea.l GRAPHICS_BASE(a3),a6
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,6
         color 1
         print "no comments"
         bra .exit

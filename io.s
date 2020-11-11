   if 0
copyr:   call printstr
         db ansiclrscn,green,'$'

         mov ax,3d00h
         mov dx,copyleft
         int 21h
         jc readtent.e1

         mov bx,ax
.loop:   mov ah,3fh   ;read file
         mov cx,1
         mov dx,x0
         int 21h
         jc .exit

         mov dl,[x0]
         cmp dl,26
         jz .exit

         mov cx,2000
.delay:  mov ah,2
         loop .delay
         int 21h
         jmp .loop

.exit:   call getkey
         jmp setcolors.e1

setcolors:mov ax,3d00h
         mov dx,cf
         int 21h
         jc readtent.e1

         mov bx,ax
         mov ah,3fh   ;read file
         mov cx,7
         mov dx,palette
         int 21h
.e1:     jmp loadpat.e1

savecf:  mov ah,3ch   ;create a file
         mov dx,cf
         xor cx,cx
         int 21h
         jc readtent.e1

         mov bx,ax
         mov ah,40h   ;write
         mov cx,7
         mov dx,palette
         int 21h
         jmp loadpat.e1
  endif
readtent:move.l filehl(a3),d1
         move.l #iobseg,d2
         move.l #iobseg_end-iobseg,d3
         jsr Read(a6)
         lsr.l d0
         move.w d0,tsz(a3)
         sub.w d0,filesz(a3)
.e1:     rts

loadpat: bsr makepath
         bsr fn2path
         move.l #curpath,d1
         move.l #MODE_OLD,d2
         move.l doslib(a3),a6
         jsr Open(a6)
         tst.l d0
         beq readtent\.e1

         move.l d0,filehl(a3)
         move.l filesz(a3),d1
         lsr.l d1
         swap d1
         tst.w d1
         bne .exit2

         swap d1
         subq.l #3,d1
         bls .exit2

         move.l d1,filesz(a3)
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
         ;bcs .l1

         bsr fillrt
         bsr puttent
         tst filesz(a3)
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

.outpuff:tst.l iobseg+4
         bpl .loop          ;directory?

         movea.l #iobseg+8,a0
.l1:     tst.b (a0)+
         bne .l1

         move.l a0,d0
         sub.l #iobseg+8+1+4,d0
         bls .loop

         lea.l svfn(a3),a4   ;check against a pattern in svfn
         lea.l iobseg+8,a5
         move.w d0,-(sp)
         bsr parse
         move.l d0,d1
         move.w (sp)+,d0
         tst.b d1
         beq .loop

         cmpi.b #11+1,d0    ;we can show only 11 first chars of fn
         bcs .l2

         moveq #11,d0
.l2:     move.l GRAPHICS_BASE(a3),a6
         movea.l RASTER_PORT(a3),a1
         move.w d0,-(sp)
         clr.w d0
         btst #0,d5
         beq .l3

         move.w #20*8,d0
.l3:     move.w d5,d1
         lsr.w #1,d1
         addq.w #2,d1
         lsl.w #3,d1
         subq.w #2,d1
         jsr Move(a6)
         color 1    ;red
         move.w d6,d0
         lea.l nformat(a3),a0
         bsr printd0
         color 3    ;black
         move.w (sp)+,d0
         lea.l iobseg+8,a0
         movea.l RASTER_PORT(a3),a1  ;remove?
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
         beq showfree

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
         jsr    Examine(a6)    ;get disk name
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

.setout: tst.l iobseg+4
         bpl .loop          ;directory?

         lea.l svfn(a3),a4   ;check against a pattern in svfn
         lea.l iobseg+8,a5
         bsr parse
         tst.b d0
         beq .loop

         addq.l #1,d6
         cmp.l d6,d5
         bne .loop

         move.l iobseg+124,filesz(a3)
         lea.l fn(a3),a1
         lea.l iobseg+8,a0
.copy:   move.b (a0)+,(a1)+
         bne .copy

.close:  move.l tmplock(a3),d1
         jmp UnLock(a6)

    if 0
savepat: mov ah,3ch   ;create a file
         mov dx,svfn
         xor cx,cx
         int 21h
         jc .error

         mov bx,ax
         mov [filehl],ax
         mov ah,40h   ;write
         mov cx,6
         mov dx,x0
         int 21h

         mov si,tiles
         mov dx,[boxsz_xmin]  ;dl - xmin, dh - ymin
         xor cx,cx  ;cl - currow, ch - curcol
.loop0:  xor bx,bx
.loop2:  mov al,[si+bx]
         or al,al
         jnz .cont1

.loop4:  inc bx
         cmp bl,8
         jnz .loop2

         add si,tilesize
         inc ch
         cmp ch,hormax
         jnz .loop0

         xor ch,ch
         inc cx
         cmp cl,vermax
         jnz .loop0
         jmp loadpat.exit2

.error:  call printstr
         db 'can''t save$'
         jmp getkey

.cont1:  mov ah,0ffh
.loop3:  inc ah
         shl al,1
         jc .cont4
         jz .loop4
         jmp .loop3

.cont4:  push ax
         mov al,ch
         shl al,1
         shl al,1
         shl al,1
         add al,ah
         sub al,dl
         mov [x0],al
         mov al,cl
         shl al,1
         shl al,1
         shl al,1
         add al,bl
         sub al,dh
         mov [y0],al
         push bx
         push cx
         push dx
         mov ah,40h
         mov bx,[filehl]
         mov cx,2
         mov dx,x0
         int 21h
         pop dx
         pop cx
         pop bx
         pop ax
         jmp .loop3

;;ioerror: tstb @#errst           ;must be after iocf
;;         beq exit20

;;ioerr1:
;;         jsr r3,@#printstr
;;         .byte 12
;;         .asciz "IO ERROR"
;;         jmp @#getkey

showcomm:cmp byte [fn],0
         je .exit

         call totext
         mov si,fn
         mov di,svfn
         mov dx,di
.c1:     lodsb
         mov [di],al
         inc di
         cmp al,'.'
         jne .c1

         mov word [di],'T'+'X'*256
         mov word [di+2],'T'
         mov ax,3d00h
         int 21h
         jc .error

         mov bx,ax
         mov ax,3
         int 10h

.loop:   mov ah,3fh   ;read file
         mov cx,1
         mov dx,x0
         int 21h
         jc .fin

         or ax,ax
         jz .fin

         mov dl,[x0]
         mov ah,2
         int 21h
         jmp .loop

.fin:    mov ah,3eh    ;fclose
         int 21h
.exit:   call curoff
         call getkey
         jmp tograph

.error:  call printstr
         db 'no comments$'
         jmp .exit
   endif


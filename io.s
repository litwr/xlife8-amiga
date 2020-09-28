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

readtent:mov ah,3fh   ;read file
         mov bx,[filehl]
         mov cx,3072
         xor dx,dx
         push ds
         mov ds,[iobseg]
         int 21h
         pop ds
         shr ax,1
         mov [tsz],ax
         sub [filesz],ax
.e1:     retn

loadpat: mov ax,3d00h
         mov dx,fn
         int 21h
         jc readtent.e1

         mov [filehl],ax
         mov bx,ax
         mov ax,4202h   ;fseek
         xor cx,cx
         xor dx,dx
         int 21h
         shr dx,1
         rcr ax,1
         or dx,dx
         jnz .exit2

         sub ax,3
         jbe .exit2

         mov [filesz],ax
         mov ax,4200h
         int 21h
         mov si,[live]
         mov di,[born]
         mov ah,3fh   ;read file
         mov cx,6
         mov dx,x0
         int 21h
         mov al,byte [live+1]
         or al,byte [born+1]
         cmp al,2
         jnc .l1

         test byte [born],1
         jz .l2

.l1:     mov [live],si
         mov [born],di
         jmp .exit2

.l2:     push si
         push di
         call readtent   ;should adjust filesz
         call showrect
         pop di
         pop si
         jc .l1

         call fillrt
         call puttent
         cmp [filesz],0
         je .exit2

.l3:     mov ah,3fh
         mov bx,[filehl]
         mov cx,2
         mov dx,x0
         int 21h
         call putpixel
         dec [filesz]
         jnz .l3

.exit2:  mov bx,[filehl]
.e1:     mov ah,3eh    ;fclose
         int 21h
         retn
  endif
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

         lea.l -4(a0),a1
         cmp.b #'8',(a1)+
         bne .loop

         move.b (a1)+,d2
         and.b #$df,d2
         cmp.b #'X',d2
         bne .loop

         move.b (a1)+,d2
         and.b #$df,d2
         cmp.b #'L',d2
         bne .loop

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
         jsr Move(a6)
         lea.l lformat(a3),a0
         move.l (sp)+,d0
         bsr printd0l
         print 'K free'
         color 3  ;black

         move.l tmplock(a3),d1
         move.l doslib(a3),a6
         jmp UnLock(a6)

findfn:  ;fn# in D5
         clr.l d6   ;absolute counter
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
         beq showfree          ;no: done

.setout: addq.l #1,d6
         cmp.l d6,d5
         bne .loop

         lea.l fn(a3),a1
         lea.l iobseg+8,a0
.copy:   move.l (a0)+,(a1)+
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


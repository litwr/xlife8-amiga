  if 0
curon:  mov ah,1
        mov cx,607h
        int 10h
        retn

curoff: mov ah,1
        mov cx,201fh
        int 10h
        retn

curonz: mov ah,1
        mov cx,7
        int 10h
        retn
  endif
initxt: 
        moveq #1,d0   ;draw frame vertical borders
        move.b #$80,d1
        move.w #191,d2
        movea.l BITPLANE1_PTR(a3),a1
        movea.l BITPLANE2_PTR(a3),a2
.c1:    move.b d0,(3,a1)
        move.b d0,(3,a2)
        move.b d1,(35,a1)
        move.b d1,(35,a2)
        adda.l #40,a1
        adda.l #40,a2
        dbra d2,.c1

initxt2: move.l GRAPHICS_BASE(a3),a6    ;must follow initxt
         bsr showtopology
         movea.l RASTER_PORT(a3),a1
         move.w #18*8,d0
         move.w #198,d1
         jsr Move(a6)

         movea.l RASTER_PORT(a3),a1
         lea texts+1(a3),a0
         move.w #1,d0
         jsr Text(a6)

         movea.l RASTER_PORT(a3),a1
         move.w #32*8,d0
         move.w #198,d1
         jsr Move(a6)

         movea.l RASTER_PORT(a3),a1
         lea texts+2(a3),a0
         move.w #1,d0
         jsr Text(a6)

         movea.l RASTER_PORT(a3),a1
         move.w #36*8,d0
         move.w #198,d1
         jsr Move(a6)

         movea.l RASTER_PORT(a3),a1
         lea texts+3(a3),a0
         move.w #1,d0
         jmp Text(a6)

   if 0
totext: mov ax,1    ;set video mode #4 = 40x25x16
.e1:    int 10h
        jmp stop_timer2

tograph:cmp [zoom],0
        je .l1

        call totext
        call curonz
        call initxt2
        call setviewport
        call showmode2
        call showscn
        call showtopology2
        call showrules
        call xyout2
        jmp dispatcher.c270

.l1:    mov ax,4    ;set video mode #4 = 320x200x4
        int 10h

        mov bl,[palette]
        mov bh,1
        mov ah,0bh
        int 10h

        call initxt
        call showmode
        call showscn
        call start_timer2
        call showtopology
        call showrules
        jmp xyout

showmode2: cmp [mode],1
        mov al,[zbgr]
        jz .modego

        mov al,[zbgs]
.modego: mov [czbg],al
        retn

showmode: xor bx,bx    ;bg=0=black
        mov bl,[bgr]
        cmp [mode],1
        jz .modego

        mov bl,[bgs]
.modego: mov ah,0bh
        int 10h
        retn
   endif

showtopology:
         tst.b topology(a3)
         beq .l1

         movea.l RASTER_PORT(a3),a1
         moveq #4,d0
         jsr SetDrMd(a6)

.l1:     movea.l RASTER_PORT(a3),a1
         moveq #1,d0
         move.w #198,d1
         jsr Move(a6)

         movea.l RASTER_PORT(a3),a1
         lea texts(a3),a0
         move.w #1,d0
         jsr Text(a6)

         movea.l RASTER_PORT(a3),a1
         moveq #0,d0
         jmp SetDrMd(a6)

   if 0
showtopology2:
        mov al,9
        cmp [topology],0
        jz .l1

        mov al,69h
.l1:    mov [es:1921],al
        retn

printstr:pop dx         ;uses: si,dx,ax
         mov si,dx
.l1:     lodsb
         cmp al,'$'
         jnz .l1

         mov ah,9
         int 21h
         jmp si
  endif
digiout:	;in: d0 - length, a0 - scrpos, a1 - data
	 moveq #0,d1          ;use blitter?
	 lea digifont(a3),a2
.c0:     moveq #6,d2
         move.b -(a1),d1
         move.l d1,d3
         andi.b #$f,d1
         lsl.b #3,d1
         
.c1:     move.b (a2,d1),(a0)
	 adda.l #40,a0
	 addq #1,d1
	 dbra d2,.c1

	 subi #1,d0
	 bmi .ce

         suba.w #40*7+1,a0
	 moveq #6,d2
	 andi.b #$f0,d3
         lsr.b #1,d3
.c2:     move.b (a2,d3),(a0)
	 adda.l #40,a0
	 addq #1,d3
	 dbra d2,.c2
         suba.w #40*7+1,a0

	 dbra d0,.c0
.ce:	 rts
  if 0
digiout2:        ;in: cx - length, di - srcpos, si - data
         mov ah,5   ;color
.c1:     lodsb
         add al,'0'
         stosw
         loop .c1
         retn
  endif

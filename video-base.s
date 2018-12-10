TXT_PLACE_CURSOR:
	 invvideo
         print ' '
         normvideo
         rts

TXT_ON_CURSOR:        ;IN: d3,a6,a1
         move.w d0,-(sp)
         move.w d3,d0
         addi.w #184,d0
         moveq #8,d1
         jsr Move(a6)

         moveq #1,d0
         jsr SetDrMd(a6)
         move.w (sp)+,d0
         rts

TXT_REMOVE_CURSOR:  ;IN: d3,a6,a1
         move.w d3,d0
         addi.w #192,d0

         bsr TXT_REMOVE_CURSOR2\.e
         
         move.w d3,d0
         addi.w #184,d0
         moveq #8,d1
         jmp Move(a6)

TXT_REMOVE_CURSOR2:  ;IN: d3,a6,a1
         move.w d3,d0
         addi.w #184,d0
.e:      moveq #8,d1
         jsr Move(a6)

         moveq #1,d0
         jsr SetDrMd(a6)

         print ' '
         rts

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

initxt: moveq #1,d0     ;draw frame vertical borders
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

initxt2: bsr showtopology    ;must follow initxt
         ;movea.l RASTER_PORT(a3),a1
	 movepen 18*8,198
         ;movea.l RASTER_PORT(a3),a1
         lea texts+1(a3),a0  ;%
         moveq #1,d0
         jsr Text(a6)

         ;movea.l RASTER_PORT(a3),a1
         movepen 32*8,198
         ;movea.l RASTER_PORT(a3),a1
         lea texts+2(a3),a0  ;X
         moveq #1,d0
         jsr Text(a6)

         ;movea.l RASTER_PORT(a3),a1
         movepen 36*8,198
         ;movea.l RASTER_PORT(a3),a1
         lea texts+3(a3),a0  ;Y
         moveq #1,d0
         jmp Text(a6)

totext:
        bra clrscn

tograph:bsr clrscn
        bsr showmode
        bsr showscn
        bsr showrules
        bsr xyout
        bra initxt

showmode:moveq #12,d2
         tst.b mode(a3)
         bne .e1

         moveq #8,d2
.e1:     moveq #0,d3
         moveq #0,d0
         moveq #0,d1
         move.l GRAPHICS_BASE(a3),a6
         MOVE.L	VIEW_PORT(A3),A0
         jmp SetRGB4(a6)

showtopology:
         move.l GRAPHICS_BASE(a3),a6
	 movea.l RASTER_PORT(a3),a1
         color 3
         tst.b topology(a3)
         beq .l1
         
         invvideo
.l1:     ;movea.l RASTER_PORT(a3),a1
         moveq #0,d0
         move.w #198,d1
         jsr Move(a6)

         ;movea.l RASTER_PORT(a3),a1
         lea texts(a3),a0
         moveq #1,d0
         jsr Text(a6)

         ;movea.l RASTER_PORT(a3),a1
         moveq #0,d0
         jmp SetDrMd(a6)  ;normvideo

printstr:
         movea.l (sp),a2
         movea.l a2,a0
         moveq #0,d0
.l1:     addq.w #1,d0
         tst.b (a2)+
         bne .l1
         
         move.l a2,d1
         addq.l #1,d1
         andi.b #$fe,d1
         move.l d1,(sp)

         ;movea.l GRAPHICS_BASE(a3),a6
         ;movea.l RASTER_PORT(a3),a1
         subq.w #1,d0
         jmp Text(a6)

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


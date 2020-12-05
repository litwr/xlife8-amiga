TXT_PLACE_CURSOR:
         invvideo
         print ' '
         normvideo
         rts

TXT_ON_CURSOR:        ;IN: d3,a6,a1,d4,d1
         move.w d0,-(sp)
         move.w d3,d0
         add.w d4,d0
         movea.l RASTER_PORT(a3),a1
         jsr Move(a6)

         moveq #1,d0
         movea.l RASTER_PORT(a3),a1
         jsr SetDrMd(a6)
         move.w (sp)+,d0
         rts

TXT_REMOVE_CURSOR:  ;IN: d3,a6,a1,d1,d0
         add.w d3,d0
         movea.l RASTER_PORT(a3),a1
         movem.w d0/d1,-(sp)
         jsr Move(a6)

         moveq #1,d0
         movea.l RASTER_PORT(a3),a1
         jsr SetDrMd(a6)

         print ' '
         movem.w (sp)+,d0/d1

         subi.w #8,d0
         movea.l RASTER_PORT(a3),a1
         jmp Move(a6)

TXT_DRV_UPD:  ;CHANGE: d0,d1,a0,a1,a2,a6
         moveq #30,d1
         clr.w d0
         move.l GRAPHICS_BASE(a3),a6
         movea.l RASTER_PORT(a3),a1
         jsr Move(a6)
         moveq #1,d0
         movea.l RASTER_PORT(a3),a1
         jsr SetDrMd(a6)
         print '   '
         moveq #30,d1
         clr.w d0
         movea.l RASTER_PORT(a3),a1
         jsr Move(a6)
         moveq.l #4,d0
         lea.l curdisk(a3),a0
         movea.l RASTER_PORT(a3),a1
         jmp Text(a6)           ;print diskid

initxt: tst.b zoom(a3)
        bne initxt2

        moveq #1,d0     ;draw frame vertical borders
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
         movepen 18*8,198
         movea.l RASTER_PORT(a3),a1
         lea texts+1(a3),a0  ;%
         moveq #1,d0
         jsr Text(a6)

         movepen 32*8,198
         movea.l RASTER_PORT(a3),a1
         lea texts+2(a3),a0  ;X
         moveq #1,d0
         jsr Text(a6)

         movepen 36*8,198
         movea.l RASTER_PORT(a3),a1
         lea texts+3(a3),a0  ;Y
         moveq #1,d0
         jmp Text(a6)

totext:  bra clrscn

tograph:bsr clrscn
        bsr showmode
        bsr showscn
        bsr showrules
        bsr xyout
        bra initxt

showmode:move.w lightgreen(a3),d2
         tst.b mode(a3)
         bne.s .e1

         move.w COLORS(a3),d2
.e1:     move.w d2,d3
         lsr.w #4,d2
         move.w d2,d1
         lsr.w #4,d1
         moveq #0,d0
         moveq #15,d4
         and.l d4,d3   ;for the A1200 bug
         and.l d4,d2
         and.l d4,d1
         move.l GRAPHICS_BASE(a3),a6
         MOVE.L VIEW_PORT(a3),a0
         jmp SetRGB4(a6)

showtopology:
         move.l GRAPHICS_BASE(a3),a6
         movea.l RASTER_PORT(a3),a1
         color 3
         tst.b topology(a3)
         beq.s .l1

         invvideo
.l1:     movepen 0,198
         movea.l RASTER_PORT(a3),a1
         lea texts(a3),a0
         moveq #1,d0
         jsr Text(a6)
         movea.l RASTER_PORT(a3),a1
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
         movea.l RASTER_PORT(a3),a1
         subq.w #1,d0
         jmp Text(a6)

digiout:   ;in: d0 - length, a0 - scrpos, a1 - data
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

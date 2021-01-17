insteps: bsr totext
         move.l GRAPHICS_BASE(a3),a6 
         ;movea.l RASTER_PORT(a3),a1
.c38:    movepenq 0,6
         color 2
         print "NUMBER OF GENERATIONS: "   ;24 chars = 192 pixels
         color 3
         bsr TXT_PLACE_CURSOR
.c3:     lea stringbuf(a3),a4
         moveq #0,d3
         moveq #0,d6
.c1:     movem.l a1/a4/a6/d3/d6,-(sp)
         bsr getkey
         movem.l (sp)+,a1/a4/a6/d3/d6
         cmpi.b #$d,d0  ;enter
         beq .c11

         cmpi.b #27,d0   ;esc
         bne .c16
.c20:    rts

.c16:    cmpi.b #8,d0    ;backspace
         beq .c12

         cmpi.b #'0'+10,d0
         bcc .c1

         cmpi.b #'0',d0
         bcs .c1

         cmpi.b #5*8,d3
         beq .c1

         moveq #6,d1
         moveq #92,d4
         add.w d4,d4
         bsr TXT_ON_CURSOR
         move.l a4,a0
         move.b d0,(a4)+
         addq.w #8,d3
         moveq #1,d0
         jsr Text(a6)
.cont4:  bsr TXT_PLACE_CURSOR
         bra .c1

.c12:    subq.l #1,a4
         subq.b #8,d3
         bcs .c3

         move.w #192,d0
         moveq #6,d1
         bsr TXT_REMOVE_CURSOR
         bra .cont4

.c11:    move.w #184,d0
         moveq #6,d1
         bsr TXT_REMOVE_CURSOR
         tst.b d3
         beq .c20

          move.w d3,d1   ;convert to binary
          lsr #3,d1
          suba.w d1,a4
          moveq #0,d1
          subq.b #8,d3
          lsr #2,d3
.c33:     move.b (a4)+,d0
          sub.b #'0',d0
          beq .c34

          lea tobin(a3),a0
          move.w (a0,d3),d5
.c32:     add.w d5,d1
          bcc .c38a
          bra .c38       ;65535=max

.c38a:    subq.b #1,d0
          bne .c32

.c34:     sub.w #2,d3
          bpl .c33

         move.w d1,d6
         bra .c20

bornstay:
         lea stringbuf(a3),a4
         moveq #0,d3
.c1:     movem.l a1/a4/a6/d2/d3,-(sp)
         bsr getkey
         movem.l (sp)+,a1/a4/a6/d2/d3
         cmpi.b #$d,d0  ;enter
         beq.s .c11

         cmpi.b #8,d0    ;backspace
         beq .c12

         cmpi.b #'0',d2
         beq.s .c40

         cmpi.b #27,d0  ;esc
         bne.s .c40
.c11:    rts

.c40:    cmp.b d2,d0
         bcs .c1

         cmpi.b #'9',d0
         bcc .c1

         move.l a4,a0
         move.w d3,d1
.c4:     tst.w d1
         beq .c5

         cmp.b -(a0),d0
         beq .c1

         subq.w #8,d1
         bra .c4

.c5:     move d7,d1
         moveq #56,d4
         bsr TXT_ON_CURSOR
         move.l a4,a0
         move.b d0,(a4)+
         addq.w #8,d3
         moveq #1,d0
         jsr Text(a6)
.cont4:  bsr TXT_PLACE_CURSOR
         bra .c1

.c12:    subq.l #1,a4
         subq.b #8,d3
         bmi bornstay

         move d7,d1
         moveq #64,d0
         bsr TXT_REMOVE_CURSOR
         bra .cont4

inborn:  bsr totext
         move.l GRAPHICS_BASE(a3),a6 
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,6
         color 2  ;green
         print 'THE RULES ARE DEFINED BY '
         invvideo ;blue
         print 'BORN'
         normvideo
         color 2
         print ' AND '
         invvideo ;blue
         print 'STAY'
         normvideo
         color 2
         print ' V'
         movepenq 0,14
         print 'ALUES.  FOR EXAMPLE, '
         color 1
         invvideo ;purple
         print "CONWAYS'S LIFE"
         normvideo
         color 2
         print ' HAS'
         movepenq 0,22
         print 'BORN=3 AND STAY=23, '
         color  1 ;purple
         invvideo
         print 'SEEDS'
         normvideo
         color 2
         print ' - BORN=2 AND E'
         movepenq 0,30
         print 'MPTY STAY, '
         color 1
         invvideo ;purple
         print 'HIGHLIFE'
         normvideo
         color 2
         print ' - BORN=36 AND STAY=2'
         movepenq 0,38
         print '3, '
         color 1
         invvideo ;purple
         print 'LIFE WITHOUT DEATH'
         normvideo
         color 2
         print ' - BORN=3 AND STAY='
         movepenq 0,46
         print '012345678, ...'
         movepenq 0,54
         color 3
         print 'BORN = '
         bsr TXT_PLACE_CURSOR

         moveq #54,d7
         moveq #'1',d2
         bsr bornstay
         move.w d0,-(sp)
         move d7,d1
         moveq #56,d0
         bsr TXT_REMOVE_CURSOR
         move.w (sp)+,d0
         rts

instay:  movepenq 0,62
         print 'STAY = '
         bsr TXT_PLACE_CURSOR

         moveq #62,d7
         moveq #'0',d2
         bra bornstay

indens:  bsr totext
         move.l GRAPHICS_BASE(a3),a6 
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,6
         color 3
         print "SELECT DENSITY OR PRESS "
         color 1
         print "ESC"
         color 3
         print " TO EXIT"
         movepenq 36,14
         color 1
         print '0'
         color 2
         print ' - 12.5%'
         movepenq 36,22
         color 1
         print '1'
         color 2
         print ' - 28%'
         movepenq 36,30
         color 1
         print '2'
         color 2
         print ' - 42%'
         movepenq 36,38
         color 1
         print '3'
         color 2
         print ' - 54%'
         movepenq 36,46
         color 1
         print '4'
         color 2
         print ' - 64%'
         movepenq 36,54
         color 1
         print '5'
         color 2
         print ' - 73%'
         movepenq 36,62
         color 1
         print '6'
         color 2
         print ' - 81%'
         movepenq 36,70
         color 1
         print '7'
         color 2
         print ' - 88.5%'
         movepenq 36,78
         color 1
         print '8'
         color 2
         print ' - 95%'
         movepenq 36,86
         color 1
         print '9'
         color 2
         print ' - 100%'
.c1:     bsr getkey
         cmpi.b #27,d0   ;ESC
         beq .c2

         cmpi.b #'0',d0
         bcs .c1

         cmpi.b #'0'+10,d0
         bcc .c1

         subi #'0'-1,d0
         move.b d0,density(a3)
.c2:     bra tograph

inmode:  ;move.l GRAPHICS_BASE(a3),a6 
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,22
         color 2
         print 'SELECT BENCHMARK MODE'
         movepenq 8,30
         color 1
         print '0'
         color 2
         print ' - CALCULATIONS'
         movepenq 8,38
         color 1
         print '1'
         color 2
         print ' - VIDEO'
         movepenq 8,46
         color 1
         print '2'
         color 2
         print ' - BOTH'
.c1:     bsr getkey
         cmpi.b #'0',d0
         bcs .c1

         cmpi.b #'3',d0
         bcc .c1

         subi.b #'1',d0
         rts

help:    bsr totext
         move.l GRAPHICS_BASE(a3),a6 
         ;movea.l RASTER_PORT(a3),a1
         normvideo
         movepenq 72,6
         color 3
         print '*** XLIFE COMMANDS ***'
         movepenq 0,14
         color 1
         print '!'
         color 2
         print ' randomize screen'
         movepenq 0,22
         color 1
         print '%'
         color 2
         print ' set random density - default=42%'
         movepenq 0,30
         color 1
         print '+'
         color 2
         print '/'
	 color 1
         print '-'
         color 2
         print ' zoom in/out'
         movepenq 0,38
	 color 1
         print '.'
         color 2
         print '/'
	 color 1
         print 'H'
         color 2
         print ' center/home the cursor'
         movepenq 0,46
	 color 1
         print '?'
         color 2
         print ' show this help'
         movepenq 0,54
	 color 1
         print 'B'
         color 2
         print ' benchmark'
         movepenq 0,62
	 color 1
         print 'C'
         color 2
         print ' clear the screen'
         movepenq 0,70
	 color 1
         print 'E'
         color 2
         print ' toggle the pseudocolor mode'
         movepenq 0,78
	 color 1
         print 'g'
         color 2
         print ' toggle the run/stop mode'
         movepenq 0,86
	 color 1
         print 'h'
         color 2
         print ' toggle the hiding (fastest) mode'
         movepenq 0,94
	 color 1
         print 'l'
         color 2
         print ' load and transform a pattern'
         movepenq 0,102
	 color 1
         print 'L'
         color 2
         print ' reload a pattern'
         movepenq 0,110
	 color 1
         print 'o'
         color 2
         print ' one step'
         movepenq 0,118
	 color 1
         print 'Q'
         color 2
         print ' quit'
         movepenq 0,126
	 color 1
         print 'R'
         color 2
         print ' set the rules'
         movepen 0,134
	 color 1
         print 'S'
         color 2
         print ' save'
         movepen 0,142
	 color 1
         print 'T'
         color 2
         print ' toggle plain/torus topology'
         movepen 0,150
	 color 1
         print 'v'
         color 2
         print ' show some info'
         movepen 0,158
	 color 1
         print 'V'
         color 2
         print ' show comments to a pattern'
         movepen 0,166
	 color 1
         print 'X'
         color 2
         print '/'
	 color 1
         print 'Z'
         color 2
         print ' reload/set&save a palette'
         movepen 0,182
	 invvideo
         print 'Use '
         ;normvideo
         color 1
         print 'cursor keys'
	 color 2
         invvideo
         print ' to set a position and a '
         ;normvideo
         movepen 0,190
	 color 1
         print 'space key'
         color 2
         invvideo
         print ' to toggle the current cell. Us'
         movepen 0,198
         print 'e a '
         ;normvideo
         color 1
         print 'shift'
         color 2
         invvideo
         print ' to speed up the movement'
         normvideo
         bsr getkey3
         jmp tograph

xyout:   tst.b zoom(a3)
         ;bne xyout2

         lea ycrsr+2(a3),a1
         moveq #2,d0
         movea.l BITPLANE2_PTR(a3),a0
         adda.l #192*40+39,a0
         bsr digiout

         lea xcrsr+2(a3),a1
         moveq #2,d0
         movea.l BITPLANE2_PTR(a3),a0
         adda.l #192*40+35,a0
         bra digiout

infoout: ;must be before showtinfo
         ;;cmp [zoom],0
	 tst.b zoom(a3)
         ;;jnz infoout2
	 ;**bne infoout2

         ;;mov bx,gencnt
	 lea gencnt+4(a3),a1
         ;;mov dx,7
	 moveq #6,d0

;;         mov #<statusline*64+16384+2>,r2
         ;;mov di,192*40+2
         movea.l BITPLANE2_PTR(a3),a0
         adda.l #192*40+1+6,a0
         ;;call digiout
	 bsr digiout
         ;;mov bx,cellcnt
	 lea cellcnt+3(a3),a1
         ;;mov dx,5
	 moveq #4,d0

;;         mov #<statusline*64+16384+18>,r2
         ;;mov di,192*40+18
         movea.l BITPLANE2_PTR(a3),a0
         adda.l #192*40+9+4,a0
         ;;call digiout
	 bsr digiout

showtinfo:
         ;;mov bx,tinfo
         lea tinfo(a3),a0
         ;;mov si,[tilecnt]
         move.w tilecnt(a3),d0
         ;;shr si,1
         ;;shr si,1
         lsr #2,d0
         ;;cmp si,hormax*vermax/4
         cmpi.w #hormax*vermax/4,d0
         ;;jnz .c1
         bne .c1

         ;;mov word [bx],1
         move.b #1,(a0)+
         ;;mov byte [bx+2],0
         move.b #0,(a0)
         ;;jmp .c2
         bra .c3

.c1:     ;;mov word [bx],0a0ah
         move.b #$a,(a0)+
         ;;mov al,[si+ttab]
         lea ttab(a3),a2
         move.b (a2,d0),d0
         ;;mov ah,al
         move.b d0,d1
         andi.b #$f0,d1
         bne .c2

         andi.b #$f,d0
         ori.b #$a0,d0
.c2:     move.b d0,(a0)
.c3:     lea tinfo+2(a3),a1
         ;;mov dx,3
         moveq #2,d0
         ;;mov di,192*40+30
         movea.l BITPLANE2_PTR(a3),a0
         adda.l #192*40+14+3,a0
         bra digiout

calcx:   move.b crsrbit(a3),d2   ;$80 -> 0, $40 -> 1, ...
         moveq #-1,d0
.c1:     addq.b #1,d0
         add.b d2,d2
         bcc.s .c1
.e0:     rts

crsrpg:  move.b d4,i1(a3)   ;d4 = 0
         movea.l a5,a0
         suba.l BITPLANE1_PTR(a3),a0
         adda.l BITPLANE3_PTR(a3),a0
         move.b #$ff,d7
         move.b d7,6*nextline-1(a0)
         move.b d7,-nextline-1(a0)
         move.b d7,-1(a0)
         move.b d7,nextline-1(a0)
         move.b d7,2*nextline-1(a0)
         move.b d7,3*nextline-1(a0)
         move.b d7,4*nextline-1(a0)
         move.b d7,5*nextline-1(a0)
         rts

crsrcalc:
         ;;mov bx,[crsrtile]
         movea.l crsrtile(a3),a1
         ;;mov bx,[bx+video]
         move.l (video,a1),d1
         subq.w #(40-hormax)/2,d1
         divu #40,d1    ;nextline
         move.b d1,crsry(a3)
         move.w d1,d0
         add.b crsrbyte(a3),d0
         move.w #0,d1
         swap d1
         lsl.w #3,d1
         move.b d1,crsrx(a3)
         move.b crsrbit(a3),d2
.c10:    add.b d2,d2
         bcs.s .c8

         addq #1,d1
         bra.s .c10

.c8:     divu #100,d1
         move.b d1,xcrsr(a3)
         move.w #0,d1
         swap d1
         divu #10,d1
         move.w d1,d2
         lsl #4,d2
         swap d1
         add.b d2,d1
         move.b d1,xcrsr+1(a3)

         divu #100,d0
         move.b d0,ycrsr(a3)
         move.w #0,d0
         swap d0
         divu #10,d0
         move.w d0,d2
         lsl #4,d2
         swap d0
         or.b d2,d0
         move.b d0,ycrsr+1(a3)

         bsr xyout
         tst.b zoom(a3)
         bne.s .c18
         rts

.c18:   ;mov di,up
        movea.l #up,a5
        ;mov al,[vptilecy]
        move.b vptilecy(a3),d0
        ;mov ah,al
        move.b d0,d7
        ;add al,8
        addq.b #8,d0
        ;or ah,ah
        tst.b d7
        bmi.s .c33

        ;mov di,down
        movea.w #down,a5
        ;sub al,16
        subi.b #16,d0
        ;cmp ah,24
        cmpi.b #24,d7
        bcs.s .c34

.c33:   ;mov [vptilecy],al
        move.b d0,vptilecy(a3)
        bra.s .c31

.c34:   ;mov di,left
        movea.w #left,a5
        ;mov al,[vptilecx]
        move.b vptilecx(a3),d0
        ;mov ah,al
        move.b d0,d7
        ;add al,8
        addq.b #8,d0
        ;or ah,ah
        tst.b d7
        bmi.s .c35

        ;mov di,right
        movea.w #right,a5
        ;sub al,16
        subi.b #16,d0
        ;cmp ah,40
        cmpi.b #40,d7
        bcs.s showscnz

.c35:   ;mov [vptilecx],al
        move.b d0,vptilecx(a3)
.c31:   ;add di,[viewport]
        adda.l viewport(a3),a5
        ;mov bx,[di]
        movea.l (a5),a1
        ;mov [viewport],bx
        move.l a1,viewport(a3)
        ;mov di,[bx+dr]
        movea.l dr(a1),a5
        ;mov di,[di+dr]
        movea.l dr(a5),a5
        ;mov di,[di+right]
        movea.l right(a5),a5
        ;mov di,[di+right]
        movea.l right(a5),a5
        ;add bx,44*tilesize
        adda.l #(2*hormax+4)*tilesize,a1
        ;;cmp bx,di
        cmpa.l a5,a1
        beq.s showscnz

        bsr setviewport

showscnz:movea.l BITPLANE1_PTR(a3),a5    ;must be after crsrcalc
         lea.l nextline(a5),a5
         movea.l viewport(a3),a4
         moveq #0,d4
         move.b d4,i1(a3)
         moveq #7,d1
         sub.b crsrbyte(a3),d1
         move.b d1,i1+1(a3)
         bsr calcx
         moveq #7,d1
         sub.b d0,d1
         move.b d1,temp(a3)
         moveq #5,d1
         tst.b pseudoc(a3)
         bne.w showscnzp

.loop3:  moveq #3,d2
.loop4:  moveq #7,d5
         cmpa.l crsrtile(a3),a4
         bne.s .loop2

         addq.b #1,i1(a3)
.loop2:  move.b (a4)+,d0
         moveq #7,d3
.loop1:  lsl.b d0
         bcs.s .cont2

         ;output an empty space
         cmp.b (a5)+,d4
         beq.s .zero

         move.b d4,nextline-1(a5)
         move.b d4,2*nextline-1(a5)
         move.b d4,3*nextline-1(a5)
         move.b d4,4*nextline-1(a5)
         move.b d4,5*nextline-1(a5)
         move.b d4,-1(a5)
.zero:   setcursor .cont7
.cont7:  dbra d3,.loop1
         bra.s .c1

.cont2:  ;output a cell
         move.b #$7e,nextline(a5)
         move.b #$7e,2*nextline(a5)
         move.b #$7e,3*nextline(a5)
         move.b #$7e,4*nextline(a5)
         move.b #$3c,5*nextline(a5)
         move.b #$3c,(a5)+
         setcursor .cont8
.cont8:  dbra d3,.loop1

.c1:     lea.l 8*nextline-8(a5),a5
         dbra d5,.loop2

         subq.b #1,d2
         beq.s .cont3

         lea.l tilesize*hormax-8(a4),a4
         bra.w .loop4

.cont3:  subq.b #1,d1    ;xlimit
         beq.w calcx\.e0     ;rts

         suba.l #192*nextline-8,a5
         suba.l #tilesize*(hormax*2-1)+8,a4
         bra.w .loop3

showscnzp:
.loop3:  moveq #3,d2
.loop4:  moveq #7,d5
         ;lea bp,[si+count0]
         lea.l count0(a4),a6
         cmpa.l crsrtile(a3),a4
         bne .loop2

         addq.b #1,i1(a3)
.loop2:  ;mov ax,[ds:bp]
         move.b (a6)+,d0
         move.b (a6)+,d3
         ;and ax,18c0h
         andi.b #$c0,d0
         andi.b #$18,d3
         ;mov ch,al
         move.b d0,d6
         ;shl ah,1
         lsl.b d3
         ;or ch,ah
         or.b d3,d6
         ;mov ax,[ds:bp+2]
         move.b (a6)+,d0
         move.b (a6)+,d3
         ;and ax,318h
         andi.b #$18,d0
         andi.b #3,d3
         ;shr al,1
         lsr.b d0
         ;or ch,al
         or.b d0,d6
         ;or ch,ah
         or.b d3,d6
         ;lodsb
         ;add bp,4
         ;mov bl,8
         move.b (a4)+,d0
         moveq #7,d3
.loop1:  lsl.b d0
         bcc.w .space

         lsl.b d6
         bcc.s .new

         move.b #$7e,nextline(a5)
         move.b #$7e,2*nextline(a5)
         move.b #$7e,3*nextline(a5)
         move.b #$7e,4*nextline(a5)
         move.b #$3c,5*nextline(a5)
         move.b #$3c,(a5)+
         setcursor .cont7
.cont7:  dbra d3,.loop1
         bra.s .c1

.new:    move.b #$7e,nextline(a5)
         move.b #$66,2*nextline(a5)
         move.b #$66,3*nextline(a5)
         move.b #$7e,4*nextline(a5)
         move.b #$3c,5*nextline(a5)
         move.b #$3c,(a5)+
         setcursor .cont8
.cont8:  dbra d3,.loop1
         bra.s .c1

.space:  lsl.b d6
         cmp.b (a5)+,d4
         beq.s .zero

         move.b d4,nextline-1(a5)
         move.b d4,2*nextline-1(a5)
         move.b d4,3*nextline-1(a5)
         move.b d4,4*nextline-1(a5)
         move.b d4,5*nextline-1(a5)
         move.b d4,-1(a5)
.zero:   setcursor .cont6
.cont6:  dbra d3,.loop1

.c1:     lea.l 8*nextline-8(a5),a5
         dbra d5,.loop2

         subq.b #1,d2
         beq.s .cont3

         lea.l tilesize*hormax-8(a4),a4
         bra.w .loop4

.cont3:  subq.b #1,d1    ;xlimit
         beq.w calcx\.e0     ;rts

         suba.l #192*nextline-8,a5
         suba.l #tilesize*(hormax*2-1)+8,a4
         bra.w .loop3

showscn: bsr infoout
         tst.b zoom(a3)
         bne showscnz

         tst.w tilecnt(a3)
         beq crsrset

         tst.b pseudoc(a3)
         bne showscnp

showscn2: movea.l startp(a3),a4
.l1:      movea.l (video,a4),a5
          adda.l BITPLANE1_PTR(A3),A5
          move.l (a4),d0
          move.b d0,(nextline*3,a5)
          swap d0
          move.b d0,(nextline,a5)
          lsr.l #8,d0
          move.b d0,(a5)
          swap d0
          move.b d0,(nextline*2,a5)
          move.l (4,a4),d0
          move.b d0,(nextline*7,a5)
          swap d0
          move.b d0,(nextline*5,a5)
          lsr.l #8,d0
          move.b d0,(nextline*4,a5)
          swap d0
          move.b d0,(nextline*6,a5)
          movea.l (next,a4),a4
          cmpa.w #1,a4
          bne .l1
          bra crsrset

showscnp: movea.l startp(a3),a4
.l1:      movea.l (video,a4),a0
          movea.l a0,a1
          adda.l BITPLANE1_PTR(a3),a0
          adda.l BITPLANE2_PTR(a3),a1
          move.l (a4),d0
          move.l (count3,a4),d1
          vidmacp
          move.b d0,(nextline*3,a1)
          move.b d1,(nextline*3,a0)
          lsr.w #8,d0
          move.l (count2,a4),d1
          vidmacp
          move.b d0,(nextline*2,a1)
          move.b d1,(nextline*2,a0)
          swap d0
          move.l (count1,a4),d1
          vidmacp
          move.b d0,(nextline,a1)
          move.b d1,(nextline,a0)
          lsr.w #8,d0
          move.l (count0,a4),d1
          vidmacp
          move.b d0,(a1)
          move.b d1,(a0)
          move.l (4,a4),d0
          move.l (count7,a4),d1
          vidmacp
          move.b d0,(nextline*7,a1)
          move.b d1,(nextline*7,a0)
          lsr.w #8,d0
          move.l (count6,a4),d1
          vidmacp
          move.b d0,(nextline*6,a1)
          move.b d1,(nextline*6,a0)
          swap d0
          move.l (count5,a4),d1
          vidmacp
          move.b d0,(nextline*5,a1)
          move.b d1,(nextline*5,a0)
          lsr.w #8,d0
          move.l (count4,a4),d1
          vidmacp
          move.b d0,(nextline*4,a1)
          move.b d1,(nextline*4,a0)
          movea.l (next,a4),a4
          cmpa.w #1,a4
          bne .l1
          bra crsrset

chgdrv:  moveq #nudrives+1,d5
.loop:   subq.l #1,d5
         beq .exit

         clr.l d0
         move.b drvidx(a3),d0
         addq.b #4,d0
         cmpi.b #nudrives*4,d0
         bne .cont

         clr.b d0
.cont:   move.b d0,drvidx(a3)
         lea.l drives(a3),a4
         move.l (a4,d0),curdisk(a3)
         bsr makepath
         move.l TASK_PTR(A3),a4
         move.l $b8(a4),saveWPTR(a3)   ;pr_Windowptr
         move.l #-1,$b8(a4)
         move.l #curpath,d1
         move.l doslib(a3),a6 
         move.l #-2,d2         ;'read' mode
         jsr Lock(a6)          ;find file
         move.l d0,d1
         beq .loop

         jsr UnLock(a6)
         move.l saveWPTR(a3),$b8(a4)
.exit:   rts

loadmenu:bsr totext
         move.l GRAPHICS_BASE(a3),a6
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,6
         color 2  ;green
         print 'INPUT FILENAME, AN EMPTY STRING MEANS TO'
         movepenq 0,14
         print ' SHOW DIRECTORY. PRESS '
         color 1 ;red
         print 'TAB'
         color 2
         print ' TO USE RAMDIS'
         movepenq 0,22
         print 'K, '
         color 1
         print '*'
         color 2
         print ' TO CHANGE DRIVE, '
         color 1
         print 'ESC'
         color 2
         print ' TO EXIT'
         color 3
         movepenq 0,30
.c80:    moveq.l #4,d0
         lea.l curdisk(a3),a0
         movea.l RASTER_PORT(a3),a1
         jsr Text(a6)           ;print diskid
         bsr TXT_PLACE_CURSOR
.c3:     lea stringbuf(a3),a4
         moveq #0,d3   ;length
.c1:     movem.l a1/a4/a6/d3,-(sp)
.c1d:    bsr getkey
         movem.l (sp)+,a1/a4/a6/d3

         cmpi.b #$d,d0  ;enter
         beq .c11

         cmpi.b #8,d0    ;backspace
         beq .c12

         cmpi.b #27,d0  ;esc
         bne .c17

.c100:   move.b #-1,d0
         rts

.c101:   move.b #1,d0
         rts

.c17:    cmpi.b #'*',d0
         bne .c21

         movem.l a1/a4/a6/d3,-(sp)
         bsr chgdrv
         bsr TXT_DRV_UPD
         bra .c1d

.c21:    cmpi.b #9,d0  ;TAB
         bne .c18

         bsr ramdisk
         bra .c100

.c18:    cmpi.b #'!',d0
         bcs .c1

         cmpi.b #126,d0
         bcc .c1

         lea.l nofnchar(a3),a0
.c5:     cmp.b (a0)+,d0
         beq .c1

         cmpa.l #stringbuf,a0
         bne .c5

         cmpi.b #'a',d0
         bcs .c6

         cmpi.b #'z'+1,d0
         bcc .c6

         subi.b #'a'-'A',d0
.c6:     cmpi.b #FNMAXLEN*8,d3
         bcc .c1

         move.l a4,a0
         move.b d0,(a4)+
         addq.b #8,d3
         moveq #30,d1  ;vertical pos
         moveq #24,d4  ;hor pos - it is defined by length of a drive name
         move.l a0,-(sp)
         bsr TXT_ON_CURSOR
         move.l (sp)+,a0
         moveq #1,d0
         jsr Text(a6)
.cont4:  bsr TXT_PLACE_CURSOR
         bra .c1

.c12:    subq.l #1,a4
         subq.b #8,d3
         bcs .c3

         moveq #30,d1
         moveq #40,d0
         bsr TXT_REMOVE_CURSOR
         bra .cont4

.c11:    tst.b d3
         beq menu2

         move.b #".",(a4)+
         move.b #"8",(a4)+
         move.b #"x",(a4)+
         move.b #"l",(a4)+
         clr.b (a4)
         lea.l fn(a3),a1
         lea.l stringbuf(a3),a0
.copy:   move.b (a0)+,(a1)+
         bne .copy
         bra .c101

menu2:   bsr setdirmsk
         cmpi.b #27,d0     ;esc
         beq .c100

         bsr showdir  ;returns number of directory entries in D6
         move.l GRAPHICS_BASE(a3),a6
         movepenq 0,6
         color 2
         print 'ENTER FILE# OR '
         color 1
         print 'ESC'
         color 2
         print ': '
         color 3
         bsr TXT_PLACE_CURSOR

.c3:     lea.l stringbuf+1(a3),a4
         move.l a4,a5
         moveq #0,d3
.c1:     movem.l a4/a5/a6/d3/d6,-(sp)
         bsr getkey
         movem.l (sp)+,a4/a5/a6/d3/d6
         cmpi.b #27,d0     ;esc
         bne .c17

.c100:   ;call curon
         bra loadmenu

.c17:    cmpi.b #$d,d0
         beq.s .c11

         cmpi.b #8,d0   ;backspace
         beq.s .c12

         cmpi.b #'0',d0
         bcs.s .c1

         cmpi.b #'9'+1,d0
         bcc.s .c1

         cmpi.b #3*8,d3
         beq.s .c1

         addq.b #8,d3
         moveq #6,d1
         move.w #152,d4
         bsr TXT_ON_CURSOR
         move.l a4,a0
         move.b d0,(a4)+
         moveq #1,d0
         jsr Text(a6)
.cont4:  bsr TXT_PLACE_CURSOR
         bra.s .c1

.c11:    moveq #0,d0  ;result
         move.b d3,d4
         tst.b d3
         beq.s .c3

         move.l a5,-(sp)
         bra.s .l4

.l2:     mulu #10,d0
.l4:     clr.w d1
         move.b (a5)+,d1
         subi.b #'0',d1
         add.w d1,d0
         subq #8,d4
         bne.s .l2

         move.l (sp)+,a5
.c21:    cmp d6,d0
         bcc.s .c1

.l3:     bsr findfn
         or.l d6,d6     ;sets SF
         rts

.c12:    subq.l #1,a4
         subq.b #8,d3
         bcs .c3

         moveq #6,d1
         move.w #168,d0
         bsr TXT_REMOVE_CURSOR
         bra.s .cont4

getsvfn: bsr totext
         move.l GRAPHICS_BASE(a3),a6
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,6
         color 2  ;green
         print 'Enter filename ('
         color 1  ;red,
         print 'ESC'
         color 2
         print ' - exit, '
         color 1
         print '*'
         color 2
         print ' - drive)'
         color 3  ;black
         movepenq 0,30
.c80:    moveq.l #4,d0
         lea.l curdisk(a3),a0
         movea.l RASTER_PORT(a3),a1
         jsr Text(a6)           ;print diskid
         bsr TXT_PLACE_CURSOR
.c3:     lea.l svfn(a3),a4
         moveq #0,d3   ;length
.c1:     movem.l a1/a4/a6/d3,-(sp)
.c1d:    bsr getkey
         movem.l (sp)+,a1/a4/a6/d3
         cmpi.b #$d,d0
         beq .c11

         cmpi.b #8,d0  ;backspace
         beq .c12

         cmpi.b #27,d0   ;esc
         bne .c17
.c100:   rts

.c101:   move.b #1,d0
         rts

.c17:    cmpi.b #'*',d0
         bne .c18

         movem.l a1/a4/a6/d3,-(sp)
         bsr chgdrv
         bsr TXT_DRV_UPD
         bra .c1d

.c18:    cmpi.b #'!',d0
         bcs .c1

         cmpi.b #126,d0
         bcc .c1

         lea.l nofnchar(a3),a0
.c5:     cmp.b (a0)+,d0
         beq .c1

         cmpa.l #stringbuf,a0
         bne .c5

         cmpi.b #'a',d0
         bcs .c6

         cmpi.b #'z'+1,d0
         bcc .c6

         subi.b #'a'-'A',d0
.c6:     cmpi.b #FNMAXLEN*8,d3
         bcc .c1

         move.l a4,a0
         move.b d0,(a4)+
         addq.b #8,d3
         moveq #30,d1  ;vertical pos
         moveq #24,d4  ;hor pos - it is defined by length of a drive name
         move.l a0,-(sp)
         bsr TXT_ON_CURSOR
         move.l (sp)+,a0
         moveq #1,d0
         jsr Text(a6)
.cont4:  bsr TXT_PLACE_CURSOR
         bra .c1

.c11:    tst.b d3
         beq menu2

         move.b #".",(a4)+
         move.b #"8",(a4)+
         move.b #"x",(a4)+
         move.b #"l",(a4)+
         clr.b (a4)
         bra .c101

.c12:    subq.l #1,a4
         subq.b #8,d3
         bcs .c3

         moveq #30,d1
         moveq #40,d0
         bsr TXT_REMOVE_CURSOR
         bra .cont4

showrect:
         bsr clrrow25
         move.l GRAPHICS_BASE(a3),a6 
         ;movea.l RASTER_PORT(a3),a1
         movepen 0,198
         color 1
         print "MOVE, "
         invvideo ;purple
         print "R"
         normvideo
         print "OTATE, "
         invvideo ;purple
         print "F"
         normvideo
         print "LIP, "
         invvideo ;purple
         print "ENTER"
         normvideo
         print ", "
         invvideo ;purple
         print "ESC"
         normvideo
         color 3
         print "  X   Y"
         bsr xyout
         clr.w xdir(a3)
         clr.b xchgdir(a3)
.c10:    bsr drawrect
         bsr showtent
.c11:    bsr crsrflash
         bsr getkey2
         cmpi.b #$9b,d0   ;extended keys
         beq .c101

         cmpi.b #'.',d0     ;to center
         beq .c100

         cmpi.b #'H',d0     ;to home
         beq .c100

         cmpi.b #mouseright_char,d0
         bne.s .c7

         move.l #10,d1      ;10/50 sec (PAL), 10/60 sec (NTSC)
         move.l doslib(a3),a6     ;DOS base address
         jsr Delay(a6)
         bra.s .rotate

.c7:     cmpi.b #'r',d0
         bne.s .c1

.rotate: bsr clrrect
         not.b xchgdir(a3)
         move.w xdir(a3),d0
         rol.w #8,d0
         not.b d0
         move.w d0,xdir(a3)
         bra.s .c10

.c1:     cmpi.b #'f',d0
         bne.s .c2

         bsr clrrect
         not.b xdir(a3)
         bra.s .c10

.c2:     cmpi.b #$d,d0
         beq .exit

         cmpi.b #27,d0      ;esc
         bne .c22

.exit:   move.w d0,-(sp)
         bsr clrrect
         move.w (sp)+,d0
         rts

.c22:    cmpi.b #mouseleft_char,d0
         bne.s .c11

         bsr crsrclr
         movea.l SCREEN_HANDLE(a3),a0
         move.w 16(a0),d0   ;screen.y
         move.w 18(a0),d2   ;screen.x
         subq.w #2,d0
         bcs .c10

         cmpi.w #192,d0
         bcc .c10

         subq.w #2,d2
         bcs .c10

         cmpi.w #280,d2
         bcc .c10

         subi.w #32,d2
         bcs .c10

.l12:    movem.w d0/d2,-(sp)
         bsr clrrect
         movem.w (sp)+,d0/d2
         bsr mousecursor
         bsr crsrset
         bsr crsrcalc
         bra .c10

.c100:   move.w d0,-(sp)
         bsr clrrect
         move.w (sp)+,d0
         bsr dispatcher\.e0
         bra .c10

.c101:   move.w d0,-(sp)
         bsr clrrect   ;in: x8poscp, y8poscp!!!  FIXME!!
         move.w (sp)+,d0
         bsr dispatcher\.e1
         bra .c10

xchgxy:  tst.b xchgdir(a3)
         beq exit7

         movem.w d0/d1,-(sp)
         move.b x0,d0
         move.b y0,d1
         move.b d1,x0
         move.b d0,y0
         movem.w (sp)+,d0/d1
exit7:   rts

drawrect: bsr xchgxy
         clr.w xcut(a3)
         move.b crsrbyte(a3),y8byte(a3)

         bsr calcx
         or.b crsrx(a3),d0
         move.b d0,d4   ;rectulx
         tst.b xdir(a3)
         beq .c4

         sub.b x0(a3),d4
         cmp.b d4,d0
         ;cmp al,dl
         bcc .c2

         not.b d4
         or.b d4,d4
         beq .c10

         addq.b #1,xcut(a3)
.c10:    ;mov dl,al
         move.b d0,d4
         addq.b #1,d4
         bra .c7

.c4:     add.b x0(a3),d4
         cmp.b d0,d4
         ;cmp dl,al
         bcs .c5

         cmpi.b #hormax*8+1,d4
         bcs .c2

.c5:     move.b #hormax*8,d4
         addq.b #1,xcut(a3)

.c2:     ;mov bl,dl
         move.b d4,d1
         ;sub dl,al
         sub.b d0,d4
         ;cmp bl,dl
         cmp.b d4,d1
         bcc .c7

         neg.b d4
.c7:     ;mov [x8poscp],dl
         move.b d4,x8poscp(a3)
         ;mov dh,[crsry]
         move.b crsry(a3),d5
         ;or dh,[crsrbyte]
         or.b crsrbyte(a3),d5
         ;mov al,dh
         move.b d5,d0
         ;mov bl,[y0]
         move.b y0(a3),d1
         ;cmp [ydir],bh
         tst.b ydir(a3)
         beq .c3

         ;mov cl,dh
         move.b d5,d3
         ;sub dh,bl
         sub.b d1,d5
         ;cmp cl,dh
         cmp.b d5,d3
         bcc .c1

         not.b d5
         or.b d5,d5
         beq .c12

         addq.b #1,ycut(a3)
.c12:    ;mov dh,al
         move.b d0,d5
         ;inc dh
         addq.b #1,d5
         bra .c8

.c3:     ;add dh,bl
         add.b d1,d5
         ;cmp dh,bl
         cmp.b d1,d5
         bcs .c6

         ;cmp dh,vermax*8+1
         cmpi.b #vermax*8+1,d5
         bcs .c1

.c6:     ;mov dh,vermax*8
         move.b #vermax*8,d5
         addq.b #1,ycut(a3)

.c1:     ;mov bl,dh
         move.b d5,d1
         ;sub dh,al
         sub.b d0,d5
         ;cmp bl,dh
         cmp.b d5,d1
         bcc .c8

         neg.b d5
.c8:     ;mov [y8poscp],dh
         move.b d5,y8poscp(a3)

         ;mov si,[crsrtile]
         move.l crsrtile(a3),a5
         ;mov bl,[crsrbit]
         move.b crsrbit(a3),d1
         bsr ymove
         ;cmp bh,[ycut]
         tst.b ycut(a3)
         bne .c11

         bsr xmove
.c11:    ;mov dl,[x8poscp]
         move.b x8poscp(a3),d4
         ;mov dh,[y8poscp]
         move.b y8poscp(a3),d5
         ;mov cl,[crsrbyte]
         move.b crsrbyte(a3),d3
         ;mov [y8byte],cl
         move.b d3,y8byte(a3)
         ;mov bl,[crsrbit]
         move.b crsrbit(a3),d1
         ;mov si,[crsrtile]
         move.l crsrtile(a3),a5
         bsr xmove
         ;cmp bh,[xcut]
         tst.b xcut(a3)
         bne drrect1\.rts

ymove:   ;cmp [ydir],0
         tst.b ydir(a3)
         bne loopup

loopdn:  bsr drrect1
.c10:    bsr pixel11
         ;dec dh
         subq.b #1,d5
         beq drrect1\.rts

         addq.b #1,y8byte(a3)
         ;cmp byte [y8byte],8
         cmpi.b #8,y8byte(a3)
         bne loopdn

         ;mov si,[down+si]
         movea.l down(a5),a5
         ;mov [y8byte],bh
         clr.b y8byte(a3)
         bra loopdn

loopup:  bsr drrect1
.c11:    bsr pixel11
         subq.b #1,d5
         beq drrect1\.rts

         subq.b #1,y8byte(a3)
         bpl loopup

         ;mov si,[up+si]
         move.l up(a5),a5
         ;mov byte [y8byte],7
         move.b #7,y8byte(a3)
         bra loopup

xmove:   ;cmp [xdir],bh
         tst.b xdir(a3)
         bne looplt

looprt:  bsr drrect1
.c12:    bsr pixel11
         subq.b #1,d4
         beq drrect1\.rts

         ;shr bl,1
         lsr.b d1
         bcc .c12

         ;mov si,[right+si]
         movea.l right(a5),a5
         ;mov bl,128
         move.b #128,d1
         bra looprt

looplt:  bsr drrect1
.c15:    bsr pixel11
         subq.b #1,d4
         beq drrect1\.rts

         ;shl bl,1
         lsl.b d1
         bcc .c15

         ;mov si,[si+left]
         movea.l left(a5),a5
         ;mov bl,1
         move.b #1,d1
         bra looplt

drrect1: move.l video(a5),d0
         clr.l d3
         move.b y8byte(a3),d3
         mulu #40,d3
         add.l d3,d0
.rts:    rts


loopup2: bsr xclrect2
         beq.s drrect1\.rts

         subq.b #1,y8byte(a3)
         bpl loopup2

         movea.l up(a5),a5
         move.b #7,y8byte(a3)
         bra loopup2

clrrect:  ;in: x8poscp, y8poscp
         bsr xchgxy
         bsr calcx   ;sets ah=0
         tst.b xdir(a3)
         beq.s .c3

         subq.b #8,d0
         not.b d0
.c3:     ;add al,[x8poscp]
         add.b x8poscp(a3),d0
         lsr.b #3,d0
         ;mov dl,al
         move.b d0,d4
         ;inc dl
         addq.b #1,d4
         ;mov [x8poscp],dl
         move.b d4,x8poscp(a3)
         ;mov dh,[y8poscp]
         move.b y8poscp(a3),d5
         ;mov cl,[crsrbyte]
         move.b crsrbyte(a3),d3
         ;mov [y8byte],cl
         move.b d3,y8byte(a3)
         ;mov si,[crsrtile]
         movea.l crsrtile(a3),a5
         ;cmp [ydir],ah
         tst.b ydir(a3)
         bne.s loopup2

loopdn2: bsr xclrect2
         beq.s xclrect2\.rts

         addq.b #1,y8byte(a3)
         ;cmp byte [y8byte],8
         cmpi.b #8,y8byte(a3)
         bne loopdn2

         ;mov si,[si+down]
         movea.l down(a5),a5
         ;mov byte [y8byte],0
         clr.b y8byte(a3)
         bra.s loopdn2

xclrect2:;push si
         move.l a5,-(sp)
         ;cmp [xdir],0
         tst.b xdir(a3)
         bne.s .c2

.c1:     bsr clrect12
         ;mov si,[si+right]
         movea.l right(a5),a5
         ;dec dl
         subq.b #1,d4
         bne .c1
         bra .c3

.c2:     bsr clrect12
         ;mov si,[si+left]
         movea.l left(a5),a5
         subq.b #1,d4
         bne .c2

.c3:     movea.l (sp)+,a5
         ;mov dl,[x8poscp]
         move.b x8poscp(a3),d4
         ;dec dh
         subq.b #1,d5
.rts:    rts

clrect12:clr.l d6
         move.b y8byte(a3),d6
         move.b (a5,d6),d1
         clr.l d0
         tst.b pseudoc(a3)
         beq .cont2

         move.b d1,d0
         move.l d6,d3
         lsl.l #2,d3
         move.l count0(a5,d3),d1
         vidmacp
.cont2:  mulu #nextline,d6
         add.l video(a5),d6
         movea.l BITPLANE1_PTR(a3),a0
         move.b d1,(a0,d6)
         movea.l BITPLANE2_PTR(a3),a0
         move.b d0,(a0,d6)
         movea.l BITPLANE3_PTR(a3),a0
         move.b #0,(a0,d6)
         rts

crsrset1:
         ;;mov si,[crsrtile]
	 movea.l crsrtile(a3),a0
	 moveq #0,d0
         move.b crsrbyte(a3),d0
         mulu #nextline,d0
         add.l (video,a0),d0

         ;;xor bx,bx
         moveq #0,d1

         ;;mov bl,[crsrbit]
         move.b crsrbit(a3),d1
         rts

setdirmsk:
         bsr totext
         move.l GRAPHICS_BASE(a3),a6 
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,6
         color 2
         print 'SET DIRECTORY MASK ('
         color 1
         print 'ENTER'
         color 2
         print ' = ?#)'
         color 3
         movepenq 0,14
         bsr TXT_PLACE_CURSOR
.c3:     lea svfn(a3),a4
         moveq #0,d3   ;length
.c1:     movem.l a1/a4/a6/d3,-(sp)
         bsr getkey
         movem.l (sp)+,a1/a4/a6/d3
         cmpi.b #$d,d0
         beq .c11

         cmpi.b #8,d0    ;backspace
         beq .c12

         cmpi.b #27,d0     ;esc
         beq .rts

         cmpi.b #'!',d0
         bcs .c1

         cmpi #126,d0
         bcc .c1

         cmpi.b #FNMAXLEN*8,d3    ;fn length limit
         bcc .c1

         lea.l nofnchar+6(a3),a0
.c50:    move.b (a0)+,d1
         cmp.b d0,d1
         beq .c1

         cmpa.l #stringbuf,a0
         bne .c50

         cmpi.b #'a',d0
         bcs .c6

         cmpi.b #'z'+1,d0
         bcc .c6

         sub.b #'a'-'A',d0
.c6:     move.l a4,a0
         move.b d0,(a4)+
         moveq #14,d1
         moveq #0,d4
         move.l a0,-(sp)
         bsr TXT_ON_CURSOR
         move.l (sp)+,a0
         addq #8,d3
         moveq #1,d0
         jsr Text(a6)
.cont4:  bsr TXT_PLACE_CURSOR
         bra .c1

.c11:    tst.b d3
         bne .c5

         move.b #"#",(a4)+
         move.b #"?",(a4)+
.c5:     clr.b (a4)
.rts:    rts

.c12:    subq.l #1,a4
         subq.b #8,d3
         bcs .c3

         moveq #14,d1
         moveq #8,d0
         bsr TXT_REMOVE_CURSOR
         bra .cont4

setviewport:
        tst.b mouseact(a3)
        beq.s setdirmsk\.rts

        ;mov di,viewport
        lea.l viewport(a3),a5
        ;mov ax,[crsrtile]
        ;mov [di],ax
        move.l crsrtile(a3),(a5)
        ;mov si,vptilecx
        lea.l vptilecx(a3),a4
        ;mov al,[crsry]
        move.b crsry(a3),d0
        ;mov word [si],102h
        move.w #$201,(a4)
        ;cmp al,8
        cmpi.b #8,d0
        bcc.s .c1

        ;dec [vptilecy]
        subq.b #1,vptilecy(a3)
        ;add word [di],tilesize*hormax
        add.l #tilesize*hormax,(a5)      ;up
        bra.s .c2

.c1:    ;cmp al,vermax*8-8    ;184
        cmpi.b #vermax*8-8,d0
        bcs.s .c2

        ;inc [vptilecy]
        addq.b #1,vptilecy(a3)
        ;sub word [di],tilesize*hormax
        sub.l #tilesize*hormax,(a5)       ;down
.c2:    ;mov al,[crsrx]
        move.b crsrx(a3),d0
        ;cmp al,8
        cmpi.b #8,d0
        bcc.s .c3

        ;sub byte [si],2
        subq.b #2,(a4)
        ;add word [di],tilesize*2
        add.l #tilesize*2,(a5)
        bra.s .c5

.c3:    ;cmp al,16
        cmpi.b #16,d0
        bcc.s .c6

        ;dec byte [si]
        subq.b #1,(a4)
        ;add word [di],tilesize
        add.l #tilesize,(a5)
        bra.s .c5

.c6:    ;cmp al,hormax*8-8  ;152
        cmpi.b #hormax*8-8,d0
        bcs.s .c8

        ;add byte [si],2
        addq.b #2,(a4)
        ;sub word [di],tilesize*2
        sub.l #tilesize*2,(a5)
        bra.s .c5

.c8:    ;cmp al,hormax*8-16   ;144
        cmpi.b #hormax*8-16,d0
        bcs.s .c5

        ;inc byte [si]
        addq.b #1,(a4)
        ;sub word [di],tilesize
        sub.l #tilesize,(a5)
.c5:    ;mov bx,[di]
        movea.l (a5),a1
        ;mov bx,[bx+ul]
        movea.l ul(a1),a1
        ;mov ax,[bx+left]
        ;mov [di],ax
        move.l left(a1),(a5)
        ;mov cl,3
        ;shl word [si],cl
        bsr calcx
        ;mov ah,[crsrbyte]
        move.b (a4),d1
        lsl.b #3,d1
        add.b d0,d1
        move.b d1,(a4)+
        move.b (a4),d1
        lsl.b #3,d1
        add.b crsrbyte(a3),d1
        move.b d1,(a4)
        ;add [si],ax
        rts

crsrset: bsr crsrset1
         tst.b zoom(a3)
         bne.s pixel11\.rts

pixel11: movea.l BITPLANE3_PTR(a3),a0   ;it should be after crsrset, IN: d1 - crsrbit, d0 - addr of video tile line
         or.b d1,(a0,d0)
.rts:    rts

crsrclr: tst.b zoom(a3)
         bne.s .c3

         movea.l crsrtile(a3),a4
         moveq #0,d4
         move.b crsrbyte(a3),d4
         ;move.b (a4,d4.w),d0
         movea.l BITPLANE3_PTR(a3),a0
         ;move.w d4,d6
         mulu #nextline,d4
         add.l (video,a4),d4
         move.b crsrbit(a3),d1
         not.b d1
         and.b d1,(a0,d4)
         ;movea.l BITPLANE1_PTR(a3),a0
         ;movea.l BITPLANE2_PTR(a3),a1
         ;tst.b pseudoc(a3)
         ;bne.s .c2

         ;move.b #0,(a1,d4)
         ;move.b d0,(a0,d4)
         ;rts

.c2:     ;lsl.w #2,d6
         ;move.l (count0,a4,d6.w),d1
         ;vidmacp
         ;move.b d1,(a0,d4)
         ;move.b d0,(a1,d4)
         rts

.c3:     movea.l crsrtile(a3),a0
         move.l (video,a0),d1
         movea.l viewport(a3),a0
         sub.l (video,a0),d1
         divu #nextline,d1
         moveq #0,d0
         move.w d1,d0
         add.b crsrbyte(a3),d0
         move.w #0,d1
         swap d1
         lsl.w #3,d1
         move.b crsrbit(a3),d2
.c10:    add.b d2,d2
         bcs.s .c8

         addq.l #1,d1
         bra.s .c10

.c8:     mulu #320,d0
         add.w d1,d0
         movea.l BITPLANE3_PTR(a3),a0
         adda.l d0,a0
         moveq #0,d1
         move.b d1,(a0)
         move.b d1,nextline(a0)
         move.b d1,2*nextline(a0)
         move.b d1,3*nextline(a0)
         move.b d1,4*nextline(a0)
         move.b d1,5*nextline(a0)
         move.b d1,6*nextline(a0)
         move.b d1,7*nextline(a0)
         rts

outdec:  lea.l temp(a3),a1        ;in: d0
         move.w d0,(a1)
         lea.l sformat(a3),a0
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
         movea.l GRAPHICS_BASE(a3),a6
         jmp Text(a6)

infov:   bsr crsrclr
         bsr totext
         movea.l GRAPHICS_BASE(a3),a6
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,6
         ;color 2
         tst.b fn(a3)
         beq.s .c11

         print "Last loaded filename: "
         lea.l fn(a3),a4
         movea.l a4,a0
.c1:     cmpi.b #'.',(a4)+
         bne.s .c1

         suba.l a0,a4
         move.l a4,d0
         subq.l #1,d0
         movea.l RASTER_PORT(a3),a1
         jsr Text(a6)
.c11:    bsr boxsz
         beq.w .c12

         move.w d3,-(sp)
         move.w d6,-(sp)
         move.w d0,-(sp)
         movepenq 0,14
         print "Active pattern size: "

         move.w (sp)+,d0
         bsr outdec
         print "x"

         clr.l d0
         move.b boxsz_cury(a3),d0
         bsr outdec
         movepenq 0,22
         print "Box life bounds: "

         clr.l d0
         move.b boxsz_xmin(a3),d0
         bsr outdec
         print "<=x<="

         move.w (sp)+,d0
         bsr outdec
         print " "

         clr.l d0
         move.b boxsz_ymin(a3),d0
         bsr outdec
         print "<=y<="

         move.w (sp)+,d0
         bsr outdec
.c12:    movepenq 0,30
         print "Rules: "
         bsr showrules2
         bsr getkey3
         bra tograph

outinnum:color 2
         invvideo
         lsl.w #3,d7
         lea.l xformat(a3),a0
         movea.l a5,a1
         lea.l stuffChar(pc),a2
         move.l #-1,charCount(a3)
         move.l a3,-(sp)
         lea.l stringbuf(a3),a3
         movea.l 4.w,a6
         jsr RawDoFmt(a6)
         move.l (sp)+,a3
         move.l charCount(a3),d0
         move.w d0,d4
         subq.w #1,d4
         lsl.w #3,d4
         add.w d4,d7
         lea.l stringbuf(a3),a0
         movea.l RASTER_PORT(a3),a1
         move.l GRAPHICS_BASE(a3),a6
         jsr Text(a6)
         color 1
         normvideo
         print ']: '
         normvideo
         color 3     ;must be followed by inputdec

inputhex:bsr TXT_PLACE_CURSOR
.c3:     lea.l stringbuf(a3),a4
         moveq #0,d3
.loop:   movem.l a4/a5/a6/d3/d5/d6,-(sp)
         bsr getkey
         movem.l (sp)+,a4/a5/a6/d3/d5/d6
         cmpi.b #27,d0     ;esc
         bne.s .c17

.c101:   move.w d5,d1
         move.w d7,d0
         add.w #8,d0
         bsr TXT_REMOVE_CURSOR
         subq.b #8,d3
         bcc.s .c101

         ori.b #1,d0    ;sets NZ
         rts

.c17:    cmpi.b #$d,d0
         beq.s .c11

         cmpi.b #8,d0   ;backspace
         beq.w .c12

         cmpi.b #3*8,d3
         beq.s .loop

         move.b d0,d2
         cmpi.b #'0',d0
         bcs.s .loop

         cmpi.b #'9'+1,d0
         bcs.s .ok

         cmpi.b #'A',d0
         bcs.s .loop

         cmpi.b #'F'+1,d0
         bcs.s .okl

         cmpi.b #'a',d0
         bcs.s .loop

         cmpi.b #'f'+1,d0
         bcc.s .loop

         subi.b #32,d2
.okl:    subi.b #7,d2
.ok:     subi.b #'0',d2
         addq.b #8,d3
         move.w d5,d1
         move.w d7,d4
         bsr TXT_ON_CURSOR
         move.l a4,a0
         move.b d0,(a4)
         moveq #1,d0
         jsr Text(a6)
         move.b d2,(a4)+
.cont4:  bsr TXT_PLACE_CURSOR
         bra.w .loop

.c11:    moveq #0,d0  ;result
         move.b d3,d4
         tst.b d3
         beq.s .c101

         lea.l stringbuf(a3),a0
         bra.s .l4

.l2:     lsl.w #4,d0
.l4:     clr.w d1
         move.b (a0)+,d1
         add.w d1,d0
         subq #8,d4
         bne.s .l2

.c21:    move.w d0,-(sp)
         move.w d5,d1
         move.w d7,d0
         add.w #8,d0
         bsr TXT_REMOVE_CURSOR
         move.w (sp)+,d0
         eor.b d1,d1     ;sets ZF
         rts

.c12:    subq.l #1,a4
         subq.b #8,d3
         bcs .c3

         move.w d5,d1
         move.w d7,d0
         add.w #16,d0
         bsr TXT_REMOVE_CURSOR
         bra.s .cont4

chgcolors:
         bsr totext
         move.l GRAPHICS_BASE(a3),a6 
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,6
         color 2  ;green
         print 'PRESS '
         color 1  ;red
         invvideo
         print 'ENTER'
         color 2
         normvideo
         print ' TO USE THE DEFAULT VALUE OR'
         movepenq 0,14
         print 'INPUT A HEXADECIMAL NUMBER (0-FFF).'
         movepenq 0,22
         color 1
         print 'THE EDIT BACKGROUND ['
         lea.l COLORS(a3),a5
         moveq #22,d5  ;Y
         moveq #24,d7  ;X
         bsr outinnum
         bne.s .l1

         move.w d0,(a5)
.l1:     color 1
         movepenq 0,30
         print 'THE GO BACKGROUND ['
         lea.l lightgreen(a3),a5
         moveq #30,d5 ;Y
         move.l #22,d7
         bsr outinnum
         bne.s .l2

         move.w d0,(a5)
.l2:     color 1
         movepenq 0,38
         print 'THE LIVE CELL ['
         lea.l COLORS+2(a3),a5
         moveq #38,d5
         moveq #18,d7
         bsr outinnum
         bne.s .l3

         move.w d0,(a5)
.l3:     color 1
         movepenq 0,46
         print 'THE NEW CELL ['
         lea.l COLORS+4(a3),a5
         moveq #46,d5
         moveq #17,d7
         bsr outinnum
         bne.s .l4

         move.w d0,(a5)
.l4:     color 1
         movepenq 0,54
         print 'TEXT COLOR ['
         lea.l COLORS+6(a3),a5
         moveq #54,d5
         moveq #15,d7
         bsr outinnum
         bne.s .l7

         move.w d0,(a5)
.l7:     color 1
         movepenq 0,62
         print 'THE CURSOR OVER AN EMPTY CELL ['
         lea.l COLORS+8(a3),a5
         moveq #62,d5
         moveq #34,d7
         bsr outinnum
         bne.s .l5

         move.w d0,(a5)
.l5:     color 1
         movepenq 0,70
         print 'THE CURSOR OVER A LIVE CELL ['
         lea.l COLORS+10(a3),a5
         moveq #70,d5
         moveq #32,d7
         bsr outinnum
         bne.s .l6

         move.w d0,(a5)
.l6:     color 1
         movepenq 0,78
         print 'THE CURSOR OVER A NEW CELL ['
         lea.l COLORS+12(a3),a5
         moveq #78,d5
         moveq #31,d7
         bsr outinnum
         bne.s .l9

         move.w d0,(a5)
.l9:     movepenq 0,86
         color 1
         print 'TO SAVE THIS CONFIG?'
.l8:     bsr getkey
         move.l d0,d4
         movea.l VIEW_PORT(a3),a0
         movea.l GRAPHICS_BASE(a3),a6
         lea.l COLORS(a3),a1   ; Pointer to the color list
         moveq #8,d0           ; 8 colors to set
         jsr LoadRGB4(a6)      ; Set the colors
         ori.b #32,d4
         cmpi.b #'n',d4
         beq.s putpixel2\.e1   ; -> rts

         cmpi.b #'y',d4
         bne.s .l8
         bra savecf

putpixel2:
         ;;mov di,[di+video]
         move.l video(a5),d0
         mulu #40,d6
         add.l d6,d0
         movea.l BITPLANE2_PTR(a3),a0
         or.b d3,(a0,d0)
         movea.l BITPLANE1_PTR(a3),a0
         not.b d3
         and.b d3,(a0,d0)
.e1:     rts

showtent:
         ;mov ax,word [x0]
         ;push ax
         move.w x0(a3),-(sp)
         clr.b ppmode(a3)
         lea.l iobseg,a4
         move.l tsz(a3),d6
.loop:   beq .fin

         move.w (a4)+,x0(a3)
         move.l d6,-(sp)
         bsr putpixel
         move.l (sp)+,d6
         subq.l #1,d6
         bra .loop

.fin:    ;pop ax
         ;mov word [x0],ax
         move.w (sp)+,x0(a3)
         addq.b #1,ppmode(a3)
         rts

clrscn:  movea.l BITPLANE1_PTR(a3),a0
         movea.l BITPLANE2_PTR(a3),a2
         move.w #nextline*50-1,d1
.e0:     moveq #0,d0
.l1:     move.l d0,(a0)+
         move.l d0,(a2)+
         dbra d1,.l1
         rts

clrrow25:movea.l BITPLANE1_PTR(a3),a0
	 movea.l BITPLANE2_PTR(a3),a2
     lea.l 40*192(a0),a0
     lea.l 40*192(a2),a2
     move.w #nextline*2-1,d1
     bra clrscn\.e0

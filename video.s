insteps: bsr totext
         move.l GRAPHICS_BASE(a3),a6 
         ;movea.l RASTER_PORT(a3),a1
.c38:    movepenq 0,8
         color 2
         print "NUMBER OF GENERATIONS: "   ;24 chars = 192 pixels
         bsr TXT_PLACE_CURSOR
.c3:     lea stringbuf(a3),a4
         moveq #0,d3
         moveq #0,d6
.c1:     movem.l a1/a4/a6/d3,-(sp)
         bsr getkey
         movem.l (sp)+,a1/a4/a6/d3
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

         moveq #8,d1
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
         moveq #8,d1
         bsr TXT_REMOVE_CURSOR
         bra .cont4

.c11:    move.w #184,d0
         moveq #8,d1
         bsr TXT_REMOVE_CURSOR
         tst.b d3
         beq .c20

;         sub si,bx   ;convert to binary
          move.w d3,d1
          lsr #3,d1
          suba.w d1,a4
;         xor dx,dx
          moveq #0,d1
;         dec bx
          subq.b #8,d3
;         shl bx,1
          lsr #2,d3
.c33:    ;;lodsb
          move.b (a4)+,d0
          sub.b #'0',d0
;         or al,al
;         jz .c34
          beq .c34

;         mov di,[tobin+bx]
          lea tobin(a3),a0
          move.w (a0,d3),d5
.c32:     ;;add dx,di
          add.w d5,d1
;         jnc .c38a
          bcc .c38a
;         jmp .c38       ;65535=max
          bra .c38

.c38a:    ;;dec al
          subq.b #1,d0
;         jnz .c32
          bne .c32

.c34:     ;;sub bx,2
          sub.w #2,d3
;         jns .c33
          bpl .c33

         move.w d1,d6
         bra .c20

bornstay:
;;         mov si,stringbuf
;;.c3:     mov di,si
;;.c1:     call getkey
.c3:     lea stringbuf(a3),a4
         moveq #0,d3
.c1:     movem.l a1/a4/a6/d2/d3,-(sp)
         bsr getkey
         movem.l (sp)+,a1/a4/a6/d2/d3
         cmpi.b #$d,d0  ;enter
         beq .c11

         cmpi.b #8,d0    ;backspace
         beq .c12

         cmpi.b #'0',d2
         beq .c40

         cmpi.b #27,d0  ;esc
         beq .c11

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

.c11:    rts

.c12:    subq.l #1,a4
         subq.b #8,d3
         bmi .c3

         move d7,d1
         moveq #64,d0
         bsr TXT_REMOVE_CURSOR
         bra .cont4

inborn:  bsr totext
         move.l GRAPHICS_BASE(a3),a6 
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,8
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
         movepenq 0,16
         print 'ALUES.  FOR EXAMPLE, '
         color 1
         invvideo ;purple
         print "CONWAYS'S LIFE"
         normvideo
         color 2
         print ' HAS'
         movepenq 0,24
         print 'BORN=3 AND STAY=23, '
         color  1 ;purple
         invvideo
         print 'SEEDS'
         normvideo
         color 2
         print ' - BORN=2 AND E'
         movepenq 0,32
         print 'MPTY STAY, '
         color 1
         invvideo ;purple
         print 'HIGHLIFE'
         normvideo
         color 2
         print ' - BORN=36 AND STAY=2'
         movepenq 0,40
         print '3, '
         color 1
         invvideo ;purple
         print 'LIFE WITHOUT DEATH'
         normvideo
         color 2
         print ' - BORN=3 AND STAY='
         movepenq 0,48
         print '012345678, ...'
         movepenq 0,56
         color 3
         print 'BORN = '
         bsr TXT_PLACE_CURSOR

         moveq #56,d7
         moveq #'1',d2
         bsr bornstay
         move d7,d1
         moveq #56,d0
         bra TXT_REMOVE_CURSOR

instay:  movepenq 0,64
         print 'STAY = '
         bsr TXT_PLACE_CURSOR

         moveq #64,d7
         moveq #'0',d2
         bra bornstay

indens:  bsr totext
         move.l GRAPHICS_BASE(a3),a6 
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,8
         color 3
         print "SELECT DENSITY OR PRESS "
         color 1
         print "ESC"
         color 3
         print " TO EXIT"
         movepenq 36,16
         color 1
         print '0'
         color 2
         print ' - 12.5%'
         movepenq 36,24
         color 1
         print '1'
         color 2
         print ' - 28%'
         movepenq 36,32
         color 1
         print '2'
         color 2
         print ' - 42%'
         movepenq 36,40
         color 1
         print '3'
         color 2
         print ' - 54%'
         movepenq 36,48
         color 1
         print '4'
         color 2
         print ' - 64%'
         movepenq 36,56
         color 1
         print '5'
         color 2
         print ' - 73%'
         movepenq 36,64
         color 1
         print '6'
         color 2
         print ' - 81%'
	 movepenq 36,72
         color 1
         print '7'
         color 2
         print ' - 88.5%'
         movepenq 36,80
         color 1
         print '8'
         color 2
         print ' - 95%'
	 movepenq 36,88
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
         movepenq 0,24
         color 2
         print 'SELECT BENCHMARK MODE'
         movepenq 8,32
         color 1
         print '0'
         color 2
         print ' - CALCULATIONS'
         movepenq 8,40
         color 1
         print '1'
         color 2
         print ' - VIDEO'
         movepenq 8,48
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
         movepenq 72,8
         color 3
         print '*** XLIFE COMMANDS ***'
         movepenq 0,16
         color 1
         print '!'
         color 2
         print ' randomize screen'
         movepenq 0,24
         color 1
         print '%'
         color 2
         print ' set random density - default=42%'
         movepenq 0,32
	 color 1
         print '+'
         color 2
         print '/'
	 color 1
         print '-'
         color 2
         print ' zoom in/out'
         movepenq 0,40
	 color 1
         print '.'
         color 2
         print '/'
	 color 1
         print 'H'
         color 2
         print ' center/home the cursor'
         movepenq 0,48
	 color 1
         print '?'
         color 2
         print ' show this help'
         movepenq 0,56
	 color 1
         print 'B'
         color 2
         print ' benchmark'
         movepenq 0,64
	 color 1
         print 'C'
         color 2
         print ' clear the screen'
         movepenq 0,72
	 color 1
         print 'E'
         color 2
         print ' toggle the pseudocolor mode'
         movepenq 0,80
	 color 1
         print 'g'
         color 2
         print ' toggle the run/stop mode'
         movepenq 0,88
	 color 1
         print 'h'
         color 2
         print ' toggle the hiding (fastest) mode'
         movepenq 0,96
	 color 1
         print 'l'
         color 2
         print ' load and transform a pattern'
         movepenq 0,104
	 color 1
         print 'L'
         color 2
         print ' reload a pattern'
         movepenq 0,112
	 color 1
         print 'o'
         color 2
         print ' one step'
         movepenq 0,120
	 color 1
         print 'Q'
         color 2
         print ' quit'
         movepen 0,128
	 color 1
         print 'R'
         color 2
         print ' set the rules'
         movepen 0,136
	 color 1
         print 'S'
         color 2
         print ' save'
         movepen 0,144
	 color 1
         print 't'
         color 2
         print ' toggle plain/torus topology'
         movepen 0,152
	 color 1
         print 'v'
         color 2
         print ' show some info'
         movepen 0,160
	 color 1
         print 'V'
         color 2
         print ' show comments to a pattern'
         movepen 0,168
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
         bsr getkey
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
         bsr digiout

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

.c1:        
	 ;;mov word [bx],0a0ah
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
         bcc .c1
         rts

  if 0
showscnz:
;;xlimit   = $14
;;ylimit   = $15
;;         #assign16 i1,viewport
         mov si,[viewport]

;;         lda #5
;;         sta xlimit
         mov bl,5

         cmp [pseudoc],0
         jnz showscnzp

;;         lda #$c
;;         sta cont2+2
;;         lda #0
;;         sta cont2+1
         xor di,di

;;loop3    lda #3
;;         sta ylimit
.loop3:  mov bh,3

;;loop4    ldy #0              ;check sum?
;;loop2    lda (i1),y
;;         ldx #0
;;loop1    asl
;;         sta 7
;;         lda #32
;;         bcc cont2
.loop4:  mov cx,8
.loop2:  lodsb
         mov dl,8
         mov dh,al
.loop1:  mov al,20h   ;space char
         mov ah,[czbg]
         or ah,[zfg]
         shl dh,1
         jnc .cont2

;;         lda #81         ;live cell char
;;cont2    sta $c00,x
;;         lda 7
;;         inx
;;         cpx #8
;;         bne loop1
         mov al,9   ;live cell char and attribute
.cont2:  stosw
         dec dl
         jnz .loop1

;;         lda #39    ;CY=1
;;         adc cont2+1
;;         sta cont2+1
;;         bcc nocy1
         add di,80-16

;;         inc cont2+2
;;nocy1    iny
;;         cpy #8
;;         bne loop2
         loop .loop2

;;         dec ylimit
;;         beq cont3
         dec bh
         jz .cont3

;;         lda #<tilesize*20-1 ;CY=1
;;         adc i1
;;         sta i1
;;         lda i1+1
;;         adc #>tilesize*20
;;         sta i1+1
;;         bcc loop4
         add si,tilesize*hormax-8
         jmp .loop4

;;cont3    dec xlimit
;;         bne cont11
;;         rts
.cont3:  dec bl         ;xlimit
         jnz .cont11
         retn

;;cont11   lda cont2+1    ;CY=1
;;         sbc #<952
;;         sta cont2+1
;;         lda cont2+2
;;         sbc #>952
;;         sta cont2+2
;;         lda i1   ;CY=1
;;         sbc #<tilesize*39
;;         sta i1
;;         lda i1+1
;;         sbc #>tilesize*39
;;         sta i1+1
;;         bne loop3
.cont11: sub di,24*80-16
         sub si,tilesize*(hormax*2-1)+8
         jmp .loop3

showscnzp:
         mov si,[viewport]
         mov dl,5  ;xlimit
         xor di,di
.loop3:  mov dh,3   ;ylimit
.loop4:  mov cl,8
         lea bp,[si+count0]
.loop2:  mov ax,[ds:bp]
         and ax,18c0h
         mov ch,al
         shl ah,1
         or ch,ah
         mov ax,[ds:bp+2]
         and ax,318h
         shr al,1
         or ch,al
         or ch,ah
         lodsb
         add bp,4
         mov bl,8
         mov bh,al
.loop1:  shl ch,1
         rcr al,1
         shl bh,1
         rcr al,1
         mov ah,[zfg]
         test al,0c0h
         mov al,20h   ;space char and attribute
         jns .cont2

         mov al,9    ;live cell char
         jpe .cont2

         mov ah,[zfgnc]    ;new cell attr
.cont2:  or ah,[czbg]
         stosw
         dec bl
         jnz .loop1

         add di,80-16
         dec cl
         jnz .loop2

         dec dh
         jz .cont3

         add si,tilesize*hormax-8
         jmp .loop4

.cont3:  dec dl         ;xlimit
         jnz .cont11
         retn

.cont11: sub di,24*80-16
         sub si,tilesize*(hormax*2-1)+8
         jmp .loop3
	endif

gexit:    jmp crsrset

showscn:
	 ;;call infoout
	 bsr infoout
         ;;or [zoom],0
	 tst.b zoom(a3)
         ;;jz .l1
         ;;jmp showscnz
	 ;**bne showscnz

.l1:
;;         tst @#tilecnt
         ;;cmp [tilecnt],0
	 tst.w tilecnt(a3)
;;         beq gexit
         ;;jz gexit
	 beq gexit

;;         tstb @#pseudoc
         ;;or [pseudoc],0
	 tst.b pseudoc(a3)
         bne showscnp

showscn2:
	 ;;mov @#startp,r0
         ;;mov si,[startp]
	 movea.l startp(a3),a4

.l1:
;;       mov video(r0),r5
	 ;;mov di,[video+si]
	 movea.l (video,a4),a5
    	 adda.l BITPLANE1_PTR(A3),A5

         ;;lodsw
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

;;         mov next(r0),r0
         ;;mov si,[next-8+si]
	 movea.l (next,a4),a4

;;         cmp #1,r0
         ;;cmp si,1
	 cmpa.w #1,a4
	 bne .l1

	 jmp crsrset

showscnp:
	 ;;mov @#startp,r0
         ;;mov si,[startp]
	 movea.l startp(a3),a4

.l1:
;;       mov video(r0),r5
	 ;;mov di,[video+si]
	 movea.l (video,a4),a0
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

;;         mov next(r0),r0
         ;;mov si,[next-8+si]
	 movea.l (next,a4),a4

;;         cmp #1,r0
         ;;cmp si,1
	 cmpa.w #1,a4
	 bne .l1

	 jmp crsrset

   if 0
chgdrv:  mov al,[curdrv]
         mov bx,drives
.l2:     inc ax
         cmp al,26
         jnz .l1

         xor ax,ax
.l1:     mov [curdrv],al
         xlatb
         or al,al
         mov dl,al
         mov al,[curdrv]
         jz .l2

         mov [loadmenu.c80],dl
         mov [es:si],dl
         sub dl,'A'
         mov ah,0eh
         int 21h

.ee1:    mov ah,3bh
         mov dx,patpath
         int 21h
         retn
    endif

loadmenu:bsr totext
         move.l GRAPHICS_BASE(a3),a6
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,8
         color 2  ;green
         print 'INPUT FILENAME, AN EMPTY STRING MEANS TO'
         movepenq 0,16
         print ' SHOW DIRECTORY. PRESS '
         color 1 ;red
         print 'TAB'
         color 2
         print ' TO USE RAMDIS'
         movepenq 0,24
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
         movepenq 0,32
.c80:    moveq.l #4,d0
         lea.l curdisk(a3),a0
         movea.l RASTER_PORT(a3),a1
         jsr Text(a6)
         ;print 'DH0:'
         bsr TXT_PLACE_CURSOR
.c3:     lea stringbuf(a3),a4
         moveq #0,d3   ;length
.c1:     movem.l a1/a4/a6/d3,-(sp)
         bsr getkey
         movem.l (sp)+,a1/a4/a6/d3

         cmpi.b #$d,d0  ;enter
         beq .c11

         cmpi.b #8,d0    ;backspace
         beq .c12

         cmpi.b #27,d0  ;esc
         bne .c17

.c100:   ;;mov ch,al
.c101:   ;;;call curoff  ;curoff changes ch?
         ;;or ch,ch
         rts

.c17:    cmpi.b #'*',d0
         bne .c21

         ;;mov ch,al
         ;;mov si,240
         ;;call chgdrv
         ;;jmp .c1
         bra .c1

.c21:    cmpi.b #9,d0  ;TAB
         bne .c18

         bsr ramdisk
         ;;mov ch,1
         ;;jmp .c101
         bra .c101

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
         moveq #32,d1  ;vertical pos
         moveq #24,d4  ;hor pos - it is defined by length of a drive name
	 bsr TXT_ON_CURSOR
         moveq #1,d0
         jsr Text(a6)
.cont4:  bsr TXT_PLACE_CURSOR
         bra .c1

.c12:    subq.l #1,a4
         subq.b #8,d3
         bcs .c3

         moveq #32,d1
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
         bra .c101

menu2:   bsr setdirmsk
         cmpi.b #27,d0     ;esc
         beq .c100

         bsr showdir  ;returns number of directory entries in D6
         move.l GRAPHICS_BASE(a3),a6
         movepenq 0,8
         ;db clrtoeol
         color 2
         print 'ENTER FILE# OR '
         color 1
         print 'ESC'
         color 2
         print ': '
         color 3
         bsr TXT_PLACE_CURSOR

.c3:     ;;mov di,stringbuf+1      ;+1?
         ;;mov si,di
         ;;xor cx,cx
         lea.l stringbuf+1(a3),a4
         move.l a4,a5
         moveq #0,d3
.c1:     movem.l a1/a4/a5/a6/d3/d6,-(sp)
         bsr getkey
         movem.l (sp)+,a1/a4/a5/a6/d3/d6
         cmpi.b #27,d0     ;esc
         bne .c17

.c100:   ;call curon
         bra loadmenu

.c17:    cmpi.b #$d,d0
         beq .c11

         cmpi.b #8,d0   ;backspace
         beq .c12

         cmpi.b #'0',d0
         bcs .c1

         cmpi.b #'9'+1,d0
         bcc .c1

         cmpi.b #3*8,d3
         beq .c1

         move.l a4,a0
         move.b d0,(a4)+
         addq.b #8,d3
         moveq #8,d1
         move.w #152,d4
         bsr TXT_ON_CURSOR
         moveq #1,d0
         jsr Text(a6)
.cont4:  bsr TXT_PLACE_CURSOR
         bra .c1

.c11:    moveq #0,d0  ;result
         move.b d3,d4
         tst.b d3
         beq .c3
         move.l a5,-(sp)
         bra .l4

.l2:     mulu #10,d0
.l4:     add.b (a5)+,d0
         subi.b #'0',d0
         subq #8,d4
         bne .l2

         move.l (sp)+,a5
.c21:    cmp d6,d0
         bcc .c1

.l3:     ;;call findfn
         ;;;call curoff
         ;;xor ax,ax     ;sets ZF
         rts

.c12:    subq.l #1,a4
         subq.b #8,d3
         bcs .c3

         moveq #8,d1
         move.w #168,d0
         bsr TXT_REMOVE_CURSOR
         bra .cont4
    if 0
getsvfn: call totext
         mov al,[loadmenu.c80]
         mov [.c80],al
         call printstr
         db green,'Enter filename (',red,'ESC',green,' - exit, '
         db red,'*',green,' - drive)',black,0dh,10
.c80:    db 'A:$'

.c3:     mov di,svfn
         xor cl,cl
.c1:     call getkey
         cmp al,0dh
         je .c11

         cmp al,8  ;backspace
         je .c12

         cmp al,27   ;esc
         jne .c17

.c100:   mov ch,al
.c101:   or ch,ch
         retn

.c17:    cmp al,'*'
         jne .c18

         mov ch,al
         mov si,80
         call chgdrv
         jmp .c1

.c18:    cmp al,'!'
         jc .c1

         cmp al,126
         jnc .c1

         mov si,nofnchar
         mov dl,al
.c5:     lodsb
         cmp al,dl
         je .c1

         cmp dl,'a'
         jc .c6

         cmp dl,'z'+1
         jnc .c6

         sub dl,'a'-'A'
.c6:     cmp si,stringbuf
         jne .c5

         cmp cl,8
         jnc .c1

         mov [di],dl
         inc di
         inc cl
         mov ah,2
         int 21h
         jmp .c1

.c11:    or cl,cl
         je .c100

         mov word [di],'.'+'8'*256
         mov word [di+2],'X'+'L'*256
         xor ch,ch
         mov [di+4],ch
         jmp .c101

.c12:    dec di
         dec cl
         js .c3

         call delchr
         jmp .c1

showrect:
         call printstr
         db 27,'[25H'
         db clrtoeol,gpurple,'MOVE, ',gwhite,'R',gpurple,'OTATE, '
         db gwhite,'F',gpurple,'LIP, ',gwhite,'ENTER',gpurple,', '
         db gwhite,'ESC',gcyan,'  X   Y',black,'$'
         call xyout
         xor bx,bx
         mov word [xdir],bx
         mov [xchgdir],bl
.c10:    call drawrect
         call showtent
.c11:    call crsrflash
         call getkey2
         cmp ah,4bh    ;cursor left
         jz .c100

         cmp ah,4dh  ;cursor right
         jz .c100

         cmp ah,48h  ;cursor up
         jz .c100

         cmp ah,50h   ;cursor down
         jz .c100

         cmp al,'.'     ;to center
         jz .c100

         cmp ah,47h     ;to home
         jz .c100

         cmp al,'r'
         jnz .c1

         call clrrect
         not [xchgdir]
         mov ax,word [xdir]
         xchg al,ah
         not al
         mov word [xdir],ax
         jmp .c10

.c1:     cmp al,'f'
         jne .c2

         call clrrect
         not [xdir]
         jmp .c10

.c2:     cmp al,0dh
         je exit7

         cmp al,27      ;esc
         stc
         je exit7
         jmp .c11

.c100:   push ax
         call clrrect
         pop ax
         call dispatcher.e0
         jmp .c10
   endif

xchgxy:  tst.b xchgdir(a3)
         beq exit7

         movem.w d0/d1,-(sp)
         move.b x0,d0
         move.b y0,d1
         move.b d1,x0
         move.b d0,y0
         movem.w (sp)+,d0/d1
exit7:   rts

  if 0
drawrect: call xchgxy
         xor bx,bx
         mov [xcut],bx       ;0 -> xcut,ycut
         mov al,[crsrbyte]
         mov [y8byte],al
         call calcx
         or al,[crsrx]
         mov dl,al   ;r1 - rectulx
         cmp bh,[xdir]
         je .c4

         sub dl,[x0]
         cmp al,dl
         jnc .c2

         not dl
         or dl,dl
         je .c10

         inc byte [xcut]
.c10:    mov dl,al
         inc dl
         jmp .c7

.c4:     add dl,[x0]
         cmp dl,al
         jc .c5

         cmp dl,hormax*8+1
         jc .c2

.c5:     mov dl,hormax*8
         inc byte [xcut]

.c2:     mov bl,dl
         sub dl,al
         cmp bl,dl
         jnc .c7

         neg dl
.c7:     mov [x8poscp],dl
         mov dh,[crsry]
         or dh,[crsrbyte]
         mov al,dh
         mov bl,[y0]
         cmp [ydir],bh
         je .c3

         mov cl,dh
         sub dh,bl
         cmp cl,dh
         jnc .c1

         not dh
         or dh,dh
         je .c12

         inc byte [ycut]
.c12:    mov dh,al
         inc dh
         jmp .c8

.c3:     add dh,bl
         cmp dh,bl
         jc .c6

         cmp dh,vermax*8+1
         jc .c1

.c6:     mov dh,vermax*8
         inc byte [ycut]

.c1:     mov bl,dh
         sub dh,al
         cmp bl,dh
         jnc .c8

         neg dh
.c8:     mov [y8poscp],dh

         mov si,[crsrtile]
         mov bl,[crsrbit]
         call ymove
         cmp bh,[ycut]
         jne .c11

         call xmove
.c11:    mov dl,[x8poscp]
         mov dh,[y8poscp]
         mov cl,[crsrbyte]
         mov [y8byte],cl
         mov bl,[crsrbit]
         mov si,[crsrtile]
         call xmove
         cmp bh,[xcut]
         ;jne exitdrawrect   ;optimize 8088
         je ymove
         jmp exitdrawrect

ymove:   cmp [ydir],0
         jne loopup

loopdn:  call drrect1
.c10:    call pixel11p
         dec dh
         je exitdrawrect

         inc byte [y8byte]
         cmp byte [y8byte],8
         jne loopdn

         mov si,[down+si]
         mov [y8byte],bh
         jmp loopdn

loopup:  call drrect1
.c11:    call pixel11p
         dec dh
         je exitdrawrect

         dec byte [y8byte]
         jns loopup

         mov si,[up+si]
         mov byte [y8byte],7
         jmp loopup

xmove:   cmp [xdir],bh
         jne looplt

looprt:  call drrect1
.c12:    call pixel11p
         dec dl
         je exitdrawrect

         shr bl,1
         jnc .c12

         mov si,[right+si]
         mov bl,128
         jmp looprt

looplt:  call drrect1
.c15:    call pixel11p
         dec dl
         je exitdrawrect

         shl bl,1
         ;;movb r0,r0
         jnc .c15

         mov si,[si+left]
         mov bl,1
         jmp looplt

drrect1: mov di,[si+video]
         mov cl,[y8byte]
         shr cl,1
         jnc .l1

         add di,2000h
.l1:     mov al,80
         mul cl
         add di,ax
exitdrawrect: retn

loopup2: call xclrect2
         je exitdrawrect

         dec byte [y8byte]
         jns loopup2

         mov si,[si+up]
         mov byte [y8byte],7
         jmp loopup2

clrrect:  ;in: x8poscp, y8poscp
         call xchgxy
         call calcx   ;sets ah=0
         cmp [xdir],ah
         je .c3

         sub al,8
         not al
.c3:     add al,[x8poscp]
         shr al,1
         shr al,1
         shr al,1
         mov dl,al
         inc dl
         mov [x8poscp],dl
         mov dh,[y8poscp]
         mov cl,[crsrbyte]
         mov [y8byte],cl
         cmp [pseudoc],ah
         jne clrectpc

         mov si,[crsrtile]
         cmp [ydir],ah
         jne loopup2

loopdn2: call xclrect2
         je exitclrect2

         inc byte [y8byte]
         cmp byte [y8byte],8
         jne loopdn2

         mov si,[si+down]
         mov byte [y8byte],0
         jmp loopdn2

xclrect2:push si
         cmp [xdir],0
         jne .c2

.c1:     call clrect12
         mov si,[si+right]
         dec dl
         jnz .c1
         jmp .c3

.c2:     call clrect12
         mov si,[si+left]
         dec dl
         jnz .c2

.c3:     pop si
         mov dl,[x8poscp]
         dec dh
exitclrect2: retn

clrect12:xor bx,bx
         mov al,[y8byte]
         mov bl,al
         mov di,[si+video]
         shr al,1
         jnc .l1

         add di,2000h
.l1:     mov cl,80
         mul cl
         add di,ax
         mov bl,[si+bx]
         shl bx,1
         mov ax,[bx+vistab]
         stosw
         retn

clrectpc:mov dh,[crsrbyte]
         cmp [ydir],0
         je .c3

         sub dh,8
         not dh
.c3:     mov dl,[y8poscp]
         add dh,dl
         shr dh,1
         shr dh,1
         shr dh,1
         inc dh
         mov si,[crsrtile]
         cmp [ydir],0
         jne loopuppc

loopdnpc:call xclrectpc
         je exitclrectpc

         mov si,[si+down]
         jmp loopdnpc

loopuppc:call xclrectpc
         je exitclrectpc

         mov si,[si+up]
         jmp loopuppc

xclrectpc: push si
         cmp [xdir],0
         jne .c2

.c1:     call clrect1pc
         mov si,[si+right]
         dec dl
         jnz .c1
         jmp .c3

.c2:     call clrect1pc
         mov si,[si+left]
         dec dl
         jnz .c2
.c3:     pop si
         mov dl,[x8poscp]
         dec dh
exitclrectpc: retn

clrect1pc: push si
         push dx
         call showscnp1
         pop dx
         pop si
         retn
  endif

crsrset1:
;;         mov @#crsrtile,r0
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
         movepenq 0,8
         color 2
         print 'SET DIRECTORY MASK ('
         color 1
         print 'ENTER'
         color 2
         print ' = ?#)'
         color 3
         movepenq 0,16
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
         beq .c13

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
         moveq #16,d1
         moveq #0,d4
         bsr TXT_ON_CURSOR
         addq #8,d3
         moveq #1,d0
         jsr Text(a6)
.cont4:  bsr TXT_PLACE_CURSOR
         bra .c1

.c11:    tst.b d3
         bne .c5

         move.b #"#",(a4)+
         move.b #"?",(a4)+
.c5:     move.b #".",(a4)+
         move.b #"8",(a4)+
         move.b #"x",(a4)+
         move.b #"l",(a4)+
         clr.b (a4)
.c13:    rts

.c12:    subq.l #1,a4
         subq.b #8,d3
         bcs .c3

         moveq #16,d1
         moveq #8,d0
         bsr TXT_REMOVE_CURSOR
         bra .cont4
   if 0
setviewport:
;         ld hl,(crsrtile)
;         ld (viewport),hl
;         ld ix,vptilecx
;;        mov #viewport,r3
;;        mov @#crsrtile,@r3
;;        mov #vptilecx,r0
;;        movb @#crsry,r1
        mov di,viewport
        mov ax,[crsrtile]
        mov [di],ax
        mov si,vptilecx
        mov al,[crsry]

;         ld a,2
;         ld (vptilecx),a
;         dec a
;         ld (vptilecy),a
;;        mov #258,@r0      ;$102
       mov word [si],102h

;         ld hl,(ycrsr)
;         ld a,l
;         or h
;         jr nz,cont1

;         ld a,(ycrsr+2)
;         cp 8
;         jr nc,cont1
;;        cmpb r1,#8
;;        bcc 1$
        cmp al,8
        jnc .c1

;;        decb @#vptilecy
;;        add #tilesize*hormax,@r3  ;up
;;        br 2$
        dec [vptilecy]
        add word [di],tilesize*hormax
        jmp .c2

;cont1    ld a,(ycrsr)
;         dec a
;         jr nz,cont2

;         ld a,(ycrsr+1)
;         cp 8
;         jr c,cont2
;         jr nz,cont4

;         ld a,(ycrsr+2)
;         cp 4
;         jr c,cont2
;;1$:     cmpb r1,#184
;;        bcs 2$

;;        incb @#vptilecy     ;down
;;        sub #tilesize*hormax,@r3
.c1:    cmp al,vermax*8-8    ;184
        jc .c2

        inc [vptilecy]
        sub word [di],tilesize*hormax

;cont2    ld hl,(xcrsr)
;         ld a,l
;         or h
;         jr nz,cont3

;         ld a,(xcrsr+2)
;         cp 8
;         jr nc,cont3
;;2$:     movb @#crsrx,r1
;;        cmpb r1,#8
;;        bcc 3$
.c2:    mov al,[crsrx]
        cmp al,8
        jnc .c3

;         dec (ix)
;         dec (ix)
;         ld hl,(viewport)      ;left2
;         ld de,tilesize*2
;         add hl,de
;         ld (viewport),hl
;         jr cont5
;;        decb @r0
;;        decb @r0
;;        add #tilesize*2,@r3
;;        br 5$
        sub byte [si],2
        add word [di],tilesize*2
        jmp .c5

;cont3    ld a,(xcrsr)
;         or a
;         jr nz,cont6

;         ld a,(xcrsr+1)
;         cp 1
;         jr c,cont7
;         jr nz,cont6

;         ld a,(xcrsr+2)
;         cp 6
;         jr nc,cont6
;;3$:     cmpb r1,#16
;;        bcc 6$
.c3:    cmp al,16
        jnc .c6

;cont7    dec (ix)
;         ld hl,(viewport)      ;left1
;         ld de,tilesize
;         add hl,de
;         ld (viewport),hl
;         jr cont5
;;        decb @r0
;;        add #tilesize,@r3
;;        br 5$
        dec byte [si]
        add word [di],tilesize
        jmp .c5

;cont6    ld a,(xcrsr)
;         dec a
;         jr nz,cont8

;         ld a,(xcrsr+1)
;         cp 5
;         jr nz,cont8

;         ld a,(xcrsr+2)
;         cp 2
;         jr c,cont8
;;6$:     cmpb r1,#152
;;        bcs 8$
.c6:    cmp al,hormax*8-8  ;152
        jc .c8

;         inc (ix)
;         inc (ix)
;         ld hl,(viewport)      ;right2
;         ld de,(~(tilesize*2))+1
;         add hl,de
;         ld (viewport),hl
;         jr cont5
;;        incb @r0
;;        incb @r0
;;        sub #tilesize*2,@r3
;;        br 5$
        add byte [si],2
        sub word [di],tilesize*2
        jmp .c5

;cont8    ld a,(xcrsr)
;         dec a
;         jr nz,cont5

;         ld a,(xcrsr+1)
;         cp 4
;         jr c,cont5
;         jr nz,cont10

;         ld a,(xcrsr+2)
;         cp 4
;         jr c,cont5
;;8$:     cmpb r1,#144
;;        bcs 5$
.c8:    cmp al,hormax*8-16   ;144
        jc .c5

;cont10   inc (ix)
;         ld hl,(viewport)      ;right1
;         ld de,(~tilesize)+1
;         add hl,de
;         ld (viewport),hl
;;        incb @r0
;;        sub #tilesize,@r3
        inc byte [si]
        sub word [di],tilesize

;cont5    ld iy,(viewport)
;         ld hl,fixvp
;         call calllo
;         ld (viewport),hl
;;5$:     mov @r3,r4
;;        mov ul(r4),r4
;;        mov left(r4),@r3
.c5:    mov bx,[di]
        mov bx,[bx+ul]
        mov ax,[bx+left]
        mov [di],ax

;         ld b,3
;loop12   sla (ix)
;         sla (ix+1)
;         djnz loop12
;;        asl @r0
;;        asl @r0
;;        asl @r0
        mov cl,3
        shl word [si],cl

;         ld a,(crsrbyte)
;         add a,(ix+1)
;         ld (ix+1),a
;;        movb @#crsrbyte,r1
;;        swab r1
;;        add r1,@r0    ;vptilecy
;;        call @#calcx
;;        add r1,@r0
;;        return
        call calcx
        mov ah,[crsrbyte]
        add [si],ax
        retn
   endif

crsrset: bsr crsrset1
         tst.b zoom(a3)
         bne gexit2

pixel11: movea.l BITPLANE1_PTR(a3),a0   ;it should be after crsrset, IN: d1 - crsrbit, d0 - addr of video tile line
         or.b d1,(a0,d0)
         movea.l BITPLANE2_PTR(a3),a0
         or.b d1,(a0,d0)
gexit2:  rts         ;this is also gexit3

   if 0
pixel11p:push si
         push bx
         call pixel11
         pop bx
         pop si
         retn
    endif

crsrclr: tst.b zoom(a3)
         bne gexit2

         movea.l crsrtile(a3),a4
         moveq #0,d4
         move.b crsrbyte(a3),d4
         move.b (a4,d4.w),d0
         movea.l BITPLANE1_PTR(a3),a0
         movea.l BITPLANE2_PTR(a3),a1
         move.w d4,d6
         mulu #nextline,d4
         add.l (video,a4),d4

         tst.b pseudoc(a3)
         bne .c2

         move.b #0,(a1,d4)
         move.b d0,(a0,d4)
         rts

.c2:     lsl.w #2,d6
         move.l (count0,a4,d6.w),d1
         vidmacp
         move.b d1,(a0,d4)
         move.b d0,(a1,d4)
         rts

crsrcalc:
         ;;mov bx,[crsrtile]
         movea.l crsrtile(a3),a1
         ;;mov bx,[bx+video]
         move.l (video,a1),d1
         subq.w #4,d1   ;(40-hormax)/2
         divu #40,d1
         ;move.b d1,crsry(a3)
         move.w d1,d0
         add.b crsrbyte(a3),d0
         move.w #0,d1
         swap d1
         lsl.w #3,d1
         ;move.b d1,crsrx(a3)

         move.b crsrbit(a3),d2
.c10:    add.b d2,d2
         bcs .c8

         addq #1,d1
         bra .c10

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
         ;bne .c18
         rts

.c18:   ;;mov di,up
        ;;mov al,[vptilecy]
        ;;mov ah,al
        ;;add al,8
        ;;or ah,ah
        ;;js .c33

        ;;mov di,down
        ;;sub al,16
        ;;cmp ah,24
        ;;jc .c34

.c33:   ;;mov [vptilecy],al
        ;;jmp .c31

.c34:   ;;mov di,left
        ;;mov al,[vptilecx]
        ;;mov ah,al
        ;;add al,8
        ;;or ah,ah
        ;;js .c35

        ;;mov di,right
        ;;sub al,16
        ;;cmp ah,40
        ;;jc .c30

.c35:   ;;mov [vptilecx],al
.c31:   ;;add di,[viewport]
        ;;mov bx,[di]
        ;;mov [viewport],bx
        ;;mov di,[bx+dr]
        ;;mov di,[di+dr]
        ;;mov di,[di+right]
        ;;mov di,[di+right]
        ;;add bx,44*tilesize
        ;;cmp bx,di
        ;;jz .c30

        ;;call setviewport
.c30:   ;;call showscnz
        ;;mov ah,2
        ;;xor bh,bh
        ;;mov dx,word [vptilecx]
        ;;int 10h
        ;;retn
        rts
    if 0
outdec:  xor dx,dx            ;in: ax
         call todec
         mov si,stringbuf
         xor cx,cx
         mov cl,[si]
         add si,cx
         mov ah,2
.loop:   cmp si,stringbuf
         jz .exit

         mov dl,[si]
         dec si
         int 21h
         jmp .loop

.exit:   retn

infov:   call totext
         cmp [fn],0
         je .c11

         call printstr
         db 'Last loaded filename: $'

         mov si,fn
.c1:     lodsb
         cmp al,'.'
         jz .c11

         mov dl,al
         mov ah,2
         int 21h
         jmp .c1

.c11:    call boxsz
         je .c12

         push dx
         push cx
         push ax
         call printstr
         db 0dh,10,'Active pattern size: $'

         pop ax
         xor ah,ah
         call outdec
         mov dl,'x'
         int 21h

         xor ax,ax
         mov al,[boxsz_cury]
         call outdec
         call printstr
         db 0dh,10,'Box life bounds: $'

         xor ax,ax
         mov al,[boxsz_xmin]
         call outdec
         call printstr
         db '<=x<=$'

         pop cx
         xor ax,ax
         mov al,ch
         call outdec
         mov dl,' '
         int 21h

         xor ax,ax
         mov al,[boxsz_ymin]
         call outdec
         call printstr
         db '<=y<=$'

         pop ax
         xor ah,ah
         call outdec
.c12:    call printstr
         db 0dh,10,'Rules: $'

         call showrules2
         call curoff
         call getkey
         jmp tograph

outinnum:mov cl,10
         xor ah,ah
         div cl
         mov dx,ax
         mov ah,2
         or al,al
         jz .l1

         or dl,'0'
         int 21h
.l1:     mov dl,dh
         or dl,'0'
         int 21h
         call printstr
         db purple,']: ',black,'$'  ;must be followed by inputdec

inputdec:mov si,stringbuf  ;in: ch - limit hi
         xor cl,cl
.c1:     call getkey
         cmp al,0dh
         je .c11

         cmp al,8    ;backspace
         je .c12

         cmp al,'0'+10
         jnc .c1

         cmp al,'0'
         jc .c1

         cmp cl,2
         je .c1

         inc cx
         mov dl,al
         mov ah,2
         int 21h

         sub al,'0'
         mov [si],al
         inc si
         jmp .c1

.c12:    dec si
         dec cl
         js inputdec

         call delchr
         jmp .c1

.c11:    or cl,cl
         je .exit

         mov di,stringbuf
         xor ax,ax
         cmp cl,1
         je .c16

         mov al,[di]
         inc di
         mov ah,10
         mul ah
.c16:    add al,[di]
         cmp al,ch
         jnc .c1
.exit:   retn          ;should proper set CF

chgcolors:
         mov ax,3
         int 10h
         call printstr
         db green,'PRESS ',red,'ENTER',green
         db ' TO USE THE DEFAULT VALUE OR INPUT THE DECIMAL NUMBER.',0dh,10,purple
         db 'PALETTE# FOR ZOOM OUT MODE (0-1)[',cyan,'$'
         mov al,[palette]
         mov ch,2
         call outinnum
         jnc .l1

         mov [palette],al
.l1:     call printstr
         db 0dh,10,purple,'THE ZOOM OUT GO BACKGROUND (0-31)[',cyan,'$'
         mov al,[bgr]
         mov ch,32
         call outinnum
         jnc .l2

         mov [bgr],al
.l2:     call printstr
         db 0dh,10,purple,'THE ZOOM OUT EDIT BACKGROUND (0-31)[',cyan,'$'
         mov al,[bgs]
         mov ch,32
         call outinnum
         jnc .l3

         mov [bgs],al
.l3:     call printstr
         db 0dh,10,purple,'THE ZOOM IN GO BACKGROUND (0-7)[',cyan,'$'
         mov al,[zbgr]
         mov cx,804h
         shr al,cl
         call outinnum
         jnc .l4

         mov cl,4
         shl al,cl
         mov [zbgr],al
.l4:     call printstr
         db 0dh,10,purple,'THE ZOOM IN EDIT BACKGROUND (0-7)[',cyan,'$'
         mov al,[zbgs]
         mov cx,804h
         shr al,cl
         call outinnum
         jnc .l5

         mov cl,4
         shl al,cl
         mov [zbgs],al
.l5:     call printstr
         db 0dh,10,purple,'THE ZOOM IN LIVE CELL (0-15)[',cyan,'$'
         mov al,[zfg]
         mov ch,16
         call outinnum
         jnc .l6

         mov [zfg],al
.l6:     call printstr
         db 0dh,10,purple,'THE ZOOM IN NEW CELL (0-15)[',cyan,'$'
         mov al,[zfgnc]
         mov ch,16
         call outinnum
         jnc .l7

         mov [zfgnc],al
.l7:     call printstr
         db 0dh,10,'TO SAVE THIS CONFIG?$'
.l8:     call getkey
         or al,32
         cmp al,'n'
         je putpixel2.e1

         cmp al,'y'
         jne .l8
         jmp savecf
   endif
putpixel2:
         ;;mov di,[di+video]
         ;;mov bl,dl
         ;;shl bx,1
         ;;mov bx,[bx+vistab]
         ;;shr cl,1
         ;;jnc .l1

         ;;add di,2000h
.l1:     ;;mov al,80
         ;;mul cl
         ;;add di,ax

         ;;mov ax,bx
         ;;not ax
         ;;and [es:di],ax
         ;;shl bx,1
         ;;or [es:di],bx
.e1:     rts
   if 0
showtent:mov ax,word [x0]
         push ax
         mov [ppmode],0
         mov bp,[tsz]
         xor si,si
.loop:   or bp,bp
         je .fin

         push es
         mov es,[iobseg]
         lods word [es:si]
         pop es
         dec bp
         mov word [x0],ax
         call putpixel
         jmp .loop

.fin:    pop ax
         mov word [x0],ax
         inc [ppmode]
         retn
  endif

clrscn:  movea.l BITPLANE1_PTR(a3),a0
	 movea.l BITPLANE2_PTR(a3),a2
	 moveq #0,d0
	 move.w #nextline*50-1,d1
.l1:	 move.l d0,(a0)+
	 move.l d0,(a2)+
	 dbra d1,.l1
	 rts


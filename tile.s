clear:    bsr zerocc
          bsr zerogc
          ;;mov si,[startp]
          movea.l startp(a3),a4

.c10:	 tst.b (sum,a4)
	 beq .c11

          ;;mov [si+sum],al
          clr.b (sum,a4)
          ;;mov [si],ax
          ;;mov [si+2],ax
          clr.l (a4)
          ;;mov [si+4],ax
          ;;mov [si+6],ax
          clr.l (4,a4)

.c11:
;;         mov next(r0),r0
	 ;;mov si,[si+next]
	 movea.l (next,a4),a4
;;          cmp r0,#1
         ;;cmp si,1
	 cmpa.l #1,a4
;;          bne 10$
         ;;jnz .c10
         bne .c10

         ;;call showscn
         bsr showscn
         ;;call cleanup0
         bsr cleanup0
	 ;;jmp infoout
         bra infoout

chkaddt:
	 ;;or cx,cx
         tst.w d2
         ;;jz exit2
         beq.s exit2

chkadd:
	 ;;cmp word [di+next],0    ;in: di
         tst.l (next,a5)
         ;;jnz exit2
         bne.s exit2

addnode:
         ;;mov ax,[startp]
         ;;mov [di+next],ax
         move.l startp(a3),(next,a5)

         ;;mov [startp],di
         move.l a5,startp(a3)

         ;;inc [tilecnt]
         addq.w #1,tilecnt(a3)
exit2:
	 rts

chkadd2: ;;cmp word [ds:bp+next],0
         tst.l (next,a6)
         bne exit2

addnode2:                 ;in: A6
         ;;mov ax,[startp]
         ;;mov [ds:bp+next],ax
	 move.l startp(a3),(next,a6)

         ;;mov [startp],bp
         move.l a6,startp(a3)

         ;;inc [tilecnt]
         addq.w #1,tilecnt(a3)

exit:
	 ;;retn 
	 rts

torus:
;;         mov #tiles,r0
        ;;mov si,tiles       ;top border
        lea tiles(a3),a0
;;         mov #hormax,r1
        ;;mov cx,hormax
        move.w #hormax-1,d0

.c5:
;;5$:      mov r0,r2
;;         add #<hormax*<vermax-1>-1>*tilesize,r2
;;         lea ax,[si+(hormax*(vermax-1)-1)*tilesize]
         move.l a0,a1
         adda.l #(hormax*(vermax-1)-1)*tilesize,a1
;;         mov r2,ul(r0)
         ;;mov [si+ul],ax
         move.l a1,(ul,a0)

;;         mov r0,r2
;;         add #hormax*<vermax-1>*tilesize,r2
         ;;lea ax,[si+hormax*(vermax-1)*tilesize]
         move.l a0,a1
         adda.l #hormax*(vermax-1)*tilesize,a1
;;         mov r2,up(r0)
         ;;mov [si+up],ax
         move.l a1,(up,a0)

;;         mov r0,r2
;;         add #<hormax*<vermax-1>+1>*tilesize,r2
         ;;lea ax,[si+(hormax*(vermax-1)+1)*tilesize]
         move.l a0,a1
         adda.l #(hormax*(vermax-1)+1)*tilesize,a1
;;         mov r2,ur(r0)
         ;;mov [si+ur],ax
	 move.l a1,(ur,a0)

;;         add #tilesize,r0
         ;;add si,tilesize
         adda.l #tilesize,a0
;;         sob r1,5$
         ;;loop .c5
         dbra d0,.c5

;;         mov #tiles+<<vermax-1>*hormax*tilesize>,r0
         ;;mov si,tiles+(vermax-1)*hormax*tilesize
         movea.l #tiles+(vermax-1)*hormax*tilesize,a0
;;         mov #hormax,r1
         ;;mov cx,hormax
         move.w #hormax-1,d0

.c4:
;;4$:      mov r0,r2
         ;;mov ax,si
         movea.l a0,a1
;;         sub #<<vermax-1>*hormax-1>*tilesize,r2
         ;;sub ax,((vermax-1)*hormax-1)*tilesize
         suba.l #((vermax-1)*hormax-1)*tilesize,a1
;;         mov r2,dr(r0)
         ;;mov [si+dr],ax
         move.l a1,(dr,a0)

;;         mov r0,r2
         ;;mov ax,si
         movea.l a0,a1
;;         sub #<vermax-1>*hormax*tilesize,r2
         ;;sub ax,(vermax-1)*hormax*tilesize
         suba.l #(vermax-1)*hormax*tilesize,a1
;;         mov r2,down(r0)
         ;;mov [si+down],ax
         move.l a1,(down,a0)

;;         mov r0,r2
         ;;mov ax,si
         movea.l a0,a1
;;         sub #<<vermax-1>*hormax+1>*tilesize,r2
         ;;sub ax,((vermax-1)*hormax+1)*tilesize
         suba.l #((vermax-1)*hormax+1)*tilesize,a1
;;         mov r2,dl(r0)
         ;;mov [si+dle],ax
         move.l a1,(dl,a0)

;;         add #tilesize,r0
         ;;add si,tilesize
	 adda.l #tilesize,a0
;;         sob r1,4$
         ;;loop .c4
         dbra d0,.c4

;;         mov #tiles,r0
        ;;mov si,tiles
        movea.l #tiles,a0
;;         mov #vermax,r1
        ;;mov cx,vermax
        move.w #vermax-1,d0

.c3:
;;3$:      mov r0,r2
;;         add #<hormax-1>*tilesize,r2
         ;;lea ax,[si+(hormax-1)*tilesize]
         lea ((hormax-1)*tilesize,a0),a1
;;         mov r2,left(r0)
         ;;mov [si+left],ax
	 move.l a1,(left,a0)

;;         mov r0,r2
;;         sub #tilesize,r2
         ;;lea ax,[si-tilesize]
         lea (-tilesize,a0),a1
;;         mov r2,ul(r0)
         ;;mov [si+ul],ax
         move.l a1,(ul,a0)

;;         mov r0,r2
;;         add #<2*hormax-1>*tilesize,r2
         ;;lea ax,[si+(2*hormax-1)*tilesize]
         lea ((2*hormax-1)*tilesize,a0),a1
;;         mov r2,dl(r0)
         ;;mov [si+dle],ax
         move.l a1,(dl,a0)

;;         add #hormax*tilesize,r0
         ;;add si,hormax*tilesize
         adda.l #hormax*tilesize,a0
;;         sob r1,3$
         ;;loop .c3
         dbra d0,.c3

;;         mov #tiles+<<hormax-1>*tilesize>,r0
         ;;mov si,tiles+(hormax-1)*tilesize
         movea.l #tiles+(hormax-1)*tilesize,a0
;;         mov #vermax,r1
         ;;mov cx,vermax
         move.w #vermax-1,d0

.c2:
;;2$:      mov r0,r2
;;         sub #<2*hormax-1>*tilesize,r2
         ;;lea ax,[si-(2*hormax-1)*tilesize]
         lea (-(2*hormax-1)*tilesize,a0),a1
;;         mov r2,ur(r0)
         ;;mov [si+ur],ax
         move.l a1,(ur,a0)

;;         mov r0,r2
;;         sub #<hormax-1>*tilesize,r2
         ;;lea ax,[si-(hormax-1)*tilesize]
         lea.l (-(hormax-1)*tilesize,a0),a1
;;         mov r2,right(r0)
         ;;mov [si+right],ax
         move.l a1,(right,a0)

;;         mov r0,r2
;;         add #tilesize,r2
         ;;lea ax,[si+tilesize]
         lea (tilesize,a0),a1
;;         mov r2,dr(r0)
         ;;mov [si+dr],ax
         move.l a1,(dr,a0)

;;         add #hormax*tilesize,r0
         ;;add si,hormax*tilesize
         adda.l #hormax*tilesize,a0
;;         sob r1,2$
         ;;loop .c2
         dbra d0,.c2

;;         mov #tiles + <<hormax*vermax-1>*tilesize>,@#tiles+ul
         ;;mov word [tiles+ul],tiles + (hormax*vermax-1)*tilesize
         move.l #tiles+(hormax*vermax-1)*tilesize,tiles+ul(a3)
;;         mov #tiles + <<hormax*<vermax-1>>*tilesize>,@#tiles+ur+<<hormax-1>*tilesize>
         ;;mov word [tiles+ur+(hormax-1)*tilesize],tiles + hormax*(vermax-1)*tilesize
         move.l #tiles+hormax*(vermax-1)*tilesize,tiles+ur+(hormax-1)*tilesize(a3)
;;         mov #tiles+<<hormax-1>*tilesize>,@#tiles+dl+<hormax*<vermax-1>*tilesize>
	 ;;mov word [tiles+dle+hormax*(vermax-1)*tilesize],tiles+(hormax-1)*tilesize
	 move.l #tiles+(hormax-1)*tilesize,tiles+dl+hormax*(vermax-1)*tilesize
;;         mov #tiles,@#tiles+dr+<<vermax*hormax-1>*tilesize>
         ;;mov word [tiles+dr+(vermax*hormax-1)*tilesize],tiles
	 move.l #tiles,tiles+dr+(vermax*hormax-1)*tilesize
;;         return
         ;;retn
         rts

plain:
;;         mov #tiles,r0
         ;;mov si,tiles
         move.l #tiles,a0
;;         mov #hormax,r1
         ;;mov cx,hormax
         move.w #hormax-1,d0
;;         mov #plainbox,r2
         ;;mov ax,plainbox
         move.l #plainbox,a1

.c5:
;;       mov r2,ul(r0)
         ;;mov [si+ul],ax
         move.l a1,(ul,a0)
;;         mov r2,up(r0)
         ;;mov [si+up],ax
         move.l a1,(up,a0)
;;         mov r2,ur(r0)
         ;;mov [si+ur],ax
         move.l a1,(ur,a0)
;;         add #tilesize,r0
         ;;add si,tilesize
         adda.l #tilesize,a0
;;         sob r1,5$
         ;;loop .c5
         dbra d0,.c5

;;         mov #tiles+<<vermax-1>*hormax*tilesize>,r0
         ;;mov si,tiles+(vermax-1)*hormax*tilesize
         move.l #tiles+(vermax-1)*hormax*tilesize,a0
;;         mov #hormax,r1
         ;;mov cx,hormax
         move.w #hormax-1,d0

.c4:
;;         mov r2,dr(r0)
         ;;mov [si+dr],ax
         move.l a1,(dr,a0)
;;         mov r2,down(r0)
         ;;mov [si+down],ax
         move.l a1,(down,a0)
;;         mov r2,dl(r0)
         ;;mov [si+dle],ax
         move.l a1,(dl,a0)
;;         add #tilesize,r0
         ;;add si,tilesize
         adda.l #tilesize,a0
;;         sob r1,4$
         ;;loop .c4
	 dbra d0,.c4

;;         mov #tiles,r0
         ;;mov si,tiles
         move.l #tiles,a0
;;         mov #vermax,r1
         ;;mov cx,vermax
         move.w #vermax-1,d0

.c3:
;;         mov r2,left(r0)
         ;;mov [si+left],ax
         move.l a1,(left,a0)
;;         mov r2,ul(r0)
         ;;mov [si+ul],ax
         move.l a1,(ul,a0)
;;         mov r2,dl(r0)
         ;;mov [si+dle],ax
         move.l a1,(dl,a0)
;;         add #tilesize*hormax,r0
         ;;add si,tilesize*hormax
         adda.l #tilesize*hormax,a0
;;         sob r1,3$
         ;;loop .c3
	 dbra d0,.c3

;;         mov #tiles+<<hormax-1>*tilesize>,r0
         ;;mov si,tiles+(hormax-1)*tilesize
	 move.l #tiles+(hormax-1)*tilesize,a0
;;         mov #vermax,r1
         ;;mov cx,vermax
         move.w #vermax-1,d0

.c2:
;;         mov r2,ur(r0)
         ;;mov [si+ur],ax
         move.l a1,(ur,a0)
;;         mov r2,right(r0)
         ;;mov [si+right],ax
         move.l a1,(right,a0)
;;         mov r2,dr(r0)
         ;;mov [si+dr],ax
         move.l a1,(dr,a0)
;;         add #tilesize*hormax,r0
         ;;add si,tilesize*hormax
         adda.l #tilesize*hormax,a0
;;         sob r1,2$
         ;;loop .c2
	 dbra d0,.c2
;;         return
         ;;retn
         rts

random:  bsr randomize
         move.l #$fc0000,a0
         move d5,d4
         lea bittab(a3),a1

         ;;xor bp,bp   ;dir: 0 - left, 1 - right
         moveq #0,d6
         ;;mov di,tiles+(hormax*4+3)*tilesize
         lea tiles+(hormax*4+3)*tilesize(a3),a5
         ;;mov dx,(vermax-8)*256+hormax-6    ;dh - ver rnd max, dl - hor rnd max
         move.b #vermax-8,d7
         move.b #hormax-6,d3
         ;;mov bx,right
         moveq #right,d1

.cont3:  
	 ;;mov word [di+sum],1
         move.b #1,(sum,a5)
         ;;mov cx,8
         move.w #7,d2

.loop1:  
	 ;;call rndbyte
         bsr rndbyte
         ;;loop .loop1
         dbra d2,.loop1
         ;;sub di,8
         suba #8,a5

         bsr chkadd
         ;;dec dl
         subq.b #1,d3
         ;;jz .cont2
         beq .cont2

.cont4:  
	 ;;mov di,[di+bx]
         movea.l (a5,d1),a5
         ;;jmp .cont3
         bra .cont3

.cont2:  
	 ;;dec dh
         subq.b #1,d7 
         ;;jz calccells
         beq calccells

         ;;mov bl,left
         moveq #left,d1
         ;;mov dl,hormax-6       ;hor rnd max
         move.b #hormax-6,d3
         ;;xor bp,1
         eori.b #1,d6
         ;;jnz .cont1
         bne .cont1

        ;;mov bl,right
        moveq #right,d1
.cont1: 
	;;mov di,[di+down]
        movea.l (down,a5),a5
        ;;jmp .cont3
        bra .cont3

calccells: bsr zerocc
         tst.w tilecnt(a3)
         bne.s .c12
         rts
.c12:
         ;;mov si,[startp]
         movea.l startp(a3),a0
.c2:
         ;;mov cx,8
         moveq #7,d2
         ;;xor ax,ax
         moveq #0,d0
         lea.l tab3(a3),a2
.c4:
         ;;lodsb
         move.b (a0)+,d0
         ;;or al,al
         ;;jz .c5
         beq.s .c5

         ;;mov bx,tab3
         ;;xlatb
         move.b (a2,d0),d0
         ;;call inctsum
         bsr.s inctsum
         ;;mov ah,cl
.c5:
         ;;loop .c4
         dbra d2,.c4

         ;;mov [si+sum-8],ah
         move.b d2,(sum-8,a0)
         ;;mov si,[si+next-8]
         movea.l (next-8,a0),a0
         ;;cmp si,1
         cmpa.l #1,a0
         ;;jnz .c2
         bne.s .c2
         ;;jmp infoout
         bra infoout

inctsum:            ;in: d0
         cellsum
	 rts


putpixel:     ;IN: x0,y0; DON'T USE: D1
         bsr xchgxy
         ;;mov dx,word [x0]
         move.b x0(a3),d3
         move.b y0(a3),d4
         bsr calcx

         ;;mov cl,dl
         move.b d3,d5
         ;;or al,[crsrx]
         or.b crsrx(a3),d0
         ;;cmp [xdir],0
         tst.b xdir(a3)
         beq .c4

         ;;cmp al,cl
         cmp.b d5,d0
         bcs .c100

         ;sub al,cl
         sub.b d5,d0
         bra .c2

.c4:     ;;add al,cl
         add.b d5,d0
         ;;cmp al,cl
         cmp.b d5,d0
         bcs .c100
  if hormax<>32
         ;;cmp al,hormax*8
         cmpi.b #hormax*8,d0
         bcc .c100
  endif

.c2:     ;;mov ch,[crsry]
         clr.l d6
         move.b crsry(a3),d6
         ;add ch,[crsrbyte]
         add.b crsrbyte(a3),d6
         ;;mov cl,dh
         move.b d4,d5
         ;;cmp [ydir],0
         tst.b ydir(a3)
         beq .c3

         ;;cmp ch,cl
         cmp.b d5,d6
         bcs .c100

         ;;sub ch,cl
         sub.b d5,d6
         bra .c1

.c3:     ;;add ch,cl
         add.b d5,d6
         ;;cmp ch,cl
         cmp.b d5,d6
         bcs .c100

         ;;cmp ch,vermax*8
         cmpi.b #vermax*8,d6
         bcs .c1
.c100:   rts

.c1:     ;;xor cl,cl
         ;;xchg cl,ch
         clr.l d5
         ;;xor dh,dh
         clr.l d4
         ;;mov dl,[crsry]
         ;;sub cx,dx
         sub.b crsry(a3),d6
         subx.b d4,d5
         ;;xor ah,ah
         clr d7
         ;;mov dl,[crsrx]
         ;;sub ax,dx
         sub.b crsrx(a3),d0
         subx.b d4,d7
         ;;mov di,[crsrtile]     ;for chkadd
         move.l crsrtile(a3),a5
.c22:    ;;test cx,0fff8h
         tst.b d5
         ;;js .cup
         bmi .cup
         ;;jne .cdown
         bne .cdown

         move d6,d2
         andi.b #$f8,d2
         bne .cdown

.c23:    ;;test ax,0fff8h
         tst.b d7
         ;;js .cleft
         bmi .cleft
         ;;jne .cright
         bne .cright

         move d0,d2
         andi.b #$f8,d2
         bne .cright

         ;;mov bx,7
         ;;sub bl,al
         neg.b d0
         addq.b #7,d0
         ext.w d0
         lea.l bittab(a3),a1
         ;;mov dl,[bittab+bx]
         move.b (a1,d0.w),d3
         ;;;and ch,7
         ;;cmp [ppmode],bh
         tst.b ppmode(a3)
         bne putpixel3
         bra putpixel2

.cright: ;;mov di,[di+right]   ;y=0, x=/=0
         move.l right(a5),a5
         ;;sub ax,8
         subq.b #8,d0
         subx.b d4,d7
         bra .c23

.cdown:  ;;mov di,[di+down]   ;y=/=0
         move.l down(a5),a5
         ;;sub cx,8
         subq.b #8,d6
         subx.b d4,d5
         bra .c22

.cup:    ;;mov di,[di+up]   ;y=/=0
         move.l up(a5),a5
         ;;add cx,8
         addq.b #8,d6
         addx.b d4,d5
         bra .c22

.cleft:  ;;mov di,[di+left]   ;y=0, x=/=0
         move.l left(a5),a5
         ;;add ax,8
         addq.b #8,d0
         addx.b d4,d7
         bra .c23

putpixel3:
         ;;mov bl,cl
         ;;or [di+bx],dl
         or.b d3,(a5,d6)
         bra chkadd

mousecursor:   ;IN: d0 - Y, d2 - X
         moveq #7,d3
         move.w d0,d1
         lsr.w #3,d0
         and.w d3,d1
         move.b d1,crsrbyte(a3)
         move.w d2,d1
         lsr.w #3,d2
         and.w d3,d1
         mulu #hormax,d0
         add.w d2,d0
         mulu #tilesize,d0
         add.l #tiles,d0
         move.l d0,crsrtile(a3)
         sub.b d1,d3
         move.b bittab(a3,d3),crsrbit(a3)
         rts

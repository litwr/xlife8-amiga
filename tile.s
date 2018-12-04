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
          clr.l (a4)+
          ;;mov [si+4],ax
          ;;mov [si+6],ax
          clr.l (a4)

.c11:
;;         mov next(r0),r0
	 ;;mov si,[si+next]
	 movea.l (next-4,a4),a4
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
         beq exit2

chkadd:
	 ;;cmp word [di+next],0    ;in: di
         tst.l (next,a5)
         ;;jnz exit2
         bne exit2

addnode:
         ;;mov ax,[startp]
         ;;mov [di+next],ax
         move.l startp(a3),(next,a5)

         ;;mov [startp],di
         move.l a5,startp(a3)

         ;;inc [tilecnt]
         addq.w #1,tilecnt(a3)
exit2:   
	 ;;retn
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

    if 0
random:
;;;uses: adjcell:2 - r2, i1:2 - r3/r5, i2 - r4, t1 - r1
;;         clr r1   ;dir: 0 - left, 1 - right
;;         mov #tiles+<<hormax*4+3>*tilesize>,r2
;;         mov #16,r3    ;ver rnd max
;;         mov #right,r5
;;         mov #14,r4    ;hor rnd max
         in al,61h
         or al,1
         out 61h,al         ;enable timer 2 gate
         MOV     AL,94H          ;SET TIMER 2 HARDWARE
         OUT     43H,AL
         mov     al,251
         OUT     42H,AL

         xor bp,bp   ;dir: 0 - left, 1 - right
         mov di,tiles+(hormax*4+3)*tilesize
         mov dx,(vermax-8)*256+hormax-6    ;dh - ver rnd max, dl - hor rnd max
         mov bx,right


;;;cont3    ldy #sum
;;;         sta (adjcell),y
;;;         lda #8
;;;         sta t3
;;23$:     mov #1,sum(r2)
;;         mov #8,r0
.cont3:  mov word [di+sum],1
         mov cx,8

;;;loop1    jsr rndbyte
;;;         dec t3
;;;         bne loop1
;;1$:      call @#rndbyte
;;         sob r0,1$
;;         sub #8,r2
.loop1:  call rndbyte
         loop .loop1
         sub di,8

;;;         jsr chkadd
;;;         dec i2
;;;         beq cont2
;;        call @#chkadd
;;        dec r4
;;        beq 22$
         call chkadd
         dec dl
         jz .cont2

;;;         ldy i1+1
;;;cont4    lda (adjcell),y
;;;         tax
;;;         iny
;;;         lda (adjcell),y
;;;         stx adjcell
;;;         sta adjcell+1
;;;         bne cont3
;;         add r5,r2
;;24$:     mov @r2,r2
;;         br 23$
.cont4:  mov di,[di+bx]
         jmp .cont3


;;;cont2    dec i1
;;;         beq cont5
;;22$:     dec r3
;;         beq calccells
.cont2:  dec dh
         jz calccells

;;         mov #14,r4   ;hor rnd max
;;         mov #left,r5
;;         mov #1,r0
;;         xor r0,r1
;;         bne 21$
         mov bl,left
         mov dl,hormax-6       ;hor rnd max
         xor bp,1
         jnz .cont1

;;;         ldy #right
;;;cont1    sty i1+1
;;;         ldy #down
;;;         bne cont4
;;         mov #right,r5
;;21$:     add #down,r2
;;         br 24$
        mov bl,right
.cont1: mov di,[di+down]
        jmp .cont3

calccells: call zerocc
         cmp [tilecnt],0
         jnz .c12
         retn

.c12:    mov si,[startp]
.c2:     mov cx,8
         xor ax,ax
.c4:     lodsb
         or al,al
         jz .c5

         mov bx,tab3
         xlatb
         call inctsum
         mov ah,cl
.c5:     loop .c4
         mov [si+sum-8],ah
         mov si,[si+next-8]
         cmp si,1
         jnz .c2
         jmp infoout

inctsum:            ;in: al
         cellsum .l1
.l1:     retn

putpixel:     ;IN: x0,y0; DON'T USE: SI,BP
         call xchgxy
         mov dx,word [x0]
         call calcx

         mov cl,dl
         or al,[crsrx]
         cmp [xdir],0
         jz .c4

         cmp al,cl
         jc .c100

         sub al,cl
         jmp .c2

.c4:     add al,cl
         cmp al,cl
         jc .c100
if hormax<>32
         cmp al,hormax*8
         jnc .c100
endif
.c2:     mov ch,[crsry]
         add ch,[crsrbyte]
         mov cl,dh
         cmp [ydir],0
         jz .c3

         cmp ch,cl
         jc .c100

         sub ch,cl
         jmp .c1

.c3:     add ch,cl
         cmp ch,cl
         jc .c100

         cmp ch,vermax*8
         jc .c1

.c100:   retn

.c1:     xor cl,cl
         xchg cl,ch
         xor dh,dh
         mov dl,[crsry]
         sub cx,dx
         xor ah,ah
         mov dl,[crsrx]
         sub ax,dx
         mov di,[crsrtile]     ;for chkadd
.c22:    test cx,0fff8h
         js .cup           ;12$
         jne .cdown        ;11$

.c23:    test ax,0fff8h
         js .cleft         ;13$
         jne .cright       ;10$

         mov bx,7
         sub bl,al
         mov dl,[bittab+bx]
         ;and ch,7
         cmp [ppmode],bh
         jne putpixel3
         jmp putpixel2

.cright: mov di,[di+right]   ;y=0, x=/=0
         sub ax,8
         jmp .c23

.cdown:  mov di,[di+down]   ;y=/=0
         sub cx,8
         jmp .c22

.cup:    mov di,[di+up]   ;y=/=0
         add cx,8
         jmp .c22

.cleft:  mov di,[di+left]   ;y=0, x=/=0
         add ax,8
         jmp .c23

putpixel3:
         mov bl,cl
         or [di+bx],dl
         jmp chkadd
  endif


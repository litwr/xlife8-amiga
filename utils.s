;calcspd
;zerocnt
;zerocc
;todec
;makepath
;fn2path

zerocc:   inibcd cellcnt,3
          rts

zerogc:   inibcd gencnt,4
          rts

stuffChar:  move.b  d0,(a3)+        ;Put data to an output string, used by RawDoFmt
            addq.l #1,charCount
            rts

makepath2:
         lea.l curdisk(a3),a0
.loop1:  move.b (a0)+,(a1)+
         bne .loop1

         lea.l curdir1(a3),a0
         subq.l #1,a1
.loop2:  move.b (a0)+,(a1)+
         bne .loop2

         subq.l #1,a1
         rts

makepath:
         lea.l curpath(a3),a1
.sv:     bsr.s makepath2
.loop3:  move.b (a0)+,(a1)+
         bne .loop3
         rts

fn2path: lea.l fn(a3),a0
         subq.l #1,a1
         move.b #'/',(a1)+
.loop:   move.b (a0)+,(a1)+
         bne .loop
         rts

boxsz:   move.b #vermax*8,boxsz_ymin(a3)
         move.b #hormax*8,boxsz_xmin(a3)
         ;xor cx,cx               ;dl=boxsz_ymax, ch=boxsz_xmax
         clr.l d5    ;cl
         clr.l d6    ;ch
         ;xor dx,dx
         clr.l d3    ;dl
         clr.l d4    ;dh
         ;mov [boxsz_curx],cx
         clr.w boxsz_curx(a3)
         ;mov si,tiles
         lea.l tiles(a3),a4
.c0:     ;mov cl,8
         move.b #8,d5
         ;xor bx,bx
         clr.l d1   ;bx, bl
         ;xor ax,ax
         clr.l d0   ;al
         clr.l d7   ;ah
.c9:     ;or ah,[si]
         or.b (a4)+,d7
         ;inc si
         ;dec cl
         subq.b #1,d5
         bne .c9

         subq.l #8,a4
         ;or ah,ah
         tst.b d7
         beq .c17

         ;mov cl,ah
         move.b d7,d5
         ;mov dh,0ffh
         move.b #$ff,d4
.c2:     ;inc dh
         addq.b #1,d4
         ;shl cl,1
         lsl.b d5
         bcc .c2

         ;mov al,[boxsz_curx]
         move.b boxsz_curx(a3),d0
         ;shl al,1
         ;shl al,1
         ;shl al,1
         lsl.b #3,d0
         ;mov bl,al
         move.b d0,d1
         ;add dh,al
         add.b d0,d4
         ;cmp dh,[boxsz_xmin]
         cmp.b boxsz_xmin(a3),d4
         bcc .c12

         ;mov [boxsz_xmin],dh
         move.b d4,boxsz_xmin(a3)
.c12:    ;mov dh,8
         move.b #8,d4
.c3:     ;dec dh
         subq.b #1,d4
         ;shr ah,1
         lsr.b d7
         bcc .c3

         ;add dh,bl
         add.b d1,d4
         ;cmp dh,ch
         cmp.b d6,d4
         bcs .c13

         ;mov ch,dh
         move.b d4,d6
.c13:    ;mov di,si
         movea.l a4,a5
.c4:     ;lodsb
         move.b (a4)+,d0
         ;or al,al
         beq .c4

         ;sub si,di
         suba.l a5,a4
         ;dec si
         subq.l #1,a4
         ;mov al,[boxsz_cury]
         move.b boxsz_cury(a3),d0
         ;shl al,1
         ;shl al,1
         ;shl al,1
         lsl.b #3,d0
         ;mov bl,al
         move.b d0,d1
         ;add ax,si
         add.l a4,d0
         ;cmp al,[boxsz_ymin]
         cmp.b boxsz_ymin(a3),d0
         bcc .c15

         ;mov [boxsz_ymin],al
         move.b d0,boxsz_ymin(a3)
.c15:    ;mov si,di
         movea.l a5,a4
         ;add di,8
         addq.l #8,a5
.c5:     ;dec di
         ;cmp [di],bh
         tst.b -(a5)
         beq .c5

         ;sub di,si
         suba.l a4,a5
         ;add di,bx
         adda.l d1,a5
         ;xor dh,dh
         clr.l d4
         ;cmp di,dx
         cmpa.l d3,a5
         bcs .c17

         ;mov dx,di
         move.l a5,d3
.c17:    ;add si,tilesize
         adda.l #tilesize,a4
         ;inc byte [boxsz_curx]
         addq.b #1,boxsz_curx(a3)
         ;cmp byte [boxsz_curx],hormax
         cmpi.b #hormax,boxsz_curx(a3)
         bne .c0

.c8:     ;mov [boxsz_curx],bh
         clr.b boxsz_curx(a3)
         ;inc byte [boxsz_cury]
         addq.b #1,boxsz_cury(a3)
         ;cmp byte [boxsz_cury],vermax
         cmpi.b #vermax,boxsz_cury(a3)
         bne .c0

.c7:     ;mov bl,dl
         move.b d3,d1
         ;sub bl,[boxsz_ymin]
         sub.b boxsz_ymin(a3),d1
         ;inc bx
         addq.b #1,d1
         ;mov [boxsz_cury],bl
         move.b d1,boxsz_cury(a3)
         ;mov al,ch
         move.b d6,d0
         ;sub al,[boxsz_xmin]
         sub.b boxsz_xmin(a3),d0
         ;inc ax       ;returns xsize in al
         addq.b #1,d0   ;returns xsize in d0
         ;mov [boxsz_curx],al
         move.b d0,boxsz_curx(a3)
         ;mov ah,[tiles]
         move.b tiles(a3),d7
         ;or ah,dl
         or.b d3,d7
         ;or ah,ch  ;ch = boxsz_xmax, dl = boxsz_ymax
         or.b d6,d7 ;d6 = boxsz_xmax, d3 = boxsz_ymax
         rts

randomize:
         clr d5
         move.b $bfea01,d5
         swap d5
         move.b $bfe901,d5
         lsl #8,d5
         move.b $bfe801,d5
         lsl #8,d5
         add.b gencnt+3(a3),d5
         add.b cellcnt+2(a3),d5
         add.w tilecnt(a3),d5
         add.w crsrtick(a3),d5
         move.l d5,d6
         swap d6
         eor.w d6,d5
         swap d5
         clr.w d5
         swap d5
         rts

rndbyte: movem.l d1/d2/d3/d6/d7,-(sp)
         moveq #0,d0
         moveq #7,d6
         moveq #0,d2
         move.b density(a3),d2
         subq.b #1,d2
         moveq #0,d3
.l1:     move.b (a0,d5.l),d0
         add.w d0,d5
         rol.w d2,d5
         rol.w #1,d4
         add.w d4,d5
         add.w d5,d0
         rol.b d4,d0
         andi.w #7,d0
         or.b (a1,d0),d3
         dbra d2,.l1

         or.b d3,(a5)+
         movem.l (sp)+,d1/d2/d3/d6/d7
.rts:    rts

drawline:  ;IN: d7 - x1, d4 - y1, d5 - x2, d6 - y2
         move.w d5,d0
         sub.w d7,d0
         bpl.s .c1

         neg.w d0
.c1:     cmpi.w #2,d0
         bcc.s .c3

         move.w d6,d0
         sub.w d4,d0
         bpl.s .c2

         neg.w d0
.c2:     cmpi.w #2,d0
         bcs.s rndbyte\.rts

.c3:     move.w d7,d2
         add.w d5,d2
         lsr.w d2
         move.w d4,d0
         add.w d6,d0
         lsr.w d0
         movem.w d0/d2/d4,-(sp)
         bsr.s mousepixel
         movem.w (sp)+,d0/d2/d4
         movem.w d0/d2/d5/d6,-(sp)
         move.w d2,d5
         move.w d0,d6
         bsr.s drawline
         movem.w (sp)+,d0/d2/d5/d6
         move.w d2,d7
         move.w d0,d4
         bra.s drawline

mousepixel:   ;IN: d2 - x, d0 - y
         moveq #7,d3
         move.w d0,d1
         lsr.w #3,d0
         and.w d3,d1
         move.w d2,d4
         lsr.w #3,d2
         and.w d3,d4
         mulu #hormax,d0
         add.w d2,d0
         mulu #tilesize,d0
         add.l #tiles,d0
         move.l d0,a5
         sub.b d4,d3
         moveq #0,d0
         move.b bittab(a3,d3),d0
         move.b d0,(sum,a5)
         or.b d0,(a5,d1)
         cmpi.b #mouseleft_char,mouseleft(a3)
         beq chkadd

         eor.b d0,(a5,d1)
         rts

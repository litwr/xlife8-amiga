;calcspd
;zerocnt
;zerocc
;todec
;makepath

zerocc:   inibcd cellcnt,3
          rts

zerogc:   inibcd gencnt,4
          rts

stuffChar:  move.b  d0,(a3)+        ;Put data to an output string, used by RawDoFmt
            rts

makepath: lea curdisk,a0
         lea curpath,a1
.loop1:  move.b (a0)+,(a1)+
         bne .loop1

.loop2:  lea curdir1,a0
         move.b (a0)+,(a1)+
         bne .loop2

.loop3:  move.b (a0)+,(a1)+
         bne .loop3

         move.b d0,(a1)
         rts

  if 0
boxsz:   mov byte [boxsz_ymin],vermax*8
         mov byte [boxsz_xmin],hormax*8
         xor cx,cx               ;dl=boxsz_ymax, ch=boxsz_xmax
         xor dx,dx
         mov [boxsz_curx],cx
         mov si,tiles
.c0:     mov cl,8
         xor bx,bx
         xor ax,ax
.c9:     or ah,[si]
         inc si
         dec cl
         jnz .c9

         sub si,8
         or ah,ah
         je .c17

         mov cl,ah
         mov dh,0ffh
.c2:     inc dh
         shl cl,1
         jnc .c2

         mov al,[boxsz_curx]
         shl al,1
         shl al,1
         shl al,1
         mov bl,al
         add dh,al
         cmp dh,[boxsz_xmin]
         jnc .c12

         mov [boxsz_xmin],dh
.c12:    mov dh,8
.c3:     dec dh
         shr ah,1
         jnc .c3

         add dh,bl
         cmp dh,ch
         jc .c13

         mov ch,dh
.c13:    mov di,si
.c4:     lodsb
         or al,al
         je .c4

         sub si,di
         dec si
         mov al,[boxsz_cury]
         shl al,1
         shl al,1
         shl al,1
         mov bl,al
         add ax,si
         cmp al,[boxsz_ymin]
         jnc .c15

         mov [boxsz_ymin],al
.c15:    mov si,di
         add di,8
.c5:     dec di
         cmp [di],bh
         je .c5

         sub di,si
         add di,bx
         xor dh,dh
         cmp di,dx
         jc .c17

         mov dx,di
.c17:    add si,tilesize
         inc byte [boxsz_curx]
         cmp byte [boxsz_curx],hormax
         ;jne .c0   ;optimize 8088
         je .c8
.c01:    jmp .c0

.c8:     mov [boxsz_curx],bh
         inc byte [boxsz_cury]
         cmp byte [boxsz_cury],vermax
         jne .c01

.c7:     mov bl,dl
         sub bl,[boxsz_ymin]
         inc bx
         mov [boxsz_cury],bl
         mov al,ch
         sub al,[boxsz_xmin]
         inc ax       ;returns xsize in al
         mov [boxsz_curx],al
         mov ah,[tiles]
         or ah,dl
         or ah,ch  ;ch = boxsz_xmax, dl = boxsz_ymax
         retn
  endif

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
         rts


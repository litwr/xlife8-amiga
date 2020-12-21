;*;live, born - word
;*;fillrt
;*;setrconst

fillrt1: 
         ;;mov ax,1
         moveq #1,d0
         ;;or ch,ch
         tst.b d3
         ;;jz .c1
         beq .c1

.c2:     ;;shl ax,1
         lsl.w #1,d0
         ;;dec ch
         subq #1,d3
         ;;jnz .c2
         bne .c2
.c1:     rts

fillrtsl:
         ;;adc ch,dh
         addx.b d5,d3
         bsr fillrt1
         ;;mov [temp],ax
         move.w d0,d6
         ;;or ch,bl
         or.b d2,d3
         rts

fillrtsr:
         ;;adc ch,bh
         clr.b d0
         addx.b d0,d3
         bsr fillrt1
         ;;mov [temp2],ax
         move.w d0,d7 
         rts

fillrt2: 
         ;;jnc .c1
         bcc .c1

         ;;mov ax,[live]
         move.w live(a3),d0
         ;;test [temp],ax
         and.w d6,d0
         ;;jz .c3
         beq .c3

.c2:     ;;shl dl,1
         lsl.b #1,d4
         ;;or [gentab+bx],dl
         or.b d4,(a2,d2)
         ;;shr dl,1
         lsr.b #1,d4
         ;;jnz .c3
         bne .c3

.c1:     ;;mov ax,[born]
         move.w born(a3),d0
         and.w d6,d0
         ;;jnz .c2
         bne .c2

.c3:     ;;or dh,dh
         tst.b d5
         ;;jz .c11
         beq .c11

         ;;mov ax,[live]
         move.w live(a3),d0
         ;;test [temp2],ax
         and.w d7,d0
         ;;jz .c13
         beq .c13

.c12:    ;;or [bx+gentab],dl
         or.b d4,(a2,d2)
         rts

.c11:    ;;mov ax,[born]
         move.w born(a3),d0
         ;;test [temp2],ax
         and d7,d0
         ;;jnz .c12
         bne .c12
.c13:    rts

fillrt:
           ;;xor bx,bx
           clr.l d2
           lea.l gentab(a3),a2
;;.c1:     mov dl,1
;;         mov [bx+gentab],bh
.c1:     moveq #1,d4
         clr.b (a2,d2)

;;         mov dh,bl
         move.b d2,d5
;;         and dh,1
         andi.b #1,d5
;;         xor ch,ch
         clr.b d3
;;         or ch,bl
         or.b d2,d3
;;         mov cl,5
;;         shr ch,cl
         lsr.b #5,d3

;;         clc
         addi.b #0,d0   ;0 -> X
;;         push cx
         move.w d3,-(sp)
         bsr fillrtsl

         ;;and ch,30
         and.b #30,d3
         ;;shr ch,1
         ;;shr ch,1
         lsr.b #2,d3
         ;;pushf
      ;move sr,-(sp)
         clr.w d1           ;d1 is used to keep X flag
         roxr.w #1,d1
         move.w d1,d0
         add.w d0,d0
         bsr fillrtsr

         ;;popf
      ;move (sp)+,ccr
         add.w d1,d1
         bsr fillrt2

         ;;mov dl,4
         moveq #4,d4
         ;;mov dh,bl
         move.b d2,d5
         ;;and dh,8
         andi.b #8,d5
         ;;mov cl,3
         ;;shr dh,cl
         lsr.b #3,d5
         ;;pop cx
         move.w (sp)+,d3
         bsr fillrtsl
         ;;and ch,16
         andi.b #16,d3
         ;;mov cl,4
         ;;shl ch,cl
         lsl.b #4,d3

         ;;pushf
      ;move sr,-(sp)
         roxr #1,d1
         ;;mov ch,bl
         move.b d2,d3
         ;;and ch,7
         andi.b #7,d3
         ;;popf
         ;;pushf
      ;move (sp)+,ccr
      ;move sr,-(sp)
         move.w d1,d0
         add.w d0,d0
         bsr fillrtsr
         ;;popf
         ;;pushf
      ;move (sp)+,ccr
      ;move sr,-(sp)
         move.w d1,d0
         add.w d0,d0

         bsr fillrt2
         ;;mov dl,16
         moveq #16,d4
         ;;popf
     ;move (sp)+,ccr
         add.w d1,d1
         bsr fillrt2
         ;;mov dl,64
         moveq #64,d4
         ;;mov dh,bl
         move.b d2,d5
         ;;and dh,64
         andi.b #64,d5
         ;;shl dh,1
         ;;shl dh,1
         moveq #0,d0
         lsl.b #2,d5
         ;;adc dh,bh
         addx.b d0,d5
         ;;mov ch,bl
         move.b d2,d3
         ;;and ch,56
         andi.b #56,d3
         ;;mov cl,3
         ;;shr ch,cl
         lsr.b #3,d3
         bsr fillrtsl
         ;;shl ch,1
         lsl.b #1,d3
         ;;pushf
     ;move sr,-(sp)
         roxr #1,d1
         ;;mov ch,bl
         move.b d2,d3
         ;;and ch,7
         andi.b #7,d3
         ;;popf
         ;;pushf
     ;move (sp)+,ccr
     ;move sr,-(sp)
         move.w d1,d0
         add.w d0,d0
         bsr fillrtsr
         ;;popf
     ;move (sp)+,ccr
         add.w d1,d1
         bsr fillrt2
         ;;inc bl
         addq.b #1,d2
         ;;jnz .c1
         bne .c1         
.ep1:    rts

;;setrconst:      ;IN: si - string, di - end of string, bp - live/born
setrconst:      ;IN: a4 - end of string, d3 - length*8, a2 - live/born
          clr.w (a2)
.c2:      tst.b d3
          beq fillrt\.ep1
          
          ;;lodsb
          move.b -(a4),d0
          ;;mov dx,1
          moveq #1,d4
          ;;sub al,'0'
          subi.b #'0',d0
          ;;jz .c11
          beq .c11

;;.c1:      shl dx,1
.c1:      lsl.w #1,d4
          ;;dec al
          subq.b #1,d0
          ;;jnz .c1
          bne .c1

;;.c11:     or [bx],dx
.c11:     or.w d4,(a2)
          subq.b #8,d3
          bra .c2

showrules:
         move.l GRAPHICS_BASE(a3),a6 
         movea.l RASTER_PORT(a3),a1
         movepen 20*8,198
         color 2

        ;;mov ah,2
        ;;xor bh,bh
        ;;mov dx,24*256+20
        ;;int 10h
        ;;call printstr
        ;;db "          $"
        ;;mov ah,2
        ;;mov dx,24*256+20
        ;;int 10h

        ;;mov di,live
        lea live(a3),a5
        ;;mov cl,0
        moveq #0,d2

        ;;mov al,1
        moveq #0,d0
.loop1: 
        ;;test al,[di]
        btst.b d0,(1,a5)
        ;;jnz .cont1
        bne .cont1

.loop2: 
        ;;shl al,1
        addq.b #1,d0
        ;;jnz .loop1
        cmpi.b #8,d0
        bne .loop1

        ;;mov al,[di+1]
        move.b (a5),d0
        ;;or al,al
        ;;jz .cont4
        beq .cont4

        ;;mov al,'8'
        move.b #'8',d0
        ;;call .showr1
        bsr .showr1
        ;;jnz .cont4
        bne .cont4
.e1:    
        ;;retn
        rts

.cont4: 
        ;;mov al,'/'
        move.b #'/',d0
        ;;call .showr1
        bsr .showr1
        ;;jz .e1
        beq .e1

        ;;mov al,1
        moveq #0,d0
.loop4: 
        ;;test al,[di+2]
        btst.b d0,(3,a5)
        ;;jnz .cont5
        bne .cont5

.loop5: ;;shl al,1
        addq.b #1,d0
        ;;jnz .loop4
        cmpi.b #8,d0
        bne .loop4

        ;;mov al,[di+3]
        move.b (2,a5),d0
        ;;or al,al
        ;;jz .e1
        beq .e1

        ;;mov al,'8'
        move.b #8,d0
        ;;jmp .showr1
        bra .showr1

.cont5: 
        ;;call .showr0
        bsr .showr0
        ;;jnz .loop5
        bne .loop5
        rts

.cont1: 
        ;;call .showr0
        bsr .showr0
        ;;jnz .loop2
        bne .loop2
        rts

.showr0:
         ;;mov dl,al
         move.b d0,d3
         ;;mov ch,0ffh
         move.b #$ff,d4

.loop3: 
        ;;inc ch
        addq.b #1,d4

        ;;shr al,1
        subq.b #1,d0
        ;;jnc .loop3
        bpl .loop3

        ;;mov al,ch
        move.b d4,d0
        ;;xor al,'0'
        add.b #'0',d0
.showr1:
        ;;cmp cl,10
        cmpi.b #10,d2
        ;;jnz .cont2
        bne .cont2

        ;;mov al,'*'
        move.b #'*',d0
.cont2: 
        ;;mov ah,0eh
        ;;mov bl,2
        ;;int 10h
         movem d2/d3,-(sp)
         move.b d0,stringbuf(a3)
         lea stringbuf(a3),a0
         moveq #1,d0
         ;move.l GRAPHICS_BASE(a3),a6 
         movea.l RASTER_PORT(a3),a1
         jsr Text(a6)
         movem (sp)+,d2/d3

        ;;inc cl
        addq #1,d2
        ;;mov al,dl
        move.b d3,d0
        ;;cmp cl,11
        cmpi.b #11,d2
        ;;retn
        rts

showrules2:
        ;mov si,stringbuf
        lea.l stringbuf(a3),a4
        ;mov dx,si
        move.l a4,d4
        ;mov di,1
        move.w live(a3),d4
        ;xor ax,ax
        clr.l d0
.c1:    ;test [live],di
        lsr.w d4
        ;jz .c2
        bcc .c2

        bsr .c20
.c2:    ;inc ax
        addq.b #1,d0
        ;shl di,1
        tst.w d4
        bne .c1

        ;mov byte [si],'/'
        ;inc si
        move.b #'/',(a4)+
        ;mov di,1
        move.w born(a3),d4
        ;xor ax,ax
        clr.l d0
.c4:    lsr.w d4
        bcc .c5

        bsr .c20
.c5:    addq.b #1,d0
        tst.w d4
        bne .c4

        lea.l stringbuf(a3),a0
        suba.l a0,a4
        move.l a4,d0
        movea.l RASTER_PORT(a3),a1
        jmp Text(a6)

.c20:   ;mov bl,al
        move.b d0,d1
        ;add bl,'0'
        addi.b #'0',d1
        ;mov [si],bl
        ;inc si
        move.b d1,(a4)+
.exit:  rts


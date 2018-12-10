;*;live, born - word
;*;fillrt
;*;setrconst

   if 0
;;fillrt1: mov #1,r4
;;         tstb r3
;;         beq 1$
fillrt1: mov ax,1
         or ch,ch
         jz .c1

;;2$:      asl r4
;;         dec r3
;;         bne 2$
.c2:     shl ax,1
         dec ch
         jnz .c2

;;1$:      return
.c1:     retn

;;fillrtsl: adc r3
;;         add r2,r3
;;         call @#fillrt1
;;         mov r4,@#temp
;;         clr r3
;;         bisb r1,r3
;;         return
fillrtsl:adc ch,dh
         call fillrt1    ;sets ch=0
         mov [temp],ax
         or ch,bl
         retn

;;fillrtsr: adc r3
;;         call @#fillrt1
;;         mov r4,@#temp2
;;         return
fillrtsr:adc ch,bh
         call fillrt1
         mov [temp2],ax
         retn

;;fillrt2: bcc 1$
fillrt2: jnc .c1

;;         mov @#live,r4
;;         bit @#temp,r4
;;         beq 3$
         mov ax,[live]
         test [temp],ax
         jz .c3

;;2$:      asl r0
;;         bisb r0,gentab(r1)
.c2:     shl dl,1
         or [gentab+bx],dl

;;         asr r0
;;         bne 3$
         shr dl,1
         jnz .c3

;;1$:      mov @#born,r4
;;         bit @#temp,r4
;;         bne 2$
.c1:     mov ax,[born]
         test [temp],ax
         jnz .c2

;;3$:      tst r2
;;         beq 11$
.c3:     or dh,dh
         jz .c11

;;         mov @#live,r4
;;         bit r4,@#temp2
;;         beq 13$
         mov ax,[live]
         test [temp2],ax
         jz .c13

;;12$:     bisb r0,gentab(r1)
;;         return
.c12:    or [bx+gentab],dl
         retn

;;11$:     mov @#born,r4
;;         bit @#temp2,r4
;;         bne 12$
;;13$:     return
.c11:    mov ax,[born]
         test [temp2],ax
         jnz .c12
.c13:    retn

;;fillrt:  clr r1          ;XR
fillrt:  xor bx,bx

;;1$:      mov #1,r0       ;AC
;;         clrb gentab(r1)
.c1:     mov dl,1
         mov [bx+gentab],bh

;;         mov r1,r2
;;         bic #65535-1,r2     ;i1
;;         clr r3
;;         bisb r1,r3
;;         asr r3
;;         asr r3
;;         asr r3
;;         asr r3
;;         asr r3
         mov dh,bl
         and dh,1
         xor ch,ch
         or ch,bl
         mov cl,5
         shr ch,cl

;;         clc
;;         push r3
;;         call @#fillrtsl
         clc
         push cx
         call fillrtsl

;;         bic #65535-30,r3
;;         asr r3
;;         asr r3
;;         mfps r5
;;         call @#fillrtsr
         and ch,30
         shr ch,1
         shr ch,1
         pushf
         call fillrtsr

;;         mtps r5
;;         call @#fillrt2
         popf
         call fillrt2

;;         mov #4,r0
;;         mov r1,r2
;;         bic #65535-8,r2
;;         asr r2
;;         asr r2
;;         asr r2
;;         pop r3
         mov dl,4
         mov dh,bl
         and dh,8
         mov cl,3
         shr dh,cl
         pop cx

;;         call @#fillrtsl
;;         bic #65535-16,r3
;;         aslb r3
;;         aslb r3
;;         aslb r3
;;         aslb r3
         call fillrtsl
         and ch,16
         mov cl,4
         shl ch,cl

;;         mfps r5
;;         mov r1,r3
;;         bic #65535-7,r3
;;         call @#fillrtsr
;;         mtps r5
         pushf
         mov ch,bl
         and ch,7
         popf
         pushf
         call fillrtsr
         popf
         pushf

;;         call @#fillrt2
;;         mov #16,r0
;;         mtps r5
;;         call @#fillrt2
;;         mov #64,r0
;;         mov r1,r2
;;         bic #65535-64,r2
         call fillrt2
         mov dl,16
         popf
         call fillrt2
         mov dl,64
         mov dh,bl
         and dh,64

;;         aslb r2
;;         aslb r2
;;         adc r2
;;         mov r1,r3
;;         bic #65535-56,r3
;;         asr r3
;;         asr r3
;;         asr r3
;;         call @#fillrtsl
;;         aslb r3
;;         mfps r5
;;         mov r1,r3
;;         bic #65535-7,r3
         shl dh,1
         shl dh,1
         adc dh,bh
         mov ch,bl
         and ch,56
         mov cl,3
         shr ch,cl
         call fillrtsl
         shl ch,1
         pushf
         mov ch,bl
         and ch,7
         popf
         pushf

;;         call @#fillrtsr
;;         mtps r5
;;         call @#fillrt2
;;         incb r1
;;         movb r1,r1
;;         bne 1$
;;         return   ;ZF=1 required for loadpat
         call fillrtsr
         popf
         call fillrt2
         inc bl
         jz .ep1    ;optimize 8088?
         jmp .c1
.ep1:    retn

;;setrconst:    ;IN: R4 - string, R3 - end of string, R5 - live/born
setrconst:      ;IN: si - string, di - end of string, bp - live/born

;;          clr @r5
;;2$:       cmp r4,r3
;;          bpl exit5
          mov word [bx],0
.c2:      cmp si,di
          jns fillrt.ep1

;;          movb (r4)+,r0
;;          mov #1,r1
;;          sub #'0,r0
;;          beq 11$
          lodsb
          mov dx,1
          sub al,'0'
          jz .c11

;;1$:       asl r1
;;          sob r0,1$
.c1:      shl dx,1
          dec al
          jnz .c1

;;11$:      bis r1,@r5
;;          br 2$
.c11:     or [bx],dx
          jmp .c2
   endif
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
        ;;retn
        rts

.cont1: 
        ;;call .showr0
        bsr .showr0
        ;;jnz .loop2
        bne .loop2
        ;;retn
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
   if 0
showrules2:
;;        mov #stringbuf,r3
;;        mov r3,r4
;;        mov #1,r1
;;        clr r2
;;1$:     bit r1,@#live
;;        beq 2$
        mov si,stringbuf
        mov dx,si
        mov di,1
        xor ax,ax
.c1:    test [live],di
        jz .c2

;;        call @#20$
;;2$:     inc r2
;;        asl r1
;;        bpl 1$
        call .c20
.c2:    inc ax
        shl di,1
        jns .c1

;;        movb #'/,(r4)+
;;        mov #1,r1
;;        clr r2
;;4$:     bit r1,@#born
;;        beq 5$
        mov byte [si],'/'
        inc si
        mov di,1
        xor ax,ax
.c4:    test [born],di
        jz .c5

;;        call @#20$
;;5$:     inc r2
;;        asl r1
;;        bpl 4$
        call .c20
.c5:    inc ax
        shl di,1
        jns .c4

;;        clrb @r4
;;        sub r3,r4
;;        mov #32,r1
;;        sub r4,r1
;;        asr r1
;;        mov #19,r2
;;        jmp @#showptxt
         mov byte [si],'$'
         mov ah,9
         int 21h
         retn

;;20$:    mov r2,r0
;;        add #'0,r0
;;        movb r0,(r4)+
.c20:   mov bl,al
        add bl,'0'
        mov [si],bl
        inc si
.exit:  retn
   endif

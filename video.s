  if 0
insteps: call totext
.c38:    call printstr
         db ansiclrscn,green,"NUMBER OF GENERATIONS: "
         db black,'$'

.c3:     mov si,stringbuf
         xor bx,bx
         mov [temp2],bx
.c1:     call getkey
         cmp al,0dh  ;enter
         jz .c11

         cmp al,27   ;esc
         jnz .c16
.c20:    jmp curoff

.c16:    cmp al,8    ;backspace
         jz .c12

         cmp al,'0'+10
         jnc .c1

         cmp al,'0'
         jc .c1

         cmp bl,5
         jz .c1

         inc bx
         mov dl,al
         mov ah,2
         int 21h

         sub al,'0'
         mov [si],al
         inc si
         jmp .c1

.c12:    dec si
         dec bx
         js .c3

         call delchr
         jmp .c1

.c11:    cmp bl,0
         jz .c20

         sub si,bx   ;convert to binary
         xor dx,dx
         dec bx
         shl bx,1
.c33:    lodsb
         or al,al
         jz .c34

         mov di,[tobin+bx]
.c32:    add dx,di
         jnc .c38a
         jmp .c38       ;65535=max

.c38a:   dec al
         jnz .c32

.c34:    sub bx,2
         jns .c33

         mov [temp2],dx
         jmp .c20

bornstay:
         mov si,stringbuf
.c3:     mov di,si
.c1:     call getkey
         cmp al,0dh    ;Enter
         jz .c11

         cmp al,8     ;backspace
         jz .c12

         cmp cl,'0'
         jz .c40

         cmp al,27    ;esc
         jz .c11

.c40:    cmp al,cl
         jc .c1

         cmp al,'9'
         jnc .c1

         mov bp,si
.c4:     cmp bp,di
         jz .c5

;;         cmpb (r2)+,r0
         cmp al,[ds:bp]
         lahf
         inc bp
         sahf
         jz .c1
         jmp .c4

.c5:     mov [di],al
         inc di
         mov dl,al
         mov ah,2
         int 21h
         jmp .c1

.c11:    retn

.c12:    dec di
         cmp di,si
         js .c3

         call delchr
         jmp .c1

delchr:  push si              ;changes: ax,dx
         call printstr
         db 8,' ',8,'$'
         pop si
         retn

inborn:  call printstr
         db green,'THE RULES ARE DEFINED BY ',blue
         db 'BORN',green,' AND ',blue,'STAY',green
         db ' VALUES.  FOR EXAMPLE, ',purple
         db 'CONWAYS''S LIFE',green,' HAS BORN=3 AND STAY=23, '
         db  purple,'SEEDS',green,' - BORN=2 AND EMPTY STAY, '
         db purple,'HIGHLIFE,',green,' - BORN=36 AND STAY=23, '
         db purple,'LIFE WITHOUT DEATH',green
         db ' - BORN=3 AND STAY=012345678, ...',0dh,10,black
         db 'BORN = $'

;;         mov #'1,r5
;;         jmp @#bornstay
         mov cl,'1'
         jmp bornstay


instay:  call printstr
         db 0dh,10,'STAY = $'

         mov cl,'0'
         jmp bornstay

indens:  call totext
         call printstr
;         .byte 146
;         .ascii "SELECT DENSITY OR PRESS "
;         .byte 145
;         .ascii "KT"
;         .byte 146
;         .ascii " TO EXIT"
         db black,'SELECT DENSITY OR PRESS ',red,'ESC',black,' TO EXIT'
;         .byte 10,9,145,'0,147
;         .ascii " - 12.5%"
         db 0dh,10,t9,red,'0',green,' - 12.5%'
;         .byte 10,9,145,'1,147
;         .ascii " - 28%"
         db 0dh,10,t9,red,'1',green,' - 28%'
;         .byte 10,9,145,'2,147
;         .ascii " - 42%"
         db 0dh,10,t9,red,'2',green,' - 42%'
;         .byte 10,9,145,'3,147
;         .ascii " - 54%"
         db 0dh,10,t9,red,'3',green,' - 54%'
;         .byte 10,9,145,'4,147
;         .ascii " - 64%"
         db 0dh,10,t9,red,'4',green,' - 64%'
;         .byte 10,9,145,'5,147
;         .ascii " - 73%"
         db 0dh,10,t9,red,'5',green,' - 73%'
;         .byte 10,9,145,'6,147
;         .ascii " - 81%"
         db 0dh,10,t9,red,'6',green,' - 81%'
;         .byte 10,9,145,'7,147
;         .ascii " - 88.5%"
         db 0dh,10,t9,red,'7',green,' - 88.5%'
;         .byte 10,9,145,'8,147
;         .ascii " - 95%"
         db 0dh,10,t9,red,'8',green,' - 95%'
;         .byte 10,9,145,'9,147
;         .asciz " - 100%"
;         .byte 0
         db 0dh,10,t9,red,'9',green,' - 100%',black,'$'
;1$:      call @#getkey
;         cmpb #9,r0
;         beq 2$
         call curoff
.c1:     call getkey
         cmp al,27   ;ESC
         jz .c2

;         cmpb r0,#'0
;         bcs 1$
         cmp al,'0'
         jc .c1

;         cmpb r0,#'0+10
;         bcc 1$
         cmp al,'0'+10
         jnc .c1

;         sub #'0-1,r0
;         movb r0,@#density
         sub al,'0'-1
         mov [density],al

;2$:      jmp @#tograph
.c2:     jmp tograph

inmode:  call printstr
         db 0dh,10,0dh,10,green
         db 'SELECT BENCHMARK MODE'
         db 0dh,10,32,red,'0',green
         db ' - CALCULATIONS'
         db 0dh,10,32,red,'1',green
         db ' - VIDEO'
         db 0dh,10,32,red,'2',green
         db " - BOTH$"
.c1:     call getkey
         cmp al,'0'
         jc .c1

         cmp al,'3'
         jnc .c1

         sub al,'1'
         retn

help:    call totext
         call printstr
         db black,bold,t9,'*** XLIFE COMMANDS ***',normal
         db 0dh,10,red,'!',green
         db ' randomize screen'
         db 0dh,10,red,'%',green
         db ' set random density - default=42%'
         db 0dh,10,red,'+',green,'/',red,'-',green
         db ' zoom in/out'
         db 0dh,10,red,'.',green,'/',red,'Home',green
         db ' center/home cursor'
         db 0dh,10,red,'?',green
         db ' show this help'
         db 0dh,10,red,'B',green
         db ' benchmark'
         db 0dh,10,red,'C',green
         db ' clear screen'
         db 0dh,10,red,'E',green
         db ' toggle pseudocolor mode'
         db 0dh,10,red,'g',green
         db ' toggle run/stop mode'
         db 0dh,10,red,'h',green
         db ' toggle hide mode - about 10% faster'
         db 0dh,10,red,'l',green
         db ' load and transform file'
         db 0dh,10,red,'L',green
         db ' reload pattern'
         db 0dh,10,red,'o',green
         db ' one step'
         db 0dh,10,red,'Q',green
         db ' quit'
         db 0dh,10,red,'R',green
         db ' set the rules'
         db 0dh,10,red,'S',green
         db ' save'
         db 0dh,10,red,'t',green
         db ' toggle plain/torus topology'
         db 0dh,10,red,'v',green
         db ' show some info'
         db 0dh,10,red,'V',green
         db ' show comments to the pattern'
         db 0dh,10,red,'X',green,'/',red,'Z',green
         db ' reload/set&save palette'
         db 0dh,10,0dh,10,black
         db 'Use ',red,'cursor keys'
         db black, ' to set the position and '
         db red, 'space key', black
         db ' to toggle the current cell. '
         db 'Use ',red, 'shift', black
         db ' to speed up the movement$'
         call curoff
         call getkey
         jmp tograph

xyout:   cmp [zoom],0
         jnz xyout2

         mov dx,3
         mov di,192*40+66
         mov bx,xcrsr
         call digiout
         mov dl,3
         mov di,192*40+74
         ;mov bx,ycrsr
         jmp digiout
   endif
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

	 rts
   if 0
showtinfo:  ;;mov #tinfo,r0  ;must be after infoout
;;            mov @#tilecnt,r3
;;            asr r3
;;            asr r3
;;            cmp #120,r3   ;sets CY=0
;;            bne 1$
         ;cmp [zoom],0
         ;jnz showtinfo2

            mov bx,tinfo
            mov si,[tilecnt]
            shr si,1
            shr si,1
            cmp si,hormax*vermax/4
            jnz .c1

            mov word [bx],1
            mov byte [bx+2],0
            jmp .c2

.c1:        mov word [bx],0a0ah
            mov al,[si+ttab]
            mov ah,al
            and al,0fh
            mov [bx+2],al
            mov al,ah
            mov cl,4
            shr al,cl
            jz .c2

;;            movb r2,1(r0)
            mov [bx+1],al

;;2$:         mov #3,r1
;;            mov #<statusline*64+16384+30>,r2
;;            call @#digiout
;;            mov #todata,@#pageport
.c2:       mov dx,3
           mov di,192*40+30
           jmp digiout

xyout2:    mov cx,3
           mov di,24*80+66
           mov si,xcrsr
           call digiout2
           mov cl,3
           mov di,24*80+74
           jmp digiout2

infoout2: ;must be before showtinfo2
           mov si,gencnt
           mov cx,7
           mov di,24*80+2
           call digiout2
           mov si,cellcnt
           mov cl,5
           mov di,24*80+18
           call digiout2

showtinfo2:  ;must be after infoout2
           mov bx,tinfo      ;it is too similar to showtinfo
           mov si,[tilecnt]
           shr si,1
           shr si,1
           cmp si,hormax*vermax/4
           jnz .c1

           mov word [bx],1
           mov byte [bx+2],0
           jmp .c2

.c1:       mov word [bx],0f0f0h
           mov al,[si+ttab]
           mov cl,al
           and al,0fh
           mov [bx+2],al
           mov al,cl
           mov cl,4
           shr al,cl
           jz .c2

           mov [bx+1],al
.c2:       mov cx,3
           mov si,bx
           mov di,24*80+30
           jmp digiout2

calcx:   ;;movb @#crsrbit,r1  ;$80 -> 0, $40 -> 1, ...
;;         bis #65280,r1      ;$ff00, IN: R1, OUT: R1
;;1$:      add #256,r1
;;         aslb r1
;;         bcc 1$

;;         swab r1
;;         return
         mov ah,[crsrbit]
         mov al,0ffh
.c1:     inc al
         shl ah,1
         jnc .c1
         retn

showscnz:
;;xlimit   = $14
;;ylimit   = $15
;;         #assign16 i1,viewport
         mov si,[viewport]

;;         lda #5
;;         sta xlimit
         mov bl,5

;;         lda pseudoc
;;         bne showscnzp
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

gexit:    ;;jmp crsrset

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
;;         beq showscn2
         ;;jz showscn2
;;          br  showscnp
         ;;jmp showscnp
	 ;**bne showscnp

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
	 lsr.w #8,d0
	 move.b d0,(nextline*2,a5)

	 swap d0
	 move.b d0,(nextline,a5)

	 lsr.w #8,d0
	 move.b d0,(a5)

         move.l (4,a4),d0

	 move.b d0,(nextline*7,a5)
	 lsr.w #8,d0
	 move.b d0,(nextline*6,a5)

	 swap d0
	 move.b d0,(nextline*5,a5)

	 lsr.w #8,d0
	 move.b d0,(nextline*4,a5)

;;         mov next(r0),r0
         ;;mov si,[next-8+si]
	 movea.l (next,a4),a4

;;         cmp #1,r0
         ;;cmp si,1
	 cmpa.w #1,a4

;;          bne 1$
          ;;jz .lz
          ;;jmp .l1
	 bne .l1

.lz:
	 ;;jmp crsrset
	 rts

 if 0
showscnp: ;;mov @#startp,r0
;;1$:       call @#showscnp1
;;          mov next(r0),r0
;;          cmp #1,r0
;;          bne 1$
;;          jmp @#crsrset
          mov si,[startp]
.c1:      call showscnp1
          mov si,[si+next-8]
          cmp si,1
          jnz .c1
          jmp crsrset

showscnp1: ;;mov video(r0),r5
;;          mov @r0,r1
;;          bne 3$
          mov di,[si+video]
          lodsb
          or al,al
          jnz .c3

;;          mov #tovideo,@#pageport
;;          clr @r5
;;          clr 64(r5)
;;          mov #todata,@#pageport
;;          br 4$
          mov word [es:di],0
          jmp .c4

;;3$:       vidmacp count0,0
;;          swab r1
;;          bne 5$
.c3:      vidmacp count0-1,0
.c4:      lodsb
          or al,al
          jnz .c5

          mov word [es:di+2000h],0
          jmp .c6

.c5:      vidmacp count1-2,2000h
.c6:      lodsb
          or al,al
          jnz .c7

          mov word [es:di+80],0
          jmp .c8

.c7:      vidmacp count2-3,80
.c8:      lodsb
          or al,al
          jnz .c9

          mov word [es:di+2050h],0
          jmp .c10

.c9:      vidmacp count3-4,2050h
.c10:     lodsb
          or al,al
          jnz .c12

          mov word [es:di+160],0
          jmp .c14

.c12:     vidmacp count4-5,160
.c14:     lodsb
          or al,al
          jnz .c15

          mov word [es:di+20a0h],0
          jmp .c16

.c15:     vidmacp count5-6,20a0h
.c16:     lodsb
          or al,al
          jnz .c17

          mov word [es:di+240],0
          jmp .c18

.c17:     vidmacp count6-7,240
.c18:     lodsb
          or al,al
          jnz .c19

          mov word [es:di+20f0h],0
          retn

.c19:     vidmacp count7-8,20f0h
          retn

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

loadmenu:call totext
         call printstr
         db green,'INPUT FILENAME, AN EMPTY STRING MEANS TO SHOW DIRECTORY. PRESS '
         db red,'TAB',green,' TO USE RAMDISK, ',red,'*',green
         db ' TO CHANGE DRIVE, ',red,'ESC',green,' TO EXIT',0dh,10,black
.c80:    db 'A:$'

.c3:     mov di,fn
         xor cl,cl    ;length
.c1:     call getkey
         cmp al,0dh
         jz .c11

         cmp al,8   ;backspace
         jz .c12

         cmp al,27   ;esc
         jnz .c17

.c100:   mov ch,al
.c101:   ;call curoff  ;curoff changes ch?
         or ch,ch
         retn

.c17:    cmp al,'*'
         jnz .c21

         mov ch,al
         mov si,240
         call chgdrv
         jmp .c1

.c21:    cmp al,9    ;TAB
         jnz .c18

         call curoff
         call ramdisk
         mov ch,1
         jmp .c101

.c18:    cmp al,'!'
         jc .c1

         cmp al,126
         jnc .c1

         mov si,nofnchar   ;+1? allows ?-char
         mov dl,al
.c5:     lodsb
         cmp dl,al
         jz .c1

         cmp dl,'a'
         jc .c6

         cmp dl,'z'+1
         jnc .c6

         sub dl,'a'-'A'
.c6:     cmp si,stringbuf
         jnz .c5

         cmp cl,8
         jnc .c1

         mov [di],dl
         inc di
         inc cl
         mov ah,2
         int 21h
         jmp .c1

.c12:    dec di
         dec cl
         js .c3

         call delchr
         jmp .c1

.c11:    or cl,cl
         jz menu2

         mov word [di],'8'*256+'.'
         mov word [di+2],'L'*256+'X'
         xor ch,ch
         mov [di+4],ch
         jmp .c101

menu2:   call setdirmsk
         cmp al,27     ;esc
         jz .c100

;;*         .text "run/stop"
;;*         .byte 30
;;*         .text " and "
;;*         .byte 28
;;*         .text "cbm key"
;;*         .byte 30
;;*         .text " as usual"
;;*         .byte $d,0

         call showdir  ;returns number of directory entries in BP
.c6:     call printstr
         db home,clrtoeol,green
         db 'ENTER FILE# OR ',red,'ESC',green,': ',black,'$'

.c3:     mov di,stringbuf+1      ;+1?
         mov si,di
         xor cx,cx
.c1:     call getkey
         cmp al,27     ;esc
         jnz .c17

.c100:   ;call curon
         jmp loadmenu

.c17:    cmp al,0dh
         jz .c11

         cmp al,8   ;backspace
         jz .c12

         cmp al,'0'
         jc .c1

         cmp al,'9'+1
         jnc .c1

         cmp cl,3
         jz .c1

         mov [di],al
         inc di
         inc cl
         mov ah,2
         mov dl,al
         int 21h
         jmp .c1

.c11:    xor ax,ax
         or cl,cl
         jz .c3

         lodsb
         sub al,'0'
         dec cl
         jz .c21

         mov ch,10
.l2:     mul ch
         add al,[si]
         inc si
         sub al,'0'
         dec cl
         jnz .l2

.c21:    cmp ax,bp
         jc .l3
         jmp .c6      ;optimize 8088

.l3:     call findfn
         ;call curoff
         xor ax,ax     ;sets ZF
         retn

.c12:    dec di
         dec cl
         js .c3

         call delchr
         jmp .c1

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

xchgxy:  or [xchgdir],0
         jz exit7

         mov al,[x0]
         xchg al,[y0]
         mov [x0],al
exit7:   retn


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

crsrset1:
;;         mov @#crsrtile,r0     ;sets r0,r1
;;         movb @#crsrbyte,r1
;;         swab r1
;;         asr r1
;;         asr r1
;;         add video(r0),r1
;;         movb @#crsrbit,r0
;;         return
         mov si,[crsrtile]
         xor bx,bx
         mov bl,[crsrbyte]
         shl bx,1
         mov di,[crsrtab+bx]
         add di,[si+video]
         xor bx,bx
         mov bl,[crsrbit]
         retn

setdirmsk:
         call printstr
         db ansiclrscn,green,'SET DIRECTORY MASK ('
         db red,'ENTER',green
         db ' = *)',black,0dh,10,'$'

.c3:     mov di,svfn
         xor cx,cx
.c1:     call getkey
         cmp al,0dh
         jz .c11

         cmp al,8    ;backspace
         jz .c12

         cmp al,27     ;esc
         jz .c13

         cmp al,'!'
         jc .c1

         cmp al,126
         jnc .c1

         cmp cl,8    ;fn length limit
         jnc .c1

         mov si,nofnchar+1
         mov dl,al
.c50:    lodsb
         cmp al,dl
         jz .c1

         cmp si,stringbuf
         jnz .c50

         cmp dl,'a'
         jc .c6

         cmp dl,'z'+1
         jnc .c6

         sub dl,'a'-'A'
.c6:     mov [di],dl
         inc di
         inc cl
         mov ah,2
         int 21h
         jmp .c1

.c11:    or cl,cl
         jnz .c5

         mov byte [di],'*'
         inc di
.c5:     mov word [di],'8'*256+'.'
         mov word [di+2],'L'*256+'X'
         mov byte [di+4],ch
.c13:    retn

.c12:    dec di
         dec cl
         js .c3

         call delchr
         jmp .c1

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

crsrset: call crsrset1
         cmp [zoom],0
         jnz gexit2

pixel11:
;;         asl r0
;;         mov vistab(r0),r2
;;         mov r2,r0
;;         asl r2
;;         bis r0,r2
;;         bis r2,@r1
         shl bx,1    ;it should be after crsrset, IN: bx - crsrbit, di - addr of video tile line
         mov si,[bx+vistab]
         mov ax,si
         shl ax,1
         or ax,si
         or [es:di],ax
gexit2:  retn         ;this is also gexit3

pixel11p:push si
         push bx
         call pixel11
         pop bx
         pop si
         retn

crsrclr: ;;tstb @#zoom
;;         bne 1$
         cmp [zoom],0
         jnz gexit2

;;         mov @#crsrtile,r0
;;         movb @#crsrbyte,r1
         mov si,[crsrtile]
         xor bx,bx
         mov bl,[crsrbyte]

;;         mov r1,r2
;;         add r0,r2
;;         movb @r2,r2
         mov al,[si+bx]

;;         swab r1
;;         asr r1
;;         asr r1
;;         add video(r0),r1
         shl bx,1
         mov bp,bx
         mov di,[crsrtab+bx]
         add di,[si+video]

;;         tstb @#pseudoc
;;         bne 2$
         cmp [pseudoc],0
         jnz .c2

;;         mov #tovideo,@#pageport
;;         asl r2
;;         mov vistab(r2),r2
         xor bx,bx
         mov bl,al
         shl bx,1
         mov ax,[bx+vistab]

         xor bx,bx        ;for crsrflash
         mov bl,[crsrbit]
         shl bx,1
         mov bx,[bx+vistab]
         mov bp,bx
         shl bp,1
         or bp,bx
         and ax,bp
         not bp
         mov bx,[es:di]
         and bx,bp
         or ax,bx

         stosw
         retn

;;2$:      movb @#crsrbyte,r3
;;         asl r3
;;         asl r3
;;         add r0,r3
;;         bitb #15,@#crsrbit
;;         bne 3$
.c2:     shl bp,1
         add bp,si
         mov cl,4
         mov ch,[crsrbit]
         test ch,0fh
         jnz .c3

;;         mov count0(r3),r3
;;         bic #^B1110011100111111,r3
;;         mov r3,r4
;;         swab r4
;;         aslb r4   ;sets CY=0
;;         bis r3,r4
;;         rorb r2    ;uses CY=0
;;         asrb r2
;;         asrb r2
;;         asrb r2
;;         call @#8$
;;         br 5$
         mov dx,[ds:bp+count0]
         and dx,1100011000000b
         shl dh,1
         or dh,dl
         shr al,cl
         shr ch,cl   ;crsrflash
.c4:     or al,dh
         mov bx,vistabpc
         xlatb

         xchg al,ch        ;for crsrflash
         xlatb
         mov cl,al
         shr cl,1
         or al,cl
         and ch,al
         not al
         and al,[es:di]
         or al,ch

         stosb
         retn

;;3$:      inc r1
;;         mov count0+2(r3),r3
;;         bic #^B1111110011100111,r3
;;         asl r3
;;         asl r3
;;         asl r3
;;         mov r3,r4
;;        swab r4
;;         asl r4
;;         bis r3,r4
;;         bic #^B1111111111110000,r2
;;         call @#8$
;;         swab r2
;;         br 5$
.c3:     mov dx,[ds:bp+count0+2]
         and dx,1100011000b
         shr dl,1
         or dh,dl
         mov cl,4
         shl dh,cl
         and al,0fh
         inc di
         jmp .c4

;;1$:      clrb @#crsrpgmk
;;         call @#showscnz
;;         incb @#crsrpgmk
;;         ;mov @#crsrtile,r0   ;do not remove! ???
;;         return

;;8$:      bisb r4,r2
;;         bic #^B1111111100000000,r2
;;         mov #tovideo,@#pageport
;;         movb vistabpc(r2),r2
;;         return

crsrcalc:
;;        mov @#crsrtile,r0
;;        mov video(r0),r0     ;start of coorditates calculation
        mov bx,[crsrtile]
        mov bx,[bx+video]
        push bx

;;        sub #videostart,r0
        sub bx,40-hormax
        push bx

;;        asl r0
;;        asl r0
;;        mov r0,@#crsrx
        shl bx,1
        shl bx,1
        mov [crsrx],bl
        pop ax
        mov cl,40
        div cl
        mov [crsry],al
        pop ax
;;        clr r1
;;        movb @#crsrbit,r2
;;10$:    aslb r2
;;        bcs 8$

;;        inc r1
;;        br 10$
        xor bh,bh
        xchg bl,bh
        mov cl,[crsrbit]
.c10:   shl cl,1
        jc .c8

        inc bl
        jmp .c10

;;8$:     add r1,r0
;;        clr r1
;;        cmpb r0,#100
;;        bcs 1$
.c8:    add bh,bl
        xor bl,bl
.c8x:   cmp bh,100
        jc .c1

;;        inc r1
;;        sub #100,r0
        inc bl
        sub bh,100
        jmp .c8x

;;1$:     movb r1,@#xcrsr
;;        clr r1
.c1:    mov [xcrsr],bl
        xor bl,bl

;;3$:     cmpb r0,#10
;;        bcs 2$
.c3:    cmp bh,10
        jc .c2

;;        inc r1
;;        sub #10,r0
;;        br 3$
        inc bl
        sub bh,10
        jmp .c3

.c2:    mov word [xcrsr+1],bx
        mov cl,80
        div cl
        shl al,1
        add al,[crsrbyte]

        mov [ycrsr],1
        sub al,100
        jnc .l1

        dec [ycrsr]
        add al,100
.l1:    xor ah,ah
        mov cl,10
        div cl
        mov word [ycrsr+1],ax
        call xyout
        cmp [zoom],0
        jnz .c18
        retn

;;18$:    mov #up,r1
;;        movb @#vptilecy,r3
;;        add #8,r3
;;        movb @#vptilecy,r2
;;        bmi 33$
.c18:   mov di,up
        mov al,[vptilecy]
        mov ah,al
        add al,8
        or ah,ah
        js .c33

;;        mov #down,r1
;;        sub #16,r3
;;        cmpb r2,#24
;;        bcs 34$
        mov di,down
        sub al,16
        cmp ah,24
        jc .c34

;;33$:    movb r3,@#vptilecy
;;        br 31$
.c33:   mov [vptilecy],al
        jmp .c31

;;34$:    mov #left,r1
;;        movb @#vptilecx,r3
;;        add #8,r3
;;        movb @#vptilecx,r2
;;        bmi 35$
.c34:   mov di,left
        mov al,[vptilecx]
        mov ah,al
        add al,8
        or ah,ah
        js .c35

;;        mov #right,r1
;;        sub #16,r3
;;        cmpb r2,#40
;;        bcs 30$
        mov di,right
        sub al,16
        cmp ah,40
        jc .c30

;;35$:    movb r3,@#vptilecx
.c35:   mov [vptilecx],al

;;31$:    add @#viewport,r1
;;       mov @r1,r3
;;        mov r3,@#viewport
;;        mov dr(r3),r1
;;        mov dr(r1),r1
;;        mov right(r1),r1
;;        mov right(r1),r1
.c31:   add di,[viewport]
        mov bx,[di]
        mov [viewport],bx
        mov di,[bx+dr]
        mov di,[di+dr]
        mov di,[di+right]
        mov di,[di+right]

;;        add #44*tilesize,r3
;;        cmp r1,r3
;;        beq 30$
        add bx,44*tilesize
        cmp bx,di
        jz .c30

;;        call @#setviewport
;;30$:    jmp @#showscnz
        call setviewport
.c30:   call showscnz
        mov ah,2
        xor bh,bh
        mov dx,word [vptilecx]
        int 10h
        retn

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

putpixel2:
         mov di,[di+video]
         mov bl,dl
         shl bx,1
         mov bx,[bx+vistab]
         shr cl,1
         jnc .l1

         add di,2000h
.l1:     mov al,80
         mul cl
         add di,ax

         mov ax,bx
         not ax
         and [es:di],ax
         shl bx,1
         or [es:di],bx
.e1:     retn

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

printfloat: mov si,stringbuf
        xor cx,cx
        mov cl,[stringbuf]
        mov ah,2
        cmp cl,1
        jnz .l3

        mov dl,'.'
        int 21h
        mov dl,'0'
        int 21h
.l3:    add si,cx
.l2:    std
        lodsb
        cld
        cmp cl,ah
        jnz .l1

        mov dl,'.'
        push ax
        int 21h
        pop ax
.l1:    mov dl,al
        int 21h
        loop .l2
        retn
 endif

clrscn:  movea.l BITPLANE1_PTR(a3),a0
	 movea.l BITPLANE2_PTR(a3),a1
	 moveq #0,d0
	 move.w #nextline*50-1,d1
.l1:	 move.l d0,(a0)+
	 move.l d0,(a1)+
	 dbra d1,.l1
	 rts


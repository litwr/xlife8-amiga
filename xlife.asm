;this program doesn"t contain code of the original Xlife
;**it is the conversion from 6502 port for Commodore +4 v4
;**from z80 port for Amstrad CPC6128 v2, from K1801VM1 port for BK0011 v1
;**from 8088 port for IBM PC v1
;written by litwr, 2018
;it is under GNU GPL

	 include "amiga.mac"
         include "xlife.mac"

; A5 = CUSTOM, A3 = DATA SECTION

	section Code
start:  
        lea Data(PC),a3
        MOVE.L #CUSTOM,A5
	move.w	DMACONR(A5),d0
	or.w #$8000,d0
	move.w d0,olddmareq(a3)
        move.l	4,a6
	lea	gfxname(a3),a1
	jsr	OldOpenLibrary(a6)
	move.l	d0,gfxbase(a3)
	move.l 	d0,a6
	move.l 	38(a6),oldcopper(a3)

	jsr WaitTOF(a6)
	move.l	4,a6
	jsr Forbid(a6)
	;jsr Disable(a6)
        
        MOVE.L #COPPERLIST,COP1LCH(A5)	;Write to Copper location register
	MOVE.W COPJMP1(A5),d0		;Force copper to jump
        MOVE.W	#$7FFF,DMACON(A5)		; Clear DMA channels
	MOVE.W #(DMAF_SETCLR!DMAF_COPPER!DMAF_RASTER!DMAF_MASTER),DMACON(A5)
                            ;Enable bit-plane and Copper DMA

	if 0
	 mov [iobseg],ds
         add [iobseg],1000h  ;64k/16

         mov ah,19h   ;get current disk
         int 21h
         add al,"A"
         mov [loadmenu.c80],al

         xor bx,bx
.l2:     mov dl,bl
         mov ah,0eh   ;set current drive
         int 21h
         mov ah,19h
         int 21h
         mov [drives+bx],bh
         cmp al,bl
         jnz .l1

         add al,"A"
         cmp al,[loadmenu.c80]
         jnz .l3

         mov [curdrv],bl
.l3:     mov [drives+bx],al
.l1:     inc bx
         cmp bl,26
         jnz .l2

         mov dl,[loadmenu.c80]
         sub dl,"A"
         mov ah,0eh
         int 21h

         call chgdrv.ee1
         mov ax,0b800h
         mov es,ax
         call copyr
         call setcolors
         ;;incb @#errst
         call help
       endif

mainloop:
         ;call crsrflash
.e1:     bsr.s dispatcher
         move.b mode,d0
         beq mainloop

         cmp.b d0,3
         bne .c3

         ;mov ah,3bh
         ;mov dx,rootpath
         ;int 21h

         ;mov ax,3
         ;call totext.e1

         move.w #$7fff,DMACON(A5)
	 move.w	olddmareq,DMACON(A5)
	 move.l	oldcopper,COP1LCH(A5)
	 move.l 	gfxbase,a6
 	 jsr WaitTOF(a6)
	 move.l	4,a6
	 jsr Permit(a6)
	 ;jsr Enable(a6)
         move.l gfxbase,a1
	 jsr	CloseLibrary(a6)
         clr.l d0
         rts

.c3:     tst.w tilecnt
         bne .c4

         clr.b mode
         bsr.s incgen
         bsr.s tograph
         bra.s mainloop

.c4:     cmp.b d0,2
         bne .c5

         bsr.s generate     ;hide
         bsr.s cleanup
         bra.s mainloop.e1

.c5:     bsr.s zerocc
         bsr.s generate
         bsr.s showscn
         bsr.s cleanup
         bra.s mainloop

         ;include "io.s"
         ;include "ramdisk.s"
         include "video-base.s"
         include "video.s"
         ;include "utils.s"
         ;include "interface.s"
         ;include "rules.s"
         include "tile.s"
         ;include "ramdata.s"

;TIMERV          EQU     4096       ;1193180Hz/TIMERV=FREQ OF INTR8, approx 291.304 Hz

	if 0
start_timer:    cli                 ;SAVE/SET INTR8 VECTOR
                xor ax,ax
                mov [timercnt],ax
                mov [timercnt+2],ax
                mov ds,ax
                mov ax,[8*4]
                MOV [cs:SAVE8LO],ax
                mov ax,[8*4+2]
                MOV [cs:SAVE8HI],ax
                mov word [8*4],intr8
                mov [8*4+2],cs
                MOV     AL,36H          ;SET TIMER 0 HARDWARE
                OUT     43H,AL
                MOV     AL,TIMERV AND 0FFH
                OUT     40H,AL
                MOV     AL,TIMERV SHR 8
                jmp stop_timer.e1

stop_timer:     CLI                 ;SAVE/SET INTR8 VECTOR
                xor ax,ax
                mov ds,ax
                MOV ax,[cs:SAVE8LO]
                mov [8*4],ax
                MOV ax,[cs:SAVE8HI]
                mov [8*4+2],ax
                MOV     AL,36H          ;RESTORE TIMER HARDWARE
                OUT     43H,AL
                XOR     AL,AL
                OUT     40H,AL
.e1:            OUT     40H,AL
                jmp stop_timer2.e1

start_timer2:   cli                 ;SAVE/SET INTR8 VECTOR
                xor ax,ax
                cmp ax,[cs:SAVE8LO2]
                jne stop_timer2.exit

                mov ds,ax
                mov ax,[8*4]
                MOV [cs:SAVE8LO2],ax
                mov ax,[8*4+2]
                MOV [cs:SAVE8HI2],ax
                mov word [8*4],intr82
                mov [8*4+2],cs
                jmp stop_timer2.e1


stop_timer2:    CLI                 ;SAVE/SET INTR8 VECTOR
                xor ax,ax
                cmp ax,[cs:SAVE8LO2]
                je .exit

                mov ds,ax
                XCHG ax,[cs:SAVE8LO2]
                mov [8*4],ax
                MOV ax,[cs:SAVE8HI2]
                mov [8*4+2],ax
.e1:            push cs
                pop ds
.exit:          STI
.e2:            RETN

intr8:   inc [cs:timercnt]
         jnz .c1

         inc [cs:timercnt+2]
.c1:     test [cs:timercnt],TIMERV-1
         jz .c2

         push ax
         MOV AL,20H
         OUT 20H,AL
         pop ax
         iret

.c2:     db  0eah
SAVE8LO  DW      0
SAVE8HI  DW      0

intr82:  inc [cs:crsrticks]
          db  0eah
SAVE8LO2  DW      0
SAVE8HI2  DW      0

crsrflash:
         test [crsrticks],3
         jne stop_timer2.e2

         test [crsrticks],4
         je .l1

         jmp crsrset
.l1:     jmp crsrclr
	endif

generate:
	 ;;mov si,[startp]           ;currp=si
         movea.w startp,a4           ;currp=a4
         ;;mov cx,0c0c0h
         move.w #$c0c0,d2
         ;;mov dx,3030h
         move.w #$3030,d3
         ;;mov bp,0c0ch
         move.w #$c0c,d6
         ;;mov ax,303h
         move.w #$303,d0
.c30:    setcount 0
         setcount 2
         setcount 4
         setcount 6
         ;;mov si,[next+si]
         move.w (next,a4),a4
         ;;cmp si,1
         cmpa.w #1,a4
         bne .c30

         move.w a4,startp
.c5:     tst.b (sum,a4)
         beq .lnext

         ;;xor bx,bx
         moveq #0,d1
         ;;or bl,byte [si]            ;top row
         move.b (a4),d1            ;top row
         beq .ldown

         move.l d1,a1
         ;;mov di,[si+up]    ;adjcell=di, this line replaces iniadjc call!
	 move.w (up,a4),a5

         ;;shl bx,1
         adda.w a1,a1

         ;;mov cx,[bx+tab1213]
         move.w (tab1213,a1),d2
         ;;mov dx,[bx+tab1011]
	 move.w (tab1011,a1),d3
         ;;add [di+count7+2],cx
	 add.w d2,(count7+2,a5)
         ;;add [di+count7],dx
	 add.w d3,(count7,a5)

         ;;add [si+count1+2],cx
	 add.w d2,(count1+2,a4)
         ;;add [si+count1],dx
	 add.w d3,(count1,a4)

         ;;mov ax,[bx+tab2223]
         ;;add [si+count0+2],ax
	 add.w (tab2223,r1),(count0+2,a4)
         ;;mov ax,[bx+tab2021]
         ;;add [si+count0],ax
	 add (tab2021,a1),(count0,a4)
         bsr.s chkadd

.ldown:
         ;;xor bx,bx
         moveq #0,d1
         ;;or bl,[si+7]            ;bottom row
         move.b (7+a4),d1
         beq .lleft

;;         mov down(r0),r2          ;adjcell=r2
         mov di,[si+down]
	 move.w (down,a4),a5          ;adjcell=r2

         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]

;;         add r3,count0+2(r2)
         add [di+count0+2],cx
;;         add r4,count0(r2)
         add [di+count0],dx

;;         add r3,count6+2(r0)
         add [si+count6+2],cx
;;         add r4,count6(r0)
         add [si+count6],dx

;;         add tab2223(r1),count7+2(r0)
         mov ax,[bx+tab2223]
         add [si+count7+2],ax
;;         add tab2021(r1),count7(r0)
         mov ax,[bx+tab2021]
         add [si+count7],ax
         call chkadd

.lleft:
;;         mov left(r0),r2          ;adjcell=r2
         mov di,[si+left]

;;         mov #1024,r4             ;item to add
         mov dx,1024

;;         clr r3     ;change indicator
         xor cx,cx

;;         mov @r0,r1               ;2 rows
         mov bx,[si]
         or bx,bx
         jns .c6

;;         mov r1,r3
         mov cx,bx

;;         add r4,count0+2(r2)
         add [di+count0+2],dx

;;         add r4,count1+2(r2)
         add [di+count1+2],dx

;;         add r4,count2+2(r2)
         add [di+count2+2],dx

.c6:
;;         tstb r1
         or bl,bl
         jns .c7

;;         mov r1,r3
         mov cx,bx

;;         add r4,count0+2(r2)
         add [di+count0+2],dx

;;         add r4,count1+2(r2)
         add [di+count1+2],dx

;;         mov ul(r0),r5          ;adjcell2=r5
         mov bp,[si+ul]

;;         add r4,count7+2(r5)
         add [ds:bp+count7+2],dx
         call chkadd2

.c7:
;;         mov 2(r0),r1               ;2 rows
         mov bx,[si+2]
         or bx,bx
         jns .c8

;;         mov r1,r3
         mov cx,bx

;;         add r4,count2+2(r2)
         add [di+count2+2],dx

;;         add r4,count3+2(r2)
         add [di+count3+2],dx

;;         add r4,count4+2(r2)
         add [di+count4+2],dx

.c8:
;;         tstb r1
         or bl,bl
         jns .c9

;;         mov r1,r3
         mov cx,bx

;;         add r4,count1+2(r2)
         add [di+count1+2],dx

;;         add r4,count2+2(r2)
         add [di+count2+2],dx

;;         add r4,count3+2(r2)
         add [di+count3+2],dx

.c9:
;;         mov 4(r0),r1               ;2 rows
         mov bx,[si+4]
         or bx,bx
         jns .c10

;;         mov r1,r3
         mov cx,bx

;;         add r4,count4+2(r2)
         add [di+count4+2],dx

;;         add r4,count5+2(r2)
         add [di+count5+2],dx

;;         add r4,count6+2(r2)
         add [di+count6+2],dx

.c10:
;;         tstb r1
         or bl,bl
         jns .c11

;;         mov r1,r3
         mov cx,bx

;;         add r4,count3+2(r2)
         add [di+count3+2],dx
;;         add r4,count4+2(r2)
         add [di+count4+2],dx
;;         add r4,count5+2(r2)
         add [di+count5+2],dx

.c11:
;;         mov 6(r0),r1               ;2 rows
         mov bx,[si+6]
         or bx,bx
         jns .c12

;;         mov r1,r3
         mov cx,bx
;;         add r4,count6+2(r2)
         add [di+count6+2],dx
;;         add r4,count7+2(r2)
         add [di+count7+2],dx
;;         mov dl(r0),r5          ;adjcell2=r5
         mov bp,[si+dle]
;;         add r4,count0+2(r5)
         add [ds:bp+count0+2],dx
;;         call @#chkadd2
         call chkadd2
.c12:
;;         tstb r1
         or bl,bl
         jns .c14

;;         mov r1,r3
         mov cx,bx
;;         add r4,count5+2(r2)
         add [di+count5+2],dx
;;         add r4,count6+2(r2)
         add [di+count6+2],dx
;;         add r4,count7+2(r2)
         add [di+count7+2],dx
.c14:    call chkaddt

;;         mov right(r0),r2          ;adjcell=r2
         mov di,[si+right]
;;         mov #8,r4                ;item to add
         mov dx,8

;;         clr r3
         xor cx,cx

;;         mov @r0,r1               ;2 rows
         mov bx,[si]

;;         asr r1
         shr bx,1
         jnc .c15

         inc cx
;;         mov ur(r0),r5          ;adjcell2=r5
         mov bp,[si+ur]

;;         add r4,count7(r5)
         add [ds:bp+count7],dx
;;         add r4,count0(r2)
         add [di+count0],dx
;;         add r4,count1(r2)
         add [di+count1],dx
         call chkadd2

;*lr1
.c15:
;;         tstb r1
         or bl,bl
         jns .c16

;;         mov r1,r3
         mov cx,bx
;;         add r4,count0(r2)
         add [di+count0],dx
;;         add r4,count1(r2)
         add [di+count1],dx
;;         add r4,count2(r2)
         add [di+count2],dx

;*lr2
.c16:
;;         mov 2(r0),r1               ;2 rows
         mov bx,[si+2]

;;         asr r1
         shr bx,1
         jnc .c17

;;         adc r3
         inc cx
;;         add r4,count1(r2)
         add [di+count1],dx
;;         add r4,count2(r2)
         add [di+count2],dx
;;         add r4,count3(r2)
         add [di+count3],dx

;*lr3
.c17:
;;         tstb r1
         or bl,bl
         jns .c18

;;         mov r1,r3
         mov cx,bx
;;         add r4,count2(r2)
         add [di+count2],dx
;;         add r4,count3(r2)
         add [di+count3],dx
;;         add r4,count4(r2)
         add [di+count4],dx

;*lr4
.c18:
;;         mov 4(r0),r1               ;2 rows
         mov bx,[si+4]

;;         asr r1
         shr bx,1
         jnc .c19

;;         adc r3
         inc cx
;;         add r4,count3(r2)
         add [di+count3],dx
;;         add r4,count4(r2)
         add [di+count4],dx
;;         add r4,count5(r2)
         add [di+count5],dx

;*lr5
.c19:
;;         tstb r1
         or bl,bl
         jns .c20

;;         mov r1,r3
         mov cx,bx
;;         add r4,count4(r2)
         add [di+count4],dx
;;         add r4,count5(r2)
         add [di+count5],dx
;;         add r4,count6(r2)
         add [di+count6],dx

;*lr6
.c20:
;;         mov 6(r0),r1               ;2 rows
         mov bx,[si+6]
;;         asr r1
         shr bx,1
         jnc .c21

;;         adc r3
         inc cx
;;         add r4,count5(r2)
         add [di+count5],dx
;;         add r4,count6(r2)
         add [di+count6],dx
;;         add r4,count7(r2)
         add [di+count7],dx

;*lr7
.c21:
;;         tstb r1
         or bl,bl
         jns .c22

;;         mov r1,r3
         mov cx,bx
;;         add r4,count6(r2)
         add [di+count6],dx
;;         add r4,count7(r2)
         add [di+count7],dx
;;         mov dr(r0),r5          ;adjcell2=r5
         mov bp,[si+dr]
;;         add r4,count0(r5)
         add [ds:bp+count0],dx
         call chkadd2

.c22:    call chkaddt
;;         movb 6(r0),r1
         xor bx,bx
         or bl,[si+6]
         jz .c23

;;         asl r1
         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]
;;         add r3,count7+2(r0)
         add [si+count7+2],cx
;;         add r4,count7(r0)
         add [si+count7],dx
;;         add r3,count5+2(r0)
         add [si+count5+2],cx
;;         add r4,count5(r0)
         add [si+count5],dx
;;         add tab2223(r1),count6+2(r0)
         mov ax,[bx+tab2223]
         add [si+count6+2],ax
;;         add tab2021(r1),count6(r0)
         mov ax,[bx+tab2021]
         add [si+count6],ax

;*l2
.c23:
;;         movb 5(r0),r1
         xor bx,bx
         or bl,[si+5]
         jz .c24

;;         asl r1
         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]
;;         add r3,count6+2(r0)
         add [si+count6+2],cx
;;         add r4,count6(r0)
         add [si+count6],dx
;;         add r3,count4+2(r0)
         add [si+count4+2],cx
;;         add r4,count4(r0)
         add [si+count4],dx
;;         add tab2223(r1),count5+2(r0)
         mov ax,[bx+tab2223]
         add [si+count5+2],ax
;;         add tab2021(r1),count5(r0)
         mov ax,[bx+tab2021]
         add [si+count5],ax

;*l3
.c24:
;;         movb 4(r0),r1
         xor bx,bx
         or bl,[si+4]
         jz .c25

;;         asl r1
         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]
;;         add r3,count5+2(r0)
         add [si+count5+2],cx
;;         add r4,count5(r0)
         add [si+count5],dx
;;         add r3,count3+2(r0)
         add [si+count3+2],cx
;;         add r4,count3(r0)
         add [si+count3],dx
;;         add tab2223(r1),count4+2(r0)
         mov ax,[bx+tab2223]
         add [si+count4+2],ax
;;         add tab2021(r1),count4(r0)
         mov ax,[bx+tab2021]
         add [si+count4],ax

;*l4
.c25:
;;         movb 3(r0),r1
         xor bx,bx
         or bl,[si+3]
         jz .c26

;;         asl r1
         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]
;;         add r3,count4+2(r0)
         add [si+count4+2],cx
;;         add r4,count4(r0)
         add [si+count4],dx
;;         add r3,count2+2(r0)
         add [si+count2+2],cx
;;         add r4,count2(r0)
         add [si+count2],dx
;;         add tab2223(r1),count3+2(r0)
         mov ax,[bx+tab2223]
         add [si+count3+2],ax
;;         add tab2021(r1),count3(r0)
         mov ax,[bx+tab2021]
         add [si+count3],ax

;*l5
.c26:
;;         movb 2(r0),r1
         xor bx,bx
         or bl,[si+2]
         jz .c27

;;         asl r1
         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]
;;         add r3,count3+2(r0)
         add [si+count3+2],cx
;;         add r4,count3(r0)
         add [si+count3],dx
;;         add r3,count1+2(r0)
         add [si+count1+2],cx
;;         add r4,count1(r0)
         add [si+count1],dx
;;         add tab2223(r1),count2+2(r0)
         mov ax,[bx+tab2223]
         add [si+count2+2],ax
;;         add tab2021(r1),count2(r0)
         mov ax,[bx+tab2021]
         add [si+count2],ax

;*l6
.c27:
;;         movb 1(r0),r1
         xor bx,bx
         or bl,[si+1]
         jz .lnext

;;         asl r1
         shl bx,1
;;         mov tab1213(r1),r3
         mov cx,[bx+tab1213]
;;         mov tab1011(r1),r4
         mov dx,[bx+tab1011]
;;         add r3,count2+2(r0)
         add [si+count2+2],cx
;;         add r4,count2(r0)
         add [si+count2],dx
;;         add r3,count0+2(r0)
         add [si+count0+2],cx
;;         add r4,count0(r0)
         add [si+count0],dx
;;         add tab2223(r1),count1+2(r0)
         mov ax,[bx+tab2223]
         add [si+count1+2],ax
;;         add tab2021(r1),count1(r0)
         mov ax,[bx+tab2021]
         add [si+count1],ax

.lnext:
;;28$:
;;         mov next(r0),r0
         mov si,[si+next]

;;         cmp #1,r0
         cmp si,1
         jz stage2
         jmp .c5

stage2:
;;         mov @#startp,r0
         mov si,[startp]

;*genloop2
.c1:
;;         clrb sum(r0)
         mov byte [si+sum],0
         genmac 0
         genmac 1
         genmac 2
         genmac 3
         genmac 4
         genmac 5
         genmac 6
         genmac 7
;;         mov next(r0),r0
         mov si,[si+next]
;;         cmp #1,r0
         cmp si,1
         jz incgen
         jmp .c1

incgen:   mov bx,gencnt+7
          stc
          incbcd rts2
          incbcd rts2
          incbcd rts2
          incbcd rts2
          incbcd rts2
          incbcd rts2
          incbcd rts2
rts2:     retn

cleanup:  inc [clncnt]
          test [clncnt],15
          jnz rts2

cleanup0:
;;          mov @#startp,r0
          mov bx,[startp]
;;          clr r2        ;mark 1st
          xor si,si
.c1:
;;          tstb sum(r0)
         test byte [bx+sum],0ffh

;*         beq delel
         jz .delel

;;          mov r0,r2     ;save pointer to previous
          mov si,bx
;;          mov next(r0),r0
          mov bx,[bx+next]
;;          cmp #1,r0
          cmp bx,1
          jnz .c1

          retn

.delel:   dec [tilecnt]
;;          mov #count0,r1
          mov di,count0
;;          add r0,r1
          add di,bx
          xor ax,ax
          mov cx,16
.c2c:
          mov [di],ax
          add di,2
          loop .c2c
;;          mov next(r0),r1
;;          clr next(r0)
;;          mov r1,r0
          xchg ax,[bx+next]
          mov bx,ax
;;          tst r2
          or si,si
          jz .del1st

;;         mov r1,next(r2)
         mov [si+next],ax
;;         dec r1
         dec ax
         jnz .c1

.c4:     retn

.del1st:
;;         mov r1,@#startp
         mov [startp],bx

;;         tst @#tilecnt
         or [tilecnt],0
         jnz .c1
         retn


	section Data
oldcopper:	dc.l 0
gfxbase:	dc.l 0

olddmareq:	dc.w 0
oldintreq:	dc.w 0
oldintena:	dc.w 0
oldadkcon:	dc.w 0

bittab    dc.b 1,2,4,8,16,32,64,128

ttab      dc.b 0,1,2,3,3,4,5,6,7,8,8,9,16,17,18,19,19,20
          dc.b 21,22,23,24,24,25,32,33,34,35,35,36
          dc.b 37,38,39,40,40,41,48,49,50,51,51,52
          dc.b 53,54,55,56,56,57,64,65,66,67,67,68
          dc.b 69,70,71,72,72,73,80,81,82,83,83,84
          dc.b 85,86,87,88,88,89,96,97,98,99,99,100
          dc.b 101,102,103,104,104,105,112,113,114,115,115,116
          dc.b 117,118,119,120,120,121,128,129,130,131,131,132
          dc.b 133,134,135,136,136,137,144,145,146,147,147,148
          dc.b 149,150,151,152,152,153
;ttab:     repeat hormax*vermax/4
;          zv = (%-1)*400/hormax/vermax
;          dc.b (zv/10)*16 + zv mod 10
;          end repeat

digifont  dc.w 0a00ah,2828h,0a828h,282ah,2828h,2828h,0a00ah,0  ;8th columns are free
          dc.w 8002h,8002h,800ah,8002h,8002h,8002h,0a82ah,0
          dc.w 0a00ah,2828h,2800h,0a000h,0ah,28h,0a82ah,0
          dc.w 0a00ah,2828h,2800h,0a000h,2800h,2828h,0a00ah,0
          dc.w 2800h,0a000h,0a802h,280ah,0aa2ah,2800h,2800h,0
          dc.w 0a82ah,28h,0a02ah,2800h,2800h,2828h,0a00ah,0    ;5
          dc.w 0a00ah,2828h,28h,0a02ah,2828h,2828h,0a00ah,0
          dc.w 0a82ah,2828h,0a000h,8002h,8002h,8002h,8002h,0
          dc.w 0a00ah,2828h,2828h,0a00ah,2828h,2828h,0a00ah,0
          dc.w 0a00ah,2828h,2828h,0a80ah,2800h,2828h,0a00ah,0
          dc.w 0,0,0,0,0,0,0                ;space
;crsrtab   dc.w 0,2000h,80,2050h,160,20a0h,240,20f0h
startp    dc.w 1
tilecnt   dc.w 0
;viewport  dc.w tiles
;crsrtile  dc.w tiles
;timercnt  dc.w 0, 0
;temp      dc.w 0
;temp2     dc.w 0
;iobseg    dc.w 0
;filehl    dc.w 0
;filesz    dc.w 0
;tsz       dc.w 0
;saved     dc.w 0
;tobin     dc.w 1,10,100,1000,10000
;x0        dc.b 0   ;word aligned for the speed
;y0        dc.b 0
;live      dc.w 12  ;x0,y0,live,born have to go sequently
;born      dc.w 8

         include "tab12.s"
gentab:
         include "gentab.s"
         include "vistab.s"

tab3      dc.b 0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4
          dc.b 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
          dc.b 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
          dc.b 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
          dc.b 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
          dc.b 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
          dc.b 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
          dc.b 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
          dc.b 1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5
          dc.b 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
          dc.b 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
          dc.b 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
          dc.b 2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6
          dc.b 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
          dc.b 3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7
          dc.b 4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8

tiles:
         include "initiles.s"

;crsrbyte  dc.b 0      ;y%8  word aligned
;crsrbit   dc.b 128    ;x bit position
;i1        dc.b 0,0
;cellcnt   dc.b 0,0,0,0,0
;gencnt    dc.b 0,0,0,0,0,0,0
;crsrx     dc.b 0      ;[x/8]*8, word aligned
;crsry     dc.b 0      ;[y/8]*8
;vptilecx  dc.b 0      ;must be word aligned
;vptilecy  dc.b 0
;xcrsr     dc.b 0,0,0
;ycrsr     dc.b 0,0,0  ;must follow xcrsr
;tinfo     dc.b 0,0,0
;xchgdir   dc.b 0
;xdir      dc.b 0      ;linear transformation, word aligned
;ydir      dc.b 0
;clncnt    dc.b 0
;pseudoc   dc.b 0
;mode      dc.b 0      ;0-stop, 1-run, 2-hide, 3-exit
;zoom      dc.b 0
;fn        dc.b 0,0,0,0,0,0,0,0,0,0,0,0
;density   dc.b 3
;czbg      dc.b 0
;palette   dc.b 0
;bgr       dc.b 0ah
;bgs       dc.b 0
;zbgr      dc.b 20h
;zbgs      dc.b 0
;zfg       dc.b 3
;zfgnc     dc.b 5
;topology  dc.b 0      ;0 - torus
;crsrticks dc.b 1
;;errst:     dc.b 0   ;0 - do not print i/o-errors message, 1 - print
;ppmode    dc.b 1    ;putpixel mode: 0 - tentative, 1 - active
;crsrpgmk  dc.b 1   ;0 - do not draw cursor during showscnz, 1 - draw
;svfn      dc.b 0,0,0,0,0,0,0,0,0,0,0,0
;drives    rb 26
;curdrv    dc.b 0
;patpath   dc.b "\PATTERNS",0
;rootpath  dc.b "\",0
;cf        dc.b "\COLORS.CFG",0
;copyleft  dc.b "\CR.TXT",0
;nofnchar  dc.b "?,./:;<=>[\]|"
;stringbuf rb 19     ;must be after nofnchar

gfxname		DC.B	'graphics.library',0
	SECTION	Copper,DATA_C
COPPERLIST:
	DC.W BPL1PTH,2		;$21000 -> BPL1
	DC.W BPL1PTL,$1000
        DC.W BPL2PTH,2		;$23800 -> BPL2
	DC.W BPL2PTL,$3800

	DC.W	BPLCON0,$2200	; Bit-Plane control reg.
	DC.W	BPLCON1,$0000	; Hor-Scroll
	DC.W	BPLCON2,$0010	; Sprite/Gfx priority
	;DC.W	BPL1MOD,$0000	; Modulo (odd)  ;256,320
        DC.W	BPL1MOD,$0001	; Modulo (odd)  ;248
	DC.W	BPL2MOD,$0000	; Modulo (even)
	;DC.W	DIWSTRT,$2C81	; Screen Size Start, 320
        ;DC.W	DIWSTRT,$2C81	; Screen Size Start, 256
        DC.W	DIWSTRT,$2C81	; Screen Size Start, 248
	;DC.W	DIWSTOP,$2CC1	; Screen Size Stop, 320
        ;DC.W	DIWSTOP,$2C81	; Screen Size Stop, 256
	DC.W	DIWSTOP,$2C79	; Screen Size Stop, 248
	;DC.W	DDFSTRT,$0038	; H-start, 320
        ;DC.W	DDFSTRT,$0038	; H-start, 256
	DC.W	DDFSTRT,$0038	; H-start, 248
	;DC.W	DDFSTOP,$00D0	; H-stop, 320
        ;DC.W	DDFSTOP,$00B0	; H-stop, 256
	DC.W	DDFSTOP,$00ac	; H-stop, 248

	DC.W	COLOR00,$0F00	; Color #0 = red
	DC.W	COLOR01,$0FF0	; Color #1 = yellow
	DC.W	COLOR02,$0000	; Color #2 = black
	DC.W	COLOR03,$0FFF	; Color #3 = white
	DC.L	$FFFFFFFE	;End of Copper list


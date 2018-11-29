;this program doesn"t contain code of the original Xlife
;**it is the conversion from 6502 port for Commodore +4 v4
;**from z80 port for Amstrad CPC6128 v2, from K1801VM1 port for BK0011 v1
;**from 8088 port for IBM PC v1
;written by litwr, 2018
;it is under GNU GPL

	 include "amiga.mac"
         include "xlife.mac"

; A2 = CUSTOM, A3 = Data

;	section Code
start:
      basereg SOD,a3
	lea SOD,A3
        MOVE.L #CUSTOM,A2
	move.w	DMACONR(A2),d0
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

        MOVE.L #COPPERLIST,COP1LCH(A2)	;Write to Copper location register
	MOVE.W COPJMP1(A2),d0		;Force copper to jump
        MOVE.W	#$7FFF,DMACON(A2)		;Clear DMA channels
	MOVE.W #(DMAF_SETCLR!DMAF_COPPER!DMAF_RASTER!DMAF_MASTER),DMACON(A2)
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
         ;incb @#errst
         call help
       endif

     bsr clrscn
     ;move.l #$00003800,tiles+4(a3)
     ;move.l #$00007000,tiles(a3)
     move.l #$00000070,tiles+4(a3)
     move.w #1,tilecnt(a3)
     move.b #1,mode(a3)
     move.b #6,(tiles+sum,a3)
     move.l #1,(tiles+next,a3)
     ;bsr showscn

mainloop:
     btst.b #6,CIAAPRA ;if mouse button pressed then exit
     beq .exit

         ;call crsrflash
.e1:     ;bsr.s dispatcher
         move.b mode(a3),d0
         beq mainloop

         cmp.b #3,d0
         bne .c3

         ;mov ah,3bh
         ;mov dx,rootpath
         ;int 21h

         ;mov ax,3
         ;call totext.e1
.exit:
         MOVE.L #CUSTOM,A2
         move.w #$7fff,DMACON(A2)
	 move.w	olddmareq(a3),DMACON(A2)
	 move.l	oldcopper(a3),COP1LCH(A2)
	 move.l gfxbase(a3),a6
 	 jsr WaitTOF(a6)
	 move.l	4,a6
	 jsr Permit(a6)
	 ;jsr Enable(a6)
         move.l gfxbase(a3),a1
	 jsr	CloseLibrary(a6)
         moveq #0,d0
         rts

.c3:     tst.w tilecnt(a3)
         bne .c4

         clr.b mode(a3)
         bsr incgen
         ;bsr.s tograph
         bra.s mainloop

.c4:     cmp.b #2,d0
         bne .c5

         bsr generate     ;hide
         bsr cleanup
         bra.s .e1

.c5:     ;bsr.s zerocc
         bsr generate
         bsr.s showscn
         ;bsr cleanup
         bra.s mainloop

         ;include "io.s"
         ;include "ramdisk.s"
         ;include "video-base.s"
         include "video.s"
         ;include "utils.s"
         ;include "interface.s"
         ;include "rules.s"
         include "tile.s"
         ;include "ramdata.s"

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
         movea.l startp(a3),a4 ;currp   ;;mov si,[startp]
         move.w #$c0c0,d2		;;mov cx,0c0c0h
         move.w #$3030,d3		;;mov dx,3030h
         move.w #$c0c,d6		;;mov bp,0c0ch
         move.w #$303,d0		;;mov ax,303h
.c30:    setcount 0
         setcount 2
         setcount 4
         setcount 6
         move.l (next,a4),a4		;;mov si,[next+si]
         cmpa.w #1,a4			;;cmp si,1
         bne .c30			;;jnz .c30

         movea.l startp(a3),a4		;;mov si,[startp]
.c5:     tst.b (sum,a4)			;;cmp byte [si+sum],0
         beq .lnext			;;jz .$
					;;jmp .lnext

         moveq #0,d1			;;xor bx,bx
         move.b (a4),d1  ;top row	;;or bl,byte [si]
         beq .ldown			;;jz .ldown

	 movea.l (up,a4),a5		;;mov di,[si+up]
         add.w d1,d1			;;shl bx,1

	basereg SOD,a6
	 lea (a3,d1),a6
         move.w (tab1213,a6),d2		;;mov cx,[bx+tab1213]
	 move.w (tab1011,a6),d3		;;mov dx,[bx+tab1011]
	 add.w d2,(count7+2,a5)		;;add [di+count7+2],cx
	 add.w d3,(count7,a5)		;;add [di+count7],dx
	 add.w d2,(count1+2,a4)		;;add [si+count1+2],cx
	 add.w d3,(count1,a4)		;;add [si+count1],dx

	 move.w (tab2223,a6),d0		;;mov ax,[bx+tab2223]
	 add.w d0,(count0+2,a4)		;;add [si+count0+2],ax
         move.w (tab2021,a6),d0		;;mov ax,[bx+tab2021]
	 add.w d0,(count0,a4)		;;add [si+count0],ax
	endb a6
         bsr chkadd			;;call chkadd

.ldown:  moveq #0,d1			;;xor bx,bx
         move.b (7,a4),d1 ;bottom row	;;or bl,[si+7]
         beq .lleft

	 movea.l (down,a4),a5 ;adjcell	;;mov di,[si+down]
	 add.w d1,d1			;;shl bx,1

	basereg SOD,a6
	 lea (a3,d1),a6
	 move.w (tab1213,a6),d2		;;mov cx,[bx+tab1213]
         move.w (tab1011,a6),d3		;;mov dx,[bx+tab1011]

	 add.w d2,(count0+2,a5)		;;add [di+count0+2],cx
         add.w d3,(count0,a5)		;;add [di+count0],dx

         add.w d2,(count6+2,a4)		;;add [si+count6+2],cx
         add.w d3,(count6,a4)		;;add [si+count6],dx

	 move.w (tab2223,a6),d0		;;mov ax,[bx+tab2223]
         add.w d0,(count7+2,a4)		;;add [si+count7+2],ax
         move.w (tab2021,a6),d0		;;mov ax,[bx+tab2021]
         add.w d0,(count7,a4)		;;add [si+count7],ax
	endb a6
         bsr chkadd			;;call chkadd

.lleft:  movea.l (left,a4),a5 ;adjcell	;;mov di,[si+left]
	 move.w #1024,d3  ;item to add	;;mov dx,1024
	 clr.w d2  ;change indicator	;;xor cx,cx
         move.w (a4),d1  ;2 rows	;;mov bx,[si]
					;;or bx,bx
         bpl .c6			;;jns .c6

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count0+2,a5)		;;add [di+count0+2],dx
         add.w d3,(count1+2,a5)		;;add [di+count1+2],dx
         add.w d3,(count2+2,a5)		;;add [di+count2+2],dx

.c6:	 tst.b d1			;;or bl,bl
	 bpl .c7			;;jns .c7

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count0+2,a5)		;;add [di+count0+2],dx
         add.w d3,(count1+2,a5)		;;add [di+count1+2],dx
         movea.l (ul,a4),a6  ;adjcell2	;;mov bp,[si+ul]
         add.w d3,(count7+2,a6)		;;add [ds:bp+count7+2],dx
         bsr chkadd2			;;call chkadd2

.c7:     move.w (2,a4),d1  ;2 rows	;;mov bx,[si+2]
					;;or bx,bx
	 bpl .c8			;;jns .c8

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count2+2,a5)		;;add [di+count2+2],dx
         add.w d3,(count3+2,a5)		;;add [di+count3+2],dx
         add.w d3,(count4+2,a5)		;;add [di+count4+2],dx

.c8:     tst.b d1			;;or bl,bl
	 bpl .c9

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count1+2,a5)		;;add [di+count1+2],dx
         add.w d3,(count2+2,a5)		;;add [di+count2+2],dx
         add.w d3,(count3+2,a5)		;;add [di+count3+2],dx

.c9:     move.w (4,a4),d1  ;2 rowsx	;;mov bx,[si+4]
					;;or bx,b
         bpl .c10

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count4+2,a5)		;;add [di+count4+2],dx
         add.w d3,(count5+2,a5)		;;add [di+count5+2],dx
         add.w d3,(count6+2,a5)		;;add [di+count6+2],dx

.c10:    tst.b d1			;;or bl,bl
	 bpl .c11			;;jns .c11

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count3+2,a5)		;;add [di+count3+2],dx
         add.w d3,(count4+2,a5)		;;add [di+count4+2],dx
         add.w d3,(count5+2,a5)		;;add [di+count5+2],dx

.c11:    move.w (6,a4),d1  ;2 rows	;;mov bx,[si+6]
					;;or bx,bx
	 bpl .c12

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count6+2,a5)		;;add [di+count6+2],dx
         add.w d3,(count7+2,a5)		;;add [di+count7+2],dx
         movea.l (dl,a4),a6  ;adjcell2	;;mov bp,[si+dle]
         add.w d3,(count0+2,a6)		;;add [ds:bp+count0+2],dx
         bsr chkadd2			;;call chkadd2
.c12:    tst.b d1			;;or bl,bl
	 bpl .c14

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count5+2,a5)		;;add [di+count5+2],dx
         add.w d3,(count6+2,a5)		;;add [di+count6+2],dx
         add.w d3,(count7+2,a5)		;;add [di+count7+2],dx
.c14:    bsr chkaddt

         movea.l (right,a4),a5 ;adjcell ;;mov di,[si+right]
         moveq #8,d3  ;item to add	;;mov dx,8
	 clr.w d2			;;xor cx,cx
         move.w (a4),d1  ;2 rows	;;mov bx,[si]
         lsr.w #1,d1			;;shr bx,1
         bcc .c15			;jnc .c15

         addq #1,d2			;;inc cx
         movea.l (ur,a4),a6  ;adjcell2	;;mov bp,[si+ur]
         add.w d3,(count7,a6)		;;add [ds:bp+count7],dx
         add.w d3,(count0,a5)		;;add [di+count0],dx
         add.w d3,(count0+2,a5)		;;add [di+count1],dx
         bsr chkadd2			;;call chkadd2

.c15:	 tst.b d1			;;or bl,bl
         bpl .c16			;;jns .c16

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count0,a5)		;;add [di+count0],dx
         add.w d3,(count1,a5)		;;add [di+count1],dx
         add.w d3,(count2,a5)		;;add [di+count2],dx
.c16:    move.w (2,a4),d1  ;2 rows	;;mov bx,[si+2]
         lsr.w #1,d1			;;shr bx,1
         bcc .c17

	 addq #1,d2			;;inc cx
         add.w d3,(count1,a5)		;;add [di+count1],dx
         add.w d3,(count2,a5)		;;add [di+count2],dx
         add.w d3,(count3,a5)		;;add [di+count3],dx
.c17:	 tst.b d1			;;or bl,bl
         bpl .c18			;;jns .c18

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count2,a5)		;;add [di+count2],dx
         add.w d3,(count3,a5)		;;add [di+count3],dx
         add.w d3,(count4,a5)		;;add [di+count4],dx

.c18:    move.w (4,a4),d1  ;2 rows	;;mov bx,[si+4]
	 lsr.w #1,d1			;;shr bx,1
	 bcc .c19			;;jnc .c19

	 addq #1,d2			;;inc cx
         add.w d3,(count3,a5)		;;add [di+count3],dx
         add.w d3,(count4,a5)		;;add [di+count4],dx
         add.w d3,(count5,a5)		;;add [di+count5],dx
.c19:	 tst.b d1			;;or bl,bl
         bpl .c20			;;jns .c20

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count4,a5)		;;add [di+count4],dx
         add.w d3,(count5,a5)		;;add [di+count5],dx
         add.w d3,(count6,a5)		;;add [di+count6],dx
.c20:    move.w (6,a4),d1  ;2 rows	;;mov bx,[si+6]
	 lsr.w #1,d1			;;shr bx,1
	 bcc .c21			;;jnc .c21

	 addq #1,d2			;;inc cx		;;adc r3
         add.w d3,(count5,a5)		;;add [di+count5],dx
         add.w d3,(count6,a5)		;;add [di+count6],dx
         add.w d3,(count7,a5)		;;add [di+count7],dx
.c21:	 tst.b d1			;;or bl,bl
         bpl .c22			;;jns .c22

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count6,a5)		;;add [di+count6],dx
         add.w d3,(count7,a5)		;;add [di+count7],dx
         movea.l (dr,a4),a6  ;adjcell2	;;mov bp,[si+dr]
         add.w d3,(count0,a6)		;;add [ds:bp+count0],dx
         bsr chkadd2			;;call chkadd2

.c22:    bsr chkaddt			;;call chkaddt
	 moveq #0,d1			;;xor bx,bx
         move.b (6,a4),d1		;;or bl,[si+6]
	 beq .c23			;;jz .c23

	basereg SOD,a6
	 add.w d1,d1			;;shl bx,1
	 lea (a3,d1),a6
	 move.w (tab1213,a6),d2		;;mov cx,[bx+tab1213]
         move.w (tab1011,a6),d3		;;mov dx,[bx+tab1011]
         add.w d2,(count7+2,a4)		;;add [si+count7+2],cx
         add.w d3,(count7,a4)		;;add [si+count7],dx
         add.w d2,(count5+2,a4)		;;add [si+count5+2],cx
         add.w d3,(count5,a4)		;;add [si+count5],dx
         move.w (tab2223,a6),d0		;;mov ax,[bx+tab2223]
         add.w d0,(count6+2,a4)		;;add [si+count6+2],ax
         move.w (tab2021,a6),d0		;;mov ax,[bx+tab2021]
	endb a6
         add.w d0,(count6,a4)		;;add [si+count6],ax		
.c23:	 moveq #0,d1			;;xor bx,bx
         move.b (5,a4),d1		;;or bl,[si+5]
	 beq .c24			;;jz .c24

	basereg SOD,a6
	 add d1,d1			;;shl bx,1
	 lea (a3,d1),a6
         move.w (tab1213,a6),d2		;;mov cx,[bx+tab1213]
         move.w (tab1011,a6),d3		;;mov dx,[bx+tab1011]
         add.w d2,(count6+2,a4)		;;add [si+count6+2],cx
         add.w d3,(count6,a4)		;;add [si+count6],dx
         add.w d2,(count4+2,a4)		;;add [si+count4+2],cx
         add.w d3,(count4,a4)		;;add [si+count4],dx
         move.w (tab2223,a6),d0		;;mov ax,[bx+tab2223]
         add.w d0,(count5+2,a4)		;;add [si+count5+2],ax
         move.w (tab2021,a6),d0		;;mov ax,[bx+tab2021]
	endb a6
         add.w d0,(count5,a4)		;;add [si+count5],ax
.c24:	 moveq #0,d1			;;xor bx,bx
	 move.b (4,a4),d1		;;or bl,[si+4]
	 beq .c25			;;jz .c25

	basereg SOD,a6
	 add.w d1,d1			;;shl bx,1
	 lea (a3,d1),a6
         move.w (tab1213,a6),d2		;;mov cx,[bx+tab1213]
         move.w (tab1011,a6),d3		;;mov dx,[bx+tab1011]
         add.w d2,(count5+2,a4)		;;add [si+count5+2],cx
         add.w d3,(count5,a4)		;;add [si+count5],dx
         add.w d2,(count3+2,a4)		;;add [si+count3+2],cx
         add.w d3,(count3,a4)		;;add [si+count3],dx
	 move.w (tab2223,a6),d0		;;mov ax,[bx+tab2223]
         add.w d0,(count4+2,a4)		;;add [si+count4+2],ax
         move.w (tab2021,a6),d0		;;mov ax,[bx+tab2021]
         add.w d0,(count4,a4)		;;add [si+count4],ax
	endb a6

.c25:	 moveq #0,d1			;;xor bx,bx
	 move.b (3,a4),d1		;;or bl,[si+3]
	 beq .c26			;;jz .c26

	basereg SOD,a6
	 add d1,d1			;;shl bx,1
	 lea (a3,d1),a6
         move.w (tab1213,a6),d2		;;mov cx,[bx+tab1213]
         move.w (tab1011,a6),d3		;;mov dx,[bx+tab1011]
         add.w d2,(count4+2,a4)		;;add [si+count4+2],cx
         add.w d3,(count4,a4)		;;add [si+count4],dx
         add.w d2,(count2+2,a4)		;;add [si+count2+2],cx
         add.w d3,(count2,a4)		;;add [si+count2],dx
	 move.w (tab2223,a6),d0		;;mov ax,[bx+tab2223]
         add.w d0,(count3+2,a4)		;;add [si+count3+2],ax
         move.w (tab2021,a6),d0		;;mov ax,[bx+tab2021]		;;add tab2021(r1),count3(r0)
         add.w d0,(count3,a4)		;;add [si+count3],ax
	endb a6

.c26:	 moveq #0,d1			;;xor bx,bx
	 move.b (2,a4),d1		;;or bl,[si+2]
	 beq .c27			;;jz .c27

	basereg SOD,a6
	 add.w d1,d1			;;shl bx,1
	 lea (a3,d1),a6
         move.w (tab1213,a6),d2		;;mov cx,[bx+tab1213]
         move.w (tab1011,a6),d3		;;mov dx,[bx+tab1011]
         add.w d2,(count3+2,a4)		;;add [si+count3+2],cx
         add.w d3,(count3,a4)		;;add [si+count3],dx
         add.w d2,(count1+2,a4)		;;add [si+count1+2],cx
         add.w d3,(count1,a4)		;;add [si+count1],dx
	 move.w (tab2223,a6),d0		;;mov ax,[bx+tab2223]
         add.w d0,(count2+2,a4)		;;add [si+count2+2],ax
         move.w (tab2021,a6),d0		;;mov ax,[bx+tab2021]
         add.w d0,(count2,a4)		;;add [si+count2],ax
	endb a6

.c27:    moveq #0,d1			;;xor bx,bx		;;movb 1(r0),r1
	 move.b (1,a4),d1		;;or bl,[si+1]
	 beq .lnext			;;jz .lnext

	basereg SOD,a6
	 add.w d1,d1			;;shl bx,1
	 lea (a3,d1),a6
         move.w (tab1213,a6),d2		;;mov cx,[bx+tab1213]
         move.w (tab1011,a6),d3		;;mov dx,[bx+tab1011]
         add.w d2,(count2+2,a4)		;;add [si+count2+2],cx
         add.w d3,(count2,a4)		;;add [si+count2],dx
         add.w d2,(count0+2,a4)		;;add [si+count0+2],cx
         add.w d3,(count0,a4)		;;add [si+count0],dx
	 move.w (tab2223,a6),d0		;;mov ax,[bx+tab2223]
         add.w d0,(count1+2,a4)		;;add [si+count1+2],ax
         move.w (tab2021,a6),d0		;;mov ax,[bx+tab2021]
         add.w d0,(count1,a4)		;;add [si+count1],ax
	endb a6
.lnext:
         move.l (next,a4),a4		;;mov si,[si+next]
         cmpa.l #1,a4			;;cmp si,1
	 bne .c5			;;jz stage2
					;;jmp .c5

stage2:  movea.l startp(a3),a4		;;mov si,[startp]
.c1:	 clr.b (sum,a4)			;;mov byte [si+sum],0		;;clrb sum(r0)
	 lea.l gentab(a3),a2
	 lea.l tab3(a3),a0
         genmac 0
         genmac 1
         genmac 2
         genmac 3
         genmac 4
         genmac 5
         genmac 6
         genmac 7
	 movea.l (next,a4),a4		;;mov si,[si+next]
	 cmpa.l #1,a4			;;cmp si,1
	 bne .c1			;;jz incgen
					;;jmp .c1
incgen:  lea (gencnt+7,a3),a1		;;mov bx,gencnt+7
         move #16,CCR			;;stc
         incbcd rts2
         incbcd rts2
         incbcd rts2
         incbcd rts2
         incbcd rts2
         incbcd rts2
         incbcd rts2
rts2:	 rts				;;retn


cleanup: addi.b #1,clncnt(a3)		;;inc [clncnt]
	 cmpi.b #16,clncnt(a3)		;;test [clncnt],15
	 bne rts2			;;jnz rts2

cleanup0:
         move.b #0,clncnt(a3)   ;;!!
	 movea.l startp(a3),a1		;;mov bx,[startp]		;;mov @#startp,r0
	 movea #0,a4  ;mark 1st		;;xor si,si			;;clr r2
.c1:	 cmpi.b #$ff,(sum,a1)		;;test byte [bx+sum],0ffh	;;tstb sum(r0)
	 beq .delel			;;jz .delel			;;beq delel

		      ;save pointer to previous
	 move.l a1,a4			;;mov si,bx			;;mov r0,r2
	 movea.l (next,a1),a1		;;mov bx,[bx+next]		;;mov next(r0),r0
	 cmpa.l #1,a1			;;cmp bx,1			;;cmp #1,r0
	 bne .c1			;;jnz .c1
	 rts				;;retn

.delel:	 subq.w #1,(tilecnt,a3)		;;dec [tilecnt]
	 lea (count0,a1),a5		;;lea di,[count0+bx]		;;mov #count0,r1
									;;add r0,r1
	 moveq #0,d0			;;xor ax,ax
	 moveq #7,d2			;;mov cx,16
.c2c:	 move.l d0,(a5)+		;;mov [di],ax
					;;add di,2
	 dbra d2,.c2c			;;loop .c2c
	 move.l (next,a1),d1		;;xchg ax,[bx+next]		;;mov next(r0),r1
	 move.l d0,(next,a1)						;;clr next(r0)
	 movea.l d1,a1			;;mov bx,ax			;;mov r1,r0
	 cmpa.l #0,a4			;;or si,si			;;tst r2
	 beq .del1st			;;jz .del1st

	 move.l d1,(next,a4)		;;mov [si+next],ax		;;mov r1,next(r2)
	 subq #1,d1			;;dec ax			;;dec r1
	 bne .c1			;;jnz .c1

.c4:	rts				;;retn

.del1st:
	 movea.l startp(a3),a1		;;mov [startp],bx		;;mov r1,@#startp
	 tst.w tilecnt(a3)		;;or [tilecnt],0		;;tst @#tilecnt
	 bne .c1			;;jnz .c1
	 rts				;;retn

	endb a3

	section Data
SOD:
oldcopper:	dc.l 0
gfxbase:	dc.l 0
startp:         dc.l tiles

olddmareq:	dc.w 0
;oldintreq:	dc.w 0
;oldintena:	dc.w 0
;oldadkcon:	dc.w 0

bittab:   dc.b 1,2,4,8,16,32,64,128

ttab:     dc.b 0,1,2,3,3,4,5,6,7,8,8,9,16,17,18,19,19,20
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

digifont  dc.w $0a00a,$2828,$0a828,$282a,$2828,$2828,$0a00a,0  ;8th columns are free
          dc.w $8002,$8002,$800a,$8002,$8002,$8002,$0a82a,0
          dc.w $0a00a,$2828,$2800,$0a000,$0a,$28,$0a82a,0
          dc.w $0a00a,$2828,$2800,$0a000,$2800,$2828,$0a00a,0
          dc.w $2800,$0a000,$0a802,$280a,$0aa2a,$2800,$2800,0
          dc.w $0a82a,$28,$0a02a,$2800,$2800,$2828,$0a00a,0    ;5
          dc.w $0a00a,$2828,$28,$0a02a,$2828,$2828,$0a00a,0
          dc.w $0a82a,$2828,$0a000,$8002,$8002,$8002,$8002,0
          dc.w $0a00a,$2828,$2828,$0a00a,$2828,$2828,$0a00a,0
          dc.w $0a00a,$2828,$2828,$0a80a,$2800,$2828,$0a00a,0
          dc.w 0,0,0,0,0,0,0                ;space
;crsrtab   dc.w 0,$2000,80,$2050,160,$20a0,240,$20f0
tilecnt   dc.w 0
;viewport  dc.l tiles
;crsrtile  dc.l tiles
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

;crsrbyte  dc.b 0      ;y%8  word aligned
;crsrbit   dc.b 128    ;x bit position
;i1        dc.b 0,0
;cellcnt   dc.b 0,0,0,0,0
gencnt    dc.b 0,0,0,0,0,0,0
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
clncnt    dc.b 0
pseudoc   dc.b 0
mode      dc.b 0      ;0-stop, 1-run, 2-hide, 3-exit
zoom      dc.b 0
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

	 CNOP 0,4
tiles:
         include "initiles.s"

	SECTION	Copper,DATA_C
COPPERLIST:
	DC.W BPL1PTH,startpl1>>16
	DC.W BPL1PTL,startpl1&$ffff
        DC.W BPL2PTH,startpl2>>16
	DC.W BPL2PTL,startpl1&$ffff

	DC.W	BPLCON0,$2200	; Bit-Plane control reg.
	DC.W	BPLCON1,$0000	; Hor-Scroll
	DC.W	BPLCON2,$0010	; Sprite/Gfx priority
	;DC.W	BPL1MOD,$0000	; Modulo (odd)  ;256,320
        DC.W	BPL1MOD,$0000	; Modulo (odd)  ;248
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


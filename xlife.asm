;this program doesn't contain code of original Xlife
;**it is the conversion from the 6502 port for the Commodore +4
;**from the z80 port for the Amstrad CPC6128, from the K1801VM1 port for the BK0011
;**from the 8088 port for the IBM PC
;written by litwr, 2018-2021
;thanks to Tuomas Järvensivu's blog post Crash course to Amiga assembly programming
;a lot of thanks to guys at http://eab.abime.net
;some materials were used from AsmOne examples coded by Rune Gram-Madsen
;it is under GNU GPL

	 include "amiga.mac"
         include "xlife.mac"

; A2 = CUSTOM, A3 = Data

	section Code

         basereg SOD,a3
         lea.l SOD,A3

         include "system1.s"

start:   movea.l 4.w,a6
         lea dosname(a3),a1
         jsr OldOpenLibrary(a6)
         move.l d0,doslib(a3)
         bsr chgdrv
         bsr copyr
         bsr setcolors
         movea.l BITPLANE3_PTR(a3),a0   ;clear cursor plane
         move.w #nextline*50-1,d1
         moveq #0,d0
.l1:     move.l d0,(a0)+
         dbra d1,.l1
         addq.b #1,errst(a3)
         bsr showmode
         bsr help
mainloop:bsr crsrflash
.e1:     bsr dispatcher
         move.b mode(a3),d0
         beq.s mainloop

         cmp.b #3,d0
         bne.s .c3

.exit:   move.l doslib(a3),a1
         movea.l 4.w,a6
         jmp CloseLibrary(a6)

.c3:     tst.w tilecnt(a3)
         bne.s .c4

         clr.b mode(a3)
         bsr incgen
         bsr tograph
         bra.s mainloop

.c4:     cmp.b #2,d0
         bne.s .c5

         bsr generate     ;hide
         bsr cleanup
         bra.s .e1

.c5:     bsr zerocc
         bsr generate
         bsr showscn
         bsr cleanup
         bra.s mainloop

         include "parsepattern.s"
         include "io.s"
         include "ramdisk.s"
         include "video-base.s"
         include "video.s"
         include "utils.s"
         include "interface.s"
         include "rules.s"
         include "tile.s"

rasteri:     btst #6,$dff01e   ;blitter?
             bne.s rasterie

             addq.l #1,timercnt
rasterie:    move.l interruptv,-(sp)
             rts

generate:
         movea.l startp(a3),a4 ;currp   ;;mov si,[startp]
         move.l #$c0c0c0c0,d2
         move.l #$30303030,d3
         move.l #$c0c0c0c,d6
         move.l #$3030303,d0
.c30:    setcount 0
         setcount 4
         move.l (next,a4),a4		;;mov si,[next+si]
         cmpa.w #1,a4			;;cmp si,1
         bne .c30			;;jnz .c30

         movea.l startp(a3),a4		;;mov si,[startp]
.c5:     tst.b (sum,a4)			;;cmp byte [si+sum],0
         beq .lnext

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
	 moveq #4,d3  ;item to add	;;mov dx,1024
	 clr.w d2  ;change indicator	;;xor cx,cx
         move.w (a4),d1  ;2 rows	;;mov bx,[si]
					;;or bx,bx
         bpl .c6			;;jns .c6

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count0+2,a5)		;;add [di+count0+2],dx
         add.w d3,(count1+2,a5)		;;add [di+count1+2],dx
         movea.l (ul,a4),a6  ;adjcell2	;;mov bp,[si+ul]
         add.w d3,(count7+2,a6)		;;add [ds:bp+count7+2],dx
         bsr chkadd2			;;call chkadd2

.c6:	 tst.b d1			;;or bl,bl
	 bpl .c7			;;jns .c7

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count0+2,a5)		;;add [di+count0+2],dx
         add.w d3,(count1+2,a5)		;;add [di+count1+2],dx
         add.w d3,(count2+2,a5)		;;add [di+count2+2],dx
.c7:     move.w (2,a4),d1  ;2 rows	;;mov bx,[si+2]
					;;or bx,bx
	 bpl .c8			;;jns .c8

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count1+2,a5)		;;add [di+count1+2],dx
         add.w d3,(count2+2,a5)		;;add [di+count2+2],dx
         add.w d3,(count3+2,a5)		;;add [di+count3+2],dx
.c8:     tst.b d1			;;or bl,bl
	 bpl .c9

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count2+2,a5)		;;add [di+count2+2],dx
         add.w d3,(count3+2,a5)		;;add [di+count3+2],dx
         add.w d3,(count4+2,a5)		;;add [di+count4+2],dx
.c9:     move.w (4,a4),d1  ;2 rows	;;mov bx,[si+4]
					;;or bx,b
         bpl .c10

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count3+2,a5)		;;add [di+count3+2],dx
         add.w d3,(count4+2,a5)		;;add [di+count4+2],dx
         add.w d3,(count5+2,a5)		;;add [di+count5+2],dx
.c10:    tst.b d1			;;or bl,bl
	 bpl .c11			;;jns .c11

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count4+2,a5)		;;add [di+count4+2],dx
         add.w d3,(count5+2,a5)		;;add [di+count5+2],dx
         add.w d3,(count6+2,a5)		;;add [di+count6+2],dx
.c11:    move.w (6,a4),d1  ;2 rows	;;mov bx,[si+6]
					;;or bx,bx
	 bpl .c12

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count5+2,a5)		;;add [di+count5+2],dx
         add.w d3,(count6+2,a5)		;;add [di+count6+2],dx
         add.w d3,(count7+2,a5)		;;add [di+count7+2],dx
.c12:    tst.b d1			;;or bl,bl
	 bpl .c14

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count6+2,a5)		;;add [di+count6+2],dx
         add.w d3,(count7+2,a5)		;;add [di+count7+2],dx
         movea.l (dl,a4),a6  ;adjcell2	;;mov bp,[si+dle]
         add.w d3,(count0+2,a6)		;;add [ds:bp+count0+2],dx
         bsr chkadd2			;;call chkadd2
.c14:    bsr chkaddt

         movea.l (right,a4),a5 ;adjcell ;;mov di,[si+right]
         move.w #$800,d3  ;item to add	;;mov dx,8
	 clr.w d2			;;xor cx,cx
         move.w (a4),d1  ;2 rows	;;mov bx,[si]
         lsr.w #1,d1			;;shr bx,1
         bcc .c15			;jnc .c15

	 move.w d3,d2			;;inc cx
         add.w d3,(count0,a5)		;;add [di+count0],dx
         add.w d3,(count1,a5)		;;add [di+count1],dx
         add.w d3,(count2,a5)		;;add [di+count2],dx
.c15:	 tst.b d1			;;or bl,bl
         bpl .c16			;;jns .c16

	 move.w d1,d2			;;mov cx,bx
         movea.l (ur,a4),a6  ;adjcell2	;;mov bp,[si+ur]
         add.w d3,(count7,a6)		;;add [ds:bp+count7],dx
         add.w d3,(count0,a5)		;;add [di+count0],dx
         add.w d3,(count1,a5)		;;add [di+count1],dx
         bsr chkadd2			;;call chkadd2

.c16:    move.w (2,a4),d1  ;2 rows	;;mov bx,[si+2]
         lsr.w #1,d1			;;shr bx,1
         bcc .c17

	 move.w d3,d2			;;inc cx
         add.w d3,(count2,a5)		;;add [di+count2],dx
         add.w d3,(count3,a5)		;;add [di+count3],dx
         add.w d3,(count4,a5)		;;add [di+count4],dx
.c17:	 tst.b d1			;;or bl,bl
         bpl .c18			;;jns .c18

	 move.w d3,d2			;;mov cx,bx
         add.w d3,(count1,a5)		;;add [di+count1],dx
         add.w d3,(count2,a5)		;;add [di+count2],dx
         add.w d3,(count3,a5)		;;add [di+count3],dx
.c18:    move.w (4,a4),d1  ;2 rows	;;mov bx,[si+4]
	 lsr.w #1,d1			;;shr bx,1
	 bcc .c19			;;jnc .c19

	 move.w d3,d2			;;inc cx
         add.w d3,(count4,a5)		;;add [di+count4],dx
         add.w d3,(count5,a5)		;;add [di+count5],dx
         add.w d3,(count6,a5)		;;add [di+count6],dx
.c19:	 tst.b d1			;;or bl,bl
         bpl .c20			;;jns .c20

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count3,a5)		;;add [di+count3],dx
         add.w d3,(count4,a5)		;;add [di+count4],dx
         add.w d3,(count5,a5)		;;add [di+count5],dx
.c20:    move.w (6,a4),d1  ;2 rows	;;mov bx,[si+6]
	 lsr.w #1,d1			;;shr bx,1
	 bcc .c21			;;jnc .c21

	 move.w d3,d2			;;inc cx		;;adc r3
         add.w d3,(count6,a5)		;;add [di+count6],dx
         add.w d3,(count7,a5)		;;add [di+count7],dx
         movea.l (dr,a4),a6  ;adjcell2	;;mov bp,[si+dr]
         add.w d3,(count0,a6)		;;add [ds:bp+count0],dx
         bsr chkadd2			;;call chkadd2
.c21:	 tst.b d1			;;or bl,bl
         bpl .c22			;;jns .c22

	 move.w d1,d2			;;mov cx,bx
         add.w d3,(count5,a5)		;;add [di+count5],dx
         add.w d3,(count6,a5)		;;add [di+count6],dx
         add.w d3,(count7,a5)		;;add [di+count7],dx
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
         add.w d0,(count6,a4)		;;add [si+count6],ax
	endb a6
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
         add.w d0,(count5,a4)		;;add [si+count5],ax
	endb a6
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
         cmpa.w #1,a4			;;cmp si,1
	 bne .c5			;;jz stage2
					;;jmp .c5

stage2:  movea.l startp(a3),a4		;;mov si,[startp]
	 lea.l gentab(a3),a2
	 lea.l tab3(a3),a0
.c1:	 clr.b (sum,a4)			;;mov byte [si+sum],0		;;clrb sum(r0)
         genmac 0
         genmac 1
         genmac 2
         genmac 3
         genmac 4
         genmac 5
         genmac 6
         genmac 7
	 movea.l (next,a4),a4		;;mov si,[si+next]
	 cmpa.w #1,a4			;;cmp si,1
	 bne .c1			;;jz incgen
					;;jmp .c1

incgen:  lea (gencnt+4,a3),a1		;;mov bx,gencnt+7
	 moveq #0,d0
         move #16,CCR			;;stc
         incbcd .irts
         incbcd .irts
         incbcd .irts
         move.b -(a1),d1
	 abcd d0,d1
.irts:	 move.b d1,(a1)
iexit:	 rts				;;retn

cleanup: addq.b #8,clncnt(a3)		;;inc [clncnt]
	 				;;test [clncnt],15
	 bpl iexit			;;jnz iexit

cleanup0:
         move.b #0,clncnt(a3)
	 movea.l startp(a3),a1		;;mov bx,[startp]		;;mov @#startp,r0
	 movea #0,a4  ;mark 1st		;;xor si,si			;;clr r2
.c1:	 tst.b (sum,a1)			;;test byte [bx+sum],0ffh	;;tstb sum(r0)
	 beq .delel			;;jz .delel			;;beq delel

		      ;save pointer to previous
	 movea.l a1,a4			;;mov si,bx			;;mov r0,r2
	 movea.l (next,a1),a1		;;mov bx,[bx+next]		;;mov next(r0),r0
	 cmpa.w #1,a1			;;cmp bx,1			;;cmp #1,r0
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
	 cmpa.l d0,a4			;;or si,si			;;tst r2
	 beq .del1st			;;jz .del1st

	 move.l d1,(next,a4)		;;mov [si+next],ax		;;mov r1,next(r2)
	 subq #1,d1			;;dec ax			;;dec r1
	 bne .c1			;;jnz .c1

	 rts				;;retn

.del1st:
	 move.l d1,startp(a3)		;;mov [startp],bx		;;mov r1,@#startp
	 tst.w tilecnt(a3)		;;cmp [tilecnt],0		;;tst @#tilecnt
	 bne .c1			;;jnz .c1
	 rts				;;retn

crsrflash:
         move.w crsrtick(a3),d0
         ;add.w tilecnt(a3),d0
         addq.w #1,d0
         cmpi.w #10000,d0
         bcs .l1

         clr.w crsrtick(a3)
         tst.b zoom(a3)
         bne .l2

         bsr crsrclr
         bchg.b #0,crsrstate(a3)
         beq crsrset
.l2:     rts

.l1:     ;addq.w #8,d0
         move.w d0,crsrtick(a3)
         rts

    endb a3

SOD:
startp:       dc.l 1  ;it is the fastest variable, no need an offset
doslib        dc.l 0
interruptv    dc.l 0
;oldcopper     dc.l 0
tmplock       dc.l 0
charCount     dc.l 0
stacklimit    dc.l 0
filesz    dc.l 0
tsz       dc.l 0
filehl    dc.l 0
saveWPTR  dc.l 0

crsrtick      dc.w 0

bittab:   dc.b 1,2,4,8,16,32,64,128

ttab:     rept hormax*vermax/4
          dc.b (((REPTN-1)*400/hormax/vermax)/10)*16+((REPTN-1)*400/hormax/vermax)%10
          endr

tilecnt   dc.w 0
viewport  dc.l tiles
crsrtile  dc.l tiles
timercnt  dc.l 0
temp      dc.w 0    ;32-bit alignment
temp2     dc.w 0
saved     dc.w 0
tobin     dc.w 1,10,100,1000,10000
x0        dc.b 0   ;word aligned for the speed
y0        dc.b 0
live      dc.w 12  ;x0,y0,live,born have to go sequently
born      dc.w 8
mouseprevX dc.w 0
mouseprevY dc.w 0
mouseoldX dc.w 0
mouseoldY dc.w 0
mousesaveX dc.w 0
mousesaveY dc.w 0

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

digifont dc.b	$3c,$66,$6e,$76,$66,$66,$3c,0   ;8th columns are free
	 dc.b	$18,$18,$38,$18,$18,$18,$7e,0
	 dc.b	$3c,$66,6,$c,$30,$60,$7e,0
	 dc.b	$3c,$66,6,$c,6,$66,$3c,0
	 dc.b	6,$c,$1e,$36,$7f,6,6,0
	 dc.b	$7e,$60,$7c,6,6,$66,$3c,0    ;5
	 dc.b	$3c,$66,$60,$7c,$66,$66,$3c,0
	 dc.b	$7e,$66,$c,$18,$18,$18,$18,0
	 dc.b	$3c,$66,$66,$3c,$66,$66,$3c,0
	 dc.b	$3c,$66,$66,$3e,6,$66,$3c,0
         dc.w   0,0,0,0,0,0,0                ;space

crsrbyte  dc.b 0      ;y%8  word aligned
crsrbit   dc.b 128    ;x bit position
xdir      dc.b 0      ;linear transformation, word aligned
ydir      dc.b 0
vptilecx  dc.b 0      ;must be word aligned
vptilecy  dc.b 0
cellcnt   dc.b 0,0,0
gencnt    dc.b 0,0,0,0
crsrx     dc.b 0      ;[x/8]*8
crsry     dc.b 0      ;[y/8]*8
xcrsr     dc.b 0,0
ycrsr     dc.b 0,0  ;must follow xcrsr
tinfo     dc.b 0,0
xchgdir   dc.b 0
clncnt    dc.b 0
pseudoc   dc.b 0
mode      dc.b 0      ;0-stop, 1-run, 2-hide, 3-exit
zoom      dc.b 0
fn        blk.b 31,0    ;30 is max filename length
density   dc.b 3
i1        dc.b 0,0
topology  dc.b 0      ;0 - torus
errst     dc.b 0   ;0 - do not print i/o-errors message, 1 - print
mouseact  dc.b 1   ;0 - do not call setviewport after mouse action
ppmode    dc.b 1    ;putpixel mode: 0 - tentative, 1 - active
svfn      blk.b 31
cf        dc.b "colors.cfg",0
copyleft  dc.b "cr.txt",0
nofnchar  dc.b "?#()|%[]~/:;<>'*+"
          dc.b '"'
stringbuf blk.b 21     ;must be after nofnchar
crsrstate  dc.b 0
tbformat   dc.b "TIME: %d.%02ds",0
sbformat   dc.b "SPEED: %d.%02d",0
nformat    dc.b "%3d ",0
sformat    dc.b "%d",0
xformat    dc.b "%03x",0
lformat    dc.b "%ld",0
ulformat   dc.b "N%09ld",0
dosname  dc.b "dos.library",0
curdir1  dc.b "xlife-8/",0
curdir2  dc.b "patterns",0
curpath  blk.b 34
curpathsv blk.b 34
ioerrmsg dc.b " i/o err #%ld",0
mouseleft dc.b 0
mouseright dc.b 0

        CNOP 0,4
curdisk dc.b "xxx:",0
drvidx  dc.b 0

        CNOP 0,4
nudrives = 12
drives  dc.b 'BAD:'
        dc.b 'DF0:'
        dc.b 'DF1:'
        dc.b 'DF2:'
        dc.b 'DF3:'
        dc.b 'DH0:'
        dc.b 'DH1:'
        dc.b 'DH2:'
        dc.b 'DH3:'
        dc.b 'RAM:'
        dc.b 'RAD:'
        dc.b 'XLF:'

SCREEN_DEFS:
	DC.W	0,0		; X-Y position
	DC.W	320		; Width
	DC.W	ScreenHeight		; Hight
	DC.W	3		; Depth
	DC.B	0,1		; Pen colors
	DC.W	0		; V_HIRES
	DC.W	SCREENQUIET	;CUSTOMSCREEN
	DC.L	FONT_ATTR	; use Topaz 8 as standard font
	DC.L	0 ;SCREEN_NAME
	DC.L	0
	DC.L	0

	
;***  Window structure  ***

WINDOW_DEFS:
	dc.w	0,0		; X-Y position
	dc.w	320		; Current width
	dc.w	ScreenHeight		; Current higth
	dc.b	0,1
	dc.l	RAWKEY+MOUSEBUTTONS		; Report only raw keys
	dc.l	BACKDROP+BORDERLESS+ACTIVATE+RMBTRAP
	dc.l	0	;Intuition Direct Communications Message Port
	dc.l	0
	DC.L	0	;REQUESTER_NAME	; Window name
SCREEN_HANDLE:
	dc.l	0	;custom screen pointer
	dc.l	0
	dc.w	320		; Min width 
	dc.w	ScreenHeight		; Min higth
	dc.w	320		; Max width
	dc.w	ScreenHeight		; Max higth
	dc.w	CUSTOMSCREEN	; A normal window
	EVEN

;---  Topaz font  ---

FONT_ATTR:
	DC.L	FONT_NAME	; Name
	DC.W	8		; Size
	DC.B	0
	DC.B	0
	DC.W	8		; Size

lightgreen: dc.w $0c0

COLORS:
	DC.W	$080,$ee0,$000,$FFF  ;green, yellow, black, white
	DC.W	$E00,$e0e,$0EE,$00e  ;red, magenta, cyan, blue

texts:	dc.b 'G%XY'

FONT_NAME:		DC.B	'topaz.font',0
CONSOLE_NAME:		DC.B	'console.device',0
INTUITION_NAME:		DC.B	'intuition.library',0
GRAPHICS_NAME:		DC.B	'graphics.library',0

     CNOP 0,4
CONSOLE_DEVICE:     DC.L	0
INTUITION_BASE:     DC.L	0
GRAPHICS_BASE:      DC.L	0
TASK_OLDWINDOW:     DC.L	0

BITPLANE1_PTR:      DC.L	0
BITPLANE2_PTR:      DC.L	0
BITPLANE3_PTR:      DC.L	0
TASK_PTR:           DC.L	0
ERROR_STACK:        DC.L	0

KEYB_BUFFER:		DCB.B	KB2_SIZE,0
KEYB_OUTBUFFER:		DC.W	0
KEYB_INBUFFER:		DC.W	0

IO_REQUEST:		DCB.B	32,0
KEY_BUFFER:		DCB.B	80,0
KEY_PORT:		DC.L	0
KEY_MSG:		DC.L	0
RASTER_PORT:		dc.l	0
VIEW_PORT:		dc.l 0
wbmsg: dc.l 0
MY_EVENT:	DC.L	0	; Insert after each event
EVENT_IECLASS:	DC.B	IECLASS_RAWKEY
		DC.B	0	; SUBCLASS - A Joke
IECODE:		DC.W	0	; RAWKEY - Inserted
IEQUAL:		DC.W	0	; QUALIFIER - SHIFT, CTRL, ETC.
IEADDR:		DC.L	0	; IAddress
		DC.L	0
		DC.L	0	; TimeStamp
WINDOW_HANDLE:	DC.L	0

         include "ramdata.s"

     CNOP 0,4
tiles:
         include "initiles.s"

     CNOP 0,4
iobseg:      dcb.b   8192,0
iobseg_end:

getkey:
	MOVE.W	KEYB_OUTBUFFER(A3),D0
	CMP.W	KEYB_INBUFFER(A3),D0	; Is buffer empty
	BNE	KEYB_STILLKEYSINBUFFER	; No ??
	BSR	KEYB_GETKEYS		; Empty, Wait on a key from
	BRA.S	getkey

getkey2:  ;******* KEY POLLING *******
	 MOVE.W	KEYB_OUTBUFFER(A3),D0
	 CMP.W	KEYB_INBUFFER(A3),D0	; Is buffer empty
	 BNE	KEYB_STILLKEYSINBUFFER	; No ??

	 MOVE.L	KEY_PORT(A3),A0	; Our key port
	 MOVE.L	4.W,A6
	 JSR	GetMsg(A6)
	 MOVE.L	D0,KEY_MSG(A3)   ;D0=0 means no message
	 BNE	KEYB_GETKEYS0
	 rts

start_timer:
         move.l $6c,interruptv(a3)
         clr.l timercnt(a3)
         move.l #rasteri,$6c
         rts

stop_timer:
         move.l interruptv(a3),$6c
         rts

dispatcher:
         bsr getkey2
         ;tst.b d0
         bne .e0
         rts

.e0:     cmp.b #"g",d0
         bne .c3

         ;;cmp [mode],0
         tst.b mode(a3)
         beq .c2

.c53:
         ;;dec [mode]
         subi.b #1,mode(a3)
         bra .c40

.c2:
         ;;inc [mode]
         addi.b #1,mode(a3)
.c40:    bra tograph

.c3:
         cmpi.b #"Q",d0
         bne .c5

         move.b #3,mode(a3)
.c101:   rts

.c5:     cmpi.b #"h",d0
         bne .c4

         cmpi.b #2,mode(a3)
         beq .c53

         moveq #2,d0
         move.b d0,mode(a3)
         bra clrscn
.c4:
         ;;cmp [mode],2
         cmpi.b #2,mode(a3)
         beq .c101

         cmpi.b #"T",d0
         bne .c6

         tst.b topology(A3)
         beq .c84

         bsr torus
         move.b #0,topology(a3)
         bra .c40

.c84:    bsr plain
         addq.b #1,topology(A3)
         bra tograph

.c6:     cmpi.b #'o',d0
         bne .c7

         tst.b mode(a3)
         bne .c101

         tst.w tilecnt(a3)
         bne .c108

         bsr incgen
         bra .c202

.c108:   bsr zerocc
         bsr generate
         bsr showscn
         bra cleanup

.c7:     cmpi.b #'?',d0
         bne .c8

         bra help

.c8:     cmpi.b #'C',d0
         bne .c10

         tst.w tilecnt(a3)
         bne .c201

         bsr zerogc
.c202:   bra infoout
.c201:   jmp clear

.c10:    cmpi.b #'E',d0
         bne .c11

         subq.b #1,pseudoc(a3)
         beq tograph

         move.b #1,pseudoc(a3)
         bra tograph

.c11:    cmpi.b #'!',d0
         bne .c12

         bsr random
         bra showscn

.c12:    cmpi.b #'%',d0
         bne .c14
         jmp indens

.c14:    cmpi.b #'B',d0
         bne .c15

.c159:   bsr insteps
         tst.w d6
         beq .c142

         move.w d6,x0(a3)
         move.w d6,temp2(a3)
         bsr inmode
         beq .c500
         bhi .c400

         bsr start_timer
.c146:   tst.w tilecnt(a3)
         bne .c147

         bsr incgen
         bra .c148

.c147:   bsr generate
         bsr cleanup
.c148:   subq.w #1,temp2(a3)
         bne .c146

.c401:   bsr benchcalc
.c142:   bsr tograph
         bra calccells

.c400:   bsr tograph
         bsr start_timer
.c5146:  tst.w tilecnt(a3)
         bne .c5147

         bsr incgen
         bra .c5148

.c5147:  bsr zerocc
         bsr generate
         bsr showscn
         bsr cleanup
.c5148:  subq.w #1,temp2(a3)
         bne .c5146
         bra .c401

.c500:   bsr tograph
         bsr start_timer
.c4147:  bsr showscn
         subq.w #1,temp2(a3)
         bne .c4147
         bra .c401

.c15:    cmpi.b #'R',d0
         bne .c16

         bsr inborn
         cmpi.b #27,d0         ;esc
         beq .c200

         lea.l born(a3),a2
         bsr setrconst
         bsr instay
         lea.l live(a3),a2
         bsr setrconst
         bsr fillrt
.c200:   bra tograph

.c16:    cmpi.b #' ',d0    ;space
         bne .c170

         ;;mov di,[crsrtile]
         movea.l crsrtile(a3),a5
         ;;mov [di+sum],al        ;always writes no-zero value
         move.b d0,(sum,a5)
         ;;call chkadd
         bsr chkadd
         ;;xor bx,bx
         ;;mov bl,[crsrbyte]
         moveq #0,d0
         move.b crsrbyte(a3),d0
          ;;add bx,di
         ;;mov al,[crsrbit]
         move.b crsrbit(a3),d1
         ;;mov ah,al
         ;;xor al,[bx]
         move.b (a5,d0),d2
         eor.b d1,d2
         ;;mov [bx],al
         move.b d2,(a5,d0)
         ;;test al,ah
         and.b d2,d1
         ;;jz .c79
         beq .c79

         moveq #1,d0
         bsr inctsum

         ;;call infoout
         bsr infoout
         ;;jmp .c270
         bra .c270

.c79:    bsr calccells
         jmp .c270

.c170:   cmpi.b #'.',d0
         bne .c171

         bsr crsrclr
         move.l #tiles+tilesize*(hormax*vermax/2+hormax/2-1),crsrtile(a3)
         moveq #1,d0
         move.b d0,crsrbyte(a3)
.c272:   move.b d0,crsrbit(a3)
         bsr .c270
         tst.b zoom(a3)
         beq .c100

         ;;call setviewport
         ;;call showscnz
.c270:   bsr crsrset
         jmp crsrcalc

.c171:   cmpi.b #"H",d0    ;home
         bne .c172

         bsr crsrclr
         move.l #tiles,crsrtile(a3)
         move.b #0,crsrbyte(a3)
         move.b #$80,d0
         bra .c272

.c172:   cmpi.b #'l',d0
         bne .c173

         move.b zoom(a3),d0
         move.w d0,-(sp)
         clr.b zoom(a3)
         bsr loadmenu
         bmi .c302

.c303:   bsr tograph
         bsr loadpat
.c302:   move.w (sp)+,d0
         move.b d0,zoom(a3)
         bsr calccells
         bra tograph

.c173:   cmpi.b #'L',d0
         bne .c174

         tst.b fn(a3)
         bne .c317
.c100:   rts

.c317:   move.b zoom(a3),d0
         move.w d0,-(sp)
         clr.b zoom(a3)
         bra .c303

.c174:   cmpi.b #'+',d0
         bne .c175

         ;;cmp [zoom],0
         ;;jnz .c100

         ;;inc [zoom]
         ;;call tograph
         ;;jmp .c270

.c175:   cmpi.b #'-',d0
         bne .c176

         ;;cmp [zoom],0
         ;;jz .c100

         ;;mov [zoom],0
         ;;jmp tograph

.c176:   cmpi.b #'V',d0
         bne .c177

         ;;jmp showcomm

.c177:   cmpi.b #'v',d0
         bne .c178
         ;;jmp infov

.c178:   cmpi.b #'Z',d0
         bne .c179

         ;;call totext
         ;;call chgcolors
.c220:   jmp tograph

.c179:   cmpi.b #'X',d0
         bne .c18

         ;;call totext
         ;;call setcolors
         ;;jmp .c220

.c18:    cmpi.b #'S',d0
         bne .c20

         ;bsr boxsz
         ;beq .c101

         ;;call getsvfn
         ;;jnz .c220

         ;;call savepat
         ;;jmp .c220

.c20:    cmpi.b #$9b,d0   ;extended keys
         bne .c100

.e1:     bsr getkey2
         cmpi.b #$43,d0   ;cursor right
         beq .c20cr

         cmpi.b #$20,d0
         bne .c160

         bsr getkey2
         cmpi.b #$40,d0   ;cursor right shifted
         bne .c160x

         ;;add [vptilecx],8
	 bsr crsrclr
         movea.l crsrtile(a3),a0
         movea.l (right,a0),a1
.csct2:  cmpa.l #plainbox,a1
         beq .c270
         bra .csct

.c20cr:  ;;inc [vptilecx]
         bsr crsrclr
         move.b crsrbit(a3),d0
         cmpi.b #1,d0
         beq .c71

         lsr.b d0
         move.b d0,crsrbit(a3)
         bra .c270

.c71:	 bsr crsrclr
         movea.l crsrtile(a3),a0
         movea.l (right,a0),a1
         cmpa.l #plainbox,a1
         beq .c270

         move.b #$80,crsrbit(a3)
         bra .csct

.c160:   cmpi.b #$44,d0   ;cursor left
         beq .c160cl

         cmpi.b #$20,d0
         bne .c161

         bsr getkey2
.c160x:  cmpi.b #$41,d0
         bne .c101

         ;;sub [vptilecx],8
	 bsr crsrclr
         movea.l crsrtile(a3),a0
         movea.l (left,a0),a1
         bra .csct2

.c160cl:  ;;inc [vptilecx]
         bsr crsrclr
         move.b crsrbit(a3),d0
         cmpi.b #$80,d0
         beq .c71x

         lsl.b d0
         move.b d0,crsrbit(a3)
         bra .c270

.c71x:	 bsr crsrclr
         movea.l crsrtile(a3),a0
         movea.l (left,a0),a1
         cmpa.l #plainbox,a1
         beq .c270

         move.b #1,crsrbit(a3)
         bra .csct

.c161:   cmpi.b #$41,d0  ;cursor up
         beq .c161cu

         cmpi.b #$54,d0  ;shifted
         bne .c162

	 ;;sub [vptilecy],8
	 bsr crsrclr
         movea.l crsrtile(a3),a0
         movea.l (up,a0),a1
         bra .csct2

.c161cu: ;;dec [vptilecy]
	 bsr crsrclr
         move.b crsrbyte(a3),d0
         beq .c71cu

         subq.b #1,crsrbyte(a3)
         bra .c270

.c71cu:  movea.l crsrtile(a3),a0
         movea.l (up,a0),a1
         cmpa.l #plainbox,a1
         beq .c270

         move.b #7,crsrbyte(a3)
         bra .csct

.c162:   cmpi.b #$42,d0  ;cursor down
         beq .c162cd

         cmpi.b #$53,d0  ;shifted
         bne .c101

	 ;;add [vptilecy],8
	 bsr crsrclr
         movea.l crsrtile(a3),a0
         movea.l (down,a0),a1
         bra .csct2

.c162cd: ;;inc [vptilecy]
         bsr crsrclr
         move.b crsrbyte(a3),d0
         cmpi.b #7,d0
         beq .c71cd

         addq.b #1,crsrbyte(a3)
         bra .c270

.c71cd:	 movea.l crsrtile(a3),a0
         movea.l (down,a0),a1
         cmpa.l #plainbox,a1
         beq .c270

         move.b #0,crsrbyte(a3)
.csct:   move.l a1,crsrtile(a3)
         bra .c270

benchcalc: bsr stop_timer
         move.l timercnt(a3),d5
         move.l d5,d3
         add.l d5,d5
         movea.l 4,a2
         cmp.b #50,VBlankFrequency(a2)
         beq .l8

         add.l d5,d5      ;60 Hz
         add.l d3,d5
         move d5,d3
         clr.w d5
         swap d5
         divu #3,d5
         swap d5
         swap d3
         move.w d5,d3
         swap d3
         divu #3,d3
         move.w d3,d5
         swap d3
         lsr.w #2,d3
         negx.l d5
         neg.l d5

.l8:     move.l d5,d7
         divu #100,d5
         swap d5
         lea tbformat(a3),a0
         lea temp(a3),a1
         move.l d5,(a1)
         lea stuffChar(pc),a2
         move.l #-1,charCount(a3)
         move.l a3,-(sp)
         lea stringbuf(a3),a3
         movea.l 4.w,a6
         jsr RawDoFmt(a6)
         move.l (sp)+,a3

         bsr totext
         move.l GRAPHICS_BASE(a3),a6 
         movea.l RASTER_PORT(a3),a1
         movepenq 0,6
         color 2
         move.l charCount(a3),d0
         lea stringbuf(a3),a0
         jsr Text(a6)

         move.w x0(a3),d5  ;*100
         mulu #10000,d5
         move.l d5,d6
         exg d6,d7
         tst.l d6
         bne .divok

         addq.l #1,d6
.divok:  clr.w d5
         swap d5
         divu d6,d5
         swap d5
         swap d7
         move.w d5,d7
         swap d7
         divu d6,d7
         move.w d7,d5
         swap d7
         add.w d7,d7
         sub.w d7,d6
         negx.l d5
         neg.l d5

         divu #100,d5
         swap d5
         move.l #-1,charCount(a3)
         lea sbformat(a3),a0
         lea temp(a3),a1
         move.l d5,(a1)
         lea stuffChar(pc),a2
         move.l a3,-(sp)
         lea stringbuf(a3),a3
         movea.l 4.w,a6
         jsr RawDoFmt(a6)
         move.l (sp)+,a3

         move.l GRAPHICS_BASE(a3),a6 
         movea.l RASTER_PORT(a3),a1
         movepenq 0,14
         color 2
         move.l charCount(a3),d0
         lea stringbuf(a3),a0
         jsr Text(a6)
         bra getkey

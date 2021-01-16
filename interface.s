getkey:
	move.w KEYB_OUTBUFFER(A3),D0
	cmp.w KEYB_INBUFFER(A3),D0	; Is buffer empty
	bne KEYB_STILLKEYSINBUFFER	; No ??
	bsr KEYB_GETKEYS		; Empty, Wait on a key from
	bra.s getkey

getkey2:  ;******* KEY POLLING *******  ;it changes D0-D4, A0-A2, A4, A6
     bsr KEYB_GETKEYS
	 move.w KEYB_OUTBUFFER(A3),D0
	 cmp.w KEYB_INBUFFER(A3),D0	; Is buffer empty?
	 bne KEYB_STILLKEYSINBUFFER	; No ??
     moveq #0,d0
     rts

getkey3:
     bsr getkey
.loop
     moveq #100,d0
.delay
     mulu #100,d1
     dbra d0,.delay
     bsr getkey2
     bne.s .loop
     rts

start_timer:
         move.l $6c,interruptv(a3)
         clr.l timercnt(a3)
         move.l #rasteri,$6c
         rts

stop_timer:
         move.l interruptv(a3),$6c
.rts:    rts

dispatcher:
         bsr getkey2
         ;tst.b d0
         bne.s .e0
.exit:   rts

.e0:     cmpi.b #mouseleft_char,d0     ;mouse left button
         bne .e01

.mouse:  bsr crsrclr
         movea.l SCREEN_HANDLE(a3),a0
         move.w 16(a0),d0   ;screen.y
         move.w d0,mouseprevY(a3)
         move.w 18(a0),d2   ;screen.x
         move.w d2,mouseprevX(a3)
         tst.b zoom(a3)
         beq.s .l11

         lsr.w #3,d0
         cmpi.w #24,d0
         bcc.s .exit

         move.b d0,vptilecy(a3)
         lsr.w #3,d2
         move.b d2,vptilecx(a3)
         move.l viewport(a3),d3
         sub.l #tiles,d3
         divu #tilesize,d3
         divu #hormax,d3
         lsl.w #3,d3
         add.w d3,d0
         swap d3
         lsl.w #3,d3
         add.w d3,d2
         bra.s .l12

.l11:    subq.w #2,d0
         bcs.s .exit

         cmpi.w #192,d0
         bcc.s .exit

         subq.w #2,d2
         bcs.s .exit

         cmpi.w #280,d2
         bcc.s .exit

         subi.w #32,d2
         bcs.s stop_timer\.rts

.l12:    move.w d2,mousesaveX(a3)
         move.w d0,mousesaveY(a3)
         bsr mousecursor
         move.w mousesaveX(a3),d2
         move.w mousesaveY(a3),d0
         bsr mousepixel
         tst.w mouseoldX(a3)    ;FIXME!! (0,0) is ok!
         bne.s .l20

         tst.w mouseoldY(a3)
         beq.s .l16

.l20:    move.w mouseoldX(a3),d7
         move.w mouseoldY(a3),d4
         move.w mousesaveX(a3),d5
         move.w mousesaveY(a3),d6
         bsr drawline
.l16:    bsr calccells
         move.w mousesaveX(a3),mouseoldX(a3)
         move.w mousesaveY(a3),mouseoldY(a3)
         move.b #0,mouseact(a3)
         bsr .c273
         move.b #1,mouseact(a3)
         bra showscn

.e01:    cmpi.b #mouseright_char,d0     ;mouse right button
         beq .mouse

         cmpi.b #"g",d0
         bne.s .c3

         tst.b mode(a3)
         beq.s .c2

.c53:    subi.b #1,mode(a3)
         bra .c40

.c2:     addi.b #1,mode(a3)
.c40:    bra tograph

.c3:     cmpi.b #"Q",d0
         bne.s .c5

         move.b #3,mode(a3)
.rts1:   rts

.c5:     cmpi.b #"h",d0
         bne.s .c4

         cmpi.b #2,mode(a3)
         beq.s .c53

         moveq #2,d0
         move.b d0,mode(a3)
         bra clrscn

.c4:     cmpi.b #2,mode(a3)
         beq.s .rts1

         cmpi.b #"T",d0
         bne.s .c6

         tst.b topology(A3)
         beq.s .c84

         bsr torus
         move.b #0,topology(a3)
         bra.s .c40

.c84:    bsr plain
         addq.b #1,topology(A3)
         bra tograph

.c6:     cmpi.b #'o',d0
         bne.s .c7

         tst.b mode(a3)
         bne.s .rts1

         tst.w tilecnt(a3)
         bne.s .c108

         bsr incgen
         bra .c202

.c108:   bsr zerocc
         bsr generate
         bsr showscn
         bra cleanup

.c7:     cmpi.b #'?',d0
         bne.s .c7a

.help:   bsr crsrclr
         bra help

.help0:  bsr getkey2
         cmpi.b #$7e,d0
         beq.s .help
         rts

.c7a:    cmpi.b #'C',d0
         bne.s .c10

         tst.w tilecnt(a3)
         bne.s .c201

         bsr zerogc
.c202:   bra infoout
.c201:   bra clear

.c10:    cmpi.b #'E',d0
         bne.s .c11

         subq.b #1,pseudoc(a3)
         beq tograph

         move.b #1,pseudoc(a3)
         bra tograph

.c11:    cmpi.b #'!',d0
         bne.s .c12

         bsr random
         bra showscn

.c12:    cmpi.b #'%',d0
         bne.s .c14

         bsr crsrclr
         bra indens

.c14:    cmpi.b #'B',d0
         bne .c15

         bsr crsrclr
         bsr insteps
         tst.w d6
         beq.s .c142

         move.w d6,x0(a3)
         move.w d6,temp2(a3)
         bsr inmode
         beq.s .c500
         bhi.s .c400

         bsr start_timer
.c146:   tst.w tilecnt(a3)
         bne.s .c147

         bsr incgen
         bra.s .c148

.c147:   bsr generate
         bsr cleanup
.c148:   subq.w #1,temp2(a3)
         bne.s .c146

.c401:   bsr crsrclr
         bsr benchcalc
.c142:   bsr tograph
         bra calccells

.c400:   bsr tograph
         bsr start_timer
.c5146:  tst.w tilecnt(a3)
         bne .c5147

         bsr incgen
         bra.s .c5148

.c5147:  bsr zerocc
         bsr generate
         bsr showscn
         bsr cleanup
.c5148:  subq.w #1,temp2(a3)
         bne.s .c5146
         bra.s .c401

.c500:   bsr tograph
         bsr start_timer
.c4147:  bsr showscn
         subq.w #1,temp2(a3)
         bne.s .c4147
         bra.s .c401

.c15:    cmpi.b #'R',d0
         bne.s .c16

         bsr crsrclr
         bsr inborn
         cmpi.b #27,d0         ;esc
         beq.s .c200

         lea.l born(a3),a2
         bsr setrconst
         bsr instay
         lea.l live(a3),a2
         bsr setrconst
         bsr fillrt
.c200:   bra tograph

.c16:    cmpi.b #' ',d0    ;space
         bne.s .c170

         ;;mov di,[crsrtile]
         movea.l crsrtile(a3),a5
         ;;mov [di+sum],al        ;always writes no-zero value
         move.b d0,(sum,a5)
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
         beq.s .c79

         moveq #1,d0
         bsr inctsum
.c270a:  bsr infoout
         bsr showscn
         bra.s .c270

.c79:    bsr calccells
         bra.s .c270a

.c170:   cmpi.b #'.',d0
         bne.s .c171

         bsr crsrclr
         move.l #tiles+tilesize*(hormax*vermax/2+hormax/2-1),crsrtile(a3)
         moveq #1,d0
         move.b d0,crsrbyte(a3)
.c272:   move.b d0,crsrbit(a3)
.c273:   bsr.s .c270
         tst.b zoom(a3)
         beq .rts

         bsr setviewport   ;FIXME!!
         bsr showscnz
.c270:   bsr crsrset
         bra crsrcalc

.c171:   cmpi.b #"H",d0    ;home
         bne.s .c172

         bsr crsrclr
         move.l #tiles,crsrtile(a3)
         move.b #0,crsrbyte(a3)
         move.b #$80,d0
         bra.s .c272

.c172:   cmpi.b #'l',d0
         bne.s .c173

         bsr crsrclr
         move.b zoom(a3),d0
         move.w d0,-(sp)
         clr.b zoom(a3)
         bsr loadmenu
         bmi.s .c302

.c303:   bsr tograph
         bsr loadpat
.c302:   move.w (sp)+,d0
         move.b d0,zoom(a3)
         bsr calccells
         bra tograph

.c173:   cmpi.b #'L',d0
         bne.s .c174

         tst.b fn(a3)
         bne.s .c317
.rts:    rts

.c317:   move.b zoom(a3),d0
         move.w d0,-(sp)
         clr.b zoom(a3)
         bra.s .c303

.c174:   cmpi.b #'+',d0
         bne.s .c175

         tst.b zoom(a3)
         bne.s .rts

         bsr crsrclr
         addq.b #1,zoom(a3)
         bsr setviewport
         bsr tograph
         bra .c270

.c175:   cmpi.b #'-',d0
         bne.s .c176

         tst.b zoom(a3)
         beq.s .rts

         bsr crsrclr
         clr.b zoom(a3)
         bra tograph

.c176:   cmpi.b #'V',d0
         beq showcomm

         cmpi.b #'v',d0
         beq infov

         cmpi.b #'Z',d0
         bne.s .c179

         bsr crsrclr
         bsr totext
         bsr chgcolors
.c220:   bra tograph

.c179:   cmpi.b #'X',d0
         bne .c18

         bsr totext
         bsr setcolors
         bra.s .c220

.c18:    cmpi.b #'S',d0
         bne.s .c20

         bsr boxsz
         beq .rts

         bsr crsrclr
         bsr getsvfn
         beq.s .c220

         bsr savepat
         bra.s .c220

.c20:    cmpi.b #$9b,d0   ;extended keys
         bne.w .rts

.e1:     bsr getkey2
         cmpi.b #$3f,d0   ;help key
         beq .help0

         cmpi.b #$43,d0   ;cursor right
         beq.s .c20cr

         cmpi.b #$20,d0
         bne.s .c160

         bsr getkey2
         cmpi.b #$40,d0   ;cursor right shifted
         bne.s .c160x

         addq.b #8,vptilecx(a3)
         bsr crsrclr
         movea.l crsrtile(a3),a0
         movea.l (right,a0),a1
.csct2:  cmpa.l #plainbox,a1
         beq .c270
         bra .csct

.c20cr:  addq.b #1,vptilecx(a3)
         bsr crsrclr
         move.b crsrbit(a3),d0
         cmpi.b #1,d0
         beq.s .c71

         lsr.b d0
         move.b d0,crsrbit(a3)
         bra .c270

.c71:    movea.l crsrtile(a3),a0
         movea.l (right,a0),a1
         cmpa.l #plainbox,a1
         beq .c270

         move.b #$80,crsrbit(a3)
         bra .csct

.c160:   cmpi.b #$44,d0   ;cursor left
         beq.s .c160cl

         cmpi.b #$20,d0
         bne .c161

         bsr getkey2
.c160x:  cmpi.b #$41,d0
         bne .rts

         subq.b #8,vptilecx(a3)
         bsr crsrclr
         movea.l crsrtile(a3),a0
         movea.l (left,a0),a1
         bra.s .csct2

.c160cl: subq.b #1,vptilecx(a3)
         bsr crsrclr
         move.b crsrbit(a3),d0
         cmpi.b #$80,d0
         beq.s .c71x

         lsl.b d0
         move.b d0,crsrbit(a3)
         bra .c270

.c71x:   movea.l crsrtile(a3),a0
         movea.l (left,a0),a1
         cmpa.l #plainbox,a1
         beq .c270

         move.b #1,crsrbit(a3)
         bra .csct

.c161:   cmpi.b #$41,d0  ;cursor up
         beq.s .c161cu

         cmpi.b #$54,d0  ;shifted
         bne.s .c162

	     subq.b #8,vptilecy(a3)
         bsr crsrclr
         movea.l crsrtile(a3),a0
         movea.l (up,a0),a1
         bra .csct2

.c161cu: subq.b #1,vptilecy(a3)
         bsr crsrclr
         move.b crsrbyte(a3),d0
         beq.s .c71cu

         subq.b #1,crsrbyte(a3)
         bra .c270

.c71cu:  movea.l crsrtile(a3),a0
         movea.l (up,a0),a1
         cmpa.l #plainbox,a1
         beq .c270

         move.b #7,crsrbyte(a3)
         bra.s .csct

.c162:   cmpi.b #$42,d0  ;cursor down
         beq.s .c162cd

         cmpi.b #$53,d0  ;shifted
         bne .rts

	     addq.b #8,vptilecy(a3)
         bsr crsrclr
         movea.l crsrtile(a3),a0
         movea.l (down,a0),a1
         bra .csct2

.c162cd: addq.b #1,vptilecy(a3)
         bsr crsrclr
         move.b crsrbyte(a3),d0
         cmpi.b #7,d0
         beq.s .c71cd

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
         bra getkey3

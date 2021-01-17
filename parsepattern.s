stackchecking = 1

   if stackchecking
checkstack macro
     move.l stacklimit(a3),a0
     lea.l \1(a0),a0
     cmpa.l a0,sp
     bcs error_exit
endm

error_exit:
     move.l #$aa00aa00,sp
     moveq #1,d4
     moveq #0,d0
     rts
   else
checkstack macro
endm
   endif

ucase:cmpi.b #'a',d2
      bcs .l1

      cmpi.b #'z'+1,d2
      bcc .l1

      sub.b #'a'-'A',d2
.l1:  rts

nextpat:     ;patpos = a4, result = a2
     move.l a4,-(sp)
     suba.l a2,a2
.loop:
     tst.b (a4)
     beq .exit

     cmp.b #')',(a4)+
     bne .loop

     move.l a4,a2
.exit:
     move.l (sp)+,a4
     rts

nextpat2:     ;patpos = a4, result = a2
     moveq #0,d0
     move.l a4,-(sp)
     suba.l a2,a2
.loop:
     cmp.b #'|',(a4)
     bne.s .l1

     tst.w d0
     beq.s .res

.l1: tst.b (a4)
     beq.s nextpat\.exit

     cmp.b #'(',(a4)
     bne.s .l2

     addq.w #1,d0
.l2: cmp.b #')',(a4)+
     bne.s .loop

     subq.w #1,d0
     bra.s .loop
.res:
     lea.l 1(a4),a2
     bra.s nextpat\.exit

length:   ;patpos = a4, result = d0
     move.l a4,-(sp)
.loop:
     cmp.b #'|',(a4)
     beq .exit

     cmp.b #')',(a4)+
     bne .loop

     subq.l #1,a4
.exit:
     suba.l (sp),a4
     move.l a4,d0
     move.l (sp)+,a4
     rts

multitude:
     move.l a4,a1
     clr.l d0
     cmpa.l d0,a2
     bne.s .l1
.exit:
     rts

.l1: checkstack 16
     movem.l a2/a4/a5,-(sp)
     move.l a2,a4
     bsr parse
     movem.l (sp)+,a2/a4/a5
     tst.l d0
     bne .exit

     movea.l a4,a1   ;t
.loop4:
     bsr length
     cmpi.b #1,d0
     bne.s .l8

     cmp.b #'%',(a4)
     beq.s .l13

.l8: clr.l d1
.loop5:
     cmp.w d0,d1
     beq.s .l10

     move.b (a5,d1.w),d2
     bsr ucase
     move.b d2,d3
     move.b (a4,d1.w),d2
     bsr ucase
     cmp.b d2,d3
     beq .l12

.l13:lea.l 1(a4,d0.w),a4
     bra .l11

.l12:addq.l #1,d1
     bra .loop5

.l10:checkstack 20
     lea.l 1(a4,d0.w),a4
     movem.l a1/a2/a4/a5,-(sp)
     move.l a1,a4
     lea.l (a5,d0.w),a5
     bsr multitude
     movem.l (sp)+,a1/a2/a4/a5
     tst.l d0
     bne .exit

.l11:cmpa.l a2,a4
     beq exit0
     bra .loop4

parse500:  ;patpos = a4, datapos = a5, result = d0 (0 - fail, 1 - success)
        ;it can also use a value of global variable `stacklimit' if stack checking is enabled
        ;it changes d0-d3, a0-a2
        ;d4 is changed only if stack checking is enabled
        ;d4 is -1 if parse got a failure due to the small stack size
        ;d4 is 0 otherwise
  if stackchecking
     moveq #0,d4
     move.l sp,error_exit+2
  endif
     checkstack 8
.loop:
     movem.l a4/a5,-(sp)
     bsr.s parse
     movem.l (sp)+,a4/a5
     tst.l d0
     bne.s parse\.exit

     bsr nextpat2
     move.l a2,d0
     movea.l a2,a4
     bne.s .loop
     rts

parse:
.loop:
     cmpi.b #'|',(a4)
     beq.s .c1

     tst.b (a4)
     bne.s .l1

.c1: tst.b (a5)
     bne exit0

     moveq #1,d0
.exit:
     rts

.l1: cmp.b #'#',(a4)
     bne .l2

     addq.l #1,a4
     tst.b (a4)
     beq exit0

     cmp.b #'%',(a4)
     bne .l3

     addq.l #1,a4
     bra .loop

.l3: cmp.b #'?',(a4)
     bne .l4

     addq.l #1,a4
.loop2:
     checkstack 12
     movem.l a4/a5,-(sp)
     bsr parse
     movem.l (sp)+,a4/a5
     tst.l d0
     bne .exit

     tst.b (a5)+
     bne .loop2
     bra .exit

.l4: cmp.b #'(',(a4)
     bne .loop3

     bsr nextpat      ;lastpos=a2
     addq.l #1,a4
     bra multitude
.loop3:
     checkstack 12
     movem.l a4/a5,-(sp)
     addq.l #1,a4
     bsr parse
     movem.l (sp)+,a4/a5
     tst.l d0
     bne .exit

     move.b (a4),d2
     bsr ucase
     move.b d2,d3
     move.b (a5)+,d2
     bsr ucase
     cmp.b d2,d3
     beq .loop3
     bra .exit

.l2: cmp.b #'%',(a4)
     bne .l6

     addq.l #1,a4
     bra .loop

.l6: cmp.b #'(',(a4)
     bne .l7

     addq.l #1,a4
     bsr nextpat      ;lastpos=a2
     clr.l d0
     cmpa.l d0,a2
     beq .exit
.loop4:
     bsr length
     cmpi.b #1,d0
     bne .l8

     cmp.b #'%',(a4)
     bne .l8

     checkstack 18
     move.w d0,-(sp)
     movem.l a2/a4/a5,-(sp)
     movea.l a2,a4
     bsr parse
     movem.l (sp)+,a2/a4/a5
     move.w (sp)+,d1
     tst.l d0
     bne .exit

     move.w d1,d0
.l8: clr.l d1
.loop5:
     cmp.w d0,d1
     beq .l10

     move.b (a5,d1.w),d2
     bsr ucase
     move.b d2,d3
     move.b (a4,d1.w),d2
     bsr ucase
     cmp.b d2,d3
     beq .l12

     lea.l 1(a4,d0.w),a4
     bra .l11

.l12:addq.l #1,d1
     bra .loop5

.l10:checkstack 16
     lea.l 1(a4,d0.w),a4
     movem.l a2/a4/a5,-(sp)
     movea.l a2,a4
     lea.l (a5,d0.w),a5
     bsr parse
     movem.l (sp)+,a2/a4/a5
     tst.l d0
     bne .exit

.l11:cmpa.l a2,a4
     beq exit0
     bra .loop4

.l7: move.b (a5)+,d2
     move.b (a4)+,d1
     cmp.b #'?',d1
     beq .l14

     bsr ucase
     move.b d2,d3
     move.b d1,d2
     bsr ucase
     cmp.b d2,d3
     beq .loop
     bne exit0

.l14:tst.b d2
     bne .loop
exit0:
     clr.l d0
     rts


ucase:cmpi.b #'a',d2
      bcs .l1

      cmpi.b #'z'+1,d2
      bcc .l1

      sub.b #'a'-'A',d2
.l1:  rts

next:     ;patpos = a4, result = a2
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
     bne .l1
.exit:
     rts

.l1: movem.l a2/a4/a5,-(sp)
     move.l a2,a4
     bsr parse
     movem.l (sp)+,a2/a4/a5
     tst.l d0
     bne .exit

     movea.l a4,a1   ;t
.loop4:
     bsr length
     cmpi.b #1,d0
     bne .l8

     cmp.b #'%',(a4)
     beq .l13

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

.l13:lea.l 1(a4,d0.w),a4
     bra .l11

.l12:addq.l #1,d1
     bra .loop5

.l10:lea.l 1(a4,d0.w),a4
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

parse:  ;patpos = a4, datapos = a5, result = d0
.loop:
     tst.b (a4)
     bne .l1

     tst.b (a5)
     bne exit0

     moveq.l #1,d0
.exit:
     rts

.l1: cmp.b #'#',(a4)
     bne .l2

     addq.l #1,a4
     cmp.b #'%',(a4)
     bne .l3

     addq.l #1,a4
     bra .loop

.l3: cmp.b #'?',(a4)
     bne .l4

     addq.l #1,a4
.loop2:
     move.l a5,-(sp)
     move.l a4,-(sp)
     bsr parse
     move.l (sp)+,a4
     move.l (sp)+,a5
     tst.l d0
     bne .exit

     tst.b (a5)+
     bne .loop2
     bra .exit

.l4: cmp.b #'(',(a4)
     bne .loop3

     bsr next      ;lastpos=a2
     addq.l #1,a4
     bsr multitude
     bra .exit
.loop3:
     move.l a5,-(sp)
     move.l a4,-(sp)
     addq.l #1,a4
     bsr parse
     move.l (sp)+,a4
     move.l (sp)+,a5
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
     bsr next      ;lastpos=a2
     clr.l d0
     cmpa.l d0,a2
     beq .exit
.loop4:
     bsr length
     cmpi.b #1,d0
     bne .l8

     cmp.b #'%',(a4)
     bne .l8

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

.l10:lea.l 1(a4,d0.w),a4
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
     beq .loop

     bsr ucase
     move.b d2,d3
     move.b d1,d2
     bsr ucase
     cmp.b d2,d3
     beq .loop

exit0:
     clr.l d0
     rts


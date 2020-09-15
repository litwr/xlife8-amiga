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
     movea.l 12(sp),a2   ;lastpos
     clr.l d0
     cmpa.l d0,a2
     bne .l1
.exit:
     rts

.l1: movea.l 8(sp),a5   ;datapos
     move.l a5,-(sp)
     move.l a2,-(sp)
     bsr parse
     addq.l #8,sp
     tst.l d0
     bne .exit

     movea.l 4(sp),a4   ;t
.loop4:
     bsr length
     cmpi.b #1,d0
     bne .l8

     cmp.b #'%',(a4)
     beq .l11

.l8: clr.l d1
.loop5:
     cmp.w d0,d1
     beq .l10

     move.b (a5,d1.w),d2
     cmp.b (a4,d1.w),d2
     bne .l11

     addq.l #1,d1
     bra .loop5

.l10:move.w d0,-(sp)

     move.w d0,-(sp)
     move.l a4,-(sp)
     movea.l 4(sp),a0
     move.l a0,-(sp)
     lea.l (a5,d0.w),a0
     move.l a0,-(sp)
     move.l a2,-(sp)
     bsr multitude
     move.l (sp)+,a2
     move.l (sp)+,a5
     move.l (sp)+,a4
     move.l (sp)+,a4
     move.w (sp)+,a0
     movea.l 8(sp),a5   ;datapos
     tst.l d0
     bne .exit

     suba.l a0,a5
.l11:lea.l (a4,d0.w),a4
     addq.l #1,a4
     cmpa.l a2,a4
  if 0
     beq .exit0
     bra .loop4
  endif
parse:  ;patpos = [sp-8], datapos = [sp-4], result = d0
     movea.l 4(sp),a4   ;patpos
     movea.l 8(sp),a5   ;datapos
.loop:
     tst.b (a4)
     bne .l1

     tst.b (a5)
     bne .exit0

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
     bne .l5

     bsr next      ;lastpos=a2
     move.l a2,-(sp)
     move.l a5,-(sp)
     lea.l 1(a4),a0
     move.l a0,-(sp)
     bsr multitude
     lea.l 12(sp),sp
     bra .exit
.l5:
     addq.l #1,a4
.loop3:
     move.l a5,-(sp)
     move.l a4,-(sp)
     bsr parse
     move.l (sp)+,a4
     move.l (sp)+,a5
     tst.l d0
     bne .exit

     move.b -1(a4),d1
     cmp.b (a5)+,d1
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
     move.l a4,-(sp)
     move.l a5,-(sp)
     move.l a2,-(sp)
     bsr parse
     move.l (sp)+,a2
     move.l (sp)+,a5
     move.l (sp)+,a4
     move.w (sp)+,d1
     tst.l d0
     bne .exit

     move.w d1,d0
.l8: clr.l d1
.loop5:
     cmp.w d0,d1
     beq .l10

     move.b (a5,d1.w),d2
     cmp.b (a4,d1.w),d2
     bne .l11

     addq.l #1,d1
     bra .loop5

.l10:lea.l (a5,d0.w),a0
     move.w d0,-(sp)
     move.l a4,-(sp)
     move.l a0,-(sp)
     move.l a2,-(sp)
     bsr parse
     move.l (sp)+,a2
     move.l (sp)+,a5
     move.l (sp)+,a4
     move.w (sp)+,a0
     tst.l d0
     bne .exit

     suba.l a0,a5
     move.w a0,d0
.l11:lea.l (a4,d0.w),a4
     addq.l #1,a4
     cmpa.l a2,a4
     beq .exit0
     bra .loop4

.l7: move.b (a5)+,d0
     move.b (a4)+,d1
     cmp.b #'?',d1
     beq .loop

     cmp.b d1,d0
     beq .loop

.exit0:
     clr.l d0
     rts


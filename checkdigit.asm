	 include "amiga.mac"
bufsz = 64
         moveq #0,d7
         cmpi.l #1,d0		; more than 1 arg?
         beq.s .exit

         move.b (a0),d6
         movea.l 4.w,a6
         lea dosname(pc),a1
         jsr OldOpenLibrary(a6)
         move.l d0,doslib
         movea.l d0,a6

         move.l #fn,d1
         move.l #MODE_OLD,d2
         jsr Open(a6)
         tst.l d0
         beq.s .exit

         move.l d0,filehl
         move.l #string,d2
         move.l d0,d1
         moveq.l #1,d3
         jsr Read(a6)
         cmpi.l #1,d3
         bne.s .close

         cmp.b string(pc),d6
         bne.s .close

         moveq #5,d7
.close:  move.l filehl(pc),d1
         jsr Close(a6)
.exit:   move.l doslib(pc),a1
         movea.l 4.w,a6
         jsr CloseLibrary(a6)
         move.l d7,d0
         rts

doslib dc.l 0
filehl dc.l 0
dosname  dc.b "dos.library",0
fn dc.b "t:gotline",0
string blk.b bufsz


	 include "amiga.mac"
bufsz = 64

         movea.l 4.w,a6
         lea dosname(pc),a1
         jsr OldOpenLibrary(a6)
         move.l d0,doslib
         movea.l d0,a6

         jsr Input(a6)          ;get stdin
         move.l #string,d2
         move.l d0,d1
         moveq.l #bufsz,d3
         jsr Read(a6)

         move.l #fn,d1
         move.l #MODE_NEWFILE,d2
         jsr Open(a6)
         tst.l d0
         beq .exit

         move.l d0,filehl
         move.l #string,d2
         move.l d0,d1
         moveq.l #1,d3
         jsr Write(a6)

         move.l filehl(pc),d1
         jsr Close(a6)
.exit:   move.l doslib(pc),a1
         movea.l 4.w,a6
         jmp CloseLibrary(a6)

doslib dc.l 0
filehl dc.l 0
dosname  dc.b "dos.library",0
fn dc.b "t:gotline",0
string blk.b bufsz


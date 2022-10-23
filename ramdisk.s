ramdisk: bsr totext
         move.l GRAPHICS_BASE(a3),a6 
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,6
         color 2  ;green
         print 'ENTER FILE# OR HIT '
         color 1 ;red
         print 'ESC'
         movepenq 32,14
         print '0'
         color 3
         print ' GLIDER GUN'
         movepenq 32,22
         color 1
         print '1'
         color 3
         print ' SMALL FISH'
         movepenq 32,30
         color 1
         print '2'
         color 3
         print ' HEAVYWEIGHT SPACESHIP'
         movepenq 32,38
         color 1
         print '3'
         color 3
         print ' R-PENTOMINO'
         movepenq 32,46
         color 1
         print '4'
         color 3
         print ' BUNNIES'
         movepenq 32,54
         color 1
         print '5'
         color 3
         print ' LIDKA'
         movepenq 32,62
         color 1
         print '6'
         color 3
         print ' BIG GLIDER'
         movepenq 32,70
         color 1
         print '7'
         color 3
         print ' BI-GUN'
         movepenq 32,78
         color 1
         print '8'
         color 3
         print ' ACORN'
         movepenq 32,86
         color 1
         print '9'
         color 3
         print ' SWITCH ENGINE PUFFER'
.c1:     bsr getkey2
         cmpi.b #27,d0    ;esc
         beq pexit

         cmpi.b #'0',d0
         bcs .c1

         cmpi #'9'+1,d0
         bcc .c1

         sub.b #'0',d0
         lsl #2,d0
         add.l #ramptrs,d0
         move.l d0,a1
         move.l (a1),a1
         move.w (a1)+,x0(a3)   ;geometry
         bsr maketent
         bsr tograph
         bsr showrect
         cmpi.b #27,d0
         beq pexit

puttent: move.l tsz(a3),d1
         lea.l iobseg,a2
.loop:   or.l d1,d1
         beq pexit

         move.w (a2)+,x0(a3)
         subq.l #1,d1
         bsr putpixel
         bra .loop

maketent:   ;in: a1
         clr.l d0
         move.w (a1)+,d0
         subq.l #8,d0
         lsr.l #1,d0
         move.l d0,tsz(a3)
         lea.l iobseg,a2
.loop:   move.w (a1)+,(a2)+
         subq.l #1,d0
         bne .loop
pexit:   rts

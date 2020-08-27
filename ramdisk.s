ramdisk: bsr totext
         move.l GRAPHICS_BASE(a3),a6 
         ;movea.l RASTER_PORT(a3),a1
         movepenq 0,8
         color 2  ;green
         print 'ENTER FILE# OR HIT '
         color 1 ;red
         print 'ESC'
         movepenq 32,16
         print '0'
         color 3
         print ' GLIDER GUN'
         movepenq 32,24
         color 1
         print '1'
         color 3
         print ' SMALL FISH'
         movepenq 32,32
         color 1
         print '2'
         color 3
         print ' HEAVYWEIGHT SPACESHIP'
         movepenq 32,40
         color 1
         print '3'
         color 3
         print ' R-PENTOMINO'
         movepenq 32,48
         color 1
         print '4'
         color 3
         print ' BUNNIES'
         movepenq 32,56
         color 1
         print '5'
         color 3
         print ' LIDKA'
         movepenq 32,64
         color 1
         print '6'
         color 3
         print ' BIG GLIDER'
         movepenq 32,72
         color 1
         print '7'
         color 3
         print ' BI-GUN'
         movepenq 32,80
         color 1
         print '8'
         color 3
         print ' ACORN'
         movepenq 32,88
         color 1
         print '9'
         color 3
         print ' SWITCH ENGINE PUFFER'
.c1:     jsr getkey2
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
         ;;call tograph
         ;;call showrect
         ;;jc pexit

puttent: move.w tsz(a3),d1
         lea.l iobseg,a2
.loop:   or.w d1,d1
         beq pexit

         move.w (a2)+,x0(a3)
         subq.w #1,d1
         bsr putpixel
         bra .loop

maketent:   ;in: a1
         move.w (a1)+,d0
         subq #8,d0
         lsr.w #1,d0
         move.w d0,tsz(a3)
         lea.l iobseg,a2
.loop:   move.w (a1)+,(a2)+
         subq.w #1,d0
         bne .loop
pexit:   rts

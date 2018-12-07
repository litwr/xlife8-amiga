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

dispatcher:
	 ;;call getkey2
         bsr getkey2
         ;tst.b d0
         bne .e0
         rts
.e0:
         ;;cmpb #'g,r0
         ;;cmp al,'g'
         cmp.b #"g",d0

         ;;bne 3$
         ;;jnz .c3
         bne .c3

         ;;cmp [mode],0
         tst.b mode(a3)
         ;;jz .c2
         beq .c2

.c53:    
         ;;dec [mode]
         subi.b #1,mode(a3)
         ;;jmp .c40
         bra .c40

.c2:     
         ;;inc [mode]
         addi.b #1,mode(a3)
.c40:    bra tograph

.c3:     
         ;;cmp al,'Q'
         cmpi.b #"Q",d0
         ;;jnz .c5
         bne .c5

         ;;mov [mode],3
         move.b #3,mode(a3)
.c101:   
         ;;retn
         rts

.c5:
         ;;cmp al,'h'
         cmpi.b #"h",d0
         ;;jnz .c4
         bne .c4

         ;;cmp [mode],2
         cmpi.b #2,mode(a3)
         ;;jz .c53
         beq .c53

         moveq #2,d0
         move.b d0,mode(a3)
         bra clrscn
.c4:     
         ;;cmp [mode],2
         cmpi.b #2,mode(a3)
         ;;je .c101
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

         ;;call random
         bra showscn

.c12:    cmpi.b #'%',d0
         bne .c14

         ;jmp indens

.c14:    cmpi.b #'B',d0
         bne .c15

;;.c159:   call insteps
         ;;mov ax,[temp2]
         ;;or ax,ax
         ;;jz .c142

         ;;mov word [x0],ax
         ;;call inmode
         ;;js .c402
         ;;jnz .c400
         ;;jmp .c500

;;.c402:   call start_timer
;;.c146:   cmp [tilecnt],0
         ;;jnz .c147

         ;;call incgen
         ;;jmp .c148

;;.c147:   call generate
;;         call cleanup
;;.c148:   dec word [x0]
;;         jnz .c146

;;.c401:   call benchcalc
;;.c142:   call tograph
         ;;jmp calccells

;;.c400:   call tograph
         ;;call start_timer

;;.c5146:  cmp [tilecnt],0
         ;;jnz .c5147

         ;;call incgen
         ;;jmp .c5148

;;.c5147:  call zerocc
;;         call generate
         ;;call showscn
         ;;call cleanup
;;.c5148:  dec word [x0]
         ;;jnz .c5146
         ;;jmp .c401

;;.c500:   call tograph
         ;;call start_timer
;;.c4147:  call showscn
         ;;dec word [x0]
         ;;jnz .c4147
         ;;jmp .c401

.c15:    cmpi.b #'R',d0
         bne .c16

         ;;call totext
         ;;call inborn
         ;;cmp al,27         ;esc
         ;;jz .c200

         ;;mov bx,born
         ;;call setrconst
         ;;call instay
         ;;mov bx,live
         ;;call setrconst
         ;;call fillrt
;;.c200:   jmp tograph

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

         ;;mov al,[zoom]
         ;;push ax
         ;;mov [zoom],0
         ;;call loadmenu
         ;;jnz .c302

;;.c303:   call tograph
         ;;call loadpat
;;.c302:   pop ax
         ;;mov [zoom],al
         ;;call calccells
         ;;jmp tograph

.c173:   cmpi.b #'L',d0
         bne .c174

         ;;cmp [fn],0
         ;;jne .c317
.c100:   rts

;;.c317:   mov al,[zoom]
         ;;push ax
         ;;mov [zoom],0
         ;;jmp .c303

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
;;.c220:   jmp tograph

.c179:   cmpi.b #'X',d0
         bne .c18

         ;;call totext
         ;;call setcolors
         ;;jmp .c220

.c18:    cmpi.b #'S',d0
         bne .c20

         ;;call boxsz
         ;;je .c20

         ;;call getsvfn
         ;;jnz .c220

         ;;call savepat
         ;;jmp .c220

.c20:    cmpi.b #$9b,d0   ;extended keys
         bne .c100

         bsr getkey2
         cmpi.b #$43,d0   ;cursor right
         beq .c20cr

         cmpi.b #$20,d0
         bne .c160

         bsr getkey2
         cmpi.b #$40,d0
         bne .c160x

         ;;add [vptilecx],8
	 bsr crsrclr
         movea.l crsrtile(a3),a0
         movea.l (right,a0),a1
.csct2:  cmpa.l #plainbox,a1
         beq .c270
         bra .csct

.c20cr:  ;;inc [vptilecx]
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

   if 0
benchcalc: call stop_timer
         mov ax,20480    ;=4096*5=TIMERV*5
         mul [timercnt]
         mov cx,59659    ;=1193180/20
         div cx
         inc ax
         shr cx,1
         cmp dx,cx
         jbe .c143

         inc ax
.c143:   push ax
         xor dx,dx
         call todec      ;takes centiseconds in ds:ax
         call totext
         call printstr
         db 'TIME: $'
         call printfloat
         mov ax,[temp2]
         mov cx,10000
         mul cx
         pop si
         or si,si
         jz .c143x

.c143b:  mov cx,ax
         mov ax,dx
         xor dx,dx
         div si
         xchg ax,cx
         div si
         shr si,1
         cmp dx,si
         jb .c143a

         inc ax
.c143a:  mov dx,cx
         call todec
         call printstr
         db 's',0dh,10,'SPEED: $'
         call printfloat
         call printstr
         db black,'$'
.c143x:  call curoff
         jmp getkey
   endif


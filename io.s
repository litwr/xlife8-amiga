   if 0
copyr:   call printstr
         db ansiclrscn,green,'$'

         mov ax,3d00h
         mov dx,copyleft
         int 21h
         jc readtent.e1

         mov bx,ax
.loop:   mov ah,3fh   ;read file
         mov cx,1
         mov dx,x0
         int 21h
         jc .exit

         mov dl,[x0]
         cmp dl,26
         jz .exit

         mov cx,2000
.delay:  mov ah,2
         loop .delay
         int 21h
         jmp .loop

.exit:   call getkey
         jmp setcolors.e1

setcolors:mov ax,3d00h
         mov dx,cf
         int 21h
         jc readtent.e1

         mov bx,ax
         mov ah,3fh   ;read file
         mov cx,7
         mov dx,palette
         int 21h
.e1:     jmp loadpat.e1

savecf:  mov ah,3ch   ;create a file
         mov dx,cf
         xor cx,cx
         int 21h
         jc readtent.e1

         mov bx,ax
         mov ah,40h   ;write
         mov cx,7
         mov dx,palette
         int 21h
         jmp loadpat.e1

readtent:mov ah,3fh   ;read file
         mov bx,[filehl]
         mov cx,3072
         xor dx,dx
         push ds
         mov ds,[iobseg]
         int 21h
         pop ds
         shr ax,1
         mov [tsz],ax
         sub [filesz],ax
.e1:     retn

loadpat: mov ax,3d00h
         mov dx,fn
         int 21h
         jc readtent.e1

         mov [filehl],ax
         mov bx,ax
         mov ax,4202h   ;fseek
         xor cx,cx
         xor dx,dx
         int 21h
         shr dx,1
         rcr ax,1
         or dx,dx
         jnz .exit2

         sub ax,3
         jbe .exit2

         mov [filesz],ax
         mov ax,4200h
         int 21h
         mov si,[live]
         mov di,[born]
         mov ah,3fh   ;read file
         mov cx,6
         mov dx,x0
         int 21h
         mov al,byte [live+1]
         or al,byte [born+1]
         cmp al,2
         jnc .l1

         test byte [born],1
         jz .l2

.l1:     mov [live],si
         mov [born],di
         jmp .exit2

.l2:     push si
         push di
         call readtent   ;should adjust filesz
         call showrect
         pop di
         pop si
         jc .l1

         call fillrt
         call puttent
         cmp [filesz],0
         je .exit2

.l3:     mov ah,3fh
         mov bx,[filehl]
         mov cx,2
         mov dx,x0
         int 21h
         call putpixel
         dec [filesz]
         jnz .l3

.exit2:  mov bx,[filehl]
.e1:     mov ah,3eh    ;fclose
         int 21h
         retn

printbp: mov dl,' '
         cmp bp,10
         jnc .l1

         mov ah,2
         int 21h
.l1:     cmp bp,100
         jnc .l2

         mov ah,2
         int 21h
.l2:     mov ax,bp
.ee:     xor dx,dx
.ee1:    call todec
         lea si,[stringbuf+bx]
.l3:     mov dl,[si]
         dec si
         mov ah,2
         int 21h
         dec bx
         jnz .l3
         retn
    endif
showdir: bsr totext
    moveq #80,d6
    rts
    if 0
         call printstr   ;OUT: BP
         db ansiclrscn,10,'$'

         xor bp,bp
         mov dx,svfn
         mov cx,20h
         mov ah,4eh
         int 21h
         jc .exit

.l3:     call printstr
         db red,'$'
         call printbp
         call printstr
         db black,' $'
         mov di,80h ;dta
         push di
.l2:     mov dl,[di+1eh] ;fn offset in DTA
         inc di
         cmp dl,'.'
         jz .l1

         mov ah,2
         int 21h
         jmp .l2

.l1:     call printstr
         db ' ',blue,'$'
         pop di
         mov ax,[di+1ah]  ;size offset in DTA
         call printbp.ee
         call printstr
         db green,'$'
         test bp,1
         jnz .l4

         mov ah,3
         xor bx,bx
         int 10h
         dec ah
         mov dl,20
         int 10h
         jmp .l5

.l4:     call printstr
         db 0dh,10,'$'
.l5:     inc bp
         cmp bp,1000
         jz .exit

         mov ah,4fh
         mov dx,80h   ;dta
         int 21h
         jnc .l3

.exit:   test bp,1
         jz showfree

         call printstr
         db 0dh,10,'$'
showfree:mov ah,36h     ;after showdir
         xor dx,dx
         int 21h
         mul cx
         mov cx,10    ;1024
.l1:     shr dx,1
         rcr ax,1
         loop .l1
         mul bx
         call printbp.ee1
         call printstr
         db 'K free',black,'$'

         retn

findfn:  xor bp,bp          ;in: ax
         mov di,ax
         mov dx,svfn
         mov cx,20h
         mov ah,4eh
         int 21h
.l3:     cmp di,bp
         jz .l7

         inc bp
         mov ah,4fh
         mov dx,80h
         int 21h
         jmp .l3

.l7:     mov di,80h ;dta
         mov si,fn
.l2:     mov al,[di+1eh] ;fn offset in DTA
         inc di
         mov [si],al
         inc si
         or al,al
         jnz .l2
         jmp showdir.exit

savepat: mov ah,3ch   ;create a file
         mov dx,svfn
         xor cx,cx
         int 21h
         jc .error

         mov bx,ax
         mov [filehl],ax
         mov ah,40h   ;write
         mov cx,6
         mov dx,x0
         int 21h

         mov si,tiles
         mov dx,[boxsz_xmin]  ;dl - xmin, dh - ymin
         xor cx,cx  ;cl - currow, ch - curcol
.loop0:  xor bx,bx
.loop2:  mov al,[si+bx]
         or al,al
         jnz .cont1

.loop4:  inc bx
         cmp bl,8
         jnz .loop2

         add si,tilesize
         inc ch
         cmp ch,hormax
         jnz .loop0

         xor ch,ch
         inc cx
         cmp cl,vermax
         jnz .loop0
         jmp loadpat.exit2

.error:  call printstr
         db 'can''t save$'
         jmp getkey

.cont1:  mov ah,0ffh
.loop3:  inc ah
         shl al,1
         jc .cont4
         jz .loop4
         jmp .loop3

.cont4:  push ax
         mov al,ch
         shl al,1
         shl al,1
         shl al,1
         add al,ah
         sub al,dl
         mov [x0],al
         mov al,cl
         shl al,1
         shl al,1
         shl al,1
         add al,bl
         sub al,dh
         mov [y0],al
         push bx
         push cx
         push dx
         mov ah,40h
         mov bx,[filehl]
         mov cx,2
         mov dx,x0
         int 21h
         pop dx
         pop cx
         pop bx
         pop ax
         jmp .loop3

;;ioerror: tstb @#errst           ;must be after iocf
;;         beq exit20

;;ioerr1:
;;         jsr r3,@#printstr
;;         .byte 12
;;         .asciz "IO ERROR"
;;         jmp @#getkey

showcomm:cmp byte [fn],0
         je .exit

         call totext
         mov si,fn
         mov di,svfn
         mov dx,di
.c1:     lodsb
         mov [di],al
         inc di
         cmp al,'.'
         jne .c1

         mov word [di],'T'+'X'*256
         mov word [di+2],'T'
         mov ax,3d00h
         int 21h
         jc .error

         mov bx,ax
         mov ax,3
         int 10h

.loop:   mov ah,3fh   ;read file
         mov cx,1
         mov dx,x0
         int 21h
         jc .fin

         or ax,ax
         jz .fin

         mov dl,[x0]
         mov ah,2
         int 21h
         jmp .loop

.fin:    mov ah,3eh    ;fclose
         int 21h
.exit:   call curoff
         call getkey
         jmp tograph

.error:  call printstr
         db 'no comments$'
         jmp .exit
   endif


;Ahmed Pervaiz 22L-6563
; Aadil Salman Butt 22L-6967

[org 0x0100]

jmp start
tickcount: dw 0
score : dw 0
brickPosition dw 7240
destroyTimerCount: dw 0
rabbitPos: dw 0
MaxCarrots: dw 5

Clrscr:		
			push es
			push ax
			push di

			mov ax,132*0*2 ;location
			mov bx,132*43	;counter
			push ax		
			push bx 
			push 0x00
			call Bgfill
			
			pop di
			pop ax
			pop es
		ret
;------------------------------------------		
Delay:      push cx
			mov cx, 0xFFFF
loop1:		loop loop1
			mov cx, 0xFFFF
loop2:		loop loop2
			pop cx
		ret
;------------------------------------------			
HighRes:	
		mov ah,0x00
		mov al,0x54
		int 10h 
ret	

;---------------------

Building:
	push bp
	mov bp,sp
	push di
	push ax
	push bx
	push cx
	push dx
	push es
	
	mov ax, 0xb800
	mov es, ax
	
	mov ax,[bp+10]	;loc
	mov bx,[bp+8]	;width
	mov si,[bp+6]	;height
	mov cx,[bp+4]	;attribute
	
	mov di, ax	; di is starting x-position
	
	mov dx, ax
	add dx, bx
		
	Buildingy:		
		Buildingx:
			mov word[es:di], cx
			add di, 2
			cmp di, dx
			jne Buildingx
		mov di, ax
		sub di, 264
		
		mov dx, di
		mov ax, di
		
		add dx, bx
			
		dec si
		jnz Buildingy
	
	
	pop es
	pop dx
	pop cx
	pop bx
	pop ax
	pop di
	mov sp,bp
	pop bp 
	ret 8

City:
	push di
	push ax
	push bx
	push cx
	push dx
	
	push 3168	;loc
	push 20 	;width
	push 8		;height
	push 0x7020	;attribute
	call Building
	
	push 3190	;loc
	push 20 	;width
	push 5		;height
	push 0x1020	;attribute
	call Building
	
	push 3216	;loc
	push 30 	;width
	push 5		;height
	push 0x3020	;attribute
	call Building
	
	push 3256	;loc
	push 10 	;width
	push 10		;height
	push 0x4020	;attribute
	call Building
	
	push 3276
	push 20
	push 10
	push 0x1020
	call Building
	
	push 3306
	push 30
	push 10
	push 0x1020
	call Building
	
	push 3336
	push 30
	push 5
	push 0x1020
	call Building
	
	
	pop dx
	pop cx
	pop bx
	pop ax
	pop di

;------------------------------------------------------------------------
PlayAnimationR:	
			push es
			push di
			push ax
			push bx
			push cx
			push dx
			push si

			mov ax, 0xb800
			mov es, ax
			
			mov dx, 132*13*2 ;starting pos
			sub dx,2
			mov ch, 14	   ;total number of rows to be moved	;outer counter
			mov di, dx
			
repp:		mov bx, [es:di]		;store first element so that it'd be pasted at the last place	;i.e., movement from right to left
			sub di, 2
			mov cl, 131		    ;total number of columns in a row to be copied	;inner counter
Copy:	
			mov ax, [es:di]
			add di, 2
			mov [es:di], ax
			sub di, 4
			dec cl
			jnz Copy

			add di, 2
			mov [es:di], bx		;paste first element at last position
			sub di, 2
			
			dec ch
			jnz repp
		
			pop si
			pop dx
			pop cx
			pop bx
			pop ax
			pop di
			pop es
		ret	
;------------------------------------------------------------------------
PlayAnimationL:	
			push es
			push di
			push ax
			push bx
			push cx
			push dx
			push si

			mov ax, 0xb800
			mov es, ax
			
			mov dx, 132*16*2 ;starting pos
			mov ch, 10	   ;total number of rows to be moved	;outer counter
			mov di, dx

repp1:		mov bx, [es:di]		;store first element so that it'd be pasted at the last place	;i.e., movement from right to left
			add di, 2
			mov cl, 131		;total number of columns in a row to be copied	;inner counter
CopyR:	
			mov ax, [es:di]
			sub di, 2
			mov [es:di], ax
			add di, 4
			dec cl
			jnz CopyR

			sub di, 2
			mov [es:di], bx		;paste first element at last position
			add di, 2
			
			dec ch
			jnz repp1
		
			pop si
			pop dx
			pop cx
			pop bx
			pop ax
			pop di
			pop es
		ret	
;------------------------------------------
PlayAnimation:
			call PlayAnimationL
			call PlayAnimationR
ret
;------------------------------------------
Bgfill:
			push bp
			mov bp, sp
			push es
			push di
			push ax
			push bx
			push cx
			push dx
			
			mov ax,0xb800
			mov es,ax
			mov ax,[bp+8]
			mov di,ax	
			mov cx,[bp+6]
			mov ah,[bp+4]
			mov al,0x20
doo:
			mov [es:di], ax
			add di,2
			loop doo
			
			
			pop dx
			pop cx
			pop bx
			pop ax
			pop di
			pop es
			pop bp
			
		ret 6
;------------------------------------------		
Grass:
			push bp
			mov bp, sp
			push es
			push di
			push ax
			push bx
			push cx
			push dx
			
			mov ax,0xb800
			mov es,ax
			
			mov ax,132*27*2 ;location
			mov bx,132*16	;counter
			push ax		
			push bx 
			push 0x22
			call Bgfill
			
			mov di,132*31*2
			mov ax,0x2020
			mov bx,5
againn:
			mov cx,44	;(132*2)/6
b1:
			mov [es:di], ax
			add di,6
			loop b1
			add di,264
			sub bx,1
			jnz againn
			
			
			pop dx
			pop cx
			pop bx
			pop ax
			pop di
			pop es
			pop bp
	ret 
;------------------------------------------
Road:
			push bp
			mov bp, sp
			push es
			push di
			push ax
			push bx
			push cx
			push dx
			
			mov ax,0xb800
			mov es,ax
			
			mov ax,132*14*2 ;starting loc
			mov bx,132*12	;no of rows
			push ax		
			push bx 
			push 0x70
			call Bgfill
			
			mov di,132*21*2
			add di,10
			mov ax,10
			mov bx,4
again:
			mov cx,25
l1:
			mov [es:di], ax
			add di,2
			loop l1
			add di,16
			sub bx,1
			jnz again
			
			
			pop dx
			pop cx
			pop bx
			pop ax
			pop di
			pop es
			pop bp
	ret 
	
;---------------------------------------
CarFunction:
	push (132*16*2 + 80) ; location
    call CarPrint
	
	push (132*16*2 + 200)
	call CarPrint
	ret

CarPrint:
	push bp
	mov bp, sp
	push di
	push cx
	push si
	push es
	push ax
	
	mov ax, 0xb800
	mov es, ax
	
    mov di, [bp+4]   ; starting position of car
    mov cx, 15 		 ; Car width
    mov si, 4        ; Car height

	upperBody:
		mov word [es:di], 0x4020
		add di, 2
		sub cx, 1
		jne upperBody

		mov cx, 15
		add di, 234
		sub si, 1
		jne upperBody

	downBody:
		sub di, 15
		mov cx, 30
		mov si, 4

		lop2:
			mov word [es:di], 0x44
			add di, 2
			sub cx, 1
			jne lop2

			mov cx, 30
			add di, 204
			sub si, 1
			jne lop2

	Tyre:
		sub di, 255
		mov cx, 4
		mov si, 2

		loop3:
			mov word [es:di], 0x00
			add di, 2
			sub cx, 1
			jne loop3

			mov cx, 4
			add di, 257
			sub si, 1
			jne loop3

	r2:
		sub di, 500
		mov cx, 4
		mov si, 2

		loop4:
			mov word [es:di], 0x00 
			add di, 2
			sub cx, 1
			jne loop4

			mov cx, 4
			add di, 257
			sub si, 1
			jne loop4

	Window:
		sub di, 1869
		mov cx, 11
		mov si, 2

		loop5:
		mov word [es:di], 0x77
		add di, 2
		sub cx, 1
		jne loop5

		add di, 242
		mov cx, 11
		sub si, 1
		jne loop5
	
	pop ax
	pop es
	pop si
	pop cx
	pop di
	pop bp
	ret 2
		
;------------------------------------------


Rabbit:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	push si 
	
	mov ax, 0xb800
	mov es, ax

	Rabutt:		
		mov di,[bp+4] ;starting pos
		mov ax, di
		mov cx, 3 	; height
		BodyOuter:				
			mov dx, 6 	;width
			BodyInner:
				mov word[es:di], 0xFFFF
				add di, 2
				dec dx
				jne BodyInner
			mov di, ax
			sub di, 264
			
			mov dx, di
			mov ax, di
			
			dec cx
			jnz BodyOuter
		
		mov di, [bp+4]
		sub di, 6
		mov dx, 2 ; width
		Tail:				
			mov word[es:di], 0xFFFF
			add di, 2
			dec dx
			jne Tail
		
		mov di, [bp+4]
		sub di, 526
		mov cx, 3
		Ear1:
			mov word[es:di], 0x44FF
			sub di, 264
			dec cx
			jnz Ear1
		mov di, [bp+4]
		sub di, 786
		mov cx, 2
		Ear2:
			mov word[es:di], 0x44FF
			sub di, 264
			dec cx
			jnz Ear2
			
		mov di, [bp+4]
		sub di, 258
		mov word[es:di], 0x00FF
		
	pop si
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2
	


upleft1:
	mov ax,0xb800
	mov es,ax
	mov di,9652-112-264
	;mov di,7629
	mov ax,[es:di]
	mov si,di
	add si,2
	mov cx,130
	poi:
		mov bx,word[es:si]
		mov word[es:di],bx
		add di,2
		add si,2
		loop poi
	ret

upleft2:

	mov ax,0xb800
	mov es,ax
	;mov di,9652-112-264
	mov di,7629
	mov ax,[es:di]
	mov si,di
	add si,2
	mov cx,130
	poi2:
		mov bx,word[es:si]
		mov word[es:di],bx
		add di,2
		add si,2
		loop poi2
	ret

mov1:
	call upright1
	call upleft2
	ret

upright1:
	mov ax,0xb800
	mov es,ax
	mov di,9652+112-264
	mov ax,[es:di]
	mov si,di
	sub si,2
	mov cx,100
	poi1:
		mov bx,word[es:si]
		mov word[es:di],bx
		sub di,2
		sub si,2
		loop poi1
	ret

upright2:
	mov ax,0xb800
	mov es,ax
	mov di,7916
	mov ax,[es:di]
	mov si,di
	sub si,2
	mov cx,100
	poi3:
		mov bx,word[es:si]
		mov word[es:di],bx
		sub di,2
		sub si,2
		loop poi3
	ret


mov2:
	call upright2
	call upleft1
	ret


bricks:

	mov ax,0xb800
	mov es,ax
	mov di,11200-264
	mov cx,18
	w:
	mov word[es:di],0x4444
	add di,2
	loop w
	sub di,264*6
	;mov di,9652
	sub di,36
	mov cx,18
	x:
	mov word[es:di],0x0000
	add di,2
	loop x

	sub di,264*6
	;mov di,8068
	sub di,36
	mov cx,18
	y:
	mov word[es:di],0x5555
	add di,2
	loop y
	ret

uprabbit:
	mov ax,0xb800
	mov es,ax
	mov di,10682
	mov cx,9
	mov si,5
	myjmp:
	mov word[es:di],0x3333
	add di,2
	loop myjmp
	mov cx,10
	sub di,264+20
	sub si,1
	jne myjmp

	push 10418-(264*5)
	call Rabbit
	mov [rabbitPos],di
	ret

randomBrick:
	push cx
	push dx
	push ax
	rdtsc
	xor dx,dx
	mov cx,3
	div cx

	mov ax,0xb800
	mov es,ax
	mov di,7240+(264*2)

	cmp dx,1
	je redbrick
	jbe magentabrick
	jae blackbrick

	redbrick:
	mov ax,0x4444
	jmp formbrick

	magentabrick:
	mov ax,0x5555
	jmp formbrick

	blackbrick:
	mov ax,0x0000
	jmp formbrick

	formbrick:
	mov cx,18
	y1:
	mov word[es:di],ax
	add di,2
	loop y1

	pop ax
	pop dx
	pop cx
	ret


; Boundarychck:
; mov ax,0xb800
; mov es,ax
; mov di,10684-(264*5)
; mov ax,[es:di]

; ;mov word[es:di],0x0000
; cmp ax,0x4444
; jne chkclr
; ret

chkclr:
cmp ax ,0x5555
jne chk2clr
ret

chk2clr:
cmp ax,0x2222
jne nxtchk
ret

nxtchk:
add di,7
mov ax,[es:di]
cmp ax,0x4444
jne chk2clr1

ret
chk2clr1:
cmp ax ,0x5555
jne chk3clr1
ret

chk3clr1:
cmp ax,0x2222
jne nxtchk1
ret

nxtchk1:
jmp GameOver
ret

scrollBricks:
	mov ax, brickPosition
    cmp ax, 40*2*132; right limit checked
	jge resetBrickPosition  ; right boundary exceeded so resetBrickPosition
	
	cmp ax,30*2*132; left limit checked
	jl resetBrickPosition
	call uprabbit
	call randomBrick
	
	add word [brickPosition], 2  ; Increment brick position
    jmp scrollBricks

resetBrickPosition:
    ; Reset brick position to left boundary
    mov word [brickPosition], 30*2*132
    jmp scrollBricks

carrot:
	push bp
	mov bp, sp
	push ax
	push es
	push di
	mov ax,0xb800
	mov es,ax
	
	mov di,10682-(264*7)
		mov word[es:di],0x6666
		mov word[es:(di+2)], 0x6666
		mov word[es:(di+4)], 0x6666
		mov word[es:(di+6)], 0x6666
		mov word[es:(di+8)], 0x6666
		
	pop di
	pop es
	pop ax
	pop bp
	ret
RandomCarrot:
push ax
mov ax,0xb800
mov es,ax
xor dx,dx
mov cx,132*27*2; max screen position
div cx;random screen pos
mov ax,dx
pop ax
ret

; carrot:
; pusha
; mov ax,0xb800
; mov es,ax
; mov di,11200-264; start pso on bricks
; call RandomCarrot
; add di,ax
; mov ax,0x6666
; mov word[es:di],ax
; popa
; ret

carrotcheck:
	 push ax
	 push es
	 push di
	 push cx
	 mov ax,0xb800
	 mov es,ax
	 mov di,10684-(264*7)
	 mov cx,9
	 L1:
		mov ax,[es:di]
		cmp ax,0x6666
		jne NotFound

	 Found:
		 add word[cs:score],10
		 mov word[es:di],0x0000
	 NotFound:
		 loop L1
		
	 pop cx
	 pop di
	 pop es
	 pop ax
	ret
EndGame:
mov ax,[rabbitPos]
	cmp ax,132*27*2
	jae GameOver
	
	ret
	; mov bp, sp
    ; mov ax, [bp - 2]  ;Rabbit pos
	
	; cmp ax, 11200
    ; jl GameOver  
	
	; ret

scrolldown:

    
	call uprabbit
	
	;call carrot
	
	call Delay
	call Delay

	push bp
	mov bp,sp
	push ax
	push cx
	push si
	push di
	push es
	push ds
	push bx

	mov ax,0xb800
	mov es,ax
	mov ds,ax



	std
	mov bx,6
	scrollLoop1:
	mov di,11348-264
	mov si,11084-264
	mov dx,13
	scrollLoop2:
	mov cx,132
	rep movsw
	sub dx,1
	jnz scrollLoop2
	sub bx,1
	jnz scrollLoop1
	cld
	
	call randomBrick
	

	pop bx
	pop ds
	pop es
	pop di
	pop si
	pop cx
	pop ax
	pop bp
	ret
	
printnum: 
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax
	mov ax, [bp+4]
	mov bx, 10
	mov cx, 0

	nextdigit: mov dx, 0
	div bx
	add dl, 0x30
	push dx ; save ascii value on stack
	inc cx ; increment count of values
	cmp ax, 0 ; is the quotient zero
	jnz nextdigit ; if no divide it again
	mov di, 264*26 ; point di to 70th column


	nextpos: pop dx ; remove a digit from the stack
	mov dh, 0x07 ; use normal attribute
	mov [es:di], dx ; print char on screen
	add di, 2 ; move to next screen location
	loop nextpos

	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2


DestroyBrick:
	pusha
	
	cmp word[cs:destroyTimerCount], 30
	jl skipDestroyBrick
	
	;if more than 15 ticks on the brick then break brick and lose game
	mov cx, 132
	mov di, 10824
	
	DestroyLoop:
		mov word[es:di], 0x2222
		add di, 2
		loop DestroyLoop
	call GameOver
	
	skipDestroyBrick:		
		popa
		ret
	
kbisr: 
	push ax
	push es
	push cs
	pop ds

	mov ax, 0xb800
	mov es, ax ; point es to video memory

	in al, 0x60 

	cmp al, 0x48 ;is key == up
	jne nomatch
	
	mov word[cs:destroyTimerCount], 0 ;when jump up then reset destroy time
	call scrolldown 
	;call Boundarychck
	call carrotcheck
	add word[cs:score], 10

	call GameEndCheck

	nomatch:
		cmp al,0x01
		je GameOver
	
		mov al, 0x20
		out 0x20, al ; send EOI to PIC

	pop es
	pop ax

	iret

; timer interrupt service routine
;------------------------------------------------------
timer: 
	push ax

	inc word [cs:tickcount]; increment tick count
	
	inc word[cs:destroyTimerCount] ;if standing then inc the timer 
	call DestroyBrick
	;call carrot
	;call carrotcheck
	
	push word [cs:score]
	call Score; print score 

	mov al, 0x20
	out 0x20, al ; end of interrupt

	pop ax
	iret ; return from interrupt
	
Score:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax         
	mov ax, [bp+4]           ; load number in ax
	mov bx, 10        
	mov cx, 0           ; initialize count of digits
	
	nxtdigit:
		mov dx, 0        ; zero upper half of dividend
		div bx ; divide by 10
		add dl, 0x30 ; convert digit into ascii value
		push dx ; save ascii value on stack
		inc cx ; increment count of values
		cmp ax, 0 ; is the quotient zero
		jnz nxtdigit ; if no divide it again
		mov di, (264 *26) + 258; point di to 70th column
		
	nxtpos:
		pop dx ; remove a digit from the stack
		mov dh, 0x07 ; use normal attribute
		mov [es:di], dx ; print char on screen
		add di, 2 ; move to next screen location
		loop nxtpos ; repeat for all digits on stack
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
	ret 2 

;------------------------------------------
PrintScreen:
		call Clrscr
		call HighRes
		
		call Grass
		call City
		call Road
		call CarFunction
		push 10684
		call Rabbit
		call bricks
		call carrot
		call EndGame
	ret
	
;--------------------------------------
moving:
	push cx
	
	o:
		xor ax, ax
		mov es, ax ; point es to IVT base
		cli ; disable interrupts
		mov word [es:9*4], kbisr ; store offset at n*4
		mov [es:9*4+2], cs ; store segment at n*4+2
		mov word [es:8*4], timer; store offset at n*4
		mov [es:8*4+2], cs ; store segment at n*4+2
		sti ; enable interrupts
		
		
		mov cx, 2
		moving1:
			call mov1
			
			call PlayAnimation
			call Delay
			call Delay
			call carrot ;calling it here makes it print on every brick
			;call scrollBricks
			loop moving1

		mov cx, 1
		moving2:
			call mov2
			call PlayAnimation
		
			;call scrollBricks
			loop moving2
			
		jmp o
		
		
	pop cx
	ret
;------------------------------------------
GameEndCheck:
	push bp
	mov bp, sp
	pusha
    mov di, 10684  ;Rabbit pos
	add di, 264
	
	cmp word[es:di], 0x2220
	je GameOver
	
	popa
	pop bp
	ret

start:
	;call HighRes
	call PrintScreen
	call moving
	
	GameOver: 
	mov ax, 0x4c00
	int 0x21
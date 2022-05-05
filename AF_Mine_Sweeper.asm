    .MODEL SMALL
    .STACK 1024

.DATA		; data section

; key definitions
LEFT    EQU     4bh
RIGHT   EQU     4dh
UP      EQU     48h
DOWN    EQU     50h

ESCAPE  EQU     1Bh
ENTERK  EQU     0Dh

board   DB  100 DUP(?)      ; space to save 10x10 board
posX    DW  0               ; cursor position
posY    DW  0

nMines  DW  0               ; number of mines
nHidden DW  100             ; number of hidden positions

showHidden DB 0FFh          ; flag to show or hide the hidden board contents

loseMsg DB  'You lost!', 0
winMsg  DB  'You won!', 0
waitmsg DB  'Press ESC to exit...', 0

.CODE       ; code section
.STARTUP

main PROC       ; program entry point
    mov ah, 0
    mov al, 3h  ; set graphics mode to 640x480
    int 10h

    mov ax, 0b800h      ; point to screen with es
    mov es, ax

    call clearBoard     ; clear the board
    call generateMines  ; generate the mines randomly
    call fillNumbers    ; count all mines
    call printBoard     ; print the initial board
    call printCursor    ; print the cursor
gameStart:
    mov   ah, 01h       ; wait for key pressed
    int   16h
    jnz   readKey
    jmp   noKey
readKey:
    mov   ah, 00h       ; get code of key pressed
    int   16h
    cmp   al, ESCAPE    ; if it was escape, exit
    jne   ifarrow
    jmp   exit
ifarrow:
    cmp   ah, LEFT
    je    moveLeft
    cmp   ah, RIGHT
    je    moveRight
    cmp   ah, UP
    je    moveUp
    cmp   ah, DOWN
    je    moveDown
    cmp   al, ENTERK    ; if it's enter
    je    showMine
    cmp   al, ' '       ; if it's a space
    je    flipBoard
    cmp   al, 'f'       ; if it's a f character
    je    jmpflag
    jmp   noKey         ; if any other key, ignore it    
jmpflag:
    jmp   flag
flipBoard:
    mov   al, [showHidden]  ; flip the hidden state of the board
    xor   al, 080h
    mov   [showHidden], al
    jmp   update
moveRight:
    mov   ax, [posX]
    inc   ax
    cmp   ax, 10
    jl    updrpos
    jmp   noKey
updrpos:
    mov   [posX], ax
    jmp   update
moveLeft:
    mov   ax, [posX]
    dec   ax
    cmp   ax, 0
    jge   updlpos
    jmp   noKey
updlpos:
    mov   [posX], ax
    jmp   update
moveDown:
    mov   ax, [posY]
    inc   ax
    cmp   ax, 10
    jge   noKey
    mov   [posY], ax
    jmp   update
moveUp:
    mov   ax, [posY]
    dec   ax
    cmp   ax, 0
    jl    noKey
    mov   [posY], ax
    jmp   update
showMine:
    mov   ax, [posX]
    mov   bx, [posY]
    call  getBoardChar  ; get character at current position
    test  al, 80h       ; see if the char is hidden
    jz    noKey         ; if it's not hidden, we can't unhide it
    test  al, 40h       ; see if it has a flag
    jnz   noKey         ; if it has a flag, we can't unhide it
    xor   al, 80h       ; else, clear hidden

    cmp   al, 9         ; see if it was a mine
    je    lose          ; if it was a mine, end game

    mov   ax, [posX]
    mov   bx, [posY]
    call  clearEmpty    ; unhide and clear all neighboring empty spaces

    mov   ax, [nHidden]
    cmp   ax, [nMines]  ; see if we uncovered all but the mines
    je    win           ; if so, we won

    jmp   update
flag:
    mov   ax, [posX]
    mov   bx, [posY]
    call  getBoardChar  ; get character at current position
    test  al, 80h       ; see if the char is hidden
    jz    noKey         ; if it's not hidden, we can't put a flag there
    xor   al, 40h       ; else, set flag
    mov   cl, al
    mov   ax, [posX]
    mov   bx, [posY]
    call  setBoardChar  ; set character at current position
update:
    call printBoard
    call printCursor    ; print the cursor
noKey:
    jmp gameStart   
win:
    call showMines      ; show all mines as flagged
    call printBoard
    lea si, winMsg      ; print a win message
    mov dl, 10
    mov dh, 10
    call printString
    jmp waitEsc
lose:
    call showAll        ; set all board chars as visible
    call printBoard
    lea si, loseMsg     ; print a lose message
    mov dl, 10
    mov dh, 10
    call printString

waitEsc:
    lea si, waitMsg     ; print a press key message
    mov dl, 10
    mov dh, 11
    call printString
waitKey:                ; wait for escape key
    mov   ah, 00h       ; wait for a key
    int   16h
    cmp   al, ESCAPE    ; if it was escape, exit
    jne   waitKey

exit:

    mov ah, 0
    mov al, 3h  ; set graphics mode to 640x480
    int 10h

    mov ah,4ch
    int 21h
main ENDP

;///////////////////////////////////
;  clear the minesweeper board using
;  the char in al
;///////////////////////////////////
clearBoard  PROC
    lea si, board
    mov cx, 100
    mov al, 80h         ; set all spaces as hidden
iloop:
    mov [si], al
    inc si 
    loop iloop
    ret
clearBoard  ENDP

;///////////////////////////////////
;  show all the mines and numbers in
;  the board 
;///////////////////////////////////
showAll  PROC
    lea si, board
    mov cx, 100
sloop:
    mov al, [si]        ; get char from board
    and al, 0Fh         ; set all spaces as not hidden
    mov [si], al
    inc si 
    loop sloop
    ret
showAll  ENDP

;///////////////////////////////////
;  show all the mines flagged
;///////////////////////////////////
showMines  PROC
    lea si, board
    mov cx, 100
floop:
    mov al, [si]        ; get char from board
    and al, 0Fh         
    cmp al, 9           ; if it was a mine
    jne skip
    or al, 0C0h          ; flag it
    mov [si], al
skip:    
    inc si 
    loop floop
    ret
showMines  ENDP


;///////////////////////////////////
; generate random mines in the board
;///////////////////////////////////
generateMines  PROC
    lea si, board
    mov cx, 10          ; generate at least 10 mines
    call random10       ; get a random number between 0 and 10
    add  cx, ax         ; count between 0 and 16 mines
    mov  [nMines], cx   ; save as number of mines
    mov  ax, 20          
    call delay
gloop:

    call random10       ; get a random number between 0 and 10
    mov  dx, ax         ; save number in dx

    shl  ax, 5
    xor  ax, [nMines]
    call delay
    
    call random10       ; get another random number between 0 and 10
    mov  bx, ax         ; save number in bx

    mov   ax, dx        ; set the char at the generated position
    call  getBoardChar
    and   al, 0Fh
    jne   gloop

    push  cx
    mov   ax, dx        ; set the char at the generated position
    mov   cx, 89h       ; set a hidden mine
    call  setBoardChar
    pop   cx       
    loop gloop
    ret
generateMines  ENDP

;///////////////////////////////////
; count the neighboring mines for all
; empty board positions
;///////////////////////////////////
fillNumbers PROC
    mov di, 0           ; di will count the rows
yloop:
    mov si, 0           ; si will count the columns
xloop:
    mov ax, si
    mov bx, di
    call getBoardChar   ; get char at x, y
    and al, 0Fh 
    je  count
    jmp next            ; if not free, go to next
count:    
    mov dx, 0           ; count of neighboring mines
countl:
    cmp si, 0           ; if no left position, go to right
    je  countr
    mov ax, si
    dec ax
    mov bx, di
    call getBoardChar   ; get char at x-1, y
    and al, 0Fh 
    cmp al, 9           ; test if it's a mine
    jne countul         ; if no mine, go to next neighbor
    inc dx              ; else, increment number of mines
countul:
    cmp di, 0           ; if no up position, go to down
    je  countdl
    mov ax, si
    dec ax
    mov bx, di
    dec bx
    call getBoardChar   ; get char at x-1, y-1
    and al, 0Fh 
    cmp al, 9           ; test if it's a mine
    jne countdl         ; if no mine, go to next neighbor
    inc dx              ; else, increment number of mines
countdl:
    cmp di, 9           ; if no down position, go to right
    je  countr
    mov ax, si
    dec ax
    mov bx, di
    inc bx
    call getBoardChar   ; get char at x-1, y+1
    and al, 0Fh 
    cmp al, 9           ; test if it's a mine
    jne countr          ; if no mine, go to next neighbor
    inc dx              ; else, increment number of mines
countr:
    cmp si, 9           ; if no right position, go to up
    je  countu
    mov ax, si
    inc ax
    mov bx, di
    call getBoardChar   ; get char at x+1, y
    and al, 0Fh 
    cmp al, 9           ; test if it's a mine
    jne countur         ; if no mine, go to next neighbor
    inc dx              ; else, increment number of mines
countur:
    cmp di, 0           ; if no up position, go to down
    je  countdr
    mov ax, si
    inc ax
    mov bx, di
    dec bx
    call getBoardChar   ; get char at x+1, y-1
    and al, 0Fh 
    cmp al, 9           ; test if it's a mine
    jne countdr         ; if no mine, go to next neighbor
    inc dx              ; else, increment number of mines
countdr:
    cmp di, 9           ; if no down position, go to up
    je  countu
    mov ax, si
    inc ax
    mov bx, di
    inc bx
    call getBoardChar   ; get char at x+1, y+1
    and al, 0Fh 
    cmp al, 9           ; test if it's a mine
    jne countu          ; if no mine, go to next neighbor
    inc dx              ; else, increment number of mines
countu:
    cmp di, 0           ; if no up position, go to down
    je  countd
    mov ax, si
    mov bx, di
    dec bx
    call getBoardChar   ; get char at x, y-1
    and al, 0Fh 
    cmp al, 9           ; test if it's a mine
    jne countd          ; if no mine, go to next neighbor
    inc dx              ; else, increment number of mines
countd:
    cmp di, 9           ; if no down position, go to save count
    je  savecnt
    mov ax, si
    mov bx, di
    inc bx
    call getBoardChar   ; get char at x, y+1
    and al, 0Fh 
    cmp al, 9           ; test if it's a mine
    jne savecnt         ; if no mine, go to save count
    inc dx              ; else, increment number of mines
savecnt:
    mov ax, si
    mov bx, di
    call getBoardChar   ; get char at x, y
    mov cl, al
    add cl, dl          ; put number of mines
    mov ax, si
    mov bx, di
    call setBoardChar   ; set char at x, y   
next:
    inc si              ; increment x position
    cmp si, 10          ; repeat for all x positions in board
    jge endxloop
    jmp xloop
endxloop:
    inc di              ; increment y position
    cmp di, 10          ; repeat for all y positions in board
    jge endyloop
    jmp yloop
endyloop:    
    ret
fillNumbers ENDP

;///////////////////////////////////
; generate a random number 0-10
;///////////////////////////////////
random10:
    push bx
    push cx
    push dx 
    mov ah, 2ch  
    int 21h      ; get time from MS-DOS
    mov ax, dx   
    mov bx, 10
    mov dx, 0
    div bx       
    mov ax, dx   ; get number between 0 and 10 (remainder)
    pop dx
    pop cx
    pop bx
    ret

;///////////////////////////////////
; delay ax milliseconds
;///////////////////////////////////
delay:
    push bx
    push cx
    push dx
    mov  bx,03e8h   ; 1000 micros 
    mul  bx         ; multiply by required millis
    mov  cx, 0
    mov  dx, ax
    mov  ah, 86H    ; delay using interrupt 15
    int  15h        
    pop  dx
    pop  cx
    pop  bx
    ret


;///////////////////////////////////
; print the current board state
; ax = 0 shows current board
; ax = 1 shows all hidden positions
;///////////////////////////////////
printBoard PROC
    push si
    push di
    push bx
    push cx
    push dx
        
    mov dx, ax          ; put hide or show selection in dx

    mov di, 0           ; point to start of screen with di
    lea si, board       ; point to board with si
    mov bx, 10          ; bx will count the rows
rowloop:
    mov cx, 10          ; cx will count the columns
colloop:
    mov al, [si]        ; load char from board
    and al, [showHidden]    
    test al, 80h        ; if the space is hidden
    jne hide            ; don't show
    and al, 0Fh         ; else, show count
    jne showCount
    mov ax, 32
    jmp print
showCount:
    cmp al, 9           ; see if it's a mine
    jne digit           ; if not, print digit
    mov ax, 10
    sub ax, bx
    cmp [posY], ax
    jne redmine
    mov ax, 10
    sub ax, cx
    cmp ax, [posX]
    jne redmine
    mov ah, 0CFh         ; show a * to indicate mine
    mov al, '*'
    jmp print
redmine:    
    mov ah, 04h         ; else, show a * to indicate mine
    mov al, '*'
    jmp print
digit:
    mov ah, 07h
    add al, '0'
    jmp print      
hide:
    test al, 40h        ; see if it has a flag
    je  setSpace        ; if not, show a space
    mov ah, 074h        ; else, show a flag
    mov al, 'F'
    jmp print
setSpace:
    mov ax, 7720h
print:
    mov es:[di], ax     ; save char in screen
    add di,2            ; advance position in screen
    inc si              ; advance position in board
    loop colloop    
    add di,140          ; jump to next row in screen    
    dec bx
    jg  rowloop         ; repeat for all rows

    pop dx
    pop cx
    pop bx
    pop di
    pop si
    ret
printBoard ENDP

;///////////////////////////////////
; print cursor at the current position
;///////////////////////////////////
printCursor PROC
    mov bx, [posY]
    mov ax, 80
    mul bx
    add ax, [posX]
    shl ax, 1
    mov di, ax
    mov ax, 2220h       ; green cursor
    mov es:[di], ax 
    ret
printCursor ENDP

;///////////////////////////////////
; get the char at position x = ax,
; y = bx in the board
;///////////////////////////////////
getBoardChar PROC
    push si
    push bx
    push dx
    mov si, ax      ; save x position in si
    mov ax, 10      ; calculate offset address in board
    mul bx
    add ax, si
    lea si, board
    add si, ax
    mov al, [si]    ; get char from board
    pop dx
    pop bx
    pop si
    ret
getBoardChar ENDP

;///////////////////////////////////
; set the char cl at position ax, bx in 
; the board
;///////////////////////////////////
setBoardChar PROC
    push si
    push bx
    push dx
    mov si, ax      ; save x position in si
    mov ax, 10      ; calculate offset address in board
    mul bx
    add ax, si
    lea si, board
    add si, ax
    mov [si], cl        ; save char in board
    pop dx
    pop bx
    pop si
    ret
setBoardChar ENDP

;///////////////////////////////////
; prints al char at x=dl,y=dh
;///////////////////////////////////
printChar PROC
    push bx
    push cx
    mov bh, 0           ; page zero
    mov cx, 1           ; print 1 char
    mov ah,2            ; move cursor
    int 10h
    mov bl,0Fh          ; print in white
    mov ah,09h          ; print char with int 10
    int 10h
    pop cx
    pop bx
    ret
printChar ENDP

;///////////////////////////////////
; prints a zero terminated string 
; pointed by si at position x=dl,y=dh
;///////////////////////////////////
printString PROC
    push si
    push dx
ploop:  
    mov al, [si]    ; load a char from the string
    cmp al, 0       ; if it's the end of the string
    je endprint     ; go to end 
    call printChar  ; else, print char
    inc dl          ; advance cursor position
    inc si          ; advance to next char in string
    jmp ploop       ; print next char in string       
endprint:   
    pop dx
    pop si
    ret
printString ENDP


;///////////////////////////////////
;  show all empty spaces in board 
; starting at position x= ax, y = bx
;///////////////////////////////////
clearEmpty  PROC
    push si
    push di
    mov si, ax
    mov di, bx

    cmp di, 0           ; if bad position, return
    jl  clearend
    cmp di, 9           ; if bad position, return
    jg  clearend
    cmp si, 0           ; if bad position, return
    jl  clearend
    cmp si, 9           ; if bad position, return
    jg  clearend

    mov ax, si
    mov bx, di
    call getBoardChar   ; get char at x, y
    test al, 80h 
    je clearend        ; if not hidden, return
unhide:
    mov cl, al
    and cl, 0Fh
    mov ax, si
    mov bx, di
    call setBoardChar   ; unhide position
    mov   ax, [nHidden]
    dec   ax            ; decrement hidden positions
    mov   [nHidden], ax
    cmp cl, 0
    jne clearend        ; if not empty, return   
clearl:
    mov ax, si
    dec ax
    mov bx, di
    call clearEmpty     ; recurse at x-1, y
clearul:
    mov ax, si
    dec ax
    mov bx, di
    dec bx
    call clearEmpty     ; recurse at x-1, y-1
cleardl:
    mov ax, si
    dec ax
    mov bx, di
    inc bx
    call clearEmpty     ; recurse at x-1, y+1
clearr:
    mov ax, si
    inc ax
    mov bx, di
    call clearEmpty     ; recurse at x+1, y
clearur:
    mov ax, si
    inc ax
    mov bx, di
    dec bx
    call clearEmpty     ; recurse at x+1, y-1
cleardr:
    mov ax, si
    inc ax
    mov bx, di
    inc bx
    call clearEmpty     ; recurse at x+1, y+1
clearu:
    mov ax, si
    mov bx, di
    dec bx
    call clearEmpty     ; recurse at x, y-1
cleard:
    mov ax, si
    mov bx, di
    inc bx
    call clearEmpty     ; recurse at x, y+1
clearend:   
    pop di
    pop si
    ret
clearEmpty  ENDP

END
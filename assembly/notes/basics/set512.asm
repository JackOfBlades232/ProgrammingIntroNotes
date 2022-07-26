%include "../../stud_io.inc"
global _start

section .bss
set512  resd 16         ; array of bits for 512-elem set

section .text
; fill array
_start: xor eax, eax    ; EAX := 0
        mov ecx, 16     ; counter for set512
        mov esi, set512 ; init elem adr
lp:     mov [esi+4*ecx-4], eax  ; fill elem with zeroes
        loop lp         ; cycle
; set index 
        mov ebx, 132    ; put index in ebx 
; set/get/unset elem
        mov cl, bl      ; move bit num to cl
        and cl, 11111b  ; take five bits
        mov eax, 1      ; create mask in EAX
        shl eax, cl     ; compute mask
        ; not eax       ; invert mask for unsetting
        mov edx, ebx    ; move elem num to edx
        shr edx, 5      ; shift to get array elem num
        or [set512+4*edx], eax  ; apply mask
        ; and [set512+4*edx], eax  ; if unsetting
        ; test [set512+4*edx], eax  ; test if elem is in set by ZF==0
; count elements
        xor ebx, ebx    ; EBX := 0
        mov ecx, 15     ; last index
lp1:    mov eax, [set512+4*ecx] ; load elem into EAX
lp2:    test eax, 1     ; is 1 in lowest bit?
        jz notone       ; if not, jump
        inc ebx         ; if yes, increment elem counter
notone: shr eax, 1      ; shift EAX
        test eax, eax   ; is there anything in EAX?
        jnz lp2         ; if yes, cont nested loop
        jecxz quit      ; if ECX is zero, jump out
        dec ecx         ; else, decrement it
        jmp lp1         ; cont outer cycle
quit:   FINISH          ; exit with macro

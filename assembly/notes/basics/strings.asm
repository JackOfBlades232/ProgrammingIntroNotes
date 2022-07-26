%include "../../stud_io.inc"
global _start

section .bss
buf     resb 1024

section .text
; fill arr
_start: mov al, 63      ; put @ code minus 1 in AL
        mov edi, buf    ; EDI is used for writing to arrays
        mov ecx, 1024   ; set counter
        cld             ; set DF=0 (array direction normal)
lp:     stosb           ; al -> [edi], edi++ (stosw and stosd also)
        loop lp         ; loop until ecx is 0
; increment arr
        mov esi, buf    ; for reading
        mov edi, esi    ; for writing (better to move reg to reg)
        mov ecx, 1024   ; set counter
        cld             ; set direction
lp1:    lodsb           ; read char
        inc eax         ; increment read elem
        stosb           ; write back to arr
        movsb           ; copy from [esi] to [edi] (movsw and movsd also)
        loop lp1        ; loop until ecx is 0
; other actions
        mov esi, buf    ; put esi on array
        mov edi, buf    ; put edi on array
        mov ecx, 3      ; set counter
        cld             ; set dir
        rep movsb       ; repeat movsb until ECX==0
        mov esi, buf    ; put esi on array
        mov edi, buf    ; put edi on array
        cmpsb           ; compare [esi] and [edi], set flags and inc/dec
        lodsb           ; put [esi] into al
        scasb           ; compare AL/AX/EAX to [edi] and inc/dec
; find @ in arr
        mov edi, buf    ; put edi on arr
        mov ecx, 1024   ; set counter
        mov al, '@'     ; put @ in AL
        cld             ; set dir
        repnz scasb     ; perform scasb, dec ECX, check ZF, cont if ZF=0
        ; if repz, then if ZF=1
; copy array part with shift
        std             ; set DF=1 (inverse dir)
        mov edi, buf+15+5   ; init write adr
        mov esi, buf+15 ; init read adr
        mov ecx, 6      ; counter
        rep movsb       ; repeat copy
; output arr
        mov esi, buf    ; ESI is used for reading arrays
        mov ecx, 1024   ; set counter
        cld             ; set direction
lp2:    lodsb           ; [esi] -> al, esi++ (losw and losd also)
        PUTCHAR al      ; print string char
        loop lp2        ; loop until ecx is 0
        PUTCHAR 10      ; print newline
        FINISH          ; exit with macro

;; cmdl.asm ;;
global _start

section .text

; constants
nlstr   db  10, 0           ; string in mem for new lines

; subpr: calculate len of str, limited by char #0 (arg1 := adr) : len->eax
strlen:
        push ebp            
        mov ebp, esp
        xor eax, eax      
        mov ecx, [ebp+8]    ; arg 1  
.lp:    cmp byte [eax+ecx], 0   ; check delimiter
        jz .quit            
        inc eax            
        jmp short .lp     
.quit:  pop ebp             ; no local var-s, so wont touch esp
        ret

; subpr: print str to stdo, limited by char #0 (arg1 := adr)
print_str:
        push ebp
        mov ebp, esp
        push ebx            ; will be corrupted
        mov ebx, [ebp+8]    ; arg 1
        push ebx            ; (also store in stack as param for strlen)
        call strlen         
        add esp, 4          ; length is now in eax
%ifdef OS_FREEBSD
        push eax            ; lenght
        push ebx            ; arg 1
        push dword 1        ; stdout
        mov eax, 4          ; write syscall
        push eax            ; extra dword
        int 80h
        add esp, 16
%elifdef OS_LINUX
        mov edx, eax        ; len now in edx
        mov ecx, ebx        ; arg1 now in ecx
        mov ebx, 1          ; stdout
        mov eax, 4          ; write syscall
        int 80h
%else
%error please define either OS_FREEBSD or OS_LINUX
%endif
        pop ebx             ; restore ebx
        mov esp, ebp
        pop ebp
        ret

; main code (print all cli args)
_start:
        mov ebx, [esp]      ; cli arg count (put in stack automatically) (argc)
        mov esi, esp        ; will traverse stack with esi
        add esi, 4          ; move to first arg (program name) (argv)
again:  push dword [esi]    ; argv[i] (arg adresses in stack)
        call print_str
        add esp, 4
        push dword nlstr    ; new line after arg
        call print_str      ; not very effective, but fewer lines of code
        add esp, 4          ; syscalls are costly, better to reduce their num
        add esi, 4          ; to next arg
        dec ebx
        jnz again

%ifdef OS_FREEBSD
        push dword 0        ; success
        mov eax, 1          ; _exit
        push eax            ; extra word
        int 80h
%else
        mov ebx, 0          ; success
        mov eax, 1          ; _exit
        int 80h
%endif

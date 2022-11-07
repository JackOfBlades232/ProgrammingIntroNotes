; calling subprogram in one line
%macro pcall 1-*
  %rep %0 - 1
    %rotate -1
                push dword %1
  %endrep
  %rotate -1
                call %1
                add esp, (%0 - 1) * 4
%endmacro

; making syscall in 1 line
%macro kernel 1-*
%ifdef OS_FREEBSD
  %rep %0
    %rotate -1
                push dword %1
  %endrep
                mov eax, [esp]
                int 80h
                jnc %%ok            ; check for error
                mov ecx, eax        ; if error, code to ecx, -1 to eax
                mov eax, -1
                jmp short %%q
  %%ok:         xor ecx, ecx        ; else, zero out ecx
  %%q:          add esp, %0 * 4     ; clear stack
%else ; assuming it is linux
  %if %0 > 1
                push ebx
    %if %0 > 4
                push esi
                push edi
                push ebp            ; saving non-cdecl regs
    %endif
  %endif
  %rep %0
    %rotate -1
                push dword %1
  %endrep
                pop eax             ; now distibute from stack to regs
  %if %0 > 1
                pop ebx
    %if %0 > 2
                pop ecx
      %if %0 > 3
                pop edx
        %if %0 > 4
                pop esi
          %if %0 > 5
                pop edi
            %if %0 > 6
                pop ebp
              %if %0 > 7
                %error "Can't do Linux syscall with 7+ params"
              %endif
            %endif
          %endif
        %endif
      %endif
    %endif
  %endif
                int 80h
                mov ecx, eax        ; handle errors the same way
                and ecx, 0fffff000h 
                cmp ecx, 0fffff000h
                jne %%ok
                mov ecx, eax
                neg ecx             ; so that error is from 1 to ...
                mov eax, -1
                jmp short %%q
  %%ok:         xor ecx, ecx
  %%q:
  %if %0 > 1
    %if %0 > 4
                pop ebp
                pop edi
                pop esi
    %endif
                pop ebx
  %endif
%endif
%endmacro

; print character to stdio
%macro putchar 1
                pusha 
                pushf
  %ifstr %1
                mov al, %1
  %elifnum %1
                mov al, %1
  %elifidni %1, al
                nop
  %elifidni %1, bl
                mov al, bl
  %elifidni %1, cl
                mov al, cl
  %elifidni %1, dl
                mov al, dl
  %elifidni %1, ah
                mov al, ah
  %elifidni %1, bh
                mov al, bh
  %elifidni %1, ch
                mov al, ch
  %elifidni %1, dh
                mov al, dh
  %else
                mov al, %1
  %endif
                sub esp, 4
                mov edi, esp
                mov [edi], al
                kernel 4, 1, edi, 1
                add esp, 4
                popf
                popa
%endmacro

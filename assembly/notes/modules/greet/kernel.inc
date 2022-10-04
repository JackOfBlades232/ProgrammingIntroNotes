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
%elifdef OS_LINUX
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
%else
  %error "Please define either OS_LINUX or OS_FREEBSD"
%endif
%endmacro

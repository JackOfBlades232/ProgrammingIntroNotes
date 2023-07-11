;; parallel/simple_mutex.asm ;;
global mutex
global mutex_lock
global mutex_unlock

section         .data

mutex           dd 1                    ; mutex is initially open

section         .text

mutex_lock:     mov     eax, 0
.again:         xchg    eax, [mutex]    ; xchg is atomic, thus this works
                cmp     eax, 0
                je      .again
                ret
                
mutex_unlock:   mov     dword [mutex], 1
                ret

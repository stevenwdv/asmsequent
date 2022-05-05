
;Old, doesn't work as GUI

tokens     equ rsi

;tokens
fbound
print_tokens:
        save rsi
        mov     tokens, arg1

        align 16
        .loop:
                xor     eax,eax
                lodsb
                test    al,al
                jz      .done

                cmp     al, TOKEN_VAR
                je      .var

                ;no var
                cinvoke puts, [s_tokens+(rax-TOKEN_NOT)*8]
                jmp     .loop

              .var:
                lodsq
                lea     arg2, [rax+1]   ;skip TERM.type
                ;debx    arg2
                cinvoke puts, arg2
                jmp     .loop
      .done:

        rest
        ret
restore tokens
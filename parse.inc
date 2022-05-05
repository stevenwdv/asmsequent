;Operator precedence parser

;▼ mind order STACK<>PTR!
;STACK   PTR     ACT
;$       $       pop_noconstruct + accept
;$               push
;        $       pop
;var             pop
;        var     push
;(       )       pop_noconstruct + inc
;(               push
;        (       push
;        )       pop
;!       op      pop
;!               push
;        !       push
;stong   weak    pop
;weak    strong  push
;Lassoc  sameop  pop
;Rassoc  sameop  push

;(!)
;&      L assoc
;|      L assoc
;->     R assoc
;<->    L assoc


;TODO linux wine reduce '[...] but found [no number]'

tokens          equ rsi
term_stack      equ r12
term_stack_lenb equ r13

;Parse tokens for one side
; on success, term = term on that side or NULL and error=0
; on error, error = malloced error string

;tokens, term O -> error
fbound
parse:
        save rsi,r12,r13,r15
        mov     tokens, arg1

        xor     term_stack_lenb,term_stack_lenb
        malloc  term_stack_lenb
        mov     term_stack, rax

        sub     rsp, 8
        push    term_stack_lenb ; =TOKEN_END

        align 16
        .loop:
                movzx   eax, byte[rsi]

if FALSE
                debs    ""
                debs    "P:"
                debi    rax
                debs    "S:"
                mov     r9, [rsp]
                debi    r9
end if

                test    al,al
                jz      .P_end                  ;P: END

                cmp     byte[rsp], TOKEN_END
                je      .push                   ;S: END

                cmp     al, TOKEN_VAR
                je      .push                   ;P: VAR

                cmp     byte[rsp], TOKEN_VAR
                je      .pop                    ;S: VAR

                cmp     al, TOKEN_RPAREN
                je      .P_rparen               ;P: RPAREN

                cmp     al, TOKEN_LPAREN
                je      .push                   ;P: LPAREN

                cmp     al, TOKEN_NOT
                je      .P_not                  ;P: NOT

                cmp     byte[rsp], TOKEN_NOT
                je      .S_not                  ;S: NOT

                cmp     al, byte[rsp]
                jb      .push                   ;P: strong, S: weak
                ja      .pop                    ;P: weak, S: strong

                cmp     al, TOKEN_IMP
                je      .push                   ;P=S and P: Rassoc
                jmp     .pop                    ;P=S and P: Lassoc

      .error_pop: ;r15: type
        .unwind_loop:
                cmp     byte[rsp], TOKEN_END
                je      .unwind_done

                add     rsp, $10
                jmp     .unwind_loop
      .unwind_done:
        shr     term_stack_lenb, 3      ;Not actually in bytes anymore..
        mov     [rsp], term_stack_lenb

       frame
        test    term_stack_lenb,term_stack_lenb
        jz      .term_free_done
        mov     rsi, term_stack
        .term_free_loop:
                lodsq
                ccall   term_free, rax
                dec     term_stack_lenb
                jnz     .term_free_loop

      .term_free_done:
        mfree   term_stack
       endf

        pop     term_stack_lenb
        add     rsp, 8          ;pop TOKEN_END

        cmp     r15, TOKEN_LPAREN
        je      .error_lparen
        cmp     r15, TOKEN_RPAREN
        je      .error_rparen

        xor     eax,eax
        cmp     r15, TOKEN_AND
        setae   al
        inc     rax

        asprintf s_parse_e_pop, rax, [s_tokens+(r15-TOKEN_NOT)*8], term_stack_lenb
        mov      rax, arg1
        resti
        ret

      .error_lparen:
        asprintf_noparams s_parse_e_lparen
        mov      rax, arg1
        resti
        ret

      .error_rparen:
        asprintf_noparams s_parse_e_rparen
        mov      rax, arg1
        resti
        ret

      .error_stack_multiple:
        shr     term_stack_lenb, 3
        asprintf s_parse_e_stack_multiple, term_stack_lenb
        mov     rax, arg1
        resti
        ret

      .accept: ;P: END, S: END
        add     rsp, $10

        test    term_stack_lenb,term_stack_lenb
        jz      .accept_empty

        mov     r15, [term_stack]
        mfree   term_stack

        cmp     term_stack_lenb, 8
        ja      .error_stack_multiple

        mov     arg1, r15
        xor     eax,eax
        resti
        ret

      .accept_empty:
        mfree   term_stack

        xor     arg1,arg1
        xor     eax,eax
        rest
        ret


fbound
.P_end:
        cmp     byte[rsp], TOKEN_END
        je      .accept                 ;P: END, S: END
        jmp     .pop                    ;P: END, S: !END

fbound
.P_rparen:
        cmp     byte[rsp], TOKEN_LPAREN
        jne     .pop                    ;P: RPAREN, S: !LPAREN
        ;pop '(' and skip ')'
        add     rsp, 2*8                ;P: RPAREN, S: LPAREN
        inc     rsi
        jmp     .loop

fbound
.P_not:
        inc     rsi
        sub     rsp, 8
        push    TOKEN_NOT
        jmp     .loop

fbound
.S_not:
        cmp     al, TOKEN_AND
        jb      .push                   ;P: !op, S: NOT
        ;cmp     al, TOKEN_EQV
        ;ja      .push
        jmp     .pop                    ;P: op, S: NOT


fbound
.pop: ;AKA reduce
        pop     r15                     ;type
        cmp     r15, TOKEN_VAR
        jne     .pop.operator

        pop     r15                     ;TERMVAR ptr
        add     term_stack_lenb, 8
       frame
        realloc term_stack, term_stack_lenb
       endf
        mov     term_stack, rax

        mov     [term_stack+term_stack_lenb-8], r15     ;add var to stack
        jmp     .loop

      .pop.operator:
        add     rsp, 8
        cmp     r15, TOKEN_NOT
        je      .pop.not

        cmp     r15, TOKEN_LPAREN
        je      .error_pop
        cmp     r15, TOKEN_RPAREN
        je      .error_pop

        cmp     term_stack_lenb, 8*2
        jb      .error_pop

       frame
        malloc  sizeof.TERM
       endf
        dec     r15                     ;TERM_NOT-TOKEN_NOT = -1
        mov     [rax+TERM.type], r15b

        mov     rcx, [term_stack+term_stack_lenb-8*2]
        mov     [rax+TERM.t1], rcx
        mov     rcx, [term_stack+term_stack_lenb-8]
        mov     [rax+TERM.t2], rcx

        ;replace with operator
        sub     term_stack_lenb, 8
        mov     [term_stack+term_stack_lenb-8], rax
        jmp     .loop

      .pop.not:
        cmp     term_stack_lenb, 8
        jb      .error_pop

       frame
        malloc  sizeof.TERM1
       endf
        mov     [rax+TERM.type], TERM_NOT
        mov     rcx, [term_stack+term_stack_lenb-8]
        mov     [rax+TERM.t1], rcx

        ;replace with operator
        mov     [term_stack+term_stack_lenb-8], rax
        jmp     .loop


fbound
.push: ;AKA shift
        inc     rsi
        cmp     rax, TOKEN_VAR
        jne     .push.nvar

        ;P: VAR
        lodsq
        push    rax             ;TERMVAR ptr
        push    TOKEN_VAR
        jmp     .loop

      .push.nvar: ;P: !VAR
        sub     rsp, 8
        push    rax
        jmp     .loop

restore tokens,term_stack,term_stack_len
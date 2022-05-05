indent  equ r13

;buffer, sequent, indent
fbound
print_seq:
        save r12,r13,rbx,rsi,rdi
        mov     buf,     arg1
        mov     sequent, arg2
        mov     indent,  arg3

        inc     indent
        ccall   print_side, buf, tside, tlen, indent
        dec     indent

        do_indent
        doprint s_seq_sep, 3

        cmp     child1, NULL
        jne     @f
        cmp     child2, NULL
        jne     @f

        cmp     state, SEQUENT_CLOSED
        je      .closed

        doprint s_seq_open, 11
        jmp     .fside

      .closed:
        doprint s_seq_closed, 9
        jmp     .fside

      @@:
        doprint s_newline, 2

      .fside:
        inc     indent
        ccall   print_side, buf, fside, flen, indent

        cmp     child1, NULL
        je      @f

        do_indent
        doprint s_newline, 2

        do_indent
        movzx   eax, [sequent+SEQUENT_ACTION.action]
        doprint [s_actions+rax*8], 8

        ccall   print_seq, buf, child1, indent

        cmp     [sequent+SEQUENT.child2], NULL
        je      @f

        do_indent
        doprint s_newline, 2
        ccall   print_seq, buf, child2, indent

      @@:
        rest
        ret
restore indent


side    equ r12
len     equ r13
indent  equ r14

;buffer, side, len, indent
fbound
print_side:
        save r12,r13,r14,r15,rbx,rsi,rdi
        mov     buf,    arg1
        mov     side,   arg2
        mov     len,    arg3
        mov     indent, arg4

        xor     r15,r15
        align 16
      @@:
                cmp     r15, len
                je      @f
                do_indent
                ccall   print_term, buf, qword[side+r15*8]
                doprint s_newline, 2
                inc     r15
                jmp     @b
      @@:
        rest
        ret
restore side,len,indent


term    equ r12

;buffer, term
fbound
print_term:
        save r12,r13,r14,rbx,rsi,rdi
        mov     buf,    arg1
        mov     term,   arg2

        mov     r13b, [term+TERM.type]
        test    r13b,r13b
        jne     @f

        ;if TERM_VAR
        lea     rdx, [term+TERM.t1] ;var name
        mov     rdi, rdx
        xor     eax,eax
        or      rcx, -1
        repnz scasb
        neg     rcx
        lea     r12, [rcx-2]
        mov     r13, rdx ;var name

        doprint r13, r12
        resti
        ret

      @@: ;if not TERM_VAR
        cmp     r13b, TERM_NOT
        jne     @f

        ;if TERM_NOT
        doprint s_op_not, 1
        ccall   print_term, buf, [term+TERM.t1]      ;print t1
        resti
        ret

      @@: ;if not TERM_NOT
        doprint s_lparen, 1
        ccall   print_term, buf, [term+TERM.t1]      ;print t1

        doprint s_space, 1
        movsx   r13, r13b

        cmp     r13, TERM_IMP
        je      .op_imp
        ja      .op_eqv
        mov     r14, 1
        jmp     @f
      .op_imp:
        mov     r14, 2
        jmp     @f
      .op_eqv:
        mov     r14, 3

      @@:
        doprint [s_ops+(r13-TERM_AND)*8], r14   ;print operator
        doprint s_space, 1

        ccall   print_term, buf, [term+TERM.t2]      ;print t2
        doprint s_rparen, 1

        rest
        ret

restore term,indent
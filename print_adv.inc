
;Old, doesn't work as GUI


;sequent, indent
indent  equ r13

fbound
print_seq:
        save r12,r13
        mov     sequent, arg1
        mov     indent, arg2

        do_indent
        cinvoke printf, s_seq, sequent

        inc     indent

        do_indent
        cinvoke printf, s_side_t
        ccall   print_side, tside, tlen, indent

        do_indent
        cinvoke printf, s_side_f
        ccall   print_side, fside, flen, indent

        do_indent
        cmp     child1, NULL
        je      .no_childs

        cinvoke puts, s_seq_childs
        inc     indent

        ccall   print_seq, child1, indent

        cmp     [sequent+SEQUENT.child2], NULL
        je      .one_child
        ccall   print_seq, child2, indent
        restX
        ret

      .no_childs:
        movzx   arg2, state
        cinvoke printf, s_seq_state
      .one_child:
        restX
        ret
restore indent


;side, len, indent
side    equ r12
len     equ r13
indent  equ r14

fbound
print_side:
        save r12,r13,r14,r15
        mov     side,   arg1
        mov     len,    arg2
        mov     indent, arg3

        do_indent
        cinvoke printf, s_side, side
        cinvoke printf, s_side_terms, len

        inc     indent
        xor     r15,r15
        align 16
      @@:
                cmp     r15, len
                je      @f
                ccall   print_term, qword[side+r15*8], indent
                inc     r15
                jmp     @b
      @@:
        restX
        ret
restore side,len,indent


;term, indent
term    equ r12
indent  equ r13
fbound
print_term:
        save r12,r13,r15
        mov     term,   arg1
        mov     indent, arg2

        do_indent

        cinvoke printf, s_term, term
        movzx   arg2, [term+TERM.type]
        cinvoke printf, s_term_type

        inc     indent

        cmp     [term+TERM.type], TERM_VAR
        jne     @f

        ;if TERM_VAR
        do_indent
        lea     r15, [term+TERM.t1] ;var name
        cinvoke puts, r15
        jmp     .ret

      @@: ;if not TERM_VAR
        ccall   print_term, [term+TERM.t1], indent ;print t1

        cmp     byte[term+TERM.type], TERM_NOT
        jbe     .ret
        ;if has t2
        ccall   print_term, [term+TERM.t2], indent  ;print t2

      .ret:
        restX
        ret

restore term,indent
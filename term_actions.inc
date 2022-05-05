CHILDNULL = FALSE

macro action_not sidefrom*, sideto*, lenfrom*, lento* {
        save r12,r13,r14,r15,rsi,rdi
        mov     sequent,  arg1
        mov     el_index, arg2

        malloc  sizeof.SEQUENT
        mov     new_seq, rax
        mov     child1, new_seq
        mov     child2, NULL

if CHILDNULL
        mov     [new_seq+SEQUENT.child1], NULL
        mov     [new_seq+SEQUENT.child2], NULL
end if

        ;===sideto===
        mov     r15, lento
        inc     r15             ;r15 = incremented lento
        shl     r15, 3          ;*8

        malloc  r15             ;alloc 8*(lento+1)

        ;copy old to new
        mov     rsi, sideto     ;rsi = old sideto
        mov     rdi, rax        ;rdi = new sideto
        mov     rcx, lento      ;rcx = lento
        rep movsq


        ;add elem sidefrom[el_index].t1
        mov     r11, sidefrom           ;sidefrom ptr
        mov     r11, [r11+el_index*8]   ;elem ptr
        mov     r11, [r11+TERM.t1]      ;elem.t1 (=TERM ptr)
        sub     r15, 8
        mov     [rax+r15], r11          ;new sideto[(old)lento*8] = elem.t1

        ;update sideto
        mov     [new_seq+SEQUENT.#sideto], rax
        ;update lento
        mov     r11, lento
        inc     r11
        mov     [new_seq+SEQUENT.#lento], r11


        ;===sidefrom===
        dec_lenfrom equ r15
        mov     dec_lenfrom, lenfrom
        dec     dec_lenfrom
        mov     r11, dec_lenfrom
        shl     r11, 3
        malloc  r11              ;alloc 8*(lenfrom-1)

        mov     rsi, sidefrom    ;rsi = old sidefrom
        mov     rdi, rax         ;rdi = new sidefrom
        mov     rcx, el_index    ;rcx = el_index
        rep movsq

        add     rsi, 8           ;skip elem
        mov     rcx, dec_lenfrom ;rcx = lenfrom-1
        sub     rcx, el_index    ;rcx = lenfrom-1 - elem_index
        rep movsq


        ;update sidefrom
        mov     [new_seq+SEQUENT.#sidefrom], rax
        ;update lenfrom
        mov     r11, lenfrom
        dec     r11
        mov     [new_seq+SEQUENT.#lenfrom], r11

        rest
        ret
}


macro action_chop side1*, side2*, len1*, len2* {
        save r12,r13,r14,r15,rsi,rdi
        mov     sequent,  arg1
        mov     el_index, arg2

        malloc  sizeof.SEQUENT
        mov     new_seq, rax
        mov     child1, new_seq
        mov     child2, NULL

if CHILDNULL
        mov     [new_seq+SEQUENT.child1], NULL
        mov     [new_seq+SEQUENT.child2], NULL
end if

        ;===side1===
        inc_len1 equ r15
        mov     inc_len1, len1
        inc     inc_len1
        mov     r11, inc_len1
        shl     r11, 3
        malloc  r11             ;alloc 8*(len1 + 1)

        mov     rsi, side1      ;rsi = old side1
        mov     rdi, rax        ;rdi = new side1
        mov     rcx, el_index   ;rcx = el_index
        rep movsq

        add     rsi, 8          ;skip elem
        mov     rcx, len1
        dec     rcx             ;rcx = len1 - 1
        sub     rcx, el_index   ;rcx = len1 - 1 - elem_index
        rep movsq

        ;add elems
        mov     rsi, side1              ;side1 ptr
        mov     rsi, [rsi+el_index*8]   ;elem ptr
        add     rsi, TERM.t1            ;elem.t1 (=TERM ptr)
        movsq                           ;add to new side1: elem.t1
        movsq                           ;add to new side1: elem.t2

        mov     [new_seq+SEQUENT.#side1], rax     ;update side1
        mov     [new_seq+SEQUENT.#len1], inc_len1 ;update len1


        ;===side2===
        mov     rax, len2
        shl     rax, 3
        malloc  rax             ;alloc 8*len2

        mov     rsi, side2      ;rsi = old side2
        mov     rdi, rax        ;rdi = new side2
        mov     rcx, len2       ;rcx = len2
        rep movsq

        mov     [new_seq+SEQUENT.#side2], rax     ;update side2
        mov     rax, len2
        mov     [new_seq+SEQUENT.#len2], rax      ;update len2

        rest
        ret
}


;side1 contains element to be chopped and will receive the chopped terms (excl. t1 if t1_at2)
macro action_chop_split side1*, side2*, len1*, len2*, t1_at2=FALSE {
        seq1 equ r14
        seq2 equ rbx

        save r12,r13,r14,r15,rbx,rsi,rdi
        mov     sequent,  arg1
        mov     el_index, arg2

        macro create_seq@action_chop_split n_seq* \{
                malloc  sizeof.SEQUENT

                mov     seq\#n_seq, rax
                mov     child\#n_seq, rax

if CHILDNULL
                mov     [rax+SEQUENT.child1], NULL
                mov     [rax+SEQUENT.child2], NULL
end if
        \}

        rept 2 % \{create_seq@action_chop_split %\}
        purge create_seq@action_chop_split

        ;===side1===
if ~t1_at2
        mov     r15, len1
        shl     r15, 3
end if

        macro side1@action_chop_split n_child* \{
        if t1_at2
                mov     r15, len1
         if n_child=1
                dec     r15
         end if
                shl     r15, 3
        end if
                malloc  r15             ;alloc 8*(len1 [-1])

                mov     rsi, side1      ;rsi = old side1
                mov     rdi, rax        ;rdi = new side1
                mov     rcx, el_index   ;rcx = el_index
                rep movsq

                add     rsi, 8          ;skip elem
                mov     rcx, len1
                dec     rcx             ;rcx = len1 - 1
                sub     rcx, el_index   ;rcx = len1 - 1 - elem_index
                rep movsq

        if ~(t1_at2 & n_child=1)
                ;add elem.tX
                mov     rsi, side1              ;side1 ptr
                mov     rsi, [rsi+el_index*8]   ;elem ptr
                add     rsi, TERM.t\#n_child    ;elem.tX (=TERM ptr)
                movsq                           ;add to new side1: elem.tX
        end if

                mov     [seq\#n_child+SEQUENT.#side1], rax ;update side1
                mov     rax, len1
        if t1_at2 & n_child=1
                dec     rax
        end if
                mov     [seq\#n_child+SEQUENT.#len1], rax  ;set len1
        \}

        rept 2 % \{side1@action_chop_split %\}
        purge side1@action_chop_split

        ;===side2===
if ~t1_at2
        mov     r15, len2
        shl     r15, 3
end if

        macro side2@action_chop_split n_child* \{
        if t1_at2
                mov     r15, len2
         if n_child=1
                inc     r15
         end if
                shl     r15, 3
        end if
                malloc  r15             ;alloc 8*(len2 [+1])

                mov     rsi, side2      ;rsi = old side2
                mov     rdi, rax        ;rdi = new side2
                mov     rcx, len2       ;rcx = len2
                rep movsq

        if t1_at2 & n_child=1
                ;add elem.t1
                mov     rsi, side1              ;side1 ptr
                mov     rsi, [rsi+el_index*8]   ;elem ptr
                add     rsi, TERM.t1            ;elem.t1 (=TERM ptr)
                movsq                           ;add to new side2: elem.t1
        end if

                mov     [seq\#n_child+SEQUENT.#side2], rax ;update side2
                mov     rax, len2
        if t1_at2 & n_child=1
                inc     rax
        end if
                mov     [seq\#n_child+SEQUENT.#len2], rax  ;set len2
        \}

        rept 2 % \{side2@action_chop_split %\}
        purge side2@action_chop_split

        rest
        ret
        restore seq1,seq2
}


;side1 contains element to be chopped
macro action_imp_eqv_r side1*, side2*, len1*, len2*, split* {
        seq1 equ r14
        seq2 equ rbx

        save r12,r13,r14,r15,rbx,rsi,rdi
        mov     sequent,  arg1
        mov     el_index, arg2

        macro create_seq@action_imp_eqv_r n_seq* \{
                malloc  sizeof.SEQUENT

                mov     seq\#n_seq, rax
                mov     child\#n_seq, rax

if CHILDNULL
                mov     [rax+SEQUENT.child1], NULL
                mov     [rax+SEQUENT.child2], NULL
end if
        \}

        create_seq@action_imp_eqv_r 1
if split
        create_seq@action_imp_eqv_r 2
else
        mov     child2, NULL
end if
        purge create_seq@action_imp_eqv_r

        ;===side1===
        mov     r15, len1
        shl     r15, 3

        macro side1@action_imp_eqv_r n_child*, tx* \{
                malloc  r15             ;alloc 8*len1

                mov     rsi, side1      ;rsi = old side1
                mov     rdi, rax        ;rdi = new side1
                mov     rcx, el_index   ;rcx = el_index
                rep movsq

                add     rsi, 8          ;skip elem
                mov     rcx, len1
                dec     rcx             ;rcx = len1 - 1
                sub     rcx, el_index   ;rcx = len1 - 1 - elem_index
                rep movsq

                ;add elem.tX
                mov     rsi, side1              ;side1 ptr
                mov     rsi, [rsi+el_index*8]   ;elem ptr
                add     rsi, TERM.t\#tx         ;elem.tX (=TERM ptr)
                movsq                           ;add to new side1: elem.tX

                mov     [seq\#n_child+SEQUENT.#side1], rax ;update side1
                mov     rax, len1
                mov     [seq\#n_child+SEQUENT.#len1], rax  ;set len1
        \}

        ;child1 gets t2, child2 gets t1
        side1@action_imp_eqv_r 1, 2
if split
        side1@action_imp_eqv_r 2, 1
end if
        purge side1@action_imp_eqv_r

        ;===side2===
        mov     r15, len2
        inc     r15
        shl     r15, 3

        macro side2@action_imp_eqv_r n_child* \{
                malloc  r15             ;alloc 8*(len2 + 1)

                mov     rsi, side2      ;rsi = old side2
                mov     rdi, rax        ;rdi = new side2
                mov     rcx, len2       ;rcx = len2
                rep movsq

                ;add elem.tX
                mov     rsi, side1              ;side1 ptr
                mov     rsi, [rsi+el_index*8]   ;elem ptr
                add     rsi, TERM.t\#n_child    ;elem.tX (=TERM ptr)
                movsq                           ;add to new side2: elem.tX

                mov     [seq\#n_child+SEQUENT.#side2], rax ;update side2
                mov     rax, len2
                inc     rax
                mov     [seq\#n_child+SEQUENT.#len2], rax  ;set len2
        \}

        side2@action_imp_eqv_r 1
if split
        side2@action_imp_eqv_r 2
end if
        purge side2@action_imp_eqv_r

        rest
        ret
        restore seq1,seq2
}


;side1 contains element to be chopped
macro action_eqv_l side1*, side2*, len1*, len2* {
        seq1 equ r14
        seq2 equ rbx

        save r12,r13,r14,r15,rbx,rsi,rdi
        mov     sequent,  arg1
        mov     el_index, arg2

        macro create_seq@action_eqv_l n_seq* \{
                malloc  sizeof.SEQUENT

                mov     seq\#n_seq, rax
                mov     child\#n_seq, rax

if CHILDNULL
                mov     [rax+SEQUENT.child1], NULL
                mov     [rax+SEQUENT.child2], NULL
end if
        \}

        rept 2 % \{create_seq@action_eqv_l %\}
        purge create_seq@action_eqv_l

        ;===side1===
        mov     r15, len1
        inc     r15
        shl     r15, 3

        macro side1@action_eqv_l n_child* \{
        if n_child=2
                sub     r15, 2*8
        end if
                malloc  r15             ;alloc 8*(len1 + 1)

                mov     rsi, side1      ;rsi = old side1
                mov     rdi, rax        ;rdi = new side1
                mov     rcx, el_index   ;rcx = el_index
                rep movsq

                add     rsi, 8          ;skip elem
                mov     rcx, len1
                dec     rcx             ;rcx = len1 - 1
                sub     rcx, el_index   ;rcx = len1 - 1 - elem_index
                rep movsq

        if n_child=1
                ;add elems
                mov     rsi, side1              ;side1 ptr
                mov     rsi, [rsi+el_index*8]   ;elem ptr
                add     rsi, TERM.t1            ;elem.t1 (=TERM ptr)
                movsq                           ;add to new side1: elem.t1
                movsq                           ;add to new side1: elem.t2
        end if

                mov     [seq\#n_child+SEQUENT.#side1], rax ;update side1
                mov     rax, len1
        if n_child=1
                inc     rax
        else
                dec     rax
        end if
                mov     [seq\#n_child+SEQUENT.#len1], rax  ;set len1
        \}

        rept 2 % \{side1@action_eqv_l %\}
        purge side1@action_eqv_l

        ;===side2===
        mov     r15, len2
        shl     r15, 3

        macro side2@action_eqv_l n_child* \{
        if n_child=2
                add     r15, 2*8
        end if
                malloc  r15             ;alloc 8*len2

                mov     rsi, side2      ;rsi = old side2
                mov     rdi, rax        ;rdi = new side2
                mov     rcx, len2       ;rcx = len2
                rep movsq

        if n_child=2
                ;add elems
                mov     rsi, side1              ;side1 ptr
                mov     rsi, [rsi+el_index*8]   ;elem ptr
                add     rsi, TERM.t1            ;elem.t1 (=TERM ptr)
                movsq                           ;add to new side2: elem.t1
                movsq                           ;add to new side2: elem.t2
        end if

                mov     [seq\#n_child+SEQUENT.#side2], rax ;update side2
                mov     rax, len2
        if n_child=2
                add     rax, 2
        end if
                mov     [seq\#n_child+SEQUENT.#len2], rax  ;set len2
        \}

        rept 2 % {side2@action_eqv_l %\}
        purge side2@action_eqv_l

        rest
        ret
        restore seq1,seq2
}



sequent  equ r12
el_index equ r13
new_seq  equ r14

tside   equ [sequent+SEQUENT.tside]
fside   equ [sequent+SEQUENT.fside]
tlen    equ [sequent+SEQUENT.tlen]
flen    equ [sequent+SEQUENT.flen]
child1  equ [sequent+SEQUENT.child1]
child2  equ [sequent+SEQUENT.child2]


t_actions:

;sequent, el_index
fbound
.not:   action_not tside, fside, tlen, flen

;sequent, el_index
fbound
.and:   action_chop tside, fside, tlen, flen

;sequent, el_index
fbound
.or:    action_chop_split tside, fside, tlen, flen

;sequent, el_index
fbound
.imp:   action_chop_split tside, fside, tlen, flen, TRUE

;sequent, el_index
fbound
.eqv:   action_eqv_l tside, fside, tlen, flen


f_actions:

;sequent, el_index
fbound
.not:   action_not fside, tside, flen, tlen

;sequent, el_index
fbound
.and:   action_chop_split fside, tside, flen, tlen

;sequent, el_index
fbound
.or:    action_chop fside, tside, flen, tlen

;sequent, el_index
fbound
.imp:   action_imp_eqv_r fside, tside, flen, tlen, FALSE

;sequent, el_index
fbound
.eqv:   action_imp_eqv_r fside, tside, flen, tlen, TRUE



restore sequent,el_index, new_seq, tside,fside,tlen,flen,child1,child2
purge   action_not,action_chop,action_chop_split,action_imp_eqv_r

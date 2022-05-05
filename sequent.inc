SEQUENT_CLOSED = 0
SEQUENT_OPEN   = 1

ACTION_LNOT = 0
ACTION_LAND = 1
ACTION_LOR  = 2
ACTION_LIMP = 3
ACTION_LEQV = 4
ACTION_RNOT = 1000b+0
ACTION_RAND = 1000b+1
ACTION_ROR  = 1000b+2
ACTION_RIMP = 1000b+3
ACTION_REQV = 1000b+4

struct SEQUENT
  tside  rq 1
  fside  rq 1
  tlen   rq 1
  flen   rq 1
  child1 rq 1
  child2 rq 1
  extra  rb 1
ends

struct SEQUENT_ACTION
  tside  rq 1
  fside  rq 1
  tlen   rq 1
  flen   rq 1
  child1 rq 1
  child2 rq 1
  action rb 1   ;ACTION_*
ends

struct SEQUENT_STATE
  tside  rq 1
  fside  rq 1
  tlen   rq 1
  flen   rq 1
  child1 dq NULL
  child2 dq NULL
  state  rb 1   ;SEQUENT_[OPEN/CLOSED]
ends


sequent equ r12
tside   equ [sequent+SEQUENT.tside]
fside   equ [sequent+SEQUENT.fside]
tlen    equ [sequent+SEQUENT.tlen]
flen    equ [sequent+SEQUENT.flen]
child1  equ [sequent+SEQUENT.child1]
child2  equ [sequent+SEQUENT.child2]
state   equ [sequent+SEQUENT_STATE.state]
action  equ [sequent+SEQUENT_ACTION.action]


;n_side: 0=tside, 1=fside
macro solve_seq_side n_side* {
        local ..term_loop, ..next_side, ..loop_next

if n_side=0
        mov     len, tlen
else
        mov     len, flen
end if
        test    len,len
        jz      ..next_side

if n_side=0
        mov     side, tside
else
        mov     side, fside
end if
        xor     el_index,el_index
        ..term_loop:
                mov     cur_term, [side+el_index*8]     ;ptr to TERM
                cmp     [cur_term+TERM.type], TERM_VAR
                je      ..loop_next

                mov     last_el_index, el_index
                mov     last_type, [cur_term+TERM.type]

if n_side=0
                mov     jmp_table, term_t_actions_jmp

                cmp     last_type, TERM_OR
                jae     ..loop_next
else
                mov     jmp_table, term_f_actions_jmp

                cmp     last_type, TERM_AND
                je      ..loop_next
                cmp     last_type, TERM_EQV
                je      ..loop_next
end if

                jmp     seq_solve.term_found

              ..loop_next:
                inc     el_index
                cmp     el_index, len
                jb      ..term_loop

      ..next_side:
}



;childptr  equ r13
side      equ r10
len       equ rcx
el_index  equ r11
jmp_table equ rax
last_type equ r9b
last_el_index equ arg2
cur_term  equ r8

;Solve sequent (make tree) and return if the tableau beneath is open or closed

;TODO mode
;sequent -> SEQUENT_[OPEN/CLOSED]
fbound
seq_solve:
        save r12,r13
        mov     sequent,  arg1
        ;mov     childptr, arg2

        ccall   seq_is_closed, sequent
        test    rax,rax
        jz      .not_closed

        ;realloc sequent, sizeof.SEQUENT_STATE
        ;mov     sequent, rax
        ;mov     arg1, sequent
        ;test    childptr,childptr
        ;jz      @f
        ;mov     [childptr], sequent
      ;@@:
if ~CHILDNULL
        mov     child1, NULL
        mov     child2, NULL
end if

        mov     rax, SEQUENT_CLOSED
        mov     state, SEQUENT_CLOSED
        resti
        ret

      .not_closed:
        xor     last_type,last_type


        solve_seq_side 1        ;fside has less splitting actions
        solve_seq_side 0


        test    last_type,last_type
        jz      .open_leaf

        ;if no nonsplitting found, take last splitting
      .term_found:
        cmp     jmp_table, term_f_actions_jmp
        sete    r8b
        shl     r8b, 3
        or      r8b, last_type
        dec     r8b ;-TERM_NOT
        mov     action, r8b

        movzx   r9d, last_type
        ccall   qword[jmp_table+(r9-TERM_NOT)*8], sequent ;, el_index

        ;lea     arg2, [sequent+SEQUENT.child1]
        ccall   seq_solve, child1

        cmp     child2, NULL
        je      .one_child

        mov     r13, rax
        ;lea     arg2, [sequent+SEQUENT.child2]
        ccall   seq_solve, child2
        test    r13,r13
        jnz     .open
        test    rax,rax
        jnz     .open

        ;mov     arg1, sequent
        mov     rax, SEQUENT_CLOSED
        resti
        ret

      .open_leaf:
        ;realloc sequent, sizeof.SEQUENT_STATE
        ;mov     sequent, rax
        mov     state, SEQUENT_OPEN
        ;mov     arg1, sequent
        ;test    childptr,childptr
        ;jz      .open
        ;mov     [childptr], sequent

if ~CHILDNULL
        mov     child1, NULL
        mov     child2, NULL
end if

      .open:
        mov     rax, SEQUENT_OPEN

      .one_child: ;return result of ccall solve_seq, child1
        ;mov     arg1, sequent
        rest
        ret

restore side,len,el_index,jmp_table,last_term,last_type,cur_term,last_el_index ;,childptr
purge   solve_seq_side


;TODO check contents?

;Is sequent closed? (same terms on both sides)
;closed=0/1
;Only checks addresses, not contents (hence tokens_group_vars is necessary)

;sequent -> closed
fbound
seq_is_closed:
        save rsi,rdi
        mov     sequent, arg1

        cmp     tlen, 0
        jz      .not_closed
        cmp     flen, 0
        jz      .not_closed

        mov     rsi, tside
        mov     rdx, tlen

        mov     r8, fside
        mov     r9, flen
        @@:
                lodsq
                mov     rdi, r8         ;fside
                mov     rcx, r9         ;flen
                repne scasq
                je      .closed
                dec     rdx
                jnz     @b

      .not_closed:
        xor     eax,eax
        resti
        ret

      .closed:
        mov     rax, TRUE
        rest
        ret


if FALSE ;seq_is_open is not really needed anymore

;Is sequent open? (only TERMVARS and not closed)

;sequent -> open
fbound
seq_is_open:
        save rsi,rdi
        mov     sequent, arg1

        macro check_side@seq_is_open side*, len* {
                mov     rsi, side
                mov     rdx, len

              @@:
                lodsq
                cmp     [rax+TERM.type], TERM_VAR
                jne     .not_open
                dec     rdx
                jnz     @b
        }

        cmp     [rcx+SEQUENT.tlen], 0
        jz      .check_fside
        check_side@seq_is_open [rcx+SEQUENT.tside], [rcx+SEQUENT.tlen]

      .check_fside:
        cmp     [rcx+SEQUENT.flen], 0
        jz      .open
        check_side@seq_is_open [rcx+SEQUENT.fside], [rcx+SEQUENT.flen]

        purge check_side@seq_is_open

      .open:
        mov     rax, TRUE
        resti
        ret

      .not_open:
        xor     eax,eax
        rest
        ret
end if


;TODO use stack instead of recursion

;Free sequent and child sequents (etc) and sides of sequents
;Does not free terms

;sequent
fbound
seq_free:
        save r12
        mov     sequent, arg1

        mfree   tside
        mfree   fside

        cmp     child1, NULL
        je      @f
        ccall   seq_free, child1

        cmp     child2, NULL
        je      @f
        ccall   seq_free, child2

      @@:
        mfree   sequent

        rest
        ret

restore sequent, tside,fside,tlen,flen,child1,child2
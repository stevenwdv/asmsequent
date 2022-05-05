TERM_VAR = 0
TERM_NOT = 1
TERM_AND = 2
TERM_OR  = 3
TERM_IMP = 4
TERM_EQV = 5

;I will often refer to TERMVAR & TERM1 structs as just TERM
struct TERMVAR
  type db TERM_VAR
  t1   rb 0     ;string of some length, NULL-terminated
ends

struct TERM1
  type rb 1     ;one of TERM_*
  t1   rq 1     ;ptr to TERM
ends

struct TERM TERM1
  t2   rq 1     ;ptr to TERM
ends


term equ r12

;Free term and child terms
;Does not free TERMVARs

;term
fbound
term_free:
        push    NULL
        push    arg1

        .loop:
                pop     rax
                test    rax,rax ;is NULL?
                jz      .done

                mov     cl, [rax+TERM.type]
                test    cl,cl
                jz      .loop   ;skip vars

                push    [rax+TERM.t1]

                cmp     cl, TERM_AND
                jb      @f

                push    [rax+TERM.t2]

                bt      rsp, 3
                jc      .align_free

               frame
                mfree   rax
               endf
                jmp     .loop

              @@: ;TERM1
                bt      rsp, 3
                jc      .align_free

               frame
                mfree   rax
               endf
                jmp     .loop

              .align_free:
                ;align stack
               frame_odd
                mfree   rax
               endf
                jmp     .loop

      .done:
        ret
restore term
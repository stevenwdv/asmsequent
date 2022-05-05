TOKEN_END    = 0
TOKEN_VAR    = 1
TOKEN_NOT    = 2
TOKEN_AND    = 3
TOKEN_OR     = 4
TOKEN_IMP    = 5
TOKEN_EQV    = 6
TOKEN_LPAREN = 7
TOKEN_RPAREN = 8
TOKEN_SEQSEP = 9


struct TOKEN
  type  rb 1     ;TOKEN_*
ends
struct TOKENVAROFF
  type   db TOKEN_VAR
  offset rd 1    ;offset in string
  pad    rd 1
ends
struct TOKENVAR
  type   db TOKEN_VAR
  term   rq 1    ;ptr to TERMVAR
ends

string      equ r12
tokens      equ r13
tokens_lenb equ r14
tokens_len  equ rbx
var_offset  equ edi
var_offsetm equ rdi     ;(mask) bit 63 is set if we are not reading a var

;TODO more operator aliases ('=', '==')
;TODO remove tokens_len?

;Tokenizes string and
; on success, error=0 and tokens is a malloced TOKEN/TOKENVAR array and tokens_len the length number of tokens
; on error, error is a malloced error string

;string, tokens O, tokens_len O -> error
fbound
tokenize:
        save r12,r13,r14,r15,rbx,rsi,rdi
        mov     string, arg1

        xor     tokens_lenb,tokens_lenb
        xor     tokens_len,tokens_len
        bts     var_offsetm, 63

        malloc  tokens_lenb
        mov     tokens, rax

        mov     rsi, string
        align 16
        .loop:
                lodsb
                test    al,al
                jz      .done

                cmp     al, 'A'
                jb      .no_letter
                cmp     al, 'z'
                ja      .no_letter
                cmp     al, 'Z'
                jbe     .letter
                cmp     al, 'a'
                jb      .no_letter

              .letter:  ;I could use a loop here, but.. this works
                bt      var_offsetm, 63
                jnc     .loop
                lea     var_offsetm, [rsi-1]
                sub     var_offsetm, string
                jmp     .loop

              .no_letter:
                bts     var_offsetm, 63
                jc      .no_append_var

                ;append var
                add     tokens_lenb, sizeof.TOKENVAR
                inc     tokens_len
                mov     r15b, al
                realloc tokens, tokens_lenb
                mov     tokens, rax
                mov     al, r15b

                mov     [tokens+tokens_lenb-sizeof.TOKENVAR+TOKEN.type], TOKEN_VAR
                mov     [tokens+tokens_lenb-sizeof.TOKENVAR+TOKENVAROFF.offset], var_offset
                bts     var_offsetm, 63
                mov     byte[rsi-1], 0  ;write 0 terminator

              .no_append_var:

                cmp     al, ' '
                je      .loop

                cmp     al, '('
                je      .token_lparen

                cmp     al, ')'
                je      .token_rparen

                cmp     al, '!'
                je      .token_not

                cmp     al, '&'
                je      .token_and

                cmp     al, '|'
                je      .token_or

                cmp     al, '-'
                je      .token_dash

                cmp     al, '<'
                je      .token_lt

                cmp     al, '/'
                je      .token_slash

                cmp     al, '\'
                je      .token_backslash

                cmp     al, '~'
                je      .token_not

                cmp     al, 9   ;tab
                je      .loop
                cmp     al, 10  ;LF
                je      .loop
                cmp     al, 13  ;CR
                je      .loop

                cmp     al, '@'
                je      .token_sep

                jmp     .error_unknown

              .append:
                inc     tokens_lenb
                inc     tokens_len
                mov     r15b, al
                realloc tokens, tokens_lenb
                mov     tokens, rax
                mov     al, r15b

                mov     [tokens+tokens_lenb-1+TOKEN.type], al
                jmp     .loop

      .error_unexpected_gt:
        test    al,al
        jz      @f
        movzx   eax, al
        jmp     .error_unexpected_gt.print
      @@:
        mov     rax, 'E'
      .error_unexpected_gt.print:
        sub     rsi, string
        asprintf s_tok_e_unexpected, ('>'), rax, rsi
        jmp     .error

      .error_unexpected_dash:
        test    al,al
        jz      @f
        movzx   eax, al
        jmp     .error_unexpected_dash.print
      @@:
        mov     rax, 'E'
      .error_unexpected_dash.print:
        sub     rsi, string
        asprintf s_tok_e_unexpected, ('-'), rax, rsi
        jmp     .error

      .error_unexpected_backslash:
        test    al,al
        jz      @f
        movzx   eax, al
        jmp     .error_unexpected_backslash.print
      @@:
        mov     rax, 'E'
      .error_unexpected_backslash.print:
        sub     rsi, string
        asprintf s_tok_e_unexpected, ('\'), rax, rsi
        jmp     .error

      .error_unexpected_slash:
        test    al,al
        jz      @f
        movzx   eax, al
        jmp     .error_unexpected_slash.print
      @@:
        mov     rax, 'E'
      .error_unexpected_slash.print:
        sub     rsi, string
        asprintf s_tok_e_unexpected, ('/'), rax, rsi
        jmp     .error

      .error_unknown:
        movzx   eax, al
        sub     rsi, string
        asprintf s_tok_e_unknown, rax, rsi

      .error:
        mov     r12, arg1
        mfree   tokens
        mov     rax, r12
        resti
        ret

      .done:
        bt      var_offsetm, 63
        jc      @f

        ;append var
        add     tokens_lenb, sizeof.TOKENVAR
        inc     tokens_len
        realloc tokens, tokens_lenb
        mov     tokens, rax

        mov     [tokens+tokens_lenb-sizeof.TOKENVAR+TOKEN.type], TOKEN_VAR
        mov     [tokens+tokens_lenb-sizeof.TOKENVAR+TOKENVAROFF.offset], var_offset

      @@:
        ;append end token
        inc     tokens_lenb
        inc     tokens_len
        realloc tokens, tokens_lenb
        mov     tokens, rax
        mov     [tokens+tokens_lenb-1+TOKEN.type], TOKEN_END

        xor     eax,eax
        mov     arg2, tokens
        mov     arg3, tokens_len
        rest
        ret


fbound
.token_lparen:
        mov     al, TOKEN_LPAREN
        jmp     .append

fbound
.token_rparen:
        mov     al, TOKEN_RPAREN
        jmp     .append

fbound
.token_not:
        mov     al, TOKEN_NOT
        jmp     .append

fbound
.token_and:
        cmp     byte[rsi], '&'
        je      .token_and.inc
        mov     al, TOKEN_AND
        jmp     .append

      .token_and.inc:
        inc     rsi
        mov     al, TOKEN_AND
        jmp     .append

fbound
.token_or:
        cmp     byte[rsi], '|'
        je      .token_or.inc
        mov     al, TOKEN_OR
        jmp     .append

      .token_or.inc:
        inc     rsi
        mov     al, TOKEN_OR
        jmp     .append

fbound
.token_sep:
        mov     al, TOKEN_SEQSEP
        jmp     .append

fbound
.token_dash:
        lodsb
        cmp     al, '>'
        jne     .error_unexpected_gt
        mov     al, TOKEN_IMP
        jmp     .append

fbound
.token_lt:
        lodsb
        cmp     al, '-'
        jne     .error_unexpected_dash
        lodsb
        cmp     al, '>'
        jne     .error_unexpected_gt
        mov     al, TOKEN_EQV
        jmp     .append

fbound
.token_slash:
        lodsb
        cmp     al, '\'
        jne     .error_unexpected_backslash
        mov     al, TOKEN_AND
        jmp     .append

fbound
.token_backslash:
        lodsb
        cmp     al, '/'
        jne     .error_unexpected_slash
        mov     al, TOKEN_OR
        jmp     .append

restore string,tokens,tokens_len,tokens_lenb,var_offset,var_offsetm



string      equ r12
tokens      equ rsi
vars        equ r13
vars_lenb   equ r14
cur_name    equ r15

;Group variables of sequent and transform TOKENVAROFFs to TOKENVARs
;Data pointed to by tokens will be updated, vars will contain TERMVARs and vars_len is the number of TERMVARs

;string, tokens, vars O, vars_len O
fbound
tokens_group_vars:
        save r12,r13,r14,r15,rbx,rsi,rdi
        mov     string, arg1
        mov     tokens, arg2

        xor     vars_lenb,vars_lenb
        malloc  vars_lenb
        mov     vars, rax

        align 16
        .loop:
                lodsb
                test    al,al
                jz      .done

                cmp     al, TOKEN_VAR
                jne     .loop

                xor     eax,eax
                lodsd
                lea     cur_name, [string+rax]
                lodsd   ;skip pad

                xor     ecx,ecx         ;byte index in vars

                mov     rbx, tokens      ;save
                align 16
                .equalvarloop:
                        cmp     rcx, vars_lenb
                        je      .no_vars_equal

                        mov     rsi, cur_name
                        mov     rdi, [vars+rcx]
                        inc     rdi             ;skip TERM.type

                        .equalcharloop:
                                cmpsb
                                jne     .var_not_equal
                                cmp     byte[rsi-1], 0
                                jnz     .equalcharloop
                                jmp     .var_equal      ;end reached

                      .var_not_equal:
                        add     rcx, 8
                        jmp     .equalvarloop

              .no_vars_equal: ;add var
                add     vars_lenb, 8
                realloc vars, vars_lenb
                mov     vars, rax

                ;find length
                mov     rdi, cur_name
                or      rcx, -1
                xor     al,al
                repne scasb     ;search for 0 terminator
                neg     rcx     ;rcx: length + 0 terminator + type

                mov     rdi, rcx ;save termsize
                malloc  rcx      ;allocate TERMVAR including type
                mov     rcx, rdi ;restore termsize for rep movsb
                mov     [vars+vars_lenb-8], rax         ;add ptr to new TERMVAR to vars

                ;init new TERMVAR
                mov     [rax+TERM.type], TERM_VAR       ;set type
                ;set name
                mov     rsi, cur_name
                lea     rdi, [rax+TERM.t1]
                dec     rcx  ;-TERM.t1
                rep movsb    ;also uses rcx

                mov     tokens, rbx     ;restore
                mov     [tokens-sizeof.TOKENVAR.term], rax ;change offset to ptr to new TERMVAR in tokens
                jmp     .loop

              .var_equal: ;[vars+rcx] = ptr to TERMVAR
                mov     tokens, rbx     ;restore

                mov     rcx, [vars+rcx]
                mov     [tokens-sizeof.TOKENVAR.term], rcx ;change offset to ptr to existing TERMVAR in tokens
                jmp     .loop

      .done:
        mov     arg3, vars
        mov     arg4, vars_lenb
        shr     arg4, 3
        rest
        ret
restore string,tokens,vars,vars_lenb,cur_name



split_sides.ERR_NO_SEP        = 1
split_sides.ERR_MULTIPLE_SEPS = 2

fside equ arg2

;Split sides of tokens on the TOKEN_SEP
; on success, fside will be a pointer to the fside, which is now preceded by a TOKEN_END instead of a TOKEN_SEP and error=0
; on error, error is one of split_sides.ERR_*

;tokens, fside O -> error
fbound
split_sides:
        save rsi
        mov     rsi, arg1

        align 16
        .loop:
                lodsb
                cmp     al, TOKEN_SEQSEP
                je      .found

                test    al,al
                jz      .none

                cmp     al, TOKEN_VAR
                jne     .loop
                lodsq
                jmp     .loop

      .found:
        mov     fside, rsi

        align 16
        .notmoreloop:
                lodsb
                test    al,al
                jz      .done

                cmp     al, TOKEN_VAR
                jne     @f
                lodsq
                jmp     .notmoreloop

              @@:
                cmp     al, TOKEN_SEQSEP
                jne     .notmoreloop
                jmp     .multiple

      .none:
        mov     rax, split_sides.ERR_NO_SEP
        resti
        ret

      .multiple:
        mov     rax, split_sides.ERR_MULTIPLE_SEPS
        resti
        ret

      .done:
        mov     byte[fside-1], TOKEN_END
        xor     eax,eax
        rest
        ret
restore sep_pos


vars_orig equ r12
vars_len  equ r13

;Free vars_len variables in vars

;vars, vars_len
fbound
free_variables:
        save r12,r13,rsi
        mov     vars_orig, arg1
        test    arg2,arg2
        jz      .free_vars_done
        mov     vars_len,  arg2

        mov     rsi, arg1
        align 16
        .free_vars_loop:
                lodsq
                mfree   rax
                dec     vars_len
                jnz     .free_vars_loop
      .free_vars_done:
        mfree   vars_orig
        rest
        ret
restore vars_orig, vars_len

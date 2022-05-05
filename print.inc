;String buffer
struct BUFFER
  buf      rq 1
  len      rq 1
  alloclen rq 1
ends

buf     equ rbx ;ptr to BUFFER

;(Not used)
macro doprintf fmt*, [params] {
    common
        save_inline r12
        asprintf fmt, params
        mov     r12, arg1
        doprint r12
        mfree   r12
        rest
}

;TODO make more efficient for additions of 1 or 2 chars?

;Append len chars of string to buf
macro doprint string*, len* {
        local ..buf_realloc, ..buf_skip_realloc
        ;invoke  GetWindowTextLength, [hOutput]
        ;invoke  SendMessage, [hOutput], EM_SETSEL, rax, rax
        ;invoke  SendMessage, [hOutput], EM_REPLACESEL, TRUE, string

        mov     rsi, string

        mov     rax, [buf+BUFFER.len]
        mov     rdi, rax
        add     rax, len
        mov     [buf+BUFFER.len], rax
        cmp     rax, [buf+BUFFER.alloclen]
        ja      ..buf_realloc

        add     rdi, [buf+BUFFER.buf]
        jmp     ..buf_skip_realloc

      ..buf_realloc:
        add     rax, 4096
        mov     [buf+BUFFER.alloclen], rax
        realloc [buf+BUFFER.buf], rax
        mov     [buf+BUFFER.buf], rax
        add     rdi, rax

      ..buf_skip_realloc:
        mov     rcx, len
        rep movsb

        ;cinvoke fprintf, [stdout], string
        ;cinvoke flushall
}

;See __asprintf
macro asprintf fmt*, [params] {
    common
        local argscount
        argscount = 0

    forward
        argscount = argscount + 1
    common
        if argscount and 1
          sub rsp, 8
        end if
    reverse
        push    params
    common
        mov     arg2, fmt
        sub     rsp, $20
        call    __asprintf
        if argscount and 1
          add rsp, $20 + 8*argscount + 8
        else
          add rsp, $20 + 8*argscount
        end if
}

;aka malloc&copy
macro asprintf_noparams fmt* {
        local fmt2
        if fmt eqtype rax
          if ~(fmt in <rbx,r13,r14,r15,rsi>)
            mov rsi,fmt
            fmt2 = rsi
          else
            fmt2 = fmt
          end if
        else
          fmt2 = fmt
        end if

        save_inline rsi,rdi,r12

        or      rcx, -1
        mov     rdi, fmt2
        xor     eax,eax
        repnz scasb
        neg     rcx
        lea     r12, [rcx-1]    ;r12: length + 0 terminator

        malloc  r12
        if ~(fmt2 eq rsi)
          mov     rsi, fmt2
        end if
        mov     rdi, rax
        mov     rcx, r12
        rep movsb

        mov     arg1, rax
        lea     rax, [r12-1]
       rest
}


fmt     equ r12
string  equ r13

;malloc&printf

;stack: reverse params
;string O, fmt -> chars_len
fbound
__asprintf:
        save r12,r13
        mov     fmt, arg2
        lea     r13, [rsp + $28 + 8 + $20]

        cinvoke vscprintf, fmt, r13     ;length (or -1 if fmt=NULL)
        inc     rax ;0 terminator

        malloc  rax
        mov     arg3, r13
        mov     string, rax
        cinvoke vsprintf, string, fmt   ;rax=len
        mov     arg1, string
        rest
        ret
restore fmt,params,string



macro do_indent {ccall print_indent, indent}

sequent equ r12

tside   equ [sequent+SEQUENT.tside]
fside   equ [sequent+SEQUENT.fside]
tlen    equ [sequent+SEQUENT.tlen]
flen    equ [sequent+SEQUENT.flen]
child1  equ [sequent+SEQUENT.child1]
child2  equ [sequent+SEQUENT.child2]
state   equ [sequent+SEQUENT_STATE.state]

if PRINTMODE=0
  include 'print_norm.inc'
else
  include 'print_adv.inc'
end if

restore sequent, tside,fside,tlen,flen,child1,child2
purge do_indent

if DEBUGSTR
  include 'print_tokens.inc'
end if

;indent
;buffer: rbx
indent  equ r12
fbound
print_indent:
        test    arg1,arg1
        jz      .ret

        save r12
        mov     indent, arg1
        align 16
        @@:
                doprint s_indent, 3
                dec     indent
                jnz     @b
        rest
      .ret:
        ret
restore indent, buf,buflen
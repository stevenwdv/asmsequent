format PE64 GUI
entry start

;TODO use 32b e.x more often

;make fbound insert bounds
DEBUGBOUND = FALSE
;enable console and deb*
DEBUGSTR   = FALSE
;print allocations etc (requires DEBUGSTR)
DEBUGMEM   = FALSE

;norm/adv
PRINTMODE  = 0

include '%inc%\win64a.inc'

include 'proc_mod.inc'
include 'memory.inc'

macro rpop [arg] {reverse pop arg}

macro fbound {
if DEBUGBOUND
      nop
      times 8 int3
      nop
end if
      align 16
}

macro debs s* {
if DEBUGSTR
      save_inline rax, rcx, rdx, r8, r9, r10, r11
;       cinvoke puts, s
        cinvoke fputs, s, [stdout]
        cinvoke fputc, 10, [stdout]
        cinvoke flushall
      rest
end if
}
macro debi i* {
if DEBUGSTR
      save_inline rax, rcx, rdx, r8, r9, r10, r11
;       cinvoke printf, s_debug_udec, i
        cinvoke fprintf, [stdout], s_debug_udec, i
        cinvoke flushall
      rest
end if
}
macro debx i* {
if DEBUGSTR
      save_inline rax, rcx, rdx, r8, r9, r10, r11
;       cinvoke printf, s_debug_hex, i
        cinvoke fprintf, [stdout], s_debug_hex, i
        cinvoke flushall
      rest
end if
}


;Tab control item
struct TCITEM
  mask        rd 1
  dwState     rd 1
  dwStateMask rd 2      ;2 because of padding, probably due to alignment
  pszText     rq 1
  cchTextMax  rd 1
  iImage      rd 1
  lParam      rd 1
ends


arg1 equ rcx
arg2 equ rdx
arg3 equ r8
arg4 equ r9

NEWL equ 13,10

;=====================================
section '.data' data readable writable
;=====================================

if DEBUGSTR
s_debug_udec            db "%llu",10,0
s_debug_hex             db "%#llx",10,0
end if

if DEBUGMEM
s_malloc                db "+",0
s_realloc               db "*%#llx",10,"+",0
s_free                  db "-%#llx",10,0
end if

s_newline               db NEWL,0
s_space                 db " ",0
s_err                   db "Error",0
s_err_tside             db "Error on true side",0
s_err_fside             db "Error on false side",0

s_e_no_sep              db "No @ separator seen",0
s_e_multiple_sep        db "Multiple @ separators seen",0
s_e_sep                 dq s_e_no_sep, s_e_multiple_sep

s_state_closed          db "Closed (always false)",0
s_state_open            db "Open (can be true)",0
s_states                dq s_state_closed, s_state_open

s_tableau               db "Tableau",0
s_valuations            db "Valuations",0

align 8

_heap                   rq 1

term_t_actions_jmp      dq t_actions.not, t_actions.and, t_actions.or, t_actions.imp, t_actions.eqv
term_f_actions_jmp      dq f_actions.not, f_actions.and, f_actions.or, f_actions.imp, f_actions.eqv

stdout                  rq 1

hDlg                    rq 1
hInput                  rq 1
hState                  rq 1
hTabs                   rq 1

hOutput                 rq 1
hValuations             rq 1

tabTableau              TCITEM TCIF_TEXT,,,s_tableau,,,
tabValuations           TCITEM TCIF_TEXT,,,s_valuations,,,


include 'print_strings.inc'
include 'parse_strings.inc'


;=======================================
section '.code' code readable executable
;=======================================

include 'sequent.inc'
include 'term.inc'
include 'print.inc'
include 'tokens.inc'
include 'parse.inc'
include 'term_actions.inc'


;hdlg, msg, wparam, lparam
fbound
dlg_proc:
        save_nothing

        cmp     arg2, WM_NOTIFY
        je      .notify

        cmp     arg2, WM_COMMAND
        je      .command

        cmp     arg2, WM_INITDIALOG
        je      .init

        cmp     arg2, WM_CLOSE
        je      .close

        jmp     .not_processed

      .notify:
        cmp     arg3, ID_TABS
        je      .switch_tab

        jmp     .not_processed

      .switch_tab:
        invoke  SendMessage, [hTabs], TCM_GETCURSEL, NULL, NULL

       save_inline rax
        invoke  ShowWindow, [hOutput+rax*8], SW_SHOW
       rest

        neg     rax
        inc     rax
        invoke  ShowWindow, [hOutput+rax*8], SW_HIDE

        jmp     .processed

      .command:
        cmp     arg3, BN_CLICKED shl 16 + ID_DOITBTN
        je      .solve

        jmp     .not_processed

      .solve:
        ccall   solve
        jmp     .processed

      .init:
        mov     [hDlg], arg1

        invoke  GetDlgItem, arg1, ID_INPUT
        mov     [hInput], rax

        invoke  GetDlgItem, [hDlg], ID_OUTPUT
        mov     [hOutput], rax

        invoke  GetDlgItem, [hDlg], ID_STATE
        mov     [hState], rax

        invoke  GetDlgItem, [hDlg], ID_TABS
        mov     [hTabs], rax

        invoke  GetDlgItem, [hDlg], ID_VALUATIONS
        mov     [hValuations], rax

        invoke  SendMessage, [hTabs], TCM_INSERTITEM, 0, tabTableau
        invoke  SendMessage, [hTabs], TCM_INSERTITEM, 1, tabValuations

        jmp     .processed

      .close:
        invoke  EndDialog, arg1, NULL

      .processed:
        mov     rax, TRUE
        resti
        ret

      .not_processed:
        xor     eax,eax
        rest
        ret


fbound
start:
        save_nothing

        cinvoke GetProcessHeap
        mov     [_heap], rax

if DEBUGSTR
        invoke  AllocConsole

        invoke  CreateFile, "CONOUT$", GENERIC_WRITE, 0, NULL, 3, FILE_ATTRIBUTE_NORMAL, NULL
        cinvoke open_osfhandle, rax, 1
        cinvoke fdopen, rax, "w"
        mov     [stdout], rax

;        cinvoke iob_func, 0
;        mov     rcx, [stdout]
;        mov     [rax+8], rcx
;
;        cinvoke iob_func, 0
;        add     rax, 8
;        cinvoke freopen, "CONOUT$", "w", rax
end if

        debs    " ====> starting..."
        invoke  GetModuleHandle, NULL
        invoke  DialogBoxParam, eax, 100, HWND_DESKTOP, dlg_proc, NULL

        invoke  ExitProcess, 0
        rest
        ret


;TODO optimize register count?

;Action when pressing Solve button
fbound
solve:
        save r12,r13,r14,r15,rsi,rbx

        ;==== GET INPUT SEQUENT ====

        invoke  GetWindowTextLength, [hInput]
        inc     rax             ;for 0 terminator, may be unnecessary? (TODO)
        mov     r12, rax        ;r12: buflen

        ;allocate buffer
        malloc  rax
        string equ rbx
        mov     string, rax

        invoke  GetWindowText, [hInput], rax, r12

        ;==== TOKENIZE ====

        debs    " ====> tokenizing..."

        ccall   tokenize, string
        test    rax,rax
        jnz     .error_tokenize
        tokens equ r12
        mov     tokens, arg2

        ;==== GROUP VARIABLES ====

        debs    " ====> grouping..."
        ccall   tokens_group_vars, string ;, arg2/tokens
        vars     equ rsi
        vars_len equ r15
        mov     vars, arg3
        mov     vars_len, arg4
        mfree   string

        ;==== SPLIT SIDES ====

        debs    " ====> splitting..."
        ccall   split_sides, tokens
        test    al,al
        jnz     .error_split
        tokens_fside equ r13
        mov     tokens_fside, arg2

        ;==== PARSE ====

        ;tside
        debs    " ====> parsing tside..."
        ccall   parse, tokens
        term_tside equ r14
        test    rax,rax
        jnz     .error_parse_tside
        mov     term_tside, arg1

        ;fside
        debs    " ====> parsing fside..."
        ccall   parse, tokens_fside
        restore tokens_fside
        term_fside equ r13
        test    rax,rax
        jnz     .error_parse_fside
        mov     term_fside, arg1

        mfree   tokens


        ;==== PREPARE SEQUENT ====

        debs    " ====> allocating sequent..."

        malloc  sizeof.SEQUENT_STATE
        mov     [rax+SEQUENT.child1], NULL
        mov     [rax+SEQUENT.child2], NULL
        sequent equ r12
        mov     sequent, rax

        macro alloc_side sidepre* {
                local ..empty, ..done

                test    term_#sidepre#side,term_#sidepre#side
                jz      ..empty

                malloc  8
                mov     [sequent+SEQUENT.#sidepre#side], rax
                mov     [sequent+SEQUENT.#sidepre#len], 1
                mov     [rax], term_#sidepre#side
                jmp     ..done

              ..empty:
                malloc  0
                mov     [sequent+SEQUENT.#sidepre#side], rax
                mov     [sequent+SEQUENT.#sidepre#len], 0

              ..done:
        }

        alloc_side t
        alloc_side f
        purge alloc_side


        ;==== SOLVE ====

        debs    " ====> solving..."
        ccall   seq_solve, sequent, NULL
        ;mov     sequent, arg1


        ;==== PRINT SOLUTION ====

        debs    " ====> printing solution..."

        invoke  SetWindowText, [hState], [s_states+rax*8]


        ;allocate buffer
        malloc  4096
        sub     rsp, 8
        push    4096
        push    0
        push    rax

        mov     arg1, rsp
       frame
        ccall   print_seq, , sequent, 0
       endf

        buf     equ r14
        buflen  equ r13
        pop     buf
        pop     buflen
        add     rsp, $10

        ;append 0 terminator
        inc     buflen
        realloc buf, buflen
        mov     buf, rax
        mov     byte[rax+buflen-1], 0

        invoke  SetWindowText, [hOutput], rax
        mfree   buf
        restore buf,buflen


        debs    " ====> printing valuations..."
        invoke  SendMessage, [hValuations], LB_RESETCONTENT, NULL, NULL
        ccall   add_valuations, sequent


        ;==== FREE ====

        debs    " ====> freeing resources..."

        cmp     [sequent+SEQUENT.tlen], 0
        jz      @f

        mov     rax, [sequent+SEQUENT.tside]
        ccall   term_free, qword[rax]
      @@:

        cmp     [sequent+SEQUENT.flen], 0
        jz      @f

        mov     rax, [sequent+SEQUENT.fside]
        ccall   term_free, qword[rax]
      @@:

        ccall   seq_free, sequent

        ccall   free_variables, vars, vars_len

        resti
        ret


.error_tokenize:
        mov     r12, rax
        mfree   string
        invoke  MessageBox, [hDlg], r12, s_err, MB_ICONERROR+MB_OK
        mfree   r12
        resti
        ret

.error_split:
        mov     r13, rax
        mfree   tokens
        ccall   free_variables, vars, vars_len
        invoke  MessageBox, [hDlg], [s_e_sep + 8*(r13-split_sides.ERR_NO_SEP)], s_err, MB_ICONERROR+MB_OK
        resti
        ret

.error_parse_tside:
        mov     r13, rax
        mfree   tokens
        ccall   free_variables, vars, vars_len
        invoke  MessageBox, [hDlg], r13, s_err_tside, MB_ICONERROR+MB_OK
        mfree   r13
        resti
        ret

.error_parse_fside:
        mov     r13, rax
        mfree   tokens

        test    term_tside,term_tside
        jz      .error_parse_fside.skip_free

        ccall   term_free, term_tside
      .error_parse_fside.skip_free:

        ccall   free_variables, vars, vars_len
        invoke  MessageBox, [hDlg], r13, s_err_fside, MB_ICONERROR+MB_OK
        mfree   r13
        rest
        ret

restore vars,vars_len,sequent,tokens,term_tside,term_fside



sequent equ r12

;Add valuations (from open leaf sequents) to hValuations list
;TODO no duplicate vars + no duplicate valuations

;sequent
fbound
add_valuations:
        save r12
        mov     sequent, arg1

        cmp     [sequent+SEQUENT.child1], NULL
        je      .leaf

        ccall   add_valuations, [sequent+SEQUENT.child1]
        cmp     [sequent+SEQUENT.child2], NULL
        jne     .two_childs
        resti
        ret

      .two_childs:
        ccall   add_valuations, [sequent+SEQUENT.child2]
        resti
        ret

      .leaf:
        cmp     [sequent+SEQUENT_STATE.state], SEQUENT_OPEN
        je      .open
        resti
        ret

      .open:
       save_inline rsi,rdi
        mov     rdx, 3  ;rdx: tot length

        macro add_side prefix* {
                local ..loop, ..skip

                mov     r8, [sequent+SEQUENT.#prefix#len]
                test    r8,r8
                jz      ..skip
                mov     rsi, [sequent+SEQUENT.#prefix#side]
                align 16
                ..loop:
                        lodsq
                        lea     rdi, [rax+TERMVAR.t1]
                        xor     eax,eax
                        or      rcx, -1
                        repnz scasb
                        neg     rcx
                        dec     rcx             ;rcx: var length + 1 for ' '

                        add     rdx, rcx
                        dec     r8
                        jnz     ..loop
              ..skip:
        }

        add_side t
        add_side f
        purge add_side

        mov     rax, rdx
        malloc  rax
        mov     rdi, rax
        mov     r10, rax

        macro copy_side prefix* {
                local ..loop, ..char_loop, ..char_done, ..skip

                mov     r8, [sequent+SEQUENT.#prefix#len]
                test    r8,r8
                jz      ..skip
                mov     r9, [sequent+SEQUENT.#prefix#side]
                align 16
                ..loop:
                        mov     rsi, [r9]      ;TERMVAR ptr
                        inc     rsi             ;skip TERM.type
                        ..char_loop:
                                lodsb
                                test    al,al
                                jz      ..char_done

                                stosb
                                jmp     ..char_loop

                      ..char_done:
                        mov     eax, ' '
                        stosb

                        add     r9, 8
                        dec     r8
                        jnz     ..loop
              ..skip:
        }

        copy_side t

        mov     eax, '|'
        stosb
        mov     eax, ' '
        stosb

        copy_side f

        purge copy_side
        mov     byte[rdi], 0

        mov     rdi, r10
        invoke  SendMessage, [hValuations], LB_ADDSTRING, , r10
        mfree   rdi

       rest

        rest
        ret
restore sequent



;====================================
section '.idata' import data readable
;====================================

library kernel,'kernel32.dll',\
        user,'user32.dll',\
        msvcrt,'msvcrt.dll'

import  kernel,\
        GetProcessHeap,'GetProcessHeap',\
        GetModuleHandle,'GetModuleHandleA',\
        HeapAlloc,'HeapAlloc',\
        HeapReAlloc,'HeapReAlloc',\
        HeapFree,'HeapFree',\
        HeapWalk,'HeapWalk',\
        ExitProcess,'ExitProcess',\
        AllocConsole,'AllocConsole',\
        CreateFile,'CreateFileA',\
        GetTickCount64,'GetTickCount64'

import  user,\
        DialogBoxParam,'DialogBoxParamA',\
        MessageBox,'MessageBoxA',\
        SendMessage,'SendMessageA',\
        GetDlgItem,'GetDlgItem',\
        GetWindowTextLength,'GetWindowTextLengthA',\
        GetWindowText,'GetWindowTextA',\
        SetWindowText,'SetWindowTextA',\
        EndDialog,'EndDialog',\
        ShowWindow,'ShowWindow'

import  msvcrt,\
        vscprintf,'_vscprintf',\
        vsprintf,'vsprintf',\
        fputs,'fputs',\
        fputc,'fputc',\
        getchar,'getchar',\
        fprintf,'fprintf',\
        fdopen,'_fdopen',\
        open_osfhandle,'_open_osfhandle',\
        flushall,'_flushall',\
        printf,'printf',\
        puts,'puts',\
        putchar,'putchar',\
        iob_func,'__iob_func',\
        freopen,'freopen'


;=====================================
section '.rsrc' resource data readable
;=====================================

directory     RT_DIALOG, dialogs,\
              RT_MANIFEST, manifests,\
              RT_VERSION, vinfos


resource      dialogs, 100, LANG_ENGLISH + SUBLANG_DEFAULT, main_dialog

;CONTROL\s*(".*?[^\\]"|""|'.*?[^\\]'|'')\s*,\s*(\d+?)\s*,\s*(.+?)\s*,\s*(.+?)\s*,\s*(\d+?)\s*,\s*(\d+?)\s*,\s*(\d+?)\s*,\s*(\d+)
;  dialogitem "$3", $1, $2, $5,$6,$7,$8, $4

ID_INPUT      = 101
ID_OUTPUT     = 102
ID_DOITBTN    = 103
ID_STATE      = 104
ID_TABS       = 105
ID_VALUATIONS = 106

NEWL equ ,13,10,

;"  a&b|c->(d<->e)|!f || g&&h" NEWL "@" NEWL "  a/\b\/c->(d<->e)\/~f"
;Long:
;a&b|c->(d<->e)<->a&b|c->(d<->e)<->a&b|c->(d<->e) @ a&b|c->(d<->e)->HEYhello<->!!hi&a&b|c->(d<->e)->HEYhello<->!!hi

dialog main_dialog, "ASM Sequent", 0,0,470,340, DS_CENTER + WS_CAPTION + WS_SYSMENU + WS_MINIMIZEBOX, WS_EX_CLIENTEDGE, , "Consolas",10
       dialogitem "BUTTON", "Input: sequent", -1, 2,0,466,70, BS_GROUPBOX + WS_CHILD + WS_VISIBLE + WS_GROUP
       dialogitem "EDIT", "  a&b|c->(d<->e)|!f || g&&h" NEWL "@" NEWL "  a/\b\/c->(d<->e)\/~f",\
              ID_INPUT, 4,9,463,58, ES_LEFT + ES_MULTILINE + ES_WANTRETURN + WS_CHILD + WS_VISIBLE + WS_BORDER + WS_VSCROLL + WS_HSCROLL + WS_TABSTOP

       dialogitem "SysTabControl32", "", ID_TABS, 1,71,469,237+15, TCS_TABS + TCS_MULTILINE + WS_CHILD + WS_VISIBLE + WS_TABSTOP

       dialogitem "EDIT", "", ID_OUTPUT, 4,86,463,234, ES_LEFT + ES_MULTILINE + ES_READONLY + WS_CHILD + WS_VISIBLE + WS_BORDER + WS_VSCROLL + WS_HSCROLL + WS_TABSTOP
       dialogitem "LISTBOX", "", ID_VALUATIONS, 4,86,463,234, LBS_STANDARD + WS_CHILD + WS_TABSTOP + LBS_EXTENDEDSEL

       dialogitem "BUTTON", "Solve!", ID_DOITBTN, 407,324,60,14, BS_PUSHBUTTON + WS_CHILD + WS_VISIBLE + WS_GROUP + WS_TABSTOP
       dialogitem "STATIC", "", ID_STATE, 4,327,133,10, SS_LEFT + WS_CHILD + WS_VISIBLE + WS_GROUP
enddialog



resource      manifests,\
              1, LANG_ENGLISH + SUBLANG_DEFAULT, manifest

resdata manifest
;Use visual styles
  db '<assembly xmlns="urn:schemas-microsoft-com:asm.v1" manifestVersion="1.0"><dependency><dependentAssembly>'
  db '<assemblyIdentity type="Win32" name="Microsoft.Windows.Common-Controls" version="6.0.0.0" processorArchitecture="amd64" '
  db 'publicKeyToken="6595b64144ccf1df" language="*"></assemblyIdentity></dependentAssembly></dependency></assembly>'
endres


resource      vinfos,\
              1, LANG_ENGLISH + SUBLANG_DEFAULT, vinfo

COPY equ ,$a9, ;(c)
versioninfo vinfo, VOS__WINDOWS32, VFT_APP, VFT2_UNKNOWN, LANG_ENGLISH + SUBLANG_DEFAULT, 0,\
            'FileDescription','Assembly sequent solver',\
            'FileVersion','1.0',\
            'ProductVersion','1.0',\
            'ProductName','Assembly sequent solver',\
            'LegalCopyright','Copyright ' COPY ' Steven WdV'

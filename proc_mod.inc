purge fastcall

;Just taken from FASM source and modfified
;Now uses unaligned qwords variable
macro fastcall proc,[arg]
 { common local stackspace,argscount,counter
    if argscount < 4
     qwords = 4
    else
     qwords = argscount
    end if
    counter = 0
    if qwords
     if defined current@frame
      if current@frame < qwords
       current@frame = qwords
      end if
     else
      if argscount and 1
       qwords = argscount+1
      end if
      if qwords
       sub rsp, qwords*8
      end if
     end if
    end if
   forward
    counter = counter + 1
    define type@param
    define definition@param arg
    match =float value,definition@param
    \{ define definition@param value
       define type@param float \}
    match =addr value,definition@param
    \{ define definition@param value
       define type@param addr \}
    match any=,any,definition@param
    \{ \local ..string,..continue
       jmp ..continue
       align sizeof.TCHAR
       ..string TCHAR definition@param,0
       ..continue:
       define definition@param ..string
       define type@param addr \}
    match any,definition@param
    \{ match \`any,any
       \\{ \\local ..string,..continue
           jmp ..continue
           align sizeof.TCHAR
           ..string TCHAR definition@param,0
           ..continue:
           define definition@param ..string
           define type@param addr \\} \}
    match param,definition@param
    \{ local opcode,origin
       size@param = 0
       if param eqtype 0 | param eqtype 0f | type@param eq addr
        size@param = 8
       else if param eqtype byte 0 | param eqtype byte 0f
        match prefix value,definition@param
         \\{ if prefix eq qword
              size@param = 8
             else if prefix eq dword
              size@param = 4
             else if prefix eq word
              size@param = 2
             else if prefix eq byte
              size@param = 1
             end if \\}
       else if ~ param in <xmm0,xmm1,xmm2,xmm3,xmm4,xmm5,xmm6,xmm7,xmm8,xmm9,xmm10,xmm11,xmm12,xmm13,xmm14,xmm15>
        virtual
         origin = $
         inc param
         load opcode byte from origin
         if opcode = 67h | opcode = 41h
          load opcode byte from origin+1
         end if
         if opcode and 0F8h = 48h
          size@param = 8
         else if opcode = 66h
          size@param = 2
         else if opcode = 0FFh
          size@param = 4
         else
          size@param = 1
         end if
        end virtual
       end if
       if counter = 1
        if type@param eq float
         if ~ param eq xmm0
          if size@param = 4
           if param eqtype byte 0 | param eqtype byte 0f
            mov eax,param
            movd xmm0,eax
           else
            movd xmm0,param
           end if
          else
           if param eqtype 0 | param eqtype 0f | param eqtype byte 0 | param eqtype byte 0f
            mov rax,param
            movq xmm0,rax
           else
            movq xmm0,param
           end if
          end if
         end if
         if vararg@fastcall & ~ param eq rcx
           movq rcx,xmm0
         end if
        else if type@param eq addr
         if ~ param eq rcx
          lea rcx,[param]
         end if
        else if size@param = 8
         if ~ param eq rcx
          mov rcx,param
         end if
        else if size@param = 4
         if ~ param eq ecx
          mov ecx,param
         end if
        else if size@param = 2
         if ~ param eq cx
          mov cx,param
         end if
        else if size@param = 1
         if ~ param eq cl
          mov cl,param
         end if
        end if
       else if counter = 2
        if type@param eq float
         if ~ param eq xmm1
          if size@param = 4
           if param eqtype byte 0 | param eqtype byte 0f
            mov eax,param
            movd xmm1,eax
           else
            movd xmm1,param
           end if
          else
           if param eqtype 0 | param eqtype 0f | param eqtype byte 0 | param eqtype byte 0f
            mov rax,param
            movq xmm1,rax
           else
            movq xmm1,param
           end if
          end if
         end if
         if vararg@fastcall & ~ param eq rdx
           movq rdx,xmm1
         end if
        else if type@param eq addr
         if ~ param eq rdx
          lea rdx,[param]
         end if
        else if size@param = 8
         if ~ param eq rdx
          mov rdx,param
         end if
        else if size@param = 4
         if ~ param eq edx
          mov edx,param
         end if
        else if size@param = 2
         if ~ param eq dx
          mov dx,param
         end if
        else if size@param = 1
         if ~ param eq dl
          mov dl,param
         end if
        end if
       else if counter = 3
        if type@param eq float
         if ~ param eq xmm2
          if size@param = 4
           if param eqtype byte 0 | param eqtype byte 0f
            mov eax,param
            movd xmm2,eax
           else
            movd xmm2,param
           end if
          else
           if param eqtype 0 | param eqtype 0f | param eqtype byte 0 | param eqtype byte 0f
            mov rax,param
            movq xmm2,rax
           else
            movq xmm2,param
           end if
          end if
         end if
         if vararg@fastcall & ~ param eq r8
           movq r8,xmm2
         end if
        else if type@param eq addr
         if ~ param eq r8
          lea r8,[param]
         end if
        else if size@param = 8
         if ~ param eq r8
          mov r8,param
         end if
        else if size@param = 4
         if ~ param eq r8d
          mov r8d,param
         end if
        else if size@param = 2
         if ~ param eq r8w
          mov r8w,param
         end if
        else if size@param = 1
         if ~ param eq r8b
          mov r8b,param
         end if
        end if
       else if counter = 4
        if type@param eq float
         if ~ param eq xmm3
          if size@param = 4
           if param eqtype byte 0 | param eqtype byte 0f
            mov eax,param
            movd xmm3,eax
           else
            movd xmm3,param
           end if
          else
           if param eqtype 0 | param eqtype 0f | param eqtype byte 0 | param eqtype byte 0f
            mov rax,param
            movq xmm3,rax
           else
            movq xmm3,param
           end if
          end if
         end if
         if vararg@fastcall & ~ param eq r9
           movq r9,xmm3
         end if
        else if type@param eq addr
         if ~ param eq r9
          lea r9,[param]
         end if
        else if size@param = 8
         if ~ param eq r9
          mov r9,param
         end if
        else if size@param = 4
         if ~ param eq r9d
          mov r9d,param
         end if
        else if size@param = 2
         if ~ param eq r9w
          mov r9w,param
         end if
        else if size@param = 1
         if ~ param eq r9b
          mov r9b,param
         end if
        end if
       else
         if type@param eq addr
          lea rax,[param]
          mov [rsp+(counter-1)*8],rax
         else if param eqtype [0] | param eqtype byte [0]
          if size@param = 8
           mov rax,param
           mov [rsp+(counter-1)*8],rax
          else if size@param = 4
           mov eax,param
           mov [rsp+(counter-1)*8],eax
          else if size@param = 2
           mov ax,param
           mov [rsp+(counter-1)*8],ax
          else
           mov al,param
           mov [rsp+(counter-1)*8],al
          end if
         else if size@param = 8
          virtual
           origin = $
           mov rax,param
           load opcode byte from origin+1
          end virtual
          if opcode = 0B8h
           mov rax,param
           mov [rsp+(counter-1)*8],rax
          else
           mov qword [rsp+(counter-1)*8],param
          end if
         else if param in <xmm0,xmm1,xmm2,xmm3,xmm4,xmm5,xmm6,xmm7,xmm8,xmm9,xmm10,xmm11,xmm12,xmm13,xmm14,xmm15>
          movq [rsp+(counter-1)*8],param
         else
          mov [rsp+(counter-1)*8],param
         end if
       end if \}
   common
    argscount = counter
    call proc
    if qwords & ~defined current@frame
     add rsp, qwords*8
    end if }





purge frame
purge endf

macro frame0 odd* {
      local qwords, current
      if qwords + (1-(qwords and 1)) * odd + (qwords and 1) * (1-odd)
        sub rsp, (qwords + (1-(qwords and 1)) * odd + (qwords and 1) * (1-odd)) * 8
      end if
      current = 0
      current@frame equ current
      qwords@frame equ qwords
      odd@frame equ odd
}
macro frame {frame0 FALSE}
macro frame_odd {frame0 TRUE}

macro endfi {
      local qwords
      qwords = qwords@frame + (1-(qwords@frame and 1)) * odd@frame + (qwords@frame and 1) * (1-odd@frame)
      if qwords
       add rsp, qwords*8
      end if
}

macro endf {
      qwords@frame = current@frame
      endfi
      restore qwords@frame,current@frame,odd@frame
}


;==== For functions ====

macro save_func0 do_odd*, noalign*, do_save*, [arg] {
  common
      local argscount
      argscount = 0
  forward
      if do_save
        argscount = argscount + 1
        if argscount <= 4
          mov     [rsp + argscount*8], arg
        else
          push    arg
        end if
      end if
  common
      local odd
      odd = FALSE

      if argscount <= 4
        if do_odd
          odd = TRUE
        end if
      else if noalign
        odd = argscount and 1
      else if argscount and 1 xor do_odd
        odd = TRUE
      end if

      frame0 odd

      macro fin@save \{
        common
            local revcount
            revcount = argscount
            endfi

        reverse
            if do_save
              if revcount > 4
                pop   arg
              else
                mov   arg, [rsp + revcount*8]
              end if
              revcount = revcount - 1
            end if
        common
      \}
}

macro save [arg] {common save_func0 TRUE, FALSE, TRUE, arg}
macro save_nothing [arg] {common save_func0 TRUE, FALSE, FALSE}
macro save_noalign [arg] {common save_func0 NULL, TRUE, TRUE, arg}


;=== For inline use ===

macro save_inline0 do_odd*, noalign*, [arg] {
  common
      local argscount
      argscount = 0
  forward
      argscount = argscount + 1
      push arg
  common

      frame0 noalign*(argscount and 1) + (1-noalign)*(argscount and 1 xor do_odd)

      macro fin@save \{
        common
            local revcount
            revcount = argscount
            endfi

        reverse
            pop   arg
        common
      \}
}

macro save_inline [arg] {common save_inline0 FALSE, FALSE, arg}
macro save_inline_odd [arg] {common save_inline0 TRUE, FALSE, arg}
macro save_inline_noalign [arg] {common save_inline0 NULL, TRUE, arg}
;save_nothing_inline would be just to use frame




macro resti {fin@save}
macro rest {
      qwords@frame = current@frame
      fin@save
      restore qwords@frame,current@frame,odd@frame
      purge fin@save
}

;s=`...`.split("\n")
;{const m=[],r=s=>m.splice(m.indexOf(s),1),f={"-":r,"*":r,"+":s=>m.push(s)};for(const l of s)f[l[0]]&&f[l[0]](l.substr(1));m}

macro malloc size* {
if DEBUGMEM
if size eqtype rax
     save_inline size
      cinvoke fprintf, [stdout], s_malloc, qword[rsp+20h+8]
      cinvoke flushall
     rest
else
      cinvoke fprintf, [stdout], s_malloc, size
      cinvoke flushall
end if
;int3
end if
      cinvoke HeapAlloc,[_heap],HEAP_GENERATE_EXCEPTIONS,size
if DEBUGMEM
      debx    rax
end if
}

macro realloc ptr*, size* {
if DEBUGMEM
if size eqtype rax
     save_inline size,ptr
      cinvoke fprintf, [stdout], s_realloc, qword[rsp+20h]      ;size left out
      cinvoke flushall
     rest
else
     save_inline ptr
      cinvoke fprintf, [stdout], s_realloc, qword[rsp+20h+8]    ;size left out
      cinvoke flushall
     rest
end if
;int3
end if
      cinvoke HeapReAlloc,[_heap],HEAP_GENERATE_EXCEPTIONS,ptr,size
if DEBUGMEM
      debx    rax
end if
}

macro mfree ptr {
if DEBUGMEM
     save_inline ptr
      cinvoke fprintf, [stdout], s_free, qword[rsp+20h+8]
      cinvoke flushall
     rest
end if
      cinvoke HeapFree,[_heap],HEAP_GENERATE_EXCEPTIONS,ptr
}
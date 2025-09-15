                title   'IBM PC system routines'
                name    pixel
                page    55,132

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                             ;
; IBM PC SPECIFIC SYSTEM ROUTINES                             ;
;                                                             ;
; Contains various system routines.                           ;
; To avoid various DOS oddities, the lowest level             ;
; implementation as possible is used.                         ;
;                                                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

                .386
                .model  small, c

                .code

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                             ;
; Rename file                                                 ;
;                                                             ;
; Renames a file from the first filename to the second.       ;
;                                                             ;
; Function rename(var a, b: stringt): byte;                   ;
;                                                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

rename  proc    syscall a, b: dword
        mov     ah,056h
        mov     edx,a
        mov     edi,b
        int     21h
        jc      rename01
        mov     ax,0
rename01:
        ret
rename  endp

        end

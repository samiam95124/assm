!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
!                            8080 -> 386 TRANSLATOR                           !
!                                                                             !
!                       COPYRIGHT (C) 1999 S. A. MOORE                        !
!                                                                             !
! Contains the lookup table for 8080 instructions, and the code building      !
! routines.                                                                   !
! The 8080 registers are placed in corresponding 386 registers as follows:    !
!                                                                             !
! 8080      386                                                               !
! ========================                                                    !
! f (flag)  (flag)                                                            !
! a         al                                                                !
! b         bh                                                                !
! c         bl                                                                !
! d         dh                                                                !
! e         dl                                                                !
! h         ch                                                                !
! l         cl                                                                !
!                                                                             !
! In the runtime, esi keeps the base of the original program, and all data    !
! accesses use that as a base.                                                !
! The top ends of ebx, edx, and ecx are kept clear (0). This allows their use !
! in addressing offsets.                                                      !
! The original code is at esi, just past the instruction byte for the lookup. !
! The code being build is at edi, and should be returned just after the       !
! instruction built.                                                          !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
! Register equates
!
rgeax:  equ     $00
rgebx:  equ     $03
rgecx:  equ     $01
rgedx:  equ     $02
rgesi:  equ     $06
rgedi:  equ     $07
rgebp:  equ     $05
rgesp:  equ     $04
rgax:   equ     $00
rgbx:   equ     $03
rgcx:   equ     $01
rgdx:   equ     $02
rgal:   equ     $00
rgah:   equ     $04
rgbl:   equ     $03
rgbh:   equ     $07
rgcl:   equ     $01
rgch:   equ     $05
rgdl:   equ     $02
rgdh:   equ     $06
!
! Size overrides
!
opovr:  equ     $66                     ! operand size override
adovr:  equ     $67                     ! address size override
!
! Translator cases. Each case translates a single instruction. For a small
! instruction set as the 8080, we just provide a case for all 256 instructions.
! The comments indicate what instruction we are building.
!
inop:   movb    [edi],$90               ! nop
        inc     edi
        ret
!
ilxib:  movb    [edi],opovr             ! mov bx,imm
        inc     edi
        movb    [edi],$b8+rgbx
        inc     edi
        movsw
        ret
!
istaxb: movb    [edi],$88               ! mov [esi+ebx],al
        inc     edi
        movb    [edi],$00+rgal*8+$04
        inc     edi
        movb    [edi],$00+rgbx*8+rgesi
        inc     edi
        ret
!
****** inc bx not quite eqivalent to inx b, because inx b
does not affect flags at all, whereas inc bx affects zero, parity,
etc.
iinxb:  movb    [edi],opovr             ! inc bx
        inc     edi
        movb    [edi],$40+rgbx
        inc     edi
        ret
!
iinrb:  movb    [edi],$fe               ! inc bh
        inc     edi
        movb    [edi],$c0+$00*8+rgbh
        inc     edi
        ret
                
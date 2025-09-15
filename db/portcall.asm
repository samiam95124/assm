!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
!                   PORTABLE SYSTEM CALLS DEFINITION FILE                     !
!                                                                             !
!                      Copyright (C) 1994 S. A. Moore                         !
!                          All rights reserved                                !
!                                                                             !
! The portable calls system is used to gain access to system functions from   !
! a DB simulated task. The task executes an instruction code that is illegal  !
! on the real world CPU, and DB recognizes it as a signal to execute a        !
! system call. It is "portable" because the basic formats, methods and        !
! functions are identical on all target CPUs. Only the registers used and     !
! the illegal instruction used to "trip" the call change.                     !
!                                                                             !
! This file defines the call formats, codes and error returns for the         !
! portable calls format used with DB. Typically, it is identical to all the   !
! files for other CPUs except for the "leader" of the call, which is          !
! choosen to be an instruction that is completely illegal, and hopefully an   !
! instruction that will allways be so. However, since there is no garantee    !
! that this is so, you should allways use this file and be ready to           !
! reassemble to it when required. This will only happen if you move "up"      !
! in a CPU series, to a more powerful version of the same CPU with extended   !
! instructions, and the new CPU has made use of our unfortunate choice of     !
! leader instruction.                                                         !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! Call codes
!
_prt_term:      equ     0               ! Terminate program
_prt_open:      equ     1               ! Open file
_prt_close:     equ     2               ! close file
_prt_read:      equ     3               ! Read from file
_prt_write:     equ     4               ! write to file
!
! Error codes
!
_err_null:      equ     0               ! No error
_err_call:      equ     1               ! Invalid call number
_err_mod:       equ     2               ! Invalid file mode
_err_full:      equ     3               ! File table full
_err_ivfn:      equ     4               ! Invalid filename
_err_exist:     equ     5               ! Non-existant file
_err_filn:      equ     6               ! File descriptor passed is bad
_err_eof:       equ     7               ! End of file reached
!
! Open mode bits
!
_mod_read:      equ     1 shl 0         ! Open for reading
_mod_write:     equ     1 shl 1         ! Open for writing

!
! Execute portable system call, assigned to illegal CPU instruction
!
prtcal: macro   function
        defb    $ed, $ff, function
        endmac
!
! Open file for read by fixed name. Returns the file id in C.
!
openrf: macro   filename
        push    b                       ! save registers
        push    h
        mvi     b,_mod_read             ! set read mode
        mvi     a,len 'filename'        ! set filename length
        lxi     h,oprf____str           ! index filename string
        prtcal  _prt_open               ! open the file
        jnc     oprf____done            ! exit if good
        prtcal  _prt_term               ! else terminate with error
oprf____str:
        defb    'filename'              ! filename to open
oprf____done:
        mov     a,c                     ! save fid
        pop     h                       ! restore registers
        pop     b
        mov     c,a                     ! restore fid
        endmac
!
! Open file for write by fixed name. Returns the file id in C.
!
openwf: macro   filename                ! this macro

        push    b                       ! save registers
        push    h
        mvi     b,_mod_write            ! set read mode
        mvi     a,len 'filename'        ! set filename length
        lxi     h,oprf____str           ! index filename string
        prtcal  _prt_open               ! open the file
        jnc     oprf____done            ! exit if good
        prtcal  _prt_term               ! else terminate with error
oprf____str:
        defb    'filename'              ! filename to open
oprf____done:
        mov     a,c                     ! save fid
        pop     h                       ! restore registers
        pop     b
        mov     c,a                     ! restore fid
        endmac
!
! Close file. Closes the file by the id in C.
!
close:  macro
        prtcal  _prt_close              ! close file
        jnc     close____done           ! no error, exit
        prtcal  _prt_term               ! else terminate with error
close____done:
        endmac
!
! Write string to file
!
writes: macro   string
        push    d                       ! save registers
        push    h
        lxi     d,len string            ! set length of string
        lxi     h,wrs____str            ! index string
        prtcal  _prt_write              ! write
        jnc     wrs____done             ! exit if good
        prtcal  _prt_term               ! else terminate with error
wrs____str:
        defb    string                  ! string to print
wrs____done:
        pop     h                       ! restore registers
        pop     d
        endmac

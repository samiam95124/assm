!
! Hello under Win95
!
        m386                            ! use portable 386

cr:     equ     $0d                     ! carriage return
lf:     equ     $0a                     ! line feed
!
! From "winbase.h"
!
std_input_handle:       equ     -10     ! return standard input handle
std_output_handle:      equ     -11     ! return standard output handle
std_error_handle:       equ     -12     ! return standard error handle

start:
        mov     eax,std_output_handle   ! set return standard output handle
        push    eax
        calll   [getstdhandle]          ! get the handle
        pushd   0                       ! set no overlap
        pushd   bytcnt                  ! set address of byte counter
        pushd   msglen                  ! set character count
        pushd   message                 ! set address of message
        push    eax                     ! set stdout handle
        calll   [writefile]             ! write the message
        ret                             ! done, exit
!
! Hello message
!
message:                                ! message
        defb    'Hello, Windows 95 !', cr, lf
msglen: equ     21                      ! length of message
!
! Variables
!
bytcnt: defvs   4                       ! dummy, holds bytes written

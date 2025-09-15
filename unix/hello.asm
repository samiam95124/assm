!
! Hello under Linux
!
        m386                            ! use portable 386

cr:     equ     $0d                     ! carriage return
lf:     equ     $0a                     ! line feed

start:
        mov     eax,4                   ! Set system call "write"
        mov     ebx,1                   ! set stdout file
        mov     ecx,message             ! index message to write
        mov     edx,msglen              ! set message length
        int     $80                     ! execute system call

! terminate program via _exit () system call

        mov     eax,1                   ! Set system call "exit"
        mov     ebx,0                   ! clear program exit code
        int     $80                     ! execute system
!
! Hello message
!
message:                                ! message
        defb    'Hello, Linux !', cr, lf
msglen: equ     16                      ! length of message

space:  defvs   40
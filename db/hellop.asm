!
! Print "hello, world" using portable system calls
!
        include portcall                ! use portable calls definition

cr:     equ     13                      ! carriage return
lf:     equ     10                      ! line feed

start:
        openwf  _output                 ! open output file
        writes  'Hello, world\cr\lf'    ! print "hello, world"
        close                           ! close output file
        prtcal  _prt_term               ! end program

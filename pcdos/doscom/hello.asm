!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
!                              HELLO IN DOS                                   !
!                                                                             !
! Writes the string "Hello, world" on the console. This is a DOS .COM file,   !
! to be located at $100, and loaded from a simple binary image.               !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        m86                     ! set machine type to 8086

cr:     equ     $0d             ! carriage return
lf:     equ     $0a             ! line feed

start:
        mov     dx,hellostr     ! index string
        mov     ah,9            ! set dos function "output character string"
        int     $21             ! execute
        ret                     ! and exit to os

hellostr:
        defb    'Hello, world', cr, lf, '$'


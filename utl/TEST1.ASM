!
! Print single character
!
prtchr: push    b               ! save all registers for cp/m
        push    d
        push    h
        mov     e,a             ! place character
        mvi     c,6             ! set "direct console" command
        call    5               ! call cp/m
        pop     h               ! restore registers
        pop     d
        pop     b
        ret
!
message:
        defb    'Hello, world', 13, 10
        defb    'The rain in spain', 13, 10
        defb    'falls mainly on the plain', 13, 10
        defb    'but takes a break on tuesdays', 13, 10
        defb    'or major holidays', 13, 10
        defb    0

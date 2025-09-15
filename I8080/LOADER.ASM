!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
!                          CP/M EMULATOR LOADER                               !
!                      Copyright (C) 1994 S. A. Moore                         !
!                          All right reserved                                 !
!                                                                             !
! Simply moves the block of code following this loader for a length of $1300  !
! to the CP/M 64k maximum configuration BDOS address of $ec00, and executes   !
! it's first address there.                                                   !
! This program should be concatenated with the BDOS emulator program, already !
! located at the proper address, with this program appearing first.           !
! This loader is located at 0.                                                !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

bdos:   equ     $ec00                   ! location of BDOS for 64k CP/M

cpmload:
        lxi     h,_pend                 ! index the beginning of BDOS
        lxi     b,$10000-$ec00          ! set maximum length of BDOS
        lxi     d,$ec00                 ! set destination
move:
        mov     a,m                     ! get a byte
        stax    d                       ! place at destination
        inx     h                       ! next
        inx     d
        dcx     b
        mov     a,c                     ! check done
        ora     b
        jnz     move                    ! loop
        jmp     bdos                    ! go BDOS emulator

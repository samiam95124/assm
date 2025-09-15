!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
!                       8051 PROCESSOR ONCHIP DEFINITIONS                     !
!                                                                             !
!                        Copyright (C) 1994 S. A. Moore                       !
!                                                                             !
! Contains the definitions for onboard pheriperals, bytes and bits in the     !
! 8051. Note that the 8051 can address some operands both by bit and by byte. !
! The mnemonics that are used are as close as possible to the ones used in    !
! the Intel manual.                                                           !
!                                                                             !
! Bit addressing:                                                             !
!                                                                             !
! Bits can be defined in two ways. You can equate the bit to be a 0-7 value,  !
! so that you must specify:                                                   !
!                                                                             !
!       setb    ip.pt2                                                        !
!                                                                             !
! Or, you can just set the equate to the bit address, so you specify:         !
!                                                                             !
!       setb    pt2                                                           !
!                                                                             !
! The first form makes it clear which register you are accessing when you     !
! access a bit within that register. The second is more concise (does         !
! anyone need to be told where the "cy" bit lives ?).                         !
! In this equate file we use the "concise" format. However, any or all of the !
! bits can be converted to the "long" form by simply removing, say, the       !
! "psw." from the psw bits, for example.                                      !
! Some of the bits in registers are NOT accessable by bit instructions. We    !
! equate the bits in such registers in the 0-7 form. These can be used to     !
! form your own bit tests/sets/resets such as:                                !
!                                                                             !
!       mov     a,tmod                  ; get timer/counter control           !
!       orl     a,#1 shl t1gt           ; set gate bit                        !
!       mov     tmod,a                  ; update                              !
!                                                                             !
! Register addressing:                                                        !
!                                                                             !
! In 8051 assembly language, the registers r0-r7 are NOT equates, but are     !
! directly parsed by the assembler (this is because there is otherwise no     !
! difference between direct address mode and register mode).                  !
! HOWEVER, registers can also be addressed directly. BUT, since the active    !
! register set is selected from a bank of 4 register sets, you must also      !
! specify the bank when selecting register direct addresses.                  !
! Direct addressing for registers is used to get access to registers not in   !
! the current bank, to use direct address modes with registers, etc.          !
! In this file, we use two conventions. The first accesses the registers in   !
! bank 0 only. This is the most common case, since that is the default bank   !
! on power up, and may be the only register bank most applications use.       !
!                                                                             !
!       rr0-rr7                                                               !
!                                                                             !
! Directly accesses r0-r7 in bank 0.                                          !
!                                                                             !
!       r0b0-r7b3                                                             !
!                                                                             !
! Is the form used to access any register, in any bank.                       !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Ram registers

rr0:    equ     $00                     ! Register 0 direct
rr1:    equ     $00                     ! Register 1 direct
rr2:    equ     $00                     ! Register 2 direct
rr3:    equ     $00                     ! Register 3 direct
rr4:    equ     $00                     ! Register 4 direct
rr5:    equ     $00                     ! Register 5 direct
rr6:    equ     $00                     ! Register 6 direct
rr7:    equ     $00                     ! Register 7 direct

r0b0:   equ     $00                     ! Register 0 bank 0 direct
r1b0:   equ     $00                     ! Register 1 bank 0 direct
r2b0:   equ     $00                     ! Register 2 bank 0 direct
r3b0:   equ     $00                     ! Register 3 bank 0 direct
r4b0:   equ     $00                     ! Register 4 bank 0 direct
r5b0:   equ     $00                     ! Register 5 bank 0 direct
r6b0:   equ     $00                     ! Register 6 bank 0 direct
r7b0:   equ     $00                     ! Register 7 bank 0 direct

r0b1:   equ     $00                     ! Register 0 bank 1 direct
r1b1:   equ     $00                     ! Register 1 bank 1 direct
r2b1:   equ     $00                     ! Register 2 bank 1 direct
r3b1:   equ     $00                     ! Register 3 bank 1 direct
r4b1:   equ     $00                     ! Register 4 bank 1 direct
r5b1:   equ     $00                     ! Register 5 bank 1 direct
r6b1:   equ     $00                     ! Register 6 bank 1 direct
r7b1:   equ     $00                     ! Register 7 bank 1 direct

r0b2:   equ     $00                     ! Register 0 bank 2 direct
r1b2:   equ     $00                     ! Register 1 bank 2 direct
r2b2:   equ     $00                     ! Register 2 bank 2 direct
r3b2:   equ     $00                     ! Register 3 bank 2 direct
r4b2:   equ     $00                     ! Register 4 bank 2 direct
r5b2:   equ     $00                     ! Register 5 bank 2 direct
r6b2:   equ     $00                     ! Register 6 bank 2 direct
r7b2:   equ     $00                     ! Register 7 bank 2 direct

r0b3:   equ     $00                     ! Register 0 bank 2 direct
r1b3:   equ     $00                     ! Register 1 bank 2 direct
r2b3:   equ     $00                     ! Register 2 bank 2 direct
r3b3:   equ     $00                     ! Register 3 bank 2 direct
r4b3:   equ     $00                     ! Register 4 bank 2 direct
r5b3:   equ     $00                     ! Register 5 bank 2 direct
r6b3:   equ     $00                     ! Register 6 bank 2 direct
r7b3:   equ     $00                     ! Register 7 bank 2 direct

! sfrs

acc:    equ     $e0                     ! Accumulator
b:      equ     $f0                     ! B register
psw:    equ     $d0                     ! Program Status Word
 cy:    equ     psw.7                   !  Carry Flag
 ac:    equ     psw.6                   !  Auxiliary Carry Flag
 f0:    equ     psw.5                   !  General Purpose Flag
 rs1:   equ     psw.4                   !  Register Bank selector bit 1
 rs0:   equ     psw.3                   !  Register Bank selector bit 0
 ov:    equ     psw.2                   !  Overflow Flag
 p:     equ     psw.0                   !  Parity Flag
sp:     equ     $81                     ! Stack Pointer
dptr:   equ     $82                     ! Data Pointer Word
dpl:    equ     $82                     ! Data Pointer Low Byte
dph:    equ     $83                     ! Data Pointer High Byte
p0:     equ     $80                     ! Port 0
p1:     equ     $90                     ! Port 1
p2:     equ     $a0                     ! Port 2
p3:     equ     $b0                     ! Port 3
ip:     equ     $b8                     ! Interrupt Priority Control
 pt2:   equ     ip.5                    !  Timer 2 priority level
 ps:    equ     ip.4                    !  Serial Port priority level
 pt1:   equ     ip.3                    !  Timer 1 priority level
 px1:   equ     ip.2                    !  External Int. 1 Priority level
 pt0:   equ     ip.1                    !  Timer 0 priority level
 px0:   equ     ip.0                    !  External Int. 0 priority level
ie:     equ     $a8                     ! Interrupt Enable Control
 ea:    equ     ie.7                    !  Disable all interrupts
 et2:   equ     ie.5                    !  E/D Timer 2 overflow or capture int.
 es:    equ     ie.4                    !  E/D serial port int
 et1:   equ     ie.3                    !  E/D Timer 1 overflow int.
 ex1:   equ     ie.2                    !  E/D Externam Int. 1
 et0:   equ     ie.1                    !  E/D Timer 0 overflow int.
 ex0:   equ     ie.0                    !  E/D External Int. 0
tmod:   equ     $89                     ! Timer/Counter Mode Control
 t1gt:  equ     7                       !  Timer 1 Gate
 t1ct:  equ     6                       !  Timer 1 Counter/Timer select
 t1m1:  equ     5                       !  Timer 1 Mode select 1
 t1m0:  equ     4                       !  Timer 1 Mode select 0
 t0gt:  equ     7                       !  Timer 0 Gate
 t0ct:  equ     6                       !  Timer 0 Counter/Timer select
 t0m1:  equ     5                       !  Timer 0 Mode select 1
 t0m0:  equ     4                       !  Timer 0 Mode select 0
tcon:   equ     $88                     ! Timer/Counter Control
 tf1:   equ     tcon.7                  !  Timer 1 overflow flag
 tr1:   equ     tcon.6                  !  Timer 1 run control bit
 tf0:   equ     tcon.5                  !  Timer 0 overlow flag
 tr0:   equ     tcon.4                  !  Timer 0 run control bit
 ie1:   equ     tcon.3                  !  External Int. 1 edge flag
 it1:   equ     tcon.2                  !  External Int. 1 type control bit
 ie0:   equ     tcon.1                  !  External Int. 0 edge flag
 it0:   equ     tcon.0                  !  External Int. 0 type control bit
t2con:  equ     $c8                     ! Timer/Counter 2 Control
 tf2:   equ     t2con.7                 !  Timer 2 overflow flag
 exf2:  equ     t2con.6                 !  Timer 2 external flag
 rclk:  equ     t2con.5                 !  Receive clock flag
 tlck:  equ     t2con.4                 !  Transmit clock flag
 exen2: equ     t2con.3                 !  Timer 2 external enable
 tr2:   equ     t2con.2                 !  Timer 2 start/stop flag
 ct2:   equ     t2con.1                 !  Timer or Counter select
 cprl2: equ     t2con.0                 !  Capture/Reload flag
th0:    equ     $8c                     ! Timer/Counter 0 High Byte
tl0:    equ     $8a                     ! Timer/Counter 0 Low Byte
th1:    equ     $8d                     ! Timer/Counter 1 High Byte
tl1:    equ     $8b                     ! Timer/Counter 1 Low Byte
th2:    equ     $cd                     ! Timer/Counter 2 High Byte
tl2:    equ     $cc                     ! Timer/Counter 2 Low Byte
rcap2h: equ     $cb                     ! T/C Capture Register High Byte
rcap2l: equ     $ca                     ! T/C Capture Register Low Byte
scon:   equ     $98                     ! Serial Control
 sm0:   equ     scon.7                  !  Serial Port mode 0
 sm1:   equ     scon.6                  !  Serial Port mode 1
 sm2:   equ     scon.5                  !  Enable multiprocessor comm.
 ren:   equ     scon.4                  !  E/D reception
 tb8:   equ     scon.3                  !  9th bit transmit
 rb8:   equ     scon.2                  !  9th bit received
 ti:    equ     scon.1                  !  Transmit int. flag
 ri:    equ     scon.0                  !  Recive int. flag
sbuf:   equ     $99                     ! Serial Data Buffer
pcon:   equ     $87                     ! Power Control
 smod:  equ     7                       !  Double baud rate bit
 gf1:   equ     3                       !  General purpose flag bit
 gf2:   equ     2                       !  General purpose flag bit
 pd:    equ     1                       !  Power Down bit
 idl:   equ     0                       !  Idle Mode bit

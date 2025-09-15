!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                             !
!                 Z280 PROCESSOR DEFINITIONS                  !
!                                                             !
!                     3/89 S. A. MOORE                        !
!                                                             !
! Contains the internal definitions used within the Z280      !
! processor itself. This is required because the Z280         !
! is in many ways a collection of pheriperals itself, and has !
! A number of standard I/O addresses.                         !
!                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!
! Internal processor registers, accessed by ldctl instruction
!
msr: equ $00 ! master status register
isr: equ $16 ! interrupt status register
ttp: equ $06 ! interrupt/trap table pointer
iop: equ $08 ! I/O page register
btr: equ $ff ! bus timing and initalization register
bcr: equ $02 ! bus timing and control register
slr: equ $04 ! stack limit register
tcr: equ $10 ! trap control register
ccr: equ $12 ! cache control register
lar: equ $14 ! local address register

!
! Interrupt/trap table addresses (relative to base)
!
nmivec: equ $04 ! NMI vector
inavec: equ $08 ! interrupt line a vector
inbvec: equ $0c ! interrupt line b vector
incvec: equ $10 ! interrupt line c vector
ct0vec: equ $14 ! counter/timer 0 vector
ct1vec: equ $18 ! counter/timer 1 vector
ct2vec: equ $20 ! counter/timer 2 vector
dm0vec: equ $24 ! DMA channel 0 vector
dm1vec: equ $28 ! DMA channel 1 vector
dm2vec: equ $2c ! DMA channel 2 vector
dm3vec: equ $30 ! DMA channel 3 vector
sdrvec: equ $34 ! UART receive vector
sdtvec: equ $38 ! UART transmit vector
sstvec: equ $3c ! single-step trap vector
bohvec: equ $40 ! breakpoint-on-halt trap vector
divvec: equ $44 ! division exception vector
sovvec: equ $48 ! stack overflow warning trap vector

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
!                        STANDARD SPARC V9 DEFINITIONS                        !
!                                                                             !
!                        Copyright (C) 2005 S. A. Moore                       !
!                                                                             !
! Contains the required standard SPARC V9 definitions, found in the SPARC     !
! Architecture Manual, V9, page 285.                                          !
!                                                                             !
! NOTES:                                                                      !
!                                                                             !
! 1. Unlike the recommended SPARC format, these values appear without  a      !
! leading '#' (sharp) character.                                              !
!                                                                             !
! 2. AS is not case sensitive. Although the case of values here is as was     !
! presented in the SPARC Architecture Manual, no case sensitivity is implied  !
! or enforced.                                                                ! 
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Prefetch instruction values

n_reads:    equ  0
one_read:   equ  1
n_writes:   equ  2
one_write:  equ  3
page:       equ  4

! Membar values

sync:       equ  $40
memissue:   equ  $20
lookaside:  equ  $10
storestore: equ  $08
loadstore:  equ  $04
storeload:  equ  $02
loadload:   equ  $01

! ASIs (Address Space Identifiers)

! Short names

ASI_AIUP:   equ  $10 ! ASI_AS_IF_USER_PRIMARY
ASI_AIUS:   equ  $11 ! ASI_AS_IF_USER_SECONDARY
ASI_AIUP_L: equ  $18 ! ASI_AS_IF_USER_PRIMARY_LITTLE
ASI_AIUS_L: equ  $19 ! ASI_AS_IF_USER_SECONDARY_LITTLE
ASI_P:      equ  $80 ! ASI_PRIMARY
ASI_S:      equ  $81 ! ASI_SECONDARY
ASI_PNF:    equ  $82 ! ASI_PRIMARY_NOFAULT
ASI_SNF:    equ  $83 ! ASI_SECONDARY_NOFAULT
ASI_P_L:    equ  $88 ! ASI_PRIMARY_LITTLE
ASI_S_L:    equ  $89 ! ASI_SECONDARY_LITTLE
ASI_PNF_L:  equ  $8a ! ASI_PRIMARY_NOFAULT_LITTLE
ASI_SNF_L:  equ  $8b ! ASI_SECONDARY_NOFAULT_LITTLE

! Long names

ASI_AS_IF_USER_PRIMARY:          equ $10
ASI_AS_IF_USER_SECONDARY:        equ $11
ASI_AS_IF_USER_PRIMARY_LITTLE:   equ $18
ASI_AS_IF_USER_SECONDARY_LITTLE: equ $19
ASI_PRIMARY:                     equ $80
ASI_SECONDARY:                   equ $81
ASI_PRIMARY_NOFAULT:             equ $82
ASI_SECONDARY_NOFAULT:           equ $83
ASI_PRIMARY_LITTLE:              equ $88
ASI_SECONDARY_LITTLE:            equ $89
ASI_PRIMARY_NOFAULT_LITTLE:      equ $8a
ASI_SECONDARY_NOFAULT_LITTLE:    equ $8b

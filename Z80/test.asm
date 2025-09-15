!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                              !
!     DB Z80 DEBUGGER                                          !
!                                                              !
!     3/21/80 S. A. Moore                                      !
!                                                              !
!     This grandaddy program originates from my work at        !
!     spacelabs, inc., and has proven to be my right-hand      !
!     man for debugging scads of other programs.               !
!     It is a testimony to what good structure and             !
!     documenting habits can accomplish. It's successor        !
!     will probabally be written in pascal, and use this       !
!     program as a construction guide.                         !
!     This is one program that seems to have given back        !
!     more than the effort i ever put in to it.                !
!                                                              !
!     Version 1.0 3/80 on zilog's rio os                       !
!                                                              !
!     Version 1.0 4/80 on spacelabs 1400 terminal              !
!                                                              !
!     Version 1.3 7/80 on micropolis mdos                      !
!                                                              !
!     Version 1.3 6/81 on rsys os                              !
!                                                              !
!     Version 1.3 6/81 on cardkey d2000 terminal               !
!                                                              !
!     Version 2.1 2/84 on our system prom                      !
!                                                              !
!     Version 2.2 4/85 on CP/M                                 !
!                                                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Equates

false:  equ     0               ! value of false
true:   equ     1               ! not false ! value of true
cr:     equ     $0d             ! carriage return
lf:     equ     $0a             ! line feed
bksp:   equ     $08             ! backspace (rubout)dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd

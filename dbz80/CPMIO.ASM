!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                              !
!     CP/M I/O interface                                       !
!                                                              !
!     1/1/86 S. A. Moore                                       !
!                                                              !
!     Interfaces DB to CP/M. Utilizes the direct I/O call      !
!     (in order to prevent exit on CTL-C and other system      !
!     interference). Also utilizes the list output call.       !
!     This module will allow interface to CP/M's 1.0 - 2.2     !
!     and ensuing compatable versions.                         !
!                                                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! CP/M equations

_bdos:  equ     5               ! Entry to CP/M

cdcon:  equ     6               ! direct console I/O
clstout: equ    5               ! list output

! Other equations

restart: equ    5               ! restart number to use
delete: equ     $7f             ! \del character
!
! External initalize
!
!      Sets up 'rst 6' breakpoint for DB.
!
!      In parameters: none
!      Out parameters: restart code: a
!
exinit: push    hl
        ld      a,$c3           ! get a 'jp' instruction
        ld      (restart*8),a   ! and place a vector at rst 6
        ld      hl,brkvec       ! index breakpoint re-entry
        ld      (restart*8+1),hl
        ld      a,10            ! set tabs
        ld      (tabo),a
        ld      a,16
        ld      (tabt),a
        ld      a,restart*8 or $c7  ! get restart code
        pop     hl              ! clean up and return
        ret                     ! with restart code in a

!
! Console input
!
!      Fetches the next console character. If no
!      character is ready, returns 0.
!      Returns the character in a.
!
!      In parameters: none
!      Out parameters: character - a
!
conin:  push    bc
        push    de
        push    hl
        ld      e,$ff           ! flag input required
        ld      c,cdcon         ! set "direct console" command
        call    _bdos           ! execute system call
        or      a               ! check no character
        jr      z,conin01       ! yes, exit
        and     $7f             ! mask parity
        cp      delete          ! check delete key
        jr      nz,conin01      ! no
        ld      a,bksp          ! translate to backspace
conin01: pop    hl              ! clean up and return
        pop     de
        pop     bc
        ret

!
! Console output routine
!
!      Outputs the character in a to the console.
!
!      In parameters: character - a
!      Out parameters: none
!      Modifies: af
!
conout: push    bc
        push    de
        push    hl
        push    af              ! save character
        ld      e,a             ! set character
        ld      c,cdcon         ! set "direct console" command
        call    _bdos           ! execute system call
        pop     af              ! restore character
        cp      bksp            ! check was backspace
        jr      nz,conout01     ! no, exit
        ld      e,' '           ! yes, erase content at position
        ld      c,cdcon         ! set "direct console" command
        call    _bdos           ! execute system call
        ld      e,bksp          ! back up again
        ld      c,cdcon         ! set "direct console" command
        call    _bdos           ! execute system call
conout01: pop   hl              ! clean up and return
        pop     de
        pop     bc
        ret

!
! List output
!
!      Outputs the character in a to the list.
!
!      In parameters: character - a
!      Out parameters: none
!      Modifies: af
!
lstout: push    bc
        push    de
        push    hl
        ld      e,a             ! set character
        ld      c,clstout       ! set "list output" command
        call    _bdos           ! execute system call
        pop     hl              ! clean up and return
        pop     de
        pop     bc
        ret

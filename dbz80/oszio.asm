!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                              !
! Os/z I/O interface                                           !
!                                                              !
! 11/88 S. A. Moore                                            !
!                                                              !
! This is an intermediate step for the Os/z. We relocate to    !
! the top of memory, then load the target at 0. Two special    !
! program features must exist: the target must accept the      !
! SP as given, and a 'jp' instruction must exist at location   !
! 0. The former because the normal 0 stack would overwrite us, !
! the latter to enable us to replace the startup jump with     !
! a restart handler.                                           !
!                                                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! System calls
!
_sys_term: equ  0               ! terminate program
_sys_open: equ  1               ! open file
_sys_close: equ 2               ! close file
_sys_read: equ  3               ! read file
_sys_write: equ 4               ! write file
_sys_location: equ 5            ! find file location
_sys_position: equ 6            ! set position
_sys_length: equ 7              ! find length
_sys_size: equ  8               ! set size
_sys_copy: equ  9               ! copy file
_sys_move: equ  10              ! move file
_sys_time: equ  11              ! get current time
_sys_schedule: equ 12           ! schedule wakeup
!
! System errors
!
einvsc: equ     1               ! invalid system call
eivfid: equ     2               ! invalid file descriptor
edflt:  equ     3               ! device not operational
edrdy:  equ     4               ! device not ready
edbsy:  equ     5               ! device busy
edict:  equ     6               ! device inactive
erpt:   equ     7               ! read protect
ewpt:   equ     8               ! write protect
eeof:   equ     9               ! past eof
epos:   equ     10              ! not a positionable device
eivpos: equ     11              ! invalid position
eivfsp: equ     12              ! invalid file specification
efovf:  equ     13              ! file limit exceeded
efnfnd: equ     14              ! file not found
effnd:  equ     15              ! file found
eivtim: equ     16              ! invalid time format
!
! Other equations
!
restart: equ    0               ! restart number to use
delete: equ     $7f             ! \del character
!
! External initalize
!
!      In parameters: none
!      Out parameters: restart code: a
!
exinit: push    bc
        push    de
        push    hl
        ld      hl,connam       ! open _console file
        ld      de,8            ! set length
        ld      b,2             ! set existing
        defb    $ed,$71         ! open
        defw    _sys_open
        jr      z,exinit01      ! skip no error
        defb    $ed,$71         ! terminate with error
        defw    _sys_term
exinit01: ld    a,b             ! place FID
        ld      (confil),a
        ld      hl,lstnam       ! open _list file
        ld      de,5            ! set length
        ld      b,2             ! set existing
        defb    $ed,$71         ! open
        defw    _sys_open
        jr      z,exinit02      ! skip no error
        defb    $ed,$71         ! terminate with error
        defw    _sys_term
exinit02:
! ld a,$c3 ! get a 'jp' instruction
! ld (restart*8),a ! and place a vector at rst 6
! ld hl,brkvec ! index breakpoint re-entry
! ld (restart*8+1),hl
        ld      a,10            ! set tabs
        ld      (tabo),a
        ld      a,16
        ld      (tabt),a
        ld      a,restart*8 or $c7  ! get restart code
        pop     hl              ! clean up and return
        pop     de
        pop     bc
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
        ld      a,(confil)      ! get FID
        ld      de,0            ! set probe
        defb    $ed,$71         ! read
        defw     _sys_read
        cp      edict           ! check inactive
        jr      z,conin03       ! yes, go
        or      a               ! check other error
        jr      z,conin01       ! no, skip
        defb    $ed,$71         ! terminate with error
        defw     _sys_term
conin01: ld     hl,chrbuf       ! index buffer
        ld      de,1            ! set length
        defb    $ed,$71         ! read
        defw    _sys_read
        jr      z,conin02       ! skip no error
        defb    $ed,$71         ! terminate with error
        defw    _sys_read
conin02: ld     a,(chrbuf)      ! get character
        and     $7f             ! mask parity
        cp      delete          ! check delete key
        jr      nz,conin04      ! no, exit
        ld      a,bksp          ! translate to backspace
        jr      conin04         ! exit
conin03: xor    a               ! set no character ready
conin04: or     a               ! set flags
        pop     hl              ! clean up and return
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
        ld      hl,chrbuf       ! index buffer
        ld      (hl),a          ! place character
        ld      de,1            ! set length
        defb    $ed,$71         ! write
        defw    _sys_write
        jr      z,conout01      ! skip no error
        defb    $ed,$71         ! terminate with error
        defw    _sys_write
conout01: ld    a,(hl)          ! get character
        cp      bksp            ! check was backspace
        jr      nz,conout03     ! no, exit
        ld      (hl),' '        ! yes, erase content at position
        defb    $ed,$71         ! write
        defw    _sys_write
        jr      z,conout02      ! skip no error
        defb    $ed,$71         ! terminate with error
        defw    _sys_write
conout02: ld    (hl),bksp       ! back up again
        defb    $ed,$71         ! write
        defw    _sys_write
        jr      z,conout03      ! skip no error
        defb    $ed,$71         ! terminate with error
        defw    _sys_write
conout03: pop   hl              ! clean up and return
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
        ld      hl,chrbuf       ! index buffer
        ld      (hl),a          ! place character
        ld      de,1            ! set length
        defb    $ed,$71         ! write
        defw    _sys_write
        jr      z,lstout01      ! skip no error
        defb    $ed,$71         ! terminate with error
        defw    _sys_write
lstout01: pop   hl              ! clean up and return
        pop     de
        pop     bc
        ret

!
! Data
!
connam: defb    '_console'      ! name of _console
confil: defvs   1               ! console FID
lstnam: defb    '_list'         ! name of _list
lstfil: defvs   1               ! list FID
chrbuf: defvs   1               ! character buffer

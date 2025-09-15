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
conin:  ld      a,$10
        out     ($65),a
        in      a,($65)
        and     $01
        jr      z,conin01
        in      a,($64)
        and     $7f             ! mask parity
        cp      delete          ! check delete key
        jr      nz,conin01      ! no
        ld      a,bksp          ! translate to backspace
conin01: ret

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
        ld      c,a
conout01: ld    a,$10
        out     ($65),a
        in      a,($65)
        and     $04
        jr      z,conout01
        ld      a,c
        out     ($64),a
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
lstout: ret


! Return from interrupt

ireti:  call    prtstr          ! print 'reti'
        defb    'ret', 'i' or $80
        ret                     ! and exit

! Return from non-maskable interrupt
        
iretn:  call    prtstr          ! print 'retn'
        defb    'ret', 'n' or $80
        ret                     ! and exit
        
! Set interrupt mode 0
        
iimz:   call    prtstr          ! print 'im'
        defb    'i', 'm' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print '0'
        defb    '0' or 80
        ret                     ! and exit
        
! Set interrupt mode 1
        
iimo:   call    prtstr          ! print 'im'
        defb    'i', 'm' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print '1'
        defb    '1' or $80
        ret                     ! and exit
        
! Set interrupt mode 2
        
iimt:   call    prtstr          ! print 'im'
        defb    'i', 'm' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print '2'
        defb    '2' or $80
        ret                     ! and exit

! Load interrupt register from accumulator
        
ildia:  call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'i,a'
        defb    'i,', 'a' or $80
        ret                     ! and exit
        
! Load accumulator from interrupt register
        
ildai:  call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'a,i'
        defb    'a,', 'i' or $80
        ret                     ! and exit
        
! Load refresh register from accumulator
        
ildra:  call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'r,a'
        defb    'r,', 'a' or $80
        ret                     ! and exit
        
! Load accumulator from refresh register
        
ildar:  call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'a,r'
        defb    'a,', 'r' or $80
        ret                     ! and exit
        
! dissassembly error (no operation, just table filler)
        
diserr: ret                     ! do nothing

!
! Table jump on byte
!
!     Vectors into the word table hl
!     by the byte a.
!
!     In parameters: word table address - hl, byte - a
!     Out parameters: word from table - de
!
getwrd: push    hl
        ld      e,a             ! set up the byte
        ld      d,0
        ex      de,hl
        add     hl,hl           ! * 2 for word table
        add     hl,de           ! offset into table
        ld      e,(hl)          ! get the word
        inc     hl
        ld      d,(hl)
        pop     hl              ! clean up and return
        ret

!
! Get 8080 address
!
!     Gets the handler address for the 8080
!     code a.
!
!     In parameters: code byte - a
!     Out parameters: handler address - de
!
getadd: push    af
        push    hl
        cp      $76             ! remove 'halt' code
        ld      de,ihalt        ! index 'halt' handler
        jr      z,getadd03      ! yes
        bit     7,a             ! chk move or accins minimize
        jr      nz,getadd01     ! accins
        bit     6,a             ! check for move
        ld      de,ilds         ! index move handler
        jr      nz,getadd03     ! yes
        jr      getadd02        ! no, go table lookup
getadd01: bit   6,a             ! check for accins ins
        ld      de,iacco        ! index accins handler
        jr      z,getadd03      ! yes
getadd02: res   7,a             ! we've minimized this
        ld      hl,ctone        ! index code table
        call    getwrd          ! get the table word
getadd03: pop   hl              ! clean up and return
        pop     af
        ret

!
! Print double register
!
!     Prints a double register spec.
!
!     In parameters: code - a, state - b
!     Out parameters: none
!     Modifies: none
!
pdr:    push    af
        rlca
        res     7,a
        jr      nc,pdr01
        set     7,a
pdr01:  rlca                    ! and move into place
        rlca
        rlca
        and     $07             ! mask relevant bits
        cp      6               ! chk for 'hl'
        jr      z,pdr02         ! yes
        cp      2               ! no, try again
        jr      z,pdr02         ! yes
        add     a,bdr           ! else just add offset
        call    prtmsg          ! and print
        jr      pdr03
pdr02:  call    pst             ! print the state register
pdr03:  pop     af              ! clean up and return
        ret

!
! Print single high register
!
!     Prints a high register.
!
!     In parameters: code - a, state - b, code address - hl
!     Out parameters: none
!     Modifies: none
!
psrh:   push    af
        rrca                    ! move spec down
        rrca
        rrca
        call    psrl            ! and use the low register routine
        pop     af              ! clean up and return
        ret

!
! Print single register
!
!     Prints a single register.
!
!     In parameters: code - a, state - b, code address - hl
!     Out parameters: none
!     Modifies: none
!
psrl:   push    af
        and     $07             ! mask relevant bits
        cp      $06             ! memory reference
        jr      z,psrl02        ! yes, go to state handler
psrl01: add     a,bsreg         ! else offset into message table
        call    prtmsg          ! and print
        jr      psrl03
psrl02: ld      a,b             ! chk the state
        or      a
        ld      a,$06           ! load for possible re-do
        jr      z,psrl01        ! re-do it
        call    prtstr          ! print '('
        defb    '(' or $80
        call    pst             ! print the state register
        ld      a,(hl)          ! get the displacement
        inc     hl
        call    pshsym          ! print as signed byte
        call    prtstr          ! print ')'
        defb    ')' or $80
psrl03: pop     af              ! clean up and return
        ret

!
! Print the state register
!
!     Prints the current state register
!
!     In parameters: state - b
!     Out parameters: none
!     Modifies: none
!
pst:    push    af
        ld      a,b             ! get the current state
        add     a,bexreg        ! offset into message table
        call    prtmsg          ! and print
        pop     af
        ret

!
! Print accumulator operation
!
!     Prints accumulator operation code
!
!     In parameters: code - a
!     Out parameters: none
!     Modifies: none
!
popr:   push    af
        rrca                    ! move down
        rrca
        rrca
        and     $07             ! mask code
        add     a,bopr          ! offset into message table
        call    prtmsg
        pop     af              ! clean up and return
        ret

!
! Print condition code
!
!     Prints condition code.
!
!     In parameters: code - a
!     Out parameters: none
!     Modifies: none
!
pcond:  push    af
        rrca                    ! move down
        rrca
        rrca
        and     $07             ! mask code
        add     a,bccode        ! offset into message table
        call    prtmsg          ! and print
        pop     af              ! clean up and return
        ret

!
! Parse byte immediate
!
!     Gets an imm code byte and prints.
!
!     In parameters: code address - hl
!     Out parameters: new code address - hl
!     Modifies: none
!
cdhex:  push    af
        ld      a,(hl)          ! get a byte from the address
        inc     hl              ! next byte
        call    prtstr          ! print '$'
        defb    '$' or $80
        call    prtbsm          ! print hex byte
        pop     af              ! clean up and return
        ret

!
! Parse word immediate
!
!      Parses imm word from code.
!
!      In parameters: code address - hl
!      Out parameters: new code address - hl
!      Modifies: none
!
cdword: push    af
        ld      e,(hl)          ! get low byte of word
        inc     hl              ! next byte
        ld      d,(hl)          ! load the word into hl
        inc     hl              ! next byte
        ex      de,hl
        call    prtstr          ! print '$'
        defb    '$' or $80
        call    prtsym          ! and print it
        ex      de,hl
        pop     af              ! clean up and return
        ret

!
! Tab to labels field
!
!     Outputs spaces to reach the labels
!     field. If the line is already past,
!     will do nothing.
!
!     In parameters: none
!     Out parameters: none
!     Modifies: none
!
tab0:   push    af
        ld      a,20            ! get 0th tab value
        call    tab             ! do the tab
        pop     af              ! clean up and return
        ret

!
! Tab to opcodes collumn
!
!     Tabs to the opcodes collumn by outputting spaces.
!
!     In parameters: none
!     Out parameters: none
!     Modifies: none
!
tab1:   push    af
        ld      a,(tabo)        ! get the opcode tab
        add     a,20            ! offset
        call    tab             ! execute
        pop     af              ! clean up and return
        ret

!
! Tab to operands field
!
!     Tabs to the operands field by outputting spaces.
!
!     In parameters: none
!     Out parameters: none
!     Modifies: none
!
tab2:   push    af
        ld      a,(tabt)        ! get the operands tab
        add     a,20            ! offset
        call    tab             ! execute
        pop     af              ! clean up and return
        ret

!
! Tab to collumn
!
!     Expects tab value in a.
!     Tabs to that point by outputing spaces.
!
!     In parameters: tab - a
!     Out parameters: none
!     Modifies: af
!
tab:    push    bc
        ld      c,a             ! save tab
        ld      a,(lincnt)      ! get the line counter
        ld      b,a             ! save
        ld      a,c             ! restore tab
        sub     b               ! find left to do
        jr      c,tab02         ! done, exit
        jr      z,tab02         ! same
tab01:  call    prtstr          ! else output space
        defb    ' ' or $80
        dec     a               ! count
        jr      nz,tab01        ! and loop
tab02:  pop     bc              ! clean up and return
        ret

!
! First (8080) page code table
!
ctone:  defw    inop            ! 00
        defw    ilxi            ! 01
        defw    istax           ! 02
        defw    iincd           ! 03
        defw    iincs           ! 04
        defw    idecs           ! 05
        defw    imvi            ! 06
        defw    irlca           ! 07
        defw    iexa            ! 08
        defw    idad            ! 09
        defw    ildax           ! 0a
        defw    idecd           ! 0b
        defw    iincs           ! 0c
        defw    idecs           ! 0d
        defw    imvi            ! 0e
        defw    irrca           ! 0f
        defw    idjnz           ! 10
        defw    ilxi            ! 11
        defw    istax           ! 12
        defw    iincd           ! 13
        defw    iincs           ! 14
        defw    idecs           ! 15
        defw    imvi            ! 16
        defw    irla            ! 17
        defw    ijr             ! 18
        defw    idad            ! 19
        defw    ildax           ! 1a
        defw    idecd           ! 1b
        defw    iincs           ! 1c
        defw    idecs           ! 1d
        defw    imvi            ! 1e
        defw    irra            ! 1f
        defw    ijrnz           ! 20
        defw    ilxi            ! 21
        defw    istdr           ! 22
        defw    iincd           ! 23
        defw    iincs           ! 24
        defw    idecs           ! 25
        defw    imvi            ! 26
        defw    idaa            ! 27
        defw    ijrz            ! 28
        defw    idad            ! 29
        defw    ildrd           ! 2a
        defw    idecd           ! 2b
        defw    iincs           ! 2c
        defw    idecs           ! 2d
        defw    imvi            ! 2e
        defw    icpl            ! 2f
        defw    ijrnc           ! 30
        defw    ilxi            ! 31
        defw    ista            ! 32
        defw    iincd           ! 33
        defw    iincs           ! 34
        defw    idecs           ! 35
        defw    imvi            ! 36
        defw    iscf            ! 37
        defw    ijrc            ! 38
        defw    idad            ! 39
        defw    ilda            ! 3a
        defw    idecd           ! 3b
        defw    iincs           ! 3c
        defw    idecs           ! 3d
        defw    imvi            ! 3e
        defw    iccf            ! 3f
        
! Note that codes 40-c0 have been state minimized
! and do not need to be here
        
        defw    iret            ! c0
        defw    ipop            ! c1
        defw    ijp             ! c2
        defw    ijp             ! c3
        defw    icall           ! c4
        defw    ipush           ! c5
        defw    iacci           ! c6
        defw    irst            ! c7
        defw    iret            ! c8
        defw    iret            ! c9
        defw    ijp             ! ca
        defw    icbx            ! cb
        defw    icall           ! cc
        defw    icall           ! cd
        defw    iacci           ! ce
        defw    irst            ! cf
        defw    iret            ! d0
        defw    ipop            ! d1
        defw    ijp             ! d2
        defw    iouta           ! d3
        defw    icall           ! d4
        defw    ipush           ! d5
        defw    iacci           ! d6
        defw    irst            ! d7
        defw    iret            ! d8
        defw    iexx            ! d9
        defw    ijp             ! da
        defw    iina            ! db
        defw    icall           ! dc
        defw    iddx            ! dd
        defw    iacci           ! de
        defw    irst            ! df
        defw    iret            ! e0
        defw    ipop            ! e1
        defw    ijp             ! e2
        defw    ixthl           ! e3
        defw    icall           ! e4
        defw    ipush           ! e5
        defw    iacci           ! e6
        defw    irst            ! e7
        defw    iret            ! e8
        defw    ijpdr           ! e9
        defw    ijp             ! ea
        defw    ixchg           ! eb
        defw    icall           ! ec
        defw    iedx            ! ed
        defw    iacci           ! ee
        defw    irst            ! ef
        defw    iret            ! f0
        defw    ipop            ! f1
        defw    ijp             ! f2
        defw    idi             ! f3
        defw    icall           ! f4
        defw    ipush           ! f5
        defw    iacci           ! f6
        defw    irst            ! f7
        defw    iret            ! f8
        defw    isphl           ! f9
        defw    ijp             ! fa
        defw    iei             ! fb
        defw    icall           ! fc
        defw    ifdx            ! fd
        defw    iacci           ! fe
        defw    irst            ! ff

!
! second (z80) page code mapping table.
! note that the codes 00-3f and bc-ff
! are not valid z80 instructions and do not need
! to be here.
!
cttwo:  defw    ixins           ! 40
        defw    ixouts          ! 41
        defw    isbcdr          ! 42
        defw    ilrdi           ! 43
        defw    ineg            ! 44
        defw    iretn           ! 45
        defw    iimz            ! 46
        defw    ildia           ! 47
        defw    ixins           ! 48
        defw    ixouts          ! 49
        defw    iadcdr          ! 4a
        defw    ilird           ! 4b
        defw    diserr          ! 4c
        defw    ireti           ! 4d
        defw    diserr          ! 4e
        defw    ildra           ! 4f
        defw    ixins           ! 50
        defw    ixouts          ! 51
        defw    isbcdr          ! 52
        defw    ilrdi           ! 53
        defw    diserr          ! 54
        defw    diserr          ! 55
        defw    iimo            ! 56
        defw    ildai           ! 57
        defw    ixins           ! 58
        defw    ixouts          ! 59
        defw    iadcdr          ! 5a
        defw    ilird           ! 5b
        defw    diserr          ! 5c
        defw    diserr          ! 5d
        defw    iimt            ! 5e
        defw    ildar           ! 5f
        defw    ixins           ! 60
        defw    ixouts          ! 61
        defw    isbcdr          ! 62
        defw    diserr          ! 63
        defw    diserr          ! 64
        defw    diserr          ! 65
        defw    diserr          ! 66
        defw    irrd            ! 67
        defw    ixins           ! 68
        defw    ixouts          ! 69
        defw    iadcdr          ! 6a
        defw    diserr          ! 6b
        defw    diserr          ! 6c
        defw    diserr          ! 6d
        defw    diserr          ! 6e
        defw    irld            ! 6f
        defw    diserr          ! 70
        defw    diserr          ! 71
        defw    isbcdr          ! 72
        defw    ilrdi           ! 73
        defw    diserr          ! 74
        defw    diserr          ! 75
        defw    diserr          ! 76
        defw    diserr          ! 77
        defw    ixins           ! 78
        defw    ixouts          ! 79
        defw    iadcdr          ! 7a
        defw    ilird           ! 7b
        
! Note that the invalid codes 7c-9f
! have been eliminated.
        
        defw    ixld            ! a0
        defw    ixcp            ! a1
        defw    ixin            ! a2
        defw    ixout           ! a3
        defw    diserr          ! a4
        defw    diserr          ! a5
        defw    diserr          ! a6
        defw    diserr          ! a7
        defw    ixld            ! a8
        defw    ixcp            ! a9
        defw    ixin            ! aa
        defw    ixout           ! ab
        defw    diserr          ! ac
        defw    diserr          ! ad
        defw    diserr          ! ae
        defw    diserr          ! af
        defw    ixld            ! b0
        defw    ixcp            ! b1
        defw    ixin            ! b2
        defw    ixout           ! b3
        defw    diserr          ! b4
        defw    diserr          ! b5
        defw    diserr          ! b6
        defw    diserr          ! b7
        defw    ixld            ! b8
        defw    ixcp            ! b9
        defw    ixin            ! ba
        defw    ixout           ! bb

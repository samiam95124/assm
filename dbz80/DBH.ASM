!
! Disassembly section.
! Here we provide the routines to list
! symbolic Z80 code.
! The routines in the processor control
! package are used to do this.
!

!
! Dissassemble single instruction
!
!      Expects instruction to dissassemble at hl.
!      Output occurs only if 'listsw' is true,
!      else listing will be suppressed.
!      Outputs '???' for invalid instruction.
!
!      In parameters: dissassembly address - hl
!      Out parameters: new address - hl
!
disass: push    bc
        push    de
        call    pwords          ! print the location
        call    lenins          ! get instruction length
        ld      c,a             ! save error
        ld      b,e             ! get length in b
        push    hl              ! save pointer
diss01: ld      a,(hl)          ! get a byte
        inc     hl              ! next
        call    phexs           ! print
        djnz    diss01          ! loop for all code bytes
        pop     de              ! restore pointer
        ex      de,hl
        ld      a,c             ! recover error
        or      a
        jr      nz,diss05       ! invalid instruction
        push    de              ! save end address
        push    hl              ! save start address
        call    tab0            ! index labels field
        call    prtnsm          ! print possible label
        call    tab1            ! index opcodes field
        ld      b,0             ! say state one (base 8080)
        ld      a,(hl)          ! get instruction byte to process
        inc     hl              ! next
        
! At this point we have a 8080 type base in the
! a register. We may enter with one of three states:
! 8080, ix or iy. The ix and iy states are the same
! as the base 8080, except that ix or iy substitutes
! for hl, and that many 8080 instructions are invalid
! in state ix or iy.
        
        call    getadd          ! get the 8080 handler addr
        push    de              ! save function vector
        ld      de,diss06       ! index return point
        ex      de,hl
        ex      (sp),hl         ! trade return for vector
        push    hl              ! and push vector
        ex      de,hl           ! restore pointer to hl
        ret                     ! and go function
diss05: call    tab1            ! index opcode field
        call    prtstr          ! print '???' error message
        defb    '??', '?' or $80
        inc     hl              ! increment past opcode
        jr      diss07          ! and exit
diss06: pop     hl              ! recover start
        pop      hl             ! recover end
diss07: call    crlf            ! next line
        pop     de              ! clean up and return
        pop     bc
        ret

! Complement carry
        
iccf:   call    prtstr          ! print 'ccf'
        defb    'cc', 'f' or $80
        ret
        
! Set carry
        
iscf:   call    prtstr          ! print 'scf'
        defb    'sc', 'f' or $80
        ret
        
! Increment single
        
iincs:  call    prtstr          ! print 'inc'
        defb    'in', 'c' or $80
        call    tab2            ! index operands field
        call    psrh            ! print a single register by state
        ret
        
! Decrement single
        
idecs:  call    prtstr          ! print 'dec'
        defb    'de', 'c' or $80
        call    tab2            ! index operands field
        call    psrh            ! print a single register by state
        ret
        
! Complement accumulator
        
icpl:   call    prtstr          ! print 'cpl'
        defb    'cp', 'l' or $80
        ret
        
! Decimal adjust accumulator
        
idaa:   call    prtstr          ! print 'daa'
        defb    'da', 'a' or $80
        ret
        
! No operation
        
inop:   call    prtstr          ! print 'nop'
        defb    'no', 'p' or $80
        ret
        
! Move single
        
ilds:   call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        call    psrh            ! print destination by state
        call    prtstr          ! print ','
        defb    ',' or $80
        call    psrl            ! print sorce by state
        ret                     ! all states allowed

        
! Store accumulator
        
istax:  call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        bit     4,a             ! chk what register pair
        jr      nz,ista01       ! register pair de
        call    prtstr          ! print '(bc),a'
        defb    '(bc),', 'a' or $80
        ret
ista01: call    prtstr          ! print '(de),a'
        defb    '(de),', 'a' or $80
        ret
        
! Load accumulator
        
ildax:  call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        bit     4,a             ! chk what register pair
        jr      nz,ilda01       ! register pair de
        call    prtstr          ! print 'a,(bc)'
        defb    'a,(bc', ')' or $80
        ret
ilda01: call    prtstr          ! print 'a,(de)'
        defb    'a,(de', ')' or $80
        ret
        
! Accumulator operations
        
iacco:  call    popr            ! print the operation
        call    tab2            ! index operands field
        push    af              ! save code
        and     $38             ! mask code
        cp      $10             ! check 'sub' code
        jr      z,iacco01       ! yes, skip
        bit     5,a             ! check logicals
        jr      nz,iacco01      ! yes, skip
        call    prtstr          ! else print 'a,'
        defb    'a', ',' or $80
iacco01: pop    af              ! restore code
        call    psrl            ! print single register by state
        ret

! Rotate accumulator right
        
irrca:  call    prtstr          ! print 'rrca'
        defb    'rrc', 'a' or $80
        ret
        
! Rotate accumulator left
        
irlca:  call    prtstr          ! print 'rlca'
        defb    'rlc', 'a' or $80
        ret
        
! Rotate accumulator right through carry
        
irra:   call    prtstr          ! print 'rra'
        defb    'rr', 'a' or $80
        ret
        
! Rotate accumulator left through carry
        
irla:   call    prtstr          ! print 'rla'
        defb    'rl', 'a' or $80
        ret
        
! Push register pair
        
ipush:  call    prtstr          ! print 'push'
        defb    'pus', 'h' or $80
        call    tab2            ! index operands field
        call    pdr             ! print double register by state
        ret
        
! Pop register pair
        
ipop:   call    prtstr          ! print 'pop'
        defb    'po', 'p' or $80
        call    tab2            ! index operands field
        call    pdr             ! print double register by state
        ret
        
! Add double registers
        
idad:   call    prtstr          ! print 'add'
        defb    'ad', 'd' or $80
        call    tab2            ! index operands field
        call    pst             ! print the state register
        call    prtstr          ! print ','
        defb    ',' or $80
        call    pdr             ! print double register by state
        ret

! Increment double register
        
iincd:  call    prtstr          ! print 'inc'
        defb    'in', 'c' or $80
        call    tab2            ! index operands field
        call    pdr             ! print double register by state
        ret
        
! Decrement double register
        
idecd:  call    prtstr          ! print 'dec'
        defb    'de', 'c' or $80
        call    tab2            ! index operands field
        call    pdr             ! print double register by state
        ret
        
! Exchange hl with de
        
ixchg:  call    prtstr          ! print 'ex'
        defb    'e', 'x' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'de,hl'
        defb    'de,h', 'l' or $80
        ret
        
! Exchange double register with first on stack
        
ixthl:  call    prtstr          ! print 'ex'
        defb    'e', 'x' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print '(sp),'
        defb    '(sp)', ',' or $80
        call    pst             ! print the state register
        ret
        
! Load stack pair from double register
        
isphl:  call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'sp,'
        defb    'sp', ',' or $80
        call    pst             ! print the state register
        ret
        
! Load register pair immediate
        
ilxi:   call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        call    pdr             ! print double register by state
        call    prtstr          ! print ','
        defb    ',' or $80
        call    cdword          ! print double imm. value
        ret

! Load single register immediate
        
imvi:   call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        call    psrh            ! print single register by state
        call    prtstr          ! print ','
        defb    ',' or $80
        call    cdhex           ! print single imm. value
        ret
        
! Accumulator immediate operations
        
iacci:  call    popr            ! print the operation
        call    tab2            ! index operands field
        push    af              !  save code
        and     $38             ! mask
        cp      $10             ! check 'sub' code
        jr      z,iacci01       ! yes, skip
        bit     5,a             ! check logicals
        jr      nz,iacci01      ! yes, skip
        call    prtstr          ! else print 'a,'
        defb    'a', ',' or $80
iacci01: pop    af              ! restore
        call    cdhex           ! print single imm. value
        ret
        
! Store accumulator direct
        
ista:   call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print '('
        defb    '(' or $80
        call    cdword          ! print double value
        call    prtstr          ! print '),a'
        defb    '),', 'a' or $80
        ret
        
! Load accumulator direct
        
ilda:   call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'a,('
        defb    'a,', '(' or $80
        call    cdword          ! print double value
        call    prtstr          ! print ')'
        defb    ')' or $80
        ret

! Store double register direct
        
istdr:  call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print '('
        defb    '(' or $80
        call    cdword          ! print double value
        call    prtstr          ! print '),'
        defb    ')', ',' or $80
        call    pst             ! print the state register
        ret
        
! Load double register direct
        
ildrd:  call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        call    pst             ! print the state register
        call    prtstr          ! print ',('
        defb    ',', '(' or $80
        call    cdword          ! print double value
        call    prtstr          ! print ')'
        defb    ')' or $80
        ret
        
! Jp address
        
ijp:    call    prtstr          ! print 'jp'
        defb    'j', 'p' or $80
        call    tab2            ! index operands field
        bit     0,a             ! chk conditional jump
        jr      nz,ijp01        ! no
        call    pcond           ! yes, print condition
        call    prtstr          ! print ','
        defb    ',' or $80
ijp01:  call    cdword          ! print double value
        ret                     ! and exit
        
! Jump double register
        
ijpdr:  call    prtstr          ! print 'jp'
        defb    'j', 'p' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print '('
        defb    '(' or $80
        call    pst             ! print the state register
        call    prtstr          ! print ')'
        defb    ')' or $80
        ret                     ! and exit

! Call address
        
icall:  call    prtstr          ! print 'call'
        defb    'cal', 'l' or $80
        call    tab2            ! index operands field
        bit     0,a             ! chk conditional
        jr      nz,ical01       ! no
        call    pcond           ! yes, print condition
        call    prtstr          ! print ','
        defb    ',' or $80
ical01: call    cdword          ! print address
        ret
        
! Return to stack address
        
iret:   call    prtstr          ! print 'ret'
        defb    're', 't' or $80
        bit     0,a             ! chk conditional
        ret     nz              ! no
        call    tab2            ! index operands field
        call    pcond           ! yes, print condition
        ret                     ! and exit
        
! Restart
        
irst:   call    prtstr          ! print 'rst'
        defb    'rst', ' ' or $80
        call    tab2            ! index operands field
        and     $38             ! strip the 'restart' bits
        ld      l,a             ! and construct an address
        ld      h,0
        call    prtsym          ! print the resulting address
        ret                     ! and exit
        
! Enable interrupts
        
iei:    call    prtstr          ! print 'ei'
        defb    'e', 'i' or $80
        ret                     ! and exit
        
! Disable interrupts
        
idi:    call    prtstr          ! print 'di'
        defb    'd', 'i' or $80
        ret

! Input immediate to accumulator
        
iina:   call    prtstr          ! print 'in'
        defb    'i', 'n' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'a,('
        defb    'a,', '(' or $80
        call    cdhex           ! print single imm. value
        call    prtstr          ! print ')'
        defb    ')' or $80
        ret
        
! Output immediate from a register
        
iouta:  call    prtstr          ! print 'out'
        defb    'ou', 't' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print '('
        defb    '(' or $80
        call    cdhex           ! print single imm. value
        call    prtstr          ! print a '),a'
        defb    '),', 'a' or $80
        ret
        
! Halt (wait for interrupt)
        
ihalt:  call    prtstr          ! print 'halt'
        defb    'hal', 't' or $80
        ret
        
! Decrement b, jump if not zero
        
idjnz:  call    prtstr          ! print 'djnz'
        defb    'djn', 'z' or $80
        call    tab2            ! index operands field
idj01:  ld      a,(hl)          ! get displacement
        inc     hl              ! next
        ld      c,a
        or      a               ! do sign extend on displacement
        ld      b,0
        jp      p,idj02
        ld      b,$ff
idj02:  add     hl,bc
        call    prtsym          ! and output
        ret
        
! Jump relative
        
ijr:    call    prtstr          ! print 'jr'
        defb    'j', 'r' or $80
        call    tab2            ! index operands field
        jr      idj01           ! else same as 'djnz'

! Jump relative zero
        
ijrz:   call    prtstr          ! print 'jr'
        defb    'j', 'r' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'z,'
        defb    'z', ',' or $80
        jr      idj01           ! else same as 'djnz'
        
! Jump relative non-zero
        
ijrnz:  call    prtstr          ! print 'jr'
        defb    'j', 'r' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'nz,'
        defb    'nz', ',' or $80
        jr      idj01           ! else same as 'djnz'
        
! Jump relative carry
        
ijrc:   call    prtstr          ! print 'jr'
        defb    'j', 'r' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'c,'
        defb    'c', ',' or $80
        jr      idj01           ! else same as 'djnz'
        
! Jump relative no carry
        
ijrnc:  call    prtstr          ! print 'jr'
        defb    'j', 'r' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'nc,'
        defb    'nc', ',' or $80
        jr      idj01           ! else same as 'djnz'
        
! Exchange af with alternate register pair
        
iexa:   call    prtstr          ! print 'af,af''
        defb    'e', 'x' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'af,af'''
        defb    'af,af', '''' or $80
        ret
        
! Exchange alternate register set with regular set
        
iexx:   call    prtstr          ! print 'exx'
        defb    'ex', 'x' or $80
        ret                     ! and exit

! 'cb' extention page handler
        
icbx:   ld      a,b             ! chk the state
        or      a
        jr      z,icbx01        ! state zero, do regular
        inc     hl              ! do look-ahead for extention
        ld      a,(hl)
        dec     hl
        jr      icbx02
icbx01: ld      a,(hl)          ! get extended code byte
icbx02: push    af
        rlca                    ! separate type
        rlca
        and     $03
        jr      z,irtex         ! extended rotate
        add     a,bbt           ! bit operation, print
        call    prtmsg
        call    tab2            ! index operands field
        pop     af
        push    af
        rrca                    ! move bit spec into place
        rrca
        rrca
        and     $07             ! mask
        add     a,'0'           ! convert to ascii
        call    prtchr          ! and print
        pop     af
        call    prtstr          ! print ','
        defb    ',' or $80
        call    psrl            ! print single register by state
        ret                     ! and exit
        
! 'cb' extended rotates
        
irtex:  pop     af
        push    af
        rrca                    ! move operation code into place
        rrca
        rrca
        and     $07             ! mask relevant bits
        cp      $06             ! chk valid code
        ccf                     ! ajust code 7
        sbc     a,0
        add     a,brx           ! print the operation
        call    prtmsg
        call    tab2            ! index operands field
        pop     af
        call    psrl            ! print single register by state
        ret

! 'fd' extention handler
        
ifdx:   inc     b               ! go to state 2
        
! 'dd' extention handler
        
iddx:   inc     b               ! go to state 1
        ld      a,(hl)          ! get the byte
        inc     hl              ! next
        call    getadd          ! get the 8080 handler
        push    de              ! jp (de)
        ret
        
! 'ed' extention handler
        
iedx:   ld      a,(hl)          ! get the extended code byte
        inc     hl              ! next
        ld      c,a             ! save code
        sub     $40             ! set up table jump
        cp      $3c             ! chk 7c->
        jr      c,iedx01        ! no, go parse page
        sub     $24             ! yes, adjust codes
iedx01: push    hl              ! save pointer
        ld      hl,cttwo        ! index extention table
        call    getwrd          ! get the word
        pop     hl              ! restore pointer
        ld      a,c             ! restore code
        push    de              ! jp (de)
        ret
        
! Extended input
        
ixins:  call    prtstr          ! print 'in'
        defb    'i', 'n' or $80
        call    tab2            ! index operands field
        call    psrh            ! print single register by state
        call    prtstr          ! print ',(c)'
        defb    ',(c', ')' or $80
        ret
        
! Extended output
        
ixouts: call    prtstr          ! print 'out'
        defb    'ou', 't' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print '(c),'
        defb    '(c)', ',' or $80
        call    psrh            ! print single register by state
        ret

! Add double registers with carry
        
iadcdr: call    prtstr          ! print 'adc'
        defb    'ad', 'c' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'hl,'
        defb    'hl', ',' or $80
        call    pdr             ! print double register
        ret
        
! Subtract double registers with carry
        
isbcdr: call    prtstr          ! print 'sbc'
        defb    'sb', 'c' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print 'hl,'
        defb    'hl', ',' or $80
        call    pdr             ! print double register by state
        ret
        
! Load double register to direct
        
ilrdi:  call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        call    prtstr          ! print '('
        defb    '(' or $80
        call    cdword          ! print double imm. value
        call    prtstr          ! print '),'
        defb    ')', ',' or $80
        call    pdr             ! print double register
        ret
        
! Load direct to double register
        
ilird:  call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        call    tab2            ! index operands field
        call    pdr             ! print double register
        call    prtstr          ! print ',('
        defb    ',', '(' or $80
        call    cdword          ! print double imm. value
        call    prtstr          ! print ')'
        defb    ')' or $80
        ret
        
! Negate accumulator
        
ineg:   call    prtstr          ! print 'neg'
        defb    'ne', 'g' or $80
        ret

! Rotate right digit
        
irrd:   call    prtstr          ! print 'rrd'
        defb    'rr', 'd' or $80
        ret                     ! and exit
        
! Rotate left digit
        
irld:   call    prtstr          ! print 'rld'
        defb    'rl', 'd' or $80
        ret
        
! Extended load
        
ixld:   call    prtstr          ! print 'ld'
        defb    'l', 'd' or $80
        jr      ixot02          ! else same as ixout
        
! Extended compare
        
ixcp:   call    prtstr          ! proint 'cp'
        defb    'c', 'p' or $80
        jr      ixot02          ! else same as ixout
        
! Extended input
        
ixin:   call    prtstr          ! print 'in'
        defb    'i', 'n' or $80
        jr      ixot02          ! else same as ixout
        
! Extended output
        
ixout:  bit     4,a             ! chk repeat
        jr      nz,ixot01       ! yes
        call    prtstr          ! no, print 'out'
        defb    'ou', 't' or $80
        jr      ixot02
ixot01: call    prtstr          ! print 'ot'
        defb    'o', 't' or $80
ixot02: bit     3,a             ! chk inc/dec
        jr      nz,ixot03       ! decrement
        call    prtstr          ! print 'i'
        defb    'i' or $80
        jr       ixot04
ixot03: call    prtstr          ! print 'd'
        defb    'd' or $80
ixot04: bit     4,a             ! chk repeat
        ret     z               ! no, exit
        call    prtstr          ! print 'r'
        defb    'r' or $80
        ret

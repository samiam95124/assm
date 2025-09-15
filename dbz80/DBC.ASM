
! List command.
! lists (disassembles) memory from the first command
! provided address to the second command provided
! address. list format is one per line. If the
! second parameter does not exist, will do a single
! instruction list.
        
list:   call    expr            ! get starting address of list
        call    gensav          ! save
        call    skpspc          ! chk for param separator, ','
        cp      ','
        jr      nz,list01       ! no second parameter, default to first
        call    getchr          ! skip separator
        call    expr            ! get ending address
list01: call    genfnc          ! generate function call
        
        ex      de,hl           ! place parameters
        pop     hl
        ex      (sp),hl
list02: push    hl              ! save old address
        call    disass          ! do single instruction dissassembly
        ld      a,h             ! get high byte for comparision
        ex      (sp),hl         ! trade new for old address
        cpl                     ! special case test for
        and     h               ! ffff-0000 crossing
        jp      m,list03        ! yes
        pop     hl              ! get new address
        push    hl              ! save again
        ex      de,hl           ! compare end - current
        push    hl
        sbc     hl,de
        pop     de
        ccf                     ! complement status
list03: pop     hl              ! restore new address
        ret     nc              ! it's below us, exit
        call    inpchk          ! chk for input break
        jr      list02          ! else loop

! Enter bytes into memory.
! Enters any number of command provided byte values
! to a command provided address, squentially from the
! address upwards.
        
entr:   call    expr            ! parse entry address
        call    skpspc          ! skip spaces
        call    getchr          ! check ','
        cp      ','
        ld      a,eparm         ! flag error
        jp      nz,error        ! process
entr01: call    gensav          ! save address
        call    expr            ! parse value
        call    genexc          ! save
        call    genrst          ! get the address
        call    genstb          ! store the byte
        call    geninc          ! next location
        call    skpspc          ! check another value
        cp      ','
        jp      nz,next         ! no, exit
        call    getchr          ! skip
        jr      entr01          ! and loop

! Assign varible.
! Assigns the command provided value to the varible.
! Each one of the registers are considered a varible
! by DB. loading a byte register with a value > 255
! will result in truncation. Note that we provide
! three non-register varibles (for counting, etc.)
! x, y and z. These are unchanged by program execution.
        
assign: call    skpspc          ! skip spaces
        cp      '_'             ! check leading variable character
        jr      z,assign01      ! yes
        call    alpha
        ld      a,esymf         ! flag error
        jp      nz,error        ! process if not
assign01: call  varsch          ! get the varible address
        ld      b,a             ! save the type (byte/word)
        ld      a,esymf         ! flag error
        jp      z,error         ! not found, error
        call    skpspc          ! skip spaces
        call    getchr          ! check '='
        cp      ','
        ld      a,eparm         ! flag error
        jp      nz,error        ! error
        call    expr            ! get the value
        call    genexc          ! save
        call    genlod          ! get the address of varible
        dec     b               ! check type (byte/word)
        jr      z,assign02      ! byte, skip
        call    genstw          ! word, do word store
        jp      next            ! and exit
assign02: call  genstb          ! byte, do byte store
        jp      next            ! and exit

! Input/output port.
! If just the port is provided, will input a byte
! from the port and prints that. If a value is present,
! outputs the command provided value to the command
! provided port.
        
port:   call    expr            ! get port to output to
        call    skpspc          ! check second parameter
        cp      ','
        jr      nz,port01       ! no
        call    getchr          ! yes, skip
        call    gensav          ! save port
        call    expr            ! get value to output
        call    genfnc          ! generate function call
        
        ex      de,hl           ! save value
        pop     hl              ! get port
        ex      (sp),hl
        ex      de,hl
        ld      a,d             ! chk < 256
        or      a
        ld      a,erange        ! flag error
        jp      nz,error
        ld      c,e             ! place port number
        out     (c),l           ! good value, output
        ret                     ! exit
        
port01: call    genfnc          ! generate function
        
        ld      a,h             !  check port < 256
        or      a
        ld      a,erange        ! flag error
        jp      nz,error        ! error
        ld      c,l             ! set up port value
        in      a,(c)           ! and input from that port
        call    phex            ! print
        call    crlf            ! next line
        ret                     ! exit

! Flag set/reset commands.
! Each of the zero, half-carry, parity, add/subtract
! or carry flags are either set or reset.
        
! Flag reset commands
        
fp:     ld      b,not $80       ! reset sign flag
        jr      fnc01
        
fnz:    ld      b,not $40       ! reset zero flag
        jr      fnc01
        
fnh:    ld      b,not $10       ! reset half-carry flag
        jr      fnc01
        
fpo:    ld      b,not $04       ! reset parity flag
        jr      fnc01
        
fa:     ld      b,not $02       ! reset add/subtract flag
        jr      fnc01
        
fnc:    ld      b,not $01       ! reset carry flag
fnc01:  ld      hl,afreg        ! index flag register
        call    genlda          ! generate 'ld a,(afreg)'
        ld      a,b             ! get the mask
        call    genani          ! generate 'and x'
        call    gensta          ! generate 'ld (afreg),a'
        jp      next            ! exit
        
! Flag set commands
        
fm:     ld      b,$80           ! set sign flag
        jr      fc01
        
fz:     ld      b,$40           ! set zero flag
        jr      fc01
        
fh:     ld      b,$10           ! set half-carry flag
        jr      fc01
        
fpe:    ld      b,$04           ! set parity flag
        jr      fc01
        
fs:     ld      b,$02           ! set add/subtract flag
        jr      fc01
        
fc:     ld      b,$01           ! set carry flag
fc01:   ld      hl,afreg        ! index flag register
        call    genlda          ! generate 'ld a,(afreg)'
        ld      a,b             ! get mask
        call    genori          ! generate 'or x'
        call    gensta          ! generate 'ld (afreg),a'
        jp      next            ! exit

! Move bytes.
! Moves a command specified number of bytes from
! one location to another.
        
move:   call    expr            ! get start sorce address
        call    gensav          ! save that parameter
        call    skpspc          ! skip spaces
        call    getchr          ! get ',' param separator
        cp      ','
        ld      a,eparm         ! flag error
        jp      nz,error
        call    expr            ! get end sorce address
        call    gensav          ! save that parameter
        call    skpspc          ! skip spaces
        call    getchr          ! get ',' param separator
        cp      ','
        ld      a,eparm         ! flag error
        jp      nz,error        ! error
        call    expr            ! get destination address
        call    genfnc          ! generate function
        
        ld      b,h             ! save
        ld      c,l
        pop     hl
        ex      (sp),hl
        ex      de,hl
        pop     hl
        ex      (sp),hl
        ex      de,hl
        sbc     hl,de           ! find number of bytes to move
        ld      a,eblock        ! flag error
        jp      c,error         ! cannot move negative bytes
        inc     hl              ! adjust byte count
        push    bc              ! save
        ld      b,h             ! and save in bc
        ld      c,l
        pop     hl
        ex      de,hl           ! swap sorce and dest for move
        ldir                    ! do move
        ret                     ! exit

! Compare bytes.
! Compares two blocks for equal content.
        
comp:   call    expr            ! get start sorce address
        call    gensav          ! save
        call    skpspc          ! skip spaces
        call    getchr          ! get ',' param separator
        cp      ','
        ld      a,eparm         ! flag error
        jp      nz,error        ! error
        call    expr            ! get end sorce address
        call    gensav          ! save
        call    skpspc          ! skip spaces
        call    getchr          ! get ',' param separator
        cp      ','
        ld      a,eparm         ! flag error
        jp      nz,error
        call    expr            ! get destination address
        call    genfnc          ! generate function
        
        ld      b,h             ! save
        ld      c,l
        pop     hl
        ex      (sp),hl
        ex      de,hl
        pop     hl
        ex      (sp),hl
        ex      de,hl
        sbc     hl,de           ! find block length
        ld      a,eblock        ! flag error
        jp      c,error         ! error
        inc     hl              ! adjust
        push    bc              ! save
        ld      b,h
        ld      c,l
        pop     hl
comp01: ld      a,(de)          ! compare bytes
        cp      (hl)
        jr      z,comp02        ! good, go next byte
        call    inpchk          ! chk input break
        call    pword           ! print address
        call    prtstr          ! and ' '
        defb    ' ' or $80
        ld      a,(hl)          ! get 'was' byte
        call    phex            ! print
        call    prtstr          ! print ' s/b '
        defb    ' s/b', ' ' or $80
        ld      a,(de)          ! get 's/b' byte
        call    phex            ! print
        call    crlf            ! next line
comp02: dec     bc              ! count
        inc     de
        inc     hl
        ld      a,b
        or      c
        jr      nz,comp01       ! and loop until done
        ret                     ! exit

! Fill memory.
! Fills a block with the command provided value
        
fill:   call    expr            ! get starting address
        call    gensav          ! save
        call    skpspc          ! skip spaces
        call    getchr          ! get ',' param separator
        cp      ','
        ld      a,eparm         ! flag error
        jp      nz,error
        call    expr            ! get ending address
        call    gensav          ! save
        call    skpspc          ! skip spaces
        call    getchr          ! check ','
        cp      ','
        ld      a,eparm         ! flag error
        jp      nz,error        ! error
        call    expr            ! get fill byte
        call    genfnc          ! generate function
        
        ld      a,h             ! check < 256
        or      a
        ld      a,erange        ! flag error
        jp      nz,error        ! error
        ld      c,l             ! save
        pop     hl
        ex      (sp),hl
        ex      de,hl
        pop     hl
        ex      (sp),hl
        ex      de,hl
        sbc     hl,de           ! find fill length
        ld      a,eblock        ! flag error
        jp      c,error         ! cannot fill negative
        inc     hl              ! adjust fill
        ld      a,c
        ld      b,h             ! save length
        ld      c,l
        ld      l,a
        ex      de,hl           ! place address in hl
fill01: ld      (hl),e          ! place a byte
        inc     hl              ! next byte
        dec     bc              ! count
        ld      a,b
        or      c
        jr      nz,fill01       ! loop until done
        ret                     ! exit

! Search for byte.
! Searches a block for the command provided value
        
sear:   call    expr            ! get starting address
        call    gensav
        call    skpspc          ! skip spaces
        call    getchr          ! get ',' param separator
        cp      ','
        ld      a,eparm         ! flag error
        jp      nz,error
        call    expr            ! get ending address
        call    gensav
        call    skpspc          ! skip spaces
        call    getchr
        cp      ','
        ld      a,eparm         ! flag error
        jp      nz,error
        call    expr
        call    genfnc          ! generate function
        
        ld      a,h
        or      a
        ld      a,erange        ! flag error
        jp      nz,error
        ld      c,l
        pop     hl
        ex      (sp),hl
        ex      de,hl
        pop     hl
        ex      (sp),hl
        ex      de,hl
        sbc     hl,de           ! find length
        ld      a,eblock        ! flag error
        jp      c,error         ! negative
        inc     hl              ! adjust length
        ld      a,c
        ld      b,h             ! and save in bc
        ld      c,l
        ld      l,a
        ex      de,hl           ! put address in hl
sear01: ld      a,e             ! and byte in a
        cp      (hl)            ! chk byte is there
        jr      nz,sear02       ! no
        call    inpchk          ! yes, check input break
        call    pword           ! print the address found
        call    crlf            ! next line
sear02: inc     hl              ! next byte
        dec     bc
        ld      a,b
        or      c
        jr      nz,sear01       ! loop until done
        ret                     ! exit

! Search for non-occurances.
! Searches a block for non-occurances of the command
! provided value.
        
searn:  call    expr            ! get starting address
        call    gensav
        call    skpspc          ! skip spaces
        call    getchr          ! get ',' param separator
        cp      ','
        ld      a,eparm         ! flag error
        jp      nz,error
        call    expr            ! get ending address
        call    gensav
        call    skpspc          ! skip spaces
        call    getchr          ! check ','
        cp      ','
        ld      a,eparm         ! flag error
        jp      nz,error
        call    expr
        call    genfnc          ! generate function
        
        ld      a,h
        or      a
        ld      a,erange        ! flag error
        jp      nz,error
        ld      c,l
        pop     hl
        ex      (sp),hl
        ex      de,hl
        pop     hl
        ex      (sp),hl
        ex      de,hl
        sbc     hl,de           ! find length
        ld      a,eblock        ! flag error
        jp      c,error         ! negative
        inc     hl              ! adjust length
        ld      a,c
        ld      b,h             ! save in bc
        ld      c,l
        ld      l,a
        ex      de,hl           ! place address in hl
searn01: ld     a,e             ! and byte in a
        cp      (hl)            ! chk byte is there
        jr      z,searn02       ! yes, skip
        call    inpchk          ! else chk input break
        call    pword           ! print address
        call    prtstr          ! print ' '
        defb    ' ' or $80
        ld      a,(hl)          ! get non-match byte
        call    phex            ! print it
        call    crlf            ! next line
searn02: inc    hl              ! next byte
        dec     bc              ! count
        ld      a,b
        or      c
        jr      nz,searn01      ! loop until done
        ret                     ! exit

! Enable word symbols
        
wsym:   call    genfnc          ! generate function
        
        ld      a,(lstops)      ! get list options
        set     fwsym,a         ! set flag
        ld      (lstops),a      ! replace options
        ret                     ! exit
        
! Disable word symbols
        
nwsym:  call    genfnc          ! generate function
        
        ld      a,(lstops)      ! get list options
        res     fwsym,a         ! reset flag
        ld      (lstops),a      ! replace options
        ret                     ! exit
        
! Enable byte symbols
        
bsym:   call    genfnc          ! generate function
        
        ld      a,(lstops)      ! get list options
        set     fbsym,a         ! set flag
        ld      (lstops),a      ! replace options
        ret                     ! exit
        
! Disable byte symbols
        
nbsym:  call    genfnc          ! generate function
        
        ld      a,(lstops)      ! get list options
        res     fbsym,a         ! reset flag
        ld      (lstops),a      ! replace options
        ret                     ! exit

! Return from subroutine.
! Sets a breakpoint at the location pointed
! to by the fos word, then executes at the
! current pc. Upon return, the breakpoint is
! cleared. The result is an efective 'return'
! from the current routine. Note that if
! there is already a breakpoint set at (fos),
! it will not be cleared.
        
return: call    genfnc          ! generate function
        
        ld      hl,(spreg)      ! get the sp
        ld      e,(hl)          ! get the fos word
        inc     hl
        ld      d,(hl)
        ex      de,hl           ! in hl
        call    mbrkt           ! see if it exists already
        jr      z,return01      ! yes, skip
        call    sbrkt           ! no, set the breakpoint
        call    goadd           ! go to the pc
        call    cbrkt           ! clear the breakpoint
        ret                     ! exit
return01: call  goadd           ! go to the pc
        ret                     ! exit

! Enable printer
        
ptr:    call    genfnc          ! generate function
        
        ld      a,(lstops)      ! get the list options
        set     fpatt,a         ! set printer attach
        ld      (lstops),a      ! update
        ret                     ! exit
        
! Disable printer
        
nptr:   call    genfnc          ! generate function
        
        ld      a,(lstops)      ! get list options
        res     fpatt,a         ! disable printer
        ld      (lstops),a      ! update
        ret                     ! exit

! Enable expand
        
sexp:   call    genfnc          ! genrate function
        
        ld      a,(trops)       ! get trace options
        set     frd,a           ! set expand
        ld      (trops),a       ! update
        ret                     ! exit
        
! Disable expand
        
rexp:   call    genfnc          ! generate function call
        
        ld      a,(trops)       ! get trace options
        res     frd,a           ! disable expand
        ld      (trops),a       ! update
        ret                     ! exit

! Halt trap
        
shlt:   call    genfnc          ! generate function
        
        ld      a,(trops)       ! get trace options
        set     fhlt,a          ! set halt trap
        ld      (trops),a       ! update
        ret                     ! exit
        
! No halt trap
        
rhlt:   call    genfnc          ! generate function
        
        ld      a,(trops)       ! get trace options
        res     fhlt,a          ! no halt trap
        ld      (trops),a       ! update
        ret                     ! exit

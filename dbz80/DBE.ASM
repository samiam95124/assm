!
! Get character from input buffer
!
!      Gets a single character from the input buffer into
!      a. If the character is not eoln,
!      will increment iptr past the character. (i.e., will
!      not skip past eoln)
!      Returns z if eoln is encountered.
!
!      In parameters: none
!      Out parameters: character from buffer - a, status - z
!
getchr: push    hl
        call    chkchr          ! check next
        jr      z,getc01        ! end of line, exit
        ld      hl,(iptr)       ! else skip iptr past char
        inc     hl
        ld      (iptr),hl
getc01: pop     hl              ! clean up and return
        ret
!
! Check next character
!
!     Returns the next character from the input
!     buffer. Returns z if the eoln is encountered.
!
!     In parameters: none
!     Out parameters: character - a, status - z
!
chkchr: push    hl
        ld      hl,(iptr)       ! get input pointer
        ld      a,(hl)          ! get a character
        or      a               ! set flags by eoln (0)
        pop     hl              ! clean up and return
        ret

!
! Skip spaces in command line
!
!      Skips iptr over any spaces in the command buffer.
!      Returns the character at the resulting iptr in a.
!      Returns z if eoln encountered.
!
!      In parameters: none
!      Out parameters: character at pointer - a, status - z
!
skpspc: call    chkchr          ! check next
        ret     z               ! eoln, exit
        cp      ' '             ! check space
        ret     nz              ! no, exit
        call    getchr          ! skip space
        jr      skpspc          ! and loop

!
! Check end of command
!
!     Checks for end of command string.
!     Skips any spaces, and returns zero if
!     either a eol or ';' is encountered.
!
!     In parameters: none
!     Out parameters: first non-space character encountered - a
!
chkend: call    skpspc          ! skip spaces
        ret     z               ! eol, return
        cp      ';'             ! check '!'
        ret                     ! and return with that status

!
! READ NUMERIC
!
!     Reads a numeric and returns the value
!     in hl. The numeric may be preceded by
!     a radix specifier:
!
!          % - Binary (base 2)
!          @ - Octal (base 8)
!          $ - Hexadecimal (base 16)
!
!     In parameters: none
!     Out parameters: value - hl
!     Modifies: af
!
rdnum:  push    bc
        push    de
        ld      hl,0            ! initalize result
        
! Find radix
        
        call    chkchr          ! check next
        cp      '%'             ! check binary
        ld      b,2             ! set radix
        jr      z,rdnum01       ! yes, go
        cp      '@'             ! check octal
        ld      b,8             ! set radix
        jr      z,rdnum01       ! yes, go
        cp      '$'             ! check hexadecimal
        ld      b,16            ! set radix
        jr      z,rdnum01       ! yes, go
        ld      b,10            ! set decimal default
        jr      rdnum02         ! go
rdnum01: call   getchr          ! skip specifier
        
! Verify primary digit
        
rdnum02: call   chkchr          ! check next
        call    alpha           ! check alphabetical
        jr      z,rdnum03       ! yes
        call    digit           ! check digit
        jr      nz,rdnum06      ! no, error
        
! Read and convert digits
        
rdnum03: call   getchr          ! get next
        call    lcase           ! convert to lower case
        sub     '0'             ! convert '0'-'9'
        jr      c,rdnum06       ! error
        cp      10              ! check '0'-'9'
        jr      c,rdnum04       ! yes
        sub     'a'-'0'-10      ! convert 'a'-'f'
        jr      c,rdnum06       ! error
        cp      b               ! verify within radix
        jr      nc,rdnum06      ! no, error
        
! Scale
        
rdnum04: ld     c,a             ! save new value
        ex      de,hl           ! save result
        ld      hl,0            ! clear accumulator
        push    bc              ! save radix
rdnum05: add    hl,de           ! scale
        jr      c,rdnum06       ! error
        djnz    rdnum05         ! loop
        add     hl,bc           ! add new value
        jr      c,rdnum06       ! error
        pop     bc              ! restore radix
        call    chkchr          ! check next
        call    alpha           ! check alphabetical
        jr      z,rdnum03       ! yes, loop
        call    digit           ! check digit
        jr      z,rdnum03       ! yes, loop
        jr      rdnum07         ! exit
rdnum06: ld     a,eivnum        ! flag error
        jp      error           ! process
rdnum07: pop    de              ! clean up and return
        pop     bc
        ret

!
! Print numeric
!
!     Prints the numeric in hl, using the radix
!     in c. Only as many digits as required are
!     output.
!
!     In parameters: value - hl, radix - c
!     Out parameters: none
!     Modifies: af
!
prtnum: push    bc
        push    de
        push    hl
        ld      de,255          ! place stack flag
        push    de
prtnum01: ld    e,c             ! copy radix
        ld      d,0
        call    div             ! do division by radix
        ld      a,l             ! result = 0 ?
        or      h
        jr      z,prtnum02      ! yes
        push    de              ! no, push a digit
        jr      prtnum01        ! and loop
prtnum02: ld    a,e             ! get digit
        inc     e               ! check flag
        jr      z,prtnum05      ! yes, exit
        cp      10              ! check '0'-'9'
        jr      nc,prtnum03     ! no, skip
        add     a,'0'           ! convert '0'-'9'
        jr      prtnum04        ! go
prtnum03: add   a,'a'-10        ! convert 'a'-'f'
prtnum04: call  prtchr          ! print character
        pop     de              ! next digit
        jr      prtnum02        ! loop
prtnum05: pop   hl              ! clean up and return
        pop     de
        pop     bc
        ret

!
! Input character
!
!      Inputs a single character from the control
!      device. Waits if a character is not ready.
!      Returns the input character in a.
!
!      In parameters:none
!      Out parameters:input character-a
!
inpchr: call    inpvec          ! call the external input
        or      a               ! chk null
        jr      z,inpchr        ! wait if so
        ret                     ! else exit with character

!
! Output character
!
!      Outputs a single character to the console
!      device. Expects character to print in a.
!      If 'prtsw' is true, will echo the character to the
!      list device. Note that in this version the console
!      device must allways be connected.
!
!      In parameters: output character - a
!      Out parameters: none
!      Modifies: af
!
prtchr: push    af              ! save the character
        call    cotvec          ! send to console output
        ld      a,(lstops)      ! get list options
        bit     fpatt,a         ! check list enable
        ex      (sp),hl         ! restore character
        ld      a,h
        pop     hl
        call    nz,lotvec       ! echo to list if so
        ld      a,(lincnt)      ! get the line counter
        inc     a               ! count character output
        ld      (lincnt),a
        ret

!
! Output cr-lf
!
!      Outputs a cr-lf to the output stream. Leaves
!      all registers unmodified.
!
!      In parameters: none
!      Out parameters: none
!      Modifies: none
!
crlf:   push    af
        ld      a,cr            ! print a cr
        call    prtchr
        ld      a,lf            ! print a lf
        call    prtchr
        xor     a               ! clear line count
        ld      (lincnt),a
        pop     af
        ret

!
! Check input break
!
!      Checks the control device for a cancel (ctl-c)
!      or stop (ctl-s). If a stop is input, this routine
!      will wait until some other character is pressed
!      before returning. If ctl-c is input, will
!      abort to next command.
!
!      In parameters: none
!      Out parameters: none
!      Modifies: af
!
inpchk: call    inpvec          ! call the external input
        or      a               ! chk null
        ret     z               ! exit if so
inpc01: cp      ctlc            ! is it cancel ?
        jp      z,cexec         ! yes, restart zbug
        cp      ctls            ! is it stop
        ret     nz              ! no, exit with status
        call    inpchr          ! yes, get a break-lock character
        jr      inpc01          ! and loop

!
! Match two strings
!
!      Matches strings at hl and de. the string at de
!      must be terminated by bit 7 high.
!      The string at hl is terminated by a non-label
!      character (see 'chklcr').
!      Returns z if there is a match.
!
!      In parameters: unterminated string address - hl,
!                    terminated string address-de.
!      Out parameters: status - z
!
match:  push    bc
        push    de
        push    hl
match01: ld     a,(hl)          ! get a
        call    chklcr          ! check label character
        jr      nz,match03      ! no, exit
        call    lcase           ! convert lower case
        ld      c,a             ! save
        ld      a,(de)          ! get b
        and     $7f             ! mask terminator
        call    lcase           ! convert lower case
        cp      c               ! check equal
        jr      nz,match03      ! no, exit
        ld      a,(de)          ! get b
        bit     7,a             ! check terminal
        inc     de              ! next
        inc     hl
        jr      z,match01       ! no, loop
        ld      a,(hl)          ! check end a
        call    chklcr
        jr      nz,match02      ! yes
        xor     a               ! no, set no match
        inc     a
        jr      match03         ! exit
match02: xor    a               ! set match
match03: pop    hl              ! clean up and return
        pop     de
        pop     bc
        ret

!
! Convert to lower case
!
!     Converts the character in a to lower case,
!     if it lies in the set ['A'-'Z']. Returns
!     the lower case character in a.
!
!     In parameters: (possibly upper case) character - a
!     Out parameters: (definately lower case) character - a
!
lcase:  cp      'A'             ! check 'A'-'Z'
        ret     c               ! no
        cp      'Z'+1
        ret     nc              ! no
        add     a,' '           ! yes, convert to lower case
        ret                     ! and exit
!
! Check label character
!
!     Checks wether the character in a lies in the set:
!     ['a'-'z', 'A'-'Z', '0'-'9', '_', ''''], all of
!     which are allowed to specify labels ('''' being
!     used for Z80 alternate registers).
!     Returns z if the character is a label character.
!
!     In parameters: character - a
!     Out parameters: status - z
!     Modifies: f
!
chklcr: call    alpha           ! check alphabetical
        ret     z               ! yes
        call    digit           ! check digit
        ret     z               ! yes
        cp      '_'             ! check '_'
        ret     z               ! yes
        cp      ''''            ! check ''''
        ret                     ! exit with status

!
! Check alphabetical
!
!     Checks if the character in a lies in the set
!     ['A'-'Z', 'a'-'z']. Returns z if so.
!
!     In parameters: character - a
!     Out parameters: status - z
!     Modifies: f
!
alpha:  push    bc
        ld      c,a             ! save character
        call    lcase           ! find lower case
        cp      'a'             ! check 'a'-'z'
        jr      c,alpha01
        cp      'z'+1
        jr      nc,alpha01
        xor     a               ! set true
        jr      alpha02         ! exit
alpha01: xor    a               ! set false
        inc     a
alpha02: ld     a,c             ! restore character
        pop     bc              ! clean up and return
        ret
!
! Check digit
!
!     Checks if the character in a lies in the
!     set ['0'-'9']. Returns z if so.
!
!     In parameters: character - a
!     Out parameters: status - z
!     Modifies: f
!
digit:  push    bc
        ld      c,a             ! save character
        cp      '0'             ! check '0'-'9'
        jr      c,digit01       ! no
        cp      '9'+1
        jr      nc,digit01      ! no
        xor     a               ! set true
        jr      digit02         ! exit
digit01: xor    a               ! set false
        inc     a
digit02: ld     a,c             ! restore character
        pop     bc              ! clean up and return
        ret

!
! Multiply words
!
!     Performs 16-bit multiply. Returns hl=hl*de
!
!     In parameters: multiplicand - hl, multiplier - de
!     Out parameters: result - hl
!     Modifies: af
!
mult:   push    bc
        push    de
        ld      c,l             ! save multipler
        ld      b,h
        ld      hl,0            ! initalize result
        ld      a,16            ! set bit counter
mult01: rr      b               ! rotate out lsb
        rr      c
        jr      nc,mult02       ! skip add if no carry
        add     hl,de           ! carry, add in this power
mult02: sla     e               ! generate next power
        rl      d
        dec     a               ! count bits
        jr      nz,mult01       ! and loop for all powers
        pop     de              ! clean up and return
        pop     bc
        ret

!
! Divide word
!
!     Performs 16-bit divide. Returns hl=hl/de,
!     de=hl.mod.de.
!
!     In parameters: dividend - hl, divisor - de
!     Out parameters: quotient - hl, remainder - de
!     Modifies: af
!
div:    push    bc
        ld      a,e             ! check zero divide
        or      d
        ld      a,edivz         ! flag error
        jp      z,next          ! trap out if so
        ld      c,l             ! save dividend
        ld      b,h
        ld      hl,0            ! initalize remainder
        ld      a,17            ! set bit count
div01:  or      a               ! clear carry
        sbc     hl,de           ! subtract this power
        ccf                     ! invert carry sense
        jr      c,div02         ! and skip if good
        add     hl,de           ! else restore remainder
        or      a               ! clear carry
div02:  rl      c               ! shift go/no go bit into
        rl      b               ! quotient while shifting
        adc     hl,hl           ! dividend into remainder
        dec     a               ! count bits
        jr      nz,div01        ! and loop for all powers
        srl     h               ! restore remainder
        rr      l
        ex      de,hl           ! place in de
        ld      l,c             ! place quotient in hl
        ld      h,b
        pop     bc              ! clean up and return
        ret

!
! SHIFT LEFT
!
!     Expects the value to shift in hl, and the shift
!     count in de. Shifts the value count times.
!
!     In parameters: value - hl, shift count - de
!     Out parameters: shifted value - hl
!     Modifies: af
!
bshl:   push    bc
        push    de
        ld      a,d             ! check count > 16
        or      a
        jr      nz,bshl02       ! yes
        ld      a,e
        cp      17
        jr      nc,bshl02       ! yes
bshl01: ld      a,e             ! check count = 0
        or      d
        jr      z,bshl03        ! yes, exit
        rl      l               ! rotate
        rl      h
        dec     de              ! count
        jr      bshl01          ! loop
bshl02: ld      hl,0            ! set value
bshl03: pop     de              ! clean up and return
        pop     bc
        ret

!
! SHIFT RIGHT
!
!     Expects the value to shift in hl, and the shift count
!     in de. Shifts the value count times.
!
!     In parameters: value - hl, shift count - de
!     Out parameters: shifted value - hl
!     Modifies: af
!
bshr:   push    bc
        push    de
        ld      a,d             ! check count > 16
        or      a
        jr      nz,bshr02       ! yes
        ld      a,e
        cp      17
        jr      nc,bshr02       ! yes
bshr01: ld      a,e             ! check count = 0
        or      d
        jr      z,bshr03        ! yes, exit
        rr      h               ! rotate
        rr      l
        dec     de              ! count
        jr      bshr01          ! loop
bshr02: ld      hl,0            ! set value
bshr03: pop     de              ! clean up and return
        pop     bc
        ret

!
! Expression parser
!
!      Parses the expression at the current iptr
!      character. The appropriate code for the
!      expression is generated.
!
!      In parameters: none
!      Out parameters: none
!      Modifies: af
!
expr:   push    de
        push    hl
        call    sexpr           ! parse simple expression
expr01: call    skpspc          ! skip spaces
        cp      '='             ! check equal
        jr      nz,expr02       ! no
        call    getchr          ! skip
        call    chkchr          ! check next
        cp      '>'             ! check greater than
        ld      hl,gengeq       ! index greater than or equal
        jr      z,expr04        ! yes, go
        cp      '<'             ! check less than
        ld      hl,genleq       ! index less than or equal routine
        jr      z,expr04        ! yes, go
        ld      hl,genequ       ! index equal to routine
        jr      expr05          ! go
expr02: cp      '<'             ! check less than
        jr      nz,expr03       ! no
        call    getchr          ! skip
        call    chkchr          ! check next
        cp      '='             ! check equal
        ld      hl,genleq       ! index less than or equal
        jr      z,expr04        ! yes, go
        cp      '>'             ! check greater than
        ld      hl,genneq       ! index not equal routine
        jr      z,expr04        ! yes, go
        ld      hl,genltn       ! index less than routine
        jr      expr05          ! go
expr03: cp      '>'             ! check greater than
        jr      nz,expr06       ! no
        call    getchr          ! skip
        call    chkchr          ! check next
        cp      '='             ! check equal
        ld      hl,gengeq       ! index greater than or equal routine
        jr      z,expr04        ! yes, go
        cp      '<'             ! check less than
        ld      hl,genneq       ! index not equal routine
        jr      z,expr04        ! yes, go
        ld      hl,gengtn       ! index greater than routine
        jr      expr05          ! go
expr04: call    getchr          ! skip
expr05: call    gensav          ! generate save last
        call    sexpr           ! parse simple expression
        call    genexc          ! generate exchange
        call    genrst          ! generate restore last
        ld      de,expr01       ! index return (loop)
        push    de              ! save on stack
        jp      (hl)            ! go routine
expr06: pop     hl              ! clean up and return
        pop     de
        ret

!
! Parse simple expression
!
!     Parses and generates code for a simple
!     expression.
!
!     In parameters: none
!     Out parameters: none
!     Modifies: af
!
sexpr:  push    de
        push    hl
        call    term            ! parse term
sexpr01: call   skpspc          ! skip spaces
        cp      '+'             ! check add
        ld      hl,genadd       ! index add routine
        jr      z,sexpr02       ! yes, go
        cp      '-'             ! check subtract
        ld      hl,gensub       ! index subtract routine
        jr      z,sexpr02       ! yes, go
        ld      de,sexop        ! index simple expression operators
        call    search          ! search them
        jr      nz,sexpr04      ! none found, exit
        ld      a,(hl)          ! get table address
        inc     hl
        ld      h,(hl)
        ld      l,a
        jr      sexpr03         ! go
sexpr02: call   getchr          ! skip
sexpr03: call   gensav          ! generate save last
        call    term            ! parse term
        call    genexc          ! generate exchange
        call    genrst          ! generate restore last
        ld      de,sexpr01      ! index return (loop)
        push    de              ! save on stack
        jp      (hl)            ! go routine
sexpr04: pop    hl              ! clean up and return
        pop     de
        ret

!
! Parse term
!
!     Parses and generates code for the term.
!
!     In parameters: none
!     Out parameters: none
!     Modifies: af
!
term:   push    de
        push    hl
        call    factor          ! parse factor
term01: call    skpspc          ! skip spaces
        cp      '*'             ! check multiply
        ld      hl,term05       ! index multiply routine
        jr      z,term02        ! yes, go
        cp      '/'             ! check divide
        ld      hl,term06       ! index divide routine
        jr      z,term02        ! yes, go
        ld      de,termop       ! index term operators
        call    search          ! search them
        jr      nz,term04       ! none found, exit
        ld      a,(hl)          ! get table address
        inc     hl
        ld      h,(hl)
        ld      l,a
        jr      term03          ! go
term02: call    getchr          ! skip
term03: call    gensav          ! generate save last
        call    factor          ! parse factor
        call    genexc          ! generate exchange
        call    genrst          ! generate restore last
        ld      de,term01       ! index return (loop)
        push    de              ! save on stack
        jp      (hl)            ! go routine
term04: pop     hl              ! clean up and return
        pop     de
        ret
!
term05: ld      hl,mult         ! index multiply routine
        call    gencal          ! generate call
        ret                     ! exit
!
term06: ld      hl,div          ! index divide routine
        call    gencal          ! generate call
        ret                     ! exit
!
term07: ld      hl,div          ! index divide routine
        call    gencal          ! generate call
        call    genexc          ! generate exchange to modulo
        ret                     ! exit
!
term08: ld      hl,bshl         ! index shift left routine
        call    gencal          ! generate call
        ret                     ! exit
!
term09: ld      hl,bshr         ! index shift right routine
        call    gencal          ! generate call
        ret                     ! exit

!
! Parse factor
!
!     Parses and generates code for a factor.
!
!     In parameters: none
!     Out parameters: none
!     Modifies: af
!
factor: push    de
        push    hl
        call    skpspc
        cp      '+'             ! check '+'
        jr      nz,factor01     ! no, skip
        call    getchr          ! skip
        call    factor          ! parse factor
        jp      factor11        ! exit
factor01: cp    '-'             ! check '-'
        jr      nz,factor02
        call    getchr          ! skip
        call    factor          ! get factor
        call    genneg          ! negate
        jp      factor11        ! and exit
factor02: ld    de,notop        ! index 'not'
        ld      hl,(iptr)       ! index input
        call    match           ! check match
        jr      nz,factor05     ! no, skip
factor03: call  chkchr          ! check next
        call    chklcr          ! check terminal
        jr      nz,factor04     ! yes, go
        call    getchr          ! skip
        jr      factor03        ! loop
factor04: call  factor          ! parse factor
        call    geninv          ! generate complement
        jr      factor11        ! and exit
factor05: call  chkchr          ! check next
        cp      '('             ! is it (<expr>) ?
        jr      nz,factor06     ! no
        call    getchr          ! yes, skip character
        call    expr            ! and parse (<expr>)
        call    skpspc          ! skip spaces
        call    getchr          ! chk trailing ')'
        cp      ')'
        ld      a,eparen        ! flag error
        jp      nz,error        ! and err if not
        jr      factor11        ! exit
factor06: cp    '['             ! is it [<expr>] ?
        jr      nz,factor07     ! no
        call    getchr          ! yes, skip character
        call    expr            ! and parse [<expr>]
        call    getchr          ! chk trailing ']'
        cp      ']'
        ld      a,eindr         ! flag error
        jp      nz,error        ! and err if not
        call    geninw          ! generate indirect word load
        jr      factor11        ! exit
factor07: cp    '%'             ! check radixes
        jr      z,factor08      ! yes
        cp      '@'
        jr      z,factor08      ! yes
        cp      '$'
        jr      z,factor08      ! yes
        call    digit           ! check digit
        jr      nz,factor09     ! no
factor08: call  rdnum           ! read numeric
        call    genlod          ! yes, generate literal load
        jr      factor11        ! and exit
factor09: cp    '_'             ! is it a variable/symbol ?
        jr      z,factor090     ! yes
        call    alpha
        ld      a,efact         ! flag error
        jp      nz,error        ! err if not
factor090: call varsch          ! look for a varible
        dec     a               ! chk byte
        jr      z,factor10      ! yes
        dec     a               ! check word
        ld      a,esymf         ! flag error
        jp      nz,error        ! and exit if not
        call    genald          ! word, generate indirect
        jr      factor11        ! address load and exit
factor10: call  genlod          ! else generate address load
        call    geninb          ! generate indirect load byte
factor11: pop   hl              ! clean up and return
        pop     de
        ret

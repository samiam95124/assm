! Command routines
!
!
! Clear memory
!
!      Clears from the bottom of the data area
!      to the bottom of the stack.
!
!      In parameters: none
!      Out parameters: none
!      Modifies: none
!
clearm: push    hl
        ld      hl,datap
clrm01: ld      (hl),0          ! clear a memory location
        push    hl              ! save that address
        sbc     hl,sp           ! and compare with sp
        pop     hl
        inc     hl
        jr      nz,clrm01       ! loop if not equal
        pop     hl              ! else clean up and return
        ret

!
! Display registers routine
!
!      Prints headers and outside registers,
!      then does a single instruction dissassembly
!      at the current pc.
!
!      In parameters: none
!      Out parameters: none
!      Modifies: af
!
!
! print flag based on bit true/false
!
dsreg:  push    bc
        push    de
        push    hl
        ld      a,mrhd          ! print register header
        call    lprtms
        ld      a,(afreg)       ! get flags
        ld      c,a             ! save
        ld      a,'m'           ! print sign flag
        bit     7,c
        call    dsr01
        ld      a,'z'           ! print zero flag
        bit     6,c
        call    dsr01
        ld      a,'h'           ! print half-carry flag
        bit     4,c
        call    dsr01
        ld      a,'e'           ! print parity flag
        bit     2,c
        call    dsr01
        ld      a,'s'           ! print add/subtract flag
        bit     1,c
        call    dsr01
        ld      a,'c'           ! print carry flag
        bit     0,c
        call    dsr01
        call    prtstr          ! print space
        defb    ' ' or $80
        ld      a,(afreg+1)     ! print a
        call    phexs
        ld      a,(bcreg+1)     ! print b
        call    phexs
        ld      a,(bcreg)       ! print c
        call    phexs
        ld      a,(dereg+1)     ! print d
        call    phexs
        ld      a,(dereg)       ! print e
        call    phexs
        ld      a,(hlreg+1)     ! print h
        call    phexs
        ld      a,(hlreg)       ! print l
        call    phexs
        ld      hl,(ixreg)      ! print ix
        call    pwords
        ld      hl,(iyreg)      ! print iy
        call    pwords
        ld      hl,(spreg)      ! print sp
        call    pwords
        ld      a,(ireg)        ! print i
        call    phexs
        ld      a,(afrega)      ! print f'
        call    phexs
        ld      a,(afrega+1)    ! print a'
        call    phexs
        ld      a,(bcrega+1)    ! print b'
        call    phexs
        ld      a,(bcrega)      ! print c'
        call    phexs
        ld      a,(derega+1)    ! print d'
        call    phexs
        ld      a,(derega)      ! print e'
        call    phexs
        ld      a,(hlrega+1)    ! print h'
        call    phexs
        ld      a,(hlrega)      ! print l'
        call    phexs
        ld      hl,(spreg)      ! print (sp)
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        call    pword
        call    crlf            ! next line
        ld      hl,(pcreg)      ! get the pc
        call    disass          ! list instruction at pc
        pop     hl              ! clean up and return
        pop     de
        pop     bc
        ret
        
dsr01:  jr      nz,dsr02        ! flag is true
        ld      a,' '           ! flag is false, replace with space
dsr02:  call    prtchr          ! print
        ret

!
! Remove all active breakpoints
!
!      Lifts all non-free breakpoints in the
!      breakpoint table from external memory.
!
!      In parameters: none
!      Out parameters: none
!      Modifies: af
!
cabrk:  push    bc
        push    de
        push    hl
        ld      a,(brkset)      ! get the breakpoint set byte
        ld      c,a             ! save
        ld      b,8             ! set number of breakpoints
        ld      hl,brktbl       ! index breakpoint table
cabrk01: ld     e,(hl)          ! get address of breakpoint
        inc     hl
        ld      d,(hl)
        inc     hl
        rr      c               ! test breakpoint set
        jr      nc,cabrk02      ! no
        ld      a,(rstno)       ! check breakpoint still set
        ex      de,hl           ! index in hl
        cp      (hl)
        ld      a,ebrkc         ! flag error
        jp      nz,error        ! and err if not
        ld      a,(de)          ! clear breakpoint by
        ld      (hl),a          ! restoring byte under it
        cp      (hl)            ! check properly restored
        ld      a,ebrkc         ! flag error
        jp      nz,error        ! and err if not
        ex      de,hl           ! restore
cabrk02: inc    hl              ! and index next entry
        djnz    cabrk01         ! loop for all breakpoints
        pop     hl              ! clean up and return
        pop     de
        pop     bc
        ret

!
! Set active breakpoints
!
!     Sets all non-free breakpoints in external memory.
!
!     In parameters: none
!     Out parameters: none
!     Modifies: af
!
sabrk:  push    bc
        push    de
        push    hl
        ld      a,(brkset)      ! get breakpoint set byte
        ld      c,a             ! save
        ld      b,8             ! get number of entries to do
        ld      hl,brktbl       ! index breakpoint table
sabk01: ld      e,(hl)          ! get the address of breakpoint
        inc     hl
        ld      d,(hl)
        inc     hl
        rr      c               ! see if set
        jr      nc,sabrk02      ! no
        ex      de,hl           ! index in hl
        ld      a,(hl)          ! save byte under breakpoint
        ld      (de),a
        ld      a,(rstno)       ! get the proper restart
        ld      (hl),a          ! and set it
        cp      (hl)            ! check set properly
        ld      a,ebrkc         ! flag error
        jp      nz,error        ! and err if not
        ex      de,hl           ! restore
sabrk02: inc    hl              ! skip to next entry
        djnz    sabk01          ! loop if not end of table
        pop     hl
        pop     de
        pop     bc
        ret

!
! Match breakpoint addresses
!
!      Checks if address de lies under any breakpoints.
!
!      Returns the following error codes:
!
!      a=0=address hl lies under a breakpoint
!      a=1=address hl does not line under a breakpoint
!
!      In parameters: memory address - hl
!      Out parameters: status code - a
!
mbrkt:  push    bc
        push    de
        push    hl
        ex      de,hl           ! place match address in de
        ld      a,(brkset)      ! get breakpoint set byte
        ld      c,a             ! save
        ld      b,8             ! get number of entries to do
        ld      hl,brktbl       ! index breakpoint table
mbrk01: rr      c               ! check breakpoint set
        jr      nc,mbrk02       ! no
        ld      a,(hl)          ! compare address to entry
        cp      e
        jr      nz,mbrk02       ! not equal
        inc     hl              ! equal, chk upper byte
        ld      a,(hl)
        dec     hl
        cp      d
        jr      z,mbrk03        ! equal
mbrk02: inc     hl              ! not equal, skip to next entry
        inc     hl
        inc     hl
        djnz    mbrk01          ! loop if not end of table
        xor     a               ! else set not-found status
        inc     a
        jr      mbrk04          ! and exit
mbrk03: xor     a               ! set found status
mbrk04: pop     hl              ! clean up and return
        pop     de
        pop     bc
        ret

!
! Set breakpoint
!
!     Sets a breakpoint entry for hl.
!     If breakpoint is already set, will
!     do nothing.
!
!     In parameters: address - hl
!     Out parameters: status code - a
!
sbrkt:  push    bc
        push    de
        push    hl
        ld      a,(brkset)      ! get the breakpoint set byte
        ld      c,a             ! save
        ld      b,8             ! set number of breakpoints
        call    mbrkt           ! check breakpoint set
        jr      z,sbrk04        ! yes, just exit
        ex      de,hl           ! place address in de
        ld      hl,brktbl       ! index breakpoint table
sbrk01: rr      c               ! check this entry set
        jr      nc,sbrk02       ! yes
        inc     hl              ! next entry
        inc     hl
        inc     hl
        djnz    sbrk01          ! loop
        ld      a,ebrkf         ! flag table full error
        jp      error           ! and err
sbrk02: ld      (hl),e          ! place address
        inc     hl
        ld      (hl),d
        ld      a,9             ! set up for bit set
        sub     b
        ld      b,a
        ld      a,$80
sbrk03: rlca                    ! rotate into place
        djnz    sbrk03          ! loop
        ld      c,a             ! save
        ld      a,(brkset)      ! get the breakpoint sets
        or      c               ! set the new breakpoint
        ld      (brkset),a      ! update
sbrk04: pop     hl              ! clean up and return
        pop     de
        pop     bc
        ret

!
! Clear breakpoint
!
!     Clears the breakpoint hl.
!     If the breakpoint is not set, will do nothing.
!
!     In parameters: address - hl
!     Out parameters: none
!     Modifies: af
!
cbrkt:  push    bc
        push    de
        push    hl
        ex      de,hl           ! place address in de
        ld      a,(brkset)      ! get the breakpoint set byte
        ld      c,a             ! save
        ld      b,8             ! set number of breakpoints
        ld      hl,brktbl       ! index breakpoint table
cbrk01: rr      c               ! see if set
        jr      nc,cbrk03       ! no
        ld      a,(hl)          ! yes, compare addresses
        cp      e
        jr      nz,cbrk03       ! no match
        inc     hl
        ld      a,(hl)
        dec     hl
        cp      d
        jr      nz,cbrk03       ! no match
        ld      a,9             ! matches, do bit set
        sub     b
        ld      b,a
        ld      a,$80
cbrk02: rlca                    ! move bit
        djnz    cbrk02          ! loop till in place
        cpl                     ! invert for clear
        ld      c,a             ! save
        ld      a,(brkset)      ! get breakpoint sets
        and     c               ! mask bit
        ld      (brkset),a      ! update
        jr      cbrk04          ! exit
cbrk03: inc     hl              ! skip to next entry
        inc     hl
        inc     hl
        djnz    cbrk01          ! and loop
cbrk04: pop     hl              ! clean up and return
        pop     de
        pop     bc
        ret

!
! Go address
!
!     Executes real instructions at the current pc.
!     If a breakpoint is set at the pc, will single
!     step one instruction to get past it, then
!     execute.
!
!     In parameters: none
!     Out parameters: status code - a
!
goadd:  push    hl
        ld      hl,(pcreg)      ! get the pc
        call    mbrkt           ! check if breakpoint set
        jr      nz,goadd01      ! no, go excute
        call    sstep           ! else single-step one
goadd01: call   sabrk           ! set all breakpoints
        call    exec            ! go execute
        call    cabrk           ! clear all breakpoints
        pop     hl              ! clean up and return
        ret                     ! with error

!
! Print hex word with space
!
!      Expects word to print in hl. Prints the word with
!      a trailing space.
!
!      In parameters: word value - hl
!      Out parameters: none
!      Modifies: af
!
pwords: ld      a,h             ! print the high register
        call    phex
        ld      a,l             ! print the low register
        call    phexs           ! with a trailing space
        ret

!
! Print hex word
!
!      Expects word value n hl. Prints the word value.
!
!      In parameters: word value - hl
!      Out parameters: none
!      Modifies: af
!
pword:  ld      a,h             ! print high register
        call    phex
        ld      a,l             ! print low register
        call    phex
        ret

!
! Print signed hex byte
!
!      Expects 2's complement signed byte value in a.
!      Prints the sign and the value (three positions).
!
!      In parameters: signed byte value - a
!      Out parameters: none
!      Modifies: af
!
pshex:  or      a               ! set the sign flag
        jp      m,psh01         ! negative
        call    prtstr          ! positive, print '+'
        defb    '+' or $80
        jr      psh02
psh01:  call    prtstr          ! negative, print '-'
        defb    '-' or $80
        neg                     ! and adjust the value
psh02:  call    phex            ! print the adjusted value
        ret

!
! Print hex byte with space
!
!      Expects hex byte value in a. Prints the value
!      with a space.
!
!      In parameters: byte value - a
!      Out parameters: none
!      Modifies: af
!
phexs:  call    phex            ! print the value
        call    prtstr          ! print trailing space
        defb    ' ' or $80
        ret

!
! Print hex byte
!
!      Expects hex byte value in a. Prints the value.
!
!      In parameters: byte value - a
!      Out parameters: none
!      Modifies: af
!
phex:   push    af              ! save the value
        rrca                    ! shift upper digit into place
        rrca
        rrca
        rrca
        call    phx01           ! and print it
        pop     af              ! print the lower digit
!
! print the digit in lower half of a
!
phx01:  and     $0f             ! mask digit
        add     a,'0'           ! convert to ascii
        cp      '9'+1           ! chk hex digit
        jr      c,phx02         ! no
        add     a,39            ! yes, convert hex digit
phx02:  call    prtchr          ! and print that
        ret

!
! Input command line
!
!     Does a character-by-character input to the inbuff
!     command buffer. Processes rubouts, ctl-c, lf
!     (single step). Terminates on cr.
!
!     Returns the following error codes:
!
!     a=0=no error or special event
!     a=1=single step command input
!
!     In parameters: none
!     Out parameters: status code - a
!
iline:  push    bc
        push    de
        push    hl
ilin01: call    prtstr          ! print the prompt
        defb    '*', ' ' or $80
        ld      hl,ibuff        ! index begining of buffer
        ld      (iptr),hl       ! reset command pointer
        ld      b,0             ! initalize character count
ilin02: call    inpchr          ! get input character
        ld      c,a             ! save character
        cp      bksp            ! is it rubout ?
        jr      z,ilin03        ! yes
        cp      ctlc            ! check cancel
        jr      z,ilin020       ! yes, restart entry
        cp      lf              ! check single-step
        jr      z,ilin04        ! yes
        xor     cr              ! check line terminate
        jr      z,ilin05        ! yes, exit
        ld      a,c             ! recover character
        cp      ' '             ! check control character
        jr      c,ilin02        ! yes, ignore
        ld      (hl),a          ! put char in ibuff
        ld      a,b             ! chk line overflow
        cp      maxlin-1        ! minus one space for the prompt
        jr      z,ilin02        ! yes, ignore character
        inc     b               ! else increment count
        inc     hl              ! and buffer pointer
        ld      a,c             ! restore the character
        call    prtchr          ! and print it
        jr      ilin02          ! and loop
ilin020: call   crlf            ! terminate line
        jr      ilin01          ! loop
ilin03: ld      a,b             ! chk line count
        or      a               ! for zero (no characters input)
        jr      z,ilin02        ! yes, do nothing
        ld      a,bksp          !else do rubout
        call    prtchr
        dec     b
        dec     hl
        jr      ilin02          ! and loop
ilin04: ld      a,b             ! check on zero
        or      a
        jr      nz,ilin02       ! no, ignore command
        xor     a               ! else flag single-step
        inc     a
ilin05: ld      (hl),0          ! place line termination
        call    crlf            ! next line
        pop     hl              ! clean up and return
        pop     de
        pop     bc
        ret

!
! Search for varible
!
!      Searches from the command pointer position
!      for a word or byte varible identifier,
!      returns the address of the table entry in hl,
!      and it's type in a (byte/word).
!
!      Returns the following error codes:
!
!      a=0=varible not found
!      a=1=byte varible found
!      a=2=word varible found
!
!      In parameters: none
!      Out parameters: varible address - hl, status code - a
!
varsch: push    de
        ld      de,(stable)     ! search for symbol
        call    search
        jr      z,vars01        ! found, exit
        ld      de,vtword       ! search for a word varible
        call    search
        ld      e,(hl)          ! get the varible address
        inc     hl
        ld      d,(hl)
        ex      de,hl
        jr      z,vars01        ! found, exit to status tree
        ld      de,vtbyte       ! next, search byte table
        call    search
        ld      e,(hl)          ! get the varible address
        inc     hl
        ld      d,(hl)
        ex      de,hl
        jr      z,vars02        ! found, exit to status tree
        xor     a               ! set not found status
        jr      vars03          ! and exit
vars01: inc     a               ! status tree
vars02: inc     a
vars03: pop     de              ! clean up and return
        ret

!
! Print byte value/symbol
!
!     Expects a byte value in a. Checks if value
!     is in symbol table, and if it is, prints the symbol.
!     Otherwise, just prints the value.
!
!     In parameters: byte value - a
!     Out parameters: none
!     Modifies: af
!
prtbsm: push    de
        push    hl
        ld      l,a             ! set up value in hl
        ld      h,0
        ld      a,(lstops)      ! get list options
        bit     fbsym,a         ! test byte symbols enabled
        jr      z,prtbsm01      ! no, skip
        ld      de,(stable)     ! index symbol table
        call    schval          ! find the value
        jr      nz,prtbsm01     ! not found
        ex      de,hl
        call    pstr            ! found, print the symbol
        jr      prtbsm02
prtbsm01: ld    a,l             ! print the byte value
        call    phex
prtbsm02: pop   hl              ! clean up and return
        pop     de
        ret

!
! Print signed byte value/symbol
!
!     Expects a signed byte value in a. Checks if value
!     is in symbol table, and if it is, prints the symbol.
!     Otherwise, just prints the value.
!
!     In parameters: signed byte value - a
!     Out parameters: none
!     Modifies: af
!
pshsym: push    de
        push    hl
        ld      l,a             ! set up value
        ld      h,0
        or      a               ! extend sign
        jp      p,pshsym01      ! positive
        ld      h,$ff           ! negative, extend
pshsym01: ld    a,(lstops)      ! get list options
        bit     fbsym,a         ! check symbol enable
        jr      z,pshsym02      ! no, skip
        ld      de,(stable)     ! index symbol table
        call    schval          ! find the value
        jr      nz,pshsym02     ! not found
        ex      de,hl
        call    prtstr          ! print '+'
        defb    '+' or $80
        call    pstr            ! found, print the symbol
        jr      pshsym03        ! and exit
pshsym02: ld    a,l             ! print the signed byte value
        call    pshex
pshsym03: pop   hl              ! clean up and return
        pop     de
        ret

!
! Print value or symbol
!
!     Expects a word value in hl. Checks if value
!     is in the symbol table, and if it is, prints the
!     corresponding symbol, else will just print the
!     value in hex.
!
!     In parameters: word value - hl
!     Out parameters: none
!     Modifies: none
!
prtsym: push    de
        ld      a,(lstops)      ! get the list options
        bit     fwsym,a         ! test word symbols enabled
        jr      z,prtsym01      ! no, skip
        ld      de,(stable)     ! index symbol table
        call    schval          ! see if value is there
        jr      nz,prtsym01     ! no
        ex      de,hl           ! else print the symbol
        call    pstr
        ex      de,hl
        jr      prtsym02        ! and exit
prtsym01: call  pword           ! print the value in hex
prtsym02: pop   de              ! clean up and return
        ret

!
! Print symbol
!
!     Expects a word value in hl. Checks if the value
!     is in the symbol table, and if it is, prints the
!     corresponding symbol followed by a ':',
!     else will do nothing (no printout).
!
!     In parameters: word value - hl
!     Out parameters: none
!     Modifies: af
!
prtnsm: push    de
        ld      a,(lstops)      ! get list options
        bit     fwsym,a         ! check symbols enable
        jr      z,prtnsm01      ! no
        ld      de,(stable)     ! index symbol table
        call    schval          ! see if value is there
        jr      nz,prtnsm01     ! no
        ex      de,hl           ! else print the symbol
        call    pstr
        ex      de,hl
        call    prtstr          ! print ':'
        defb    ':' or $80
prtnsm01: pop   de              ! clean up and return
        ret

!
! Search for value
!
!      Expects table address in de, value to find in hl.
!      Searches table for value, and if found, returns
!      the entry address in de.
!
!      Returns the following error codes:
!
!      a=0=found
!      a=1=not found
!
!      In parameters: match value - hl, table address - de
!      Out parameters: entry address - de, status code - a
!
schval: push    bc
        push    hl
schval01: push  de              ! save starting address
        ld      a,(de)          ! chk end table
        or      a
        jr      z,schval04      ! yes
schval02: ld    a,(de)          ! skip to end of string
        inc     de
        bit     7,a
        jr      z,schval02
        ld      a,(de)          ! get low byte
        cp      l               ! compare
        jr      nz,schval03
        inc     de
        ld      a,(de)          ! get high byte
        dec     de
        xor     h               ! compare
        jr      z,schval05      ! match found, exit
schval03: pop   af              ! not found, purge old de
        inc     de              ! go to next entry
        inc     de
        jr      schval01        ! and try again
schval04: xor   a               ! set not found status
        inc     a
schval05: pop   de              ! clean up and return
        pop     hl
        pop     bc
        ret

!
! Search string - address table
!
!      Expects a string-address table address in de.
!      Searches for a match to the command string at iptr,
!      and returns with the address of after the string in
!      hl. If the string is not found, will return the
!      address of after the end of the table in hl.
!
!      Returns the following error codes:
!
!      a=0=found
!      a=1=not found
!
!      In parameters: table address - de
!      Out parameters: matched string address - hl, status - a
!
search: push    de
        ld      hl,(iptr)       ! get the command pointer
search01: call  match           ! see if strings match
        push    af              ! save matched status
search02: ld    a,(de)          ! no, skip to next entry
        bit     7,a
        inc     de
        jr      z,search02
        pop     af              ! restore matched status
        jr      z,search03      ! matched, go
        inc     de
        inc     de
        ld      a,(de)          ! chk end of table
        or      a
        jr      nz,search01     ! loop if not
        inc     a               ! yes, set not-found status
        jr      search05        ! and exit
search03: ld    a,(hl)          ! skip to end of substring
        call    chklcr
        jr      nz,search04
        inc     hl
        jr      search03
search04: xor   a               ! set found status
        ld      (iptr),hl       ! update the command pointer
search05: ex    de,hl           ! place address in hl
        pop     de              ! clean up and return
        ret

!
! Print the string after call with cr-lf
!
!      prints the string following the call, then cr-lf,
!      and resumes control after the string.
!      String ends with bit 7 high.
!
!      In parameters: string address - first on stack
!      Out parameters: none
!      Modifies: none
!
lprtst: ex      (sp),hl         ! trade hl for string address
        push    af
        call    lpstr           ! print the string with cr-lf
        pop     af
        ex      (sp),hl         ! restore hl, new return address
        ret                     ! exit
!
! Print string after call
!
!      Prints the string following the call, and resumes
!      control after the string. String ends with
!      bit 7 high.
!
!      In parameters: string address - first on stack
!      Out parameters: none
!      Modifies: none
!
prtstr: ex      (sp),hl         ! trade hl for string address
        push    af
        call    pstr            ! print the string
        pop     af
        ex      (sp),hl         ! and restore hl, new return address
        ret

!
! Print message with cr-lf
!
!      Expects a message code in a.
!      prints the a'th message from the message pool,
!      with the first being 0, followed by cr-lf.
!
!      In parameters: message code - a
!      Out parameters: none
!      Modifies: af
!
lprtms: call    prtmsg          ! print message
        call    crlf            ! next line
        ret                     ! exit
!
! Print message
!
!      Expects a message code in a. Prints the a'th
!      message from the message pool, with the first
!      being 0.
!
!      In parameters: message code - a
!      Out parameters: none
!      Modifies: af
!
prtmsg: push    hl
        ld      hl,msgtbl       ! index the message pool
pmsg01: or      a               ! chk message found
        jr      z,pmsg03        ! yes, go print
pmsg02: bit     7,(hl)          ! else skip over message
        inc     hl
        jr      z,pmsg02
        dec     a               ! and count
        jr      pmsg01          ! try again
pmsg03: call    pstr            ! print the message
pmsg04: pop     hl              ! clean up and return
        ret

!
! Print string with cr-lf
!
!     Prints the string at hl, followed by cr-lf.
!     Returns hl pointing after the last character
!     in the string.
!
!     In parameters: string address - hl
!     Out parameters: end of string - hl
!     Modifies: af, hl
!
lpstr:  call    pstr            ! print string
        call    crlf            ! followed by cr-lf
        ret                     ! exit
!
! print string
!
!      prints the string at hl.
!      returns hl pointing after the last character
!      in the string.
!
!      no error possible
!
!      in parameters:string address-hl
!      out parameters:none
!      modifies:af
!
pstr:   ld      a,(hl)          ! get a string character
        and     $7f             ! strip indicator bit
        call    prtchr          ! print it
        bit     7,(hl)          ! chk for end of string
        inc     hl
        jr      z,pstr          ! loop if not end
        ret                     ! else exit with hl on next character

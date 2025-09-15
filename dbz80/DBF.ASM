!
! Code generation utilies section
! here is where we keep all the routines
! to manage the code buffer and generate
! the z80 instructions that execute the
! input commands. Without exception,
! all actuall formulation of code is done
! elsewhere, athough it is possible that some
! Z80 dependant assumptions are made elsewhere.
!

!
! Generate code byte
!
!     Generates a single byte of code from
!     the a register. The value of a is
!     placed at the current codptr position,
!     and codptr is advanced by one.
!
!     In parameters: code byte - a
!     Out parameters: none
!
genbyt: push    de
        push    hl
        ld      hl,(codptr)     ! get the coding pointer
        ld      de,codbuf       ! find the current length
        or      a               ! clear carry for subtract
        sbc     hl,de
        ld      e,a             ! save byte
        ld      a,l             ! get the lower order result
        cp      maxcod          ! check buffer overflow
        ld      a,ecdovf        ! flag error
        jp      nc,error        ! error, process
        ld      hl,(codptr)     ! recover the pointer
        ld      (hl),e          ! else place the code byte
        inc     hl              ! and update the pointer
        ld      (codptr),hl     ! replace
        pop     hl              ! clean up and return
        pop     de
        ret

!
! Generate code word
!
!     Generates the code word hl. The word
!     is placed at the current codptr position
!     in intel l/h format, and codptr advanced
!     by two.
!
!     In parameters: code word - hl
!     Out parameters: none
!
genwrd: ld      a,l             ! generate low byte
        call    genbyt
        ld      a,h             ! generate high byte
        call    genbyt
        ret                     ! and exit

!
! Generate code string
!
!     Generates the code string following
!     the call. the code string must be preceded
!     by a 1-256 length byte (0=256).
!     Control is resumed after the string.
!
!     In parameters: string - on stack
!     Out parameters: none
!
gencod: ex      (sp),hl         ! trade hl for return address
        push    bc
        ld      b,(hl)          ! get the byte count
        inc     hl              ! and skip
gencod01: ld    a,(hl)          ! get the byte
        call    genbyt          ! generate that
        inc     hl              ! next byte
        djnz    gencod01        ! loop for next byte
        pop     bc              ! clean up
        ex      (sp),hl         ! trade return address for hl
        ret                     ! and return after string

!
! Generate unconditional jump
!
!     Generates an unconditional jump to address hl.
!
!     In parameters: jump address-hl
!     Out parameters: none
!
genjmp: ld      a,$c3           ! generate 'jp x'
        call    genbyt
        call    genwrd          ! genrate jump address
        ret                     ! and exit

!
! Generate jump on zero
!
!     Generates a jump on zero to address hl.
!
!     In parameters: jump address - hl
!     Out parameters: status - a
!
genjpz: ld      a,$ca           ! generate 'jp z,x'
        call    genbyt
        call    genwrd          ! generate jump address
        ret                     ! and exit

!
! Generate jump on not-zero
!
!     Generates a jump on not-zero to address hl.
!
!     In parameters: jump address - hl
!     Out parameters: none
!
genjpnz: ld     a,$c2           ! generate 'jp nz,x'
        call    genbyt
        call    genwrd          ! generate jump address
        ret                     ! and exit

!
! Generate hl save
!
!     Generates a save of hl on the stack.
!
!     In parameters: none
!     Out parameters: none
!
gensav: call    gencod          ! generate code
        defb    gensav01-_-1
        push    hl              ! save hl on stack
gensav01: ret                   ! and exit

!
! Generate hl restore
!
!      Generates a restore of hl off the stack.
!
!      In parameters: none
!      Out parmeters: none
!
genrst: call    gencod          ! generate code
        defb    genrst01-_-1
        pop     hl              ! restore hl from stack
genrst01: ret                   ! exit

!
! Generate exchange
!
!     Generates the exchange de,hl code
!
!     In parameters: none
!     Out parameters: status - a
!
genexc: call    gencod          ! generate code
        defb    genexc01-_-1
        ex      de,hl           ! exchange de with hl
genexc01: ret                   ! exit

!
! Generate copy
!
!     Generates an hl to de copy.
!
!     In parameters: none
!     Out parameters: none
!
gencpy: call    gencod          ! generate code
        defb    gencpy01-_-1
        ld      d,h             ! move low byte
        ld      e,l             ! move high byte
gencpy01: ret                   ! exit

!
! Generate call
!
!     Generates a call to address hl.
!
!     In parameters: call address - hl
!     Out parameters: none
!
gencal: ld      a,$cd           ! generate a 'call'
        call    genbyt
        call    genwrd          ! generate call address
        ret                     ! exit

!
! Generate return
!
!     Genrates a return instruction.
!
!     In parameters: none
!     Out parameters: none
!
genret: call    gencod          ! generate code
        defb    genret01-_-1
        ret                     ! return
genret01: ret                   ! exit

!
! Generate function call
!
!     Generates a call to the address on the stack
!     (placed by 'call'ing this routine). Does
!     not return to caller, but simply exits to 'next'.
!
!     In parameters: none
!     Out parameters: none
!     Modifies: hl
!
genfnc: pop     hl              ! get the call address
        call    gencal          ! generate the call
        jp      next            ! and exit

!
! Generate literal load
!
!     Generates a literal load to hl, from
!     the value passed in hl, i.e., hl is
!     placed in-line as a literal.
!
!     In parameters: literal value - hl
!     out parameters:status code-a
!
genlod: ld      a,$21           ! generate 'ld hl,x'
        call    genbyt
        call    genwrd          ! generate the value
        ret                     ! and exit

!
! Generate indirect address load
!
!     Generates an indirect load to hl by the
!     address hl.
!
!     In parameters: load address - hl
!     Out parameters: none
!
genald: ld      a,$2a           ! generate 'ld hl,(x)'
        call    genbyt
        call    genwrd          ! generate literal address
        ret                     ! exit

!
! Generate indirect address store
!
!     Generates a store of hl to the address hl.
!
!     In parameters: store address - hl
!     Out parameters: none
!
genast: ld      a,$22           ! generate 'ld (x),hl'
        call    genbyt
        call    genwrd          ! generate store address
        ret                     ! exit

!
! Genrate indirect word load
!
!     Generates an indirect word load to hl,
!     i.e., hl = (hl). The a register is used
!     to do this.
!
!     In parameters: none
!     Out parameters: none
!
geninw: call    gencod          ! generate code
        defb    geninw01-_-1
        ld      a,(hl)          ! get low byte
        inc     hl
        ld      h,(hl)          ! get high byte
        ld      l,a             ! place low byte
geninw01: ret                   ! exit

!
! Generate indirect load byte
!
!     Generates an indirect load byte to hl,
!     with the high byte cleared, i.e.,
!     l = (hl), h = 0.
!
!     In parameters: none
!     Out parameters: none
!
geninb: call    gencod          ! generate code
        defb    geninb01-_-1
        ld      l,(hl)          ! get byte
        ld      h,0             ! clear upper byte
geninb01: ret                   ! exit

!
! Generate indirect store byte
!
!     Generates a store of e at (hl).
!
!     In parameters: none
!     Out parameters: none
!
genstb: call    gencod          ! generate code
        defb    genstb01-_-1
        ld      (hl),e          ! store byte
genstb01: ret                   ! exit

!
! Generate indirect store of word
!
!     Generates a store of de at (hl).
!
!     In parameters: none
!     Out parameters: none
!
genstw: call    gencod          ! generate code
        defb    genstw01-_-1
        ld      (hl),e          ! place low byte
        inc     hl              ! next
        ld      (hl),d          ! place high byte
genstw01: ret                   ! exit

!
! Generate word test
!
!     Generates a word of hl, i.e., if hl is zero,
!     the z flag is set, otherwise reset.
!
!     In parameters: none
!     Out parameters: none
!
gentst: call    gencod          ! generate code
        defb    gentst01-_-1
        ld      a,l             ! 'or' bytes together
        or      h
gentst01: ret                   ! exit

!
! Generate and
!
!     Generates an 'and' of hl and de, i.e.,
!     hl = hl.and.de
!
!     In parameters: none
!     Out parameters: none
!
genand: call    gencod          ! generate code
        defb    genand01-_-1
        ld      a,h             ! 'and' high bytes
        and     d
        ld      h,a
        ld      a,l             ! 'and' low bytes
        and     e
        ld      l,a
genand01: ret                   ! exit

!
! Generate or
!
!     Generates an 'or' of hl and de, i.e.,
!     hl = hl.or.de
!
!     In parameters: none
!     Out parameters: none
!
genor:  call    gencod          ! generate code
        defb    genor01-_-1
        ld      a,h             ! 'or' high bytes
        or      d
        ld      h,a
        ld      a,l             ! 'or' low bytes
        or      e
        ld      l,a
genor01: ret                    ! exit

!
! Generate xor
!
!     Generates an 'xor' of hl and de, i.e.,
!     hl = hl.xor.de
!
!     In parameters: none
!     Out parameters: none
!
genxor: call    gencod          ! generate code
        defb    genxor01-_-1
        ld      a,h             ! 'xor' high bytes
        xor     d
        ld      h,a
        ld      a,l             ! 'xor' low bytes
        xor     e
        ld      l,a
genxor01: ret                   ! exit

!
! Generate '=' comparision
!
!     Generates an '=' comparison between hl and de,
!     i.e., hl = hl = de. Note true = $ffff, false = 0
!
!     In parameters: none
!     Out parameters: none
!
genequ: call    gencod          ! generate code
        defb    genequ01-_-1
        or      a               ! clear carry
        sbc     hl,de           ! compare hl, de
        ld      hl,false        ! get value false
        jr      nz,genequ01     ! not equal
        dec     hl              ! equal, convert to true
genequ01: ret                   ! exit

!
! Generate '>' comparision
!
!     Generates a '>' comparision between hl and de,
!     i.e., hl = hl > de. Note true = $ffff, false = 0.
!
!     In parameters: none
!     Out parameters: none
!
gengtn: call    gencod          ! generate code
        defb    gengtn01-_-1
        ex      de,hl           ! swap
        or      a               ! clear carry
        sbc     hl,de           ! compare hl,de
        ld      hl,true         ! get value true
        jr      c,gengtn01      ! less than
        inc     hl              ! greater than, conv to true
gengtn01: ret                   ! exit

!
! Generate '<' comparision
!
!     Generates a '<' comparision between hl and de,
!     i.e., hl = hl < de. Note true = $ffff, false = 0.
!
!     In parmeters: none
!     Out parameters: none
!
genltn: call    gencod          ! generate code
        defb    genltn01-_-1
        or      a               ! clear carry
        sbc     hl,de           ! compare hl,de
        ld      hl,true         ! get value true
        jr      c,genltn01      ! greater than
        inc     hl              ! less than, convert to false
genltn01: ret                   ! exit

!
! Generate '>=' comparision
!
!     Generates a '>=' comparision between hl and de,
!     i.e., hl = hl >= de. Note true = $ffff, false = 0.
!
!     In parameters: none
!     Out parameters: none
!
gengeq: call    gencod          ! generate code
        defb    gengeq01-_-1
        or      a               ! clear carry
        sbc     hl,de           ! compare hl,de
        ld      hl,false        ! get value false
        jr      c,gengeq01      ! less than
        dec     hl              ! greater than or equal to
gengeq01: ret                   ! exit

!
! Generate '<=' comparision
!
!     Generates a '<=' comparision between hl and de,
!     i.e., hl = hl <= de. Note true = $ffff, false = 0.
!
!     In parameters: none
!     Out parameters: none
!
genleq: call    gencod          ! generate code
        defb    genleq01-_-1
        ex      de,hl           ! swap
        or      a               ! clear carry
        sbc     hl,de           ! compare hl,de
        ld      hl,false        ! get value false
        jr      c,genleq01      ! greater than
        dec     hl              ! less than or equal to, true
genleq01: ret                   ! exit

!
! Generate '<>' comparision
!
!     Generates a '<>' comparsion between hl and de,
!     i.e., hl = hl <> de. Note true = $ffff, false = 0.
!
!     In parameters: none
!     Out parameters: none
!
genneq: call    gencod          ! generate code
        defb    genneq01-_-1
        or      a               ! clear carry
        sbc     hl,de           ! compare hl,de
        ld      hl,false        ! get value false
        jr      z,genneq01      ! equal
        dec     hl              ! not equal, convert to true
genneq01: ret                   ! exit

!
! Generate add
!
!     Generates a add hl to de, i.e., hl = hl + de.
!
!     In parameters: none
!     Out parameters: none
!
genadd: call    gencod          ! generate code
        defb    genadd01-_-1
        add     hl,de           ! do 'add'
genadd01: ret                   ! exit

!
! Generate subtract
!
!     Generates a subtract of de from hl, i.e., hl = hl - de.
!
!     In parameters: none
!     Out parameters: none
!
gensub: call    gencod          ! generate code
        defb    gensub01-_-1
        or      a               ! clear carry
        sbc     hl,de           ! do 'sub'
gensub01: ret                   ! exit

!
! Generate load to a
!
!     Generates a load to the a register from
!     location hl.
!
!     In parameters: load address - hl
!     Out parameters: none
!
genlda: ld      a,$3a           ! generate 'ld a,(x)'
        call    genbyt
        call    genwrd          ! generate load address
        ret                     ! exit

!
! Generate store of a
!
!     Generates a store of a to the location hl.
!
!     In parameters: store address - hl
!     Out parameters: none
!
gensta: ld      a,$32           ! generate 'ld (x),a'
        call    genbyt
        call    genwrd          ! generate store address
        ret                     ! exit

!
! Generate immediate load a
!
!     Generates a literal load to a.
!     Literal in a.
!
!     In parameters: literal - a
!     Out parmaeters: status code - a
!
genldi: push    bc
        ld      b,a             ! save literal
        ld      a,$3e           ! generate 'ld a,x'
        call    genbyt
        ld      a,b             ! restore the literal
        call    genbyt          ! generate literal
        pop     bc              ! clean up and return
        ret                     ! with

!
! Generate a 'and' immediate
!
!     Generates a 'and' with literal a.
!
!     In parameters: literal - a
!     Out parameters: none
!
genani: push    bc
        ld      b,a             ! save literal
        ld      a,$e6           ! generate 'and x'
        call    genbyt
        ld      a,b             ! get the literal
        call    genbyt          ! generate
        pop     bc              ! clean up and return
        ret                     ! with error

!
! Generate 'or' immediate
!
!     Generates an 'or' with the literal a.
!
!     In parameters: literal - a
!     Out parameters: none
!
genori: push    bc
        ld      b,a             ! save literal
        ld      a,$f6           ! generate 'or x'
        call    genbyt
        ld      a,b             ! get the literal
        call    genbyt          ! generate
        pop     bc              ! clean up and return
        ret                     ! with error

!
! Generate negate
!
!     Generates a negation of hl.
!
!     In parameters: none
!     Out parameters: none
!
genneg: call    gencod          ! generate code
        defb    genneg01-_-1
        ld      a,l             ! complement hl
        cpl
        ld      l,a
        ld      a,h
        cpl
        ld      h,a
        inc     hl              ! and increment for 2's comp
genneg01: ret                   ! exit

!
! Generate complement
!
!     Generates the complement of hl.
!
!     In parameters: none
!     Out parameters: none
!
geninv: call    gencod          ! generate code
        defb    geninv01-_-1
        ld      a,l             ! complement hl
        cpl
        ld      l,a
        ld      a,h
        cpl
        ld      h,a
geninv01: ret                   ! exit

!
! Generate increment
!
!     Generates the increment of hl.
!
!     In parameters: none
!     Out parameters: none
!
geninc: call    gencod          ! generate code
        defb    geninc01-_-1
        inc     hl              ! increment
geninc01: ret                   ! exit

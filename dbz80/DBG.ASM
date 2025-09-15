!
! Processor control.
! Here are the routines used to simulate the
! z80 code, and the interface between zbug and
! the target program.
! This pacage is self-contained, and has the following
! external entries defined:
!
!     exec-restores all registers, including the pc,
!     resulting in real-time execution at the current pc.
!
!     sstep-single-steps (simulates) an instruction
!     at the current pc.
!
!     lenins-detemine instruction length.
!

!
! Execute real instructions
!
!      Creates a jump vector in the insbuf
!      and executes it.
!
!      In parameters: none
!      Out parameters: none
!      Modifies: af, bc', de', hl', ix, iy
!
exec:   push    bc
        push    de
        push    hl
        ld      a,true          ! set external execute true
        ld      (extflg),a
        ld      a,$c3           ! place jump vector
        ld      (insbuf),a      ! place
        ld      hl,(pcreg)      ! get the current pc
        ld      (insbuf+1),hl   ! place
        call    goinst          ! execute the vector
        pop     hl              ! clean up and return
        pop     de
        pop     bc
        ret

!
! Single step
!
!      Single-steps the instruction at the pc.
!      The result of this routine is the same
!      as if a breakpoint were set directly
!      after the current instruction.
!
!      In parameters: none
!      Out parameters: none
!      Modifies: bc', de', hl', ix, iy
!
sstep:  push    bc
        push    de
        push    hl
        ld      a,true          ! set external execute true
        ld      (extflg),a
        ld      hl,(pcreg)      ! get the current pc
        call    lenins          ! get instruction length
        jp      nz,error        ! error
        push    de              ! save length codes
        ld      c,e             ! set up length
        ld      b,0
        ld      de,insbuf       ! index instruction buffer
        ldir                    ! move instruction to buffer
        ld      (pcreg),hl      ! update pc
        pop     bc              ! recover codes
        ld      a,4             ! find pad length
        sub     c
        ex      de,hl           ! point to buffer
sstep01: jr     z,sstep02       ! done, skip
        ld      (hl),0          ! place a pad ('nop')
        inc     hl              ! next
        dec     a               ! count pads
        jr      nz,sstep01      ! not done, loop
sstep02: ld     a,$c3           ! place jump vector
        ld      (ucvec),a
        ld      hl,recov        ! to standard recovery
        ld      (ucvec+1),hl
        ld      l,b             ! else set up function code
        ld      h,0
        add     hl,hl           ! * 2 for word table
        ld      de,spctbl       ! index special functions
        add     hl,de           ! offset into them
        ld      e,(hl)          ! get function handler addr
        inc     hl
        ld      d,(hl)
        ex      de,hl           ! into hl
        ld      de,sstep03      ! index return point
        push    de              ! save on stack
        jp      (hl)            ! go function
sstep03: pop    hl              ! clean up and return
        pop     de
        pop     bc
        ret

!
! These are the special function routines.
! Here we process control tranfer instructions
! in order to keep control of the target program.
! Note that there is no specific requirement
! to actually execute the instruction, so some
! of these routines simply simulate the code
! on the spot.
!
        
! Jump hl
        
jphl:   ld      hl,(hlreg)      ! get the hl register
        ld      (pcreg),hl      ! set up as pc
        ret                     ! and exit
        
! Jump ix
        
jpix:   ld      hl,(ixreg)      ! get the ix register
        ld      (pcreg),hl      ! set up as pc
        ret                     ! and exit
        
! Jump iy
        
jpiy:   ld      hl,(iyreg)      ! get the iy register
        ld      (pcreg),hl      ! set up as pc
        ret                     ! and exit
        
! Jump address
        
jump:   ld      hl,(insbuf+1)   ! get the address
        ld      (condpc),hl     ! set up as conditional pc
        ld      hl,recovc       ! get address of recovery
        ld      (insbuf+1),hl   ! set as jump address
        jp      goinst          ! execute code
        
! Call address
        
calla:  ld      a,(insbuf)      ! get the opcode
        set     1,a             ! change 'call' to 'jp
        res     2,a             ! with the same condition
        bit     0,a             ! test unconditional
        jr      z,calla01       ! no
        res     3,a             ! yes, adjust
calla01: ld     (insbuf),a      ! place 'jp' code
        ld      hl,(insbuf+1)   ! get the address
        ld      (condpc),hl     ! set up as conditional pc
        ld      hl,recovs       ! get address of recovery
        ld      (insbuf+1),hl   ! set as jump address
        jp      goinst          ! execute code

! Return
        
retrn:  ld      a,(insbuf)      ! get the opcode
        set     1,a             ! change 'ret' to 'jp'
        bit     0,a             ! of the same condition
        jr      z,retrn01       ! not conditional
        res     3,a             ! conditional, adjust
retrn01: ld     (insbuf),a      ! place 'jp' code
        ld      hl,recovr       ! get address of recovery
        ld      (insbuf+1),hl   ! place
        jp      goinst          ! execute code
        
! Unconditional return
        
retun:  ld      hl,(spreg)      ! get the stack pair
        ld      e,(hl)          ! get the stack address
        inc     hl              ! to simulate 'pop'
        ld      d,(hl)
        inc     hl
        ld      (spreg),hl      ! update sp
        ld      (pcreg),de      ! and pc
        ret                     ! exit
        
! Restart
        
rstart: ld      de,(pcreg)      ! get the current pc
        ld      hl,(spreg)      ! get the sp
        dec     hl              ! simulate a 'push' of pc
        ld      (hl),d
        dec     hl
        ld      (hl),e
        ld      (spreg),hl      ! update sp
        ld      a,(insbuf)      ! get the opcode
        and     $38             ! mask 'rst' bits
        ld      l,a             ! set up
        ld      h,0
        ld      (pcreg),hl      ! set new pc
        ret                     ! and exit

        
! Jump relative
        
jpr:    ld      a,(insbuf+1)    ! get the displacement
        ld      c,a             ! set up
        ld      b,0
        or      a               ! check sign
        jp      p,jpr01         ! positive, skip
        ld      b,$ff           ! else do sign extend
jpr01:  ld      hl,(pcreg)      ! get the current pc
        add     hl,bc           ! offset by displacement
        ld      (condpc),hl     ! set as conditional pc
        ld      a,5             ! get vector jp displacement
        ld      (insbuf+1),a    ! place
        ld      a,$c3           ! place the linkage vector
        ld      (cvec),a        ! place
        ld      hl,recovc       ! get recovery address
        ld      (cvec+1),hl     ! place
        jp      goinst          ! go execute
        
! Halt
        
hltrp:  ld      a,(trops)       ! get the trace options
        bit     fhlt,a          ! check trap halt
        jp      z,goinst        ! no, go execute
        ld      hl,(pcreg)      ! back the pc up over halt
        dec     hl
        ld      (pcreg),hl
        ld      a,ehlt          ! flag error
        jp      error           ! go error
        
! Repeat instruction
        
rept:   ld      a,(trops)       ! get the trace options
        bit     frd,a           ! test expand repeat
        jp      z,goinst        ! no, just execute
        ld      a,(insbuf+1)    ! else get opcode
        res     4,a             ! set repeat = false
        ld      (insbuf+1),a    ! replace
        call    goinst          ! execute that instruction
        ld      a,(bcreg+1)     ! get the b register
        or      a               ! test zero
        jr      nz,rept01       ! no, go repeat
        ld      a,(insbuf+1)    ! get the instruction again
        bit     1,a             ! test byte/word count
        ld      a,0             ! set no error
        ret     nz              ! byte, exit
        ld      a,(bcreg)       ! get the c register
        or      a               ! test zero
        ret     z               ! yes, just execute
rept01: ld       hl,(pcreg)     ! get the pc
        dec     hl              ! back up to instruction
        dec     hl              ! start to simulate
        ld      (pcreg),hl      ! the repeat flag
        ret                     ! and exit

!
! Go instruction
!
!     Transfers control from zbug to the target
!     program instruction in insbuf, by restoring
!     all outside registers and jumping to insbuf.
!     All possible re-entries are also handled here.
!     distroies all zbug registers.
!
!     In parmeters: none
!     Out parmeters: none
!     Modifies: all
!
goinst: ld      (spsave),sp     ! save zbug stack
        ld      a,(ireg)        ! load all outside registers
        ld      i,a
        ld      hl,(afrega)     ! alternates first
        push    hl
        pop     af
        ex      af,af'
        ld      hl,(hlrega)
        ld      de,(derega)
        ld      bc,(bcrega)
        exx
        ld      hl,(afreg)      ! and normal registers
        push    hl
        pop     af
        ld      hl,(hlreg)
        ld      de,(dereg)
        ld      bc,(bcreg)
        ld      iy,(iyreg)
        ld      ix,(ixreg)
        ld      sp,(spreg)
        jp      insbuf          ! execute pseudo instruction
        
! The pseudo instruction at insbuf is executed,
! now there are several re-entries possible.
        
! For the breakpoint re-entry, we must store all
! registers including the pc, which can be found
! on the stack.
        
recovb: ld      (hlreg),hl      ! save hl
        pop     hl              ! get pc
        dec     hl              ! back up over breakpoint
        ld      (pcreg),hl      ! set pc
        ld      hl,(hlreg)      ! restore hl
        jr      recov           ! go recover

! For the call recovery, we push the pc
! onto the stack and do a jump
        
recovs: ld      (hlreg),hl      ! save hl
        ld      hl,(pcreg)      ! get the pc
        push    hl              ! place on stack
        ld      hl,(condpc)     ! get the next pc
        ld      (pcreg),hl      ! set up
        ld      hl,(hlreg)      ! restore hl
        jr      recov           ! go recover
        
! For the return recovery, we pop an address off the
! the stack and take that as our next pc.
        
recovr: ld      (hlreg),hl      ! save hl
        pop     hl              ! get return address off stack
        ld      (pcreg),hl      ! set up as address
        ld      hl,(hlreg)      ! recover hl
        jr      recov           ! and go recover
        
! For our conditional recovery, we replace the next pc
! with the conditional pc.
        
recovc: ld      (hlreg),hl      ! save hl
        ld      hl,(condpc)     ! get the conditional pc
        ld      (pcreg),hl      ! and set up as new pc
        ld      hl,(hlreg)      ! restore hl
        
! For unconditional single-step re-entry
! we just save all external registers
! and restore internal registers.
        
recov:  ld      (spreg),sp
        ld      (iyreg),iy
        ld      (ixreg),ix
        ld      (hlreg),hl
        ld      (dereg),de
        ld      (bcreg),bc
        ld      sp,(spsave)     ! restore internal stack
        push    af
        pop     hl
        ld      (afreg),hl
        ld      a,i
        ld      (ireg),a
        ex      af,af' ! now do the alternates
        push    af
        pop     hl
        ld      (afrega),hl
        exx
        ld      (hlrega),hl
        ld      (derega),de
        ld      (bcrega),bc
        ret                     ! exit

!
! Determine instruction length
!
!     Determines the instruction length
!     via a table and some on the spot coding.
!     Returns the length in e, and returns
!     a special function code in d.
!     This is used by 'sstep', and may be ignored
!     by other users.
!     Returns nz if instruction is invalid.
!
!     In parmeters: address - hl
!     Out parameters: instruction length - e, special function
!                    code - d, status - f
!
lenins: push    bc
        push    hl
        ld      a,(hl)          ! get instruction byte
        inc     hl              ! next byte
        ld      c,a             ! save
        call    getlen          ! get length byte
        ld      b,a             ! save
        and     $03             ! mask length
        ld      e,a             ! save
        ld      a,b
        rlca                    ! move down function code
        rlca
        rlca
        and     $07             ! mask
        ld      d,a             ! save
        ld      a,c             ! check extentions
        cp      $ed             ! z80 extended ops
        jr      nz,lenins04     ! no
        ld      a,(hl)          ! yes, get 2nd opcode
        cp      $45             ! check 'reti'
        jr      z,lenins01      ! yes
        cp      $4d             ! check 'retn'
        jr      nz,lenins02     ! no
lenins01: ld    d,10            ! yes, set special function
        jr      lenins10        ! code and exit
lenins02: ld    c,a             ! save code
        call    getlen          ! check valid instruction
        bit     4,a
        jr      nz,lenins12     ! no, error
        ld      a,c             ! get code
        and     $c7             ! check word parameter
        cp      $43             ! like 'ld de,(x)'
        jr      nz,lenins03     ! no
        inc     e               ! yes, add word to count
        inc     e
        jr      lenins10        ! and exit
lenins03: ld    a,c             ! check repeat codes
        and     $f0
        cp      $b0
        jr      nz,lenins10     ! no, exit no error
        ld      d,11            ! yes, set handler code
        jr      lenins10        ! and exit
lenins04: cp    $dd             ! check ix extention
        jr      nz,lenins05     ! no
        ld      a,(hl)          ! yes, get 2nd code byte
        inc     hl              ! next
        cp      $e9             ! check 'jp (ix)'
        jr      nz,lenins06     ! no
        ld      d,9             ! set function code
        jr      lenins06        ! and continue
lenins05: cp    $fd             ! check iy extention
        jr      nz,lenins08     ! no
        ld      a,(hl)          ! yes, get 2nd code byte
        inc     hl              ! next
        cp      $e9             ! check 'jp (iy)'
        jr      nz,lenins06     ! no
        ld      d,8             ! set function code
lenins06: ld    c,a             ! save code byte
        call    getlen          ! get length code
        bit     3,a             ! test extendable code
        jr      nz,lenins11     ! exit if not
        bit     2,a             ! test displacement
        jr      z,lenins07      ! no
        inc     e               ! yes, add displacement
lenins07: and   $03             ! mask length
        add     a,e             ! prefix+displacement+opcode
        ld      e,a             ! place
        ld      a,c             ! get opcode again
        cp      $cb             ! check bit/rotate extention
        jr      nz,lenins10     ! exit no error if not
        inc     hl              ! skip displacement
        ld      a,(hl)          ! get code byte
        ld      c,a             ! save
        and     $07             ! test '(hl)' code byte
        cp      $06
        jr      nz,lenins12     ! no, error
        ld      a,c             ! get the code byte
        jr      lenins09        ! go check
lenins08: cp    $cb             ! check bit/rotate extention
        jr      nz,lenins10     ! no, exit (code is 8080)
        ld      a,(hl)          ! validate code
lenins09: and   $f8             ! mask
        cp      $30             ! check
        jr      z,lenins12      ! and exit if not
lenins10: xor   a               ! set no error
        jr      lenins13        ! and exit
lenins11: inc   e               ! conpensate illegal exts
lenins12: ld    a,eicd          ! flag error
        or      a               ! set error flags
lenins13: pop   hl              ! clean up and return
        pop     bc
        ret

!
! Get opcode length
!
!     Performs a table lookup for the opcode
!     a in the opcode length table.
!     Note that further processing is required
!     to determine actual opcode length.
!
!     In parameters: opcode - a
!     Out parameters: length code - a
!
getlen: push    de
        push    hl
        ld      hl,cdlent       ! index code length table
        ld      e,a             ! set up opcode
        ld      d,0
        add     hl,de           ! offset into table
        ld      a,(hl)          ! get length byte
        pop     hl              ! clean up and return
        pop     de
        ret

!
! Special code handlers table
!
spctbl: defw    goinst          ! no special handler
        defw    jump            ! jump address
        defw    calla           ! call address
        defw    retrn           ! return
        defw    rstart          ! restart
        defw    hltrp           ! halt
        defw    jpr             ! jump relative
        defw    jphl            ! jump hl
        defw    jpiy            ! jump iy
        defw    jpix            ! jump ix
        defw    retun           ! unconditional return
        defw    rept            ! block codes

!
! Opcode length table.
! This 256 byte table contians encoded length/
! special function information that the processor
! control package uses to perform its function.
! The following bit meanings apply to each byte:
!
!     bits 7-5: special function code
!     bit    4: invalid extended code instructions
!     bit    3: invalid base code instructions
!     bit    2: displacement extention
!     bits 1-0: opcode length (1-3)
!
cdlent: defb    $19,$1b,$19,$19  ! 00-03
        defb    $19,$19,$1a,$19  ! 04-07
        defb    $19,$11,$19,$19  ! 08-0b
        defb    $19,$19,$1a,$19  ! 0c-0f
        defb    $da,$1b,$19,$19  ! 10-13
        defb    $19,$19,$1a,$19  ! 14-17
        defb    $da,$11,$19,$19  ! 18-1b
        defb    $19,$19,$1a,$19  ! 1c-1f
        defb    $da,$13,$13,$11  ! 20-23
        defb    $19,$19,$1a,$19  ! 24-27
        defb    $da,$11,$13,$11  ! 28-2b
        defb    $19,$19,$1a,$19  ! 2c-2f
        defb    $da,$1b,$1b,$19  ! 30-33
        defb    $15,$15,$16,$19  ! 34-37
        defb    $da,$11,$1b,$19  ! 38-3b
        defb    $19,$19,$1a,$19  ! 3c-3f
        defb    $09,$09,$09,$09  ! 40-43
        defb    $09,$09,$05,$09  ! 44-47
        defb    $09,$09,$09,$09  ! 48-4b
        defb    $19,$09,$15,$09  ! 4b-4f
        defb    $09,$09,$09,$09  ! 50-53
        defb    $19,$19,$05,$09  ! 54-57
        defb    $09,$09,$09,$09  ! 58-5b
        defb    $19,$19,$05,$09  ! 5c-5f
        defb    $09,$09,$09,$19  ! 60-63
        defb    $19,$19,$15,$09  ! 64-67
        defb    $09,$09,$09,$19  ! 68-6b
        defb    $19,$19,$15,$09  ! 6c-6f
        defb    $15,$15,$05,$05  ! 70-73
        defb    $15,$15,$b9,$15  ! 74-77
        defb    $09,$09,$09,$09  ! 78-7b
        defb    $19,$19,$15,$19  ! 7c-7f
        defb    $19,$19,$19,$19  ! 80-83
        defb    $19,$19,$15,$19  ! 84-87
        defb    $19,$19,$19,$19  ! 88-8b
        defb    $19,$19,$15,$19  ! 8c-8f
        defb    $19,$19,$19,$19  ! 90-93
        defb    $19,$19,$15,$19  ! 94-97
        defb    $19,$19,$19,$19  ! 98-9b
        defb    $19,$19,$15,$19  ! 9c-9f
        defb    $09,$09,$09,$09  ! a0-a3
        defb    $19,$19,$15,$19  ! a4-a7
        defb    $09,$09,$09,$09  ! a8-ab
        defb    $19,$19,$15,$19  ! ac-af
        defb    $09,$09,$09,$09  ! b0-b3
        defb    $19,$19,$15,$19  ! b4-b7
        defb    $09,$09,$09,$09  ! b8-bb
        defb    $19,$19,$15,$19  ! bc-bf
        defb    $79,$19,$3b,$3b  ! c0-c3
        defb    $5b,$19,$1a,$99  ! c4-c7
        defb    $79,$79,$3b,$16  ! c8-cb
        defb    $5b,$5b,$1a,$99  ! cc-cf
        defb    $79,$19,$3b,$1a  ! d0-d3
        defb    $5b,$19,$1a,$99  ! d4-d7
        defb    $79,$19,$3b,$1a  ! d8-defb
        defb    $5b,$19,$1a,$99  ! dc-df
        defb    $79,$11,$3b,$11  ! e0-e3
        defb    $5b,$11,$1a,$99  ! e4-e7
        defb    $19,$f1,$3b,$19  ! e8-eb
        defb    $5b,$1a,$1a,$99  ! ec-ef
        defb    $79,$19,$3b,$19  ! f0-f3
        defb    $1a,$19,$1a,$99  ! f4-f7
        defb    $79,$11,$3b,$19  ! f8-fb
        defb    $5b,$19,$1a,$99  ! fc-ff

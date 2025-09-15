!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                              !
!     DB Z80 DEBUGGER                                          !
!                                                              !
!     3/21/80 S. A. Moore                                      !
!                                                              !
!     This grandaddy program originates from my work at        !
!     spacelabs, inc., and has proven to be my right-hand      !
!     man for debugging scads of other programs.               !
!     It is a testimony to what good structure and             !
!     documenting habits can accomplish. It's successor        !
!     will probabally be written in pascal, and use this       !
!     program as a construction guide.                         !
!     This is one program that seems to have given back        !
!     more than the effort i ever put in to it.                !
!                                                              !
!     Version 1.0 3/80 on zilog's rio os                       !
!                                                              !
!     Version 1.0 4/80 on spacelabs 1400 terminal              !
!                                                              !
!     Version 1.3 7/80 on micropolis mdos                      !
!                                                              !
!     Version 1.3 6/81 on rsys os                              !
!                                                              !
!     Version 1.3 6/81 on cardkey d2000 terminal               !
!                                                              !
!     Version 2.1 2/84 on our system prom                      !
!                                                              !
!     Version 2.2 4/85 on CP/M                                 !
!                                                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Equates

false:  equ     0               ! value of false
true:   equ     1               ! not false ! value of true
cr:     equ     $0d             ! carriage return
lf:     equ     $0a             ! line feed
bksp:   equ     $08             ! backspace (rubout)
ctls:   equ     'S'-64          ! stop (ctl-s)
ctlc:   equ     'C'-64          ! cancel (ctl-c)
fhlt:   equ     0               ! halt trap flag
frd:    equ     1               ! repeat display flag
fpatt:  equ     0               ! printer attach switch
fwsym:  equ     1               ! word list flag
fbsym:  equ     2               ! byte list flag

! Errors/messages
        
bsreg:  equ     0               ! disassembler messages
bexreg: equ     8
bdr:    equ     11
bopr:   equ     19
bccode: equ     27
brx:    equ     35
bbt:    equ     41
mrhd:   equ     45              ! register display header
eparm:  equ     46              ! missing parameter
eparen: equ     47              ! missing ')'
eindr:  equ     48              ! missing ']'
eterm:  equ     49              ! invalid command termination
ebrkf:  equ     50              ! breakpoint table full
ecom:   equ     51              ! command not found
erange: equ     52              ! value out of range
eblock: equ     53              ! invalid block specification
enovf:  equ     54              ! numeric overflow
efact:  equ     55              ! invalid factor
ecdovf: equ     56              ! code buffer overflow
eicd:   equ     57              ! invalid machine instruction
esymf:  equ     58              ! symbol/varible not found
ehlt:   equ     59              ! halt trap
ebrkc:  equ     60              ! breakpoint check error
edivz:  equ     61              ! zero divide
eivnum: equ     62              ! invalid numeric
eunspc: equ     63              ! unspec error (must be last)

! Interface vectors for DB.
! These are here for ease of use as well as ease
! of change. DB uses two i/o devices, input and output
! primitive routines, with an optional third list output
! routine. In the present version of DB, these routines
! are right above DB in memory, but if DB is in ram,
! these can be dynamically changed to fit the debugging
! situation.
! The other entries are for use with general DB
! debugging (see user's manual).
!

! Coldstart entry vector.
! This vector is always assumed to be the main entrance
! to DB. Here all registers, breakpoints and other
! data are cleared and the internal stack pair is reset.
        
cstvec: jp      coldst

! Capture entry vector.
! This entry is for people who need DB's services but
! haven't been using DB. all breakpoints and other
! data are cleared, but the registers are saved as in a
! normal breakpoint re-entry. Note that the pc is assumed
! to be on the stack, and is decremented by one
! (to adjust for a restart).
        
capvec: jp      captur

! Breakpoint entry vector.
! This is the re-entry DB uses for it's breakpoints.
! All registers are re-saved, the internal stack pair
! is restored (restored-not reset!!!) and an internal
! return is executed. The user should not use this entry,
! because zbug will be expecting items to have been set
! up within DB. The one exeption is when DB is
! expecting a breakpoint and the user is somehow able
! to re-enter DB (debugging another debugger,
! for instance).
        
brkvec: jp      recovb

! Initalize vector.
! DB expects a routine here to do any initalization
! required.
! This routine should:
!
!     Do any console or list device initalization
!     required. This routine is also responsible
!     for the breakpoint set-up. The routine should
!     place a vector to brkvec where it will be
!     entered by a restart determined by the initalize
!     routine. This routine should return the restart
!     to be used in the a register.
!     To re-iterate:
!
!     External initalization routine
!     In parameters: none
!     Out parameters: restart code - a
!     Modifies: af only
!
inivec: jp      exinit

! Input vector.
! DB expects an idealized input routine here.
! This routine should:
!
!     Check the input status. If it is null,
!     (no character waiting) the routine should
!     return a=0 (ascii null). If there is a character,
!     this should be returned in the a register.
!     DB exepects all control characters to be input,
!     and accepts only $08 (ctl-h) as a rubout.
!     This should be translated as aplicable,
!     probabally from the character $7f.
!     Any parity should be masked (bit 7 is false).
!     The input routine should save and restore all
!     registers except the a register.
!     To re-iterate:
!
!     General character input routine
!     In parameters: none
!     Out parameters: status or character in the a register
!     Modifies: af only
        
inpvec: jp      conin

! Console output vector.
! DB expects an idealized console output routine here.
! This routine should:
!
!     Output the character in the a register.
!     DB outputs rubouts as $08 (ctl-h) and expects
!     the last character to be erased and printout to
!     start over that character position. This should
!     be translated to the proper control sequence for
!     the device. The output routine should save and
!     restore all registers but the a register.
!     To re-iterate:
!
!     Console output routine
!     In parameters: character to be output in the a register
!     Out parameters: none
!     Modifies: af only
        
cotvec: jp      conout

! List output vector.
! This is an optional vector, and should ethier be a
! return or point to a return if not used.
! If the routine is provided, it should:
!
!     Output the character in the a register.
!     DB outputs rubouts as $08 (ctl-h) taken
!     to mean that the last character is to be erased.
!     Since not many printers can do that, the list
!     routine should probably translate this to a
!     back-arrow (different characters on different
!     printers) to show that the previous character
!     no longer matters. The list output routine
!     should save and restore all registers except
!     the a register.
!     To re-iterate:
!
!     List output routine
!     In parameters: character to be output in the a register
!     Out parameters:none
!     Modifies:af only
        
lotvec: jp      lstout

! Capture entry.
! loads all outside registers, including the pc
! (off the stack). Resets the internal stack, then
! jumps to the executive.

captur: ld      (spreg),sp      ! save the outside sp
        ld      sp,istack       ! reset the internal sp
        push    hl              ! save outside hl
        ld      hl,(spreg)      ! and save the outside sp
        push    hl
        call    clearm
        ld      hl,true         ! set true value
        ld      (tvar),hl
        pop     hl              ! recover outside sp
        ld      (spreg),hl      ! save
        ld      hl,cold01       ! get re-entry address
        ex      (sp),hl         ! put on the internal stack
        ld      (spsave),sp     ! save the internal sp
        ld      sp,(spreg)      ! and get the external sp
        jp      recovb          ! and go recover

! Coldstart entry.
! Clears everything, resets the stack,
! prints the sign-on message and falls
! through to the executive

coldst: ld      sp,istack       ! set the internal sp
        call    clearm          ! clear our ram
        ld      hl,true         ! set true value
        ld      (tvar),hl
cold01: call    inivec          ! initalize zbug
        ld      (rstno),a       ! set restart to use
        call    lprtst          ! print the sign-on message
        defb    'Z80 coresident debugger vs. 2.2 copyright (C) 1994 S. A. Moor'
        defb    'e' or $80

! Command executive.
! Prints DB's prompt ('*') and inputs a line,
! then procedes to compile the line.
        
cexec:  ld      sp,istack       ! reset stack
        ld      hl,codbuf       ! index code buffer
        ld      (codptr),hl     ! reset pointer
        ld      a,(extflg)      ! get execute flag
        or      a               ! check true
        call    nz,crlf         ! next line if so
        xor     a               ! set flag false
        ld      (extflg),a
        call    iline           ! input user line
        jp      nz,dstep        ! single-step, go execute
        call    skpspc          ! skip any spaces
        jp      z,cexec         ! null line, restart
        jr      fndcom          ! go find command
        
! Check for multiple command(s).
! Multiple commands are followed by a ';' character.
! DB must find this or a cr to make a valid command
! line.
        
next:   ld      sp,istack       ! reset stack
        call    skpspc          ! skip any spaces
        call    getchr          ! chk next character
        jr      z,rexec         ! end of line
        cp      ';'             ! check for multiple command
        ld      a,eterm         ! termination error
        jr      nz,error        ! no, go error
        call    skpspc          ! skip to next command
fndcom: ld      de,cmdtbl       ! and go to it
        call    search          ! find command entry
        ld      a,ecom          ! flag error
        jp      nz,error        ! command not found
        ld      a,(hl)          ! get command address
        inc     hl
        ld      h,(hl)
        ld      l,a
        jp      (hl)            ! go command
        
! Command line has been coded,
! terminate and execute
        
rexec:  ld      hl,cexec        ! index command re-entry
        call    genjmp          ! generate a jump
        jp      codbuf          ! and go program
        
! Command error.
! Prints 'error' and restarts the command executive.
        
error:  ld      sp,istack       ! reset stack
        cp      eunspc          ! check unspecified error
        jr      c,error01       ! no, skip
        ld      a,eunspc        ! yes, flag unspecified error
error01: call   prtstr          ! print preamble
        defb    '***', ' ' or $80
        call    lprtms          ! print error message
        jr      cexec           ! go restart executive

! Here are the command routines.
! all of these are on a 'zero stack level',
! and use only one possible exit, 'next'.
! Each of the command routines are usually split into
! two halves! one to parse the command line,
! and one to actually execute the code.
! The exception to this is when a command can be
! coded without subroutine call(s).

! Single-step with display.
! First dissassembles an instruction at the current
! outside pc, then if this instruction is a good one,
! we will execute it then stop. Note that if the opcode
! is invalid, we will trap with a fatal error and the
! pc still pointing at the invalid opcode.
! An optional 'count' parameter is allowed,
! which is checked for zero and decremented on each loop.
        
dstep:  call    chkend          ! check parameter
        jr      nz,dstep01      ! yes
        call    genfnc          ! generate function call
        
        call    sstep           ! single step instruction
        call    dsreg           ! display the registers
        ret                     ! and exit
        
dstep01: call   expr            ! parse count
        call    genfnc          ! generate function
        
dstep02: ld     a,h             ! test done
        or      l
        ret     z               ! yes, exit
        call    sstep           ! else step one
        call    dsreg           ! display registers
        dec     hl              ! count
        jr      dstep02         ! and loop

! Single step without display.
! This does exactly the same thing as 'dstep',
! but does not print the disassembly.
        
step:   call    chkend          ! check parameter
        jr      nz,step01       ! yes
        call    genfnc          ! generate function call
        
        call    sstep           ! single step instrunction
        ret                     ! exit
        
step01: call    expr            ! parse count
        call    genfnc          ! generate function
        
step02: ld      a,h             ! test done
        or      l
        ret     z               ! yes, exit
        call    sstep           ! else step one
        dec     hl              ! count
        jr      step02          ! loop

! Reset breakpoint.
! Resets the breakpoint at the command provided address.
! If the breakpoint does not exist, will do nothing
! (no error). If a parameter is not provided, will clear
! the entire breakpoint table.
        
clr:    call    chkend          ! check parameter
        jr      z,clr01         ! no
        call    expr            ! get address of breakpoint
        ld      hl,cbrkt        ! index breakpoint clear
        call    gencal          ! routine and produce call
        jp      next            ! exit
clr01:  ld      a,0             ! get a clear byte
        call    genldi          ! to a
        ld      hl,brkset       ! index breakpoint set byte
        call    gensta          ! produce a store to clear
        jp      next            ! exit

! List/set breakpoints command.
! If there is an address, a breakpoint will be
! set there, unless it is already set, in which
! case nothing will happen. If there is no address,
! lists all non-free breakpoints one to a line.
        
disb:   call    chkend          ! check parameter
        jr      nz,disb04       ! yes, go set
disb01: call    genfnc          ! generate function call
        
        ld      hl,brktbl       ! index breakpoint table
        ld      a,(brkset)      ! get breakpoint set byte
        ld      c,a             ! save
        ld      b,8             ! entries to do
disb02: ld      e,(hl)          ! get an entry address
        inc     hl
        ld      d,(hl)
        inc     hl
        inc     hl              ! point to next entry
        rr      c               ! chk null entry
        jr      nc,disb03       ! yes, skip print
        ex      de,hl           ! else print the address
        call    prtsym          ! print possible symbol
        call    crlf            ! next line
        ex      de,hl
disb03: djnz    disb02          ! loop if not end of table
        ret                     ! exit
        
disb04: call    expr            ! get address to set
        call    genfnc          ! generate function
        
        call    sbrkt           ! set breakpoint
        ret                     ! exit

! Continue at the address.
! Executes real instructions at the command address.
! If there is no command address,
! will execute at the current pc.
        
cont:   call    chkend          ! chk no parameter
        jr      z,cont01        ! yes
        call    expr            ! get the parameter
        ld      hl,pcreg        ! index the pc
        call    genast          ! generate address store
cont01: call    genfnc          ! generate function
        
        call    goadd           ! execute
        ret                     ! exit

! Display the registers
! See 'dsreg' routine
!
disp:   ld      hl,dsreg        ! index display routine
        call    gencal          ! generate the call
        jp      next            ! exit

! Print value.
! Prints out the command provided value,
! one to a line. Prints decimal, binary, octal and hex.
        
prt:    call    expr            ! get the value to print
        call    genfnc          ! generate function call
        
        ld      c,10            ! set decimal
        call    prtnum          ! print
        ld      a,20            ! tab to 1st
        call    tab
        call    prtstr          ! separate
        defb    '%' or $80
        ld      c,2             ! set binary
        call    prtnum          ! print
        ld      a,40            ! tab to 2nd
        call    tab
        call    prtstr          ! separate
        defb    '@' or $80
        ld      c,8             ! set octal
        call    prtnum          ! print
        ld      a,60            ! tab to 3rd
        call    tab
        call    prtstr          ! separate
        defb    '$' or $80
        ld      c,16            ! set hexadecimal
        call    prtnum          ! print
        call    crlf            ! terminate line
        ret                     ! exit

! Until command.
! Checks the command provided value for zero,
! and restarts the command line if it is.
! The loop value may be ommited, in which case
! it becomes a loop forever. The standard 'until'
! command generates a break check. An alternate
! form is avalible which does not.
        
until:  ld      hl,inpchk       ! generate break check
        call    gencal
        
! This entry is used for no break check.
        
nbutl:  call    chkend          ! check parameter
        jr      z,nbutl01       ! no
        call    expr            ! get the value to test
        call    gentst          ! generate word test
        ld      hl,codbuf       ! index start of code
        call    genjpz          ! generate a 'jp z,codbuf'
        jp      next            ! exit
nbutl01: ld     hl,codbuf       ! generate unconditional
        call    genjmp          ! restart
        jp      next            ! exit

! Stop command.
! Checks the provided value for non-zero,
! and if so, restarts the command executive.
! This is used to provide an 'easy exit' for
! a given condition.
        
stop:   call    expr            ! get the value to test
        call    gentst          ! generate word test
        ld      hl,cexec        ! index command restart
        call    genjpnz         ! generate a 'jp nz,cexec'
        jp      next            ! exit

! Dump command.
! Dumps memory from the first command provided address
! to the secound command provided address in the following
! format! the address is printed followed by a ':', then
! up to 16 bytes are displayed separated by a space.
! This is followed by the ascii version of the data,
! in single quotes.
! If the second parameter does not exist, a single byte
! dump will happen.
        
dump:   call    expr            ! get the starting address
        call    gensav          ! save
        call    skpspc          ! get param separator, ','
        cp      ','
        jr      nz,dump01       ! default to first value
        call    getchr          ! skip the separator
        call    expr            ! get the ending address
dump01: call    genfnc          ! generate function call

        ex      de,hl           ! place parameters
        pop     hl
        ex      (sp),hl
dump02: call    inpchk          ! first chk for break
        call    pword           ! print the address
        call    prtstr          ! with trailing ' '
        defb    ' ' or $80
        ld      b,16            ! set number of bytes
        push    hl              ! save starting location
dump03: ld      a,(hl)          ! get a byte from memory
        call    phexs           ! and print with space
        push    hl              ! save pointer
        or      a               ! reset carry
        sbc     hl,de           ! compare end and start addresses
        pop     hl
        jr      nc,dump04       ! it's below us, skip
        inc     hl
        djnz    dump03          ! loop
dump04: ld      a,5+16*3+1      ! set ascii collumn
        call    tab             ! tab to position
        pop     hl              ! restore start address
        ld      b,16            ! set count
        call    prtstr          ! separate
        defb    '"' or $80
dump07: ld      a,(hl)          ! get byte
        and     $7f             ! strip parity
        cp      $7f             ! check \del
        jr      z,dump08        ! yes
        cp      ' '             ! check control character
        jr      nc,dump09       ! no
dump08: ld      a,'\\'          ! replace with '\'
dump09: call    prtchr          ! print character
        push    hl              ! compare end and start address
        or      a
        sbc     hl,de
        pop     hl
        ld      c,true          ! flag end
        jr      nc,dump10       ! it's below us, skip
        inc     hl              ! next
        djnz    dump07          ! loop
        ld      c,false         ! flag not end
dump10: call    prtstr          ! terminate
        defb    '"' or $80
        call    crlf            ! next line
        inc     c               ! check end
        jr      nz,dump02       ! loop to set up next line
        ret                     ! exit

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
!
! Data tables
!
! Message pool
!
msgtbl:
        
        defb    'b' or $80      ! bsreg
        defb    'c' or $80
        defb    'd' or $80
        defb    'e' or $80
        defb    'h' or $80
        defb    'l' or $80
        defb    '(hl', ')' or $80
        defb    'a' or $80
        defb    'h', 'l' or $80  ! bexreg
        defb    'i', 'x' or $80
        defb    'i', 'y' or $80
        defb    'b', 'c' or $80  ! bdr
        defb    'd', 'e' or $80
        defb    'h', 'l' or $80
        defb    's', 'p' or $80
        defb    'b', 'c' or $80
        defb    'd', 'e' or $80
        defb    'h', 'l' or $80
        defb    'a', 'f' or $80
        defb    'ad', 'd' or $80  ! bopr
        defb    'ad', 'c' or $80
        defb    'su', 'b' or $80
        defb    'sb', 'c' or $80
        defb    'an', 'd' or $80
        defb    'xo', 'r' or $80
        defb    'o', 'r' or $80
        defb    'c', 'p' or $80
        defb    'n', 'z' or $80  ! bccode
        defb    'z' or $80
        defb    'n', 'c' or $80
        defb    'c' or $80
        defb    'p', 'o' or $80
        defb    'p', 'e' or $80
        defb    'p' or $80
        defb    'm' or $80
        defb    'rl', 'c' or $80  ! brx
        defb    'rr', 'c' or $80
        defb    'r', 'l' or $80
        defb    'r', 'r' or $80
        defb    'sl', 'a' or $80
        defb    'sr', 'a' or $80
        defb    'sr', 'l' or $80
        defb    'bi', 't' or $80  ! bbt
        defb    're', 's' or $80
        defb    'se', 't' or $80
        defb    'flags  a  b  c  d  e  h  l  ix   iy   sp   i  f'' a'' b'' '
        defb    'c'' d'' e'' h'' l'' (sp', ')' or $80
        defb    'Missing paramete', 'r' or $80  ! error messages
        defb    'Missing '')', '''' or $80
        defb    'Missing '']', '''' or $80
        defb    'Invalid command terminatio', 'n' or $80
        defb    'Breakpoint table ful', 'l' or $80
        defb    'Command not foun', 'd' or $80
        defb    'Value out of rang', 'e' or $80
        defb    'Invalid block specificatio', 'n' or $80
        defb    'Numeric overflo', 'w' or $80
        defb    'Invalid facto', 'r' or $80
        defb    'Code buffer overflo', 'w' or $80
        defb    'Invalid machine cod', 'e' or $80
        defb    'Symbol/varible not foun', 'd' or $80
        defb    'Halt tra', 'p' or $80
        defb    'Breakpoint chec', 'k' or $80
        defb    'Zero divid', 'e' or $80
        defb    'Invalid numeri', 'c' or $80
        defb    'Error unspecified - notify S. A. Moor', 'e' or $80
        defb    0               ! end of table

!
! command table
!
cmdtbl: defb    'w', 's' or $80  ! (w)ord (s)ymbols
        defw    wsym
        defb    'nw', 's' or $80  ! (n)o (w)ord (s)ymbols
        defw    nwsym
        defb    'b', 's' or $80  ! (b)yte (s)ymbols
        defw    bsym
        defb    'nb', 's' or $80  ! (n)o (b)yte (s)ymbols
        defw    nbsym
        defb    'fp', 'e' or $80  ! (f)lag (p)arity (e)ven
        defw    fpe
        defb    'fp', 'o' or $80  ! (f)lag (p)arity (o)dd
        defw    fpo
        defb    'f', 'p' or $80  ! (f)lag (p)ositive
        defw    fp
        defb    'f', 'm' or $80  ! (f)lag (m)inus
        defw    fm
        defb    'f', 'z' or $80  ! (f)lag (z)ero
        defw    fz
        defb    'fn', 'z' or $80  ! (f)lag (n)ot (z)ero
        defw    fnz
        defb    'f', 'h' or $80  ! (f)lag (h)alf-carry
        defw    fh
        defb    'fn', 'h' or $80  ! (f)lag (n)o (h)alf-carry
        defw    fnh
        defb    'f', 'a' or $80  ! (f)lag (a)dd
        defw    fa
        defb    'f', 's' or $80  ! (f)lag (s)ubtract
        defw    fs
        defb    'f', 'c' or $80  ! (f)lag (c)arry
        defw    fc
        defb    'fn', 'c' or $80  ! (f)lag (n)o (c)arry
        defw    fnc
        defb    'd', 's' or $80  ! (d)isplay (s)tep
        defw    dstep
        defb    's', 's' or $80  ! (s)ingle (s)tep
        defw    step
        defb    'c', 'p' or $80  ! (c)om(p)are
        defw    comp
        defb    's', 'e' or $80  ! (se)arch
        defw    sear
        defb    's', 'n' or $80  ! (s)earch (n)ot
        defw    searn
        defb    'nb', 'u' or $80  ! (n)o (b)reak (u)ntil
        defw    nbutl
        defb    're', 't' or $80  ! (ret)urn
        defw    return
        defb    'pt', 'r' or $80  ! (p)rin(t)e(r)
        defw    ptr
        defb    'npt', 'r' or $80  ! (n)o (p)rin(t)e(r)
        defw    nptr
        defb    'r', 'd' or $80  ! (r)epeat (d)isplay
        defw    sexp
        defb    'nr', 'd' or $80  ! (n)o (r)epeat (d)isplay
        defw    rexp
        defb    'h', 't' or $80  ! (h)alt (t)rap
        defw    shlt
        defb    'nh', 't' or $80  ! (n)o (h)alt (t)rap
        defw    rhlt
        defb    'p', 't' or $80  ! (p)or(t)
        defw    port
        defb    'm' or $80      ! (m)ove
        defw    move
        defb    'f' or $80      ! (f)ill
        defw    fill
        defb    'r' or $80      ! (r)egisters
        defw    disp
        defb    'd' or $80      ! (d)ump
        defw    dump
        defb    'e' or $80      ! (e)nter
        defw    entr
        defb    'l' or $80      ! (l)ist
        defw    list
        defb    'g' or $80      ! (g)o
        defw    cont
        defb    'c' or $80      ! (c)lear
        defw    clr
        defb    'b' or $80      ! (b)reakpoints
        defw    disb
        defb    'p' or $80      ! (p)rint
        defw    prt
        defb    'u' or $80      ! (u)ntil
        defw    until
        defb    's', 't' or $80  ! (st)op
        defw    stop
        defb    'a' or $80      ! (a)ssign
        defw    assign
        defb    0               ! end of table

!
! Simple expression operators table
!
sexop:  defb    'o', 'r' or $80  ! or
        defw    genor
        defb    'xo', 'r' or $80  ! xor
        defw    genxor
        defb    0               ! end of table
!
! Term operators table
!
termop: defb    'mo', 'd' or $80  ! mod
        defw    term07
        defb    'an', 'd' or $80  ! and
        defw    genand
        defb    'sh', 'l' or $80  ! shl
        defw    term08
        defb    'sh', 'r' or $80  ! shr
        defw    term09
        defb    0               ! end of table
!
! Factor operators table
!
notop:  defb    'no', 't' or $80  ! not

!
! word variable table
!
vtword: defb    'stb', 'l' or $80  ! (s)ymbol (t)a(bl)e
        defw    stable
        defb    'bc', '''' or $80  ! bc'
        defw    bcrega
        defb    'de', '''' or $80  ! de'
        defw    derega
        defb    'hl', '''' or $80  ! hl'
        defw    hlrega
        defb    'b', 'c' or $80  ! bc
        defw    bcreg
        defb    'd', 'e' or $80  ! de
        defw    dereg
        defb    'h', 'l' or $80  ! hl
        defw    hlreg
        defb    'i', 'x' or $80  ! ix
        defw    ixreg
        defb    'i', 'y' or $80  ! iy
        defw    iyreg
        defb    's', 'p' or $80  ! sp
        defw    spreg
        defb    'p', 'c' or $80  ! pc
        defw    pcreg
        defb    'x' or $80      ! x (user variable)
        defw    xvar
        defb    'y' or $80      ! y (user variable)
        defw    yvar
        defb    'z' or $80      ! z (user variable)
        defw    zvar
        defb    't' or $80      ! value of true
        defw    tvar
        defb    'f', 'l' or $80  ! value of false
        defw    fvar
        defb    0               ! end of table

!
! byte variables table
!
vtbyte: defb    'tab', '1' or $80  ! tab set 1
        defw    tabo
        defb    'tab', '2' or $80  ! tab set 2
        defw    tabt
        defb    'a', '''' or $80  ! a'
        defw    afrega+1
        defb    'b', '''' or $80  ! b'
        defw    bcrega+1
        defb    'c', '''' or $80  ! c'
        defw    bcrega
        defb    'd', '''' or $80  ! d'
        defw    derega+1
        defb    'e', '''' or $80  ! e'
        defw    derega
        defb    'h', '''' or $80  ! h'
        defw    hlrega+1
        defb    'l', '''' or $80  ! l'
        defw    hlrega
        defb    'f', '''' or $80  ! f'
        defw    afrega
        defb    'a' or $80      ! a
        defw    afreg+1
        defb    'b' or $80      ! b
        defw    bcreg+1
        defb    'c' or $80      ! c
        defw    bcreg
        defb    'd' or $80      ! d
        defw    dereg+1
        defb    'e' or $80      ! e
        defw    dereg
        defb    'h' or $80      ! h
        defw    hlreg+1
        defb    'l' or $80      ! l
        defw    hlreg
        defb    'i' or $80      ! i
        defw    ireg
        defb    'f' or $80      ! f
        defw    afreg
        defb    0               ! end of table

! Equations

maxlin: equ     80              ! maximum input line length
maxcod: equ     255             ! maximum code length

!
! Data area for DB
!
datap:  defvs                   ! start of data area

rstno:  defvs   1               ! restart code in use
ibuff:  defvs   maxlin          ! input buffer
iptr:   defvs   2               ! buffer pointer
codbuf: defvs   maxcod          ! code buffer
codptr: defvs   2               ! buffer pointer
lstops: defvs   1               ! disassembly list switch
trops:  defvs   1               ! trace options
extflg: defvs   1               ! external execute flag
lincnt: defvs   1               ! line counter
tabo:   defvs   1               ! 1st tab set
tabt:   defvs   1               ! 2nd tab set
condpc: defvs   2               ! conditional pc
uncdpc: defvs   2               ! unconditional pc
spsave: defvs   2               ! internal stack save
insbuf: defvs   4               ! instruction buffer
ucvec:  defvs   3               ! unconditional vector (must follow insbuf)
cvec:   defvs   3               ! cond vec (must be +/- 128 of insbuf)
afrega: defvs   2               ! alternate af
bcrega: defvs   2               ! alternate bc
derega: defvs   2               ! alternate de
hlrega: defvs   2               ! alternate hl
ireg:   defvs   1               ! interrupt register save
afreg:  defvs   2               ! af save
bcreg:  defvs   2               ! bc save
dereg:  defvs   2               ! de save
hlreg:  defvs   2               ! hl save
ixreg:  defvs   2               ! ix save
iyreg:  defvs   2               ! iy save
spreg:  defvs   2               ! sp save
pcreg:  defvs   2               ! pc save
xvar:   defvs   2               ! user variables
yvar:   defvs   2
zvar:   defvs   2
tvar:   defvs   2               ! value of true
fvar:   defvs   2               ! value of false
brkset: defvs   1               ! breakpoint set byte
brktbl: defvs   3*8             ! brkpnt store (addr, byte)
stkspc: defvs   80              ! space for internal stack
istack: defvs                   ! the stack is set to the top of the page

iniend: defvs                   ! end of initalized data
        
datlen: equ     iniend-datap    ! number of data bytes
        
! These entries are not initalized
        
stable: defvs   2               ! symbol table address
        
datend: defvs                   ! end of data section

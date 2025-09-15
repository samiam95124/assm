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
bksp:   equ     $08             ! backspace (rubout)dddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddddd
ctls:   equ     'S'-64          ! stop (ctl-s)
ctlc:   equ     'C'-64          ! cancel (ctl-c)
fhlt:		equ     0               ! halt trap flag
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

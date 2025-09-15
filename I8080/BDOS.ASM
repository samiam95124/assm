!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
!                              CP/M EMULATOR                                  !
!                                                                             !
!                       Copyright (C) 1994 S. A. Moore                        !
!                           All rights reserved                               !
!                                                                             !
! Emulates a CP/M system running under DB. This is the BDOS/BIOS section of   !
! RUNCPM. RUNCPM is loaded at 0, relocates itself to the standard CP/M        !
! address for a 64k (maximum size memory) configuration. It then formats      !
! memory to appear as required for CP/M operation, loads the target program,  !
! then operates as a "bridge" from CP/M type system calls to DB oriented      !
! calls using the simulated I/O methods.                                      !
! The command line format for RUNCPM is:                                      !
!                                                                             !
!       runcpm <command> {<param>}                                            !
!                                                                             !
! The command is a file specification for the .cpm, .obj or .com program      !
! that will be run (the default extention is .cpm). All that appears after    !
! the command is set up as the "command tail" for CP/M programs.              !
! All serial I/O under CP/M is set/taken from the console. Disk files are all !
! what exists in the current directory. Any simulated paths or disk changes   !
! are just ignored. Existing files may be accessed. New files may be created, !
! but not by the same name as an existing file. No deletes or renames are     !
! allowed, instead they generate errors and halt the program. The simulated   !
! program may not, therefore, do any damage to the host system or create      !
! non-obvious files. No random access or mixed read/write may be performed    !
! on a file. Instead, a file must be opened, read or written sequentailly,    !
! then closed.                                                                !
! The CP/M enviornment is simulated fairly closely. The biggest difference is !
! that the fcb fields assigned to the system no longer have meaning. Instead, !
! a FID or file ID number is stored in a reserved field of the fcb. This      !
! number corresponds to the I/O port number of the simulated I/O address.     !
! programs that attempt to read or change system specific fields within the   !
! FCB won't work, but this was a standard problem even on the real CP/M, so   !
! most well behaved programs do not do this.                                  !
! While reading this source, quite a few confusing formatting features will   !
! appear, such as placing seemingly meaningless jumps at various locations.   !
! Like other systems we know, CP/M was rife with undocumented features, most  !
! of which were well known and unfortunately used. We attempt to do something !
! reasonable with most of them.                                               !
! The format of the CP/M run environment at the 64kb configuration is:        !
!                                                                             !
!       $0000 --> -----------------------------                               !
!                 |        PAGE 0 AREA        |                               !
!       $0100 --> -----------------------------                               !
!                 |           TPA             |                               !
!                 |                           |                               !
!                 ~                           ~                               !
!                                                                             !
!                 ~                           ~                               !
!                 |                           |                               !
!                 |                           |                               !
!       $e400 --> -----------------------------                               !
!                 |           CCP             |                               !
!       $ec00 --> -----------------------------                               !
!                 |           BDOS            |                               !
!       $fa00 --> -----------------------------                               !
!                 |           BIOS            |                               !
!                 -----------------------------                               !
!                                                                             !
! Definitions:                                                                !
!                                                                             !
! TPA:  Transient Program Area.                                               !
! CCP:  Console Control Program.                                              !
! BDOS: Basic Disk Operating System.                                          !
! BIOS: Basic Input Output System.                                            !
!                                                                             !
! This program is implemented in 8080 assembly language, but can be used with !
! Z80, Z280, Z180 and 8085 simulation as well.                                !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        include portcall        ! include portable system calls
!
! Macro: define string with leading length
!
lenstr: macro   string
        defb    len string      ! place string length 1st
        defb    string          ! then string data
        endmac
!
! Equations
!
warmv:  equ     $0000           ! warmstart vector
iobyte: equ     $0003           ! current I/O byte
drvusr: equ     $0004           ! current drive/user
bdosv:  equ     $0005           ! bdos vector
dfcb1:  equ     $005c           ! default FCB 1
dfcb2:  equ     $006c           ! default FCB 2
ddbuf:  equ     $0080           ! default disk buffer
tpa:    equ     $0100           ! transient program area
ccp:    equ     $e400           ! where CCP would be
ccpw:   equ     ccp+3           ! warmstart vector for ccp
bios:   equ     $fa00           ! bios location
maxlen: equ     ccp+6           ! maximum length of input buffer
curlen: equ     ccp+7           ! current length of input buffer
inpbuf: equ     ccp+8           ! input buffer
inpptr: equ     inpbuf+128      ! input buffer pointer
!
! Standard CP/M FCB
!
fdr:    equ     0               ! drive code
ffn:    equ     1               ! file name primary
ffe:    equ     9               ! file name extention
fex:    equ     12              ! extention number
fs1:    equ     13              ! reserved, used by us to store byte count
fs2:    equ     14              ! reserved, used by us to store port no. 
frc:    equ     15              ! record count
fal:    equ     16              ! file allocation (group vector)
fcr:    equ     32              ! current record
fr0:    equ     33              ! random record byte low
fr1:    equ     34              ! random record byte mid
fr2:    equ     35              ! random record byte high
!
! Errors
!
eunimp: equ     1               ! unimplemented cp/m call
eclovf: equ     2               ! console line overflow
epgmov: equ     3               ! tpa overflow
ebdsec: equ     4               ! cp/m bad sector error
eselct: equ     5               ! cp/m select error
ewpt:   equ     6               ! cp/m write protect  

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Entry vectors                                                               !
!                                                                             !
! As this program resides at a CP/M BDOS address, here is the front section   !
! of bdos, formatted as the original, except that two jump vectors are        !
! provided in front of it, one to initalize this program, and another         !
! reserved. This serves both to regularize module entry and to even up the    !
! module location to mode 256. This will place the normal bdos service        !
! entry point at $ec06, it's place in original CP/M.                          !
! The "error vectors" following this jump were original CP/M undocumenteds    !
! that allowed the error handlers for CP/M to be changed. We point them to    !
! error routines in case they are called, but changing them will have no      !
! effect.                                                                     !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

runcpm:
        jmp     init            ! initalize program
        jmp     0               ! reserved
        jmp     bdos            ! bdos (system) call

        defw    badsece         ! 'BAD SECTOR' error
        defw    selecte         ! 'SELECT' error
        defw    roe             ! 'R/O' error
        defw    froe            ! 'FILE R/O' error

badsece:
        mvi     a,ebdsec        ! set error
        call    error           ! process
selecte:
        mvi     a,eselct        ! etc.
        call    error
roe: 
        mvi     a,ewpt
        call    error
froe:
        mvi     a,ewpt
        call    error

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Initalize module                                                            !
!                                                                             !
! Initalizes data, loads and formats the command line, loads the target       !
! program, and executes it.                                                   !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

init:   
        mvi     a,$c3           ! set up warmstart vector
        sta     warmv
        lxi     h,biosv+3       ! set to bios vector
        shld    warmv+1
        mvi     a,0             ! set I/O byte
        sta     iobyte
        mvi     a,0             ! set drive/user
        sta     drvusr
        mvi     a,$c3           ! set bdos vector
        sta     bdosv
        lxi     h,bdos
        shld    bdosv+1
        mvi     a,$c3           ! set ccp vectors
        sta     ccp
        lxi     h,sysres
        shld    ccp+1
        mvi     a,$c3
        sta     ccpw
        lxi     h,sysres
        shld    ccpw+1
        lxi     h,biosv         ! index bios vector table
        lxi     d,bios          ! index destination
        lxi     b,biosvn*3      ! set length
init01:
        mov     a,m             ! transfer bytes
        stax    d
        inx     h
        inx     d
        mov     a,c
        ora     b
        jnz     init01
        mvi     a,127           ! set maximum length
        sta     maxlen
        lxi     h,inpbuf        ! clear input buffer
        lxi     b,128
        call    filmem
        lxi     h,inpbuf        ! set input buffer pointer
        shld    inpptr
        lxi     h,ddbuf         ! set dma address
        shld    dmaadr
        lxi     h,constr        ! index console filename
        mvi     a,8             ! set length
        prtcal  _prt_open       ! open file
        cc      syserr          ! process error
        mov     a,c             ! place as console file
        sta     confil
        lxi     h,lststr        ! index list filename
        mvi     a,5             ! set length
        prtcal  _prt_open       ! open file
        cc      syserr          ! process error
        mov     a,c             ! place as list file
        sta     lstfil
        call    loadlin         ! load command line
        call    loadprg         ! load target program
        call    setcmd          ! set command line
        jp      tpa             ! go target program

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Process BDOS call                                                           !
!                                                                             !
! This is the standard bdos entry for programs.                               !
! The bdos call code is validated, and used to perform a table lookup. If the !
! call is invalid, a standard return is taken.                                !
! The cp/m rule is obeyed that a=l and b=h on return. The contents of the     !
! flags and c are not so settled. After examining the returns from the real   !
! cp/m there seems to be no pattern. We return the flags according to an      !
! 'ora a' of the value in a, and c contains the call code.                    !
! aside from the above, other registers/modes are unmodified.                 !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

bdos:
        xchg                    ! save de parameter
        shld    desave
        lxi     h,0             ! get stack
        dad     sp
        shld    stacks          ! save
        lxi     sp,istack       ! index our internal stack
        lxi     h,0             ! set null return value
        mov     a,c             ! check valid bdos call
        cpi     41
        jnc     bdos01          ! no, go exit null
        lxi     h,bdos01        ! set up return address
        push    h               ! place on stack
        mov     l,c             ! set up code
        mvi     h,0
        dad     h               ! * 2
        lxi     d,bdostbl       ! offset into table
        dad     d
        mov     a,m             ! get low byte
        inx     h               ! index high byte
        mov     h,m             ! get high byte
        mov     l,a             ! place low byte
        pchl                    ! go as call with return on stack
bdos01:
        mov     a,l             ! set cp/m compatibility
        mov     b,h
        ora     a               ! set flags
        lhld    stacks          ! reload user stack
        sphl
        mov     l,a             ! restore hl
        mov     h,b
        ret                     ! return to user program

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Open file                                                                   !
!                                                                             !
! Opens the file by the fcb at de for processing. The filename, drive and     !
! user are converted to a standard filespec, and that is opened and the fid   !
! placed in a reserved spot of the fcb. Since the fid is specified as cleared !
! as cleared on entry to open, make or search, accidents should generally be  !
! avoidable.                                                                  !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
openf:   
        lhld    desave          ! index fcb
        inx     h               ! index primary
        xchg                    ! place in de
        lxi     h,filnm1        ! index 1st filename buffer
        call    cvtnam          ! convert cp/m filename to buffer
        mvi     a,20            ! set length of buffer
        mvi     b,_mod_read or _mod_write or _mod_exist ! set existing,
                                ! read or write file
        prtcal  _prt_open       ! open the file
        cpi     _err_nexist     ! check file not found
        jz      openf01         ! yes, go
        ora     a               ! check other error
        cnz     syserr          ! yes, terminate
        lxi     d,fs2           ! index fid byte in fcb
        lhld    desave
        dad     d
        mov     m,c             ! place fid
        jmp     openf02         ! exit
openf01: 
        mvi     a,$ff           ! set not found code
openf02: 
        mov     l,a             ! set up return
        mvi     h,0
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Make file                                                                   !
!                                                                             !
! Creates the file by the fcb at de for processing. The filename, drive and   !
! user are converted to a standard filespec, and that is opened and the fid   !
! placed in a reserved spot of the fcb. Since the fid is specified as cleared !
! as cleared on entry to open, make or search, accidents should generally be  !
! avoidable.                                                                  !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        
makef:   
        lhld    desave          ! index fcb
        inx     h               ! index primary
        xchg                    ! place in de
        lxi     h,filnm1        ! index 1st filename buffer
        call    cvtnam          ! convert cpm/ filename to buffer
        mvi     a,20            ! set length of buffer
        mvi     b,_mod_read or _mod_write or _mod_noexist ! set non-existing,
                                ! read or write file
        prtcal  _prt_open       ! open the file
        cpi     _err_exist      ! check file exists
        jz      makef01         ! yes, go
        ora     a               ! check other error
        cnz     error           ! yes, go
        lxi     d,fs2           ! index fid byte in fcb
        lhld    desave
        dad     d
        mov     m,c             ! place fid
        jmp     makef02          ! exit
makef01: 
        mvi     a,$ff           ! set not found code
makef02: 
        mov     l,a             ! set up return
        mvi     h,0
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Close file                                                                  !
!                                                                             !
! Closes the file by the fcb in de. A close will never fail to find the       !
! filename (no matter what the fcb filename says), as in cp/m. But unlike     !
! cp/m, failure to close a file will probally have bad results, like running  !
! out of fids.                                                                !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

closef:  
        call    getfid          ! get fid
        prtcal  _prt_close      ! close file
        cc      syserr          ! process any error
        lxi     h,0             ! clear result
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Delete file                                                                 !
!                                                                             !
! Removes the file by the fcb at de.                                          !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

deletef: 
        lhld    desave          ! index fcb
        inx     h               ! index primary
        xchg                    ! place in de
        lxi     h,filnm1        ! index 1st filename buffer
        call    cvtnam          ! convert filename
        mvi     a,20            ! set length
        mvi     b,0             ! set move to nowhere
!        prtcal  _prt_move      ! move
        cpi     _err_nexist     ! check file not found
        jz      deletef01       ! yes, go
        ora     a               ! check other error
        cnz     syserr          ! yes, process
        jmp     deletef02       ! exit
deletef01:
        mvi     a,$ff           ! set file not found code
deletef02:
        mov     l,a             ! set result
        mvi     h,0
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Rename file                                                                 !
!                                                                             !
! Renames the file by the fcb at de.                                          !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

renamef:
        lhld    desave          ! index fcb
        inx     h               ! index primary
        xchg                    ! place in de
        lxi     h,filnm1        ! index 1st filename buffer
        call    cvtnam          ! convert 1st filename
        lhld    desave          ! index fcb
        lxi     d,16+1          ! index 2nd primary
        dad     d
        xchg                    ! place in de
        lxi     h,filnm2        ! index 1st filename buffer
        call    cvtnam          ! convert 1st filename
        mvi     a,20            ! set length
        lxi     d,filnm2        ! index altername filename buffer
        mvi     b,20            ! set length of alternate
!        prtcal  _prt_move       ! move
        cpi     _err_nexist     ! check file not found
        jz      renamef01       ! yes, go
        ora     a               ! check other error
        cnz     syserr          ! yes, process
        jmp     renamef02       ! exit
renamef01:
        mvi     a,$ff           ! set file not found code
renamef02:
        mov     l,a             ! set result
        mvi     h,0
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Read sequential                                                             !
!                                                                             !
! Reads 128 bytes to the current dma address.                                 !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

rdseq:
        call    getfid          ! get fid
        lhld    dmaadr          ! get the dma address
        lxi     d,128           ! set length to read
        prtcal  _prt_read       ! read 'sector'
        cpi     _err_eof        ! check eof
        jz      rdseq01         ! yes, go
        ora     a               ! check other error
        cnz     syserr          ! yes, process
        jmp     rdseq02         ! exit
rdseq01:
        mvi     a,$ff           ! set end of file code
rdseq02:
        mov     l,a             ! set result
        mvi     h,0
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Write sequential                                                            !
!                                                                             !
! Writes 128 bytes from the current dma address.                              !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

wrseq:
        call    getfid          ! get fid
        lhld    dmaadr          ! get the dma address
        lxi     d,128           ! set length to write
        prtcal  _prt_write      ! write 'sector'
        cpi     _err_eof        ! check eof
        jz      wrseq01         ! yes, go
        ora     a               ! check other error
        cnz     syserr          ! yes, process
        jmp     wrseq02         ! exit
wrseq01:
        mvi     a,$ff           ! set end of file code
wrseq02:
        mov     l,a             ! set result
        mvi     h,0
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Read random                                                                 !
!                                                                             !
! Reads 128 bytes to the current DMA address. The file is first positioned    !
! to the given random record, then the record read, then the file positioned  !
! yet again, to emulate cp/m's lack of advance on such a call.                !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

rdrnd:
        call    getfid          ! get fid
        lxi     h,fr2           ! convert random position to byte count
        mov     a,m
        rar
        dcx     h
        mov     a,m
        rar
        mov     e,a
        dcx     h
        mov     a,m
        rar
        mov     h,a
        mvi     a,0
        rar
        mov     l,a
        mvi     d,0
rdrnd01:
!        prtcal  _prt_position   ! position file
        cpi     _err_eof        ! check eof
        jz      rdrnd02         ! yes, go
        ora     a               ! check other error
        cnz     syserr          ! yes, process
        push    d               ! save that position
        push    h
        lhld    dmaadr          ! get the dma address
        lxi     d,128           ! set length to read
        prtcal  _prt_read       ! read 'sector'
        cpi     _err_eof        ! check eof
        jz      rdrnd02         ! yes, go
        ora     a               ! check other error
        cnz     syserr          ! yes, process
        pop     h               ! restore position
        pop     d
!        prtcal  _prt_position   ! position file
        cpi     _err_eof        ! check eof
        jz      rdrnd02         ! yes, go
        ora     a               ! check other error
        cnz     syserr          ! yes, process
rdrnd02:
        mvi     a,4             ! set seek past end code
        mov     l,a             ! set result
        mvi     h,0
        ret

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Write random                                                                !
!                                                                             !
! Writes 128 bytes to the current DMA address. The file is first positioned   !
! to the given random record, then the record written, then the file          !
! positioned yet again, to emulate cp/m's lack of advance on such a call.     !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

wrrnd:
        call    getfid          ! get fid
        lxi     h,fr2           ! convert random position to byte count
        mov     a,m
        rar
        dcx     h
        mov     a,m
        rar
        mov     e,a
        dcx     h
        mov     a,m
        rar
        mov     h,a
        mvi     a,0
        rar
        mov     l,a
        mvi     d,0
wrrnd01:
!        prtcal  _prt_position   ! position file
        cpi     _err_eof        ! check eof
        jz      wrrnd02         ! yes, go
        ora     a               ! check other error
        cnz     syserr          ! yes, process
        push    d               ! save that position
        push    h
        lhld    dmaadr          ! get the dma address
        lxi     d,128           ! set length to read
        prtcal  _prt_write      ! write 'sector'
        cpi     _err_eof        ! check eof
        jz      wrrnd02         ! yes, go
        ora     a               ! check other error
        cnz     syserr          ! yes, process
        pop     h               ! restore position
        pop     d
        prtcal  _prt_position   ! position file
        cpi     _err_eof        ! check eof
        jz      wrrnd02         ! yes, go
        ora     a               ! check other error
        cnz     syserr          ! yes, process
        jmp     wrrnd03         ! exit
wrrnd02:
        mvi     a,4             ! set seek past end code
wrrnd03:
        mov     l,a             ! set result
        mvi     h,0
        ret

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Compute file size                                                           !
!                                                                             !
! Computes the file size in records, and returns that.                        !
! Since cpm specifies that the file need not be opened to do this, we must    !
! open, read the file length, and close the file.                             !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

compfs:
        lhld    desave          ! index fcb
        inx     h               ! index primary
        xchg                    ! place in de
        lxi     h,filnm1        ! index 1st filename buffer
        call    cvtnam          ! convert cp/m filename to buffer
        mvi     a,20            ! set length
        mvi     b,_mod_read or _mod_exist ! set existing for read only
        prtcal  _prt_open       ! open file
        cpi     _err_nexist     ! check file does not exist
        jz      compfs01        ! yes, go
        ora     a               ! check other error
        cnz     syserr          ! yes, go
        prtcal  _prt_length     ! find file size
        cc      syserr          ! process error
        prtcal  _prt_close      ! close file
        cc      syserr          ! process error
        mov     a,l             ! convert and place length
        ral
        mov     c,a             ! save
        mov     a,h
        ral
        mov     b,a
        mov     a,e
        ral
        lxi     d,fr0           ! index fcb position to place
        lhld    desave
        dad     d
        mov     m,c             ! place
        inx     h
        mov     m,b
        inx     h
        mov     m,e
        jmp     compfs02        ! exit
compfs01:
        mvi     a,$ff           ! set not found code
compfs02:
        mov     l,a             ! set result
        mvi     h,0
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Find current record position                                                !
!                                                                             !
! Finds the record position for an already open file.                         !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

setrnd:
        call    getfid          ! get fid
        prtcal  _sys_location   ! find file location
        cc      syserr          ! process error
        mov     a,l             ! convert
        ral
        mov     c,a
        mov     a,h
        ral
        mov     b,a
        mov     a,e
        ral
        lhld    desave          ! index position in fcb
        lxi     d,fr0
        dad     d
        mov     m,c             ! place
        inx     h
        mov     m,b
        inx     h
        mov     m,e
        inx     h
        lxi     h,0             ! reset return code
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Set dma address                                                             !
!                                                                             !
! Sets the dma address from de.                                               !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

setdma:
        lhld    desave          ! get address
        shld    dmaadr          ! place
        lxi     h,0             ! reset return code
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Print string                                                                !
!                                                                             !
! Prints a '$' terminated string to the console.                              !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

prtstr:
        lhld    desave          ! index string
prtstr01:
        mov     a,m             ! get a character
        cpi     '$'             ! check terminator
        jnz     prtstr02        ! no, skip
        push    h               ! save string index
        mov     e,a             ! place character
        call    conout          ! output character
        pop     h               ! restore string index
        inx     h               ! next character
        jmp     prtstr01        ! loop next character
prtstr02:
        lxi     h,0             ! set result code
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Read console buffer                                                         !
!                                                                             !
! Performs the line input function to buffer de.                              !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

rdcbuf:
        lda     confil          ! get console fid
        mov     c,a             ! place
        lhld    desave          ! get buffer pointer
        mov     a,m             ! get the max
        mov     d,a             ! save
        mvi     b,0             ! clear length
        inx     h               ! index length
        push    h               ! save pointer to that
        inx     h               ! index data
rdcbuf01:
        call    getfil          ! get a console character
        ani     $7f             ! mask parity
        cpi     '\del'          ! '\del', check delete
        jz      rdcbuf03        ! yes, go
        cpi     '\bs'           ! check backspace
        jz      rdcbuf03        ! yes, go 
        cpi     '\cr'           ! check terminate line
        jz      rdcbuf04        ! yes, go
        cp      'a'             ! check lower case
        jc      rdcbuf02        ! no, skip
        cp      'z'+1
        jnc     rdcbuf02        ! no, skip
        sbi     'a'             ! convert to upper case
        adi     'A'
rdcbuf02:
        mov     e,a             ! save character
        mov     a,b             ! check line overflow
        cmp     d
        jz      rdcbuf01        ! yes, ignore character
        inr     b               ! count characters
        mov     m,e             ! place character in buffer
        inx     h
        mov     a,e             ! echo to console
        call    putfil
!
! Erase last character
!
rdcbuf03:        
        dcr     b               ! check length is 0
        inr     b
        jz      rdcbuf01        ! yes, ignore
        dcr     b               ! find new length
        mvi     a,'\bs'         ! back up
        call    putfil
        mvi     a,' '           ! erase
        call    putfil
        mvi     a,'\bs'         ! back up again
        call    putfil
        dcx     h               ! back up in buffer
        jmp     rdcbuf01        ! loop
!
! Terminate line
!
rdcbuf04:
        mvi     a,'\cr'         ! next line
        call    putfil
        mvi     a,'\lf'
        call    putfil
        pop     h               ! restore buffer length address
        mov     m,b             ! place resulting length
        mov     l,b             ! set up result code
        mvi     h,0
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Set disk                                                                    !
!                                                                             !
! Sets the current disk from e. Note that bits 4-7 of e are discarded.        !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

setdsk:
        mov     a,e             ! mask drive
        ani     $0f
        mov     e,a
        lda     drvusr          ! get drive/user
        ani     $f0             ! mask drive
        ora     e               ! set new drive
        sta     drvusr          ! replace
        lxi     h,0             ! set result code
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Get disk                                                                    !
!                                                                             !
! Gets the current disk select.                                               !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

getdsk:
        lda     drvusr          ! get the drive
        ani     $0f             ! mask
        mov     l,a             ! set result
        mvi     h,0
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Return login vector                                                         !
!                                                                             !
! Hardwired to return drive a: only.                                          !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

retlgv:
        lxi     h,1             ! set a: is accessed
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Reset disk                                                                  !
!                                                                             !
! Resets the disk to a:, and the dma address to $0080.                        !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

rstdsk:
        lda     drvusr          ! get current select
        ani     $f0             ! save only user
        sta     drvusr
        lxi     h,ddbuf         ! set dma address
        lxi     h,0             ! set result
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Reset drives                                                                !
!                                                                             !
! Reset a vector of drives. A no - op.                                        !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

rstvec:
        lxi     h,0             ! set result
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Write protect drive                                                         !
!                                                                             !
! Write protect current drive. A no - op.                                     !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

wptdsk:
        lxi     h,0             ! set result
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Get read only vector                                                        !
!                                                                             !
! Gets the read only vector. A no - op (allways returns no drive protected).  !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

getrov:
        lxi     h,0             ! set read-only vector
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Set/get user code                                                           !
!                                                                             !
! Sets or gets the user number.                                               !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

sgusr:
        mov     a,e             ! get set/get user
        cpi     $ff
        jz      sgusr01         ! get user, skip
        ral                     ! move user number to top digit
        ral
        ral
        ral
        ani     $f0             ! mask
        mov     e,a             ! save
        lda     drvusr          ! get drive/user
        ani     $0f             ! mask
        ora     e               ! place user number
        sta     drvusr          ! update
        xra     a               ! set result
        jmp     sgusr02         ! exit
sgusr01:
        lda     drvusr          ! get drive/user
        rar                     ! move down
        rar
        rar
        rar
        ani     $0f             ! mask
sgusr02:
        mov     l,a             ! set result code
        mvi     h,0
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Get I/O byte                                                                !
!                                                                             !
! Returns the current I/O byte.                                               !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

getiob:
        lda     iobyte          ! get I/O byte
        mov     l,a             ! place
        mvi     h,0             ! set result
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Set I/O byte                                                                !
!                                                                             !
! Sets the I/O byte.                                                          !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

setiob:
        mov     a,e             ! place I/O byte
        sta     iobyte
        lxi     h,0             ! set result
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Console input                                                               !
!                                                                             !
! Inputs a single character from the console.                                 !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

coninp:
        lda     confil          ! get console fid
        mov     c,a             ! place
        call    getfil          ! get character
        ani     $7f             ! clear parity
        mov     l,a             ! set result code
        mvi     h,0
        ret                     ! exit        

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Console status                                                              !
!                                                                             !
! Gets status on the console. Right now, hardwared to give console ready.     !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

consts:
        lxi     h,1             ! set ready
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Console output                                                              !
!                                                                             !
! Outputs a single character to the console.                                  !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

conout:
        lda     confil          ! get console fid
        mov     c,a             ! place
        mov     a,e             ! get character to output
        call    putfil          ! output character
        lxi     h,0             ! set result code
        ret                     ! exit        

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Direct console access                                                       !
!                                                                             !
! Inputs or outputs a direct console character.                               !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

dircon:
        mov     a,e             ! check input/output
        cpi     $ff
        jnz     dircon01        ! output, go
        call    coninp          ! process console input
        ret                     ! exit
dircon01:
        call    conout          ! process console output
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! List output                                                                 !
!                                                                             !
! Outputs a single character to the list file.                                !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

lstout:
        lda     lstfil          ! get list fid
        mov     c,a             ! place
        mov     a,e             ! get character to output
        call    putfil          ! output character
        lxi     h,0             ! set result code
        ret                     ! exit        

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Console input BIOS                                                          !
!                                                                             !
! Inputs a single character with parity stripped.                             !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

coninb:
        push    b
        lda     confil          ! get console fid
        mov     c,a             ! place
        call    getfil          ! get next character
        ani     $7f             ! remove parity
        pop     b               ! clean up and return
        ret

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Console status BIOS                                                         !
!                                                                             !
! Gets the console status.                                                    !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

constb:
        mvi     a,$ff           ! set allways ready
        ret                     ! exit        

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Console output BIOS                                                         !
!                                                                             !
! Outputs a single character from c to the console file.                      !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

conoutb:
        push    b
        mov     b,c             ! save character
        lda     confil          ! get console fid
        mov     c,a             ! place
        mov     a,b             ! get character
        call    putfil          ! output character
        pop     b               ! clean up and return
        ret

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! List output BIOS                                                            !
!                                                                             !
! Outputs a single character from c to the list file.                         !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

listb:
        push    b
        mov     b,c             ! save character
        lda     lstfil          ! get list fid
        mov     c,a             ! place
        mov     a,b             ! get character
        call    putfil          ! output character
        pop     b               ! clean up and return
        ret

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! List status BIOS                                                            !
!                                                                             !
! Gets the list status.                                                       !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

liststb:
        mvi     a,$ff           ! set allways ready
        ret                     ! exit        

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Get version number                                                          !
!                                                                             !
! Returns version number 2.2, the last widely distributed version of cp/m.    !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

getver:
        lxi     h,$0022         ! set cp/m 2.2
        ret                     ! exit

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Load command line                                                           !
!                                                                             !
! Loads the command line to be executed into the ccp buffer.                  !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

loadlin:
        push    b
        push    d
        push    h
        lxi     h,cmdstr        ! index _command
        lxi     d,8             ! set length
        mvi     b,_mod_read or _mod_exist ! set existing read
        prtcal  _prt_open       ! open file
        cc      syserr          ! process error
        lxi     h,inpbuf        ! index buffer
        mvi     b,0             ! set character count
loadlin01:
        call    getfil          ! get file character
        cp      '\cr'
        jz      loadlin03       ! yes, go
        mov     a,b             ! check overflow
        cpi     128
        mvi     a,eclovf        ! flag error
        cnc     error           ! yes, process
        mov     a,m             ! get character
        cpi     'a'             ! check lower case
        jc      loadlin02       ! no, skip
        cpi     'z'+1
        jnc     loadlin02       ! no, skip
        sbi     'a'             ! convert to upper case
        adi     'A'
loadlin02:
        mov     m,a             ! place character
        inx     h               ! next location
        inr     b               ! count characters
        jmp     loadlin01       ! loop next character
loadlin03:
        mvi     m,0             ! terminate line with zero
        mov     a,b             ! place count
        sta     curlen
        lxi     h,inpbuf        ! set current position
        shld    curpos
        prtcal  _prt_close      ! close command file
        pop     h               ! clean up and return
        pop     d
        pop     b
        ret

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Load command file                                                           !
!                                                                             !
! Loads the file at the current command file position. A .com extention is    !
! placed at the end. The command file position is skipped over the filename.  !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

loadprg:
        push    b
        push    d
        push    h
        lxi     h,filnm1        ! clear filename buffer
        lxi     b,20
        xra     a
        call    filmem
        xchg                    ! index filename in de
        lhld    curpos          ! index current command position
        mvi     b,0             ! clear length
loadprg01:
        mov     a,m             ! get a character
        ora     a               ! check end
        jz      loadprg02       ! yes, go
        cpi     ' '
        jz      loadprg02       ! yes, go
        mov     a,b             ! check overflow
        cpi     20-4+1          ! which means leave 4 characters for extention
        cnc     error           ! yes, process error
        mov     a,m             ! place character
        stax    d
        inx     h
        inx     d
        inr     b               ! count characters
        jmp     loadprg01       ! loop
loadprg02:
        mvi     m,'.'           ! place extention
        inx     h
        mvi     m,'C'
        inx     h
        mvi     m,'O'
        inx     h
        mvi     m,'M'
        lxi     h,filnm1        ! index filename
        lxi     d,20            ! set length
        mvi     b,_mod_read or _mod_exist ! set existing read only
        prtcal  _prt_open       ! open file
        cc      syserr          ! process error
        prtcal  _prt_length     ! get the length of the file
        cc      syserr          ! process error
        mov     a,e             ! check > 65535
        ora     d
        mvi     a,epgmov        ! flag error
        cnz     error           ! yes, process error
        lxi     d,ccp-tpa+1     ! check it fits in the tpa
        mov     a,e
        sub     l
        mov     a,d
        sbb     h
        mvi     a,epgmov        ! flag error
        cc      error           ! no, process error
        xchg                    ! place length in de
        lxi     h,tpa           ! index destination
        prtcal  _prt_read       ! read the target
        cc      syserr          ! process error
        prtcal  _prt_close      ! close the file
        cc      syserr          ! process error
        pop     h               ! clean up and return
        pop     d
        pop     b
        ret

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Input single character from file                                            !
!                                                                             !
! Gets a single character from file c to a.                                   !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

getfil:
        push    d
        push    h
        lxi     h,bytbuf        ! index input buffer
        lxi     d,1             ! set 1 character length
        prtcal  _prt_read       ! read character
        cc      syserr          ! terminate on error
        mov     a,m             ! get input character
        pop     h               ! restore registers and exit
        pop     d
        ret

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Output single character to file                                             !
!                                                                             !
! Puts a single character to file c from a.                                   !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

putfil:
        push    d
        push    h
        lxi     h,bytbuf        ! index input buffer
        lxi     d,1             ! set 1 character length
        mov     m,a             ! place character
        prtcal  _prt_write      ! write character
        cc      syserr          ! terminate on error
        pop     h               ! restore registers and exit
        pop     d
        ret

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Get fid from fcb                                                            !
!                                                                             !
! Gets the fid for the fcb at desave in c.                                    !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

getfid:
        push    d
        push    h
        lhld    desave          ! get fcb address
        lxi     d,fs2           ! index fid save byte
        dad     d
        mov     c,m             ! get fid
        pop     h               ! clean up and return
        pop     d
        ret

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Terminate with system error                                                 !
!                                                                             !
! Expects an error in a. Terminates the run with the error.                   !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

syserr:
        prtcal  _prt_term       ! just terminate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Process unimplememted cp/m call                                             !
!                                                                             !
! Gives an error for an unimplemented cp/m call.                              !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

unimp:
        mvi     a,eunimp        ! flag error
        call    error

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Process runcpm error                                                        !
!                                                                             !
! Expects an error code in a. The error is printed, and the run terminated.   !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

error:
        lxi     h,errmsg        ! index the error message table
error01:
        mov     e,m             ! get length
        mvi     d,0             ! set up
        inx     h               ! index string data
        dcr     a               ! check at message
        jz      error02         ! yes, exit
        dad     d               ! skip string
        jmp     error01         ! loop next string
error02:
        lda     confil          ! get console file
        mov     c,a             ! place
        prtcal  _prt_write      ! write to console
        prtcal  _prt_term       ! and terminate with any error        

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Reset system                                                                !
!                                                                             !
! Simply terminates the program.                                              !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

sysres:
        xra     a               ! set no error
        prtcal  _prt_term       ! terminate

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Fill memory with byte                                                       !
!                                                                             !
! Fills memory at position hl for bc count bytes with the byte in a.          !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

filmem:
        push    b
        push    h
filmem01:
        mov     m,a             ! place a byte
        dcx     b               ! count
        mov     a,c             ! check done
        ora     b
        mov     a,m             ! restore byte
        inx     h               ! next byte
        jnz     filmem01        ! loop
        pop     h               ! clean up and return
        pop     b
        ret

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Set command line and parameters                                             !
!                                                                             !
! Places the remainder of the command line in the default buffer, and         !
! prepares the command parameters.                                            !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

setcmd:
        push    b
        push    d
        push    h
        lhld    curpos          ! index current line position
        lxi     d,ddbuf+1       ! index destination
        mvi     c,0             ! clear count
setcmd01:
        mov     a,m             ! get a character
        stax    d               ! place
        inx     h               ! next character
        inx     d
        inr     c               ! count
        ora     a               ! check termination
        jnz     setcmd01        ! no, loop for more
        dcr     c               ! back out count of terminator
        mov     a,c             ! place count
        sta     ddbuf
        lxi     h,dfcb1         ! clear both default fcbs
        lxi     b,16*2
        xra     a
        call    filmem
        lxi     h,dfcb1+1       ! clear filenames to spaces
        lxi     b,8+3
        mvi     a,' '
        call    filmem
        lxi     h,dfcb2+1       ! clear filenames to spaces
        lxi     b,8+3
        mvi     a,' '
        call    filmem
        lxi     d,dfcb1         ! index 1st fcb
        call    parcpf          ! place the filename
        lxi     d,dfcb2         ! index 2nd fcb
        call    parcpf          ! place the filename
        pop     h               ! clean up and return
        pop     d
        pop     b
        ret

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Parse default fcb                                                           !
!                                                                             !
! Parses a single filename off the command line, and places the result in the !
! fcb position at de. The filename parsed is a simple alphanumeric with '_',  !
! which must lead with an alphabetical or '_' character. No drive or user     !
! selection is allowed or parsed. By cp/m rules, no errors are produced if    !
! the filename is missing or garbled. Instead, the fcb is just left blank.    !
! This routine is dependent on the command line being in all upper case.      !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

parcpf:
        push    b
        push    d
        push    h
        inx     d               ! skip drive
        lhld    curpos          ! index current command line position
        mvi     c,0             ! clear length of primary
parcpf01:
        mov     a,m             ! get character
        inx     h               ! next
        cpi     ' '             ! check space
        jz      parcpf01        ! yes, loop
        dcx     h               ! back up to good character
        ora     a               ! check end of line
        jz      parcpf08        ! yes, exit
        cpi     '_'             ! check valid leading character
        jz      parcpf02        ! yes, go
        cpi     'A'
        jc      parcpf08        ! no, exit
        cpi     'Z'+1
        jnc     parcpf08        ! no, exit
parcpf02:
        mov     a,c             ! check filename overflow
        cpi     8+1
        mov     a,m             ! get character
        jnc     parcpf03        ! yes, skip placement
        stax    d               ! place character
        inx     d               ! next fcb character
parcpf03:
        inx     h               ! next line character
        inr     c               ! count
        mov     a,m             ! get next character
        cpi     '.'             ! check extention starts
        jz      parcpf04        ! yes, go
        cpi     '_'             ! check valid filename character
        jz      parcpf02        ! yes, loop
        cpi     '0'
        jc      parcpf08        ! no, exit
        cpi     '9'+1
        jc      parcpf02        ! yes, loop
        cpi     'A'
        jc      parcpf08        ! no, exit
        cpi     'Z'+1
        jc      parcpf02        ! yes, loop
        jmp     parcpf08        ! no, exit
parcpf04:
        inx     h               ! skip '.'
        mvi     a,8             ! find skip to extention
        sub     c
        add     e               ! advance to extention of fcb
        mov     e,a
        mvi     a,0
        add     d
        mov     d,a
        mvi     c,0             ! clear extention length
parcpf05:
        mov     a,m             ! get a character
        cpi     '_'             ! check valid filename character
        jz      parcpf06        ! yes, go
        cpi     '0'
        jc      parcpf08        ! no, exit
        cpi     '9'+1
        jc      parcpf06        ! yes, go
        cpi     'A'
        jc      parcpf08        ! no, exit
        cpi     'Z'+1
        jnc      parcpf08       ! no, exit
parcpf06:
        mov     a,c             ! check extention overflow
        cpi     3+1
        mov     a,m             ! get character
        jnc     parcpf07        ! yes, skip placement
        stax    d               ! place character
        inx     d               ! next fcb character
parcpf07:
        inx     h               ! next line character
        inr     c               ! count
        jmp     parcpf05        ! loop
parcpf08:
        pop     h               ! clean up and return
        pop     d
        pop     b
        ret

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Convert fcb name                                                            !
!                                                                             !
! Converts the FCB name given at de to the filename in buffer hl. De points   !
! at the primary, not the fcb.                                                !
! The fcb name is converted to a system format filename. This means that it   !
! is converted to a filename of the form:                                     !
!                                                                             !
!    file.ext                                                                 !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

cvtnam:
        push    b
        push    d
        push    h
        mvi     b,8             ! move 8 primary characters
cvtnam01:
        ldax    d               ! get a character
        inx     d               ! next character
        cpi     ' '             ! check space
        jz      cvtnam02        ! no, skip
        mov     m,a             ! place primary character
        inx     h               ! next character
cvtnam02:
        dcr     b               ! count characters
        jnz     cvtnam01        ! loop next character
        mvi     m,'.'           ! place extention
        inx     h               ! next character
        mvi     b,8             ! move 3 extention characters
cvtnam03:
        ldax    d               ! get a character
        inx     d               ! next character
        cpi     ' '             ! check space
        jz      cvtnam04        ! no, skip
        mov     m,a             ! place primary character
        inx     h               ! next character
cvtnam04:
        dcr     b               ! count characters
        jnz     cvtnam03        ! loop next character
        pop     h               ! clean up and return
        pop     d
        pop     b
        ret

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Bdos handler table                                                          !
!                                                                             !
! Contains a table of handler addresses, in function number order, of all     !
! cp/m functions.                                                             !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

bdostbl:
        defw    sysres          !  0: System reset
        defw    coninp          !  1: Console input
        defw    conout          !  2: Console output
        defw    coninp          !  3: Reader input
        defw    conout          !  4: Punch output
        defw    lstout          !  5: List output
        defw    dircon          !  6: Direct console I/O
        defw    getiob          !  7: Get I/O byte
        defw    setiob          !  8: Set I/O byte
        defw    prtstr          !  9: Print string
        defw    rdcbuf          ! 10: Read console buffer
        defw    consts          ! 11: Get console status
        defw    getver          ! 12: Get version number
        defw    rstdsk          ! 13: Reset disk system
        defw    setdsk          ! 14: Set current disk
        defw    openf           ! 15: Open file
        defw    closef          ! 16: Close file
        defw    unimp           ! 17: Search first
        defw    unimp           ! 18: Search next
        defw    unimp           ! 19: Delete file
        defw    rdseq           ! 20: Read sequential
        defw    wrseq           ! 21: Write sequential
        defw    makef           ! 22: Make file
        defw    unimp           ! 23: Rename file
        defw    retlgv          ! 24: Return login vector
        defw    getdsk          ! 25: Get current disk
        defw    setdma          ! 26: Set dma address
        defw    unimp           ! 27: Get addr(alloc)
        defw    wptdsk          ! 28: Write protect disk
        defw    getrov          ! 29: Get write-only vector
        defw    unimp           ! 30: Set file attributes
        defw    unimp           ! 31: Get addr(disk params)
        defw    sgusr           ! 32: Set/get user
        defw    unimp           ! 33: Read random
        defw    unimp           ! 34: Write random
        defw    compfs          ! 35: Compute file size
        defw    setrnd          ! 36: Set random record
        defw    rstvec          ! 37: Reset drives
        defw    unimp           ! 38: Invalid
        defw    unimp           ! 39: Invalid
        defw    unimp           ! 40: Write random with fill

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Bios vector set                                                             !
!                                                                             !
! Contains a table of jumps, in bios vector order, for each bios direct       !
! function. This table must be located at the fixed bios base address, so     !
! it must be moved to that location before use.                               !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

biosv:
        jmp     sysres          ! boot
        jmp     sysres          ! warm boot
        jmp     constb          ! console status
        jmp     coninb          ! console input
        jmp     conoutb         ! console output
        jmp     listb           ! list output
        jmp     conoutb         ! punch output
        jmp     coninb          ! reader input
        jmp     unimp           ! home disk
        jmp     unimp           ! select disk
        jmp     unimp           ! set track
        jmp     unimp           ! set sector
        jmp     unimp           ! set dma address
        jmp     unimp           ! read sector
        jmp     unimp           ! write sector
        jmp     listb           ! list status
        jmp     unimp           ! sector translate

biosvn: equ     (_-biosv)/3     ! number of vectors

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Error messages                                                              !
!                                                                             !
! Error messages are in <len> <string> format.                                !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

errmsg:
        lenstr  'Runcpm error: Unimplemented CP/M call'
        lenstr  'Runcpm error: Console line input overflow'
        lenstr  'Runcpm error: TPA overflow'
        lenstr  'Target signal: CP/M bad sector'
        lenstr  'Target signal: CP/M select'
        lenstr  'Target signal: CP/M write protect'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Filename strings                                                            !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

constr: defb    '_console'
lststr: defb    '_list'
cmdstr: defb    '_command'

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Variables                                                                   !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

confil: defvs   1               ! FID of console file
lstfil: defvs   1               ! FID of list file
bytbuf: defvs   1               ! byte buffer
desave: defvs   2               ! de save on input
curpos: defvs   2               ! current command line parsing position
dmaadr: defvs   2               ! current cp/m "dma" address
filnm1: defvs   20              ! filename 1 store
filnm2: defvs   20              ! filename 2 store
stacks: defvs   2               ! external stack save
        defvs   32              ! internal stack space
istack: defvs                   ! internal stack (at top)

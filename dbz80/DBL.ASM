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

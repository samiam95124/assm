!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                              !
!                         SPARC V9 ASSEMBLER TEST FILE                         !
!                                                                              !
! Format: IP SparcV9 Assembler.                                                !
!                                                                              !
! Contains all of the SPARC V9 operations possible, in alphabetical order.     !
! This file has several uses. First, it is a complete listing of all           !
! instructions and modes. Second, it can be used as an assembler test,         !
! including the assemble statement in a debugger. Third, it can be used as the !
! starting point for a simple assembler, because the output words can be used  !
! to form a lookup table which contains the basic instruction values, with     !
! just the register fields missing.                                            !
!                                                                              !
! Notes:                                                                       !
!                                                                              !
! 1. Each of the modes of the instructions are listed, as possible. Where a    !
! register appears, each of the 32 register combinations are not listed.       !
!                                                                              !
! 2. The common addressing mode for SPARC is as follows:                       !
!                                                                              !
!    [rx+ry]                                                                   !
!    [rx+imm]                                                                  !
!                                                                              !
! The immediate bit in the instruction specifies if a second register appears, !
! or if an immediate signed offset appears. Because the g0 register can be     !
! specified, or allways zero, there are actually several shorthand forms:      !
!                                                                              !
!    [loc]                                                                     !
!    [rx]                                                                      !
!    [rx+ry]                                                                   !
!    [rx+loc]                                                                  !
!                                                                              !
! Where "loc" is a 13 bit signed offset. The first form may or may not be      !
! useful, since it specifies a location at the very start or very end of       !
! memory.                                                                      !
!                                                                              !
! 3. When the "alternate address" forms of instructions are used, the ASI      !
! specifier is not available when the immediate mode is used. Instead, the     !
! %asi register is used. This results in the following forms:                  !
!                                                                              !
!    [loc]%asi                                                                 !
!    [rx]an                                                                    !
!    [rx+ry]an                                                                 !
!    [rx+loc]%asi                                                              !
!                                                                              !
! Where "an" is an immediate ASI number.                                       !
!                                                                              !
! 4. Where %icc or %xcc exists, this appears in this listing. This is because  !
! the codes for them have no obvious integer equivalent. However, the codes    !
! for %fcc0-%fcc3 are related to the integers 0-3, and only the %fcc0 form     !
! appears.                                                                     !
!                                                                              !
! 5. impdep1 and impdep2 are listed as instructions even though the V9         !
! specification does not specify an assembly language instruction for these.   !
! They are valid in the SUN SPARC assembler.                                   !
!                                                                              !
! 6. The instructions are alphabetical by instruction, but not by mode. I      !
! don't recommend running a sort on the file.                                  !
!                                                                              !
! 7. synthetic instructions are included in alphabetical order, but are marked !
! as synthetic in the comments.                                                !
!                                                                              !
! 8. Synonyms are included in alphabetical order, but are marked as synonyms   !
! in the comments.                                                             !
!                                                                              !
! 9. Assemble under IP SPARC V9 assembler with:                                !
!                                                                              !
!    as allsparcv9=allsparcv9                                                  !
!                                                                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Artificial program target address

! Sparc as always puts out 0 for relocatables, so this must be zero to match
! in the binary listing.

loc:        equ 0 ! 0x123

! artificial variable target address

dat:        equ 0x456

! Instructions

add         %l0,%l1,%l2
add         %l0,1,%l2
addcc       %l0,%l1,%l2 
addcc       %l0,1,%l2         
addc        %l0,%l1,%l2
addc        %l0,1,%l2
addccc      %l0,%l1,%l2
addccc      %l0,1,%l2        
and         %l0,%l1,%l2
and         %l0,1,%l2
andcc       %l0,%l1,%l2
andcc       %l0,1,%l2        
andn        %l0,%l1,%l2
andn        %l0,1,%l2
andncc      %l0,%l1,%l2
andncc      %l0,1,%l2        
ba          %icc,loc
bn          %icc,loc
bne         %icc,loc
bnz         %icc,loc        ! synonym
be          %icc,loc
bz          %icc,loc        ! synonym
bg          %icc,loc
ble         %icc,loc
bge         %icc,loc
bl          %icc,loc
bgu         %icc,loc
bleu        %icc,loc
bcc         %icc,loc
bgeu        %icc,loc        ! synonym
bcs         %icc,loc
blu         %icc,loc        ! synonym
bpos        %icc,loc
bneg        %icc,loc
bvc         %icc,loc
bvs         %icc,loc
baa         %icc,loc
bna         %icc,loc
bnea        %icc,loc
bnza        %icc,loc       ! synonym
bea         %icc,loc
bza         %icc,loc       ! synonym
bga         %icc,loc
blea        %icc,loc
bgea        %icc,loc
bla         %icc,loc
bgua        %icc,loc
bleua       %icc,loc
bcca        %icc,loc
bgeua       %icc,loc       ! synonym
bcsa        %icc,loc
blua        %icc,loc       ! synonym
bposa       %icc,loc
bnega       %icc,loc
bvca        %icc,loc
bvsa        %icc,loc
bapt        %icc,loc
bnpt        %icc,loc
bnept       %icc,loc
bnzpt       %icc,loc       ! synonym
bept        %icc,loc
bzpt        %icc,loc       ! synonym
bgpt        %icc,loc
blept       %icc,loc
bgept       %icc,loc
blpt        %icc,loc
bgupt       %icc,loc
bleupt      %icc,loc
bccpt       %icc,loc
bgeupt      %icc,loc       ! synonym
bcspt       %icc,loc
blupt       %icc,loc       ! synonym
bpospt      %icc,loc
bnegpt      %icc,loc
bvcpt       %icc,loc
bvspt       %icc,loc
baapt       %icc,loc
bnapt       %icc,loc
bneapt      %icc,loc
bnzapt      %icc,loc      ! synonym
beapt       %icc,loc
bzapt       %icc,loc      ! synonym
bgapt       %icc,loc
bleapt      %icc,loc
bgeapt      %icc,loc
blapt       %icc,loc
bguapt      %icc,loc
bleuapt     %icc,loc
bccapt      %icc,loc
bgeuapt     %icc,loc      ! synonym
bcsapt      %icc,loc
bluapt      %icc,loc      ! synonym
bposapt     %icc,loc
bnegapt     %icc,loc
bvcapt      %icc,loc
bvsapt      %icc,loc
bapn        %icc,loc
bnpn        %icc,loc
bnepn       %icc,loc
bnzpn       %icc,loc     ! synonym
bepn        %icc,loc
bzpn        %icc,loc     ! synonym
bgpn        %icc,loc
blepn       %icc,loc
bgepn       %icc,loc
blpn        %icc,loc
bgupn       %icc,loc
bleupn      %icc,loc
bccpn       %icc,loc
bgeupn      %icc,loc     ! synonym
bcspn       %icc,loc
blupn       %icc,loc     ! synonym
bpospn      %icc,loc
bnegpn      %icc,loc
bvcpn       %icc,loc
bvspn       %icc,loc
baapn       %icc,loc
bnapn       %icc,loc
bneapn      %icc,loc
bnzapn      %icc,loc      ! synonym
beapn       %icc,loc
bzapn       %icc,loc      ! synonym
bgapn       %icc,loc
bleapn      %icc,loc
bgeapn      %icc,loc
blapn       %icc,loc
bguapn      %icc,loc
bleuapn     %icc,loc
bccapn      %icc,loc
bgeuapn     %icc,loc      ! synonym
bcsapn      %icc,loc
bluapn      %icc,loc      ! synonym
bposapn     %icc,loc
bnegapn     %icc,loc
bvcapn      %icc,loc
bvsapn      %icc,loc
ba          %xcc,loc
bn          %xcc,loc
bne         %xcc,loc
bnz         %xcc,loc      ! synonym
be          %xcc,loc
bz          %xcc,loc      ! synonym
bg          %xcc,loc
ble         %xcc,loc
bge         %xcc,loc
bl          %xcc,loc
bgu         %xcc,loc
bleu        %xcc,loc
bcc         %xcc,loc
bgeu        %xcc,loc      ! synonym
bcs         %xcc,loc
blu         %xcc,loc      ! synonym
bpos        %xcc,loc
bneg        %xcc,loc
bvc         %xcc,loc
bvs         %xcc,loc
baa         %xcc,loc
bna         %xcc,loc
bnea        %xcc,loc
bnza        %xcc,loc     ! synonym
bea         %xcc,loc
bza         %xcc,loc     ! synonym
bga         %xcc,loc
blea        %xcc,loc
bgea        %xcc,loc
bla         %xcc,loc
bgua        %xcc,loc
bleua       %xcc,loc
bcca        %xcc,loc
bgeua       %xcc,loc     ! synonym
bcsa        %xcc,loc
blua        %xcc,loc     ! synonym
bposa       %xcc,loc
bnega       %xcc,loc
bvca        %xcc,loc
bvsa        %xcc,loc
bapt        %xcc,loc
bnpt        %xcc,loc
bnept       %xcc,loc
bnzpt       %xcc,loc     ! synonym
bept        %xcc,loc
bzpt        %xcc,loc     ! synonym
bgpt        %xcc,loc
blept       %xcc,loc
bgept       %xcc,loc
blpt        %xcc,loc
bgupt       %xcc,loc
bleupt      %xcc,loc
bccpt       %xcc,loc
bgeupt      %xcc,loc     ! synonym
bcspt       %xcc,loc
blupt       %xcc,loc     ! synonym
bpospt      %xcc,loc
bnegpt      %xcc,loc
bvcpt       %xcc,loc
bvspt       %xcc,loc
baapt       %xcc,loc
bnapt       %xcc,loc
bneapt      %xcc,loc
bnzapt      %xcc,loc     ! synonym
beapt       %xcc,loc
beapt       %xcc,loc     ! synonym
bgapt       %xcc,loc
bleapt      %xcc,loc
bgeapt      %xcc,loc
blapt       %xcc,loc
bguapt      %xcc,loc
bleuapt     %xcc,loc
bccapt      %xcc,loc
bgeuapt     %xcc,loc     ! synonym
bcsapt      %xcc,loc
bluapt      %xcc,loc     ! synonym
bposapt     %xcc,loc
bnegapt     %xcc,loc
bvcapt      %xcc,loc
bvsapt      %xcc,loc
bapn        %xcc,loc
bnpn        %xcc,loc
bnepn       %xcc,loc
bnzpn       %xcc,loc     ! synonym
bepn        %xcc,loc
bzpn        %xcc,loc     ! synonym
bgpn        %xcc,loc
blepn       %xcc,loc
bgepn       %xcc,loc
blpn        %xcc,loc
bgupn       %xcc,loc
bleupn      %xcc,loc
bccpn       %xcc,loc
bgeupn      %xcc,loc     ! synonym
bcspn       %xcc,loc
blupn       %xcc,loc     ! synonym
bpospn      %xcc,loc
bnegpn      %xcc,loc
bvcpn       %xcc,loc
bvspn       %xcc,loc
baapn       %xcc,loc
bnapn       %xcc,loc
bneapn      %xcc,loc
bnzapn      %xcc,loc      ! synonym
beapn       %xcc,loc
bzapn       %xcc,loc      ! synonym
bgapn       %xcc,loc
bleapn      %xcc,loc
bgeapn      %xcc,loc
blapn       %xcc,loc
bguapn      %xcc,loc
bleuapn     %xcc,loc
bccapn      %xcc,loc
bgeuapn     %xcc,loc      ! synonym
bcsapn      %xcc,loc
bluapn      %xcc,loc      ! synonym
bposapn     %xcc,loc
bnegapn     %xcc,loc
bvcapn      %xcc,loc
bvsapn      %xcc,loc
ba          loc
bn          loc
bne         loc
bnz         loc          ! synonym
be          loc
bz          loc          ! synonym
bg          loc
ble         loc
bge         loc
bl          loc
bgu         loc
bleu        loc
bcc         loc
bgeu        loc         ! synonym
bcs         loc
blu         loc         ! synonym
bpos        loc
bneg        loc
bvc         loc
bvs         loc
baa         loc
bna         loc
bnea        loc
bnza        loc         ! synonym
bea         loc
bza         loc         ! synonym
bga         loc
blea        loc
bgea        loc
bla         loc
bgua        loc
bleua       loc
bcca        loc
bgeua       loc         ! synonym
bcsa        loc
blua        loc         ! synonym
bposa       loc
bnega       loc
bvca        loc
bvsa        loc
brz         %l0,loc
brlez       %l0,loc
brlz        %l0,loc
brnz        %l0,loc
brgz        %l0,loc
brgez       %l0,loc
brza        %l0,loc
brleza      %l0,loc
brlza       %l0,loc
brnza       %l0,loc
brgza       %l0,loc
brgeza      %l0,loc
brzpt       %l0,loc
brlezpt     %l0,loc
brlzpt      %l0,loc
brnzpt      %l0,loc
brgzpt      %l0,loc
brgezpt     %l0,loc
brzapt      %l0,loc
brlezapt    %l0,loc
brlzapt     %l0,loc
brnzapt     %l0,loc
brgzapt     %l0,loc
brgezapt    %l0,loc
brzpn       %l0,loc
brlezpn     %l0,loc
brlzpn      %l0,loc
brnzpn      %l0,loc
brgzpn      %l0,loc
brgezpn     %l0,loc
brzapn      %l0,loc
brlezapn    %l0,loc
brlzapn     %l0,loc
brnzapn     %l0,loc
brgzapn     %l0,loc
brgezapn    %l0,loc
btst        %l0,%l1        ! synthetic
btst        1,%l1          ! synthetic
bset        %l0,%l1        ! synthetic
bset        1,%l1          ! synthetic
bclr        %l0,%l1        ! synthetic
bclr        1,%l1          ! synthetic
btog        %l0,%l1        ! synthetic
btog        1,%l1          ! synthetic
call        loc
cas         [%l0],%l1,%l2  ! synthetic
casl        [%l0],%l1,%l2  ! synthetic
casx        [%l0],%l1,%l2  ! synthetic
casxl       [%l0],%l1,%l2  ! synthetic
casa        [%l0]1,%l1,%l2    
casa        [%l0]%asi,%l1,%l2    
casxa       [%l0]1,%l1,%l2   
casxa       [%l0]%asi,%l1,%l2   
clr         %l0            ! synthetic       
clrb        [dat]          ! synthetic
clrb        [%l0]          ! synthetic
clrb        [%l0+%l1]      ! synthetic
clrb        [%l0+1]        ! synthetic
clrh        [dat]          ! synthetic
clrh        [%l0]          ! synthetic
clrh        [%l0+%l1]      ! synthetic
clrh        [%l0+1]        ! synthetic
clr         [dat]          ! synthetic
clr         [%l0]          ! synthetic
clr         [%l0+%l1]      ! synthetic
clr         [%l0+1]        ! synthetic
clrx        [dat]          ! synthetic
clrx        [%l0]          ! synthetic
clrx        [%l0+%l1]      ! synthetic
clrx        [%l0+1]        ! synthetic
cmp         %l0,%l1        ! synthetic
dec         %l0             ! synthetic
dec         1,%l0           ! synthetic
deccc       %l0             ! synthetic
deccc       1,%l0           ! synthetic
done               
fabss       %f0,%f1    
fabsd       %f0,%f2    
fabsq       %f0,%f1    
fadds       %f0,%f1,%f2
faddd       %f0,%f2,%f4
faddq       %f0,%f4,%f8
fba         loc
fbn         loc
fbu         loc
fbg         loc
fbug        loc
fbl         loc
fbul        loc
fblg        loc
fbne        loc
fbnz        loc          ! synonym
fbe         loc
fbz         loc          ! synonym
fbue        loc
fbge        loc
fbge        loc
fbuge       loc
fble        loc
fbule       loc
fbo         loc
fbaa        loc
fbna        loc
fbua        loc
fbga        loc
fbuga       loc
fbla        loc
fbula       loc
fblga       loc
fbnea       loc
fbnza       loc          ! synonym
fbea        loc
fbza        loc          ! synonym
fbuea       loc
fbgea       loc
fbgea       loc
fbugea      loc
fblea       loc
fbulea      loc
fboa        loc
fba         %fcc0,loc
fbn         %fcc0,loc
fbu         %fcc0,loc
fbg         %fcc0,loc
fbug        %fcc0,loc
fbl         %fcc0,loc
fbul        %fcc0,loc
fblg        %fcc0,loc
fbne        %fcc0,loc
fbnz        %fcc0,loc        ! synonym
fbe         %fcc0,loc
fbz         %fcc0,loc        ! synonym
fbue        %fcc0,loc
fbge        %fcc0,loc
fbge        %fcc0,loc
fbuge       %fcc0,loc
fble        %fcc0,loc
fbule       %fcc0,loc
fbo         %fcc0,loc
fbaa        %fcc0,loc
fbna        %fcc0,loc
fbua        %fcc0,loc
fbga        %fcc0,loc
fbuga       %fcc0,loc
fbla        %fcc0,loc
fbula       %fcc0,loc
fblga       %fcc0,loc
fbnea       %fcc0,loc
fbnza       %fcc0,loc       ! synonym
fbea        %fcc0,loc
fbza        %fcc0,loc       ! synonym
fbuea       %fcc0,loc
fbgea       %fcc0,loc
fbgea       %fcc0,loc
fbugea      %fcc0,loc
fblea       %fcc0,loc
fbulea      %fcc0,loc
fboa        %fcc0,loc
fbapt       %fcc0,loc
fbnpt       %fcc0,loc
fbupt       %fcc0,loc
fbgpt       %fcc0,loc
fbugpt      %fcc0,loc
fblpt       %fcc0,loc
fbulpt      %fcc0,loc
fblgpt      %fcc0,loc
fbnept      %fcc0,loc
fbnzpt      %fcc0,loc       ! synonym
fbept       %fcc0,loc
fbzpt       %fcc0,loc       ! synonym
fbuept      %fcc0,loc
fbgept      %fcc0,loc
fbgept      %fcc0,loc
fbugept     %fcc0,loc
fblept      %fcc0,loc
fbulept     %fcc0,loc
fbopt       %fcc0,loc
fbaapt      %fcc0,loc
fbnapt      %fcc0,loc
fbuapt      %fcc0,loc
fbgapt      %fcc0,loc
fbugapt     %fcc0,loc
fblapt      %fcc0,loc
fbulapt     %fcc0,loc
fblgapt     %fcc0,loc
fbneapt     %fcc0,loc
fbnzapt     %fcc0,loc       ! synonym
fbeapt      %fcc0,loc
fbzapt      %fcc0,loc       ! synonym
fbueapt     %fcc0,loc
fbgeapt     %fcc0,loc
fbgeapt     %fcc0,loc
fbugeapt    %fcc0,loc
fbleapt     %fcc0,loc
fbuleapt    %fcc0,loc
fboapt      %fcc0,loc
fbapn       %fcc0,loc
fbnpn       %fcc0,loc
fbupn       %fcc0,loc
fbgpn       %fcc0,loc
fbugpn      %fcc0,loc
fblpn       %fcc0,loc
fbulpn      %fcc0,loc
fblgpn      %fcc0,loc
fbnepn      %fcc0,loc
fbnzpn      %fcc0,loc       ! synonym
fbepn       %fcc0,loc
fbzpn       %fcc0,loc       ! synonym
fbuepn      %fcc0,loc
fbgepn      %fcc0,loc
fbgepn      %fcc0,loc
fbugepn     %fcc0,loc
fblepn      %fcc0,loc
fbulepn     %fcc0,loc
fbopn       %fcc0,loc
fbaapn      %fcc0,loc
fbnapn      %fcc0,loc
fbuapn      %fcc0,loc
fbgapn      %fcc0,loc
fbugapn     %fcc0,loc
fblapn      %fcc0,loc
fbulapn     %fcc0,loc
fblgapn     %fcc0,loc
fbneapn     %fcc0,loc
fbnzapn     %fcc0,loc        ! synonym
fbeapn      %fcc0,loc
fbzapn      %fcc0,loc        ! synonym
fbueapn     %fcc0,loc
fbgeapn     %fcc0,loc
fbgeapn     %fcc0,loc
fbugeapn    %fcc0,loc
fbleapn     %fcc0,loc
fbuleapn    %fcc0,loc
fboapn      %fcc0,loc
fcmps       %fcc0,%f0,%f1
fcmpd       %fcc0,%f0,%f2
fcmpq       %fcc0,%f0,%f4
fcmpes      %fcc0,%f0,%f1
fcmped      %fcc0,%f0,%f2
fcmpeq      %fcc0,%f0,%f4
fdivs       %f0,%f1,%f2
fdivd       %f0,%f2,%f4
fdivq       %f0,%f4,%f8
fdmulq      %f0,%f2,%f4          
fitos       %f0,%f1
fitod       %f0,%f2
fitoq       %f0,%f4
flush       1
flush       %l0
flush       %l0+%l1
flush       %l0+1
flushw                
fmovs       %f0,%f1
fmovd       %f0,%f2
fmovq       %f0,%f4
fmovsa      %icc,%f0,%f1
fmovsn      %icc,%f0,%f1
fmovsne     %icc,%f0,%f1
fmovsnz     %icc,%f0,%f1     ! synonym
fmovse      %icc,%f0,%f1
fmovsz      %icc,%f0,%f1     ! synonym
fmovsg      %icc,%f0,%f1
fmovsle     %icc,%f0,%f1
fmovsge     %icc,%f0,%f1
fmovsl      %icc,%f0,%f1
fmovsgu     %icc,%f0,%f1
fmovsleu    %icc,%f0,%f1
fmovscc     %icc,%f0,%f1
fmovsgeu    %icc,%f0,%f1    ! synonym
fmovscs     %icc,%f0,%f1
fmovslu     %icc,%f0,%f1    ! synonym
fmovspos    %icc,%f0,%f1
fmovsneg    %icc,%f0,%f1
fmovsvc     %icc,%f0,%f1
fmovsvs     %icc,%f0,%f1
fmovda      %icc,%f0,%f1
fmovdn      %icc,%f0,%f1
fmovdne     %icc,%f0,%f1
fmovdnz     %icc,%f0,%f1      ! synonym
fmovde      %icc,%f0,%f1
fmovdz      %icc,%f0,%f1      ! synonym
fmovdg      %icc,%f0,%f1
fmovdle     %icc,%f0,%f1
fmovdge     %icc,%f0,%f1
fmovdl      %icc,%f0,%f1
fmovdgu     %icc,%f0,%f1
fmovdleu    %icc,%f0,%f1
fmovdcc     %icc,%f0,%f1
fmovdgeu    %icc,%f0,%f1      ! synonym
fmovdcs     %icc,%f0,%f1
fmovdlu     %icc,%f0,%f1      ! synonym
fmovdpos    %icc,%f0,%f1
fmovdneg    %icc,%f0,%f1
fmovdvc     %icc,%f0,%f1
fmovdvs     %icc,%f0,%f1
fmovqa      %icc,%f0,%f1
fmovqn      %icc,%f0,%f1
fmovqne     %icc,%f0,%f1
fmovqnz     %icc,%f0,%f1     ! synonym
fmovqe      %icc,%f0,%f1
fmovqz      %icc,%f0,%f1     ! synonym
fmovqg      %icc,%f0,%f1
fmovqle     %icc,%f0,%f1
fmovqge     %icc,%f0,%f1
fmovql      %icc,%f0,%f1
fmovqgu     %icc,%f0,%f1
fmovqleu    %icc,%f0,%f1
fmovqcc     %icc,%f0,%f1
fmovqgeu    %icc,%f0,%f1     ! synonym
fmovqcs     %icc,%f0,%f1
fmovqlu     %icc,%f0,%f1     ! synonym
fmovqpos    %icc,%f0,%f1
fmovqneg    %icc,%f0,%f1
fmovqvc     %icc,%f0,%f1
fmovqvs     %icc,%f0,%f1
fmovsa      %fcc0,%f0,%f1
fmovsn      %fcc0,%f0,%f1
fmovsu      %fcc0,%f0,%f1
fmovsg      %fcc0,%f0,%f1
fmovsug     %fcc0,%f0,%f1
fmovsl      %fcc0,%f0,%f1
fmovsul     %fcc0,%f0,%f1
fmovslg     %fcc0,%f0,%f1
fmovsne     %fcc0,%f0,%f1   
fmovsnz     %fcc0,%f0,%f1      ! synonym
fmovse      %fcc0,%f0,%f1
fmovsz      %fcc0,%f0,%f1      ! synonym
fmovsue     %fcc0,%f0,%f1
fmovsge     %fcc0,%f0,%f1
fmovsuge    %fcc0,%f0,%f1
fmovsle     %fcc0,%f0,%f1
fmovsule    %fcc0,%f0,%f1
fmovso      %fcc0,%f0,%f1
fmovda      %fcc0,%f0,%f1
fmovdn      %fcc0,%f0,%f1
fmovdu      %fcc0,%f0,%f1
fmovdg      %fcc0,%f0,%f1
fmovdug     %fcc0,%f0,%f1
fmovdl      %fcc0,%f0,%f1
fmovdul     %fcc0,%f0,%f1
fmovdlg     %fcc0,%f0,%f1
fmovdne     %fcc0,%f0,%f1
fmovdnz     %fcc0,%f0,%f1      ! synonym
fmovde      %fcc0,%f0,%f1
fmovdz      %fcc0,%f0,%f1      ! synonym
fmovdue     %fcc0,%f0,%f1
fmovdge     %fcc0,%f0,%f1
fmovduge    %fcc0,%f0,%f1
fmovdle     %fcc0,%f0,%f1
fmovdule    %fcc0,%f0,%f1
fmovdo      %fcc0,%f0,%f1
fmovqa      %fcc0,%f0,%f1
fmovqn      %fcc0,%f0,%f1
fmovqu      %fcc0,%f0,%f1
fmovqg      %fcc0,%f0,%f1
fmovqug     %fcc0,%f0,%f1
fmovql      %fcc0,%f0,%f1
fmovqul     %fcc0,%f0,%f1
fmovqlg     %fcc0,%f0,%f1
fmovqne     %fcc0,%f0,%f1
fmovqnz     %fcc0,%f0,%f1     ! synonym
fmovqe      %fcc0,%f0,%f1
fmovqz      %fcc0,%f0,%f1     ! synonym
fmovque     %fcc0,%f0,%f1
fmovqge     %fcc0,%f0,%f1
fmovquge    %fcc0,%f0,%f1
fmovqle     %fcc0,%f0,%f1
fmovqule    %fcc0,%f0,%f1
fmovqo      %fcc0,%f0,%f1
fmovrse     %l0,%f0,%f1
fmovrsz     %l0,%f0,%f1       ! synonym
fmovrslez   %l0,%f0,%f1
fmovrslz    %l0,%f0,%f1
fmovrsne    %l0,%f0,%f1
fmovrsnz    %l0,%f0,%f1       ! synonym
fmovrsgz    %l0,%f0,%f1
fmovrsgez   %l0,%f0,%f1
fmovrde     %l0,%f0,%f1
fmovrdz     %l0,%f0,%f1       ! synonym
fmovrdlez   %l0,%f0,%f1
fmovrdlz    %l0,%f0,%f1
fmovrdne    %l0,%f0,%f1
fmovrdnz    %l0,%f0,%f1       ! synonym
fmovrdgz    %l0,%f0,%f1
fmovrdgez   %l0,%f0,%f1
fmovrqe     %l0,%f0,%f1
fmovrqz     %l0,%f0,%f1       ! synonym
fmovrqlez   %l0,%f0,%f1
fmovrqlz    %l0,%f0,%f1
fmovrqne    %l0,%f0,%f1
fmovrqnz    %l0,%f0,%f1       ! synonym
fmovrqgz    %l0,%f0,%f1
fmovrqgez   %l0,%f0,%f1
fmuls       %f0,%f1,%f2
fmuld       %f0,%f2,%f4
fmulq       %f0,%f4,%f8
fnegs       %f0,%f1
fnegd       %f0,%f2
fnegq       %f0,%f4
fsmuld      %f0,%f2,%f4               
fsqrts      %f0,%f1
fsqrtd      %f0,%f2
fsqrtq      %f0,%f4
fstoi       %f0,%f1
fdtoi       %f0,%f1
fqtoi       %f0,%f1
fstod       %f0,%f2
fstoq       %f0,%f4
fdtos       %f0,%f1
fdtoq       %f0,%f4
fqtos       %f0,%f1
fqtod       %f0,%f2
fstox       %f0,%f2
fdtox       %f0,%f2
fqtox       %f0,%f2
fsubs       %f0,%f1,%f2
fsubd       %f0,%f2,%f4
fsubq       %f0,%f4,%f8
fxtos       %f0,%f1
fxtod       %f0,%f2
fxtoq       %f0,%f4
illtrap     1           
impdep1               
impdep2
inc         %l0             ! synthetic
inc         1,%l0           ! synthetic
inccc       %l0             ! synthetic
inccc       1,%l0           ! synthetic
iprefetch   loc             ! synthetic
jmp         loc             ! synthetic   
jmpl        loc,%l0
ldd         [dat],%l0
ldd         [%l0],%l0
ldd         [%l0+%l1],%l0
ldd         [%l0+1],%l0
ldda        [%l0]1,%l0
ldda        [%l0+%l1]1,%l0
ldda        [dat]%asi,%l0
ldda        [%l0+1]%asi,%l0
ldd         [dat],%f0
ldd         [%l0],%f0
ldd         [%l0+%l1],%f0
ldd         [%l0+1],%f0
ldda        [%l0]1,%f0
ldda        [%l0+%l1]1,%f0
ldda        [dat]%asi,%f0
ldda        [%l0+1]%asi,%f0
ld          [dat],%f0
ld          [%l0],%f0
ld          [%l0+%l1],%f0
ld          [%l0+1],%f0
lda         [%l0]1,%f0
lda         [%l0+%l1]1,%f0
lda         [dat]%asi,%f0
lda         [%l0+1]%asi,%f0
ld          [dat],%fsr
ld          [%l0],%fsr
ld          [%l0+%l1],%fsr
ld          [%l0+1],%fsr
ldq         [dat],%f0
ldq         [%l0],%f0
ldq         [%l0+%l1],%f0
ldq         [%l0+1],%f0
ldqa        [%l0]1,%f0
ldqa        [%l0+%l1]1,%f0
ldqa        [dat]%asi,%f0
ldqa        [%l0+1]%asi,%f0
ldsb        [dat],%l0
ldsb        [%l0],%l0
ldsb        [%l0+%l1],%l0
ldsb        [%l0+1],%l0
ldsba       [%l0]1,%l0
ldsba       [%l0+%l1]1,%l0
ldsba       [dat]%asi,%l0
ldsba       [%l0+1]%asi,%l0   
ldsh        [dat],%l0           
ldsh        [%l0],%l0
ldsh        [%l0+%l1],%l0
ldsh        [%l0+1],%l0
ldsha       [%l0]1,%l0
ldsha       [%l0+%l1]1,%l0
ldsha       [dat]%asi,%l0
ldsha       [%l0+1]%asi,%l0   
ldstub      [dat],%l0           
ldstub      [%l0],%l0
ldstub      [%l0+%l1],%l0
ldstub      [%l0+1],%l0
ldstuba     [%l0]1,%l0
ldstuba     [%l0+%l1]1,%l0
ldstuba     [dat]%asi,%l0
ldstuba     [%l0+1]%asi,%l0   
ldsw        [dat],%l0
ldsw        [%l0],%l0
ldsw        [%l0+%l1],%l0
ldsw        [%l0+1],%l0
ldswa       [%l0]1,%l0
ldswa       [%l0+%l1]1,%l0
ldswa       [dat]%asi,%l0
ldswa       [%l0+1]%asi,%l0   
ldub        [dat],%l0  
ldub        [%l0],%l0
ldub        [%l0+%l1],%l0
ldub        [%l0+1],%l0
lduba       [%l0]1,%l0
lduba       [%l0+%l1]1,%l0
lduba       [dat]%asi,%l0
lduba       [%l0+1]%asi,%l0       
lduh        [dat],%l0           
lduh        [%l0],%l0
lduh        [%l0+%l1],%l0
lduh        [%l0+1],%l0
lduha       [%l0]1,%l0
lduha       [%l0+%l1]1,%l0
lduha       [dat]%asi,%l0
lduha       [%l0+1]%asi,%l0       
lduw        [dat],%l0
lduw        [%l0],%l0
lduw        [%l0+%l1],%l0
lduw        [%l0+1],%l0
lduwa       [%l0]1,%l0
lduwa       [%l0+%l1]1,%l0
lduwa       [dat]%asi,%l0
lduwa       [%l0+1]%asi,%l0       
ld          [dat],%l0          ! synonym
ld          [%l0],%l0          ! synonym
ld          [%l0+%l1],%l0      ! synonym
ld          [%l0+1],%l0        ! synonym
lda         [%l0]1,%l0         ! synonym
lda         [%l0+%l1]1,%l0     ! synonym
lda         [dat]%asi,%l0      ! synonym
lda         [%l0+1]%asi,%l0    ! synonym
ldx         [dat],%l0
ldx         [%l0],%l0
ldx         [%l0+%l1],%l0
ldx         [%l0+1],%l0
ldxa        [%l0]1,%l0
ldxa        [%l0+%l1]1,%l0
ldxa        [dat]%asi,%l0
ldxa        [%l0+1]%asi,%l0       
ldx         [dat],%fsr
ldx         [%l0],%fsr
ldx         [%l0+%l1],%fsr
ldx         [%l0+1],%fsr
membar      1
mov         %l0,%l1            ! synthetic
mov         1,%l0              ! synthetic
mov         %y,%l0             ! synthetic
mov         %asr7,%l0          ! synthetic
mov         %l0,%y             ! synthetic
mov         1,%y               ! synthetic
mov         %l0,%asr7          ! synthetic
mov         1,%asr7            ! synthetic
mova        %icc,%l0,%l1
mova        %icc,1,%l0
movn        %icc,%l0,%l1
movn        %icc,1,%l0
movne       %icc,%l0,%l1
movne       %icc,1,%l0
movnz       %icc,%l0,%l1       ! synonym
movnz       %icc,1,%l0         ! synonym
move        %icc,%l0,%l1
move        %icc,1,%l0
movz        %icc,%l0,%l1       ! synonym
movz        %icc,1,%l0         ! synonym
movg        %icc,%l0,%l1
movg        %icc,1,%l0
movle       %icc,%l0,%l1
movle       %icc,1,%l0
movge       %icc,%l0,%l1
movge       %icc,1,%l0
movl        %icc,%l0,%l1
movl        %icc,1,%l0
movgu       %icc,%l0,%l1
movgu       %icc,1,%l0
movleu      %icc,%l0,%l1
movleu      %icc,1,%l0
movcc       %icc,%l0,%l1
movcc       %icc,1,%l0
movgeu      %icc,%l0,%l1      ! synonym
movgeu      %icc,1,%l0        ! synonym
movcs       %icc,%l0,%l1
movcs       %icc,1,%l0
movlu       %icc,%l0,%l1      ! synonym
movlu       %icc,1,%l0        ! synonym
movpos      %icc,%l0,%l1
movpos      %icc,1,%l0
movneg      %icc,%l0,%l1
movneg      %icc,1,%l0
movvc       %icc,%l0,%l1
movvc       %icc,1,%l0
movvs       %icc,%l0,%l1
movvs       %icc,1,%l0
mova        %xcc,%l0,%l1
mova        %xcc,1,%l0
movn        %xcc,%l0,%l1
movn        %xcc,1,%l0
movne       %xcc,%l0,%l1
movne       %xcc,1,%l0
movnz       %xcc,%l0,%l1     ! synonym
movnz       %xcc,1,%l0       ! synonym
move        %xcc,%l0,%l1
move        %xcc,1,%l0
movz        %xcc,%l0,%l1     ! synonym
movz        %xcc,1,%l0       ! synonym
movg        %xcc,%l0,%l1
movg        %xcc,1,%l0
movle       %xcc,%l0,%l1
movle       %xcc,1,%l0
movge       %xcc,%l0,%l1
movge       %xcc,1,%l0
movl        %xcc,%l0,%l1
movl        %xcc,1,%l0
movgu       %xcc,%l0,%l1
movgu       %xcc,1,%l0
movleu      %xcc,%l0,%l1
movleu      %xcc,1,%l0
movcc       %xcc,%l0,%l1
movcc       %xcc,1,%l0
movgeu      %xcc,%l0,%l1     ! synonym
movgeu      %xcc,1,%l0       ! synonym
movcs       %xcc,%l0,%l1
movcs       %xcc,1,%l0
movlu       %xcc,%l0,%l1
movlu       %xcc,1,%l0
movpos      %xcc,%l0,%l1
movpos      %xcc,1,%l0
movneg      %xcc,%l0,%l1
movneg      %xcc,1,%l0
movvc       %xcc,%l0,%l1
movvc       %xcc,1,%l0
movvs       %xcc,%l0,%l1
movvs       %xcc,1,%l0
movrne      %l0,%l1,%l2
movrne      %l0,1,%l2
movrnz      %l0,%l1,%l2     ! synonym
movrnz      %l0,1,%l2       ! synonym
movre       %l0,%l1,%l2
movre       %l0,1,%l2
movrz       %l0,%l1,%l2     ! synonym
movrz       %l0,1,%l2       ! synonym
movrgez     %l0,%l1,%l2
movrgez     %l0,1,%l2
movrlz      %l0,%l1,%l2
movrlz      %l0,1,%l2
movrlez     %l0,%l1,%l2
movrlez     %l0,1,%l2
movrgz      %l0,%l1,%l2
movrgz      %l0,1,%l2
mulscc      %l0,%l1,%l2
mulscc      %l0,1,%l1
mulx        %l0,%l1,%l2
mulx        %l0,1,%l1
neg         %l0,%l1        ! synthetic
neg         %l0            ! synthetic
nop
not         %l0,%l1        ! synthetic
not         %l0            ! synthetic
or          %l0,%l1,%l2
or          %l0,1,%l2
orcc        %l0,%l1,%l2
orcc        %l0,1,%l2
orn         %l0,%l1,%l2
orn         %l0,1,%l2
orncc       %l0,%l1,%l2
orncc       %l0,1,%l2
popc        %l0,%l1
popc        1,%l0
prefetch    [loc],1
prefetch    [%l0],1
prefetch    [%l0+%l1],1
prefetch    [%l0+1],2
prefetcha   [%l0]1,1
prefetcha   [%l0+%l1]1,1
prefetcha   [dat]%asi,1
prefetcha   [%l0+1]%asi,1
rd          %asi,%l0
rd          %asr7,%l0
rd          %ccr,%l0
rd          %fprs,%l0
rd          %pc,%l0
rdpr        %tpc,%l0
rdpr        %tnpc,%l0
rdpr        %tstate,%l0
rdpr        %tt,%l0
rdpr        %tick,%l0
rdpr        %tba,%l0
rdpr        %pstate,%l0
rdpr        %tl,%l0
rdpr        %pil,%l0
rdpr        %cwp,%l0
rdpr        %cansave,%l0
rdpr        %canrestore,%l0
rdpr        %cleanwin,%l0
rdpr        %otherwin,%l0
rdpr        %wstate,%l0
rdpr        %fq,%l0
rdpr        %ver,%l0
rd          %tick,%l0         
rd          %y,%l0  
restore     %l0,dat,%l1
restore     %l0,%l1,%l2
restore                   ! synthetic
restored     
retry
ret                       ! synthetic
retl                      ! synthetic
return      loc  
save        %l0,dat,%l1
save        %l0,%l1,%l2          
save                      ! synthetic          
saved           
sdiv        %l0,%l1,%l2
sdiv        %l0,1,%l1
sdivcc      %l0,%l1,%l2
sdivcc      %l0,1,%l1
sdivx       %l0,%l1,%l2
sdivx       %l0,1,%l1
sethi       1,%l0
set         1,%l0           ! synthetic
setuw       1,%l0           ! synthetic
setsw       1,%l0           ! synthetic
setx        1,%l0,%l1       ! synthetic
signx       %l0,%l1         ! synthetic
signx       %l0             ! synthetic
sir         1         
sll         %l0,%l1,%l2
sll         %l0,1,%l1
sllx        %l0,%l1,%l2
sllx        %l0,1,%l1
smul        %l0,%l1,%l2
smul        %l0,1,%l1 
smulcc      %l0,%l1,%l2
smulcc      %l0,1,%l1
sra         %l0,%l1,%l2
sra         %l0,1,%l1
srax        %l0,%l1,%l2
srax        %l0,1,%l1
srl         %l0,%l1,%l2
srl         %l0,1,%l1
srlx        %l0,%l1,%l2
srlx        %l0,1,%l1
stb         %l0,[dat]
stb         %l0,[%l0]
stb         %l0,[%l0+%l1]
stb         %l0,[%l0+1]
stba        %l0,[%l0]1
stba        %l0,[%l0+%l1]1
stba        %l0,[dat]%asi
stba        %l0,[%l0+1]%asi
stub        %l0,[dat]       ! synonym
stub        %l0,[%l0]       ! synonym
stub        %l0,[%l0+%l1]   ! synonym
stub        %l0,[%l0+1]     ! synonym
stuba       %l0,[%l0]1      ! synonym
stuba       %l0,[%l0+%l1]1  ! synonym
stuba       %l0,[dat]%asi   ! synonym
stuba       %l0,[%l0+1]%asi ! synonym
stsb        %l0,[dat]       ! synonym
stsb        %l0,[%l0]       ! synonym
stsb        %l0,[%l0+%l1]   ! synonym
stsb        %l0,[%l0+1]     ! synonym
stsba       %l0,[%l0]1      ! synonym
stsba       %l0,[%l0+%l1]1  ! synonym
stsba       %l0,[dat]%asi   ! synonym
stsba       %l0,[%l0+1]%asi ! synonym
stbar             
std         %l0,[dat]
std         %l0,[%l0]
std         %l0,[%l0+%l1]
std         %l0,[%l0+1]
stda        %l0,[%l0]1
stda        %l0,[%l0+%l1]1
stda        %l0,[dat]%asi
stda        %l0,[%l0+1]%asi
std         %f0,[dat]
std         %f0,[%l0]
std         %f0,[%l0+%l1]
std         %f0,[%l0+1]
stda        %f0,[%l0]1
stda        %f0,[%l0+%l1]1
stda        %f0,[dat]%asi
stda        %f0,[%l0+1]%asi     
st          %f0,[dat]
st          %f0,[%l0]
st          %f0,[%l0+%l1]
st          %f0,[%l0+1]
sta         %f0,[%l0]1
sta         %f0,[%l0+%l1]1
sta         %f0,[dat]%asi
sta         %f0,[%l0+1]%asi     
st          %fsr,[dat]
st          %fsr,[%l0]
st          %fsr,[%l0+%l1]
st          %fsr,[%l0+1]
sth         %l0,[dat]
sth         %l0,[%l0]
sth         %l0,[%l0+%l1]
sth         %l0,[%l0+1]
stha        %l0,[%l0]1
stha        %l0,[%l0+%l1]1
stha        %l0,[dat]%asi
stha        %l0,[%l0+1]%asi 
stuh        %l0,[dat]       ! synonym
stuh        %l0,[%l0]       ! synonym
stuh        %l0,[%l0+%l1]   ! synonym
stuh        %l0,[%l0+1]     ! synonym
stuha       %l0,[%l0]1      ! synonym
stuha       %l0,[%l0+%l1]1  ! synonym
stuha       %l0,[dat]%asi   ! synonym
stuha       %l0,[%l0+1]%asi ! synonym
stsh        %l0,[dat]       ! synonym
stsh        %l0,[%l0]       ! synonym
stsh        %l0,[%l0+%l1]   ! synonym
stsh        %l0,[%l0+1]     ! synonym
stsha       %l0,[%l0]1      ! synonym
stsha       %l0,[%l0+%l1]1  ! synonym
stsha       %l0,[dat]%asi   ! synonym
stsha       %l0,[%l0+1]%asi ! synonym
stq         %f0,[dat]
stq         %f0,[%l0]
stq         %f0,[%l0+%l1]
stq         %f0,[%l0+1]
stqa        %f0,[%l0]1
stqa        %f0,[%l0+%l1]1
stqa        %f0,[dat]%asi
stqa        %f0,[%l0+1]%asi     
stw         %l0,[dat]
stw         %l0,[%l0]
stw         %l0,[%l0+%l1]
stw         %l0,[%l0+1]
stwa        %l0,[%l0]1
stwa        %l0,[%l0+%l1]1
stwa        %l0,[dat]%asi
stwa        %l0,[%l0+1]%asi        
st          %l0,[dat]       ! synonym
st          %l0,[%l0]       ! synonym
st          %l0,[%l0+%l1]   ! synonym
st          %l0,[%l0+1]     ! synonym
sta         %l0,[%l0]1      ! synonym
sta         %l0,[%l0+%l1]1  ! synonym
sta         %l0,[dat]%asi   ! synonym
sta         %l0,[%l0+1]%asi ! synonym
stuw        %l0,[dat]       ! synonym
stuw        %l0,[%l0]       ! synonym
stuw        %l0,[%l0+%l1]   ! synonym
stuw        %l0,[%l0+1]     ! synonym
stuwa       %l0,[%l0]1      ! synonym
stuwa       %l0,[%l0+%l1]1  ! synonym
stuwa       %l0,[dat]%asi   ! synonym
stuwa       %l0,[%l0+1]%asi ! synonym
stsw        %l0,[dat]       ! synonym
stsw        %l0,[%l0]       ! synonym
stsw        %l0,[%l0+%l1]   ! synonym
stsw        %l0,[%l0+1]     ! synonym
stswa       %l0,[%l0]1      ! synonym
stswa       %l0,[%l0+%l1]1  ! synonym
stswa       %l0,[dat]%asi   ! synonym
stswa       %l0,[%l0+1]%asi ! synonym
stx         %l0,[dat]
stx         %l0,[%l0]
stx         %l0,[%l0+%l1]
stx         %l0,[%l0+1]
stxa        %l0,[%l0]1
stxa        %l0,[%l0+%l1]1
stxa        %l0,[dat]%asi
stxa        %l0,[%l0+1]%asi        
stx         %fsr,[dat]
stx         %fsr,[%l0]
stx         %fsr,[%l0+%l1]
stx         %fsr,[%l0+1]
sub         %l0,%l1,%l2
add         %l0,1,%l2
subcc       %l0,%l1,%l2 
subcc       %l0,1,%l2         
subc        %l0,%l1,%l2
subc        %l0,1,%l2
subccc      %l0,%l1,%l2
subccc      %l0,1,%l2        
swap        [dat],%l0
swap        [%l0],%l0
swap        [%l0+%l1],%l0
swap        [%l0+1],%l0
swapa       [%l0]1,%f0
swapa       [%l0+%l1]1,%f0
swapa       [dat]%asi,%f0
swapa       [%l0+1]%asi,%f0
taddcc      %l0,%l1,%l2
taddcc      %l0,1,%l1
taddcctv    %l0,%l1,%l2
taddcctv    %l0,1,%l1
ta          1
tn          1
tne         1
tnz         1               ! synonym
te          1
tz          1               ! synonym
tg          1
tle         1
tge         1
tl          1
tgu         1
tleu        1
tcc         1
tgeu        1               ! synonym
tcs         1
tlu         1               ! synonym
tpos        1
tneg        1
tvc         1
tvs         1
ta          %icc,1
tn          %icc,1
tne         %icc,1
tnz         %icc,1         ! synonym
te          %icc,1
tz          %icc,1         ! synonym
tg          %icc,1
tle         %icc,1
tge         %icc,1
tl          %icc,1
tgu         %icc,1
tleu        %icc,1
tcc         %icc,1
tgeu        %icc,1         ! synonym
tcs         %icc,1
tlu         %icc,1         ! synonym
tpos        %icc,1
tneg        %icc,1
tvc         %icc,1
tvs         %icc,1
ta          %xcc,1
tn          %xcc,1
tne         %xcc,1
tnz         %xcc,1        ! synonym
te          %xcc,1
tz          %xcc,1        ! synonym
tg          %xcc,1
tle         %xcc,1
tge         %xcc,1
tl          %xcc,1
tgu         %xcc,1
tleu        %xcc,1
tcc         %xcc,1
tgeu        %xcc,1        ! synonym
tcs         %xcc,1
tlu         %xcc,1        ! synonym
tpos        %xcc,1
tneg        %xcc,1
tvc         %xcc,1
tvs         %xcc,1
tst         %l0           ! synthetic
tsubcc      %l0,%l1,%l2
tsubcc      %l0,1,%l1
tsubcctv    %l0,%l1,%l2
tsubcctv    %l0,1,%l1
udiv        %l0,%l1,%l2
udiv        %l0,1,%l1
udivcc      %l0,%l1,%l2
udivcc      %l0,1,%l1
udivx       %l0,%l1,%l2
udivx       %l0,1,%l1
umul        %l0,%l1,%l2
umul        %l0,1,%l1 
umulcc      %l0,%l1,%l2
umulcc      %l0,1,%l1
wr          %l0,%l1,%asi
wr          %l0,1,%asi
wr          %l0,%l1,%asr7
wr          %l0,1,%asr7
wr          %l0,%l1,%ccr
wr          %l0,1,%ccr
wr          %l0,%l1,%fprs
wr          %l0,1,%fprs
wrpr        %l0,%l1,%tpc
wrpr        %l0,1,%tpc
wrpr        %l0,%l1,%tnpc
wrpr        %l0,1,%tnpc
wrpr        %l0,%l1,%tstate
wrpr        %l0,1,%tstate
wrpr        %l0,%l1,%tt
wrpr        %l0,1,%tt
wrpr        %l0,%l1,%tick
wrpr        %l0,1,%tick
wrpr        %l0,%l1,%tba
wrpr        %l0,1,%tba
wrpr        %l0,%l1,%pstate
wrpr        %l0,1,%pstate
wrpr        %l0,%l1,%tl
wrpr        %l0,1,%tl
wrpr        %l0,%l1,%pil
wrpr        %l0,1,%pil
wrpr        %l0,%l1,%cwp
wrpr        %l0,1,%cwp
wrpr        %l0,%l1,%cansave
wrpr        %l0,1,%cansave
wrpr        %l0,%l1,%canrestore
wrpr        %l0,1,%canrestore
wrpr        %l0,%l1,%cleanwin
wrpr        %l0,1,%cleanwin
wrpr        %l0,%l1,%otherwin
wrpr        %l0,1,%otherwin
wrpr        %l0,%l1,%wstate
wrpr        %l0,1,%wstate
wr          %l0,%l1,%y
wr          %l0,1,%y
xor         %l0,%l1,%l2
xor         %l0,1,%l2
xorcc       %l0,%l1,%l2
xorcc       %l0,1,%l2        
xnor        %l0,%l1,%l2
xnor        %l0,1,%l2
xnorcc      %l0,%l1,%l2
xnorcc      %l0,1,%l2

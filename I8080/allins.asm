!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                              !
!                          8080 ASSEMBLER TEST FILE                            !
!                                                                              !
! Contains all of the 8080 operations possible, in opcode numerical order.     !
! Listing in this way has two advantages. One, it is easier to look up by      !
! opcode, and two, it can be sorted into alphabetical order, whereas the       !
! converse is not possible.                                                    !
!                                                                              !
! This list was copied out of the Intel 8080 manual. To check proper           !
! assembler operation, this file is assembled, a listing containing all        !
! generated code is made, and the result hand checked verses the book.         !
!                                                                              !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

 nop
 lxi b,$1122
 stax b
 inx b
 inr b
 dcr b
 mvi b,$33
 rlc
 dad b
 ldax b
 dcx b
 inr c
 dcr c
 mvi c,$33
 rrc
 lxi d,$1122
 stax d
 inx d
 inr d
 dcr d
 mvi d,$33
 ral
 dad d
 ldax d
 dcx d
 inr e
 dcr e
 mvi e,$33
 rar
 lxi h,$1122
 shld $1122
 inx h
 inr h
 dcr h
 mvi h,$33
 daa
 dad h
 lhld $1122
 dcx h
 inr l
 dcr l
 mvi l,$33
 cma
 lxi sp,$1122
 sta $1122
 inx sp
 inr m
 dcr m
 mvi m,$1122
 stc
 dad sp
 lda $1122
 dcx sp
 inr a
 dcr a
 mvi a,$33
 cmc
 mov b,b
 mov b,c
 mov b,d
 mov b,e
 mov b,h
 mov b,l
 mov b,m
 mov b,a
 mov c,b
 mov c,c
 mov c,d
 mov c,e
 mov c,h
 mov c,l
 mov c,m
 mov c,a
 mov d,b
 mov d,c
 mov d,d
 mov d,e
 mov d,h
 mov d,l
 mov d,m
 mov d,a
 mov e,b
 mov e,c
 mov e,d
 mov e,e
 mov e,h
 mov e,l
 mov e,m
 mov e,a
 mov h,b
 mov h,c
 mov h,d
 mov h,e
 mov h,h
 mov h,l
 mov h,m
 mov h,a
 mov l,b
 mov l,c
 mov l,d
 mov l,e
 mov l,h
 mov l,l
 mov l,m
 mov l,a
 mov m,b
 mov m,c
 mov m,d
 mov m,e
 mov m,h
 mov m,l
 hlt
 mov m,a
 mov a,b
 mov a,c
 mov a,d
 mov a,e
 mov a,h
 mov a,l
 mov a,m
 mov a,a
 add b
 add c
 add d
 add e
 add h
 add l
 add m
 add a
 adc b
 adc c
 adc d
 adc e
 adc h
 adc l
 adc m
 adc a
 sub b
 sub c
 sub d
 sub e
 sub h
 sub l
 sub m
 sub a
 sbb b
 sbb c
 sbb d
 sbb e
 sbb h
 sbb l
 sbb m
 sbb a
 ana b
 ana c
 ana d
 ana e
 ana h
 ana l
 ana m
 ana a
 xra b
 xra c
 xra d
 xra e
 xra h
 xra l
 xra m
 xra a
 ora b
 ora c
 ora d
 ora e
 ora h
 ora l
 ora m
 ora a
 cmp b
 cmp c
 cmp d
 cmp e
 cmp h
 cmp l
 cmp m
 cmp a
 rnz
 pop b
 jnz $1122
 jmp $1122
 cnz $1122
 push b
 adi $33
 rst 0
 rz
 ret
 jz $1122
 cz $1122
 call $1122
 aci $33
 rst 1
 rnc
 pop d
 jnc $1122
 out $33
 cnc $1122
 push d
 sui $33
 rst 2
 rc
 jc $1122
 in $33
 cc $1122
 sbi $33
 rst 3
 rpo
 pop h
 jpo $1122
 xthl
 cpo $1122
 push h
 ani $33
 rst 4
 rpe
 pchl
 jpe $1122
 xchg
 cpe $1122
 xri $33
 rst 5
 rp
 pop psw
 jp $1122
 di
 cp $1122
 push psw
 ori $33
 rst 6
 rm
 sphl
 jm $1122
 ei
 cm $1122
 cpi $33
 rst 7

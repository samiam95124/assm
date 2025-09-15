!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
!                          80586 ASSEMBLER TEST FILE                          !
!                                                                             !
! Contains all of the 80586 operations possible, in alphabetical order (for   !
! the most part). Because of the instruction set complexity, the entire       !
! instruction set is not included verbatium, but separated into instructions  !
! and modes. Because the same code is used in all mode instruction fields,    !
! this is considered an adequate test. However, it makes it inadvisable to    !
! sort the list for binary order.                                             !
! Because the 80586 instruction set is modal, the test is repeated twice,     !
! once for each mode type (16 or 32 bit). This allows testing of the          !
! prefixes.                                                                   !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

locb:   equ     0
locw:   equ     1
locd:   equ     3
locq:   equ     7
loct:   equ     15

start:

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Perform the suite in large mode                                             !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        large
!
! First, all the instructions in alphabetical order
!
        aaa
        aad
        aam
        aas
        adc     [locb],al        
        adc     [locw],ax
        adc     [locd],eax
        adc     al,[locb]
        adc     ax,[locw]
        adc     eax,[locd]
        adc     al,$11
        adc     ax,$1122
        adc     eax,$11223344
        adcb    [locb],$11
        adcw    [locw],$1122
        adcd    [locd],$11223344
        adcw    [locw],$11
        adcd    [locd],$11
        add     al,$11
        add     ax,$1122
        add     eax,$11223344
        addb    [locb],$11
        addw    [locw],$1122
        addd    [locd],$11223344
        addw    [locw],$11
        addd    [locd],$11
        add     [locb],al
        add     [locw],ax
        add     [locd],eax
        add     bl,[locb]
        add     bx,[locw]
        add     ebx,[locd]
        and     [locb],al        
        and     [locw],ax
        and     [locd],eax
        and     al,[locb]
        and     ax,[locw]
        and     eax,[locd]
        and     al,$11
        and     ax,$1122
        and     eax,$11223344
        andb    [locb],$11
        andw    [locw],$1122
        andd    [locd],$11223344
        andw    [locw],$11
        andd    [locd],$11
        arpl    [locw],ax
        bound   ax,[locd]
        bound   eax,[locq]
        bsf     ax,[locw]
        bsf     eax,[locd]
        bsr     ax,[locw]
        bsr     eax,[locd]
        bswap   eax
        bt      [locw],ax
        bt      [locd],eax
        btw     [locw],$11
        btd     [locd],$11
        btc     [locw],ax
        btc     [locd],eax
        btcw    [locw],$11
        btcd    [locd],$11
        btr     [locw],ax
        btr     [locd],eax
        btrw    [locw],$11
        btrd    [locd],$11
        bts     [locw],ax
        bts     [locd],eax
        btsw    [locw],$11
        btsd    [locd],$11
        call    start
        callm   [locw]
        calll   [locd]
        cbw
        cdq
        clc
        cld
        cli
        clts
        cmc
        cmp     al,$11
        cmp     ax,$1122
        cmp     eax,$11223344
        cmpb    [locb],$11
        cmpw    [locw],$1122
        cmpd    [locd],$11223344
        cmpw    [locw],$11
        cmpd    [locd],$11
        cmp     [locb],al
        cmp     [locw],ax
        cmp     [locd],eax
        cmp     bl,[locb]
        cmp     bx,[locw]
        cmp     ebx,[locd]
        cmpsb
        cmpsw
        cmpsd
        cmpxchg [locb],al
        cmpxchg [locw],ax
        cwd
        cwde
        daa
        das
        decb    [locb]
        decw    [locw]
        decd    [locd]
        dec     ax
        dec     eax        
        divb    [locb]
        divw    [locw]
        divd    [locd]
        enter   $1122,$33
        hlt
        idivb   [locb]
        idivw   [locw]
        idivd   [locd]
        imulb   [locb]
        imulw   [locw]
        imuld   [locd]
        imul    ax,[locw]
        imul    eax,[locd]
        imul    ax,[locw],$11
        imul    eax,[locd],$11
        imul    ax,$11
        imul    eax,$11
        imul    ax,[locw],$1122
        imul    eax,[locd],$11223344
        imul    ax,$1122
        imul    eax,$11223344
        in      al,$11
        in      ax,$11
        in      eax,$11
        in      al,dx
        in      ax,dx
        in      eax,dx
        incb    [locb]
        incw    [locw]
        incd    [locd]
        inc     ax
        inc     eax
        insb
        insw
        insd
        int     3
        int     $11
        into
        invd
        invlpg  [locw]
        iret
        iretd
jmprel1:
        ja      jmprel1
        jae     jmprel1
        jb      jmprel1
        jbe     jmprel1
        jc      jmprel1
        jcxz    jmprel1
        jecxz   jmprel1
        je      jmprel1
        jz      jmprel1
        jg      jmprel1
        jge     jmprel1
        jl      jmprel1
        jle     jmprel1
        jna     jmprel1
        jnae    jmprel1
        jnb     jmprel1
        jnbe    jmprel1
        jnc     jmprel1
        jne     jmprel1
        jng     jmprel1
        jnge    jmprel1
        jnl     jmprel1
        jnle    jmprel1
        jno     jmprel1
        jnp     jmprel1
        jns     jmprel1
        jnz     jmprel1
        jo      jmprel1
        jp      jmprel1
        jpe     jmprel1
        jpo     jmprel1
        js      jmprel1
        jz      jmprel1
        jmp     jmprel1
        jmp     start
        jmpm    [locw]
        jmpl    [locd]
        lahf
        lar     ax,[locw]
        lar     eax,[locd]
        lea     ax,[di]
        lea     eax,[edi]
        leave
        lgdtd   [locq]
        lidtd   [locq]
        lldt    [locw]
        lmsw    [locw]
        lock
        lodsb
        lodsw
        lodsd
looprel1:
        loop    looprel1
        loope   looprel1
        loopz   looprel1
        loopne  looprel1
        loopnz  looprel1
        lsl     ax,[locw]
        lsl     eax,[locd]
        ltr     [locw]
        mov     [bx+si],al
        mov     [bx+si],ax
        mov     [ebx+esi],eax
        mov     al,[bx+si]
        mov     ax,[bx+si]
        mov     eax,[ebx+esi]
        mov     [locw],ds
        mov     ds,[locw]
        mov     al,[locb]
        mov     ax,[locw]
        mov     eax,[locd]
        mov     [locb],al
        mov     [locw],ax
        mov     [locd],eax
        mov     al,$11
        mov     ax,$1122
        mov     eax,$11223344
        movb    [locb],$11
        movw    [locw],$1122
        movd    [locd],$11223344
        mov     cr0,eax
        mov     eax,cr0
        mov     dr0,eax
        mov     eax,dr0
        mov     tr6,eax
        mov     eax,tr6
        movsb
        movsw
        movsd
        movsxb  ax,[locb]
        movsxb  eax,[locb]
        movsxw  eax,[locw]
        movzxb  ax,[locb]
        movzxb  eax,[locb]
        movzxw  eax,[locw]
        mulb    [locb]
        mulw    [locw]
        muld    [locd]
        negb    [locb]
        negw    [locw]
        negd    [locd]
        nop
        notb    [locb]
        notw    [locw]
        notd    [locd]
        or      al,$11
        or      ax,$1122
        or      eax,$11223344
        orb     [locb],$11
        orw     [locw],$1122
        ord     [locd],$11223344
        orw     [locw],$11
        ord     [locd],$11
        or      [locb],al
        or      [locw],ax
        or      [locd],eax
        or      ah,[locb]
        or      bx,[locw]
        or      ebx,[locd]
        out     $11,al
        out     $11,ax
        out     $11,eax
        out     dx,al
        out     dx,ax
        out     dx,eax
        outsb
        outsw
        outsd
        popw    [locw]
        popd    [locd]
        pop     ax
        pop     eax
        pop     ds
        pop     es
        pop     ss
        pop     fs
        pop     gs
        popa
        popad
        popf
        popfd
        pushw   [locw]
        pushd   [locd]
        push    ax
        push    eax
        pushd   $11
        pushw   $1122
        pushd   $11223344
        push    cs
        push    ss
        push    ds
        push    es
        push    fs
        push    gs
        pusha
        pushad
        pushf
        pushfd
        rclb    [locb],1
        rclb    [locb],cl
        rclb    [locb],$11
        rclw    [locw],1
        rclw    [locw],cl
        rclw    [locw],$11
        rcld    [locd],1
        rcld    [locd],cl
        rcld    [locd],$11
        rcrb    [locb],1
        rcrb    [locb],cl
        rcrb    [locb],$11
        rcrw    [locw],1
        rcrw    [locw],cl
        rcrw    [locw],$11
        rcrd    [locd],1
        rcrd    [locd],cl
        rcrd    [locd],$11
        rolb    [locb],1
        rolb    [locb],cl
        rolb    [locb],$11
        rolw    [locw],1
        rolw    [locw],cl
        rolw    [locw],$11
        rold    [locd],1
        rold    [locd],cl
        rold    [locd],$11
        rorb    [locb],1
        rorb    [locb],cl
        rorb    [locb],$11
        rorw    [locw],1
        rorw    [locw],cl
        rorw    [locw],$11
        rord    [locd],1
        rord    [locd],cl
        rord    [locd],$11
        rep
        repe
        repz
        repne
        repnz
        ret
        ret     $1122
        sahf
        salb    [locb],1
        salb    [locb],cl
        salb    [locb],$11
        salw    [locw],1
        salw    [locw],cl
        salw    [locw],$11
        sald    [locd],1
        sald    [locd],cl
        sald    [locd],$11
        sarb    [locb],1
        sarb    [locb],cl
        sarb    [locb],$11
        sarw    [locw],1
        sarw    [locw],cl
        sarw    [locw],$11
        sard    [locd],1
        sard    [locd],cl
        sard    [locd],$11
        shlb    [locb],1
        shlb    [locb],cl
        shlb    [locb],$11
        shlw    [locw],1
        shlw    [locw],cl
        shlw    [locw],$11
        shld    [locd],1
        shld    [locd],cl
        shld    [locd],$11
        shrb    [locb],1
        shrb    [locb],cl
        shrb    [locb],$11
        shrw    [locw],1
        shrw    [locw],cl
        shrw    [locw],$11
        shrd    [locd],1
        shrd    [locd],cl
        shrd    [locd],$11
        sbb     al,$11
        sbb     ax,$1122
        sbb     eax,$11223344
        sbbb    [locb],$11
        sbbw    [locw],$1122
        sbbd    [locd],$11223344
        sbbw    [locw],$11
        sbbd    [locd],$11
        sbbb    [locb],al
        sbbw    [locw],ax
        sbbd    [locd],eax
        sbb     al,[locb]
        sbb     ax,[locw]
        sbb     eax,[locd]
        scasb
        scasw
        scasd
        seta    [locb]
        setae   [locb]
        setb    [locb]
        setbe   [locb]
        setc    [locb]
        sete    [locb]
        setg    [locb]
        setge   [locb]
        setl    [locb]
        setle   [locb]
        setna   [locb]
        setnae  [locb]
        setnb   [locb]
        setnbe  [locb]
        setnc   [locb]
        setne   [locb]
        setng   [locb]
        setnge  [locb]
        setnl   [locb]
        setnle  [locb]
        setno   [locb]
        setnp   [locb]
        setns   [locb]
        setnz   [locb]
        seto    [locb]
        setp    [locb]
        setpe   [locb]
        setpo   [locb]
        sets    [locb]
        setz    [locb]
        sgdtd   [locq]
        sidtd   [locq]
        shldw   [locw],ax,$11
        shldd   [locd],eax,$11
        shldw   [locw],ax,cl
        shldd   [locd],eax,cl
        shrdw   [locw],ax,$11
        shrdd   [locd],eax,$11
        shrdw   [locw],ax,cl
        shrdd   [locd],eax,cl
        sldt    [locw]
        smsw    [locw]
        stc
        std
        sti
        stosb
        stosw
        stosd
        str     [locw]
        sub     al,$11
        sub     ax,$1122
        sub     eax,$11223344
        subb    [locb],$11
        subw    [locw],$1122
        subd    [locd],$11223344
        subw    [locw],$11
        subd    [locd],$11
        sub     [locb],al
        sub     [locw],ax
        sub     [locd],eax
        sub     ah,[locb]
        sub     ax,[locw]
        sub     eax,[locd]
        test    al,$11
        test    ax,$1122
        test    eax,$11223344
        testb   [locb],$11
        testw   [locw],$1122
        testd   [locd],$11223344
        test    [locb],al
        test    [locw],ax
        test    [locd],eax
        verr    [locw]
        verw    [locw]
        wait
        wbinvd
        xadd    [locb],al
        xadd    [locw],ax
        xadd    [locd],eax
        xchg    [locb],al
        xchg    al,[locb]
        xchg    [locw],ax
        xchg    ax,[locw]
        xchg    [locd],eax
        xchg    eax,[locd]
        xchg    ax,bx
        xchg    bx,ax
        xchg    eax,ebx
        xchg    ebx,eax
        xlatb
        xor     al,$11
        xor     ax,$1122
        xor     eax,$11223344
        xorb    [locb],$11
        xorw    [locw],$1122
        xord    [locd],$11223344
        xorw    [locw],$11
        xord    [locd],$11
        xor     [locb],al
        xor     [locw],ax
        xor     [locd],eax
        xor     bl,[locb]
        xor     bx,[locw]
        xor     ebx,[locd]
        f2xm1
        fabs
        fadd    st,st(1)
        fadd    st(1),st
        fadds   [locd]
        faddd   [locq]
        faddp   st(2),st
        fbld    [loct]
        fbstp   [loct]
        fchs
        fclex
        fnclex
        fcom    st(1)
        fcoms   [locd]
        fcomd   [locq]
        fcomp   st(1)
        fcomps  [locd]
        fcompd  [locq]
        fcompp
        fcos
        fdecstp
        fdiv    st(1),st
        fdivs   [locd]
        fdivd   [locq]
        fdivp   st(1),st
        fdivr   st(1),st
        fdivrs  [locd]
        fdivrd  [locq]
        fdivrp  st(1),st
        ffree   st(1)
        fiaddw  [locw]
        fiaddd  [locd]
        ficomw  [locw]
        ficomd  [locd]
        ficompw [locw]
        ficompd [locd]
        fidivw  [locw]
        fidivd  [locd]
        fidivrw [locw]
        fidivrd [locd]
        fildw   [locw]
        fildd   [locd]
        fildq   [locq]
        fimulw  [locw]
        fimuld  [locd]
        fincstp
        finit
        fninit
        fistw   [locw]
        fistd   [locd]
        fistpw  [locw]
        fistpd  [locd]
        fistpq  [locq]
        fisubw  [locw]
        fisubd  [locd]
        fisubrw [locw]
        fisubrd [locd]
        fld     st(1)
        flds    [locd]
        fldd    [locq]
        fldl    [loct]
        fldcw   [locw]
        fldenvw [locw]
        fldlg2
        fldln2
        fldl2e
        fldl2t
        fldpi
        fldz
        fld1
        fmul    st,st(1)
        fmuls   [locd]
        fmuld   [locq]
        fmulp   st(1),st
        fnop
        fpatan
        fprem
        fprem1
        fptan
        frndint
        frstord [locb]
        fsaved  [locb]
        fnsaved [locb]
        fscale
        fsin
        fsincos
        fsqrt
        fst     st(1)
        fsts    [locd]
        fstd    [locq]
        fstcw   [locw]
        fnstcw  [locw]
        fstenvd [locb]
        fstp    st(1)
        fstps   [locd]
        fstpd   [locq]
        fstpl   [loct]
        fstsw   [locw]
        fnstsw  [locw]
        fstsw   ax
        fnstsw  ax
        fsub    st,st(1)
        fsubs   [locd]
        fsubd   [locq]
        fsubp   st(1),st
        fsubr   st,st(1)
        fsubrs  [locd]
        fsubrd  [locq]
        fsubrp  st(1),st
        ftst
        fucom   st(1)
        fucomp  st(1)
        fucompp
        fwait
        fxam
        fxch
        fxtract
        fyl2x
        fyl2xp1
        f2xm1
!
! Now, all the modes. Since listing the modes for each and every instruction
! is not practical, we just list all the modes for a single instruction
! (add). This extrapolates fairly well, since the same code is used in
! every instruction.
!
        add     al,8
        add     al,bl
        add     al,[locb]
        add     al,[si]
        add     al,[di]
        add     al,[bp]
        add     al,[bx]
        add     al,$55[si]
        add     al,$5566[si]
        add     al,$55[di]
        add     al,$5566[di]
        add     al,$55[bp]
        add     al,$5566[bp]
        add     al,$55[bx]
        add     al,$5566[bx]
        add     al,[bx+si]
        add     al,[bx+di]
        add     al,[bp+si]
        add     al,[bp+di]
        add     al,$55[bx+si]
        add     al,$55[bx+di]
        add     al,$55[bp+si]
        add     al,$55[bp+di]
        add     al,$5566[bx+si]
        add     al,$5566[bx+di]
        add     al,$5566[bp+si]
        add     al,$5566[bp+di]
        add     [locb],al
        add     [si],al
        add     [di],al
        add     [bp],al
        add     [bx],al
        add     $55[si],al
        add     $5566[si],al
        add     $55[di],al
        add     $5566[di],al
        add     $55[bp],al
        add     $5566[bp],al
        add     $55[bx],al
        add     $5566[bx],al
        add     [bx+si],al
        add     [bx+di],al
        add     [bp+si],al
        add     [bp+di],al
        add     $55[bx+si],al
        add     $55[bx+di],al
        add     $55[bp+si],al
        add     $55[bp+di],al
        add     $5566[bx+si],al
        add     $5566[bx+di],al
        add     $5566[bp+si],al
        add     $5566[bp+di],al
        add     ax,8
        add     ax,bx
        add     ax,[locw]
        add     ax,[si]
        add     ax,[di]
        add     ax,[bp]
        add     ax,[bx]
        add     ax,$55[si]
        add     ax,$5566[si]
        add     ax,$55[di]
        add     ax,$5566[di]
        add     ax,$55[bp]
        add     ax,$5566[bp]
        add     ax,$55[bx]
        add     ax,$5566[bx]
        add     ax,[bx+si]
        add     ax,[bx+di]
        add     ax,[bp+si]
        add     ax,[bp+di]
        add     ax,$55[bx+si]
        add     ax,$55[bx+di]
        add     ax,$55[bp+si]
        add     ax,$55[bp+di]
        add     ax,$5566[bx+si]
        add     ax,$5566[bx+di]
        add     ax,$5566[bp+si]
        add     ax,$5566[bp+di]
        add     [locw],ax
        add     [si],ax
        add     [di],ax
        add     [bp],ax
        add     [bx],ax
        add     $55[si],ax
        add     $5566[si],ax
        add     $55[di],ax
        add     $5566[di],ax
        add     $55[bp],ax
        add     $5566[bp],ax
        add     $55[bx],ax
        add     $5566[bx],ax
        add     [bx+si],ax
        add     [bx+di],ax
        add     [bp+si],ax
        add     [bp+di],ax
        add     $55[bx+si],ax
        add     $55[bx+di],ax
        add     $55[bp+si],ax
        add     $55[bp+di],ax
        add     $5566[bx+si],ax
        add     $5566[bx+di],ax
        add     $5566[bp+si],ax
        add     $5566[bp+di],ax
        add     eax,8
        add     eax,ebx
        add     eax,[locd]
        add     eax,[eax]
        add     eax,[eax*4]
        add     eax,$55[eax]
        add     eax,$55667788[eax]
        add     eax,$55[eax*4]
        add     eax,$55667788[eax*4]
        add     eax,[eax+ebx]
        add     eax,$55[eax+ebx]
        add     eax,$55667788[eax+ebx]
        add     eax,[eax+ebx*4]
        add     eax,$55[eax+ebx*4]
        add     eax,$55667788[eax+ebx*4]
        add     ebx,eax
        add     [locd],eax
        add     [eax],eax
        add     $55[eax],eax
        add     $55667788[eax],eax
        add     [eax+ebx],eax
        add     $55[eax+ebx],eax
        add     $55667788[eax+ebx],eax
        add     [eax+ebx*4],eax
        add     $55[eax+ebx*4],eax
        add     $55667788[eax+ebx*4],eax
!
! Other instructions. These instructions are included here because
! they are in a different format from other assemblers, or are
! unimplemented by other assemblers. They should eventually be
! merged with the normal instructions.
!
        callf   $1122,$33445566
        callmf  $1122,$3344
        calllf  $1122,$33445566
        jmpf    $1122,$33445566
        jmpmf   $1122,$3344
        jmplf   $1122,$33445566
        callf   [$11223344]
        callmf  [$1122]
        calllf  [$11223344]
        jmpf    [$11223344]
        jmpmf   [$1122]
        jmplf   [$11223344]
        lgs     ax,[$1122]
        lgs     eax,[$11223344]
        lss     ax,[$1122]
        lss     eax,[$11223344]
        lfs     ax,[$1122]
        lfs     eax,[$11223344]
        lds     ax,[$1122]
        lds     eax,[$11223344]
        les     ax,[$1122]
        les     eax,[$11223344]
        gs
        ss
        fs
        ds
        es
        cmpxchg8b [$1122]
        cmpxchg8b [$11223344]
        cpuid
        rdmsr
        wrmsr
        rsm

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                             !
! Perform the suite in small mode                                             !
!                                                                             !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

        small
!
! First, all the instructions in alphabetical order
!
        aaa
        aad
        aam
        aas
        adc     [locb],al        
        adc     [locw],ax
        adc     [locd],eax
        adc     al,[locb]
        adc     ax,[locw]
        adc     eax,[locd]
        adc     al,$11
        adc     ax,$1122
        adc     eax,$11223344
        adcb    [locb],$11
        adcw    [locw],$1122
        adcd    [locd],$11223344
        adcw    [locw],$11
        adcd    [locd],$11
        add     al,$11
        add     ax,$1122
        add     eax,$11223344
        addb    [locb],$11
        addw    [locw],$1122
        addd    [locd],$11223344
        addw    [locw],$11
        addd    [locd],$11
        add     [locb],al
        add     [locw],ax
        add     [locd],eax
        add     bl,[locb]
        add     bx,[locw]
        add     ebx,[locd]
        and     [locb],al        
        and     [locw],ax
        and     [locd],eax
        and     al,[locb]
        and     ax,[locw]
        and     eax,[locd]
        and     al,$11
        and     ax,$1122
        and     eax,$11223344
        andb    [locb],$11
        andw    [locw],$1122
        andd    [locd],$11223344
        andw    [locw],$11
        andd    [locd],$11
        arpl    [locw],ax
        bound   ax,[locd]
        bound   eax,[locq]
        bsf     ax,[locw]
        bsf     eax,[locd]
        bsr     ax,[locw]
        bsr     eax,[locd]
        bswap   eax
        bt      [locw],ax
        bt      [locd],eax
        btw     [locw],$11
        btd     [locd],$11
        btc     [locw],ax
        btc     [locd],eax
        btcw    [locw],$11
        btcd    [locd],$11
        btr     [locw],ax
        btr     [locd],eax
        btrw    [locw],$11
        btrd    [locd],$11
        bts     [locw],ax
        bts     [locd],eax
        btsw    [locw],$11
        btsd    [locd],$11
        call    start
        callm   [locw]
        calll   [locd]
        cbw
        cdq
        clc
        cld
        cli
        clts
        cmc
        cmp     al,$11
        cmp     ax,$1122
        cmp     eax,$11223344
        cmpb    [locb],$11
        cmpw    [locw],$1122
        cmpd    [locd],$11223344
        cmpw    [locw],$11
        cmpd    [locd],$11
        cmp     [locb],al
        cmp     [locw],ax
        cmp     [locd],eax
        cmp     bl,[locb]
        cmp     bx,[locw]
        cmp     ebx,[locd]
        cmpsb
        cmpsw
        cmpsd
        cmpxchg [locb],al
        cmpxchg [locw],ax
        cwd
        cwde
        daa
        das
        decb    [locb]
        decw    [locw]
        decd    [locd]
        dec     ax
        dec     eax        
        divb    [locb]
        divw    [locw]
        divd    [locd]
        enter   $1122,$33
        hlt
        idivb   [locb]
        idivw   [locw]
        idivd   [locd]
        imulb   [locb]
        imulw   [locw]
        imuld   [locd]
        imul    ax,[locw]
        imul    eax,[locd]
        imul    ax,[locw],$11
        imul    eax,[locd],$11
        imul    ax,$11
        imul    eax,$11
        imul    ax,[locw],$1122
        imul    eax,[locd],$11223344
        imul    ax,$1122
        imul    eax,$11223344
        in      al,$11
        in      ax,$11
        in      eax,$11
        in      al,dx
        in      ax,dx
        in      eax,dx
        incb    [locb]
        incw    [locw]
        incd    [locd]
        inc     ax
        inc     eax
        insb
        insw
        insd
        int     3
        int     $11
        into
        invd
        invlpg  [locw]
        iret
        iretd
jmprel2:
        ja      jmprel2
        jae     jmprel2
        jb      jmprel2
        jbe     jmprel2
        jc      jmprel2
        jcxz    jmprel2
        jecxz   jmprel2
        je      jmprel2
        jz      jmprel2
        jg      jmprel2
        jge     jmprel2
        jl      jmprel2
        jle     jmprel2
        jna     jmprel2
        jnae    jmprel2
        jnb     jmprel2
        jnbe    jmprel2
        jnc     jmprel2
        jne     jmprel2
        jng     jmprel2
        jnge    jmprel2
        jnl     jmprel2
        jnle    jmprel2
        jno     jmprel2
        jnp     jmprel2
        jns     jmprel2
        jnz     jmprel2
        jo      jmprel2
        jp      jmprel2
        jpe     jmprel2
        jpo     jmprel2
        js      jmprel2
        jz      jmprel2
        jmp     jmprel2
        jmp     start
        jmpm    [locw]
        jmpl    [locd]
        lahf
        lar     ax,[locw]
        lar     eax,[locd]
        lea     ax,[di]
        lea     eax,[edi]
        leave
        lgdtd   [locq]
        lidtd   [locq]
        lldt    [locw]
        lmsw    [locw]
        lock
        lodsb
        lodsw
        lodsd
looprel2:
        loop    looprel2
        loope   looprel2
        loopz   looprel2
        loopne  looprel2
        loopnz  looprel2
        lsl     ax,[locw]
        lsl     eax,[locd]
        ltr     [locw]
        mov     [bx+si],al
        mov     [bx+si],ax
        mov     [ebx+esi],eax
        mov     al,[bx+si]
        mov     ax,[bx+si]
        mov     eax,[ebx+esi]
        mov     [locw],ds
        mov     ds,[locw]
        mov     al,[locb]
        mov     ax,[locw]
        mov     eax,[locd]
        mov     [locb],al
        mov     [locw],ax
        mov     [locd],eax
        mov     al,$11
        mov     ax,$1122
        mov     eax,$11223344
        movb    [locb],$11
        movw    [locw],$1122
        movd    [locd],$11223344
        mov     cr0,eax
        mov     eax,cr0
        mov     dr0,eax
        mov     eax,dr0
        mov     tr6,eax
        mov     eax,tr6
        movsb
        movsw
        movsd
        movsxb  ax,[locb]
        movsxb  eax,[locb]
        movsxw  eax,[locw]
        movzxb  ax,[locb]
        movzxb  eax,[locb]
        movzxw  eax,[locw]
        mulb    [locb]
        mulw    [locw]
        muld    [locd]
        negb    [locb]
        negw    [locw]
        negd    [locd]
        nop
        notb    [locb]
        notw    [locw]
        notd    [locd]
        or      al,$11
        or      ax,$1122
        or      eax,$11223344
        orb     [locb],$11
        orw     [locw],$1122
        ord     [locd],$11223344
        orw     [locw],$11
        ord     [locd],$11
        or      [locb],al
        or      [locw],ax
        or      [locd],eax
        or      ah,[locb]
        or      bx,[locw]
        or      ebx,[locd]
        out     $11,al
        out     $11,ax
        out     $11,eax
        out     dx,al
        out     dx,ax
        out     dx,eax
        outsb
        outsw
        outsd
        popw    [locw]
        popd    [locd]
        pop     ax
        pop     eax
        pop     ds
        pop     es
        pop     ss
        pop     fs
        pop     gs
        popa
        popad
        popf
        popfd
        pushw   [locw]
        pushd   [locd]
        push    ax
        push    eax
        pushd   $11
        pushw   $1122
        pushd   $11223344
        push    cs
        push    ss
        push    ds
        push    es
        push    fs
        push    gs
        pusha
        pushad
        pushf
        pushfd
        rclb    [locb],1
        rclb    [locb],cl
        rclb    [locb],$11
        rclw    [locw],1
        rclw    [locw],cl
        rclw    [locw],$11
        rcld    [locd],1
        rcld    [locd],cl
        rcld    [locd],$11
        rcrb    [locb],1
        rcrb    [locb],cl
        rcrb    [locb],$11
        rcrw    [locw],1
        rcrw    [locw],cl
        rcrw    [locw],$11
        rcrd    [locd],1
        rcrd    [locd],cl
        rcrd    [locd],$11
        rolb    [locb],1
        rolb    [locb],cl
        rolb    [locb],$11
        rolw    [locw],1
        rolw    [locw],cl
        rolw    [locw],$11
        rold    [locd],1
        rold    [locd],cl
        rold    [locd],$11
        rorb    [locb],1
        rorb    [locb],cl
        rorb    [locb],$11
        rorw    [locw],1
        rorw    [locw],cl
        rorw    [locw],$11
        rord    [locd],1
        rord    [locd],cl
        rord    [locd],$11
        rep
        repe
        repz
        repne
        repnz
        ret
        ret     $1122
        sahf
        salb    [locb],1
        salb    [locb],cl
        salb    [locb],$11
        salw    [locw],1
        salw    [locw],cl
        salw    [locw],$11
        sald    [locd],1
        sald    [locd],cl
        sald    [locd],$11
        sarb    [locb],1
        sarb    [locb],cl
        sarb    [locb],$11
        sarw    [locw],1
        sarw    [locw],cl
        sarw    [locw],$11
        sard    [locd],1
        sard    [locd],cl
        sard    [locd],$11
        shlb    [locb],1
        shlb    [locb],cl
        shlb    [locb],$11
        shlw    [locw],1
        shlw    [locw],cl
        shlw    [locw],$11
        shld    [locd],1
        shld    [locd],cl
        shld    [locd],$11
        shrb    [locb],1
        shrb    [locb],cl
        shrb    [locb],$11
        shrw    [locw],1
        shrw    [locw],cl
        shrw    [locw],$11
        shrd    [locd],1
        shrd    [locd],cl
        shrd    [locd],$11
        sbb     al,$11
        sbb     ax,$1122
        sbb     eax,$11223344
        sbbb    [locb],$11
        sbbw    [locw],$1122
        sbbd    [locd],$11223344
        sbbw    [locw],$11
        sbbd    [locd],$11
        sbbb    [locb],al
        sbbw    [locw],ax
        sbbd    [locd],eax
        sbb     al,[locb]
        sbb     ax,[locw]
        sbb     eax,[locd]
        scasb
        scasw
        scasd
        seta    [locb]
        setae   [locb]
        setb    [locb]
        setbe   [locb]
        setc    [locb]
        sete    [locb]
        setg    [locb]
        setge   [locb]
        setl    [locb]
        setle   [locb]
        setna   [locb]
        setnae  [locb]
        setnb   [locb]
        setnbe  [locb]
        setnc   [locb]
        setne   [locb]
        setng   [locb]
        setnge  [locb]
        setnl   [locb]
        setnle  [locb]
        setno   [locb]
        setnp   [locb]
        setns   [locb]
        setnz   [locb]
        seto    [locb]
        setp    [locb]
        setpe   [locb]
        setpo   [locb]
        sets    [locb]
        setz    [locb]
        sgdtd   [locq]
        sidtd   [locq]
        shldw   [locw],ax,$11
        shldd   [locd],eax,$11
        shldw   [locw],ax,cl
        shldd   [locd],eax,cl
        shrdw   [locw],ax,$11
        shrdd   [locd],eax,$11
        shrdw   [locw],ax,cl
        shrdd   [locd],eax,cl
        sldt    [locw]
        smsw    [locw]
        stc
        std
        sti
        stosb
        stosw
        stosd
        str     [locw]
        sub     al,$11
        sub     ax,$1122
        sub     eax,$11223344
        subb    [locb],$11
        subw    [locw],$1122
        subd    [locd],$11223344
        subw    [locw],$11
        subd    [locd],$11
        sub     [locb],al
        sub     [locw],ax
        sub     [locd],eax
        sub     ah,[locb]
        sub     ax,[locw]
        sub     eax,[locd]
        test    al,$11
        test    ax,$1122
        test    eax,$11223344
        testb   [locb],$11
        testw   [locw],$1122
        testd   [locd],$11223344
        test    [locb],al
        test    [locw],ax
        test    [locd],eax
        verr    [locw]
        verw    [locw]
        wait
        wbinvd
        xadd    [locb],al
        xadd    [locw],ax
        xadd    [locd],eax
        xchg    [locb],al
        xchg    al,[locb]
        xchg    [locw],ax
        xchg    ax,[locw]
        xchg    [locd],eax
        xchg    eax,[locd]
        xchg    ax,bx
        xchg    bx,ax
        xchg    eax,ebx
        xchg    ebx,eax
        xlatb
        xor     al,$11
        xor     ax,$1122
        xor     eax,$11223344
        xorb    [locb],$11
        xorw    [locw],$1122
        xord    [locd],$11223344
        xorw    [locw],$11
        xord    [locd],$11
        xor     [locb],al
        xor     [locw],ax
        xor     [locd],eax
        xor     bl,[locb]
        xor     bx,[locw]
        xor     ebx,[locd]
        f2xm1
        fabs
        fadd    st,st(1)
        fadd    st(1),st
        fadds   [locd]
        faddd   [locq]
        faddp   st(2),st
        fbld    [loct]
        fbstp   [loct]
        fchs
        fclex
        fnclex
        fcom    st(1)
        fcoms   [locd]
        fcomd   [locq]
        fcomp   st(1)
        fcomps  [locd]
        fcompd  [locq]
        fcompp
        fcos
        fdecstp
        fdiv    st(1),st
        fdivs   [locd]
        fdivd   [locq]
        fdivp   st(1),st
        fdivr   st(1),st
        fdivrs  [locd]
        fdivrd  [locq]
        fdivrp  st(1),st
        ffree   st(1)
        fiaddw  [locw]
        fiaddd  [locd]
        ficomw  [locw]
        ficomd  [locd]
        ficompw [locw]
        ficompd [locd]
        fidivw  [locw]
        fidivd  [locd]
        fidivrw [locw]
        fidivrd [locd]
        fildw   [locw]
        fildd   [locd]
        fildq   [locq]
        fimulw  [locw]
        fimuld  [locd]
        fincstp
        finit
        fninit
        fistw   [locw]
        fistd   [locd]
        fistpw  [locw]
        fistpd  [locd]
        fistpq  [locq]
        fisubw  [locw]
        fisubd  [locd]
        fisubrw [locw]
        fisubrd [locd]
        fld     st(1)
        flds    [locd]
        fldd    [locq]
        fldl    [loct]
        fldcw   [locw]
        fldenvw [locw]
        fldlg2
        fldln2
        fldl2e
        fldl2t
        fldpi
        fldz
        fld1
        fmul    st,st(1)
        fmuls   [locd]
        fmuld   [locq]
        fmulp   st(1),st
        fnop
        fpatan
        fprem
        fprem1
        fptan
        frndint
        frstord [locb]
        fsaved  [locb]
        fnsaved [locb]
        fscale
        fsin
        fsincos
        fsqrt
        fst     st(1)
        fsts    [locd]
        fstd    [locq]
        fstcw   [locw]
        fnstcw  [locw]
        fstenvd [locb]
        fstp    st(1)
        fstps   [locd]
        fstpd   [locq]
        fstpl   [loct]
        fstsw   [locw]
        fnstsw  [locw]
        fstsw   ax
        fnstsw  ax
        fsub    st,st(1)
        fsubs   [locd]
        fsubd   [locq]
        fsubp   st(1),st
        fsubr   st,st(1)
        fsubrs  [locd]
        fsubrd  [locq]
        fsubrp  st(1),st
        ftst
        fucom   st(1)
        fucomp  st(1)
        fucompp
        fwait
        fxam
        fxch
        fxtract
        fyl2x
        fyl2xp1
        f2xm1
!
! Now, all the modes. Since listing the modes for each and every instruction
! is not practical, we just list all the modes for a single instruction
! (add). This extrapolates fairly well, since the same code is used in
! every instruction.
!
        add     al,8
        add     al,bl
        add     al,[locb]
        add     al,[si]
        add     al,[di]
        add     al,[bp]
        add     al,[bx]
        add     al,$55[si]
        add     al,$5566[si]
        add     al,$55[di]
        add     al,$5566[di]
        add     al,$55[bp]
        add     al,$5566[bp]
        add     al,$55[bx]
        add     al,$5566[bx]
        add     al,[bx+si]
        add     al,[bx+di]
        add     al,[bp+si]
        add     al,[bp+di]
        add     al,$55[bx+si]
        add     al,$55[bx+di]
        add     al,$55[bp+si]
        add     al,$55[bp+di]
        add     al,$5566[bx+si]
        add     al,$5566[bx+di]
        add     al,$5566[bp+si]
        add     al,$5566[bp+di]
        add     [locb],al
        add     [si],al
        add     [di],al
        add     [bp],al
        add     [bx],al
        add     $55[si],al
        add     $5566[si],al
        add     $55[di],al
        add     $5566[di],al
        add     $55[bp],al
        add     $5566[bp],al
        add     $55[bx],al
        add     $5566[bx],al
        add     [bx+si],al
        add     [bx+di],al
        add     [bp+si],al
        add     [bp+di],al
        add     $55[bx+si],al
        add     $55[bx+di],al
        add     $55[bp+si],al
        add     $55[bp+di],al
        add     $5566[bx+si],al
        add     $5566[bx+di],al
        add     $5566[bp+si],al
        add     $5566[bp+di],al
        add     ax,8
        add     ax,bx
        add     ax,[locw]
        add     ax,[si]
        add     ax,[di]
        add     ax,[bp]
        add     ax,[bx]
        add     ax,$55[si]
        add     ax,$5566[si]
        add     ax,$55[di]
        add     ax,$5566[di]
        add     ax,$55[bp]
        add     ax,$5566[bp]
        add     ax,$55[bx]
        add     ax,$5566[bx]
        add     ax,[bx+si]
        add     ax,[bx+di]
        add     ax,[bp+si]
        add     ax,[bp+di]
        add     ax,$55[bx+si]
        add     ax,$55[bx+di]
        add     ax,$55[bp+si]
        add     ax,$55[bp+di]
        add     ax,$5566[bx+si]
        add     ax,$5566[bx+di]
        add     ax,$5566[bp+si]
        add     ax,$5566[bp+di]
        add     [locw],ax
        add     [si],ax
        add     [di],ax
        add     [bp],ax
        add     [bx],ax
        add     $55[si],ax
        add     $5566[si],ax
        add     $55[di],ax
        add     $5566[di],ax
        add     $55[bp],ax
        add     $5566[bp],ax
        add     $55[bx],ax
        add     $5566[bx],ax
        add     [bx+si],ax
        add     [bx+di],ax
        add     [bp+si],ax
        add     [bp+di],ax
        add     $55[bx+si],ax
        add     $55[bx+di],ax
        add     $55[bp+si],ax
        add     $55[bp+di],ax
        add     $5566[bx+si],ax
        add     $5566[bx+di],ax
        add     $5566[bp+si],ax
        add     $5566[bp+di],ax
        add     eax,8
        add     eax,ebx
        add     eax,[locd]
        add     eax,[eax]
        add     eax,[eax*4]
        add     eax,$55[eax]
        add     eax,$55667788[eax]
        add     eax,$55[eax*4]
        add     eax,$55667788[eax*4]
        add     eax,[eax+ebx]
        add     eax,$55[eax+ebx]
        add     eax,$55667788[eax+ebx]
        add     eax,[eax+ebx*4]
        add     eax,$55[eax+ebx*4]
        add     eax,$55667788[eax+ebx*4]
        add     ebx,eax
        add     [locd],eax
        add     [eax],eax
        add     $55[eax],eax
        add     $55667788[eax],eax
        add     [eax+ebx],eax
        add     $55[eax+ebx],eax
        add     $55667788[eax+ebx],eax
        add     [eax+ebx*4],eax
        add     $55[eax+ebx*4],eax
        add     $55667788[eax+ebx*4],eax
!
! Other instructions. These instructions are included here because
! they are in a different format from other assemblers, or are
! unimplemented by other assemblers. They should eventually be
! merged with the normal instructions.
!
        callf   $1122,$33445566
        callmf  $1122,$3344
        calllf  $1122,$33445566
        jmpf    $1122,$33445566
        jmpmf   $1122,$3344
        jmplf   $1122,$33445566
        callf   [$1122]
        callmf  [$1122]
        calllf  [$11223344]
        jmpf    [$1122]
        jmpmf   [$1122]
        jmplf   [$11223344]
        lgs     ax,[$1122]
        lgs     eax,[$11223344]
        lss     ax,[$1122]
        lss     eax,[$11223344]
        lfs     ax,[$1122]
        lfs     eax,[$11223344]
        lds     ax,[$1122]
        lds     eax,[$11223344]
        les     ax,[$1122]
        les     eax,[$11223344]
        gs
        ss
        fs
        ds
        es
        cmpxchg8b [$1122]
        cmpxchg8b [$11223344]
        cpuid
        rdmsr
        wrmsr
        rsm
!
! Finally, these instructions are specific to specific CPUs, and
! exist in a case all their own
!
        m86
        fdisi
        fndisi
        feni
        fneni

        m286
        fsetpm

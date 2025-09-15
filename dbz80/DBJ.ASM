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

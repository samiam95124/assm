module link(output);

uses 

   stddef, { standard defines }
   strlib, { string functions }
   extlib, { os support functions }
   lndef,  { global definitions file }
   common, { global variables file }
   utl;    { utilities file }

procedure dumpsym; forward; { dump symbols table }
procedure dumprld; forward; { dump rld table }
procedure origin; forward; { set program origin }
procedure prcobj; forward; { process object file }
procedure outsym; forward; { output symbol file }
procedure report; forward; { generate undefineds report }
procedure rdsyms; forward; { read symbols file }
procedure apnsym; forward; { append new symbols deck }
procedure mrgsym; forward; { merge old and new symbols decks }
procedure reduce; forward; { reduce symbols operations }
procedure srtrld; forward; { sort rld table }
procedure listsym; forward; { list symbols }
procedure srtalp; forward; { sort symbols for alphabetical order }
procedure srtval; forward; { sort symbols for value order }
procedure listxrf; forward; { perform cross reference listing }
procedure fndmax; forward; { find maximum symbol length }
procedure listmod; forward; { perform module parameter listing }
procedure srcmax; forward; { find maximum filenames length }

{*******************************************************************************

Print entry number for symbol

Given a symbol, prints the entry number for it, or the count from start of
the symbol table. A diagnostic.

*******************************************************************************}

procedure prtsyn(sym: symptr);

var sc: integer; { symbol count }
    sp: symptr;  { symbols pointer }

begin

   if sym = nil then writeln('nil') else begin

      sp := symtab; { index top of table }
      sc := 1; { set 1st entry number }
      while (sp <> sym) and (sp <> nil) do begin { traverse }
   
         sc := sc+1; { count entries }
         sp := sp^.next { link next entry }
   
      end;
      if sp <> nil then write(sc:1) { print number }
      else write('???')

   end

end;

{*******************************************************************************

Dump contents of current rld table

A diagnostic, prints each entry in the current rld table, one per line,
in fairly memonic fashion.

*******************************************************************************}

procedure dumprld;

var rp: rldptr; { pointer for rlds }
    rc: integer; { rld counter }

begin

   writeln('RLD table');
   writeln;
   rp := rldtab; { index top of table }
   rc := 1; { set 1st entry }
   while rp <> nil do begin { traverse }

      write(rc:1, ': '); { print entry number }
      write(' it[');
      case rp^.im of { insertion mode }

         imnorm: write('norm');   { normal }
         imsgof: write('off');    { signed offset }
         imnsof: write('ns off'); { non-standard offset }
         imiseg: write('i seg')   { Intel segmented }

      end;
      write('] flags[');
      if rp^.big then write('big '); { output big endian flag }
      if rp^.adf then write('add '); { output value is address flag }
      if rp^.vrs then write('var '); { output value is variable address flag }
      if rp^.def then write('def '); { output defined flag }
      write('] bits[', rp^.str:1, ':', rp^.len:1, '] val[', rp^.val, ']');
      write(' addr[');
      prthex(digits, rp^.add); { print address of insertion }
      write(']');
      if rp^.inssym <> nil then begin { insert symbol exists }

         write(' insym[');
         prtsyn(rp^.inssym);
         write(']')

      end;
      writeln; { terminate line }
      rp := rp^.next; { link next rld }
      rc := rc+1 { count entries }

   end;
   writeln { space off }

end; 

{*******************************************************************************

Dump contents of current symbol table

A diagnostic, prints each entry in the current symbol table, one per line,
in fairly memonic fashion.

*******************************************************************************}

procedure dumpsym;

var sp: symptr; { pointer for symbols table }
    sc: integer; { symbols counter }

begin

   writeln('Symbol table');
   writeln;
   sc := 1; { set 1st symbol number }
   sp := symtab; { index top of symbol table }
   while sp <> nil do begin { traverse table }

      write(sc:1, ': opr['); { print symbol number }
      case sp^.opr of { operation }

         onop: write('nop');
         oadd: write('add');
         osub: write('sub');
         omult: write('mult');
         odiv: write('div');
         omod: write('mod');
         oshl: write('shl');
         oshr: write('shr');
         oand: write('and');
         oor: write('or');
         oxor: write('xor');
         onot: write('not');
         oneg: write('neg')

      end;
      write('] lab['); { space off }
      write(output, sp^.lab^:0); { print symbol label }
      write('] flags['); { space off }
      if sp^.def then write('def '); { print defined flag }
      if sp^.add then write('add '); { print address flag }
      if sp^.gbl then write('gbl '); { print global flag }
      if sp^.ext then write('ext '); { print external flag }
      if sp^.vrs then write('var '); { print variable flag }
      write('] val[', sp^.val:1, ']');
      if sp^.lft <> nil then begin { left branch exists }

         write(' left[');
         prtsyn(sp^.lft); { print left entry number }
         write(']')

      end;
      if sp^.rgt <> nil then begin { left branch exists }

         write(' right[');
         prtsyn(sp^.rgt); { print right entry number }
         write(']')

      end;
      writeln; { terminate line }
      sp := sp^.next; { link next entry }
      sc := sc+1 { count entries }

   end;
   writeln { space off }

end;

{*******************************************************************************

Get symbol entry

Gets a symbol entry either from the free list, or creates one.

*******************************************************************************}

procedure getsym(var p: symptr);


begin

   if (fresym <> nil) and symrecycle then begin { get existing entry }

      p := fresym; { index symbol }
      fresym := fresym^.next { gap list }

   end else new(p);
   p^.opr  := onop; { clear fields }
   p^.lab  := nil; { set no label }
   p^.def  := false;
   p^.add  := false;
   p^.gbl  := false;
   p^.ext  := false;
   p^.vrs  := false;
   p^.val  := 0;
   p^.lft  := nil;
   p^.rgt  := nil;
   p^.err  := false;
   p^.par  := nil;
   p^.next := nil

end;

{*******************************************************************************

Put symbol entry

Places the given symbol entry on the free list.

*******************************************************************************}

procedure putsym(p: symptr);

begin

   { if the symbol has a label attached, get rid of it }
   if p^.lab <> nil then dispose(p^.lab);
   p^.next := fresym; { link to list }
   fresym := p

end;

{*******************************************************************************

Get rld entry

Gets an rld entry either from the free list, or creates one.

*******************************************************************************}

procedure getrld(var p: rldptr);

begin

   if frerld <> nil then begin { get existing entry }

      p := frerld; { index symbol }
      frerld := frerld^.next { gap list }

   end else new(p);
   p^.big    := false; { clear fields }
   p^.im     := imnorm;
   p^.cof    := 0;
   p^.str    := 0;
   p^.len    := 0;
   p^.add    := 0;
   p^.inssym := nil;
   p^.val    := 0;
   p^.adf    := false;
   p^.vrs    := false;
   p^.def    := false;
   p^.next   := nil

end;

{*******************************************************************************

Put rld entry

Places the given rld entry on the free list.

*******************************************************************************}

procedure putrld(p: rldptr);

begin

   p^.next := frerld; { link to list }
   frerld := p

end;

{********************************************************************************

Get block entry

Gets a block entry. If there are block entries on the free list, that is 
returned, otherwise we return an entirely new one.

********************************************************************************}

procedure getblk(var bp: blkptr);

begin

   if blkfre <> nil then begin { get a free entry }

      bp := blkfre; { index the top free entry }
      blkfre := bp^.next { gap from free list }

   end else new(bp); { just get a new entry }
   bp^.startp := 0; { clear parameters }
   bp^.endp   := 0;
   bp^.startv := 0;
   bp^.endv   := 0;
   bp^.inclst := nil;
   bp^.outp   := false;
   bp^.seq    := 0;
   bp^.lvl    := 0;
   bp^.oref   := 0;
   bp^.next   := nil;

end;

{********************************************************************************

Put block entry

Puts a used block entry. The block is placed onto the free list.

********************************************************************************}

procedure putblk(bp: blkptr);

begin

   bp^.next := blkfre; { push onto free list }
   blkfre := bp

end;

{********************************************************************************

Get block inclusion entry

Gets a block inclusion entry. If there are block inclusion entries on the free
list, that is returned, otherwise we return an entirely new one.

********************************************************************************}

procedure getbic(var ip: bicptr);

begin

   if bicfre <> nil then begin { get a free entry }

      ip := bicfre; { index the top free entry }
      bicfre := ip^.next { gap from free list }

   end else new(ip); { just get a new entry }
   ip^.sym  := nil; { clear parameters }
   ip^.blk  := nil;
   ip^.next := nil;

end;

{********************************************************************************

Put block inclusion entry

Puts a used block inclusion entry. The entry is placed onto the free list.

********************************************************************************}

procedure putbic(ip: bicptr);

begin

   ip^.next := bicfre; { push onto free list }
   bicfre := ip

end;

{********************************************************************************

Dump block list

Dumps the given block list, with inclusions. This is a diagnostic.

********************************************************************************}

procedure dmpblk(blk: blkptr); { list to dump }

var ip: bicptr; { inclusion pointer }

begin

   writeln;
   while blk <> nil do begin { traverse list }
   
      writeln('Block: ');
      write('Program start:      '); prthex(8, blk^.startp); writeln;
      write('Program end:        '); prthex(8, blk^.endp); writeln;
      write('Variable start:     '); prthex(8, blk^.startp); writeln;
      write('Variable end:       '); prthex(8, blk^.endp); writeln;
      writeln('Sequence number:    ', blk^.seq:1);
      writeln('Level number:       ', blk^.lvl:1);
      writeln('Inbound references: ', blk^.oref:1);
      ip := blk^.inclst;
      while ip <> nil do begin { traverse list }

         writeln;
         write('Inclusion:  ');
         if ip^.sym <> nil then writeln('symbol')
         else if ip^.blk <> nil then writeln('block')
         else writeln('none');
         if ip^.sym <> nil then
            if ip^.sym^.lab <> nil then 
               writeln('name:       ', ip^.sym^.lab^:0);
         ip := ip^.next { next entry }
   
      end;
      blk := blk^.next;
      writeln { space off }

  end;
  writeln

end;

{********************************************************************************

Get line entry

Gets a line entry. If there are line entries on the free list, that is returned,
otherwise we return an entirely new one.

********************************************************************************}

procedure getlin(var lp: trkptr);

begin

   if linfre <> nil then begin { get a free entry }

      lp := linfre; { index the top free entry }
      linfre := lp^.next { gap from free list }

   end else new(lp); { just get a new entry }
   lp^.line := 0; { clear parameters }
   lp^.prg := 0;
   lp^.vrs := 0;
   lp^.src := nil

end;

{********************************************************************************

Put block entry

Puts a used block entry. The block is placed onto the free list.

********************************************************************************}

procedure putlin(lp: trkptr);

begin

   lp^.next := linfre; { push onto free list }
   linfre := lp

end;

{*******************************************************************************

Move symbol to current

Moves the given symbol entry from the saved deck to the current deck.

*******************************************************************************}

procedure movsym(sym: symptr); { symbol to move }

var lp, sp: symptr; { symbol entry pointers }

begin

   { remove from saved table }
   if symsav = sym then { symbol is the root }
      symsav := symsav^.next { gap from list }
   else begin { search list }

      sp := symsav; { index top of saved symbols }
      while (sp <> sym) and (sp <> nil) do begin { traverse symbols }

         lp := sp; { set last entry }
         sp := sp^.next { link next entry }

      end;
      if sp = nil then fprterr(esysflt3); { fault: was not in list }
      lp^.next := sym^.next { gap over entry }

   end;
   sym^.next := symtab; { link into current table }
   symtab := sym

end;

{*******************************************************************************

Delete symbol from current

Removes the given symbol from the current, which must have no other references
to it besides just being in the list.

*******************************************************************************}

procedure delsym(sym: symptr); { symbol to delete }

var lp, sp: symptr; { symbol entry pointers }

begin

   { remove from symbol table }
   if symtab = sym then { symbol is the root }
      symtab := symtab^.next { gap from list }
   else begin { search list }

      sp := symtab; { index top of symbols }
      while (sp <> sym) and (sp <> nil) do begin { traverse symbols }

         lp := sp; { set last entry }
         sp := sp^.next { link next entry }

      end;
      if sp = nil then fprterr(esysflt4); { fault: was not in list }
      lp^.next := sym^.next { gap over entry }

   end

end;

{*******************************************************************************

Merge current and old symbols, rlds and blocks

The old symbols deck is linked in atop the new symbols deck, the old rlds deck
is linked atop the old symbols deck, and the block lists are concatenated.

*******************************************************************************}

procedure mrgsym;

var sp: symptr; { symbols pointer }
    rp: rldptr; { rld pointer }
    bp: blkptr; { block pointer }
    lp: trkptr; { line tracking pointer }

begin

   { merge symbols tables }
   if symtab = nil then symtab := symsav { current empty, just set to old }
   else begin { find end of current list }

      sp := symtab; { index 1st symbol }
      while sp^.next <> nil do sp := sp^.next; { find end entry }
      sp^.next := symsav { link new to old list }

   end;
   { merge rld tables }
   if rldtab = nil then rldtab := rldsav { current empty, just set to old }
   else begin { find end of current list }

      rp := rldtab; { index 1st rld }
      while rp^.next <> nil do rp := rp^.next; { find end entry }
      rp^.next := rldsav { link new to old list }

   end;
   { merge block tables }
   if blktab = nil then blktab := blksav { current empty, just set to old }
   else begin { find end of current list }

      bp := blktab; { index 1st block }
      while bp^.next <> nil do bp := bp^.next; { find end entry }
      bp^.next := blksav { link new to old list }

   end;
   { merge lin tables }
   if linlst = nil then linlst := linsav { current empty, just set to old }
   else begin { find end of current list }

      lp := linlst; { index 1st line }
      while lp^.next <> nil do lp := lp^.next; { find end entry }
      lp^.next := linsav { link new to old list }

   end

end;

{*******************************************************************************

Input variger

Inputs a variger to the given integer.
Varigers are of the following format:

   1. (byte) the tag byte.
   2-N. The variger value.

The tag byte values are:

   bit 7 - Low for integer number, high for float.
   bit 6 - Contains the sign of the integer. 
   bit 5 - Unused.
   bit 4 - Length of integer in bytes, 1-32, in -1 format.
   bit 3 -      ""              ""
   bit 2 -      ""              ""
   bit 1 -      ""              ""
   bit 0 -      ""              ""

The integer is converted by removing the sign bit and converting
to signed magnitude, then determining the byte size, then
outputting the tag and number.

*******************************************************************************}

procedure rdvar(var n: integer); { integer to input }

var t: byte;     { tag byte }
    s: integer;  { sign }
    b: byte;     { read byte holder }

begin

   readsymb(t); { get tag byte }
   if (t and $80) <> 0 then fprterr(efltfmt); { floating point not implemented }
   if (t and $40) <> 0 then s := -1 else s := 1; { set sign of value }
   if (t and $20) <> 0 then fprterr(esymfmt1); { invalid symbol file format }
   t := (t and $1f)+1; { mask byte length and adjust }
   n := 0; { clear result }
   while t <> 0 do begin { read in bytes of value }

      n := n*256; { scale up bytes for big endian format }
      readsymb(b); { get the next byte }
      n := n+b; { add in }
      t := t-1 { count bytes read }

   end;
   n := n*s { set sign of result }

end;

{*******************************************************************************

Input stepped value

Inputs a value in stepped format. This is an unsigned format. If the value 
is < 255, then it is a single byte. If > 255, the value 255 is present, then a 
16 bit, big endian format number follows, with values < 65535. If it exceeds
that, the value 65536 is present, then we continue with a 3 byte, then finally
a 4 byte format. This system is most efficient where values output tend to be
small, with only occasional large values. We use it for the difference numbers
in the line file.

*******************************************************************************}

procedure inpstp(var n: integer);

var b: byte; { read byte holder }

begin

   readsymb(b); { get 1st byte }
   n := b; { place as value }
   if n = 255 then begin { > 1 byte format }

      readsymb(b); { get high byte }
      n := b*256; { place }
      readsymb(b); { get low byte }
      n := n+b; { place }
      if n = 65535 then begin { > 2 byte format }

         readsymb(b); { get high byte }
         n := b*65536; { place }
         readsymb(b); { get mid byte }
         n := n+b*256; { place }
         readsymb(b); { get low byte }
         n := n+b; { place }
         if n = 1677215 then begin { > 3 byte format }   

            readsymb(b); { get high byte }
            n := b*1677216; { place }
            readsymb(b); { get high byte }
            n := n+b*65536; { place }
            readsymb(b); { get mid byte }
            n := n+b*256; { place }
            readsymb(b); { get low byte }
            n := n+b { place }

         end

      end

   end

end;

{*******************************************************************************

Output stepped value

Outputs a value in stepped format. This is an unsigned format. If the value 
is < 255, then it is output as a single byte. If > 255, the value 255 is 
output, then a 16 bit, big endian format number follows, with values < 65535. 
If it exceeds that, the value 65536 is output, then we continue with a 3 byte, 
then finally a 4 byte format. This system is most efficient where values output 
tend to be small, with only occasional large values. We use it for the 
difference numbers output to the line file.

*******************************************************************************}

procedure outstp(n: integer);

begin

   if n < 0 then fprterr(esysflt5); { should not be negative }
   if n < 255 then wrtsymb(n) { output 1 byte format }
   else begin { larger than 1 byte }

      wrtsymb(255); { output flag byte }
      if n < 65535 then begin { output 2 byte format }

         wrtsymb(n div 256); { output high byte }
         wrtsymb(n mod 256) { output low byte }

      end else begin { larger than 2 byte }

         wrtsymb(255); { output flag bytes }
         wrtsymb(255);
         if n < 16777215 then begin { output 3 byte format }

            wrtsymb(n div 65535); { output high byte }
            wrtsymb((n div 256) mod 256); { output mid byte }
            wrtsymb(n mod 256) { output low byte }

         end else begin { output 4 byte format }

            wrtsymb(n div 16777216); { output high byte }
            wrtsymb((n div 65535) mod 256); { output high mid byte }
            wrtsymb((n div 256) mod 256); { output low mid byte }
            wrtsymb(n mod 256) { output low byte }

         end

      end

   end

end;

{*******************************************************************************

Read next symbol file entry

Reads the next symbol file entry. The next object in the symbols file is read,
a symbol entry, an rld entry, or block entry.

The object parameters are placed in the global save area, so that a
"lookahead" mechanisim is implemented.

*******************************************************************************}

procedure rdnxt;

var b: byte;    { read byte holding }
    l: byte;    { string length }
    i: integer;

begin

   readsymb(b); { get the next object type }
   if not (objtyp(b) in [obend, obsym, obcst, obrld, obcrld, obblk, 
                         obblke, oblin, obsrc]) then 
      fprterr(esymfmt2); { invalid file symbol format }
   nxtobj := objtyp(b); { place next object type }
   if (nxtobj = obsym) or (nxtobj = obcst) then begin { symbol }

      getsym(nxtsym); { get a symbol entry }
      readsymb(b); { get the operation code }
      if b in [ord(onop), ord(oadd), ord(osub), ord(omult), ord(odiv),
               ord(omod), ord(oshl), ord(oshr), ord(oand), ord(oor),
               ord(oxor), ord(onot), ord(oneg)] then 
         case symop(b) of { operation }

         onop:  nxtsym^.opr := onop; 
         oadd:  nxtsym^.opr := oadd; 
         osub:  nxtsym^.opr := osub;
         omult: nxtsym^.opr := omult; 
         odiv:  nxtsym^.opr := odiv; 
         omod:  nxtsym^.opr := omod; 
         oshl:  nxtsym^.opr := oshl; 
         oshr:  nxtsym^.opr := oshr; 
         oand:  nxtsym^.opr := oand; 
         oor:   nxtsym^.opr := oor; 
         oxor:  nxtsym^.opr := oxor; 
         onot:  nxtsym^.opr := onot; 
         oneg:  nxtsym^.opr := oneg

      end else fprterr(esymfmt3); { invalid symbol file format }
      if nxtobj = obsym then begin { get symbol label }

         readsymb(b); { get the symbol length }
         if b+1 > maxlab then fprterr(elabovf); { label too long }
         new(nxtsym^.lab, b+1); { allocate label string }
         for i := 1 to b+1 do begin { read symbol characters }

            readsymb(b); { get a symbol character }

            if not (ascii2chr(b) in ['_', 'a'..'z', 'A'..'Z', '0'..'9', '.']) then
               fprterr(esymfmt4); { invalid symbol file format }
            nxtsym^.lab^[i] := ascii2chr(b) { place character }

         end

      end;
      readsymb(b); { get flags byte }
      if (b and $80) <> 0 then fprterr(esymfmt5); { invalid symbol file format }
      if (b and $40) <> 0 then fprterr(esymfmt6); { invalid symbol file format }
      if (b and $20) <> 0 then fprterr(esymfmt7); { invalid symbol file format }
      nxtsym^.vrs := (b and $10) <> 0; { set variable space flag }
      nxtsym^.ext := (b and $08) <> 0; { set external flag }
      nxtsym^.gbl := (b and $04) <> 0; { set global flag }
      nxtsym^.add := (b and $02) <> 0; { set address flag }
      nxtsym^.def := (b and $01) <> 0; { set defined flag }
      if nxtsym^.def then rdvar(nxtsym^.val) { defined, get value }

   end else if (nxtobj = obrld) or (nxtobj = obcrld) then begin { rld }

      getrld(nxtrld); { get an rld entry }
      readsymb(b); { get it tag byte }
      nxtrld^.big := (b and $80) <> 0; { set big endian flag }
      if (b and $40) <> 0 then fprterr(esymfmt8); { invalid symbol format }
      if (b and $20) <> 0 then fprterr(esymfmt9); { invalid symbol format }
      case (b and $18) div $08 of { insertion type }

         0: nxtrld^.im := imnorm; { normal }
         1: nxtrld^.im := imsgof; { signed offset }
         2: nxtrld^.im := imnsof; { non-standard signed offset }
         3: nxtrld^.im := imiseg  { Intel segment }

      end;
      nxtrld^.str := b and $7; { set bit field start }
      readsymb(b); { get bit length }
      nxtrld^.len := b+1; { place }
      if nxtrld^.im = imnsof then begin { constant offset exists }

         readsymb(b); { get constant offset }
         nxtrld^.cof := b { place }

      end;
      rdvar(nxtrld^.add); { get address }
      if nxtobj = obcrld then begin { constant rld, value exists }

         readsymb(b); { get flag byte }
         if (b and $80) <> 0 then fprterr(esymfmt10); { invalid symbol file format }
         if (b and $40) <> 0 then fprterr(esymfmt11); { invalid symbol file format }
         if (b and $20) <> 0 then fprterr(esymfmt12); { invalid symbol file format }
         nxtrld^.vrs := (b and $10) <> 0; { set variable space flag }
         if (b and $08) <> 0 then fprterr(esymfmt13); { invalid symbol file format }
         if (b and $04) <> 0 then fprterr(esymfmt14); { invalid symbol file format }
         nxtrld^.adf := (b and $02) <> 0; { set address space flag }
         nxtrld^.def := (b and $01) <> 0; { set defined flag }
         { the 'defined' flag must be set for constant rlds }
         if not nxtrld^.def then fprterr(esymfmt15); { invalid symbol file format }
         rdvar(nxtrld^.val) { get value }

      end

   end else if nxtobj = obblk then begin { block }

      { Blocks preceed their inclusion entries in the linker file, and each
        inclusion entry places itself on the parent list. So the inclusion list
        for this entry will be self-resolving. }
      getblk(nxtblk); { get a block entry }
      rdvar(nxtblk^.startp); { get starting program address }
      rdvar(nxtblk^.endp); { get ending program address }
      rdvar(nxtblk^.startv); { get starting variable address }
      rdvar(nxtblk^.endv) { get ending variable address }

   end else if nxtobj = oblin then begin { line tracking entry }

      getlin(nxtlin); { get a line tracking entry }
      inpstp(i); { get the line difference }
      linnxt := linnxt+i; { find net line count }
      inpstp(i); { get the program difference }
      pgmnxt := pgmnxt+i; { find net program count }
      inpstp(i); { get the variable difference }
      varnxt := varnxt+i; { find net variable count }
      { now set the counts in the entry }
      nxtlin^.line := linnxt;
      nxtlin^.prg := pgmnxt; 
      nxtlin^.vrs := varnxt; 
      nxtlin^.src := srcnxt { set source filename }
      
   end else if nxtobj = obsrc then begin { source file name entry }

      readsymb(l); { get length of filename }
      new(srcnxt, l); { allocate string }
      for i := 1 to l do begin { get string }

         readsymb(b); { get a byte of string }
         srcnxt^[i] := chr(b) { place }

      end;
      { reset the counters }
      linnxt := 0; { line tracking }
      pgmnxt := 0; { program counter }
      varnxt := 0 { variable counter }

   end { else must be end of file or block }

end;      

{*******************************************************************************

Adjust rld entry

Adjusts an rld entry by adding the current program and variable offsets
to the address and value fields, as appropriate by the rld flags.

*******************************************************************************}

procedure adjrld(rld: rldptr);

begin

   rld^.add := rld^.add+poff; { offset address }
   { if value is defined, and in address space, offset by address }
   if rld^.def then begin { value field is defined }

      if rld^.adf then { value is in address space } 
         rld^.val := rld^.val+poff { offset in program space }
      else if rld^.vrs then { value is in variable space }
         rld^.val := rld^.val+voff { offset in variable space }

   end

end;

{*******************************************************************************

Adjust symbol entry

Adjusts a symbol entry by adding the current program and variable offsets
to the value field, as appropriate by the symbol flags.

*******************************************************************************}

procedure adjsym(sym: symptr);

begin

   if sym^.def and (sym^.opr = onop) then begin 

      { symbol is defined and simple }
      if sym^.add then { symbol is in program space }
         sym^.val := sym^.val+poff { offset in program space }
      else if sym^.vrs then { symbol is in variable space }
         sym^.val := sym^.val+voff { offset in variable space }

   end

end;

{*******************************************************************************

Adjust block entry

Adjusts a block entry by adding the current program and variable offsets
to the program and variable fields.

*******************************************************************************}

procedure adjblk(blk: blkptr);

begin

   blk^.startp := blk^.startp+poff; { offset program space start }
   blk^.endp := blk^.endp+poff; { offset program space end }
   blk^.startv := blk^.startv+voff; { offset variable space start }
   blk^.endv := blk^.endv+voff; { offset variable space end }

end;

{*******************************************************************************

Adjust line entry

Adjusts a line entry by adding the current program and variable offsets
to the program and variable fields.

*******************************************************************************}

procedure adjlin(lin: trkptr);

begin

   lin^.prg := lin^.prg+poff; { offset program space }
   lin^.vrs := lin^.vrs+voff { offset variable space }

end;

{*******************************************************************************

Adjust symbol, RLD, block and line decks

Adjusts all of the current symbol, RLD, block and line entries by the current
poff and voff parameters.

*******************************************************************************}

procedure adjusts;

var sp: symptr; { pointer for symbols }
    rp: rldptr; { pointer for rlds }
    bp: blkptr; { pointer for block entries }
    lp: trkptr; { pointer for line tracking entries }

begin

   sp := symtab; { index top of symbols }
   while sp <> nil do begin { traverse }

      adjsym(sp); { adjust symbol entry }
      sp := sp^.next { link next symbol }

   end;
   rp := rldtab; { index top of RLDs }
   while rp <> nil do begin { traverse }

      adjrld(rp); { adjust RLD entry }
      rp := rp^.next { link next RLD }

   end;
   bp := blktab; { index top of blocks }
   while bp <> nil do begin { traverse }

      adjblk(bp); { adjust block entry }
      bp := bp^.next { link next block }

   end;
   lp := linlst; { index top of lines }
   while lp <> nil do begin { traverse }

      adjlin(lp); { adjust line entry }
      lp := lp^.next { link next line }

   end

end;

{*******************************************************************************

Sort symbols for alphabetical order

Places the symbols in acending alphabetical order.
 
*******************************************************************************}

procedure srtalp;

var sp, dp, lp, np: symptr; { symbols pointers }

begin

   dp := nil; { clear desination list }
   while symtab <> nil do begin { sort entry into place }

      sp := symtab; { index top entry }
      symtab := symtab^.next; { gap from original list }
      sp^.next := nil; { terminate }
      if dp = nil then dp := sp { insert at top }
      else if gtrp(sp^.lab^, dp^.lab^) then begin { insert at top }

         sp^.next := dp; { insert into list }
         dp := sp

      end else begin { insert middle or end }

         np := dp; { index top of list }
         while not gtrp(sp^.lab^, np^.lab^) and
               (np^.next <> nil) do begin { traverse }

            lp := np; { set new last }
            np := np^.next { next entry }

         end;
         if gtrp(sp^.lab^, np^.lab^) then begin { insert before }

            sp^.next := np; { link to next }
            lp^.next := sp { link to last }

         end else np^.next := sp { insert after }

      end

   end;
   symtab := dp { place sorted list }

end;

{*******************************************************************************

Sort symbols for numeric order

Places the symbols in acending numeric order.
 
*******************************************************************************}

procedure srtval;

var sp, dp, lp, np: symptr; { symbols pointers }

begin

   dp := nil; { clear desination list }
   while symtab <> nil do begin { sort entry into place }

      sp := symtab; { index top entry }
      symtab := symtab^.next; { gap from original list }
      sp^.next := nil; { terminate }
      if dp = nil then dp := sp { insert at top }
      else if sp^.val < dp^.val then begin { insert at top }

         sp^.next := dp; { insert into list }
         dp := sp

      end else begin { insert middle or end }

         np := dp; { index top of list }
         while (sp^.val >= np^.val) and (np^.next <> nil) do begin { traverse }

            lp := np; { set new last }
            np := np^.next { next entry }

         end;
         if sp^.val < np^.val then begin { insert before }

            sp^.next := np; { link to next }
            lp^.next := sp { link to last }

         end else np^.next := sp { insert after }

      end

   end;
   symtab := dp { place sorted list }

end;

{*******************************************************************************

Find maximum length of symbols

Finds the maximum length of any symbol in the symbol table. This is used to
format tables properly.
 
*******************************************************************************}

procedure fndmax;

var sp: symptr; { pointer for symbols }

begin

   symlen := 0; { clear maximum length of symbols }
   sp := symtab; { index top symbol }
   while sp <> nil do begin { traverse }

      if max(sp^.lab^) > symlen then symlen := max(sp^.lab^); { find max }
      sp := sp^.next { next symbol }

   end

end;

{*******************************************************************************

Find maximum length of filenames

Finds the maximum length of any filename in the source list. This is used to
format tables properly.
 
*******************************************************************************}

procedure srcmax;

var fp: filept;    { pointer for symbols }
    i:  labinx;    { index for labels }
    c:  0..maxfil; { filename length counter }

begin

   srclen := 0; { clear maximum length of source files }
   fp := srclst; { index top filename }
   while fp <> nil do begin { traverse }

      addext(fp^.nam, '   ', true); { clear extention }
      c := 0; { clear label count }
      { count characters in label }
      for i := 1 to maxfil do if fp^.nam[i] <> ' ' then c := c+1;
      if c > srclen then srclen := c; { find max }
      fp := fp^.next { next symbol }

   end

end;

{*******************************************************************************

Report undefined symbols

Searches the current symbols table for undefined entries, and if found,
produces a report on all such entries. The report listing outputs up to
7 symbols on a line. At this version, we are dependent on having only 10
character internal symbols in LN.
 
*******************************************************************************}

procedure report;

var sp:     symptr;  { pointer for symbols }
    first:  boolean; { first undefined print flag }
    symcnt: integer; { count of symbols output on line }
    i:      labinx;  { index for labels }

begin

   first := true; { set first undefined symbol }
   symcnt := 0; { clear output count }
   sp := symtab; { index top symbol }
   while sp <> nil do begin { traverse }

      if (sp^.lab <> nil) and not sp^.def then begin { symbol undefined }

         if first then begin { write header }

            writeln; { space off }
            writeln('Undefined symbols:'); 
            writeln 

         end;
         for i := 1 to max(sp^.lab^) do write(sp^.lab^[i]); { output symbol }
         if (symlen*2+2) < lstlen then begin
 
            { more than one symbol fits on line }
            for i := 1 to symlen-max(sp^.lab^) do write(' '); { pad }
            write(' '); { space off }
            symcnt := symcnt+1; { count symbols output on line }
            if symcnt = lstlen div (symlen+1) then begin { line overflow }

               writeln; { terminate line }
               symcnt := 0 { clear counter }

            end

         end else { max symbol longer than line, just output and let it wrap }
            writeln;
         first := false { set not first undefined }

      end;
      sp := sp^.next { link next symbol }

   end;
   if symcnt <> 0 then writeln { terminate unfinished line }

end;

{*******************************************************************************

List symbols

Lists symbols, values and status in multicollumn format.
 
*******************************************************************************}

procedure listsym;

var sp:     symptr;  { pointer for symbols }
    symcnt: integer; { count of symbols output on line }
    i:      labinx;  { index for labels }

begin

   writeln; { space off }
   symcnt := 0; { clear output count }
   sp := symtab; { index top symbol }
   while sp <> nil do begin { traverse }

      if (sp^.lab <> nil) then begin { symbol entry } 

         for i := 1 to max(sp^.lab^) do write(sp^.lab^[i]); { output symbol }
         for i := 1 to symlen-max(sp^.lab^) do write(' '); { pad }
         write(' '); { space off }
         prthex(digits, sp^.val); { print symbol value }
         write(' '); { space off }
         if sp^.add then write('a') else write('.'); { write address status }
         if sp^.vrs then write('v') else write('.'); { write variable status }
         if sp^.gbl then write('g') else if sp^.ext then write('e') else 
            write('.'); { write variable status }
         if sp^.def then write('d') else write('.'); { write defined status }
         if sp^.opr <> onop then 
            write('c') else write('.'); { write complex/simplestatus }
         write('  ');
         symcnt := symcnt+1; { count symbols output on line }
         if symcnt = lstlen div (digits+symlen+9) then begin { line overflow }

            writeln; { terminate line }
            symcnt := 0 { clear counter }

         end;

      end;
      sp := sp^.next { link next symbol }

   end;
   if symcnt <> 0 then writeln { terminate unfinished line }

end;

{*******************************************************************************

List cross reference

Lists symbols, one per line, followed by a list of references to that symbol.
 
*******************************************************************************}

procedure listxrf;

var sp:     symptr;  { pointer for symbols }
    i:      labinx;  { index for labels }
    rp:     rldptr;  { pointer for rlds }
    adrcnt: integer; { count of addresses output on line }
    ix:     integer; { general index }

begin

   writeln; { space off }
   writeln('Symbols cross reference listing:');
   writeln;
   sp := symtab; { index top symbol }
   while sp <> nil do begin { traverse }

      if (sp^.lab <> nil) then begin { symbol entry } 

         for i := 1 to max(sp^.lab^) do write(sp^.lab^[i]); { output symbol }
         for i := 1 to symlen-max(sp^.lab^) do write(' '); { pad }
         write(' '); { space off }
         prthex(digits, sp^.val); { print symbol value }
         write(' '); { space off }
         if sp^.add then write('a') else write('.'); { write address status }
         if sp^.vrs then write('v') else write('.'); { write variable status }
         if sp^.gbl then write('g') else if sp^.ext then write('e') else 
            write('.'); { write variable status }
         if sp^.def then write('d') else write('.'); { write defined status }
         if sp^.opr <> onop then 
            write('c') else write('.'); { write complex/simplestatus }
         write('  ');
         { search for references to the symbol }
         rp := rldtab; { index top of rld table }
         adrcnt := 0; { set no entries ouput }
         while rp <> nil do begin { traverse }
  
            if rp^.inssym = sp then begin { found a reference }
        
               if symlen+digits+9+((digits+10)*(adrcnt+1)) > lstlen then begin

                  { line would overflow }
                  writeln; { next line }
                  { space over symbol area }
                  for ix := 1 to symlen+digits+9 do write(' ');
                  adrcnt := 0 { set no entries on line }

               end;
               prthex(digits, rp^.add); { print address }
               write(' '); { space off }
               { write endian status }
               if rp^.big then write('b') else write('l');
               { print bit specification }
               write(':', rp^.str:1, ':');
               write(rp^.len div 100); { print length in digits }
               write(rp^.len div 10 mod 10);
               write(rp^.len mod 10);
               write('  '); { space off }
               adrcnt := adrcnt+1 { count entries output }

            end;
            rp := rp^.next { next rld }

         end;
         writeln { terminate line }

      end;
      sp := sp^.next { link next symbol }

   end

end;

{*******************************************************************************

List module parameters

Lists all of the input files, their base addresses, and their lengths.
 
*******************************************************************************}

procedure listmod;

var fp:    filept;  { pointer for source file entries }
    pbase: integer; { base program address of modules }
    vbase: integer; { base variable address of modules }
    i:     filinx;  { index for filenames }
    ix:    integer; { general index }

begin

   writeln; { space off }
   writeln('Module parameter listing:');
   writeln;
   fp := srclst; { index top of source list }
   pbase := poff; { set 1st program base }
   vbase := voff; { set 1st variable base }
   while fp <> nil do begin { list modules }

      for i := 1 to srclen do write(fp^.nam[i]); { output filename }
      write(' '); { space off }
      { write input/output status of file }
      if not fsupp and (fp = srclst) then write('o') else write('i');
      write(' '); { space off }
      prthex(digits, pbase); { output base program address }
      write('-'); { space off }
      if fp^.plen <> 0 then { section has length }
         prthex(digits, pbase+fp^.plen-1) { output end program address }
      else for ix := 1 to digits do write('*'); 
      write(' '); { space off }
      prthex(digits, fp^.plen); { output module program length }
      write(' '); { space off }
      prthex(digits, vbase); { output base variable address }
      write('-'); { space off }
      if fp^.vlen <> 0 then { section has length }
         prthex(digits, vbase+fp^.vlen-1) { output end variable address }
      else for ix := 1 to digits do write('*'); 
      write(' '); { space off }
      prthex(digits, fp^.vlen); { output module variable length }
      writeln;
      if fsupp or (fp <> srclst) then begin { not output file }

         pbase := pbase+fp^.plen; { find next module program base }
         vbase := vbase+fp^.vlen  { find next module variable base }

      end;
      fp := fp^.next { next file entry }

   end

end;

{*******************************************************************************

Output object value
 
The given unsigned integer is output to the object file.
The number of bytes occupied by the output value can be specified, as well as 
the big/little endian structure of the output.
If the length specified is greater than the size of an integer, padding sign 
extention bytes will be used to create an effective output of that size.
This routine is dependent on integer being 32 bits, and uses equivalence of 
"packed array [1..4] of byte" to integer to extract the value of an integer.
 
*******************************************************************************}

procedure outval(val: integer;  { object integer to output }
                 len: integer;  { number of bytes to occupy }
                 big: boolean); { big endian format }

var i: integer; { counter }
    c: record case boolean of { convertion }

          false: (a: packed array [1..4] of byte);
          true:  (b: integer)

       end;
    s: integer; { sign holder }

begin

   if val < 0 then s := 255 else s := 0; { set sign extention byte }
   c.b := val; { convert integer to bytes }
   if big then begin { big endian }

      { pad > 32 bits }
      while len > 4 do begin wrtobjb(s); len := len-1 end;
      for i := len downto 1 do { output bytes }
         wrtobjb(c.a[i]) { output byte }

   end else begin { little endian }

      { output bytes to maximum of 4 }
      if len > 4 then for i := 1 to 4 do wrtobjb(c.a[i]) { output byte }
      else for i := 1 to len do wrtobjb(c.a[i]); { output byte }
      { pad > 32 bits }
      while len > 4 do begin wrtobjb(s); len := len-1 end

   end

end;

{*******************************************************************************

Output composite bit field

Outputs a value as a composite bit field. Given the inserted value, the backing 
value, and the start and length of bits to be inserted, a series of bytes is 
created with the insertion value imbedded.
Note: dependant on being able to use 'and', 'or' and 'not' on integers.

*******************************************************************************}

procedure outbit(val: integer;  { object integer to output }
                 bak: integer;  { backing value }
                 big: boolean;  { big endian format }
                 str: integer;  { start of insertion }
                 len: integer); { number of bits to occupy }

var mask: integer;
    t:    integer; { holding }

begin

   if (str = 0) and ((len mod 8) = 0) then
      { its just an ordinay bytewise insertion, in which case the
        backing is not used }
      outval(val, len div 8, big)
   else begin { bitwise insertion }

      { form bitmask }
      mask := 1;
      t := len-1;
      while t <> 0 do begin mask := mask*2+1; t := t-1 end;
      { shift up to proper bit position }
      t := str;
      while t <> 0 do begin 

         val := val*2; { shift value }
         mask := mask*2; { shift mask }
         t := t-1 { count }

      end;
      { assemble output value }
      val := (bak and not mask) or (val and mask);       
      t := (len+str) div 8; { find total byte length }
      if ((len+str) mod 8) <> 0 then t := t + 1; { round up }
      outval(val, t, big) { and output final value }

   end

end;

{*******************************************************************************

Input backing value

Inputs a backing value from the input object file. This is a byte constructed 
word value that has enough bytes to cover the given byte count, and has the 
same endian mode.
Note that it is taken on trust that the number of bytes requested exists before
the eof.

*******************************************************************************}

procedure inpbak(var bak: integer;  { returns backing value }
                     big: boolean;  { big endian format }
                     siz: integer); { size of value in bytes }

var b: byte;    { input byte holder }
    p: integer; { power holder }

begin

   bak := 0; { clear backing value }
   if big then while siz <> 0 do begin { read big endian }

      readobjb(b); { get a byte }
      bak := bak*256+b; { scale backing value and add }
      siz := siz-1 { count bytes }
      
   end else begin { little endian }

      p := 1; { set first power }
      while siz <> 0 do begin { read little endian }
  
         readobjb(b); { get a byte }
         bak := bak+b*p; { scale and add }
         siz := siz-1 { count bytes }

      end

   end

end;

{***************************************************************

Output variger

Outputs the given integer to the byte file as a variger.
Varigers are of the following format:

   1. (byte) the tag byte.
   2-N. The variger value.

The tag byte values are:

   bit 7 - Low for integer number, high for float.
   bit 6 - Contains the sign of the integer. 
   bit 5 - Unused.
   bit 4 - Length of integer in bytes, 1-32, in -1 format.
   bit 3 -      ""              ""
   bit 2 -      ""              ""
   bit 1 -      ""              ""
   bit 0 -      ""              ""

The integer is converted by removing the sign bit and converting
to signed magnitude, then determining the byte size, then
outputting the tag and number.

***************************************************************}

procedure wrtvar(n: integer); { integer to output}

var t: integer; { tag byte }
    p: integer; { power holder }

begin

   { handle 0 as special case }
   if n = 0 then begin wrtsymb(0); wrtsymb(0) end else begin

      { value is non-zero }
      t := bytes-1; { initalize tag field to max bytes }
      if n < 0 then begin { remove sign and convert to signed magnitude }
      
         t := t + $40; { place sign in tag }
         n := abs(n) { find absolute value of integer }
      
      end;
      p := toppow; { get top power }
      { find 1st non-zero digit in integer }
      while (n div p) = 0 do begin p := p div 256; t := t - 1 end;
      wrtsymb(t); { output finalized tagfield }
      while p <> 0 do begin { output bytes }
      
         wrtsymb(n div p); { output that byte }
         n := n mod p; { remove the byte }
         p := p div 256 { next lower power }      
      
      end

   end

end;

{***************************************************************
 
Output IT field

Outputs the it field, of the following format:

     ------------------
     | IT | LEN | OFF |
     ------------------

     IT is the IT flag byte

     LEN is the bit length of insertion -1

     OFF is the PC offset constant

Definition of the IT flag byte:

     bit
     7 - Low for "big endian" format insertion, high for 
         "little endian".
     6 - Low for normal insertion, high for "signed offset" 
         insertion.
     5 - Constant offset flag.
     4 - Unused
     3 - Unused
     2 - Starting bit offset (0-7)
     1 -      ""          ""
     0 -      ""          ""
 
Note that the offset field will only be present if the constant
offset flag is set.

***************************************************************}

procedure outit(r: rldptr); { rld to output it for }

var t: byte; { holding }

begin

   t := 0; { clear flag byte }
   if r^.big then t := t + $80; { place big endian flag }
   t := t+ord(r^.im)*$08; { place insertion type }
   t := t + r^.str; { place bit offset }
   wrtsymb(t); { output insertion flags }
   wrtsymb(r^.len-1); { output bit length }
   { output constant offset if required }
   if r^.cof <> 0 then wrtsymb(r^.cof)

end;

{*******************************************************************************

Output basic rld entry

Outputs a symbol entry to the symbols file.

*******************************************************************************}

procedure wrnrld(rld: rldptr); { rld to write }

var b: byte; { output byte holder }

begin

   if rld^.def then wrtsymb(ord(obcrld)) { type constant rld }
   else wrtsymb(ord(obrld)); { type general rld }
   outit(rld); { output IT field }   
   wrtvar(rld^.add); { output address }
   if rld^.def then begin { constant rld }

      b := 1; { set defined flag }
      if rld^.vrs then b := b+$10; { set variable space flag }
      if rld^.adf then b := b+$02; { set address space flag }
      wrtsymb(b); { output flag byte }
      wrtvar(rld^.val) { output value }

   end

end;

{*******************************************************************************

Output basic symbol entry

Outputs a symbol entry to the symbols file.

*******************************************************************************}
 
procedure wrnsym(sym: symptr); { symbol to write }

var i: labinx; { index for label }
    b: byte;   { output byte holder }

begin

   if sym^.lab <> nil then wrtsymb(ord(obsym)) { type symbol }
   else wrtsymb(ord(obcst)); { type constant }
   wrtsymb(ord(sym^.opr)); { output operation }
   if sym^.lab <> nil then begin { symbol }

      wrtsymb(max(sym^.lab^)-1); { output label length }
      { output label characters }
      for i := 1 to max(sym^.lab^) do wrtsymb(chr2ascii(sym^.lab^[i]))

   end;
   b := 0; { clear flags byte }
   if sym^.vrs then b := b+$10; { set variable space flag }
   if sym^.ext then b := b+$08; { set external flag }
   if sym^.gbl then b := b+$04; { set global flag }
   if sym^.add then b := b+$02; { set address space flag }
   if sym^.def then b := b+$01; { set defined flag }
   wrtsymb(b); { output flags byte }
   if sym^.def then { symbol is defined }
      wrtvar(sym^.val) { output symbol value }

end;

{*******************************************************************************

Output constant rlds

Outputs all the stand-alone rlds, which are what make up the relocation
dictionary when all the expressions are resolved. If the rld is not an
address or variable space rld (which may change further due to relocation),
or is an offset entry, and is defined (is a constant rld), it is not output.
This can happen because the patch location in the output object file has
already been modified, and will never change again. This applies even to offset
entries, because an offset is independent to the location of the program.

*******************************************************************************}

procedure outrlds;

var rp: rldptr; { pointer for rlds }

begin

   rp := rldtab; { index rld top }
   while rp <> nil do begin { traverse }

      if rp^.def and (rp^.adf or rp^.vrs) and 
         not (rp^.im = imsgof) and not (rp^.im = imnsof) then
         { defined address space or variable space, and not offset.
           We don't output signed offset because these don't change 
           after initial computation }
         wrnrld(rp);
      rp := rp^.next { link next entry }

   end

end;

{*******************************************************************************

Write rlds for symbol

Writes all rlds indexing the given symbol.
The time taken to search through the rld table for links could be mitigated
by keeping a linked list of referencing rlds.

*******************************************************************************}

procedure outrld(sym: symptr); { symbol to output rlds for }

var rp: rldptr; { pointer for rlds }

begin

   rp := rldtab; { index top of rlds }
   while rp <> nil do begin { traverse }

      if rp^.inssym = sym then wrnrld(rp); { if references symbol, output }
      rp := rp^.next { link next }

   end

end;

{*******************************************************************************

Output skeletal symbol

*******************************************************************************}

procedure wrsym(sym: symptr); { symbol to output }

var tp: symptr; { pointer for symbol }

begin

   if ftrim and sym^.def and not sym^.gbl and not sym^.ext then begin 

      { output in "trim" mode }
      getsym(tp); { get a temp symbol }
      tp^ := sym^; { copy entire symbol }
      tp^.lab := nil; { but remove label }
      wrnsym(tp); { write symbol }
      putsym(tp); { release entry }
      if sym^.lft <> nil then wrsym(sym^.lft); { output skeleton of left }
      if sym^.rgt <> nil then wrsym(sym^.rgt) { output skeleton of left }

   end else { output in normal mode }
      if sym^.lab = nil then begin { output normal if constant }

         wrnsym(sym); { output top symbol }
         if sym^.lft <> nil then wrsym(sym^.lft); { output left if exists }
         if sym^.rgt <> nil then wrsym(sym^.rgt) { output left if exists }

      end else begin { output skeleton }

         getsym(tp); { get a temp symbol }
         tp^ := sym^; { copy entire symbol }
         tp^.def := false; { but set undefined }
         wrnsym(tp); { write symbol }
         { remove label so dynamic string does not get recycled }
         tp^.lab := nil;
         putsym(tp); { release entry }

      end

end;

{*******************************************************************************

Trim rlds

The rlds indexing the symbol are changed to constant rlds by placing the
symbols value in the rld entry, and changing the rld type.

*******************************************************************************}

procedure trmrld(sym: symptr); { symbol to trim rlds for }

var rp: rldptr; { index for rlds }

begin

   rp := rldtab; { index top rld }
   while rp <> nil do begin { traverse }

      if rp^.inssym = sym then begin { found a referencing rld }

         rp^.val := sym^.val; { place symbol value }
         rp^.inssym := nil; { clear symbol linkage }
         rp^.def := true; { set defined }
         rp^.adf := sym^.add; { copy address space flag }
         rp^.vrs := sym^.vrs { copy variable space flag }

      end;
      rp := rp^.next { link next entry }

   end

end;

{*******************************************************************************

Write symbol

The symbol is written to the output symbols file. Any rlds indexing the symbol
are then output. Then the symbols dependance tree (the subentrys that define the
value of the symbol) are output in "skeletal" form. "skeletal" means they are
converted to undefined placeholders. These entries only serve to match up to the
"master" entries when the symbols file is read back in again, and so recreate
the tree.

If the "trim" flag is on, we modify how we output the symbol. If the symbol is
global or external, it is output normally, as such symbols cross module
barriers. If the symbol is defined, it will not appear at all in the output
file, and all it's associated rlds are changed to constant rlds.

This is possible because the value of the symbol will no longer change, and
may not be referenced further.

*******************************************************************************}

procedure wrtsym(sp: symptr);

var tp: symptr;  { temp pointer for symbols }

begin

   if ftrim then begin { output in "trim" mode }

      if sp^.ext or sp^.gbl then begin 

         { global or external, output normally }
         wrnsym(sp); { output symbol }
         outrld(sp); { output associated rlds }
         if sp^.lft <> nil then wrsym(sp^.lft); { output skeleton of left }
         if sp^.rgt <> nil then wrsym(sp^.rgt) { output skeleton of left }

      end else if sp^.def then { symbol defined, eliminate entirely }
         trmrld(sp) { remove associated rlds, as they have been resolved }
      else begin { symbol to be "trimmed", or converted to constant }

         getsym(tp); { get a temp symbol }
         tp^ := sp^; { copy entire symbol }
         tp^.lab := nil; { but remove label }
         wrnsym(tp); { write symbol }
         putsym(tp); { release entry }
         outrld(tp); { output associated rlds }
         if sp^.lft <> nil then wrsym(sp^.lft); { output skeleton of left }
         if sp^.rgt <> nil then wrsym(sp^.rgt) { output skeleton of left }

      end

   end else begin { output in normal mode }

      wrnsym(sp); { output symbol }
      outrld(sp); { output associated rlds }
      if sp^.lft <> nil then wrsym(sp^.lft); { output skeleton of left }
      if sp^.rgt <> nil then wrsym(sp^.rgt) { output skeleton of left }

   end

end;

{*******************************************************************************

Output block description

Outputs a complete block description. The block header is output, followed by
the inclusion list for the block.
 
*******************************************************************************}

procedure wrtblk(bp: blkptr);

var ip: bicptr; { inclusion pointer }

{ output included blocks }

procedure outblk(ip: bicptr);

begin

   while ip <> nil do begin

      { if there is a subblock, output it }
      if ip^.blk <> nil then wrtblk(ip^.blk);
      ip := ip^.next { next inclusion }

   end

end;

begin

   wrtsymb(ord(obblk)); { output object type }
   wrtvar(bp^.startp); { output starting program address }
   wrtvar(bp^.endp); { output ending program address }
   wrtvar(bp^.startv); { output starting variable address }
   wrtvar(bp^.endv); { output ending variable address }
   outblk(bp^.inclst); { output any subblocks }
   ip := bp^.inclst; { index top of inclusion list }
   while ip <> nil do begin { output inclusion list }

      { check there is an included symbol }
      if ip^.sym <> nil then wrtsym(ip^.sym); { output symbol }
      ip := ip^.next { next }

   end;
   wrtsymb(ord(obblke)); { output block end }
   bp^.outp := true { set this block is output }

end;

{*******************************************************************************

Output block list

Outputs the block list to the symbols file.

We output the blocks in nested order, that is, each block is followed by it's
subblock entries, then it's inclusion list:

block

   [block]
   inclusions

endblock

The block is included in the block it is nested in, and each symbol inclusion
belongs to the last block entry output. This means readers must keep a pushdown
list of active blocks to correctly assign entries to their parents.
 
*******************************************************************************}

procedure wrtblks;

var bp: blkptr; { block pointer }

begin

   bp := blktab; { index the top of the finished block list }
   while bp <> nil do begin { traverse }

      { if block not previously output }
      if not bp^.outp then wrtblk(bp); { output this block }
      bp := bp^.next { next block }

   end

end;

{*******************************************************************************

Output line list

Outputs the line list to the symbols file. The line list is a series of 
difference values starting from zero. Each of the line, program and variable
counters gets a difference value in the stepped difference format. This makes
the line list self starting, so that only the line list need be read from the
symbols deck, and the program origin parameters don't need to be read.
 
*******************************************************************************}

procedure wrtlins;

var lp:     trkptr;  { line tracking pointer }
    llncnt: integer; { last line }
    lprgmc: integer; { last program address }
    lglblc: integer; { last variable address }
    lsrc:   pstring; { current source file }
    l:      integer;
    i:      integer;

begin

   { start off difference counters at 0 }
   llncnt := 0;
   lprgmc := 0;
   lglblc := 0;
   lsrc := nil; { set no source file active }
   lp := linlst; { index the top of the finished line list }
   while lp <> nil do begin { traverse }

      { check both last and present have source filenames }
      if (lsrc <> nil) and (lp^.src <> nil) then begin 

         { compare last and this to see if the source filename has changed }
         if not compp(lsrc^, lp^.src^) then begin

            { filename has changed, output change marker }
            wrtsymb(ord(obsrc)); { output source name entry }
            l := len(lp^.src); { find length }
            wrtsymb(l); { output length }
            { output filename }
            for i := 1 to l do wrtsymb(chr2ascii(lp^.src^[i]))

         end

      end;
      write(symfil, ord(oblin)); { output diff set marker }
      outstp(lp^.line-llncnt); { differential line }
      outstp(lp^.prg-lprgmc); { differential program }
      outstp(lp^.vrs-lglblc); { differential variable }
      llncnt := lp^.line; { copy present to last line count }
      lprgmc := lp^.prg; { copy present to last program count }
      lglblc := lp^.vrs; { copy present to last variables count }
      lsrc := lp^.src; { copy present to last source filename }
      lp := lp^.next { go next entry }

   end

end;

{*******************************************************************************

Output symbols file

Outputs the final symbols deck, rlds and blocks. Each symbol is examined, and
written to the output symbols file. Any rlds indexing the symbol are then
output. Then the symbols dependance tree (the subentrys that define the value of
the symbol) are output in "skeletal" form. "skeletal" means they are converted
to undefined placeholders. These entries only serve to match up to the "master"
entries when the symbols file is read back in again, and so recreate the tree.

After all symbols are output, the uncommited rlds (constant rlds) are output.
If the "trim" flag is on, we modify how we output symbols. If the symbol is
global or external, it is output normally, as such symbols cross module
barriers. If the symbol is defined, it will not appear at all in the output
file, and all it's associated rlds are changed to constant rlds.

This is possible because the value of the symbol will no longer change, and
may not be referenced further.

After the rlds are output, the block structure is output.

*******************************************************************************}

procedure outsym;

var sp: symptr; { pointer for symbols }

begin

   sp := symtab; { index top of symbols }
   while sp <> nil do begin { traverse symbols }

      { check symbol is an "orphan", or belongs to no block }
      if sp^.par = nil then wrtsym(sp); { write symbol to symbols file }
      sp := sp^.next { link next symbol }
         
   end;
   outrlds; { output current rlds }
   wrtblks; { output current block structures }
   wrtlins; { output line list }
   wrtsymb(ord(obend)) { write end of file }

end;

{*******************************************************************************

Process object

Copies the input object file to the output object file. While copying, any
pending rld's are "mixed" into the output object file.

*******************************************************************************}

procedure prcobj;

var objlen: integer; { output bytes count }
    b:      byte;    { I/O byte holder }
    bytes:  integer; { number of bytes in bit field }
    v:      integer; { insertion value }
    bak:    integer; { backing value }
    t:      integer; { temp }
    i:      integer; { index }
    proc:   boolean; { processed flag }
    seg:    integer; { segment }
    off:    integer; { offset }

begin

   objlen := cursrc^.plen; { get the length of this input object }
   while objlen <> 0 do begin { read object bytes }

      proc := false; { set next not processed }
      if rldinx <> nil then begin { there is a next rld entry }

         { If we passed the next address, this is a format problem, typically
           overlapping addresses in the rld list. }
         if prgmc > rldinx^.add then fprterr(esymfmt35);
         if rldinx^.add = prgmc then begin { found an rld patchpoint, process }

            { find byte length of insertion field }
            bytes := (rldinx^.len+rldinx^.str) div 8; { find total byte length }
            if ((rldinx^.len+rldinx^.str) mod 8) <> 0 then 
               bytes := bytes+1; { round up }
            if rldinx^.inssym <> nil then { symbol exists }
               v := rldinx^.inssym^.val { value is in symbol }
            else v := rldinx^.val; { value is constant }
            if rldinx^.im = imiseg then begin

               { check proper format specified, either 4 or 6 byte seg/off }
               if (bytes <> 4) and (bytes <> 6) then prterr(esymfmt37);
               { mode is Intel segment, process specially }
               seg := v div 16; { find segment }
               off := v mod 16; { find offset }
               { check if the segment overflowed }
               if (bytes = 4) and (seg >= 65536) then prterr(efldovf);
               for i := 1 to bytes do readobjb(b); { skip input field }
               outval(off, bytes-2, rldinx^.big); { output offset }
               outval(seg, 2, rldinx^.big) { output segment }

            end else begin { standard processing }

               if (rldinx^.im = imsgof) or (rldinx^.im = imnsof) then begin
         
                  { type is signed offset, find displacement }
                  v := v-(prgmc+bytes+rldinx^.cof)
         
               end;
               t := v; { copy value }
               { move off all bits to output, which should leave only 0 or -1 }
               for i := 1 to rldinx^.len do t := t div 2;
               if (t <> 0) and (t <> -1) then begin { value overflow }
         
                  errval := v; { place error value }
                  errbits := rldinx^.len; { place error bit length }
                  prterr(efldovf); { value overflows }
         
               end;
               inpbak(bak, rldinx^.big, bytes); { get backing value }
               { output final composite }
               outbit(v, bak, rldinx^.big, rldinx^.str, rldinx^.len)

            end;
            objlen := objlen-bytes; { find advance in input }
            prgmc := prgmc+bytes; { find advance in program }
            rldinx := rldinx^.next; { index next rld entry }
            proc := true { set processed }


         end
         
      end;
      if not proc then begin { transfer input to output object bytes }

         readobjb(b); { get an input byte }
         wrtobjb(b); { output to final }
         prgmc := prgmc+1; { advance final program counter }
         objlen := objlen-1 { count input bytes }

      end

   end

end;

{*******************************************************************************

Originate module

Sets the offsets required to acheive the program and variable locations LN is
given. By default, the program frame is located at 0, and the variable frame is
placed at the end of that. However, the program frame or the variable frame
or both can be set anywhere. If the user has set a variable frame, then the
"variable after program" mode is overridden, and the variable frame will be
located where specified.

*******************************************************************************}

procedure origin;

begin

   poff := pgmloc; { set program offset }
   if fvset then voff := varloc { set variable offset to specified }
   else voff := pend^.val+poff; { set variable space to the end of program }
   if not fsupp then begin { place final output module definitions }

      srclst^.plen := pend^.val-pstr^.val; { place program length }
      srclst^.vlen := vend^.val-vstr^.val  { place variable length }

   end;
   adjusts { run offset pass }

end;

{*******************************************************************************

Sort rld table

Sorts the rld table into address acending order. This is done so that the
entries appear in order when we process the object.

Since we really want to do a quicksort for speed, what we do is to create a
custom array with the number of rld entries we need, then quicksort that.
The hit for this is a pointer word per rld, and this only exists during this
sort function.

*******************************************************************************}

procedure srtrld;

type rldarr = array of rldptr; { rld sorting array }

var rp:     rldptr;  { rld pointers }
    srttbl: ^rldarr; { rld sorting array }
    i:      integer; { index for that }
    c:      integer; { count of rlds }

{ perform quicksort }

procedure sort(l, r: integer);

var i, j: integer; { table indexes }
    x, w: rldptr;  { entry holders }

begin

   i := l; { set indexes to min and max }
   j := r;
   x := srttbl^[(l+r) div 2]; { pick up the middle element }
   repeat

      { find lower entry out of place with respect to x }
      while srttbl^[i]^.add > x^.add do i := i+1;
      { find upper entry out of place with respect to x }
      while x^.add > srttbl^[j]^.add do j := j-1;
      { perform exchange }
      if i <= j then begin { exchange elements }

         w := srttbl^[i]; 
         srttbl^[i] := srttbl^[j];
         srttbl^[j] := w;
         i := i+1;
         j := j-1

      end

   until i > j;
   if l < j then sort(l, j); { sort lower partition }
   if i < r then sort(i, r) { sort upper partition }

end;

begin

   if rldtab <> nil then begin { table not null }

      c := 1; { clear rld count }
      rp := rldtab; { index top rld }
      { count rlds }
      while rp^.next <> nil do begin rp := rp^.next; c := c+1 end;
      new(srttbl, c); { allocate sorting array }
      i := 1; { index 1st array position }
      rp := rldtab; { index 1st entry }
      for i := 1 to c do begin { copy all pointers to array }

         srttbl^[i] := rp; { place rld pointer }
         rp := rp^.next { next entry }

      end;
      sort(1, c); { perform sort }
      rldtab := nil; { clear destination list }
      { just to make things easier, we sorted the list for decending order,
        then insert it backwards }
      for i := 1 to c do begin { copy table to list }

         srttbl^[i]^.next := rldtab; { link entry into list at top }
         rldtab := srttbl^[i]

      end;
      dispose(srttbl) { release sorting table }

   end

end;

{*******************************************************************************

Check rlds

Checks if any of the rlds are overlapping. The RLD list should be sorted for
ascending order. The list is then checked if any of the rlds overlap, 
considering their start address and byte length.

Note that this is a fault check, and just issues input format errors. This is
because the feeder programs should never generate a linker file with overlapping
rlds.

*******************************************************************************}

procedure chkrld;

var bc: integer; { number of bytes in bit field }
    rp: rldptr;  { pointer for rld list }
    pc: integer; { program counter }

begin

   rp := rldtab; { index top of rld table }
   pc := pgmloc; { index program code location }
   while rp <> nil do begin { traverse }

      { If we passed the next address, this is a format problem, typically
        overlapping addresses in the rld list. }
      if pc > rp^.add then fprterr(esymfmt36)
      else pc := rp^.add; { otherwise move to that address }
      bc := (rp^.len+rp^.str) div 8; { find total byte length }
      if ((rp^.len+rp^.str) mod 8) <> 0 then 
         bc := bc+1; { round up }
      pc := pc+bc; { advance past this patch }
      rp := rp^.next { next rld }

   end

end;

{*******************************************************************************

Perform symbol operator

If the operands of a symbol are defined, then the operation on a symbol is
performed. On program or variable space values, these operations must be
done on every link resolve operation (because they can always be relocated
again). Otherwise, this may be the final resolution of the symbol.

*******************************************************************************}

procedure symopr(sym: symptr); { symbol to operate on }

var def: boolean; { operand(s) are defined flag }
    c:   integer; { count }

begin

   { validate the correct parameters exist }
   if (sym^.opr in [onot, oneg]) and 
      ((sym^.lft = nil) or (sym^.rgt <> nil)) then fprterr(esymfmt16)
   else if (sym^.opr in [oadd, osub, omult, odiv, omod, oshl, oshr, oand, oor,
            oxor]) and
           ((sym^.lft = nil) or (sym^.rgt = nil)) then fprterr(esymfmt17)
   else if (sym^.opr = onop) and 
           ((sym^.lft <> nil) or (sym^.rgt <> nil)) then fprterr(esymfmt18);
   def := true; { set operand(s) defined }
   { check left branch exists and is defined }
   if sym^.lft <> nil then if not sym^.lft^.def then def := false;
   { check right branch exists and is defined }
   if sym^.rgt <> nil then if not sym^.rgt^.def then def := false;
   if def then begin { operand(s) defined }

      case sym^.opr of { operation }

         onop:  ; { no operation }
         oadd:  sym^.val := sym^.lft^.val+sym^.rgt^.val; { add }
         osub:  sym^.val := sym^.lft^.val-sym^.rgt^.val; { subtract }
         omult: sym^.val := sym^.lft^.val*sym^.rgt^.val; { multiply }
         odiv:  sym^.val := sym^.lft^.val div sym^.rgt^.val; { divide }
         omod:  sym^.val := sym^.lft^.val mod sym^.rgt^.val; { modulo }
         oshl, oshr:  begin { shift }
     
            c := sym^.rgt^.val; { get shift value }
            sym^.val := sym^.lft^.val; { get value to shift }
            if sym^.opr = oshr then c := -c; { change direction of shift }
            if c < 0 then begin { negative shift }

               while (sym^.val > 0) and (c < 0) do begin

                  sym^.val := sym^.val div 2; { shift right }
                  c := c+1 { count }

               end

            end else begin

               { Note this next relies on having overflow off, because it will
                 shift the value off the top of the integer. }
               while (sym^.val > 0) and (c > 0) do begin

                  sym^.val := sym^.val*2; { shift left }
                  c := c-1 { count }

               end

            end
     
         end;
         oand:  sym^.val := sym^.lft^.val and sym^.rgt^.val; { and }
         oor:   sym^.val := sym^.lft^.val or sym^.rgt^.val; { or }
         { exclusive or }
         oxor:  sym^.val := (sym^.lft^.val and not sym^.rgt^.val) or
                            (not sym^.lft^.val and sym^.rgt^.val);
         onot:  sym^.val := not sym^.lft^.val; { not }
         oneg:  sym^.val := -sym^.lft^.val { negate }

      end;
      if sym^.opr <> onop then { not a simple symbol }
         sym^.def := true { set resulting symbol now defined }

   end

end;

{*******************************************************************************

Reduce symbol

Performs any operations possible on the given symbol, and frees of any entries
as possible.

*******************************************************************************}

procedure redsym(sym: symptr);

{ attempt to free up symbols entry }

procedure free(var sym: symptr); 

begin

   if sym <> nil then begin { there is a symbol }

      if sym^.lab = nil then begin

         delsym(sym); { delete entry from symbols list }
         putsym(sym); { release symbol entry }

      end;
      sym := nil { remove index }

   end

end;

begin

   if sym^.lft <> nil then begin { left branch exists }

      { add any attributes of subsymbol to this symbol }
      if sym^.lft^.add then sym^.add := true;
      if sym^.lft^.vrs then sym^.vrs := true;
      redsym(sym^.lft) { reduce }

   end;
   if sym^.rgt <> nil then begin { right branch exists }

      { add any attributes of subsymbol to this symbol }
      if sym^.rgt^.add then sym^.add := true;
      if sym^.rgt^.vrs then sym^.vrs := true;
      redsym(sym^.rgt) { reduce }

   end;
   symopr(sym); { perform operation on current symbol }
   if sym^.def and not (sym^.add or sym^.vrs) then begin 

      { value is defined, and not in address or variable space }
      sym^.opr := onop; { reduce to simple symbol }
      free(sym^.lft); { attempt to free left }
      free(sym^.rgt)  { attempt to free right }

   end

end;

{*******************************************************************************

Reduce symbols

Performs any operations possible on the symbols deck, then disposes of any 
freed entries.

*******************************************************************************}

procedure reduce;

var sp: symptr; { pointer for symbols }

begin

   sp := symtab; { index top of symbols table }
   while sp <> nil do begin { traverse }

      redsym(sp); { reduce symbol }
      sp := sp^.next { link next symbol }

   end

end;

{*******************************************************************************

Append symbol table

The start and end addresses for the program and variable spaces in the
current symbols deck are recalculated so that the frames stack ontop of the
old saved deck. Then, the current symbols and rld decks are recalcuated entry
by entry to match the new frame addresses. 
Also assigns the current module lengths.

*******************************************************************************}

procedure apnsym;

begin

   { find old pend - new pstr, to get offset that will place new code at old
     pend }
   poff := pendod-pstrnw;
   { find old vend - new vstr, to get offset that will place new code at old
     vend }
   voff := vendod-vstrnw;
   cursrc^.plen := pendnw-pstrnw; { place program length of module }
   cursrc^.vlen := vendnw-vstrnw; { place variable length of module }
   adjusts; { adjust current deck by offsets }
   pstrod := 0; { find combined old and new pstr }
   pendod := pendod+(pendnw-pstrnw); { find combined old and new pend }
   vstrod := 0; { find combined old and new vstr }
   vendod := vendod+(vendnw-vstrnw); { find combined old and new vend }
   pstr^.val := pstrod; { copy values to the actual symbol entries }
   pend^.val := pendod;
   vstr^.val := vstrod;
   vend^.val := vendod

end;

{*******************************************************************************

Check link parameter

Checks if the given symbol is one of the link parameters:

   _pstr - Program start
   _pend - Program end
   _vstr - Variable start
   _vend - Variable end

If so, then these symbols are processed specially. First, the value of the
symbol is divorced from the symbol entry itself, so that we may have only
one symbol by that name in the entire symbol table(s). The value is placed
as the "new" value. Then, if the symbol is the first such symbol to appear,
it is placed as the master entry. Otherwise, it is disposed of and the old
master entry returned instead.

*******************************************************************************}

procedure chkpar(var sym: symptr; { symbol to check for, returns master }
                 var fnd: boolean); { symbol is link parameter }

begin

   fnd := false; { set no parameter found }
   if compp(sym^.lab^, '_pstr') then begin { program start }

      { check defined, address space, global }
      if not (sym^.def and sym^.add and sym^.gbl) then fprterr(esymfmt19);
      if pstrf then fprterr(esymfmt20); { more than one in file }
      pstrf := true; { set parameter found }
      pstrnw := sym^.val; { set new program start value }
      if pstr = nil then begin { no previous entry }

         sym^.next := symtab; { link into symbols table }
         symtab := sym;
         pstr := sym { set master }

      end else begin { duplicate entry }

         putsym(sym); { dispose of new symbol }
         sym := pstr { return old master }

      end;
      fnd := true { set parameter found }

   end else if compp(sym^.lab^, '_pend') then begin { program end }

      { check defined, address space, global }
      if not (sym^.def and sym^.add and sym^.gbl) then fprterr(esymfmt21);
      if pendf then fprterr(esymfmt22); { more than one in file }
      pendf := true; { set parameter found }
      pendnw := sym^.val; { set new program end value }
      if pend = nil then begin { no previous entry } 

         sym^.next := symtab; { link into symbols table }
         symtab := sym;
         pend := sym { no previous entry, set master }

      end else begin { duplicate entry }

         putsym(sym); { dispose of new symbol }
         sym := pend { return old master }

      end;
      fnd := true { set parameter found }

   end else if compp(sym^.lab^, '_vstr') then begin { variable start }

      { check defined, variable space, global }
      if not (sym^.def and sym^.vrs and sym^.gbl) then fprterr(esymfmt23);
      if vstrf then fprterr(esymfmt24); { more than one in file }
      vstrf := true; { set parameter found }
      vstrnw := sym^.val; { set new variable start value }
      if vstr = nil then begin { no previous entry }

         sym^.next := symtab; { link into symbols table }
         symtab := sym;
         vstr := sym { set master }

      end else begin { duplicate entry }

         putsym(sym); { dispose of new symbol }
         sym := vstr { return old master }

      end;
      fnd := true { set parameter found }

   end else if compp(sym^.lab^, '_vend') then begin { variable end }

      { check defined, variable space, global }
      if not (sym^.def and sym^.vrs and sym^.gbl) then fprterr(esymfmt25);
      if vendf then fprterr(esymfmt26); { more than one in file }
      vendf := true; { set parameter found }
      vendnw := sym^.val; { set new variable end value }
      if vend = nil then begin { no previous entry }

         sym^.next := symtab; { link into symbols table }
         symtab := sym;
         vend := sym { no previous entry, set master }

      end else begin { duplicate entry }

         putsym(sym); { dispose of new symbol }
         sym := vend { return old master }

      end;
      fnd := true { set parameter found }

   end

end;

{*******************************************************************************

Read rlds attached to symbol

Reads any rld's that may follow a symbol, and attaches them to the given
symbol, as the symbol provides the solution for that rld.

*******************************************************************************}

procedure rdrlds(sym: symptr); { symbol to attach to }

begin

   while nxtobj = obrld do begin { read rlds }

      nxtrld^.next := rldtab; { link into current rld table }
      rldtab := nxtrld;
      adjrld(nxtrld); { adjust entry }
      nxtrld^.inssym := sym; { link to symbol }
      rdnxt { read next entry }

   end

end;

{*******************************************************************************

Read symbol

Reads a symbol entry from the symbols file. The next symbol is read, and also
any symbols in "subtrees" under the symbol. In this way, the entire expression
tree that represents an undefined symbol can be read. Also reads any rld's
referencing the symbol, or symbols under it.

Returns the symbol entry, as it is entered into the symbol table.

*******************************************************************************}

procedure rdsym(var sym: symptr);

var linkf:  boolean; { symbol is link parameter flag }
    sp, fp: symptr;  { symbol table pointers }
    saved:  boolean; { symbol is from saved flag }

begin

   { check valid object }
   if (nxtobj <> obsym) and (nxtobj <> obcst) then fprterr(esymfmt27);
   sym := nxtsym; { set entry pointer }
   rdnxt; { get next object }
   adjsym(sym); { adjust symbol entry }
   linkf := false; { set not a link parameter }
   if sym^.lab <> nil then { a labeled symbol }
      chkpar(sym, linkf); { check the entry is a link parameter }
   if not linkf then begin { standard symbol }

      { search old table for symbol }
      fp := nil; { set found pointer null }
      if sym^.lab <> nil then begin { is a labeled symbol }

         sp := symsav; { index top of saved symbols }
         saved := true; { set symbol from saved table }
         while sp <> nil do begin { traverse symbols }
      
            if compp(sym^.lab^, sp^.lab^) then 
               fp := sp; { save matching symbol }
            sp := sp^.next { index next symbol }
    
         end;
         if fp = nil then begin { search new table for symbol }
    
            fp := nil; { set found pointer null }
            sp := symtab; { index top of symbols }
            saved := false; { set symbol from new table }
            while sp <> nil do begin { traverse symbols }
         
               if compp(sym^.lab^, sp^.lab^) then 
                  fp := sp; { save matching symbol }
               sp := sp^.next { index next symbol }
       
            end
    
         end

      end;
      if fp <> nil then begin { symbol found }

         if sym^.def and fp^.def and not (sym^.err or fp^.err) then begin 

            { duplicate definition }
            copy(errlab, sym^.lab^); { place error label }
            prterr(edupsym); { duplicate symbol }
            sym^.err := true; { set error already reported }
            fp^.err := true

         end else 
            { if in security mode, and mating two symbols from different decks, 
              checks that both operands are either global or external, and
              flags error if not }
            if fsecr and saved and not (sym^.err or fp^.err) and
               not (sym^.gbl or sym^.ext) and 
               not (fp^.gbl or fp^.ext) then begin { error }

            copy(errlab, sym^.lab^); { place error label }
            prterr(engore); { symbol must be global or external }
            sym^.err := true; { set error already reported }
            fp^.err := true

         end;
         if sym^.def and not fp^.def then begin 

            { new symbol is defined, and the old symbol is not }
            if saved then { symbol came from saved table }
               movsym(fp); { if old in saved table, move it to new table }
            { copy new symbol parameters to old entry }
            fp^.opr := sym^.opr;
            fp^.def := sym^.def;
            fp^.add := sym^.add;
            fp^.gbl := sym^.gbl;
            fp^.ext := sym^.ext;
            fp^.vrs := sym^.vrs;
            fp^.val := sym^.val;
            fp^.par := sym^.par

         end;
         { replace new symbol with old symbol }
         putsym(sym); { dispose of new symbol }
         sym := fp { replace with existing symbol }

      end else begin { link unique symbol into new symbols table }

         sym^.next := symtab; { link into table }
         symtab := sym

      end
         
   end;
   rdrlds(sym); { read any associated rlds }
   if sym^.opr <> onop then begin { the symbol is an expression head }

      rdsym(sp); { read left symbol }
      sym^.lft := sp; { place }
      if sym^.opr in [oadd, osub, omult, odiv, omod, oshl, oshr, oand, oor,
                      oxor] then begin { right branch exists }

         rdsym(sp); { read right symbol }
         sym^.rgt := sp { place }

      end

   end

end;

{*******************************************************************************

Read symbols file

Reads in the symbols and rld entries from the currently open symbols file,
and creates in memory tables.

*******************************************************************************}

procedure rdsyms;

var sym:    symptr;  { pointer for return symbol (unused) }
    bp:     blkptr;  { pointer for blocks }
    ip:     bicptr;  { inclusion pointer }
    blkseq: integer; { block sequence number }
    blklvl: integer; { block level number }
    def:    boolean; { symbol is defining point }

begin

   pstrf  := false; { set program start found false }
   pendf  := false; { set program end found false }
   vstrf  := false; { set variable start found false }
   vendf  := false; { set variable end found false }
   blkseq := 1; { set block sequence number }
   blklvl := 0; { set block level }
   rdnxt; { start lookahead mechanisim }
   while nxtobj <> obend do begin { process entries }

      if (nxtobj = obsym) or (nxtobj = obcst) then begin

         { object is symbol or constant }
         def := nxtsym^.def; { save if next symbol is a defining point }
         rdsym(sym); { read (and possibly merge) symbol entry }
         { Check block is active, symbol is a defining point, and not a merge
           (duplicate) error. This keeps us from generating two inclusions for
           the same symbol. }
         if (blkstk <> nil) and def and not sym^.err then begin

            { generate inclusion entry }
            getbic(ip); { get a block inclusion entry }
            ip^.sym := sym; { index new entry }
            ip^.next := blkstk^.inclst; { push onto inclusion list for parent }
            blkstk^.inclst := ip;
            sym^.par := blkstk { place parent pointer }
            
         end

      end else if nxtobj = obcrld then begin { rld }

         nxtrld^.next := rldtab; { link into current rld table }
         rldtab := nxtrld;
         adjrld(nxtrld); { adjust entry }
         rdnxt { read next entry }

      end else if nxtobj = obblk then begin { block }

         blklvl := blklvl+1; { increase block nesting level }
         if blkstk <> nil then begin { generate inclusion entry }

            getbic(ip); { get a block inclusion entry }
            ip^.blk := nxtblk; { index new entry }
            ip^.next := blkstk^.inclst; { push onto inclusion list for parent }
            blkstk^.inclst := ip
            
         end;
         nxtblk^.next := blkstk; { push into block stack }
         blkstk := nxtblk;
         nxtblk^.lvl := blklvl; { place block level }
         nxtblk^.seq := blkseq; { place sequence number }
         blkseq := blkseq+1; { count blocks }
         rdnxt { read next entry }

      end else if nxtobj = obblke then begin { block end }

         if blkstk = nil then fprterr(esymfmt34); { block stack is empty }
         bp := blkstk; { index top block from stack }
         blkstk := bp^.next; { gap top entry }
         bp^.next := blktab; { move to block table }
         blktab := bp;
         blklvl := blklvl-1; { decrease block nesting level }
         rdnxt { read next entry }

      end else if nxtobj = oblin then begin { line tracking entry }

         if linlas = nil then linlst := nxtlin { enter as root }
         else linlas^.next := nxtlin; { enter as last }
         linlas := nxtlin; { set new last }
         rdnxt { read next entry }

      end else if nxtobj = obsrc then begin { line tracking filename }
      
         { there is nothing to do for this }
         rdnxt { read next entry }
         
      end else fprterr(esymfmt28) { rlds should not be floating loose ! }

   end;
   { check all link parameters existed in file read }
   if not (pstrf and pendf and vstrf and vendf) then fprterr(esymfmt29)

end;

{*******************************************************************************

Count outside to inside block references

Accounts for all of the outside to inside block references. We tour all symbols,
and classify references from that symbol to other symbols, then use that to
account for inward block references.

To do this, the block sequence and level numbers are used, according to the
following algorithim:

1. If the target symbol is outside any block (or logically at block level 0),
then no implication is inferred.

2. If the target is at a higher level than the source symbol (or the source 
symbol is outside any block), then it is an out to in reference and is counted.

3. What remains are references of equal level. These may or may not qualify as
in references based on if they target the same, or a different block. If the
sequence number for source and target are identical, no reference is counted,
otherwise it is.

*******************************************************************************}

procedure blkref;

var sp: symptr;  { pointer for symbols }
    sl: integer; { source symbol level }
    ss: integer; { source symbol sequence }
    rp: rldptr;  { pointer for rlds }
    bp: blkptr;  { pointer for blocks }

{ process implication for symbol }

procedure implicate(tsp: symptr); { target symbol }


begin

{;writeln('Implicate: sl: ', sl:1, ' ss: ', ss:1);}
   if tsp <> nil then { target not nil }
      if tsp^.par <> nil then { target symbol is in a block }
         if (sl < tsp^.par^.lvl) or (ss <> tsp^.par^.seq) then { implicated }
            tsp^.par^.oref := tsp^.par^.oref+1 { count reference }

end;

{ find block that includes program address range }

function fndblkrng(add: integer): blkptr;

var bp, fp: blkptr; { pointers to blocks }

begin

   bp := blktab; { index top of block table }
   fp := nil; { clear found pointer }
   while bp <> nil do begin { search blocks }

      if (add >= bp^.startp) and (add < bp^.endp) then begin

         fp := bp; { set block found }
         bp := nil { signal quit }

      end else bp := bp^.next { next block }

   end;

   fndblkrng := fp { return found block }

end;

begin

   { count symbols }

   sp := symtab; { index top of symbols }
   while sp <> nil do begin { traverse symbols }

      { Set sequence and level parameters for source symbol. If the symbol is
        outside any block, we use a pseudo level and sequence of zero, which
        are not the same as any other actual block. }
      if sp^.par = nil then begin { symbol is not in a block }

         sl := 0; { set symbol level 0 }
         ss := 0 { set sequence 0 }

      end else begin { symbol has a block } 

         sl := sp^.par^.lvl; { set symbol level from block }
         ss := sp^.par^.seq { set symbol sequence from block }

      end;
      implicate(sp^.lft); { process left }
      implicate(sp^.rgt); { process right }
      sp := sp^.next { link next symbol }
         
   end;
{;dmpblk(blktab);}

   { count rlds }

   rp := rldtab; { index top of table }
   while rp <> nil do begin { traverse }

{;write('rld addr: '); writeh(rp^.add); writeln;}
      bp := fndblkrng(rp^.add); { find block rld is encompassed by }
      if bp <> nil then begin { inclusive block found }

{;writeln('including block found');}
         sl := bp^.lvl; { set source level }
         ss := bp^.seq; { set source sequence }
         implicate(rp^.inssym) { perform implication }

      end;
      rp := rp^.next; { link next rld }

   end
{;dmpblk(blktab);}

end;

begin
end.

{*******************************************************************************
*                                                                              *
*                      MACHINE SPECIFIC UTILTITIES MODULE                      *
*                                                                              *
*                       Copyright (C) 2007 S. A. Moore                         *
*                            All rights reserved                               *
*                                                                              *
* PURPOSE:                                                                     *
*                                                                              *
* Gives all the machine specific utilities for this assembler. The general     *
* assembler module performs calls to the machine specific section via this     *
* module. The following interface calls exist:                                 *
*                                                                              *
* procedure mexpr(var sym: symptr); forward;                                   *
* procedure msexpr(var sym: symptr); forward;                                  *
* procedure mterm(var sym: symptr); forward;                                   *
* procedure mfactor(var sym: symptr); forward;                                 *
*                                                                              *
* All of these routines handle parsing of various expression constructs,       *
* the expression, simple expression, term and factor levels. The reason the    *
* main assembler module calls these routines is that it allows the machine     *
* specific module to implement special expression constructs for the           *
* particular assembly language being implemented. After performing special     *
* processing, the calls are sent back to the main assembler calls which        *
* the same purpose.                                                            *
*                                                                              *
* We also implement several machine specific support functions here.           *
*                                                                              *
*******************************************************************************}

module macutl(output);

uses asdef,  { generic definitions }
     common, { global variables }
     utl,    { generic utilities }
     macdef, { processor specific definitions }
     opcdef, { opcode definitions }
     opcini; { initalize reserved table }

procedure inidep; forward; { initalize processor module }
procedure regcod(var reg: regc); forward; { process register code }
procedure parcod(var p: regc; var s: symptr); forward; { parse parameter }
function sreg(p: regc): byte; forward; { convert single register }
function fndres(var s: string): opcodet; forward; { find reserved word }
function mode(m: regc): byte; forward; { find mode code }
procedure mexpr(var sym: symptr); forward; { parse expression }
procedure msexpr(var sym: symptr); forward; { parse simple expression }
procedure mterm(var sym: symptr); forward; { parse term }
procedure mfactor(var sym: symptr); forward; { parse factor }

private

{******************************************************************************

Initalize CPU dependent module for 8051

******************************************************************************}

procedure inidep;

var i: opcodet;

begin

   { output sign - on }
   writeln;
   writeln('8051 assembler vs. 1.0.00 Copyright (C) 2006 S. A. Moore');
   writeln;
   alignment := cpualign; { set CPU alignment }
   bigend := cpubigend; { set CPU endian status }
   wrdsiz := cpuwrdsiz; { set CPU word size }
   { clear and initalize reserved table }
   for i := opnull to opendif do begin

      copy(ressym[i].reslab, '');
      ressym[i].reschn := opnull

   end;
   resini { initalize reseved symbols }

end;

{******************************************************************************

Process register code

A register is one of the following: a, r0, r1, r2, r3, r4, r5, r6, r7 or c. The
input is checked for any of these sequences, and the code for the sequence is 
returned. If no sequence is found, the 'nop' code is returned, and the input 
position left unchanged. Otherwise, the input positon will be just past the 
code.

******************************************************************************}

procedure regcod(var reg: regc); { register return }

var inpsav: inpinx; { input postion save for backtrack }

begin

   inpsav := cmdrot^.inp; { save current input position }
   skpspc; { skip input spaces }
   if alpha(chkchr) then begin { possible register }

      getlab; { get register }
      if compp(labbuf, 'a') then
         reg := rga { a }
      else if compp(labbuf, 'r0') then
         reg := rgr0 { r0 }
      else if compp(labbuf, 'r1') then
         reg := rgr1 { r1 }
      else if compp(labbuf, 'r2') then
         reg := rgr2 { r2 }
      else if compp(labbuf, 'r3') then
         reg := rgr3 { r3 }
      else if compp(labbuf, 'r4') then
         reg := rgr4 { r4 }
      else if compp(labbuf, 'r5') then
         reg := rgr5 { r5 }
      else if compp(labbuf, 'r6') then
         reg := rgr6 { r6 }
      else if compp(labbuf, 'r7') then
         reg := rgr7 { r7 }
      else if compp(labbuf, 'c') then
         reg := rgc { c }
      else if compp(labbuf, 'ab') then
         reg := rgab { ab }
      else if compp(labbuf, 'dptr') then
         reg := rgdptr { dptr }
      else if compp(labbuf, 'pc') then
         reg := rgpc { pc }
      else begin

         reg := rgnl; { set null register }
         cmdrot^.inp := inpsav { restore input position }

      end

   end else begin

      reg := rgnl; { set null register }
      cmdrot^.inp := inpsav { restore input postion }

   end;

end;

{******************************************************************************

Parse parameter

Parses a parameter, and returns both the type code and an immediate expression
if it exists.
The different results by code are:

   rga, rgr0-7, rgc, rgab, and rgdptr: Indicate their respective registers.

   rgimm: Indicates immediate data in the symbol.

   rgdir: Indicates a direct address in the symbol.

   rgr0i, rgr1i: Indicate @r0 or @r1 indirect modes.

   rgdptri: Indicates the @dptr indirect mode.

   rgadptri: Indicates the @a+dptr indirect mode.

   rgapci: Indicates the @a+pc indirect mode.

******************************************************************************}

procedure parcod(var p: regc; var s: symptr);

begin

   skpspc; { skip spaces }
   if chkchr = '#' then begin { immediate mode }

      getchr; { skip '#' }
      nexpr(s); { parse expression }
      p := rgimm { set immediate mode }

   end else if chkchr = '@' then begin { indirect mode }

      getchr; { skip '@' }
      regcod(p); { find type of next }
      if p = rgnl then prterr(eregexp) { register expected }
      else if not (p in [rgr0, rgr1, rga, rgdptr]) then 
         prterr(eregt); { register is wrong type }
      case p of { register }

         rgr0:   p := rgr0i;   { @r0 }
         rgr1:   p := rgr1i;   { @r1 }
         rgdptr: p := rgdptri; { @dptr }
         rga:    begin { any of @a+x forms }

            skpspc; { skip spaces }
            if chkchr <> '+' then prterr(eplsexp); { '+' expected }
            getchr; { skip '+' }
            regcod(p); { find type of next }
            if p = rgdptr then p := rgadptri { @a+dptr }
            else if p = rgpc then p := rgapci { @a+pc }
            else prterr(eregt) { register is wrong type }

         end

      end

   end else begin { direct or register }

      regcod(p); { find type of next }
      if p = rgnl then begin { direct }

         nexpr(s); { parse expression }
         p := rgdir { change to direct mode }

      end

   end

end;

{******************************************************************************

Convert single register

Converts the single registers r0-r7 to a 3 bit code.

******************************************************************************}

function sreg(p: regc): byte;

var r: byte;

begin

   case p of

      rgnl, rga, rgc, rgab, rgimm, rgdir, rgr0i, rgr1i, rgdptr, rgdptri, 
      rgadptri, rgapci: prterr(emodt); { wrong type }
      rgr0: r := 0; { r0 }
      rgr1: r := 1; { r1 }
      rgr2: r := 2; { r2 }
      rgr3: r := 3; { r3 }
      rgr4: r := 4; { r4 }
      rgr5: r := 5; { r5 }
      rgr6: r := 6; { r6 }
      rgr7: r := 7  { r7 }

   end;
   sreg := r

end;

{******************************************************************************

Convert mode

Converts any of the following forms to a 4 bit mode code:

   a,rn     - Register
   a,direct - Direct
   a,@ri    - Register indirect
   a,#x     - Immediate

Outputs an error for any other modes.

******************************************************************************}

function mode(m: regc): byte;

var r: byte;

begin

   case m of

      rgnl, rga, rgc, rgab, rgdptr, rgdptri, 
      rgadptri, rgapci: prterr(emodt); { wrong type }
      rgr0: r := $08+0; { r0 }
      rgr1: r := $08+1; { r1 }
      rgr2: r := $08+2; { r2 }
      rgr3: r := $08+3; { r3 }
      rgr4: r := $08+4; { r4 }
      rgr5: r := $08+5; { r5 }
      rgr6: r := $08+6; { r6 }
      rgr7: r := $08+7; { r7 }
      rgdir: r := $05; { direct }
      rgr0i: r := $06; { @r0 }
      rgr1i: r := $07; { @r1 }
      rgimm: r := $04 { #imm }

   end;
   mode := r
   
end;

{******************************************************************************

Find reserved word

Finds the reserved code corresponding to a given label. The hash value is found 
for the label, then a sequential search of the entry list for a match with the 
label. The result is a code equvalent to the index for the matching label, or 0 
if none is found. See inires for more reserved table details.

******************************************************************************}

function fndres(var s: string) { label to find }
                :opcodet;      { resulting opcode }

var i: opcodet; { reserved table index }
    b: boolean;
    { free variant to convert opcodes to integers }
    r: record case boolean of

          false: (a: opcodet);
          true:  (b: integer)

       end;

begin

   r.b := hash(s, ord(pred(oplast))); { find hash value }
   i := r.a;
   { traverse chain at hash entry looking for a match }
   b := compp(s, ressym[i].reslab); { check equal }
   while not b and (ressym[i].reschn <> opnull) do begin { traverse }

      i := ressym[i].reschn; { next entry }
      b := compp(s, ressym[i].reslab) { check equal }

   end;
   { check match was found }
   if not b then i := opnull;
   fndres := i { return resulting index }

end;

{******************************************************************************

Parse expression

This vector just calls the assembler internal routine. It is here to allow
processor specific expression operators to be added to basic expression
processing.

******************************************************************************}

procedure mexpr(var sym: symptr);

begin

   expri(sym) { pass call to assembler main }

end;

{******************************************************************************

Parse simple expression

This vector just calls the assembler internal routine. It is here to allow
processor specific expression operators to be added to basic expression
processing.

******************************************************************************}

procedure msexpr(var sym: symptr);

begin

   sexpri(sym) { pass call to assembler main }

end;

{******************************************************************************

Parse term

This vector just calls the assembler internal routine. It is here to allow
processor specific expression operators to be added to basic expression
processing.

******************************************************************************}

procedure mterm(var sym: symptr);

begin

   termi(sym) { pass call to assembler main }

end;

{******************************************************************************

Parse factor

This vector just calls the assembler internal routine. It is here to allow
processor specific expression operators to be added to basic expression
processing.
We have added an extention for 8051. We allow a '.' operator, at the highest
priority. This operator is of the form:

   addr.bit

Both operands must be absolute. Addr must be in the range [$20..$2f, $80..$f8].
If in [$80..$f8], it must only occupy every eighth byte, ie., $80, $88, .. $f8.
Bit must be in the range [0..7]. The operator finds the bit address that will
specify the given bit in the given byte.
This operates in two modes for above and below $80. Below, the formula is
(addr-$20)*8+bit. Over, it is addr+bit.

******************************************************************************}

procedure mfactor(var sym: symptr);

var b: symptr; { bit value symbol }
    s: symptr; { temp symbol }

begin

   factori(sym); { pass call to assembler main }
   skpspc; { skip spaces }
   if chkchr = '.' then begin

      getchr; { skip '.' }
      { bit specification mode }
      factori(b); { parse bit number }
      if not sym^.def or sym^.add or sym^.vrs then prterr(epmba);
      if not b^.def or b^.add or b^.vrs then prterr(epmba);
      { validate 0-7 bit }
      if (b^.val < 0) or (b^.val > 7) then prterr(epoor);
      getsym(s); { get symbol entry }
      s^.def := true; { set defined }
      if sym^.val < 128 then begin { ram bits }

         { validate reachable addresses }
         if (sym^.val < $20) or (sym^.val > $2f) then prterr(epoor);
         s^.val := (sym^.val-$20)*8+b^.val { find bit address }

      end else begin { sfr bits }

         { validate reachable addresses }
         if (sym^.val and $07) <> 0 then prterr(epoor);
         s^.val := sym^.val+b^.val { find bit address }

      end;
      { dispose of free suboperand }
      if sym^.lab = nil then putsym(sym);
      sym := s { place resulting expression }

   end

end;

begin

   inidep { initalize processor dependent module }

end.

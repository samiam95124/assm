{*******************************************************************************
*                                                                              *
*                     MACHINE SPECIFIC OPCODE PROCESS MODULE                   *
*                                                                              *
*                       Copyright (C) 2007 S. A. Moore                         *
*                            All rights reserved                               *
*                                                                              *
* PURPOSE:                                                                     *
*                                                                              *
* Implements all of the opcodes for the machine being implemented. All of the  *
* opcodes, even the directives, are implemented here. The processor            *
* independent directives are sent back to the main assembler module, but some  *
* processor specific directives are handled here.                              *
*                                                                              *
*******************************************************************************}

module opcode(output);

uses asdef,  { generic definitions }
     common, { global variables }
     utl,    { generic utilities }
     direct, { generic assembler directives }
     asmain, { error handling }
     macdef, { processor specific definitions }
     macutl; { processor specific utilities }

procedure mprcopc; forward; { process opcode }

private

{*******************************************************************************

Process double operand instruction

*******************************************************************************}

procedure dopr(i: opcodet);

var l, r:       parrec; { parameters }
    opcode, rm: byte;   { opcode pieces }
    opsz:       opsize; { size of operands and operation }

begin

   pardbl(i, l, r, opsz); { parse double operand }
   if not (l.m in rmset) then prterr(emodt); { must be r/m }
   case i of { opcode }

      opadd,  opaddb,  opaddw,  opaddd:  opcode := $00; { add }
      opadc,  opadcb,  opadcw,  opadcd:  opcode := $10; { adc }
      opand,  opandb,  opandw,  opandd:  opcode := $20; { and }
      opcmp,  opcmpb,  opcmpw,  opcmpd:  opcode := $38; { cmp }
      opor,   oporb,   oporw,   opord:   opcode := $08; { or  }
      opsub,  opsubb,  opsubw,  opsubd:  opcode := $28; { sub }
      opsbb,  opsbbb,  opsbbw,  opsbbd:  opcode := $18; { sbb }
      opxor,  opxorb,  opxorw,  opxord:  opcode := $30  { xor }

   end;
   if r.m = rgimm then begin { add r/m,imm }

      { check immediate to al/ax/eax, and not a signed byte immediate to eax. 
        That case can be performed better as op r/m,imm }
      if (l.m in [rgal, rgax, rgeax]) and not ((dispsz(r.vl) = 1) and (l.m = rgeax)) 
         then begin { add a..,imm }

         opcode := opcode+$04; { set immediate to a.. }
         { check may be typed as word or double word }
         if opsz > 1 then opcode := opcode+$01; { set word bit }
         outbyt(opcode); { output opcode }
         gensym(r.vl, 0, false, imnorm, 0, 0, opsz*8)

      end else begin { add r/m,imm }

         rm := opcode; { save opcode as middle mode bits }
         opcode := $80; { set opcode }
         { check may be typed as word or double word }
         if opsz > 1 then opcode := opcode+$01; { set word bit }
         if (dispsz(r.vl) = 1) and (opsz <> 1) then begin 

            { The immediate fits in a signed byte, as is not a byte
              operation. We can reduce the instruction by using a load 
              signed immediate byte }
            opcode := opcode+$02; { set the sign extend bit }
            opsz := 1 { set output size to byte }

         end;
         outbyt(opcode); { output opcode }
         addr(rm, l); { generate addressing for left }
         gensym(r.vl, 0, false, imnorm, 0, 0, opsz*8) { output immediate }

      end

   end else if l.m in regset then begin { add reg,r/m }

      if not (r.m in rmset) then prterr(emodt); { must be r/m }
      { check may be typed as word or double word }
      if opsz > 1 then opcode := opcode+$01; { set word bit }
      outbyt(opcode+$02); { output opcode }
      addr(sreg(l.m)*8, r) { generate addressing for right }

   end else if r.m in regset then begin { add r/m,reg }

      { check may be typed as word or double word }
      if opsz > 1 then opcode := opcode+$01; { set word bit }
      outbyt(opcode+$00); { output opcode }
      addr(sreg(r.m)*8, l) { generate addressing for left }

   end else prterr(epart) { incorrect parameter }

end;

{*******************************************************************************
                     
Process arpl instruction

*******************************************************************************}

procedure arpl;

var l, r: parrec;  { parameters }
    opsz: opsize;  { size of operands and operation }

begin

   valmac(mt286); { validate 286+ }
   pardbl(oparpl, l, r, opsz); { parse double operand }
   if opsz <> 2 then prterr(epart); { must be word size }
   if not (l.m in rmset) then prterr(emodt); { must be r/m }
   if not (r.m in regset) then prterr(epart); { must be a register }
   outbyt($63); { output opcode }
   addr(sreg(r.m)*8, l) { generate addressing for left }

end;

{*******************************************************************************

Process register destination, memory/register source instruction

*******************************************************************************}

procedure regmem(i: opcodet);

var l, r: parrec;  { parameters }
    opsz: opsize;  { size of operands and operation }

begin

   pardbl(i, l, r, opsz); { parse double operand }
   if opsz < 2 then prterr(epart); { must be word or dword size }
   if not (l.m in regset) then prterr(epart); { must be a register }
   if not (r.m in rmset) then prterr(epart); { must be r/m }
   if (i = oplea) or (i = oplds) or (i = oples) or (i = oplfs) or 
      (i = oplgs) or (i = oplss) then begin

      if not (l.m in lrgset) then prterr(epart); { must be 16/32 register }
      if not (r.m in adrset) then prterr(epart) { must be a mem }

   end;
   case i of { output opcodes }

      opbound: begin valmac(mt186); outbyt($62) end;
      opbsf: begin valmac(mt386); outbyt($0f); outbyt($bc) end;
      opbsr: begin valmac(mt386); outbyt($0f); outbyt($bd) end;
      oplar: begin valmac(mt286); outbyt($0f); outbyt($02) end;
      oplsl: begin valmac(mt286); outbyt($0f); outbyt($03) end;
      oplea: outbyt($8d);
      oplds: outbyt($c5);
      oples: outbyt($c4);
      oplfs: begin valmac(mt386); outbyt($0f); outbyt($b4) end;
      oplgs: begin valmac(mt386); outbyt($0f); outbyt($b5) end;
      oplss: begin valmac(mt386); outbyt($0f); outbyt($b2) end

   end;
   addr(sreg(l.m)*8, r) { generate addressing for right }

end;

{*******************************************************************************

Process bswap instruction

*******************************************************************************}

procedure bswap;

var p: parrec; { parameters }

begin

   valmac(mt486); { validate 486+ }
   parcod(p); { parse parameter }
   if not (p.m in drgset) then prterr(epart); { must be a dword register }
   setsiz(true); { set to large }
   outbyt($0f); { output opcode }
   outbyt($c8+sreg(p.m))

end;

{*******************************************************************************

Process the bit test instructions

*******************************************************************************}

procedure btest(i: opcodet);

var l, r:   parrec; { parameters }
    opcode: byte;   { opcode pieces }
    opsz:   opsize; { size of operands and operation }

begin

   valmac(mt386); { validate 386+ }
   pardbl(i, l, r, opsz); { parse double operand }
   if not (l.m in rmset) then prterr(emodt); { must be r/m }
   outbyt($0f); { output opcode prefix }
   case i of { output opcodes }

      opbt, opbtw, opbtd:    opcode := $20;
      opbtc, opbtcw, opbtcd: opcode := $38;
      opbtr, opbtrw, opbtrd: opcode := $30;
      opbts, opbtsw, opbtsd: opcode := $28

   end;
   if r.m = rgimm then begin { m,imm }

      if absolute(r.vl) and ((r.vl^.val < 0) or (r.vl^.val > 255)) then
         prterr(epoor); { parameter out of range }
      outbyt($ba); { output opcode }
      addr(opcode, l); { generate addressing for left }
      gensym(r.vl, 0, false, imnorm, 0, 0, 1*8); { output bit number }

   end else begin 

      if opsz < 2 then prterr(epart); { must be word or dword size }
      if not (r.m in lrgset) then prterr(emodt); { must be a "large" register }
      outbyt(opcode+$83); { output opcode }
      addr(sreg(r.m)*8, l) { generate addressing for left }

    end

end;

{*******************************************************************************

Process call/jump near instructions

*******************************************************************************}

procedure caljmp(i: opcodet);

var p:      parrec;  { parameters }
    opsz:   opsize;  { size of operands and operation }
    adsz:   boolean; { address size (large/small) }
    opcsz:  boolean; { opcode size (large/small) }
    offset: integer; { offset of relative jump }

begin

   parcod(p); { parse parameter }
   if (reglen[p.m] <> 0) and (opclen[i] <> 0) and 
     (reglen[p.m] <> opclen[i]) then prterr(eopsizm); { size mismatch }
   { find the size requirement of the opcode, which can be large,
     small or default }
   opcsz := large; { set default status of operand size }
   if opclen[i] = 2 then opcsz := false { set small model }
   else if opclen[i] = 4 then opcsz := true; { set large model }
   if reglen[p.m] = 2 then opcsz := false { set small model }
   else if reglen[p.m] = 4 then opcsz := true; { set large model }
   if p.m = rgimm then begin { call/jmp imm }

      if opclen[i] = 0 then begin { generic, use "smart" algorithim }

         offset := maxint; { set largest value of offset }
         { find net offset if the target is a defined program address }
         if p.vl^.def and p.vl^.add then 
            offset := p.vl^.val-(prgmc+2);
         { check it's a jump, and can be reached by a byte offset }
         if (i = opjmp) and ((offset >= -128) and (offset <= +127)) then begin

            { offset is byte, and it's a jump. We code the same as jmps imm }
            outbyt($eb); { output opcode }
            gensym(p.vl, 0, false, imsgof, 0, 0, 1*8) { output address }

         end else begin { word or dword offset, or call (which must be so) }

            { output opcode }
            if (i = opcall) or (i = opcallm) or (i = opcalll) then outbyt($e8)
            else outbyt($e9);
            if opcsz then opsz := 4 else opsz := 2; { set size }
            gensym(p.vl, 0, false, imsgof, 0, 0, opsz*8) { output address }

         end

      end else begin { specific call/jmp }

         setsiz(opcsz); { set operand size }
         { output opcode }
         if (i = opcall) or (i = opcallm) or (i = opcalll) then outbyt($e8)
         else outbyt($e9);
         if opcsz then opsz := 4 else opsz := 2; { set size }
         gensym(p.vl, 0, false, imsgof, 0, 0, opsz*8) { output address }

      end

   end else begin { call/jmp r/m }

      { for r/m, the size must appear somewhere }
      if (opclen[i] = 0) and (reglen[p.m] = 0) then prterr(eopsiz);
      if not (p.m in rmset) then prterr(emodt); { must be r/m }
      { if register, must be 16/32 }
      if (p.m in regset) and not (p.m in lrgset) then prterr(emodt); 
      { determine address mode size }
      adsz := large; { set default address requirement }
      if p.as = 4 then adsz := true { set large if required }
      else if p.as = 2 then adsz := false; { set small if required }
      setadr(adsz); { set address size }
      setsiz(opcsz); { set operand size }
      outbyt($ff); { output opcode }
      { generate addressing }
      if (i = opcall) or (i = opcallm) or (i = opcalll) then addr($10, p)
      else addr($20, p)

   end

end;

{*******************************************************************************

Process call/jump far instructions

We process the far instructions as two separate parameters, the segment
and offset. This is essentially a "do it yourself" kit to build segmented
addresses. It also makes it possible to represent large addresses given
an assembler word size limitation.

It is also possible to give an indirect address as the first operand.
This requires only a single parameter, since the divided address will
be at the destination.

The alternate mode for segments is the "huge" model pointers. In this model,
only a single address is given, then we use a special insertion mode that
performs the huge mode calculation.

*******************************************************************************}

procedure caljmpf(i: opcodet);

var r, l:  parrec;  { parameters }
    opsz:  opsize;  { size of operands and operation }
    adsz:  boolean; { address size (large/small) }
    opcsz: boolean; { opcode size (large/small) }

begin

   parcod(l); { parse left parameter }
   if l.m = rgimm then begin { op <seg>,<off> }

      skpspc; { skip spaces }
      if chkchr = ',' then begin { process jmpf/callf segment, offset }

         getchr; { skip ',' }
         parcod(r); { parse right parameter }
         if r.m <> rgimm then prterr(epart); { bad parameter }
         { find the size requirement of the opcode, which can be large,
           small or default }
         opcsz := large; { set default status of address size }
         { if offset is known, use appropriate size }
         if absolute(r.vl) then opcsz := r.vl^.val >= 65536;
         if opclen[i] = 2 then opcsz := false { set small model }
         else if opclen[i] = 4 then opcsz := true; { set large model }
         setadr(opcsz); { set large/small address model }
         { output opcode }
         if (i = opcallf) or (i = opcallmf) or (i = opcalllf) then outbyt($9a)
         else outbyt($ea);
         if opcsz then opsz := 4 else opsz := 2; { set size }
         gensym(r.vl, 0, false, imnorm, 0, 0, opsz*8); { output offset }
         { note that the segment is allways 16 bits }
         gensym(l.vl, 0, false, imnorm, 0, 0, 2*8) { output segment }

      end else begin { process jmpf/callf addr }

         opcsz := large; { set default status of address size }
         if opclen[i] = 2 then opcsz := false { set small model }
         else if opclen[i] = 4 then opcsz := true; { set large model }
         setadr(opcsz); { set large/small address model }
         { output opcode }
         if (i = opcallf) or (i = opcallmf) or (i = opcalllf) then outbyt($9a)
         else outbyt($ea);
         { set size plus segment, which is always 2 bytes }
         if opcsz then opsz := 4+2 else opsz := 2+2;
         gensym(l.vl, 0, false, imiseg, 0, 0, opsz*8) { output address }

      end

   end else begin { op r/m }

      if not (l.m in rmset) then prterr(emodt); { must be r/m }
      { determine address mode size }
      adsz := large; { set default address requirement }
      if l.as = 4 then adsz := true { set large if required }
      else if l.as = 2 then adsz := false; { set small if required }
      opcsz := large; { set default status of address size }
      if opclen[i] = 2 then opcsz := false { set small model }
      else if opclen[i] = 4 then opcsz := true; { set large model }
      { sizes should match }
      if opcsz <> adsz then prterr(eopsizm); { size mismatch }
      setadr(adsz); { set large/small address model }
      outbyt($ff); { output opcode }
      { generate addressing }
      if (i = opcallf) or (i = opcallmf) or (i = opcalllf) then addr($18, l)
      else addr($28, l)

   end

end;

{*******************************************************************************

Process cmpxchg and xadd instructions

*******************************************************************************}

procedure xchop(i: opcodet);

var l, r:   parrec;  { parameters }
    opcode: byte;    { opcode pieces }
    opsz:   opsize;  { size of operands and operation }

begin

   valmac(mt486); { validate 486+ }
   pardbl(i, l, r, opsz); { parse double operand }
   if not (l.m in rmset) then prterr(emodt); { must be r/m }
   if not (r.m in regset) then prterr(emodt); { must be reg }
   outbyt($0f); { output first part of opcode }
   { set opcode }
   if i = opcmpxchg then opcode := $b0 else opcode := $c0;
   { check may be typed as word or double word }
   if opsz > 1 then opcode := opcode+$01; { set word bit }
   outbyt(opcode+$00); { output opcode }
   addr(sreg(r.m)*8, l) { generate addressing for left }

end;

{*******************************************************************************

Process cmpxchg8b

*******************************************************************************}

procedure cpxh8b;

var p:    parrec;  { parameters }
    adsz: boolean; { address size (large/small) }

begin

   valmac(mt586); { validate 586+ }
   parcod(p); { parse parameter }
   if not (p.m in rmset) then prterr(epart); { wrong type of parameter }
   { determine address mode size }
   adsz := large; { set default address requirement }
   if p.as = 4 then adsz := true { set large if required }
   else if p.as = 2 then adsz := false; { set small if required }
   { check required address size against present address size }
   if adsz <> large then outbyt($67); { output address mode change prefix }
   outbyt($0f); { output opcode }
   outbyt($c7);
   addr(0, p) { generate addressing }

end;

{*******************************************************************************

Process dec/inc instructions

*******************************************************************************}

procedure decinc(i: opcodet);

var p:    parrec;  { parameters }
    opsz: opsize;  { size of operands and operation }

begin

   parsgl(i, p, opsz); { parse operand }
   if not (p.m in rmset) then prterr(emodt); { must be r/m }
   if (p.m in wrgset) or (p.m in drgset) then begin { op r16/r32 }

      if (i = opdec) or (i = opdecb) or (i = opdecw) or (i = opdecd) then
         outbyt($48+sreg(p.m)) { dec r }
      else
         outbyt($40+sreg(p.m)) { inc r }

   end else begin { op r/m }

      if opsz = 1 then outbyt($fe) else outbyt($ff); { output primary opcode }
      if (i = opdec) or (i = opdecb) or (i = opdecw) or (i = opdecd) then
         addr($08, p) { dec r/m }
      else
         addr($00, p) { inc r/m }

   end

end;

{*******************************************************************************

Process div, idiv instructions

*******************************************************************************}

procedure idiv(i: opcodet);

var l, r:       parrec;  { parameters }
    opcode, rm: byte;    { opcode piece }
    opsz, rsz:  opsize;  { size of operands and operation }
    adsz:       boolean; { address size mode }

begin

   parsgl(i, l, opsz); { parse single operand }
   skpspc; { skip spaces }
   if chkchr = ',' then begin { double operand form }

      getchr; { skip ',' }
      { must be al, ax or eax }
      if not (l.m in [rgal, rgax, rgeax]) then prterr(emodt);
      { now we are in an odd position. We have allready formally parsed
        a parameter, which set the sizing for the operation. So we
        must parse the next parameter without doing that again }
      parcod(r); { parse parameter }
      rsz := reglen[r.m]; { find operand length }
      if (rsz <> 0) and (rsz <> opsz) then prterr(eopsizm); { size mismatch }
      { determine address mode size. Note that only one operand may be an
        address }
      adsz := large; { set default address requirement }
      if r.as = 4 then adsz := true { set large if required }
      else if r.as = 2 then adsz := false; { set small if required } 
      setadr(adsz); { set address sizing }
      if not (r.m in rmset) then prterr(emodt); { must be r/m }
      opcode := $f6; { set opcode }
      if (i = opdiv) or (i = opdivb) or (i = opdivw) or (i = opdivd) then
         rm := $30 else rm := $38;
      { check may be typed as word or double word }
      if opsz > 1 then opcode := opcode+$01; { set word bit }
      outbyt(opcode); { output opcode }
      addr(rm, r) { generate addressing for right }

   end else begin { single operand form }

      if not (l.m in rmset) then prterr(emodt); { must be r/m }
      opcode := $f6; { set opcode }
      if (i = opdiv) or (i = opdivb) or (i = opdivw) or (i = opdivd) then
         rm := $30 else rm := $38;
      { check may be typed as word or double word }
      if opsz > 1 then opcode := opcode+$01; { set word bit }
      outbyt(opcode); { output opcode }
      addr(rm, l) { generate addressing for right }

   end 

end;

{*******************************************************************************

Process enter instruction

*******************************************************************************}

procedure enter;

var l, r: parrec; { parameters }

begin

   valmac(mt186); { validate 186+ }
   parcod(l); { parse left parameter }
   if l.m <> rgimm then prterr(epart); { bad parameter }
   skpspc; { skip spaces }
   prcnxt(',', ecmaexp); { skip ',' }
   parcod(r); { parse right parameter }
   if r.m <> rgimm then prterr(epart); { bad parameter }
   outbyt($c8); { output opcode }
   gensym(l.vl, 0, false, imnorm, 0, 0, 2*8); { output locals count }
   gensym(r.vl, 0, false, imnorm, 0, 0, 1*8) { output level count }

end;

{*******************************************************************************

Process fadd/fdiv/fdivr/fmul/fsub/fsubr instructions

*******************************************************************************}

procedure fop(i: opcodet);

var l,r:    parrec; { parameters }
    opsz:   opsize; { size of operands and operation }
    opcode: byte;   { opcode byte }

begin

   valflt; { validate float instructions enabled }
   skpspc; { skip spaces }
   if endcmd then begin { op }

      outbyt($d8); { output 1st instruction byte }
      case i of { opcode }

         opfadd,  opfadds,  opfaddd:  outbyt($c1); { fadd }
         opfdiv,  opfdivs,  opfdivd:  outbyt($f1); { fdiv }
         opfdivr, opfdivrs, opfdivrd: outbyt($f9); { fdivr }
         opfmul,  opfmuls,  opfmuld:  outbyt($c9); { fmul }
         opfsub,  opfsubs,  opfsubd:  outbyt($e1); { fsub }
         opfsubr, opfsubrs, opfsubrd: outbyt($e9)  { fsubr }

      end

   end else begin { has operands }

      parsgl(i, l, opsz); { parse left/only parameter }
      if (l.m = rgst) or (l.m = rgstr) then begin { op st,st }

         skpspc; { skip spaces }
         prcnxt(',', ecmaexp); { skip ',' }
         parcod(r); { parse right parameter }
         { check both are st type }
         if (l.m = rgst) and (r.m = rgst) then prterr(epart);
         case i of { opcode }

            opfadd,  opfadds,  opfaddd:  opcode := $c0; { fadd }
            opfdiv,  opfdivs,  opfdivd:  opcode := $f0; { fdiv }
            opfdivr, opfdivrs, opfdivrd: opcode := $f8; { fdivr }
            opfmul,  opfmuls,  opfmuld:  opcode := $c8; { fmul }
            opfsub,  opfsubs,  opfsubd:  opcode := $e0; { fsub }
            opfsubr, opfsubrs, opfsubrd: opcode := $e8  { fsubr }
            
         end;
         if l.m = rgst then begin { op st,st(r) }

            outbyt($d8); { output 1st instruction byte }
            { output 2nd byte }
            gensym(r.vl, opcode, false, imnorm, 0, 0, 3)

         end else begin { op st(r),st }

            { check has a reversing bit }
            if (i = opfdiv) or (i = opfdivs) or (i = opfdivd) or 
               (i = opfdivr) or (i = opfdivrs) or (i = opfdivrd) or
               (i = opfmul) or (i = opfmuls) or (i = opfmuld) or
               (i = opfsub) or (i = opfsubs) or (i = opfsubd) or
               (i = opfsubr) or (i = opfsubrs) or (i = opfsubrd) then
               { flip "reverse" state }
               if (opcode and $08) <> 0 then opcode := opcode and $f7
               else opcode := opcode or $08;
            outbyt($dc); { output 1st instruction byte }
            { output 2nd byte }
            gensym(l.vl, opcode, false, imnorm, 0, 0, 3)

         end

      end else begin { op mem }

         case i of { opcode }

            opfadd,  opfadds,  opfaddd:  opcode := $00; { fadd }
            opfdiv,  opfdivs,  opfdivd:  opcode := $30; { fdiv }
            opfdivr, opfdivrs, opfdivrd: opcode := $38; { fdivr }
            opfmul,  opfmuls,  opfmuld:  opcode := $08; { fmul }
            opfsub,  opfsubs,  opfsubd:  opcode := $20; { fsub }
            opfsubr, opfsubrs, opfsubrd: opcode := $28  { fsubr }
            
         end;
         if not (l.m in adrset) then prterr(epart); { wrong type of parameter }
         { output 1st instruction byte according to size }
         if opsz = 4 then outbyt($d8) else outbyt($dc);
         addr(opcode, l) { output address mode }

      end

   end

end;

{*******************************************************************************

Process faddp/fdivp/fdivrp/fmulp/fsubp/fsubrp instructions

*******************************************************************************}

procedure fopp(i: opcodet);

var l,r:    parrec; { parameters }
    opsz:   opsize; { size of operands and operation }
    opcode: byte;   { opcode byte }

begin

   valflt; { validate float instructions enabled }
   skpspc; { skip spaces }
   if endcmd then begin { op }

      outbyt($de); { output 1st instruction byte }
      case i of { opcode }

         opfaddp:  outbyt($c1); { fadd }
         opfdivp:  outbyt($f9); { fdiv }
         opfdivrp: outbyt($f1); { fdivr }
         opfmulp:  outbyt($c9); { fmul }
         opfsubp:  outbyt($e9); { fsub }
         opfsubrp: outbyt($e1)  { fsubr }

      end

   end else begin { has operands }

      parsgl(i, l, opsz); { parse left parameter }
      if l.m = rgstr then begin { op st,st }

         skpspc; { skip spaces }
         prcnxt(',', ecmaexp); { skip ',' }
         parcod(r); { parse right parameter }
         if r.m <> rgst then prterr(epart); { must be st }
         outbyt($de); { output 1st instruction byte }
         case i of { opcode }

            opfaddp:  opcode := $c0; { fadd }
            opfdivp:  opcode := $f8; { fdiv }
            opfdivrp: opcode := $f1; { fdivr }
            opfmulp:  opcode := $c8; { fmul }
            opfsubp:  opcode := $e8; { fsub }
            opfsubrp: opcode := $e1  { fsubr }
            
         end;
         { output 2nd byte }
         gensym(l.vl, opcode, false, imnorm, 0, 0, 3)

      end else prterr(epart) { invalid parameter }

   end

end;

{*******************************************************************************

Process fiadd/fidiv/fidivr/fimul/fisub/fisubr instructions

*******************************************************************************}

procedure fopi(i: opcodet);

var l:      parrec; { parameters }
    opsz:   opsize; { size of operands and operation }
    opcode: byte;   { opcode byte }

begin

   valflt; { validate float instructions enabled }
   parsgl(i, l, opsz); { parse left/only parameter }
   if not (l.m in adrset) then prterr(epart); { wrong type of parameter }
   case i of { opcode }

      opfiadd,  opfiaddw,  opfiaddd:  opcode := $00; { fiadd }
      opfidiv,  opfidivw,  opfidivd:  opcode := $30; { fdiv }
      opfidivr, opfidivrw, opfidivrd: opcode := $38; { fdivr }
      opfimul,  opfimulw,  opfimuld:  opcode := $08; { fmul }
      opfisub,  opfisubw,  opfisubd:  opcode := $20; { fsub }
      opfisubr, opfisubrw, opfisubrd: opcode := $28  { fsubr }
      
   end;
   { output 1st instruction byte according to size }
   if opsz = 2 then outbyt($de) else outbyt($da);
   addr(opcode, l) { output address mode }

end;

{*******************************************************************************

Process fcom/fcoms/fcomd/fcomp/fcomps/fcompd/fcompp instructions

*******************************************************************************}

procedure fcom(i: opcodet);

var p:      parrec; { parameters }
    opsz:   opsize; { size of operands and operation }
    opcode: byte;   { opcode byte }

begin

   valflt; { validate float instructions enabled }
   skpspc; { skip spaces }
   if endcmd then begin { op }

      { output 1st instruction byte }
      if i = opfcompp then outbyt($de)
      else outbyt($d8);
      { output second instruction byte }
      if i = opfcom then outbyt($d1) else outbyt($d9)

   end else begin { has operands }

      parsgl(i, p, opsz); { parse parameter }
      if p.m = rgstr then begin { op st(i) }

         outbyt($d8); { output 1st instruction byte }
         { set second base byte }
         if i = opfcom then opcode := $d0 { fcom }
         else opcode := $d8; { fcomp }
         { output 2nd byte }
         gensym(p.vl, opcode, false, imnorm, 0, 0, 3)

      end else begin { op mem }

         if not (p.m in adrset) then prterr(epart); { wrong type of parameter }
         { output 1st instruction byte according to size }
         if opsz = 4 then outbyt($d8) else outbyt($dc);
         { set second base byte }
         if (i = opfcom) or (i = opfcoms) or (i = opfcomd) then
            opcode := $10 { fcom }
         else
            opcode := $18; { fcomp }
         addr(opcode, p) { output address mode }

      end

   end

end;

{*******************************************************************************

Process ffree instruction

*******************************************************************************}

procedure ffree(i: opcodet);

var p:    parrec; { parameters }
    opsz: opsize; { size of operands and operation }

begin

   valflt; { validate float instructions enabled }
   parsgl(i, p, opsz); { parse parameter }
   if p.m <> rgstr then prterr(epart); { must be st(i) }
   outbyt($dd); { output 1st instruction byte }
   { output second byte }
   gensym(p.vl, $c0, false, imnorm, 0, 0, 3)

end;

{*******************************************************************************

Process ficom/ficomp instructions

*******************************************************************************}

procedure ficom(i: opcodet);

var p:      parrec; { parameters }
    opsz:   opsize; { size of operands and operation }
    opcode: byte;   { opcode byte }

begin

   valflt; { validate float instructions enabled }
   parsgl(i, p, opsz); { parse parameter }
   if not (p.m in adrset) then prterr(epart); { wrong type of parameter }
   { output 1st instruction byte according to size }
   if opsz = 4 then outbyt($da) else outbyt($de);
   { set second base byte }
   if (i = opficom) or (i = opficomw) or (i = opficomd) then
      opcode := $10 { ficom }
   else
      opcode := $18; { ficomp }
   addr(opcode, p) { output address mode }

end;

{*******************************************************************************

Process fild/fist/fistp/fld/fst/fstp instructions

*******************************************************************************}

procedure filds(i: opcodet);

var p:      parrec; { parameters }
    opsz:   opsize; { size of operands and operation }
    opcode: byte;   { opcode byte }

begin

   valflt; { validate float instructions enabled }
   parsgl(i, p, opsz); { parse parameter }
   if p.m = rgstr then begin { op st(i) }

      { output 1st byte and set base of next }
      case i of { opcode }

         opfild, opfildw, opfildd, opfildq,
         opfist, opfistw, opfistd, opfistp, opfistpw, opfistpd, opfistpq, 
         opflds, opfldd, opfldl,
         opfsts, opfstd, opfstps, opfstpd, 
         opfstpl: prterr(epart); { bad parameter }
         opfld: begin outbyt($d9); opcode := $c0 end; { fld st(i) }
         opfst: begin outbyt($dd); opcode := $d0 end; { fst st(i) }
         opfstp: begin outbyt($dd); opcode := $d8 end { fstp st(i) }
         
      end;
      { output 2nd byte }
      gensym(p.vl, opcode, false, imnorm, 0, 0, 3)

   end else begin { op mem }

      if not (p.m in adrset) then prterr(epart); { wrong type of parameter }
      case i of { instruction }

         opfildw:  begin outbyt($df); opcode := $00 end; { fildw }
         opfildd:  begin outbyt($db); opcode := $00 end; { fildd }
         opfildq:  begin outbyt($df); opcode := $28 end; { fildq }
         opfistw:  begin outbyt($df); opcode := $10 end; { fistw }
         opfistd:  begin outbyt($db); opcode := $10 end; { fistd }
         opfistpw: begin outbyt($df); opcode := $18 end; { fistpw }
         opfistpd: begin outbyt($db); opcode := $18 end; { fistpw }
         opfistpq: begin outbyt($df); opcode := $38 end; { fistpw }
         opflds:   begin outbyt($d9); opcode := $00 end; { flds }
         opfldd:   begin outbyt($dd); opcode := $00 end; { fldd }
         opfldl:   begin outbyt($db); opcode := $28 end; { flds }
         opfsts:   begin outbyt($d9); opcode := $10 end; { fsts }
         opfstd:   begin outbyt($dd); opcode := $10 end; { fstd }
         opfstps:  begin outbyt($d9); opcode := $18 end; { fstps }
         opfstpd:  begin outbyt($dd); opcode := $18 end; { fstpd }
         opfstpl:  begin outbyt($db); opcode := $38 end  { fstpl }

      end;
      addr(opcode, p) { output address mode }

   end

end;

{*******************************************************************************

Process single memory operand instructions

*******************************************************************************}

procedure smem(i: opcodet);

var p:      parrec; { parameters }
    opsz:   opsize; { size of operands and operation }
    opcode: byte;   { opcode byte }

begin

   if (i = opfbld) or (i = opfbstp) or (i = opfldcw) or (i = opfldenvw) or
      (i = opfldenvd) or (i = opfrstorw) or (i = opfrstord) or
      (i = opfsavew) or (i = opfsaved) or (i = opfnsavew) or (i = opfnsaved) or
      (i = opfstcw) or (i = opfnstcw) or (i = opfstenvw) or (i = opfstenvd) or
      (i = opfnstenvw) or (i = opfnstenvd) then
      valflt; { validate float instructions enabled }
   parsgl(i, p, opsz); { parse parameter }
   if not (p.m in adrset) then prterr(epart); { wrong type of parameter }
   case i of { instruction }

      opfbld:     begin outbyt($df); opcode := $20 end; { fbld }
      opfbstp:    begin outbyt($df); opcode := $30 end; { fbstp }
      opfldcw:    begin outbyt($d9); opcode := $28 end; { fldcw }
      opfldenvw,
      opfldenvd:  begin outbyt($d9); opcode := $20 end; { fldenv }
      opfrstorw,
      opfrstord:  begin outbyt($dd); opcode := $20 end; { frstor }
      opfsavew, 
      opfsaved:   begin outbyt($9b); outbyt($dd); opcode := $30 end; { fsave }
      opfnsavew,
      opfnsaved:  begin outbyt($dd); opcode := $30 end; { fnsave }
      opfstcw:    begin outbyt($9b); outbyt($d9); opcode := $38 end; { fstcw }
      opfnstcw:   begin outbyt($d9); opcode := $38 end; { fnstcw }
      opfstenvw,
      opfstenvd:  begin outbyt($9b); outbyt($d9); opcode := $30 end; { fstenv }
      opfnstenvw,
      opfnstenvd: begin outbyt($d9); opcode := $30 end; { fnstenv }
      opinvlpg:   begin valmac(mt486); outbyt($0f); outbyt($01); 
                        opcode := $38 end; { invlpg }
      oplgdtw, 
      oplgdtd:    begin valmac(mt286); outbyt($0f); outbyt($01); 
                        opcode := $10 end; { lgdt }
      oplidtw,
      oplidtd:    begin valmac(mt286); outbyt($0f); outbyt($01); 
                        opcode := $18 end; { lidt }
      opsgdtw,
      opsgdtd:    begin valmac(mt286); outbyt($0f); outbyt($01); 
                        opcode := $00 end; { sgdt }
      opsidtw,
      opsidtd:    begin valmac(mt286); outbyt($0f); outbyt($01); 
                        opcode := $08 end { sidt }

   end;
   addr(opcode, p) { output address mode }

end;

{*******************************************************************************

Process fstsw/fnstsw instructions

*******************************************************************************}

procedure fstsw(i: opcodet);

var p:    parrec; { parameters }
    opsz: opsize; { size of operands and operation }

begin

   valflt; { validate float instructions enabled }
   parsgl(i, p, opsz); { parse parameter }
   if i = opfstsw then outbyt($9b); { output prefix }
   if p.m = rgax then { op ax }
      begin outbyt($df); outbyt($e0) end 
   else begin { op mem }

      if not (p.m in adrset) then prterr(epart); { wrong type of parameter }
      outbyt($dd); { output 1st instruction byte }
      addr($38, p) { output address mode }

   end

end;

{*******************************************************************************

Process fucom/fucomp/fxch instructions

*******************************************************************************}

procedure fucomx(i: opcodet);

var p:      parrec; { parameters }
    opsz:   opsize; { size of operands and operation }
    opcode: byte;   { opcode byte }

begin

   valflt; { validate float instructions enabled }
   skpspc; { skip spaces }
   { output 1st instruction byte }
   if i = opfxch then outbyt($d9) { fxch }
   else outbyt($dd); { fucom/fucomp }
   if endcmd then begin { op }

      if i = opfucom then outbyt($e1) { fucom }
      else if i = opfucomp then outbyt($e9) { fucomp }
      else outbyt($c9) { fxch }

   end else begin { op st(i) }

      parsgl(i, p, opsz); { parse parameter }
      if p.m <> rgstr then prterr(epart); { must be st(i) }
      if i = opfucom then opcode := $e0 { fucom }
      else if i = opfucomp then opcode := $e8 { fucomp }
      else opcode := $c8; { fxch }
      gensym(p.vl, opcode, false, imnorm, 0, 0, 3)

   end

end;

{*******************************************************************************

Process imul instruction

*******************************************************************************}

procedure imul(i: opcodet);

var l, r, s: parrec; { parameters }
    opsz:    opsize; { size of operands and operation }

begin

   parsgl(i, l, opsz); { parse left }
   skpspc; { skip spaces }
   if chkchr <> ',' then begin { single operand }

      if not (l.m in rmset) then prterr(emodt); { wrong type of parameter }
      { output 1st instruction byte }
      if opsz = 1 then outbyt($f6) else outbyt($f7);
      addr($28, l); { generate addressing }

   end else begin { double or triple operand }

      if not (l.m in regset) then prterr(emodt); { left must be register }
      getchr; { skip ',' }
      parcod(r); { parse right )
      { check that matches the size of the operation so far }
      if (reglen[r.m] <> 0) and (reglen[r.m] <> opsz) then
         prterr(eopsizm); { sizes do not match }
      { set address size mode. This was done already,
        but in the case of double or triple operands, the first
        must have been a register, which would not generate
        an address prefix }
      if (r.as = 2) or (r.as = 4) then setadr(r.as = 4);
      skpspc; { skip spaces }
      if chkchr <> ',' then begin { double operand }

         if r.m = rgimm then begin { op r,imm }

            valmac(mt386); { 386+ }
            if absolute(r.vl) and (dispsz(r.vl) = 1) then begin

               { immediate fits in signed byte }
               outbyt($6b); { output opcode }
               opsz := 1 { set byte size }
      
            end else outbyt($69); { output opcode }
            addr(sreg(l.m)*8, l); { generate addressing }
            gensym(r.vl, 0, false, imnorm, 0, 0, opsz*8) { generate immediate }

         end else begin { op r/m }

            if not (l.m in lrgset) then prterr(emodt); { must be large reg }
            if not ((r.m in rmset) or (r.m in brgset)) then 
               prterr(emodt); { must be r/m }
            outbyt($0f); { output instruction bytes }
            outbyt($af);
            addr(sreg(l.m)*8, r); { generate addressing }

         end
                     
      end else begin { triple operand }

         getchr; { skip ',' }
         parcod(s); { parse immediate }
         if s.m <> rgimm then prterr(emodt); { must be imm }
         valmac(mt386); { 386+ }
         if absolute(s.vl) and (dispsz(s.vl) = 1) then begin

            { immediate fits in signed byte }
            outbyt($6b); { output opcode }
            opsz := 1 { set byte size }
      
         end else outbyt($69); { output opcode }
         addr(sreg(l.m)*8, r); { generate addressing }
         gensym(s.vl, 0, false, imnorm, 0, 0, opsz*8) { generate immediate }
         
      end

   end

end;

{*******************************************************************************

Process in instruction

*******************************************************************************}

procedure inp;

var l, r: parrec; { parameters }
    opsz: opsize; { size of operands and operation }

begin

   parsgl(opin, l, opsz); { parse single operand }
   skpspc; { skip spaces }
   prcnxt(',', ecmaexp); { skip ',' }
   parcod(r); { parse right parameter }
   { must be al, ax or eax }
   if not (l.m in [rgal, rgax, rgeax]) then prterr(emodt);
   if r.m = rgimm then begin { in r,imm }

      { output opcode }
      if opsz = 1 then outbyt($e4) else outbyt($e5);
      gensym(r.vl, 0, false, imnorm, 0, 0, 1*8) { generate immediate }

   end else if r.m = rgdx then begin { in r,dx }

      { output opcode }
      if opsz = 1 then outbyt($ec) else outbyt($ed)

   end else prterr(emodt) { incorrect parameter }

end;

{*******************************************************************************

Process out instruction

*******************************************************************************}

procedure outp;

var l, r: parrec; { parameters }
    opsz: opsize; { size of operands and operation }

begin

   parcod(l); { parse I/O address }
   prcnxt(',', ecmaexp); { skip ',' }
   parsgl(opout, r, opsz); { parse double operand }
   { must be al, ax or eax }
   if not (r.m in [rgal, rgax, rgeax]) then prterr(emodt);
   if l.m = rgimm then begin { in imm,r }

      { output opcode }
      if opsz = 1 then outbyt($e6) else outbyt($e7);
      gensym(l.vl, 0, false, imnorm, 0, 0, 1*8) { generate immediate }

   end else if l.m = rgdx then begin { in r,dx }

      { output opcode }
      if opsz = 1 then outbyt($ee) else outbyt($ef)

   end else prterr(emodt) { incorrect parameter }

end;

{*******************************************************************************

Process int instruction

*******************************************************************************}

procedure int;

var p: parrec; { parameter }

begin

   parcod(p); { parse operand }
   if p.m <> rgimm then prterr(epart); { must be imm }
   if absolute(p.vl) and (p.vl^.val = 3) then outbyt($cc) { int 3 }
   else begin { int imm }

      outbyt($cd); { output opcode }
      gensym(p.vl, 0, false, imnorm, 0, 0, 1*8) { generate immediate }

   end

end;

{*******************************************************************************

Process jump conditional instructions

*******************************************************************************}

procedure jmpc(i: opcodet);

var p:      parrec;  { parameters }
    opcode: byte;    { opcode contructor byte }
    offset: integer; { offset of relative jump }

begin

   parcod(p); { parse parameter }
   if p.m <> rgimm then prterr(emodt); { must be imm }
   { form condition code }
   case i of { opcode }

      opjb,    opjbs,  opjbm,  opjbl,  
      opjnae, opjnaes, opjnaem, opjnael: opcode := $2;
      opjae,   opjaes, opjaem, opjael, 
      opjnb,  opjnbs,  opjnbm,  opjnbl:  opcode := $3;    
      opjbe,   opjbes, opjbem, opjbel, 
      opjna,  opjnas,  opjnam,  opjnal:  opcode := $6;    
      opja,    opjas,  opjam,  opjal,  
      opjnbe, opjnbes, opjnbem, opjnbel: opcode := $7;    
      opje,    opjes,  opjem,  opjel,  
      opjz,   opjzs,   opjzm,   opjzl:   opcode := $4;    
      opjne,   opjnes, opjnem, opjnel, 
      opjnz,  opjnzs,  opjnzm,  opjnzl:  opcode := $5;    
      opjl,    opjls,  opjlm,  opjll,  
      opjnge, opjnges, opjngem, opjngel: opcode := $c;    
      opjge,   opjges, opjgem, opjgel, 
      opjnl,  opjnls,  opjnlm,  opjnll:  opcode := $d;    
      opjle,   opjles, opjlem, opjlel, 
      opjng,  opjngs,  opjngm,  opjngl:  opcode := $e;    
      opjg,    opjgs,  opjgm,  opjgl,  
      opjnle, opjnles, opjnlem, opjnlel: opcode := $f;    
      opjs,    opjss,  opjsm,  opjsl:    opcode := $8;
      opjns,   opjnss, opjnsm, opjnsl:   opcode := $9;
      opjc,    opjcs,  opjcm,  opjcl:    opcode := $2;
      opjnc,   opjncs, opjncm, opjncl:   opcode := $3;
      opjo,    opjos,  opjom,  opjol:    opcode := $0;
      opjno,   opjnos, opjnom, opjnol:   opcode := $1;
      opjp,    opjps,  opjpm,  opjpl,  
      opjpe,  opjpes,  opjpem,  opjpel:  opcode := $a;    
      opjnp,   opjnps, opjnpm, opjnpl, 
      opjpo,  opjpos,  opjpom,  opjpol:  opcode := $b;    
      opjcxz,  opjecxz, opjmps: { are taken care of with special code }

   end;
   if i = opjmps then begin { jmps }

      outbyt($eb); { output opcode }
      gensym(p.vl, 0, false, imsgof, 0, 0, 1*8) { generate relative offset }

   end else if (i = opjcxz) or (i = opjecxz) then begin { jcxz, jecxz }

      setsiz(i = opjecxz); { set operand size as appropriate }
      outbyt($e3); { output opcode }
      gensym(p.vl, 0, false, imsgof, 0, 0, 1*8) { generate relative offset }
   
   end else if opclen[i] = 1 then begin { short form specified }

      outbyt($70+opcode); { output opcode }
      gensym(p.vl, 0, false, imsgof, 0, 0, 1*8) { generate relative offset }

   end else if opclen[i] = 2 then begin { medium form specified }

      setsiz(false); { set small model addressing }
      outbyt($0f); { output 1st byte of opcode }
      outbyt($80+opcode); { output 2nd byte of opcode }
      gensym(p.vl, 0, false, imsgof, 0, 0, 2*8) { generate relative offset }

   end else if opclen[i] = 4 then begin { long form specified }

      setsiz(true); { set large model addressing }
      outbyt($0f); { output 1st byte of opcode }
      outbyt($80+opcode); { output 2nd byte of opcode }
      gensym(p.vl, 0, false, imsgof, 0, 0, 4*8) { generate relative offset }

   end else begin { generic form, generate "smart" jumps }

      offset := maxint; { set largest value of offset }
      { find net offset if the target is a defined program address }
      if p.vl^.def and p.vl^.add then offset := p.vl^.val-(prgmc+2);
      if (offset >= -128) and (offset <= +127) then begin { fits in byte }

         outbyt($70+opcode); { output opcode }
         { generate relative offset }
         gensym(p.vl, 0, false, imsgof, 0, 0, 1*8)

      end else if (cmachine < mt286) or
                  ((offset >= -32768) and (offset <= +32767)) then begin

         { Machine type is lower than 386 (which means word jumps or less),
           or small mode, or displacement fits in word. generate medium
           instruction. note that executing a medium jump in large mode,
           even with the escape byte, is still cheaper than a full long
           jump (by one byte). For small mode, we don't want to extend to
           large jumps just becase the jump is unknown. }
         setsiz(false); { set small model addressing }
         outbyt($0f); { output 1st byte of opcode }
         outbyt($80+opcode); { output 2nd byte of opcode }
         gensym(p.vl, 0, false, imsgof, 0, 0, 2*8) { generate relative offset }

      end else begin

         { all else is done by long jumps }
         setsiz(true); { set large model addressing }
         outbyt($0f); { output 1st byte of opcode }
         outbyt($80+opcode); { output 2nd byte of opcode }
         gensym(p.vl, 0, false, imsgof, 0, 0, 4*8) { generate relative offset }
      
      end

   end

end;

{*******************************************************************************

Process reg/mem word single instructions

*******************************************************************************}

procedure rmems(i: opcodet);

var p:      parrec; { parameters }
    opsz:   opsize; { size of operands and operation }
    opcode: byte; { opcode builder }

begin

   parsgl(i, p, opsz); { parse operand }
   if not (p.m in rmset) then prterr(emodt); { must be r/m }
   if (opsz <> 2) then prterr(eopsizi); { wrong size }
   valmac(mt286); { 286+ }
   outbyt($0f); { output 1st opcode byte }
   case i of { opcode }

      oplldt: begin outbyt($00); opcode := $10 end; { lldt }
      oplmsw: begin outbyt($01); opcode := $30 end; { lmsw }
      opltr:  begin outbyt($00); opcode := $18 end; { ltr }
      opsldt: begin outbyt($00); opcode := $00 end; { sldt }
      opsmsw: begin outbyt($01); opcode := $20 end; { smsw }
      opstr:  begin outbyt($00); opcode := $08 end; { str }
      opverr: begin outbyt($00); opcode := $20 end; { verr }
      opverw: begin outbyt($00); opcode := $28 end; { verw }

   end;
   addr(opcode, p) { inc r/m }

end;

{*******************************************************************************

Process loop instructions

*******************************************************************************}

procedure loop(i: opcodet);

var p:    parrec; { parameters }
    opsz: opsize; { size of operands and operation }

begin

   parsgl(i, p, opsz); { parse parameter }
   if p.m <> rgimm then prterr(emodt); { must be imm }
   { form condition code }
   case i of { opcode }

      oploop, oploopw, oploopd: outbyt($e2);      
      oploope, oploopew, oplooped,      
      oploopz, oploopzw, oploopzd: outbyt($e1);      
      oploopne, oploopnew, oploopned,      
      oploopnz, oploopnzw, oploopnzd: outbyt($e0)

   end;      
   gensym(p.vl, 0, false, imsgof, 0, 0, 1*8) { generate relative offset }

end;

{*******************************************************************************

Process move instruction

*******************************************************************************}

procedure move(i: opcodet);

var l, r:   parrec; { parameters }
    opcode: byte;   { opcode pieces }
    opsz:   opsize; { size of operands and operation }

begin

   pardbl(i, l, r, opsz); { parse double operand }
   if not ((l.m in segset) or (r.m in segset) or
           (l.m in spcset) or (r.m in spcset)) then begin

      { if a segment register or 32 bit special register is involved, then we
        don't output size prefix, since the presence if the segment register
        or 32 bit special register automatically states the size. So we cast 
        move as an unadjustable instruction, then perform that here as 
        required }
      if (opsz = 2) or (opsz = 4) then setsiz(opsz = 4)

   end;
   { move has many different modes, and casing the entire mode set
     is an unusual, but effective speedup }
   case l.m of { register/mode }

      rgnl: prterr(emodt); { must be an error }

      rgcr0, rgcr2, rgcr3, rgcr4: begin { mov crx,r32 }

         valmac(mt386); { 386+ }
         if not (r.m in drgset) then prterr(emodt); { must be 32 bit register }
         outbyt($0f); { output opcode bytes }
         outbyt($22);
         addr(sreg(l.m)*8, r) { generate addressing }

      end;

      rgdr0, rgdr1, rgdr2, rgdr3, rgdr6, rgdr7: begin { mov drx,r32 }

         valmac(mt386); { 386+ }
         if not (r.m in drgset) then prterr(emodt); { must be 32 bit register }
         outbyt($0f); { output opcode bytes }
         outbyt($23);
         addr(sreg(l.m)*8, r) { generate addressing }
      
      end;
         
      rgtr4, rgtr5, rgtr6, rgtr7: begin { mov trx,r32 }

         valmac(mt386); { 386+ }
         if not (r.m in drgset) then prterr(emodt); { must be 32 bit register }
         outbyt($0f); { output opcode bytes }
         outbyt($26);
         addr(sreg(l.m)*8, r) { generate addressing }
      
      end;

      rgcs, rgss, rgds, rges, rgfs, rggs: begin { mov sreg,r/m16 }

         outbyt($8e); { output opcode byte }
         addr(sreg(l.m)*8, r) { generate addressing }

      end;

      rgal, rgah, rgbl, rgbh, rgcl, rgch, rgdl, rgdh, rgax, rgbx, rgcx, rgdx,  
      rgbp, rgsi, rgdi, rgsp, rgeax, rgebx, rgecx, rgedx, rgebp, rgesi, rgedi,
      rgesp, rgst, rgstr, rgimm, rgiad, rgir, rgird, rgirs, rgirsd, rgirr, 
      rgirrd, rgirrs, rgirrsd: begin { other modes }

         if (l.m in [rgal, rgax, rgeax]) and (r.m = rgiad) then begin

            { mov acc,[i] }
            if opsz = 1 then outbyt($a0) else outbyt($a1); { output opcode }
            { output offset by mode }
            if large or (r.as = 4) then gensym(r.vl, 0, false, imnorm, 0, 0, 32)
            else gensym(r.vl, 0, false, imnorm, 0, 0, 16)

         end else if (r.m in [rgal, rgax, rgeax]) and (l.m = rgiad) then begin

            { mov [i],acc }
            if opsz = 1 then outbyt($a2) else outbyt($a3); { output opcode }
            { output offset by mode }
            if large or (l.as = 4) then gensym(l.vl, 0, false, imnorm, 0, 0, 32)
            else gensym(l.vl, 0, false, imnorm, 0, 0, 16)
            
         end else if (l.m in regset) and (r.m = rgimm) then begin

            { mov reg,imm }
            if opsz = 1 then opcode := $b0 else opcode := $b8; { set opcode }
            outbyt(opcode+sreg(l.m)); { output opcode with register }
            gensym(r.vl, 0, false, imnorm, 0, 0, opsz*8) { output immediate }

         end else if r.m = rgimm then begin

            { mov mem,imm }
            if opsz = 1 then outbyt($c6) else outbyt($c7); { set opcode }
            addr($00, l); { output r/m }
            gensym(r.vl, 0, false, imnorm, 0, 0, opsz*8) { output immediate }

         end else if (l.m in drgset) and (r.m in drset) then begin 

            { mov r32,drx }
            valmac(mt386); { 386+ }
            outbyt($0f); { output opcode bytes }
            outbyt($21);
            addr(sreg(r.m)*8, l) { generate addressing }

         end else if (l.m in drgset) and (r.m in trset) then begin 

            { mov r32,trx }
            valmac(mt386); { 386+ }
            outbyt($0f); { output opcode bytes }
            outbyt($24);
            addr(sreg(r.m)*8, l) { generate addressing }

         end else if (l.m in drgset) and (r.m in crset) then begin 

            { mov r32,crx }
            valmac(mt386); { 386+ }
            outbyt($0f); { output opcode bytes }
            outbyt($20);
            addr(sreg(r.m)*8, l) { generate addressing }

         end else if r.m in segset then begin

            { mov r/m,sreg. This particular form of a segment move
              instruction DOES require a prefix. Go figgure }
            setsiz(false);
            outbyt($8c); { output opcode byte }
            addr(sreg(r.m)*8, l) { generate addressing }

         end else if l.m in regset then begin

            { mov reg,r/m }
            if not (r.m in rmset) then prterr(emodt); { must be r/m }
            if opsz = 1 then outbyt($8a) else outbyt($8b); { set opcode }
            addr(sreg(l.m)*8, r) { output r/m }
            
         end else if r.m in regset then begin

            { mov r/m,reg }
            if opsz = 1 then outbyt($88) else outbyt($89); { set opcode }
            addr(sreg(r.m)*8, l) { output r/m }
            
         end else prterr(emodt) { invalid operand }
            
      end

   end

end;

{*******************************************************************************

Process movsx/movzx instruction

*******************************************************************************}

procedure movszx(i: opcodet);

var l, r:   parrec; { parameters }
    opcode: byte;   { opcode builder }

begin

   valmac(mt386); { 386+ }
   parcod(l); { parse left parameter }
   skpspc; { skip spaces }
   prcnxt(',', ecmaexp); { skip ',' }
   parcod(r); { parse right parameter }
   if not (l.m in lrgset) then prterr(emodt); { must be reg16/reg32 }
   if not (r.m in rmset) then prterr(emodt); { must be r/m }
   { check right side is byte or word }
   if (reglen[r.m] <> 1) and (reglen[r.m] <> 2) and 
      (reglen[r.m] <> 0) then prterr(emodt);
   { check operand size exists }
   if (opclen[i] = 0) and (reglen[r.m] = 0) then prterr(eopsiz);
   { if both instruction size and operand size are specified, they must match }
   if (opclen[i] <> 0) and (reglen[r.m] <> 0) and 
      (opclen[i] <> reglen[r.m]) then prterr(eopsizm);
   setsiz(l.m in drgset); { set size of left operand }
   { determine address mode size }
   if (r.as = 2) or (r.as = 2) then setadr(r.as = 4);
   outbyt($0f); { output 1st opcode byte }
   { set 2nd opcode byte }
   if (i = opmovsx) or (i = opmovsxb) or (i = opmovsxw) then opcode := $be
   else opcode := $b6;
   { set byte/word bit }
   if (reglen[r.m] = 2) or (opclen[i] = 2) then opcode := opcode+1;
   outbyt(opcode); { output 2nd opcode byte }
   addr(sreg(l.m)*8, r) { output r/m }

end;

{*******************************************************************************

Process mul instruction

*******************************************************************************}

procedure mul(i: opcodet);

var l, r:      parrec;  { parameters }
    opsz, rsz: opsize;  { size of operands and operation }
    adsz:      boolean; { address size (large/small) }

begin

   parsgl(i, l, opsz); { parse single operand }
   skpspc; { skip spaces }
   if chkchr = ',' then begin { double operand form }

      getchr; { skip ',' }
      { must be al, ax or eax }
      if not (l.m in [rgal, rgax, rgeax]) then prterr(emodt);
      { now we are in an odd position. We have allready formally parsed
        a parameter, which set the sizing for the operation. So we
        must parse the next parameter without doing that again }
      parcod(r); { parse parameter }
      rsz := reglen[r.m]; { find operand length }
      if (rsz <> 0) and (rsz <> opsz) then prterr(eopsizm); { size mismatch }
      { determine address mode size. Note that only one operand may be an
        address }
      adsz := large; { set default address requirement }
      if r.as = 4 then adsz := true { set large if required }
      else if r.as = 2 then adsz := false; { set small if required } 
      setadr(adsz); { set address sizing }
      if not (r.m in rmset) then prterr(emodt); { must be r/m }
      if opsz = 1 then outbyt($f6) else outbyt($f7); { output opcode }
      addr($20, r) { generate addressing for right }

   end else begin { single operand form }

      if not (l.m in rmset) then prterr(emodt); { must be r/m }
      if opsz = 1 then outbyt($f6) else outbyt($f7); { output opcode }
      addr($20, l) { generate addressing for right }

   end 

end;

{*******************************************************************************

Process neg/not instructions

*******************************************************************************}

procedure negnot(i: opcodet);

var p:    parrec; { parameters }
    opsz: opsize; { size of operands and operation }
    rm:   byte;   { opcode builder }

begin

   parsgl(i, p, opsz); { parse operand }
   if not (p.m in rmset) then prterr(emodt); { must be r/m }
   if opsz = 1 then outbyt($f6) else outbyt($f7); { output opcode }
   { set 2nd opcode }
   if (i = opneg) or (i = opnegb) or (i = opnegw) or (i = opnegd) then
      rm := $18 else rm := $10;
   addr(rm, p) { inc r/m }

end;

{*******************************************************************************

Process push/pop instructions

*******************************************************************************}

procedure pushpop(i: opcodet);

var p:          parrec; { parameters }
    opsz:       opsize; { size of operands and operation }
    opcode, rm: byte;   { opcode builder }

begin

   parsgl(i, p, opsz); { parse operand }
   if not (p.m in segset) then begin

      { if a segment register is involved, then we don't output
        size prefix, since the presence if the segment register
        automatically states the size. So we cast push/pop as an
        unadjustable instruction, then perform that here as
        required }
      if (opsz = 2) or (opsz = 4) then setsiz(opsz = 4)

   end;
   if p.m in lrgset then begin { push/pop reg }

      if (i = oppush) or (i = oppushw) or (i = oppushd) then opcode := $50
      else opcode := $58;
      outbyt(opcode+sreg(p.m)) { output opcode }   

   end else if p.m in segset then begin { push/pop sreg }

      if p.m in [rgfs, rggs] then begin { push/pop fs/gs }

         valmac(mt286); { 386+ }
         outbyt($0f); { output 1st opcode byte }
         if (i = oppush) or (i = oppushw) or (i = oppushd) then 
            opcode := $80 else opcode := $81

      end else begin

         if (i = oppush) or (i = oppushw) or (i = oppushd) then 
            opcode := $06 else opcode := $07

      end;
      outbyt(opcode+(sreg(p.m)*8)) { output opcode }   

   end else if p.m = rgimm then begin { push imm }

      { must be push only }
      if (i <> oppush) and (i <> oppushw) and (i <> oppushd) then
         prterr(emodt);
      valmac(mt186); { 186+ }
      if (opsz = 1) or (dispsz(p.vl) = 1) then begin

         { if byte operation specified, or it fits into a byte, then
           create signed byte operation }
         outbyt($6a); { output opcode }
         opsz := 1 { set byte }

      end else outbyt($68); { output 16/32 opcode }
      gensym(p.vl, 0, false, imnorm, 0, 0, opsz*8) { generate immediate }

   end else begin { push/pop r/m }      

      if not (p.m in rmset) then prterr(emodt); { must be r/m }
      if opsz = 1 then prterr(emodt); { must be 16/32 }
      { form opcode and parameters }
      if (i = oppush) or (i = oppushw) or (i = oppushd) then { push }
         begin outbyt($ff); rm := $30 end
      else
         begin outbyt($8f); rm := $00 end;
      addr(rm, p) { push/pop r/m }

   end

end;

{*******************************************************************************

Process shift and rotate instructions

*******************************************************************************}

procedure shift(i: opcodet);

var l, r: parrec; { parameters }
    rm:   byte;   { opcode piece }
    opsz: opsize; { size of operands and operation }

begin

   parsgl(i, l, opsz); { parse left parameter }
   skpspc; { skip spaces }
   prcnxt(',', ecmaexp); { skip ',' }
   parcod(r); { parse right parameter }
   if not (l.m in rmset) then prterr(emodt); { must be r/m }
   { set opcode peice for each instruction }
   case i of { opcode }

      oprol, oprolb, oprolw, oprold: rm := $00;
      opror, oprorb, oprorw, oprord: rm := $08;
      oprcl, oprclb, oprclw, oprcld: rm := $10;
      oprcr, oprcrb, oprcrw, oprcrd: rm := $18;
      opshl, opshlb, opshlw, opshld,
      opsal, opsalb, opsalw, opsald: rm := $20;
      opshr, opshrb, opshrw, opshrd: rm := $28;
      opsar, opsarb, opsarw, opsard: rm := $38

   end;
   if r.m = rgcl then begin { op r/m,cl }

      if opsz = 1 then outbyt($d2) else outbyt($d3); { output opcode }
      addr(rm, l) { output r/m }

   end else begin { op r/m,imm }

      if r.m <> rgimm then prterr(emodt); { must be imm }
      valmac(mt186); { 186+ }
      if r.vl^.val = 1 then begin { op r/m,1 }

         if opsz = 1 then outbyt($d0) else outbyt($d1); { output opcode }
         addr(rm, l) { output r/m }

      end else begin { op r/m,imm }

         if opsz = 1 then outbyt($c0) else outbyt($c1); { output opcode }
         addr(rm, l); { output r/m }
         gensym(r.vl, 0, false, imnorm, 0, 0, 1*8) { generate shift count }

      end

   end

end;

{*******************************************************************************

Process return instructions

*******************************************************************************}

procedure ret(i: opcodet);

var p: parrec; { parameters }

begin

   skpspc; { check any parameter exists }
   if endcmd then begin { no parameter }

      if i = opret then outbyt($c3) else outbyt($cb); { output opcode }

   end else begin { parameter exists }

      parcod(p); { parse parameter }
      if p.m <> rgimm then prterr(emodt); { must be imm }
      if i = opret then outbyt($c2) else outbyt($ca); { output opcode }
      gensym(p.vl, 0, false, imnorm, 0, 0, 2*8) { generate immediate }

   end

end;

{*******************************************************************************

Process set instructions

*******************************************************************************}

procedure setcc(i: opcodet);

var p:    parrec; { parameters }
    opsz: opsize; { size of operands and operation }

begin

   valmac(mt386); { 386+ }
   parsgl(i, p, opsz); { parse left parameter }
   if not (p.m in rmset) then prterr(emodt); { must be r/m }
   outbyt($0f); { output opcode }
   case i of { opcode }

      opseta:  outbyt($97);
      opsetae: outbyt($93);
      opsetb:  outbyt($92);
      opsetbe: outbyt($96);
      opsetc:  outbyt($92);
      opsete:  outbyt($94);
      opsetg:  outbyt($9f);
      opsetge: outbyt($9d);
      opsetl:  outbyt($9c);
      opsetle: outbyt($9e);
      opsetna: outbyt($96);
      opsetnae:outbyt($92);
      opsetnb: outbyt($93);
      opsetnbe:outbyt($97);
      opsetnc: outbyt($93);
      opsetne: outbyt($95);
      opsetng: outbyt($9e);
      opsetnge:outbyt($9c);
      opsetnl: outbyt($9d);
      opsetnle:outbyt($9f);
      opsetno: outbyt($91);
      opsetnp: outbyt($9b);
      opsetns: outbyt($99);
      opsetnz: outbyt($95);
      opseto:  outbyt($90);
      opsetp:  outbyt($9a);
      opsetpe: outbyt($9a);
      opsetpo: outbyt($9b);
      opsets:  outbyt($98);
      opsetz:  outbyt($94)

   end;
   addr($00, p) { output r/m }

end;

{*******************************************************************************

Process shld/shrd instructions

*******************************************************************************}

procedure shldrd(i: opcodet);

var l, r, c: parrec; { parameters }
    opcode:  byte;   { opcode piece }
    opsz:    opsize; { size of operands and operation }

begin

   valmac(mt386); { 386+ }
   pardbl(i, l, r, opsz); { parse parameters }
   skpspc; { skip spaces }
   prcnxt(',', ecmaexp); { skip ',' }
   parcod(c); { parse shift count parameter }
   if not (l.m in rmset) then prterr(emodt); { must be r/m }
   if not (r.m in lrgset) then prterr(emodt); { must be reg16/reg32 }
   if (c.m <> rgimm) and (c.m <> rgcl) then prterr(emodt); { must be imm/cl }
   outbyt($0f); { output 1st opcode byte }
   if c.m = rgcl then opcode := $a5 else opcode := $a4; { construct 2nd }
   if (i = opshrdw) or (i = opshrdd) then opcode := opcode+$08;
   outbyt(opcode); { output }
   addr(sreg(r.m)*8, l); { output r/m }
   { generate immediate, if selected }
   if c.m = rgimm then gensym(c.vl, 0, false, imnorm, 0, 0, 1*8)

end;

{*******************************************************************************

Process test instruction

*******************************************************************************}

procedure test(i: opcodet);

var l, r: parrec; { parameters }
    opsz: opsize; { size of operands and operation }

begin

   pardbl(i, l, r, opsz); { parse double operand }
   if not (l.m in rmset) then prterr(emodt); { must be r/m }
   if r.m = rgimm then begin { test r/m,imm }

      { check immediate to al/ax/eax }
      if l.m in [rgal, rgax, rgeax] then begin { test a..,imm }

         if opsz = 1 then outbyt($a8) else outbyt($a9); { output opcode }
         gensym(r.vl, 0, false, imnorm, 0, 0, opsz*8)

      end else begin { add r/m,imm }

         if opsz = 1 then outbyt($f6) else outbyt($f7); { output opcode }
         addr($00, l); { generate addressing for left }
         gensym(r.vl, 0, false, imnorm, 0, 0, opsz*8) { output immediate }

      end

   end else begin { add r/m,reg }

      if not (r.m in regset) then prterr(emodt); { must be register }
      if opsz = 1 then outbyt($84) else outbyt($85); { output opcode }
      addr(sreg(r.m)*8, l) { generate r/m }

   end

end;

{*******************************************************************************

Process xchg instruction

*******************************************************************************}

procedure xchg(i: opcodet);

var l, r: parrec; { parameters }
    opsz: opsize; { size of operands and operation }

begin

   pardbl(i, l, r, opsz); { parse double operand }
   if not (l.m in rmset) then prterr(emodt); { must be r/m }
   if not (r.m in rmset) then prterr(emodt); { must be r/m }
   if ((l.m = rgax) or (l.m = rgeax)) and (r.m in lrgset) then
      outbyt($90+sreg(r.m)) { xchg [e]ax,reg }
   else if ((r.m = rgax) or (r.m = rgeax)) and (l.m in lrgset) then
      outbyt($90+sreg(l.m)) { xchg reg,[e]ax }
   else if l.m in regset then begin { xchg reg,r/m }

      if opsz = 1 then outbyt($86) else outbyt($87); { output opcode }
      addr(sreg(l.m)*8, r) { output r/m }

   end else if r.m in regset then begin { xchg r/m,reg }

      if opsz = 1 then outbyt($86) else outbyt($87); { output opcode }
      addr(sreg(r.m)*8, l) { output r/m }

   end else prterr(emodt) { one side must be a regsister }

end;

{*******************************************************************************

Define segmented address

Creates a segmented address as program side data. A segmented address can be
either 32 bits or 48 bits, and is of the form:

offset
segment

The offset is 16 bits on small model, and 32 bits on large model.

*******************************************************************************}

procedure defseg(i: opcodet);

var opcsz: boolean; { opcode size (large/small) }
    opsz:  opsize;  { size of operands and operation }
    sym:   symptr;  { value }

begin

   { find the size requirement of the opcode, which can be large,
     small or default }
   opcsz := large; { set default status of operand size }
   if opclen[i] = 4 then opcsz := false { set small model }
   else if opclen[i] = 6 then opcsz := true; { set large model }
   { set size plus segment, which is always 2 bytes }
   if opcsz then opsz := 4+2 else opsz := 2+2;
   repeat { parse values }

      expr(sym); { parse value }
      if sym^.typ <> stint then prterr(etyp);
      gensym(sym, 0, false, imiseg, 0, 0, opsz*8);
      skpspc; { skip spaces }
      c := chkchr; { check next }
      if c = ',' then getchr { ',', another value present }

   until c <> ',' { no more values }

end;

{*******************************************************************************

Process opcode

Expects the opcode in labbuf. A search is done for the opcode, and a handler is 
executed for the opcode.
Note that the entire process may be carried out here (for simple instructions), 
and that multiple opcodes are sometimes assigned to a single handler. In the 
multiple opcode case, the actuall 'code' for the instruction is passed to the 
handler.
for all opcodes except 'equ', 'glbl', 'extl', 'macro' and 'dv' a default line 
label declaration is performed.

*******************************************************************************}

procedure mprcopc;

var i: opcodet; { code to execute }

begin

   i := fndres(labbuf); { get reserved code }
   if i = opnull then prterr(eopcnf); { none found }
   { check perform label equation }
   if (i <> opequ) and (i <> opsetequ) and (i <> opglobal) and
      (i <> opextern) and (i <> opmacro) and (i <> opdefvs) then prclab;
   skpspc; { skip spaces }
   case i of { opcode }

      opaaa:       outbyt($37); { aaa }
      opaad:       begin outbyt($d5); outbyt($0a) end; { aad }
      opaam:       begin outbyt($d4); outbyt($0a) end; { aam }
      opaas:       outbyt($3f);
      opadc,
      opadcb,
      opadcw,
      opadcd:      dopr(i); { adc x,y }
      opadd,
      opaddb,
      opaddw,
      opaddd:      dopr(i); { add x,y }
      opand,
      opandb,
      opandw,
      opandd:      dopr(i); { and x, y }
      oparpl:      arpl; { arpl x, y }
      opbound:     regmem(i); { bound x, y }
      opbsf:       regmem(i); { bsf r,m }
      opbsr:       regmem(i); { bsr r,m }
      opbswap:     bswap; { bswap r }
      opbt,
      opbtw,
      opbtd:       btest(i); { bt m,r }
      opbtc,
      opbtcw,
      opbtcd:      btest(i); { btc m,r }
      opbtr,
      opbtrw,
      opbtrd:      btest(i); { btr m,r }
      opbts,
      opbtsw,
      opbtsd:      btest(i); { bts m,r }
      opcall:      caljmp(i); { call r/m }
      opcallm:     caljmp(i); { call r/m }
      opcalll:     caljmp(i); { call r/m }
      opcallf:     caljmpf(i); { call sa }
      opcallmf:    caljmpf(i); { call s,o }
      opcalllf:    caljmpf(i); { call s,o }
      opcbw:       begin setsiz(false); outbyt($98) end; { cbw }
      opcwde:      begin valmac(mt386); setsiz(true); outbyt($98) end; { cwde }
      opclc:       outbyt($f8); { clc }
      opcld:       outbyt($fc); { cld }
      opcli:       outbyt($fa); { cli }
      opclts:      begin valmac(mt286); outbyt($0f); outbyt($06) end; { clts }
      opcmc:       outbyt($f5); { cmc }
      opcmp:       dopr(i);
      opcmpb:      dopr(i);
      opcmpw:      dopr(i);
      opcmpd:      dopr(i);
      opcmpsb:     outbyt($a6);
      opcmpsw:     begin setsiz(false); outbyt($a7) end;
      opcmpsd:     begin setsiz(true); outbyt($a7) end;
      opcmpxchg:   xchop(i);
      opcmpxchg8b: cpxh8b;
      opcpuid:     begin valmac(mt586); outbyt($0f); outbyt($a2) end;
      opcwd:       begin setsiz(false); outbyt($99) end;
      opcdq:       begin valmac(mt386); setsiz(true); outbyt($99) end;
      opdaa:       outbyt($27);
      opdas:       outbyt($2f);
      opdec:       decinc(i);
      opdecb:      decinc(i);
      opdecw:      decinc(i);
      opdecd:      decinc(i);
      opdiv:       idiv(i);
      opdivb:      idiv(i);
      opdivw:      idiv(i);
      opdivd:      idiv(i);
      openter:     enter;
      opf2xm1:     begin valflt; outbyt($d9); outbyt($f0) end;
      opfabs:      begin valflt; outbyt($d9); outbyt($e1) end;
      opfadd:      fop(i);
      opfadds:     fop(i);
      opfaddd:     fop(i);
      opfaddp:     fopp(i);
      opfiadd:     fopi(i);
      opfiaddw:    fopi(i);
      opfiaddd:    fopi(i);
      opfbld:      smem(i);
      opfbstp:     smem(i);
      opfchs:      begin valflt; outbyt($d9); outbyt($e0) end;
      opfclex:     begin valflt; outbyt($9b); outbyt($db); outbyt($e2) end;
      opfnclex:    begin valflt; outbyt($db); outbyt($e2) end;
      opfcom:      fcom(i);
      opfcoms:     fcom(i);
      opfcomd:     fcom(i);
      opfcomp:     fcom(i);
      opfcomps:    fcom(i);
      opfcompd:    fcom(i);
      opfcompp:    fcom(i);
      opfcos:      begin valflt; outbyt($d9); outbyt($ff) end;
      opfdecstp:   begin valflt; outbyt($d9); outbyt($f6) end;
      opfdisi:     begin valflt; valmacs(mt86); outbyt($9b); outbyt($db); 
                         outbyt($e1) end;
      opfndisi:    begin valflt; valmacs(mt86); outbyt($db); outbyt($e1) end;
      opfdiv:      fop(i);
      opfdivs:     fop(i);
      opfdivd:     fop(i);
      opfdivp:     fopp(i);
      opfidiv:     fopi(i);
      opfidivw:    fopi(i);
      opfidivd:    fopi(i);
      opfdivr:     fop(i);
      opfdivrs:    fop(i);
      opfdivrd:    fop(i);
      opfdivrp:    fopp(i);
      opfidivr:    fopi(i);
      opfidivrw:   fopi(i);
      opfidivrd:   fopi(i);
      opfeni:      begin valflt; valmacs(mt86); outbyt($9b); outbyt($db); 
                         outbyt($e0) end;
      opfneni:     begin valflt; valmacs(mt86); outbyt($db); outbyt($e0) end;
      opffree:     ffree(i);
      opficom:     ficom(i);
      opficomw:    ficom(i);
      opficomd:    ficom(i);
      opficomp:    ficom(i);
      opficompw:   ficom(i);
      opficompd:   ficom(i);
      opfild:      filds(i);
      opfildw:     filds(i);
      opfildd:     filds(i);
      opfildq:     filds(i);
      opfincstp:   begin valflt; outbyt($d9); outbyt($f7) end;
      opfinit:     begin valflt; outbyt($9b); outbyt($db); outbyt($e3) end;
      opfninit:    begin valflt; outbyt($db); outbyt($e3) end;
      opfist:      filds(i);
      opfistw:     filds(i);
      opfistd:     filds(i);
      opfistp:     filds(i);
      opfistpw:    filds(i);
      opfistpd:    filds(i);
      opfistpq:    filds(i);
      opfld:       filds(i);
      opflds:      filds(i);
      opfldd:      filds(i);
      opfldl:      filds(i);
      opfld1:      begin valflt; outbyt($d9); outbyt($e8) end;
      opfldl2t:    begin valflt; outbyt($d9); outbyt($e9) end;
      opfldl2e:    begin valflt; outbyt($d9); outbyt($ea) end;
      opfldpi:     begin valflt; outbyt($d9); outbyt($eb) end;
      opfldlg2:    begin valflt; outbyt($d9); outbyt($ec) end;
      opfldln2:    begin valflt; outbyt($d9); outbyt($ed) end;
      opfldz:      begin valflt; outbyt($d9); outbyt($ee) end;
      opfldcw:     smem(i);
      opfldenvw:   smem(i);
      opfldenvd:   smem(i);
      opfmul:      fop(i);
      opfmuls:     fop(i);
      opfmuld:     fop(i);
      opfmulp:     fopp(i);
      opfimul:     fopi(i);
      opfimulw:    fopi(i);
      opfimuld:    fopi(i);
      opfnop:      begin valflt; outbyt($d9); outbyt($d0) end;
      opfpatan:    begin valflt; outbyt($d9); outbyt($f3) end;
      opfprem:     begin valflt; outbyt($d9); outbyt($f8) end;
      opfprem1:    begin valflt; outbyt($d9); outbyt($f5) end;
      opfptan:     begin valflt; outbyt($d9); outbyt($f2) end;
      opfrndint:   begin valflt; outbyt($d9); outbyt($fc) end;
      opfrstorw:   smem(i);
      opfrstord:   smem(i);
      opfsetpm:    begin valflt; valmacs(mt286); outbyt($db); outbyt($e3) end;
      opfsavew:    smem(i);
      opfsaved:    smem(i);
      opfnsavew:   smem(i);
      opfnsaved:   smem(i);
      opfscale:    begin valflt; outbyt($d9); outbyt($fd) end;
      opfsin:      begin valflt; outbyt($d9); outbyt($fe) end;
      opfsincos:   begin valflt; outbyt($d9); outbyt($fb) end;
      opfsqrt:     begin valflt; outbyt($d9); outbyt($fa) end;
      opfst:       filds(i);
      opfsts:      filds(i);
      opfstd:      filds(i);
      opfstp:      filds(i);
      opfstps:     filds(i);
      opfstpd:     filds(i);
      opfstpl:     filds(i);
      opfstcw:     smem(i);
      opfnstcw:    smem(i);
      opfstenvw:   smem(i);
      opfstenvd:   smem(i);
      opfnstenvw:  smem(i);
      opfnstenvd:  smem(i);
      opfstsw:     fstsw(i);
      opfnstsw:    fstsw(i);
      opfsub:      fop(i);
      opfsubs:     fop(i);
      opfsubd:     fop(i);
      opfsubp:     fopp(i);
      opfisub:     fopi(i);
      opfisubw:    fopi(i);
      opfisubd:    fopi(i);
      opfsubr:     fop(i);
      opfsubrs:    fop(i);
      opfsubrd:    fop(i);
      opfsubrp:    fopp(i);
      opfisubr:    fopi(i);
      opfisubrw:   fopi(i);
      opfisubrd:   fopi(i);
      opftst:      begin valflt; outbyt($d9); outbyt($e4) end;
      opfucom:     fucomx(i);
      opfucomp:    fucomx(i);
      opfucompp:   begin valflt; outbyt($da); outbyt($e9) end;
      opfwait:     outbyt($9b);
      opfxam:      begin valflt; outbyt($d9); outbyt($e5) end;
      opfxch:      fucomx(i);
      opfxtract:   begin valflt; outbyt($d9); outbyt($f4) end;
      opfyl2x:     begin valflt; outbyt($d9); outbyt($f1) end;
      opfyl2xp1:   begin valflt; outbyt($d9); outbyt($f9) end;
      ophlt:       outbyt($f4);
      opidiv:      idiv(i);
      opidivb:     idiv(i);
      opidivw:     idiv(i);
      opidivd:     idiv(i);
      opimul:      imul(i);
      opimulb:     imul(i);
      opimulw:     imul(i);
      opimuld:     imul(i);
      opin:        inp;
      opinc:       decinc(i);
      opincb:      decinc(i);
      opincw:      decinc(i);
      opincd:      decinc(i);
      opinsb:      begin valmac(mt186); outbyt($6c) end;
      opinsw:      begin valmac(mt186); setsiz(false); outbyt($6d) end;
      opinsd:      begin valmac(mt186); setsiz(true); outbyt($6d) end;
      opint:       int;
      opinto:      outbyt($ce);
      opinvd:      begin valmac(mt486); outbyt($0f); outbyt($08) end;
      opinvlpg:    smem(i);
      opiret:      begin setsiz(false); outbyt($cf) end;
      opiretd:     begin setsiz(true); outbyt($cf) end;
      opja:        jmpc(i);
      opjae:       jmpc(i);
      opjb:        jmpc(i);
      opjbe:       jmpc(i);
      opjc:        jmpc(i);
      opjcxz:      jmpc(i);
      opjecxz:     jmpc(i);
      opje:        jmpc(i);
      opjz:        jmpc(i);
      opjg:        jmpc(i);
      opjge:       jmpc(i);
      opjl:        jmpc(i);
      opjle:       jmpc(i);
      opjna:       jmpc(i);
      opjnae:      jmpc(i);
      opjnb:       jmpc(i);
      opjnbe:      jmpc(i);
      opjnc:       jmpc(i);
      opjne:       jmpc(i);
      opjng:       jmpc(i);
      opjnge:      jmpc(i);
      opjnl:       jmpc(i);
      opjnle:      jmpc(i);
      opjno:       jmpc(i);
      opjnp:       jmpc(i);
      opjns:       jmpc(i);
      opjnz:       jmpc(i);
      opjo:        jmpc(i);
      opjp:        jmpc(i);
      opjpe:       jmpc(i);
      opjpo:       jmpc(i);
      opjs:        jmpc(i);
      opjas:       jmpc(i);
      opjaes:      jmpc(i);
      opjbs:       jmpc(i);
      opjbes:      jmpc(i);
      opjcs:       jmpc(i);
      opjes:       jmpc(i);
      opjzs:       jmpc(i);
      opjgs:       jmpc(i);
      opjges:      jmpc(i);
      opjls:       jmpc(i);
      opjles:      jmpc(i);
      opjnas:      jmpc(i);
      opjnaes:     jmpc(i);
      opjnbs:      jmpc(i);
      opjnbes:     jmpc(i);
      opjncs:      jmpc(i);
      opjnes:      jmpc(i);
      opjngs:      jmpc(i);
      opjnges:     jmpc(i);
      opjnls:      jmpc(i);
      opjnles:     jmpc(i);
      opjnos:      jmpc(i);
      opjnps:      jmpc(i);
      opjnss:      jmpc(i);
      opjnzs:      jmpc(i);
      opjos:       jmpc(i);
      opjps:       jmpc(i);
      opjpes:      jmpc(i);
      opjpos:      jmpc(i);
      opjss:       jmpc(i);
      opjam:       jmpc(i);
      opjaem:      jmpc(i);
      opjbm:       jmpc(i);
      opjbem:      jmpc(i);
      opjcm:       jmpc(i);
      opjem:       jmpc(i);
      opjzm:       jmpc(i);
      opjgm:       jmpc(i);
      opjgem:      jmpc(i);
      opjlm:       jmpc(i);
      opjlem:      jmpc(i);
      opjnam:      jmpc(i);
      opjnaem:     jmpc(i);
      opjnbm:      jmpc(i);
      opjnbem:     jmpc(i);
      opjncm:      jmpc(i);
      opjnem:      jmpc(i);
      opjngm:      jmpc(i);
      opjngem:     jmpc(i);
      opjnlm:      jmpc(i);
      opjnlem:     jmpc(i);
      opjnom:      jmpc(i);
      opjnpm:      jmpc(i);
      opjnsm:      jmpc(i);
      opjnzm:      jmpc(i);
      opjom:       jmpc(i);
      opjpm:       jmpc(i);
      opjpem:      jmpc(i);
      opjpom:      jmpc(i);
      opjsm:       jmpc(i);
      opjal:       jmpc(i);
      opjael:      jmpc(i);
      opjbl:       jmpc(i);
      opjbel:      jmpc(i);
      opjcl:       jmpc(i);
      opjel:       jmpc(i);
      opjzl:       jmpc(i);
      opjgl:       jmpc(i);
      opjgel:      jmpc(i);
      opjll:       jmpc(i);
      opjlel:      jmpc(i);
      opjnal:      jmpc(i);
      opjnael:     jmpc(i);
      opjnbl:      jmpc(i);
      opjnbel:     jmpc(i);
      opjncl:      jmpc(i);
      opjnel:      jmpc(i);
      opjngl:      jmpc(i);
      opjngel:     jmpc(i);
      opjnll:      jmpc(i);
      opjnlel:     jmpc(i);
      opjnol:      jmpc(i);
      opjnpl:      jmpc(i);
      opjnsl:      jmpc(i);
      opjnzl:      jmpc(i);
      opjol:       jmpc(i);
      opjpl:       jmpc(i);
      opjpel:      jmpc(i);
      opjpol:      jmpc(i);
      opjsl:       jmpc(i);
      opjmp:       caljmp(i);
      opjmps:      jmpc(i);
      opjmpm:      caljmp(i);
      opjmpl:      caljmp(i);
      opjmpf:      caljmpf(i);
      opjmpmf:     caljmpf(i);
      opjmplf:     caljmpf(i);
      oplahf:      outbyt($9f);
      oplar:       regmem(i);
      oplds:       regmem(i);
      oples:       regmem(i);
      oplfs:       regmem(i);
      oplgs:       regmem(i);
      oplss:       regmem(i);
      oplea:       regmem(i);
      opleave:     begin valmac(mt186); outbyt($c9) end;
      opleavew:    begin valmac(mt186); setsiz(false); outbyt($c9) end;
      opleaved:    begin valmac(mt186); setsiz(true); outbyt($c9) end;
      oplgdtw:     smem(i);
      oplgdtd:     smem(i);
      oplidtw:     smem(i);
      oplidtd:     smem(i);
      oplldt:      rmems(i);
      oplmsw:      rmems(i);
      oplock:      outbyt($f0);
      oplodsb:     outbyt($ac);
      oplodsw:     begin setsiz(false); outbyt($ad) end;
      oplodsd:     begin setsiz(true); outbyt($ad) end;
      oploop:      loop(i);
      oploopw:     loop(i);
      oploopd:     loop(i);
      oploope:     loop(i);
      oploopew:    loop(i);
      oplooped:    loop(i);
      oploopz:     loop(i);
      oploopzw:    loop(i);
      oploopzd:    loop(i);
      oploopne:    loop(i);
      oploopnew:   loop(i);
      oploopned:   loop(i);
      oploopnz:    loop(i);
      oploopnzw:   loop(i);
      oploopnzd:   loop(i);
      oplsl:       regmem(i);
      opltr:       rmems(i);
      opmov:       move(i);
      opmovb:      move(i);
      opmovw:      move(i);
      opmovd:      move(i);
      opmovsb:     outbyt($a4);
      opmovsw:     begin setsiz(false); outbyt($a5) end;
      opmovsd:     begin setsiz(true); outbyt($a5) end;
      opmovsx:     movszx(i);
      opmovsxb:    movszx(i);
      opmovsxw:    movszx(i);
      opmovzx:     movszx(i);
      opmovzxb:    movszx(i);
      opmovzxw:    movszx(i);
      opmul:       mul(i);
      opmulb:      mul(i);
      opmulw:      mul(i);
      opmuld:      mul(i);
      opneg:       negnot(i);
      opnegb:      negnot(i);
      opnegw:      negnot(i);
      opnegd:      negnot(i);
      opnop:       outbyt($90);
      opnot:       negnot(i);
      opnotb:      negnot(i);
      opnotw:      negnot(i);
      opnotd:      negnot(i);
      opor:        dopr(i);
      oporb:       dopr(i);
      oporw:       dopr(i);
      opord:       dopr(i);
      opout:       outp;
      opoutsb:     begin valmac(mt186); outbyt($6e) end;
      opoutsw:     begin valmac(mt186); setsiz(false); outbyt($6f) end;
      opoutsd:     begin valmac(mt186); setsiz(true); outbyt($6f) end;
      oppop:       pushpop(i);
      oppopw:      pushpop(i);
      oppopd:      pushpop(i);
      oppopa:      begin valmac(mt186); setsiz(false); outbyt($61) end;
      oppopad:     begin valmac(mt186); setsiz(true); outbyt($61) end;
      oppopf:      begin setsiz(false); outbyt($9d) end;
      oppopfd:     begin setsiz(true); outbyt($9d) end;
      oppush:      pushpop(i);
      oppushb:     pushpop(i);
      oppushw:     pushpop(i);
      oppushd:     pushpop(i);
      oppusha:     begin valmac(mt186); setsiz(false); outbyt($60) end;
      oppushad:    begin valmac(mt186); setsiz(true); outbyt($60) end;
      oppushf:     begin setsiz(false); outbyt($9c) end;
      oppushfd:    begin setsiz(true); outbyt($9c) end;
      oprcl:       shift(i);
      oprclb:      shift(i);
      oprclw:      shift(i);
      oprcld:      shift(i);
      oprcr:       shift(i);
      oprcrb:      shift(i);
      oprcrw:      shift(i);
      oprcrd:      shift(i);
      oprol:       shift(i);
      oprolb:      shift(i);
      oprolw:      shift(i);
      oprold:      shift(i);
      opror:       shift(i);
      oprorb:      shift(i);
      oprorw:      shift(i);
      oprord:      shift(i);
      oprdmsr:     begin valmac(mt586); outbyt($0f); outbyt($32) end;
      oprep:       outbyt($f3);
      oprepe:      outbyt($f3);
      oprepz:      outbyt($f3);
      oprepne:     outbyt($f2);
      oprepnz:     outbyt($f2);
      opret:       ret(i);
      opretf:      ret(i);
      oprsm:       begin valmac(mt586); outbyt($0f); outbyt($aa) end;
      opsahf:      outbyt($9e);
      opsal:       shift(i);
      opsalb:      shift(i);
      opsalw:      shift(i);
      opsald:      shift(i);
      opsarb:      shift(i);
      opsarw:      shift(i);
      opsard:      shift(i);
      opshl:       shift(i);
      opshlb:      shift(i);
      opshlw:      shift(i);
      opshld:      shift(i);
      opshr:       shift(i);
      opshrb:      shift(i);
      opshrw:      shift(i);
      opshrd:      shift(i);
      opsbb:       dopr(i);
      opsbbb:      dopr(i);
      opsbbw:      dopr(i);
      opsbbd:      dopr(i);
      opscasb:     outbyt($ae);
      opscasw:     begin setsiz(false); outbyt($af) end;
      opscasd:     begin setsiz(true); outbyt($af) end;
      opseta:      setcc(i);
      opsetae:     setcc(i);
      opsetb:      setcc(i);
      opsetbe:     setcc(i);
      opsetc:      setcc(i);
      opsete:      setcc(i);
      opsetg:      setcc(i);
      opsetge:     setcc(i);
      opsetl:      setcc(i);
      opsetle:     setcc(i);
      opsetna:     setcc(i);
      opsetnae:    setcc(i);
      opsetnb:     setcc(i);
      opsetnbe:    setcc(i);
      opsetnc:     setcc(i);
      opsetne:     setcc(i);
      opsetng:     setcc(i);
      opsetnge:    setcc(i);
      opsetnl:     setcc(i);
      opsetnle:    setcc(i);
      opsetno:     setcc(i);
      opsetnp:     setcc(i);
      opsetns:     setcc(i);
      opsetnz:     setcc(i);
      opseto:      setcc(i);
      opsetp:      setcc(i);
      opsetpe:     setcc(i);
      opsetpo:     setcc(i);
      opsets:      setcc(i);
      opsetz:      setcc(i);
      opsgdtw:     smem(i);
      opsgdtd:     smem(i);
      opsidtw:     smem(i);
      opsidtd:     smem(i);
      opshldw:     shldrd(i);
      opshldd:     shldrd(i);
      opshrdw:     shldrd(i);
      opshrdd:     shldrd(i);
      opsldt:      rmems(i);
      opsmsw:      rmems(i);
      opstc:       outbyt($f9);
      opstd:       outbyt($fd);
      opsti:       outbyt($fb);
      opstosb:     outbyt($aa);
      opstosw:     begin setsiz(false); outbyt($ab) end;
      opstosd:     begin setsiz(true); outbyt($ab) end;
      opstr:       rmems(i);
      opsub:       dopr(i);
      opsubb:      dopr(i);
      opsubw:      dopr(i);
      opsubd:      dopr(i);
      optest:      test(i);
      optestb:     test(i);
      optestw:     test(i);
      optestd:     test(i);
      opverr:      rmems(i);
      opverw:      rmems(i);
      opwait:      outbyt($9b);
      opwbinvd:    begin valmac(mt486); outbyt($0f); outbyt($09) end;
      opwrmsr:     begin valmac(mt586); outbyt($0f); outbyt($30) end;
      opxadd:      xchop(i);
      opxchg:      xchg(i);
      opxlatb:     outbyt($d7);
      opxor:       dopr(i);
      opxorb:      dopr(i);
      opxorw:      dopr(i);
      opxord:      dopr(i);
      opcs:        outbyt($2e);
      opds:        outbyt($3e);
      opes:        outbyt($26);
      opfs:        begin valmac(mt386); outbyt($64) end;
      opgs:        begin valmac(mt386); outbyt($65) end;
      opss:        outbyt($36);

      { 80586 specific pseudo operations }

      oplarge:     begin

         valmac(mt386); { validate 386+ for large model }
         large := true { set model to large }

      end;
      opsmall:     large := false;   { set model to small }
      opfloat:     float := true; { set floating point enabled }
      opnfloat:    float := false; { set floating point disabled }
      opm86:       { restrict to 8086 instructions, small model }
         begin cmachine := mt86; large := false end;
      opm286:      { restrict to 80286 instructions, small model }
         begin cmachine := mt286; large := false end;
      opm386:      { restrict to 80386 instructions, large model }
         begin cmachine := mt386; large := true end;
      opm486:      { restrict to 80486 instructions, large model }
         begin cmachine := mt486; large := true end;
      opm586:      { restrict to 80586 instructions, large model }
         begin cmachine := mt586; large := true end;
      
      { assembler pseudo operations }

      opmacro:   macro;   { lab: macro x }
      opendmac:  endmac;  { endmac }
      opinclude: include; { include file }
      opequ:     equlab;  { lab: equ n }
      opsetequ:  setlab;  { lab: setequ n }
      opglobal:  gbllab;  { lab: global }
      opextern:  extlab;  { lab: extern }
      opalignp:  alignp;  { align program }
      opalignv:  alignv;  { align variable }
      opif:      iftr;    { if n }
      opelse:    elsec;   { else }
      opelseif:  elseif;  { elseif }
      opendif:   endif;   { endif }
      opassm:    assm;    { assm string }
      { Note that block definition is an experimental section of the assembler
        Present. }
      opblock:   block;   { start block }
      opendblk:  endblk;  { end block }
      opprint:   asprint(false); { print user message }
      operror:   asprint(true); { print user error }
      opstop:    asstop;  { stop assembly }
      opbendian: prterr(encend); { 80586 is not endian configurable }
      oplendian: prterr(encend); { 80586 is not endian configurable }
      opdefb:    defvall(true, false, 1); { defb b/str[,b/str]... }
      opdefps:   defps;   { defps n }
      opdefvs:   defvs;   { defvs n }
      opdefbe:   defval(true, false); { defbe l, n }
      opdefle:   defval(false, false); { defle l, n }
      opdefbef:  defval(true, true); { defbef l, n }
      opdeflef:  defval(false, true); { deflef l, n }
      opdeff:    defvall(cpubigend, true, 8); { deff n }
      opdefsf:   defvall(cpubigend, true, 4); { defsf n }
      opdeflf:   defvall(cpubigend, true, 10); { deflf n }
      opdefhw:   defvall(cpubigend, false, cpuwrdsiz div 2); { defhw n }
      opdefw:    defvall(cpubigend, false, cpuwrdsiz); { defw w[,w]... }
      opdefdw:   defvall(cpubigend, false, cpuwrdsiz*2); { defdw n }
      opdefqw:   defvall(cpubigend, false, cpuwrdsiz*4); { defqw n }
      opdefseg:  defseg(i); { define segmented address }
      opdefsegm: defseg(i); { define segmented address medium (32 bits) }
      opdefsegl: defseg(i); { define segmented address long (48 bits) }

   end

end;

begin

   refer(output) { hold the output file }

end.

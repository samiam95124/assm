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

module opcode;

uses asdef,  { generic definitions }
     common, { global variables }
     utl,    { generic utilities }
     direct, { generic assembler directives }
     asmain, { error handling }
     macdef, { processor specific definitions }
     macutl; { processor specific utilities }

procedure mprcopc; forward; { process opcode }

private

{******************************************************************************

Basic math operations

Encodes the following instructions:

     add  a,b
     adc  a,b
     sub  a,b
     sbc  a,b
     and  a,b
     xor  a,b
     or   a,b
     cp   a,b

******************************************************************************}

procedure bmath(i: opcodet { opcode to process });

var l, r: regc;   { holds register }
    oc:   byte;   { holds opcode being built }
    s:    symptr; { for displacements }

begin

   case i of { opcode }

      opadd: oc := $00; { add, the basic instruction }
      opadc: oc := $08; { adc }
      opsub: oc := $10; { sub }
      opsbc: oc := $18; { sbc }
      opand: oc := $20; { and }
      opxor: oc := $28; { xor }
      opor:  oc := $30; { or }
      opcp:  oc := $38  { cp }

   end;
   parcod(l, s); { parse left parameter }
   if (l = rghl) or (l = rgix) or (l = rgiy) then begin { word }

      skpspc; { skip spaces }
      prcnxt(',', ecmaexp); { process ',' }
      parcod(r, s); { parse right parameter }
      if not (r in [rga, rgbc, rgde, rghl, rgix, rgiy, rgsp]) then
         prterr(epart); { wrong type }
      if l = rgix then outbyt($dd); { ix }
      if l = rgiy then outbyt($fd); { iy }
      if i = opadd then case r of { add wr,wr }

         rgbc: outbyt($09); { add hl,bc }
         rgde: outbyt($19); { add hl,de }
         rghl, rgix, rgiy: begin

                  if l <> r then prterr(epart); { wrong type }
                  outbyt($29) { add hl,hl }

               end;
         rgsp: outbyt($39); { add hl,sp }
         rga:  begin outbyt($ed); outbyt($6d) end { add hl,a }

      end else if i = opadc then begin { adc wr,wr }

         outbyt($ed);
         case r of { register }

            rgbc: outbyt($4a); { adc wr,bc }
            rgde: outbyt($5a); { adc wr,de }
            rghl, rgix, rgiy: begin

                     if l <> r then prterr(epart); { wrong type }
                     outbyt($6a) { adc wr,wr }

                  end;
            rgsp: outbyt($7a); { adc wr,sp }
            rga:  prterr(epart) { wrong type }

         end

      end else if i = opsbc then begin { sbc wr,wr }

         outbyt($ed);
         case r of { register }

            rgbc: outbyt($42); { sbc wr,bc }
            rgde: outbyt($52); { sbc wr,de }
            rghl, rgix, rgiy: begin

                     if l <> r then prterr(epart); { wrong type }
                     outbyt($62) { sbc wr,wr }

                  end;
            rgsp: outbyt($72); { sbc wr,sp }
            rga:  prterr(epart) { wrong type }

         end

      end else prterr(epart) { wrong type }

   end else begin { byte }

      r := l; { save left }
      if chkchr = ',' then begin { must have been a, }

         getchr; { skip ',' }
         if l <> rga then prterr(epart); { wrong type }
         parcod(r, s) { parse right parameter }

      end;
      if not (r in [rga, rgb, rgc, rgd, rge, rgh, rgl, rgixh, rgixl,
                    rgiyh, rgiyl, rgihl, rgihld, rgiix, rgiixdb, rgimm,
                    rgiixd, rgiiy, rgiiydb, rgiiyd, rgisp, rgispd,
                    rgihlix, rgihliy, rgiixiy, rgiad, rgrad, rgipcd]) then
                       prterr(epart); { wrong type }
      case r of { parameter }

         rga:     outbyt($87+oc); { op a,a }
         rgb:     outbyt($80+oc); { op a,b }
         rgc:     outbyt($81+oc); { op a,c }
         rgd:     outbyt($82+oc); { op a,d }
         rge:     outbyt($83+oc); { op a,e }
         rgh:     outbyt($84+oc); { op a,h }
         rgl:     outbyt($85+oc); { op a,l }
         rgimm:   begin outbyt($c6+oc); 
                        gensym(s, 0, false, imnorm, 0, 0, 8) end; { op a,n }
         rgixh:   begin outbyt($dd); outbyt($84+oc) end; { op a,ixh }
         rgixl:   begin outbyt($dd); outbyt($85+oc) end; { op a,ixl }
         rgiyh:   begin outbyt($fd); outbyt($84+oc) end; { op a,iyh }
         rgiyl:   begin outbyt($fd); outbyt($85+oc) end; { op a,iyl }
         rgihl:   outbyt($86+oc); { op a,(hl) }
         rgihld:  begin outbyt($fd); outbyt($83+oc); { op a,(hl+dd) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgiix:   begin outbyt($dd); outbyt($86+oc); { op a,(ix) }
                        outbyt($00) end;
         rgiixdb: begin outbyt($dd); outbyt($86+oc); { op a,(ix+d) }
                        gensym(s, 0, false, imnorm, 0, 0, 8) end;
         rgiixd:  begin outbyt($fd); outbyt($81+oc); { op a,(ix+dd) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgiiy:   begin outbyt($fd); outbyt($86+oc); { op a,(iy) }
                        outbyt($00) end;
         rgiiydb: begin outbyt($fd); outbyt($86+oc); { op a,(iy+d) }
                        gensym(s, 0, false, imnorm, 0, 0, 8) end;
         rgiiyd:  begin outbyt($fd); outbyt($82+oc); { op a,(iy+dd) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgisp:   begin outbyt($dd); outbyt($80+oc); { op a,(sp) }
                        outval($0000, 2, false) end;
         rgispd:  begin outbyt($dd); outbyt($80+oc); { op a,(sp+dd) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgihlix: begin outbyt($dd); outbyt($81+oc) end; { op a,(hl+ix) }
         rgihliy: begin outbyt($dd); outbyt($82+oc) end; { op a,(hl+iy) }
         rgiixiy: begin outbyt($dd); outbyt($83+oc) end; { op a,(ix+iy) }
         rgiad:   begin outbyt($dd); outbyt($87+oc); { op a,(xx) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgrad:   begin outbyt($fd); outbyt($80+oc); { op a,<xx> }
                        gensym(s, 0, false, imsgof, 0, 0, 16) end;
         rgipcd:  begin outbyt($fd); outbyt($80+oc); { op a,(pc+xx) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end

      end

   end

end;

{******************************************************************************

Increment/decrement single

Encodes the following:

     inc   des
     dec   des

Where 'des' is any of ix, iy, hl, de, bc, sp, a, b, c, d, e, h, l, (ix+d), or
(iy+d). A variant field is produced for a displacement.

******************************************************************************}

procedure ids(i : opcodet { opcode to process });

var l      : regc;    { holds register }
    ob, ow : byte;    { holds opcode being built }
    s      : symptr;  { displacement }
    w      : boolean; { word operation }

begin

   if (i = opdec) or (i = opdecw) then
      begin ob := $01; ow := $08 end { dec }
   else
      begin ob := $00; ow := $00 end; { inc }
   w := (i = opincw) or (i = opdecw); { set word/byte status }
   parcod(l, s); { parse parameter }
   if l = rgnl then prterr(eparexp); { missing }
   if not (l in [rga, rgb, rgc, rgd, rge, rgh, rgl, rgixl,
                 rgixh, rgiyl, rgiyh, rgbc, rgde, rghl, rgihld,
                 rgix, rgiy, rgsp, rgihl, rgiad, rgiix, rgiiy, rgiixd,
                 rgiiyd, rgiixdb, rgiiydb, rgisp, rgispd, rgrad, rgipcd,
                 rgihlix, rgihliy, rgiixiy]) then
                    prterr(epart); { wrong type }
   if w and (l in [rga, rgb, rgc, rgd, rge, rgh, rgl, rgixh, rgixl,
                   rgiyh, rgiyl, rgihld, rgisp, rgispd, rgihlix,
                   rgihliy, rgiixiy]) then prterr(epart); { wrong type }
   case l of { parameter }

      rgbc:  outbyt($03+ow); { inc/dec bc }
      rgde:  outbyt($13+ow); { inc/dec de }
      rghl:  outbyt($23+ow); { inc/dec hl }
      rgsp:  outbyt($33+ow); { inc/dec sp }
      rgix:  begin outbyt($dd); outbyt($23+ow) end; { inc/dec ix }
      rgiy:  begin outbyt($fd); outbyt($23+ow) end; { inc/dec iy }
      rga:   outbyt($3c+ob); { inc/dec a }
      rgb:   outbyt($04+ob); { inc/dec b }
      rgc:   outbyt($0c+ob); { inc/dec c }
      rgd:   outbyt($14+ob); { inc/dec d }
      rge:   outbyt($1c+ob); { inc/dec e }
      rgh:   outbyt($24+ob); { inc/dec h }
      rgl:   outbyt($2c+ob); { inc/dec l }
      rgixh: begin outbyt($dd); outbyt($24+ob) end; { inc/dec ixh }
      rgixl: begin outbyt($dd); outbyt($2c+ob) end; { inc/dec ixl }
      rgiyh: begin outbyt($fd); outbyt($24+ob) end; { inc/dec iyh }
      rgiyl: begin outbyt($fd); outbyt($2c+ob) end; { inc/dec iyl }
      rgihl: if w then
                begin outbyt($dd); outbyt($03+ow) end { incw/decw (hl) }
             else outbyt($34+ob); { inc/dec (hl) }
      rgihld: begin outbyt($fd); outbyt($1c+ob); { inc/dec (hl+dd) }
                    gensym(s, 0, false, imnorm, 0, 0, 16) end;
      rgiixd: if w then begin

                outbyt($fd); outbyt($03+ow); { incw/decw (ix+dd) }
                gensym(s, 0, false, imnorm, 0, 0, 16)

              end else begin

                outbyt($fd); outbyt($0c+ob); { inc/dec (ix+dd) }
                gensym(s, 0, false, imnorm, 0, 0, 16)

              end;
      rgiiyd: if w then begin

                outbyt($fd); outbyt($13+ow); { incw/decw (iy+dd) }
                gensym(s, 0, false, imnorm, 0, 0, 16)

              end else begin

                outbyt($fd); outbyt($14+ob); { inc/dec (iy+dd) }
                gensym(s, 0, false, imnorm, 0, 0, 16)

              end;
      rgiixdb: if w then begin

                  { automatically promotes to word displacement }
                  outbyt($fd); outbyt($03+ow); { incw/decw (ix+dd) }
                  gensym(s, 0, false, imnorm, 0, 0, 16)

               end else begin

                  outbyt($dd); outbyt($34+ob); { inc/dec (ix+d) }
                  gensym(s, 0, false, imnorm, 0, 0, 8)

               end;
      rgiiydb: if w then begin

                { automatically promotes to word displacement }
                outbyt($fd); outbyt($13+ow); { incw/decw (iy+dd) }
                gensym(s, 0, false, imnorm, 0, 0, 16)

              end else begin

                outbyt($fd); outbyt($34+ob); { inc/dec (iy+d) }
                gensym(s, 0, false, imnorm, 0, 0, 8)

              end;
      rgiix:  if w then begin

                  { automatically promotes to word displacement }
                  outbyt($fd); outbyt($03+ow); { incw/decw (ix+dd) }
                  outval($0000, 2, false)

               end else begin

                  outbyt($dd); outbyt($34+ob); { inc/dec (ix+d) }
                  outbyt($00)

              end;
      rgiiy:  if w then begin

                { automatically promotes to word displacement }
                outbyt($fd); outbyt($13+ow); { incw/decw (iy+dd) }
                outval($0000, 2, false)

              end else begin

                outbyt($fd); outbyt($34+ob); { inc/dec (iy+d) }
                outbyt($00)

              end;
      rgisp:  begin

                 { automatically promotes to displacement }
                 outbyt($dd); outbyt($04+ob); { inc/dec (sp) }
                 outval($0000, 2, false)

              end;
      rgispd:  begin

                 outbyt($dd); outbyt($04+ob); { inc/dec (sp) }
                 gensym(s, 0, false, imnorm, 0, 0, 16)

              end;
      rgiad: if w then begin

                outbyt($dd); outbyt($13+ow); { incw/decw (xx) }
                gensym(s, 0, false, imnorm, 0, 0, 16)

             end else begin

                outbyt($dd); outbyt($3c+ob); { inc/dec (xx) }
                gensym(s, 0, false, imnorm, 0, 0, 16)

             end;
      rgrad: if w then begin

                outbyt($dd); outbyt($33+ow); { incw/decw <xx> }
                gensym(s, 0, false, imsgof, 0, 0, 16)

             end else begin

                outbyt($fd); outbyt($04+ob); { inc/dec <xx> }
                gensym(s, 0, false, imsgof, 0, 0, 16)

             end;
      rgipcd: if w then begin

                outbyt($dd); outbyt($33+ow); { incw/decw <xx> }
                gensym(s, 0, false, imnorm, 0, 0, 16)

             end else begin

                outbyt($fd); outbyt($04+ob); { inc/dec <xx> }
                gensym(s, 0, false, imnorm, 0, 0, 16)

             end;
      rgihlix, rgihliy, rgiixiy: begin

                outbyt($dd); { inc/dec (xx+yy) }
                if l = rgihlix then outbyt($0c+ob)
                else if l = rgihliy then outbyt($14+ob)
                else outbyt($1c+ob)

             end;

   end

end;

{******************************************************************************

Push/pop handler

Encodes a statement:

     push/pop  ww

Where 'ww' is one of ix, iy, hl, de, bc, af, nn, (hl), (addr), or <addr>.

******************************************************************************}

procedure pshpop(i: opcodet { opcode to process });

var l:  regc;   { parameter }
    oc: byte;   { holds opcode being built }
    s:  symptr; { symbol }

begin

   if i = oppush then oc := 4 { push }
   else oc := 0; { pop }
   parcod(l, s); { parse a register }
   if l = rgnl then prterr(eparexp); { missing }
   if not (l in [rgaf, rgbc, rgde, rghl, rgix, rgiy, rgihl, rgiad,
                rgrad, rgipcd, rgimm]) then prterr(epart); { wrong type }
   case l of { register }

      rgaf:   outbyt($f1+oc); { push/pop af }
      rgbc:   outbyt($c1+oc); { push/pop bc }
      rgde:   outbyt($d1+oc); { push/pop de }
      rghl:   outbyt($e1+oc); { push/pop hl }
      rgix:   begin outbyt($dd); outbyt($e1+oc) end; { push/pop ix }
      rgiy:   begin outbyt($fd); outbyt($e1+oc) end; { push/pop iy }
      rgihl:  begin outbyt($dd); outbyt($c1+oc) end; { push/pop (hl) }
      rgiad:  begin

                 outbyt($dd); outbyt($d1+oc); { push/pop (xx) }
                 gensym(s, 0, false, imnorm, 0, 0, 16)

              end;
      rgrad: begin

                 outbyt($dd); outbyt($f1+oc); { push/pop <xx> }
                 gensym(s, 0, false, imsgof, 0, 0, 16)

              end;
      rgipcd: begin

                 outbyt($dd); outbyt($f1+oc); { push/pop (pc+dd) }
                 gensym(s, 0, false, imnorm, 0, 0, 16)

              end;
      rgimm:  begin

                 if i <> oppush then prterr(epart); { wrong type }
                 outbyt($fd); outbyt($f5); { push nn }
                 gensym(s, 0, false, imnorm, 0, 0, 16)

              end

   end

end;

{******************************************************************************

Rotate single

Encodes the following:

     rlc   des
     rl    des
     rrc   des
     rr    des
     sla   des
     sra   des
     srl   des

Where 'des' is on of a, b, c, d, e, h, l, (ix+d), or (iy+d). A variant field is
produced for the displacement.

******************************************************************************}

procedure rots(i : opcodet { opcode to process });

var l:  regc; { register }
    oc: byte; { opcode under construction }
    s:  symptr; { displacement }

begin

   case i of { opcode }

      oprlc: oc := 0;  { rlc des }
      oprl:  oc := 16; { rl des }
      oprrc: oc := 8;  { rrc des }
      oprr:  oc := 24; { rr des }
      opsla: oc := 32; { sla des }
      opsra: oc := 40; { sra des }
      opsrl: oc := 56  { srl des }

   end;
   parcod(l, s); { parse register }
   if l = rgnl then prterr(eparexp); { missing }
   if (l = rgiixd) and not chkcst(s) then l := rgiixdb; { demote (ix+dd) }
   if (l = rgiiyd) and not chkcst(s) then l := rgiiydb; { demote (iy+dd) }
   if not (l in [rga, rgb, rgc, rgd, rge, rgh, rgl, rgihl,
                rgiix, rgiiy, rgiixdb, rgiiydb]) then
                   prterr(epart); { wrong type }
   if (l = rgiix) or (l = rgiixdb) then outbyt($dd);
   if (l = rgiiy) or (l = rgiiydb) then outbyt($fd);
   outbyt($cb);
   case l of { register }

      rga:     outbyt(oc+7); { op a }
      rgb:     outbyt(oc);   { op b }
      rgc:     outbyt(oc+1); { op c }
      rgd:     outbyt(oc+2); { op d }
      rge:     outbyt(oc+3); { op e }
      rgh:     outbyt(oc+4); { op h }
      rgl:     outbyt(oc+5); { op l }
      rgihl:   outbyt(oc+6); { op (hl) }
      rgiix:   begin outbyt($00); outbyt(oc+6) end; { op (ix+d) }
      rgiiy:   begin outbyt($00); outbyt(oc+6) end; { op (iy+d) }
      rgiixdb: begin gensym(s, 0, false, imnorm, 0, 0, 8); 
                     outbyt(oc+6) end; { op (ix+d) }
      rgiiydb: begin gensym(s, 0, false, imnorm, 0, 0, 8); 
                     outbyt(oc+6) end  { op (iy+d) }

   end

end;

{******************************************************************************

Restart

Encodes the following:

     rst   n

Where n is a value between 0 and 7. If n is not absolute, we output an
expression describing how to construct the final opcode. This is:

     opcode := (n and $38) or $c7

Note that a single undefined will be output.

******************************************************************************}

procedure rst;

var fld: symptr; { values }
    l:   regc;   { parameter }

begin

   parcod(l, fld); { parse parameter }
   if l = rgnl then prterr(eparexp); { missing }
   if l <> rgimm then prterr(epart); { wrong type }
   { check absolute }
   if not fld^.def or fld^.add or fld^.vrs then
      prterr(epmba); { must be absolute }
   { check valid value }
   if fld^.val and $c7 <> 0 then prterr(epoor);
   outbyt($c7 + fld^.val); { rst n }
   if fld^.lab = nil then putsym(fld) { free, dispose }

end;

{******************************************************************************

Return

Encodes the following:

     ret [cc]

Where 'cc' is a condition code z, nz, c, nc, po, pe, p, or m.
 
******************************************************************************}
 
procedure ret;

var lftcon: condc; { condition code }

begin

   concod(lftcon); { parse condition }
   case lftcon of { condition }

      ccnl: outbyt(201); { ret }
      ccnz: outbyt(192); { ret nz }
      ccz:  outbyt(200); { ret z }
      ccnc: outbyt(208); { ret nc }
      ccc:  outbyt(216); { ret c }
      ccpo: outbyt(224); { ret po }
      ccpe: outbyt(232); { ret pe }
      ccp:  outbyt(240); { ret p }
      ccm:  outbyt(248) { ret m }

   end

end;

{******************************************************************************

Set interrupt mode

Encodes the following:

     im      n

Where n is 0, 1, 2 or 3. N must be absolute (defined before the statement). No
variant fields are produced.

******************************************************************************}

procedure intm;

var s: symptr; { mode number }
    l: regc; { parameter }

begin

   parcod(l, s); { parse parameter }
   if l = rgnl then prterr(eparexp); { missing }
   if l <> rgimm then prterr(epart); { wrong type }
   { check absolute }
   if not s^.def or s^.add or s^.vrs then prterr(epmba);
   outbyt($ed); { im n }
   if s^.val = 0 then outbyt($46)      { im 0 }
   else if s^.val = 1 then outbyt($56) { im 1 }
   else if s^.val = 2 then outbyt($5e) { im 2 }
   else if s^.val = 3 then outbyt($4e) { im 3 }
   else prterr(epoor); { wrong value }
   if s^.lab = nil then putsym(s) { free, dispose }

end;

{******************************************************************************

Jump/call

Encodes the following:

     jp/call [cc,]xx
     jp/call [cc,](wr)
     jp/call [cc,]<xx>

******************************************************************************}

procedure jpcall(i: opcodet);

var l:  condc;  { condition code }
    r:  regc;   { parameter }
    s:  symptr; { symbol }
    oc: byte;   { opcode build variable }

begin

   concod(l); { parse condition code }
   if l <> ccnl then begin { condition was present }

      skpspc; { skip spaces }
      prcnxt(',', ecmaexp); { process ',' }

   end;
   if i = opjp then oc := $c2 else oc := $c4;
   case l of { condition, set up the masks }

      ccnl: if i = opjp then oc := oc+$01  { unconditional jump }
                        else oc := oc+$09; { unconditional call }
      ccnz: oc := oc+$00; { nz }
      ccz:  oc := oc+$08; { z }
      ccnc: oc := oc+$10; { nc }
      ccc:  oc := oc+$18; { c }
      ccpo: oc := oc+$20; { po }
      ccpe: oc := oc+$28; { pe }
      ccp:  oc := oc+$30; { p }
      ccm:  oc := oc+$38  { m }

   end;
   parcod(r, s); { parse right }
   if not (r in [rgimm, rgihl, rgiix, rgiiy, rgrad, rgipcd]) then
      prterr(epart); { wrong type }
   case r of { parameter type }

      rgimm:  begin outbyt(oc); 
                    gensym(s, 0, false, imnorm, 0, 0, 16) end; { op [cc,]xx }
      rgihl:  if (l = ccnl) and (i = opjp) then outbyt($e9) { jp (hl) }
              else begin outbyt($dd); outbyt(oc) end; { op [cc,](hl) }
      rgiix:  begin if i <> opjp then prterr(epart); { wrong type }
                    outbyt($dd); outbyt($e9) end; { jp (ix) }
      rgiiy:  begin if i <> opjp then prterr(epart); { wrong type }
                    outbyt($fd); outbyt($e9) end; { jp (iy) }
      rgrad:  begin outbyt($fd); outbyt(oc);
                    gensym(s, 0, false, imsgof, 0, 0, 16) end; { op [cc,]<xx> }
      rgipcd: begin outbyt($fd); outbyt(oc);
                    gensym(s, 0, false, imnorm, 0, 0, 16) end { op [cc,](pc+xx) }

   end

end;

{******************************************************************************

Jump relative conditional

Encodes the following:

     jr    [cc,]des

Where cc is any of z, nz, c, or nc. A variant field is produced for the
destination. The jump byte is expressed by pc+2-des.

******************************************************************************}

procedure jrel;

var l: condc;  { condition }
    r: regc;   { parameter }
    s: symptr; { destination }

begin

   concod(l); { parse condition }
   if l <> ccnl then begin { jr cc,des }

      case l of { condition }

         ccpo, ccpe, ccp, ccm: prterr(eparam); { wrong cond }
         ccz:  outbyt(40); { jr z,des }
         ccnz: outbyt(32); { jr nz,des }
         ccc:  outbyt(56); { jr c,des }
         ccnc: outbyt(48) { jr nc,des }

      end;
      skpspc; { skip spaces }
      prcnxt(',', ecmaexp) { no ',' }

   end else outbyt(24); { jr des }
   parcod(r, s); { parse parameter }
   if (r = rgimm) or (r = rgrad) then gensym(s, 0, false, imsgof, 0, 0, 8) { jr x }
   else if r = rgipcd then gensym(s, 0, false, imnorm, 0, 0, 8) { jr (pc+x) }
   else prterr(epart) { wrong type }

end;

{******************************************************************************

Exchange registers

Encodes the following:

     ex      de,hl
     ex      af,af'
     ex      (sp),hl
     ex      (sp),ix
     ex      (sp),iy
     ex      h,l
     ex      ix,hl
     ex      iy,hl
     ex      a,r
     ex      a,(wr)
     ex      a,(addr)
     ex      a,<addr>
     etc.

******************************************************************************}

procedure exch;

var l, r: regc;   { parameters }
    s:    symptr; { symbol }

begin

   parcod(l, s); { parse parameter left }
   if l = rgnl then prterr(eparexp); { missing }
   if not (l in [rga, rgaf, rgde, rgh, rgix, rgiy, rgisp]) then
      prterr(epart); { wrong type }
   skpspc; { skip spaces }
   prcnxt(',', ecmaexp); { process ',' }
   parcod(r, s); { parse parameter right }
   case l of { left hand }

      rgde:  begin { ex de,hl }

                if r <> rghl then prterr(epart); { wrong type }
                outbyt($eb)

             end;
      rgix:  begin { ex ix,hl }

                if r <> rghl then prterr(epart); { wrong type }
                outbyt($dd); outbyt($eb)

             end;
      rgiy:  begin { ex iy,hl }

                if r <> rghl then prterr(epart); { wrong type }
                outbyt($fd); outbyt($eb)

             end;
      rgaf:  begin { ex af,af' }

                if r <> rgafa then prterr(epart); { wrong type }
                outbyt($08)

             end;
      rgh:   begin { ex h,l }

                if r <> rgl then prterr(epart); { wrong type }
                outbyt($ed); outbyt($ef)

             end;
      rgisp: begin { ex (sp),wr }

                if r = rgix then outbyt($dd)
                else if r = rgiy then outbyt($fd)
                else if r <> rghl then prterr(epart); { wrong type }
                outbyt($e3)

             end;
      rga:  begin { ex a,x }

         if not (r in [rga, rgb, rgc, rgd, rge, rgh, rgl,
                       rgixh, rgixl, rgiyh, rgiyl, rgihl,
                       rgihld, rgiix, rgiixd, rgiiy,
                       rgiiyd, rgiixdb, rgiiydb, rgisp,
                       rgispd, rgihlix, rgihliy, rgiixiy,
                       rgiad, rgrad, rgipcd]) then
                          prterr(epart); { wrong type }
         case r of { right parameter }

            rga:     begin outbyt($ed); outbyt($3f) end; { ex a,a }
            rgb:     begin outbyt($ed); outbyt($07) end; { ex a,b }
            rgc:     begin outbyt($ed); outbyt($0f) end; { ex a,c }
            rgd:     begin outbyt($ed); outbyt($17) end; { ex a,d }
            rge:     begin outbyt($ed); outbyt($1f) end; { ex a,e }
            rgh:     begin outbyt($ed); outbyt($27) end; { ex a,h }
            rgl:     begin outbyt($ed); outbyt($2f) end; { ex a,l }
            rgixh:   begin outbyt($dd); outbyt($ed);
                           outbyt($27) end; { ex a,ixh }
            rgixl:   begin outbyt($dd); outbyt($ed);
                           outbyt($2f) end; { ex a,ixl }
            rgiyh:   begin outbyt($fd); outbyt($ed);
                           outbyt($27) end; { ex a,iyh }
            rgiyl:   begin outbyt($fd); outbyt($ed);
                           outbyt($2f) end; { ex a,iyl }
            rgihl:   begin outbyt($ed); outbyt($37) end; { ex a,(hl) }
            rgihld:  begin outbyt($fd); outbyt($ed); { ex a,(hl+dd) }
                           outbyt($1f); gensym(s, 0, false, imnorm, 0, 0, 16) end;
            rgiix:   begin outbyt($dd); outbyt($ed); { ex a,(ix+d) }
                           outbyt($37); outbyt($00) end;
            rgiiy:   begin outbyt($fd); outbyt($ed); { ex a,(iy+d) }
                           outbyt($37); outbyt($00) end;
            rgiixd:  begin outbyt($fd); outbyt($ed); { ex a,(ix+dd) }
                           outbyt($0f); gensym(s, 0, false, imnorm, 0, 0, 16) end;
            rgiiyd:  begin outbyt($fd); outbyt($ed); { ex a,(iy+dd) }
                           outbyt($17); gensym(s, 0, false, imnorm, 0, 0, 16) end;
            rgiixdb: begin outbyt($dd); outbyt($ed); { ex a,(ix+d) }
                           outbyt($37); gensym(s, 0, false, imnorm, 0, 0, 8) end;
            rgiiydb: begin outbyt($fd); outbyt($ed); { ex a,(iy+d) }
                           outbyt($37); gensym(s, 0, false, imnorm, 0, 0, 8) end;
            rgisp:   begin outbyt($dd); outbyt($ed); { ex a,(sp+dd) }
                           outbyt($07); outval($0000, 2, false) end;
            rgispd:  begin outbyt($dd); outbyt($ed); { ex a,(sp+dd) }
                           outbyt($07); gensym(s, 0, false, imnorm, 0, 0, 16) end;
            rgiad:   begin outbyt($dd); outbyt($ed); { ex a,(addr) }
                           outbyt($3f); gensym(s, 0, false, imnorm, 0, 0, 16) end;
            rgrad:   begin outbyt($fd); outbyt($ed); { ex a,<addr> }
                           outbyt($07); gensym(s, 0, false, imsgof, 0, 0, 16) end;
            rgipcd:  begin outbyt($fd); outbyt($ed); { ex a,(pc+dd) }
                           outbyt($07); gensym(s, 0, false, imnorm, 0, 0, 16) end;
            rgihlix: begin outbyt($dd); outbyt($ed); { ex a,(hl+ix) }
                           outbyt($0f) end;
            rgihliy: begin outbyt($dd); outbyt($ed); { ex a,(hl+iy) }
                           outbyt($17) end;
            rgiixiy: begin outbyt($dd); outbyt($ed); { ex a,(ix+iy) }
                           outbyt($1f) end

         end

      end

   end

end;

{******************************************************************************

Input/output port

Encodes the following:

     in    src,des
     out   des,src

Where src is any of a, b, c, d, e, h, or l. Des is (c) or immediate. Valid
combinations are determined by context.

******************************************************************************}

procedure inout(i: opcodet { opcode to parse });

var l, r, t:    regc;   { parameters }
    ls, rs, ts: symptr; { symbols }
    oc:         byte;   { opcode builder }

begin

   if i = opout then oc := 1 else oc := 0; { set type }
   parcod(l, ls); { parse left parameter }
   skpspc; { skip space }
   prcnxt(',', ecmaexp); { process ',' }
   parcod(r, rs); { parse right parameter }
   if (i = opout) or (i = opoutw) then begin { exchange operands }

      t := l;
      l := r;
      r := t;
      ts := ls;
      ls := rs;
      rs := ts

   end;
   if r = rgiad then begin { in a,(x)/out (x),a }

      if l <> rga then prterr(epart); { wrong type }
      if i = opin then outbyt($db) { in a,(x) }
      else outbyt($d3); { out (x),a }
      gensym(rs, 0, false, imnorm, 0, 0, 8)

   end else if (l = rghl) or (i = opinw) or (i = opoutw) then begin

      { inw hl,(x)/out (x),hl }
      if (l <> rghl) or (r <> rgic) then prterr(epart); { wrong type }
      outbyt($ed);
      if (i = opin) or (i = opinw) then outbyt($b7)
      else outbyt($bf)

   end else begin

      if r <> rgic then prterr(epart); { wrong type }
      if not (l in [rga, rgb, rgc, rgd, rge, rgh, rgl, rgixh, rgixl,
                    rgiyh, rgiyl, rgiad, rgihld, rgiixd, rgiiyd,
                    rgrad, rgipcd, rgispd, rgihlix, rgihliy,
                    rgihl, rgiix, rgiiy, rgisp,
                    rgiixiy]) then prterr(epart); { wrong type }
      case l of { parameter }

         rga:     begin outbyt($ed); outbyt($78+oc) end; { in/out a,(c) }
         rgb:     begin outbyt($ed); outbyt($40+oc) end; { in/out b,(c) }
         rgc:     begin outbyt($ed); outbyt($48+oc) end; { in/out c,(c) }
         rgd:     begin outbyt($ed); outbyt($50+oc) end; { in/out d,(c) }
         rge:     begin outbyt($ed); outbyt($58+oc) end; { in/out e,(c) }
         rgh:     begin outbyt($ed); outbyt($60+oc) end; { in/out h,(c) }
         rgl:     begin outbyt($ed); outbyt($68+oc) end; { in/out l,(c) }
         rgixh:   begin outbyt($dd); outbyt($ed); { in/out ixl,(c) }
                        outbyt($60+oc) end;
         rgixl:   begin outbyt($dd); outbyt($ed); { in/out ixl,(c) }
                        outbyt($68+oc) end;
         rgiyh:   begin outbyt($fd); outbyt($ed); { in/out iyh,(c) }
                        outbyt($60+oc) end;
         rgiyl:   begin outbyt($fd); outbyt($ed); { in/out iyl,(c) }
                        outbyt($68+oc) end;
         rgihl:   begin outbyt($fd); outbyt($ed); { in/out (hl),(c) }
                        outbyt($58+oc); outval($0000, 2, false) end;
         rgihld:  begin outbyt($fd); outbyt($ed); { in/out (hl+dd),(c) }
                        outbyt($58+oc); gensym(ls, 0, false, imnorm, 0, 0, 16) end;
         rgiix:   begin outbyt($fd); outbyt($ed); { in/out (ix),(c) }
                        outbyt($48+oc); outval($0000, 2, false) end;
         rgiixdb,
         rgiixd:  begin outbyt($fd); outbyt($ed); { in/out (ix+dd),(c) }
                        outbyt($48+oc); gensym(ls, 0, false, imnorm, 0, 0, 16) end;
         rgiiy:   begin outbyt($fd); outbyt($ed); { in/out (iy+dd),(c) }
                        outbyt($50+oc); outval($0000, 2, false) end;
         rgiiydb,
         rgiiyd:  begin outbyt($fd); outbyt($ed); { in/out (iy+dd),(c) }
                        outbyt($50+oc); gensym(ls, 0, false, imnorm, 0, 0, 16) end;
         rgisp:   begin outbyt($dd); outbyt($ed); { in/out (sp),(c) }
                        outbyt($40+oc); outval($0000, 2, false) end;
         rgispd:  begin outbyt($dd); outbyt($ed); { in/out (sp+dd),(c) }
                        outbyt($40+oc); gensym(ls, 0, false, imnorm, 0, 0, 16) end;
         rgihlix: begin outbyt($dd); outbyt($ed); { in/out (hl+ix),(c) }
                        outbyt($48+oc) end;
         rgihliy: begin outbyt($dd); outbyt($ed); { in/out (hl+iy),(c) }
                        outbyt($50+oc) end;
         rgiixiy: begin outbyt($dd); outbyt($ed); { in/out (ix+iy),(c) }
                        outbyt($58+oc) end;
         rgiad:   begin outbyt($dd); outbyt($ed); { in/out (addr),(c) }
                        outbyt($78+oc); gensym(ls, 0, false, imnorm, 0, 0, 16) end;
         rgrad:   begin outbyt($fd); outbyt($ed); { in/out <addr>,(c) }
                        outbyt($40+oc); gensym(ls, 0, false, imsgof, 0, 0, 16) end;
         rgipcd:  begin outbyt($fd); outbyt($ed); { in/out (pc+dd),(c) }
                        outbyt($40+oc); gensym(ls, 0, false, imnorm, 0, 0, 16) end

      end

   end

end;

{******************************************************************************

Bit operations

Encodes the following:

     bit   n,src
     set   n,des
     res   n,des

Where n is a bit number, 0 - 7. des are the following: a, b, c, d, e, h, l,
(hl), (ix+d) or (iy+d). A variant field is produced for displacements. If the
bit number is not absolute, we output an expression describing how to construct
the final opcode. Where 'op' is the extention opcode consisting of all fields
except the bit number:

     opcode := ((n and 7) * 8) or op

Note that this byte is allways at the end of the instruction, and that it will
be output completely undefined.

******************************************************************************}

procedure bit(i: opcodet { opcode to process });

var l, r:    regc;   { parameter }
    oc:      byte;   { opcode construction site }
    s, f, o: symptr; { bit number, displacement }

begin

   case i of { opcode }

      opbit: oc := 64; { bit n,src }
      opset: oc := 192; { set n,des }
      opres: oc := 128 { res n,des }

   end;
   parcod(l, f); { parse parameter }
   if l = rgnl then prterr(eparexp); { missing }
   if l <> rgimm then prterr(epart); { wrong type }
   skpspc; { skip spaces }
   prcnxt(',', ecmaexp); { no ',' }
   parcod(r, s); { parse parameter }
   if r = rgnl then prterr(eparexp); { missing }
   if (r = rgiixd) and not chkcst(s) then r := rgiixdb; { demote (ix+dd) }
   if (r = rgiiyd) and not chkcst(s) then r := rgiiydb; { demote (iy+dd) }
   if not (r in [rga, rgb, rgc, rgd, rge, rgh, rgl, rgihl,
                 rgiix, rgiixdb, rgiiy, rgiiydb]) then
                    prterr(epart); { wrong type }
   if (r = rgiixdb) or (r = rgiix) then outbyt($dd);
   if (r = rgiiydb) or (r = rgiiy) then outbyt($fd);
   outbyt($cb);
   case r of { parameter }

         rga:   oc := oc+7; { op n,a }
         rgb:   oc := oc+0; { op n,b }
         rgc:   oc := oc+1; { op n,c }
         rgd:   oc := oc+2; { op n,d }
         rge:   oc := oc+3; { op n,e }
         rgh:   oc := oc+4; { op n,h }
         rgl:   oc := oc+5; { op n,l }
         rgihl: oc := oc+6; { op n,(hl) }
         rgiix, rgiixdb, rgiiy, rgiiydb: begin

                gensym(s, 0, false, imnorm, 0, 0, 8); { op n,(xy) }
                oc := oc+6

         end

   end;
   if not f^.def or f^.add or f^.vrs then begin

      { variant bit field }
      getsym(s); { get mask entry }
      s^.def := true; { set defined }
      s^.val := $7; { set mask }
      getsym(o); { get 'and' operator entry }
      o^.opr := oand; { set 'and' }
      o^.lft := f; { set bit field }
      o^.rgt := s; { set mask }
      f := o; { set new top }
      getsym(s); { get shift value entry }
      s^.def := true; { set defined }
      s^.val := 8; { set shift left 3 bits }
      getsym(o); { get shift operator entry }
      o^.opr := omult; { shift by multiply }
      o^.lft := f; { set masked field to shift }
      o^.rgt := s; { set shift by value }
      f := o; { set new top }
      getsym(s); { get opcode entry }
      s^.def := true; { set defined }
      s^.val := oc; { set value opcode }
      getsym(o); { get combine entry }
      o^.opr := oor; { combine with 'or' }
      o^.lft := f; { set masked, shifted field }
      o^.rgt := s; { set opcode }
      gensym(o, 0, false, imnorm, 0, 0, 8) { generate opcode complex }

   end else begin { generate absolute opcode }

      if f^.val > 7 then prterr(epoor); { chk >= 0 < 7 }
      outbyt(oc+f^.val*8); { output opcode }
      if f^.lab = nil then putsym(f) { free, dispose }

   end

end;

{******************************************************************************

Extend sign

Encodes either exts a or exts hl.

******************************************************************************}

procedure exts;

var l: regc;   { parameter code }
    s: symptr; { symbol }

begin

   parcod(l, s); { parse parameter }
   outbyt($ed); { exts }
   if l = rga then outbyt($64) { exts a }
   else if l = rghl then outbyt($6c) { exts hl }
   else prterr(epart) { wrong register }

end;

{******************************************************************************

Negate sign

Encodes either neg a or neg hl.

******************************************************************************}

procedure neg;

var l: regc;   { register holder }
    s: symptr; { symbol }

begin

   parcod(l, s); { parse parameter }
   outbyt($ed);
   if l = rga then outbyt($44) { neg a }
   else if l = rghl then outbyt($4c) { neg hl }
   else prterr(epart) { wrong register }

end;

{******************************************************************************

Enable/disable interrupts

Encodes ei or di with or without an operand.

******************************************************************************}

procedure eidi(i: opcodet { opcode to process });

var s: symptr; { value temp }
    l: regc;   { parameter code }

begin

   parcod(l, s); { parse parameter }
   if l = rgnl then begin { no mask }

      if i = opei then outbyt($fb) { ei }
      else outbyt($f3) { di }

   end else begin { mask }

      if l <> rgimm then prterr(epart); { wrong type }
      outbyt($ed);
      if i = opei then outbyt($7f) { ei m }
      else outbyt($77); { di m }
      gensym(s, 0, false, imnorm, 0, 0, 8) { generate }

   end

end;

{******************************************************************************

Complement accumulator

Encodes cpl a.

******************************************************************************}

procedure cpl;

var l: regc;   { register }
    s: symptr; { symbol }

begin

   parcod(l, s); { parse parameter }
   if (l <> rga) and (l <> rgnl) then
      prterr(epart); { wrong type }
   outbyt($2f) { cpl a }

end;

{******************************************************************************

System call

Encodes the sc xx instruction.

******************************************************************************}

procedure sc;

var s: symptr; { value }
    l: regc;   { parameter code }

begin

   parcod(l, s); { parse parameter }
   if l <> rgimm then prterr(epart); { wrong type }
   outbyt($ed); { sc xx }
   outbyt($71);
   gensym(s, 0, false, imnorm, 0, 0, 16) { generate }

end;

{******************************************************************************

Load control register

Encodes the following:

     ldctl wreg,(c)
     ldctl (c),wreg
     ldctl wreg,usp
     ldctl usp,wreg

******************************************************************************}

procedure ldctl;

var l, r: regc;   { parameters }
    s:    symptr; { symbol }

begin

   parcod(l, s); { parse left parameter }
   skpspc; { skip spaces }
   prcnxt(',', ecmaexp); { no ',' }
   parcod(r, s); { parse right parameter }
   if ((l = rgic) or (l = rgusp)) and ((r <> rghl) and
      (r <> rgix) and (r <> rgiy)) then prterr(epart); { wrong type }
   if ((r = rgic) or (r = rgusp)) and ((l <> rghl) and
      (l <> rgix) and (l <> rgiy)) then prterr(epart); { wrong type }
   if (l = rgix) or (r = rgix) then outbyt($dd);
   if (l = rgiy) or (r = rgiy) then outbyt($fd);
   outbyt($ed);
   if l = rgic then outbyt($6e)      { ldctl (c),wr }
   else if r = rgic then outbyt($66) { ldctl wr,(c) }
   else if l = rgusp then outbyt($8f)  { ldctl usp,wr }
   else outbyt($87)                  { ldctl wr,usp }

end;

{******************************************************************************

Load to/from user data/program space

Encodes the following:

     ldud a,(wr)
     ldud a,(wr+d)
     ldup a,(wr)
     ldup a,(wr+d)
     ldud (wr),a
     ldud (wr+d),a
     ldup (wr),a
     ldup (wr+d),a

******************************************************************************}

procedure ldudp(i: opcodet { opcode to process });

var l, r:   regc;   { parameters }
    ls, rs: symptr; { symbols }
    oc:     byte;   { opcode builder }

begin

   if i = opldup then oc := $10 { ldup }
   else oc := 0; { ldud }
   parcod(l, ls); { parse left parameter }
   skpspc; { skip spaces }
   prcnxt(',', ecmaexp); { no ',' }
   parcod(r, rs); { parse right parameter }
   if (l = rgiixd) and not chkcst(ls) then l := rgiixdb; { demote (ix+dd) }
   if (l = rgiiyd) and not chkcst(ls) then l := rgiiydb; { demote (iy+dd) }
   if (r = rgiixd) and not chkcst(rs) then r := rgiixdb; { demote (ix+dd) }
   if (r = rgiiyd) and not chkcst(rs) then r := rgiiydb; { demote (iy+dd) }
   if l in [rgihl, rgiix, rgiixdb, rgiiy, rgiiydb] then begin

      if r <> rga then prterr(epart) { wrong type }

   end else if r in [rgihl, rgiix, rgiixdb, rgiiy, rgiiydb] then begin

      if l <> rga then prterr(epart) { wrong type }

   end else prterr(epart); { wrong type }
   if (l = rgiixdb) or (r = rgiixdb) or
      (l = rgiix) or (r = rgiix) then outbyt($dd); { ix }
   if (l = rgiiydb) or (r = rgiiydb) or
      (l = rgiiy) or (r = rgiiy) then outbyt($fd); { iy }
   outbyt($ed);
   if l = rga then outbyt($86+oc) { ldud/p a,(x) }
   else outbyt($8e+oc); { ldud/p (x),a }
   if (l = rgiix) or (r = rgiix) or
      (l = rgiiy) or (r = rgiiy) then outbyt($00)
   else if (l = rgiixdb) or (l = rgiiydb) then 
      gensym(ls, 0, false, imnorm, 0, 0, 8)
   else if (r = rgiixdb) or (r = rgiiydb) then 
      gensym(rs, 0, false, imnorm, 0, 0, 8)

end;

{******************************************************************************

Test and set

Encodes the following:

     tset r
     tset (hl)
     tset (ix+d)

******************************************************************************}

procedure tset;

var l: regc;   { parameter }
    s: symptr; { symbol }

begin

   parcod(l, s); { parse parameter }
   if l = rgnl then prterr(eparexp); { missing parameter }
   if not (l in [rga, rgb, rgc, rgd, rge, rgh, rgl,
                 rgihl, rgiix, rgiixdb, rgiiy, rgiiydb]) then
                    prterr(epart); { wrong type }
   if (l = rgiixd) and not chkcst(s) then l := rgiixdb; { demote (ix+dd) }
   if (l = rgiiyd) and not chkcst(s) then l := rgiiydb; { demote (iy+dd) }
   if (l = rgiixdb) or (l = rgiix) then outbyt($dd); { generate preambles }
   if (l = rgiiydb) or (l = rgiiy) then outbyt($fd);
   outbyt($cb);
   case l of { parameter }

      rga:     outbyt($37); { tset a }
      rgb:     outbyt($30); { tset b }
      rgc:     outbyt($31); { tset c }
      rgd:     outbyt($32); { tset d }
      rge:     outbyt($33); { tset e }
      rgh:     outbyt($34); { tset h }
      rgl:     outbyt($35); { tset l }
      rgihl:   outbyt($36); { tset (hl) }
      rgiix,
      rgiiy:   begin outbyt($00); outbyt($36) end; { tset (xy) }
      rgiixdb,
      rgiiydb: begin gensym(s, 0, false, imnorm, 0, 0, 8); 
                     outbyt($36) end { tset (xy+d) }

   end

end;

{******************************************************************************

Test input

Encodes the following:

     tsti (c)

******************************************************************************}

procedure tsti;

var l: regc;   { parameter }
    s: symptr; { symbol }

begin

   parcod(l, s); { parse parameter }
   if l = rgnl then prterr(eparexp); { missing parameter }
   if l <> rgic then prterr(epart); { wrong type }
   outbyt($ed); outbyt($70)

end;

{******************************************************************************

Jump relative unconditional

Encodes the following:

     djnz addr
     jaf  addr
     jar  addr

******************************************************************************}

procedure jruc(i: opcodet { opcode to parse });

var l: regc;   { parameter }
    s: symptr; { symbol }

begin

   parcod(l, s); { parse parameter }
   if l = rgnl then prterr(eparexp); { missing parameter }
   if i = opdjnz then outbyt($10) { djnz x }
   else if i = opjaf then begin outbyt($dd); outbyt($28) end { jaf x }
   else begin outbyt($dd); outbyt($20) end;
   if (l = rgimm) or (l = rgrad) then
      gensym(s, 0, false, imsgof, 0, 0, 8) { generate displacement }
   else if l = rgipcd then gensym(s, 0, false, imnorm, 0, 0, 8)
   else prterr(epart) { wrong type }

end;

{******************************************************************************

Divide/multiply byte

Encodes the following instructions:

     div
     divu
     mult
     multu

******************************************************************************}

procedure divmlt(i: opcodet { opcode });

var l: regc;   { parameter }
    s: symptr; { symbol }
    oc: byte;  { opcode construction byte }

begin

   case i of { opcode }

      opmult:  oc := $00; { mult }
      opmultu: oc := $01; { multu }
      opdiv:   oc := $04; { div }
      opdivu:  oc := $05  { divu }

   end;
   parcod(l, s); { parse parameter }
   skpspc; { skip spaces }
   if chkchr = ',' then begin

      getchr; { skip ',' }
      if (((i = opmult) or (i = opmultu)) and (l <> rga)) or
         (((i = opdiv) or (i = opdivu)) and (l <> rghl)) then
         prterr(epart); { wrong type }
      parcod(l, s) { parse parameter }

   end;
   if not (l in [rga, rgb, rgc, rgd, rge, rgh, rgl, rgixh, rgixl,
                 rgiyh, rgiyl, rgimm, rgiad, rgihl, rgihld, rgipcd,
                 rgiix, rgiixd, rgiixdb, rgiiy, rgiiyd, rgiiydb,
                 rgrad, rgisp, rgispd, rgihlix, rgihliy, rgiixiy]) then
                    prterr(epart); { wrong type }
   case l of { parameter }

      rga:     begin outbyt($ed); outbyt($f8+oc) end; { op[u] hl,a }
      rgb:     begin outbyt($ed); outbyt($c0+oc) end; { op[u] hl,b }
      rgc:     begin outbyt($ed); outbyt($c8+oc) end; { op[u] hl,c }
      rgd:     begin outbyt($ed); outbyt($d0+oc) end; { op[u] hl,d }
      rge:     begin outbyt($ed); outbyt($d8+oc) end; { op[u] hl,e }
      rgh:     begin outbyt($ed); outbyt($e0+oc) end; { op[u] hl,h }
      rgl:     begin outbyt($ed); outbyt($e8+oc) end; { op[u] hl,l }
      rgixh:   begin outbyt($dd); outbyt($ed); { op[u] hl,ixh }
                     outbyt($e0+oc) end;
      rgixl:   begin outbyt($dd); outbyt($ed); { op[u] hl,ixl }
                     outbyt($e8+oc) end;
      rgiyh:   begin outbyt($fd); outbyt($ed); { op[u] hl,iyh }
                     outbyt($e0+oc) end;
      rgiyl:   begin outbyt($fd); outbyt($ed); { op[u] hl,iyl }
                     outbyt($e8+oc) end;
      rgimm:   begin outbyt($fd); outbyt($ed); { op[u] hl,x }
                     outbyt($f8+oc); gensym(s, 0, false, imnorm, 0, 0, 8) end;
      rgihl:   begin outbyt($ed); outbyt($f0+oc) end; { op[u] hl,(hl) }
      rgihld:  begin outbyt($fd); outbyt($ed); { op[u] hl,(hl+dd) }
                     outbyt($d8+oc); gensym(s, 0, false, imnorm, 0, 0, 16) end;
      rgiix:   begin outbyt($dd); outbyt($ed); { op[u] hl,(ix+d) }
                     outbyt($f0+oc); outbyt($00) end;
      rgiixdb: begin outbyt($dd); outbyt($ed); { op[u] hl,(ix+d) }
                     outbyt($f0+oc); gensym(s, 0, false, imnorm, 0, 0, 8) end;
      rgiixd:  begin outbyt($fd); outbyt($ed); { op[u] hl,(ix+dd) }
                     outbyt($c8+oc); gensym(s, 0, false, imnorm, 0, 0, 16) end;
      rgiiy:   begin outbyt($fd); outbyt($ed); { op[u] hl,(iy+d) }
                     outbyt($f0+oc); outbyt($00) end;
      rgiiydb: begin outbyt($fd); outbyt($ed); { op[u] hl,(iy+d) }
                     outbyt($f0+oc); gensym(s, 0, false, imnorm, 0, 0, 8) end;
      rgiiyd:  begin outbyt($fd); outbyt($ed); { op[u] hl,(iy+dd) }
                     outbyt($d0+oc); gensym(s, 0, false, imnorm, 0, 0, 16) end;
      rgisp:   begin outbyt($dd); outbyt($ed); { op[u] hl,(sp+dd) }
                     outbyt($c0+oc); outval($0000, 2, false) end;
      rgispd:  begin outbyt($dd); outbyt($ed); { op[u] hl,(sp+dd) }
                     outbyt($c0+oc); gensym(s, 0, false, imnorm, 0, 0, 16) end;
      rgihlix: begin outbyt($dd); outbyt($ed); { op[u] hl,(hl+ix) }
                     outbyt($c8+oc) end;
      rgihliy: begin outbyt($dd); outbyt($ed); { op[u] hl,(hl+ix) }
                     outbyt($d0+oc) end;
      rgiixiy: begin outbyt($dd); outbyt($ed); { op[u] hl,(hl+ix) }
                     outbyt($d8+oc) end;
      rgiad:   begin outbyt($dd); outbyt($ed); { op[u] hl,(xx) }
                     outbyt($f8+oc); gensym(s, 0, false, imnorm, 0, 0, 16) end;
      rgrad:   begin outbyt($fd); outbyt($ed); { op[u] hl,<xx> }
                     outbyt($c0+oc); gensym(s, 0, false, imsgof, 0, 0, 16) end;
      rgipcd:  begin outbyt($fd); outbyt($ed); { op[u] hl,(pc+dd) }
                     outbyt($c0+oc); gensym(s, 0, false, imnorm, 0, 0, 16) end

   end

end;

{******************************************************************************

Word math

Encodes the following instructions:

     addw     [hl,]src
     subw     [hl,]src
     multw    [hl,]src
     multuw   [hl,]src
     divw     [dehl,]src
     divuw    [dehl,]src
     cpw      [hl,]src

******************************************************************************}

procedure mathw(i: opcodet { opcode });

var l:  regc;   { parameters }
    s:  symptr; { symbol }
    oc: byte;   { opcode construction byte }

begin

   case i of { opcode }

      { note: common denominator $02 removed }
      opaddw:   oc := $04; { addw }
      opsubw:   oc := $0c; { subw }
      opmultw:  oc := $00; { multw }
      opmultuw: oc := $01; { multuw }
      opdivw:   oc := $08; { divw }
      opdivuw:  oc := $09; { divuw }
      opcpw:    oc := $05  { cpw }

   end;
   parcod(l, s); { parse parameter }
   skpspc; { skip spaces }
   if chkchr = ',' then begin

      getchr; { skip ',' }
      if ((i = opdivw) or (i = opdivuw)) then begin

        if l <> rgdehl then prterr(epart) { wrong type }

      end else if l <> rghl then prterr(epart); { wrong type }
      parcod(l, s)

   end;
   if not (l in [rgbc, rgde, rghl, rgsp, rgix, rgiy, rgihl, rgimm, rgiad,
                 rgiix, rgiixdb, rgiixd, rgiiy, rgiiydb,
                 rgiiyd, rgrad, rgipcd]) then prterr(epart); { wrong type }
   case l of { parameter }

      rgbc:    begin outbyt($ed); outbyt($c2+oc) end; { op[u]w [de]hl,bc }
      rgde:    begin outbyt($ed); outbyt($d2+oc) end; { op[u]w [de]hl,de }
      rghl:    begin outbyt($ed); outbyt($e2+oc) end; { op[u]w [de]hl,hl }
      rgsp:    begin outbyt($ed); outbyt($f2+oc) end; { op[u]w [de]hl,sp }
      rgix:    begin outbyt($dd); outbyt($ed); { op[u]w [de]hl,ix }
                     outbyt($e2+oc) end;
      rgiy:    begin outbyt($fd); outbyt($ed); { op[u]w [de]hl,iy }
                     outbyt($e2+oc) end;
      rgimm:   begin outbyt($fd); outbyt($ed); { op[u]w [de]hl,x }
                     outbyt($f2+oc); gensym(s, 0, false, imnorm, 0, 0, 16) end;
      rgihl:   begin outbyt($dd); outbyt($ed); { op[u]w [de]hl,(hl) }
                     outbyt($c2+oc) end;
      rgiix:   begin outbyt($fd); outbyt($ed); { op[u] [de]hl,(ix+dd) }
                     outbyt($c2+oc); outval($0000, 2, false) end;
      rgiixdb,
      rgiixd:  begin outbyt($fd); outbyt($ed); { op[u] [de]hl,(ix+dd) }
                     outbyt($c2+oc); gensym(s, 0, false, imnorm, 0, 0, 16) end;
      rgiiy:   begin outbyt($fd); outbyt($ed); { op[u] [de]hl,(iy+dd) }
                     outbyt($d2+oc); outval($0000, 2, false) end;
      rgiiydb,
      rgiiyd:  begin outbyt($fd); outbyt($ed); { op[u] [de]hl,(iy+dd) }
                     outbyt($d2+oc); gensym(s, 0, false, imnorm, 0, 0, 16) end;
      rgiad:   begin outbyt($dd); outbyt($ed); { op[u] [de]hl,(xx) }
                     outbyt($d2+oc); gensym(s, 0, false, imnorm, 0, 0, 16) end;
      rgrad:   begin outbyt($dd); outbyt($ed); { op[u] [de]hl,<xx> }
                     outbyt($f2+oc); gensym(s, 0, false, imsgof, 0, 0, 16) end;
      rgipcd:  begin outbyt($dd); outbyt($ed); { op[u] [de]hl,(pc+dd) }
                     outbyt($f2+oc); gensym(s, 0, false, imnorm, 0, 0, 16) end

   end

end;

{******************************************************************************

Load immediate

******************************************************************************}

procedure loadi(i: opcodet; l: regc; ls: symptr; rs: symptr);

begin

   if not (l in [rga, rgb, rgc, rgd, rge, rgh, rgl, rgbc, rgde,
                 rghl, rgihl, rgihld, rgix, rgiy, rgsp,
                 rgixh, rgixl, rgiyh, rgiyl, rgihl,
                 rgihld, rgiix, rgiixdb, rgiixd, rgiiy, rgiiydb,
                 rgiiyd, rgisp, rgispd, rgiad, rgrad, rgipcd,
                 rgihlix, rgihliy, rgiixiy]) then
                    prterr(epart); { wrong type }
   if (i = opldw) and not (l in [rgbc, rgde, rghl, rgix, rgiy,
                                 rgsp, rgihl, rgiad, rgrad,
                                 rgipcd]) then
                                    prterr(epart); { wrong type }
   case l of { parameter }

      rga:     outbyt($3e); { ld a,x }
      rgb:     outbyt($06); { ld b,x }
      rgc:     outbyt($0e); { ld c,x }
      rgd:     outbyt($16); { ld d,x }
      rge:     outbyt($1e); { ld e,x }
      rgh:     outbyt($26); { ld h,x }
      rgl:     outbyt($2e); { ld l,x }
      rgixh:   begin outbyt($dd); outbyt($26) end; { ld ixh,x }
      rgixl:   begin outbyt($dd); outbyt($2e) end; { ld ixl,x }
      rgiyh:   begin outbyt($fd); outbyt($26) end; { ld iyh,x }
      rgiyl:   begin outbyt($fd); outbyt($2e) end; { ld iyl,x }
      rgbc:    outbyt($01); { ld bc,xx }
      rgde:    outbyt($11); { ld de,xx }
      rghl:    outbyt($21); { ld hl,xx }
      rgix:    begin outbyt($dd); outbyt($21) end; { ld ix,xx }
      rgiy:    begin outbyt($fd); outbyt($21) end; { ld hl,iy }
      rgsp:    outbyt($31); { ld sp,xx }
      rgihl:   if i = opldw then
                  begin outbyt($dd); outbyt($01) end { ld (hl),xx }
                  else outbyt($36); { ld (hl),x }
      rgihld:  begin outbyt($fd); outbyt($1e); { ld (hl+dd),x }
                     gensym(ls, 0, false, imnorm, 0, 0, 16) end;
      rgiix:   begin outbyt($dd); outbyt($36); { ld (ix),x }
                     outbyt($00) end;
      rgiixdb: begin outbyt($dd); outbyt($36); { ld (ix+d),x }
                     gensym(ls, 0, false, imnorm, 0, 0, 8) end;
      rgiixd:  begin outbyt($fd); outbyt($0e); { ld (ix+dd),x }
                     gensym(ls, 0, false, imnorm, 0, 0, 16) end;
      rgiiy:   begin outbyt($fd); outbyt($36); { ld (iy),x }
                     outbyt($00) end;
      rgiiydb: begin outbyt($fd); outbyt($36); { ld (iy+d),x }
                     gensym(ls, 0, false, imnorm, 0, 0, 8) end;
      rgiiyd:  begin outbyt($fd); outbyt($16); { ld (iy+dd),x }
                     gensym(ls, 0, false, imnorm, 0, 0, 16) end;
      rgisp:   begin outbyt($dd); outbyt($06); { ld (sp),x }
                     outval($0000, 2, false) end;
      rgispd:  begin outbyt($dd); outbyt($06); { ld (sp+dd),x }
                     gensym(ls, 0, false, imnorm, 0, 0, 16) end;
      rgiad:   begin outbyt($dd);
                  if i = opldw then outbyt($11) else outbyt($3e);
                  gensym(ls, 0, false, imnorm, 0, 0, 16)
               end;
      rgrad:   begin outbyt($fd);
                  if i = opldw then outbyt($31) else outbyt($06);
                  gensym(ls, 0, false, imsgof, 0, 0, 16)
               end;
      rgipcd:  begin outbyt($fd);
                  if i = opldw then outbyt($31) else outbyt($06);
                  gensym(ls, 0, false, imnorm, 0, 0, 16)
               end;
      rgihlix: begin outbyt($dd); outbyt($0e) end; { ld (hl+ix),x }
      rgihliy: begin outbyt($dd); outbyt($16) end; { ld (hl+iy),x }
      rgiixiy: begin outbyt($dd); outbyt($1e) end  { ld (ix+iy),x }

   end;
   if (i = opldw) or (l in [rgbc, rgde, rghl, rgix, rgiy, rgsp]) then
      gensym(rs, 0, false, imnorm, 0, 0, 16) { word immediate }
   else gensym(rs, 0, false, imnorm, 0, 0, 8) { byte immediate }

end;

{******************************************************************************

Load byte register

******************************************************************************}

procedure loadbr(l: regc; r: regc; s: symptr);

var lo, ro: byte; { opcode builders }

begin

   if (l = rgiixd) and not chkcst(s) then l := rgiixdb; { demote (ix+dd) }
   if (l = rgiiyd) and not chkcst(s) then l := rgiiydb; { demote (iy+dd) }
   if (r = rgiixd) and not chkcst(s) then r := rgiixdb; { demote (ix+dd) }
   if (r = rgiiyd) and not chkcst(s) then r := rgiiydb; { demote (iy+dd) }
   if not (l in [rga, rgb, rgc, rgd, rge, rgh, rgl, rgixh, rgixl,
                 rgiyh, rgiyl, rgihl, rgiix, rgiixdb,
                 rgiiy, rgiiydb]) then prterr(epart); { wrong type }
   if not (r in [rga, rgb, rgc, rgd, rge, rgh, rgl, rgixh, rgixl,
                 rgiyh, rgiyl, rgihl, rgiix, rgiixdb,
                 rgiiy, rgiiydb]) then prterr(epart); { wrong type }
   if (r in [rgihl, rgiix, rgiixdb, rgiiy, rgiiydb]) and
      ((l = rgixh) or (l = rgixl) or (l = rgiyh) or (l = rgiyl)) then
         prterr(epart); { wrong type }
   if ((l = rgixh) or (l = rgixl)) and
      ((r = rgiyh) or (r = rgiyl)) then prterr(epart); { wrong type }
   if ((l = rgiyh) or (l = rgiyl)) and
      ((r = rgixh) or (r = rgixl)) then prterr(epart); { wrong type }
   if ((l = rgixh) or (l = rgixl) or (l = rgiyh) or (l = rgiyl)) and
      ((r = rgh) or (r = rgl)) then prterr(epart); { wrong type }
   if ((r = rgixh) or (r = rgixl) or (r = rgiyh) or (r = rgiyl)) and
      ((l = rgh) or (l = rgl)) then prterr(epart); { wrong type }
   case l of { left register }

      rga: lo := $38; { a }
      rgb: lo := $00; { b }
      rgc: lo := $08; { c }
      rgd: lo := $10; { d }
      rge: lo := $18; { e }
      rgh, rgixh, rgiyh: lo := $20; { high }
      rgl, rgixl, rgiyl: lo := $28; { low }
      rgihl, rgiixdb, rgiix, rgiiydb, rgiiy: lo := $30 { (wr) }

   end;
   case r of { right register }

      rga: ro := $07; { a }
      rgb: ro := $00; { b }
      rgc: ro := $01; { c }
      rgd: ro := $02; { d }
      rge: ro := $03; { e }
      rgh, rgixh, rgiyh: ro := $04; { high }
      rgl, rgixl, rgiyl: ro := $05; { low }
      rgihl, rgiixdb, rgiix, rgiiydb, rgiiy: ro := $06 { (wr) }

   end;
   if (l = rgixh) or (l = rgixl) or (l = rgiixdb) or
      (r = rgixh) or (r = rgixl) or (r = rgiixdb) then
      outbyt($dd); { output ix prefix }
   if (l = rgiyh) or (l = rgiyl) or (l = rgiiydb) or
      (r = rgiyh) or (r = rgiyl) or (r = rgiiydb) then
      outbyt($fd); { output iy prefix }
   outbyt($40+lo+ro); { output final instruction }
   if (l = rgiix) or (l = rgiiy) or
      (r = rgiix) or (r = rgiiy) then
      outbyt($00); { output displacement }
   if (l = rgiixdb) or (l = rgiiydb) or
      (r = rgiixdb) or (r = rgiiydb) then gensym(s, 0, false, imnorm, 0, 0, 8)

end;

{******************************************************************************

Load word

******************************************************************************}

procedure loadwr(l: regc; r: regc; s: symptr);

var oc1, oc2, oc3: byte; { opcode builders }

begin

   if not (l in [rgbc, rgde, rghl, rgix, rgiy, rgsp,
                 rgihl, rgihld, rgiix, rgiixdb,
                 rgiixd, rgiiy, rgiiydb, rgiiyd, rgisp,
                 rgispd, rgiad, rgrad, rgipcd, rgihlix, rgihliy,
                 rgiixiy]) then prterr(epart); { wrong type }
   if not (r in [rgbc, rgde, rghl, rgix, rgiy, rgsp,
                 rgihl, rgihld, rgiix, rgiixdb,
                 rgiixd, rgiiy, rgiiydb, rgiiyd, rgisp,
                 rgispd, rgiad, rgrad, rgipcd, rgihlix, rgihliy,
                 rgiixiy]) then prterr(epart); { wrong type }
   if (l = rghl) or (l = rgix) or (l = rgiy) or
      (r = rghl) or (r = rgix) or (r = rgiy) then begin

      if (l = rgix) or (r = rgix) then outbyt($dd); { ix prefix }
      if (l = rgiy) or (r = rgiy) then outbyt($fd); { iy prefix }
      if (l = rghl) or (l = rgix) or (l = rgiy) then
         begin oc1 := $08; oc2 := $00; oc3 := $00 end
      else
         begin oc1 := $00; oc2 := $01; oc3 := $08; r := l end;
      case r of { parameter }

         rgbc, rgde, rghl, rgix, rgiy, rgsp: prterr(epart); { wrong type }
         rgihl:   begin if (l = rgix) or (l = rgiy) then begin

                           outbyt($ed); outbyt($3c+oc2); { (hl+dd) }
                           outval($0000, 2, false)

                        end else
                           outbyt($ed); outbyt($26+oc3) { (hl) }

                  end;
         rgihld:  begin outbyt($ed); outbyt($3c+oc2); { (hl+dd) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgiix:   begin if (l = rgix) or (l = rgiy) then begin

                           outbyt($ed); outbyt($2c+oc2); { (ix+dd) }
                           outval($0000, 2, false)

                        end else begin

                           outbyt($dd); outbyt($ed); { (ix+d) }
                           outbyt($26+oc3); outbyt($00)

                        end

                  end;
         rgiixdb: begin if (l = rgix) or (l = rgiy) then begin

                           outbyt($ed); outbyt($2c+oc2); { (ix+dd) }
                           gensym(s, 0, false, imnorm, 0, 0, 16)

                        end else begin

                           outbyt($dd); outbyt($ed); { (ix+d) }
                           outbyt($26+oc3); gensym(s, 0, false, imnorm, 0, 0, 8)

                        end

                  end;
         rgiixd:  begin outbyt($ed); outbyt($2c+oc2); { (ix+dd) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgiiy:   begin if (l = rgix) or (l = rgiy) then begin

                           outbyt($ed); outbyt($34+oc2); { (iy+dd) }
                           outval($0000, 2, false)

                        end else begin

                           outbyt($fd); outbyt($ed); { (iy+d) }
                           outbyt($26+oc3); outbyt($00)

                        end

                  end;
         rgiiydb: begin if (l = rgix) or (l = rgiy) then begin

                           outbyt($ed); outbyt($34+oc2); { (iy+dd) }
                           gensym(s, 0, false, imnorm, 0, 0, 16)

                        end else begin

                           outbyt($fd); outbyt($ed); { (iy+d) }
                           outbyt($26+oc3); gensym(s, 0, false, imnorm, 0, 0, 8)

                        end

                  end;
         rgiiyd:  begin outbyt($ed); outbyt($34+oc2); { (iy+dd) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgisp:   begin outbyt($ed); outbyt($04+oc2); { (sp+dd) }
                        outval($0000, 2, false) end;
         rgispd:  begin outbyt($ed); outbyt($04+oc2); { (sp+dd) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgiad:   begin outbyt($22+oc1); 
                        gensym(s, 0, false, imnorm, 0, 0, 16) end; { (xx) }
         rgrad:   begin outbyt($ed); outbyt($24+oc2); { <xx> }
                        gensym(s, 0, false, imsgof, 0, 0, 16) end;
         rgipcd:  begin outbyt($ed); outbyt($24+oc2); { (pc+xx) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgihlix: begin outbyt($ed); outbyt($0c+oc2) end; { (hl+ix) }
         rgihliy: begin outbyt($ed); outbyt($14+oc2) end; { (hl+ix) }
         rgiixiy: begin outbyt($ed); outbyt($1c+oc2) end  { (hl+ix) }

      end

   end else begin

      if (l = rgiixd) and not chkcst(s) then l := rgiixdb; { demote (ix+dd) }
      if (l = rgiiyd) and not chkcst(s) then l := rgiiydb; { demote (iy+dd) }
      if (r = rgiixd) and not chkcst(s) then r := rgiixdb; { demote (ix+dd) }
      if (r = rgiiyd) and not chkcst(s) then r := rgiiydb; { demote (iy+dd) }
      if l = rgbc then
         begin oc1 := $00; oc2 := $08 end
      else if l = rgde then
         begin oc1 := $10; oc2 := $18 end
      else if r = rgbc then
         begin oc1 := $08; oc2 := $00; r := l end
      else if r = rgde then
         begin oc1 := $18; oc2 := $10; r := l end
      else prterr(epart); { wrong type }
      if not (r in [rgihl, rgiad, rgiix, rgiixdb, rgiiydb, rgiiy]) then
         prterr(epart); { wrong type }
      case r of { parameter }

         rgihl:   begin outbyt($ed); outbyt($06+oc1) end; { (hl) }
         rgiix:   begin outbyt($dd); outbyt($ed); { (ix) }
                        outbyt($06+oc1); outbyt($00) end;
         rgiixdb: begin outbyt($dd); outbyt($ed); { (ix+d) }
                        outbyt($06+oc1); gensym(s, 0, false, imnorm, 0, 0, 8) end;
         rgiiy:   begin outbyt($fd); outbyt($ed); { (ix+d) }
                        outbyt($06+oc1); outbyt($00) end;
         rgiiydb: begin outbyt($fd); outbyt($ed); { (ix+d) }
                        outbyt($06+oc1); gensym(s, 0, false, imnorm, 0, 0, 8) end;
         rgiad:   begin outbyt($ed); outbyt($43+oc2); { (xx) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end

      end

   end

end;

{******************************************************************************

Load sp

******************************************************************************}

procedure loadsp(l: regc; r: regc; s: symptr);

begin

   if (l = rgsp) and (r = rghl) then outbyt($f9)
   else if (l = rgsp) and (r = rgix) then
      begin outbyt($dd); outbyt($f9) end
   else if (l = rgsp) and (r = rgiy) then
      begin outbyt($fd); outbyt($f9) end
   else if l = rgsp then begin

      if (r = rgiixd) and not chkcst(s) then r := rgiixdb; { demote (ix+dd) }
      if (r = rgiiyd) and not chkcst(s) then r := rgiiydb; { demote (iy+dd) }
      if not (r in [rgihl, rgiix, rgiixdb, rgiiy, rgiiydb, rgiad]) then
         prterr(epart); { wrong type }
      case r of { parameter }

         rgihl:   begin outbyt($ed); outbyt($36) end; { ld sp,(hl) }
         rgiix:   begin outbyt($dd); outbyt($ed); { ld sp,(ix+d) }
                        outbyt($36); outbyt($00) end;
         rgiixdb: begin outbyt($dd); outbyt($ed); { ld sp,(ix+d) }
                        outbyt($36); gensym(s, 0, false, imnorm, 0, 0, 8) end;
         rgiiy:   begin outbyt($fd); outbyt($ed); { ld sp,(iy+d) }
                        outbyt($36); outbyt($00) end;
         rgiiydb: begin outbyt($fd); outbyt($ed); { ld sp,(iy+d) }
                        outbyt($36); gensym(s, 0, false, imnorm, 0, 0, 8) end;
         rgiad:   begin outbyt($ed); outbyt($7b); { ld sp,(xx) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end

      end

   end else begin

      if (l = rgiixd) and not chkcst(s) then l := rgiixdb; { demote (ix+dd) }
      if (l = rgiiyd) and not chkcst(s) then l := rgiiydb; { demote (iy+dd) }
      if not (l in [rgihl, rgiix, rgiixdb, rgiiy, rgiiydb, rgiad]) then
         prterr(epart); { wrong type }
      case l of { parameter }

         rgihl:   begin outbyt($ed); outbyt($3e) end; { ld (hl),sp }
         rgiix:   begin outbyt($dd); outbyt($ed); { ld (ix+d),sp }
                        outbyt($3e); outbyt($00) end;
         rgiixdb: begin outbyt($dd); outbyt($ed); { ld (ix+d),sp }
                        outbyt($3e); gensym(s, 0, false, imnorm, 0, 0, 8) end;
         rgiiy:   begin outbyt($fd); outbyt($ed); { ld (iy+d),sp }
                        outbyt($3e); outbyt($00) end;
         rgiiydb: begin outbyt($fd); outbyt($ed); { ld (iy+d),sp }
                        outbyt($3e); gensym(s, 0, false, imnorm, 0, 0, 8) end;
         rgiad:   begin outbyt($ed); outbyt($73); { ld (xx),sp }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end

      end

   end

end;

{******************************************************************************

Load address

******************************************************************************}

procedure loadad;

var l, r: regc;   { parameters }
    s:    symptr; { symbol }

begin

   parcod(l, s); { parse left parameter }
   skpspc; { skip spaces }
   prcnxt(',', ecmaexp); { process ',' }
   parcod(r, s); { parse right parameter }
   if (l <> rghl) and (l <> rgix) and (l <> rgiy) then
      prterr(epart); { wrong type }
   if not (r in [rgiad, rgihl, rgihld, rgiix, rgiixdb, rgiixd,
                 rgiiy, rgiiydb, rgiiyd, rgrad, rgipcd, rgisp,
                 rgispd, rgihlix, rgihliy, rgiixiy]) then
                    prterr(epart); { wrong type }
   if l = rgix then outbyt($dd); { ix }
   if l = rgiy then outbyt($fd); { iy }
   if r <> rgiad then outbyt($ed);
   case r of { parameter }

      rgihl:   begin outbyt($3a); outval($0000, 2, false) end;  { lda wr,(hl) }
      rgihld:  begin outbyt($3a); 
                     gensym(s, 0, false, imnorm, 0, 0, 16) end; { lda wr,(hl+dd) }
      rgiix:   begin outbyt($2a); outval($0000, 2, false) end;  { lda wr,(ix) }
      rgiixdb,
      rgiixd:  begin outbyt($2a); 
                     gensym(s, 0, false, imnorm, 0, 0, 16) end; { lda wr,(ix+dd) }
      rgiiy:   begin outbyt($32); outval($0000, 2, false) end;  { lda wr,(iy+dd) }
      rgiiydb,
      rgiiyd:  begin outbyt($32); 
                     gensym(s, 0, false, imnorm, 0, 0, 16) end; { lda wr,(iy+dd) }
      rgisp:   begin outbyt($02); outval($0000, 2, false) end;  { lda wr,(sp+dd) }
      rgispd:  begin outbyt($02); 
                     gensym(s, 0, false, imnorm, 0, 0, 16) end; { lda wr,(sp+dd) }
      rgihlix: outbyt($0a); { ld wr,(hl+ix) }
      rgihliy: outbyt($12); { ld wr,(hl+iy) }
      rgiixiy: outbyt($1a); { ld wr,(ix+iy) }
      rgiad:   begin outbyt($21); 
                     gensym(s, 0, false, imnorm, 0, 0, 16) end; { lda wr,(xx) }
      rgrad:   begin outbyt($22); 
                     gensym(s, 0, false, imsgof, 0, 0, 16) end; { lda wr,<xx> }
      rgipcd:  begin outbyt($22); 
                     gensym(s, 0, false, imnorm, 0, 0, 16) end  { lda wr,(pc+xx) }

   end

end;

{******************************************************************************

Load EPU

******************************************************************************}

procedure mepum(i: opcodet);

var l:        regc;   { parameters }
    s:        symptr; { symbols }
    oc1, oc2: byte;   { opcode builders }

begin

   if i = opepum then begin oc1 := $00; oc2 := $00 end
   else begin oc1 := $08; oc2 := $01 end;
   parcod(l, s); { parse parameter }
   outbyt($ed);
   if not (l in [rgihl, rgihld, rgiix, rgiixdb, rgiixd,
                 rgiiy, rgiiydb, rgiiyd, rgisp, rgispd, rgiad,
                 rgihlix, rgihliy, rgiixiy, rgrad, rgipcd]) then
                   prterr(epart); { wrong type }
   case l of { parameter }

      rgihl:   outbyt($a6+oc1); { (hl) }
      rgihld:  begin outbyt($bc+oc2); 
                     gensym(s, 0, false, imnorm, 0, 0, 16) end; { (hl+dd) }
      rgiix:   begin outbyt($ac+oc2); outval($0000, 2, false) end;  { (ix) }
      rgiixdb,
      rgiixd:  begin outbyt($ac+oc2); 
                     gensym(s, 0, false, imnorm, 0, 0, 16) end; { (ix+dd) }
      rgiiy:   begin outbyt($b4+oc2); outval($0000, 2, false) end;  { (iy) }
      rgiiydb,
      rgiiyd:  begin outbyt($b4+oc2); 
                     gensym(s, 0, false, imnorm, 0, 0, 16) end; { (iy+dd) }
      rgisp:   begin outbyt($84+oc2); outval($0000, 2, false) end;  { (sp) }
      rgispd:  begin outbyt($84+oc2); 
                     gensym(s, 0, false, imnorm, 0, 0, 16) end; { (sp+dd) }
      rgihlix: outbyt($8c+oc2); { (hl+ix) }
      rgihliy: outbyt($94+oc2); { (hl+iy) }
      rgiixiy: outbyt($9c+oc2); { (ix+iy) }
      rgrad:   begin outbyt($a4+oc2); 
                     gensym(s, 0, false, imsgof, 0, 0, 16) end; { <xx> }
      rgipcd:  begin outbyt($a4+oc2); 
                     gensym(s, 0, false, imnorm, 0, 0, 16) end; { (pc+xx) }
      rgiad:   begin outbyt($a7+oc1); 
                     gensym(s, 0, false, imnorm, 0, 0, 16) end  { (xx) }

   end

end;

{******************************************************************************

Load to/from accumulator

******************************************************************************}

procedure loada(l: regc; r: regc; s: symptr);

begin

   if l = rga then begin { load to accumulator }

      if not (r in [rga, rgb, rgc, rgd, rge, rgh, rgl, rgixh,
                    rgixl, rgiyh, rgiyl, rgi, rgr, rgihl,
                    rgihld, rgibc, rgide, rgiix, rgiixd,
                    rgiixdb, rgiiy, rgiiyd,
                    rgiiydb, rgisp, rgispd, rgiad, rgrad,
                    rgipcd, rgihlix,
                    rgihliy, rgiixiy]) then
                       prterr(epart); { wrong type }
      case r of { parameter }

         rga:     outbyt($7f); { ld a,a }
         rgb:     outbyt($78); { ld a,b }
         rgc:     outbyt($79); { ld a,c }
         rgd:     outbyt($7a); { ld a,d }
         rge:     outbyt($7b); { ld a,e }
         rgh:     outbyt($7c); { ld a,h }
         rgl:     outbyt($7d); { ld a,l }
         rgixh:   begin outbyt($dd); outbyt($7c) end; { ld a,ixh }
         rgixl:   begin outbyt($dd); outbyt($7d) end; { ld a,ixl }
         rgiyh:   begin outbyt($fd); outbyt($7c) end; { ld a,iyh }
         rgiyl:   begin outbyt($fd); outbyt($7d) end; { ld a,iyl }
         rgi:     begin outbyt($ed); outbyt($57) end; { ld a,i }
         rgr:     begin outbyt($ed); outbyt($5f) end; { ld a,r }
         rgibc:   outbyt($0a); { ld a,(bc) }
         rgide:   outbyt($1a); { ld a,(de) }
         rgihl:   outbyt($7e); { ld a,(hl) }
         rgihld:  begin outbyt($fd); outbyt($7b); { ld a,(hl+dd) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgiix:   begin outbyt($dd); outbyt($7e); { ld a,(ix) }
                        outbyt($00) end;
         rgiixdb: begin outbyt($dd); outbyt($7e); { ld a,(ix+d) }
                        gensym(s, 0, false, imnorm, 0, 0, 8) end;
         rgiixd:  begin outbyt($fd); outbyt($79); { ld a,(ix+dd) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgiiy:   begin outbyt($fd); outbyt($7e); { ld a,(iy) }
                        outbyt($00) end;
         rgiiydb: begin outbyt($fd); outbyt($7e); { ld a,(iy+d) }
                        gensym(s, 0, false, imnorm, 0, 0, 8) end;
         rgiiyd:  begin outbyt($fd); outbyt($7a); { ld a,(iy+dd) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgisp:   begin outbyt($dd); outbyt($78); { ld a,(sp) }
                        outval($0000, 2, false) end;
         rgispd:  begin outbyt($dd); outbyt($78); { ld a,(sp+dd) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgiad:   begin outbyt($3a); 
                        gensym(s, 0, false, imnorm, 0, 0, 16) end; { ld a,(xx) }
         rgrad:   begin outbyt($fd); outbyt($78); { ld a,<xx> }
                        gensym(s, 0, false, imsgof, 0, 0, 16) end;
         rgipcd:  begin outbyt($fd); outbyt($78); { ld a,(pc+dd) }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgihlix: begin outbyt($dd); outbyt($79) end; { ld a,(hl+ix) }
         rgihliy: begin outbyt($dd); outbyt($7a) end; { ld a,(hl+iy) }
         rgiixiy: begin outbyt($dd); outbyt($7b) end  { ld a,(ix+iy) }

      end

   end else if r = rga then begin { load from accumulator }

      if not (l in [rga, rgb, rgc, rgd, rge, rgh, rgl, rgixh,
                    rgixl, rgiyh, rgiyl, rgi, rgr, rgihl,
                    rgihld, rgibc, rgide, rgiix, rgiixd, rgiixdb,
                    rgiiy, rgiiyd, rgiiydb, rgisp, rgispd,
                    rgiad, rgrad, rgipcd, rgihlix,
                    rgihliy, rgiixiy]) then
                       prterr(epart); { wrong type }
      case l of { parameter }

         rga:     outbyt($7f); { ld a,a }
         rgb:     outbyt($47); { ld b,a }
         rgc:     outbyt($4f); { ld c,a }
         rgd:     outbyt($57); { ld d,a }
         rge:     outbyt($5f); { ld e,a }
         rgh:     outbyt($67); { ld h,a }
         rgl:     outbyt($6f); { ld l,a }
         rgixh:   begin outbyt($dd); outbyt($67) end; { ld ixh,a }
         rgixl:   begin outbyt($dd); outbyt($6f) end; { ld ixl,a }
         rgiyh:   begin outbyt($fd); outbyt($67) end; { ld iyh,a }
         rgiyl:   begin outbyt($fd); outbyt($6f) end; { ld iyl,a }
         rgi:     begin outbyt($ed); outbyt($47) end; { ld i,a }
         rgr:     begin outbyt($ed); outbyt($4f) end; { ld r,a }
         rgibc:   outbyt($02); { ld (bc),a }
         rgide:   outbyt($12); { ld (de),a }
         rgihl:   outbyt($77); { ld (hl),a }
         rgihld:  begin outbyt($ed); outbyt($3b); { ld (hl+dd),a }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgiix:   begin outbyt($dd); outbyt($77); { ld (ix),a }
                        outbyt($00) end;
         rgiixdb: begin outbyt($dd); outbyt($77); { ld (ix+d),a }
                        gensym(s, 0, false, imnorm, 0, 0, 8) end;
         rgiixd:  begin outbyt($ed); outbyt($2b); { ld (ix+dd),a }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgiiy:   begin outbyt($fd); outbyt($77); { ld (iy),a }
                        outbyt($00) end;
         rgiiydb: begin outbyt($fd); outbyt($77); { ld (iy+d),a }
                        gensym(s, 0, false, imnorm, 0, 0, 8) end;
         rgiiyd:  begin outbyt($ed); outbyt($33); { ld (iy+dd),a }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgisp:   begin outbyt($ed); outbyt($03); { ld (sp),a }
                        outval($0000, 2, false) end;
         rgispd:  begin outbyt($ed); outbyt($03); { ld (sp+dd),a }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgiad:   begin outbyt($32); 
                        gensym(s, 0, false, imnorm, 0, 0, 16) end; { ld (xx),a }
         rgrad:   begin outbyt($ed); outbyt($23); { ld <xx>,a }
                        gensym(s, 0, false, imsgof, 0, 0, 16) end;
         rgipcd:  begin outbyt($ed); outbyt($23); { ld (pc+dd),a }
                        gensym(s, 0, false, imnorm, 0, 0, 16) end;
         rgihlix: begin outbyt($ed); outbyt($0b) end; { ld (hl+ix),a }
         rgihliy: begin outbyt($ed); outbyt($13) end; { ld (hl+iy),a }
         rgiixiy: begin outbyt($ed); outbyt($1b) end  { ld (ix+iy),a }

      end

   end

end;

{******************************************************************************

Load

Dispatches various loads to the proper handlers.

******************************************************************************}

procedure load(i: opcodet);

var l, r:   regc;   { parameters }
    ls, rs: symptr; { symbols }

begin

   parcod(l, ls); { parse left parameter }
   skpspc; { skip spaces }
   prcnxt(',', ecmaexp); { process ',' }
   parcod(r, rs); { parse right parameter }
   if r = rgimm then loadi(i, l, ls, rs) { load immediate }
   else if (l = rga) or (r = rga) then begin { load to/from a }

      if i <> opld then prterr(epart); { wrong type for word op }
      if l = rga then loada(l, r, rs)
      else loada(l, r, ls)

   end else if (l in [rga, rgb, rgc, rgd, rge, rgh, rgl,
                      rgixh, rgixl, rgiyh, rgiyl]) and
               (i = opld) then loadbr(l, r, rs)
   else if (r in [rga, rgb, rgc, rgd, rge, rgh, rgl,
                  rgixh, rgixl, rgiyh, rgiyl]) and
               (i = opld) then loadbr(l, r, ls) { load byte register }
   else if l = rgsp then loadsp(l, r, rs)
   else if r = rgsp then loadsp(l, r, ls) { load sp }
   else if l in [rgbc, rgde, rghl, rgix, rgiy] then
      loadwr(l, r, rs) { load word register }
   else
      loadwr(l, r, ls) { load word register }

end;

{******************************************************************************

Process opcode

Expects the opcode in labbuf. A search is done for the opcode, and a handler is
executed for the opcode. Note that the entire process may be carried out here
(for simple instructions), and that multiple opcodes are sometimes assigned to
a single handler. In the multiple opcode case, the actuall 'code' for the
instruction is passed to the handler. For all opcodes except 'equ', 'glbl',
'extl', 'macro' and 'dv' a default line label declaration is performed.

******************************************************************************}

procedure mprcopc;

var i: opcodet; { code to execute }

begin

   i := fndres(labbuf); { get reserved code }
   if i = opnull then prterr(eopcnf); { none found }
   { check perform label equation }
   if (i <> opequ) and (i <> opglobal) and (i <> opextern) and
      (i <> opmacro) and (i <> opdefvs) then prclab;
   skpspc; { skip spaces }
   case i of { opcode }

      oppush,
      oppop:    pshpop(i);                          { push/pop ww }
      opadd,
      opadc,
      opsub,
      opsbc,
      opand,
      opxor,
      opor,
      opcp:     bmath(i);                           { op b }
      opinc,
      opdec,
      opincw,
      opdecw:   ids(i);                             { inc/dec b/w }
      opim:     intm;                               { im n }
      oprlc,
      oprl,
      oprrc,
      oprr,
      opsla,
      opsra,
      opsrl:    rots(i);                            { rot b }
      oprst:    rst;                                { rst n }
      opret:    ret;                                { ret [cc] }
      opld,
      opldw:    load(i);                            { ld des,src }
      oplda:    loadad;
      opex:     exch;                               { ex des,src }
      opbit,
      opset,
      opres:    bit(i);                             { bit n,des }
      opjp,                                         { jp [cc,]loc }
      opcall:   jpcall(i);                          { call [cc,]loc }
      opjr:     jrel;                               { jr [cc,]loc }
      opin,
      opout,
      opinw,
      opoutw:   inout(i);                           { in/out des,src }
      opexts:   exts;                               { exts b/w }
      opneg:    neg;                                { neg b/w }
      opei,
      opdi:     eidi(i);                            { ei/di x }
      opcpl:    cpl;                                { cpl [a] }
      opsc:     sc;                                 { sc xx }
      opldctl:  ldctl;                              { ldctl des,src }
      opldup,
      opldud:   ldudp(i);                           { ldup/d des,src }
      optset:   tset;                               { tset b }
      optsti:   tsti;                               { tsti b }
      opdjnz,
      opjaf,
      opjar:    jruc(i);                            { jr x }
      opdiv,
      opdivu,
      opmult,
      opmultu:  divmlt(i);                          { div/mult[u] b }
      opaddw,
      opsubw,
      opmultw,
      opmultuw,
      opdivw,
      opdivuw,
      opcpw:    mathw(i);                           { op w }
      opepum,
      opmepu:   mepum(i);                           { epum/mepu x }
      opexx:    outbyt(217);                        { exx }
      opldi:    begin outbyt(237); outbyt(160) end; { ldi }
      opldir:   begin outbyt(237); outbyt(176) end; { ldir }
      opldd:    begin outbyt(237); outbyt(168) end; { ldd }
      oplddr:   begin outbyt(237); outbyt(184) end; { lddr }
      opcpi:    begin outbyt(237); outbyt(161) end; { cpi }
      opcpir:   begin outbyt(237); outbyt(177) end; { cpir }
      opcpd:    begin outbyt(237); outbyt(169) end; { cpd }
      opcpdr:   begin outbyt(237); outbyt(185) end; { cpdr }
      opdaa:    outbyt(39);                         { daa }
      opccf:    outbyt(63);                         { ccf }
      opscf:    outbyt(55);                         { scf }
      opnop:    outbyt(0);                          { nop }
      ophalt:   outbyt(118);                        { halt }
      oprlca:   outbyt(7);                          { rlca }
      oprla:    outbyt(23);                         { rla }
      oprrca:   outbyt(15);                         { rrca }
      oprra:    outbyt(31);                         { rra }
      opreti:   begin outbyt(237); outbyt(77) end;  { reti }
      opretn:   begin outbyt(237); outbyt(69) end;  { retn }
      opretil:  begin outbyt($ed); outbyt($55) end; { retil }
      opini:    begin outbyt(237); outbyt(162) end; { ini }
      opiniw:   begin outbyt($ed); outbyt($82) end; { iniw }
      opinir:   begin outbyt(237); outbyt(178) end; { inir }
      opinirw:  begin outbyt($ed); outbyt($92) end; { inirw }
      opind:    begin outbyt(237); outbyt(170) end; { ind }
      opindw:   begin outbyt($ed); outbyt($8a) end; { indw }
      opindr:   begin outbyt(237); outbyt(186) end; { indr }
      opindrw:  begin outbyt($ed); outbyt($9a) end; { indrw }
      opouti:   begin outbyt(237); outbyt(163) end; { outi }
      opoutiw:  begin outbyt($ed); outbyt($83) end; { outiw }
      opotir:   begin outbyt(237); outbyt(179) end; { otir }
      opotirw:  begin outbyt($ed); outbyt($93) end; { otirw }
      opoutd:   begin outbyt(237); outbyt(171) end; { outd }
      opoutdw:  begin outbyt($ed); outbyt($8b) end; { outdw }
      opotdr:   begin outbyt(237); outbyt(187) end; { otdr }
      opotdrw:  begin outbyt($ed); outbyt($9b) end; { otdrw }
      oprld:    begin outbyt(237); outbyt(111) end; { rld }
      oprrd:    begin outbyt(237); outbyt(103) end; { rrd }
      oppcache: begin outbyt($ed); outbyt($65) end; { pcache }
      opepuf:   begin outbyt($ed); outbyt($97) end; { epuf }
      opepui:   begin outbyt($ed); outbyt($9f) end; { epui }

      { assembler pseudo operations }

      opmacro:   macro;   { lab: macro x }
      opendmac:  endmac;  { endmac }
      opinclude: include; { include file }
      opequ:     equlab;  { lab: equ n }
      opglobal:  gbllab;  { lab: global }
      opextern:  extlab;  { lab: extern }
      opalignp:  alignp;  { align program }
      opalignv:  alignv;  { align variable }
      opif:      iftr;    { if n }
      opelse:    elsec;   { else }
      opelseif:  elseif;  { elseif }
      opendif:   endif;   { endif }
      opassm:    assm;    { assm string }
      opbendian: prterr(encend); { Z280 is not endian configurable }
      oplendian: prterr(encend); { Z280 is not endian configurable }
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

   end

end;

begin
end.

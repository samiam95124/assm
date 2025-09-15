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

{*******************************************************************************

Increment/decrement single register

*******************************************************************************}

procedure idsr(i: opcodet);

var l: regc; { parameter }
    s: symptr; { symbol }

begin

   parcod(l, s); { parse parameter }
   if i = opinr then outbyt($04+sreg(l)*8) { inr r }
   else outbyt($05+sreg(l)*8) { dcr r }

end;

{*******************************************************************************

Move single register

*******************************************************************************}

procedure move;

var l, r: regc; { parameter }
    s: symptr; { symbol }

begin

   parcod(l, s); { parse left parameter }
   skpspc; { skip spaces }
   prcnxt(',', ecmaexp); { process ',' }
   parcod(r, s); { parse right parameter }
   if (l = rgm) and (r = rgm) then prterr(epart); { wrong type }
   outbyt($40+sreg(l)*8+sreg(r)) { move d,s }

end;

{*******************************************************************************

Store/load via double register

*******************************************************************************}

procedure sldx(i: opcodet);

var l: regc; { parameter }
    s: symptr; { symbol }

begin

   parcod(l, s);
   if (l <> rgb) and (l <> rgd) then prterr(epart); { wrong type }
   if i = opstax then begin { stax }

      if l = rgb then outbyt($02) { stax b }
      else outbyt($12) { stax d }

   end else begin { ldax }

      if l = rgb then outbyt($0a) { ldax b }
      else outbyt($1a) { ldax d }

   end

end;

{*******************************************************************************

Register or memory to accumulator

*******************************************************************************}

procedure matha(i: opcodet);

var l: regc; { parameter }
    s: symptr; { symbol }
    oc: byte; { opcode build byte }

begin

   case i of { instruction }

      opadd: oc := $80; { add }
      opadc: oc := $88; { adc }
      opsub: oc := $90; { sub }
      opsbb: oc := $98; { sbb }
      opana: oc := $a0; { ana }
      opxra: oc := $a8; { xra }
      opora: oc := $b0; { ora }
      opcmp: oc := $b8  { cmp }

   end;
   parcod(l, s); { parse parameter }
   outbyt(oc+sreg(l)) { mov d,s }

end;

{*******************************************************************************

Push/pop

*******************************************************************************}

procedure puspop(i: opcodet);

var l: regc; { parameter }
    s: symptr; { symbol }
    oc: byte; { opcode build byte }

begin

   if i = oppush then oc := $c5 { push }
   else oc := $c1; { pop }
   parcod(l, s); { parse parameter }
   if not (l in [rgb, rgd, rgh, rgpsw]) then
      prterr(epart); { wrong type }
   case l of { register }

      rgb: outbyt(oc);      { push/pop b }
      rgd: outbyt(oc+$10);  { push/pop d }
      rgh: outbyt(oc+$20);  { push/pop h }
      rgpsw: outbyt(oc+$30) { push/pop psw }

   end

end;

{*******************************************************************************

Math double

*******************************************************************************}

procedure mathd(i: opcodet);

var l: regc; { parameter }
    s: symptr; { symbol }
    oc: byte; { opcode build byte }

begin

   case i of { instruction }

      opdad: oc := $09; { dad }
      opinx: oc := $03; { inx }
      opdcx: oc := $0b  { dcx }

   end;
   parcod(l, s); { parse parameter }
   if not (l in [rgb, rgd, rgh, rgsp]) then
      prterr(epart); { wrong type }
   case l of { register }

      rgb: outbyt(oc);     { op b }
      rgd: outbyt(oc+$10); { op d }
      rgh: outbyt(oc+$20); { op h }
      rgsp: outbyt(oc+$30) { op sp }

   end

end;

{*******************************************************************************

Math immediate

*******************************************************************************}

procedure mathi(i: opcodet);

var l: regc; { parameter }
    s: symptr; { symbol }

begin

   case i of { instruction }

      opadi: outbyt($c6); { adi }
      opaci: outbyt($ce); { aci }
      opsui: outbyt($d6); { sui }
      opsbi: outbyt($de); { sbi }
      opani: outbyt($e6); { ani }
      opxri: outbyt($ee); { xri }
      opori: outbyt($f6); { ori }
      opcpi: outbyt($fe)  { cpi }

   end;
   parcod(l, s); { parse parameter }
   if l <> rgimm then prterr(epart); { wrong type }
   gensym(s, 0, false, imnorm, 0, 0, 8) { output immediate }

end;

{*******************************************************************************

Load double register immediate

*******************************************************************************}

procedure lxi;

var l, r: regc; { parameter }
    s: symptr; { symbol }

begin

   parcod(l, s); { parse left parameter }
   if not (l in [rgb, rgd, rgh, rgsp]) then
      prterr(epart); { wrong type }
   skpspc; { skip spaces }
   prcnxt(',', ecmaexp); { process ',' }
   parcod(r, s); { parse right parameter }
   if r <> rgimm then prterr(epart); { wrong type }
   case l of { register }

      rgb:  outbyt($01); { lxi b,xx }
      rgd:  outbyt($11); { lxi d,xx }
      rgh:  outbyt($21); { lxi h,xx }
      rgsp: outbyt($31) { lxi sp,xx }

   end;
   gensym(s, 0, false, imnorm, 0, 0, 16) { output immediate }

end;

{*******************************************************************************

Load register immediate

*******************************************************************************}

procedure mvi;

var l, r: regc; { parameter }
    s: symptr; { symbol }

begin

   parcod(l, s); { parse left parameter }
   if not (l in [rga, rgb, rgc, rgd, rge, rgh, rgl, rgm]) then
      prterr(epart); { wrong type }
   skpspc; { skip spaces }
   prcnxt(',', ecmaexp); { process ',' }
   parcod(r, s); { parse right parameter }
   if r <> rgimm then prterr(epart); { wrong type }
   outbyt($06+sreg(l)*8); { output instruction }
   gensym(s, 0, false, imnorm, 0, 0, 8) { output immediate }

end;

{*******************************************************************************

Store/load a direct

*******************************************************************************}

procedure sla(i: opcodet);

var l: regc; { parameter }
    s: symptr; { symbol }

begin

   if i = opsta then outbyt($32) { sta xx }
   else outbyt($3a); { lda xx }
   parcod(l, s); { parse parameter }
   if l <> rgimm then prterr(epart); { wrong type }
   gensym(s, 0, false, imnorm, 0, 0, 16) { output immediate }

end;

{*******************************************************************************

Store/load hl direct

*******************************************************************************}

procedure slhld(i: opcodet);

var l: regc; { parameter }
    s: symptr; { symbol }

begin

   if i = opshld then outbyt($22) { shld xx }
   else outbyt($2a); { lhld xx }
   parcod(l, s); { parse parameter }
   if l <> rgimm then prterr(epart); { wrong type }
   gensym(s, 0, false, imnorm, 0, 0, 16) { output immediate }

end;

{*******************************************************************************

Jump/call

*******************************************************************************}

procedure jpcall(i: opcodet);

var l: regc; { parameter }
    s: symptr; { symbol }

begin

   case i of { instruction }

      opjmp:  outbyt($c3); { jp   xx }
      opjnz:  outbyt($c2); { jnz  xx }
      opjz:   outbyt($ca); { jz   xx }
      opjnc:  outbyt($d2); { jnc  xx }
      opjc:   outbyt($da); { jc   xx }
      opjpo:  outbyt($e2); { jpo  xx }
      opjpe:  outbyt($ea); { jpe  xx }
      opjp:   outbyt($f2); { jp   xx }
      opjm:   outbyt($fa); { jm   xx }
      opcall: outbyt($cd); { call xx }
      opcnz:  outbyt($c4); { cnz  xx }
      opcz:   outbyt($cc); { cz   xx }
      opcnc:  outbyt($d4); { cnc  xx }
      opcc:   outbyt($dc); { cc   xx }
      opcpo:  outbyt($e4); { cpo  xx }
      opcpe:  outbyt($ec); { cpe  xx }
      opcp:   outbyt($f4); { cp   xx }
      opcm:   outbyt($fc)  { cm   xx }

   end;
   parcod(l, s); { parse parameter }
   if l <> rgimm then prterr(epart); { wrong type }
   gensym(s, 0, false, imnorm, 0, 0, 16) { output immediate }

end;

{*******************************************************************************

Restart

*******************************************************************************}

procedure rst;

var l: regc; { parameter }
    s: symptr; { symbol }

begin

   parcod(l, s); { parse parameter }
   if l <> rgimm then prterr(epart); { wrong type }
   if not s^.def or s^.add or s^.vrs then
      prterr(epmba); { must be absolute }
   if s^.val > 7 then prterr(epoor); { out of range }
   outbyt($c7+s^.val*8) { rst n }

end;

{*******************************************************************************

In/out

*******************************************************************************}

procedure inout(i: opcodet);

var l: regc; { parameter }
    s: symptr; { symbol }

begin

   if i = opin then outbyt($db) { in x }
   else outbyt($d3); { out x }
   parcod(l, s); { parse parameter }
   if l <> rgimm then prterr(epart); { wrong type }
   gensym(s, 0, false, imnorm, 0, 0, 8) { output address }

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
      (i <> opextern) and (i <> opmacro) and (i <> opdefvs) and 
      (i <> opblock) and (i <> opendblk) then prclab;
   skpspc; { skip spaces }
   case i of { opcode }

      opnop:   outbyt($00); { nop }
      oprlc:   outbyt($07); { rlc }
      oprrc:   outbyt($0f); { rrc }
      opral:   outbyt($17); { ral }
      oprar:   outbyt($1f); { rar }
      opdaa:   outbyt($27); { daa }
      opcma:   outbyt($2f); { cma }
      opstc:   outbyt($37); { stc }
      opcmc:   outbyt($3f); { cmc }
      ophlt:   outbyt($76); { halt }
      opret:   outbyt($c9); { ret }
      oprnz:   outbyt($c0); { rnz }
      oprz:    outbyt($c8); { rz }
      oprnc:   outbyt($d0); { rnc }
      oprc:    outbyt($d8); { rc }
      oprpo:   outbyt($e0); { rpo }
      oprpe:   outbyt($e8); { rpe }
      oprp:    outbyt($f0); { rp }
      oprm:    outbyt($f8); { rm }
      opxthl:  outbyt($e3); { xthl }
      oppchl:  outbyt($e9); { pchl }
      opxchg:  outbyt($eb); { xchg }
      opsphl:  outbyt($f9); { sphl }
      opdi:    outbyt($f3); { di }
      opei:    outbyt($fb); { ei }
      opinr,
      opdcr:   idsr(i);     { inr/dcr r }
      opmov:   move;        { move d,s }
      opstax,
      opldax:  sldx(i);     { stax/ldax r }
      opadd,
      opadc,
      opsub,
      opsbb,
      opana,
      opxra,
      opora,
      opcmp:   matha(i);    { op r }
      oppush,
      oppop:   puspop(i);   { push/pop r }
      opinx,
      opdcx,
      opdad:   mathd(i);    { dad r }
      opadi,
      opaci,
      opsui,
      opsbi,
      opani,
      opxri,
      opori,
      opcpi:   mathi(i);    { op i }
      opmvi:   mvi;         { mvi r,i }
      oplxi:   lxi;         { lxi r,i }
      opsta,
      oplda:   sla(i);      { sta/lda addr }
      opshld,
      oplhld:  slhld(i);    { shld/lhld addr }
      opjmp,
      opjnz,
      opjz,
      opjnc,
      opjc,
      opjpo,
      opjpe,
      opjp,
      opjm,
      opcall,
      opcnz,
      opcz,
      opcnc,
      opcc,
      opcpo,
      opcpe,
      opcp,
      opcm:    jpcall(i);   { jmp addr }
      oprst:   rst;         { rst x }
      opin,
      opout:   inout(i);    { in/out x }

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
      opblock:   block;   { start block }
      opendblk:  endblk;  { end block }
      opprint:   asprint(false); { print user message }
      operror:   asprint(true); { print user error }
      opstop:    asstop;  { stop assembly }
      opbendian: prterr(encend); { 8080 is not endian configurable }
      oplendian: prterr(encend); { 8080 is not endian configurable }
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

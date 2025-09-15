module opcode;

uses asdef,  { generic definitions }
     common, { global variables }
     utl,    { generic utilities }
     direct, { generic assembler directives }
     main,   { error handling }
     macdef, { processor specific definitions }
     macutl; { processor specific utilities }

procedure mprcopc; forward; { process opcode }

private

{******************************************************************************

Process I-form instruction

******************************************************************************}

procedure iform(i: opcodet);

var s:  symptr; { expression result }
    ic: integer; { instruction constructor }

begin

   ic := $48000000; { set base opcode }
   if (i = opbl) or (i = opbla) then ic := ic or $1; { set link register bit }
   if (i = opba) or (i = opbla) then ic := ic or $2; { set absolute bit }
   nexpr(s); { parse target address }
   gensym(s, ic, bigend, imnsgof, -1, 2, 25) { generate instruction }

end;
   

{******************************************************************************

Process opcode

Expects the opcode in labbuf. A search is done for the opcode, and a handler is 
executed for the opcode.
Note that the entire process may be carried out here (for simple instructions), 
and that multiple opcodes are sometimes assigned to a single handler. In the 
multiple opcode case, the actuall 'code' for the instruction is passed to the 
handler.
for all opcodes except 'equ', 'glbl', 'extl', 'macro' and 'dv' a default line 
label declaration is performed.
For the PowerPC, note that the opcodes are performed in order of the 
"instruction forms" given by the Motorola Microprocessor user's manuals.

******************************************************************************}

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

      opb,
      opba,
      opbl,
      opbla:       iform(i); { b addr }

      opaddd:      dopr(i); { add x,y }

      opadd:
      opaddr:
      opaddo:
      opaddor:
      opaddc:
      opaddcr:
      opaddco:
      opaddcor:
      opadde:
      opadder:
      opaddeo:
      opaddeor:
      opaddi:
      opaddic:
      opaddicr:
      opaddis:
      opaddme:
      opaddmer:
      opaddmeo:
      opaddmeor:
      opaddze:
      opaddzer:
      opaddzeo:
      opaddzeor:
      opand:
      opandr:
      opandc:
      opandcr:
      opandir:
      opandisr:
      opbc:
      opbca:
      opbcl:
      opbcla:
      opbcctr:
      opbcctrl:
      opbclr:
      opbclrl:
      opcmp:
      opcmpi:
      opcmpl:
      opcmpli:
      opcntlzw:
      opcntlzwr:
      opcrand:
      opcrandc:
      opcreqv:
      opcrnand:
      opcrnor:
      opcror:
      opcrorc:
      opcrxor:
      opdcba:
      opdcbf:
      opdcbi:
      opdcbst:
      opdcbt:
      opdcbtst:
      opdcbz:
      opdivw:
      opdivwr:
      opdivwo:
      opdivwor:
      opdivwu:
      opdivwur:
      opdivwuo:
      opdivwuor:
      opdss:
      opdssall:
      opdst:
      opdstst:
      opdststt:
      opdstt:
      opeciwx:
      opecowx:
      opeieio:
      opeqv:
      opeqvr:
      opextsb:
      opextsbr:
      opextsh:
      opextshr:
      opfabs:
      opfabsr:
      opfadd:
      opfaddr:
      opfadds:
      opfaddsr:
      opfcmpo:
      opfcmpu:
      opfctiw:
      opfctiwr:
      opfctiwz:
      opfctiwzr:
      opfdiv:
      opfdivr:
      opfdivs:
      opfdivsr:
      opfmadd:
      opfmaddr:
      opfmadds:
      opfmaddsr:
      opfmr:
      opfmrr:
      opfmsub:
      opfmsubr:
      opfmsubs:
      opfmsubsr:
      opfmul:
      opfmulr:
      opfmuls:
      opfmulsr:
      opfnabs:
      opfnabsr:
      opfneg:
      opfnegr:
      opfnmadd:
      opfnmaddr:
      opfnmadds:
      opfnmaddsr:
      opfnmsub:
      opfnmsubr:
      opfnmsubs:
      opfnmsubsr:
      opfres:
      opfresr:
      opfrsp:
      opfrspr:
      opfrsqrte:
      opfrsqrter:
      opfsel:
      opfselr:
      opfsqrt:
      opfsqrtr:
      opfsqrts:
      opfsqrtsr:
      opfsub:
      opfsubr:
      opfsubs:
      opfsubsr:
      opicbi:
      opisync:
      oplbz:
      oplbzu:
      oplbzux:
      oplbzx:
      oplfd:
      oplfdu:
      oplfdux:
      oplfdx:
      oplfs:
      oplfsu:
      oplfsux:
      oplfsx:
      oplha:
      oplhau:
      oplhaux:
      oplhax:
      oplhbrx:
      oplhz:
      oplhzu:
      oplhzux:
      oplhzx:
      oplmw:
      oplswi:
      oplswx:
      oplvebx:
      oplvehx:
      oplvewx:
      oplvsl:
      oplvsr:
      oplvx:
      oplvxl:
      oplwarx:
      oplwbrx:
      oplwz:
      oplwzu:
      oplwzux:
      oplwzx:
      opmcrf:
      opmcrfs:
      opmcrxr:
      opmfcr:
      opmffs:
      opmffsr:
      opmfmsr:
      opmfspr:
      opmfsr:
      opmfsrin:
      opmftb:
      opmfvscr:
      opmtcrf:
      opmtfsb0:
      opmtfsb0r:
      opmtfsb1:
      opmtfsb1r:
      opmtfsf:
      opmtfsfr:
      opmtfsfi:
      opmtfsfir:
      opmtmsr:
      opmtspr:
      opmtsr:
      opmtsrin:
      opmtvscr:
      opmulhw:
      opmulhwr:
      opmulhwu:
      opmulhwur:
      opmulli:
      opmullw:
      opmullwr:
      opmullwo:
      opmullwor:
      opnand:
      opnandr:
      opneg:
      opnegr:
      opnego:
      opnegor:
      opnor:
      opnorr:
      opor:
      oporr:
      oporc:
      oporcr:
      opori:
      oporis:
      oprfi:
      oprlwimi:
      oprlwimir:
      oprlwinm:
      oprlwinmr:
      oprlwnm:
      oprlwnmr:
      opsc:
      opslw:
      opslwr:
      opsraw:
      opsrawr:
      opsrawi:
      opsrawir:
      opsrw:
      opsrwr:
      opstb:
      opstbu:
      opstbux:
      opstbx:
      opstfd:
      opstfdu:
      opstfdux:
      opstfdx:
      opstfiwx:
      opstfs:
      opstfsu:
      opstfsux:
      opstfsx:
      opsth:
      opsthbrx:
      opsthu:
      opsthux:
      opsthx:
      opstmw:
      opstswi:
      opstswx:
      opstvebx:
      opstvehx:
      opstvewx:
      opstvx:
      opstvxl:
      opstw:
      opstwbrx:
      opstwcxr:
      opstwu:
      opstwux:
      opstwx:
      opsubf:
      opsubfr:
      opsubfo:
      opsubfor:
      opsubfc:
      opsubfcr:
      opsubfco:
      opsubfcor:
      opsubfe:
      opsubfer:
      opsubfeo:
      opsubfeor:
      opsubfic:
      opsubfme:
      opsubfmer:
      opsubfmeo:
      opsubfmeor:
      opsubfze:
      opsubfzer:
      opsubfzeo:
      opsubfzeor:
      opsync:
      optlbia:
      optlbie:
      optlbsync:
      optw:
      optwi:
      opvaddcuw:
      opvaddfp:
      opvaddsbs:
      opvaddshs:
      opvaddsws:
      opvaddubm:
      opvaddubs:
      opvadduhm:
      opvadduhs:
      opvadduwm:
      opvadduws:
      opvand:
      opvandc:
      opvavgsb:
      opvavgsh:
      opvavgsw:
      opvavgub:
      opvavguh:
      opvavguw:
      opvcfsx:
      opvcfux:
      opvcmpbfp:
      opvcmpbfpr:
      opvcmpeqfp:
      opvcmpeqfpr:
      opvcmpequb:
      opvcmpequbr:
      opvcmpequh:
      opvcmpequhr:
      opvcmpequw:
      opvcmpequwr:
      opvcmpgefp:
      opvcmpgefpr:
      opvcmpgtfp:
      opvcmpgtfpr:
      opvcmpgtsb:
      opvcmpgtsbr:
      opvcmpgtsh:
      opvcmpgtshr:
      opvcmpgtsw:
      opvcmpgtswr:
      opvcmpgtub:
      opvcmpgtubr:
      opvcmpgtuh:
      opvcmpgtuhr:
      opvcmpgtuw:
      opvcmpgtuwr:
      opvctsxs:
      opvctuxs:
      opvexptefp:
      opvlogefp:
      opvmaddfp:
      opvmaxfp:
      opvmaxsb:
      opvmaxsh:
      opvmaxsw:
      opvmaxub:
      opvmaxuh:
      opvmaxuw:
      opvmhaddshs:
      opvmhraddshs:
      opvminfp:
      opvminsb:
      opvminsh:
      opvminsw:
      opvminub:
      opvminuh:
      opvminuw:
      opvmladduhm:
      opvmrghb:
      opvmrghh:
      opvmrghw:
      opvmrglb:
      opvmrglh:
      opvmrglw:
      opvmsummbm:
      opvmsumshm:
      opvmsumshs:
      opvmsumubm:
      opvmsumuhm:
      opvmsumuhs:
      opvmulesb:
      opvmulesh:
      opvmuleub:
      opvmuleuh:
      opvmulosb:
      opvmulosh:
      opvmuloub:
      opvmulouh:
      opvnmsubfp:
      opvnor:
      opvor:
      opvperm:
      opvpkpx:
      opvpkshss:
      opvpkshus:
      opvpkswss:
      opvpkswus:
      opvpkuhum:
      opvpkuhus:
      opvpkuwum:
      opvpkuwus:
      opvrefp:
      opvrfim:
      opvrfin:
      opvrfip:
      opvrfiz:
      opvrlb:
      opvrlh:
      opvrlw:
      opvrsqrtefp:
      opvsel:
      opvsl:
      opvslb:
      opvsldoi:
      opvslh:
      opvslo:
      opvslw:
      opvspltb:
      opvsplth:
      opvspltisb:
      opvspltish:
      opvspltisw:
      opvspltw:
      opvsr:
      opvsrab:
      opvsrah:
      opvsraw:
      opvsrb:
      opvsrh:
      opvsro:
      opvsrw:
      opvsubcuw:
      opvsubfp:
      opvsubsbs:
      opvsubshs:
      opvsubsws:
      opvsububm:
      opvsububs:
      opvsubuhm:
      opvsubuhs:
      opvsubuwm:
      opvsubuws:
      opvsumsws:
      opvsum2sws:
      opvsum4sbs:
      opvsum4shs:
      opvsum4ubs:
      opvupkhpx:
      opvupkhsb:
      opvupkhsh:
      opvupklpx:
      opvupklsb:
      opvupklsh:
      opvxor:
      opxor:
      opxorr:
      opxori:
      opxoris:

      { MPC7400 specific pseudo operations }

      opsmall:     large := false;   { set model to small }
      opfloat:     float := true; { set floating point enabled }
      opnfloat:    float := false; { set floating point disabled }
      opvect:      vect := true; { set vector instructions enabled }
      opnvect:     vect := false; { set vector instructions disabled }
      opm601:      cmachine := mt86; { set MPC601 }
      opm750:      cmachine := mt750; { set MPC750 }
      opm7400:     cmachine := mt7400; { set MPC7400 }
      
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
      opbendian: bigend := true; { set big endian mode }
      oplendian: bigend := false; { set little endian mode }
      opdefb:    defvall(true, false, 1); { defb b/str[,b/str]... }
      opdefps:   defps;   { defps n }
      opdefvs:   defvs;   { defvs n }
      opdefbe:   defval(true, false); { defbe l, n }
      opdefle:   defval(false, false); { defle l, n }
      opdefbef:  defval(true, true); { defbef l, n }
      opdeflef:  defval(false, true); { deflef l, n }
      opdeff:    defvall(bigend, true, 8); { deff n }
      opdefsf:   defvall(bigend, true, 4); { defsf n }
      opdeflf:   defvall(bigend, true, 10); { deflf n }
      opdefhw:   defvall(bigend, false, cpuwrdsiz div 2); { defhw n }
      opdefw:    defvall(bigend, false, cpuwrdsiz); { defw w[,w]... }
      opdefdw:   defvall(bigend, false, cpuwrdsiz*2); { defdw n }
      opdefqw:   defvall(bigend, false, cpuwrdsiz*4); { defqw n }

   end

end;
{}
begin
end.

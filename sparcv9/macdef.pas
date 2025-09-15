{******************************************************************************
*                                                                             *
*                      SPARC V9 ASSEMBLY MODULE VS. 1.0                       *
*                                                                             *
*                       Copyright (C) 2005 S. A. Moore                        *
*                           All rights reserved                               *
*                                                                             *
* Purpose:                                                                    *
*                                                                             *
* Provides SPARC V9 specific operations for AS. Contains all code that is     *
* dependent on the SPARC V9.                                                  *
*                                                                             *
******************************************************************************}

module macdef;

uses asdef; { generic definitions }

const

cpualign  = 4;    { 32 bit double word for SPARC V9 }
cpubigend = true; { SPARC V9 is big endian }
cpuwrdsiz = 4;    { SPARC V9 has 32 bit words }

type  opcodet = (opnull,opadd,opaddcc,opaddc,opaddccc,opand,opandcc,
                 opandn,opandncc,opba,opbn,opbne,opbnz,opbe,opbz,
                 opbg,opble,opbge,opbl,opbgu,opbleu,opbcc,opbgeu,
                 opbcs,opblu,opbpos,opbneg,opbvc,opbvs,opbaa,opbna,
                 opbnea,opbnza,opbea,opbza,opbga,opblea,opbgea,
                 opbla,opbgua,opbleua,opbcca,opbgeua,opbcsa,opblua,
                 opbposa,opbnega,opbvca,opbvsa,opbapt,opbnpt,opbnept,
                 opbnzpt,opbept,opbzpt,opbgpt,opblept,opbgept,opblpt,
                 opbgupt,opbleupt,opbccpt,opbgeupt,opbcspt,opblupt,
                 opbpospt,opbnegpt,opbvcpt,opbvspt,opbaapt,opbnapt,
                 opbneapt,opbnzapt,opbeapt,opbzapt,opbgapt,opbleapt,
                 opbgeapt,opblapt,opbguapt,opbleuapt,opbccapt,opbgeuapt,
                 opbcsapt,opbluapt,opbposapt,opbnegapt,opbvcapt,
                 opbvsapt,opbapn,opbnpn,opbnepn,opbnzpn,opbepn,
                 opbzpn,opbgpn,opblepn,opbgepn,opblpn,opbgupn,opbleupn,
                 opbccpn,opbgeupn,opbcspn,opblupn,opbpospn,opbnegpn,
                 opbvcpn,opbvspn,opbaapn,opbnapn,opbneapn,opbnzapn,
                 opbeapn,opbzapn,opbgapn,opbleapn,opbgeapn,opblapn,
                 opbguapn,opbleuapn,opbccapn,opbgeuapn,opbcsapn,
                 opbluapn,opbposapn,opbnegapn,opbvcapn,opbvsapn,
                 opbrz,opbrlez,opbrlz,opbrnz,opbrgz,opbrgez,opbrza,
                 opbrleza,opbrlza,opbrnza,opbrgza,opbrgeza,opbrzpt,
                 opbrlezpt,opbrlzpt,opbrnzpt,opbrgzpt,opbrgezpt,
                 opbrzapt,opbrlezapt,opbrlzapt,opbrnzapt,opbrgzapt,
                 opbrgezapt,opbrzpn,opbrlezpn,opbrlzpn,opbrnzpn,
                 opbrgzpn,opbrgezpn,opbrzapn,opbrlezapn,opbrlzapn,
                 opbrnzapn,opbrgzapn,opbrgezapn,opbtst,opbset,opbclr,
                 opbtog,opcall,opcas,opcasl,opcasx,opcasxl,opcasa,
                 opcasxa,opclr,opclrb,opclrh,opclrx,opcmp,opdec,
                 opdeccc,opdone,opfabss,opfabsd,opfabsq,opfba,opfbn,
                 opfbu,opfbg,opfbug,opfbl,opfbul,opfblg,opfbne,
                 opfbnz,opfbe,opfbz,opfbue,opfbge,opfbuge,opfble,
                 opfbule,opfbo,opfbaa,opfbna,opfbua,opfbga,opfbuga,
                 opfbla,opfbula,opfblga,opfbnea,opfbnza,opfbea,
                 opfbza,opfbuea,opfbgea,opfbugea,opfblea,opfbulea,
                 opfboa,opfbapt,opfbnpt,opfbupt,opfbgpt,opfbugpt,
                 opfblpt,opfbulpt,opfblgpt,opfbnept,opfbnzpt,opfbept,
                 opfbzpt,opfbuept,opfbgept,opfbugept,opfblept,opfbulept,
                 opfbopt,opfbaapt,opfbnapt,opfbuapt,opfbgapt,opfbugapt,
                 opfblapt,opfbulapt,opfblgapt,opfbneapt,opfbnzapt,
                 opfbeapt,opfbzapt,opfbueapt,opfbgeapt,opfbugeapt,
                 opfbleapt,opfbuleapt,opfboapt,opfbapn,opfbnpn,
                 opfbupn,opfbgpn,opfbugpn,opfblpn,opfbulpn,opfblgpn,
                 opfbnepn,opfbnzpn,opfbepn,opfbzpn,opfbuepn,opfbgepn,
                 opfbugepn,opfblepn,opfbulepn,opfbopn,opfbaapn,
                 opfbnapn,opfbuapn,opfbgapn,opfbugapn,opfblapn,
                 opfbulapn,opfblgapn,opfbneapn,opfbnzapn,opfbeapn,
                 opfbzapn,opfbueapn,opfbgeapn,opfbugeapn,opfbleapn,
                 opfbuleapn,opfboapn,opfcmps,opfcmpd,opfcmpq,opfcmpes,
                 opfcmped,opfcmpeq,opfdivs,opfdivd,opfdivq,opfdmulq,
                 opfitos,opfitod,opfitoq,opflush,opflushw,opfmovs,
                 opfmovd,opfmovq,opfmovsa,opfmovsn,opfmovsne,opfmovsnz,
                 opfmovse,opfmovsz,opfmovsg,opfmovsle,opfmovsge,
                 opfmovsl,opfmovsgu,opfmovsleu,opfmovscc,opfmovsgeu,
                 opfmovscs,opfmovslu,opfmovspos,opfmovsneg,opfmovsvc,
                 opfmovsvs,opfmovda,opfmovdn,opfmovdne,opfmovdnz,
                 opfmovde,opfmovdz,opfmovdg,opfmovdle,opfmovdge,
                 opfmovdl,opfmovdgu,opfmovdleu,opfmovdcc,opfmovdgeu,
                 opfmovdcs,opfmovdlu,opfmovdpos,opfmovdneg,opfmovdvc,
                 opfmovdvs,opfmovqa,opfmovqn,opfmovqne,opfmovqnz,
                 opfmovqe,opfmovqz,opfmovqg,opfmovqle,opfmovqge,
                 opfmovql,opfmovqgu,opfmovqleu,opfmovqcc,opfmovqgeu,
                 opfmovqcs,opfmovqlu,opfmovqpos,opfmovqneg,opfmovqvc,
                 opfmovqvs,opfmovsu,opfmovsug,opfmovsul,opfmovslg,
                 opfmovsue,opfmovsuge,opfmovsule,opfmovso,opfmovdu,
                 opfmovdug,opfmovdul,opfmovdlg,opfmovdue,opfmovduge,
                 opfmovdule,opfmovdo,opfmovqu,opfmovqug,opfmovqul,
                 opfmovqlg,opfmovque,opfmovquge,opfmovqule,opfmovqo,
                 opfmovrse,opfmovrsz,opfmovrslez,opfmovrslz,opfmovrsne,
                 opfmovrsnz,opfmovrsgz,opfmovrsgez,opfmovrde,opfmovrdz,
                 opfmovrdlez,opfmovrdlz,opfmovrdne,opfmovrdnz,opfmovrdgz,
                 opfmovrdgez,opfmovrqe,opfmovrqz,opfmovrqlez,opfmovrqlz,
                 opfmovrqne,opfmovrqnz,opfmovrqgz,opfmovrqgez,opfmuls,
                 opfmuld,opfmulq,opfnegs,opfnegd,opfnegq,opfsmuld,
                 opfsqrts,opfsqrtd,opfsqrtq,opfstoi,opfdtoi,opfqtoi,
                 opfstod,opfstoq,opfdtos,opfdtoq,opfqtos,opfqtod,
                 opfstox,opfdtox,opfqtox,opfsubs,opfsubd,opfsubq,
                 opfxtos,opfxtod,opfxtoq,opilltrap,opimpdep1,opimpdep2,
                 opinc,opinccc,opiprefetch,opjmp,opjmpl,opldd,opldda,
                 opld,oplda,opldq,opldqa,opldsb,opldsba,opldsh,
                 opldsha,opldstub,opldstuba,opldsw,opldswa,opldub,
                 oplduba,oplduh,oplduha,oplduw,oplduwa,opldx,opldxa,
                 opmembar,opmov,opmova,opmovn,opmovne,opmovnz,opmove,
                 opmovz,opmovg,opmovle,opmovge,opmovl,opmovgu,opmovleu,
                 opmovcc,opmovgeu,opmovcs,opmovlu,opmovpos,opmovneg,
                 opmovvc,opmovvs,opmovrne,opmovrnz,opmovre,opmovrz,
                 opmovrgez,opmovrlz,opmovrlez,opmovrgz,opmulscc,
                 opmulx,opneg,opnop,opnot,opor,oporcc,oporn,oporncc,
                 oppopc,opprefetch,opprefetcha,oprd,oprdpr,oprestore,
                 oprestored,opretry,opret,opretl,opreturn,opsave,
                 opsaved,opsdiv,opsdivcc,opsdivx,opsethi,opset,
                 opsetuw,opsetsw,opsetx,opsignx,opsir,opsll,opsllx,
                 opsmul,opsmulcc,opsra,opsrax,opsrl,opsrlx,opstb,
                 opstba,opstub,opstuba,opstsb,opstsba,opstbar,opstd,
                 opstda,opst,opsta,opsth,opstha,opstuh,opstuha,
                 opstsh,opstsha,opstq,opstqa,opstw,opstwa,opstuw,
                 opstuwa,opstsw,opstswa,opstx,opstxa,opsub,opsubcc,
                 opsubc,opsubccc,opswap,opswapa,optaddcc,optaddcctv,
                 opta,optn,optne,optnz,opte,optz,optg,optle,optge,
                 optl,optgu,optleu,optcc,optgeu,optcs,optlu,optpos,
                 optneg,optvc,optvs,optst,optsubcc,optsubcctv,opudiv,
                 opudivcc,opudivx,opumul,opumulcc,opwr,opwrpr,opxor,
                 opxorcc,opxnor,opxnorcc,opfloat,opnfloat,opmsv7,
                 opmsv8,opmsv9,opmacro,opendmac,opinclude,opequ,
                 opsetequ,opglobal,opextern,opalignp,opalignv,opif,
                 opelse,opelseif,opendif,opassm,opprint,operror,
                 opstop,opbendian,oplendian,opdefb,opdefps,opdefvs,
                 opdefbe,opdefle,opdefbef,opdeflef,opdeff,opdefsf,
                 opdeflf,opdefef,opdefw,opdefhw,opdefdw,opdefqw,
                 opdeffd,opdeffq);

      restab = array [opcodet] of { reserved symbols }
                  record
                     reslab: lab;   { reserved symbol }
                     reschn: opcodet { chain to next }
                  end;
      { registers in the SPARC }
      regc = (rgnl,         { none }
              { simple registers }
              rgr0,         { %r0/%g0 }
              rgr1,         { %r1/%g1 }
              rgr2,         { %r2/%g2 }
              rgr3,         { %r3/%g3 }
              rgr4,         { %r4/%g4 }
              rgr5,         { %r5/%g5 }
              rgr6,         { %r6/%g6 }
              rgr7,         { %r7/%g7 }
              rgr8,         { %r8/%o0 }
              rgr9,         { %r9/%o1 }
              rgr10,        { %r10/%o2 }
              rgr11,        { %r11/%o3 }
              rgr12,        { %r12/%o4 }
              rgr13,        { %r13/%o5 }
              rgr14,        { %r14/%o6 }
              rgr15,        { %r15/%o7 }
              rgr16,        { %r16/%l0 }
              rgr17,        { %r17/%l1 }
              rgr18,        { %r18/%l2 }
              rgr19,        { %r19/%l3 }
              rgr20,        { %r20/%l4 }
              rgr21,        { %r21/%l5 }
              rgr22,        { %r22/%l6 }
              rgr23,        { %r23/%l7 }
              rgr24,        { %r24/%i0 }
              rgr25,        { %r25/%i1 }
              rgr26,        { %r26/%i2 }
              rgr27,        { %r27/%i3 }
              rgr28,        { %r28/%i4 }
              rgr29,        { %r29/%i5 }
              rgr30,        { %r30/%i6 }
              rgr31,        { %r31/%i7 }
              rgf0,         { %f0 }
              rgf1,         { %f1 }
              rgf2,         { %f2 }
              rgf3,         { %f3 }
              rgf4,         { %f4 }
              rgf5,         { %f5 }
              rgf6,         { %f6 }
              rgf7,         { %f7 }
              rgf8,         { %f8 }
              rgf9,         { %f9 }
              rgf10,        { %f10 }
              rgf11,        { %f11 }
              rgf12,        { %f12 }
              rgf13,        { %f13 }
              rgf14,        { %f14 }
              rgf15,        { %f15 }
              rgf16,        { %f16 }
              rgf17,        { %f17 }
              rgf18,        { %f18 }
              rgf19,        { %f19 }
              rgf20,        { %f20 }
              rgf21,        { %f21 }
              rgf22,        { %f22 }
              rgf23,        { %f23 }
              rgf24,        { %f24 }
              rgf25,        { %f25 }
              rgf26,        { %f26 }
              rgf27,        { %f27 }
              rgf28,        { %f28 }
              rgf29,        { %f29 }
              rgf30,        { %f30 }
              rgf31,        { %f31 }
              rgf32,        { %f32 }
              rgf33,        { %f33 }
              rgf34,        { %f34 }
              rgf35,        { %f35 }
              rgf36,        { %f36 }
              rgf37,        { %f37 }
              rgf38,        { %f38 }
              rgf39,        { %f39 }
              rgf40,        { %f40 }
              rgf41,        { %f41 }
              rgf42,        { %f42 }
              rgf43,        { %f43 }
              rgf44,        { %f44 }
              rgf45,        { %f45 }
              rgf46,        { %f46 }
              rgf47,        { %f47 }
              rgf48,        { %f48 }
              rgf49,        { %f49 }
              rgf50,        { %f50 }
              rgf51,        { %f51 }
              rgf52,        { %f52 }
              rgf53,        { %f53 }
              rgf54,        { %f54 }
              rgf55,        { %f55 }
              rgf56,        { %f56 }
              rgf57,        { %f57 }
              rgf58,        { %f58 }
              rgf59,        { %f59 }
              rgf60,        { %f60 }
              rgf61,        { %f61 }
              rgf62,        { %f62 }
              rgf63,        { %f63 }
              rgicc,        { %icc }
              rgxcc,        { %xcc }
              rgfcc0,       { %fcc0 }
              rgfcc1,       { %fcc1 }
              rgfcc2,       { %fcc2 }
              rgfcc3,       { %fcc3 }
              rgasi,        { %asi }
              rgfsr,        { %fsr }
              rgasr16,      { %asr16 }
              rgasr17,      { %asr17 }
              rgasr18,      { %asr18 }
              rgasr19,      { %asr19 }
              rgasr20,      { %asr20 }
              rgasr21,      { %asr21 }
              rgasr22,      { %asr22 }
              rgasr23,      { %asr23 }
              rgasr24,      { %asr24 }
              rgasr25,      { %asr25 }
              rgasr26,      { %asr26 }
              rgasr27,      { %asr27 }
              rgasr28,      { %asr28 }
              rgasr29,      { %asr29 }
              rgasr30,      { %asr30 }
              rgasr31,      { %asr31 }
              rgy,          { %y }
              rgccr,        { %ccr }
              rgfprs,       { %fprs }
              rgpc,         { %pc }
              rgtpc,        { %tpc }
              rgtnpc,       { %tnpc }
              rgtstate,     { %tstate }
              rgtt,         { %tt }
              rgtick,       { %tick }
              rgtba,        { %tba }
              rgpstate,     { %pstate }
              rgtl,         { %tl }
              rgpil,        { %pil }
              rgcwp,        { %cwp }
              rgcansave,    { %cansave }
              rgcanrestore, { %canrestore }
              rgcleanwin,   { %cleanwin }
              rgotherwin,   { %otherwin }
              rgwstate,     { %wstate }
              rgfq,         { %fq }
              rgver,        { %ver }
              { these are the complex modes }
              rgimm,        { n } 
              rgir,         { [reg] }
              rgirr,        { [reg+reg] } 
              rgird,        { [reg+imm] }
              rgiri,        { [reg]n        (this is an asi mode) }
              rgirri,       { [reg+reg]n    ("    "  "  "   "   ) }
              rgida,        { [imm]%asi     ("    "  "  "   "   ) }
              rgira,        { [reg]%asi     ("    "  "  "   "   ) }
              rgirda);      { [reg+imm]%asi ("    "  "  "   "   ) }
      opsize = 0..10; { operation sizes }
      { machine types }
      mach = (mtv7,  { SPARC V7 }
              mtv8,  { SPARC V8 }
              mtv9); { SPARC V9 }
      { parameter records. these keep track of all the information needed to
        represent a parameter }
      parptr = ^parrec; { pointer to parameter record }
      parrec = record { parameter record }

                  m:   regc;   { main register or mode }
                  rs1: regc;   { first index register }
                  rs2: regc;   { second index register }
                  vl:  symptr; { displacement or immediate }

               end;

begin
end.

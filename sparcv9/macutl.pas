module macutl(output);

uses asdef,  { generic definitions }
     common, { global variables }
     utl,    { generic utilities }
     macdef; { processor specific definitions }

var float:    boolean;     { floating point instructions enabled flag }
    cmachine: mach;        { current machine type }
    opcbas:   array [opcodet] of integer; { opcode base codes }
    gprset:   set of regc; { set of general purpose registers r0-r31 }
    fprset:   set of regc; { set of floating point registers f0-r63 }

procedure regcod(var reg: regc); forward; { process register code }
procedure parcod(var p: parrec);  forward; { parse parameter }
function reg(p: regc): byte;  forward; { convert register to code }
function fndres(view s: string): opcodet; forward; { find reserved word }
procedure valmac(m: mach); forward;
procedure valmacs(m: mach); forward;
procedure valflt; forward;
procedure mexpr(var sym: symptr); forward; { parse expression }
procedure msexpr(var sym: symptr); forward; { parse simple expression }
procedure mterm(var sym: symptr); forward; { parse term }
procedure mfactor(var sym: symptr); forward; { parse factor }
function absolute(s: symptr): boolean; forward; { check symbol absolute }
{ output 32 bit word symbol }
procedure gensym32(sym: symptr; bak: integer; im: immode; len: integer); 
   forward;

private

var ressym: restab;  { reserved symbols table }
    oi:     opcodet; { index for reserved symbols table }

{******************************************************************************

Initalize reserved word table

The reserved word table is an array of records, with each record containing a 
label, and a entry chain. The label is the actual reserved word itself. The 
chain is an index into the reserved word table itself. Each position (index) in 
the reserved word table corresponds to a 'hash' value for a given label. The 
hash value is a random function derived for the label that gives some level 
(typ > 90%) of 'uniqueness' for a given label (see the 'hash' routine for 
details of the process). Each 'prime' label occupys the entry cooresponding to 
it's hash value (a prime entry means that it is the first label for the hash 
value to be placed). Duplicate entrys are placed in locations that have no 
prime label. The chain fields then point to these labels succesively from the 
prime entry, terminated with 0. A lookup is performed by succesively examining 
each label in a chain, starting with the prime label, until a match is found 
or the end of a chain is reached.

This routine sets up the labels and chain fields making up the table. The table 
should already be cleared.

*******************************************************************************}

procedure resini;

begin

   labcpy(ressym[opbpospn   ].reslab, 'bpospn    '); ressym[opbpospn   ].reschn := opfmovsg;
   labcpy(ressym[opbrzapt   ].reslab, 'brzapt    '); ressym[opbrzapt   ].reschn := opbrgzpn;
   labcpy(ressym[opfbnzpt   ].reslab, 'fbnzpt    '); ressym[opfbnzpt   ].reschn := opfsqrtd;
   labcpy(ressym[opfmovql   ].reslab, 'fmovql    ');
   labcpy(ressym[opfmovdz   ].reslab, 'fmovdz    '); ressym[opfmovdz   ].reschn := opextern;
   labcpy(ressym[opfmovsl   ].reslab, 'fmovsl    '); ressym[opfmovsl   ].reschn := opfmovqn;
   labcpy(ressym[opbpospt   ].reslab, 'bpospt    '); ressym[opbpospt   ].reschn := opbrlzpn;
   labcpy(ressym[opbrgzpt   ].reslab, 'brgzpt    '); ressym[opbrgzpt   ].reschn := opflushw;
   labcpy(ressym[opbrnzpn   ].reslab, 'brnzpn    '); ressym[opbrnzpn   ].reschn := opfmovso;
   labcpy(ressym[opble      ].reslab, 'ble       ');
   labcpy(ressym[opbea      ].reslab, 'bea       '); ressym[opbea      ].reschn := opiprefetch;
   labcpy(ressym[opbvca     ].reslab, 'bvca      '); ressym[opbvca     ].reschn := opcall;
   labcpy(ressym[opbrlzpt   ].reslab, 'brlzpt    '); ressym[opbrlzpt   ].reschn := opfmovqu;
   labcpy(ressym[opbnpt     ].reslab, 'bnpt      '); ressym[opbnpt     ].reschn := opassm;
   labcpy(ressym[opbrnzpt   ].reslab, 'brnzpt    '); ressym[opbrnzpt   ].reschn := opfmovsu;
   labcpy(ressym[opfsqrtq   ].reslab, 'fsqrtq    ');
   labcpy(ressym[opbept     ].reslab, 'bept      '); ressym[opbept     ].reschn := opldsh;
   labcpy(ressym[opfmovqz   ].reslab, 'fmovqz    '); ressym[opfmovqz   ].reschn := opfsqrts;
   labcpy(ressym[opmovpos   ].reslab, 'movpos    ');
   labcpy(ressym[opfmovsz   ].reslab, 'fmovsz    '); ressym[opfmovsz   ].reschn := opmovrgz;
   labcpy(ressym[opbnapt    ].reslab, 'bnapt     '); ressym[opbnapt    ].reschn := opfbopn;
   labcpy(ressym[opbeapt    ].reslab, 'beapt     '); ressym[opbeapt    ].reschn := opbgepn;
   labcpy(ressym[opbvcapt   ].reslab, 'bvcapt    '); ressym[opbvcapt   ].reschn := opbrleza;
   labcpy(ressym[opbapn     ].reslab, 'bapn      ');
   labcpy(ressym[opmovrlz   ].reslab, 'movrlz    ');
   labcpy(ressym[opbnepn    ].reslab, 'bnepn     '); ressym[opbnepn    ].reschn := opfbgpt;
   labcpy(ressym[opmovrnz   ].reslab, 'movrnz    ');
   labcpy(ressym[opbgpn     ].reslab, 'bgpn      '); ressym[opbgpn     ].reschn := opldub;
   labcpy(ressym[opbgepn    ].reslab, 'bgepn     '); ressym[opbgepn    ].reschn := opldsha;
   labcpy(ressym[opblpn     ].reslab, 'blpn      '); ressym[opblpn     ].reschn := opbtog;
   labcpy(ressym[opimpdep1  ].reslab, 'impdep1   ');
   labcpy(ressym[opimpdep2  ].reslab, 'impdep2   ');
   labcpy(ressym[opbgupn    ].reslab, 'bgupn     '); ressym[opbgupn    ].reschn := opfdivs;
   labcpy(ressym[opblupn    ].reslab, 'blupn     '); ressym[opblupn    ].reschn := opfbupt;
   labcpy(ressym[opbnegpn   ].reslab, 'bnegpn    '); ressym[opbnegpn   ].reschn := opbvcapn;
   labcpy(ressym[opbvcpn    ].reslab, 'bvcpn     '); ressym[opbvcpn    ].reschn := opfcmps;
   labcpy(ressym[opbeapn    ].reslab, 'beapn     '); ressym[opbeapn    ].reschn := opldsba;
   labcpy(ressym[opbgapn    ].reslab, 'bgapn     '); ressym[opbgapn    ].reschn := oplduba;
   labcpy(ressym[opbgeapn   ].reslab, 'bgeapn    ');
   labcpy(ressym[opbguapn   ].reslab, 'bguapn    '); ressym[opbguapn   ].reschn := opfblept;
   labcpy(ressym[opbccapn   ].reslab, 'bccapn    ');
   labcpy(ressym[opbvcapn   ].reslab, 'bvcapn    ');
   labcpy(ressym[opbrlez    ].reslab, 'brlez     '); ressym[opbrlez    ].reschn := opstuba;
   labcpy(ressym[opbrlz     ].reslab, 'brlz      '); ressym[opbrlz     ].reschn := opldsw;
   labcpy(ressym[opbrleza   ].reslab, 'brleza    '); ressym[opbrleza   ].reschn := opfbuepn;
   labcpy(ressym[opbrlza    ].reslab, 'brlza     '); ressym[opbrlza    ].reschn := opcasxl;
   labcpy(ressym[opbrgza    ].reslab, 'brgza     '); ressym[opbrgza    ].reschn := opfitod;
   labcpy(ressym[opbrlzpn   ].reslab, 'brlzpn    '); ressym[opbrlzpn   ].reschn := opfmovqo;
   labcpy(ressym[opbrgzpn   ].reslab, 'brgzpn    '); ressym[opbrgzpn   ].reschn := opmovgeu;
   labcpy(ressym[opbrlzapn  ].reslab, 'brlzapn   ');
   labcpy(ressym[opbrgezapn ].reslab, 'brgezapn  ');
   labcpy(ressym[opbset     ].reslab, 'bset      '); ressym[opbset     ].reschn := optneg;
   labcpy(ressym[opbclr     ].reslab, 'bclr      '); ressym[opbclr     ].reschn := opcasl;
   labcpy(ressym[opbtog     ].reslab, 'btog      '); ressym[opbtog     ].reschn := opstda;
   labcpy(ressym[opcall     ].reslab, 'call      ');
   labcpy(ressym[opcasl     ].reslab, 'casl      '); ressym[opcasl     ].reschn := opclrb;
   labcpy(ressym[opcasx     ].reslab, 'casx      '); ressym[opcasx     ].reschn := opsave;
   labcpy(ressym[opcasxl    ].reslab, 'casxl     '); ressym[opcasxl    ].reschn := opfbopt;
   labcpy(ressym[opclrb     ].reslab, 'clrb      '); ressym[opclrb     ].reschn := opfbza;
   labcpy(ressym[opfabss    ].reslab, 'fabss     ');
   labcpy(ressym[opfabsq    ].reslab, 'fabsq     '); ressym[opfabsq    ].reschn := opfbapt;
   labcpy(ressym[opfba      ].reslab, 'fba       ');
   labcpy(ressym[opfbu      ].reslab, 'fbu       ');
   labcpy(ressym[opbendian  ].reslab, 'bendian   ');
   labcpy(ressym[opfbg      ].reslab, 'fbg       ');
   labcpy(ressym[opfbgeapn  ].reslab, 'fbgeapn   ');
   labcpy(ressym[opfbug     ].reslab, 'fbug      ');
   labcpy(ressym[opfbul     ].reslab, 'fbul      '); ressym[opfbul     ].reschn := opldxa;
   labcpy(ressym[opfbne     ].reslab, 'fbne      ');
   labcpy(ressym[opfbge     ].reslab, 'fbge      ');
   labcpy(ressym[opfbleapn  ].reslab, 'fbleapn   ');
   labcpy(ressym[opfbgeapt  ].reslab, 'fbgeapt   ');
   labcpy(ressym[opfblgapn  ].reslab, 'fblgapn   '); ressym[opfblgapn  ].reschn := opfbneapn;
   labcpy(ressym[opbnegapn  ].reslab, 'bnegapn   '); ressym[opbnegapn  ].reschn := oplendian;
   labcpy(ressym[opfbuge    ].reslab, 'fbuge     ');
   labcpy(ressym[opfble     ].reslab, 'fble      ');
   labcpy(ressym[opfbleapt  ].reslab, 'fbleapt   ');
   labcpy(ressym[opfbule    ].reslab, 'fbule     '); ressym[opfbule    ].reschn := oplduha;
   labcpy(ressym[opfblgapt  ].reslab, 'fblgapt   '); ressym[opfblgapt  ].reschn := opfbneapt;
   labcpy(ressym[opbnegapt  ].reslab, 'bnegapt   '); ressym[opbnegapt  ].reschn := opfbueapn;
   labcpy(ressym[opbgeuapn  ].reslab, 'bgeuapn   '); ressym[opbgeuapn  ].reschn := opfmovdcc;
   labcpy(ressym[opfbugapn  ].reslab, 'fbugapn   ');
   labcpy(ressym[opinclude  ].reslab, 'include   ');
   labcpy(ressym[opfbo      ].reslab, 'fbo       ');
   labcpy(ressym[opfbnea    ].reslab, 'fbnea     ');
   labcpy(ressym[opbleuapn  ].reslab, 'bleuapn   '); ressym[opbleuapn  ].reschn := opfbueapt;
   labcpy(ressym[opbgeuapt  ].reslab, 'bgeuapt   '); ressym[opbgeuapt  ].reschn := opfbulapn;
   labcpy(ressym[opfbugapt  ].reslab, 'fbugapt   ');
   labcpy(ressym[opfbnza    ].reslab, 'fbnza     '); ressym[opfbnza    ].reschn := opfbept;
   labcpy(ressym[opfbza     ].reslab, 'fbza      ');
   labcpy(ressym[opfbulepn  ].reslab, 'fbulepn   ');
   labcpy(ressym[opbleuapt  ].reslab, 'bleuapt   '); ressym[opbleuapt  ].reschn := opfbugept;
   labcpy(ressym[opfbulapt  ].reslab, 'fbulapt   ');
   labcpy(ressym[opfbnzapn  ].reslab, 'fbnzapn   '); ressym[opfbnzapn  ].reschn := opfmovdne;
   labcpy(ressym[opfboa     ].reslab, 'fboa      ');
   labcpy(ressym[opfmovscc  ].reslab, 'fmovscc   ');
   labcpy(ressym[opfbulept  ].reslab, 'fbulept   '); ressym[opfbulept  ].reschn := opfmovdcs;
   labcpy(ressym[opbposapn  ].reslab, 'bposapn   '); ressym[opbposapn  ].reschn := opfmovrde;
   labcpy(ressym[opbrgzapn  ].reslab, 'brgzapn   ');
   labcpy(ressym[opfbnzapt  ].reslab, 'fbnzapt   '); ressym[opfbnzapt  ].reschn := opfmovdvc;
   labcpy(ressym[opfmovdue  ].reslab, 'fmovdue   ');
   labcpy(ressym[opfmovsge  ].reslab, 'fmovsge   ');
   labcpy(ressym[opbrgezpn  ].reslab, 'brgezpn   '); ressym[opbrgezpn  ].reschn := opfmovdgu;
   labcpy(ressym[opbposapt  ].reslab, 'bposapt   '); ressym[opbposapt  ].reschn := opbrlzapn;
   labcpy(ressym[opbrgzapt  ].reslab, 'brgzapt   '); ressym[opbrgzapt  ].reschn := opfmovqle;
   labcpy(ressym[opbrnzapn  ].reslab, 'brnzapn   ');
   labcpy(ressym[opfmovsle  ].reslab, 'fmovsle   '); ressym[opfmovsle  ].reschn := opfmovqne;
   labcpy(ressym[opbrlezpn  ].reslab, 'brlezpn   '); ressym[opbrlezpn  ].reschn := opfmovdlu;
   labcpy(ressym[opbrgezpt  ].reslab, 'brgezpt   '); ressym[opbrgezpt  ].reschn := opfmovsne;
   labcpy(ressym[opbrlzapt  ].reslab, 'brlzapt   '); ressym[opbrlzapt  ].reschn := opfmovqcs;
   labcpy(ressym[opfmovrqe  ].reslab, 'fmovrqe   ');
   labcpy(ressym[opbrnzapt  ].reslab, 'brnzapt   '); ressym[opbrnzapt  ].reschn := opfmovscs;
   labcpy(ressym[opfmovqvc  ].reslab, 'fmovqvc   '); ressym[opfmovqvc  ].reschn := opfmovrse;
   labcpy(ressym[opbrlezpt  ].reslab, 'brlezpt   '); ressym[opbrlezpt  ].reschn := opfmovque;
   labcpy(ressym[opfmovsvc  ].reslab, 'fmovsvc   '); ressym[opfmovsvc  ].reschn := opfmovdnz;
   labcpy(ressym[opfmovdvs  ].reslab, 'fmovdvs   '); ressym[opfmovdvs  ].reschn := opfmovqgu;
   labcpy(ressym[opfbapt    ].reslab, 'fbapt     '); ressym[opfbapt    ].reschn := opfbgpn;
   labcpy(ressym[opfmovsgu  ].reslab, 'fmovsgu   '); ressym[opfmovsgu  ].reschn := opfmovsug;
   labcpy(ressym[opfmovrdz  ].reslab, 'fmovrdz   ');
   labcpy(ressym[opfbnpt    ].reslab, 'fbnpt     '); ressym[opfbnpt    ].reschn := opfdivq;
   labcpy(ressym[opfmovqlu  ].reslab, 'fmovqlu   '); ressym[opfmovqlu  ].reschn := opfmovqul;
   labcpy(ressym[opfbupt    ].reslab, 'fbupt     '); ressym[opfbupt    ].reschn := opfsubq;
   labcpy(ressym[opfmovslu  ].reslab, 'fmovslu   '); ressym[opfmovslu  ].reschn := opfmovsul;
   labcpy(ressym[opfbgpt    ].reslab, 'fbgpt     '); ressym[opfbgpt    ].reschn := opfnegs;
   labcpy(ressym[opfbugpt   ].reslab, 'fbugpt    '); ressym[opfbugpt   ].reschn := opfmovdl;
   labcpy(ressym[opmovrlez  ].reslab, 'movrlez   ');
   labcpy(ressym[opfbulpt   ].reslab, 'fbulpt    '); ressym[opfbulpt   ].reschn := opxnorcc;
   labcpy(ressym[opfmovqnz  ].reslab, 'fmovqnz   ');
   labcpy(ressym[opfmovqvs  ].reslab, 'fmovqvs   ');
   labcpy(ressym[opfmovsnz  ].reslab, 'fmovsnz   ');
   labcpy(ressym[opfmovsvs  ].reslab, 'fmovsvs   ');
   labcpy(ressym[opfmovrqz  ].reslab, 'fmovrqz   ');
   labcpy(ressym[opfbnept   ].reslab, 'fbnept    ');
   labcpy(ressym[opfmovrsz  ].reslab, 'fmovrsz   ');
   labcpy(ressym[opfbept    ].reslab, 'fbept     '); ressym[opfbept    ].reschn := opfnegq;
   labcpy(ressym[opfbuept   ].reslab, 'fbuept    ');
   labcpy(ressym[opfbgept   ].reslab, 'fbgept    '); ressym[opfbgept   ].reschn := opelseif;
   labcpy(ressym[opfbugept  ].reslab, 'fbugept   '); ressym[opfbugept  ].reschn := opfmovdle;
   labcpy(ressym[opfblept   ].reslab, 'fblept    '); ressym[opfblept   ].reschn := opfmovda;
   labcpy(ressym[opfbopt    ].reslab, 'fbopt     '); ressym[opfbopt    ].reschn := opfbupn;
   labcpy(ressym[opfbnapt   ].reslab, 'fbnapt    '); ressym[opfbnapt   ].reschn := opalignp;
   labcpy(ressym[opfbuapt   ].reslab, 'fbuapt    '); ressym[opfbuapt   ].reschn := opfbugpn;
   labcpy(ressym[opfbgapt   ].reslab, 'fbgapt    '); ressym[opfbgapt   ].reschn := opmembar;
   labcpy(ressym[opfbneapt  ].reslab, 'fbneapt   ');
   labcpy(ressym[opfbeapt   ].reslab, 'fbeapt    '); ressym[opfbeapt   ].reschn := opfbgepn;
   labcpy(ressym[opfbzapt   ].reslab, 'fbzapt    '); ressym[opfbzapt   ].reschn := opfbulpn;
   labcpy(ressym[opfbueapt  ].reslab, 'fbueapt   '); ressym[opfbueapt  ].reschn := opfbugepn;
   labcpy(ressym[opfbupn    ].reslab, 'fbupn     '); ressym[opfbupn    ].reschn := opldswa;
   labcpy(ressym[opfbgpn    ].reslab, 'fbgpn     '); ressym[opfbgpn    ].reschn := opfdivd;
   labcpy(ressym[opfbugpn   ].reslab, 'fbugpn    ');
   labcpy(ressym[opfblpn    ].reslab, 'fblpn     '); ressym[opfblpn    ].reschn := opmacro;
   labcpy(ressym[opfbulpn   ].reslab, 'fbulpn    '); ressym[opfbulpn   ].reschn := opmulscc;
   labcpy(ressym[opfblgpn   ].reslab, 'fblgpn    '); ressym[opfblgpn   ].reschn := opfbnepn;
   labcpy(ressym[opfbnepn   ].reslab, 'fbnepn    ');
   labcpy(ressym[opfbuepn   ].reslab, 'fbuepn    ');
   labcpy(ressym[opfbgepn   ].reslab, 'fbgepn    ');
   labcpy(ressym[opfbugepn  ].reslab, 'fbugepn   ');
   labcpy(ressym[opfblepn   ].reslab, 'fblepn    ');
   labcpy(ressym[opfbopn    ].reslab, 'fbopn     '); ressym[opfbopn    ].reschn := oporncc;
   labcpy(ressym[opfbuapn   ].reslab, 'fbuapn    '); ressym[opfbuapn   ].reschn := opfcmpeq;
   labcpy(ressym[opfbgapn   ].reslab, 'fbgapn    ');
   labcpy(ressym[opfblapn   ].reslab, 'fblapn    '); ressym[opfblapn   ].reschn := opsubccc;
   labcpy(ressym[opfbulapn  ].reslab, 'fbulapn   '); ressym[opfbulapn  ].reschn := opfmovdge;
   labcpy(ressym[opfbneapn  ].reslab, 'fbneapn   ');
   labcpy(ressym[opfbzapn   ].reslab, 'fbzapn    '); ressym[opfbzapn   ].reschn := opfmovde;
   labcpy(ressym[opfbueapn  ].reslab, 'fbueapn   ');
   labcpy(ressym[opfcmps    ].reslab, 'fcmps     ');
   labcpy(ressym[opfcmpd    ].reslab, 'fcmpd     '); ressym[opfcmpd    ].reschn := opdefdw;
   labcpy(ressym[opfcmpq    ].reslab, 'fcmpq     '); ressym[opfcmpq    ].reschn := opdefqw;
   labcpy(ressym[opfcmped   ].reslab, 'fcmped    ');
   labcpy(ressym[opfcmpeq   ].reslab, 'fcmpeq    '); ressym[opfcmpeq   ].reschn := opsdivcc;
   labcpy(ressym[opfdivs    ].reslab, 'fdivs     '); ressym[opfdivs    ].reschn := opfmovd;
   labcpy(ressym[opfdivd    ].reslab, 'fdivd     ');
   labcpy(ressym[opfdivq    ].reslab, 'fdivq     ');
   labcpy(ressym[opfdmulq   ].reslab, 'fdmulq    '); ressym[opfdmulq   ].reschn := opumulcc;
   labcpy(ressym[opfitod    ].reslab, 'fitod     '); ressym[opfitod    ].reschn := opfdtoi;
   labcpy(ressym[opflush    ].reslab, 'flush     ');
   labcpy(ressym[opflushw   ].reslab, 'flushw    '); ressym[opflushw   ].reschn := opfmovsn;
   labcpy(ressym[opfmovd    ].reslab, 'fmovd     '); ressym[opfmovd    ].reschn := opstbar;
   labcpy(ressym[opfmovq    ].reslab, 'fmovq     '); ressym[opfmovq    ].reschn := opmovre;
   labcpy(ressym[opfmovsa   ].reslab, 'fmovsa    '); ressym[opfmovsa   ].reschn := opmovneg;
   labcpy(ressym[opfmovsn   ].reslab, 'fmovsn    ');
   labcpy(ressym[opfbugeapn ].reslab, 'fbugeapn  ');
   labcpy(ressym[opfmovsne  ].reslab, 'fmovsne   '); ressym[opfmovsne  ].reschn := opfmovslg;
   labcpy(ressym[opfmovse   ].reslab, 'fmovse    '); ressym[opfmovse   ].reschn := opfmovqg;
   labcpy(ressym[opfmovsg   ].reslab, 'fmovsg    ');
   labcpy(ressym[opfmovscs  ].reslab, 'fmovscs   ');
   labcpy(ressym[opfbuleapn ].reslab, 'fbuleapn  '); ressym[opfbuleapn ].reschn := optaddcctv;
   labcpy(ressym[opfbugeapt ].reslab, 'fbugeapt  ');
   labcpy(ressym[opfmovda   ].reslab, 'fmovda    ');
   labcpy(ressym[opfmovdn   ].reslab, 'fmovdn    '); ressym[opfmovdn   ].reschn := opfmovqa;
   labcpy(ressym[opprefetch ].reslab, 'prefetch  ');
   labcpy(ressym[opfmovdne  ].reslab, 'fmovdne   '); ressym[opfmovdne  ].reschn := opfmovqcc;
   labcpy(ressym[opfbuleapt ].reslab, 'fbuleapt  ');
   labcpy(ressym[opfmovdnz  ].reslab, 'fmovdnz   '); ressym[opfmovdnz  ].reschn := oprestore;
   labcpy(ressym[opba       ].reslab, 'ba        ');
   labcpy(ressym[opfmovdneg ].reslab, 'fmovdneg  ');
   labcpy(ressym[opfmovde   ].reslab, 'fmovde    '); ressym[opfmovde   ].reschn := opalignv;
   labcpy(ressym[opfmovdg   ].reslab, 'fmovdg    ');
   labcpy(ressym[opbe       ].reslab, 'be        '); ressym[opbe       ].reschn := opbrgezapn;
   labcpy(ressym[opfmovdle  ].reslab, 'fmovdle   ');
   labcpy(ressym[opbg       ].reslab, 'bg        ');
   labcpy(ressym[opfmovdge  ].reslab, 'fmovdge   ');
   labcpy(ressym[opfmovdgeu ].reslab, 'fmovdgeu  '); ressym[opfmovdgeu ].reschn := opfmovduge;
   labcpy(ressym[opbrlezapn ].reslab, 'brlezapn  ');
   labcpy(ressym[opbrgezapt ].reslab, 'brgezapt  ');
   labcpy(ressym[opbl       ].reslab, 'bl        ');
   labcpy(ressym[opfmovrdne ].reslab, 'fmovrdne  '); ressym[opfmovrdne ].reschn := opif;
   labcpy(ressym[opbn       ].reslab, 'bn        '); ressym[opbn       ].reschn := opfmovdleu;
   labcpy(ressym[opfmovqneg ].reslab, 'fmovqneg  ');
   labcpy(ressym[opbrlezapt ].reslab, 'brlezapt  ');
   labcpy(ressym[opfmovsneg ].reslab, 'fmovsneg  ');
   labcpy(ressym[opfmovdl   ].reslab, 'fmovdl    ');
   labcpy(ressym[opta       ].reslab, 'ta        ');
   labcpy(ressym[oprd       ].reslab, 'rd        '); ressym[oprd       ].reschn := oprestored;
   labcpy(ressym[opfmovdgu  ].reslab, 'fmovdgu   '); ressym[opfmovdgu  ].reschn := opfmovdug;
   labcpy(ressym[opfmovqgeu ].reslab, 'fmovqgeu  '); ressym[opfmovqgeu ].reschn := opfmovquge;
   labcpy(ressym[opte       ].reslab, 'te        ');
   labcpy(ressym[opfmovsgeu ].reslab, 'fmovsgeu  '); ressym[opfmovsgeu ].reschn := opfmovsuge;
   labcpy(ressym[optg       ].reslab, 'tg        ');
   labcpy(ressym[opbz       ].reslab, 'bz        '); ressym[opbz       ].reschn := opfmovdpos;
   labcpy(ressym[opfmovqleu ].reslab, 'fmovqleu  '); ressym[opfmovqleu ].reschn := opfmovqule;
   labcpy(ressym[opfmovrsne ].reslab, 'fmovrsne  ');
   labcpy(ressym[opfmovsleu ].reslab, 'fmovsleu  '); ressym[opfmovsleu ].reschn := opfmovsule;
   labcpy(ressym[optl       ].reslab, 'tl        ');
   labcpy(ressym[opor       ].reslab, 'or        ');
   labcpy(ressym[opfmovrdlz ].reslab, 'fmovrdlz  '); ressym[opfmovrdlz ].reschn := optn;
   labcpy(ressym[opfmovdleu ].reslab, 'fmovdleu  '); ressym[opfmovdleu ].reschn := opfmovdule;
   labcpy(ressym[opfmovrdnz ].reslab, 'fmovrdnz  ');
   labcpy(ressym[opfmovdcc  ].reslab, 'fmovdcc   ');
   labcpy(ressym[opfmovdcs  ].reslab, 'fmovdcs   ');
   labcpy(ressym[opst       ].reslab, 'st        ');
   labcpy(ressym[opfmovdlu  ].reslab, 'fmovdlu   '); ressym[opfmovdlu  ].reschn := opfmovdul;
   labcpy(ressym[opfmovqpos ].reslab, 'fmovqpos  '); ressym[opfmovqpos ].reschn := opwr;
   labcpy(ressym[opfmovrqgz ].reslab, 'fmovrqgz  ');
   labcpy(ressym[opfmovspos ].reslab, 'fmovspos  ');
   labcpy(ressym[opfmovrsgz ].reslab, 'fmovrsgz  ');
   labcpy(ressym[opfmovdpos ].reslab, 'fmovdpos  '); ressym[opfmovdpos ].reschn := opfmovrqne;
   labcpy(ressym[optz       ].reslab, 'tz        ');
   labcpy(ressym[opfmovrqlz ].reslab, 'fmovrqlz  ');
   labcpy(ressym[opfmovdvc  ].reslab, 'fmovdvc   '); ressym[opfmovdvc  ].reschn := opfmovqge;
   labcpy(ressym[opfmovrslz ].reslab, 'fmovrslz  '); ressym[opfmovrslz ].reschn := opfmovrqnz;
   labcpy(ressym[opfmovqa   ].reslab, 'fmovqa    ');
   labcpy(ressym[opfmovrsnz ].reslab, 'fmovrsnz  ');
   labcpy(ressym[opfmovqn   ].reslab, 'fmovqn    '); ressym[opfmovqn   ].reschn := opmovrne;
   labcpy(ressym[opfmovqne  ].reslab, 'fmovqne   '); ressym[opfmovqne  ].reschn := opfmovqlg;
   labcpy(ressym[opfmovqe   ].reslab, 'fmovqe    '); ressym[opfmovqe   ].reschn := opldstub;
   labcpy(ressym[opfmovqg   ].reslab, 'fmovqg    ');
   labcpy(ressym[opfmovqle  ].reslab, 'fmovqle   ');
   labcpy(ressym[opfmovqge  ].reslab, 'fmovqge   ');
   labcpy(ressym[opfmovqgu  ].reslab, 'fmovqgu   '); ressym[opfmovqgu  ].reschn := opfmovsue;
   labcpy(ressym[opfmovqcc  ].reslab, 'fmovqcc   '); ressym[opfmovqcc  ].reschn := opfmovdlg;
   labcpy(ressym[opfmovqcs  ].reslab, 'fmovqcs   ');
   labcpy(ressym[opfmovsu   ].reslab, 'fmovsu    '); ressym[opfmovsu   ].reschn := opreturn;
   labcpy(ressym[opfmovsug  ].reslab, 'fmovsug   ');
   labcpy(ressym[opfmovsul  ].reslab, 'fmovsul   ');
   labcpy(ressym[opfmovslg  ].reslab, 'fmovslg   ');
   labcpy(ressym[opfmovsue  ].reslab, 'fmovsue   '); ressym[opfmovsue  ].reschn := opfmovqug;
   labcpy(ressym[opfmovsuge ].reslab, 'fmovsuge  ');
   labcpy(ressym[opfmovsule ].reslab, 'fmovsule  ');
   labcpy(ressym[opfmovso   ].reslab, 'fmovso    ');
   labcpy(ressym[opfmovdug  ].reslab, 'fmovdug   '); ressym[opfmovdug  ].reschn := opilltrap;
   labcpy(ressym[opfmovdul  ].reslab, 'fmovdul   ');
   labcpy(ressym[opfmovdlg  ].reslab, 'fmovdlg   '); ressym[opfmovdlg  ].reschn := opldstuba;
   labcpy(ressym[opfmovduge ].reslab, 'fmovduge  ');
   labcpy(ressym[opfmovdule ].reslab, 'fmovdule  '); ressym[opfmovdule ].reschn := opld;
   labcpy(ressym[opfmovqu   ].reslab, 'fmovqu    ');
   labcpy(ressym[opfmovqug  ].reslab, 'fmovqug   ');
   labcpy(ressym[opfmovqul  ].reslab, 'fmovqul   '); ressym[opfmovqul  ].reschn := opmovrgez;
   labcpy(ressym[opfmovqlg  ].reslab, 'fmovqlg   ');
   labcpy(ressym[opfmovque  ].reslab, 'fmovque   ');
   labcpy(ressym[opfmovquge ].reslab, 'fmovquge  ');
   labcpy(ressym[opfmovqule ].reslab, 'fmovqule  '); ressym[opfmovqule ].reschn := opfmovrdgz;
   labcpy(ressym[opfmovqo   ].reslab, 'fmovqo    '); ressym[opfmovqo   ].reschn := opmovleu;
   labcpy(ressym[opfmovrse  ].reslab, 'fmovrse   ');
   labcpy(ressym[opfmovrde  ].reslab, 'fmovrde   ');
   labcpy(ressym[opfmovrdgz ].reslab, 'fmovrdgz  ');
   labcpy(ressym[opfmovrdgez].reslab, 'fmovrdgez ');
   labcpy(ressym[opfmovrqne ].reslab, 'fmovrqne  '); ressym[opfmovrqne ].reschn := optsubcctv;
   labcpy(ressym[opfmovrqnz ].reslab, 'fmovrqnz  ');
   labcpy(ressym[opfmuls    ].reslab, 'fmuls     ');
   labcpy(ressym[opfmuld    ].reslab, 'fmuld     '); ressym[opfmuld    ].reschn := opmovcc;
   labcpy(ressym[opfmulq    ].reslab, 'fmulq     '); ressym[opfmulq    ].reschn := opfstoi;
   labcpy(ressym[opfnegs    ].reslab, 'fnegs     '); ressym[opfnegs    ].reschn := opsaved;
   labcpy(ressym[opfnegd    ].reslab, 'fnegd     ');
   labcpy(ressym[opfnegq    ].reslab, 'fnegq     ');
   labcpy(ressym[opfsmuld   ].reslab, 'fsmuld    ');
   labcpy(ressym[opfsqrts   ].reslab, 'fsqrts    ');
   labcpy(ressym[opprefetcha].reslab, 'prefetcha ');
   labcpy(ressym[opfsqrtd   ].reslab, 'fsqrtd    ');
   labcpy(ressym[opfstoi    ].reslab, 'fstoi     '); ressym[opfstoi    ].reschn := opfdtox;
   labcpy(ressym[opfdtoi    ].reslab, 'fdtoi     '); ressym[opfdtoi    ].reschn := opfloat;
   labcpy(ressym[opbaa      ].reslab, 'baa       ');
   labcpy(ressym[opfqtoi    ].reslab, 'fqtoi     '); ressym[opfqtoi    ].reschn := opfsubs;
   labcpy(ressym[opfstod    ].reslab, 'fstod     '); ressym[opfstod    ].reschn := opfdtos;
   labcpy(ressym[opfdtos    ].reslab, 'fdtos     ');
   labcpy(ressym[opbcc      ].reslab, 'bcc       '); ressym[opbcc      ].reschn := opbea;
   labcpy(ressym[opadd      ].reslab, 'add       '); ressym[opadd      ].reschn := opfba;
   labcpy(ressym[opbga      ].reslab, 'bga       ');
   labcpy(ressym[opfqtos    ].reslab, 'fqtos     '); ressym[opfqtos    ].reschn := opprint;
   labcpy(ressym[opdec      ].reslab, 'dec       ');
   labcpy(ressym[opfbe      ].reslab, 'fbe       ');
   labcpy(ressym[opbge      ].reslab, 'bge       ');
   labcpy(ressym[opbla      ].reslab, 'bla       '); ressym[opbla      ].reschn := opfbg;
   labcpy(ressym[opfqtod    ].reslab, 'fqtod     '); ressym[opfqtod    ].reschn := opmovge;
   labcpy(ressym[opbna      ].reslab, 'bna       '); ressym[opbna      ].reschn := oplda;
   labcpy(ressym[opfdtox    ].reslab, 'fdtox     '); ressym[opfdtox    ].reschn := opfxtod;
   labcpy(ressym[opand      ].reslab, 'and       '); ressym[opand      ].reschn := opble;
   labcpy(ressym[opfbl      ].reslab, 'fbl       '); ressym[opfbl      ].reschn := opldd;
   labcpy(ressym[opbne      ].reslab, 'bne       ');
   labcpy(ressym[opfbn      ].reslab, 'fbn       ');
   labcpy(ressym[opcas      ].reslab, 'cas       '); ressym[opcas      ].reschn := opfbo;
   labcpy(ressym[opbcs      ].reslab, 'bcs       ');
   labcpy(ressym[opfqtox    ].reslab, 'fqtox     '); ressym[opfqtox    ].reschn := opfxtoq;
   labcpy(ressym[opinc      ].reslab, 'inc       '); ressym[opinc      ].reschn := opneg;
   labcpy(ressym[opbvc      ].reslab, 'bvc       ');
   labcpy(ressym[opfsubs    ].reslab, 'fsubs     '); ressym[opfsubs    ].reschn := opmovle;
   labcpy(ressym[opbza      ].reslab, 'bza       '); ressym[opbza      ].reschn := opfbu;
   labcpy(ressym[opbgu      ].reslab, 'bgu       ');
   labcpy(ressym[opfsubd    ].reslab, 'fsubd     ');
   labcpy(ressym[opcmp      ].reslab, 'cmp       '); ressym[opcmp      ].reschn := optge;
   labcpy(ressym[opclr      ].reslab, 'clr       '); ressym[opclr      ].reschn := opldq;
   labcpy(ressym[opfbz      ].reslab, 'fbz       '); ressym[opfbz      ].reschn := opfmovrdgez;
   labcpy(ressym[opblu      ].reslab, 'blu       ');
   labcpy(ressym[opfsubq    ].reslab, 'fsubq     ');
   labcpy(ressym[optle      ].reslab, 'tle       ');
   labcpy(ressym[opsra      ].reslab, 'sra       ');
   labcpy(ressym[opfmovrdlez].reslab, 'fmovrdlez '); ressym[opfmovrdlez].reschn := opjmp;
   labcpy(ressym[opldx      ].reslab, 'ldx       '); ressym[opldx      ].reschn := opsta;
   labcpy(ressym[opstb      ].reslab, 'stb       ');
   labcpy(ressym[opbnz      ].reslab, 'bnz       '); ressym[opbnz      ].reschn := opsub;
   labcpy(ressym[opbvs      ].reslab, 'bvs       '); ressym[opbvs      ].reschn := opret;
   labcpy(ressym[opset      ].reslab, 'set       ');
   labcpy(ressym[opnop      ].reslab, 'nop       '); ressym[opnop      ].reschn := optvc;
   labcpy(ressym[opbrz      ].reslab, 'brz       '); ressym[opbrz      ].reschn := opsir;
   labcpy(ressym[opfmovrqgez].reslab, 'fmovrqgez '); ressym[opfmovrqgez].reschn := oporn;
   labcpy(ressym[optgu      ].reslab, 'tgu       ');
   labcpy(ressym[opfmovrsgez].reslab, 'fmovrsgez '); ressym[opfmovrsgez].reschn := opnot;
   labcpy(ressym[opmov      ].reslab, 'mov       ');
   labcpy(ressym[opfxtos    ].reslab, 'fxtos     '); ressym[opfxtos    ].reschn := opstuwa;
   labcpy(ressym[opfmovrqlez].reslab, 'fmovrqlez ');
   labcpy(ressym[optlu      ].reslab, 'tlu       ');
   labcpy(ressym[opfmovrslez].reslab, 'fmovrslez ');
   labcpy(ressym[opfxtod    ].reslab, 'fxtod     '); ressym[opfxtod    ].reschn := opmovne;
   labcpy(ressym[opstq      ].reslab, 'stq       ');
   labcpy(ressym[opxor      ].reslab, 'xor       ');
   labcpy(ressym[opfxtoq    ].reslab, 'fxtoq     '); ressym[opfxtoq    ].reschn := opstswa;
   labcpy(ressym[optst      ].reslab, 'tst       ');
   labcpy(ressym[optnz      ].reslab, 'tnz       ');
   labcpy(ressym[optvs      ].reslab, 'tvs       ');
   labcpy(ressym[opstw      ].reslab, 'stw       ');
   labcpy(ressym[opstx      ].reslab, 'stx       ');
   labcpy(ressym[opilltrap  ].reslab, 'illtrap   ');
   labcpy(ressym[opinccc    ].reslab, 'inccc     '); ressym[opinccc    ].reschn := opdefle;
   labcpy(ressym[opiprefetch].reslab, 'iprefetch ');
   labcpy(ressym[opjmp      ].reslab, 'jmp       '); ressym[opjmp      ].reschn := optne;
   labcpy(ressym[opldd      ].reslab, 'ldd       ');
   labcpy(ressym[opldda     ].reslab, 'ldda      '); ressym[opldda     ].reschn := opdeff;
   labcpy(ressym[opld       ].reslab, 'ld        ');
   labcpy(ressym[oplda      ].reslab, 'lda       ');
   labcpy(ressym[opldq      ].reslab, 'ldq       ');
   labcpy(ressym[opldqa     ].reslab, 'ldqa      ');
   labcpy(ressym[opldsb     ].reslab, 'ldsb      ');
   labcpy(ressym[opldsba    ].reslab, 'ldsba     '); ressym[opldsba    ].reschn := opendif;
   labcpy(ressym[opldsh     ].reslab, 'ldsh      ');
   labcpy(ressym[opldsha    ].reslab, 'ldsha     ');
   labcpy(ressym[opldstub   ].reslab, 'ldstub    ');
   labcpy(ressym[opldstuba  ].reslab, 'ldstuba   ');
   labcpy(ressym[opldsw     ].reslab, 'ldsw      '); ressym[opldsw     ].reschn := optleu;
   labcpy(ressym[opldswa    ].reslab, 'ldswa     ');
   labcpy(ressym[opldub     ].reslab, 'ldub      '); ressym[opldub     ].reschn := oporcc;
   labcpy(ressym[oplduba    ].reslab, 'lduba     '); ressym[oplduba    ].reschn := opdefsf;
   labcpy(ressym[oplduh     ].reslab, 'lduh      '); ressym[oplduh     ].reschn := opsubc;
   labcpy(ressym[oplduha    ].reslab, 'lduha     '); ressym[oplduha    ].reschn := opdefhw;
   labcpy(ressym[oplduw     ].reslab, 'lduw      '); ressym[oplduw     ].reschn := opstsb;
   labcpy(ressym[oplduwa    ].reslab, 'lduwa     '); ressym[oplduwa    ].reschn := opsethi;
   labcpy(ressym[opldxa     ].reslab, 'ldxa      '); ressym[opldxa     ].reschn := opelse;
   labcpy(ressym[opmembar   ].reslab, 'membar    ');
   labcpy(ressym[opmova     ].reslab, 'mova      ');
   labcpy(ressym[opmovn     ].reslab, 'movn      '); ressym[opmovn     ].reschn := opstxa;
   labcpy(ressym[opmovne    ].reslab, 'movne     '); ressym[opmovne    ].reschn := opstuha;
   labcpy(ressym[opmovg     ].reslab, 'movg      '); ressym[opmovg     ].reschn := opstqa;
   labcpy(ressym[opmovle    ].reslab, 'movle     '); ressym[opmovle    ].reschn := opstsha;
   labcpy(ressym[opmovge    ].reslab, 'movge     ');
   labcpy(ressym[opmovgu    ].reslab, 'movgu     '); ressym[opmovgu    ].reschn := opsdivx;
   labcpy(ressym[opmovleu   ].reslab, 'movleu    ');
   labcpy(ressym[opmovcc    ].reslab, 'movcc     '); ressym[opmovcc    ].reschn := opdefvs;
   labcpy(ressym[opmovgeu   ].reslab, 'movgeu    ');
   labcpy(ressym[opmovcs    ].reslab, 'movcs     ');
   labcpy(ressym[opmovneg   ].reslab, 'movneg    ');
   labcpy(ressym[opmovvc    ].reslab, 'movvc     ');
   labcpy(ressym[opmovrne   ].reslab, 'movrne    '); ressym[opmovrne   ].reschn := opsetequ;
   labcpy(ressym[opmovre    ].reslab, 'movre     '); ressym[opmovre    ].reschn := opsignx;
   labcpy(ressym[opbcca     ].reslab, 'bcca      ');
   labcpy(ressym[opfbaa     ].reslab, 'fbaa      ');
   labcpy(ressym[opmovrgez  ].reslab, 'movrgez   ');
   labcpy(ressym[opaddc     ].reslab, 'addc      ');
   labcpy(ressym[opmsv7     ].reslab, 'msv7      ');
   labcpy(ressym[opfbea     ].reslab, 'fbea      '); ressym[opfbea     ].reschn := opmsv8;
   labcpy(ressym[opbgea     ].reslab, 'bgea      '); ressym[opbgea     ].reschn := opmsv9;
   labcpy(ressym[opfbga     ].reslab, 'fbga      ');
   labcpy(ressym[opdefb     ].reslab, 'defb      ');
   labcpy(ressym[opmovrgz   ].reslab, 'movrgz    ');
   labcpy(ressym[opmulscc   ].reslab, 'mulscc    '); ressym[opmulscc   ].reschn := opsmulcc;
   labcpy(ressym[opblea     ].reslab, 'blea      '); ressym[opblea     ].reschn := opfbge;
   labcpy(ressym[opfbla     ].reslab, 'fbla      '); ressym[opfbla     ].reschn := opldda;
   labcpy(ressym[opbnea     ].reslab, 'bnea      ');
   labcpy(ressym[opfbna     ].reslab, 'fbna      ');
   labcpy(ressym[opcasa     ].reslab, 'casa      '); ressym[opcasa     ].reschn := opfboa;
   labcpy(ressym[opbcsa     ].reslab, 'bcsa      '); ressym[opbcsa     ].reschn := opfble;
   labcpy(ressym[opneg      ].reslab, 'neg       '); ressym[opneg      ].reschn := optcc;
   labcpy(ressym[opfblg     ].reslab, 'fblg      '); ressym[opfblg     ].reschn := opfbne;
   labcpy(ressym[opbneg     ].reslab, 'bneg      '); ressym[opbneg     ].reschn := opbvca;
   labcpy(ressym[opnot      ].reslab, 'not       '); ressym[opnot      ].reschn := opsrl;
   labcpy(ressym[opfbua     ].reslab, 'fbua      ');
   labcpy(ressym[opbgua     ].reslab, 'bgua      ');
   labcpy(ressym[oporcc     ].reslab, 'orcc      ');
   labcpy(ressym[opandn     ].reslab, 'andn      '); ressym[opandn     ].reschn := opbapn;
   labcpy(ressym[opfbue     ].reslab, 'fbue      '); ressym[opfbue     ].reschn := opldqa;
   labcpy(ressym[opbgeu     ].reslab, 'bgeu      '); ressym[opbgeu     ].reschn := opbclr;
   labcpy(ressym[opblua     ].reslab, 'blua      '); ressym[opblua     ].reschn := opfbug;
   labcpy(ressym[opbepn     ].reslab, 'bepn      '); ressym[opbepn     ].reschn := opldsb;
   labcpy(ressym[opdone     ].reslab, 'done      '); ressym[opdone     ].reschn := opdefw;
   labcpy(ressym[opbapt     ].reslab, 'bapt      '); ressym[opbapt     ].reschn := opbgpn;
   labcpy(ressym[opbleu     ].reslab, 'bleu      ');
   labcpy(ressym[opclrh     ].reslab, 'clrh      '); ressym[opclrh     ].reschn := opfbul;
   labcpy(ressym[opstba     ].reslab, 'stba      ');
   labcpy(ressym[opbnza     ].reslab, 'bnza      '); ressym[opbnza     ].reschn := opbept;
   labcpy(ressym[opbvsa     ].reslab, 'bvsa      '); ressym[opbvsa     ].reschn := opblpn;
   labcpy(ressym[opbgpt     ].reslab, 'bgpt      '); ressym[opbgpt     ].reschn := oplduh;
   labcpy(ressym[opbnpn     ].reslab, 'bnpn      '); ressym[opbnpn     ].reschn := opbset;
   labcpy(ressym[opbrza     ].reslab, 'brza      '); ressym[opbrza     ].reschn := opcasx;
   labcpy(ressym[opfbnz     ].reslab, 'fbnz      '); ressym[opfbnz     ].reschn := opstha;
   labcpy(ressym[oporn      ].reslab, 'orn       '); ressym[oporn      ].reschn := opsth;
   labcpy(ressym[opblpt     ].reslab, 'blpt      '); ressym[opblpt     ].reschn := oppopc;
   labcpy(ressym[opjmpl     ].reslab, 'jmpl      '); ressym[opjmpl     ].reschn := opmova;
   labcpy(ressym[opbpos     ].reslab, 'bpos      '); ressym[opbpos     ].reschn := opbnpt;
   labcpy(ressym[opbrgz     ].reslab, 'brgz      '); ressym[opbrgz     ].reschn := optgeu;
   labcpy(ressym[opsdiv     ].reslab, 'sdiv      ');
   labcpy(ressym[opmove     ].reslab, 'move      '); ressym[opmove     ].reschn := opretl;
   labcpy(ressym[oprdpr     ].reslab, 'rdpr      '); ressym[oprdpr     ].reschn := opudiv;
   labcpy(ressym[opclrx     ].reslab, 'clrx      '); ressym[opclrx     ].reschn := opmovg;
   labcpy(ressym[opbzpn     ].reslab, 'bzpn      '); ressym[opbzpn     ].reschn := opbrlz;
   labcpy(ressym[opswap     ].reslab, 'swap      ');
   labcpy(ressym[opbrnz     ].reslab, 'brnz      '); ressym[opbrnz     ].reschn := oplduw;
   labcpy(ressym[opbtst     ].reslab, 'btst      ');
   labcpy(ressym[opmovl     ].reslab, 'movl      '); ressym[opmovl     ].reschn := opsrax;
   labcpy(ressym[opstwa     ].reslab, 'stwa      ');
   labcpy(ressym[opbzpt     ].reslab, 'bzpt      '); ressym[opbzpt     ].reschn := opmovn;
   labcpy(ressym[opsmul     ].reslab, 'smul      ');
   labcpy(ressym[opstsh     ].reslab, 'stsh      ');
   labcpy(ressym[opsllx     ].reslab, 'sllx      '); ressym[opsllx     ].reschn := opumul;
   labcpy(ressym[opsetx     ].reslab, 'setx      '); ressym[opsetx     ].reschn := opstuh;
   labcpy(ressym[oporncc    ].reslab, 'orncc     ');
   labcpy(ressym[opmulx     ].reslab, 'mulx      '); ressym[opmulx     ].reschn := optpos;
   labcpy(ressym[opxnor     ].reslab, 'xnor      ');
   labcpy(ressym[oppopc     ].reslab, 'popc      ');
   labcpy(ressym[opsrlx     ].reslab, 'srlx      ');
   labcpy(ressym[oprestore  ].reslab, 'restore   ');
   labcpy(ressym[opwrpr     ].reslab, 'wrpr      ');
   labcpy(ressym[opmovz     ].reslab, 'movz      ');
   labcpy(ressym[oprestored ].reslab, 'restored  ');
   labcpy(ressym[opret      ].reslab, 'ret       '); ressym[opret      ].reschn := opsll;
   labcpy(ressym[opretl     ].reslab, 'retl      ');
   labcpy(ressym[opreturn   ].reslab, 'return    ');
   labcpy(ressym[opstsw     ].reslab, 'stsw      ');
   labcpy(ressym[opsave     ].reslab, 'save      ');
   labcpy(ressym[opstuw     ].reslab, 'stuw      ');
   labcpy(ressym[opsaved    ].reslab, 'saved     ');
   labcpy(ressym[opsdivcc   ].reslab, 'sdivcc    ');
   labcpy(ressym[opsdivx    ].reslab, 'sdivx     ');
   labcpy(ressym[opsethi    ].reslab, 'sethi     '); ressym[opsethi    ].reschn := opstsba;
   labcpy(ressym[opsetsw    ].reslab, 'setsw     ');
   labcpy(ressym[opsignx    ].reslab, 'signx     ');
   labcpy(ressym[opsir      ].reslab, 'sir       ');
   labcpy(ressym[opsll      ].reslab, 'sll       '); ressym[opsll      ].reschn := opstd;
   labcpy(ressym[opsmulcc   ].reslab, 'smulcc    ');
   labcpy(ressym[opsrax     ].reslab, 'srax      '); ressym[opsrax     ].reschn := opstub;
   labcpy(ressym[opsrl      ].reslab, 'srl       ');
   labcpy(ressym[opstub     ].reslab, 'stub      ');
   labcpy(ressym[opstuba    ].reslab, 'stuba     '); ressym[opstuba    ].reschn := opxorcc;
   labcpy(ressym[opstsb     ].reslab, 'stsb      ');
   labcpy(ressym[opstsba    ].reslab, 'stsba     ');
   labcpy(ressym[opstbar    ].reslab, 'stbar     '); ressym[opstbar    ].reschn := opswapa;
   labcpy(ressym[opstd      ].reslab, 'std       '); ressym[opstd      ].reschn := opequ;
   labcpy(ressym[opstda     ].reslab, 'stda      ');
   labcpy(ressym[opsta      ].reslab, 'sta       ');
   labcpy(ressym[opsth      ].reslab, 'sth       ');
   labcpy(ressym[opstha     ].reslab, 'stha      ');
   labcpy(ressym[opstuh     ].reslab, 'stuh      ');
   labcpy(ressym[opstuha    ].reslab, 'stuha     ');
   labcpy(ressym[opstsha    ].reslab, 'stsha     ');
   labcpy(ressym[opstqa     ].reslab, 'stqa      ');
   labcpy(ressym[opstuwa    ].reslab, 'stuwa     ');
   labcpy(ressym[opstswa    ].reslab, 'stswa     ');
   labcpy(ressym[opaddcc    ].reslab, 'addcc     ');
   labcpy(ressym[opstxa     ].reslab, 'stxa      ');
   labcpy(ressym[opsub      ].reslab, 'sub       '); ressym[opsub      ].reschn := optcs;
   labcpy(ressym[opdeccc    ].reslab, 'deccc     ');
   labcpy(ressym[opsubcc    ].reslab, 'subcc     ');
   labcpy(ressym[opsubc     ].reslab, 'subc      ');
   labcpy(ressym[opfbgea    ].reslab, 'fbgea     ');
   labcpy(ressym[opdefbe    ].reslab, 'defbe     ');
   labcpy(ressym[opsubccc   ].reslab, 'subccc    ');
   labcpy(ressym[opswapa    ].reslab, 'swapa     ');
   labcpy(ressym[opandcc    ].reslab, 'andcc     '); ressym[opandcc    ].reschn := opdeffd;
   labcpy(ressym[opfblea    ].reslab, 'fblea     '); ressym[opfblea    ].reschn := opdefef;
   labcpy(ressym[optaddcctv ].reslab, 'taddcctv  ');
   labcpy(ressym[opfblga    ].reslab, 'fblga     '); ressym[opfblga    ].reschn := opfbnea;
   labcpy(ressym[opbnega    ].reslab, 'bnega     ');
   labcpy(ressym[optn       ].reslab, 'tn        ');
   labcpy(ressym[optne      ].reslab, 'tne       ');
   labcpy(ressym[opfabsd    ].reslab, 'fabsd     '); ressym[opfabsd    ].reschn := opinccc;
   labcpy(ressym[opdeflf    ].reslab, 'deflf     ');
   labcpy(ressym[opbaapn    ].reslab, 'baapn     ');
   labcpy(ressym[opfbuea    ].reslab, 'fbuea     ');
   labcpy(ressym[opbgeua    ].reslab, 'bgeua     '); ressym[opbgeua    ].reschn := opfnegd;
   labcpy(ressym[opfbuga    ].reslab, 'fbuga     ');
   labcpy(ressym[opbccpn    ].reslab, 'bccpn     '); ressym[opbccpn    ].reschn := opbeapn;
   labcpy(ressym[opfbapn    ].reslab, 'fbapn     ');
   labcpy(ressym[opbaapt    ].reslab, 'baapt     '); ressym[opbaapt    ].reschn := opbgapn;
   labcpy(ressym[opbleua    ].reslab, 'bleua     '); ressym[opbleua    ].reschn := opfbuge;
   labcpy(ressym[opfbula    ].reslab, 'fbula     '); ressym[opfbula    ].reschn := opfcmpd;
   labcpy(ressym[opfbepn    ].reslab, 'fbepn     ');
   labcpy(ressym[opbccpt    ].reslab, 'bccpt     '); ressym[opbccpt    ].reschn := opbeapt;
   labcpy(ressym[opblapn    ].reslab, 'blapn     '); ressym[opblapn    ].reschn := opfabsq;
   labcpy(ressym[opbgapt    ].reslab, 'bgapt     '); ressym[opbgapt    ].reschn := opfbule;
   labcpy(ressym[opbnapn    ].reslab, 'bnapn     '); ressym[opbnapn    ].reschn := opfabss;
   labcpy(ressym[opcasxa    ].reslab, 'casxa     '); ressym[opcasxa    ].reschn := opsubcc;
   labcpy(ressym[opblepn    ].reslab, 'blepn     '); ressym[opblepn    ].reschn := opfbnza;
   labcpy(ressym[opbgept    ].reslab, 'bgept     '); ressym[opbgept    ].reschn := opfblpn;
   labcpy(ressym[opblapt    ].reslab, 'blapt     '); ressym[opblapt    ].reschn := opbnepn;
   labcpy(ressym[opfbnpn    ].reslab, 'fbnpn     '); ressym[opfbnpn    ].reschn := opfsubd;
   labcpy(ressym[opbposa    ].reslab, 'bposa     '); ressym[opbposa    ].reschn := opbnapt;
   labcpy(ressym[opbcspn    ].reslab, 'bcspn     '); ressym[opbcspn    ].reschn := opbrgza;
   labcpy(ressym[opblept    ].reslab, 'blept     '); ressym[opblept    ].reschn := opfcmpq;
   labcpy(ressym[opfblpt    ].reslab, 'fblpt     '); ressym[opfblpt    ].reschn := opfmuld;
   labcpy(ressym[opbnept    ].reslab, 'bnept     '); ressym[opbnept    ].reschn := opbvcpn;
   labcpy(ressym[opbrgez    ].reslab, 'brgez     '); ressym[opbrgez    ].reschn := opfbnpt;
   labcpy(ressym[opbzapn    ].reslab, 'bzapn     '); ressym[opbzapn    ].reschn := opbrlza;
   labcpy(ressym[opbcspt    ].reslab, 'bcspt     '); ressym[opbcspt    ].reschn := opbgupn;
   labcpy(ressym[opbrnza    ].reslab, 'brnza     '); ressym[opbrnza    ].reschn := oplduwa;
   labcpy(ressym[opfdtoq    ].reslab, 'fdtoq     '); ressym[opfdtoq    ].reschn := opfqtod;
   labcpy(ressym[opbvcpt    ].reslab, 'bvcpt     '); ressym[opbvcpt    ].reschn := opbrlez;
   labcpy(ressym[opfbzpn    ].reslab, 'fbzpn     '); ressym[opfbzpn    ].reschn := opfstod;
   labcpy(ressym[opbzapt    ].reslab, 'bzapt     '); ressym[opbzapt    ].reschn := opblupn;
   labcpy(ressym[opbgupt    ].reslab, 'bgupt     '); ressym[opbgupt    ].reschn := opflush;
   labcpy(ressym[opfitoq    ].reslab, 'fitoq     '); ressym[opfitoq    ].reschn := opfqtoi;
   labcpy(ressym[optge      ].reslab, 'tge       ');
   labcpy(ressym[opfitos    ].reslab, 'fitos     '); ressym[opfitos    ].reschn := opfmulq;
   labcpy(ressym[opfbzpt    ].reslab, 'fbzpt     ');
   labcpy(ressym[opblupt    ].reslab, 'blupt     '); ressym[opblupt    ].reschn := opfmuls;
   labcpy(ressym[opbnzpn    ].reslab, 'bnzpn     '); ressym[opbnzpn    ].reschn := opmovcs;
   labcpy(ressym[opbvspn    ].reslab, 'bvspn     '); ressym[opbvspn    ].reschn := opfmovq;
   labcpy(ressym[operror    ].reslab, 'error     ');
   labcpy(ressym[opfmovs    ].reslab, 'fmovs     '); ressym[opfmovs    ].reschn := opmovvc;
   labcpy(ressym[opbrzpn    ].reslab, 'brzpn     ');
   labcpy(ressym[opfstoq    ].reslab, 'fstoq     '); ressym[opfstoq    ].reschn := opfqtos;
   labcpy(ressym[opbnzpt    ].reslab, 'bnzpt     '); ressym[opbnzpt    ].reschn := opmovgu;
   labcpy(ressym[opbvspt    ].reslab, 'bvspt     ');
   labcpy(ressym[opudivx    ].reslab, 'udivx     ');
   labcpy(ressym[optleu     ].reslab, 'tleu      ');
   labcpy(ressym[opbrzpt    ].reslab, 'brzpt     '); ressym[opbrzpt    ].reschn := opfqtox;
   labcpy(ressym[opmovlu    ].reslab, 'movlu     ');
   labcpy(ressym[opfstox    ].reslab, 'fstox     '); ressym[opfstox    ].reschn := opfxtos;
   labcpy(ressym[optcc      ].reslab, 'tcc       ');
   labcpy(ressym[opretry    ].reslab, 'retry     '); ressym[opretry    ].reschn := opsetsw;
   labcpy(ressym[optgeu     ].reslab, 'tgeu      ');
   labcpy(ressym[opsetuw    ].reslab, 'setuw     ');
   labcpy(ressym[optcs      ].reslab, 'tcs       ');
   labcpy(ressym[opmovnz    ].reslab, 'movnz     ');
   labcpy(ressym[opmovvs    ].reslab, 'movvs     ');
   labcpy(ressym[optpos     ].reslab, 'tpos      '); ressym[optpos     ].reschn := opstop;
   labcpy(ressym[optneg     ].reslab, 'tneg      ');
   labcpy(ressym[opmovrz    ].reslab, 'movrz     ');
   labcpy(ressym[optvc      ].reslab, 'tvc       ');
   labcpy(ressym[optsubcctv ].reslab, 'tsubcctv  ');
   labcpy(ressym[opudiv     ].reslab, 'udiv      ');
   labcpy(ressym[opudivcc   ].reslab, 'udivcc    ');
   labcpy(ressym[opumul     ].reslab, 'umul      ');
   labcpy(ressym[opumulcc   ].reslab, 'umulcc    ');
   labcpy(ressym[opwr       ].reslab, 'wr        ');
   labcpy(ressym[opxorcc    ].reslab, 'xorcc     ');
   labcpy(ressym[opxnorcc   ].reslab, 'xnorcc    ');
   labcpy(ressym[opfloat    ].reslab, 'float     ');
   labcpy(ressym[opnfloat   ].reslab, 'nfloat    ');
   labcpy(ressym[opmsv8     ].reslab, 'msv8      ');
   labcpy(ressym[opmsv9     ].reslab, 'msv9      ');
   labcpy(ressym[opmacro    ].reslab, 'macro     '); ressym[opmacro    ].reschn := opdefps;
   labcpy(ressym[opendmac   ].reslab, 'endmac    ');
   labcpy(ressym[opequ      ].reslab, 'equ       ');
   labcpy(ressym[opsetequ   ].reslab, 'setequ    ');
   labcpy(ressym[opextern   ].reslab, 'extern    ');
   labcpy(ressym[opalignp   ].reslab, 'alignp    ');
   labcpy(ressym[opaddccc   ].reslab, 'addccc    ');
   labcpy(ressym[opalignv   ].reslab, 'alignv    ');
   labcpy(ressym[opif       ].reslab, 'if        ');
   labcpy(ressym[opelse     ].reslab, 'else      ');
   labcpy(ressym[opelseif   ].reslab, 'elseif    ');
   labcpy(ressym[opendif    ].reslab, 'endif     '); ressym[opendif    ].reschn := opdeffq;
   labcpy(ressym[opassm     ].reslab, 'assm      ');
   labcpy(ressym[opprint    ].reslab, 'print     ');
   labcpy(ressym[opstop     ].reslab, 'stop      ');
   labcpy(ressym[oplendian  ].reslab, 'lendian   ');
   labcpy(ressym[opdefbef   ].reslab, 'defbef    ');
   labcpy(ressym[opdefps    ].reslab, 'defps     ');
   labcpy(ressym[opdefvs    ].reslab, 'defvs     ');
   labcpy(ressym[opdefle    ].reslab, 'defle     ');
   labcpy(ressym[opdeff     ].reslab, 'deff      ');
   labcpy(ressym[opdefsf    ].reslab, 'defsf     ');
   labcpy(ressym[opdefef    ].reslab, 'defef     ');
   labcpy(ressym[optaddcc   ].reslab, 'taddcc    ');
   labcpy(ressym[opdefw     ].reslab, 'defw      ');
   labcpy(ressym[opdefhw    ].reslab, 'defhw     ');
   labcpy(ressym[opdeflef   ].reslab, 'deflef    ');
   labcpy(ressym[opandncc   ].reslab, 'andncc    '); ressym[opandncc   ].reschn := opbccapn;
   labcpy(ressym[opfbaapn   ].reslab, 'fbaapn    '); ressym[opfbaapn   ].reschn := opendmac;
   labcpy(ressym[opdefdw    ].reslab, 'defdw     ');
   labcpy(ressym[opfbugea   ].reslab, 'fbugea    ');
   labcpy(ressym[opdefqw    ].reslab, 'defqw     ');
   labcpy(ressym[opfbeapn   ].reslab, 'fbeapn    ');
   labcpy(ressym[opbccapt   ].reslab, 'bccapt    '); ressym[opbccapt   ].reschn := opbgeapn;
   labcpy(ressym[opfbaapt   ].reslab, 'fbaapt    '); ressym[opfbaapt   ].reschn := opfbgapn;
   labcpy(ressym[opfbulea   ].reslab, 'fbulea    '); ressym[opfbulea   ].reschn := opfcmped;
   labcpy(ressym[opdeffd    ].reslab, 'deffd     ');
   labcpy(ressym[opglobal   ].reslab, 'global    ');
   labcpy(ressym[opbleapn   ].reslab, 'bleapn    '); ressym[opbleapn   ].reschn := opfbeapt;
   labcpy(ressym[opbgeapt   ].reslab, 'bgeapt    '); ressym[opbgeapt   ].reschn := opfblapn;
   labcpy(ressym[opbneapn   ].reslab, 'bneapn    '); ressym[opbneapn   ].reschn := opfbgapt;
   labcpy(ressym[opfbnapn   ].reslab, 'fbnapn    ');
   labcpy(ressym[opfboapn   ].reslab, 'fboapn    ');
   labcpy(ressym[opbcsapn   ].reslab, 'bcsapn    '); ressym[opbcsapn   ].reschn := opfblepn;
   labcpy(ressym[opbleapt   ].reslab, 'bleapt    '); ressym[opbleapt   ].reschn := opfbgept;
   labcpy(ressym[opfblapt   ].reslab, 'fblapt    '); ressym[opfblapt   ].reschn := opfblgpn;
   labcpy(ressym[opbneapt   ].reslab, 'bneapt    '); ressym[opbneapt   ].reschn := opbnegpn;
   labcpy(ressym[opbrgeza   ].reslab, 'brgeza    '); ressym[opbrgeza   ].reschn := opfbnapt;
   labcpy(ressym[opfboapt   ].reslab, 'fboapt    '); ressym[opfboapt   ].reschn := opfbuapn;
   labcpy(ressym[opbcsapt   ].reslab, 'bcsapt    '); ressym[opbcsapt   ].reschn := opbguapn;
   labcpy(ressym[opfcmpes   ].reslab, 'fcmpes    '); ressym[opfcmpes   ].reschn := opudivcc;
   labcpy(ressym[opfblgpt   ].reslab, 'fblgpt    '); ressym[opfblgpt   ].reschn := opfbnept;
   labcpy(ressym[opbnegpt   ].reslab, 'bnegpt    '); ressym[opbnegpt   ].reschn := opbvcapt;
   labcpy(ressym[opbgeupn   ].reslab, 'bgeupn    '); ressym[opbgeupn   ].reschn := opfbzapn;
   labcpy(ressym[opbluapn   ].reslab, 'bluapn    '); ressym[opbluapn   ].reschn := opfbuapt;
   labcpy(ressym[opbguapt   ].reslab, 'bguapt    '); ressym[opbguapt   ].reschn := opfmovdg;
   labcpy(ressym[optsubcc   ].reslab, 'tsubcc    '); ressym[optsubcc   ].reschn := opnfloat;
   labcpy(ressym[opdeffq    ].reslab, 'deffq     ');
   labcpy(ressym[opbleupn   ].reslab, 'bleupn    '); ressym[opbleupn   ].reschn := opfbuept;
   labcpy(ressym[opbgeupt   ].reslab, 'bgeupt    '); ressym[opbgeupt   ].reschn := opfbzapt;
   labcpy(ressym[opbluapt   ].reslab, 'bluapt    '); ressym[opbluapt   ].reschn := opfbugpt;
   labcpy(ressym[opbnzapn   ].reslab, 'bnzapn    '); ressym[opbnzapn   ].reschn := opfdmulq;
   labcpy(ressym[opbvsapn   ].reslab, 'bvsapn    '); ressym[opbvsapn   ].reschn := opfmovdn;
   labcpy(ressym[opfmovdo   ].reslab, 'fmovdo    '); ressym[opfmovdo   ].reschn := opfsmuld;
   labcpy(ressym[opbleupt   ].reslab, 'bleupt    '); ressym[opbleupt   ].reschn := opfmovsa;
   labcpy(ressym[opbrzapn   ].reslab, 'brzapn    '); ressym[opbrzapn   ].reschn := opfbulpt;
   labcpy(ressym[opfbnzpn   ].reslab, 'fbnzpn    '); ressym[opfbnzpn   ].reschn := opfmovqe;
   labcpy(ressym[opbnzapt   ].reslab, 'bnzapt    ');
   labcpy(ressym[opbvsapt   ].reslab, 'bvsapt    '); ressym[opbvsapt   ].reschn := opfmovse;
   labcpy(ressym[opfmovdu   ].reslab, 'fmovdu    ');

end;

{******************************************************************************

Initalize opcode base table

The opcode base table contains the basic code values for all instructions, with
registers, immediates and modes zeroed out. This allows the construction of an
instruction by taking the base code and oring in the registers and modes, which
greatly simplifies and speeds the process.

*******************************************************************************}

procedure basini;

begin

   opcbas[opadd       ] := $00000000;  
   opcbas[opaddcc     ] := $00000000;  
   opcbas[opaddc      ] := $00000000;  
   opcbas[opaddccc    ] := $00000000;  
   opcbas[opand       ] := $00000000;  
   opcbas[opandcc     ] := $00000000;  
   opcbas[opandn      ] := $00000000;  
   opcbas[opandncc    ] := $00000000;  
   opcbas[opba        ] := $00000000;  
   opcbas[opbn        ] := $00000000;  
   opcbas[opbne       ] := $00000000;  
   opcbas[opbnz       ] := $00000000;  
   opcbas[opbe        ] := $00000000;  
   opcbas[opbz        ] := $00000000;  
   opcbas[opbg        ] := $00000000;  
   opcbas[opble       ] := $00000000;  
   opcbas[opbge       ] := $00000000;  
   opcbas[opbl        ] := $00000000;  
   opcbas[opbgu       ] := $00000000;  
   opcbas[opbleu      ] := $00000000;  
   opcbas[opbcc       ] := $00000000;  
   opcbas[opbgeu      ] := $00000000;  
   opcbas[opbcs       ] := $00000000;  
   opcbas[opblu       ] := $00000000;  
   opcbas[opbpos      ] := $00000000;  
   opcbas[opbneg      ] := $00000000;  
   opcbas[opbvc       ] := $00000000;  
   opcbas[opbvs       ] := $00000000;  
   opcbas[opbaa       ] := $00000000;  
   opcbas[opbna       ] := $00000000;  
   opcbas[opbnea      ] := $00000000;  
   opcbas[opbnza      ] := $00000000;  
   opcbas[opbea       ] := $00000000;  
   opcbas[opbza       ] := $00000000;  
   opcbas[opbga       ] := $00000000;  
   opcbas[opblea      ] := $00000000;  
   opcbas[opbgea      ] := $00000000;  
   opcbas[opbla       ] := $00000000;  
   opcbas[opbgua      ] := $00000000;  
   opcbas[opbleua     ] := $00000000;  
   opcbas[opbcca      ] := $00000000;  
   opcbas[opbgeua     ] := $00000000;  
   opcbas[opbcsa      ] := $00000000;  
   opcbas[opblua      ] := $00000000;  
   opcbas[opbposa     ] := $00000000;  
   opcbas[opbnega     ] := $00000000;  
   opcbas[opbvca      ] := $00000000;  
   opcbas[opbvsa      ] := $00000000;  
   opcbas[opbapt      ] := $00000000;  
   opcbas[opbnpt      ] := $00000000;  
   opcbas[opbnept     ] := $00000000;  
   opcbas[opbnzpt     ] := $00000000;  
   opcbas[opbept      ] := $00000000;  
   opcbas[opbzpt      ] := $00000000;  
   opcbas[opbgpt      ] := $00000000;  
   opcbas[opblept     ] := $00000000;  
   opcbas[opbgept     ] := $00000000;  
   opcbas[opblpt      ] := $00000000;  
   opcbas[opbgupt     ] := $00000000;  
   opcbas[opbleupt    ] := $00000000;  
   opcbas[opbccpt     ] := $00000000;  
   opcbas[opbgeupt    ] := $00000000;  
   opcbas[opbcspt     ] := $00000000;  
   opcbas[opblupt     ] := $00000000;  
   opcbas[opbpospt    ] := $00000000;  
   opcbas[opbnegpt    ] := $00000000;  
   opcbas[opbvcpt     ] := $00000000;  
   opcbas[opbvspt     ] := $00000000;  
   opcbas[opbaapt     ] := $00000000;  
   opcbas[opbnapt     ] := $00000000;  
   opcbas[opbneapt    ] := $00000000;  
   opcbas[opbnzapt    ] := $00000000;  
   opcbas[opbeapt     ] := $00000000;  
   opcbas[opbzapt     ] := $00000000;  
   opcbas[opbgapt     ] := $00000000;  
   opcbas[opbleapt    ] := $00000000;  
   opcbas[opbgeapt    ] := $00000000;  
   opcbas[opblapt     ] := $00000000;  
   opcbas[opbguapt    ] := $00000000;  
   opcbas[opbleuapt   ] := $00000000;  
   opcbas[opbccapt    ] := $00000000;  
   opcbas[opbgeuapt   ] := $00000000;  
   opcbas[opbcsapt    ] := $00000000;  
   opcbas[opbluapt    ] := $00000000;  
   opcbas[opbposapt   ] := $00000000;  
   opcbas[opbnegapt   ] := $00000000;  
   opcbas[opbvcapt    ] := $00000000;  
   opcbas[opbvsapt    ] := $00000000;  
   opcbas[opbapn      ] := $00000000;  
   opcbas[opbnpn      ] := $00000000;
   opcbas[opbnepn     ] := $00000000;  
   opcbas[opbnzpn     ] := $00000000;  
   opcbas[opbepn      ] := $00000000;  
   opcbas[opbzpn      ] := $00000000;  
   opcbas[opbgpn      ] := $00000000;  
   opcbas[opblepn     ] := $00000000;  
   opcbas[opbgepn     ] := $00000000;  
   opcbas[opblpn      ] := $00000000;  
   opcbas[opbgupn     ] := $00000000;  
   opcbas[opbleupn    ] := $00000000;  
   opcbas[opbccpn     ] := $00000000;  
   opcbas[opbgeupn    ] := $00000000;  
   opcbas[opbcspn     ] := $00000000;  
   opcbas[opblupn     ] := $00000000;  
   opcbas[opbpospn    ] := $00000000;  
   opcbas[opbnegpn    ] := $00000000;  
   opcbas[opbvcpn     ] := $00000000;  
   opcbas[opbvspn     ] := $00000000;  
   opcbas[opbaapn     ] := $00000000;  
   opcbas[opbnapn     ] := $00000000;  
   opcbas[opbneapn    ] := $00000000;  
   opcbas[opbnzapn    ] := $00000000;  
   opcbas[opbeapn     ] := $00000000;  
   opcbas[opbzapn     ] := $00000000;  
   opcbas[opbgapn     ] := $00000000;  
   opcbas[opbleapn    ] := $00000000;  
   opcbas[opbgeapn    ] := $00000000;  
   opcbas[opblapn     ] := $00000000;  
   opcbas[opbguapn    ] := $00000000;  
   opcbas[opbleuapn   ] := $00000000;  
   opcbas[opbccapn    ] := $00000000;  
   opcbas[opbgeuapn   ] := $00000000;  
   opcbas[opbcsapn    ] := $00000000;  
   opcbas[opbluapn    ] := $00000000;  
   opcbas[opbposapn   ] := $00000000;  
   opcbas[opbnegapn   ] := $00000000;  
   opcbas[opbvcapn    ] := $00000000;  
   opcbas[opbvsapn    ] := $00000000;
   opcbas[opbrz       ] := $00000000;  
   opcbas[opbrlez     ] := $00000000;  
   opcbas[opbrlz      ] := $00000000;  
   opcbas[opbrnz      ] := $00000000;  
   opcbas[opbrgz      ] := $00000000;  
   opcbas[opbrgez     ] := $00000000;  
   opcbas[opbrza      ] := $00000000;  
   opcbas[opbrleza    ] := $00000000;  
   opcbas[opbrlza     ] := $00000000;  
   opcbas[opbrnza     ] := $00000000;  
   opcbas[opbrgza     ] := $00000000;  
   opcbas[opbrgeza    ] := $00000000;  
   opcbas[opbrzpt     ] := $00000000;  
   opcbas[opbrlezpt   ] := $00000000;  
   opcbas[opbrlzpt    ] := $00000000;  
   opcbas[opbrnzpt    ] := $00000000;  
   opcbas[opbrgzpt    ] := $00000000;  
   opcbas[opbrgezpt   ] := $00000000;  
   opcbas[opbrzapt    ] := $00000000;  
   opcbas[opbrlezapt  ] := $00000000;  
   opcbas[opbrlzapt   ] := $00000000;  
   opcbas[opbrnzapt   ] := $00000000;  
   opcbas[opbrgzapt   ] := $00000000;  
   opcbas[opbrgezapt  ] := $00000000;  
   opcbas[opbrzpn     ] := $00000000;  
   opcbas[opbrlezpn   ] := $00000000;  
   opcbas[opbrlzpn    ] := $00000000;  
   opcbas[opbrnzpn    ] := $00000000;  
   opcbas[opbrgzpn    ] := $00000000;  
   opcbas[opbrgezpn   ] := $00000000;  
   opcbas[opbrzapn    ] := $00000000;  
   opcbas[opbrlezapn  ] := $00000000;  
   opcbas[opbrlzapn   ] := $00000000;  
   opcbas[opbrnzapn   ] := $00000000;  
   opcbas[opbrgzapn   ] := $00000000;  
   opcbas[opbrgezapn  ] := $00000000;  
   opcbas[opbtst      ] := $00000000;  
   opcbas[opbset      ] := $00000000;  
   opcbas[opbclr      ] := $00000000;  
   opcbas[opbtog      ] := $00000000;  
   opcbas[opcall      ] := $00000000;  
   opcbas[opcas       ] := $00000000;  
   opcbas[opcasl      ] := $00000000;  
   opcbas[opcasx      ] := $00000000;  
   opcbas[opcasxl     ] := $00000000;  
   opcbas[opcasa      ] := $00000000;  
   opcbas[opcasxa     ] := $00000000;  
   opcbas[opclr       ] := $00000000;  
   opcbas[opclrb      ] := $00000000;  
   opcbas[opclrh      ] := $00000000;  
   opcbas[opclrx      ] := $00000000;  
   opcbas[opcmp       ] := $00000000;  
   opcbas[opdec       ] := $00000000;  
   opcbas[opdeccc     ] := $00000000;  
   opcbas[opdone      ] := $00000000;  
   opcbas[opfabss     ] := $00000000;  
   opcbas[opfabsd     ] := $00000000;  
   opcbas[opfabsq     ] := $00000000;  
   opcbas[opfba       ] := $00000000;  
   opcbas[opfbn       ] := $00000000;  
   opcbas[opfbu       ] := $00000000;  
   opcbas[opfbg       ] := $00000000;  
   opcbas[opfbug      ] := $00000000;  
   opcbas[opfbl       ] := $00000000;  
   opcbas[opfbul      ] := $00000000;  
   opcbas[opfblg      ] := $00000000;  
   opcbas[opfbne      ] := $00000000;  
   opcbas[opfbnz      ] := $00000000;  
   opcbas[opfbe       ] := $00000000;  
   opcbas[opfbz       ] := $00000000;  
   opcbas[opfbue      ] := $00000000;  
   opcbas[opfbge      ] := $00000000;  
   opcbas[opfbuge     ] := $00000000;  
   opcbas[opfble      ] := $00000000;  
   opcbas[opfbule     ] := $00000000;  
   opcbas[opfbo       ] := $00000000;  
   opcbas[opfbaa      ] := $00000000;  
   opcbas[opfbna      ] := $00000000;  
   opcbas[opfbua      ] := $00000000;  
   opcbas[opfbga      ] := $00000000;  
   opcbas[opfbuga     ] := $00000000;  
   opcbas[opfbla      ] := $00000000;  
   opcbas[opfbula     ] := $00000000;  
   opcbas[opfblga     ] := $00000000;  
   opcbas[opfbnea     ] := $00000000;  
   opcbas[opfbnza     ] := $00000000;  
   opcbas[opfbea      ] := $00000000;  
   opcbas[opfbza      ] := $00000000;  
   opcbas[opfbuea     ] := $00000000;  
   opcbas[opfbgea     ] := $00000000;  
   opcbas[opfbugea    ] := $00000000;  
   opcbas[opfblea     ] := $00000000;  
   opcbas[opfbulea    ] := $00000000;  
   opcbas[opfboa      ] := $00000000;  
   opcbas[opfbapt     ] := $00000000;  
   opcbas[opfbnpt     ] := $00000000;  
   opcbas[opfbupt     ] := $00000000;  
   opcbas[opfbgpt     ] := $00000000;  
   opcbas[opfbugpt    ] := $00000000;  
   opcbas[opfblpt     ] := $00000000;  
   opcbas[opfbulpt    ] := $00000000;  
   opcbas[opfblgpt    ] := $00000000;  
   opcbas[opfbnept    ] := $00000000;  
   opcbas[opfbnzpt    ] := $00000000;  
   opcbas[opfbept     ] := $00000000;  
   opcbas[opfbzpt     ] := $00000000;  
   opcbas[opfbuept    ] := $00000000;  
   opcbas[opfbgept    ] := $00000000;  
   opcbas[opfbugept   ] := $00000000;  
   opcbas[opfblept    ] := $00000000;  
   opcbas[opfbulept   ] := $00000000;  
   opcbas[opfbopt     ] := $00000000;  
   opcbas[opfbaapt    ] := $00000000;  
   opcbas[opfbnapt    ] := $00000000;  
   opcbas[opfbuapt    ] := $00000000;  
   opcbas[opfbgapt    ] := $00000000;  
   opcbas[opfbugapt   ] := $00000000;  
   opcbas[opfblapt    ] := $00000000;  
   opcbas[opfbulapt   ] := $00000000;  
   opcbas[opfblgapt   ] := $00000000;  
   opcbas[opfbneapt   ] := $00000000;  
   opcbas[opfbnzapt   ] := $00000000;  
   opcbas[opfbeapt    ] := $00000000;  
   opcbas[opfbzapt    ] := $00000000;  
   opcbas[opfbueapt   ] := $00000000;  
   opcbas[opfbgeapt   ] := $00000000;  
   opcbas[opfbugeapt  ] := $00000000;  
   opcbas[opfbleapt   ] := $00000000;  
   opcbas[opfbuleapt  ] := $00000000;  
   opcbas[opfboapt    ] := $00000000;  
   opcbas[opfbapn     ] := $00000000;  
   opcbas[opfbnpn     ] := $00000000;  
   opcbas[opfbupn     ] := $00000000;  
   opcbas[opfbgpn     ] := $00000000;  
   opcbas[opfbugpn    ] := $00000000;  
   opcbas[opfblpn     ] := $00000000;  
   opcbas[opfbulpn    ] := $00000000;  
   opcbas[opfblgpn    ] := $00000000;  
   opcbas[opfbnepn    ] := $00000000;  
   opcbas[opfbnzpn    ] := $00000000;  
   opcbas[opfbepn     ] := $00000000;  
   opcbas[opfbzpn     ] := $00000000;  
   opcbas[opfbuepn    ] := $00000000;  
   opcbas[opfbgepn    ] := $00000000;  
   opcbas[opfbugepn   ] := $00000000;  
   opcbas[opfblepn    ] := $00000000;  
   opcbas[opfbulepn   ] := $00000000;  
   opcbas[opfbopn     ] := $00000000;  
   opcbas[opfbaapn    ] := $00000000;  
   opcbas[opfbnapn    ] := $00000000;  
   opcbas[opfbuapn    ] := $00000000;  
   opcbas[opfbgapn    ] := $00000000;  
   opcbas[opfbugapn   ] := $00000000;  
   opcbas[opfblapn    ] := $00000000;  
   opcbas[opfbulapn   ] := $00000000;  
   opcbas[opfblgapn   ] := $00000000;  
   opcbas[opfbneapn   ] := $00000000;  
   opcbas[opfbnzapn   ] := $00000000;  
   opcbas[opfbeapn    ] := $00000000;  
   opcbas[opfbzapn    ] := $00000000;  
   opcbas[opfbueapn   ] := $00000000;  
   opcbas[opfbgeapn   ] := $00000000;  
   opcbas[opfbugeapn  ] := $00000000;  
   opcbas[opfbleapn   ] := $00000000;  
   opcbas[opfbuleapn  ] := $00000000;  
   opcbas[opfboapn    ] := $00000000;  
   opcbas[opfcmps     ] := $00000000;  
   opcbas[opfcmpd     ] := $00000000;  
   opcbas[opfcmpq     ] := $00000000;  
   opcbas[opfcmpes    ] := $00000000;  
   opcbas[opfcmped    ] := $00000000;  
   opcbas[opfcmpeq    ] := $00000000;  
   opcbas[opfdivs     ] := $00000000;  
   opcbas[opfdivd     ] := $00000000;  
   opcbas[opfdivq     ] := $00000000;  
   opcbas[opfdmulq    ] := $00000000;  
   opcbas[opfitos     ] := $00000000;  
   opcbas[opfitod     ] := $00000000;  
   opcbas[opfitoq     ] := $00000000;  
   opcbas[opflush     ] := $00000000;  
   opcbas[opflushw    ] := $00000000;  
   opcbas[opfmovs     ] := $00000000;  
   opcbas[opfmovd     ] := $00000000;  
   opcbas[opfmovq     ] := $00000000;  
   opcbas[opfmovsa    ] := $00000000;  
   opcbas[opfmovsn    ] := $00000000;  
   opcbas[opfmovsne   ] := $00000000;  
   opcbas[opfmovsnz   ] := $00000000;  
   opcbas[opfmovse    ] := $00000000;  
   opcbas[opfmovsz    ] := $00000000;  
   opcbas[opfmovsg    ] := $00000000;  
   opcbas[opfmovsle   ] := $00000000;  
   opcbas[opfmovsge   ] := $00000000;  
   opcbas[opfmovsl    ] := $00000000;  
   opcbas[opfmovsgu   ] := $00000000;  
   opcbas[opfmovsleu  ] := $00000000;  
   opcbas[opfmovscc   ] := $00000000;  
   opcbas[opfmovsgeu  ] := $00000000;  
   opcbas[opfmovscs   ] := $00000000;  
   opcbas[opfmovslu   ] := $00000000;  
   opcbas[opfmovspos  ] := $00000000;  
   opcbas[opfmovsneg  ] := $00000000;  
   opcbas[opfmovsvc   ] := $00000000;  
   opcbas[opfmovsvs   ] := $00000000;  
   opcbas[opfmovda    ] := $00000000;  
   opcbas[opfmovdn    ] := $00000000;  
   opcbas[opfmovdne   ] := $00000000;  
   opcbas[opfmovdnz   ] := $00000000;  
   opcbas[opfmovde    ] := $00000000;  
   opcbas[opfmovdz    ] := $00000000;  
   opcbas[opfmovdg    ] := $00000000;  
   opcbas[opfmovdle   ] := $00000000;  
   opcbas[opfmovdge   ] := $00000000;  
   opcbas[opfmovdl    ] := $00000000;  
   opcbas[opfmovdgu   ] := $00000000;  
   opcbas[opfmovdleu  ] := $00000000;  
   opcbas[opfmovdcc   ] := $00000000;  
   opcbas[opfmovdgeu  ] := $00000000;  
   opcbas[opfmovdcs   ] := $00000000;  
   opcbas[opfmovdlu   ] := $00000000;  
   opcbas[opfmovdpos  ] := $00000000;  
   opcbas[opfmovdneg  ] := $00000000;  
   opcbas[opfmovdvc   ] := $00000000;  
   opcbas[opfmovdvs   ] := $00000000;  
   opcbas[opfmovqa    ] := $00000000;  
   opcbas[opfmovqn    ] := $00000000;  
   opcbas[opfmovqne   ] := $00000000;  
   opcbas[opfmovqnz   ] := $00000000;  
   opcbas[opfmovqe    ] := $00000000;  
   opcbas[opfmovqz    ] := $00000000;  
   opcbas[opfmovqg    ] := $00000000;  
   opcbas[opfmovqle   ] := $00000000;  
   opcbas[opfmovqge   ] := $00000000;  
   opcbas[opfmovql    ] := $00000000;  
   opcbas[opfmovqgu   ] := $00000000;  
   opcbas[opfmovqleu  ] := $00000000;  
   opcbas[opfmovqcc   ] := $00000000;  
   opcbas[opfmovqgeu  ] := $00000000;  
   opcbas[opfmovqcs   ] := $00000000;  
   opcbas[opfmovqlu   ] := $00000000;  
   opcbas[opfmovqpos  ] := $00000000;  
   opcbas[opfmovqneg  ] := $00000000;  
   opcbas[opfmovqvc   ] := $00000000;  
   opcbas[opfmovqvs   ] := $00000000;  
   opcbas[opfmovsu    ] := $00000000;  
   opcbas[opfmovsug   ] := $00000000;  
   opcbas[opfmovsul   ] := $00000000;  
   opcbas[opfmovslg   ] := $00000000;  
   opcbas[opfmovsue   ] := $00000000;  
   opcbas[opfmovsuge  ] := $00000000;  
   opcbas[opfmovsule  ] := $00000000;  
   opcbas[opfmovso    ] := $00000000;  
   opcbas[opfmovdu    ] := $00000000;  
   opcbas[opfmovdug   ] := $00000000;  
   opcbas[opfmovdul   ] := $00000000;  
   opcbas[opfmovdlg   ] := $00000000;  
   opcbas[opfmovdue   ] := $00000000;  
   opcbas[opfmovduge  ] := $00000000;  
   opcbas[opfmovdule  ] := $00000000;  
   opcbas[opfmovdo    ] := $00000000;  
   opcbas[opfmovqu    ] := $00000000;  
   opcbas[opfmovqug   ] := $00000000;  
   opcbas[opfmovqul   ] := $00000000;  
   opcbas[opfmovqlg   ] := $00000000;  
   opcbas[opfmovque   ] := $00000000;  
   opcbas[opfmovquge  ] := $00000000;  
   opcbas[opfmovqule  ] := $00000000;  
   opcbas[opfmovqo    ] := $00000000;  
   opcbas[opfmovrse   ] := $00000000;  
   opcbas[opfmovrsz   ] := $00000000;  
   opcbas[opfmovrslez ] := $00000000;  
   opcbas[opfmovrslz  ] := $00000000;  
   opcbas[opfmovrsne  ] := $00000000;  
   opcbas[opfmovrsnz  ] := $00000000;  
   opcbas[opfmovrsgz  ] := $00000000;  
   opcbas[opfmovrsgez ] := $00000000;  
   opcbas[opfmovrde   ] := $00000000;  
   opcbas[opfmovrdz   ] := $00000000;  
   opcbas[opfmovrdlez ] := $00000000;  
   opcbas[opfmovrdlz  ] := $00000000;  
   opcbas[opfmovrdne  ] := $00000000;  
   opcbas[opfmovrdnz  ] := $00000000;  
   opcbas[opfmovrdgz  ] := $00000000;  
   opcbas[opfmovrdgez ] := $00000000;  
   opcbas[opfmovrqe   ] := $00000000;  
   opcbas[opfmovrqz   ] := $00000000;  
   opcbas[opfmovrqlez ] := $00000000;  
   opcbas[opfmovrqlz  ] := $00000000;  
   opcbas[opfmovrqne  ] := $00000000;  
   opcbas[opfmovrqnz  ] := $00000000;  
   opcbas[opfmovrqgz  ] := $00000000;  
   opcbas[opfmovrqgez ] := $00000000;  
   opcbas[opfmuls     ] := $00000000;  
   opcbas[opfmuld     ] := $00000000;  
   opcbas[opfmulq     ] := $00000000;  
   opcbas[opfnegs     ] := $00000000;  
   opcbas[opfnegd     ] := $00000000;  
   opcbas[opfnegq     ] := $00000000;  
   opcbas[opfsmuld    ] := $00000000;  
   opcbas[opfsqrts    ] := $00000000;  
   opcbas[opfsqrtd    ] := $00000000;  
   opcbas[opfsqrtq    ] := $00000000;  
   opcbas[opfstoi     ] := $00000000;  
   opcbas[opfdtoi     ] := $00000000;  
   opcbas[opfqtoi     ] := $00000000;  
   opcbas[opfstod     ] := $00000000;  
   opcbas[opfstoq     ] := $00000000;  
   opcbas[opfdtos     ] := $00000000;  
   opcbas[opfdtoq     ] := $00000000;  
   opcbas[opfqtos     ] := $00000000;  
   opcbas[opfqtod     ] := $00000000;  
   opcbas[opfstox     ] := $00000000;  
   opcbas[opfdtox     ] := $00000000;  
   opcbas[opfqtox     ] := $00000000;  
   opcbas[opfsubs     ] := $00000000;  
   opcbas[opfsubd     ] := $00000000;  
   opcbas[opfsubq     ] := $00000000;  
   opcbas[opfxtos     ] := $00000000;  
   opcbas[opfxtod     ] := $00000000;  
   opcbas[opfxtoq     ] := $00000000;  
   opcbas[opilltrap   ] := $00000000;  
   opcbas[opimpdep1   ] := $00000000;  
   opcbas[opimpdep2   ] := $00000000;
   opcbas[opinc       ] := $00000000;  
   opcbas[opinccc     ] := $00000000;  
   opcbas[opiprefetch ] := $00000000;  
   opcbas[opjmp       ] := $00000000;  
   opcbas[opjmpl      ] := $00000000;  
   opcbas[opldd       ] := $00000000;  
   opcbas[opldda      ] := $00000000;  
   opcbas[opld        ] := $00000000;  
   opcbas[oplda       ] := $00000000;  
   opcbas[opldq       ] := $00000000;  
   opcbas[opldqa      ] := $00000000;  
   opcbas[opldsb      ] := $00000000;  
   opcbas[opldsba     ] := $00000000;  
   opcbas[opldsh      ] := $00000000;  
   opcbas[opldsha     ] := $00000000;  
   opcbas[opldstub    ] := $00000000;  
   opcbas[opldstuba   ] := $00000000;  
   opcbas[opldsw      ] := $00000000;  
   opcbas[opldswa     ] := $00000000;  
   opcbas[opldub      ] := $00000000;  
   opcbas[oplduba     ] := $00000000;  
   opcbas[oplduh      ] := $00000000;  
   opcbas[oplduha     ] := $00000000;  
   opcbas[oplduw      ] := $00000000;  
   opcbas[oplduwa     ] := $00000000;  
   opcbas[opldx       ] := $00000000;  
   opcbas[opldxa      ] := $00000000;  
   opcbas[opmembar    ] := $00000000;  
   opcbas[opmov       ] := $00000000;
   opcbas[opmova      ] := $00000000;  
   opcbas[opmovn      ] := $00000000;  
   opcbas[opmovne     ] := $00000000;  
   opcbas[opmovnz     ] := $00000000;  
   opcbas[opmove      ] := $00000000;  
   opcbas[opmovz      ] := $00000000;  
   opcbas[opmovg      ] := $00000000;  
   opcbas[opmovle     ] := $00000000;  
   opcbas[opmovge     ] := $00000000;  
   opcbas[opmovl      ] := $00000000;  
   opcbas[opmovgu     ] := $00000000;  
   opcbas[opmovleu    ] := $00000000;  
   opcbas[opmovcc     ] := $00000000;  
   opcbas[opmovgeu    ] := $00000000;  
   opcbas[opmovcs     ] := $00000000;  
   opcbas[opmovlu     ] := $00000000;  
   opcbas[opmovpos    ] := $00000000;  
   opcbas[opmovneg    ] := $00000000;  
   opcbas[opmovvc     ] := $00000000;  
   opcbas[opmovvs     ] := $00000000;  
   opcbas[opmovrne    ] := $00000000;  
   opcbas[opmovrnz    ] := $00000000;  
   opcbas[opmovre     ] := $00000000;  
   opcbas[opmovrz     ] := $00000000;  
   opcbas[opmovrgez   ] := $00000000;  
   opcbas[opmovrlz    ] := $00000000;  
   opcbas[opmovrlez   ] := $00000000;  
   opcbas[opmovrgz    ] := $00000000;  
   opcbas[opmulscc    ] := $00000000;  
   opcbas[opmulx      ] := $00000000;  
   opcbas[opneg       ] := $00000000;  
   opcbas[opnop       ] := $00000000;
   opcbas[opnot       ] := $00000000;  
   opcbas[opor        ] := $00000000;  
   opcbas[oporcc      ] := $00000000;  
   opcbas[oporn       ] := $00000000;  
   opcbas[oporncc     ] := $00000000;  
   opcbas[oppopc      ] := $00000000;  
   opcbas[opprefetch  ] := $00000000;  
   opcbas[opprefetcha ] := $00000000;  
   opcbas[oprd        ] := $00000000;  
   opcbas[oprdpr      ] := $00000000;  
   opcbas[oprestore   ] := $00000000;  
   opcbas[oprestored  ] := $00000000;  
   opcbas[opretry     ] := $00000000;
   opcbas[opret       ] := $00000000;  
   opcbas[opretl      ] := $00000000;  
   opcbas[opreturn    ] := $00000000;  
   opcbas[opsave      ] := $00000000;  
   opcbas[opsaved     ] := $00000000;  
   opcbas[opsdiv      ] := $00000000;  
   opcbas[opsdivcc    ] := $00000000;  
   opcbas[opsdivx     ] := $00000000;  
   opcbas[opsethi     ] := $00000000;  
   opcbas[opset       ] := $00000000;  
   opcbas[opsetuw     ] := $00000000;  
   opcbas[opsetsw     ] := $00000000;  
   opcbas[opsetx      ] := $00000000;  
   opcbas[opsignx     ] := $00000000;  
   opcbas[opsir       ] := $00000000;  
   opcbas[opsll       ] := $00000000;  
   opcbas[opsllx      ] := $00000000;  
   opcbas[opsmul      ] := $00000000;  
   opcbas[opsmulcc    ] := $00000000;  
   opcbas[opsra       ] := $00000000;  
   opcbas[opsrax      ] := $00000000;  
   opcbas[opsrl       ] := $00000000;  
   opcbas[opsrlx      ] := $00000000;  
   opcbas[opstb       ] := $00000000;  
   opcbas[opstba      ] := $00000000;  
   opcbas[opstub      ] := $00000000;  
   opcbas[opstuba     ] := $00000000;  
   opcbas[opstsb      ] := $00000000;  
   opcbas[opstsba     ] := $00000000;  
   opcbas[opstbar     ] := $00000000;  
   opcbas[opstd       ] := $00000000;  
   opcbas[opstda      ] := $00000000;  
   opcbas[opst        ] := $00000000;  
   opcbas[opsta       ] := $00000000;  
   opcbas[opsth       ] := $00000000;  
   opcbas[opstha      ] := $00000000;  
   opcbas[opstuh      ] := $00000000;  
   opcbas[opstuha     ] := $00000000;  
   opcbas[opstsh      ] := $00000000;  
   opcbas[opstsha     ] := $00000000;  
   opcbas[opstq       ] := $00000000;  
   opcbas[opstqa      ] := $00000000;  
   opcbas[opstw       ] := $00000000;  
   opcbas[opstwa      ] := $00000000;  
   opcbas[opstuw      ] := $00000000;  
   opcbas[opstuwa     ] := $00000000;  
   opcbas[opstsw      ] := $00000000;  
   opcbas[opstswa     ] := $00000000;  
   opcbas[opstx       ] := $00000000;  
   opcbas[opstxa      ] := $00000000;  
   opcbas[opsub       ] := $00000000;  
   opcbas[opsubcc     ] := $00000000;  
   opcbas[opsubc      ] := $00000000;  
   opcbas[opsubccc    ] := $00000000;  
   opcbas[opswap      ] := $00000000;  
   opcbas[opswapa     ] := $00000000;  
   opcbas[optaddcc    ] := $00000000;  
   opcbas[optaddcctv  ] := $00000000;  
   opcbas[opta        ] := $00000000;  
   opcbas[optn        ] := $00000000;  
   opcbas[optne       ] := $00000000;  
   opcbas[optnz       ] := $00000000;  
   opcbas[opte        ] := $00000000;  
   opcbas[optz        ] := $00000000;  
   opcbas[optg        ] := $00000000;  
   opcbas[optle       ] := $00000000;  
   opcbas[optge       ] := $00000000;  
   opcbas[optl        ] := $00000000;  
   opcbas[optgu       ] := $00000000;  
   opcbas[optleu      ] := $00000000;  
   opcbas[optcc       ] := $00000000;  
   opcbas[optgeu      ] := $00000000;  
   opcbas[optcs       ] := $00000000;  
   opcbas[optlu       ] := $00000000;  
   opcbas[optpos      ] := $00000000;  
   opcbas[optneg      ] := $00000000;  
   opcbas[optvc       ] := $00000000;  
   opcbas[optvs       ] := $00000000;
   opcbas[optst       ] := $00000000;  
   opcbas[optsubcc    ] := $00000000;  
   opcbas[optsubcctv  ] := $00000000;  
   opcbas[opudiv      ] := $00000000;  
   opcbas[opudivcc    ] := $00000000;  
   opcbas[opudivx     ] := $00000000;  
   opcbas[opumul      ] := $00000000;  
   opcbas[opumulcc    ] := $00000000;  
   opcbas[opwr        ] := $00000000;  
   opcbas[opwrpr      ] := $00000000;  
   opcbas[opxor       ] := $00000000;  
   opcbas[opxorcc     ] := $00000000;  
   opcbas[opxnor      ] := $00000000;  
   opcbas[opxnorcc    ] := $00000000

end;

{******************************************************************************

Process register code

Checks if the next label matches any one of the simple register labels. If not,
the null register code is returned. This means that the operand is a complex,
or is invalid. If a register is found, the input position is set to past the
register label. Otherwise, the input position is left unchanged.

******************************************************************************}

procedure regcod(var reg: regc); { register return }

var inpsav: inpinx; { input postion save for backtrack }

begin

   reg := rgnl; { set null register }
   inpsav := cmdrot^.inp; { save current input position }
   skpspc; { skip input spaces }
   if chkchr = '%' then begin { has a register leader }

      getchr; { skip '%' }
      if alpha(chkchr) then begin { possible register }

         getlab; { get register }
         if labequ(labbuf, 'r0        ') then reg := rgr0 { r0 }
         else if labequ(labbuf, 'r1        ') then reg := rgr1 { r1 }
         else if labequ(labbuf, 'r2        ') then reg := rgr2 { r2 }
         else if labequ(labbuf, 'r3        ') then reg := rgr3 { r3 }
         else if labequ(labbuf, 'r4        ') then reg := rgr4 { r4 }
         else if labequ(labbuf, 'r5        ') then reg := rgr5 { r5 }
         else if labequ(labbuf, 'r6        ') then reg := rgr6 { r6 }
         else if labequ(labbuf, 'r7        ') then reg := rgr7 { r7 }
         else if labequ(labbuf, 'r8        ') then reg := rgr8 { r8 }
         else if labequ(labbuf, 'r9        ') then reg := rgr9 { r9 }
         else if labequ(labbuf, 'r10        ') then reg := rgr10 { r10 }
         else if labequ(labbuf, 'r11        ') then reg := rgr11 { r11 }
         else if labequ(labbuf, 'r12        ') then reg := rgr12 { r12 }
         else if labequ(labbuf, 'r13        ') then reg := rgr13 { r13 }
         else if labequ(labbuf, 'r14        ') then reg := rgr14 { r14 }
         else if labequ(labbuf, 'r15        ') then reg := rgr15 { r15 }
         else if labequ(labbuf, 'r16        ') then reg := rgr16 { r16 }
         else if labequ(labbuf, 'r17        ') then reg := rgr17 { r17 }
         else if labequ(labbuf, 'r18        ') then reg := rgr18 { r18 }
         else if labequ(labbuf, 'r19        ') then reg := rgr19 { r19 }
         else if labequ(labbuf, 'r20        ') then reg := rgr20 { r20 }
         else if labequ(labbuf, 'r21        ') then reg := rgr21 { r21 }
         else if labequ(labbuf, 'r22        ') then reg := rgr22 { r22 }
         else if labequ(labbuf, 'r23        ') then reg := rgr23 { r23 }
         else if labequ(labbuf, 'r24        ') then reg := rgr24 { r24 }
         else if labequ(labbuf, 'r25        ') then reg := rgr25 { r25 }
         else if labequ(labbuf, 'r26        ') then reg := rgr26 { r26 }
         else if labequ(labbuf, 'r27        ') then reg := rgr27 { r27 }
         else if labequ(labbuf, 'r28        ') then reg := rgr28 { r28 }
         else if labequ(labbuf, 'r29        ') then reg := rgr29 { r29 }
         else if labequ(labbuf, 'r30        ') then reg := rgr30 { r30 }
         else if labequ(labbuf, 'r31        ') then reg := rgr31 { r31 }
         else if labequ(labbuf, 'g0         ') then reg := rgr0 { g0 }
         else if labequ(labbuf, 'g1         ') then reg := rgr1 { g1 }
         else if labequ(labbuf, 'g2         ') then reg := rgr2 { g2 }
         else if labequ(labbuf, 'g3         ') then reg := rgr3 { g3 }
         else if labequ(labbuf, 'g4         ') then reg := rgr4 { g4 }
         else if labequ(labbuf, 'g5         ') then reg := rgr5 { g5 }
         else if labequ(labbuf, 'g6         ') then reg := rgr6 { g6 }
         else if labequ(labbuf, 'g7         ') then reg := rgr7 { g7 }
         else if labequ(labbuf, 'o0         ') then reg := rgr8 { o0 }
         else if labequ(labbuf, 'o1         ') then reg := rgr9 { o1 }
         else if labequ(labbuf, 'o2         ') then reg := rgr20 { o2 }
         else if labequ(labbuf, 'o3         ') then reg := rgr11 { o3 }
         else if labequ(labbuf, 'o4         ') then reg := rgr12 { o4 }
         else if labequ(labbuf, 'o5         ') then reg := rgr13 { o5 }
         else if labequ(labbuf, 'o6         ') then reg := rgr14 { o6 }
         else if labequ(labbuf, 'o7         ') then reg := rgr15 { o7 }
         else if labequ(labbuf, 'l0         ') then reg := rgr16 { l0 }
         else if labequ(labbuf, 'l1         ') then reg := rgr17 { l1 }
         else if labequ(labbuf, 'l2         ') then reg := rgr18 { l2 }
         else if labequ(labbuf, 'l3         ') then reg := rgr19 { l3 }
         else if labequ(labbuf, 'l4         ') then reg := rgr20 { l4 }
         else if labequ(labbuf, 'l5         ') then reg := rgr21 { l5 }
         else if labequ(labbuf, 'l6         ') then reg := rgr22 { l6 }
         else if labequ(labbuf, 'l7         ') then reg := rgr23 { l7 }
         else if labequ(labbuf, 'i0         ') then reg := rgr24 { i0 }
         else if labequ(labbuf, 'i1         ') then reg := rgr25 { i1 }
         else if labequ(labbuf, 'i2         ') then reg := rgr26 { i2 }
         else if labequ(labbuf, 'i3         ') then reg := rgr27 { i3 }
         else if labequ(labbuf, 'i4         ') then reg := rgr28 { i4 }
         else if labequ(labbuf, 'i5         ') then reg := rgr29 { i5 }
         else if labequ(labbuf, 'i6         ') then reg := rgr30 { i6 }
         else if labequ(labbuf, 'i7         ') then reg := rgr31 { i7 }
         else if labequ(labbuf, 'fp         ') then reg := rgr30 { fp }
         else if labequ(labbuf, 'sp         ') then reg := rgr14 { sp }
         else if labequ(labbuf, 'f0         ') then reg := rgf0 { f0 }
         else if labequ(labbuf, 'f1         ') then reg := rgf1 { f1 }
         else if labequ(labbuf, 'f2         ') then reg := rgf2 { f2 }
         else if labequ(labbuf, 'f3         ') then reg := rgf3 { f3 }
         else if labequ(labbuf, 'f4         ') then reg := rgf4 { f4 }
         else if labequ(labbuf, 'f5         ') then reg := rgf5 { f5 }
         else if labequ(labbuf, 'f6         ') then reg := rgf6 { f6 }
         else if labequ(labbuf, 'f7         ') then reg := rgf7 { f7 }
         else if labequ(labbuf, 'f8         ') then reg := rgf8 { f8 }
         else if labequ(labbuf, 'f9         ') then reg := rgf9 { f9 }
         else if labequ(labbuf, 'f10        ') then reg := rgf10 { f10 }
         else if labequ(labbuf, 'f11        ') then reg := rgf11 { f11 }
         else if labequ(labbuf, 'f12        ') then reg := rgf12 { f12 }
         else if labequ(labbuf, 'f13        ') then reg := rgf13 { f13 }
         else if labequ(labbuf, 'f14        ') then reg := rgf14 { f14 }
         else if labequ(labbuf, 'f15        ') then reg := rgf15 { f15 }
         else if labequ(labbuf, 'f16        ') then reg := rgf16 { f16 }
         else if labequ(labbuf, 'f17        ') then reg := rgf17 { f17 }
         else if labequ(labbuf, 'f18        ') then reg := rgf18 { f18 }
         else if labequ(labbuf, 'f19        ') then reg := rgf19 { f19 }
         else if labequ(labbuf, 'f20        ') then reg := rgf20 { f20 }
         else if labequ(labbuf, 'f21        ') then reg := rgf21 { f21 }
         else if labequ(labbuf, 'f22        ') then reg := rgf22 { f22 }
         else if labequ(labbuf, 'f23        ') then reg := rgf23 { f23 }
         else if labequ(labbuf, 'f24        ') then reg := rgf24 { f24 }
         else if labequ(labbuf, 'f25        ') then reg := rgf25 { f25 }
         else if labequ(labbuf, 'f26        ') then reg := rgf26 { f26 }
         else if labequ(labbuf, 'f27        ') then reg := rgf27 { f27 }
         else if labequ(labbuf, 'f28        ') then reg := rgf28 { f28 }
         else if labequ(labbuf, 'f29        ') then reg := rgf29 { f29 }
         else if labequ(labbuf, 'f30        ') then reg := rgf30 { f30 }
         else if labequ(labbuf, 'f31        ') then reg := rgf31 { f31 }
         else if labequ(labbuf, 'f32        ') then reg := rgf32 { f32 }
         else if labequ(labbuf, 'f33        ') then reg := rgf33 { f33 }
         else if labequ(labbuf, 'f34        ') then reg := rgf34 { f34 }
         else if labequ(labbuf, 'f35        ') then reg := rgf35 { f35 }
         else if labequ(labbuf, 'f36        ') then reg := rgf36 { f36 }
         else if labequ(labbuf, 'f37        ') then reg := rgf37 { f37 }
         else if labequ(labbuf, 'f38        ') then reg := rgf38 { f38 }
         else if labequ(labbuf, 'f39        ') then reg := rgf39 { f39 }
         else if labequ(labbuf, 'f40        ') then reg := rgf40 { f40 }
         else if labequ(labbuf, 'f41        ') then reg := rgf41 { f41 }
         else if labequ(labbuf, 'f42        ') then reg := rgf42 { f42 }
         else if labequ(labbuf, 'f43        ') then reg := rgf43 { f43 }
         else if labequ(labbuf, 'f44        ') then reg := rgf44 { f44 }
         else if labequ(labbuf, 'f45        ') then reg := rgf45 { f45 }
         else if labequ(labbuf, 'f46        ') then reg := rgf46 { f46 }
         else if labequ(labbuf, 'f47        ') then reg := rgf47 { f47 }
         else if labequ(labbuf, 'f48        ') then reg := rgf48 { f48 }
         else if labequ(labbuf, 'f49        ') then reg := rgf49 { f49 }
         else if labequ(labbuf, 'f50        ') then reg := rgf50 { f50 }
         else if labequ(labbuf, 'f51        ') then reg := rgf51 { f51 }
         else if labequ(labbuf, 'f52        ') then reg := rgf52 { f52 }
         else if labequ(labbuf, 'f53        ') then reg := rgf53 { f53 }
         else if labequ(labbuf, 'f54        ') then reg := rgf54 { f54 }
         else if labequ(labbuf, 'f55        ') then reg := rgf55 { f55 }
         else if labequ(labbuf, 'f56        ') then reg := rgf56 { f56 }
         else if labequ(labbuf, 'f57        ') then reg := rgf57 { f57 }
         else if labequ(labbuf, 'f58        ') then reg := rgf58 { f58 }
         else if labequ(labbuf, 'f59        ') then reg := rgf59 { f59 }
         else if labequ(labbuf, 'f60        ') then reg := rgf60 { f60 }
         else if labequ(labbuf, 'f61        ') then reg := rgf61 { f61 }
         else if labequ(labbuf, 'f62        ') then reg := rgf62 { f62 }
         else if labequ(labbuf, 'f63        ') then reg := rgf63 { f63 }
         else if labequ(labbuf, 'icc        ') then reg := rgicc { icc }
         else if labequ(labbuf, 'xcc        ') then reg := rgxcc { xcc }
         else if labequ(labbuf, 'asi        ') then reg := rgasi { asi }
         else if labequ(labbuf, 'fsr        ') then reg := rgfsr { fsr }
         else if labequ(labbuf, 'asr16      ') then reg := rgasr16 { asr16 }
         else if labequ(labbuf, 'asr17      ') then reg := rgasr17 { asr17 }
         else if labequ(labbuf, 'asr18      ') then reg := rgasr18 { asr18 }
         else if labequ(labbuf, 'asr19      ') then reg := rgasr19 { asr19 }
         else if labequ(labbuf, 'asr20      ') then reg := rgasr20 { asr20 }
         else if labequ(labbuf, 'asr21      ') then reg := rgasr21 { asr21 }
         else if labequ(labbuf, 'asr22      ') then reg := rgasr22 { asr22 }
         else if labequ(labbuf, 'asr23      ') then reg := rgasr23 { asr23 }
         else if labequ(labbuf, 'asr24      ') then reg := rgasr24 { asr24 }
         else if labequ(labbuf, 'asr25      ') then reg := rgasr25 { asr25 }
         else if labequ(labbuf, 'asr26      ') then reg := rgasr26 { asr26 }
         else if labequ(labbuf, 'asr27      ') then reg := rgasr27 { asr27 }
         else if labequ(labbuf, 'asr28      ') then reg := rgasr28 { asr28 }
         else if labequ(labbuf, 'asr29      ') then reg := rgasr29 { asr29 }
         else if labequ(labbuf, 'asr30      ') then reg := rgasr30 { asr30 }
         else if labequ(labbuf, 'asr31      ') then reg := rgasr31 { asr31 }
         else if labequ(labbuf, 'y          ') then reg := rgy { y }
         else if labequ(labbuf, 'ccr        ') then reg := rgccr { ccr }
         else if labequ(labbuf, 'fprs       ') then reg := rgfprs { fprs }
         else if labequ(labbuf, 'pc         ') then reg := rgpc { pc }
         else if labequ(labbuf, 'tpc        ') then reg := rgtpc { tpc }
         else if labequ(labbuf, 'tnpc       ') then reg := rgtnpc { tnpc }
         else if labequ(labbuf, 'tstate     ') then reg := rgtstate { tstate }
         else if labequ(labbuf, 'tt         ') then reg := rgtt { tt }
         else if labequ(labbuf, 'tick       ') then reg := rgtick { tick }
         else if labequ(labbuf, 'tba        ') then reg := rgtba { tba }
         else if labequ(labbuf, 'pstate     ') then reg := rgpstate { pstate }
         else if labequ(labbuf, 'tl         ') then reg := rgtl { tl }
         else if labequ(labbuf, 'pil        ') then reg := rgpil { pil }
         else if labequ(labbuf, 'cwp        ') then reg := rgcwp { cwp }
         else if labequ(labbuf, 'cansave    ') then reg := rgcansave { cansave }
         else if labequ(labbuf, 'canrestore ') then reg := rgcanrestore { canrestore }
         else if labequ(labbuf, 'cleanwin   ') then reg := rgcleanwin { cleanwin }
         else if labequ(labbuf, 'otherwin   ') then reg := rgotherwin { otherwin }
         else if labequ(labbuf, 'wstate     ') then reg := rgwstate { wstate }
         else if labequ(labbuf, 'fq         ') then reg := rgfq { fq }
         else if labequ(labbuf, 'ver        ') then reg := rgver { ver }
         else cmdrot^.inp := inpsav { restore input position }

      end else cmdrot^.inp := inpsav { restore input position }

   end else cmdrot^.inp := inpsav { restore input postion }

end;

{******************************************************************************

Parse parameter

Parses a complete SPARC operand or address specification. Processes the
following forms:

        reg
        imm
        [reg]
        [reg+reg]
        [reg+imm]
        [reg]n
        [reg+reg]n
        [imm]%asi
        [reg]%asi
        [reg+imm]%asi

The first register/mode parameter gives either a simple register or a mode.
The second parameter is either a register offset, or an immediate offset.

When one of the short forms are processed:

        [reg]
        [reg]n
        [imm]%asi
        [reg]%asi

The second register or immediate is set to g0 or zero. This means that,
for example, and instruction that takes:

        [reg+reg]

Can simply treat:

        [reg]

As the same mode, with g0 for the second register.

******************************************************************************}

procedure parcod(var p: parrec); { parameter record }

{ parse [ind] construct }

procedure parind;

var c: char; { lookahead save }
    r: regc; { register code }

begin

   getchr; { skip '[' }
   regcod(p.m); { find type of next }
   if p.m = rgnl then begin { [addr] }

      nexpr(p.vl); { parse displacement }
      p.m := rgird; { set as [reg+imm] }
      p.rs1 := rgr0 { set 0 parameter register }

   end else begin { [reg..] }

      { check addressing register }
      if not (p.m in gprset) then prterr(eregt); { must be r0-r31 }
      p.rs1 := p.m; { default to [reg] }
      p.rs2 := rgr0; { set 0 parameter register }
      p.m := rgirr; { set [reg+reg] }
      skpspc; { skip spaces }
      if (chkchr = '+') or (chkchr = '-') then begin { [reg+/-reg/imm] }

         c := chkchr; { save sign }
         getchr; { skip '+'/'-' }
         regcod(p.rs2); { find type of next }
         if p.rs2 = rgnl then begin { [reg+imm] }

            nexpr(p.vl); { parse displacement }
            p.m := rgird { set as [reg+imm] }

         end else begin { it's [reg+reg] }

            { check addressing register }
            if not (p.rs2 in gprset) then prterr(eregt); { must be r0-r31 }
            if c <> '+' then 
               prterr(emodt) { cannot subtract the second register }

         end

      end

   end;
   skpspc; { skip spaces }
   prcnxt(']', erbkexp); { check ']' }
   if not endcmd and (chkchr <> ',') then begin 

      { Not end of statement or parameter, check for asi forms. }
      regcod(r); { find if %asi }
      if r = rgnl then begin { must be immediate }

         nexpr(p.vl); { parse displacement }
         if p.m <> rgirr then prterr(emodt); { wrong mode to promote }
         p.m := rgirri { set to [reg+reg]n }

      end else if r = rgasi then begin

         if p.m <> rgird then prterr(emodt); { wrong mode to promote }
         p.m := rgirda { set to [reg+imm]%asi }

      end else prterr(eregt) { bad use of register }

   end

end;

begin

   p.m := rgnl; { clear register or mode }
   p.rs1 := rgnl; { clear index registers }
   p.rs2 := rgnl;
   p.vl := nil; { clear value symbols }
   skpspc; { skip spaces }
   if chkchr = '[' then parind { indirect mode }
   else begin { immediate or register }

      regcod(p.m); { find type of next }
      if p.m = rgnl then begin 

         { Not a register, assume is immediate. nexpr will deliver an error if
           it's not. }
         nexpr(p.vl); { parse expression }
         skpspc; { skip spaces }
         p.m := rgimm { set to immediate mode }

      end

   end

end;

{******************************************************************************

Convert register

Converts a register to a 6 bit code. The code usually needs to be shifted
and placed in the instruction. Not every register has a code, some are embedded
in the instruction type.

******************************************************************************}

function reg(p: regc): byte;

var r: byte;

begin

   case p of

      rgnl, rgimm, rgirr, rgird, rgirri, rgirda: prterr(emodt); { wrong type }
      rgr0:         r := 0; { %r0/%g0 }
      rgr1:         r := 1; { %r1/%g1 }
      rgr2:         r := 2; { %r2/%g2 }
      rgr3:         r := 3; { %r3/%g3 }
      rgr4:         r := 4; { %r4/%g4 }
      rgr5:         r := 5; { %r5/%g5 }
      rgr6:         r := 6; { %r6/%g6 }
      rgr7:         r := 7; { %r7/%g7 }
      rgr8:         r := 8; { %r8/%o0 }
      rgr9:         r := 9; { %r9/%o1 }
      rgr10:        r := 10; { %r10/%o2 }
      rgr11:        r := 11; { %r11/%o3 }
      rgr12:        r := 12; { %r12/%o4 }
      rgr13:        r := 13; { %r13/%o5 }
      rgr14:        r := 14; { %r14/%o6 }
      rgr15:        r := 15; { %r15/%o7 }
      rgr16:        r := 16; { %r16/%l0 }
      rgr17:        r := 17; { %r17/%l1 }
      rgr18:        r := 18; { %r18/%l2 }
      rgr19:        r := 19; { %r19/%l3 }
      rgr20:        r := 20; { %r20/%l4 }
      rgr21:        r := 21; { %r21/%l5 }
      rgr22:        r := 22; { %r22/%l6 }
      rgr23:        r := 23; { %r23/%l7 }
      rgr24:        r := 24; { %r24/%i0 }
      rgr25:        r := 25; { %r25/%i1 }
      rgr26:        r := 26; { %r26/%i2 }
      rgr27:        r := 27; { %r27/%i3 }
      rgr28:        r := 28; { %r28/%i4 }
      rgr29:        r := 29; { %r29/%i5 }
      rgr30:        r := 30; { %r30/%i6 }
      rgr31:        r := 31; { %r31/%i7 }
      rgf0:         r := 0; { %f0 }
      rgf1:         r := 1; { %f1 }
      rgf2:         r := 2; { %f2 }
      rgf3:         r := 3; { %f3 }
      rgf4:         r := 4; { %f4 }
      rgf5:         r := 5; { %f5 }
      rgf6:         r := 6; { %f6 }
      rgf7:         r := 7; { %f7 }
      rgf8:         r := 8; { %f8 }
      rgf9:         r := 9; { %f9 }
      rgf10:        r := 10; { %f10 }
      rgf11:        r := 11; { %f11 }
      rgf12:        r := 12; { %f12 }
      rgf13:        r := 13; { %f13 }
      rgf14:        r := 14; { %f14 }
      rgf15:        r := 15; { %f15 }
      rgf16:        r := 16; { %f16 }
      rgf17:        r := 17; { %f17 }
      rgf18:        r := 18; { %f18 }
      rgf19:        r := 19; { %f19 }
      rgf20:        r := 20; { %f20 }
      rgf21:        r := 21; { %f21 }
      rgf22:        r := 22; { %f22 }
      rgf23:        r := 23; { %f23 }
      rgf24:        r := 24; { %f24 }
      rgf25:        r := 25; { %f25 }
      rgf26:        r := 26; { %f26 }
      rgf27:        r := 27; { %f27 }
      rgf28:        r := 28; { %f28 }
      rgf29:        r := 29; { %f29 }
      rgf30:        r := 30; { %f30 }
      rgf31:        r := 31; { %f31 }
      rgf32:        r := 32; { %f32 }
      rgf33:        r := 33; { %f33 }
      rgf34:        r := 34; { %f34 }
      rgf35:        r := 35; { %f35 }
      rgf36:        r := 36; { %f36 }
      rgf37:        r := 37; { %f37 }
      rgf38:        r := 38; { %f38 }
      rgf39:        r := 39; { %f39 }
      rgf40:        r := 40; { %f40 }
      rgf41:        r := 41; { %f41 }
      rgf42:        r := 42; { %f42 }
      rgf43:        r := 43; { %f43 }
      rgf44:        r := 44; { %f44 }
      rgf45:        r := 45; { %f45 }
      rgf46:        r := 46; { %f46 }
      rgf47:        r := 47; { %f47 }
      rgf48:        r := 48; { %f48 }
      rgf49:        r := 49; { %f49 }
      rgf50:        r := 50; { %f50 }
      rgf51:        r := 51; { %f51 }
      rgf52:        r := 52; { %f52 }
      rgf53:        r := 53; { %f53 }
      rgf54:        r := 54; { %f54 }
      rgf55:        r := 55; { %f55 }
      rgf56:        r := 56; { %f56 }
      rgf57:        r := 57; { %f57 }
      rgf58:        r := 58; { %f58 }
      rgf59:        r := 59; { %f59 }
      rgf60:        r := 60; { %f60 }
      rgf61:        r := 61; { %f61 }
      rgf62:        r := 62; { %f62 }
      rgf63:        r := 63; { %f63 }
      rgicc:        r := 0; { %icc }
      rgxcc:        r := 2; { %xcc }
      rgfcc0:       r := 0; { %fcc0 }
      rgfcc1:       r := 1; { %fcc1 }
      rgfcc2:       r := 2; { %fcc2 }
      rgfcc3:       r := 3; { %fcc3 }
      rgasi:        r := 3; { %asi }
      rgfsr:        r := 0; { %fsr }
      rgasr16:      r := 16; { %asr16 }
      rgasr17:      r := 17; { %asr17 }
      rgasr18:      r := 18; { %asr18 }
      rgasr19:      r := 19; { %asr19 }
      rgasr20:      r := 20; { %asr20 }
      rgasr21:      r := 21; { %asr21 }
      rgasr22:      r := 22; { %asr22 }
      rgasr23:      r := 23; { %asr23 }
      rgasr24:      r := 24; { %asr24 }
      rgasr25:      r := 25; { %asr25 }
      rgasr26:      r := 26; { %asr26 }
      rgasr27:      r := 27; { %asr27 }
      rgasr28:      r := 28; { %asr28 }
      rgasr29:      r := 29; { %asr29 }
      rgasr30:      r := 30; { %asr30 }
      rgasr31:      r := 31; { %asr31 }
      rgy:          r := 0; { %y }
      rgccr:        r := 2; { %ccr }
      rgfprs:       r := 6; { %fprs }
      rgpc:         r := 5; { %pc }
      rgtpc:        r := 0; { %tpc }
      rgtnpc:       r := 1; { %tnpc }
      rgtstate:     r := 2; { %tstate }
      rgtt:         r := 3; { %tt }
      rgtick:       r := 4; { %tick }
      rgtba:        r := 5; { %tba }
      rgpstate:     r := 6; { %pstate }
      rgtl:         r := 7; { %tl }
      rgpil:        r := 8; { %pil }
      rgcwp:        r := 9; { %cwp }
      rgcansave:    r := 10; { %cansave }
      rgcanrestore: r := 11; { %canrestore }
      rgcleanwin:   r := 12; { %cleanwin }
      rgotherwin:   r := 13; { %otherwin }
      rgwstate:     r := 14; { %wstate }
      rgfq:         r := 0; { %fq }
      rgver:        r := 0; { %ver }

   end;
   reg := r

end;

{******************************************************************************

Output 32 bit backed symbol

Outputs a symbolic value with 32 bit backing. as/ln has the facillity to
insert up to a 256 bit value anywhere in the object, on byte alignment. For
SPARC, we often need a 32 bit backed symbol value with the value at the lsbs
of the word.

******************************************************************************}

procedure gensym32(sym: symptr;   { value to generate }
                   bak: integer;  { background value }
                   im:  immode;   { insert mode }
                   len: integer); { number of bits }

var pad: integer; { padding count }
    p:   integer; { power }

begin

   pad := 32-len div 8; { find number of padding bytes to output }
   p := $1000000; { set byte extraction power }
   while pad > 0 do begin { output padding bytes }

      outbyt(bak div p and $ff); { output byte }
      p := p div $100 { move power one byte right }

   end;   
   gensym(sym, bak, true, imnorm, 0, 0, len)

end;

{******************************************************************************

Check symbol is absolute

Checks if the given symbol is absolute, that is, if it is defined and will
never change.

******************************************************************************}

function absolute(s: symptr): boolean;

begin

   { check symbol is defined, and not typed address or variable }
   absolute := s^.symdef and not s^.symadd and not s^.symvar

end;

{******************************************************************************

Check value fits within bitfield

Checks a given value fits into a given signed bit field. The number of bits
must be at least 2, since there must be a sign bit and at least one value bit.

******************************************************************************}

procedure chkbit(v, bits: integer);

var p: integer; { power }

begin

   p := 1; { set 1st power }
   while bits > 1 do p := p*2; { find target power }
   if (v < -p) or (v >= p) then prterr(epoor) { bad value }

end;

{******************************************************************************

Validate machine type

Validates that the current machine is equal to or greater than the given
requirement. The SPARC family is downward compatible, such that a given
instruction or mode that works on a low level processor will work on all 
processors above that. So specifying the first processor to contain a feature
as the requirement will validate all processors above that as well.

******************************************************************************}

procedure valmac(m: mach); { machine requirement }

begin

   if cmachine < m then prterr(emach)

end;

{******************************************************************************

Validate machine type specific

Validates that the current machine is equal to the given machine. There are
a few operators that are specific to a given machine. We validate that the
single appropriate processor is active.

******************************************************************************}

procedure valmacs(m: mach); { machine requirement }

begin

   if cmachine <> m then prterr(emach)

end;

{******************************************************************************

Validate floating point

Validates that the floating point instructions are enabled.

******************************************************************************}

procedure valflt;

begin

   if not float then prterr(emach)

end;

{******************************************************************************

Find reserved word

Finds the reserved code corresponding to a given label. The hash value is found 
for the label, then a sequential search of the entry list for a match with the 
label. The result is a code equvalent to the index for the matching label, or 0 
if none is found. See inires for more reserved table details.

******************************************************************************}

function fndres(view s: string) { label to find }
               : opcodet;       { resulting opcode }

var i: opcodet; { reserved table index }
    b: boolean;
    { free variant to convert opcodes to integers }
    r: record case boolean of

          false: (a: opcodet);
          true:  (b: integer)

       end;

begin

   r.b := hash(s, ord(opdefqw)); { find hash value }
   i := r.a;
   { traverse chain at hash entry looking for a match }
   b := labequ(s, ressym[i].reslab); { check equal }
   while not b and (ressym[i].reschn <> opnull) do begin { traverse }

      i := ressym[i].reschn; { next entry }
      b := labequ(s, ressym[i].reslab) { check equal }

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

******************************************************************************}

procedure mfactor(var sym: symptr);

begin

   factori(sym) { pass call to assembler main }

end;

begin

   { output sign - on }
   writeln;
   writeln('SPARC V9 Assembler vs. 1.0 Copyright (C) 2005 S. A. Moore');
   writeln;
   alignment := cpualign; { set CPU alignment }
   bigend := cpubigend; { set CPU endian status }
   wrdsiz := cpuwrdsiz; { set CPU word size }
   float := true; { set floating point instructions enabled }
   cmachine := mtv9; { default to SPARC V9 (most capable machine) }
   { set general purpose register set }
   gprset := [rgr0..rgr31];
   { set floating point register set }
   fprset := [rgf0..rgf63];
   { clear and initalize reserved table }
   for oi := opnull to opdeffq do begin

      labcpy(ressym[oi].reslab, '');
      ressym[oi].reschn := opnull

   end;
   resini; { initalize reserved symbols }
   basini { initalize opcode base codes }

end. { unit }

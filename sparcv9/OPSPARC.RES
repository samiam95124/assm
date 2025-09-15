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

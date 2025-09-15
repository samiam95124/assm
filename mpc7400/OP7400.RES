type  opcode = (opnull,opaddme,opvcmpgtuwr,opaddmer,opandcr,opaddco,
                opfmadd,opaddeo,opaddcr,opbcla,opadder,opfaddr,
                opfadds,opbcctr,opbcctrl,opaddis,opendif,opcrand,
                opaddze,opdcbi,opaddor,opeieio,optlbia,opdcbz,
                opandir,opbclrl,opdcbst,opextsb,opfnegr,opnandr,
                opextsh,opcmpli,opfloat,opdefqw,opdefvs,opcrorc,
                opextshr,opfcmpu,opfdivs,opfctiw,opmffsr,opsubfo,
                opeciwx,opcreqv,opfresr,opfsubs,opcrnor,opmfmsr,
                opecowx,opfmuls,opmfspr,opdivwo,opvcfsx,opfabsr,
                opdivwr,opfrspr,opcrxor,opdivwu,opfsqrt,opmullw,
                opdstst,opmtmsr,opfadd,oplbzux,oplvewx,opfcmpo,
                opstfsx,opvpkpx,opstswi,oplhzux,opsthux,opfdiv,
                opfdivr,opfmadds,opfmsub,opstvxl,opfmsubr,opfmulr,
                opfnabs,opfnmadd,opfnmaddr,opfselr,opfsqrtr,opstswx,
                oplwzux,opstwux,opmtfsb0,opmtfsb1,opfsub,opfsubr,
                opisync,oplfdux,oplfs,oplfsu,oplfsux,oplfsx,oplhau,
                oplhaux,oplhbrx,opb,oplswi,oplvebx,opdefbef,oplvehx,
                oplvxl,oplwarx,oplwbrx,opmcrfs,opmcrxr,opmfcr,
                opmffs,opmfsrin,opdeflef,opaddicr,opendmac,opmftb,
                opaddmeo,opcrandc,opmtcrf,opaddcor,opfmaddr,opaddeor,
                opmtfsf,opglobal,opmtspr,opmtsr,opfaddsr,opvaddfp,
                opcrnand,opaddzeo,opelseif,opmulhw,opaddzer,opalignp,
                opfnabsr,opmulhwr,opmulhwu,opmulhwur,opmulli,opandisr,
                opsubfco,opdssall,opdcbtst,opsubfcr,opvmrghb,opsubfer,
                opmullwr,opmtfsfi,opvmrglb,opfnmsub,opvmrghh,opnegr,
                opfdivsr,opfctiwr,opfmsubs,opmfvscr,opmtfsfr,opvmaxub,
                oprlwimi,opfsubsr,opvsubfp,opfctiwz,opextsbr,opfmulsr,
                opnego,opdivwor,opstvebx,opmtsrin,opdivwuo,opmtvscr,
                opmullwo,opdivwur,opcntlzw,opfsqrts,opvminsw,opstfiwx,
                opdststt,opnegor,opvmaxuw,opnorr,opstwbrx,opstwcxr,
                oporcr,opstfsux,opori,oporis,opvspltw,opstvewx,
                oprlwinm,oprlwnm,oprlwnmr,opsraw,opsrawr,opsrawi,
                opsrawir,opsrwr,opba,opstbu,opbc,opstbux,opmtfsb0r,
                opmtfsb1r,opstbx,opstfdu,opstfdux,opstfdx,opstfs,
                opbl,opif,opstfsu,opsth,opsthbrx,opsthx,opstvehx,
                opstvx,opsc,opsubf,opsubfr,opsubfor,opbendian,
                opsubfc,opsubfe,opsubfeo,opsubfic,opsubfme,opsubfmer,
                opor,opsubfze,opsubfzeo,oplendian,opaddmeor,opfnmadds,
                opsubfzeor,opsync,optlbie,opfmaddsr,optw,opvaddubm,
                opinclude,opvaddubs,opvadduhm,opvaddsbs,opvadduws,
                opaddzeor,opvand,opvandc,opvavgsb,opvaddshs,opvaddcuw,
                opvadduhs,opvavgsh,opsubfmeo,opvavgsw,opvlogefp,
                opsubfcor,opvavgub,opsubfeor,opvavguh,opvadduwm,
                opvavguw,opvcfux,opmtfsfir,opvaddsws,opfnmsubr,
                opfnmsubs,optlbsync,opvmuleub,opsubfzer,opfmsubsr,
                opvupkhsb,opvmulesh,opvcmpbfp,oprlwimir,opfrsqrte,
                opvmulosb,opfctiwzr,opvmuloub,oprlwinmr,opvcmpequb,
                opvupklsh,opvmulosh,opvsubcuw,opdivwuor,opvcmpgefpr,
                opmullwor,opvctsxs,opcntlzwr,opfsqrtsr,opvpkuhus,
                opvctuxs,opvmaddfp,opvsubuwm,opvupklpx,opvmaxfp,
                opvmaxsb,opbca,opvmaxsh,opvpkuwum,opadd,opvpkswss,
                opvmaxsw,opvpkswus,opvmaxuh,opvpkuwus,opbla,opvmhaddshs,
                opbcl,opvminfp,opand,opvminsb,oplha,oplfd,opvminsh,
                opm7400,opvminub,opneg,opvminuh,opvminuw,opvmladduhm,
                opvmrghw,opvmrglh,opcmp,oprfi,opvmrglw,opvmsumshs,
                oporc,opfmr,opvmulesb,opvmuleuh,oplbz,opstb,opdss,
                opdst,opeqv,opvmulouh,oplhz,opnor,oplmw,opvperm,
                opvsum4ubs,oporr,optwi,opvsl,opslw,opvor,opfnmaddsr,
                opxor,oplvx,opvsr,opsrw,oplwz,opstw,opvpkshss,
                opvpkshus,opvcmpgefp,opvpkuhum,opvsum2sws,opvrefp,
                opvrfim,opvrfin,opvrfip,opvrfiz,opvcmpbfpr,opvrlb,
                opvcmpeqfp,opsubfmeor,opvrlw,opvrsqrtefp,opvcmpgtsb,
                opvcmpgtfp,opvcmpgtub,opvcmpequh,opvsel,opvslb,
                opvcmpgtsh,opvsldoi,opvcmpgtuh,opvslh,opfnmsubsr,
                opvnmsubfp,opvexptefp,opvslo,opvmsummbm,opvspltb,
                opvsplth,opvspltisb,opvcmpequw,opfrsqrter,opvspltish,
                opvcmpgtsw,opvmsumubm,opvcmpgtuw,opvsrab,opvsrah,
                opvmsumshm,opdcba,opvmsumuhm,opaddc,opvsraw,opadde,
                opdcbf,opvsrb,opvmsumuhs,opaddi,opvsrh,opvsro,
                opvspltisw,opandc,opicbi,opaddo,opvsrw,opvsubsbs,
                opaddr,opfabs,opdcbt,opvsubshs,opvsubsws,opfneg,
                opnand,opvsububm,opbclr,opvsububs,opandr,opdefw,
                opvsubuhm,opmcrf,opcmpi,opfsel,oplfdu,opcmpl,oplhax,
                oplfdx,opvsubuhs,opfres,opstfd,opvect,opvsubuws,
                opfmul,opvsumsws,opcror,opfmrr,opmfsr,opvsum4sbs,
                opdivw,opfrsp,opvrlh,oplbzu,opeqvr,opdstt,oplbzx,
                oplvsl,opxori,oplhzu,opsthu,opvnor,oplhzx,oplvsr,
                opslwr,opvsum4shs,opvmhraddsh,opstmw,opvslw,opvupkhpx,
                oplswx,opvxor,opvupkhsh,opvupklsb,oplwzu,opstwu,
                opxorr,oplwzx,opstwx,opxoris,opnfloat,opnvect,
                opm601,opm750,opmacro,opvcmpeqfpr,opvcmpequbr,
                opequ,opsetequ,opvcmpgtsbr,opvcmpgtfpr,opvcmpgtubr,
                opvcmpequhr,opextern,opalignv,opvcmpgtshr,opelse,
                opvcmpgtuhr,opassm,opdefb,opdefps,opdefbe,opdefle,
                opdeff,opdefsf,opdeflf,opdefhw,opvcmpequwr,opdefdw,
                opaddic,opvcmpgtswr);

   ressym[opaddme    ].reslab := 'addme     ';
   ressym[opvcmpgtuwr].reslab := 'vcmpgtuwr ';
   ressym[opaddmer   ].reslab := 'addmer    ';
   ressym[opandcr    ].reslab := 'andcr     '; ressym[opandcr    ].reschn := opcrand;
   ressym[opaddco    ].reslab := 'addco     '; ressym[opaddco    ].reschn := opaddme;
   ressym[opfmadd    ].reslab := 'fmadd     ';
   ressym[opaddeo    ].reslab := 'addeo     ';
   ressym[opaddcr    ].reslab := 'addcr     ';
   ressym[opbcla     ].reslab := 'bcla      '; ressym[opbcla     ].reschn := opdcbi;
   ressym[opadder    ].reslab := 'adder     '; ressym[opadder    ].reschn := opdefle;
   ressym[opfaddr    ].reslab := 'faddr     '; ressym[opfaddr    ].reschn := opdeflf;
   ressym[opfadds    ].reslab := 'fadds     ';
   ressym[opbcctr    ].reslab := 'bcctr     '; ressym[opbcctr    ].reschn := opfabsr;
   ressym[opbcctrl   ].reslab := 'bcctrl    ';
   ressym[opaddis    ].reslab := 'addis     ';
   ressym[opendif    ].reslab := 'endif     ';
   ressym[opcrand    ].reslab := 'crand     '; ressym[opcrand    ].reschn := opdefsf;
   ressym[opaddze    ].reslab := 'addze     '; ressym[opaddze    ].reschn := opandcr;
   ressym[opdcbi     ].reslab := 'dcbi      ';
   ressym[opaddor    ].reslab := 'addor     '; ressym[opaddor    ].reschn := opfnabs;
   ressym[opeieio    ].reslab := 'eieio     ';
   ressym[optlbia    ].reslab := 'tlbia     '; ressym[optlbia    ].reschn := opvandc;
   ressym[opdcbz     ].reslab := 'dcbz      ';
   ressym[opandir    ].reslab := 'andir     '; ressym[opandir    ].reschn := opbcctr;
   ressym[opbclrl    ].reslab := 'bclrl     ';
   ressym[opdcbst    ].reslab := 'dcbst     '; ressym[opdcbst    ].reschn := optlbie;
   ressym[opextsb    ].reslab := 'extsb     '; ressym[opextsb    ].reschn := opfmulr;
   ressym[opfnegr    ].reslab := 'fnegr     '; ressym[opfnegr    ].reschn := opmacro;
   ressym[opnandr    ].reslab := 'nandr     '; ressym[opnandr    ].reschn := opsubfc;
   ressym[opextsh    ].reslab := 'extsh     '; ressym[opextsh    ].reschn := opmcrxr;
   ressym[opcmpli    ].reslab := 'cmpli     '; ressym[opcmpli    ].reschn := opfcmpo;
   ressym[opfloat    ].reslab := 'float     ';
   ressym[opdefqw    ].reslab := 'defqw     ';
   ressym[opdefvs    ].reslab := 'defvs     ';
   ressym[opcrorc    ].reslab := 'crorc     ';
   ressym[opextshr   ].reslab := 'extshr    '; ressym[opextshr   ].reschn := opstfdux;
   ressym[opfcmpu    ].reslab := 'fcmpu     '; ressym[opfcmpu    ].reschn := opfdivr;
   ressym[opfdivs    ].reslab := 'fdivs     '; ressym[opfdivs    ].reschn := opfselr;
   ressym[opfctiw    ].reslab := 'fctiw     '; ressym[opfctiw    ].reschn := opfmsub;
   ressym[opmffsr    ].reslab := 'mffsr     '; ressym[opmffsr    ].reschn := opvsrab;
   ressym[opsubfo    ].reslab := 'subfo     ';
   ressym[opeciwx    ].reslab := 'eciwx     '; ressym[opeciwx    ].reschn := oplhbrx;
   ressym[opcreqv    ].reslab := 'creqv     '; ressym[opcreqv    ].reschn := oplvebx;
   ressym[opfresr    ].reslab := 'fresr     '; ressym[opfresr    ].reschn := opfsubr;
   ressym[opfsubs    ].reslab := 'fsubs     '; ressym[opfsubs    ].reschn := oplfdux;
   ressym[opcrnor    ].reslab := 'crnor     '; ressym[opcrnor    ].reschn := opvrfim;
   ressym[opmfmsr    ].reslab := 'mfmsr     '; ressym[opmfmsr    ].reschn := opvrfin;
   ressym[opecowx    ].reslab := 'ecowx     '; ressym[opecowx    ].reschn := opextsb;
   ressym[opfmuls    ].reslab := 'fmuls     '; ressym[opfmuls    ].reschn := oplvehx;
   ressym[opmfspr    ].reslab := 'mfspr     ';
   ressym[opdivwo    ].reslab := 'divwo     '; ressym[opdivwo    ].reschn := opstfdx;
   ressym[opvcfsx    ].reslab := 'vcfsx     '; ressym[opvcfsx    ].reschn := opvperm;
   ressym[opfabsr    ].reslab := 'fabsr     '; ressym[opfabsr    ].reschn := opdefhw;
   ressym[opdivwr    ].reslab := 'divwr     '; ressym[opdivwr    ].reschn := opextsh;
   ressym[opfrspr    ].reslab := 'frspr     '; ressym[opfrspr    ].reschn := opmulhw;
   ressym[opcrxor    ].reslab := 'crxor     '; ressym[opcrxor    ].reschn := oplwarx;
   ressym[opdivwu    ].reslab := 'divwu     '; ressym[opdivwu    ].reschn := oplwbrx;
   ressym[opfsqrt    ].reslab := 'fsqrt     '; ressym[opfsqrt    ].reschn := oprlwnm;
   ressym[opmullw    ].reslab := 'mullw     '; ressym[opmullw    ].reschn := opvrfiz;
   ressym[opdstst    ].reslab := 'dstst     '; ressym[opdstst    ].reschn := oplfsux;
   ressym[opmtmsr    ].reslab := 'mtmsr     '; ressym[opmtmsr    ].reschn := opvsraw;
   ressym[opfadd     ].reslab := 'fadd      '; ressym[opfadd     ].reschn := opvmsumshs;
   ressym[oplbzux    ].reslab := 'lbzux     '; ressym[oplbzux    ].reschn := opstfsu;
   ressym[oplvewx    ].reslab := 'lvewx     '; ressym[oplvewx    ].reschn := opmtspr;
   ressym[opfcmpo    ].reslab := 'fcmpo     '; ressym[opfcmpo    ].reschn := opsubfe;
   ressym[opstfsx    ].reslab := 'stfsx     ';
   ressym[opvpkpx    ].reslab := 'vpkpx     ';
   ressym[opstswi    ].reslab := 'stswi     ';
   ressym[oplhzux    ].reslab := 'lhzux     ';
   ressym[opsthux    ].reslab := 'sthux     ';
   ressym[opfdiv     ].reslab := 'fdiv      '; ressym[opfdiv     ].reschn := opmftb;
   ressym[opfdivr    ].reslab := 'fdivr     '; ressym[opfdivr    ].reschn := opmcrfs;
   ressym[opfmadds   ].reslab := 'fmadds    ';
   ressym[opfmsub    ].reslab := 'fmsub     ';
   ressym[opstvxl    ].reslab := 'stvxl     ';
   ressym[opfmsubr   ].reslab := 'fmsubr    '; ressym[opfmsubr   ].reschn := opmfsrin;
   ressym[opfmulr    ].reslab := 'fmulr     '; ressym[opfmulr    ].reschn := opisync;
   ressym[opfnabs    ].reslab := 'fnabs     '; ressym[opfnabs    ].reschn := opdefdw;
   ressym[opfnmadd   ].reslab := 'fnmadd    ';
   ressym[opfnmaddr  ].reslab := 'fnmaddr   ';
   ressym[opfselr    ].reslab := 'fselr     '; ressym[opfselr    ].reschn := opmtcrf;
   ressym[opfsqrtr   ].reslab := 'fsqrtr    '; ressym[opfsqrtr   ].reschn := opmulhwu;
   ressym[opstswx    ].reslab := 'stswx     ';
   ressym[oplwzux    ].reslab := 'lwzux     ';
   ressym[opstwux    ].reslab := 'stwux     ';
   ressym[opmtfsb0   ].reslab := 'mtfsb0    ';
   ressym[opmtfsb1   ].reslab := 'mtfsb1    ';
   ressym[opfsub     ].reslab := 'fsub      '; ressym[opfsub     ].reschn := opsubf;
   ressym[opfsubr    ].reslab := 'fsubr     '; ressym[opfsubr    ].reschn := oplhaux;
   ressym[opisync    ].reslab := 'isync     '; ressym[opisync    ].reschn := opsrawi;
   ressym[oplfdux    ].reslab := 'lfdux     '; ressym[oplfdux    ].reschn := opmulli;
   ressym[oplfs      ].reslab := 'lfs       ';
   ressym[oplfsu     ].reslab := 'lfsu      '; ressym[oplfsu     ].reschn := opvsel;
   ressym[oplfsux    ].reslab := 'lfsux     ';
   ressym[oplfsx     ].reslab := 'lfsx      '; ressym[oplfsx     ].reschn := oporis;
   ressym[oplhau     ].reslab := 'lhau      ';
   ressym[oplhaux    ].reslab := 'lhaux     '; ressym[oplhaux    ].reschn := opsubfr;
   ressym[oplhbrx    ].reslab := 'lhbrx     '; ressym[oplhbrx    ].reschn := opmtfsf;
   ressym[opb        ].reslab := 'b         ';
   ressym[oplswi     ].reslab := 'lswi      ';
   ressym[oplvebx    ].reslab := 'lvebx     ';
   ressym[opdefbef   ].reslab := 'defbef    ';
   ressym[oplvehx    ].reslab := 'lvehx     '; ressym[oplvehx    ].reschn := opvrfip;
   ressym[oplvxl     ].reslab := 'lvxl      '; ressym[oplvxl     ].reschn := opmtsr;
   ressym[oplwarx    ].reslab := 'lwarx     ';
   ressym[oplwbrx    ].reslab := 'lwbrx     '; ressym[oplwbrx    ].reschn := opsrawr;
   ressym[opmcrfs    ].reslab := 'mcrfs     '; ressym[opmcrfs    ].reschn := opnegor;
   ressym[opmcrxr    ].reslab := 'mcrxr     '; ressym[opmcrxr    ].reschn := opvcfux;
   ressym[opmfcr     ].reslab := 'mfcr      ';
   ressym[opmffs     ].reslab := 'mffs      '; ressym[opmffs     ].reschn := opnegr;
   ressym[opmfsrin   ].reslab := 'mfsrin    '; ressym[opmfsrin   ].reschn := opsubfze;
   ressym[opdeflef   ].reslab := 'deflef    ';
   ressym[opaddicr   ].reslab := 'addicr    ';
   ressym[opendmac   ].reslab := 'endmac    ';
   ressym[opmftb     ].reslab := 'mftb      '; ressym[opmftb     ].reschn := opnego;
   ressym[opaddmeo   ].reslab := 'addmeo    '; ressym[opaddmeo   ].reschn := opfnmadd;
   ressym[opcrandc   ].reslab := 'crandc    ';
   ressym[opmtcrf    ].reslab := 'mtcrf     ';
   ressym[opaddcor   ].reslab := 'addcor    '; ressym[opaddcor   ].reschn := opaddmer;
   ressym[opfmaddr   ].reslab := 'fmaddr    ';
   ressym[opaddeor   ].reslab := 'addeor    '; ressym[opaddeor   ].reschn := opfmadds;
   ressym[opmtfsf    ].reslab := 'mtfsf     '; ressym[opmtfsf    ].reschn := opnvect;
   ressym[opglobal   ].reslab := 'global    ';
   ressym[opmtspr    ].reslab := 'mtspr     '; ressym[opmtspr    ].reschn := opstbux;
   ressym[opmtsr     ].reslab := 'mtsr      ';
   ressym[opfaddsr   ].reslab := 'faddsr    ';
   ressym[opvaddfp   ].reslab := 'vaddfp    ';
   ressym[opcrnand   ].reslab := 'crnand    ';
   ressym[opaddzeo   ].reslab := 'addzeo    ';
   ressym[opelseif   ].reslab := 'elseif    ';
   ressym[opmulhw    ].reslab := 'mulhw     ';
   ressym[opaddzer   ].reslab := 'addzer    '; ressym[opaddzer   ].reschn := opbcctrl;
   ressym[opalignp   ].reslab := 'alignp    ';
   ressym[opfnabsr   ].reslab := 'fnabsr    '; ressym[opfnabsr   ].reschn := opsubfic;
   ressym[opmulhwr   ].reslab := 'mulhwr    '; ressym[opmulhwr   ].reschn := opvmrglw;
   ressym[opmulhwu   ].reslab := 'mulhwu    '; ressym[opmulhwu   ].reschn := oprlwnmr;
   ressym[opmulhwur  ].reslab := 'mulhwur   '; ressym[opmulhwur  ].reschn := opvpkshus;
   ressym[opmulli    ].reslab := 'mulli     '; ressym[opmulli    ].reschn := opvrefp;
   ressym[opandisr   ].reslab := 'andisr    '; ressym[opandisr   ].reschn := opalignv;
   ressym[opsubfco   ].reslab := 'subfco    '; ressym[opsubfco   ].reschn := opsubfme;
   ressym[opdssall   ].reslab := 'dssall    ';
   ressym[opdcbtst   ].reslab := 'dcbtst    '; ressym[opdcbtst   ].reschn := opsubfeo;
   ressym[opsubfcr   ].reslab := 'subfcr    ';
   ressym[opvmrghb   ].reslab := 'vmrghb    ';
   ressym[opsubfer   ].reslab := 'subfer    ';
   ressym[opmullwr   ].reslab := 'mullwr    ';
   ressym[opmtfsfi   ].reslab := 'mtfsfi    '; ressym[opmtfsfi   ].reschn := opvavgsb;
   ressym[opvmrglb   ].reslab := 'vmrglb    ';
   ressym[opfnmsub   ].reslab := 'fnmsub    '; ressym[opfnmsub   ].reschn := opvavgub;
   ressym[opvmrghh   ].reslab := 'vmrghh    ';
   ressym[opnegr     ].reslab := 'negr      ';
   ressym[opfdivsr   ].reslab := 'fdivsr    ';
   ressym[opfctiwr   ].reslab := 'fctiwr    '; ressym[opfctiwr   ].reschn := opfmsubr;
   ressym[opfmsubs   ].reslab := 'fmsubs    '; ressym[opfmsubs   ].reschn := opvminfp;
   ressym[opmfvscr   ].reslab := 'mfvscr    '; ressym[opmfvscr   ].reschn := opsubfor;
   ressym[opmtfsfr   ].reslab := 'mtfsfr    '; ressym[opmtfsfr   ].reschn := opvmaxfp;
   ressym[opvmaxub   ].reslab := 'vmaxub    ';
   ressym[oprlwimi   ].reslab := 'rlwimi    ';
   ressym[opfsubsr   ].reslab := 'fsubsr    '; ressym[opfsubsr   ].reschn := opvminsh;
   ressym[opvsubfp   ].reslab := 'vsubfp    '; ressym[opvsubfp   ].reschn := opextern;
   ressym[opfctiwz   ].reslab := 'fctiwz    '; ressym[opfctiwz   ].reschn := opvmaxsh;
   ressym[opextsbr   ].reslab := 'extsbr    '; ressym[opextsbr   ].reschn := opsrawir;
   ressym[opfmulsr   ].reslab := 'fmulsr    '; ressym[opfmulsr   ].reschn := oprlwinm;
   ressym[opnego     ].reslab := 'nego      '; ressym[opnego     ].reschn := opvand;
   ressym[opdivwor   ].reslab := 'divwor    '; ressym[opdivwor   ].reschn := opsthbrx;
   ressym[opstvebx   ].reslab := 'stvebx    ';
   ressym[opmtsrin   ].reslab := 'mtsrin    ';
   ressym[opdivwuo   ].reslab := 'divwuo    '; ressym[opdivwuo   ].reschn := opextshr;
   ressym[opmtvscr   ].reslab := 'mtvscr    '; ressym[opmtvscr   ].reschn := opmulhwr;
   ressym[opmullwo   ].reslab := 'mullwo    '; ressym[opmullwo   ].reschn := opvavguw;
   ressym[opdivwur   ].reslab := 'divwur    '; ressym[opdivwur   ].reschn := opvsplth;
   ressym[opcntlzw   ].reslab := 'cntlzw    '; ressym[opcntlzw   ].reschn := opfsqrtr;
   ressym[opfsqrts   ].reslab := 'fsqrts    '; ressym[opfsqrts   ].reschn := opmullwr;
   ressym[opvminsw   ].reslab := 'vminsw    ';
   ressym[opstfiwx   ].reslab := 'stfiwx    ';
   ressym[opdststt   ].reslab := 'dststt    '; ressym[opdststt   ].reschn := opvmaxsw;
   ressym[opnegor    ].reslab := 'negor     ';
   ressym[opvmaxuw   ].reslab := 'vmaxuw    ';
   ressym[opnorr     ].reslab := 'norr      '; ressym[opnorr     ].reschn := opstbx;
   ressym[opstwbrx   ].reslab := 'stwbrx    ';
   ressym[opstwcxr   ].reslab := 'stwcxr    '; ressym[opstwcxr   ].reschn := opvctsxs;
   ressym[oporcr     ].reslab := 'orcr      '; ressym[oporcr     ].reschn := opvrlb;
   ressym[opstfsux   ].reslab := 'stfsux    '; ressym[opstfsux   ].reschn := opvctuxs;
   ressym[opori      ].reslab := 'ori       ';
   ressym[oporis     ].reslab := 'oris      '; ressym[oporis     ].reschn := opsraw;
   ressym[opvspltw   ].reslab := 'vspltw    ';
   ressym[opstvewx   ].reslab := 'stvewx    ';
   ressym[oprlwinm   ].reslab := 'rlwinm    '; ressym[oprlwinm   ].reschn := opvmaxuh;
   ressym[oprlwnm    ].reslab := 'rlwnm     ';
   ressym[oprlwnmr   ].reslab := 'rlwnmr    '; ressym[oprlwnmr   ].reschn := opstvehx;
   ressym[opsraw     ].reslab := 'sraw      '; ressym[opsraw     ].reschn := opsync;
   ressym[opsrawr    ].reslab := 'srawr     ';
   ressym[opsrawi    ].reslab := 'srawi     '; ressym[opsrawi    ].reschn := opstfdu;
   ressym[opsrawir   ].reslab := 'srawir    ';
   ressym[opsrwr     ].reslab := 'srwr      ';
   ressym[opba       ].reslab := 'ba        ';
   ressym[opstbu     ].reslab := 'stbu      ';
   ressym[opbc       ].reslab := 'bc        ';
   ressym[opstbux    ].reslab := 'stbux     ';
   ressym[opmtfsb0r  ].reslab := 'mtfsb0r   ';
   ressym[opmtfsb1r  ].reslab := 'mtfsb1r   ';
   ressym[opstbx     ].reslab := 'stbx      ';
   ressym[opstfdu    ].reslab := 'stfdu     ';
   ressym[opstfdux   ].reslab := 'stfdux    '; ressym[opstfdux   ].reschn := opvavgsw;
   ressym[opstfdx    ].reslab := 'stfdx     ';
   ressym[opstfs     ].reslab := 'stfs      ';
   ressym[opbl       ].reslab := 'bl        ';
   ressym[opif       ].reslab := 'if        ';
   ressym[opstfsu    ].reslab := 'stfsu     '; ressym[opstfsu    ].reschn := opxoris;
   ressym[opsth      ].reslab := 'sth       ';
   ressym[opsthbrx   ].reslab := 'sthbrx    '; ressym[opsthbrx   ].reschn := opvmrghw;
   ressym[opsthx     ].reslab := 'sthx      ';
   ressym[opstvehx   ].reslab := 'stvehx    ';
   ressym[opstvx     ].reslab := 'stvx      ';
   ressym[opsc       ].reslab := 'sc        ';
   ressym[opsubf     ].reslab := 'subf      ';
   ressym[opsubfr    ].reslab := 'subfr     ';
   ressym[opsubfor   ].reslab := 'subfor    '; ressym[opsubfor   ].reschn := opvavguh;
   ressym[opbendian  ].reslab := 'bendian   ';
   ressym[opsubfc    ].reslab := 'subfc     ';
   ressym[opsubfe    ].reslab := 'subfe     ';
   ressym[opsubfeo   ].reslab := 'subfeo    '; ressym[opsubfeo   ].reschn := opnfloat;
   ressym[opsubfic   ].reslab := 'subfic    ';
   ressym[opsubfme   ].reslab := 'subfme    ';
   ressym[opsubfmer  ].reslab := 'subfmer   ';
   ressym[opor       ].reslab := 'or        ';
   ressym[opsubfze   ].reslab := 'subfze    '; ressym[opsubfze   ].reschn := opvavgsh;
   ressym[opsubfzeo  ].reslab := 'subfzeo   '; ressym[opsubfzeo  ].reschn := opvadduws;
   ressym[oplendian  ].reslab := 'lendian   ';
   ressym[opaddmeor  ].reslab := 'addmeor   '; ressym[opaddmeor  ].reschn := opfnmaddr;
   ressym[opfnmadds  ].reslab := 'fnmadds   ';
   ressym[opsubfzeor ].reslab := 'subfzeor  ';
   ressym[opsync     ].reslab := 'sync      '; ressym[opsync     ].reschn := opvslh;
   ressym[optlbie    ].reslab := 'tlbie     ';
   ressym[opfmaddsr  ].reslab := 'fmaddsr   ';
   ressym[optw       ].reslab := 'tw        '; ressym[optw       ].reschn := opvmaddfp;
   ressym[opvaddubm  ].reslab := 'vaddubm   ';
   ressym[opinclude  ].reslab := 'include   ';
   ressym[opvaddubs  ].reslab := 'vaddubs   '; ressym[opvaddubs  ].reschn := opvadduhm;
   ressym[opvadduhm  ].reslab := 'vadduhm   ';
   ressym[opvaddsbs  ].reslab := 'vaddsbs   ';
   ressym[opvadduws  ].reslab := 'vadduws   '; ressym[opvadduws  ].reschn := opvmulesb;
   ressym[opaddzeor  ].reslab := 'addzeor   '; ressym[opaddzeor  ].reschn := opvaddubs;
   ressym[opvand     ].reslab := 'vand      '; ressym[opvand     ].reschn := opelse;
   ressym[opvandc    ].reslab := 'vandc     ';
   ressym[opvavgsb   ].reslab := 'vavgsb    ';
   ressym[opvaddshs  ].reslab := 'vaddshs   ';
   ressym[opvaddcuw  ].reslab := 'vaddcuw   '; ressym[opvaddcuw  ].reschn := opvcmpbfp;
   ressym[opvadduhs  ].reslab := 'vadduhs   ';
   ressym[opvavgsh   ].reslab := 'vavgsh    '; ressym[opvavgsh   ].reschn := opvminsb;
   ressym[opsubfmeo  ].reslab := 'subfmeo   ';
   ressym[opvavgsw   ].reslab := 'vavgsw    ';
   ressym[opvlogefp  ].reslab := 'vlogefp   ';
   ressym[opsubfcor  ].reslab := 'subfcor   '; ressym[opsubfcor  ].reschn := opsubfmer;
   ressym[opvavgub   ].reslab := 'vavgub    ';
   ressym[opsubfeor  ].reslab := 'subfeor   ';
   ressym[opvavguh   ].reslab := 'vavguh    '; ressym[opvavguh   ].reschn := opvmaxsb;
   ressym[opvadduwm  ].reslab := 'vadduwm   ';
   ressym[opvavguw   ].reslab := 'vavguw    ';
   ressym[opvcfux    ].reslab := 'vcfux     ';
   ressym[opmtfsfir  ].reslab := 'mtfsfir   '; ressym[opmtfsfir  ].reschn := opm601;
   ressym[opvaddsws  ].reslab := 'vaddsws   ';
   ressym[opfnmsubr  ].reslab := 'fnmsubr   ';
   ressym[opfnmsubs  ].reslab := 'fnmsubs   '; ressym[opfnmsubs  ].reschn := opsubfzeo;
   ressym[optlbsync  ].reslab := 'tlbsync   ';
   ressym[opvmuleub  ].reslab := 'vmuleub   '; ressym[opvmuleub  ].reschn := opm750;
   ressym[opsubfzer  ].reslab := 'subfzer   ';
   ressym[opfmsubsr  ].reslab := 'fmsubsr   ';
   ressym[opvupkhsb  ].reslab := 'vupkhsb   ';
   ressym[opvmulesh  ].reslab := 'vmulesh   '; ressym[opvmulesh  ].reschn := opvsububm;
   ressym[opvcmpbfp  ].reslab := 'vcmpbfp   ';
   ressym[oprlwimir  ].reslab := 'rlwimir   '; ressym[oprlwimir  ].reschn := opvmuleuh;
   ressym[opfrsqrte  ].reslab := 'frsqrte   '; ressym[opfrsqrte  ].reschn := opvupklsb;
   ressym[opvmulosb  ].reslab := 'vmulosb   '; ressym[opvmulosb  ].reschn := opvsubsbs;
   ressym[opfctiwzr  ].reslab := 'fctiwzr   '; ressym[opfctiwzr  ].reschn := opvupkhsh;
   ressym[opvmuloub  ].reslab := 'vmuloub   '; ressym[opvmuloub  ].reschn := opvsububs;
   ressym[oprlwinmr  ].reslab := 'rlwinmr   ';
   ressym[opvcmpequb ].reslab := 'vcmpequb  ';
   ressym[opvupklsh  ].reslab := 'vupklsh   ';
   ressym[opvmulosh  ].reslab := 'vmulosh   '; ressym[opvmulosh  ].reschn := opvsubshs;
   ressym[opvsubcuw  ].reslab := 'vsubcuw   ';
   ressym[opdivwuor  ].reslab := 'divwuor   '; ressym[opdivwuor  ].reschn := opvmulouh;
   ressym[opvcmpgefpr].reslab := 'vcmpgefpr ';
   ressym[opmullwor  ].reslab := 'mullwor   '; ressym[opmullwor  ].reschn := opvpkshss;
   ressym[opvctsxs   ].reslab := 'vctsxs    ';
   ressym[opcntlzwr  ].reslab := 'cntlzwr   '; ressym[opcntlzwr  ].reschn := opmulhwur;
   ressym[opfsqrtsr  ].reslab := 'fsqrtsr   ';
   ressym[opvpkuhus  ].reslab := 'vpkuhus   '; ressym[opvpkuhus  ].reschn := opvupkhpx;
   ressym[opvctuxs   ].reslab := 'vctuxs    ';
   ressym[opvmaddfp  ].reslab := 'vmaddfp   ';
   ressym[opvsubuwm  ].reslab := 'vsubuwm   ';
   ressym[opvupklpx  ].reslab := 'vupklpx   ';
   ressym[opvmaxfp   ].reslab := 'vmaxfp    ';
   ressym[opvmaxsb   ].reslab := 'vmaxsb    '; ressym[opvmaxsb   ].reschn := opvminub;
   ressym[opbca      ].reslab := 'bca       '; ressym[opbca      ].reschn := opvsubsws;
   ressym[opvmaxsh   ].reslab := 'vmaxsh    '; ressym[opvmaxsh   ].reschn := opvminuh;
   ressym[opvpkuwum  ].reslab := 'vpkuwum   '; ressym[opvpkuwum  ].reschn := opvsubuws;
   ressym[opadd      ].reslab := 'add       ';
   ressym[opvpkswss  ].reslab := 'vpkswss   ';
   ressym[opvmaxsw   ].reslab := 'vmaxsw    '; ressym[opvmaxsw   ].reschn := opvminuw;
   ressym[opvpkswus  ].reslab := 'vpkswus   ';
   ressym[opvmaxuh   ].reslab := 'vmaxuh    ';
   ressym[opvpkuwus  ].reslab := 'vpkuwus   ';
   ressym[opbla      ].reslab := 'bla       ';
   ressym[opvmhaddshs].reslab := 'vmhaddshs '; ressym[opvmhaddshs].reschn := opvmladduhm;
   ressym[opbcl      ].reslab := 'bcl       '; ressym[opbcl      ].reschn := opvsumsws;
   ressym[opvminfp   ].reslab := 'vminfp    '; ressym[opvminfp   ].reschn := opvmrglh;
   ressym[opand      ].reslab := 'and       ';
   ressym[opvminsb   ].reslab := 'vminsb    ';
   ressym[oplha      ].reslab := 'lha       ';
   ressym[oplfd      ].reslab := 'lfd       ';
   ressym[opvminsh   ].reslab := 'vminsh    ';
   ressym[opm7400    ].reslab := 'm7400     ';
   ressym[opvminub   ].reslab := 'vminub    '; ressym[opvminub   ].reschn := opvsldoi;
   ressym[opneg      ].reslab := 'neg       ';
   ressym[opvminuh   ].reslab := 'vminuh    '; ressym[opvminuh   ].reschn := opsetequ;
   ressym[opvminuw   ].reslab := 'vminuw    ';
   ressym[opvmladduhm].reslab := 'vmladduhm '; ressym[opvmladduhm].reschn := opvrlw;
   ressym[opvmrghw   ].reslab := 'vmrghw    '; ressym[opvmrghw   ].reschn := opvspltb;
   ressym[opvmrglh   ].reslab := 'vmrglh    ';
   ressym[opcmp      ].reslab := 'cmp       ';
   ressym[oprfi      ].reslab := 'rfi       ';
   ressym[opvmrglw   ].reslab := 'vmrglw    ';
   ressym[opvmsumshs ].reslab := 'vmsumshs  ';
   ressym[oporc      ].reslab := 'orc       ';
   ressym[opfmr      ].reslab := 'fmr       '; ressym[opfmr      ].reschn := oplfs;
   ressym[opvmulesb  ].reslab := 'vmulesb   ';
   ressym[opvmuleuh  ].reslab := 'vmuleuh   ';
   ressym[oplbz      ].reslab := 'lbz       ';
   ressym[opstb      ].reslab := 'stb       ';
   ressym[opdss      ].reslab := 'dss       '; ressym[opdss      ].reschn := opori;
   ressym[opdst      ].reslab := 'dst       '; ressym[opdst      ].reschn := opequ;
   ressym[opeqv      ].reslab := 'eqv       ';
   ressym[opvmulouh  ].reslab := 'vmulouh   '; ressym[opvmulouh  ].reschn := opvpkuhum;
   ressym[oplhz      ].reslab := 'lhz       ';
   ressym[opnor      ].reslab := 'nor       '; ressym[opnor      ].reschn := opsth;
   ressym[oplmw      ].reslab := 'lmw       '; ressym[oplmw      ].reschn := opvsum4sbs;
   ressym[opvperm    ].reslab := 'vperm     ';
   ressym[opvsum4ubs ].reslab := 'vsum4ubs  ';
   ressym[oporr      ].reslab := 'orr       ';
   ressym[optwi      ].reslab := 'twi       ';
   ressym[opvsl      ].reslab := 'vsl       ';
   ressym[opslw      ].reslab := 'slw       '; ressym[opslw      ].reschn := opvsum4shs;
   ressym[opvor      ].reslab := 'vor       ';
   ressym[opfnmaddsr ].reslab := 'fnmaddsr  ';
   ressym[opxor      ].reslab := 'xor       ';
   ressym[oplvx      ].reslab := 'lvx       ';
   ressym[opvsr      ].reslab := 'vsr       ';
   ressym[opsrw      ].reslab := 'srw       ';
   ressym[oplwz      ].reslab := 'lwz       ';
   ressym[opstw      ].reslab := 'stw       ';
   ressym[opvpkshss  ].reslab := 'vpkshss   ';
   ressym[opvpkshus  ].reslab := 'vpkshus   ';
   ressym[opvcmpgefp ].reslab := 'vcmpgefp  ';
   ressym[opvpkuhum  ].reslab := 'vpkuhum   '; ressym[opvpkuhum  ].reschn := opvsubuhs;
   ressym[opvsum2sws ].reslab := 'vsum2sws  ';
   ressym[opvrefp    ].reslab := 'vrefp     ';
   ressym[opvrfim    ].reslab := 'vrfim     '; ressym[opvrfim    ].reschn := opvsrah;
   ressym[opvrfin    ].reslab := 'vrfin     ';
   ressym[opvrfip    ].reslab := 'vrfip     ';
   ressym[opvrfiz    ].reslab := 'vrfiz     ';
   ressym[opvcmpbfpr ].reslab := 'vcmpbfpr  ';
   ressym[opvrlb     ].reslab := 'vrlb      ';
   ressym[opvcmpeqfp ].reslab := 'vcmpeqfp  ';
   ressym[opsubfmeor ].reslab := 'subfmeor  '; ressym[opsubfmeor ].reschn := opvcmpequb;
   ressym[opvrlw     ].reslab := 'vrlw      '; ressym[opvrlw     ].reschn := opxorr;
   ressym[opvrsqrtefp].reslab := 'vrsqrtefp '; ressym[opvrsqrtefp].reschn := opdefbe;
   ressym[opvcmpgtsb ].reslab := 'vcmpgtsb  ';
   ressym[opvcmpgtfp ].reslab := 'vcmpgtfp  ';
   ressym[opvcmpgtub ].reslab := 'vcmpgtub  ';
   ressym[opvcmpequh ].reslab := 'vcmpequh  ';
   ressym[opvsel     ].reslab := 'vsel      ';
   ressym[opvslb     ].reslab := 'vslb      ';
   ressym[opvcmpgtsh ].reslab := 'vcmpgtsh  ';
   ressym[opvsldoi   ].reslab := 'vsldoi    ';
   ressym[opvcmpgtuh ].reslab := 'vcmpgtuh  ';
   ressym[opvslh     ].reslab := 'vslh      '; ressym[opvslh     ].reschn := opvsrb;
   ressym[opfnmsubsr ].reslab := 'fnmsubsr  '; ressym[opfnmsubsr ].reschn := opsubfzeor;
   ressym[opvnmsubfp ].reslab := 'vnmsubfp  ';
   ressym[opvexptefp ].reslab := 'vexptefp  ';
   ressym[opvslo     ].reslab := 'vslo      ';
   ressym[opvmsummbm ].reslab := 'vmsummbm  ';
   ressym[opvspltb   ].reslab := 'vspltb    ';
   ressym[opvsplth   ].reslab := 'vsplth    ';
   ressym[opvspltisb ].reslab := 'vspltisb  ';
   ressym[opvcmpequw ].reslab := 'vcmpequw  ';
   ressym[opfrsqrter ].reslab := 'frsqrter  ';
   ressym[opvspltish ].reslab := 'vspltish  ';
   ressym[opvcmpgtsw ].reslab := 'vcmpgtsw  ';
   ressym[opvmsumubm ].reslab := 'vmsumubm  ';
   ressym[opvcmpgtuw ].reslab := 'vcmpgtuw  '; ressym[opvcmpgtuw ].reschn := opvspltish;
   ressym[opvsrab    ].reslab := 'vsrab     ';
   ressym[opvsrah    ].reslab := 'vsrah     ';
   ressym[opvmsumshm ].reslab := 'vmsumshm  ';
   ressym[opdcba     ].reslab := 'dcba      ';
   ressym[opvmsumuhm ].reslab := 'vmsumuhm  ';
   ressym[opaddc     ].reslab := 'addc      ';
   ressym[opvsraw    ].reslab := 'vsraw     ';
   ressym[opadde     ].reslab := 'adde      ';
   ressym[opdcbf     ].reslab := 'dcbf      '; ressym[opdcbf     ].reschn := opfadd;
   ressym[opvsrb     ].reslab := 'vsrb      ';
   ressym[opvmsumuhs ].reslab := 'vmsumuhs  '; ressym[opvmsumuhs ].reschn := opdefb;
   ressym[opaddi     ].reslab := 'addi      '; ressym[opaddi     ].reschn := opbcla;
   ressym[opvsrh     ].reslab := 'vsrh      ';
   ressym[opvsro     ].reslab := 'vsro      ';
   ressym[opvspltisw ].reslab := 'vspltisw  '; ressym[opvspltisw ].reschn := opdeff;
   ressym[opandc     ].reslab := 'andc      ';
   ressym[opicbi     ].reslab := 'icbi      ';
   ressym[opaddo     ].reslab := 'addo      ';
   ressym[opvsrw     ].reslab := 'vsrw      ';
   ressym[opvsubsbs  ].reslab := 'vsubsbs   ';
   ressym[opaddr     ].reslab := 'addr      ';
   ressym[opfabs     ].reslab := 'fabs      ';
   ressym[opdcbt     ].reslab := 'dcbt      ';
   ressym[opvsubshs  ].reslab := 'vsubshs   ';
   ressym[opvsubsws  ].reslab := 'vsubsws   ';
   ressym[opfneg     ].reslab := 'fneg      ';
   ressym[opnand     ].reslab := 'nand      ';
   ressym[opvsububm  ].reslab := 'vsububm   ';
   ressym[opbclr     ].reslab := 'bclr      '; ressym[opbclr     ].reschn := opdcbz;
   ressym[opvsububs  ].reslab := 'vsububs   '; ressym[opvsububs  ].reschn := opvsubuhm;
   ressym[opandr     ].reslab := 'andr      ';
   ressym[opdefw     ].reslab := 'defw      ';
   ressym[opvsubuhm  ].reslab := 'vsubuhm   ';
   ressym[opmcrf     ].reslab := 'mcrf      '; ressym[opmcrf     ].reschn := opmfcr;
   ressym[opcmpi     ].reslab := 'cmpi      '; ressym[opcmpi     ].reschn := opfdiv;
   ressym[opfsel     ].reslab := 'fsel      '; ressym[opfsel     ].reschn := oplhau;
   ressym[oplfdu     ].reslab := 'lfdu      ';
   ressym[opcmpl     ].reslab := 'cmpl      '; ressym[opcmpl     ].reschn := opmffs;
   ressym[oplhax     ].reslab := 'lhax      ';
   ressym[oplfdx     ].reslab := 'lfdx      ';
   ressym[opvsubuhs  ].reslab := 'vsubuhs   ';
   ressym[opfres     ].reslab := 'fres      '; ressym[opfres     ].reschn := opfsub;
   ressym[opstfd     ].reslab := 'stfd      ';
   ressym[opvect     ].reslab := 'vect      ';
   ressym[opvsubuws  ].reslab := 'vsubuws   ';
   ressym[opfmul     ].reslab := 'fmul      '; ressym[opfmul     ].reschn := opassm;
   ressym[opvsumsws  ].reslab := 'vsumsws   ';
   ressym[opcror     ].reslab := 'cror      '; ressym[opcror     ].reschn := oporcr;
   ressym[opfmrr     ].reslab := 'fmrr      '; ressym[opfmrr     ].reschn := opvslb;
   ressym[opmfsr     ].reslab := 'mfsr      ';
   ressym[opvsum4sbs ].reslab := 'vsum4sbs  ';
   ressym[opdivw     ].reslab := 'divw      '; ressym[opdivw     ].reschn := oplfsu;
   ressym[opfrsp     ].reslab := 'frsp      ';
   ressym[opvrlh     ].reslab := 'vrlh      ';
   ressym[oplbzu     ].reslab := 'lbzu      '; ressym[oplbzu     ].reschn := oplfsx;
   ressym[opeqvr     ].reslab := 'eqvr      '; ressym[opeqvr     ].reschn := opstbu;
   ressym[opdstt     ].reslab := 'dstt      '; ressym[opdstt     ].reschn := oplswi;
   ressym[oplbzx     ].reslab := 'lbzx      '; ressym[oplbzx     ].reschn := opstfs;
   ressym[oplvsl     ].reslab := 'lvsl      '; ressym[oplvsl     ].reschn := opnorr;
   ressym[opxori     ].reslab := 'xori      ';
   ressym[oplhzu     ].reslab := 'lhzu      '; ressym[oplhzu     ].reschn := opvsrh;
   ressym[opsthu     ].reslab := 'sthu      '; ressym[opsthu     ].reschn := opvslo;
   ressym[opvnor     ].reslab := 'vnor      ';
   ressym[oplhzx     ].reslab := 'lhzx      '; ressym[oplhzx     ].reschn := oplvxl;
   ressym[oplvsr     ].reslab := 'lvsr      '; ressym[oplvsr     ].reschn := opsthx;
   ressym[opslwr     ].reslab := 'slwr      ';
   ressym[opvsum4shs ].reslab := 'vsum4shs  ';
   ressym[opvmhraddsh].reslab := 'vmhraddsh '; ressym[opvmhraddsh].reschn := opvsro;
   ressym[opstmw     ].reslab := 'stmw      '; ressym[opstmw     ].reschn := opvmhaddshs;
   ressym[opvslw     ].reslab := 'vslw      ';
   ressym[opvupkhpx  ].reslab := 'vupkhpx   ';
   ressym[oplswx     ].reslab := 'lswx      '; ressym[oplswx     ].reschn := opsrwr;
   ressym[opvxor     ].reslab := 'vxor      ';
   ressym[opvupkhsh  ].reslab := 'vupkhsh   ';
   ressym[opvupklsb  ].reslab := 'vupklsb   ';
   ressym[oplwzu     ].reslab := 'lwzu      '; ressym[oplwzu     ].reschn := opvsrw;
   ressym[opstwu     ].reslab := 'stwu      '; ressym[opstwu     ].reschn := opvcmpgefpr;
   ressym[opxorr     ].reslab := 'xorr      ';
   ressym[oplwzx     ].reslab := 'lwzx      '; ressym[oplwzx     ].reschn := opstvx;
   ressym[opstwx     ].reslab := 'stwx      ';
   ressym[opxoris    ].reslab := 'xoris     ';
   ressym[opnfloat   ].reslab := 'nfloat    ';
   ressym[opnvect    ].reslab := 'nvect     ';
   ressym[opm601     ].reslab := 'm601      ';
   ressym[opm750     ].reslab := 'm750      ';
   ressym[opmacro    ].reslab := 'macro     '; ressym[opmacro    ].reschn := opdefps;
   ressym[opvcmpeqfpr].reslab := 'vcmpeqfpr ';
   ressym[opvcmpequbr].reslab := 'vcmpequbr ';
   ressym[opequ      ].reslab := 'equ       ';
   ressym[opsetequ   ].reslab := 'setequ    ';
   ressym[opvcmpgtsbr].reslab := 'vcmpgtsbr ';
   ressym[opvcmpgtfpr].reslab := 'vcmpgtfpr ';
   ressym[opvcmpgtubr].reslab := 'vcmpgtubr ';
   ressym[opvcmpequhr].reslab := 'vcmpequhr ';
   ressym[opextern   ].reslab := 'extern    ';
   ressym[opalignv   ].reslab := 'alignv    ';
   ressym[opvcmpgtshr].reslab := 'vcmpgtshr ';
   ressym[opelse     ].reslab := 'else      ';
   ressym[opvcmpgtuhr].reslab := 'vcmpgtuhr ';
   ressym[opassm     ].reslab := 'assm      ';
   ressym[opdefb     ].reslab := 'defb      ';
   ressym[opdefps    ].reslab := 'defps     ';
   ressym[opdefbe    ].reslab := 'defbe     ';
   ressym[opdefle    ].reslab := 'defle     ';
   ressym[opdeff     ].reslab := 'deff      ';
   ressym[opdefsf    ].reslab := 'defsf     ';
   ressym[opdeflf    ].reslab := 'deflf     ';
   ressym[opdefhw    ].reslab := 'defhw     ';
   ressym[opvcmpequwr].reslab := 'vcmpequwr ';
   ressym[opdefdw    ].reslab := 'defdw     ';
   ressym[opaddic    ].reslab := 'addic     ';
   ressym[opvcmpgtswr].reslab := 'vcmpgtswr '; ressym[opvcmpgtswr].reschn := opvrsqrtefp;

{******************************************************************************
*                                                                             *
*                     MPC7400 ASSEMBLY MODULE VS. 1.0                         *
*                                                                             *
*                     Copyright (C) 2002 S. A. Moore                          *
*                          All rights reserved                                *
*                                                                             *
* Purpose:                                                                    *
*                                                                             *
* Provides MPC7400 specific operations for AS. Contains all code that is      *
* dependent on the MPC7400.                                                   *
*                                                                             *
******************************************************************************}

module macdef;

uses asdef; { generic definitions }

const

cpualign  = 4;    { 32 bit double word for MPC7400 }
cpubigend = true; { MPC7400 defaults to big endian, but can switch }
cpuwrdsiz = 4;    { MPC7400 has 32 bit words }

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

      restab = array [opcodet] of { reserved symbols }
                  record
                     reslab: lab;   { reserved symbol }
                     reschn: opcodet { chain to next }
                  end;
      { machine types }
      mach = (mt601,   { MPC601 }
              mt750,   { MPC750 }
              mt7400); { MPC7400 }
      { parameter records. these keep track of all the information needed to
        represent a parameter }
      parptr = ^ parrec; { pointer to parameter record }
      parrec = record { parameter record }

                  m: regc;    { main register or mode }
                  rb: regc;   { base register }
                  ri: regc;   { index register }
                  sc: symptr; { scale factor }
                  vl: symptr; { displacement or immediate }
                  as: opsize  { address mode size }

               end;

begin
end.

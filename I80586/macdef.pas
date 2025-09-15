{******************************************************************************
*                                                                             *
*                      80586 ASSEMBLY MODULE VS. 1.0                          *
*                                                                             *
*                     Copyright (C) 1994 S. A. Moore                          *
*                          All rights reserved                                *
*                                                                             *
* Purpose:                                                                    *
*                                                                             *
* Provides 80586 specific operations for AS. Contains all code that is        *
* dependent on the 80586.                                                     *
*                                                                             *
******************************************************************************}

module macdef;

uses asdef,  { generic definitions }
     opcdef; { opcode definitions }

const

cpualign  = 4;     { 32 bit double word for 80586 }
cpubigend = false; { 80586 is little endian }
cpuwrdsiz = 2;     { 80586 has 16 bit words }

type

      restab = array [opcodet] of { reserved symbols }
                  record
                     reslab: lab;   { reserved symbol }
                     reschn: opcodet { chain to next }
                  end;
      { registers in the 80586 }
      regc = (rgnl,     { none }
              rgal,     { al }
              rgah,     { ah } 
              rgbl,     { bl } 
              rgbh,     { bh } 
              rgcl,     { cl } 
              rgch,     { ch } 
              rgdl,     { dl } 
              rgdh,     { dh } 
              rgax,     { ax } 
              rgbx,     { bx }
              rgcx,     { cx } 
              rgdx,     { dx } 
              rgbp,     { bp } 
              rgsi,     { si } 
              rgdi,     { di } 
              rgsp,     { sp } 
              rgcs,     { cs } 
              rgss,     { ss } 
              rgds,     { ds } 
              rges,     { es } 
              rgfs,     { fs }
              rggs,     { gs } 
              rgeax,    { eax } 
              rgebx,    { ebx } 
              rgecx,    { ecx } 
              rgedx,    { edx } 
              rgebp,    { ebp } 
              rgesi,    { esi } 
              rgedi,    { edi } 
              rgesp,    { esp }
              rgcr0,    { cr0 } 
              rgcr2,    { cr2 } 
              rgcr3,    { cr3} 
              rgcr4,    { cr4 } 
              rgdr0,    { dr0 } 
              rgdr1,    { dr1 } 
              rgdr2,    { dr2 } 
              rgdr3,    { dr3 } 
              rgdr6,    { dr6 }
              rgdr7,    { dr7 } 
              rgtr4,    { tr4 }
              rgtr5,    { tr5 }
              rgtr6,    { tr6 }
              rgtr7,    { tr7 }
              rgst,     { st }
              rgstr,    { st(r) }
              rgimm,    { n } 
              rgiad,    { [n] } 
              rgir,     { [reg] }
              rgird,    { disp[reg] }
              rgirs,    { [reg*s] }
              rgirsd,   { disp[reg*s] }
              rgirr,    { [reg+reg] } 
              rgirrd,   { disp[reg+reg] } 
              rgirrs,   { [reg+reg*s] } 
              rgirrsd); { disp[reg+reg*s] }
      opsize = 0..10; { operation sizes }
      { machine types }
      mach = (mt86,   { 8086/8088 }
              mt186,  { 80186/80188 }
              mt286,  { 80286 }
              mt386,  { 80386 }
              mt486,  { 80486 }
              mt586); { 80586 }
      { parameter records. these keep track of all the information needed to
        represent a parameter }
      parptr = ^parrec; { pointer to parameter record }
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

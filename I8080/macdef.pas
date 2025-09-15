{*******************************************************************************
*                                                                              *
*                      8080 ASSEMBLY MODULE VS. 1.1                            *
*                                                                              *
*                     Copyright (C) 1994 S. A. Moore                           *
*                          All rights reserved                                 *
*                                                                              *
* Purpose:                                                                     *
*                                                                              *
* Provides 8080 specific operations for AS. Contains all code that is          *
* dependent on the 8080.                                                       *
*                                                                              *
*******************************************************************************}

module macdef;

uses asdef,  { generic definitions }
     opcdef; { opcode definitions }

const

cpualign  = 1;     { byte or none for 8080 }
cpubigend = false; { 8080 is little endian }
cpuwrdsiz = 2;     { 8080 has 16 bit words }

type

      { reserved symbols array }
      restab = array [opcodet] of { reserved symbols }
                  record
                     reslab: lab;   { reserved symbol }
                     reschn: opcodet { chain to next }
                  end;
      { registers in the 8080 }
      regc = (rgnl, rga, rgb, rgc, rgd, rge, rgh, rgl, rgm, rgsp,
              rgpsw, rgimm);

begin
end.

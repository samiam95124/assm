{******************************************************************************
*                                                                             *
*                      8051 ASSEMBLY MODULE VS. 1.0                           *
*                                                                             *
*                     Copyright (C) 1994 S. A. Moore                          *
*                          All rights reserved                                *
*                                                                             *
* Purpose:                                                                    *
*                                                                             *
* Provides 8051 specific operations for AS. Contains all code that is         *
* dependent on the 8080.                                                      *
*                                                                             *
******************************************************************************}

module macdef;

uses asdef,  { generic definitions }
     opcdef; { opcode definitions }

const

cpualign  = 1;     { byte or none for 8051 }
cpubigend = false; { 8051 is little endian }
cpuwrdsiz = 2;     { 8051 has 16 bit words }

type

      restab = array [opcodet] of { reserved symbols }
                  record
                     reslab: lab;   { reserved symbol }
                     reschn: opcodet { chain to next }
                  end;
      { registers in the 8051 }
      regc = (rgnl, rga, rgr0, rgr1, rgr2, rgr3, rgr4, rgr5, rgr6, rgr7, rgc, 
              rgab, rgimm, rgdir, rgr0i, rgr1i, rgdptr, rgdptri, rgadptri, 
              rgpc, rgapci);

begin
end.

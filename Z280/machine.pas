{*******************************************************************************
*                                                                              *
* Assembler custom header                                                      *
*                                                                              *
* This module defines the routines used in the custom module.                  *
*                                                                              *
*******************************************************************************}

module machine;

uses asdef,  { assembler definitions }
     macutl, { machine specific utilities }
     opcode; { opcode handlers }

{

Process opcode is the general custom processing call. The custom module
is called after the opcode is read, and the statement is ready to parse.

}

procedure prcopc; begin mprcopc end; { process opcode }

{

Expression callbacks.
In order to allow the custom module to perform special operators, we call
it to process expressions at all operator levels. The simplest processing
is to simply call back to the "internal" version of the routine. To process
custom operators, the custom module does some or all of the processing
on it's own.

}

procedure expr(var sym: symptr); begin mexpr(sym) end; { parse expression }
{ parse simple expression }
procedure sexpr(var sym: symptr); begin msexpr(sym) end;
procedure term(var sym: symptr); begin mterm(sym) end; { parse term }
procedure factor(var sym: symptr); begin mfactor(sym) end; { parse factor }

begin
end.

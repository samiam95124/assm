{******************************************************************************
*                                                                             *
* Assembler custom header                                                     *
*                                                                             *
* This module defines the routines used in the custom module.                 *
*                                                                             *
******************************************************************************}

module machine;

{

Process opcode is the general custom processing call. The custom module
is called after the opcode is read, and the statement is ready to parse.

}

procedure prcopc; begin end; { process opcode }

{

Expression callbacks.
In order to allow the custom module to perform special operators, we call
it to process expressions at all operator levels. The simplest processing
is to simply call back to the "internal" version of the routine. To process
custom operators, the custom module does some or all of the processing
on it's own.

}

procedure expr(var sym: symptr); begin end; { parse expression }
procedure sexpr(var sym: symptr); begin end; { parse simple expression }
procedure term(var sym: symptr); begin end; { parse term }
procedure factor(var sym: symptr); begin end; { parse factor }

begin
end.

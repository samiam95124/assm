{*******************************************************************************
*                                                                              *
*                       Symbols file printout program                          *
*                                                                              *
* Prints the formatted content of a given '.sym' file. The file is a command   *
* parameter.                                                                   *
*                                                                              *
*******************************************************************************}
 
program prtsym(command, output);

label closefiles, { exit with file close }
      exit;       { abort program }

const
 
   maxfil = 20;  { number of characters in a file name }
   maxlin = 200; { number of characters in input line }
   maxhex = 8;   { number of digits in integer }

type

   byte   = 0..255; { byte }
   filinx = 1..maxfil; { index for filenames }
   filnam = packed array [filinx] of char; { filename }
   ext    = packed array [1..3] of char; { filename extention }
   bytfil = file of byte; { raw byte file }
   lininx = 1..maxlin; { index for text line }
   line   = packed array [lininx] of char; { text line }
   { linker object definitions }
   lnkobj = (lnend,   { end of file }
             lnsym,   { symbol entry }
             lnanon,  { anonymous operator entry }
             lnpat,   { patch entry }
             lnrel,   { relocation entry }
             lnblk,   { block header }
             lnblke,  { block end }
             lnlin,   { line tracking source line difference number set }
             lnsrc);  { line tracking source file name }
 
var symfil:  bytfil;
    def:     boolean;
    i:       integer; { label index }
    b, t, l: byte;
    cmdlin:  line; { command line }
    cmdlen:  lininx; { command line length }
    cmdptr:  lininx; { command line pointer }
    symnam:  filnam; { name of symbol file }
    objcnt:  integer; { file object ordinal count }
   curlin:  integer; { current source line }
   curpgm:  integer; { current program counter }
   curvar:  integer; { current variable counter }

{*******************************************************************************

Abort program

Aborts the program.

*******************************************************************************}

procedure abort;

begin

   goto closefiles { go abort }

end;

{*******************************************************************************

Get command line

The command line is loaded to the command buffer.

*******************************************************************************}

procedure getcmd;

var i: lininx;  { command line index }

begin

   for i := 1 to maxlin do cmdlin[i] := ' '; { clear command line }
   i := 1; { set 1st command position }
   while not eoln(command) do begin { get command line characters }

      read(command, cmdlin[i]);
      if i = maxlin then begin { overflow }
 
         writeln('*** Command input line overflow');
         abort

      end;
      i := i + 1 { next character position }
   
   end;
   cmdlen := i-1; { set length of line }
   cmdptr := 1 { reset line pointer }

end;

{*******************************************************************************

Check end of line

Checks whether the input line position is at the end. This is indicated by 
cmdptr being at the extreme end of the input line.
Note that in order to ensure that this is true, a skip space to line end
should be done.

*******************************************************************************}

function endlin: boolean;

begin

   endlin := cmdptr > cmdlen { input pointer past end of line }

end;

{*******************************************************************************

Check next input character

The next character in the input buffer is returned. No advance
is made from the current position (succesive calls to this
procedure will yeild the same character).

*******************************************************************************}

function chkchr: char;

begin

   if not endlin then chkchr := cmdlin[cmdptr] { return current character }
   else chkchr := ' ' { else return space }

end;

{*******************************************************************************

Skip input character

Causes the current input character to be skipped, so that the next chkchr call
will return the next character. If endlin is true, no action will take place
(will not advance beyond end of line).

*******************************************************************************}

procedure getchr;

begin

   if not endlin then cmdptr := cmdptr+1 { advance position if not end }

end;

{*******************************************************************************

Skip input spaces or controls

Skips the input position past any spaces or controls. Will skip the end of 
line, loading the next line from the input. The view of the input is for each 
line to be terminated by an infinite series of blanks, which only this routine
will cross.

*******************************************************************************}

procedure skpspc;

begin

   while (chkchr = ' ') and not endlin do getchr { skip spaces, not end }

end;

{*******************************************************************************

Check digit

Checks wether the given character lies in the set ['0'..'9'].
Returns the status.

*******************************************************************************}

function digit(c: char)  { character to check }
              : boolean; { status of check }

begin

   digit := c in ['0'..'9']

end;

{*******************************************************************************

Check alphabetical

Checks if the given character lies in the set ['A'..'Z', 'a'..'z'] (is a
letter). Returns the status.

*******************************************************************************}

function alpha(c : char) { character to check }
              : boolean; { status }

begin

   alpha := c in ['A'..'Z', 'a'..'z']

end;

{*******************************************************************************

Parse file name

Parses a filename in the format:

     <filename> ::= [<letter>:]<letter>[<letter>/<digit>/'.']...
     <letter>   ::= 'a'..'z'/'A'..'Z'
     <digit>    ::= '0'..'9'

Obviously, this routine is CP/M dependent. If more than 10 total
characters are used (ffffff.eee), an error is generated.
The file string so parsed is returned in place, and an error
'mode' parameter indicates whether or not a file name parsing
error would be fatal to the assembly.

*******************************************************************************}

procedure parfil(var fn: filnam); { file name return }

var fi, i: filinx;    { file name index }
    p:     0..maxfil; { primary length }
    u:     integer;   { user number }

procedure getncr; { get name character }

begin

   if i > maxfil then begin { filename too long }

      writeln('*** Filename too long');
      abort
 
   end;
   fn[i] := chkchr; { place file character }
   getchr; { skip that }
   i := i + 1 { next }

end;

procedure getseq(max: filinx); { read name sequence }

var l: 0..maxfil;

begin

   l := 0; { initalize count }
   while alpha(chkchr) or digit(chkchr) do begin { filename }

      getncr; { get character }
      l := l + 1 { count }

   end;
   if l > max then begin { filename too long }

      writeln('*** Filename too long');
      abort
 
   end

end;

begin { parnam }

   skpspc; { skip spaces }
   for fi := 1 to maxfil do fn[fi] := ' '; { clear filename }
   i := 1; { initalize index }
   p := 1; { initalize primary count for first character }
   u := 0; { initalize user number }
   if not alpha(chkchr) then begin { must lead with alpha }

      writeln('*** Invalid filename');
      abort

   end;
   getncr; { get character }
   while digit(chkchr) do begin { process unit number }

      u := u*10 + (ord(chkchr) - ord('0')); { add to specification }
      getncr; { get character }
      p := p + 1 { since could be primary }

   end;
   if chkchr = ':' then begin { process unit specification }

      if u > 15 then begin { unit out of range }

         writeln('*** Invalid filename');
         abort

      end;
      getncr; { get character }
      if not alpha(chkchr) then begin { must lead off alpha }

         writeln('*** Invalid filename');
         abort

      end;
      p := 0 { re - initalize primary }

   end;
   if p > 8 then begin { too long }

      writeln('*** Filename too long');
      abort
 
   end;
   getseq(8 - p); { get rest of primary }
   if chkchr = '.' then begin { secondary }

      getncr; { get character }
      if not alpha(chkchr) or digit(chkchr) then begin { error }

         writeln('*** Invalid filename');
         abort

      end;
      getseq(3) { get secondary }

   end

end; { parnam }

{*******************************************************************************

Test filename contains an extention

Simply checks if '.' exists in the filename, which would indicate an extention
is present (in a properly parsed filename).

*******************************************************************************}

function isext(var f: filnam): boolean; { filename to check }

var i: filinx;  { index for filename }
    m: boolean; { match flag }

begin

   m := false; { set no extention found }
   for i := 1 to maxfil do if f[i] = '.' then m := true; { set found }
   isext := m { return match status }

end;

{*******************************************************************************

Append file extention

Appends a given extention, in place, to the given file name. The extention is 
usually in the form: 'ext'. The extention is placed within the file name at the
first space or period from the left hand side. This allows extention of either
an unextended filename or an extended one (in which case the new extention
simply overlays the old). The overlay is controlled via flag: if overwrite is
true, the extention will overwrite any existing, if not, any existing extention
will be left in place.
No checking is performed for a new filename that will overflow the allotted
filename length.
In the case of overflow, the filename will simply be truncated to the 8:3 
format.
Note: this routine is MS-DOS dependent.

*******************************************************************************}

procedure addext(var fn: filnam; { filename to extend }
                 e: ext;         { filename extention }
                 ovr: boolean);  { overwrite flag }

var i : 1..maxfil; { filename index }
    x : 1..3; { extention index }

begin

   i := 1; { initalize index }
   { skip to first character ' ' or '.' }
   while (fn[i] <> ' ') and (fn[i] <> '.') and 
         (i <> 9) do i := i + 1;
   if ovr or (fn[i] = ' ') then begin { plant extention }

      fn[i] := '.'; { place '.' }
      i := i + 1; { next }
      for x := 1 to 3 do begin { append file extention }

         fn[i] := e[x]; { transfer extention character }
         i := i + 1 { count }

      end

   end

end;

{*******************************************************************************

Print hex value

Prints the given unsigned value in hex, with the given number of digits.
Leading zeros are added to make up the width.

*******************************************************************************}

procedure prthex(f: byte; w: integer);
 
var i, j: byte;
    v:    integer;
    mv:   integer;
    s:    boolean;
 
begin

   s := w < 0; { save sign bit }
   w := w and maxint; { remove sign bit }
   for i := 1 to f do begin { output digits }

      v := w; { save word }
      mv := maxint; { save maxint }
      for j := 1 to f - i do 
         begin v := v div 16; mv := mv div 16 end; { extract digit }
      v := v mod 16; { mask }
      if (mv < 16) and s then v := v or 8; { add back sign bit }
      { convert ascii }
      if v >= 10 then v := v + (ord('A') - 10)
      else v := v + ord('0');
      write(chr(v)) { output }

   end

end;

{*******************************************************************************

Print flags

Read a symbols flag byte, then prints the contents symbolically.

*******************************************************************************}
 
procedure prtflg;
 
type flag = packed array [1..3] of char; { flag ascii }

var b:     byte;
    first: boolean; { first output flag }
 
procedure prtflg(f: flag);

begin

   if not first then write(' '); { space off }
   write(f); { output flag }
   first := false { set not first output }

end;
   
begin

   first := true; { set 1st flag output true }
   write('flags[');
   read(symfil, b); { read flag byte }
   if (b and $80) <> 0 then prtflg('???');
   if (b and $40) <> 0 then prtflg('???');
   if (b and $20) <> 0 then prtflg('???');
   if (b and $10) <> 0 then prtflg('var');
   if (b and $08) <> 0 then prtflg('ext');
   if (b and $04) <> 0 then prtflg('gbl');
   if (b and $02) <> 0 then prtflg('pgm');
   if (b and $01) <> 0 then prtflg('def');
   write('] ');
   def := odd(b) { set defined flag }

end;

{*******************************************************************************

Input variger

Inputs a variger to the given integer.
Varigers are of the following format:

   1. (byte) the tag byte.
   2-N. The variger value.

The tag byte values are:

   bit 7 - Low for integer number, high for float.
   bit 6 - Contains the sign of the integer. 
   bit 5 - Unused.
   bit 4 - Length of integer in bytes, 1-32, in -1 format.
   bit 3 -      ""              ""
   bit 2 -      ""              ""
   bit 1 -      ""              ""
   bit 0 -      ""              ""

The integer is converted by removing the sign bit and converting
to signed magnitude, then determining the byte size, then
outputting the tag and number.

*******************************************************************************}

procedure rdvar(var n: integer); { integer to output}

var t: byte;     { tag byte }
    s: integer;  { sign }
    b: byte;     { read byte holder }

begin

   read(symfil, t); { get tag byte }
   if (t and $80) <> 0 then begin { floating point }

      writeln('*** Floating point format not implemented');
      abort

   end;
   if (t and $40) <> 0 then s := -1 else s := 1; { set sign of value }
   if (t and $20) <> 0 then begin

      writeln('*** Bad symbol file format #1');
      abort

   end;
   t := (t and $1f)+1; { mask byte length and adjust }
   n := 0; { clear result }
   while t <> 0 do begin { read in bytes of value }

      n := n*256; { scale up bytes for big endian format }
      read(symfil, b); { get the next byte }
      n := n+b; { add in }
      t := t-1 { count bytes read }

   end;
   n := n*s { set sign of result }

end;

{*******************************************************************************

Print value

Reads and prints a value.

*******************************************************************************}

procedure prtval;
 
var n: integer;
 
begin

   rdvar(n); { get the value }
   prthex(maxhex, n) { print value }

end;

{*******************************************************************************

Print operation

Reads and prints an expression operation.

*******************************************************************************}

procedure prtop;

var b: byte;

begin

   write('op[');
   read(symfil, b); { get operation byte }
   if b > 12 then begin { bad code }

      writeln('*** Bad symbol file format #2');
      abort

   end;
   case b of { operation }

      0:  write('nop');
      1:  write('add');
      2:  write('sub');
      3:  write('mult');
      4:  write('div');
      5:  write('mod');
      6:  write('shl');
      7:  write('shr');
      8:  write('and');
      9:  write('or');
      10: write('xor');
      11: write('not');
      12: write('neg')

   end;
   write('] ')

end;

{*******************************************************************************

Print insertion type

Reads and prints an insertion type.

*******************************************************************************}

procedure prtit;

var t, b: byte;

begin

   write('it[');
   read(symfil, t); { get tagfield }
   if (t and $80) <> 0 then write('big ');
   case (t div $08) and $03 of { insertion mode }

      0: write('norm ');   { normal }
      1: write('off ');    { signed offset }
      2: write('ns off '); { non-standard offset }
      3: write('i seg ')   { Intel segment }

   end;
   if (t and $40) <> 0 then write('off ');
   write(t and $07:1, ':'); { output starting bit }
   read(symfil, b); { get bit length }
   write(b+1:1); { output }
   if (t and $20) <> 0 then begin { there is a constant offset }

      write(' cof: ');
      read(symfil, b); { get offset byte }
      write(b:1) { output }
      
   end;
   write('] ')

end;

{*******************************************************************************

Input stepped value

Inputs a value in stepped format. This is an unsigned format. If the value 
is < 255, then it is a single byte. If > 255, the value 255 is present, then a 
16 bit, big endian format number follows, with values < 65535. If it exceeds
that, the value 65536 is present, then we continue with a 3 byte, then finally
a 4 byte format. This system is most efficient where values output tend to be
small, with only occasional large values. We use it for the difference numbers
in the line file.

*******************************************************************************}

procedure inpstp(var n: integer);

var b: byte; { read byte holder }

begin

   read(symfil, b); { get 1st byte }
   n := b; { place as value }
   if n = 255 then begin { > 1 byte format }

      read(symfil, b); { get high byte }
      n := b*256; { place }
      read(symfil, b); { get low byte }
      n := n+b; { place }
      if n = 65535 then begin { > 2 byte format }

         read(symfil, b); { get high byte }
         n := b*65536; { place }
         read(symfil, b); { get mid byte }
         n := n+b*256; { place }
         read(symfil, b); { get low byte }
         n := n+b; { place }
         if n = 1677215 then begin { > 3 byte format }   

            read(symfil, b); { get high byte }
            n := b*1677216; { place }
            read(symfil, b); { get high byte }
            n := n+b*65536; { place }
            read(symfil, b); { get mid byte }
            n := n+b*256; { place }
            read(symfil, b); { get low byte }
            n := n+b { place }

         end

      end

   end

end;

begin

   writeln;
   writeln('Symbol file printer vs. 1.0.01 Copyright (C) 1994 S. A. Moore');
   writeln;

   objcnt := 1; { set 1st object }
   curlin := 0; { reset current source line }
   curpgm := 0; { reset current program counter }
   curvar := 0; { reset current variable counter }
   
   getcmd; { load command line }
   parfil(symnam); { get the name of the symbol file }
   if isext(symnam) then begin { extention exists }

      if not exists(symnam) then begin { no file }

         writeln('*** File does not exist');
         goto exit

      end

   end else begin { no extention, try our own }

      addext(symnam, 'sym', true); { search for file.opt first }
      if not exists(symnam) then begin { not found }

         writeln('*** File does not exist');
         goto exit

      end

   end;
   writeln('Contents of symbol file: ', symnam:0);
   writeln;
   assign(symfil, symnam); { open the symbols file }
   reset(symfil);
   
   repeat { read entries }

      read(symfil, t); { get entry type }
      if t > ord(lnsrc) then begin

         writeln('*** Bad symbol file format #3');
         abort

      end;
      write(objcnt:5, ': '); { output entry type }
      case lnkobj(t) of { entry type }

         lnsym: begin { symbol }

            write('Sym:  ');
            prtop; { print operation code }
            read(symfil, b); { get the string length }
            for i := 1 to b+1 do begin { read symbol characters }
            
               read(symfil, b); { get a character }
               write(chr(b)) { output }
            
            end;
            write(' '); { space off }
            prtflg; { output flags }
            if def then begin
            
              write('value[');
              prtval; { output value }
              write(']')
            
            end

         end;

         lnanon: begin { complex }

            write('Comp: ');
            prtop; { print operation code }
            prtflg; { output flags }
            if def then begin
            
              write('value[');
              prtval; { output value }
              write(']')
            
            end

         end;

         lnpat: begin { rld }
                        
            write('Pat:  ');
            prtit; { print insertion type }
            write('addr[');
            prtval; { output address }
            write(']')

         end;

         lnrel: begin { constant rld }

            write('RLD:  ');
            prtit; { print insertion type }
            write('addr[');
            prtval; { output address }
            write('] ');
            prtflg; { output flags }
            write('value[');
            prtval; { output value }
            write(']')

         end;

         lnblk: begin { block entry }

            write('Blk:  ');
            write('Prg[');
            prtval;
            write('-');
            prtval;
            write('] Var[');
            prtval;
            write('-');
            prtval;
            write(']')

         end;

         lnblke: write('Blke');

         lnlin: begin { line tracking entry }

            inpstp(i); { get the line difference }
            curlin := curlin+i; { find net line count }
            inpstp(i); { get the program difference }
            curpgm := curpgm+i; { find net program count }
            inpstp(i); { get the variable difference }
            curvar := curvar+i; { find net variable count }
            write('line track: line: ', curlin:1);
            write(' pgm: '); prthex(maxhex, curpgm);
            write(' var: '); prthex(maxhex, curvar)

         end;
       
         lnsrc: begin { source file name entry }
      
           write('Source file: ');
            read(symfil, l); { get length of filename }
            for i := 1 to l do begin
            
               read(symfil, b); { get a byte of string }
               write(chr(b)) { print }

            end;
            curlin := 0; { clear counters }
            curpgm := 0; 
            curvar := 0

         end;
        
         lnend: write('End'); { end }

      end;
      writeln; { terminate line }
      objcnt := objcnt+1; { count objects }

   until t = 0; { end of file }
  
   closefiles: { exit with file close }

   close(symfil); { close symbol file }

   exit: { exit program directly }

end.

{*******************************************************************************
*                                                                              *
*                                UTILITY MODULE                                *
*                                                                              *
*                        COPYRIGHT (C) 2006 SCOTT A. MOORE                     *
*                                                                              *
* Contains the utilities required by AS in general. Further utilities are      *
* required by each specific machine implementation, and are found in           *
* macutl.pas.                                                                  *
*                                                                              *
*******************************************************************************}

module utl(command, output);

uses strlib,  { string library }
     extlib,  { extention library }
     asdef,   { generic definitions }
     common,  { global variables }
     machine, { custom assembly module }
     asmain;  { error processing }

procedure getsym(var sym: symptr); forward; { get symbol entry }
procedure putsym(sym: symptr); forward; { put symbol entry }
{ generate symbol value }
procedure gensym(sym: symptr; bak: integer; big: boolean; im: imode;
                 cof: integer; str: integer; len: integer); forward;
procedure outbyt(byt: byte); forward; { output object byte }
function endlin: boolean; forward; { check end of line }
function chkchr: char; forward; { check next character }
procedure getchr; forward; { get next character }
procedure skpspc; forward; { skip spaces }
procedure prcnxt(c: char; e: errcod); forward; { process next character }
function digit(c: char): boolean; forward; { check digit }
function alpha(c: char): boolean; forward; { check alphabetical }
procedure nexpr(var sym: symptr); forward; { parse numeric expression }
procedure parnam(var str: string; fatal: boolean); forward;
{ append file extention }
procedure addext(var  str: string; view ext: string; extend: boolean); forward;
procedure opnsrc(view str: string); forward;
procedure clssrc; forward; { close source file }
procedure newcmd; forward; { push new command level }
procedure discmd; forward; { pop command level }
procedure prtecd(var f: text; err: errcod); forward; { print error message }
procedure prcerr(e: errcod); forward; { process error }
procedure getlab; forward; { parse label }
procedure plcsym(var sym: symptr; view s: string); forward; { place symbol }
procedure iniprm; forward; { enter linkage parameters }
procedure parcml; forward; { parse command line }
procedure getlin(var f: text); forward; { load text line }
procedure equprm; forward; { equate linkage parameters }
procedure chkudf; forward; { check undefined globals }
procedure gensmf; forward; { generate symbols file }
procedure outrlds; forward; { generate uncommitted RLDs }
{ find hash function }
function hash(view s: string; l: integer): integer; forward;
{ output value }
procedure outval(val: integer; len: integer; big: boolean); forward;
function endcmd: boolean; forward; { check end of command }
{ find existing symbol }
procedure fndsym(var  sym: symptr; view s: string); forward;
{ match string to substring }
function strsub(view a: string; pos: integer; view b: string): boolean; forward;
procedure factori(var sym: symptr); forward; { parse factor }
procedure termi(var sym: symptr); forward; { parse term }
procedure sexpri(var sym: symptr); forward; { parse simple expression }
procedure expri(var sym: symptr); forward; { parse expression }
procedure chkbrk; forward;
procedure getcmd; forward;
function chksup: boolean; forward;
procedure dmpsym(sym: symptr; sp: integer); forward;
overload procedure dmpsym(sym: symptr); forward;
procedure dmpsyms; forward;
procedure dmprlds; forward;
procedure prtnum(r: byte; fd: byte; w: integer); forward;
procedure getval(var w: integer); forward;
function ascii2chr(b: byte): char; forward;
function chr2ascii(c: char): byte; forward;
procedure getblk(var bp: blkptr); forward;
procedure putblk(bp: blkptr); forward;
procedure getbic(var ip: bicptr); forward;
procedure putbic(ip: bicptr); forward;
procedure relinc(var ip: bicptr); forward;
procedure relblk(var bp: blkptr); forward;
procedure wrtblks; forward;
procedure addinc(s: symptr); forward;
procedure outstp(n: integer); forward;

private

{*******************************************************************************

Check input break

Unimplemented in the Windows version, we let the operating system handle break
checking.

*******************************************************************************}

procedure chkbrk;

begin
end;

{*******************************************************************************

Get block entry

Gets a block entry. If there are block entries on the free list, that is 
returned, otherwise we return an entirely new one.

*******************************************************************************}

procedure getblk(var bp: blkptr);

begin

   if blkfre <> nil then begin { get a free entry }

      bp := blkfre; { index the top free entry }
      blkfre := bp^.next { gap from free list }

   end else new(bp); { just get a new entry }
   bp^.startp := 0; { clear parameters }
   bp^.endp   := 0;
   bp^.startv := 0;
   bp^.endv   := 0;
   bp^.inclst := nil;
   bp^.outp   := false;
   bp^.next   := nil;

end;

{*******************************************************************************

Put block entry

Puts a used block entry. The block is placed onto the free list.

*******************************************************************************}

procedure putblk(bp: blkptr);

begin

   bp^.next := blkfre; { push onto free list }
   blkfre := bp

end;

{*******************************************************************************

Get block inclusion entry

Gets a block inclusion entry. If there are block inclusion entries on the free
list, that is returned, otherwise we return an entirely new one.

*******************************************************************************}

procedure getbic(var ip: bicptr);

begin

   if bicfre <> nil then begin { get a free entry }

      ip := bicfre; { index the top free entry }
      bicfre := ip^.next { gap from free list }

   end else new(ip); { just get a new entry }
   ip^.sym := nil; { clear parameters }
   ip^.blk := nil;
   ip^.next := nil;

end;

{*******************************************************************************

Put block inclusion entry

Puts a used block inclusion entry. The entry is placed onto the free list.

*******************************************************************************}

procedure putbic(ip: bicptr);

begin

   ip^.next := bicfre; { push onto free list }
   bicfre := ip

end;

{*******************************************************************************

Dispose of block inclusion list

Releases all entries in a block inclusion list.

*******************************************************************************}

procedure relinc(var ip: bicptr);

var ip2: bicptr; { pointer to inclusion entry }

begin

   while ip <> nil do begin

      ip2 := ip; { index top of list }
      ip := ip2^.next; { gap out }
      putbic(ip2) { release }

   end

end;

{*******************************************************************************

Dispose of block list

Releases all entries in a block list.

*******************************************************************************}

procedure relblk(var bp: blkptr);

var bp2: blkptr; { pointer to inclusion entry }

begin

   while bp <> nil do begin

      relinc(bp^.inclst); { release any inclusion list }
      bp2 := bp; { index top of list }
      bp := bp2^.next; { gap out }
      putblk(bp2) { release }

   end

end;

{*******************************************************************************

Add inclusion entry

If a block is active, the given symbol is placed into the block as an inclusion
entry.

*******************************************************************************}

procedure addinc(s: symptr);

var ip: bicptr; { block include entry }

begin

   if blkstk <> nil then begin { block is active, place label there }

      getbic(ip); { get a new inclusion entry }
      ip^.sym := s; { set symbol inclusion }
      ip^.next := blkstk^.inclst; { push onto inclusion list }
      blkstk^.inclst := ip;
      s^.par := blkstk { place parent linkage }

   end

end;

{******************************************************************************

Convert ASCII to character

Converts an ASCII 8 bit character to local character equivalents. This is
needed when the internal characters are not ASCII. If the internal characters
are ASCII, the translation will be a no-op. Note that we don't handle ISO 646
or ISO 8859-1, which are the ISO version of ASCII, and the Western European
character sets (same as Windows) respectively.

These kinds of convertions are required because the string fields in .sym files
are stored in ASCII.

Note that characters with values 128 or over are simply returned untranslated.

******************************************************************************}

function ascii2chr(b: byte): char;

var c: char; { character holder }

begin

   if b >= 128 then c := chr(b) { out of ASCII range, just return raw }
   else c := chrtrn[b]; { translate }

   ascii2chr := c { return result }

end;

{*******************************************************************************

Convert character to ASCII

Converts a character to an ASCII value. This is needed when the internal
characters are not ASCII. If the internal characters are ASCII, the translation
will be a no-op. Note that we don't handle ISO 646 or ISO 8859-1, which are the
ISO version of ASCII, and the Western European character sets (same as Windows)
respectively.

These kinds of convertions are required because the string fields in .sym files
are stored in ASCII.

Note that characters with values 128 or over are simply returned untranslated.

*******************************************************************************}

function chr2ascii(c: char): byte;

begin

   chr2ascii := trnchr[c] { return translated character }

end;

{*******************************************************************************

Get command line

Gets the command line to the command line buffer.

*******************************************************************************}

procedure getcmd;

var ovf: boolean; { overflow flag }

begin

   reads(command, cmdrot^.sp, ovf);
   if ovf then begin { overflow }

         writeln('*** Command input line overflow');
         abort

   end;
   cmdrot^.inp := 1 { reset line pointer }

end;

{*******************************************************************************

Print error message

Given a file to print to, and an error code indicating which message to print, 
we print the error message applicable. No end of line is output.

*******************************************************************************}

procedure prtecd(var f:    text;    { file to output to }
                     err : errcod); { error to print }

begin

   case err of { error code }

      elabexp: write(f, 'Label expected');
      eopcexp: write(f, 'Opcode expected');
      elabtl:  write(f, 'Label too long');
      eiltl:   write(f, 'Source line too long');
      eexpr:   write(f, 'Expression error');
      efact:   write(f, 'Factor error');
      erpexp:  write(f, ''')'' expected');
      ecmaexp: write(f, ''','' expected');
      eregexp: write(f, 'Register expected');
      eregt:   write(f, 'Register is wrong type');
      emodt:   write(f, 'Register/mode is wrong type');
      epmba:   write(f, 'Parameter must be absolute');
      epoor:   write(f, 'Parameter is out of range');
      elpexp:  write(f, '''('' expected');
      entl:    write(f, 'Numeric too long');
      edbr:    write(f, 'Digit beyond radix');
      eparam:  write(f, 'Parameter error');
      eterm:   write(f, 'Invalid line termination');
      eopcnf:  write(f, 'Opcode not found');
      eimpl:   write(f, 'Feature not implemented');
      eiovf:   write(f, 'Input overflow');
      eifil:   write(f, 'Invalid/missing file specification');
      eopt:    write(f, 'Invalid option');
      efnfn:   write(f, 'File not found');
      enfmt:   write(f, 'Invalid numeric format');
      eduplab: write(f, 'Duplicate label');
      emlab:   write(f, 'Missing label');
      epvatt:  write(f, 'Previous attribute on label');
      emquo:   write(f, 'Quote expected');
      eifact:  write(f, 'If(s) active at program end');
      eifcex:  write(f, 'Too many nested ''if''s');
      etmei:   write(f, 'Original ''if'' not found');
      estrtl:  write(f, 'String too long');
      enstrna: write(f, 'Null string not valid');
      ereljp:  write(f, 'Relative location out of range');
      etyp:    write(f, 'Type incorrect');
      echrrng: write(f, 'Character value out of range');
      eivcpos: write(f, 'Character position out of range');
      edfext:  write(f, 'Attempt to define external');
      egbludf: begin

                  write(f, 'Global "');
                  write(f, errstr:0);
                  write(f, '" undefined in program')

               end;
      eextnal: write(f, 'File extention not allowed on object');
      epart:   write(f, 'Parameter type incorrect');
      eparexp: write(f, 'Parameter expected');
      erarexp: write(f, '''>'' expected');
      encend:  write(f, 'Target processor not endian configurable');
      eutmac:  write(f, 'Macro definition not terminated within file');
      emismac: write(f, 'No macro definition active to terminate');
      emsatt:  write(f, 'Macro symbol has external or global type');
      esymmac: write(f, 'Symbol is a macro');
      emacovf: write(f, 'Macro invocation count overflow');
      eplsexp: write(f, '''+'' expected');
      embschr: write(f, 'String must be single character');
      erbkexp: write(f, ''']'' expected');
      edisps:  write(f, 'Displacement size out of range');
      eopsiz:  write(f, 'Size of operand must be indicated');
      eopsizm: write(f, 'Size of operands does not match');
      emach:   write(f, 'Instruction/mode not available on current machine');
      eopsizi: write(f, 'Operand size incorrect for instruction');
      eparnum: write(f, 'Too many parameters on macro');
      edupmod: write(f, 'Duplicate radix selected');
      edupfld: write(f, 'Field already specified');
      ebadfld: write(f, 'Invalid print field syntax');
      einvrad: write(f, 'Radix incorrect for type');
      einvfrc: write(f, 'Fraction not used with floating point');
      einvfld: write(f, 'Invalid field value');
      enolab:  write(f, 'Label not allowed on this directive');
      enoblk:  write(f, 'No block is active to terminate');
      eblkact: write(f, 'Missing block end(s) at file end');
      edivzer: write(f, 'Divide by zero');
      efldovf: write(f, 'Value to large for field');
      easflt:  write(f, 'Assembler fault');
      easflt1: write(f, 'Assembler fault #1');
      easflt2: write(f, 'Assembler fault #2');

   end

end;

{********************************************************************************

Print numeric

Print integer in any given radix. Prints the number in the radix given to the
file given. The number will be given with leading zeros to make up the field
width.

********************************************************************************}

procedure prtnum(r  : byte;  { radix to print in }
                 fd : byte;  { field width }
                 w  : integer); { value to print }

var i, j : byte;
    v :    integer;
    min:   integer;

begin

   { first find minimum size }
   min := 0; { clear size }
   v := w; { save value }
   if v = 0 then min := 1 else while v <> 0 do begin

      min := min+1; { count digits }
      v := v div r { next digit }

   end;
   if fd < min then fd := min; { if field is too short, set to min }
   for i := 1 to fd do begin { output digits }

      v := w; { save word }
      for j := 1 to fd - i do v := v div r; { extract digit }
      v := v mod r; { mask }
      { convert ascii }
      if v >= 10 then v := v + (chr2ascii('A') - 10)
      else v := v + chr2ascii('0');
      write(ascii2chr(v)) { output }

   end

end;

{*******************************************************************************

List error

Counts the error, prints the source line where the error occured (if not already
printed), then the error pointer and error message. The error pointer is an '^'
indicating the character position of the error.

*******************************************************************************}

procedure lsterr(var f: text; err : errcod);

begin

   errcnt := errcnt + 1; { count error }
   if cmdrot <> nil then begin { command line present }

      writeln(f, cmdrot^.sp^:0); { print source line }
      { position at error }
      if cmdrot^.inp > 1 then write(f, ' ': cmdrot^.inp - 1);
      writeln(f, '^') { output error pointer }

   end;
   write(f, '*** '); { output error message line }
   if srcrot <> nil then begin { source file present }

      write(f, srcrot^.srcnam:0); { output file name }
      write(f, ':', srcrot^.lincnt:1, ' '); { output line number }

   end;
   prtecd(f, err);
   writeln(f)

end;

{*******************************************************************************

Process error

Prints an error to the errors file. Note that if the file is not open, it is 
created.

*******************************************************************************}

procedure prcerr(e: errcod);

begin

   { if no error file, just list to console }
   if not ferrf then lsterr(output, e)
   else begin { process error file }

      if not ferro then begin { file not open, open it }

         assign(errfil, errlab);
         rewrite(errfil);
         ferro := true

      end;
      lsterr(errfil, e) { list to error file }

   end

end;

{*******************************************************************************

Check end of line

Checks wether the input line position is at the end. This is indicated by a 0 
input position, because it is set to 0 by a procedure overrunning the end of the
input buffer.

*******************************************************************************}

function endlin: boolean;

begin

   { is input position past last ? }
   endlin := cmdrot^.inp > max(cmdrot^.sp^)

end;

{*******************************************************************************

Check next input character

The next character in the input buffer is returned. No advance is made from the 
current position (succesive calls to this procedure will yeild the same 
character). If endlin is true, a blank is returned (conceptually, the input line
rests in a field of infinite blanks).

*******************************************************************************}

function chkchr: char; { current input character }

var c: char;

begin

   { if endlin, then return blank, else return actuall char }
   if endlin then c := ' '
   else c := cmdrot^.sp^[cmdrot^.inp];
   chkchr := c { return result }

end;

{*******************************************************************************

Skip input character

Causes the current input character to be skipped, so that the next chkchr call 
will return the next character. If endlin is true, no action will take place 
(will not advance beyond end of line).

*******************************************************************************}

procedure getchr;

begin

   if not endlin then { process advance }
      cmdrot^.inp := succ(cmdrot^.inp) { advance one character }

end;

{*******************************************************************************

Skip input spaces

Skips the input position past any spaces or controls. Will not skip endlin.
Also skips over comment squences. this is:

     !<comment><eoln>

In the latter case, the input will be left at the end of line.

*******************************************************************************}

procedure skpspc;

begin

   repeat

      { skip any spaces }
      while not endlin and (chkchr <= ' ') do getchr;
      if chkchr = '!' then begin { skip comment }

         getchr; { skip '!' }
         { skip to eoln }
         while not endlin do getchr

      end

   until endlin or (chkchr > ' ')

end;

{*******************************************************************************

Check end of command

Checks if the next character is ';' or eoln.

*******************************************************************************}

function endcmd: boolean;

begin

   endcmd := (chkchr = ';') or endlin { ';' or end of line }

end;

{*******************************************************************************

Process next character

Given a character and an error code, checks if the next character matches that 
character, and processes the given error if not. Otherwise, the character is 
skipped, and the caller returned to.

*******************************************************************************}

procedure prcnxt(c: char;    { character to match }
                 e: errcod); { error to process }

var cs: char; { buffer }

begin

   cs := chkchr; { check next }
   if c = cs then getchr { skip if match }
   else prterr(e) { process the error }

end;

{*******************************************************************************

Check digit

Checks wether the given character lies in the set ['0'..'9']. Returns the 
status.

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

Shift integer right

Shifts a given unsigned integer right by the number of times given in a second 
unsigned integer, to give an unsigned integer result. Locations at the left of 
the integer become 0, and bits shifted out right are lost.

*******************************************************************************}

function bshr(left:  integer; { integer to shift }
              right: integer) { shift count }
              : integer;      { result }

var i: integer; { bit index }

begin

   if right > bits then left := 0 { will shift to 0 }
   { else use simple divide }
   else for i := 1 to right do left := left div 2;
   bshr := left { place result }

end;

{*******************************************************************************

Shift integer left

Shifts a given unsigned integer left by the count specified in another unsigned 
integer, to form a unsigned result integer. zero's are shifted in at the right, 
and bits are lost to the left.

The shift is performed by simple multiplication.

Significant bits at the left are deliberately zero'ed before shifting, in order 
not to cause an overflow (and a possible trap condition).

*******************************************************************************}

function bshl(left: integer;  { integer to shift }
              right: integer) { number of times to shift }
              : integer;      { result }

var i: integer; { bit index }

begin

   if right > bits then left := 0 { will shift to 0 }
   else for i := 1 to right do begin { shift bits }

      { remove overflow condition }
      if left >= maxint div 2 then left := left - maxint div 2;
      left := left * 2 { shift left }

   end;
   bshl := left { place result }

end;

{*******************************************************************************

Parse and convert numeric

Parses and converts the following:

     [radix specification] ['0'..'9', 'a'..'z', 'A'..'Z']...

Where the radix specifier is:

     % - Binary
     & - Octal
     $ - hexadecimal
     none - Decimal

Using the given radix, any digits are processed to yeild an integer unsigned 
result. Leading spaces are skipped. Overflow isn't now but should be flagged as
an error. No spaces are allowed anywhere in the format.

The routine now accepts '_' as a numeric spacer. Any numeric sequence must
start with a-z0-9$&%, but now can continue with '_' as a spacing character.
This allows things like $ffff_ffff.

*******************************************************************************}

procedure getval(var w: integer); { integer parsed }

var labbuf: blab;    { label buffer }
    c:      char;
    r:      1..16;   { radix }
    i:      blabinx; { index for label }
    v:      byte;

begin

   skpspc; { skip spaces }
   r := 10; { set default radix decimal}
   w := 0; { initalize result }
   if chkchr = '%' then begin getchr; r := 2 end { binary }
   else if chkchr = '&' then begin getchr; r := 8 end { octal }
   else if chkchr = '$' then begin getchr; r := 16 end; { hexadecimal }
   if not alpha(chkchr) and not digit(chkchr) and not (chkchr = '_') then
      prterr(enfmt); { invalid digit }
   for i := 1 to maxfil do labbuf[i] := ' '; { clear buffer }
   i := 1;
   while (alpha(chkchr) and (r = 16)) or digit(chkchr) or (chkchr = '_') do 
      begin { load buffer }

      if i > maxfil then prterr(entl); { sequence too long }
      labbuf[i] := chkchr; { place character }
      getchr; { next character }
      i := succ(i) { count }

   end;
   for i := 1 to maxfil do { process digits }
      if (labbuf[i] <> ' ') and (labbuf[i] <> '_') then begin

         { process actual digit }
         c := lcase(labbuf[i]); { get digit }
         { convert '0'..'9' }
         if digit(c) then v := chr2ascii(c) - chr2ascii('0')
         else v := chr2ascii(c) - chr2ascii('a') + 10; { convert 'a'..'z' }
         if v >= r then prterr(edbr); { check fits radix }
         w := w*r+v { scale and add in }

      end

end;

{*******************************************************************************

Check string is substring

Checks if the given string is present at a given position on a string. The 
matching string should not be zero length.

*******************************************************************************}

function strsub(view a:   string; { string to look in }
                     pos: integer; { offset positition }
                view b:   string) { string to look for }
               : boolean;         { status of match }

var i: integer; { index for label }
    f: boolean; { search flag }

begin

   i := 1; { set 1st string position }
   f := true; { flag not done }
   { advance to last matching positition }
   while f do begin

      if (pos <= max(a)) and (i <= max(b)) then begin

         { within strings }
         if a[pos] = b[i] then begin { matches }

            pos := pos+1; { advance string position }
            i := i+1 { advance string positition }

         end else f := false { terminate no match }

      end else f := false { terminate past end }

   end;
   strsub := i > max(b) { return status of match }

end;

{*******************************************************************************

Load string

Loads a string from a symbol entry. The symbol is validated for a defined, 
simple integer or string, and a copy loaded into the given string. If integer, 
it is validated for a character value, then a single character string made of 
it.

*******************************************************************************}

procedure lodstr(var s: pstring; sym: symptr);

begin

   if not sym^.def or sym^.add or sym^.vrs then
      prterr(epmba); { flag invalid }
   if (sym^.typ <> stint) and  (sym^.typ <> ststr) then
      prterr(etyp); { invalid type }
   if sym^.typ = stint then begin { integer, convert to string}

      if sym^.val > 255 then prterr(echrrng); { invalid value }
      new(s, 1); { get a string }
      s^[1] := chr(sym^.val) { place character }

   end else s := copy(sym^.str^) { place string }

end;

{*******************************************************************************

Load integer

Loads an integer from a symbol entry. The symbol is validated for a defined, 
simple integer or string, and a copy loaded into the given integer. If string, 
it is validated for a single character value, then converted to integer.

*******************************************************************************}

function lodint(sym: symptr): integer;

var i: integer; { result return }

begin

   if (sym^.typ <> stint) and  (sym^.typ <> ststr) then
      prterr(etyp); { invalid type }
   if sym^.typ = stint then i := sym^.val { return integer }
   else begin { string }

      { check single character }
      if max(sym^.str^) <> 1 then prterr(embschr);
      i := chr2ascii(sym^.str^[1]) { place character value }

   end;
   lodint := i { return result }

end;

{*******************************************************************************
 
Parse label
 
Parses a label from the input. Labels consist of a sequence of characters in 
the set ['0'..'9', 'A'..'Z', 'a'..'z', '_']. The caller is responsible for 
verifying any extra resrictions on the first character (typ. ['A'..'Z', 
'a'..'z', '_']).
The label is returned in the system variable labbuf.
 
*******************************************************************************}
 
procedure getlab;
 
var i: labinx; { label index }
 
begin

   for i := 1 to maxlab do labbuf[i] := ' '; { clear buff }
   i := 1;
   while alpha(chkchr) or digit(chkchr) or (chkchr = '_') or (chkchr = '.') do

      begin { accept valid characters }
      if i >= maxlab then prterr(elabtl); { too long }
      labbuf[i] := chkchr; { place character }
      getchr; { skip character }
      i := i+1 { count }

   end

end;

{*******************************************************************************
 
Find hash function

Finds a hash function for the given label. This is a random number, based on 
the label (so that succesive calls with the same label will give the same 
value), and limited to a specified range (ie. hash(l) > 0 <= limit). For 
information as to how this is used, consult the appropriate calling procedure.
Although various 'perfect' algorithims may be found by trial and error to 
exactly describe a given index set, we use a very simple algorithim which is 
standard throughout. Each letter of the index label is added together (without 
incurring overflow), then the modulo is taken to limit the value to the desired 
size.

Utilities exist for printing this number on a given label (by hand). See 
elsewhere.

Note: We always calculate the hash using the ASCII equivalent code.

*******************************************************************************}

function hash(view s: string;  { label to base value }
                   l: integer) { upper limit of value }
             : integer;        { hash value }

var i: inpinx; { label index }
    r: integer; { result }

begin

   r := 0; { initalize result }
   for i := 1 to len(s) do r := r + chr2ascii(lcase(s[i])); { add chars }

   hash := r mod l + 1 { limit to proper length }

end;

{*******************************************************************************
 
Get symbol entry
 
Gets a symbol entry to the in place variable. The symbol may be recycled from 
used entries, or produced afresh. Various fields are set to null or false, see 
code for specifics.
 
*******************************************************************************}
 
procedure getsym(var sym: symptr); { symbol to load }

begin

   if symrot = nil then new(sym) { none free, get a new one }
   else begin { load free }

      sym := symrot; { index free symbol }
      symrot := symrot^.chn { gap free list }

   end;
   { initalize entry }
   sym^.opr := onop;  { no operation }
   sym^.lab := nil;   { no label }
   sym^.def := false; { set undefined }
   sym^.add := false; { not address }
   sym^.gbl := false; { not global }
   sym^.ext := false; { not external }
   sym^.vrs := false; { not variable }
   sym^.stv := false; { not 'set' variable } 
   sym^.typ := stint; { set integer type }
   sym^.val := 0;     { initalize to 0 }
   sym^.str := nil;   { no string value }
   sym^.flt := nil;   { no real value }
   sym^.mac := nil;   { set not a macro }
   sym^.inv := 0;     { clear invocation count }
   sym^.chn := nil;   { no next }
   sym^.lft := nil;   { no left }
   sym^.rgt := nil;   { no right }
   sym^.par := nil;   { parent block }

end;

{*******************************************************************************
 
Dispose symbol entry
 
Places the given symbol on the free list. May we recommend that you remove the 
symbol from any other lists first.
 
*******************************************************************************}
 
procedure putsym(sym: symptr); { symbol to dispose }
 
begin

   if sym^.lab <> nil then dispose(sym^.lab); { free any label }
   if sym^.str <> nil then dispose(sym^.str); { free any string }
   sym^.chn := symrot; { link next free entry }
   symrot := sym { place in list }

end;

{*******************************************************************************
 
Find symbol
 
Searches for the symbol matching the given label. Returns the matching symbol, 
or null if not found.

*******************************************************************************}

procedure fndsym(var  sym: symptr;  { returns symbol found }
                 view s:   string); { label to find }

var ptr: symptr; { symbol table pointer }
    i:   syminx; { symbol table index }

begin

   i := hash(s, maxsym); { find hash value }
   ptr := symtab[i]; { index }
   sym := nil; { flag no symbol }
   while ptr <> nil do { traverse list }
      if not compp(ptr^.lab^, s) then { skip entry }
         ptr := ptr^.chn { advance }
      else begin sym := ptr; ptr := nil end { found entry }

end;

{*******************************************************************************
 
Place symbol
 
Places a given label in the symbols table. The symbols table, like the reserved 
table, is a hash indexed table. The table 'header' proper consists of an array 
of symbol pointers, each corresponding to a hash value for that list. Label 
lookups proceed by examining each list item at a hash address. Here, we perform 
a lookup for the given label, and if it is found, return that. Otherwise, a new 
label is created and inserted into the proper list. The various symbol fields 
are default initalized on a newly created entry. Coresponding to the needs of 
this program, there is no difference noted between the old and new cases.

*******************************************************************************}

procedure plcsym(var sym: symptr;  { returns symbol placed }
                 view s:  string); { label to place }

var ptr, lstptr : symptr;
    i : syminx; { symbol table index }

begin

   i := hash(s, maxsym); { find hash value }
   ptr := symtab[i]; { index }
   sym := nil; { flag no symbol }
   while ptr <> nil do { traverse list }
      if not compp(ptr^.lab^, s) then begin { skip entry }

      lstptr := ptr; { save present position }
      ptr := ptr^.chn { advance }

   end else begin sym := ptr; ptr := nil end; { found entry }
   if sym = nil then begin { place new symbol }

      if symtab[i] = nil then begin { empty list }

         getsym(symtab[i]); { get new symbol }
         sym := symtab[i]

      end else begin { insert at list end }

         getsym(lstptr^.chn); { get new symbol }
         sym := lstptr^.chn

      end;
      new(sym^.lab, len(s)); { get a storage string for label }
      copy(sym^.lab^, s) { place label }

   end else if sym^.typ = stmac then prterr(esymmac); { is macro }

end;

{*******************************************************************************

Output stepped value

Outputs a value in stepped format. This is an unsigned format. If the value 
is < 255, then it is output as a single byte. If > 255, the value 255 is 
output, then a 16 bit, big endian format number follows, with values < 65535. 
If it exceeds that, the value 65536 is output, then we continue with a 3 byte, 
then finally a 4 byte format. This system is most efficient where values output 
tend to be small, with only occasional large values. We use it for the 
difference numbers output to the line file.

*******************************************************************************}

procedure outstp(n: integer);

begin

   if n < 0 then fprterr(easflt); { should not be negative }
   if n < 255 then write(symfil, n) { output 1 byte format }
   else begin { larger than 1 byte }

      write(symfil, 255); { output flag byte }
      if n < 65535 then begin { output 2 byte format }

         write(symfil, n div 256); { output high byte }
         write(symfil, n mod 256) { output low byte }

      end else begin { larger than 2 byte }

         write(symfil, 255); { output flag bytes }
         write(symfil, 255);
         if n < 16777215 then begin { output 3 byte format }

            write(symfil, n div 65535); { output high byte }
            write(symfil, (n div 256) mod 256); { output mid byte }
            write(symfil, n mod 256) { output low byte }

         end else begin { output 4 byte format }

            write(symfil, n div 16777216); { output high byte }
            write(symfil, (n div 65535) mod 256); { output high mid byte }
            write(symfil, (n div 256) mod 256); { output low mid byte }
            write(symfil, n mod 256) { output low byte }

         end

      end

   end

end;

{*******************************************************************************

Output line file marker

Outputs a file name marker to the line file. The file name marker is just the
string containing the source file name. This is followed by a "resync" counter
marker that brings the line, program and variable counters back into sync with
the file by offseting them from zero. The idea is that clients don't have to
do the work of tracking file nesting structure if they don't want to.

*******************************************************************************}

procedure linmrk;

var i: blabinx;   { index for label }
    l: 0..maxfil; { length of file name }

begin

   if (srcrot <> nil) and not fsupp and flin then begin { file active }

      write(symfil, ord(lnsrc)); { output source name entry }
      l := len(srcrot^.srcnam); { find length }
      write(symfil, l); { output length }
      { output filename }
      for i := 1 to l do write(symfil, chr2ascii(srcrot^.srcnam[i]));
      { Now, output a "resync" counter set. This assumes that all the counters
        have been zeroed, and resets them back to the value for the file that
        has been set or reset as active. }
      write(symfil, ord(lnlin)); { output diff set marker }
      outstp(srcrot^.llncnt); { differential line }
      outstp(lprgmc); { differential program }
      outstp(lglblc) { differential variable }

   end

end;

{*******************************************************************************
 
Open source file
 
Opens the given file as a source. The source files are kept as
a stack of entries. Each entry contains:
 
     1. The file itself
     2. The complete file specification.
     3. The list chain
 
A new entry is stacked, and the file is opened. No check is made for the 
existance of the file.
Note that the top of the stack describes the currently active source file.

*******************************************************************************}

procedure opnsrc(view str: string); { file to open }

var ptr: srcptr;

begin

   if srcfre <> nil then begin { get old entry }

      ptr := srcfre; { index entry }
      srcfre := srcfre^.nxtsrc { gap free list }

   end else new(ptr); { get new entry }
   copy(ptr^.srcnam, str); { place file name for utilites }
   ptr^.lincnt := 0; { reset line counter }
   ptr^.llncnt := 0; { reset previous line count }
   ptr^.nxtsrc := srcrot; { index next list item }
   srcrot := ptr; { insert into list }
   assign(ptr^.srcfil, str); { open the file }
   reset(ptr^.srcfil);
   linmrk { output line file start }

end;

{*******************************************************************************

Close source file

The top of the source file stack is closed and the top entry removed. The 
removed entry is saved in the free list. 
It is a serious fault if the source stack is empty.

*******************************************************************************}

procedure clssrc;

var ptr: srcptr;

begin

   if srcrot = nil then fprterr(easflt); { stack empty }
   ptr := srcrot; { index tos }
   srcrot := srcrot^.nxtsrc; { pop from stack }
   close(ptr^.srcfil); { close the file }
   ptr^.nxtsrc := srcfre; { link into free list }
   srcfre := ptr;
   linmrk { output marker }

end;

{*******************************************************************************

Load text line

The line contained in the given text file is loaded into the inplin buffer, and 
inpptr is reset to 1. The line can be terminated by either an eoln or eof. In 
the case of eoln, the eoln is skipped, and the file positioned at the next 
line.

2002/06 Added force end of line. Also made routine less paranoid about line
endings since text handling has improved since as was written.

*******************************************************************************}

procedure getlin(var f: text);

label 1; { eoln jump }

var i:   inpinx;  { line index }
    b:   linbuf;  { input line buffer }
    frc: boolean; { forcing flag }

begin

   for i := 1 to maxlin do b[i] := ' '; { clear line buffer }
   if eof(f) then flend := true { set end of file }
   else begin { read line }

      flend := false; { not file end }
      { clear input line }
      i := 1; { 1st position }
      frc := false; { set no force }
      while not eoln(f) or frc do begin

         if eoln(f) then begin { eoln encountered }

            if not frc then goto 1; { abort on eoln }
            readln(f); { skip line }
            i := i-1; { back up over force }
            if eoln(f) or eof(f) then goto 1; { abort unforced eoln }

         end;
         { error on line overflow }
         if i >= maxlin then fprterr(eiovf);
         read(f, b[i]); { get a command character }
         frc := b[i] = '\\'; { set force status }
         i := i + 1; { next position }

      end;
      1: { abort loop }
      if not eof(f) then readln(f); { skip line }
      new(cmdrot^.sp, i-1); { get a string }
      copy(cmdrot^.sp^, b); { place line }
      cmdrot^.inp := 1; { reset line pointer }
      if fplin then begin { print input line }

         prtnum(16, 8, prgmc); { print program address }
         write(' ');
         prtnum(16, 8, glblc); { print variable address }
         write(srcrot^.lincnt:6, ' ');
         { output supress status }
         if chksup then write('S') else write(' ');
         write(' ');
         for i := 1 to max(cmdrot^.sp^) do write(cmdrot^.sp^[i]);
         writeln

      end

   end

end;

{*******************************************************************************

Parse file name

Parses a filename in the format:

     <filename> ::= [<letter>:]<letter>[<letter>/<digit>/'.']...
     <letter>   ::= 'a'..'z'/'A'..'Z'
     <digit>    ::= '0'..'9'

This routine is DOS dependent. The file string so parsed is returned 
in place, and an error 'mode' parameter indicates whether or not a file name 
parsing error would be fatal to the assembly.

*******************************************************************************}

procedure parnam(var str:   string;   { file name return }
                     fatal: boolean); { errors are fatal }

var i: integer; { file name index }
    p: integer; { primary length }
    u: integer; { user number }

procedure getncr; { get name character }

begin

   if i > max(str) then { filename too long }
      { disposition error }
      if fatal then fprterr(eifil) else prterr(eifil);
   str[i] := chkchr; { place file character }
   getchr; { skip that }
   i := i + 1 { next }

end;

procedure getseq(max: blabinx); { read name sequence }

var l: 0..maxfil;

begin

   l := 0; { initalize count }
   while alpha(chkchr) or digit(chkchr) or (chkchr = '_') do begin { filename }

      getncr; { get character }
      l := l + 1 { count }

   end;
   if l > max then { too long }
      { disposition error }
      if fatal then fprterr(eifil) else prterr(eifil)

end;

begin { parnam }

   skpspc; { skip spaces }
   clears(str); { clear filename }
   i := 1; { initalize index }
   if chkchr = '"' then begin { parse as string }

      getchr; { skip '"' }
      while (chkchr <> '"') and not endlin do begin { get string characters }

         if i > max(str) then { overflow }
            { disposition error }
            if fatal then fprterr(eifil) else prterr(eifil);
         str[i] := chkchr; { place }
         getchr; { skip to next }
         i := i+1 { next character }
         
      end;
      if chkchr = '"' then getchr { skip '"' }

   end else begin { parse as name }

      p := 1; { initalize primary count for first character }
      u := 0; { initalize user number }
      if not alpha(chkchr) then { must lead with alpha }
         { send error to appropriate handler }
         if fatal then fprterr(eifil) else prterr(eifil);
      getncr; { get character }
      while digit(chkchr) do begin { process unit number }

         { add to specification }
         u := u*10 + (chr2ascii(chkchr) - chr2ascii('0'));
         getncr; { get character }
         p := p + 1 { since could be primary }

      end;
      if chkchr = ':' then begin { process unit specification }

         if u > 15 then { unit out of range }
            { disposition error }
            if fatal then fprterr(eifil) else prterr(eifil);
         getncr; { get character }
         if not alpha(chkchr) then { must lead off alpha }
            { disposition error }
            if fatal then fprterr(eifil) else prterr(eifil);
         p := 0 { re - initalize primary }

      end;
      if p > maxfil then { too long }
         { disposition error }
         if fatal then fprterr(eifil) else prterr(eifil);
      getseq(maxfil-p); { get rest of primary }
      if chkchr = '.' then begin { secondary }

         getncr; { get character }
         if not alpha(chkchr) or digit(chkchr) then { error }
            if fatal then fprterr(eifil) else prterr(eifil);
         getseq(3) { get secondary }

      end

   end

end; { parnam }

{*******************************************************************************

Check options

Checks if a sequence of options is present in the input, and if so, parses and 
processes them. An option is a '#', followed by the option identifier. The 
identifier must be one of the valid options. Further processing may occur, on 
input after the option, depending on the option specified (see the handlers).
Consult the operator's manual for full option details.

*******************************************************************************}

procedure chkopt;

begin

   skpspc; { skip spaces }
   while chkchr = optchr do begin { parse option }

      getchr; { skip option introducer }
      getlab; { get option }
      { check verbose mode }
      if compp(labbuf, 'verbose') or
              compp(labbuf, 'v') then fverb := true
      { check quiet mode }
      else if compp(labbuf, 'noverbose') or
              compp(labbuf, 'nv') then fverb := false
      { check diagnostics on }
      else if compp(labbuf, 'diag') then fdiag := true
      { check output line file }
      else if compp(labbuf, 'line') or
              compp(labbuf, 'l') then flin := true
      { check supress line file }
      else if compp(labbuf, 'noline') or
              compp(labbuf, 'nl') then flin := false
      else if compp(labbuf, 'error') or
              compp(labbuf, 'e') then begin

              ferrf := true; { set error file requested }
              { check for file spec }
              skpspc;
              if chkchr = '=' then begin{ present }

                 getchr;
                 parnam(errlab, true) { parse file }

              end

      end
      else if compp(labbuf, 'printline') then fplin := true
      else if compp(labbuf, 'printmacro') then fpmac := true
      else fprterr(eopt); { no option found }
      skpspc { skip spaces }

   end

end;

{*******************************************************************************
 
Parse file specification for list
 
Parses a file specification, and adds the resulting filespec to the file name 
list.
 
*******************************************************************************}
 
procedure parfil;
 
var ptr: fnmptr;
 
begin

   new(ptr); { get new name entry }
   ptr^.nxtfil := nil; { terminate }
   { if there was a last entry, place as next list item }
   if lstfil <> nil then lstfil^.nxtfil := ptr;
   { if null list, place as first item }
   if filrot = nil then filrot := ptr;
   lstfil := ptr; { set new last file }
   parnam(ptr^.filnam, true) { parse file w/ fatal error }

end;

{*******************************************************************************

Parse command line

The structure of a command line is:

     file [= file] [file]... [#option]...

Each given file specifies a source to be included, in sequence, into the 
assembly. However, if the first file is followed by a '=', it is the result 
file for the assembly. Options can appear anywhere, and are parsed according to 
the option handler.
The result of this routine is a list of filenames (in filrot), consisting of 
each filename encountered in turn. If the fsupp flag is false, the first name 
in the list is the output file. The entire line is parsed.

*******************************************************************************}

procedure parcml;

begin

   chkopt; { check option }
   parfil; { parse first file }
   chkopt; { check option }
   skpspc; { skip spaces }
   { check if the first file is the output }
   if chkchr = '=' then begin getchr; fsupp := false; parfil end
   else fsupp := true; { not output }
   chkopt; { check option }
   skpspc; { skip spaces }
   while alpha(chkchr) or (chkchr = '"') do begin { parse files }

      parfil; { parse }
      chkopt; { check option }
      skpspc { skip spaces }

   end;
   if not endlin then fprterr(eifil) { not line end }

end;

{*******************************************************************************

Append file extention

Appends a given extention, in place, to the given file name. The extention is 
usually in the form: '.ext'. The extention is placed within the file name at 
the first space or period from the left hand side. This allows extention of 
either an unextended filename or an extended one (in which case the new 
extention simply overlays the old). The overlay is controlled via flag: if 
overwrite is true, the extention will overwrite any existing, if not, any 
existing extention will be left in place.
Note: this routine is CP/M dependant.

*******************************************************************************}

procedure addext(var  str:    string;   { filename to extend }
                 view ext:    string;   { filename extention }
                      extend: boolean); { overwrite flag }

var i: integer; { filename index }
    x: integer; { label index }

begin

   i := 1; { initalize index }
   { skip to first character ' ' or '.' }
   while (str[i] <> ' ') and (str[i] <> '.') do i := i+1;
   if extend or (str[i] = ' ') then
      for x := 1 to max(ext) do { append file extention }
         if ext[x] <> ' ' then begin { append non - space characters }

         str[i] := ext[x]; { transfer extention character }
         i := i+1 { count }

      end

end;

{*******************************************************************************

Recognize control memnonic

Attempts to recognize a control memnonic at the present position. If found, the 
equivalent control character is returned, else just returns space. The input 
position is left past the sequence.

*******************************************************************************}

procedure conchr(var c: char);

var s:   packed array [1..4] of char; { holding cell }
    r:   integer; { hash calculator holding }
    chn: 0..35; { index of control characters }
    i:   1..4; { index for cell }
    ips: array [1..4] of inpinx; { pointer saves }

procedure lookup; { attempt lookup of memnonic }

var i:   1..4; { index for cell }

begin

   { find hash }
   r := 0;
   for i := 1 to 4 do r := r + chr2ascii(s[i]);
   chn := r mod 35 + 1;
   c := ' '; { set none found }
   while (c = ' ') and (chn <> 0) do case chn of { hash index }

      21: begin if s = 'nul\00'   then c := chr(0);   chn := 0  end;
      16: begin if s = 'soh\00'   then c := chr(1);   chn := 15 end;
      2:  begin if s = 'stx\00'   then c := chr(2);   chn := 0  end;
      23: begin if s = 'etx\00'   then c := chr(3);   chn := 33 end;
      14: begin if s = 'eot\00'   then c := chr(4);   chn := 0  end;
      10: begin if s = 'enq\00'   then c := chr(5);   chn := 0  end;
      24: begin if s = 'ack\00'   then c := chr(6);   chn := 0  end;
      28: begin if s = 'bel\00'   then c := chr(7);   chn := 0  end;
      4:  begin if s = 'bs\00\00' then c := chr(8);   chn := 3  end;
      11: begin if s = 'ht\00\00' then c := chr(9);   chn := 12 end;
      1:  begin if s = 'lf\00\00' then c := chr(10);  chn := 18 end;
      25: begin if s = 'vt\00\00' then c := chr(11);  chn := 0  end;
      30: begin if s = 'ff\00\00' then c := chr(12);  chn := 29 end;
      3:  begin if s = 'cr\00\00' then c := chr(13);  chn := 13 end;
      17: begin if s = 'so\00\00' then c := chr(14);  chn := 0  end;
      12: begin if s = 'si\00\00' then c := chr(15);  chn := 0  end;
      29: begin if s = 'dle\00'   then c := chr(16);  chn := 34 end;
      13: begin if s = 'dc1\00'   then c := chr(17);  chn := 0  end;
      27: begin if s = 'xon\00'   then c := chr(17);  chn := 26 end;
      5:  begin if s = 'dc2\00'   then c := chr(18);  chn := 0  end;
      6:  begin if s = 'dc3\00'   then c := chr(19);  chn := 0  end;
      15: begin if s = 'xoff'     then c := chr(19);  chn := 31 end;
      7:  begin if s = 'dc4\00'   then c := chr(20);  chn := 0  end;
      35: begin if s = 'nak\00'   then c := chr(21);  chn := 0  end;
      32: begin if s = 'syn\00'   then c := chr(22);  chn := 0  end;
      18: begin if s = 'etb\00'   then c := chr(23);  chn := 19 end;
      26: begin if s = 'can\00'   then c := chr(24);  chn := 0  end;
      22: begin if s = 'em\00\00' then c := chr(25);  chn := 0  end;
      31: begin if s = 'sub\00'   then c := chr(26);  chn := 0  end;
      19: begin if s = 'esc\00'   then c := chr(27);  chn := 22 end;
      8:  begin if s = 'fs\00\00' then c := chr(28);  chn := 0  end;
      9:  begin if s = 'gs\00\00' then c := chr(29);  chn := 0  end;
      20: begin if s = 'rs\00\00' then c := chr(30);  chn := 0  end;
      33: begin if s = 'us\00\00' then c := chr(31);  chn := 0  end;
      34: begin if s = 'del\00'   then c := chr(127); chn := 0  end

   end

end;

begin { conchr }

   { load cell }
   for i := 1 to 4 do begin

      ips[i] := cmdrot^.inp; { save input at that }
      { insure we aren't fooled by \nul }
      if chkchr = '\00' then s[i] := '?'
      else s[i] := lcase(chkchr);
      getchr

   end;
   lookup; { try 4 characters }
   if c = ' ' then begin

      s[4] := '\00'; { knock out character }
      lookup; { try 3 characters }
      if c = ' ' then begin

         s[3] := '\00'; { knock out character }
         lookup; { try 2 characters }
         if c <> ' ' then cmdrot^.inp := ips[3] { restore to that }

      end else cmdrot^.inp := ips[4] { restore to that }

   end;
   if c = ' ' then cmdrot^.inp := ips[1] { if not found, restore position }

end; { conchr }

{*******************************************************************************

Parse string

Parses and returns an input string. A string is any characters between single 
quotes on a line. A double quote sequence within a string denotes a single 
quote character.
A '\' character introduces a force sequence as:

   \<memnonic>  - an ascii memonic denoting the control
                  character desired (as '\cr', etc.).

   \<number>    - the ascii value of the character
                  desired (with prefixes '$', '@' and
                  '%' possible).

   \<character> - all others just force the given character,
                  including '\'' for quote.

Since a string is as big as the input line, no overflow errors are required. 
The one error consists of a missing quote.

*******************************************************************************}

procedure parstr(var s: pstring); { return string }

label 1; { exit label }

var c: char;
    v: integer;
    b: linbuf;  { buffer for string }
    l: inpinx;  { length of string }
    i: inpinx;  { index for string }

begin

   l := 0; { null string }
   skpspc; { skip leading spaces }
   if chkchr <> '''' then prterr(emquo); { no leading quote }
   getchr; { skip }
   while not endlin do begin { process string }

      c := chkchr; { check next }
      if c = '\\' then begin { control sequence }

         getchr; { skip }
         c := chkchr; { next }
         if c in ['$', '&', '%', '0'..'9'] then begin

            { process numeric force }
            getval(v); { get numeric }
            if v > 255 then prterr(echrrng); { range error }
            c := chr(v) { place character }

         end else begin

            conchr(c); { get possible control character }
            if c = ' ' then begin

               c := chkchr; { not found, is a force }
               getchr { skip }

            end

         end

      end else if c = '''' then begin { found a quote }

         getchr; { skip }
         c := chkchr;
         if c <> '''' then goto 1; { was an exit quote }
         getchr { skip }

      end else getchr; { skip }
      l := l+1; { add to length }
      b[l] := c { place character }

   end;
   prterr(emquo); { no trailing quote }
   1: { terminate }
   new(s, l); { create a new string }
   for i := 1 to l do s^[i] := b[i] { copy into place }

end;

{*******************************************************************************

Check 'if' supression

Checks if an 'if' supression is active. Note that this isn't the same thing as 
the supress flag.

*******************************************************************************}

function chksup: boolean;

var f: boolean; { result flag }

begin

   f := false;
   if ifrot <> nil then { there is a nested if }
      if ifrot^.ifsup or ifrot^.ifold then f := true; { suppress active }
   chksup := f { set result }

end;

{*******************************************************************************
 
Output object byte

If the object is not supressed, the given unsigned byte is output to the object 
file, and the current program location is incremented.

*******************************************************************************}

procedure outbyt(byt: byte); { object byte to output }

begin

   { check in suppress mode, or in if deleted section }
   if not fsupp and not chksup then begin

      write(objfil, byt); { write byte to file }
      prgmc := prgmc + 1  { count }

   end

end;

{*******************************************************************************

Output object value
 
If the object is not supressed, the given unsigned integer is output to the 
object file, and the current program location is incremented (by 2).
The number of bytes occupied by the output value can be specified, as well as 
the big/little endian structure of the output.

If the length specified is greater than the size of an integer, padding sign 
extention bytes will be used to create an effective output of that size.
This routine is dependent on integer being 32 bits, and uses equivalence of 
"packed array [1..4] of byte" to integer to extract the value of an integer.
 
*******************************************************************************}

procedure outval(val: integer;  { object integer to output }
                 len: integer;  { number of bytes to occupy }
                 big: boolean); { big endian format }

var i: integer; { counter }
    c: record case boolean of { convertion }

          false: (a: packed array [1..4] of byte);
          true:  (b: integer)

       end;
    s: integer; { sign holder }

begin

   if val < 0 then s := 255 else s := 0; { set sign extention byte }
   c.b := val; { convert integer to bytes }
   if big then begin { big endian }

      { pad > 32 bits }
      while len > 4 do begin outbyt(s); len := len-1 end;
      for i := len downto 1 do { output bytes }
         outbyt(c.a[i]) { output byte }

   end else begin { little endian }

      { output bytes to maximum of 4 }
      if len > 4 then for i := 1 to 4 do outbyt(c.a[i]) { output byte }
      else for i := 1 to len do outbyt(c.a[i]); { output byte }
      { pad > 32 bits }
      while len > 4 do begin outbyt(s); len := len-1 end

   end

end;

{*******************************************************************************

Output composite bit field

Outputs a value as a composite bit field. Given the inserted value, the backing 
value, and the start and length of bits to be inserted, a series of bytes is 
created with the insertion value imbedded.

Note: dependant on being able to use 'and', 'or' and 'not' on integers.

*******************************************************************************}

procedure outbit(val: integer;  { object integer to output }
                 bak: integer;  { backing value }
                 big: boolean;  { big endian format }
                 str: integer;  { start of insertion }
                 len: integer); { number of bits to occupy }

var mask: integer;
    t:    integer; { holding }

begin

   if (str = 0) and ((len mod 8) = 0) then
      { its just an ordinary bytewise insertion, in which case the
        backing is not used }
      outval(val, len div 8, big)
   else begin { bitwise insertion }

      { form bitmask }
      mask := 1;
      t := len-1;
      while t <> 0 do begin mask := mask*2+1; t := t-1 end;
      { shift up to proper bit position }
      t := str;
      while t <> 0 do begin 

         val := val*2; { shift value }
         mask := mask*2; { shift mask }
         t := t-1 { count }

      end;
      { assemble output value }
      val := (bak and not mask) or (val and mask);       
      t := (len+str) div 8; { find total byte length }
      if ((len+str) mod 8) <> 0 then t := t + 1; { round up }
      outval(val, t, big) { and output final value }

   end

end;

{*******************************************************************************

Check next function code

A function is one of the following:

     mod - modulo
     not - complement
     and - logical and
     or  - logical or
     xor - logical exclusive - or
     shl - shift left
     shr - shift right
     lt  - less than
     le  - less than or equal
     eq  - equal
     ne  - not equal
     gt  - greater than
     ge  - greater than or equal
     lft - left string
     rgt - right string
     cat - concatenate
     len - length string
     +   - add, positive
     -   - subtract, negate
     *   - multiply
     /   - divide

A look - ahead parse is done for any of these sequences. If one is found, it's 
code is returned, else a 'nop' is returned. If a previous lookahead is active, 
that code will be returned instead. The result is that successive calls to 
this procedure always return the same code.

*******************************************************************************}

procedure chkcod(var c: symop); { code for symbol encountered }

var inpsav: inpinx; { position save for backtrack }

const oprmax = 21; { number of items in operator table }

fixed oprtbl: packed array 21 of record

         opn: packed array 3 of char; { name of operator (string) }
         op:  symop;
         chn: 0..21 { chain to next entry }

      end = array 

          record '*  ', omult, 0 end,  { 1 }
          record 'not', onot,  3 end,  { 2 }
          record 'ne ', one,   8 end,  { 3 }
          record '-  ', osub,  0 end,  { 4 }
          record 'eq ', oeq,   7 end,  { 5 }
          record 'mod', omod,  9 end,  { 6 }
          record 'len', olen,  0 end,  { 7 }
          record '+  ', oadd,  0 end,  { 8 }
          record '/  ', odiv,  0 end,  { 9 }
          record 'xor', oxor,  11 end, { 10 }
          record 'gt ', ogt,   0 end,  { 11 }
          record 'lft', olft,  0 end,  { 12 }
          record 'shl', oshl,  0 end,  { 13 }
          record 'and', oand,  0 end,  { 14 }
          record 'lt ', olt,   0 end,  { 15 }
          record 'or ', oor,   17 end, { 16 }
          record 'ge ', oge,   0 end,  { 17 }
          record 'cat', ocat,  0 end,  { 18 }
          record 'shr', oshr,  20 end, { 19 }
          record 'rgt', orgt,  18 end, { 20 }
          record 'le ', ole,   0 end   { 21 }

      end;

{ match operator string }

function matopr: symop; 

var c: symop;
    h: integer;

begin

   c := onop; { set no code found }
   h := hash(labbuf, oprmax); { find hash }
   { search for matching entry, or end of chain }
   repeat

      if compp(labbuf, oprtbl[h].opn) then begin { found }

         c := oprtbl[h].op; { set code found in table }
         h := 0

      end else h := oprtbl[h].chn { link next table entry }

   until h = 0; { until end of chain }

   matopr := c { return match or nop }

end;

begin

   if codsav = onop then begin { no last code }

      inpsav := cmdrot^.inp; { save current position }
      skpspc; { skip spaces }
      if alpha(chkchr) then begin { parse operator }

         getlab; { get operator }
         codsav := matopr; { match operator }
         { if we didn't find a match, restore input position }
         if codsav = onop then cmdrot^.inp := inpsav

      end else begin { check single character codes }

         clears(labbuf); { clear target string }
         labbuf[1] := chkchr; { place character }
         codsav := matopr; { match operator }
         { if we matched, move forward one character }
         if codsav <> onop then getchr

      end;

   end;
   c := codsav { return code }

end;

{*******************************************************************************
 
Get next function code
 
Gets and skips the next function code (see chkcod for details). Succesive calls 
to this procedure will return succesive codes.
 
*******************************************************************************}
 
procedure getcod;

begin

   chkcod(codsav); { make sure current code is loaded }
   codsav := onop { clear last code }

end;

{*******************************************************************************

Parse factor

Parses a factor, and returns an in place, possibly complex, symbol. See expr 
for further details.

*******************************************************************************}

procedure factori(var sym: symptr); { factor return }

var cod: symop; { operation code save }
    ptr: symptr;
    s:   pstring; { string temp }

begin { factor }

   skpspc; { skip spaces }
   chkcod(cod); { check next code }
   if cod = oadd then begin { +, positivition ? }

      getcod; { skip code }
      factor(sym); { get factor }
      if sym^.typ = ststr then begin { convert string to int }

         getsym(ptr); { get symbol }
         ptr^.def := true; { set defined }
         ptr^.val := lodint(sym); { get value }
         if sym^.lab = nil then putsym(sym); { dispose if free }
         sym := ptr { place result }

      end

   end else if (cod = osub) or (cod = onot) then begin

      { operations: - (negation), not (complement) }
      getcod; { skip code }
      if cod = osub then cod := oneg; { change subtract to neg }
      factor(ptr); { parse factor }
      if not ptr^.def or ptr^.add or ptr^.vrs then begin

         { operand undefined, return complex symbol }
         getsym(sym); { get symbol }
         sym^.opr := cod; { set operation }
         sym^.add := ptr^.add; { set same addressed }
         sym^.vrs := ptr^.vrs; { set same variable }
         sym^.lft := ptr; { set operand linkage }

      end else begin { defined, process operation }

         getsym(sym); { get symbol }
         sym^.def := true; { set defined }
         sym^.add := ptr^.add; { set same addressed }
         sym^.vrs := ptr^.vrs; { set same variable }
         { negate }
         if cod = oneg then sym^.val := - lodint(ptr)
         else sym^.val := not lodint(ptr); { complement }
         { if operand free, dispose }
         if (ptr^.lab = nil) and ptr^.def then
            putsym(ptr)

      end

   end else if cod = olen then begin { length }

      getcod; { skip }
      factor(ptr); { parse factor }
      lodstr(s, ptr); { get the string }
      getsym(sym); { get symbol }
      sym^.def := true; { set defined }
      sym^.val := max(s^); { place length }
      dispose(s); { release temp string }
      if ptr^.lab = nil then putsym(ptr) { dispose free symbol }

   end else if alpha(chkchr) or (chkchr = '_') or (chkchr = '.') then begin

      { symbol }
      getlab; { get symbol }
      if compp(labbuf, '_') then begin

         { opcode location }
         getsym(sym); { get symbol }
         sym^.def := true; { set defined }
         sym^.add := true; { set address }
         sym^.val := opcloc { set to opcode location }

      end else plcsym(sym, labbuf) { place in table }

   end else if chkchr = '(' then begin { (expr) }

      getchr; { skip '(' }
      expr(sym); { parse expression }
      prcnxt(')', erpexp) { no ')' }

   end else if digit(chkchr) or (chkchr = '%') or
      (chkchr = '&') or (chkchr = '$') then begin { numeric }

      getsym(sym); { get symbol }
      sym^.def := true; { set defined }
      getval(sym^.val) { place numeric value }

   end else if chkchr = '''' then begin

      parstr(s);
      getsym(sym); { get symbol }
      sym^.def := true; { set defined }
      { single character, convert to int }
      if max(s^) = 1 then begin

         sym^.val := chr2ascii(s^[1]); { place value }
         dispose(s) { release temp string }

      end else begin

         sym^.typ := ststr; { set string type }
         sym^.str := s { place string }

      end

   end else prterr(efact) { no leaders found }

end;

{*******************************************************************************
 
Parse term
 
Parses a term, and returns an in place, posibly complex, symbol. See expr for 
full details.
 
*******************************************************************************}

procedure termi(var sym: symptr); { term symbol return }

var lftptr, rgtptr: symptr;  { left and right factors }
    op:             symop;   { operator save }
    s:              pstring; { string temp }
    i:              integer; { integer temp }

begin { term }

   factor(sym); { parse factor }
   skpspc; { skip input spaces }
   { valid operations: *, /, mod, and, shl, shr }
   chkcod(op); { check next code }
   while op in [omult, odiv, omod, oand, oshl, oshr, olft, orgt] do begin

      getcod; { get operator }
      lftptr := sym; { set left factor }
      factor(rgtptr); { parse right factor }
      if not lftptr^.def or not rgtptr^.def or
         lftptr^.add or lftptr^.add or 
         rgtptr^.vrs or rgtptr^.vrs then begin

         { either operand undefined, produce complex symbol }
         if (lftptr^.typ <> stint) or (rgtptr^.typ <> stint) then
            prterr(epmba); { wrong type for undefined }
         getsym(sym); { get symbol }
         sym^.opr := op; { place operation }
         { set address to the conjuction of entries }
         sym^.add := lftptr^.add or rgtptr^.add;
         { set variable to the conjunction of entries }
         sym^.vrs := lftptr^.vrs or rgtptr^.vrs;
         sym^.lft := lftptr; { set left linkage }
         sym^.rgt := rgtptr { set right linkage }

      end else begin

         getsym(sym); { get symbol }
         sym^.def := true; { set defined }
         { set address to the conjunction of entries }
         sym^.add := lftptr^.add or rgtptr^.add;
         { set variable to the conjunction of entries }
         sym^.vrs := lftptr^.vrs or rgtptr^.vrs;
         case op of { operation, find which to perform }

            omult: sym^.val := lodint(lftptr)*lodint(rgtptr); { multiply }
            odiv: begin { divide }

               i := lodint(rgtptr); { get right }
               if i = 0 then prterr(edivzer); { flag divide by zero }
               sym^.val := lodint(lftptr) div i { divide }

            end;
            omod: begin { modulo }

               i := lodint(rgtptr); { get right }
               if i = 0 then prterr(edivzer); { flag divide by zero }
               sym^.val := lodint(lftptr) mod i { modulo }

            end;
            oand: sym^.val := lodint(lftptr) and lodint(rgtptr); { and }
            { shift left }
            oshl: sym^.val := bshl(lodint(lftptr), lodint(rgtptr));
            { shift right }
            oshr: sym^.val := bshr(lodint(lftptr), lodint(rgtptr));
            olft: begin { left string }

                     lodstr(s, lftptr); { get left string }
                     { find left characters from string }
                     s := extract(s, 1, rgtptr^.val);
                     sym^.str := s; { place string }
                     sym^.typ := ststr { set result type string }

                  end;
            orgt: begin { right string }

                     lodstr(s, lftptr); { get left string }
                     { find right characters from string }
                     s := extract(s, max(s^)-rgtptr^.val+1, max(s^));
                     sym^.str := s; { place string }
                     sym^.typ := ststr { set result type string }

                  end

         end;
         { dispose of free suboperands }
         if (lftptr^.lab = nil) and lftptr^.def then
            putsym(lftptr);
         if (rgtptr^.lab = nil) and rgtptr^.def then
            putsym(rgtptr)

      end;
      chkcod(op) { check next code }

   end

end;

{*******************************************************************************

Parse simple expression

Parses a simple expression, and returns an in place, possibly complex, symbol. 
See expr for full details.

*******************************************************************************}

procedure sexpri(var sym: symptr); { simple expression head ret }

var lftptr, rgtptr: symptr;  { left and right terms }
    op:             symop;   { operation code save }
    s1, s2:         pstring; { string temps }

begin { sexpr }

   term(sym); { parse term }
   skpspc; { skip input spaces }
   { valid operators: +, -, or, xor }
   chkcod(op); { check next code }
   while op in [oadd, osub, oor, oxor, ocat] do begin

      getcod; { get operator }
      lftptr := sym; { set left term }
      term(rgtptr); { parse right term }
      { Check either operand undefined or in the variable space, or in the 
        address space. Such variables get recalculated. }
      if not lftptr^.def or not rgtptr^.def or 
         lftptr^.add or lftptr^.add or 
         rgtptr^.vrs or rgtptr^.vrs then begin

         { operand(s) undefined, produce operator entry }
         if (lftptr^.typ <> stint) or (rgtptr^.typ <> stint) then
            prterr(epmba); { wrong type for undefined }
         getsym(sym); { get symbol }
         sym^.opr := op; { place operation code }
         { set address as the conjunction of left and right }
         sym^.add := lftptr^.add or rgtptr^.add;
         { set variable as the conjunction of left and right }
         sym^.vrs := lftptr^.vrs or rgtptr^.vrs;
         sym^.lft := lftptr; { set left linkage }
         sym^.rgt := rgtptr { set right linkage }

      end else begin { operation possible, create defined }

         getsym(sym); { get symbol }
         sym^.def := true; { set defined }
         { set address as the conjunction of left and right }
         sym^.add := lftptr^.add or rgtptr^.add;
         { set variable as the conjunction of left and right }
         sym^.vrs := lftptr^.vrs or rgtptr^.vrs;
         case op of { operator, find which to perform }

            oadd: sym^.val := lodint(lftptr)+lodint(rgtptr); { add }
            osub: sym^.val := lodint(lftptr)-lodint(rgtptr); { subtract }
            oor: sym^.val := lodint(lftptr) or lodint(rgtptr); { or }
            oxor: sym^.val := lodint(lftptr) xor lodint(rgtptr); { xor }
            ocat: begin { concatenate strings }

                     lodstr(s1, lftptr); { get left string }
                     lodstr(s2, rgtptr); { get right string }
                     s1 := cat(s1, s2); { find concatenation }
                     dispose(s2); { release temp string }
                     sym^.str := s1; { place string }
                     sym^.typ := ststr { set result type string }

                  end

         end;
         { free subentries }
         if (lftptr^.lab = nil) and lftptr^.def then
            putsym(lftptr);
         if (rgtptr^.lab = nil) and rgtptr^.def then
            putsym(rgtptr)

      end;
      chkcod(op) { check next code }

   end

end;

{*******************************************************************************

Parse expression

The symtax for an expression is:

     <expr>   ::= <sexpr> ['lt'/'le'/'eq'/'ne'/'gt'/'ge' <sexpr>]...
     <sexpr>  ::= <term> ['+'/'-'/'or'/'xor'/'cat' <term>]...
     <term>   ::= <factor> ['*'/'/'/'mod'/'and'/'shl'/'shr'/'lft'/'rgt'
                  <factor>]...
     <factor> ::= +<factor>/-<factor>/not<factor>/len<factor>/
                  <symbol>/(<expr>)/<number>/<string>
     <symbol> ::= <letter>/'_' [<letter>/<digit>/'_']...
     <number> ::= <digit>/'%'/'@'/'$' [<letter>/<digit>]...
     <letter> ::= 'a'..'z'/'A'..'Z'
     <digit>  ::= '0'..'9'
     <string> ::= '<anysequence>'

The expression is parsed, and the result returned as a symbol entry. Any 
operations that are 'defined' (whose operands are not derived from address 
space or variable space quanta) are performed and the data structure for the 
expression siplified.

All other operations are preserved (but in fact performed anyway) in the form 
of an expression tree. This tree is rooted in the returned symbol. The entry 
returned may or may not actually contain a label (and therefore be a symbol 
proper).

It is also possible for a result to have BOTH address and variable types, in 
which case it is simply to be treated as an undefined complex.
Note that if the entry returned is a 'simple' symbol and no references to it 
are required, it should be returned to the free symbols list upon operation 
completion.

Note: The expressions with lt, le, eq, ne, gt, and ge are required to be 
absolute and resolved here, since they don't exist as operators in the linker
files.

*******************************************************************************}

procedure expri(var sym: symptr); { expression head ret }

var lftptr, rgtptr: symptr;  { left and right terms }
    op:             symop;   { operation code save }
    s1, s2:         pstring; { string temps }

begin

   sexpr(sym); { parse simple expression }
   skpspc; { skip input spaces }
   { valid operators: lt, le, eq, ne, gt, ge }
   chkcod(op); { check next code }
   while op in [olt, ole, oeq, one, ogt, oge] do begin

      getcod; { get operator }
      lftptr := sym; { set left term }
      sexpr(rgtptr); { parse right simple expression }
      if not lftptr^.def or lftptr^.add or lftptr^.vrs or
         not rgtptr^.def or rgtptr^.add or lftptr^.vrs then
         prterr(epmba); { error }
      { operation possible, create defined }
      getsym(sym); { get symbol }
      sym^.def := true; { set defined }
      { set address as the conjunction of left and right }
      sym^.add := lftptr^.add or rgtptr^.add;
      { set variable as the conjunction of left and right }
      sym^.vrs := lftptr^.vrs or rgtptr^.vrs;
      if (lftptr^.typ = stint) and (rgtptr^.typ = stint) then
        case op of { operator, find which to perform }

         olt: sym^.val := -1*ord(lftptr^.val < rgtptr^.val);
         ole: sym^.val := -1*ord(rgtptr^.val <= lftptr^.val);
         oeq: sym^.val := -1*ord(lftptr^.val = rgtptr^.val);
         one: sym^.val := -1*ord(lftptr^.val <> rgtptr^.val);
         ogt: sym^.val := -1*ord(lftptr^.val > rgtptr^.val);
         oge: sym^.val := -1*ord(lftptr^.val >= rgtptr^.val)

      end else begin { string operation }

         lodstr(s1, lftptr); { get strings }
         lodstr(s2, rgtptr);
         case op of { operator }

            olt: sym^.val := -1 * { less than }
                    ord(gtrc(lftptr^.str^, rgtptr^.str^));
            ole: sym^.val := -1 * { less than or equal }
                    ord(not gtrc(rgtptr^.str^, lftptr^.str^));
            oeq: sym^.val := -1 * { equal to }
                    ord(compc(lftptr^.str^, rgtptr^.str^));
            one: sym^.val := -1 * { not equal to }
                    ord(not compc(lftptr^.str^, rgtptr^.str^));
            ogt: sym^.val := -1 * { greater than }
                    ord(gtrc(rgtptr^.str^, lftptr^.str^));
            oge: sym^.val := -1 * { greater than or equal }
                    ord(not gtrc(lftptr^.str^, rgtptr^.str^))

         end;
         dispose(s1); { release string temps }
         dispose(s2)

      end;
      { free subentries }
      if (lftptr^.lab = nil) and lftptr^.def then
         putsym(lftptr);
      if (rgtptr^.lab = nil) and rgtptr^.def then
         putsym(rgtptr);
      chkcod(op) { check next code }

   end

end;

{*******************************************************************************

Parse integer expression

Evaluates an integer expression (see expr). Validates for an integer type. If 
the returned type is a string, it is converted to integer.

*******************************************************************************}

procedure nexpr(var sym: symptr);

var ptr: symptr; { temp pointer }

begin

   expr(sym); { parse expression }
   if sym^.typ = ststr then begin { convert to integer }

      getsym(ptr); { get symbol }
      ptr^.def := true; { set defined }
      ptr^.val := lodint(sym); { get value }
      if sym^.lab = nil then putsym(sym); { dispose if free }
      sym := ptr { place result }

   end

end;

{*******************************************************************************

Push command level

Places a new command entry atop the current stack.

*******************************************************************************}

procedure newcmd;

var p: cmdptr; { temp }

begin

   if cmdfre <> nil then begin { an old entry is avalible }

      p := cmdfre; { save entry }
      cmdfre := cmdfre^.nxt { delete from free list }

   end else new(p); { get a new header entry }
   p^.nxt := cmdrot; { insert into current list }
   cmdrot := p;
   p^.sp := nil; { clear string }
   p^.inp := 0; { clear input index }
   p^.mac := nil { clear macro constructor }

end;

{*******************************************************************************

Pop command level

Removes the last command level.

*******************************************************************************}

procedure discmd;

var p: cmdptr; { temp }

begin

   if cmdrot = nil then prterr(easflt); { fault }
   if cmdrot^.sp <> nil then dispose(cmdrot^.sp); { dispose of command string }
   p := cmdrot; { index top }
   cmdrot := cmdrot^.nxt; { delete from list }
   p^.nxt := cmdfre; { insert to free list }
   cmdfre := p

end;

{*******************************************************************************

Generate symbol value

Expects a symbol, and an 'insertion type'. The symbol's value,
defined or undefined, is output by the method appropriate to
the insertion parameters. These parameters are:

     1. The big/little endian mode of values.
     2. A flag for program counter offset mode.
     3. The insertion type.
     4. The length of the output value.

The endian mode simply determines the format of the output value. The length 
indicates the number of bytes the value will occupy. The insertion type is one
of the following:

     1. Normal, insert plain value.
     2. Signed offset, finds a signed offset from the PC following the value.
     3. Non-standard offset, same as signed, but allows for machines that
        use another PC base besides the end of the instruction.
     4. Segmented. Special case for 80x86 processors.
   
Note that the output handler used depends on the definition state of the symbol.
At the same time as the value is output, a 'RLD' entry is created with the 
insertion type, program count and symbol linkage applicable. The RLD is output 
only if the symbol is not absolute.

Note that the symbol must not be recycled (as the RLD table indexes it), if it 
is not absolute.

Note that if an 'if' is active (code deleted), no rld's are generated, and no 
errors indicated for relative range.

*******************************************************************************}

procedure gensym(sym: symptr;   { value to generate }
                 bak: integer;  { background value }
                 big: boolean;  { big endian }
                 im:  imode;    { insertion mode }
                 cof: integer;  { constant offset }
                 str: integer;  { starting bit }
                 len: integer); { number of bits }

var rld: rldptr;  { rld pointer }
    w:   integer; { jump displacement }
    i:   integer; { counter }
    t:   integer; { temp }
    bt:  integer; { number of bytes in output }
    seg: integer; { segment }
    off: integer; { offset }

begin

   rld := nil; { set not output as rld }
   bt := (str+len) div 8; { find the number of bytes occupied by value }
   if ((str+len) mod 8) <> 0 then bt := bt + 1; { round up }
   if not chksup then begin { not in 'if' }

      if not sym^.def or sym^.add or sym^.vrs or 
         (im in [imsgof, imnsof]) then begin

         if sym^.def and sym^.add and (im in [imsgof, imnsof]) then begin

            { displacement can be calculated, find displacement.
              This assumed using the program counter after the value output. }
            w := sym^.val-(prgmc+bt+cof);
            { check result fits in specified output length }
            t := w; { copy value }
            { move off all bits to output, which should leave only 0 or -1 }
            for i := 1 to len do t := t div 2;
            if (t <> 0) and (t <> -1) then 
               prterr(ereljp); { location out of range }
            outbit(w, bak, big, str, len) { output displacement value }

         end else begin { symbol not absolute }

            new(rld); { get an rld }
            rld^.rldchn := rldrot; { index next table item }
            rldrot := rld; { link into table }
            rld^.big := big; { place endian mode }
            rld^.im  := im; { place insertion mode }
            rld^.cof := cof; { place constant offset }
            rld^.str := str; { place starting bit }
            rld^.len := len; { place bit length }
            rld^.add := prgmc; { place program location }
            rld^.inssym := sym; { set symbol linkage }
            outbit(sym^.val, bak, big, str, len) { output value }

         end

      end else begin

         { symbol is defined, output the value }
         if im = imiseg then begin { Intel segment }

            seg := sym^.val div 16; { find segment }
            off := sym^.val mod 16; { find offset }
            { check if the segment overflowed }
            if (bt = 4) and (seg >= 65536) then prterr(efldovf);
            outval(off, bt-2, big); { output offset }
            outval(seg, 2, big) { output segment }

         end else outbit(sym^.val, bak, big, str, len);

      end

   end;
   { if symbol is free, dispose }
   if (sym^.lab = nil) and sym^.def and (rld = nil) then putsym(sym)

end;

{*******************************************************************************

Enter linkage parameters

Places the linkage parameters '_pstr', '_pend', '_vstr', and '_vend' in the 
symbols table. They are set as defined variable or address entries. Although 
references may be made to these symbols, they are not defined until the end of 
the assembly.

*******************************************************************************}

procedure iniprm;

var sym: symptr;
 
begin

   plcsym(sym, '_pstr'); { place program start }
   sym^.def := true; { set defined }
   sym^.add := true; { set address }
   sym^.gbl := true; { set global }
   plcsym(sym, '_pend'); { place program end }
   sym^.def := true; { set defined }
   sym^.add := true; { set address }
   sym^.gbl := true; { set global }
   plcsym(sym, '_vstr'); { place variables start }
   sym^.def := true; { set defined }
   sym^.vrs := true; { set variable }
   sym^.gbl := true; { set global }
   plcsym(sym, '_vend'); { place variables end }
   sym^.def := true; { set defined }
   sym^.vrs := true; { set variable }
   sym^.gbl := true { set global }

end;

{*******************************************************************************
 
Equate linkage parameters
 
The program start, program end, variable start, and variable end parameters are 
set, from the current location counters.
This routine should be called at assembly end only.
 
*******************************************************************************}
 
procedure equprm;
 
var sym: symptr;
 
begin

   plcsym(sym, '_pend'); { find program end }
   sym^.val := prgmc; { set }
   plcsym(sym, '_vend'); { find variables end }
   sym^.val := glblc { set }

end;

{*******************************************************************************

Check undefined globals

Searches the symbols table for undefined globals, and outputs an error for each 
one found.

*******************************************************************************}

procedure chkudf;

var i:   syminx; { symbol index }
    sym: symptr;

begin

   for i := 1 to maxsym do begin { traverse table head }

      sym := symtab[i]; { index entry list }
      while sym <> nil do begin { traverse symbols list }

         if sym^.gbl and not sym^.def then begin

            { undefined global, print error }
            copy(errstr, sym^.lab^); { set label to print }
            prcerr(egbludf)

         end;
         sym := sym^.chn { index next list symbol }

      end

   end

end;

{*******************************************************************************

Output variger

Outputs the given integer to the byte file as a variger.
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

The integer is converted by removing the sign bit and converting to signed 
magnitude, then determining the byte size, then outputting the tag and number.

*******************************************************************************}

procedure wrtvar(var f: bytfil; n: integer); { integer to output}

var t: integer; { tag byte }
    p: integer; { power holder }

begin

   { handle 0 as special case }
   if n = 0 then begin write(f, 0); write(f, 0) end else begin

      { value is non-zero }
      t := bytes-1; { initalize tag field to max bytes }
      if n < 0 then begin { remove sign and convert to signed magnitude }
      
         t := t + $40; { place sign in tag }
         n := abs(n) { find absolute value of integer }
      
      end;
      p := toppow; { get top power }
      { find 1st non-zero digit in integer }
      while (n div p) = 0 do begin p := p div 256; t := t - 1 end;
      write(f, t); { output finalized tagfield }
      while p <> 0 do begin { output bytes }
      
         write(f, n div p); { output that byte }
         n := n mod p; { remove the byte }
         p := p div 256 { next lower power }      
      
      end

   end

end;

{*******************************************************************************

Assemble symbol flags

In the output symbols file, a symbol's flags occupy a single byte. Here we 
assemble the flags for a given symbol into a byte, in the same format as it is 
to be output. See the symbols file format for further details.

*******************************************************************************}

function flags(sym { symbol to process } : symptr)
               : byte; { collected flags byte }

var fl: byte; { flags construction site }

begin

   fl := 0; { initalize flags }
   if sym^.def then fl := fl + $01; { defined }
   if sym^.add then fl := fl + $02; { address }
   if sym^.gbl then fl := fl + $04; { global }
   if sym^.ext then fl := fl + $08; { external }
   if sym^.vrs then fl := fl + $10; { variable }
   flags := fl { return flags byte }

end;

{*******************************************************************************

Output actuall symbol

Outputs a given symbol. No attempt is made to comvert or output subtrees of the 
symbol. The exact format of the symbol depends on the labeled status, and the 
undefined status. See the applicable documentation.

*******************************************************************************}

procedure wrtsym(sym { symbol to output } : symptr);

var i: labinx; { label index }

begin

   if sym^.lab <> nil then write(symfil, ord(lnsym)) { labeled }
   else write(symfil, ord(lnanon)); { unlabeled }
   write(symfil, ord(sym^.opr)); { operation }
   if sym^.lab <> nil then begin { output label }

      write(symfil, max(sym^.lab^)-1); { output length of symbol }
      for i := 1 to max(sym^.lab^) do { output characters }
         write(symfil, chr2ascii(sym^.lab^[i]))

   end;
   write(symfil, flags(sym)); { flags }
   if sym^.def then { output value }
         wrtvar(symfil, sym^.val) { output value }

end;

{*******************************************************************************
 
Output symbol skeleton
 
Outputs the symbol entry to the symbols file. If the symbol is labeled, a copy 
is made and forced undefined, then the copy is output. If not, it is just 
written as is, and each subtree output using the same (this entire) algorithim.
The result is a 'skeletal' expression tree, with labeled symbols represented as 
undefined terminals. All of this 'folds up' upon input to another processor.
 
*******************************************************************************}
 
procedure outsym(sym { symbol to output } : symptr);
 
var ptr : symptr;
 
begin

   if sym^.lab <> nil then begin { labeled symbol }

      getsym(ptr); { get a copy }
      ptr^ := sym^; { copy symbol }
      ptr^.opr := onop; { force terminal }
      ptr^.def := false; { undefined }
      ptr^.gbl := false; { not global }
      ptr^.ext := false; { not external }
      wrtsym(ptr); { output }
      { must remove these dynamic pointers, or else they will get recycled }
      ptr^.lab := nil;
      ptr^.str := nil;
      putsym(ptr) { dispose of copy }

   end else begin { simple symbol }

      wrtsym(sym); { output }
      { output any left subtree }

      if sym^.lft <> nil then outsym(sym^.lft);
      { output any right subtree }
      if sym^.rgt <> nil then outsym(sym^.rgt)

   end

end;

{*******************************************************************************
 
Output IT field

Outputs the it field, of the following format:

     ------------------
     | IT | LEN | OFF |
     ------------------

     IT is the IT flag byte

     LEN is the bit length of insertion -1

     OFF is the PC offset constant

Definition of the IT flag byte:

     bit
     7   - Low for "big endian" format insertion, high for 
           "little endian".
     6   - Unused
     5-3 - Insertion mode:
             0 - Normal
             1 - Signed offset
             2 - Signed offset, non-standard offset
             3 - Intel "huge" mode segmented
             4 - SPARC aligned signed offset (32 bit)
             5 - SPARC "broken field" signed offset
     2   - Starting bit offset (0-7)
     1   -      ""          ""
     0   -      ""          ""
 
Note that the offset field will only be present if the constant offset flag is 
set.

*******************************************************************************}

procedure outit(r: rldptr); { rld to output it for }

var t: byte; { holding }

begin

   t := 0; { clear flag byte }
   if r^.big then t := t + $80; { place big endian flag }
   t := t+ord(r^.im)*$08; { place insertion mode }
   t := t + r^.str; { place bit offset }
   write(symfil, t); { output insertion flags }
   write(symfil, r^.len-1); { output bit length }
   { output constant offset if required }
   if r^.im = imnsof then write(symfil, r^.cof)

end;

{*******************************************************************************
 
Output RLD entries for symbol
 
Outputs any RLD's indexing the given symbol. Each RLD output is removed from 
the RLD table (so that it will not be output in later stages).
 
*******************************************************************************}
 
procedure outrld(sym { symbol to compare } : symptr);
 
var ptr, lst: rldptr;
 
begin

   ptr := rldrot; { index RLD table }
   while ptr <> nil do begin { traverse RLD list }

      if ptr^.inssym = sym then begin { RLD indexes symbol }

         write(symfil, ord(lnpat)); { associated RLD }
         outit(ptr); { output the IT field }
         wrtvar(symfil, ptr^.add); { output address }
         { delete rld from list }
         if ptr = rldrot then rldrot := ptr^.rldchn { chk 1st }
         else begin { list mid }

            lst^.rldchn := ptr^.rldchn; { gap list }
            ptr := lst { back up pointer }

         end

      end;
      lst := ptr; { set last entry }
      ptr := ptr^.rldchn { index next }

   end

end;

{*******************************************************************************

Output uncomitted RLD's

Outputs RLD entries not subordinate to a labeled symbol. The RLD's may occur in 
two forms: coordinate and complex. Coordinate refers to an RLD whose insertion 
value is known at the time of output. No further information besides the RLD 
proper is required here. Complex RLD's have a value described by an undefined 
expression tree. They are uncommited because they where the result of an 
unlabeled 'dw' or similar statement.

The complete skeletal expression tree (for the RLD) is output, Followed by the 
RLD (in the same format as a comitted symbol - RLD pair).
 
*******************************************************************************}
 
procedure outrlds;
 
var ptr: rldptr;
 
begin

   ptr := rldrot; { index RLD table }
   while ptr <> nil do begin { traverse RLD list }

      if ptr^.inssym^.def then begin { coordinate RLD }

         write(symfil, ord(lnrel)); { type: coordinate }
         outit(ptr); { output the IT field }
         wrtvar(symfil, ptr^.add); { output address }
         write(symfil, flags(ptr^.inssym)); { flags }
         wrtvar(symfil, ptr^.inssym^.val) { output value }

      end else begin { complex RLD }

         wrtsym(ptr^.inssym); { output prime entry }
         write(symfil, ord(lnpat)); { associated RLD }
         outit(ptr); { output the IT field }
         wrtvar(symfil, ptr^.add); { output address }
         if ptr^.inssym^.lft <> nil then { output left tree }
            outsym(ptr^.inssym^.lft);
         if ptr^.inssym^.rgt <> nil then { output rgt tree }
            outsym(ptr^.inssym^.rgt)

      end;
      ptr := ptr^.rldchn { next list RLD }

   end

end;

{*******************************************************************************

Generate symbol

Generates a single symbol to the output symbols file. If the symbol given is
integer, not a "setequ" symbol, and has not already been output, it is output
to the file, followed by any indexing rld's, then the left and the right
subtrees, if they exist.

*******************************************************************************}

procedure gensymb(sym: symptr);

begin

   { if symbol is an integer, not a "setequ" variable }
   if (sym^.typ = stint) and not sym^.stv then begin

      { symbol valid for output }        
      wrtsym(sym); { output prime entry }
      outrld(sym); { output RLD's indexing }
      { output possible left skeletal subtree }
      if sym^.lft <> nil then outsym(sym^.lft);
      { output possible right skeletal subtree }
      if sym^.rgt <> nil then outsym(sym^.rgt)

   end

end;

{*******************************************************************************

Generate symbols file

Uses the symbols and RLD tables to form the output symbols file. The exact
format of the symbols file should be examined in the appropriate document.

Each symbol in the symbols table is examined, and written as is to the output 
file. Any RLD's indexing the symbol are then output, followed by a 'skeleton 
set' of the symbol's dependance tree. 'Skeletal' refers to the fact that each 
labeled symbol used to build the definition for the symbol is output as 
undefined, and no attempt is made to process it's subtrees.

When the table is reloaded, these undefined entries 'latch' back onto the 
actual symbol, therefore recreating the tree. After all symbols are output, the 
uncommitted RLD entries are output.

*******************************************************************************}

procedure gensmf;

var i:   syminx; { symbol index }
    sym: symptr;

begin

   for i := 1 to maxsym do begin { traverse table head }

      sym := symtab[i]; { index entry list }
      while sym <> nil do begin { traverse symbols list }

         { check symbol is an "orphan", or belongs to no block }
         if sym^.par = nil then gensymb(sym); { generate single symbol }
         sym := sym^.chn { index next list symbol }

      end

   end

end;

{*******************************************************************************

Dump symbol

This is a diagnostic.

*******************************************************************************}

procedure dmpsym(sym: symptr; sp: integer);

begin

   if sym <> nil then begin { not a null pointer }

      if sp > 0 then write(' ': sp);
      if sym^.lab <> nil then begin

         { this should be lab^: 20, but there is a compiler bug }
         write(sym^.lab^);
         if max(sym^.lab^) < 20 then write(' ': 20-max(sym^.lab^));
         write(' ')

      end else write('<anonymous>          ');
      case sym^.opr of 

         onop:  write('onop  ');
         oadd:  write('oadd  ');
         osub:  write('osub  ');
         omult: write('omult ');
         odiv:  write('odiv  ');
         omod:  write('omod  ');
         oshl:  write('oshl  ');
         oshr:  write('oshr  ');
         oand:  write('oand  ');
         oor:   write('oor   ');
         oxor:  write('oxor  ');
         onot:  write('onot  ');
         oneg:  write('oneg  ');
         olt:   write('olt   ');
         ole:   write('ole   ');
         oeq:   write('oeq   ');
         one:   write('one   ');
         ogt:   write('ogt   ');
         oge:   write('oge   ');
         olft:  write('olft  ');
         orgt:  write('orgt  ');
         ocat:  write('ocat  ');
         olen:  write('olen  ')

      end;
      if sym^.def then write('d') else write(' ');
      if sym^.add then write('a') else write(' ');
      if sym^.gbl then write('g') else write(' ');
      if sym^.ext then write('e') else write(' ');
      if sym^.vrs then write('v') else write(' ');
      if sym^.stv then write('s') else write(' ');
      case sym^.typ of

         stint: write('int ');
         ststr: write('str ');
         stflt: write('flt ');
         stmac: write('mac ')

      end;
      write(sym^.val: 6, ' ($'); prtnum(16, 8, sym^.val); write(') ');
      if sym^.str <> nil then write('''', sym^.str^:20, '''');
      if sym^.flt <> nil then write(sym^.flt^:20);
      write(sym^.mac <> nil:5);
      write(sym^.inv:6);
      writeln;
      if sym^.lft <> nil then begin

         write('L');
         dmpsym(sym^.lft, sp+3) { dump left }

      end;
      if sym^.rgt <> nil then begin

         write('R');
         dmpsym(sym^.rgt, sp+3) { dump right }

      end;
      sym := sym^.chn { index next list symbol }

   end else writeln('<null>')

end;

overload procedure dmpsym(sym: symptr); begin dmpsym(sym, 0) end;

{*******************************************************************************

Dump symbols

This is a diagnostic.

*******************************************************************************}

procedure dmpsyms;

var i:   syminx; { symbol index }
    sym: symptr;

begin

   writeln('Symbols dump:');
   writeln;
   for i := 1 to maxsym do begin { traverse table head }

      sym := symtab[i]; { index entry list }
      while sym <> nil do begin { traverse symbols list }

         dmpsym(sym, 0); { dump this entry }
         sym := sym^.chn { index next list symbol }

      end

   end

end;

{*******************************************************************************

Dump rlds

This is a diagnostic.
 
*******************************************************************************}
 
procedure dmprlds;
 
var ptr: rldptr;
 
begin

   writeln('RLDs dump:');
   writeln;
   ptr := rldrot; { index RLD table }
   while ptr <> nil do begin { traverse RLD list }

      write('Sym: ');
      dmpsym(ptr^.inssym, 0);
      write('Rld: big: ', ord(ptr^.big):1, ' im: ');
      case ptr^.im of

         imnorm:  write('norm                 ');
         imsgof:  write('sgn off              ');
         imnsof:  write('nstd off             ');
         imssof:  write('SPARC aln sgn off    ');
         imsbsof: write('SPARC bkn fld sgn off');
         imiseg:  write('Intel seg            ');

      end;
      write(' cof: ', ptr^.cof:1, ' str: ', ptr^.str:1, ' len: ', ptr^.len:3);
      write(' addr: ');
      prtnum(16, 8, ptr^.add);
      writeln;
      ptr := ptr^.rldchn { next list RLD }

   end

end;

{*******************************************************************************

Output block description

Outputs a complete block description. The block header is output, followed by
the inclusion list for the block.
 
*******************************************************************************}

procedure wrtblk(bp: blkptr);

var ip: bicptr; { inclusion pointer }

{ output included blocks }

procedure outblk(ip: bicptr);

begin

   while ip <> nil do begin

      { if there is a subblock, output it }
      if ip^.blk <> nil then wrtblk(ip^.blk);
      ip := ip^.next { next inclusion }

   end

end;

begin

   write(symfil, ord(lnblk)); { output object type }
   wrtvar(symfil, bp^.startp); { output starting program address }
   wrtvar(symfil, bp^.endp); { output ending program address }
   wrtvar(symfil, bp^.startv); { output starting variable address }
   wrtvar(symfil, bp^.endv); { output ending variable address }
   outblk(bp^.inclst); { output any subblocks }
   ip := bp^.inclst; { index top of inclusion list }
   while ip <> nil do begin { output inclusion list }

      { check there is an included symbol }
      if ip^.sym <> nil then gensymb(ip^.sym); { output symbol }
      ip := ip^.next { next }

   end;
   write(symfil, ord(lnblke)); { output block end }
   bp^.outp := true { set this block is output }

end;

{*******************************************************************************

Output block list

Outputs the block list to the symbols file.

We output the blocks in nested order, that is, each block is followed by it's
subblock entries, then it's inclusion list:

block

   [block]
   inclusions

endblock

The block is included in the block it is nested in, and each symbol inclusion
belongs to the last block entry output. This means readers must keep a pushdown
list of active blocks to correctly assign entries to their parents.
 
*******************************************************************************}

procedure wrtblks;

var bp: blkptr; { block pointer }

begin

   bp := blklst; { index the top of the finished block list }
   while bp <> nil do begin { traverse }

      { if block not previously output }
      if not bp^.outp then wrtblk(bp); { output this block }
      bp := bp^.next { next block }

   end

end;

begin
end.

module utl(output);

uses

   stddef, { standard defines }
   strlib, { string functions }
   extlib, { os support functions }
   lndef,  { global definitions file }
   common, { global variables file }
   main;   { abort }

procedure prthex(f: byte; w: integer); forward; { print hex value }
procedure prterr(e: errcod); forward; { print error }
procedure fprterr(e: errcod); forward; { print fatal error }
procedure parcmd; forward; { parse command line }
{ add file extention }
procedure addext(var fn: filnam; ext: extbuf; extend: boolean); forward;
procedure readsymb(var b: byte); forward; { read symbols file byte }
procedure readobjb(var b: byte); forward; { read object file byte }
procedure flushout; forward; { flush output buffer }
procedure wrtobjb(b: byte); forward; { output object byte }
procedure flushsym; forward; { flush symbols buffer }
procedure wrtsymb(b: byte); forward; { output symbols byte }
function ascii2chr(b: byte): char; forward; { translate ascii value to char }
function chr2ascii(c: char): byte; forward; { translate char to ascii value }

private

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

{******************************************************************************

Convert character to ASCII

Converts a character to an ASCII value. This is needed when the internal
characters are not ASCII. If the internal characters are ASCII, the translation
will be a no-op. Note that we don't handle ISO 646 or ISO 8859-1, which are the
ISO version of ASCII, and the Western European character sets (same as Windows)
respectively.

These kinds of convertions are required because the string fields in .sym files
are stored in ASCII.

Note that characters with values 128 or over are simply returned untranslated.

******************************************************************************}

function chr2ascii(c: char): byte;

begin

   chr2ascii := trnchr[c] { return translated character }

end;

{******************************************************************************

Print hexadecimal

Print a hexadecimal number with field width. Prints right justified with left
hand zeros filling the field. Also allows for the fact that an unsigned 32 bit
number can be read into a 32 bit signed number.
One remaining problem is how to detect and convert the invalid value $80000000.

******************************************************************************}

procedure prthex(f: byte; w: integer);
 
var buff: packed array [1..10] of char; { buffer for number in ascii }
    i:    integer; { index for same }
    t:    integer; { holding }
 
begin

   { set sign of number and convert }
   if w < 0 then begin

      w := w+1+maxint; { convert number to 31 bit unsigned }
      t := w div $10000000 + 8; { extract high digit }
      writeh(output, t); { ouput that }
	   w := w mod $10000000; { remove that digit }
      f := 7 { force field to full }     

   end;
   hexs(buff, w); { convert the integer }
   for i := 1 to f-len(buff) do write('0'); { pad with leading zeros }
   write(output, buff:0) { output number }

end;

{******************************************************************************

Process error

Prints the given error code.

******************************************************************************}

procedure prterr { (e: errcod) };

begin

   write('*** ');
   case e of { error }

      einvfil: write('Invalid/missing file specification');
      enovf:   write('Input numeric overflow');
      ecmdsyn: write('Command line syntax invalid');
      elabovf: write('Label too long');
      eequexp: write('''='' expected');
      eoptnf:  write('Option not found');
      einvnum: write('Invalid number format');
      enofs:   write('No output file specified');
      efilnf:  begin 

         write('File '''); 
         write(output, errfil:0); 
         write(''' not found') 

      end;
      edbr:    write('Digit beyond radix specified');
      esymfmt1, esymfmt2, esymfmt3, esymfmt4, esymfmt5, esymfmt6, esymfmt7,
      esymfmt8, esymfmt9, esymfmt10, esymfmt11, esymfmt12, esymfmt13, esymfmt14, 
      esymfmt15, esymfmt16, esymfmt17, esymfmt18, esymfmt19, esymfmt20, 
      esymfmt21, esymfmt22, esymfmt23, esymfmt24, esymfmt25, esymfmt26, 
      esymfmt27, esymfmt28, esymfmt29, esymfmt30, esymfmt31, esymfmt32, 
      esymfmt33, esymfmt34, esymfmt35, esymfmt36, esymfmt37:
         write('Invalid symbol file format, reporting code: ', 
               ord(e)-ord(esymfmt1)+1:1);
      edupsym: begin

         write('Duplicate symbol ''');
         write(output, errlab:0);
         write('''')

      end;
      efldovf: begin

         write('Value $');
         prthex(digits, errval); { print value }
         write(' exceeds output field of ', errbits:1, ' bits provided at $');
         prthex(digits, prgmc)

      end;
      efltfmt: write('Floating point format not implemented');
      esymlen: write('Symbol length exceeds linker capability');
      engore: begin

         write('Symbol ''');
         write(output, errlab:0);
         write(''' must be global or external to cross module boundary')

      end;
      erldovf: write('Relocation table size exceeds linker capability');
      eobjfmt: write('Invalid object file format');
      ecmdovf: write('Command line too long');
      esysflt1, esysflt2, esysflt3, esysflt4, esysflt5: 
         write('LN internal fault #: ', ord(e)-ord(esysflt1)+1:1, 
               ': Please notify S. A. Moore');

   end;
   seterr(1); { flag error to OS }
   writeln { terminate line }

end;

{******************************************************************************

Process fatal error

Prints the given error code, and aborts the program.

******************************************************************************}

procedure fprterr { (e: errcod) };

begin

   prterr(e); { print error }
   abort { terminate program }

end;

{******************************************************************************

Check end of line

Checks if the end of the command buffer has been reached.

******************************************************************************}

function endlin: boolean;

begin

   endlin := cmdptr > cmdlen { input pointer past end of line }

end;

{******************************************************************************

Check next command line character

Returns the next character in the command line, or a space if past the end.

******************************************************************************}

function chkchr: char;

begin

   if not endlin then chkchr := cmdlin[cmdptr] { return current character }
   else chkchr := ' ' { else return space }

end;

{******************************************************************************

Get next command line character

If not at the end of the command line, skip to the next command line
character.

******************************************************************************}

procedure getchr;

begin

   if not endlin then cmdptr := cmdptr+1 { advance position if not end }

end;

{******************************************************************************

Skip spaces

Skips spaces in the command line.

******************************************************************************}

procedure skpspc;

begin

   while (chkchr = ' ') and not endlin do getchr { skip spaces, not end }

end;

{******************************************************************************

Check digit

Checks wether the given character lies in the set ['0'..'9']. Returns the 
status.

******************************************************************************}

function digit(c: char)  { character to check }
              : boolean; { status of check }

begin

   digit := c in ['0'..'9']

end;

{******************************************************************************

Check alphabetical

Checks if the given character lies in the set ['A'..'Z', 'a'..'z'] (is a 
letter). Returns the status.

******************************************************************************}

function alpha(c : char) { character to check }
              : boolean; { status }

begin

   alpha := c in ['A'..'Z', 'a'..'z']

end;

{******************************************************************************

Append file extention

Places a new extention to the given filename. If the ovr flag is true, then
any existing extention is overwritten, otherwise the new extention is only
added if the existing name has none.

******************************************************************************}

procedure addext(var  fn:     filnam;   { filename to extend }
                      ext:    extbuf;   { filename extention }
                      extend: boolean); { overwrite flag }

var p, n, e: filnam; { name component holders }

begin

   brknam(fn, p, n, e); { break filename into components }
   { if overwrite is true or extention empty, place new extention }
   if extend or (e[1] = ' ') then copy(e, ext);
   maknam(fn, p, n, e) { reconstruct the filename }

end;

{******************************************************************************

Parse filename

Gets a filename from the command line and validates it.

******************************************************************************}

procedure parfil(var n: string); 

var fi: integer;  { index for filename }

begin

   skpspc; { skip spaces }
   clears(n); { clear filename }
   fi := 1; { set 1st character }
   if chkchr = '"' then begin { parse string }

      getchr; { skip '"' }
      while (chkchr <> '"') and not endlin do begin { get string characters }

         if fi > max(n) then fprterr(einvfil); { overflow }
         n[fi] := chkchr; { place }
         getchr; { skip to next }
         fi := fi+1 { next character }
         
      end;
      if chkchr = '"' then getchr { skip '"' }

   end else while chkchr in valfch do begin

      if fi > max(n) then fprterr(einvfil); { overflow }
      n[fi] := chkchr; { place }
      getchr; { skip to next }
      fi := fi+1 { next character }

   end;
   if not validfile(n) then fprterr(einvfil); { check and error on filename }

end;

{******************************************************************************

Parse and convert numeric

Parses and converts the following:

     [radix specification] ['0'..'9', 'a'..'z', 'A'..'Z']...

Where the radix specifier is:

     % - Binary
     & - Octal
     $ - hexadecimal
     none - Decimal

Using the given radix, any digits are processed to yeild an integer
unsigned result.
Leading spaces are skipped.
Overflow isn't now but should be flagged as an error.
No spaces are allowed anywhere in the format.

******************************************************************************}

procedure parnum(var i: integer); { integer parsed }

var r: 1..16;   { radix }
    v: integer; { value holding }

begin

   skpspc; { skip spaces }
   r := 10; { set default radix decimal}
   i := 0; { initalize result }
   if chkchr = '%' then begin r := 2; getchr end { binary }
   else if chkchr = '&' then begin r := 8; getchr end { octal }
   else if chkchr = '$' then begin r := 16; getchr end; { hexadecimal }
   if not alpha(chkchr) and not digit(chkchr) then
      fprterr(einvnum); { invalid digit }
   while (alpha(chkchr) and (r = 16)) or
      digit(chkchr) do begin { load buffer }
   
         { convert '0'..'9' }
         if digit(chkchr) then v := ord(chkchr) - ord('0')
         else v := ord(lcase(chkchr)) - ord('a') + 10; { convert 'a'..'z' }
         getchr; { skip }
         if v >= r then fprterr(edbr); { check fits radix }
         i := i * r + v { scale and add in }

      end;

end;

{******************************************************************************

Parse label

Parses a label, which is:

    '_'/'a'..'z' ['_', '0'..'9', 'a'..'z']...

The label is returned in the provided buffer.

******************************************************************************}

procedure parlab(var l: labl);

var i: 0..maxlab; { index for label }

begin

   for i := 1 to maxlab do l[i] := ' '; { clear label buffer }
   i := 0; { clear index }
   while chkchr in ['_', '0'..'9', 'a'..'z', 'A'..'Z'] do begin

      { parse label characters }
      if i >= maxlab then fprterr(elabovf);
      i := i + 1; { next character }
      l[i] := chkchr; { place character }
      getchr { skip }

   end

end;

{******************************************************************************

Get new file specification entry

Gets a new file list entry, and places that in the source list at the end.

******************************************************************************}

procedure getsrc(var ptr: filept);

var i: filinx; { index for filename }

begin

   new(ptr); { get new name entry }
   ptr^.next := nil; { terminate }
   { if there was a last entry, place as next list item }
   if srclas <> nil then srclas^.next := ptr;
   { if null list, place as first item }
   if srclst = nil then srclst := ptr;
   srclas := ptr; { set new last file }
   for i := 1 to maxfil do ptr^.nam[i] := ' '; { clear filename }
   ptr^.plen := 0; { clear program length }
   ptr^.vlen := 0 { clear variable length }

end;

{******************************************************************************

Check options

Checks if a sequence of options is present in the input, and if
so, parses and processes them. An option is a '#', followed by
the option identifier. The identifier must be one of the valid
options. Further processing may occur, on input after the
option, depending on the option specified (see the handlers).
Consult the operator's manual for full option details.

******************************************************************************}

procedure paropt;

var l: labl;    { label holder }

begin

   skpspc; { skip spaces }
   while chkchr = optchr do begin { parse option }

      getchr; { skip option character }
      parlab(l); { get option }
      { check verbose mode }
      if compp(l, 'verbose') or
         compp(l, 'v') then fverb := true { set verbose }
      { check quiet mode }
      else if compp(l, 'noverbose') or
              compp(l, 'nv') then 
         fverb := false { set no verbose }
      else if compp(l, 'error') or
              compp(l, 'e') then begin

         { check for file spec }
         skpspc;
         if chkchr = '=' then begin { present }

            getchr;
            parfil(errnam); { parse file }
            ferrf := true { set error file requested }

         end else begin { not present }

            { we have to get the error file name from somewhere }
            if fsupp then fprterr(enofs) { no output file specified }
            else begin

               errnam := srclst^.nam; { place object file name }
               ferrf := true { set error file requested }

            end

         end

      { check program location }
      end else if compp(l, 'programst') or
                  compp(l, 'ps') then begin

         { parse location specification }
         skpspc; { skip spaces }
         if chkchr <> '=' then fprterr(eequexp); { '=' expected }
         getchr; { skip '=' }
         parnum(pgmloc) { get program location }

      { check variable location }
      end else if compp(l, 'variablest') or
                  compp(l, 'vs') then begin

         { parse location specification }
         skpspc; { skip spaces }
         if chkchr <> '=' then fprterr(eequexp); { '=' expected }
         getchr; { skip '=' }
         parnum(varloc); { get variable location }
         fvset := true { set variable start has been set }

      end else if compp(l, 'trim') or
                  compp(l, 't') then 
         ftrim := true { set trim defined symbols mode }
      else if compp(l, 'secure') or
              compp(l, 's') then 
         fsecr := true { set secure references mode }
      else if compp(l, 'undefine') or
              compp(l, 'u') then 
         fundf := true { output undefined symbols listing }
      else if compp(l, 'noundefine') or
              compp(l, 'nu') then 
         fundf := false { no output undefined symbols listing }
      else if compp(l, 'listlab') or
              compp(l, 'll') then 
         fllab := true { output label ordered symbol listing }
      else if compp(l, 'nolistlab') or
              compp(l, 'nll') then 
         fllab := false { no output label ordered symbol listing }
      else if compp(l, 'listval') or
              compp(l, 'lv') then 
         flval := true { output value ordered symbol listing }
      else if compp(l, 'nolistval') or
              compp(l, 'nlv') then 
         flval := false { no output value ordered symbol listing }
      else if compp(l, 'listmod') or
              compp(l, 'lm') then 
         flmod := true { output module listing }
      else if compp(l, 'nolistmod') or
              compp(l, 'nlm') then 
         flmod := false { no output module listing }
      else if compp(l, 'listxref') or
              compp(l, 'lx') then 
         flxrf := true { output cross reference listing }
      else if compp(l, 'nolistxref') or
              compp(l, 'nlx') then 
         flxrf := false { no output cross reference listing }
      { check listing width }
      else if compp(l, 'listwidth') or
              compp(l, 'lw') then begin

         { parse listing width specification }
         skpspc; { skip spaces }
         if chkchr <> '=' then fprterr(eequexp); { '=' expected }
         getchr; { skip '=' }
         parnum(lstlen) { get listing width }

      end else if compp(l, 'listlength') or
                  compp(l, 'lln') then begin

         { parse listing length specification }
         skpspc; { skip spaces }
         if chkchr <> '=' then fprterr(eequexp); { '=' expected }
         getchr; { skip '=' }
         parnum(lstpag) { get listing length }

      end else if compp(l, 'deleteunusedblocks') or
                  compp(l, 'dub') then fldub := true
      else if compp(l, 'nodeleteunusedblocks') or
              compp(l, 'ndub') then fldub := false
      else if compp(l, 'dumpsymbol') then
         fsdmp := true { print symbols dump (diagnostic) }
      else if compp(l, 'norecycle') then symrecycle := false
      else fprterr(eoptnf); { option not found }
      skpspc { skip spaces }

   end

end;

{******************************************************************************

Parse command line

The structure of a command line is:

     file [= file] [file]... [#option]...

Each given file specifies a source to be included, in sequence,
into the compilation. However, if the first file is followed by a
'=', it is the result file for the compilation. Options can appear
anywhere, and are parsed according to the option handler.
The result of this routine is a list of filenames (in fillst),
consisting of each filename encountered in turn. If the fsupp
flag is false, the first name in the list is the output file.
The entire line is parsed.

******************************************************************************}

procedure parcmd;

var fp: filept; { pointer for filename entries }

begin

   paropt; { parse any options }
   getsrc(fp); { get a file entry }
   parfil(fp^.nam); { parse first file }
   skpspc; { skip spaces }
   { check if the first file is the output }
   if chkchr = '=' then begin

      getchr;
      fsupp := false;
      getsrc(fp); { get a file entry }
      parfil(fp^.nam) { parse single source file element }

   end else fsupp := true; { not output }
   skpspc; { skip spaces }
   while chkchr in valfch+['"'] do begin { parse files }

      getsrc(fp); { get a file entry }
      parfil(fp^.nam); { parse source file }
      skpspc { skip spaces }

   end;
   paropt; { parse any options }
   if not endlin then fprterr(ecmdsyn) { not line end }

end;

{******************************************************************************

Read byte from symbols file

Reads a byte from the symbols file. Maintains a read cache.

******************************************************************************}

procedure readsymb { (var b: byte) } ;

var buf: byte;

begin


   if syminx = symtop then begin { the read buffer is empty }

      symtop := 1;  { reset top }
      while not eof(symfil) and (symtop < maxsav) do begin

         { not at file end, and more space exists in buffer }
         read(symfil, buf); { get a byte }
         symbuf[symtop] := buf;
         symtop := symtop+1 { next location in buffer }

      end;
      syminx := 1 { reset index }

   end;
   if syminx = symtop then prterr(esymfmt30); { should not be empty }
   b := symbuf[syminx]; { return next byte }
   syminx := syminx+1 { next byte }

end;

{******************************************************************************

Read byte from object file

Reads a byte from the object file. Maintains a read cache.

******************************************************************************}

procedure readobjb(var b: byte);

var buf: byte;

begin

   if obiinx = obitop then begin { the read buffer is empty }

      obitop := 1;  { reset top }
      while not eof(objinp) and (obitop < maxsav) do begin

         { not at file end, and more space exists in buffer }
         read(objinp, buf); { get a byte }
         obibuf[obitop] := buf;
         obitop := obitop+1 { next location in buffer }

      end;
      obiinx := 1 { reset index }

   end;
   if obiinx = obitop then prterr(eobjfmt); { should not be empty }
   b := obibuf[obiinx]; { return next byte }
   obiinx := obiinx+1 { next byte }

end;

{******************************************************************************

Flush output object buffer 

Empties the output buffer to the output object file.

******************************************************************************}

procedure flushout;

var i: savinx; { index for save buffer }

begin

   { write the current buffer out }
   for i := 1 to oboinx-1 do write(objout, obobuf[i]);
   oboinx := 1 { reset to buffer start }

end;

{******************************************************************************
 
Output byte to object file

Outputs bytes to the object file, with caching.

******************************************************************************}

procedure wrtobjb { (b: byte) } ; { object byte to output }

begin

   if oboinx = maxsav then flushout; { buffer is full, empty it }
   { place byte in buffer }
   obobuf[oboinx] := b;
   oboinx := oboinx+1 { next byte }

end;

{******************************************************************************

Flush output symbol buffer 

Empties the output buffer to the output symbol file.

******************************************************************************}

procedure flushsym;

var i: savinx; { index for save buffer }

begin

   { write the current buffer out }
   for i := 1 to syoinx-1 do write(symfil, syobuf[i]);
   syoinx := 1 { reset to buffer start }

end;

{******************************************************************************
 
Output byte to symbol file

Outputs bytes to the symbol file, with caching.

******************************************************************************}

procedure wrtsymb(b: byte); { object byte to output }

begin

   if syoinx = maxsav then flushsym; { buffer is full, empty it }
   { place byte in buffer }
   syobuf[syoinx] := b;
   syoinx := syoinx+1 { next byte }

end;

begin
end.

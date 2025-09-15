{*******************************************************************************
*                                                                              *
*                      ASSEMBLY LANGUAGE LISTER                                *
*                                                                              *
*                   Copyright (C) 1994 S. A. Moore                             *
*                        All rights reserved                                   *
*                                                                              *
* Lists an assembly file, or any file that has one-to-one correspondence       *
* between the source representation and the object representation, by using    *
* the line tracking specifications in the symbol file.                         *
*                                                                              *
* The command format is:                                                       *
*                                                                              *
*    al [/option] file [/option]                                               *
*                                                                              *
* The filename specifies the primary name of the .sym and .obj combination.    *
* It also sets the name of the output file, which is file.lst, and the title   *
* of each page in the listing, but that can be overriden.                      *
*                                                                              *
* The size of each page, the number of lines, and the number of characters on  *
* each page dictates the listing format. Each page gets a page header, and can *
* list the program counter, variables counter and line number for each line    *
* in the listing. Each line automatically wraps if it overflows, and the       *
* listing is paginated. The left and top margins of each page can be set, and  *
* the right and bottom margins are also determined from that.                  *
*                                                                              *
* The following options are implemented:                                       *
*                                                                              *
* Long name    Short Name  Meaning                                             *
* ---------------------------------------------------------------------------- *
* expand           e           Expand data that runs over a single line.       *
* noexpand         ne          Do not expand data.                             *
* lines=n          l=n         Set the lines on the printed page.              *
* chars=n          c=n         Set the characters per line on the printed      *
*                              page.                                           *
* leftmargin=n     lm=n        Sets the number of blank spaces to leave to the *
*                              left of the listing.                            *
* topmargin=n      tm=n        Sets the number of blank spaces to leave at the *
*                              top of the listing.                             *
* dataline=n       dl=n        Set the number of data words to place on each   *
*                              line.                                           *
* programdigit=n   pd=n        Set number of digits in a program or variable   *
*                              address.                                        *
* variabledigit=n  vd=n        Set number of digits in a program or variable   *
*                              address.                                        *
* linedigit        ld          Set number of digits in line number (0=none).   *
* wordsize         ws          Sets the number of bytes in a instruction or    *
*                              data word.                                      *
* bigendian        be          Sets the endian mode of data words to big       *
*                              endian.                                         *
* littleendian     le          Sets the endiam mode of data words to little    *
*                              endian.                                         *
* title=n          t=n         Sets the title string on each page.             *
* tab=n,r                      Sets tabs.                                      *
* pageheader       ph          Enable page headers.                            *
* nopageheader     nph         Disable page headers.                           *
* collumnheader    ch          Enable collumn headers.                         *
* nocollumnheader  nch         Disable collumn headers.                        *
* pagetitleheader  pth         Enable page/title headers.                      *
* nopagetitleheader npth       Disable page/title headers.                     *
*                                                                              *
* Each page has a header that appears as follows:                              *
*                                                                              *
*                            title                                  page 1     *
*                                                                              *
*                                                                              *
*    Program  Variable Code              C line Source                         *
*    --------------------------------------------------------------------      *
*    00000000 00000000 00 00 00 00 00 00 +    1 <source>                       *
*                                                                              *
* The title line contains the title of the listing, and this is set by default *
* to be the name of the file being listed. It can be changed by option. The    *
* page number is also printed on this line. This is followed by the line       *
* header, which explains each field of the listing.                            *
*                                                                              *
* Each fields size and if it exists or not is set by options.  Each line of    *
* source has both a program location and a variable location. The size of each *
* address, and if it exists or not, is set by option. If an address is too     *
* large for the field selected, it is printed as all '?' characters.           *
*                                                                              *
* For each line, any number of data words can appear. These represent words    *
* in the program space, and can be instructions or data. For most processors   *
* the word size is byte, and the endian mode does not apply. If the target     *
* processor word size is larger than a byte, and it will only allow word       *
* aligned accesses, options can be used to set the word size and endian mode.  *
* If, however, the assembler can generate bytes, this will generate an error,  *
* because the alignment of the listing can fall out of sync. Note that the     *
* majority of modern microprocessors are byte aligned.                         *
*                                                                              *
* The number of words that can appear on a line can be set by option. By       *
* default, up to 6 words can appear. If more words appear for a line than      *
* can appear on the line, an "extended line" will follow the line with a '+'   *
* mark in the 'C' or contination collumn. These extended lines will appear     *
* until all of the words for the line have been printed. Extended lines can    *
* optionally be suppressed.                                                    *
*                                                                              *
* After the data words, the continuation field appears, followed by the line   *
* number of the source, if it is enabled. The digit size of the line number    *
* can be set by option. If the line is too large for the selected field, it    *
* will be printed as all '?' characters.                                       *
*                                                                              *
* The source line appears last on the line. If the source line is to large for *
* the selected line width, it will be continued on the next line, with a '+'   *
* in the continuation field. It may or may not also be accompanied by data     *
* words, but such extended source lines are not suppressed by turning          *
* extend off.                                                                  *
*                                                                              *
* al can print source lines up to 10,000 characters, and also maintains a set  *
* of tab stops for that many characters per line. The default tab stops are    *
* set starting at line position 9, and continue every 8 characters after that. *
* The tab settings can be changed by the tab option. Whenever a tab option     *
* appears, the default tabs are cleared so that only the user selected tabs    *
* are active. There are two ways to set tabs, either individually, or in       *
* groups:                                                                      *
*                                                                              *
* /tab=9 /tab=18                                                               *
*                                                                              *
* Sets individual tabs at positions 9 and 18.                                  *
*                                                                              *
* /tab=9,8                                                                     *
*                                                                              *
* Sets a group of tabs that start with position 9, and repeat every 8          *
* characters beyond that. In fact, this is the default tab specification.      *
*                                                                              *
* Tabs can be combinined in any way. The total set of tabs is the addition of  *
* all tab specifications (except the default). It is not an error to           *
* redundantly set tabs.                                                        *
*                                                                              *
* If a tab occurs in the source, but there is no further tab beyond the        *
* present line location, the tab will simply be ignored.                       *
*                                                                              *
* Note that the use of tabs in source files is an unreliable method to specify *
* spacing in character based source files. Since the location of the tabs are  *
* not carried with the source file, but are up to the printer or viewer of the *
* source file, it is easy to set them incorrectly and have a misformatted      *
* file as a result. The best method is to use spaces instead of tabs. Legacy   *
* files containing tabs should have their tabs converted to the proper number  *
* of spaces by use of an editor or other utility that can perform this         *
* convertion.                                                                  *
*                                                                              *
* When a form-feed appears in the source, this automatically starts a new      *
* page.                                                                        *
*                                                                              *
* The line tracking system gives two kinds of entries:                         *
*                                                                              *
* 1. Source filename specifications.                                           *
* 2. Line/program/variable change specifications.                              *
*                                                                              *
* The source line specifications tell us anytime a new source filename is to   *
* be used. The filename is fully pathed, and exists anytime a filename change  *
* is needed. For example, if a nested file is being parsed by the assembler,   *
* it will output a source change when the nesting starts, and another change   *
* to go back to the original file.                                             *
*                                                                              *
* The line differences are a set of numbers for the line, program and variable *
* difference since the last entry. Storing only the difference between the     *
* current line and the last means that the precision of the number, and        *
* therefore the number of bytes it occupies, is minimal. This is because the   *
* numbers tend to be small increments in a real world file.                    *
*                                                                              *
* The compression of difference numbers is furthered by representing them in   *
* a "stepped" value format. This works by outputting first as a one byte       *
* number that is not $ff. If that overflows, we output $ff, then a two byte    *
* number that is not $ffff, and so on. This can continue to any length of      *
* number.                                                                      *
*                                                                              *
* Problems/issues:                                                             *
*                                                                              *
* 1. Check dataline = 0 works (shuts off all byte printing in file).           *
*                                                                              *
*******************************************************************************}

program al(output);

uses strlib, { string handling library }
     extlib, { extentions }
     parlib; { parsing }

label 99; { abort program }

const maxfil = 250;   { number of characters in a file name }
      maxdig = 8;     { number of hex digits in integer }
      maxlab = 10;    { maximum characters in label }
      maxcmd = 250;   { maximum length of command line buffer }
      maxlin = 10000; { maximum line to store }

      { default settings }

      defchr = 120;   { number of characters on listing line }
      defpag = 60;    { number of lines per page }
      defmgn = 6;     { standard left margin }
      deftop = 1;     { standard top margin }
      defwrd = 6;     { standard bytes on line }
      deflin = 5;     { default line number digits (99,999) }
      defsiz = 1;     { default size of instruction word (byte) }
      defend = true;  { default endian mode of instruction word (big endian) }
      defpgm = 8;     { default number of digits in program address }
      defvar = 8;     { default number of digits in variable address }

type filinx = 1..maxfil; { index for filenames }
     filnam = packed array [filinx] of char; { filename }
     labinx = 1..maxlab; { index for label }
     lab    = packed array [labinx] of char; { label }

     { symbol file objects }
     objtyp  = (obend,   { end of file }
                obsym,   { symbol }
                obcst,   { constant }
                obrld,   { relocation }
                obcrld,  { constant relocation }
                obblk,   { block begin }
                obblke,  { block end }
                oblin,   { line tracking difference set }
                obsrc,   { line tracking source name }
                obnull); { null } 

var linec:   byte;      { line counter }
    pagec:   byte;      { page counter }
    charc:   byte;      { character counter }
    lincnt:  integer;   { source lines counter }
    linnxt:  integer;   { next line count }
    pgmcnt:  integer;   { program counter }
    pgmnxt:  integer;   { next program counter }
    varcnt:  integer;   { variables counter }
    varnxt:  integer;   { next variables counter }
    symfil:  bytfil;    { symbol file }
    objfil:  bytfil;    { object file }
    srcfil:  text;      { source file }
    lstfil:  text;      { output listing file }
    bytcnt:  integer;   { number of bytes in line }
    bytsav:  integer;
    srcopn:  boolean;   { source file is open }
    symopn:  boolean;   { symbol file is open }
    objopn:  boolean;   { object file is open }
    nambuf:  filnam;    { filename holder }
    titbuf:  filnam;    { title holder }
    titlen:  0..maxfil; { number of characters in title }
    expand:  boolean;   { expand lines flag }
    c:       char;      { character read holder }
    b:       byte;      { read byte holder }
    i:       integer;   { index }
    ti:      filinx;    { index for title label }
    p, n, e: filnam;    { filename components }
    cmdhan:  parhan;    { handle for command parsing }
    err:     boolean;   { error holder }
    valfch:  chrset;    { valid file characters }
    linpag:  integer;   { lines per page }
    chrlin:  integer;   { characters per line }
    mgnlft:  integer;   { left margin }
    mgntop:  integer;   { top margin }
    linovh:  integer;   { line printing overhead }
    datlin:  integer;   { number of bytes to place on line, can be zero }
    tabdrm:  array maxlin of boolean; { tabbing drum }
    tabset:  boolean;   { user tabs were set }
    lindig:  integer;   { digits in line number }
    datsiz:  integer;   { size of instruction word }
    datend:  boolean;   { endian mode of instruction word, true=big endian }
    pgmdig:  integer;   { number of digits in program address }
    vardig:  integer;   { number of digits in variable address }
    paghdr:  boolean;   { turn page header on/off }
    colhdr:  boolean;   { turn collumn header on/off }
    ptihdr:  boolean;   { turn page/titiel header on/off }

    { line tracking entry read in by rdnxt }
    lindif:  integer;   { line difference }
    prgdif:  integer;   { program counter difference }
    vrsdif:  integer;   { variable counter difference }
    srcnam:  pstring;   { source filename }
    endsym:  boolean;   { symbol file end }

{*******************************************************************************

Abort program

*******************************************************************************}

procedure abort;

begin

   goto 99

end;

{*******************************************************************************

Check options

Checks if a sequence of options is present in the input, and if so, parses and 
processes them. An option is a '#', followed by the option identifier. The 
identifier must be one of the valid options. Further processing may occur, on 
input after the option, depending on the option specified (see the handlers).
Consult the operator's manual for full option details.

*******************************************************************************}

procedure paropt;

var n:       lab;     { option string holding }
    err:     boolean; { error flag }
    i, x, j: integer;

{ get number }

procedure getnum(var i: integer);

begin

   skpspc(cmdhan); { skip spaces }
   if chkchr(cmdhan) <> '=' then begin

      writeln('*** "=" expected');
      abort

   end;
   getchr(cmdhan); { skip '=' }
   parnum(cmdhan, i, 10, err); { get number }
   if err or (i < 0) then begin

      writeln('*** Invalid number');
      abort

   end

end;

begin

   skpspc(cmdhan); { skip spaces }
   while chkchr(cmdhan) = optchr do begin { parse option }

      getchr(cmdhan); { skip option introducer }
      parlab(cmdhan, n, err); { parse option label }
      if err then begin

         writeln('*** Invalid option');
         abort

      end;
      { check expand mode }
      if compp(n, 'expand') or compp(n, 'e') then
         expand := true { expand overflow bytes on line }
      else if compp(n, 'noexpand') or compp(n, 'ne') then
         expand := false { don't expand overflow bytes on line }
      else if compp(n, 'lines') or compp(n, 'l') then 
         getnum(linpag) { get lines per page }
      else if compp(n, 'chars') or compp(n, 'c') then 
         getnum(chrlin) { get characters per line }
      else if compp(n, 'leftmargin') or compp(n, 'lm') then 
         getnum(mgnlft) { get left margin }
      else if compp(n, 'topmargin') or compp(n, 'tm') then 
         getnum(mgntop) { get top margin }
      else if compp(n, 'dataline') or compp(n, 'dl') then 
         getnum(datlin) { get number of data items to place on line }
      else if compp(n, 'programdigit') or compp(n, 'pd') then 
         getnum(pgmdig) { get number of digits in a program address }
      else if compp(n, 'variabledigit') or compp(n, 'vd') then 
         getnum(vardig) { get number of digits in a variable address }
      else if compp(n, 'linedigit') or compp(n, 'ld') then 
         getnum(lindig) { get number of digits in a line number }
      else if compp(n, 'wordsize') or compp(n, 'ws') then 
         getnum(datsiz) { get number of bytes in a word }
      else if compp(n, 'bigendian') or compp(n, 'be') then 
         datend := true { set big endian }
      else if compp(n, 'littleendian') or compp(n, 'le') then 
         datend := false { set little endian }
      else if compp(n, 'pageheader') or compp(n, 'ph') then 
         paghdr := true { turn page headers on }
      else if compp(n, 'nopageheader') or compp(n, 'nph') then 
         paghdr := false { turn page headers off }
      else if compp(n, 'collumnheader') or compp(n, 'ch') then 
         colhdr := true { turn collumn headers on }
      else if compp(n, 'nocollumnheader') or compp(n, 'nch') then 
         colhdr := false { turn collumn headers off }
      else if compp(n, 'pagetitleheader') or compp(n, 'pth') then 
         ptihdr := true { turn page/title headers on }
      else if compp(n, 'nopagetitleheader') or compp(n, 'npth') then 
         ptihdr := false { turn page/title headers off }
      else if compp(n, 'title') or compp(n, 't') then begin

         skpspc(cmdhan); { skip spaces }
         if chkchr(cmdhan) <> '=' then begin
        
            writeln('*** "=" expected');
            abort
        
         end;
         getchr(cmdhan); { skip '=' }
         parstr(cmdhan, titbuf, err); { get title string }
         if err then begin
        
            writeln('*** Invalid string');
            abort
        
         end

      end else if compp(n, 'tab') then begin

         j := 0; { set no continuing tabs }
         getnum(i); { get the new tab }
         if (i < 1) or (i > maxlin) then begin

            writeln('*** Invalid tab setting');
            abort

         end;
         skpspc(cmdhan); { skip spaces }
         if chkchr(cmdhan) = ',' then begin { there is a continuation }

            getchr(cmdhan); { skip ',' }
            parnum(cmdhan, j, 10, err); { get number }
            if err then begin
           
               writeln('*** Invalid number');
               abort
           
            end;
            if (i < 1) or (i > maxlin) then begin
            
               writeln('*** Invalid tab setting');
               abort
            
            end

         end;
         { User tabs override the all of the preset tabs, so if this is the
           first tab, clear the tabbing drum. }
         if not tabset then 
            for x := 1 to maxlin do tabdrm[x] := false; { clear tabbing drum }
         tabdrm[i] := true; { set that as tab }
         { if an increment is specified, then place repeating tabs }
         if j > 0 then begin

            i := i+j; { next tab }
            while i <= maxlin do begin

               tabdrm[i] := true; { set tab }
               i := i+j

            end

         end;
         tabset := true { flag user tabs were set }
        
      end else begin { no option found }

         writeln('*** No option found');
         abort

      end;
      skpspc(cmdhan) { skip spaces }

   end

end;

{*******************************************************************************

Find digits in hexadecimal number

Find the number of digits required in an unsigned number.

*******************************************************************************}

function fndhex(i: integer): integer;

var ld: integer; { length in digits }

begin

   ld := 1; { set minimum }
   if i >= $10 then ld := ld+1; { add digits for powers }
   if i >= $100 then ld := ld+1;
   if i >= $1000 then ld := ld+1;
   if i >= $10000 then ld := ld+1;
   if i >= $100000 then ld := ld+1;
   if i >= $1000000 then ld := ld+1;
   if i >= $10000000 then ld := ld+1;

   fndhex := ld { return result }

end;

{*******************************************************************************
                                                             
Print numeric                                               
                                                             
Print integer in any given radix. Prints the number in the radix given to the
file given. The number will be given with leading zeros to make up the field
width.

If the unsigned number does not fit in the allowed field, the field is filled
with '?'s.
                                                             
*******************************************************************************}

procedure wrthex(var f:  text;     { file to print to }
                     fd: byte;     { field width }
                     w:  integer); { value to print }

var i, j: byte;
    v:    integer;

begin

   if fndhex(w) <= fd then { number fits in field }
      for i := 1 to fd do begin { output digits }

         v := w; { save word }
         for j := 1 to fd - i do v := v div 16; { extract digit }
         v := v mod 16; { mask }
         { convert ascii }
         if v >= 10 then v := v + (ord('A') - 10)
         else v := v + ord('0');
         write(f, chr(v)) { output }

      end
   else for i := 1 to fd do write(f, '?') { write unknown to field }

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

{*******************************************************************************

Read next line tracking entry

Reads the next line tracking entry in the file. This can be a differential
entry, or a filename change entry. Both of these are set up in global variables.
If there is no line difference entry, then the difference variables are set to
0. If there is no source file difference, the source next is set to nil.

*******************************************************************************}

procedure rdnxt;

var b:      byte;   { read byte holding }
    l:      byte;   { string length }
    i:      integer;
    nxtobj: objtyp; { next object type }

{ skip variger }

procedure rdvar;

var t: byte;  { tag byte }
    b: byte;  { read byte holder }

begin

   read(symfil, t); { get tag byte }
   t := (t and $1f)+1; { mask byte length and adjust }
   while t <> 0 do begin { skip bytes of value }

      read(symfil, b); { get the next byte }
      t := t-1 { count bytes read }

   end

end;

begin

   repeat

      { clear all next parameters }
      lindif := 0;
      prgdif := 0;
      vrsdif := 0;
      srcnam := nil;
      endsym := false;
      read(symfil, b); { get the next object type }
      if not (objtyp(b) in [obend, obsym, obcst, obrld, obcrld, obblk, 
                            obblke, oblin, obsrc]) then begin

         writeln('*** Invalid symbol file');
         abort

      end;
      nxtobj := objtyp(b); { place next object type }
      if (nxtobj = obsym) or (nxtobj = obcst) then begin { symbol }

         read(symfil, b); { skip the operation code }
         if nxtobj = obsym then begin { get symbol label }

            read(symfil, b); { get the symbol length }
            { skip symbol characters }
            for i := 1 to b+1 do read(symfil, b);

         end;
         read(symfil, b); { skip flags byte }
         if odd(b) then rdvar { defined, get value }

      end else if (nxtobj = obrld) or (nxtobj = obcrld) then begin { rld }

         read(symfil, b); { skip it tag byte }
         read(symfil, b); { skip bit length }
         if (b and $18) div $08 = 2 then read(symfil, b); { skip constant offset }
         rdvar; { skip address }
         if nxtobj = obcrld then begin { constant rld, value exists }

            read(symfil, b); { skip flag byte }
            rdvar { skip value }

         end

      end else if nxtobj = obblk then begin { block }

         rdvar; { skip starting program address }
         rdvar; { skip ending program address }
         rdvar; { skip starting variable address }
         rdvar { skip ending variable address }

      end else if nxtobj = oblin then begin { line tracking entry }

         inpstp(lindif); { get the line difference }
         inpstp(prgdif); { get the program difference }
         inpstp(vrsdif) { get the variable difference }
         
      end else if nxtobj = obsrc then begin { source file name entry }

         read(symfil, l); { get length of filename }
         new(srcnam, l); { allocate string }
         for i := 1 to l do begin { get string }

            read(symfil, b); { get a byte of string }
            srcnam^[i] := chr(b) { place }

         end

      end else if nxtobj = obend then endsym := true { set end of file }
      else begin

         writeln('*** Invalid symbol file');
         abort

      end

   until nxtobj in [oblin, obsrc, obend]

end;      

{*******************************************************************************

Read line from line file

*******************************************************************************}

procedure readlin;

begin

   if not endsym then repeat { not end of symbols, read line spec }

      rdnxt; { read next object from symbols file }
      linnxt := lincnt+lindif; { add to line to find next }
      pgmnxt := pgmcnt+prgdif; { add to program count to find next }
      varnxt := varcnt+vrsdif; { add to variable count to find next }
      if srcnam <> nil then begin { process filename change }

         if srcopn then close(srcfil); { close previous source }
         if not exists(srcnam^) then begin { not found }

            write('*** Input source file ', srcnam^:0, ' does not exist');
            abort

         end;
         assign(srcfil, srcnam^); { open new source file }
         reset(srcfil);
         srcopn := true; { set source file open }
         linec := 0; { clear source line }
         lincnt := 0

      end

   until (srcnam = nil) or endsym { continue until line diff found or end }

end;

{*******************************************************************************

Find digits in decimal number

Find the number of digits required in an unsigned number.

*******************************************************************************}

function fnddig(i: integer): integer;

var ld: integer; { length in digits }

begin

   ld := 1; { set minimum }
   if i >= 10 then ld := ld+1; { add digits for powers }
   if i >= 100 then ld := ld+1;
   if i >= 1000 then ld := ld+1;
   if i >= 10000 then ld := ld+1;
   if i >= 100000 then ld := ld+1;
   if i >= 1000000 then ld := ld+1;
   if i >= 10000000 then ld := ld+1;
   if i >= 100000000 then ld := ld+1;
   if i >= 1000000000 then ld := ld+1;

   fnddig := ld { return result }

end;

{*******************************************************************************

Read word

Reads a word in from the given byte file, with the given length and endian mode.
The length is the number of bytes that consitute the number. The endian mode
tells how to assemble the bytes into a word. Returns an error variable that is
true if eof is encountered.

*******************************************************************************}

procedure readwrd(var f: bytfil;     { file to read from }
                      l: integer;    { length of word in bytes }
                      e: boolean;    { endian mode. true=big endian }
                  var w: integer;    { word read }    
                  var err: boolean); { eof encountered }

var bs: array 4 of byte; { byte holding }
    p:  integer;         { power holding }
    i:  integer;

begin

   err := false; { set no error }
   for i := 1 to 4 do bs[i] := 0; { clear byte array for short reads }
   for i := 1 to l do
      if eof(f) then err := true { end of file encountered }
      else read(f, bs[i]); { read in bytes }
   p := 1; { set 1st power }
   w := 0; { clear result }
   if e then { assemble in big endian mode }
      for i := l downto 1 do begin

      w := w+bs[i]*p; { add this power }
      p := p*256 { find next power }

   end else { assemble in little endian mode }
      for i := 1 to l do begin

      w := w+bs[i]*p; { add this power }
      p := p*256 { find next power }

   end

end;

{*******************************************************************************

Print page header

Prints the header for the current page.

*******************************************************************************}

procedure prtpag;

{ space required for "page xxxx" where xxxx is the page number }

const pagspc = 4+1+4+1;

var i:      integer; { index }
    titspc: integer; { space to title }
    titlen: integer; { length of title string }

begin

   page(lstfil);
   linec := 1; { set line count before header }
   for i := 1 to mgntop do begin

      writeln(lstfil); { output top margin }
      linec := linec+1 { count }

   end;
   if paghdr then begin { output page header }

      if ptihdr then begin { page/title line is on }

         titlen := len(titbuf); { find length of title string }                
         { clip title string to space allowed }
         if titlen > chrlin-mgnlft-pagspc then titlen := chrlin-mgnlft-pagspc;
         titspc := (chrlin-mgnlft) div 2-titlen div 2; { find space to title }
         for i := 1 to mgnlft do write(lstfil, ' '); { output left margin }
         { space to title }
         for i := 1 to titspc do write(lstfil, ' ');
         { output title }
         write(lstfil, titbuf:titlen);
         { space to page, leaving 4 characters for page number }
         for i := titspc+titlen to chrlin-mgnlft-pagspc do write(lstfil, ' ');
         writeln(lstfil, 'Page ', pagec:1);
         linec := linec+1; { count line }
         writeln(lstfil); { space off }
         linec := linec+1; { count line }
         writeln(lstfil); { space off }
         linec := linec+1 { count line }

      end;
      if colhdr then begin { print collumn header }

         for i := 1 to mgnlft do write(lstfil, ' '); { output left margin }
         if pgmdig <> 0 then write(lstfil, 'Program':-pgmdig, ' ');
         if vardig <> 0 then write(lstfil, 'Variable':-vardig, ' '); 
         if datlin <> 0 then write(lstfil, 'Code': -(datlin*(datsiz*2+1)));
         write(lstfil, 'C ');
         if lindig <> 0 then write(lstfil, 'Line':-lindig, ' ');
         writeln(lstfil, 'Source');
         linec := linec+1; { count line }
         for i := 1 to mgnlft do write(lstfil, ' '); { output left margin }
         for i := 1 to chrlin-mgnlft do write(lstfil, '-');
         writeln(lstfil);
         linec := linec+1 { count line }

      end

   end;
   pagec := pagec+1; { increment page }

end;

{*******************************************************************************

Print line

Prints the header for a line. The header is of the format:

      Program  Variable Line Code              C Source
      --------------------------------------------------------------------
      00000000 00000000    1 00 00 00 00 00 00 + <source>

The bytes that appear in the code field are pulled from the object file. The
number of total bytes remaining are put input and output. Up to 6 bytes are
pulled from the object file. The number of bytes printed is subtracted from the
input total and returned. If that is non-zero on exit, then the '+' (more bytes
to come) field is placed. The source line, if the source line flag is input, is
then printed as read from the source file.

The program offset is used to provide a correction factor for bytes
overflowing a single source line.

*******************************************************************************}

procedure prtlin(view line:   string;   { source line to print }
                 var  offchr: integer;  { offset of start of line portion }
                 var  bytcnt: integer;  { number of bytes left to print }
                      offset: integer;  { program offset }
                      prtpos: integer); { virtual print line position }

var bytsav: integer; { byte count save }
    span:   integer; { span of bytes output }
    lpos:   integer; { position on print line }
    l:      integer; { length of line }
    err:    boolean; { error encountered }
    i:      integer;
    w:      integer;

begin

   if linec < 1 then prtpag; { output header }
   for i := 1 to mgnlft do write(lstfil, ' '); { output left margin }
   if pgmdig <> 0 then begin { program count is enabled }

      wrthex(lstfil, pgmdig, pgmcnt+offset); { output program count }
      write(lstfil, ' ')

   end;
   if vardig <> 0 then begin { variable count is enabled }
  
      wrthex(lstfil, vardig, varcnt); { output variable count }
      write(lstfil, ' ')

   end;
   { output object code bytes }
   bytsav := bytcnt; { set count }
   if bytsav > datlin then bytsav := datlin; { limit bytes per line }
   span := 0; { clear span counter }
   while (bytsav > 0) and not eof(objfil) do begin

      readwrd(objfil, datsiz, datend, w, err); { get next instruction word }
      if err then begin

         writeln('*** Premature end of object file');
         abort

      end;
      wrthex(lstfil, datsiz*2, w); { output }
      write(lstfil, ' '); { separate }
      bytsav := bytsav-datsiz; { count bytes }
      span := span+datsiz

   end;
   bytsav := datlin-span; { set remainder count }
   while bytsav <> 0 do begin

      for i := 1 to datsiz*2 do write(lstfil, ' ');
      write(lstfil, ' ');
      bytsav := bytsav-datsiz

   end;
   bytcnt := bytcnt-span;
   if (bytcnt = 0) and (offchr = 1) then write(lstfil, '  ')
   else write(lstfil, '+ ');
   if lindig <> 0 then begin { write line number }

      { print line count or '?'s if too large }
      if fnddig(lincnt) <= lindig then write(lstfil, lincnt:lindig)
      else { write(lstfil, '?':lindig);} 
         for i := 1 to lindig do write(lstfil, '?');
      write(lstfil, ' ')

   end;
   lpos := linovh+1; { set print position after preamble }
   l := len(line); { find total length of line }
   { print line }
   while (offchr <= l) and (lpos <= chrlin) do begin

      { check next character is a tab }
      if line[offchr] = '\ht' then begin

         { search for the next tab stop }
         i := prtpos+1; { index next character position }
         while (i < maxlin) and not tabdrm[i] do i := i+1;
         if tabdrm[i] then begin { tab stop found }
        
            write(lstfil, ' '); { space towards tab }
            lpos := lpos+1; { advance real print position }
            prtpos := prtpos+1; { advance virtual print position }
            { If we have arrived at the tab, then skip it. Otherwise, we will
              keep spacing until we find it, possibly over multiple lines. }
            if tabdrm[prtpos] then offchr := offchr+1 { next position }

         end else offchr := offchr+1 { just ignore tab }

      end else begin

         { print normal character }
         write(lstfil, line[offchr]); { print character from line }
         lpos := lpos+1; { advance real print position }
         prtpos := prtpos+1; { advance virtual print position }
         offchr := offchr+1 { advance source line offset }

      end

   end;
   linec := linec+1; { increment lines on page }
   if linec > linpag then linec := 0; { set next page }
   writeln(lstfil) 

end;

{*******************************************************************************

Print lines

Prints complete line sets. This is either one line, or a series of lines if
expand is on and more bytes exist than can be printed on a single line.

*******************************************************************************}

procedure prtlins;

var bytcnt: integer; { number of bytes in line }
    offset: integer; { program offset }
    linbuf: packed array maxlin of char; { source line holding }
    offchr: integer; { offset in source line }
    prtpos: integer; { offset in printed line }
    linlen: integer; { length of line }

begin

   { get line from source file }
   reads(srcfil, linbuf, err);
   readln(srcfil); { next line }
   if err then begin

      writeln('*** Source line to long');
      abort

   end;
   linlen := len(linbuf); { set total length of line }
   offchr := 1; { set first line character as offset }
   prtpos := 1; { set first print position character }
   offset := 0; { clear program offset }
   { find number of bytes on line }
   if lincnt+1 = linnxt then { bytes appear between this and next }
      bytcnt := pgmnxt-pgmcnt
   else { nothing betweeen this and next }
      bytcnt := 0;
   { if we hit the end of file, assign all remaining bytes to this line }
   if endsym and not eof(objfil) then begin

      bytcnt := length(objfil)-location(objfil)+1;

   end;
   if bytcnt mod datsiz > 0 then begin { must be even to the word }

      writeln('*** Line contains partial words');
      abort

   end;
   { if no data is to be printed, we will just dispose of it }
   if datlin = 0 then 
      while (bytcnt > 0) and not eof(objfil) do 
         begin read(objfil, b); bytcnt := bytcnt-1 end;
   { print the first line, even if it is blank }
   prtlin(linbuf, offchr, bytcnt, offset, prtpos);
   offset := datlin; { set offset for more }
   { If expand is set, then we will print lines, blank or otherwise, until all
     the object bytes appear. If expand is off, we just discard all further
     object bytes without printing. }
   if not expand then
      while (bytcnt > 0) and not eof(objfil) do 
         begin read(objfil, b); bytcnt := bytcnt-1 end;
   { Keep printing while there is more on the line, or if there are more object
     bytes to print. }
   while (offchr <= linlen) or ((bytcnt > 0) and not eof(objfil)) do begin

      prtlin(linbuf, offchr, bytcnt, offset, prtpos);
      offset := offset+datlin { update offset }

   end

end;

{*******************************************************************************

Main program

*******************************************************************************}

begin

   writeln;
   writeln('Code module lister vs. 1.13.00 Copyright (C) 2007 S. A. Moore');
   writeln;

   endsym := false; { set not end of symbol file }
   linec := 0;
   pagec := 1; { 1st page }
   lincnt := 0; { clear line count }
   linnxt := 0; { clear line next }
   pgmcnt := 0; { clear program counter }
   pgmnxt := 0; { clear program next }
   varcnt := 0; { clear variables counter }
   varnxt := 0; { clear variables next }
   srcopn := false; { set source file not open }
   symopn := false; { set symbol file not open }
   objopn := false; { set object file not open }
   expand := false; { set no expand mode }
   linpag := defpag; { set default number of lines per page }
   chrlin := defchr; { set default number of characters on line }
   mgnlft := defmgn; { set default left margin }
   mgntop := deftop; { set default top margin }
   datlin := defwrd; { set default bytes on line }
   clears(titbuf); { set no title active }
   lindig := deflin; { set line count digits to default }
   datsiz := defsiz; { set default byte size of instruction words }
   datend := defend; { set default endian mode of instruction words }
   pgmdig := defpgm; { set default number of digits in program address }
   vardig := defvar; { set default number of digits in variable address }
   paghdr := true; { turn page header on }
   colhdr := true; { turn collumn header on }
   ptihdr := true; { turn page/title header on }
   tabset := false; { set no user tabs were set }
   for i := 1 to maxlin do tabdrm[i] := false; { clear tabbing drum }
   { set up a standard tab drum for every 8th space tabbing }
   i := 9; { set 1st tab }
   while i < maxlin do begin

      tabdrm[i] := true; { set a tab at this position }
      i := i+8 { next tab }

   end;

   { parse and process command line }
   openpar(cmdhan); { open parser }
   openfil(cmdhan, '_command', maxcmd); { open command line level }
   filchr(valfch); { get the filename valid characters }
   valfch := valfch-['=']; { remove parsing characters }
   setfch(cmdhan, valfch); { set that for active parsing }
   paropt; { parse options }
   parfil(cmdhan, nambuf, false, err); { parse filename }
   if err then begin

      writeln('*** Invalid filename');
      abort

   end;
   paropt; { parse options }
   closefil(cmdhan); { close command parse }
   closepar(cmdhan); { close parser }

   { Overhead on printing a line. Includes left margin, program count, variable
     count, room for up to 6 bytes of object code, overflow indicator, and 
     spaces between all. }
   linovh := 
      mgnlft+                      { left margin }
      ord(pgmdig <> 0)*(pgmdig+1)+ { program address }
      ord(vardig <> 0)*(vardig+1)+ { variable address }
      datlin*(datsiz*2+1)+         { data words on line }
      2+                           { continuation field }
      ord(lindig <> 0)*(lindig+1); { line number }

   { create title from filename }
   if len(titbuf) = 0 then brknam(nambuf, p, titbuf, e); { break down name }

   { open symbol file }
   brknam(nambuf, p, n, e); { break down name }
   copy(e, 'sym'); { place symbol extention }
   maknam(nambuf, p, n, e); { reconstruct }
   if not exists(nambuf) then begin { not found }

      write('*** Input symbol file ', nambuf:0, ' does not exist');
      abort

   end;
   assign(symfil, nambuf); { open it }
   reset(symfil);
   symopn := true; { set open }

   { open object file }
   brknam(nambuf, p, n, e); { break down name }
   copy(e, 'obj'); { place object extention }
   maknam(nambuf, p, n, e); { reconstruct }
   if not exists(nambuf) then begin { not found }

      write('*** Input object file ', nambuf:0, ' does not exist');
      abort

   end;
   assign(objfil, nambuf); { open it }
   reset(objfil);
   objopn := true; { set open }

   { open listing file }
   brknam(nambuf, p, n, e); { break down name }
   copy(e, 'lst'); { place object extention }
   maknam(nambuf, p, n, e); { reconstruct }
   assign(lstfil, nambuf); { open it }
   rewrite(lstfil);

   writeln('Creating list file in: ', nambuf:0);

   readlin; { read next line spec }
   if not srcopn then begin { should have a file leading }

      writeln('*** Invalid line file format');
      abort

   end;

   repeat { printout }

      if linec < 1 then prtpag; { output header }
      if not eof(srcfil) then begin { print source line }

         charc := 1; { set inital character count }
         lincnt := lincnt+1; { next source line count }
         if lincnt = linnxt then begin { we have reached the next line }

            pgmcnt := pgmnxt; { set new program count }
            varcnt := varnxt;
            readlin; { read next line spec }

         end;
         if srcfil^ = '\ff' then begin { form line }

               readln(srcfil); { skip contents of line }
               if linec <> 3 then linec := 0 { set next page }

         end else prtlins { print source line }

      end

   until eof(srcfil); { end of file }

   writeln('Function complete');

   99: { terminate program }

   if symopn then close(symfil); { close line file }
   if srcopn then close(srcfil); { close source file }
   if objopn then close(objfil) { close object file }

end.

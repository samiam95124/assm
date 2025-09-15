module db(output);

uses stddef,   { standard defines }
     strlib,   { string library }
     extlib,   { extention library }
     dbdef,    { db defines }
     defi8080, { CPU specific defines }
     simi8080, { simulator defines }
     cmdi8080; { CPU specific command defines }

var

   brklst:  brkptr; { permenent breakpoint list }
   tmplst:  brkptr; { temporary breakpoint list }
   excbuf:  packed array [excinx] of byte; { intermediate buffer }
   excptr:  excinx; { current code fill/execute pointer }

procedure prtnum(r:  byte; fd: byte; w:  integer); forward;
procedure putbrk(p: brkptr); forward;
procedure putbyt(b: byte); forward;
procedure defcmd(view s: string; ccodg: byte; ccodm: byte); forward;
procedure prtfil(var fn: filnam); forward;
procedure getlin(var f: text); forward;
procedure parlin; forward;
procedure exclin; forward;

private

var 

   cmdlin:  packed array [lininx] of char; { command input line }
   cmdlen:  lininx; { length of command line }
   cmdptr:  lininx; { command line index }
   labbuf:  labl; { label buffer }
   vartab:  array [varcod] of labl; { fixed variables lookup table }
   stack:   array [stkinx] of integer; { exec operand stack }
   stkptr:  stkinx; { exec operand stack top }
   brkfre:  brkptr; { free breakpoint entry list }
   cmdlst:  cdfptr; { command definition list }

{*******************************************************************************

Print numeric

Print integer in any given radix. Prints the number in the radix given to the
file given. The number will be given with leading zeros to make up the field
width.

*******************************************************************************}

procedure prtnum(r:  byte;     { radix to print in }
                 fd: byte;     { field width }
                 w:  integer); { value to print }

var i, j: byte;
    v:    integer;

begin

   for i := 1 to fd do begin { output digits }

      v := w; { save word }
      for j := 1 to fd - i do v := v div r; { extract digit }
      v := v mod r; { mask }
      { convert ascii }
      if v >= 10 then v := v + (ord('a') - 10)
      else v := v + ord('0');
      write(chr(v)) { output }

   end

end;

{*******************************************************************************

Get a new breakpoint entry

Either recycles an existing breakpoint entry, or returns a new one.

*******************************************************************************}

procedure getbrk(var p: brkptr);

begin

   if brkfre <> nil then begin { get existing free entry }

      p := brkfre; { get top entry }
      brkfre := brkfre^.next { gap list }

   end else new(p); { get a new entry }
   p^.addr := 0; { clear entry }
   p^.data := 0;
   p^.next := nil

end;

{*******************************************************************************

Put used breakpoint entry

Places a used breakpoint on the free list.

*******************************************************************************}

procedure putbrk(p: brkptr);

begin

   p^.next := brkfre; { link onto free list }
   brkfre := p

end;

{*******************************************************************************

Load text line

The line contained in the given text file is loaded into the inplin buffer, and
inpptr is reset to 1.
The line can be terminated by either an eoln or eof. In the case of eoln, the
eoln is skipped, and the file positioned at the next line.

*******************************************************************************}

procedure getlin(var f: text);

label 1; { eoln jump }

var i: lininx; { line index }

begin

   if not eof(f) then begin { read line }

      { clear input line }
      for i := 1 to maxlin do cmdlin[i] := ' ';
      i := 1; { 1st position }
      while (i <= maxlin) and not eof(f) do begin

         if eoln(f) then goto 1; { abort on eoln }
         { check line overflow }
         if (i < maxlin) then begin

            read(f, cmdlin[i]); { get a command character }
            i := i + 1 { next position }

         end

      end;
      1: { abort loop }
      if not eof(f) then readln(f); { skip line }
      cmdlen := i - 1; { set length of line }
      cmdptr := 1 { reset line pointer }

   end

end;

{*******************************************************************************

Check end of line

Checks if we are at the end of the command line.

*******************************************************************************}

function endlin: boolean;

begin

   endlin := cmdptr > cmdlen { input pointer past end of line }

end;

{*******************************************************************************

Check next character

Returns the next character in the command line, or space if we have reached the
end.

*******************************************************************************}

function chkchr: char;

begin

   if not endlin then chkchr := cmdlin[cmdptr] { return current character }
   else chkchr := ' ' { else return space }

end;

{*******************************************************************************

Get next character

Skips to the next character in the command line. If we are at the end, does
nothing.

*******************************************************************************}

procedure getchr;

begin

   if not endlin then cmdptr := cmdptr+1 { advance position if not end }

end;

{*******************************************************************************

Skip spaces

Skips spaces in the command line. If we are at the end, does nothing.

*******************************************************************************}

procedure skpspc;

begin

   while (chkchr = ' ') and not endlin do getchr { skip spaces, not end }

end;

{*******************************************************************************

Check end of command

Checks if we are either at the end of the command line, or at a ";" character,
which is the command separator.

*******************************************************************************}

function endcmd: boolean;

begin

   endcmd := (chkchr = ';') or endlin { command separator or end of line }

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

{******************************************************************************
 
Parse label
 
Parses a label from the input. Labels consist of a sequence of characters in
the set ['0'..'9', 'A'..'Z', 'a'..'z', '_']. The caller is responsible for
verifying any extra resrictions on the first character (typ. ['A'..'Z',
'a'..'z', '_']).
The label is returned in the system variable labbuf.
 
*******************************************************************************}
 
procedure parlab;
 
var i: labinx; { label index }
 
begin

   skpspc; { skip spaces }
   for i := 1 to maxlab do labbuf[i] := ' '; { clear buffer }
   i := 1;
   { check label exists }
   if not (alpha(chkchr) or (chkchr = '_')) then prterr(elabexp);
   while alpha(chkchr) or digit(chkchr) or (chkchr = '_') do
      begin { accept valid characters }

      if i = maxlab then prterr(elabtl); { too long }
      labbuf[i] := chkchr; { place character }
      getchr; { skip character }
      i := i+1 { count }

   end

end;

{*******************************************************************************

Print filename

*******************************************************************************}

procedure prtfil(var fn: filnam);

var i: filinx; { index for filename }

begin

   for i := 1 to maxfil do if fn[i] <> ' ' then write(fn[i])

end;

{*******************************************************************************

Find labels equal

Checks if the two labels equal each other, regarless of case.

*******************************************************************************}

function labequ(var a: labl; b: labl): boolean;

var i: labinx; { index for labels }
    f: boolean; { match flag }

begin

   f := true; { set matches }
   { check label matches }
   for i := 1 to maxlab do if lcase(a[i]) <> lcase(b[i]) then f := false;
   labequ := f { return match status }

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

*******************************************************************************}

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
      prterr(enfmt); { invalid digit }
   while (alpha(chkchr) and (r = 16)) or
      digit(chkchr) do begin { load buffer }
   
         { convert '0'..'9' }
         if digit(chkchr) then v := ord(chkchr) - ord('0')
         else v := ord(lcase(chkchr)) - ord('a') + 10; { convert 'a'..'z' }
         getchr; { skip }
         if v >= r then prterr(edbr); { check fits radix }
         i := i * r + v { scale and add in }

      end;

end;

{*******************************************************************************

Define command entry

Places a new command in the command list. For each command, the compile and the
execute code is defined. This is a non-zero byte code that controls which
compile statement is selected to compile the command at compile time, and which
execute statement is selected to execute at runtime.

There are two types of codes, one for global, machine indepedent commands, and
another for machine dependent commands. Each of these codes can occupy from
1 to 127, and in actuality, they are united to a single code in the execute
format. The global commands occupy 1 to 127, and the machine commands occupy
129 through 255.

*******************************************************************************}

procedure defcmd(view s: string;    { name of command }
                      ccodg: byte;  { global compile command }
                      ccodm: byte); { machine compile command }

var cp: cdfptr; { command entry pointer }

begin

   new(cp); { get a new command entry }
   copy(cp^.name, s); { place command name }
   cp^.ccodg := ccodg; { place global compile command }
   cp^.ccodm := ccodm; { place machine compile command }
   cp^.next := cmdlst; { push onto command list }
   cmdlst := cp

end;

{*******************************************************************************

Place byte in code buffer

Paces the given byte in the code buffer, and advances. Errors on overflow.

*******************************************************************************}

procedure putbyt(b: byte);

begin

   if excptr = maxexc then prterr(ecodovf); { code buffer overflow }
   excbuf[excptr] := b; { place intermediate byte }
   excptr := excptr+1 { next location }

end;

{*******************************************************************************

Get byte from code buffer

Get the next byte from the code buffer, and advances. Errors on overflow.

*******************************************************************************}

procedure getbyt(var b: byte);

begin

   if excptr = maxexc then prterr(esys); { code buffer overflow }
   b := excbuf[excptr]; { place intermediate byte }
   excptr := excptr+1 { next location }

end;

{*******************************************************************************

Place integer in code buffer

Places an integer in the code buffer and advances.

*******************************************************************************}

procedure putint(n: integer);

var r: record case boolean of { converter case }
      
          false: (a: integer);
          true:  (b: packed array [1..4] of byte)

       end;
    i: 1..4; { index for same }

begin

   r.a := n; { convert integer }
   for i := 4 downto 1 do putbyt(r.b[i]) { place in code }

end;

{*******************************************************************************

Get integer from code buffer

Gets an integer from the code buffer and advances.

*******************************************************************************}

procedure getint(var n: integer);

var r: record case boolean of { converter case }
      
          false: (a: integer);
          true:  (b: packed array [1..4] of byte)

       end;
    i: 1..4; { index for same }
    b: byte; { byte holder }

begin

   for i := 4 downto 1 do begin getbyt(b); r.b[i] := b end; { load integer }
   n := r.a { get integer }

end;

{*******************************************************************************

Find variable

Parses and looks up a variable by name. Returns the variable code. This should
probally be restated as a procedure.

*******************************************************************************}

function fndvar: varcod;

var s, c: varcod; { variable code indexes }

begin

   parlab; { get command label }
   s := vnull; { set no variable }
   for c := vnull to vend do if labequ(vartab[c], labbuf) then s := c;
   if s = vnull then prterr(evarnf); { variable not found }
   fndvar := s { return resulting variable }

end;

{*******************************************************************************

Place code constant

Places the given integer as a loadable constant.

*******************************************************************************}

{ put constant in code }

procedure putcst(n: integer);

begin

   putbyt(ord(ilodc)); { place as load }
   putint(n)

end;

{*******************************************************************************

Compile expression

Compiles an expression to the equivalent sequence in the code buffer. At this
time, all that is implemented is to get a number.

*******************************************************************************}

{ compile expression }

procedure expr;

var n: integer; { temp }

begin

   parnum(n); { get value }
   putbyt(ord(ilodc)); { place as load }
   putint(n)

end;

{*******************************************************************************

Compile command

Compiles the command by the given command code and places that in the code
buffer.

*******************************************************************************}

procedure cmpcmd(c: cmdcod);

var v: varcod; { variable code holder }

begin

   case c of { command }

      cds:   putbyt(ord(ids));  { single step with display }
      css:   putbyt(ord(iss));  { single step }
      ccp:   prterr(ecmdnimp);
      cse:   prterr(ecmdnimp); 
      csn:   prterr(ecmdnimp);
      cnbu:  prterr(ecmdnimp);
      cret:  prterr(ecmdnimp);
      cptr:  prterr(ecmdnimp);
      cnptr: prterr(ecmdnimp);
      cht:   prterr(ecmdnimp);
      cnht:  prterr(ecmdnimp);
      cport: begin { input output port }

         expr; { load port address }
         skpspc; { skip spaces }
         if chkchr = ',' then begin { output operation }
   
            getchr; { skip ',' }
            expr; { load value address }
            putbyt(ord(iout)) { place output code }

         end else putbyt(ord(iin)) { place input code } 

      end;
      cmove: prterr(ecmdnimp);
      cfill: prterr(ecmdnimp);
      creg:  putbyt(ord(ireg)); { display cpu status/registers }
      cdump: begin { dump memory }

         expr; { load starting address }
         skpspc; { skip spaces }
         if chkchr = ',' then begin { end address exists }

            getchr; { skip ',' }
            expr { load end address }

         end else putcst(0); { else default to 0 }
         putbyt(ord(idump)) { list code }

      end;
      centr: begin { enter memory }

         expr; { load starting address }
         putbyt(ord(iloda)); { place "load address" instruction }
         skpspc; { skip spaces }
         if chkchr <> ',' then prterr(ecmaexp); { ',' expected }
         repeat { entry values }

            getchr; { skip ',' }
            expr; { load value }
            putbyt(ord(iputm)); { place "put" instruction }
            skpspc { skip spaces }

         until chkchr <> ',' { until no more values }

      end;
      clist: begin { list machine code } 

         expr; { load starting address }
         skpspc; { skip spaces }
         if chkchr = ',' then begin { end address exists }

            getchr; { skip ',' }
            expr { load end address }

         end else putcst(0); { otherwise default to 0 }
         putbyt(ord(ilist)) { list code }

      end;
      cgo:   begin { execute program }

         skpspc; { skip spaces }
         if not endcmd then begin { go address exists }

            expr; { load execution address }
            putbyt(ord(igo)) { place go code }

         end else putbyt(ord(igop)) { place go code using present addr }

      end;
      cclr:  prterr(ecmdnimp);
      cbrk:  begin { set/display breakpoints }

         skpspc; { skip spaces }
         if endcmd then putbyt(ord(idisb)) { display all breakpoints }
         else begin { break addresses specified }

            expr; { load break address }
            putbyt(ord(isetb)); { set breakpoint }
            skpspc; { skip spaces }
            while chkchr = ',' do begin { more break addresses }

               getchr; { skip ',' }
               expr; { load break address }
               putbyt(ord(isetb)); { set breakpoint }
               skpspc { skip spaces }
    
            end

         end

      end;
      cprt:  begin { print value }

         expr; { load value to print }
         putbyt(ord(iprt)); { place print code }
         skpspc; { skip spaces }
         while chkchr = ',' do begin { further parameters }

            getchr; { skip ',' }
            expr; { load value to print }
            putbyt(ord(iprt)); { place print code }
            skpspc { skip spaces }

         end

      end;
      cutl:  prterr(ecmdnimp);
      cstp:  prterr(ecmdnimp);
      cquit: putbyt(ord(iquit)); { quit DB }
      cass:  begin

         v := fndvar; { find variable }
         skpspc; { skip spaces }
         if chkchr <> ',' then prterr(ecmaexp); { ',' expected }
         getchr; { skip ',' }
         expr; { load value }
         putbyt(ord(iass)); { place assign instruction }
         putbyt(ord(v)) { place variable code }

      end;
      cei:   putbyt(ord(iei)); { enable interrupts }
      cdi:   putbyt(ord(idi)); { enable interrupts }
      chelp: putbyt(ord(ihelp)); { help commands }
             
   end

end;

{*******************************************************************************

Parse and compile command

Parses and compiles the next command on the line, placing that in the code
buffer.

*******************************************************************************}

procedure parcmd;

var g, m: byte; { command indexes }
    cp:   cdfptr; { command pointer }

begin

   skpspc; { skip spaces }
   parlab; { get command label }
   g := 0; { set no command }
   m := 0;
   cp := cmdlst; { index top of command list }
   while cp <> nil do begin { traverse list }

      { check entry matches our command name, and set compile code if so }
      if labequ(cp^.name, labbuf) then begin

         g := cp^.ccodg; { get global code }
         m := cp^.ccodm { get machine specific code }

      end;
      cp := cp^.next { next command in list }

   end;
   if (g = 0) and (m = 0) then prterr(ecmdnf); { command not found }
   if g <> 0 then cmpcmd(cmdcod(g)) { compile global command }
   else compmach(m) { compile machine specific }

end;

{*******************************************************************************

Parse and compile line

Parses and compiles a series of commands on the same line, placing the result in
the code buffer.

*******************************************************************************}

procedure parlin;

begin

   excptr := 1; { reset code buffer fill pointer }
   skpspc; { skip spaces }
   while not endlin do begin { parse commands on line }

      parcmd; { parse command }
      skpspc; { skip remaining spaces }
      if not endcmd then prterr(ecmdend); { end of command expected }
      if chkchr = ';' then begin { another command cometh }

         getchr; { skip ';' }
         skpspc; { skip spaces }
         if endlin then prterr(ecmdexp); { command expected }

      end

   end;
   putbyt(ord(iendl)) { terminate intermediate buffer }

{ this diagnostic prints the intermediate code to be executed }

{;i := 1; while i < excptr do begin prtnum(16, 2, excbuf[i]); write(' ');
i := i+1 end; writeln;}

end;

{*******************************************************************************

Dump memory

Dumps memory between the given start and end addresses.

*******************************************************************************}

procedure dump(s, e: integer);

var bytes: 0..16;   { bytes on line count }
    ls:    integer; { line start save }
    i:     1..16;   { byte index }

procedure prtasc(c: byte); { print byte as ascii character }

begin

   c := c and $7f; { mask parity }
   { replace unprintable characters with '.' }
   if (c < ord(' ')) or (c > ord('~')) then c := ord('.');
  write(chr(c)) { output }

end;

{ print ascii field }

procedure prtacf;

begin

   write(' *'); { bracket }
   while ls < s do begin { output ascii characters }

      prtasc(getmem(ls, 1)); { output character }
      ls := ls+1 { end }

   end;
   write('*') { bracket } 

end;

begin

   bytes := 0; { initalize byte count }
   ls := s; { save line start address }
   while s <= e do begin { dump bytes }

      if bytes = 0 then begin { print line address }

         prtnum(16, 4, s); { print address }
         write(': ') { separate }

      end;
      prtnum(16, 2, getmem(s, 1)); { print a byte }
      write(' ');
      bytes := bytes + 1; { count bytes }
      s := s + 1; { next address }
      if bytes >= 16 then begin { line full, finish }

         prtacf; { print ascii field }
         writeln; { terminate }
         bytes := 0 { clear byte count }

      end

   end;
   if bytes <> 0 then begin { last line unfinished }

      for i := 1 to 16-bytes do write('   '); { pad missing bytes }
      prtacf; { print ascii field }
      writeln { terminate }

   end

end;

{*******************************************************************************

Execute command

Executes a single intermediate command. The command at the current exec buffer
position is executed.

*******************************************************************************}

procedure exccmd;

var i:    byte;    { instruction holder }
    s, e: integer; { start and end }
    ea:   integer; { byte entry address }
    n:    integer; { temp }
    v:    byte;    { temp }
    bp:   brkptr;  { breakpoint list entry pointer }

begin

   i := excbuf[excptr]; { get instruction }
   excptr := excptr+1; { advance next instruction }
   case intcod(i) of { intermediate command }

      ireg: display; { display cpu status/registers }
      ids: begin exeins; display end;
      iss: exeins; { execute single instruction }
      iquit: abort; { terminate DB }
      ilist: begin { list machine code } 

         stkptr := stkptr-1; { pop ending address }
         e := stack[stkptr];
         stkptr := stkptr-1; { pop starting address }
         s := stack[stkptr];
         while s <= e do begin { list code }

            listinst(s); { list instruction }
            s := s+instlen(s) { skip instruction }
 
         end

      end;
      idump: begin { dump memory } 

         stkptr := stkptr-1; { pop ending address }
         e := stack[stkptr];
         stkptr := stkptr-1; { pop starting address }
         s := stack[stkptr];
         dump(s, e) { perform dump }

      end;
      iass: begin { variable assign }

         stkptr := stkptr-1; { get value }
         n := stack[stkptr];
         getbyt(v); { get the variable code }
         case v of { variable }

             1  { va  }:  putreg(rega, n and $ff); { cpu register a }
             2  { vb  }:  putreg(regb, n and $ff); { cpu register b }
             3  { vc  }:  putreg(regc, n and $ff); { cpu register c }
             4  { vd  }:  putreg(regd, n and $ff); { cpu register d }
             5  { ve  }:  putreg(rege, n and $ff); { cpu register e }
             6  { vh  }:  putreg(regh, n and $ff); { cpu register h }
             7  { vl  }:  putreg(regl, n and $ff); { cpu register l }
             8  { vbc }: begin putreg(regc, n and $ff); { cpu register bc }
                               putreg(regb, (n div 256) and $ff) end;
             9  { vde }: begin putreg(rege, n and $ff); { cpu register de }
                               putreg(regd, (n div 256) and $ff) end;
             10 { vhl }: begin putreg(regl, n and $ff); { cpu register hl }
                               putreg(regh, (n div 256) and $ff) end;
             11 { vpc }: putreg(regpc, n and $ffff); { cpu register pc }
             12 { vsp }: putreg(regsp, n and $ffff); { cpu register sp }
             13 { vf  }: begin { cpu flags register }
 
                putreg(regfs, n and $80); putreg(regfz, n and $40);
                putreg(regf5, n and $20); putreg(regfa, n and $10);
                putreg(regf3, n and $08); putreg(regfp, n and $04);
                putreg(regf1, n and $02); putreg(regfc, n and $01)

             end

          end

      end;
{ The I/O instructions should go to the machine side }
      iin: begin { input value from port }

         stkptr := stkptr-1; { get address to input }
         v := getio(stack[stkptr], 1); { input value from port }
         prtnum(16, 2, v); { print }
         writeln { next line }

      end;
      iout: begin { output value to port }

         stkptr := stkptr-1; { get value to output }
         n := stack[stkptr];
         stkptr := stkptr-1; { get address to output }
         putio(stack[stkptr], n, 1) { output value to port }

      end;
      iei: putreg(regie, ord(true)); { enable interrupts }
      idi: putreg(regie, ord(false)); { disable interrupts }
      igo: begin { execute program }

         stkptr := stkptr-1; { get address to execute }
         goins(stack[stkptr]) { execute }

      end;
      igop: goins(getreg(regpc)); { go present address }
      iprt: begin { print value }

         stkptr := stkptr-1; { get value to print }
         prtnum(16, 4, stack[stkptr]); { print }
         writeln { next line }

      end;
      idisb: begin { display all breakpoints }

         bp := brklst; { index top of breakpoint list }
         while bp <> nil do begin { traverse list }

            prtnum(16, 4, bp^.addr); { print address of breakpoint }
            writeln; { next line }
            bp := bp^.next { link next entry }

         end

      end;
      isetb: begin { set permenent breakpoint }

         stkptr := stkptr-1; { get address to set }
         getbrk(bp); { get a new breakpoint entry }
         bp^.next := brklst; { insert into permenent list }
         brklst := bp;
         bp^.addr := stack[stkptr] and $ffff { place address of breakpoint }

      end;
      ihelp: begin { help commands }

          writeln('fpe                - Flag parity even');
          writeln('fpo                - flag parity odd');
          writeln('fp                 - flag positive');
          writeln('fm                 - flag minus');
          writeln('fz                 - flag zero');
          writeln('fnz                - flag non-zero');
          writeln('fa                 - flag arithmetic carrry');
          writeln('fna                - flag no arithmetic carry');
          writeln('fc                 - flag carry');
          writeln('fnc                - flag no carry');
          writeln('s                  - display step');
          writeln('ss                 - single step');
          writeln('cp                 - compare');
          writeln('se                 - search');
          writeln('sn                 - search not');
          writeln('nbu                - no break until');
          writeln('ret                - return');
          writeln('ptr                - printer');
          writeln('nptr               - no printer');
          writeln('ht                 - halt trap');
          writeln('nht                - no halt trap');
          writeln('io port[,data]     - port input/output');
          writeln('m                  - move');
          writeln('f                  - fill');
          writeln('r                  - registers');
          writeln('d start[,end]      - dump');
          writeln('e addr,val[,val].. - enter');
          writeln('l start[,end]      - list');
          writeln('g start[,brk]..    - go');
          writeln('c [brk[,brk]..]    - clear');
          writeln('b brk[,brk]..      - set breakpoints');
          writeln('p val[,val]..      - print');
          writeln('u cond             - until');
          writeln('st                 - stop');
          writeln('a var,val          - assign');
          writeln('quit               - quit debugger');
          writeln('ei                 - enable interrupts');
          writeln('di                 - disable interrupts');
          writeln('h                  - help commands');

      end;
      ilodc: begin getint(stack[stkptr]); { load constant }
                           stkptr := stkptr+1 end;
      iloda: begin { get entry address }

         stkptr := stkptr-1; { get entry value }
         ea := stack[stkptr] 

      end;
      iputm: begin { place memory value }

         stkptr := stkptr-1; { get entry value }
         putmem(ea, stack[stkptr], 1); { place in memory }
         ea := ea+1 { next address }

      end;
      iendl: ; { end of line }
   
   end

end;

{*******************************************************************************

Execute buffer

Executes a single code buffer. At this point, the buffer contains single
instruction codes followed by parameters. It is excuted until the end of the
buffer is detected.

*******************************************************************************}

procedure exclin;

begin

   excptr := 1; { reset execution pointer }
   stkptr := 1; { reset stack pointer }
   { execute commands until end of line }
   while not (excbuf[excptr] = ord(iendl)) do begin

      if excbuf[excptr] >= 128 then execmach { execute machine specific }
      else exccmd

   end

end;

{*******************************************************************************

Initialze DB

*******************************************************************************}

begin

   brklst := nil; { clear permenent breakpoint list }
   tmplst := nil; { clear temporary breakpoint list }
   brkfre := nil; { clear free breakpoint entry list }
   cmdlst := nil; { clear command definition list }
   { initalize command table 
          Name            Compile G   CM }
   defcmd('s',            ord(cds),   0); { single step with display }
   defcmd('stepdisplay',  ord(cds),   0); { single step with display }
   defcmd('ss',           ord(css),   0); { single step }
   defcmd('singlestep',   ord(css),   0); { single step }
   defcmd('cp',           ord(ccp),   0); { compare }
   defcmd('comp',         ord(ccp),   0); { compare }
   defcmd('se',           ord(cse),   0); { search }
   defcmd('search',       ord(cse),   0); { search }
   defcmd('sn',           ord(csn),   0); { search not }
   defcmd('searchnot',    ord(csn),   0); { search not }
   defcmd('nbu',          ord(cnbu),  0); { no break until }
   defcmd('nobreakuntil', ord(cnbu),  0); { no break until }
   defcmd('ret',          ord(cret),  0); { return }
   defcmd('ptr',          ord(cptr),  0); { printer }
   defcmd('printer',      ord(cptr),  0); { printer }
   defcmd('nptr',         ord(cnptr), 0); { no printer }
   defcmd('noprinter',    ord(cnptr), 0); { no printer }
   defcmd('ht',           ord(cht),   0); { halt trap }
   defcmd('halttrap',     ord(cht),   0); { halt trap }
   defcmd('nht',          ord(cnht),  0); { no halt trap }
   defcmd('nohalttrap',   ord(cnht),  0); { no halt trap }
   defcmd('io',           ord(cport), 0); { read/write port }
   defcmd('m',            ord(cmove), 0); { move memory }
   defcmd('move',         ord(cmove), 0); { move memory }
   defcmd('f',            ord(cfill), 0); { fill memory }
   defcmd('fill',         ord(cfill), 0); { fill memory }
   defcmd('r',            ord(creg),  0); { display registers }
   defcmd('registers',    ord(creg),  0); { display registers }
   defcmd('d',            ord(cdump), 0); { dump memory }
   defcmd('dump',         ord(cdump), 0); { dump memory }
   defcmd('e',            ord(centr), 0); { enter memory }
   defcmd('enter',        ord(centr), 0); { enter memory }
   defcmd('l',            ord(clist), 0); { list assembly language }
   defcmd('list',         ord(clist), 0); { list assembly language }
   defcmd('g',            ord(cgo),   0); { go }
   defcmd('go',           ord(cgo),   0); { go }
   defcmd('c',            ord(cclr),  0); { clear memory }
   defcmd('clear',        ord(cclr),  0); { clear memory }
   defcmd('b',            ord(cbrk),  0); { breakpoints }
   defcmd('break',        ord(cbrk),  0); { breakpoints }
   defcmd('p',            ord(cprt),  0); { print }
   defcmd('print',        ord(cprt),  0); { print }
   defcmd('u',            ord(cutl),  0); { until }
   defcmd('until',        ord(cutl),  0); { until }
   defcmd('st',           ord(cstp),  0); { stop }
   defcmd('stop',         ord(cstp),  0); { stop }
   defcmd('a',            ord(cass),  0); { assign }
   defcmd('assign',       ord(cass),  0); { assign }
   defcmd('quit',         ord(cquit), 0); { quit debugger }
   defcmd('ei',           ord(cei),   0); { enable interrupts }
   defcmd('di',           ord(cdi),   0); { disable interrupts }
   defcmd('h',            ord(chelp), 0); { help commands }
   defcmd('help',         ord(chelp), 0); { help commands }
   { intalize predefined variables table }
   copy(vartab[vnull], '');
   copy(vartab[   va], 'a');
   copy(vartab[   vb], 'b');
   copy(vartab[   vc], 'c');
   copy(vartab[   vd], 'd');
   copy(vartab[   ve], 'e');
   copy(vartab[   vh], 'h');
   copy(vartab[   vl], 'l');
   copy(vartab[  vbc], 'b');
   copy(vartab[  vde], 'd');
   copy(vartab[  vhl], 'h');
   copy(vartab[  vpc], 'p');
   copy(vartab[  vsp], 's');
   copy(vartab[   vf], 'f');
   copy(vartab[ vend], '');

end.

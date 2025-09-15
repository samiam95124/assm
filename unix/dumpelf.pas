{******************************************************************************
*                                                                             *
*                             ELF FILE DUMPER                                 *
*                                                                             *
*                            02/05 S. A. Moore                                *
*                                                                             *
* Dumps Unix ELF files. Dumps pretty much everything literally from the file. *
*                                                                             *
******************************************************************************}

program elfdump(command, output);

uses strlib,
     extlib;


label 99; { abort program }

const

   hdrfix = 176;  { number of bytes in the fixed portion of the header }
   maxlab = 30;   { number of characters in label }
   maxext = 4;    { number of characters in an extention }
   maxlin  = 200; { number of characters in a text line }
   maxfil  = 100; { number of characters in a file name }

type 

   lininx  = 1..maxlin;  { index for text line }
   linbuf  = packed array [lininx] of char; { a text line }
   filinx  = 1..maxfil; { index for file names }
   filnam  = packed array [filinx] of char; { a file name }
   { errors }
   errcod = (efilovf,  { filename too long }
             einvfil,  { Invalid filename }
             einvcmd,  { command line syntax invalid }
             einvffm,  { invalid file format }
             efilnf,   { input file not found }
             ecmdovf); { command line overflow }

var 

   exefil:  bytfil;  { .exe file to read from }
   cmdlin:  linbuf;  { command line buffer }
   cmdptr:  lininx;  { command line index }
   cmdlen:  lininx;  { command line length }
   cmdovf:  boolean; { command line overflow }
   valfch:  chrset;  { valid file characters }
   dmpfil:  filnam;  { file to dump }
   fopnsrc: boolean; { source file is open }
   phdoff:  integer; { program header offset }
   phdcnt:  integer; { number of program headers }
   shdoff:  integer; { section header table offset }
   shdcnt:  integer; { number of section headers }
   shdsiz:  integer; { size of section header }
   shsinx:  integer; { index for section header strings }
   strtab:  integer; { string table offset }
   w:       integer;
   i:       integer;
   b:       byte;

{******************************************************************************

Process error

Prints the given error code, and aborts the program.

******************************************************************************}

procedure error(e: errcod);

begin

   write('*** ');
   case e of { error }

      efilovf: write('Filename too long');
      einvfil: write('Invalid filename');
      einvcmd: write('Command line invalid');
      einvffm: write('ELF file format invalid');
      efilnf:  write('Input file not found');
      ecmdovf: write('Command line too long');

   end;
   writeln; { terminate line }
   goto 99 { terminate program }

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

Parse filename

Gets a filename from the command line and validates it.

******************************************************************************}

procedure parfil(var n: filnam); 

var fi:  0..maxfil;  { index for filename }

begin

   clears(n); { clear filename }
   fi := 1; { set 1st character }
   while chkchr in valfch do begin

      if fi = maxfil then error(einvfil); { overflow }
      fi := fi+1; { next character }
      n[fi] := chkchr; { place }
      getchr { skip to next }

   end;
   if not valid(n) then error(einvfil); { check and error on filename }

end;

{******************************************************************************

Parse command line

The structure of a command line is:

     file

The first file is the input .exe file.

******************************************************************************}

procedure parcmd;

begin

   parfil(dmpfil); { parse file to dump }
   skpspc; { skip spaces }
   if not endlin then error(einvcmd) { invalid command line }

end;

{******************************************************************************

Read a 16 bit word from file

******************************************************************************}

procedure readwrd(var f: bytfil; var w: integer);

var b1, b2: byte;
    i1, i2: integer;

begin

   read(f, b1); { get low byte }
   read(f, b2); { get high byte }
   i1 := b1; { expand the value }
   i2 := b2;
   w := i2*256+i1 { place result }

end;
    
{******************************************************************************

Read a 32 bit word from file

******************************************************************************}

procedure readdwd(var f: bytfil; var w: integer);

var b1, b2, b3, b4: byte;
    i1, i2, i3, i4: integer;

begin

   read(f, b1); { get low byte }
   read(f, b2); { get mid low byte }
   read(f, b3); { get mid high byte }
   read(f, b4); { get high byte }
   i1 := b1; { expand the value }
   i2 := b2;
   i3 := b3;
   i4 := b4;
   w := i4*16777216+i3*65536+i2*256+i1 { place result }

end;

{******************************************************************************

Print hexadecimal

Print a hexadecimal number with field width. Prints right justified with left
hand zeros filling the field. Also allows for the fact that an unsigned 32 bit
number can be read into a 32 bit signed number.
One remaining problem is how to detect and convert the invalid value $80000000.

******************************************************************************}

procedure prthex(f: byte; w: integer);
 
var buff: array [1..10] of char; { buffer for number in ascii }
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
   hexsp(buff, w); { convert the integer }
   for i := 1 to f-lenp(buff) do write('0'); { pad with leading zeros }
   writesp(output, buff) { output number }

end;

{******************************************************************************

Print string

Prints a string given as an offset from the string table. Positions to that
location in the string table, prints the string, then restores the position.

******************************************************************************}

procedure prtstr(off: integer); { offset for string }

var l: integer; { file location save }
    b: byte;

begin

   l := location(exefil); { save the current location of the file }
   position(exefil, strtab+off+1); { position to that }
   repeat { read and print characters }

      read(exefil, b); { get next }
      if b <> 0 then write(chr(b)); { print }
   
   until b = 0; { until zero terminate }
   position(exefil, l) { restore position }

end;


begin

   { initalize }
   fopnsrc := false; { set source file not open }
   filchr(valfch); { get the filename valid characters }

   { get command line }
   readsp(command, cmdlin, cmdovf);
   if cmdovf then error(ecmdovf); { too long }
   cmdlen := lenp(cmdlin); { find length }
   cmdptr := 1; { set 1st character }
   parcmd; { parse command line }

   { open dump file }
   if not exists(dmpfil) then error(efilnf);
   assign(exefil, dmpfil); { open the dump file }
   reset(exefil);
   fopnsrc := true; { set source file is open }

   { read and print header section }
   writeln;
   writeln('Dump for ELF format file: ');
   writesp(output, dmpfil);
   writeln;
   writeln;
   writeln('ELF header');
      writeln('==============================================================');
   read(exefil, b); { skip }
   if b <> $7f then error(einvffm); 

   { validate 'ELF' }
   read(exefil, b);
   if b <> ord('E') then error(einvffm); 
   read(exefil, b);
   if b <> ord('L') then error(einvffm); 
   read(exefil, b);
   if b <> ord('F') then error(einvffm);
   writeln('Magic numbers:                           $7f, ''ELF''');
   read(exefil, b);
   if (b < 1) or (b > 2) then error(einvffm);
   write('Class:                                   ');
   if b = 1 then writeln('32 bit objects')
   else writeln('64 bit objects');
   read(exefil, b);
   if (b < 1) or (b > 2) then error(einvffm);
   write('Data encoding:                           ');
   if b = 1 then writeln('2''s complement, little endian')
   else writeln('2''s complement, big endian');
   read(exefil, b);
   if b <> 1 then error(einvffm);
   write('File version:                            ');
   writeln('Current version');
   read(exefil, b);
   write('OS ABI identification:                   ');
   if b = 0 then writeln('UNIX System V ABI')
   else if b = 1 then writeln('HP-UX')
   else if b = 2 then writeln('NetBSD. ')
   else if b = 3 then writeln('Linux. ')
   else if b = 6 then writeln('Sun Solaris. ')
   else if b = 7 then writeln('IBM AIX. ')
   else if b = 8 then writeln('SGI Irix. ')
   else if b = 9 then writeln('FreeBSD. ')
   else if b = 10 then writeln('Compaq TRU64 UNIX. ')
   else if b = 11 then writeln('Novell Modesto. ')
   else if b = 12 then writeln('OpenBSD. ')
   else if b = 97 then writeln('ARM')
   else if b = 255 then writeln('Standalone (embedded) application')
   else error(einvffm);
   read(exefil, b);
   write('ABI version:                             ');
   prthex(2, b);
   writeln;
   { discard the padding bytes }
   for i := 10 to 16 do read(exefil, b);
   readwrd(exefil, w); { get object file type }
   if (w < 0) or (w > 4) then error(einvffm);
   write('File type:                               ');
   case w of { file type }

      0: writeln('None');
      1: writeln('Relocatable file');
      2: writeln('Executable file');
      3: writeln('Shared object file');
      4: writeln('Core file')
   
   end;
   readwrd(exefil, w); { get machine type }
   if (w < 0) or (w > 94) then error(einvffm);
   write('Machine type:                            ');
   case w of { machine type }

      0: writeln('No machine');
      1: writeln('AT&T WE 32100');
      2: writeln('SUN SPARC');
      3: writeln('Intel 80386');
      4: writeln('Motorola m68k family');
      5: writeln('Motorola m88k family');
      7: writeln('Intel 80860');
      8: writeln('MIPS R3000 big-endian');
      9: writeln('IBM System/370');
     10: writeln('MIPS R3000 little-endian');
     11, 12, 13, 14: error(einvffm);
     15: writeln('HPPA');
     17: writeln('Fujitsu VPP500');
     18: writeln('Sun''s "v8plus"');
     19: writeln('Intel 80960');
     20: writeln('PowerPC');
     21: writeln('PowerPC 64-bit');
     22: writeln('IBM S390');
     23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36: error(einvffm);
     36: writeln('NEC V800 series');
     37: writeln('Fujitsu FR20');
     38: writeln('TRW RH-32');
     39: writeln('Motorola RCE');
     40: writeln('ARM');
     41: writeln('Digital Alpha');
     42: writeln('Hitachi SH');
     43: writeln('SPARC v9 64-bit');
     44: writeln('Siemens Tricore');
     45: writeln('Argonaut RISC Core');
     46: writeln('Hitachi H8/300');
     47: writeln('Hitachi H8/300H');
     48: writeln('Hitachi H8S');
     49: writeln('Hitachi H8/500');
     50: writeln('Intel Merced');
     51: writeln('Stanford MIPS-X');
     52: writeln('Motorola Coldfire');
     53: writeln('Motorola M68HC12');
     54: writeln('Fujitsu MMA Multimedia Accelerator');
     55: writeln('Siemens PCP');
     56: writeln('Sony nCPU embeded RISC');
     57: writeln('Denso NDR1 microprocessor');
     58: writeln('Motorola Start*Core processor');
     59: writeln('Toyota ME16 processor');
     60: writeln('STMicroelectronic ST100 processor');
     61: writeln('Advanced Logic Corp. Tinyj emb.fam');
     62: writeln('AMD x86-64 architecture');
     63: writeln('Sony DSP Processor');
     64, 65: error(einvffm);
     66: writeln('Siemens FX66 microcontroller');
     67: writeln('STMicroelectronics ST9+ 8/16 mc');
     68: writeln('STmicroelectronics ST7 8 bit mc');
     69: writeln('Motorola MC68HC16 microcontroller');
     70: writeln('Motorola MC68HC11 microcontroller');
     71: writeln('Motorola MC68HC08 microcontroller');
     72: writeln('Motorola MC68HC05 microcontroller');
     73: writeln('Silicon Graphics SVx');
     74: writeln('STMicroelectronics ST19 8 bit mc');
     75: writeln('Digital VAX');
     76: writeln('Axis Communications 32-bit embedded processor');
     77: writeln('Infineon Technologies 32-bit embedded processor');
     78: writeln('Element 14 64-bit DSP Processor');
     79: writeln('LSI Logic 16-bit DSP Processor');
     80: writeln('Donald Knuth''s educational 64-bit processor');
     81: writeln('Harvard University machine-independent object files');
     82: writeln('SiTera Prism');
     83: writeln('Atmel AVR 8-bit microcontroller');
     84: writeln('Fujitsu FR30');
     85: writeln('Mitsubishi D10V');
     86: writeln('Mitsubishi D30V');
     87: writeln('NEC v850');
     88: writeln('Mitsubishi M32R');
     89: writeln('Matsushita MN10300');
     90: writeln('Matsushita MN10200');
     91: writeln('picoJava');
     92: writeln('OpenRISC 32-bit embedded processor');
     93: writeln('ARC Cores Tangent-A5');
     94: writeln('Tensilica Xtensa Architecture');

   end;
   readdwd(exefil, w); { get object file version }
   if w <> 1 then error(einvffm);
   write('Object file version:                     ');        
   writeln('Current version');
   readdwd(exefil, w);
   write('Entry point virtual address:             ');
   prthex(8, w);
   writeln;
   readdwd(exefil, phdoff);
   write('Program header table file offset:        ');
   prthex(8, phdoff);
   writeln;
   readdwd(exefil, shdoff);
   write('Section header table file offset:        ');
   prthex(8, shdoff);
   writeln;
   readdwd(exefil, w);
   write('Processor specific flags:                ');
   prthex(8, w);
   writeln;
   readwrd(exefil, w);
   write('ELF header size:                         ');
   prthex(8, w);
   writeln;
   readwrd(exefil, w);
   write('Program header table entry size:         ');
   prthex(8, w);
   writeln;
   readwrd(exefil, phdcnt);
   write('Program header table entry count:        ');
   prthex(8, phdcnt);
   writeln;
   readwrd(exefil, shdsiz);
   write('Section header table entry size:         ');
   prthex(8, shdsiz);
   writeln;
   readwrd(exefil, shdcnt);
   write('Section header table entry count:        ');
   prthex(8, shdcnt);
   writeln;
   readwrd(exefil, shsinx);
   write('Section header string table index:       ');
   prthex(8, shsinx);
   writeln;
   { find location of string table }
   position(exefil, shdoff+shdsiz*shsinx+1);
   readdwd(exefil, w); { Section name (string tbl index) }
   readdwd(exefil, w); { Section type }
   readdwd(exefil, w); { Section flags }
   readdwd(exefil, w); { Section virtual addr at execution }
   readdwd(exefil, strtab); { Section file offset }

   writeln;
   writeln('Program headers');
   writeln;
   position(exefil, phdoff+1); { position to program headers }
   for i := 1 to phdcnt do begin { output program headers }

      writeln;
      writeln('Program header:                          ', i:1);
      writeln('==============================================================');
      readdwd(exefil, w); { Segment type }
      write('Segment type:                            ');
      if w = 0 then writeln('Entry unused')
      else if w = 1 then writeln('Loadable program segment')
      else if w = 2 then writeln('Dynamic linking information')
      else if w = 3 then writeln('Program interpreter')
      else if w = 4 then writeln('Auxiliary information')
      else if w = 5 then writeln('Reserved')
      else if w = 6 then writeln('Entry for header table itself')
      else if w = 7 then writeln('Thread-local storage segment')
      else if w = 8 then writeln('Number of defined types')
      else if w = $60000000 then writeln('Start of OS-specific')
      else if w = $6474e550 then writeln('GCC .eh_frame_hdr segment')
      else if w = $6ffffffa then writeln('LOSUNW')
      else if w = $6ffffffa then writeln(' Sun Specific segment')
      else if w = $6ffffffb then writeln('Stack segment')
      else if w = $6fffffff then writeln('HISUNW')
      else if w = $6fffffff then writeln('End of OS-specific')
      else if w = $70000000 then writeln('Start of processor-specific')
      else if w = $7fffffff then writeln('End of processor-specific');
      readdwd(exefil, w); { Segment file offset }
      write('Segment file offset:                     ');
      prthex(8, w);
      writeln;
      readdwd(exefil, w); { Segment virtual address }
      write('Segment virtual address:                 ');
      prthex(8, w);
      writeln;
      readdwd(exefil, w); { Segment physical address }
      write('Segment physical address:                ');
      prthex(8, w);
      writeln;
      readdwd(exefil, w); { Segment size in file }
      write('Segment size in file:                    ');
      prthex(8, w);
      writeln;
      readdwd(exefil, w); { Segment size in memory }
      write('Segment size in memory:                  ');
      prthex(8, w);
      writeln;
      readdwd(exefil, w); { Segment flags }
      write('Segment flags:                           ');
      if w and 4 <> 0 then write('r') else write('-');
      if w and 2 <> 0 then write('w') else write('-');
      if w and 1 <> 0 then write('x') else write('-');
      writeln;
      readdwd(exefil, w); { Segment alignment }
      write('Segment alignment:                       ');
      prthex(8, w);
      writeln;

   end;

   writeln;
   writeln('Section headers');
   writeln;
   position(exefil, shdoff+1); { position to program headers }
   for i := 1 to shdcnt do begin { output program headers }

      writeln;
      writeln('Section header:                          ', i:1);
      writeln('==============================================================');
      readdwd(exefil, w); { Section name (string tbl index) }
      write('Section name:                            ');
      prtstr(w);
      writeln;
      readdwd(exefil, w); { Section type }
      write('Section type:                            ');
      if w = 0 then writeln('Section header table entry unused')
      else if w = 1 then writeln('Program data')
      else if w = 2 then writeln('Symbol table')
      else if w = 3 then writeln('String table')
      else if w = 4 then writeln('Relocation entries with addends')
      else if w = 5 then writeln('Symbol hash table')
      else if w = 6 then writeln('Dynamic linking information')
      else if w = 7 then writeln('Notes')
      else if w = 8 then writeln('Program space with no data (bss)')
      else if w = 9 then writeln('Relocation entries, no addends')
      else if w = 10 then writeln('Reserved')
      else if w = 11 then writeln('Dynamic linker symbol table')
      else if w = 14 then writeln('Array of constructors')
      else if w = 15 then writeln('Array of destructors')
      else if w = 16 then writeln('Array of pre-constructors')
      else if w = 17 then writeln('Section group')
      else if w = 18 then writeln('Extended section indeces')
      else if w = 19 then writeln('Number of defined types. ')
      else if w = $6ffffff7 then writeln('Prelink library list')
      else if w = $6ffffff8 then writeln('Checksum for DSO content. ')
      else if w = $6ffffffa then writeln('Sun-specific low bound. ')
      else if w = $6ffffffa then writeln('SUNW_move')
      else if w = $6ffffffb then writeln('SUNW_COMDAT')
      else if w = $6ffffffc then writeln('SUNW_syminfo')
      else if w = $6ffffffd then writeln('Version definition section. ')
      else if w = $6ffffffe then writeln('Version needs section. ')
      else if w = $6fffffff then writeln('Version symbol table. ')
      else if w = $6fffffff then writeln('Sun-specific high bound. ');
      readdwd(exefil, w); { Section flags }
      write('Section flags:                           ');
      if w and %1 <> 0 then write('Write ');
      if w and %10 <> 0 then write('Alloc ');
      if w and %100 <> 0 then write('Exec ');
      if w and %1000 <> 0 then write('Merge ');
      if w and %10000 <> 0 then write('Strings ');
      if w and %100000 <> 0 then write('Info ');
      if w and %1000000 <> 0 then write('Order ');
      if w and %10000000 <> 0 then write('Nonstd ');
      if w and %100000000 <> 0 then write('Group ');
      if w and %1000000000 <> 0 then write('TLS ');
      writeln;
      readdwd(exefil, w); { Section virtual addr at execution }
      write('Section virtual addr at execution:       ');
      prthex(8, w);
      writeln;
      readdwd(exefil, w); { Section file offset }
      write('Section file offset:                     ');
      prthex(8, w);
      writeln;
      readdwd(exefil, w); { Section size in bytes }
      write('Section size in bytes:                   ');
      prthex(8, w);
      writeln;
      readdwd(exefil, w); { Link to another section }
      write('Link to another section:                 ');
      prthex(8, w);
      writeln;
      readdwd(exefil, w); { Additional section information }
      write('Additional section information:          ');
      prthex(8, w);
      writeln;
      readdwd(exefil, w); { Section alignment }
      write('Section alignment:                       ');
      prthex(8, w);
      writeln;
      readdwd(exefil, w); { Entry size if section holds table }
      write('Entry size if section holds table:       ');
      prthex(8, w);
      writeln;

   end;

   99:; { abort program }

   if fopnsrc then close(exefil) { close input file }

end.

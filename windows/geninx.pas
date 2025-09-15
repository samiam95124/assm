{******************************************************************************
*                                                                             *
*                             CATALOG PREINDEXER                              *
*                                                                             *
*                              95/04 S. A. Moore                              *
*                                                                             *
* Generates preindex files for genpe. A preindex file is a catalog file that  *
* is already in hash table format, and is binary, not ascii. This speeds the  *
* reading of the catalog file for genpe.                                      *
*                                                                             *
* The index is output into a file called "catalog.inx". The file has the      *
* following format:                                                           *
*                                                                             *
* 1. A signature, '.inx'.                                                     *
* 2. The number of module files.                                              *
* 3. The number of symbol names.                                              *
* 4. The hash length (size of hash table used).                               *
* 5. Module filename dictionary.                                              *
* 6. Symbol name dictionary.                                                  *
*                                                                             *
* Symbols are carried in strings, and each string consists of:                *
*                                                                             *
* 1. Len (8 bits).                                                            *
* 2. Data (N*bytes).                                                          *
*                                                                             *
* The length gives the number of characters in the string that follows.       *
*                                                                             *
* The module filename directory is simply a series of strings containing      *
* filenames of .dll modules with their complete paths. Each module filename   *
* in the file is numbered from 1 to N, and these numbers are referenced by    *
* the symbols that follow.                                                    *
*                                                                             *
* The hash length is a 32 bit big endian word that contains the hash table    *
* length.                                                                     *
*                                                                             *
* Each symbol in the symbol name dictionary has the following format:         *
*                                                                             *
* 1. Symbol string.                                                           *
* 2. Number of containing module (32 bits big endian).                        *
* 3. Duplicate flag (8 bits, 0 or 1).                                         *
*                                                                             *
* Symbols are arranged in "lists" according to the hash table entry they      *
* belong under. Each list is terminated by a null string. Each entry of the   *
* table, 1 to the table size, has its list appearing in order. A list can be  *
* null.                                                                       *
*                                                                             *
* The number of lists should match the number of table entries, or it is an   *
* error.                                                                      *
*                                                                             *
******************************************************************************}

program geninx(output);

uses strlib, { strings }
     extlib, { extentions }
     parlib; { parsing }

const

   maxlab  = 1000; { number of characters in label }
   maxfil  = 100;  { number of characters in a file name }
   cmdmax  = 2000; { maximum length of command string for parser }
   modmax  = 1000; { size of module hash table }

type 

   labinx  = 1..maxlab;    { index for standard label }
   labl    = packed array [labinx] of char; { a standard label }
   filinx  = 1..maxfil; { index for file names }
   filnam  = packed array [filinx] of char; { a file name }
   { module name hash structure }
   modinx = 1..modmax; { index for name head table }
   modptr = ^modnam; { pointer to entry }
   modnam = record { module name entry }
   
      name:   pstring; { module export name }
      mname:  pstring; { filename name of containing module }
      dup:    boolean; { is a duplicate }
      ref:    boolean; { was referenced by a symbol }
      modnum: integer; { logical module number }
      next:   modptr   { next entry in chain }
   
   end;
   mdrptr = ^modref; { module reference pointer }
   modref = record { module reference }

      mname: pstring; { module filename }
      next:  mdrptr { next entry }

   end;

var

    trnchr:  array [char] of byte; { character to ascii translation array }
    modtbl:  array [modinx] of modptr; { module chain table }
    modlst:  mdrptr; { list of modules }
    modcnt:  integer; { number of different modules }
    mi:      modinx; { module name table index }
    catfil:  bytfil; { catalog output file }
    mp:      mdrptr; { module reference pointer }
    symcnt:  integer; { total symbols counter }
    sp:      modptr; { symbols pointer }
    i:       integer;

{ ASCII value to internal character set convertion array }

fixed chrtrn: array [0..127] of char = array

   '\nul',  { 0   } '\soh',  { 1   } '\stx',  { 2   } '\etx',  { 3   }
   '\eot',  { 4   } '\enq',  { 5   } '\ack',  { 6   } '\bel',  { 7   }
   '\bs',   { 8   } '\ht',   { 9   } '\lf',   { 10  } '\vt',   { 11  }
   '\ff',   { 12  } '\cr',   { 13  } '\so',   { 14  } '\si',   { 15  }
   '\dle',  { 16  } '\dc1',  { 17  } '\dc2',  { 18  } '\dc3',  { 19  }
   '\dc4',  { 20  } '\nak',  { 21  } '\syn',  { 22  } '\etb',  { 23  }
   '\can',  { 24  } '\em',   { 25  } '\sub',  { 26  } '\esc',  { 27  }
   '\fs',   { 28  } '\gs',   { 29  } '\rs',   { 30  } '\us',   { 31  }
   ' ',     { 32  } '!',     { 33  } '"',     { 34  } '#',     { 35  }
   '$',     { 36  } '%',     { 37  } '&',     { 38  } '''',    { 39  }
   '(',     { 40  } ')',     { 41  } '*',     { 42  } '+',     { 43  }
   ',',     { 44  } '-',     { 45  } '.',     { 46  } '/',     { 47  }
   '0',     { 48  } '1',     { 49  } '2',     { 50  } '3',     { 51  }
   '4',     { 52  } '5',     { 53  } '6',     { 54  } '7',     { 55  }
   '8',     { 56  } '9',     { 57  } ':',     { 58  } ';',     { 59  }
   '<',     { 60  } '=',     { 61  } '>',     { 62  } '?',     { 63  }
   '@',     { 64  } 'A',     { 65  } 'B',     { 66  } 'C',     { 67  }
   'D',     { 68  } 'E',     { 69  } 'F',     { 70  } 'G',     { 71  }
   'H',     { 72  } 'I',     { 73  } 'J',     { 74  } 'K',     { 75  }
   'L',     { 76  } 'M',     { 77  } 'N',     { 78  } 'O',     { 79  }
   'P',     { 80  } 'Q',     { 81  } 'R',     { 82  } 'S',     { 83  }
   'T',     { 84  } 'U',     { 85  } 'V',     { 86  } 'W',     { 87  }
   'X',     { 88  } 'Y',     { 89  } 'Z',     { 90  } '[',     { 91  }
   '\\',    { 92  } ']',     { 93  } '^',     { 94  } '_',     { 95  }
   '`',     { 96  } 'a',     { 97  } 'b',     { 98  } 'c',     { 99  }
   'd',     { 100 } 'e',     { 101 } 'f',     { 102 } 'g',     { 103 }
   'h',     { 104 } 'i',     { 105 } 'j',     { 106 } 'k',     { 107 }
   'l',     { 108 } 'm',     { 109 } 'n',     { 110 } 'o',     { 111 }
   'p',     { 112 } 'q',     { 113 } 'r',     { 114 } 's',     { 115 }
   't',     { 116 } 'u',     { 117 } 'v',     { 118 } 'w',     { 119 }
   'x',     { 120 } 'y',     { 121 } 'z',     { 122 } '{',     { 123 }
   '|',     { 124 } '}',     { 125 } '~',     { 126 } '\del'   { 127 }

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

Find label hash function without case

Finds a hash function for the given label. The maximum specifies the maximum
value desired from the hash generator. The return value will be between
1 and the max. The "add" parameter is a "stirring" parameter that just changes
the hash value to a different set of values. This is used to optimize fixed
tables, done using an external generator program. See the program for details,
but the basic idea is that we will find an add that gives the optimum set of
hash values for a fixed set of labels.
Note that for dynamic tables, the add parameter can be left to 0.

******************************************************************************}

function hash(view s:    string;  { label to find hash for }
                   add:  integer; { stirring parameter }
                   maxv: integer) { maximum value returned }
              : integer;          { return hash }

var i, r : integer;

begin

   r := 0;
   for i := 1 to max(s) do if lcase(s[i]) <> ' ' then
      r := r + chr2ascii(lcase(s[i])) + add;

   hash := r mod maxv + 1

end;

{******************************************************************************

Write 32 bit big endian number

Writes a 32 bit unsigned big endian number to a byte file.

******************************************************************************}

procedure writebe32(var f: bytfil; { file to write to }
                        i: integer); { integer to write }

begin

   write(f, i div 16777216); { write high }
   write(f, i div 65536 mod 256); { write high middle }
   write(f, i div 256 mod 256); { write low middle }
   write(f, i mod 256) { write low }

end;

{******************************************************************************

Write length preceeding string

Writes a length preceeded string to a bytefile.

******************************************************************************}

procedure wrtlstr(var f: bytfil; { file to write to }
                  view s: string); { string to write }

var i, l: integer;

begin

   l := len(s); { find length of string }
   if l > 255 then begin { too long }

      writeln('*** String too long for format');
      halt

   end;
   write(f, l); { write length }
   for i := 1 to l do write(f, ord(s[i])) { write string }

end;

{******************************************************************************

Get new module name entry

Gets a new module name entry.

******************************************************************************}

procedure getmod(var mp: modptr);

begin

   new(mp); { get a new entry }
   mp^.next := nil; { terminate entry }
   mp^.name := nil; { set no name }
   mp^.mname := nil; { set module name empty }
   mp^.dup := false; { set not duplicated }
   mp^.ref := false { set not referenced }

end;

{******************************************************************************

Find module name entry

Finds a module name entry matching the given module name.

******************************************************************************}

function fndmod(view s: string): modptr;

var p, r: modptr; { pointers for symbol table }

begin
   
   r := nil; { clear result pointer }
   p := modtbl[hash(s, 0, modmax)]; { index the top entry }
   while p <> nil do begin { traverse chain }

      if compp(s, p^.name^) then begin { entry found }

         r := p; { place result pointer }
         p := nil { nix search pointer }

      end else p := p^.next { index next entry }

   end;

   fndmod := r { return result pointer }

end;

{******************************************************************************

Place module symbol

Places a new module name entry. Any previous module name is found, and if there
is one, it is flagged as a duplicate and no further action takes place. If not,
then the name is placed in a new module name entry.

In our model, names are coined by their module, so c:\windows\mymod.dll
containing export findit becomes "mymod_findit". This is entered into the
module symbol dictionary.

Also counts module duplicates found.

******************************************************************************}

procedure newmod(view n:    string;   { exported symbol }
                      mn:   pstring;  { module filename }
                 view nm:   string;   { module name alone }
                      modn: integer;  { logical module number }
                 var  cnt:  integer); { duplicates count }

var p: modptr; { module name pointer }
    i: modinx; { index for module name table }
    b: labl;   { buffer for coining }

{ "transmute" a name, or change all characters outside of the range _a-z0-9
  to a "_" character }

procedure trans(var s: string);

var i: integer;

begin

   { change first character if not in _a-z }
   if not (s[1] in ['a'..'z', 'A'..'Z', '_', ' ']) then s[1] := '_';
   { change all next characters if not in _a-z0-9 }
   for i := 2 to max(s) do
      if not (s[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_', ' ']) then s[i] := '_'

end;

begin

   { produce coined module symbol of the form "module_symbol" }
   copy(b, nm); { place module }
   trans(b); { transmute }
   cat(b, '_'); { separate }
   cat(b, n); { add symbol }
   p := fndmod(b); { find any previous module export by that name }
   if p <> nil then begin

      p^.dup := true; { flag as duplicate }
      cnt := cnt+1 { count duplicates }

   end else begin { enter new module name }

      getmod(p); { get a new entry }
      p^.name := copy(b); { place name }
      p^.mname := mn; { place module filename }
      p^.modnum := modn; { place module logical number }
      i := hash(b, 0, modmax); { find the top entry }
      p^.next := modtbl[i]; { place the next entry link }
      modtbl[i] := p { plant our symbol }

   end

end;   

{*******************************************************************************

Parse and load catalog file

Parses and loads the catalog file. The catalog file is allways "catalog", and
has the format:

WBTRV32 WBTRVINIT

The first line item is the module name, as in WBTRV32.dll. The second is the 
export name. The module gives the dll file that contains the export, and will
be used to coin the reference so that it can be attached to the correct
module.

Note: we pretty much expect the module names to appear in sequence. We keep
a module name in storage that is used to label all entries, and that is only
changed when the module name changes in the file. If the module names were
all mixed up, many duplicate module names would be created.

*******************************************************************************}

procedure parcat;

label nextline, { go to next line }
      exit;     { exit routine }

var cathan:    parhan;  { handle for instruction parsing }
    modl, exp: labl;    { labels to parse }
    err:       boolean; { parsing error }
    mn:        pstring; { running module name }
    dupcnt:    integer; { duplicate counter }
    cf:        boolean; { catalog file found }
    cfn:       filnam;  { catalog file name }
    pth:       filnam;  { search path }
    pm, nm, em: labl;   { module filename parts }
    mp:        mdrptr;  { module reference pointer }
    lp:        mdrptr;  { module reference last entry }

procedure caterr(view es: string);

begin

   prterr(cathan, output, es, true); { print error }
   getlin(cathan); { skip to new line }
   goto nextline

end;
   
begin

   cf := false; { set no catalog file found }
   copy(cfn, 'catalog.cat'); { set catalog name }
   if exists(cfn) then cf := true
   else begin { search paths for it }
   
      getusr(pth); { get the user path }
      maknam(cfn, pth, 'catalog', 'cat'); { create name }
      if exists(cfn) then cf := true
      else begin { search the program path }

         getpgm(pth); { get the program path }
         maknam(cfn, pth, 'catalog', 'cat'); { create name }
         cf := exists(cfn)

      end

   end;
   if not cf then goto exit; { no catalog file found, exit }
   mn := nil; { clear module name }
   lp := nil; { set no module last pointer }
   dupcnt := 0; { clear duplicate count }
   writeln('Reading catalog file ', cfn:0);
   openpar(cathan); { open parser }
   openfil(cathan, cfn, cmdmax); { open file to parse }

   nextline: { start new line }

   while not endfil(cathan) do begin { process instructions }
   
      skpspc(cathan); { skip leading spaces }
      if chkchr(cathan) = '!' then { skip comment line }
         while not endlin(cathan) do getchr(cathan)
      else if not endlin(cathan) then begin { command line }

         parfil(cathan, modl, true, err); { get module name }
         if err then caterr('Invalid catalog syntax');
         parlab(cathan, exp, err); { get export name }
         if err then caterr('Invalid catalog syntax');
         if mn = nil then begin

            mn := copy(modl); { create a new module filename }
            brknam(mn^, pm, nm, em); { extract the name }
            new(mp); { get a new module refernece }
            mp^.mname := mn; { place module filename }
            mp^.next := nil; { clear next entry }
            if lp = nil then modlst := mp { place as root }
            else lp^.next := mp; { place as last }
            lp := mp; { set new last }
            modcnt := modcnt+1 { count total modules }

         end else
            { also create one if the names don't match }
            if not compp(mn^, modl) then begin

            mn := copy(modl); { create new module name }
            brknam(mn^, pm, nm, em); { extract the name }
            new(mp); { get a new module refernece }
            mp^.mname := mn; { place module filename }
            mp^.next := nil; { clear next entry }
            if lp = nil then modlst := mp { place as root }
            else lp^.next := mp; { place as last }
            lp := mp; { set new last }
            modcnt := modcnt+1 { count total modules }

         end;
         newmod(exp, mn, nm, modcnt, dupcnt); { create new module name }
         symcnt := symcnt+1; { count total symbols }
         skpspc(cathan); { skip trailing spaces }
         if chkchr(cathan) = '!' then { skip comment line }
            while not endlin(cathan) do getchr(cathan);
         if not endlin(cathan) then caterr('Invalid catalog symtax')
         
      end;
      getlin(cathan) { skip to new line }

   end;
   if dupcnt > 0 then
      writeln('Catalog file contains ', dupcnt:1, ' duplicate exports');

   exit: { exit routine }

end;

begin { main }

   write('Catalog index generator vs. 1.13.01 copyright (C) 2007 ');
   writeln('S. A. Moore');

   for mi := 1 to modmax do modtbl[mi] := nil; { clear module name table }
   modlst := nil; { clear module name list }
   modcnt := 0; { clear module name count }
   symcnt := 0; { clear total symbols count }

   { Form character to ASCII value translation array from ASCII value to 
     character translation array. }
   for i := 1 to 255 do trnchr[chr(i)] := 0; { null out array }
   for i := 1 to 127 do trnchr[chrtrn[i]] := i; { form translation }

   parcat; { read the catalog file, if it exists }

   { create the output file }
   assign(catfil, 'catalog.inx');
   rewrite(catfil);
   
   { write the signature '.inx' }
   write(catfil, ord('.'));
   write(catfil, ord('i'));
   write(catfil, ord('n'));
   write(catfil, ord('x'));

   { output some statistics }
   writeln;
   writeln('Total modules: ', modcnt);
   writeln('Total symbols: ', symcnt);
   
   { write the parameter block }
   writebe32(catfil, modcnt); { number of modules }
   writebe32(catfil, symcnt); { number of symbols }
   writebe32(catfil, modmax); { number of hash table entries }

   { write the module directory }
   mp := modlst; { index top of list }
   while mp <> nil do begin

      wrtlstr(catfil, mp^.mname^); { write module name string }
      mp := mp^.next { next in list }

   end;

   { write the symbol directory }
   for mi := 1 to modmax do begin { traverse the hash table }

      sp := modtbl[mi]; { index export symbols list }
      while sp <> nil do begin { traverse symbols list }

         wrtlstr(catfil, sp^.name^); { output symbol string }
         writebe32(catfil, sp^.modnum); { output logical module number }
         write(catfil, ord(sp^.dup)); { output duplicate flag }
         sp := sp^.next { next symbol in list }

      end;
      write(catfil, 0) { terminate export symbol list }

   end;

   { close catalog and exit }
   close(catfil);

   writeln;
   writeln('Catalog created in catalog.inx');
   writeln;
   writeln('Function complete')

end.

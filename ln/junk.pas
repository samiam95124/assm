module common;

uses lndef; { global definitions file }

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

var bits:   integer; { number of bits in integer }
    bytes:  integer; { number of bytes in integer }
    digits: integer; { number of digits in integer }
    toppow: integer; { top byte power, precalculated for speed }
    cmdlin: linbuf; { command line buffer }
    cmdptr: lininx; { command line index }
    cmdlen: lininx; { command line length }
    srclst: filept; { source files list }
    srclas: filept; { last source file entry }
    cursrc: filept; { current source file under process }
    errnam: filnam; { error file name }
    errval: integer; { error value }
    errbits: integer; { error number of bits }
    pgmloc: integer; { starting location of program overlay }
    varloc: integer; { starting location variables overlay }
    errfil: filnam; { error file print buffer }
    errlab: labl;   { error label print buffer }
    symfil: bytfil; { open symbols file }
    objinp: bytfil; { object input file }
    objout: bytfil; { object output file }
    fresym: symptr; { free symbol entries list }
    frerld: rldptr; { free rld entries list }
    symtab: symptr; { current symbols table }
    symsav: symptr; { saved symbols table }
    rldtab: rldptr; { current rlds table }
    rldsav: rldptr; { saved rlds table }
    blktab: blkptr; { block list }
    blkstk: blkptr; { block stack }
    blksav: blkptr; { saved block list }
    blkfre: blkptr; { free block list }
    bicfre: bicptr; { free block inclusion list }
    linlst: trkptr; { line tracking list }
    linlas: trkptr; { line tracking list last entry }
    linfre: trkptr; { free line tracking list }
    linsav: trkptr; { saved line tracking list }
    poff:   integer; { current program frame offset }
    voff:   integer; { current variable frame offset }
    pstrf:  boolean; { program start parameter found in file }
    pstrnw: integer; { program start value, new }
    pstrod: integer; { program start value, old }
    pstr:   symptr;  { program start symbol entry }
    pendf:  boolean; { program end parameter found in file }
    pendnw: integer; { program end value, new }
    pendod: integer; { program end value, old }
    pend:   symptr;  { program end symbol entry }
    vstrf:  boolean; { variable start parameter found in file }
    vstrnw: integer; { variable start value, new }
    vstrod: integer; { variable start value, old }
    vstr:   symptr;  { variable start symbol entry }
    vendf:  boolean; { variable end parameter found in file }
    vendnw: integer; { variable end value, new }
    vendod: integer; { variable end value, old }
    vend:   symptr;  { variable end symbol entry }
    prgmc:  integer; { program counter for final output }
    rldinx: rldptr;  { index for rlds on output }
    fsupp:  boolean; { suppress output flag }
    fverb:  boolean; { verbose mode flag }
    ferrf:  boolean; { error file requested flag }
    ftrim:  boolean; { trim symbols flag. this flag causes all non-global
                       and non-external symbols to be suppressed on output.
                       This was done for space reasons back in the days
                       when we were 64k limited, and is pretty much a
                       specialty flag now. It's use eliminates information
                       needed for listing products and external symbols
                       users (like DB) }
    fsecr:  boolean; { secure references flag }
    fundf:  boolean; { produce undefined symbols listing flag }
    fsdmp:  boolean; { diagnostic: produce symbols dump }
    fvset:  boolean; { variable start has been set }
    fllab:  boolean; { perform label ordered symbol listing }
    flval:  boolean; { perform value ordered symbol listing }
    flmod:  boolean; { perform module parameter listing }
    flxrf:  boolean; { perform cross reference listing }
    fldub:  boolean; { delete unused blocks }
    lstlen: integer; { length of output line for listings }
    lstpag: integer; { length of output page for listings }
    symlen: 0..maxlab; { maximum length of symbols }
    nambuf: filnam;  { filename buffer }
    srclen: 0..maxfil; { maximum length of filenames }
    symbuf: savbuf;  { cache for symbols file }
    syminx: savinx;  { data read index for same }
    symtop: savinx;  { data top index for same }
    obibuf: savbuf;  { cache for object input file }
    obiinx: savinx;  { data read index for same }
    obitop: savinx;  { data top index for same }
    obobuf: savbuf;  { cache for object output file }
    oboinx: savinx;  { data read index for same }
    { I suppose the symbols and object output buffers could be combined }
    syobuf: savbuf;  { cache for symbol output file }
    syoinx: savinx;  { data read index for same }
    valfch: chrset;  { valid file characters }
    cmdovf: boolean; { command line overflow }
    symrecycle: boolean; { recycle symbols }
    trnchr: array [char] of byte; { character to ascii translation array }

    { block containing the symbols object read in by rdnxt }
    nxtobj: objtyp; { next sym file object type }
    nxtsym: symptr; { next symbol from sym file }
    nxtrld: rldptr; { next rld from rld file }
    nxtblk: blkptr; { next block entry }
    nxtbic: bicptr; { next block inclusion entry }
    nxtlin: trkptr; { next line tracking entry }

    { block containing running line, program and variable counters for
      line tracking entries }
    linnxt: integer; { line tracking }
    pgmnxt: integer; { program counter }
    varnxt: integer; { variable counter }
    srcnxt: pstring; { source file }

begin                                      
end. { unit }       


/*
 * Milan modeling program
 */

#include <stdio.h>
#include <ctype.h>
#include <setjmp.h>

/*
 * Global definitions
 */

#define TRUE 1
#define FALSE 0
#define FOREVER 1

#define MAXCMD 200   /* maximum length of command line */
#define MAXCOD 200   /* maximum length of encode buffer */
#define MAXMEM 65536 /* maximum size of CPU memory in 32 bit words */
#define MAXSTK 1000  /* maximum size of execution stack */

/*
 * User executive codes
 */

#define UETERM  0  /* terminate program */
#define UELINE  1  /* terminate line */
#define UELOAD  2  /* load inline to stack */
#define UEDISP  3  /* display registers */
#define UEDISPC 4  /* display control registers */
#define UEDISPF 5  /* display float registers */
#define UELIST  6  /* list instructions */
#define UELCST  7  /* load inline constant (64 bit BE) */ 
#define UEPRT   8  /* print value */
#define UEEQU   9  /* test equal */
#define UENEQU  10 /* test not equal */
#define UELTN   11 /* test less than */
#define UEGTN   12 /* test greater than */
#define UELEQU  13 /* test less than or equal */
#define UEGEQU  14 /* test greater than or equal */
#define UEADD   15 /* add */
#define UESUB   16 /* subtract */
#define UEMULT  17 /* multiply */
#define UEDIV   18 /* divide */
#define UEMOD   19 /* modulo */
#define UENEG   20 /* negate */
#define UEDUMP  21 /* dump memory */
#define UESTEP  22 /* step processor */
#define UEDSTEP 23 /* display step processor */
#define UESPEC  24 /* display special variables */
#define UERSET  25 /* set register value */
#define UEUDLRD 26 /* UDLink read command */
#define UEUDLWR 27 /* UDLink write command */
#define UEEXIT  28 /* exit simulator */
#define UELDSR  29 /* load s-record format */
#define UESHL   30 /* shift left */
#define UEOR    31 /* 'or' bits */
#define UEAND   32 /* 'and' bits */
#define UESHR   33 /* shift right */
#define UEINST  34 /* insert data to memory */
#define UEASSML 35 /* assemble lines */
#define UEDUP   36 /* duplicate stack top */
#define UECRSET 37 /* set control register value */
#define UESETPC 38 /* set pc register */
#define UEHELP  39 /* help */

/*
 * Events
 */

#define EVREAD  1  /* CPU read event */
#define EVWRITE 2  /* CPU write event */

/*
 * CP0 registers
 */

#define CP0IBASE    0  /* instruction address space base */
#define CP0IBOUND   1  /* instruction address space bound */
#define CP0DBASE    2  /* data address space base */
#define CP0DBOUND   3  /* data address space bound */
#define CP0BADVADDR 8  /* bad virtual address */
#define CP0COUNT    9  /* timer count */
#define CP0COMPARE  11 /* timer compare */
#define CP0SR       12 /* status register */
#define CP0CAUSE    13 /* cause of last exception */
#define CP0EPC      14 /* exception program counter */
#define CP0PRID     15 /* processor revision register */
#define CP0CONFIG   16 /* configuration register */
#define CP0CALG     17 /* cache attributes control */
#define CP0IWATCH   18 /* instruction watch address */
#define CP0DWATCH   19 /* data watch address */
#define CP0ECC      26 /* cache ECC */
#define CP0CACHEERR 27 /* cache error and status */
#define CP0TAGLO    28 /* cache tag register */
#define CP0ERROREPC 30 /* error exception register */

/*
 * Exception codes
 */

#define ECINT    0  /* interrupt */
#define ECIBOUND 2  /* instruction bound */
#define ECDBOUND 3  /* data bound */
#define ECADEL   4  /* address error exception (load) */
#define ECADES   5  /* address error exception (store) */
#define ECIBE    6  /* bus error exception (fetch) */
#define ECDBE    7  /* bus error exception (data ref) */
#define ECSYS    8  /* syscall exception */
#define ECBP     9  /* breakpoint exception */
#define ECRI     10 /* reserved instruction */
#define ECCPU    11 /* coprocessor unuseable */
#define ECOV     12 /* arithmetic overflow */
#define ECTR     13 /* trap */
#define ECFPE    15 /* floating point */
#define ECWATCH  16 /* watch exception */

/*
 * UDlink parameters
 */

#define UDLBAS 0x10000 /* base address of chip */
#define UDLLEN 190     /* length of register map */

/*
 * Errors
 */

#define ENONE    0  /* no error */
#define ECMDNF   1  /* command not found */
#define ELABEXP  2  /* label expected */
#define EIIECP   3  /* illegal instruction exception */
#define EOECP    4  /* overflow exception processed */
#define ECMAEXP  5  /* ',' expected */
#define EREGEXP  6  /* register expected */
#define ETRAP    7  /* conditional trap taken */
#define EBREAK   8  /* break exception */
#define EDFOPN   9  /* error opening file */
#define EDFHAN   10 /* invalid handle */
#define EDFNOPN  11 /* file not open */
#define EDFREAD  12 /* error reading file */
#define EDFWRITE 13 /* error writing file */
#define EDITINS  14 /* invalid tap instruction */
#define ESYSCAL  15 /* system call taken */
#define EUNSCOP  16 /* unsable coprocessor */
#define EOPNFIL  17 /* cannot open file */
#define EMEMRNG  18 /* memory range error */
#define ENUMEXP  19 /* number expected */
#define ELPEXP   20 /* '(' expected */
#define ERPEXP   21 /* ')' expected */
#define EOPCNF   22 /* opcode not found */
#define EINVREG  23 /* invalid register number */
#define EBADEXC  24 /* bad executive code */

/*
 * Types
 */

typedef long long int ll;           /* 64 bit signed */
typedef unsigned long long int ull; /* 64 bit unsigned */
typedef unsigned char uc;           /* 8 bit unsigned */
typedef unsigned long ul;           /* 32 bit unsigned */
typedef struct event_ *evtptr; /* pointer to event */
typedef struct event_ { /* event record */

   evtptr next;         /* next entry */
   void   (*func)(int); /* event handler to execute */
   int    reason;       /* code for event type */

} event;

/*
 * functions that need predeclaration
 */

int pdisplay();
int pdispcon();
int pdispflt();
int plist();
int pprint();
int pdump();
int pstep();
int pdstep();
int pspec();
int psetreg();
int pudlrd();
int pudlwr();
int pexit();
int pldsr();
int passm();
int passml();
int pcmmt();
int psetpc();
int phelp();
void udlink();
void expr();
int exclin(char line[]);
 
/*
 * Command verb table
 */

struct cmdety {

   char *name;    /* name of command */
   int (*func)(); /* function to execute */

} cmdtbl[] = {

  { "reg",     pdisplay },
  { "r",       pdisplay },
  { "dispcon", pdispcon },
  { "dc",      pdispcon },
  { "dispflt", pdispflt },
  { "df",      pdispflt },
  { "list",    plist  },
  { "l",       plist },
  { "print",   pprint },
  { "p",       pprint },
  { "d",       pdump },
  { "dump",    pdump },
  { "step",    pstep },
  { "s",       pstep },
  { "dstep",   pdstep },
  { "ds",      pdstep },
  { "spec",    pspec },
  { "setr",    psetreg },
  { "sr",      psetreg },
  { "udlr",    pudlrd },
  { "udlw",    pudlwr },
  { "exit",    pexit },
  { "loadsr",  pldsr },
  { "ldsr",    pldsr },
  { "assm",    passm },
  { "as",      passm },
  { "assml",   passml },
  { "al",      passml },
  { "comment", pcmmt },
  { "c",       pcmmt },
  { "setpc",   psetpc },
  { "spc",     psetpc },
  { "help",    phelp },
  { "h",       phelp },
  { "",        0 }

};

/*
 * Register name equivalence table
 */

struct regety {

   char *name; /* name of register */
   int  reg;   /* number of register (address) */
   int  loc;   /* location (what normal/coprocessor register exists in) */

} regtbl[] = {

  { "r0",       0,  0 },
  { "r1",       1,  0 },
  { "r2",       2,  0 },
  { "r3",       3,  0 },
  { "r4",       4,  0 },
  { "r5",       5,  0 },
  { "r6",       6,  0 },
  { "r7",       7,  0 },
  { "r8",       8,  0 },
  { "r9",       9,  0 },
  { "r10",      10, 0 },
  { "r11",      11, 0 },
  { "r12",      12, 0 },
  { "r13",      13, 0 },
  { "r14",      14, 0 },
  { "r15",      15, 0 },
  { "r16",      16, 0 },
  { "r17",      17, 0 },
  { "r18",      18, 0 },
  { "r19",      19, 0 },
  { "r20",      20, 0 },
  { "r21",      21, 0 },
  { "r22",      22, 0 },
  { "r23",      23, 0 },
  { "r24",      24, 0 },
  { "r25",      25, 0 },
  { "r26",      26, 0 },
  { "r27",      27, 0 },
  { "r28",      28, 0 },
  { "r29",      29, 0 },
  { "r30",      30, 0 },
  { "r31",      31, 0 },
  { "ibase",    0,  1 },
  { "ibound",   1,  1 },
  { "dbase ",   2,  1 },
  { "dbound",   3,  1 },
  { "badvaddr", 8,  1 },
  { "count",    9,  1 },
  { "compare",  11, 1 },
  { "sr",       12, 1 },
  { "cause",    13, 1 },
  { "prld",     15, 1 },
  { "config",   16, 1 },
  { "calg",     17, 1 },
  { "iwatch",   18, 1 },
  { "dwatch",   19, 1 },
  { "ecc",      26, 1 },
  { "cacheerr", 27, 1 },
  { "taglo",    28, 1 },
  { "epc",      14, 1 },
  { "errorepc", 30, 1 },
  { "",         0,  0 }

};  

/*
 * Instruction disassembly table
 *
 * Contains the instruction set in a table form for disassembly.
 * each entry is a 12 character string containing an 8 character
 * memonic followed by four "descriptors". The descriptors
 * stand for each of the 4 register fields in the middle of
 * the instruction, and appear as:
 *
 * a-d: Register. The a..d ordering gives the appearance order of
 * the register in the parameter line.
 *
 * i:   Immediate, used in bits 15-0, a signed 16 bit number.
 *      This will be in slot 11.
 * o:   Offset. The lower 15 bits of the instruction are added
 *      to the next instruction's pc, then the resulting address
 *      is printed. This will be in the register bit 11 slot.
 * z:   Field of zeros. The register slot is verified to contain
 *      zeros, and an error output if not.
 * s:   Segment address. The lower 26 bits  are shifted left 2
 *      bits, then all bits above the 28th are masked in from
 *      the address of the next instruction. The resulting value
 *      is printed. This appears in register bit slot 21.
 * f:   Function. The lower 26 bits are printed in hex. Occupys
 *      slot 21.
 * q:   Base/offset. The register indicated combines with a
 *      16 bit immediate to form an "o(r)" format.
 * y:   System call code. Bits 6-25 form a 20 bit parameter code.
 *      Slot 21.
 *
 * Each of the tables appear here in the same array, opcodes (64),
 * specials (64) and register immediates (32). The format is
 * like the MIPS instruction encoding table for MIPS III.
 *
 */

char opctbl[64+64+32+8][11] = {

   /* opcodes */ 
   "*          ", "&          ", "j      s   ", "jal    s   ",
   "beq    abo ", "bne    abo ", "blez   azo ", "bgtz   azo ",
   "addi   bai ", "addiu  bai ", "slti   bai ", "sltiu  bai ",
   "andi   bai ", "ori    bai ", "xori   bai ", "lui    zai ",
   "$          ", "cop1   f   ", "cop2   f   ", "???        ",
   "beql   abo ", "bnel   abo ", "blezl  azo ", "bgtzl  azo ",
   "daddi  bai ", "daddiu bai ", "ldl    qa  ", "ldr    qa  ",
   "???        ", "???        ", "???        ", "???        ",
   "lb     qa  ", "lh     qa  ", "lwl    qa  ", "lw     qa  ",
   "lbu    qa  ", "lhu    qa  ", "lwr    qa  ", "lwu    qa  ",
   "sb     qa  ", "sh     qa  ", "swl    qa  ", "sw     qa  ",
   "sdl    qa  ", "sdr    qa  ", "swr    qa  ", "???        ",
   "ll     qa  ", "lwc1   qa  ", "lwc2   qa  ", "???        ",
   "lld    qa  ", "ldc1   qa  ", "ldc2   qa  ", "ld     qa  ",
   "sc     qa  ", "swc1   qa  ", "swc2   qa  ", "???        ",
   "scd    qa  ", "sdc1   qa  ", "sdc2   qa  ", "sd     qa  ",

   /* specials */
   "sll    zbac", "???        ", "srl    zbac", "sra    zbac",
   "sllv   cbaz", "???        ", "srlv   cbaz", "srav   cbaz",
   "jr     azzz", "jalr   bzaz", "???        ", "???        ",
   "syscally   ", "break  y   ", "???        ", "sync   zzzz",
   "mfhi   zzaz", "mthi   azzz", "mflo   zzaz", "mtlo   azzz",
   "dsllv  cbaz", "???        ", "dsrlv  cbaz", "dsrav  cbaz",
   "mult   abzz", "multu  abzz", "div    abzz", "divu   abzz",
   "dmult  abzz", "dmultu abzz", "ddiv   abzz", "ddivu  abzz",
   "add    bcaz", "addu   bcaz", "sub    bcaz", "subu   bcaz",
   "and    bcaz", "or     bcaz", "xor    bcaz", "nor    bcaz",
   "???        ", "???        ", "slt    bcaz", "sltu   bcaz",
   "dadd   bcaz", "daddu  bcaz", "dsub   bcaz", "dsubu  bcaz",
   "tge    ab  ", "tgeu   ab  ", "tlt    ab  ", "tltu   ab  ",
   "teq    ab  ", "???        ", "tne    ab  ", "???        ",
   "dsll   zbac", "???        ", "dsrl   zbac", "dsra   zbac",
   "dsll32 zbac", "???        ", "dsrl32 zbac", "dsra32 zbac",

   /* register immediate */
   "bltz   a o ", "bgez   a o ", "bltzl  a o ", "bgezl  a o ",
   "???        ", "???        ", "???        ", "???        ",
   "tgei   a i ", "tgeiu  a i ", "tlti   a i ", "tltiu  a i ",
   "teqi   a i ", "???        ", "tnei   a i ", "???        ",
   "bltzal a o ", "bgezal a o ", "bltzalla o ", "bgezalla o ",
   "???        ", "???        ", "???        ", "???        ",
   "???        ", "???        ", "???        ", "???        ",
   "???        ", "???        ", "???        ", "???        ",

   /* coprocessor 0 */
   "mfc0    abz", "dmfc0   abz", "???        ", "???        ",
   "mtc0    abz", "dmtc0   abz", "???        ", "???        ",
   
};

/*
 * R4640 context
 *
 * Notes:
 *
 * 1. Need to verify the 32/64 bit behavior of the actual CPU for
 * pc, epc, and errorpc registers.
 *
 * 2. The R4640 can access bytes, words and doubles. The size of
 * emulated memory is going to assist the operations that match it,
 * and slow those that don't. I picked 32 bits. Be aware that
 * memory array addresses are the byte address divided by 4.
 *
 */

struct con4640 {

   ull reg[32];               /* register set */
   ull pc;                    /* execution point */
   ull hi;                    /* hi result */
   ull low;                   /* low result */
   ull cp0[32];               /* control registers */
   float fgr[32];             /* floating point registers */   
   ul fcr[32];                /* floating point control registers */
   ull slot;                  /* slot address save */
   int noint;                 /* single cycle interrupt disable */
   ull sav;                   /* pc save during instruction */
   unsigned long mem[MAXMEM]; /* memory emulation array */

};

/*
 * Global variables
 */

char    cmdlin[MAXCMD];  /* user command line */
int     cmdinx;          /* index for command line */
char    codbuf[MAXCOD];  /* encode run buffer */
int     codinx;          /* index for code buffer */
struct  con4640 context; /* context for CPU */
ll      stack[MAXSTK];   /* execution stack */
int     stkptr;          /* stack pointer */
jmp_buf errenv;          /* context for error jump */
evtptr  actevt;          /* active events list */
evtptr  freevt;          /* free events list */
ull     cpuadr;          /* offset CPU address for event access */
ull     cpudat;          /* CPU read/write data for event access */
int     stpexc;          /* stop exceptions flag */
FILE    *diagfil[10];    /* diagnostics files */

/*******************************************************************************

Process error

Prints the given error message.

*******************************************************************************/

void error(int errcod)

{

  printf("*** Error: ");
  switch (errcod) {

     case ENONE:    break;
     case ECMDNF:   printf("Command not found"); break;
     case ELABEXP:  printf("Label expected"); break;
     case EIIECP:   printf("Illegal instruction exception processed"); break;
     case EOECP:    printf("Overflow exception processed"); break;
     case ECMAEXP:  printf("\',\' expected"); break;
     case EREGEXP:  printf("Register expected"); break;
     case ETRAP:    printf("Conditional trap processed"); break;
     case EBREAK:   printf("Break instruction exception processed"); break;
     case EDFOPN:   printf("Cannot open diagnostic file"); break;
     case EDFHAN:   printf("Invalid handle for diagnostic file"); break;
     case EDFNOPN:  printf("Diagnostic file not open"); break;
     case EDFREAD:  printf("Cannot read diagnostic file"); break;
     case EDFWRITE: printf("Cannot write diagnostic file"); break;
     case EDITINS:  printf("Invalid diagnostic instruction"); break;
     case ESYSCAL:  printf("System call trap processed"); break;
     case EUNSCOP:  printf("Unusable coprocessor trap"); break;
     case EOPNFIL:  printf("Cannot open file"); break;
     case EMEMRNG:  printf("Reference beyond end of memory"); break;
     case ENUMEXP:  printf("Number expected"); break;
     case ELPEXP:   printf("\'(\' expected"); break;
     case ERPEXP:   printf("\')\' expected"); break;
     case EOPCNF:   printf("Opcode not found"); break;
     case EINVREG:  printf("Invalid register number"); break;
     case EBADEXC:  printf("System error: Bad exec code"); break;
     default:       printf("System error"); break;

  }
  printf("\n");
  longjmp(errenv, 0); /* go back to parser */

}

/*******************************************************************************

Get event record

*******************************************************************************/

evtptr getevt()

{

   evtptr ep; /* event pointer */

   if (freevt) { /* free events exist */

      ep = freevt; /* set event */
      freevt = freevt->next; /* gap list */

   } else ep = (evtptr) malloc(sizeof(event));
   return ep; /* exit with pointer */

}      

/*******************************************************************************

Put event record

*******************************************************************************/

void putevt(evtptr ep)

{

   ep->next = freevt; /* place on free list */
   freevt = ep;

}

/*******************************************************************************

Insert event to active list

*******************************************************************************/

void insevt(evtptr ep)

{

   ep->next = actevt; /* insert to list */
   actevt = ep;

}

/*******************************************************************************

Execute active events

Calls each of the event handlers in the active queue. The active queue
becomes empty.

*******************************************************************************/

void exeact()

{

   evtptr ep; /* event pointer */

   while (actevt) { /* while active events exist */

      (*actevt->func)(actevt->reason); /* execute function */
      ep = actevt; /* save top entry */
      actevt = actevt->next; /* gap list */
      putevt(ep); /* release entry */

   }

}

/*******************************************************************************

Get line from file

*******************************************************************************/

void getlin(FILE *fp, char buff[])

{

   int i; /* index for input buffer */
   char c; /* character buffer */

   i = 0; /* set 1st character */
   c = getc(fp); /* get next character */
   while ((c != EOF) && (c != '\n')) { /* gather characters */

      buff[i++] = c; /* place character in buffer */
      c = getc(fp); /* get next character */

   }
   buff[i] = 0; /* terminate buffer */
   cmdinx = 0; /* reset the command line index */

}

/*******************************************************************************

Get number

Returns the next number found.

*******************************************************************************/

ll getnum()

{

   ll v; /* return value */

   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   v = 0; /* clear result */
   if (cmdlin[cmdinx] == '$') { /* hex value */

      cmdinx++; /* skip '$' */
      if (!isalnum(cmdlin[cmdinx])) error(ENUMEXP);
      while (isalnum(cmdlin[cmdinx]))
         isalpha(cmdlin[cmdinx]) ? v = v*16+tolower(cmdlin[cmdinx++])-'a'+10:
                                   (v = v*16+cmdlin[cmdinx++]-'0');

   } else {
 
      if (!isdigit(cmdlin[cmdinx])) error(ENUMEXP);
      while isdigit(cmdlin[cmdinx]) v = v*10+cmdlin[cmdinx++]-'0';

   }
   return v; /* exit with result */

}

/*******************************************************************************

Get label

Returns the next label from the command line.

*******************************************************************************/

void getlab(char lab[])

{

   int i; /* index for label */

   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   i = 0; /* set 1st label position */
   while (((cmdlin[cmdinx] >= 'a') && (cmdlin[cmdinx] <= 'z')) ||
          ((cmdlin[cmdinx] >= '0') && (cmdlin[cmdinx] <= '9')))
      /* is a valid label character */
      lab[i++] = cmdlin[cmdinx++]; /* place next character */
   lab[i] = 0; /* terminate label */
   if (!(*lab)) error(ELABEXP);

}

/*******************************************************************************

Lookup function

Searches for a parse function by the given name. Returns the associated
routine, or a 0 if none is found.

*******************************************************************************/

int (*fndfnc(char lab[]))()

{

   struct cmdety *cp;  /* pointer to command entries */
   int (*fp)(); /* pointer to command function */

   cp = cmdtbl; /* index 1st entry */
   fp = 0; /* set nothing found */
   while (*cp->name) { /* search for commands */
   
      if (!strcmp(lab, cp->name)) fp = cp->func; /* found command, set */
      cp++; /* next entry */

   }
  
   return fp; /* return with function address */

}

/*******************************************************************************

Lookup register

Searches for a register by the given name. Returns the associated
register address, plus a code indicating what place (main registers,
coprocessor, etc.), that it resides
Returns 0 for register found, -1 for not.

*******************************************************************************/

int fndreg(char lab[], int *addr, int *loc)

{

   struct regety *rp;  /* pointer to register entries */
   int (*fp)(); /* pointer to command function */

   rp = regtbl; /* index 1st entry */
   *addr = 0; /* set nothing found */
   while (*rp->name) { /* search for commands */
   
      if (!strcmp(lab, rp->name)) { /* found register */

         *addr = rp->reg; /* set register address */
         *loc = rp->loc; /* set location */
         return 0; /* exit found */

      }
      rp++; /* next entry */

   }
   return -1; /* set nothing found */

}
     
/*******************************************************************************

List instruction

Lists the instruction at the given address. What a joy... the mips only has
one length of instruction (32 bits). This means that if you want to list
multiple instructions, just list and increment by 1.
The address should be aligned. Bits 0 and 1 are ignored.

*******************************************************************************/

/* process register specs  */

void prtreg(ul   addr,     /* address */
            ul   inst,     /* instruction code */
            char insstr[], /* instruction templete */
            int  *cnt)     /* output counter */

{

   int  b;     /* bit */
   int  i;     /* string index */
   char r;     /* register id */
   int  first; /* first parameter flag */
   ul   t;     /* temp */

   first = TRUE; /* set 1st parameter to output */
   /* for the first pass, we assign the registers in order of disassembly
      line appearance */
   r = 'a'; /* set 1st register */
   for (r = 'a'; r <= 'c'; r++) { /* iterate registers */

      /* search for that register */
      b = 21; /* set bit equivalent */
      for (i = 7; i < 11; i++) {

         if (insstr[i] == r) {

            /* separate if not first parameter */
            if (!first) { putchar(','); (*cnt)++; }
            printf("%x", inst >> b & 0x1f); /* print register */
            (*cnt)++; /* count that */
            if ((inst >> b & 0x1f) > 15) (*cnt)++;
            first = FALSE; /* set no longer first parameter */

         }
         b = b-5; /* set new bit equivalent */

      }

   }
   /* process the ad hoc specs */
   b = 21; /* set bit equivalent of high register */
   for (i = 7; i < 11; i++) { 

      switch (insstr[i]) {

         case 's': /* segment address */
            /* separate if not first parameter */
            if (!first) { putchar(','); (*cnt)++; }
            printf("%8.8lx", inst<<2&0x0fffffff | addr+4&0xf0000000);
            *cnt += 8; /* count */
            first = FALSE; /* set no longer first parameter */
            break;
         case 'z': /* zero check */
            if (inst >> b & 0x1f != 0) { printf("???"); *cnt += 3; }
            break;
         case 'o': /* pc offset */
            /* separate if not first parameter */
            if (!first) { putchar(','); (*cnt)++; }
            t = inst << 2 & 0x3ffff; /* find offset */
            /* sign extend result */
            if ((t&0x20000) != 0) t |= 0xfffc0000;
            printf("%8.8lx", addr+4+(long)t);
            *cnt += 8; /* count */
            first = FALSE; /* set no longer first parameter */
            break;
	     case 'i': /* immediate */
            /* separate if not first parameter */
            if (!first) { putchar(','); (*cnt)++; }
            printf("%4.4x", inst & 0xffff);
            *cnt += 4; /* count */
            first = FALSE; /* set no longer first parameter */
            break;
         case 'f': /* coprocessor function */
            /* separate if not first parameter */
            if (!first) { putchar(','); (*cnt)++; }
            printf("%7.7x", inst & 0x03ffffff);
            *cnt += 7; /* count */
            first = FALSE; /* set no longer first parameter */
            break;
         case 'q': /* base/offset */
            /* separate if not first parameter */
            if (!first) { putchar(','); (*cnt)++; }
            printf("%4.4x(%i)", inst & 0xffff, inst >> b & 0x1f);
            *cnt += 4; /* count */
            first = FALSE; /* set no longer first parameter */
            break;
         case 'y': /* system call code */
            /* separate if not first parameter */
            if (!first) { putchar(','); (*cnt)++; }
            printf("%5.5x", inst >> 6 & 0xfffff);
            *cnt += 5; /* count */
            first = FALSE; /* set no longer first parameter */
            break;

      }
      b = b-5; /* set new bit equivalent */

   }

}

/* The main instruction dispatcher */

void lstins(ul addr, /* address to list */
            int fld) /* field width */

{

   unsigned long inst; /* instruction holder */
   int i; /* table index */
   int cnt; /* output space count */

   addr = addr&0xfffffffffffffffcull; /* reset lower bits */
   inst = context.mem[addr >> 2]; /* get instruction */
   /* print address and instruction in hex */
   printf("%8.8lx: %8.8lx         ", addr, inst);
   cnt = 8+1+1+8+1+8; /* set output count */
   i = inst >> 26; /* get opcode */
   if (opctbl[i][0] == '*') /* special */
      i = 64+(inst & 0x3f);
   else if (opctbl[i][0] == '&') /* register immediate */
      i = 64+64+(inst >> 16 & 0x1f);
   else if (opctbl[i][0] == '$') /* coprocessor 0 */            
      i = 64+64+32+(inst >> 21 & 0x07);
   printf("%7.7s ", opctbl[i]); /* print memonic */
   cnt += 8; /* count */
   prtreg(addr, inst, opctbl[i], &cnt); /* print registers */
   /* pad as required to end of field */
   while (cnt < fld) { putchar(' '); cnt++; }

}

/*******************************************************************************

Diagnostics service

This routine gets the diagnostic escape instruction. This is a special
instruction that will not occur when executing a normal program, and in fact
has an invalid encoding in the real processor.
The diagnostic escape allows the program to perform I/O to the operator
console in simulation mode, and is strictly used for simulation testing.
The operands are passed in registers, and the calls are as follows:

Open channel ===================================================================

Opens a channel by name and handle. The program picks the handle, and gives
the filename to open. Opening an already in use ID will close the current ID.

R4: $0000000000000001 - Instruction code.
R5: $xxxxxxxxxxxxxxxx - Channel identifier, 0-9. Indicates which channel is
                        being opened.
R6: $xxxxxxxxxxxxxxxx - Channel string. Gives the address of a character string
                        giving the path or device to be opened.
R7: $xxxxxxxxxxxxxxxx - Channel mode. 0=read, 1=write.

Close channel ==================================================================

Closes a specific channel by ID.

R4: $0000000000000002 - Instruction code
R5: $xxxxxxxxxxxxxxxx - Channel identifier, 0-9.

Read ===========================================================================

Reads the specified number of bytes to an address from the channel.

R4: $0000000000000003 - Instruction code.
R5: $xxxxxxxxxxxxxxxx - Channel identifer, 0-9.
R6: $xxxxxxxxxxxxxxxx - Address to read to.
R7: $xxxxxxxxxxxxxxxx - Length of data.

Write ==========================================================================

Writes the specified number of bytes to a channel from the address.

R4: $0000000000000004 - Instruction code.
R5: $xxxxxxxxxxxxxxxx - Channel identifier, 0-9.
R6: $xxxxxxxxxxxxxxxx - Address to write from.
R7: $xxxxxxxxxxxxxxxx - Length of data.

================================================================================

The channel IDs 0, 1 and 2 are predefined, and equate as follows:

0 - Standard input.
1 - Standard output.
2 - Standard error.

Just as in C. A filename can be any unix valid path. On a read file, the
file must already exist. On a write file, if it exists, it is deleted,
then a new file created.
There are no error returns. on return, registers r2 and r3 are both cleared.
All errors are handled by aborting the program with an error message.

*******************************************************************************/

void diagsvs()

{

   switch (context.reg[4]) { /* tap instruction */

      case 1: /* open */
	 /* check handle is valid */
         if (context.reg[5] < 0 | context.reg[5] > 9) error(EDFHAN);
         /* check for open file, and close if so */
         if (diagfil[context.reg[5]]) fclose(diagfil[context.reg[5]]);
         if (context.reg[7]) /* write */
            diagfil[context.reg[5]] =
               fopen((char *)&context.mem[context.reg[6]], "w");
         else /* read */
            diagfil[context.reg[5]] =
               fopen((char *)&context.mem[context.reg[6]], "r");
         if (!diagfil[context.reg[5]]) error(EDFOPN);
         break; 
      case 2: /* close */
         /* check handle is valid */
         if (context.reg[5] < 0 | context.reg[5] > 9) error(EDFHAN);
         /* check file is open */
         if (!diagfil[context.reg[5]]) error(EDFNOPN);
         fclose(diagfil[context.reg[5]]); /* close the file */
         break;
      case 3: /* read */
        /* check handle is valid */
         if (context.reg[5] < 0 | context.reg[5] > 9) error(EDFHAN);
         /* check file is open */
         if (!diagfil[context.reg[5]]) error(EDFNOPN);
         if (fread(&context.mem[context.reg[6]], 1, context.reg[7],
                   diagfil[context.reg[5]]) != context.reg[7])
            error(EDFREAD);
         break;
      case 4:
        /* check handle is valid */
         if (context.reg[5] < 0 | context.reg[5] > 9) error(EDFHAN);
         /* check file is open */
         if (!diagfil[context.reg[5]]) error(EDFNOPN);
         if (fwrite(&context.mem[context.reg[6]], 1, context.reg[7],
                   diagfil[context.reg[5]]) != context.reg[7])
            error(EDFWRITE);
         break;
      default: error(EDITINS); /* invalid tap instruction */

   }

}

/*******************************************************************************

Step processor

Single steps the MIPS through the next instruction. For efficiently reasons,
this may later have to be changed to an exit on event.
Good news-bad news. The good news is that the MIPS is very easy to emulate
in software, because it is flagless, and we don't have to calculate flag
settings after each instruction. The bad news is that the "jump delay slot"
creates a circumstance where the address change does not occur until after the
next instruction, and we have to account for that. The way we simulate that
is by simulating the pipeline. Each instruction stores the address the NEXT
instruction is to use to update the PC. Then, each instruction completes,
transfers the slot address to the current address, and creates a new slot
address.
This routine uses a lot of macros, and that deserves some explaination.
The exec does a lot of similar, and very low level, operations for each
instruction. Because the exec is time critical, it is definately worth it
to put virtually all the work in line for each instruction. For example,
most instructions use a standard slot setup that consists of the pc+4.
But I still have all instructions set up their own slot, vs. putting that
at the top, because it saves cycles for the instructions that don't do
the default slot thing (jumps).
The result is that lots of macros help to clean up this code. The other way is
to rely on compiler inlining.
Hazards: Hazards (for us) are where there may be differing behavior from
the CPU. The MIPs docs says specifically that these are not defined, and
I have interpreted behavior as whatever has the best execution time.
We may improve some of the hazards (move to CPU behavior) as time goes
on, and we find out what the actual behavior is:

1. On jal and jalr instructions, the link register (31) is written before
the slot instruction is executed. The CPU probally delays the write until
the actual control transfer. This makes the behavior different for a slot
instruction that accesses the link register.

2. For 32 arithmetic (add, etc), the effect of having a 64 bit register
content will vary, because we may or may not use 64 bit math. The MIPs
spec leaves this undefined.

3. Although bad opcodes are caught, bad fields are not (fields non-zero).
Unknown what the CPU does with these.

Is this routine machine dependent ? Probally. There are loads of places
where we depend on shifts, etc. doing the right thing with the bits.
It would need to be rechecked on a non-sparc machine.
The diagnostic service tap is assigned instruction code $3fffffff. This is
a regimm instruction with a $1f rt field. This was choosen because it is the
last reserved instruction in the map, and is on a branch (regimm). It will
not trip unless you set ALL unused bits to 1's (it must exactly match
$3fffffff).
The processor is assumed to be in big endian mode. If little endian mode is
required, it would probally be more efficient to write another executor just
to perform that mode.
At present, we don't implement floating point modes, and all references to
coprocessor 1 generate traps.

*******************************************************************************/
   
/* sign extend 16 bit to 64 bit */

#define SIGN(val) (val & 0x8000 ? val | 0xffffffffffff0000ll: val & 0x000000000000ffffll)

/* this macro makes it easier to access registers */

#define REG(bit) (context.reg[inst >> bit & 0x1f])

/* macro yeilds 16 bit signed offset address */

#define OFF ((context.pc+1)+(SIGN(inst) << 2))

/* process branch from 16 bit offset */

#define BRANCH16 (context.slot = OFF)

/* process slot continue */

#define CONT (context.slot = context.pc+4)

/* get double word from memory */

#define DOUBLE(addr) ((ull)context.mem[addr/4] << 32 | context.mem[addr/4+1])

/* process general excption */

void except(int exception) /* exception code */

{

   context.cp0[CP0EPC] = context.sav; /* set error pc */
   context.cp0[CP0CAUSE] = exception << 1; /* set cause */ 
   context.cp0[CP0SR] |= 0x00000002; /* set exception process bit */
   if (context.sav != context.slot-4) { /* must be in delay slot */

      context.cp0[CP0EPC] = context.sav-4; /* set to branch */
      context.cp0[CP0CAUSE] |= 0x80000000; /* set delay slot bit */

   }
   if (context.cp0[CP0CAUSE] & 0x00800000) { /* interrupt vector select */

      if (context.cp0[CP0SR] & 0x00400000) context.pc = 0xbfc00400;
      else context.pc = 0x80000200;

   } else {

      if (context.cp0[CP0SR] & 0x00400000) context.pc = 0xbfc00380;
      else context.pc = 0x80000180;

   }
   CONT; /* normal continue */

}

void illegal(void) { if (stpexc) error(EIIECP); else except(ECRI); }

void oexcept(void) { if (stpexc) error(EOECP); else except(ECOV); }

void trap(void) { if (stpexc) error(ETRAP); else except(ECTR); }

void breake(void) { if (stpexc) error(EBREAK); else except(ECBP); }

void syscall(void) { if (stpexc) error(ESYSCAL); else except(ECSYS); }

void nocop() { if (stpexc) error(EUNSCOP); else except(ECCPU); }

void stpins(void)

{

   unsigned long inst; /* instruction save */
   unsigned long a, b; /* temps */
   unsigned long long al, bl, cl, p1, p2, p3, p4, c; /* big temps */
   int s; /* sign flag */
 
   context.noint = FALSE; /* reset any single cycle disable */
   inst = context.mem[context.pc >> 2]; /* get current instruction */
   context.sav = context.pc; /* save current pc */
   context.pc = context.slot; /* set up the slot instruction */
   context.reg[0] = 0ll; /* set up zero register */
   /* decode and go instruction */
   switch (inst >> 26) {

      case 0x00: switch (inst & 0x3f) { /* process special page */

         case 0x00: REG(11) = (long)REG(16) << (inst >> 6 & 0x1f); CONT; break;
         case 0x02: REG(11) = (long)((ul)REG(16) >> (inst >> 6 & 0x1f)); CONT; break;
         case 0x03: REG(11) = (long)REG(16) >> (inst >> 6 & 0x1f); CONT; break;
         case 0x04: REG(11) = (long)REG(16) << REG(6); CONT; break;
         case 0x06: REG(11) = (long)((ul)REG(16) >> REG(6)); CONT; break;
         case 0x07: REG(11) = (long)REG(16) >> REG(6); CONT; break;
         case 0x08: context.slot = REG(21); break;
         case 0x09: REG(11) = context.pc+1; context.slot = REG(21); break;
         case 0x0a: illegal();
         case 0x0b: illegal();
         case 0x0c: syscall();
         case 0x0d: breake();
         case 0x0f: ;
         case 0x10: REG(11) = context.hi; CONT; break;
         case 0x11: context.hi = REG(11); CONT; break;
         case 0x12: REG(11) = context.low; CONT; break;
         case 0x13: context.low = REG(11); CONT; break;
         case 0x14: REG(11) = REG(16) << REG(6); CONT; break;
         case 0x16: REG(11) = REG(16) >> REG(6); CONT; break;
         case 0x17: REG(11) = (ll)REG(16) >> REG(6); CONT; break;
         case 0x18: al = (long) REG(21)* (long) REG(16); context.hi = al >> 32;
                    context.low = al & 0xffffffff; CONT; break;
         case 0x19: al = REG(21)*REG(16); context.hi = al >> 32;
                    context.low = al & 0xffffffff; CONT; break;
         case 0x1a: context.low = (long) REG(16)/(long) REG(21);
                    context.hi = (long) REG(16)%(long) REG(21); CONT; break;
         case 0x1b: context.low = REG(16)/REG(21); context.hi = REG(16)%REG(21); 
                    CONT; break;
         case 0x1c: al = REG(21); bl = REG(16); p1 = (al&0xffffffff)*(bl&0xffffffff);
                    p2 = (al>>32)*(bl&0xffffffff); p3 = (al&0xffffffff)*(bl>>32);
                    p4 = (al>>32)*(bl>>32); c = 0xffffffff-p1<(p2<<32);
                    context.low = p1+(p2<<32); context.hi = (p2>>32)+c;
                    c = 0xffffffff-context.low<(p3<<32); context.low += p3<<32;
                    context.hi = (p3>>32)+c+p4; CONT; break;
         case 0x1d: al = REG(21); bl = REG(16); s = al^bl&0x8000000000000000ull==0ull;
                    al&0x8000000000000000ull?al:-al; bl&0x8000000000000000ull?bl:-bl;
                    p1 = (al&0xffffffff)*(bl&0xffffffff);
                    p2 = (al>>32)*(bl&0xffffffff); p3 = (al&0xffffffff)*(bl>>32);
                    p4 = (al>>32)*(bl>>32); c = 0xffffffff-p1<(p2<<32);
                    context.low = p1+(p2<<32); context.hi = (p2>>32)+c;
                    c = 0xffffffff-context.low<(p3<<32); context.low += p3<<32;
                    context.hi = (p3>>32)+c+p4; if (s) { context.low = ~context.low;
                    context.hi = ~context.hi; c = context.low == 0xffffffffffffffffull;
                    context.hi += c; } CONT; break;
         case 0x1e: context.low = (ll)REG(21)/(ll)REG(16);
                    context.hi = (ll)REG(21)%(ll)REG(16); CONT; break;
         case 0x1f: context.low = (ll)REG(21)/(ll)REG(16);
                    context.hi = (ll)REG(21)%(ll)REG(16); CONT; break;
         case 0x20: a = REG(21); b = REG(16); 
                    if (!((a^b) & 0x80000000) && ((a+b^a) & 0x80000000)) oexcept(); 
                    (ll) REG(11) = (long) a+b; CONT; break;
         case 0x21: a = REG(21); b = REG(16); REG(11) = a+b; CONT; break;
         case 0x22: a = REG(21); b = REG(16); 
                    if (((a^b) & 0x80000000) && ((a+b^a) & 0x80000000)) oexcept();
                    (ll) REG(11) = (long) a-b; CONT; break; 
         case 0x23: a = REG(21); b = REG(16); (ll) REG(11) = (long) a-b; CONT;
                    break;
         case 0x24: a = REG(21); b = REG(16); (ll)REG(11) = (long) a&b; CONT;
                    break;
         case 0x25: a = REG(21); b = REG(16); (ll)REG(11) = (long) a|b; CONT;
                    break;
         case 0x26: a = REG(21); b = REG(16); (ll)REG(11) = (long) a^b; CONT;
                    break;
         case 0x27: a = REG(21); b = REG(16); (ll)REG(11) = (long) ~(a|b); CONT;
                    break;
         case 0x2a: REG(11) = (ll)REG(21) < (ll)REG(16); CONT; break;
         case 0x2b: REG(11) = REG(21) < REG(16); CONT; break;
         case 0x2c: al = REG(21); bl = REG(16); 
                    if (!((al^bl) & 0x8000000000000000ull) &&
                         ((al+bl^al) & 0x8000000000000000ull)) oexcept(); 
                    REG(11) = al+bl; CONT; break;
         case 0x2d: REG(11) = REG(21)+REG(16); CONT; break;
         case 0x2e: al = REG(21); bl = REG(16); 
                    if (((al^bl) & 0x80000000) && ((al+bl^al) & 0x80000000)) oexcept();
                    REG(11) = al-bl; CONT; break; 
         case 0x2f: REG(11) = REG(21)-REG(16); CONT; break;
         case 0x30: if ((ll)REG(21) >= (ll)REG(16)) trap(); CONT; break;
         case 0x31: if (REG(21) >= REG(16)) trap(); CONT; break;
         case 0x32: if ((ll)REG(21) < (ll)REG(16)) trap(); CONT; break;
         case 0x33: if (REG(21) < REG(16)) trap(); CONT; break;
         case 0x34: if (REG(21) == REG(16)) trap(); CONT; break;
         case 0x36: if (REG(21) != REG(16)) trap(); CONT; break;
         case 0x38: REG(11) = REG(16) << (inst >> 6 & 0x1f); CONT; break;
         case 0x3a: REG(11) = REG(16) >> (inst >> 6 & 0x1f); CONT; break;
         case 0x3b: REG(11) = (ll)REG(16) >> (inst >> 6 & 0x1f); CONT; break;
         case 0x3c: REG(11) = REG(16) << ((inst >> 6 & 0x1f)+32); CONT; break;
         case 0x3e: REG(11) = REG(16) >> ((inst >> 6 & 0x1f)+32); CONT; break;
         case 0x3f: REG(11) = (ll)REG(16) >> ((inst >> 6 & 0x1f)+32); CONT; break;

      } break;
      case 0x01: switch (inst >> 16 & 0x1f) { /* process regimm page */

         case 0x00: if (REG(21) < 0ull) BRANCH16; else CONT; break;
         case 0x01: if (REG(21) >= 0ull) BRANCH16; else CONT; break;
         case 0x02: if (REG(21) < 0ull) BRANCH16; else { context.pc += 4; CONT; }
                    break;
         case 0x03: if (REG(21) >= 0ull) BRANCH16; else { context.pc += 4; CONT; }
                    break;
         case 0x08: if ((ll)REG(21) >= SIGN(inst)) trap(); CONT; break;
         case 0x09: if (REG(21) >= SIGN(inst)) trap(); CONT; break;
         case 0x0a: if ((ll)REG(21) < SIGN(inst)) trap(); CONT; break;
         case 0x0b: if (REG(21) < SIGN(inst)) trap(); CONT; break;
         case 0x0c: if (REG(21) == SIGN(inst)) trap(); CONT; break;
         case 0x0e: if (REG(21) == SIGN(inst)) trap(); CONT; break;
         case 0x10: if (REG(21) < 0ull) { context.reg[31] = context.pc+4; BRANCH16; }
                    else CONT; break;
         case 0x11: if (REG(21) >= 0ull) { context.reg[31] = context.pc+4; BRANCH16; }
                    else CONT; break;
         case 0x12: if (REG(21) < 0ull) { context.reg[31] = context.pc+4; BRANCH16; }
                    else { context.pc += 4; CONT; } break;
         case 0x13: if (REG(21) >= 0ull) { context.reg[31] = context.pc+4; BRANCH16; }
                    else { context.pc += 4; CONT; } break;
         case 0x1f: if (inst == 0x3fffffff) { diagsvs(); CONT; }
                    else illegal();
         default:   illegal();

      } break;
      case 0x02: context.slot = (inst << 2 & 0x0fffffff) | 
                                (context.pc & 0xfffffffff0000000ull); break;
      case 0x03: context.reg[31] = context.pc+4; 
                 context.slot = (inst << 2 & 0x0fffffff) |
                                (context.pc & 0xfffffffff0000000ull); break;
      case 0x04: if (REG(21) == REG(16)) BRANCH16; else CONT; break;
      case 0x05: if (REG(21) != REG(16)) BRANCH16; else CONT; break;
      case 0x06: if ((long) REG(21) < 0) BRANCH16; else CONT; break;
      case 0x07: if ((long) REG(21) > 0) BRANCH16; else CONT; break;
      case 0x08: a = REG(21); b = SIGN(inst); 
                 if (!((a^b) & 0x80000000) && ((a+b^a) & 0x80000000)) oexcept(); 
                 (long long) REG(16) = (long) a+b; CONT; break;
      case 0x09: REG(16) = (ul)REG(21)+(ul)SIGN(inst); CONT; break;
      case 0x0a: REG(16) = REG(21) < SIGN(inst); CONT; break;
      case 0x0b: REG(16) = REG(21) < inst & 0x0000ffff; CONT; break;
      case 0x0c: REG(16) = REG(21) & inst & 0x0000ffff; CONT; break;
      case 0x0d: REG(16) = REG(21) | inst & 0x0000ffff; CONT; break;
      case 0x0e: REG(16) = REG(21) ^ inst & 0x0000ffff; CONT; break;
      case 0x0f: REG(16) = inst << 16; CONT; break;
      case 0x10: switch (inst >> 26 & 0x1f) { /* cop0 */

         case 0x00: REG(16) = context.cp0[inst >> 11 & 0x1f];
         case 0x01: REG(16) = context.cp0[inst >> 11 & 0x1f];
         case 0x04: context.cp0[inst >> 11 & 0x1f] = REG(16);
         case 0x05: context.cp0[inst >> 11 & 0x1f] = REG(16);
         default: illegal();

      }
      case 0x11: nocop();
      case 0x12: nocop();
      case 0x14: if (REG(21) == REG(16)) BRANCH16; 
                 else { context.pc = context.pc+4; CONT; }
      case 0x15: if (REG(21) != REG(16)) BRANCH16;
                 else { context.pc = context.pc+4; CONT; }
      case 0x16: if ((long) REG(21) < 0) BRANCH16;
                 else { context.pc = context.pc+4; CONT; }
      case 0x17: if ((long) REG(21) > 0) BRANCH16;
                 else { context.pc = context.pc+4; CONT; }
      case 0x18: al = REG(21); bl = SIGN(inst);
                 if (!((al^bl) & 0x8000000000000000ull) && 
                      ((a+b^a) & 0x8000000000000000ull)) oexcept(); 
                 REG(16) = al+bl; CONT; break;                
      case 0x19: REG(16) = REG(21)+SIGN(inst); CONT; break;
      case 0x1a: al = REG(21)+SIGN(inst); a = al%8*8; bl = DOUBLE(al) << a;
                 al = 0xffffffffffffffffull >> 64-al%8*8; REG(16) = bl|REG(16)&al;
                 CONT; break;
      case 0x1b: al = REG(21)+SIGN(inst); a = (8-al%8)*8; bl = DOUBLE(al) >> a;
                 al = 0xffffffffffffffffull << 64-a; REG(16) = bl|REG(16)&al; CONT;
                 break;
      case 0x20: al = REG(21)+SIGN(inst); b = context.mem[al/4];
	         b >>= 3-al%4*8; b & 0x80 ? REG(16) = b | 0xffffffffffff0000ull:
                                            (REG(16) = b & 0x00000000000000ffull); break;
      case 0x21: al = REG(21)+SIGN(inst); b = context.mem[al/4];
                 b >>= 1-al%2*16; REG(16) = SIGN(inst); break;
      case 0x22: al = REG(21)+SIGN(inst); a = al%4*8; bl = context.mem[al/4] << a;
                 al = 0xffffffffull >> 32-a; REG(16) = bl|REG(16)&al;
                 CONT; break;
      case 0x23: (ll) REG(16) = (long) context.mem[REG(21)+SIGN(inst)/4]; break;
      case 0x24: al = REG(21)+SIGN(inst); b = context.mem[al/4];
                 b >>= 3-al%4*8; REG(16) = b & 0x00000000000000ff; break;
      case 0x25: al = REG(21)+SIGN(inst); b = context.mem[al/4];
                 b >>= 1-al%2*16; REG(16) = b & 0x000000000000ffff; break;
      case 0x26: al = REG(21)+SIGN(inst); a = al%4*8; bl = context.mem[al/4] >> 24-a;
                 al = 0xffffff00ull << a; REG(16) = bl|REG(16)&al;
                 CONT; break;
      case 0x27: REG(16) = context.mem[REG(21)+SIGN(inst)/4]; break;
      case 0x28: al = REG(21)+SIGN(inst); a = 3-al%4*8; b = (REG(16) & 0xff) << a;
                 context.mem[al/4] = b|context.mem[al/4] & ~(0x000000ff << a); break; 
      case 0x29: al = REG(21)+SIGN(inst); a = 1-al%4*16; b = (REG(16) & 0xffff) << a;
                 context.mem[al/4] = b|context.mem[al/4] & ~(0x0000ffff << a); break; 
      case 0x2a: al = REG(21)+SIGN(inst); a = al%4*8; bl = REG(16) >> a;
                 cl = 0xffffffff00000000ull >> a; context.mem[al/4] = bl|REG(16)&cl;
                 CONT; break;
      case 0x2b: al = REG(21)+SIGN(inst); context.mem[al/4] = REG(16); break;
      case 0x2c: al = REG(21)+SIGN(inst); a = al%8*8; bl = REG(16) >> a;
                 cl = 0xffffffffffffff00ull << 56-a;
                 context.mem[al/4] = bl>>32|context.mem[al/4]&cl>>32;
                 context.mem[al/4] = bl|context.mem[al/4]&cl;
                 CONT; break;
      case 0x2d: al = REG(21)+SIGN(inst); a = al%8*8; bl = REG(16) << 56-a;
                 cl = 0xffffffffffffffffull << 56-a;
                 context.mem[al/4] = bl>>32|context.mem[al/4]&cl>>32;
                 context.mem[al/4] = bl|context.mem[al/4]&cl;
                 CONT; break;
      case 0x2e: al = REG(21)+SIGN(inst); a = 24-al%4*8; bl = REG(16) << a;
                 cl = 0xffffffffull << a; context.mem[al/4] = bl|REG(16)&cl;
                 CONT; break;
      case 0x30: (ll) REG(16) = (long) context.mem[REG(21)+SIGN(inst)/4];
                 context.noint = TRUE; break;
      case 0x31: nocop();
      case 0x32: nocop();
      case 0x34: al = REG(21)+SIGN(inst); 
                 REG(16) = (ull) context.mem[al/4] << 32|context.mem[al/4+1];
                 context.noint = TRUE; break;
      case 0x35: nocop();
      case 0x36: nocop();
      case 0x37: al = REG(21)+SIGN(inst); 
                 REG(16) = (ull) context.mem[al/4] << 32|context.mem[al/4+1]; break;
      case 0x38: if (context.noint) { context.mem[REG(21)+SIGN(inst)/4] = REG(16);
                                      REG(16) = 1ull; }  
                 else REG(16) = 0ull; break;
      case 0x39: nocop();
      case 0x3a: nocop();
      case 0x3c: if (context.noint) { al = REG(21)+SIGN(inst)/4; bl = REG(16);
                                      context.mem[al] = bl >> 32; context.mem[al] = bl;
                                      REG(16) = 1ull; }
                 else REG(16) = 0ull; break;
      case 0x3d: nocop();
      case 0x3e: nocop();
      case 0x3f: al = REG(21)+SIGN(inst)/4; bl = REG(16); context.mem[al] = bl >> 32;
                 context.mem[al] = b; break;
      default:   illegal();

   }

}

/*******************************************************************************

Place constant

Places a 64 bit unsigned constant into the code line as a load.

*******************************************************************************/

void plccst(ull cst)

{

   int i;

   codbuf[codinx++] = UELCST; /* place load constant instruction */
   /* place 64 bit constant */
   for (i = 0; i < 8; i++) { codbuf[codinx++] = cst >> 56; cst <<= 8; }

}

/*******************************************************************************

Parse factor

*******************************************************************************/

void factor()

{

   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   if (cmdlin[cmdinx] == '+') { /* positivition */

      cmdinx++; /* skip '+' */
      factor(); /* parse factor */

   } else if (cmdlin[cmdinx] == '-') { /* negation */

      cmdinx++; /* skip '-' */
      factor(); /* parse factor */
      codbuf[codinx++] = UENEG; /* place tolken */

   } else if (cmdlin[cmdinx] == '(') { /* (expr) */

      cmdinx++; /* skip '(' */
      expr(); /* parse expression */
      while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
      if (cmdlin[cmdinx] != ')') printf("*** Error: \')\' expected\n");
      else cmdinx++; /* skip ')' */

   } else plccst(getnum()); /* must be number */

}

/*******************************************************************************

Parse term

*******************************************************************************/

void term()

{

   factor(); /* parse factor */
   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   if (cmdlin[cmdinx] == '*') { /* multiply */
  
      cmdinx++; /* skip '*' */
      factor(); /* parse factor */
      codbuf[codinx++] = UEMULT; /* place tolken */

   } else if (cmdlin[cmdinx] == '/') { /* divide */

      cmdinx++; /* skip '/' */
      factor(); /* parse factor */
      codbuf[codinx++] = UEDIV; /* place tolken */

   } else if (cmdlin[cmdinx] == '%') { /* modulo */

      cmdinx++; /* skip '%' */
      factor(); /* parse factor */
      codbuf[codinx++] = UEMOD; /* place tolken */

   }

}

/*******************************************************************************

Parse simple expression

*******************************************************************************/

void sexpr()

{

   term(); /* parse term */
   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   if (cmdlin[cmdinx] == '+') { /* add */
  
      cmdinx++; /* skip '+' */
      term(); /* parse term */
      codbuf[codinx++] = UEADD; /* place tolken */

   } else if (cmdlin[cmdinx] == '-') { /* subtract */

      cmdinx++; /* skip '-' */
      term(); /* parse term */
      codbuf[codinx++] = UESUB; /* place tolken */

   }

}

/*******************************************************************************

Parse expression

*******************************************************************************/

void expr()

{

   sexpr(); /* get simple expression */
   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   if (cmdlin[cmdinx] == '<') {

      cmdinx++; /* skip '<' */
      if (cmdlin[cmdinx] == '=') { /* less than or equal */

         cmdinx++; /* skip '=' */
         sexpr(); /* get simple expression */
         codbuf[codinx++] = UELEQU; /* place tolken */

      } else { /* less than */

         sexpr(); /* get simple expression */
         codbuf[codinx++] = UELTN; /* place tolken */

      }
         
   } else if (cmdlin[cmdinx] == '>') {

      cmdinx++; /* skip '>' */
      if (cmdlin[cmdinx] == '=') { /* greater than or equal */
      
         cmdinx++; /* skip '=' */
         sexpr(); /* get simple expression */
         codbuf[codinx++] = UEGEQU; /* place tolken */
         
      } else { /* greater than */

         sexpr(); /* get simple expression */
         codbuf[codinx++] = UEGTN; /* place tolken */

      }

   } else if (cmdlin[cmdinx] == '=') {

      cmdinx++; /* skip '=' */
      sexpr(); /* get simple expression */
      codbuf[codinx++] = UEEQU; /* place tolken */

   } else if (cmdlin[cmdinx] == '!')
      if (cmdlin[cmdinx+1] == '=') { /* not equal */

      cmdinx += 2; /* skip "!=" */
      sexpr(); /* get simple expression */
      codbuf[codinx++] = UENEQU; /* place tolken */

   }

}

/*******************************************************************************

Dump memory

*******************************************************************************/

void dump(ll a, ll b)

{

   int c; /* word count */

   c = 0; /* clear word count */
   while (a <= b) { /* dump memory words */

      if (!c) printf("%8.8llx: ", a); /* place address on line */
      printf("%8.8lx ", context.mem[a/4]);
      a += 4; /* next word */
      c++; /* count words */
      if (c == 8) { putchar('\n'); c = 0; } /* process lines */

   }
   if (c) putchar('\n'); /* finish line */

}
     
/*******************************************************************************

Assemble instruction

Assembles a single instruction, and leaves the instruction word on the stack.
This works because the MIPS instructions are all one word.
the assembly allows full expressions. Because the expressions yeild at runtime,
the final instruction is contructed at runtime. This is done by starting
with a base opcode loaded onto the stack, then 'or'ing successive parameter
values into that. The result is just about the fastest method you can imagine
to construct arbitrary instructions at runtime, and is of course fantasically
unessesary. We do this only because it allows access to the standard expression
processor.
Note That excess bits are discarded, ie., if an immediate value field is over
16 bits, it just gets truncated, as with other bit fields.
Note: The "syscall" and "break" instructions require a parameter. "syscall 0"
and "break 0" is the equivalent of the assemblers "syscal" (no parameters) and
"break".

*******************************************************************************/

void assm(ul addr) /* reference address to assemble to */

{

   int i; /* index for opcode table */
   int f; /* found opcode index */
   char l[20], l1[7]; /* label holders */
   ul opc; /* opcode contructor */
   char c; /* register code character */
   int first; /* first parameter flag */
   int b; /* bit shift value */
   int ln; /* length of opcode */
   int x, y; /* general indexes */

   getlab(l); /* get opcode to process */
   ln = strlen(l); /* find length of opcode */
   if (ln > 7) error(EOPCNF); /* bad length */
   f = -1; /* set no opcode found */
   /* search for matching opcode */
   for (i = 0; i < 64+64+32+8; i++) {

      /* for a rather incompetent compare, copy space terminated to temp */
      y = 0;
      for (x = 0; x < 7; x++) if (opctbl[i][x] != ' ') l1[y++] = opctbl[i][x];
      l1[y] = 0; /* terminate */
      if (!strcmp(l1, l)) f = i;

   }
   if (f < 0) error(EOPCNF); /* no opcode found */
   /* place base opcode */
   if (f < 64) plccst(f<<26); /* base page */   
   else if (f < 64+64) plccst(f-64); /* specials */
   else if (f < 64+64+32) plccst(1<<26|f-64-64<<16); /* register immediate */
   else plccst(0x10<<26|f-64-64-32<<21); /* coprocessor 0 */
   first = TRUE; /* set on first parameter */
   /* search for registers in descriptor. we take advantage of some
      interesting facts here:

      1. All registers appear first in the parameter list.
      2. If a "special" parameter appears, there is only one,
         and that appears last.

      seems like those MIPS guys had their act together eh ? actually,
      this is standard CPU design */
   for (c = 'a'; c <= 'd'; c++) {

      b = 21; /* set bit equivalent of high register */
      /* search all descriptor locations */
      for (i = 7; i < 11; i++) {

         if (opctbl[f][i] == c) { /* found a register */

            if (!first) { /* check parameter separator */

               while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
               if (cmdlin[cmdinx] != ',') 
                  error(ECMAEXP); /* missing ',' error */
               cmdinx++; /* skip ',' */

            }
            expr(); /* parse register expression */
            plccst(0x1f); /* place mask */
            codbuf[codinx++] = UEAND; /* place 'and' command */
            plccst(b); /* place bit shift */
            codbuf[codinx++] = UESHL; /* place shift command */
            codbuf[codinx++] = UEOR; /* place or command */
            first = FALSE; /* set not first */

         }
         b = b-5; /* set new bit equivalent */

      }

   }
   /* now search for the specials */
   for (i = 7; i < 11; i++) { 

      b = 21; /* set bit equivalent of high register */
      c = opctbl[f][i]; /* check special parameter */
      if ((c == 's')|(c == 'o')|(c == 'i')|(c == 'f')|(c == 'q')|(c == 'y')) {

         /* special parameter */
         if (!first) { /* check parameter separator */

            while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
            if (cmdlin[cmdinx] != ',') 
               error(ECMAEXP); /* missing ',' error */
            cmdinx++; /* skip ',' */

         }
         switch (opctbl[f][i]) { /* process the parameter */

            case 's': /* segment address */
               expr(); /* parse address expression */
               plccst(2); /* place shift count */
               codbuf[codinx++] = UESHR; /* place shift command */
               plccst(0x03ffffff); /* place mask */
               codbuf[codinx++] = UEAND; /* place 'and' command */
               codbuf[codinx++] = UEOR; /* place 'or' command */
               break;
            case 'z': break; /* requires no action */
            case 'o': /* pc offset */
               expr(); /* parse address expression */
               plccst(addr+1); /* place instruction address+1 */
               codbuf[codinx++] = UESUB; /* place subtract command */
               plccst(2); /* place shift count */
               codbuf[codinx++] = UESHR; /* place shift command */
               plccst(0xffff); /* place mask */
               codbuf[codinx++] = UEAND; /* place 'and' command */
               codbuf[codinx++] = UEOR; /* place 'or' command */
               break;
	        case 'i': /* immediate */
               expr(); /* parse immediate expression */
               plccst(0xffff); /* mask result */
               codbuf[codinx++] = UEAND; /* place 'and' command */
               codbuf[codinx++] = UEOR; /* place 'or' command */
               break;
            case 'f': /* coprocessor function */
               expr(); /* parse immediate expression */
               plccst(0x03ffffff); /* mask result */
               codbuf[codinx++] = UEAND; /* place 'and' command */
               codbuf[codinx++] = UEOR; /* place 'or' command */
               break;
            case 'q': /* base/offset */
               expr(); /* parse immediate expression */
               plccst(0xffff); /* mask result */
               codbuf[codinx++] = UEAND; /* place 'and' command */
               codbuf[codinx++] = UEOR; /* place or command */
               while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
               if (cmdlin[cmdinx] != '(')
                  error(ELPEXP); /* missing '(' error */
               cmdinx++; /* skip '(' */
               expr(); /* parse register expression */
               plccst(0x1f); /* place mask */
               codbuf[codinx++] = UEAND; /* place 'and' command */
               plccst(b); /* place bit shift */
               codbuf[codinx++] = UESHL; /* place shift command */
               codbuf[codinx++] = UEOR; /* place or command */
               while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
               if (cmdlin[cmdinx] != ')')
                  error(ERPEXP); /* missing ')' error */
               cmdinx++; /* skip ')' */
               break;
            case 'y': /* system call code */
               expr(); /* parse immediate expression */
               plccst(0x000fffff); /* mask result */
               codbuf[codinx++] = UEAND; /* place 'and' command */
               plccst(6); /* place shift count */
               codbuf[codinx++] = UESHL; /* place shift command */
               codbuf[codinx++] = UEOR; /* place or command */
               break;

         }

      }
      b = b-5; /* set new bit equivalent */

   }

}

/*******************************************************************************

Parse display registers/status

*******************************************************************************/

int pdisplay()

{

   codbuf[codinx++] = UEDISP; /* place display code */

   return 0;

}

/*******************************************************************************

Parse display control registers

*******************************************************************************/

int pdispcon()

{

   codbuf[codinx++] = UEDISPC; /* place display code */

   return 0;

}

/*******************************************************************************

Parse display float registers

*******************************************************************************/

int pdispflt()

{

   codbuf[codinx++] = UEDISPF; /* place display code */

   return 0;

}

/*******************************************************************************

Parse list instructions

*******************************************************************************/

int plist()

{

   expr(); /* parse starting expression */
   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   if (cmdlin[cmdinx] == ',') {

      cmdinx++; /* skip ',' */
      expr(); /* parse ending expression */   

   } else codbuf[codinx++] = UEDUP; /* otherwise just list single instruction */
   codbuf[codinx++] = UELIST; /* place list code */

}

/*******************************************************************************

Parse dump memory

*******************************************************************************/

int pdump()

{

   expr(); /* parse starting expression */
   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   if (cmdlin[cmdinx] == ',') { /* get ending expression */

      cmdinx++; /* skip ',' */
      expr(); /* parse ending expression */

   } else { /* dump standard range (256 bytes) */

      codbuf[codinx++] = UEDUP; /* duplicate stack top */
      plccst(255); /* set increment */
      codbuf[codinx++] = UEADD; /* add to second */
      
   }
   codbuf[codinx++] = UEDUMP; /* place list code */

}

/*******************************************************************************

Parse print values

*******************************************************************************/

int pprint()

{

   expr(); /* parse expression */
   codbuf[codinx++] = UEPRT; /* place print code */

}

/*******************************************************************************

Parse step

*******************************************************************************/

int pstep()

{

   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   /* set up parameter */
   if (cmdlin[cmdinx] != ';' && cmdlin[cmdinx]) expr(); else plccst(1);    
   codbuf[codinx++] = UESTEP; /* place step code */

}

/*******************************************************************************

Parse display step

*******************************************************************************/

int pdstep()

{

   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   /* set up parameter */
   if (cmdlin[cmdinx] != ';' && cmdlin[cmdinx]) expr(); else plccst(1);    
   codbuf[codinx++] = UEDSTEP; /* place display step code */

}

/*******************************************************************************

Parse special variables print

*******************************************************************************/

int pspec()

{

   codbuf[codinx++] = UESPEC; /* place display step code */

}

/*******************************************************************************

Parse set register

*******************************************************************************/

int psetreg()

{

   char lab[20]; /* command label */
   int r; /* address of register */
   int l; /* location of register */

   getlab(lab); /* get register label */
   if (fndreg(lab, &r, &l) < 0) error(EREGEXP);
   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   /* check ',' */
   if (cmdlin[cmdinx] != ',') error(ECMAEXP);
   cmdinx++; /* skip ',' */
   expr(); /* parse assignment expression */
   if (l) codbuf[codinx++] = UECRSET; /* set control register */
   else { /* normal/standard set */

      if (r == 0) error(EINVREG); /* cannot write r0 */
      codbuf[codinx++] = UERSET; /* set normal register */

   }
   codbuf[codinx++] = r; /* place address */

}

/*******************************************************************************

Parse set PC

*******************************************************************************/

int psetpc()

{

   expr(); /* parse value */
   codbuf[codinx++] = UESETPC; /* set pc */

}

/*******************************************************************************

Parse UDLink ASIC read

*******************************************************************************/

int pudlrd()

{

   expr(); /* parse register to read */
   codbuf[codinx++] = UEUDLRD; /* place register */

}

/*******************************************************************************

Parse UDLink ASIC write

*******************************************************************************/

int pudlwr()

{

   expr(); /* parse register to read */
   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   if (cmdlin[cmdinx] != ',') error(ECMAEXP);
   cmdinx++; /* skip ',' */
   expr(); /* parse ending expression */
   codbuf[codinx++] = UEUDLWR; /* place register */

}

/*******************************************************************************

Parse exit

*******************************************************************************/

int pexit()

{

   codbuf[codinx++] = UEEXIT; /* place exit code */

}

/*******************************************************************************

Parse load Motorola S record format

*******************************************************************************/

int pldsr()

{

   char lab[20]; /* command label */
   int i;        /* index for same */

   getlab(lab); /* get filename */
   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   if (cmdlin[cmdinx] == ',') { /* offset parameter exists */

      cmdinx++; /* skip ',' */
      expr(); /* parse offset expression */

   } else plccst(0); /* otherwise default to 0 */
   codbuf[codinx++] = UELDSR; /* load motorola S-record format */
   i = 0; /* set 1st filename character */
   /* place string after tolken */
   do { codbuf[codinx++] = lab[i]; } while (lab[i++]);
                                      
}

/*******************************************************************************

Assemble single instruction

Inserts a single instruction to the given address. The format is:

   as addr,opcode par,par...

The instruction is parsed an compile time, and the result is constructed (!)
at runtime and processed as a word insert to memory.

*******************************************************************************/

int passm()

{

   ul addr; /* assembly address */

   addr = getnum(); /* get address */
   plccst(addr); /* place address on stack */
   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   if (cmdlin[cmdinx] != ',') error(ECMAEXP);
   cmdinx++; /* skip ',' */
   assm(addr); /* parse instruction */
   codbuf[codinx++] = UEINST; /* insert memory */

}

/*******************************************************************************

Assemble instructions

Inputs a series of lines from the user and assembles them to the target
address. The format is:

   as addr

Then each line from the user contains an assembly line.

*******************************************************************************/

int passml()

{

   plccst(getnum()); /* place address on stack */
   codbuf[codinx++] = UEASSML; /* assemble lines */

}

/*******************************************************************************

Comment

Simply skips over the rest of the line.

*******************************************************************************/

int pcmmt()

{

   while (cmdlin[cmdinx]) cmdinx++;

}

/*******************************************************************************

Help

Prints a list of help subjects.

*******************************************************************************/

int phelp()

{

   codbuf[codinx++] = UEHELP; /* assemble lines */
   codbuf[codinx++] = 0; /* later, this will be a subject number */

}

/*******************************************************************************

Compile single line

Compiles a single line from the given input buffer to the given output
buffer.

*******************************************************************************/

void cmplin(char iline[], /* input line (ASCII) */
            char oline[]) /* output line (ICODE) */

{

   char lab[20]; /* command label */
   int (*fp)(); /* pointer to command function */
   int r; /* function result */

   codinx = 0; /* set encode index to start */
   while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
   if (cmdlin[cmdinx]) { /* line not null */

      getlab(lab); /* get command label */
      fp = fndfnc(lab); /* find in command table */
      if (fp) r = fp(); /* execute command */
      else error(ECMDNF); /* process command error */
      while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
      while (cmdlin[cmdinx] == ';') { /* multiple commands */

         cmdinx++; /* skip ';' */
         getlab(lab); /* get command label */
         fp = fndfnc(lab); /* find in command table */
         if (fp) r = fp(); /* execute command */
         else error(ECMDNF); /* process command error */
         while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */

      }

   }
   codbuf[codinx] = UELINE; /* place line terminator */

/* uncomment to get a line dump */

/*

{ int i; for (i = 0; i <= codinx; i++) printf("%2.2x ", (uc) codbuf[i]);
  printf("\n"); }

*/

}   

/*******************************************************************************

Print help list

*******************************************************************************/

int help(int sub)

{

   printf("\n");
   printf("Commands:\n");
   printf("\n");
   printf("reg/r              Display integer registers/instruction\n");
   printf("dispcon/dc         Display control (COP0) registers\n");
   printf("dispflt/df         Display floating point regsiters\n");
   printf("list/l start[,end] List single/multiple instructions\n");
   printf("print/p value      Print number\n");
   printf("dump/d start[,end] Dump words\n");
   printf("step/s             Step processor\n");
   printf("dstep/ds           Step and display registers\n");
   printf("spec               Print special variables (diagnostic)\n");
   printf("setr/sr reg,value  Set register\n");
   printf("udlr addr          Read UDLink register\n");
   printf("udlw addr,value    Write UDLink register\n");
   printf("exit               Exit to unix\n");
   printf("loadsr/ldsr\n");
   printf("assm/as addr,inst  Assemble instruction\n");
   printf("assml/al addr      Assemble multiple instructions\n");
   printf("comment/c          Comment remaining line\n");
   printf("\n");
   printf("For any value field:\n");
   printf("\n");
   printf("-a  Negate\n");
   printf("a*b Multiply\n");
   printf("a/b Divide\n");
   printf("a+b Add\n");
   printf("a-b Subtract\n");
   printf("\n");
   printf("()  Group operands\n");
   printf("$x  Specify number in hex\n");
   printf("\n");

}

/*******************************************************************************

Display registers and context

*******************************************************************************/

int display()

{

   int i; /* register counter */

   for (i = 0; i < 32; i++) { /* display the registers */

      printf("%2.2i:%16.16llx ", i, context.reg[i]);
      if ((i & 3) == 3) printf("\n");

   }
   printf("hi:%16.16llx lo:%16.16llx\n", context.hi, context.low);
   printf("\n");
   lstins(context.pc, 1);
   printf("\n");

}

/*******************************************************************************

Display control registers

*******************************************************************************/

int dispcon()

{

   printf("ibase:    %8.8llx ", context.cp0[0]);
   printf("ibound:   %8.8llx ", context.cp0[1]);
   printf("dbase:    %8.8llx ", context.cp0[2]);
   printf("dbound:   %8.8llx\n", context.cp0[3]);
   printf("badvaddr: %8.8llx ", context.cp0[8]);
   printf("count:    %8.8llx ", context.cp0[9]);
   printf("compare:  %8.8llx ", context.cp0[11]);
   printf("sr:       %8.8llx\n", context.cp0[12]);
   printf("cause:    %8.8llx ", context.cp0[13]);
   printf("prld:     %8.8llx ", context.cp0[15]);
   printf("config:   %8.8llx ", context.cp0[16]);
   printf("calg:     %8.8llx\n", context.cp0[17]);
   printf("iwatch:   %8.8llx ", context.cp0[18]);
   printf("dwatch:   %8.8llx ", context.cp0[19]);
   printf("ecc:      %8.8llx ", context.cp0[26]);
   printf("cacheerr: %8.8llx\n", context.cp0[27]);
   printf("taglo:    %8.8llx\n", context.cp0[28]);
   printf("epc:      %8.8llx\n", context.cp0[14]);
   printf("errorepc: %8.8llx\n", context.cp0[30]);
 
}

/*******************************************************************************

Display float registers

This needs to output in fixed point.

*******************************************************************************/

int dispflt()

{

   int i; /* index for float registers */

   for (i = 0; i < 32; i++) { 

      printf("%2.2i:%f ", i, context.fgr[i]);
      if ((i & 3) == 3) printf("\n");

   }
   printf("fcr0: %8.8x fcr31: %8.8x\n", context.fcr[0], context.fcr[31]);

}

/*******************************************************************************

Display special variables

*******************************************************************************/

void spec()

{

   printf("Slot:            %llx\n", context.slot);
   printf("Single cycle DI: %i\n", context.noint);

}

/*******************************************************************************
 
Load motorola S record file

Loads the specified file into memory. Only data records are loaded from the
file, and other records are discarded. The checksum is discarded unchecked,
and all lines that don't begin with "S" are simply skipped.
The contents of each data line is placed at the address the line indicates.

*******************************************************************************/

void loadsrec(char lab[], ll offset)

{

   FILE *sfile; /* source file */
   int c; /* incoming file character */
   ul addr; /* placement address */
   int ln; /* data length */
   int h, l; /* read buffers */
   ul b; /* byte construction buffer */
   int i; /* index */
   int bc; /* byte count */

   strcat(lab, ".srec"); /* add extention to file */
   printf("Loading: %s\n", lab);
   /* open and error file */
   if (!(sfile = fopen(lab, "r"))) error(EOPNFIL);
   c = getc(sfile); /* get 1st file character */
   while (c != EOF) { /* read lines */

      if (c == 'S') { /* process S-record */

         c = getc(sfile); /* get type code */
         h = getc(sfile); /* get length components */
         /* convert from hex */
         isalpha(h) ? h = tolower(h)-'a'+10: (h -= '0');
         l = getc(sfile);
         isalpha(l) ? l = tolower(l)-'a'+10: (l -= '0');
         ln = h*16+l; /* construct length */
         /* comment this out if you don't want to see the header */
         if (0 /* c == '0' */) {

            c = getc(sfile); /* skip address */
            c = getc(sfile);
            c = getc(sfile);
            c = getc(sfile);
            /* print the header */
            for (i = 0; i < ln-2-1; i++) {

               h = getc(sfile); /* get byte components */
               /* convert from hex */
               isalpha(h) ? h = tolower(h)-'a'+10: (h -= '0');
               l = getc(sfile);
               isalpha(l) ? l = tolower(l)-'a'+10: (l -= '0');
               b = h*16+l; /* construct byte */
               putchar(b); /* output character */

            }
            putchar('\n'); /* next line */

         }
         if ((c >= '1') && (c <= '3')) { /* it's a data record */

            /* read and construct address */
            addr = 0; /* clear address */
            bc = (c-'0')*2+2; /* find the number of digits */
            for (i = 0; i < bc; i++) {

               c = getc(sfile); /* get next character */
               /* scale and add new digit */
               isalpha(c) ? addr = addr*16+tolower(c)-'a'+10:
                            (addr = addr*16+c-'0');

            }
            ln -= bc/2+1; /* adjust length to actual bytes */
            addr += offset; /* adjust by offset */
            for (i = 0; i < ln; i++) { /* read and place bytes in memory */

               /* check and flag beyond end of memory */
               if (addr >= MAXMEM) error(EMEMRNG);
               h = getc(sfile); /* get byte components */
               /* convert from hex */
               isalpha(h) ? h = tolower(h)-'a'+10: (h -= '0');
               l = getc(sfile);
               isalpha(l) ? l = tolower(l)-'a'+10: (l -= '0');
               b = h*16+l; /* construct byte */
               /* now merge that with memory word */
               b <<= (3-addr%4)*8; /* shift byte into place */
               /* combine and place */
               context.mem[addr/4] = context.mem[addr/4] & 
                                     0xffffffff00ffffffull >> addr%4*8 | b;
               addr++; /* next byte address */

            }

         }

      }
      /* skip any remaining line */
      while ((c != '\n') && (c != EOF)) c = getc(sfile);
      c = getc(sfile); /* get next character */

   }
   fclose(sfile); /* close the file */

}

/*******************************************************************************

Assemble lines

Prompts for, and assembles a sequence of lines to a given address. Because we
need access to the line compiler, we save both input and output buffers for
this operation.

*******************************************************************************/

int assml(ul addr) /* address to assemble to */

{

   char cmdlins[MAXCMD]; /* user command line save */
   int  cmdinxs;         /* index for command line save */
   char codbufs[MAXCOD]; /* encode run buffer save */
   int  codinxs;         /* index for code buffer save */
   int  i;               /* copy index */
   int  done;            /* input complete flag */
   
   /* save off buffers */
   for (i = 0; i < MAXCMD; i++) cmdlins[i] = cmdlin[i];
   cmdinxs = cmdinx;
   for (i = 0; i < MAXCOD; i++) codbufs[i] = codbuf[i];
   codinxs = codinx;
   done = FALSE; /* set user input not complete */
   do { /* input user lines */

      lstins(addr, 50); /* list present contents of location */
      printf(" -> "); /* print user prompt */
      getlin(stdin, cmdlin); /* get user line */
      codinx = 0; /* set encode index to start */
      while (cmdlin[cmdinx] == ' ') cmdinx++; /* skip any spaces */
      if (cmdlin[cmdinx]) { /* line not null */

         plccst(addr); /* place address on stack */
         assm(addr); /* parse instruction */
         codbuf[codinx++] = UEINST; /* insert memory */
         codbuf[codinx] = UELINE; /* place line terminator */
         exclin(codbuf); /* execute that */
         addr += 4; /* next address */

      } else done = TRUE; /* set user input complete */

   } while (!done); /* until user flags done */
   /* restore buffers */
   for (i = 0; i < MAXCMD; i++) cmdlin[i] = cmdlins[i];
   cmdinx = cmdinxs;
   for (i = 0; i < MAXCOD; i++) codbuf[i] = codbufs[i];
   codinx = codinxs;

}

/*******************************************************************************

Interpret line

Executes a given encode line

*******************************************************************************/

int exclin(char line[])

{

   int i;        /* code index */
   int x;        /* general index */
   ull al, bl;   /* variables */
   evtptr ep;    /* event pointer */
   char lab[20]; /* label */

   i = 0; /* set 1st code in line */
   stkptr = 0; /* reset stack */
   while (line[i] != UELINE) { /* execute tolkens */

      switch (line[i]) {

         case UEDISP: /* display registers and context */
            i++; /* skip command tolken */
            display(); /* perform display */
            break;
         case UEDISPC: /* display control registers */
            i++; /* skip command tolken */
            dispcon(); /* perform display */
            break; 
         case UEDISPF: /* display floating point registers */
            i++; /* skip commmand tolken */
            dispflt(); /* perfomm display */
            break;
         case UELIST: /* list instructions */
            i++; /* skip command tolken */
            bl = stack[--stkptr]; /* get ending value */
            al = stack[--stkptr]; /* get starting value */
            /* list instructions */
            while (al <= bl) { lstins(al, 1); al += 4; putchar('\n'); }
            break;
         case UEDUMP: /* dump memory */
            i++; /* skip command tolken */
            bl = stack[--stkptr]; /* get ending value */
            al = stack[--stkptr]; /* get starting value */
            dump(al, bl); /* perform dump */
            break;
         case UELCST: /* load 64 bit constant */
            i++; /* skip command tolken */
            /* load constant */
            for (x = 0; x < 8; x++) al = (al<<8)|(uc)line[i++];
            stack[stkptr++] = al; /* place result */
            break;
         case UEPRT: /* print value */
            i++; /* skip command tolken */
            printf("%lli\n", stack[--stkptr]);
            break;
         case UEEQU: /* test equal */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] == al; /* place result */
            break;
         case UENEQU: /* test not equal */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] != al; /* place result */
            break;
         case UELTN: /* test less than */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] < al; /* place result */
            break;
         case UEGTN: /* test greater than */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] > al; /* place result */
            break;
         case UELEQU: /* test less than or equal */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] <= al; /* place result */
            break;
         case UEGEQU: /* test greater than or equal */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] >= al; /* place result */
            break;
         case UEADD: /* add */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] + al; /* place result */
            break;
         case UESUB: /* subtract */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] - al; /* place result */
            break;
         case UEMULT: /* multiply */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] * al; /* place result */
            break;
         case UEDIV: /* divide */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] / al; /* place result */
            break;
         case UEMOD: /* modulo */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] % al; /* place result */
            break;
         case UENEG: /* negate */
            i++; /* skip command tolken */
            stack[stkptr-1] = -stack[stkptr-1]; /* place result */
            break;                                             
         case UESHL: /* shift left */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] << al; /* place result */
            break;
         case UESHR: /* shift right */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] >> al; /* place result */
            break;
         case UEOR: /* 'or' bits */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] | al; /* place result */
            break;
         case UEAND: /* 'and' bits */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get right side */
            stack[stkptr-1] = stack[stkptr-1] & al; /* place result */
            break;
         case UESTEP: /* step processor */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get step count */
            while (al-- > 0) stpins(); /* step processor that many times */
            break;
         case UEDSTEP: /* display step processor */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get step count */
            /* step processor with display */
            while (al-- > 0) { stpins(); display(); }
            break;
         case UESPEC: /* display step processor */
            i++; /* skip command tolken */
            spec(); /* print special variables */
            break;
         case UERSET: /* set register contents */
            i++; /* skip command tolken */
            /* place contents of register */
            context.reg[line[i++]] = stack[--stkptr];
            break;
         case UECRSET: /* set register contents */
            i++; /* skip command tolken */
            /* place contents of register */
            context.cp0[line[i++]] = stack[--stkptr];
            break;
         case UESETPC: /* set PC contents */
            i++; /* skip command tolken */
            /* place contents of pc */
            context.pc = stack[--stkptr];
            context.slot = context.pc+4; /* reset slot */
            break;
         case UEUDLRD: /* read UDLink ASIC */
            i++; /* skip command tolken */
            cpuadr = stack[--stkptr]; /* set address to read */
            ep = getevt(ep); /* get event record */
            ep->func = udlink; /* place handler */
            ep->reason = EVREAD; /* place reason */
            insevt(ep); /* insert to active */
            exeact(); /* run cycle */
            printf("%16.16lli\n", cpudat);
            break;
         case UEUDLWR: /* write UDLink ASIC */
            i++; /* skip command tolken */
            cpudat = stack[--stkptr]; /* set data to write */
            cpuadr = stack[--stkptr]; /* set address to read */
            ep = getevt(); /* get event record */
            ep->func = udlink; /* place handler */
            ep->reason = EVWRITE; /* place reason */
            insevt(ep); /* insert to active */
            exeact(); /* run cycle */
            break;
         case UEEXIT: exit(0); /* exit simulator */
         case UELDSR: /* read motorola S-record file */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get load offset */
            x = 0; /* set 1st character */
            /* transfer filename to buffer */
            do { lab[x++] = line[i]; } while (line[i++]);
            loadsrec(lab, al); /* process load */
            break;
         case UEINST: /* insert word to memory */
            i++; /* skip command tolken */
            al = stack[--stkptr]; /* get data */
            bl = stack[--stkptr]; /* get address */
            context.mem[bl/4] = al; /* place word to address */
            break;
         case UEASSML: /* assemble lines */
            i++; /* skip command tolken */
            assml(stack[--stkptr]); /* assemble with address */
            break;
         case UEDUP: /* duplicate stack top */
            i++; /* skip command tolken */
            al = stack[stkptr-1]; /* get top of stack */
            stack[stkptr++] = al; /* place copy */
            break;
         case UEHELP: /* print help */
            i++; /* skip command tolken */
            help(line[i++]); /* print help */
            break;
         default: error(EBADEXC); /* bad code */

      }

   }

}

/*******************************************************************************

UDlink ASIC event handler

*******************************************************************************/

uc udlreg[UDLLEN]; /* register emulation array */

void udlink(int reason)

{

   if (reason == EVREAD) cpudat = udlreg[cpuadr];
   else udlreg[cpuadr] = cpudat;

}

/*******************************************************************************

Main program

Initalize globals, then accept user commands.

*******************************************************************************/

void main()

{

   int i;
   evtptr ep;

   printf("Milan model simulator vs. 1.0\n");
   printf("\n");

   /* place the default diagnostic files */
   diagfil[0] = stdin; /* standard input */
   diagfil[1] = stdout; /* standard output */
   diagfil[2] = stderr; /* standard error */
   context.slot = 4; /* set up the default pipeline address */
   stpexc = TRUE; /* stop exceptions */
   do { /* process command lines */

      setjmp(errenv); /* place error continuation */
      printf("> ");
      getlin(stdin, cmdlin); /* get user command line */
      cmplin(cmdlin, codbuf); /* compile line */
      exclin(codbuf); /* execute */

   } while (FOREVER);

}

{*******************************************************************************
*                                                                              *
*                           I8080 SIMULATOR MODULE                             *
*                                                                              *
* This is the central access interface to the I8080 CPU. Here, we simulate the *
* CPU, but this interface can also be used to connect to a live target, either *
* remotely, on another partition, or in local memory.                          *
*                                                                              *
* Access routines:                                                             *
*                                                                              *
* function getreg(reg: regtyp): integer;                                       *
*                                                                              *
* Get the current value of a register. The reg is a code for the register,     *
* defined in defi8080.pas. Note that flags are in a register, as are special   *
* CPU features.                                                                *
*                                                                              *
* procedure putreg(reg: regtyp; data: integer);                                * 
*                                                                              *
* Put the value of a register.                                                 *
*                                                                              *
* function getmem(addr: integer; len: integer): integer;                       *
*                                                                              *
* Get a value from memory.                                                     *
*                                                                              *
* procedure putmem(addr: integer; data: integer; len: integer);                *
*                                                                              *
* Put a byte of memory.                                                        *
*                                                                              *
* function getio(addr: integer; len: integer): byte;                           *
*                                                                              *
* Get a value from an I/O port.                                                *
*                                                                              *
* procedure putio(addr: integer; data: byte; len: integer);                    *
*                                                                              *
* Put a value to an I/O port.                                                  *
*                                                                              *
* procedure exeins;                                                            *
*                                                                              *
* Execute a single instruction at the current program counter.                 *
*                                                                              *
* procedure reserve(r: rsvtyp; base: integer; len: integer;                    *
*                   procedure event(e: evttyp; var insaddr: integer;           *
*                                   accaddr: integer; var data: integer;       *
*                                   len: integer));                            *
*                                                                              *
* Reserves a memory, I/O invalid instruction or other resource for simulation  *
* use.                                                                         *
*                                                                              *
*******************************************************************************}

module simi8080;

uses stddef,   { standard defines }
     strlib,   { string library }
     extlib,   { extention library }
     dbdef,    { db general definitions }
     defi8080, { I8080 specific definitions }
     event,    { events coupler }
     db;       { db main module }

function getreg(reg: regtyp): integer; forward;
procedure putreg(reg: regtyp; data: integer); forward;
function getmem(addr: integer; len: integer): byte; forward;
procedure putmem(addr: integer; data: byte; len: integer); forward;
function getio(addr: integer; len: integer): integer; forward;
procedure putio(addr: integer; data: integer; len: integer); forward;
procedure exeins; forward;
procedure reserve(r: rsvtyp; base: integer; len: integer; 
                  procedure event(e: evttyp; var insaddr: integer; 
                                  accaddr: integer; var data: integer; 
                                  len: integer)); forward;

private

var

   memory:  array [0..maxmem] of byte; { processor simulated memory }
   cpu:     record { 8080 CPU context }

      pc:  word;    { register pc }
      sp:  word;    { register sp }
      ie:  boolean; { interrupt enable flag }
      case boolean of { word/byte access }

         { here we are using an undiscriminated union to gain two different
           viewpoints of each register. this is dependent on a little endian
           processor, and must be changed to reside on a big endian processor }
         true:  (af, bc, de, hl: word); { word access }
         false: (fz: boolean; { zero flag }
                 fc: boolean; { carry flag }
                 fa: boolean; { auxiliary carry flag }
                 fs: boolean; { sign flag }
                 fp: boolean; { parity flag }
                 f5: boolean; { extra flag bit 5 }
                 f3: boolean; { extra flag bit 3 }
                 f1: boolean; { extra flag bit 1 }
                 a:  byte;    { register a }
                 c:  byte;    { register c }
                 b:  byte;    { register b }
                 e:  byte;    { register e }
                 d:  byte;    { register d }
                 l:  byte;    { register l }
                 h:  byte)    { register h }

      { end }

   end;   
   parity:    array [0..255] of boolean; { parity odd/even table }
   addr:      integer; { address for memory }
   outhan:    array [0..255] of evthan; { out event handlers }
   inphan:    array [0..255] of evthan; { in event handlers }
   invi08han: evthan; { invalid instruction $08 handler }
   invi10han: evthan; { invalid instruction $10 handler }
   invi18han: evthan; { invalid instruction $18 handler }
   invi20han: evthan; { invalid instruction $20 handler }
   invi28han: evthan; { invalid instruction $28 handler }
   invi30han: evthan; { invalid instruction $30 handler }
   invi38han: evthan; { invalid instruction $38 handler }
   invicbhan: evthan; { invalid instruction $cb handler }
   invid9han: evthan; { invalid instruction $d9 handler }
   inviddhan: evthan; { invalid instruction $dd handler }
   inviedhan: evthan; { invalid instruction $ed handler }
   invifdhan: evthan; { invalid instruction $fd handler }
   halthan:   evthan; { halt instruction handler }
   pi, t, c, bi: integer;

{*******************************************************************************

Read register

Reads a CPU register.

*******************************************************************************}

function getreg(reg: regtyp): integer;

var r: integer; { register value }

begin

   case reg of { register }

      rega:  r := cpu.a;       { a }
      regb:  r := cpu.b;       { b }
      regc:  r := cpu.c;       { c }
      regd:  r := cpu.d;       { d }
      rege:  r := cpu.e;       { e }
      regh:  r := cpu.h;       { h }
      regl:  r := cpu.l;       { l }
      regpc: r := cpu.pc;      { pc }
      regsp: r := cpu.sp;      { sp }
      regfz: r := ord(cpu.fz); { zero flag }
      regfc: r := ord(cpu.fc); { carry flag }
      regfa: r := ord(cpu.fa); { auxiliary carry flag }
      regfs: r := ord(cpu.fs); { sign flag }
      regfp: r := ord(cpu.fp); { parity flag }
      regf5: r := ord(cpu.f5); { bit 5 of flags register }
      regf3: r := ord(cpu.f3); { bit 3 of flags register }
      regf1: r := ord(cpu.f1); { bit 1 of flags register }
      { interrupt enable is not actually accessable on a real machine }
      regie: r := ord(cpu.ie); { interrupt enable flag }

   end;

   getreg := r { return register }

end;

{*******************************************************************************

Write register

Write a CPU register.

*******************************************************************************}

procedure putreg(reg: regtyp; data: integer);

begin

   case reg of { register }

      rega:  cpu.a := data;       { a }
      regb:  cpu.b := data;       { b }
      regc:  cpu.c := data;       { c }
      regd:  cpu.d := data;       { d }
      rege:  cpu.e := data;       { e }
      regh:  cpu.h := data;       { h }
      regl:  cpu.l := data;       { l }
      regpc: cpu.pc := data;      { pc }
      regsp: cpu.sp := data;      { sp }
      regfz: cpu.fz := data <> 0; { zero flag }
      regfc: cpu.fc := data <> 0; { carry flag }
      regfa: cpu.fa := data <> 0; { auxiliary carry flag }
      regfs: cpu.fs := data <> 0; { sign flag }
      regfp: cpu.fp := data <> 0; { parity flag }
      regf5: cpu.f5 := data <> 0; { bit 5 of flags register }
      regf3: cpu.f3 := data <> 0; { bit 3 of flags register }
      regf1: cpu.f1 := data <> 0; { bit 1 of flags register }
      { interrupt enable is not actually accessable on a real machine }
      regie: cpu.ie  := data <> 0; { interrupt enable flag }

   end

end;

{*******************************************************************************

Read memory location

Reads a single memory location. Accepts a length, which is usually one of 1,
2, 4, or 8. Typically, we try to perform the operation using a bus operation
that matches the size. However, this routine will try to perform whatever
length is requested.

*******************************************************************************}

function getmem(addr: integer; { address to access }
                len:  integer) { length of transfer }
                : byte;        { byte read }

var v: integer; { value to read }

begin

   if len = 1 then v := memory[addr] { get memory location }
   else if len = 2 then v := memory[addr]+memory[addr+1]*$100 { 16 bit }
   else if len = 4 then { 32 bit }
      v := memory[addr]+memory[addr+1]*$100+memory[addr+2]*$10000+
           memory[addr+3]*$1000000
   else prterr(einvacl); { bad length }

   getmem := v { return value }

end;

{*******************************************************************************

Write memory location

Writes a single memory location.

*******************************************************************************}

procedure putmem(addr: integer;  { address to access }
                 data: byte;     { byte written }
                 len:  integer); { length to write }

begin

   if len = 1 then memory[addr] := data and $ff { set memory location }
   else if len = 2 then begin { set 16 bits }

      memory[addr] := data and $ff; { low byte }
      memory[addr+1] := data div $100 and $ff { high byte }

   end else if len = 3 then begin { set 32 bits }

      memory[addr] := data and $ff; { low byte }
      memory[addr+1] := data div $100 and $ff; { mid low byte }
      memory[addr+2] := data div $10000 and $ff; { mid high byte }
      memory[addr+3] := data div $1000000 and $ff { high byte }

   end else prterr(einvacl) { bad length }

end;

{*******************************************************************************

Read I/O location

Reads a single I/O port address. If the machine does not implement a separate
I/O address system, this causes an error.

This routine is only used by the I/O commands in the monitor.

*******************************************************************************}

function getio(addr: integer; { address to access }
               len:  integer) { length of operation }
                : integer;    { byte read }

var insadr: integer; { instruction carrier for events }
    data:   integer; { data carrier for events }

begin

   { Because there is no real I/O in the system, we get the result directly from
     an I/O event. There is no instruction associated with it, so this is just
     zero. }
   if len <> 1 then prterr(einvacl); { invalid length }
   insadr := 0; { set no PC active }
   exeevt(etioread, insadr, addr, data, 1, inphan[addr]);

   getio := data { return data }

end;

{*******************************************************************************

Write I/O location

Writes a single I/O port address. If the machine does not implement a separate
I/O address system, this causes an error.

This routine is only used by the I/O commands in the monitor.

*******************************************************************************}

procedure putio(addr: integer;  { address to access }
                data: integer;  { data written }
                len:  integer); { length of operation }

var insadr: integer; { instruction carrier for events }

begin

   { Because there is no real I/O in the system, we get the result directly from
     an I/O event. There is no instruction associated with it, so this is just
     zero. }
   if len <> 1 then prterr(einvacl); { invalid length }
   insadr := 0; { set no PC active }
   exeevt(etiowrite, insadr, addr, data, 1, inphan[addr])

end;

{*******************************************************************************

Execute 8080 CPU instruction

Executes a single instruction at the current 8080 PC address. All changes are
reflected in the CPU context record.

Instruction emulation notes:

1. The undefined bits in the 8080 flag byte are stored and restored in the
"push psw" and "pop psw" instructions intact. The Intel manuals imply they
are fixed bits, but experience has shown that the intact model is correct.
This should be checked again against actual machines.

2. The action for illegal instructions is to output an error message, but
leave the pc pointing at the illegal instruction. This way, the user may not
pass an illegal instruction.

3. The action for the "halt" instruction is identical to an illegal
instruction, outputting a message and leaving the PC pointing at the halt
instruction. This is (without the message) identical to what the real CPU
does.

4. Since HLL does not allow for parity calculation (indeed, most main CPU's
don't do this anymore), the parity check is done via a lookup table.

5. Since registers are kept as bytes and not words, word arithmetic is
performed by concatenating byte registers. This is a trade off against having
to unpack byte registers on byte operations.

This routine is the portable version. On a given machine, it is customary to
create an assembly language routine that does this job more efficently. For
that purpose, this routine can serve as a guide. Still, the existance of a
portable version allows quick porting to new machines, and may give enough
speed to handle many functions adequately (such as remote stepping).

The CPU simulation routine is packed in a separate module to allow it to be
compiled with bounds checking off. This allows us to take advantage of lack
of overflow checking.

*******************************************************************************}

procedure exeins;

var i:       integer; { instruction byte }
    t, t1:   integer; { holding }
    ts, ts1: byte;    { byte holding }
    ta:      word;    { address holding }
    tb:      boolean; { holding }
    data:    integer; { data carrier for events }
    insadr:  integer; { instruction carrier for events }
    accadr:  integer; { access address carrier for events }

begin

   i := memory[cpu.pc]; { get instruction byte }
   cpu.pc := cpu.pc+1; { advance next byte }
   case i of { instruction }

      $00: ; { nop }
      $01: begin cpu.c := memory[cpu.pc]; cpu.pc := cpu.pc+1;
                 cpu.b := memory[cpu.pc]; cpu.pc := cpu.pc+1 end;
      $02: memory[cpu.bc] := cpu.a;
      $03: cpu.bc := cpu.bc+1;
      $04: begin t := cpu.b; cpu.b := cpu.b+1; cpu.fz := cpu.b = 0;
                 cpu.fs := cpu.b > 127; cpu.fp := parity[cpu.b]; 
                 cpu.fa := (t and $10) <> (cpu.b and $10) end;
      $05: begin t := cpu.b; cpu.b := cpu.b-1; if cpu.b < 0 then cpu.b := $ff;
                 cpu.fz := cpu.b = 0; cpu.fs := cpu.b > 127; 
                 cpu.fp := parity[cpu.b]; 
                 cpu.fa := (t and $10) <> (cpu.b and $10) end;
      $06: begin cpu.b := memory[cpu.pc]; cpu.pc := cpu.pc+1 end;
      $07: begin cpu.fc := cpu.a > 127; cpu.a := cpu.a*2; 
                 if cpu.fc then cpu.a := cpu.a+1 end;
      $08: begin insadr := cpu.pc-1; 
                 exeevt(etinvi08, insadr, insadr, data, 1, invi08han);
                 cpu.pc := insadr;
           end;
      $09: begin t := cpu.hl+cpu.bc; cpu.hl := t; cpu.fc := t > 65535 end;
      $0a: cpu.a := memory[cpu.bc];
      $0b: cpu.bc := cpu.bc-1;
      $0c: begin t := cpu.c; cpu.c := cpu.c+1; cpu.fz := cpu.c = 0;
                 cpu.fs := cpu.c > 127; cpu.fp := parity[cpu.c];
                 cpu.fa := (t and $10) <> (cpu.c and $10) end;
      $0d: begin t := cpu.c; cpu.c := cpu.c-1; if cpu.c < 0 then cpu.c := $ff;
                 cpu.fz := cpu.c = 0; cpu.fs := cpu.c > 127;
                 cpu.fp := parity[cpu.c];
                 cpu.fa := (t and $10) <> (cpu.c and $10) end;
      $0e: begin cpu.c := memory[cpu.pc]; cpu.pc := cpu.pc+1 end;
      $0f: begin cpu.fc := odd(cpu.a); cpu.a := cpu.a div 2;
                 if cpu.fc then cpu.a := cpu.a+128 end;
      $10: begin insadr := cpu.pc-1; 
                 exeevt(etinvi10, insadr, insadr, data, 1, invi10han);
                 cpu.pc := insadr;
           end;
      $11: begin cpu.e := memory[cpu.pc]; cpu.pc := cpu.pc+1;
                 cpu.d := memory[cpu.pc]; cpu.pc := cpu.pc+1 end;
      $12: memory[cpu.de] := cpu.a;
      $13: cpu.de := cpu.de+1;
      $14: begin t := cpu.d; cpu.d := cpu.d+1; cpu.fz := cpu.d = 0;
                 cpu.fs := cpu.d > 127; cpu.fp := parity[cpu.d]; 
                 cpu.fa := (t and $10) <> (cpu.d and $10) end;
      $15: begin t := cpu.d; cpu.d := cpu.d-1; cpu.fz := cpu.d = 0;
                 cpu.fs := cpu.d > 127; cpu.fp := parity[cpu.d]; 
                 cpu.fa := (t and $10) <> (cpu.d and $10) end;
      $16: begin cpu.d := memory[cpu.pc]; cpu.pc := cpu.pc+1 end;
      $17: begin tb := cpu.fc; cpu.fc := cpu.a > 127;
                 cpu.a := cpu.a*2; if tb then cpu.a := cpu.a+1 end;
      $18: begin insadr := cpu.pc-1; 
                 exeevt(etinvi18, insadr, insadr, data, 1, invi18han);
                 cpu.pc := insadr;
           end;
      $19: begin t := cpu.hl+cpu.de; cpu.hl := t; cpu.fc := t > 65535 end;
      $1a: cpu.a := memory[cpu.de];
      $1b: cpu.de := cpu.de-1;
      $1c: begin t := cpu.e; cpu.e := cpu.e+1; cpu.fz := cpu.e = 0;
                 cpu.fs := cpu.e > 127; cpu.fp := parity[cpu.e]; 
                 cpu.fa := (t and $10) <> (cpu.e and $10) end;
      $1d: begin t := cpu.e; cpu.e := cpu.e-1; cpu.fz := cpu.e = 0;
                 cpu.fs := cpu.e > 127; cpu.fp := parity[cpu.e];
                 cpu.fa := (t and $10) <> (cpu.e and $10) end;
      $1e: begin cpu.e := memory[cpu.pc]; cpu.pc := cpu.pc+1 end;
      $1f: begin tb := cpu.fc; cpu.fc := odd(cpu.a); cpu.a := cpu.a div 2;
                 if tb then cpu.a := cpu.a+128 end;
      $20: begin insadr := cpu.pc-1; 
                 exeevt(etinvi20, insadr, insadr, data, 1, invi20han);
                 cpu.pc := insadr;
           end;
      $21: begin cpu.l := memory[cpu.pc]; cpu.pc := cpu.pc+1; 
                 cpu.h := memory[cpu.pc]; cpu.pc := cpu.pc+1 end;
      $22: begin t := memory[cpu.pc]+memory[cpu.pc+1]*256;
                 cpu.pc := cpu.pc+2; memory[t] := cpu.l;
                 memory[t+1] := cpu.h end;
      $23: cpu.hl := cpu.hl+1;
      $24: begin t := cpu.h; cpu.h := cpu.h+1; cpu.fz := cpu.h = 0;
                 cpu.fs := cpu.h > 127; cpu.fp := parity[cpu.h];
                 cpu.fa := (t and $10) <> (cpu.h and $10) end;
      $25: begin t := cpu.h; cpu.h := cpu.h-1; cpu.fz := cpu.h = 0;
                 cpu.fs := cpu.h > 127; cpu.fp := parity[cpu.h];
                 cpu.fa := (t and $10) <> (cpu.h and $10) end;
      $26: begin cpu.h := memory[cpu.pc]; cpu.pc := cpu.pc+1 end;
      $27: begin t := cpu.a; if ((cpu.a and $0f) > $09) or cpu.fa then
                    cpu.a := cpu.a+$06;
                 if ((cpu.a and $f0) > $90) or cpu.fc then
                    cpu.a := cpu.a+$60; 
                 cpu.fa := (t and $10) <> (cpu.a and $10);
                 cpu.fc := (t and $80) > (cpu.a and $80) end;
      $28: begin insadr := cpu.pc-1; 
                 exeevt(etinvi28, insadr, insadr, data, 1, invi28han);
                 cpu.pc := insadr;
           end;
      $29: begin t := cpu.hl+cpu.hl; cpu.hl := t; cpu.fc := t > 65535 end;
      $2a: begin t := memory[cpu.pc]+memory[cpu.pc+1]*256; cpu.pc := cpu.pc+2;
                 cpu.l := memory[t]; cpu.h := memory[t+1] end;
      $2b: cpu.hl := cpu.hl-1;
      $2c: begin t := cpu.l; cpu.l := cpu.l+1; cpu.fz := cpu.l = 0;
                 cpu.fs := cpu.l > 127; cpu.fp := parity[cpu.l]; 
                 cpu.fa := (t and $10) <> (cpu.l and $10) end;
      $2d: begin t := cpu.l; cpu.l := cpu.l-1; cpu.fz := cpu.l = 0;
                 cpu.fs := cpu.l > 127; cpu.fp := parity[cpu.l]; 
                 cpu.fa := (t and $10) <> (cpu.l and $10) end;
      $2e: begin cpu.l := memory[cpu.pc]; cpu.pc := cpu.pc+1 end;
      $2f: cpu.a := not cpu.a;
      $30: begin insadr := cpu.pc-1; 
                 exeevt(etinvi30, insadr, insadr, data, 1, invi30han);
                 cpu.pc := insadr;
           end;
      $31: begin cpu.sp := memory[cpu.pc]+memory[cpu.pc]*256; 
                 cpu.pc := cpu.pc+2 end;
      $32: begin t := memory[cpu.pc]+memory[cpu.pc+1]*256; cpu.pc := cpu.pc+2;
                 memory[t] := cpu.a end;
      $33: cpu.sp := cpu.sp+1; 
      $34: begin ts := memory[cpu.hl]; ts1 := ts+1; memory[ta] := ts1;
                 cpu.fz := ts1 = 0; cpu.fs := ts1 > 127; cpu.fp := parity[ts1];
                 cpu.fa := (ts and $10) <> (ts1 and $10) end;
      $35: begin ts := memory[cpu.hl]; ts1 := ts-1; memory[ta] := ts1;
                 cpu.fz := ts1 = 0; cpu.fs := ts1 > 127; cpu.fp := parity[ts1];
                 cpu.fa := (ts and $10) <> (ts1 and $10) end;
      $36: begin memory[cpu.hl] := memory[cpu.pc]; cpu.pc := cpu.pc+1 end;
      $37: cpu.fc := true;
      $38: begin insadr := cpu.pc-1; 
                 exeevt(etinvi38, insadr, insadr, data, 1, invi38han);
                 cpu.pc := insadr;
           end;
      $39: begin t := cpu.hl+cpu.sp; cpu.hl := t; cpu.fc := t > 65535 end;
      $3a: begin cpu.a := memory[memory[cpu.pc]+memory[cpu.pc+1]*256]; 
                 cpu.pc := cpu.pc+2 end;
      $3b: cpu.sp := cpu.sp-1;
      $3c: begin t := cpu.a; cpu.a := cpu.a+1; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fa := (t and $10) <> (cpu.a and $10) end;
      $3d: begin t := cpu.a; cpu.a := cpu.a-1; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fa := (t and $10) <> (cpu.a and $10) end;
      $3e: begin cpu.a := memory[cpu.pc]; cpu.pc := cpu.pc+1 end;
      $3f: cpu.fc := not cpu.fc;
      $40: cpu.b := cpu.b;
      $41: cpu.b := cpu.c;
      $42: cpu.b := cpu.d;
      $43: cpu.b := cpu.e;
      $44: cpu.b := cpu.h;
      $45: cpu.b := cpu.l;
      $46: cpu.b := memory[cpu.hl];
      $47: cpu.b := cpu.a;
      $48: cpu.c := cpu.b;
      $49: cpu.c := cpu.c;
      $4a: cpu.c := cpu.d;
      $4b: cpu.c := cpu.e;
      $4c: cpu.c := cpu.h;
      $4d: cpu.c := cpu.l;
      $4e: cpu.c := memory[cpu.hl];
      $4f: cpu.c := cpu.a;
      $50: cpu.d := cpu.b;
      $51: cpu.d := cpu.c;
      $52: cpu.d := cpu.d;
      $53: cpu.d := cpu.e;
      $54: cpu.d := cpu.h;
      $55: cpu.d := cpu.l;
      $56: cpu.d := memory[cpu.hl];
      $57: cpu.d := cpu.a;
      $58: cpu.e := cpu.b;
      $59: cpu.e := cpu.c;
      $5a: cpu.e := cpu.d;
      $5b: cpu.e := cpu.e;
      $5c: cpu.e := cpu.h;
      $5d: cpu.e := cpu.l;
      $5e: cpu.e := memory[cpu.hl];
      $5f: cpu.e := cpu.a;
      $60: cpu.h := cpu.b;
      $61: cpu.h := cpu.c;
      $62: cpu.h := cpu.d;
      $63: cpu.h := cpu.e;
      $64: cpu.h := cpu.h;
      $65: cpu.h := cpu.l;
      $66: cpu.h := memory[cpu.hl];
      $67: cpu.h := cpu.a;
      $68: cpu.l := cpu.b;
      $69: cpu.l := cpu.c;
      $6a: cpu.l := cpu.d;
      $6b: cpu.l := cpu.e;
      $6c: cpu.l := cpu.h;
      $6d: cpu.l := cpu.l;
      $6e: cpu.l := memory[cpu.hl];
      $6f: cpu.l := cpu.a;
      $70: memory[cpu.hl] := cpu.b;
      $71: memory[cpu.hl] := cpu.c;
      $72: memory[cpu.hl] := cpu.d;
      $73: memory[cpu.hl] := cpu.e;
      $74: memory[cpu.hl] := cpu.h;
      $75: memory[cpu.hl] := cpu.l;
      $76: begin insadr := cpu.pc-1; 
                 exeevt(ethalt, insadr, insadr, data, 1, halthan);
                 cpu.pc := insadr;
           end;
      $77: memory[cpu.hl] := cpu.a;
      $78: cpu.a := cpu.b;
      $79: cpu.a := cpu.c;
      $7a: cpu.a := cpu.d;
      $7b: cpu.a := cpu.e;
      $7c: cpu.a := cpu.h;
      $7d: cpu.a := cpu.l;
      $7e: cpu.a := memory[cpu.hl];
      $7f: cpu.a := cpu.a;
      $80: begin cpu.fa := (cpu.a and $f)+(cpu.b and $f) > $f;
                 t := cpu.a+cpu.b; cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $81: begin cpu.fa := (cpu.a and $f)+(cpu.c and $f) > $f;
                 t := cpu.a+cpu.c; cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $82: begin cpu.fa := (cpu.a and $f)+(cpu.d and $f) > $f;
                 t := cpu.a+cpu.d; cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $83: begin cpu.fa := (cpu.a and $f)+(cpu.e and $f) > $f;
                 t := cpu.a+cpu.e; cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $84: begin cpu.fa := (cpu.a and $f)+(cpu.h and $f) > $f;
                 t := cpu.a+cpu.h; cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $85: begin cpu.fa := (cpu.a and $f)+(cpu.l and $f) > $f;
                 t := cpu.a+cpu.l; cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $86: begin cpu.fa := (cpu.a and $f)+(memory[cpu.hl] and $f) > $f;
                 t := cpu.a+memory[cpu.hl]; cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $87: begin cpu.fa := (cpu.a and $f)+(cpu.a and $f) > $f;
                 t := cpu.a+cpu.a; cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $88: begin cpu.fa := (cpu.a and $f)+(cpu.b and $f)+ord(cpu.fc) > $f;
                 t := cpu.a+cpu.b+ord(cpu.fc); cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $89: begin cpu.fa := (cpu.a and $f)+(cpu.c and $f)+ord(cpu.fc) > $f;
                 t := cpu.a+cpu.c+ord(cpu.fc); cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $8a: begin cpu.fa := (cpu.a and $f)+(cpu.d and $f)+ord(cpu.fc) > $f;
                 t := cpu.a+cpu.d+ord(cpu.fc); cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $8b: begin cpu.fa := (cpu.a and $f)+(cpu.e and $f)+ord(cpu.fc) > $f;
                 t := cpu.a+cpu.e+ord(cpu.fc); cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $8c: begin cpu.fa := (cpu.a and $f)+(cpu.h and $f)+ord(cpu.fc) > $f;
                 t := cpu.a+cpu.h+ord(cpu.fc); cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $8d: begin cpu.fa := (cpu.a and $f)+(cpu.l and $f)+ord(cpu.fc) > $f;
                 t := cpu.a+cpu.l+ord(cpu.fc); cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $8e: begin cpu.fa := (cpu.a and $f)+(memory[cpu.hl] and $f)+
                           ord(cpu.fc) > $f;
                 t := cpu.a+memory[cpu.hl]+ord(cpu.fc); cpu.a := t;
                 cpu.fc := t > $ff; cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127;
                 cpu.fp := parity[cpu.a] end;
      $8f: begin cpu.fa := (cpu.a and $f)+(cpu.a and $f)+ord(cpu.fc) > $f;
                 t := cpu.a+cpu.a+ord(cpu.fc); cpu.a := t; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $90: begin cpu.fa := (cpu.a and $f)-(cpu.b and $f) < 0;
                 t := cpu.a-cpu.b; cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $91: begin cpu.fa := (cpu.a and $f)-(cpu.c and $f) < 0;
                 t := cpu.a-cpu.c; cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $92: begin cpu.fa := (cpu.a and $f)-(cpu.d and $f) < 0;
                 t := cpu.a-cpu.d; cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $93: begin cpu.fa := (cpu.a and $f)-(cpu.e and $f) < 0;
                 t := cpu.a-cpu.e; cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $94: begin cpu.fa := (cpu.a and $f)-(cpu.h and $f) < 0;
                 t := cpu.a-cpu.h; cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $95: begin cpu.fa := (cpu.a and $f)-(cpu.l and $f) < 0;
                 t := cpu.a-cpu.l; cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $96: begin cpu.fa := (cpu.a and $f)-(memory[cpu.hl] and $f) < 0;
                 t := cpu.a-memory[cpu.hl]; cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $97: begin cpu.fa := (cpu.a and $f)-(cpu.a and $f) < 0;
                 t := cpu.a-cpu.a; cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $98: begin cpu.fa := (cpu.a and $f)-(cpu.b and $f)-ord(cpu.fc) < 0;
                 t := cpu.a-cpu.b-ord(cpu.fc); cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $99: begin cpu.fa := (cpu.a and $f)-(cpu.c and $f)-ord(cpu.fc) < 0;
                 t := cpu.a-cpu.c-ord(cpu.fc); cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $9a: begin cpu.fa := (cpu.a and $f)-(cpu.d and $f)-ord(cpu.fc) < 0;
                 t := cpu.a-cpu.d-ord(cpu.fc); cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $9b: begin cpu.fa := (cpu.a and $f)-(cpu.e and $f)-ord(cpu.fc) < 0;
                 t := cpu.a-cpu.e-ord(cpu.fc); cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $9c: begin cpu.fa := (cpu.a and $f)-(cpu.h and $f)-ord(cpu.fc) < 0;
                 t := cpu.a-cpu.h-ord(cpu.fc); cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $9d: begin cpu.fa := (cpu.a and $f)-(cpu.l and $f)-ord(cpu.fc) < 0;
                 t := cpu.a-cpu.l-ord(cpu.fc); cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $9e: begin cpu.fa := (cpu.a and $f)-(memory[cpu.hl] and $f)-
                 ord(cpu.fc) < 0;
                 t := cpu.a-memory[cpu.hl]-ord(cpu.fc); cpu.a := t; 
                 cpu.fc := t < 0; cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127;
                 cpu.fp := parity[cpu.a] end;
      $9f: begin cpu.fa := (cpu.a and $f)-(cpu.a and $f)-ord(cpu.fc) < 0;
                 t := cpu.a-cpu.a-ord(cpu.fc); cpu.a := t; cpu.fc := t < 0;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $a0: begin cpu.a := cpu.a and cpu.b; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a];
                 cpu.fc := false; cpu.fa := false end;
      $a1: begin cpu.a := cpu.a and cpu.c; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $a2: begin cpu.a := cpu.a and cpu.d; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $a3: begin cpu.a := cpu.a and cpu.e; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $a4: begin cpu.a := cpu.a and cpu.h; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $a5: begin cpu.a := cpu.a and cpu.l; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $a6: begin cpu.a := cpu.a and memory[cpu.hl]; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $a7: begin cpu.a := cpu.a and cpu.a; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $a8: begin cpu.a := cpu.a xor cpu.b; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a];
                 cpu.fc := false; cpu.fa := false end;
      $a9: begin cpu.a := cpu.a xor cpu.c; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a];
                 cpu.fc := false; cpu.fa := false end;
      $aa: begin cpu.a := cpu.a xor cpu.d; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a];
                 cpu.fc := false; cpu.fa := false end;
      $ab: begin cpu.a := cpu.a xor cpu.e; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a];
                 cpu.fc := false; cpu.fa := false end;
      $ac: begin cpu.a := cpu.a xor cpu.h; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a];
                 cpu.fc := false; cpu.fa := false end;
      $ad: begin cpu.a := cpu.a xor cpu.l; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a];
                 cpu.fc := false; cpu.fa := false end;
      $ae: begin cpu.a := cpu.a xor memory[cpu.hl]; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a];
                 cpu.fc := false; cpu.fa := false end;
      $af: begin cpu.a := 0; cpu.fz := true; cpu.fs := false; cpu.fp := true;
                 cpu.fc := false; cpu.fa := false end;
      $b0: begin cpu.a := cpu.a or cpu.b; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $b1: begin cpu.a := cpu.a or cpu.c; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $b2: begin cpu.a := cpu.a or cpu.d; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $b3: begin cpu.a := cpu.a or cpu.e; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $b4: begin cpu.a := cpu.a or cpu.h; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $b5: begin cpu.a := cpu.a or cpu.l; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $b6: begin cpu.a := cpu.a or memory[cpu.hl]; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $b7: begin cpu.a := cpu.a or cpu.a; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a];
                 cpu.fc := false; cpu.fa := false end;
      $b8: begin cpu.fa := (cpu.a and $f)-(cpu.b and $f) < 0; t := cpu.a-cpu.b;
                 cpu.fc := t < 0; if cpu.fc then t := t+$100; cpu.fz := t = 0;
                 cpu.fs := t > 127; cpu.fp := parity[t] end;
      $b9: begin cpu.fa := (cpu.a and $f)-(cpu.c and $f) < 0; t := cpu.a-cpu.c;
                 cpu.fc := t < 0; if cpu.fc then t := t+$100; cpu.fz := t = 0;
                 cpu.fs := t > 127; cpu.fp := parity[t] end;
      $ba: begin cpu.fa := (cpu.a and $f)-(cpu.d and $f) < 0; t := cpu.a-cpu.d;
                 cpu.fc := t < 0; if cpu.fc then t := t+$100; cpu.fz := t = 0;
                 cpu.fs := t > 127; cpu.fp := parity[t] end;
      $bb: begin cpu.fa := (cpu.a and $f)-(cpu.e and $f) < 0; t := cpu.a-cpu.e;
                 cpu.fc := t < 0; if cpu.fc then t := t+$100; cpu.fz := t = 0;
                 cpu.fs := t > 127; cpu.fp := parity[t] end;
      $bc: begin cpu.fa := (cpu.a and $f)-(cpu.h and $f) < 0; t := cpu.a-cpu.h;
                 cpu.fc := t < 0; if cpu.fc then t := t+$100; cpu.fz := t = 0;
                 cpu.fs := t > 127; cpu.fp := parity[t] end;
      $bd: begin cpu.fa := (cpu.a and $f)-(cpu.l and $f) < 0; t := cpu.a-cpu.l;
                 cpu.fc := t < 0; if cpu.fc then t := t+$100; cpu.fz := t = 0;
                 cpu.fs := t > 127; cpu.fp := parity[t] end;
      $be: begin cpu.fa := (cpu.a and $f)-(memory[cpu.hl] and $f) < 0;
                 t := cpu.a-memory[cpu.hl]; cpu.fc := t < 0;
                 if cpu.fc then t := t+$100; cpu.fz := t = 0;
                 cpu.fs := t > 127; cpu.fp := parity[t] end;
      $bf: begin cpu.fa := (cpu.a and $f)-(cpu.a and $f) < 0; t := cpu.a-cpu.a;
                 cpu.fc := t < 0; if cpu.fc then t := t+$100; cpu.fz := t = 0;
                 cpu.fs := t > 127; cpu.fp := parity[t] end;
      $c0: if not cpu.fz then 
              begin cpu.pc := memory[cpu.sp]+memory[cpu.sp+1]*256;
                    cpu.sp := cpu.sp+2 end;
      $c1: begin cpu.c := memory[cpu.sp]; cpu.sp := cpu.sp+1; 
                 cpu.b := memory[cpu.sp]; cpu.sp := cpu.sp+1 end;
      $c2: if not cpu.fz then cpu.pc := memory[cpu.pc]+memory[cpu.pc+1]*256
           else cpu.pc := cpu.pc+2;
      $c3: cpu.pc := memory[cpu.pc]+memory[cpu.pc+1]*256;
      $c4: begin t := memory[cpu.pc]; cpu.pc := cpu.pc+1;
                 t := t+memory[cpu.pc]*256; cpu.pc := cpu.pc+1;
                 if not cpu.fz then begin
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := t end
           end;
      $c5: begin cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.b;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.c end;
      $c6: begin cpu.fa := (cpu.a and $f)+(memory[cpu.pc] and $f) > $f;
                 t := cpu.a+memory[cpu.pc]; cpu.a := t; cpu.fc := t > $ff;
                 cpu.pc := cpu.pc+1; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a] end;
      $c7: begin cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := $0000 end;
      $c8: if cpu.fz then begin cpu.pc := memory[cpu.sp]+memory[cpu.sp+1]*256;
                                cpu.sp := cpu.sp+2 end;
      $c9: begin cpu.pc := memory[cpu.sp]+memory[cpu.sp+1]*256; 
                 cpu.sp := cpu.sp+2 end;
      $ca: if cpu.fz then cpu.pc := memory[cpu.pc]+memory[cpu.pc+1]*256
           else cpu.pc := cpu.pc+2;
      $cb: begin insadr := cpu.pc-1; 
                 exeevt(etinvicb, insadr, insadr, data, 1, invicbhan);
                 cpu.pc := insadr;
           end;
      $cc: begin t := memory[cpu.pc]; cpu.pc := cpu.pc+1;
              t := t+memory[cpu.pc]*256; cpu.pc := cpu.pc+1;
              if cpu.fz then begin
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := t end
           end;
      $cd: begin t := memory[cpu.pc]+memory[cpu.pc+1]*256;
                 cpu.pc := cpu.pc+2;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := t end;
      $ce: begin cpu.fa := (cpu.a and $f)+
                           (memory[cpu.pc] and $f)+ord(cpu.fc) > $f;
                 t := cpu.a+memory[cpu.pc]+ord(cpu.fc); cpu.a := t;
                 cpu.pc := cpu.pc+1; cpu.fc := t > $ff;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $cf: begin cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := $0008 end;
      $d0: if not cpu.fc then begin
              cpu.pc := memory[cpu.sp]+memory[cpu.sp+1]*256; 
              cpu.sp := cpu.sp+2 end;
      $d1: begin cpu.e := memory[cpu.sp]; cpu.sp := cpu.sp+1; 
                 cpu.d := memory[cpu.sp]; cpu.sp := cpu.sp+1 end;
      $d2: if not cpu.fc then cpu.pc := memory[cpu.pc]+memory[cpu.pc+1]*256
           else cpu.pc := cpu.pc+2;
      $d3: begin insadr := cpu.pc-1; accadr := memory[cpu.pc]; data := cpu.a;
                 exeevt(etiowrite, insadr, accadr, data, 1, outhan[accadr]);
                 cpu.pc := cpu.pc+1 end;
      $d4: begin t := memory[cpu.pc]; cpu.pc := cpu.pc+1;
                 t := t+memory[cpu.pc]*256; cpu.pc := cpu.pc+1;
                 if not cpu.fc then begin
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := t end
           end;
      $d5: begin cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.d;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.e end;
      $d6: begin cpu.fa := (cpu.a and $f)-(memory[cpu.pc] and $f) < 0;
                 t := cpu.a-memory[cpu.pc]; cpu.a := t; cpu.fc := t < 0;
                 cpu.pc := cpu.pc+1; cpu.fz := cpu.a = 0;
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a] end;
      $d7: begin cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := $0010 end;
      $d8: if cpu.fc then begin cpu.pc := memory[cpu.sp]+memory[cpu.sp+1]*256;
              cpu.sp := cpu.sp+2 end;
      $d9: begin insadr := cpu.pc-1; 
                 exeevt(etinvid9, insadr, insadr, data, 1, invid9han);
                 cpu.pc := insadr;
           end;
      $da: if cpu.fc then cpu.pc := memory[cpu.pc]+memory[cpu.pc+1]*256
           else cpu.pc := cpu.pc+2;
      $db: begin insadr := cpu.pc-1; accadr := memory[cpu.pc];
                 exeevt(etioread, insadr, accadr, data, 1, inphan[accadr]);
                 cpu.a := data; cpu.pc := cpu.pc+1 end;
      $dc: begin t := memory[cpu.pc]; cpu.pc := cpu.pc+1;
                 t := t+memory[cpu.pc]*256; cpu.pc := cpu.pc+1;
                 if cpu.fc then begin
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := t end
           end;
      $dd: begin insadr := cpu.pc-1; 
                 exeevt(etinvidd, insadr, insadr, data, 1, inviddhan);
                 cpu.pc := insadr;
           end;
      $de: begin cpu.fa := (cpu.a and $f)-
                           (memory[cpu.pc] and $f)-ord(cpu.fc) < 0;
                 t := cpu.a-memory[cpu.pc]-ord(cpu.fc); cpu.a := t;
                 cpu.fc := t < 0; cpu.pc := cpu.pc+1;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127; 
                 cpu.fp := parity[cpu.a] end;
      $df: begin cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := $0018 end;
      $e0: if not cpu.fp then begin
              cpu.pc := memory[cpu.sp]+memory[cpu.sp+1]*256; 
              cpu.sp := cpu.sp+2 end;
      $e1: begin cpu.l := memory[cpu.sp]; cpu.sp := cpu.sp+1; 
                 cpu.h := memory[cpu.sp]; cpu.sp := cpu.sp+1 end;
      $e2: if not cpu.fp then 
              cpu.pc := memory[cpu.pc]+memory[cpu.pc+1]*256
           else cpu.pc := cpu.pc+2;
      $e3: begin t := cpu.h; t1 := cpu.l; cpu.l := memory[cpu.sp]; 
                 cpu.h := memory[cpu.sp+1]; memory[cpu.sp] := t1;
                 memory[cpu.sp+1] := t end;
      $e4: begin t := memory[cpu.pc]; cpu.pc := cpu.pc+1;
                 t := t+memory[cpu.pc]*256; cpu.pc := cpu.pc+1;
                 if not cpu.fp then begin
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := t end
           end;
      $e5: begin cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.h;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.l end;
      $e6: begin t := cpu.a; cpu.a := cpu.a and memory[cpu.pc];
                 cpu.pc := cpu.pc+1; cpu.fz := cpu.a = 0; 
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false;
                 cpu.fa := false end;
      $e7: begin cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := $0020 end;
      $e8: if cpu.fp then begin cpu.pc := memory[cpu.sp]+memory[cpu.sp+1]*256;
                                cpu.sp := cpu.sp+2 end;
      $e9: cpu.pc := cpu.hl;
      $ea: if cpu.fp then cpu.pc := memory[cpu.pc]+memory[cpu.pc+1]*256
           else cpu.pc := cpu.pc+2;
      $eb: begin ta := cpu.hl; cpu.hl := cpu.de; cpu.de := ta end;
      $ec: begin t := memory[cpu.pc]; cpu.pc := cpu.pc+1;
                 t := t+memory[cpu.pc]*256; cpu.pc := cpu.pc+1;
                 if cpu.fp then begin
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := t end
           end;
      $ed: begin insadr := cpu.pc-1; 
                 exeevt(etinvied, insadr, insadr, data, 1, inviedhan);
                 cpu.pc := insadr;
           end;
      $ee: begin cpu.a := cpu.a xor memory[cpu.pc];
                 cpu.pc := cpu.pc+1; cpu.fz := cpu.a = 0; 
                 cpu.fs := cpu.a > 127; cpu.fp := parity[cpu.a]; 
                 cpu.fc := false; cpu.fa := false end;
      $ef: begin cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := $0028 end;
      $f0: if not cpu.fs then
              begin cpu.pc := memory[cpu.sp]+memory[cpu.sp+1]*256;
                    cpu.sp := cpu.sp+2 end;
      $f1: begin cpu.af := memory[cpu.sp]+memory[cpu.sp+1]*256;
                 cpu.sp := cpu.sp+2 end;
      $f2: if not cpu.fs then cpu.pc := memory[cpu.pc]+memory[cpu.pc+1]*256
           else cpu.pc := cpu.pc+2;
      $f3: cpu.ie := false;
      $f4: begin t := memory[cpu.pc]; cpu.pc := cpu.pc+1;
                 t := t+memory[cpu.pc]*256; cpu.pc := cpu.pc+1;
                 if not cpu.fs then begin
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := t end
           end;
      $f5: begin cpu.sp := cpu.sp-2; memory[cpu.sp] := cpu.af;
                 memory[cpu.sp+1] := cpu.af div 256 end;
      $f6: begin cpu.a := cpu.a or memory[cpu.pc]; cpu.pc := cpu.pc+1;
                 cpu.fz := cpu.a = 0; cpu.fs := cpu.a > 127;
                 cpu.fp := parity[cpu.a]; cpu.fc := false;
                 cpu.fa := false end;
      $f7: begin cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc;
                 cpu.pc := $0030 end;
      $f8: if cpu.fs then begin cpu.pc := memory[cpu.sp]+memory[cpu.sp+1]*256;
                                cpu.sp := cpu.sp+2 end;
      $f9: cpu.sp := cpu.hl;
      $fa: if cpu.fs then cpu.pc := memory[cpu.pc]+memory[cpu.pc+1]*256
           else cpu.pc := cpu.pc+2;
      $fb: cpu.ie := true;
      $fc: begin t := memory[cpu.pc]; cpu.pc := cpu.pc+1;
              t := t+memory[cpu.pc]*256; cpu.pc := cpu.pc+1;
              if cpu.fs then begin
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc and $ff;
                 cpu.pc := t end
           end;
      $fd: begin insadr := cpu.pc-1; 
                 exeevt(etinvifd, insadr, insadr, data, 1, invifdhan);
                 cpu.pc := insadr;
           end;
      $fe: begin cpu.fa := (cpu.a and $f)-(memory[cpu.pc] and $f) < 0;
                 t := cpu.a-memory[cpu.pc]; cpu.fc := t < 0;
                 if cpu.fc then t := t+$100; cpu.pc := (cpu.pc+1) and $ffff;
                 cpu.fz := t = 0; cpu.fs := t > 127; 
                 cpu.fp := parity[t] end;
      $ff: begin cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc div 256;
                 cpu.sp := cpu.sp-1; memory[cpu.sp] := cpu.pc and $ff;
                 cpu.pc := $0038 end

   end

end;

{*******************************************************************************

Default invalid instruction handler

Handles the invalid instruction event.

*******************************************************************************}

procedure invins(e: evttyp; var insaddr: integer; accaddr: integer; 
                 var data: integer; len: integer);

begin

   refer(e, insaddr, accaddr, data, len); { set parameters unused }

   prterr(einvins) { flag invalid instruction }

end;

{*******************************************************************************

I/O reads and writes

I/O reads and writes always produce an error until they are mapped.

*******************************************************************************}

procedure ioins(e: evttyp; var insaddr: integer; accaddr: integer; 
                 var data: integer; len: integer);

begin

   refer(e, insaddr, accaddr, data, len); { set parameters unused }

   prterr(eunmapio) { flag unmapped I/O instruction }

end;

{*******************************************************************************

Halt instruction

Halt instruction produces an error.

*******************************************************************************}

procedure haltins(e: evttyp; var insaddr: integer; accaddr: integer; 
                  var data: integer; len: integer);

begin

   refer(e, insaddr, accaddr, data, len); { set parameters unused }

   prterr(ehltins) { flag halt instruction }

end;

{*******************************************************************************

Reserve event

Reserves a event or exception in the CPU. The given event type is reserved. If
the event involves a range of addresses, then the base and length of the range
are specified. The event handler is specified, which is used to set the vector
to be used in that event.

*******************************************************************************}

procedure reserve(r: rsvtyp; base: integer; len: integer; 
                  procedure event(e: evttyp; var insaddr: integer; 
                                  accaddr: integer; var data: integer; 
                                  len: integer));

var i: 0..255; { index for I/O address blocks }

begin

   case r of { reservation type }

      rsinvi08: getevt(event, invi08han); { place event for $08 instruction }
      rsinvi10: getevt(event, invi10han); { place event for $10 instruction }
      rsinvi18: getevt(event, invi18han); { place event for $18 instruction }
      rsinvi20: getevt(event, invi20han); { place event for $20 instruction }
      rsinvi28: getevt(event, invi28han); { place event for $28 instruction }
      rsinvi30: getevt(event, invi30han); { place event for $30 instruction }
      rsinvi38: getevt(event, invi38han); { place event for $38 instruction }
      rsinvicb: getevt(event, invicbhan); { place event for $cb instruction }
      rsinvid9: getevt(event, invid9han); { place event for $d9 instruction }
      rsinvidd: getevt(event, inviddhan); { place event for $dd instruction }
      rsinvied: getevt(event, inviedhan); { place event for $ed instruction }
      rsinvifd: getevt(event, invifdhan); { place event for $fd instruction }
      rsinvi: begin { reserve all invalid instructions }

         getevt(event, invi08han); { place event for $08 instruction }
         getevt(event, invi10han); { place event for $10 instruction }
         getevt(event, invi18han); { place event for $18 instruction }
         getevt(event, invi20han); { place event for $20 instruction }
         getevt(event, invi28han); { place event for $28 instruction }
         getevt(event, invi30han); { place event for $30 instruction }
         getevt(event, invi38han); { place event for $38 instruction }
         getevt(event, invicbhan); { place event for $cb instruction }
         getevt(event, invid9han); { place event for $d9 instruction }
         getevt(event, inviddhan); { place event for $dd instruction }
         getevt(event, inviedhan); { place event for $ed instruction }
         getevt(event, invifdhan)  { place event for $fd instruction }

      end;    
      rsmemread: ; { memory reservations not implemented } 
      rsmemwrite: ; { memory reservations not implemented }
      rsmemrdwr: ; { memory reservations not implemented } 
      { reserve input address range }
      rsioread: for i := base to base+len-1 do getevt(event, inphan[i]);
      rsiowrite: for i := base to base+len-1 do getevt(event, outhan[i]); 
      rsiordwr: begin

         { reserve input and output address range }
         for i := base to base+len-1 do getevt(event, inphan[i]);
         for i := base to base+len-1 do getevt(event, outhan[i])

      end;

      rshalt: getevt(event, halthan)

   end

end;

{ same for non-address events }

overload procedure reserve(r: rsvtyp; 
                           procedure event(e: evttyp; var insaddr: integer; 
                                           accaddr: integer; var data: integer;
                                           len: integer));

begin

   reserve(r, 0, 0, event)

end;

{*******************************************************************************

Initialize CPU specific module

*******************************************************************************}

begin

   { Clearing memory may or may not be advisable. For a large memory processor,
     it can not only be time consuming, but it could cause virtual memory to be
     unecessarily allocated. Also, most virtual memory hosts arrange to have
     memory cleared for new run tasks. For the small memory 8080, we clear it. }

   for addr := 0 to maxmem do memory[addr] := 0; { clear memory }

   { clear CPU registers }
   cpu.a := 0;
   cpu.b := 0;
   cpu.c := 0;
   cpu.d := 0;
   cpu.e := 0;
   cpu.h := 0;
   cpu.l := 0;
   cpu.pc := 0;
   cpu.sp := 0;
   cpu.fc := false;
   cpu.fa := false;
   cpu.fs := false;
   cpu.fp := false;
   cpu.f5 := false;
   cpu.f3 := false;
   cpu.f1 := false;
   cpu.ie := true; { interrupts enabled }

   { fill parity lookup table }
   for pi := 0 to 255 do begin

      t := pi; { place this value }
      c := 0; { clear bit count }
      for bi := 0 to 7 do begin

         if odd(t) then c := c+1; { count bits in byte }
         t := t div 2 { next bit }

      end;
      parity[pi] := not odd(c) { set parity as even bit count }

   end;
   
   { process default reservations }

   reserve(rsinvi08, invins); { $08 instruction }
   reserve(rsinvi10, invins); { $08 instruction }
   reserve(rsinvi18, invins); { $08 instruction }
   reserve(rsinvi20, invins); { $08 instruction }
   reserve(rsinvi28, invins); { $08 instruction }
   reserve(rsinvi30, invins); { $08 instruction }
   reserve(rsinvi38, invins); { $08 instruction }
   reserve(rsinvicb, invins); { $08 instruction }
   reserve(rsinvid9, invins); { $08 instruction }
   reserve(rsinvidd, invins); { $08 instruction }
   reserve(rsinvied, invins); { $08 instruction }
   reserve(rsinvifd, invins); { $08 instruction }
   reserve(rsiordwr, 0, 256, ioins); { all I/O instructions }
   reserve(rshalt, haltins); { halt instruction }

end.
{******************************************************************************

Test program for multiword math

Presents a library of functions that maps basic math functions on arbitrary
length numbers to standard Pascal integers, by using arrays of integers.

******************************************************************************}

program multi(output);

const

   { SVS specific constants }
   maxint  = 2147483647; { redefine for 32 bits }

   maxval = 32767; { maximum value of a single "unit", sqrt(maxint),
                     so that maxval*maxval can still be contained
                     within integer. This enables 15 bits of data
                     to be processed }
   maxlen = 18;    { number of values whose total bit capacity meets
                     or exceeds 256 bits }

type

   { SVS specific types }
   integer = longint; { redefine integers for 32 bits }
   valnum = 0..maxval; { unit of value }
   valinx = 1..maxlen; { index for value body }
   value = record { extended number }
      
      sign: integer; { sign of number }
      val:  array [valinx] of integer { body of number }

   end;

var a, b: value; { test values }

{ print number diagnostic }

procedure prtdig(var a: value);

var i: valinx; { index for value }

begin

   if a.sign < 0 then write('-');
   for i := 1 to maxlen do write(a.val[i]:1, ' ')

end;

{ place integer in superger, only 0..maxval allowed }

procedure place(var a: value; b: integer);

var i: valinx;

begin

   a.sign := 1; { set positive }
   for i := 1 to maxlen do a.val[i] := 0; { clear result }
   a.val[maxlen] := b { place value }

end;

{ check unsigned a greater than b }

function gtr(var a, b: value): boolean;

var i: valinx; { index for values }
    
begin

   i := 1; { set msv }
   { find first non-equal value }
   while (a.val[i] = b.val[i]) and (i < maxlen) do i := i+1;
   gtr := a.val[i] > b.val[i] { set greater status }

end;

{ check a equals b }

function equ(var a, b: value): boolean;

var i: valinx; { index for values }

begin

   i := 1; { set msv }
   { find first non-equal value }
   while (a.val[i] = b.val[i]) and (i < maxlen) do i := i+1;
   equ := (a.val[i] = b.val[i]) and (a.sign = b.sign)

end;

{ add a and b and place the result in a }

procedure add(var a, b: value);

var i: valinx; { index for values }
    c: integer; { carry }

begin

   c := 0; { clear carry }
   if a.sign = b.sign then { signs are equal, add, sign is a }
      for i := maxlen downto 1 do begin { add numbers }

      a.val[i] := a.val[i]+b.val[i]+c; { add values }
      if a.val[i] > maxval then begin a.val[i] := a.val[i]-(maxval+1); c := 1 end
      else c := 0

   end else if gtr(a, b) then begin { a minus b }

      for i := maxlen downto 1 do begin { subtract numbers }

         a.val[i] := a.val[i]-b.val[i]-c; { subtract values }
         if a.val[i] < 0 then begin a.val[i] := a.val[i]+maxval+1; c := 1 end
         else c := 0

      end;

   end else { b minus a }
      for i := maxlen downto 1 do begin { subtract numbers }

      a.val[i] := b.val[i]-a.val[i]-c; { subtract values }
      if a.val[i] < 0 then begin a.val[i] := a.val[i]+maxval+1; c := 1 end
      else c := 0;
      a.sign := b.sign { set sign to greater }

   end;
   c := 0; { check result is zero }
   for i := 1 to maxlen do c := c or a.val[i];
   if c = 0 then a.sign := 1 { set all zeros positive } 

end; 

{ subtract b from a and place the result in a }

procedure sub(var a, b: value);

var ss: integer; { sign save }

begin

   ss := b.sign; { save sign of b }
   b.sign := -b.sign; { negate sign of b }
   add(a, b); { add to a }
   b.sign := ss { restore sign of b }

end;

{ multiply a times b and place the result in a }

procedure mult(var a, b: value);

var r:    value; { result holder }
    one:  value; { holder for one }
    zero: value; { holder for zero }
    t:    value; { temp }

begin

   place(r, 0); { clear  result }
   r.sign := a.sign*b.sign; { set sign of result }
   place(one, 1); { place one constant }
   place(zero, 0); { place zero constant }
   t := b; { save mutiplier }
   { repetitive add }
   while not equ(t, zero) do begin add(r, a); sub(t, one) end;
   a := r { copy result to a }

end;

{ divide a by b, place the result in a, modulo in b }

procedure divi(var a, b: value);

var r:   value;  { result }
    one: value;  { store for 1 }   

begin

   place(r, 0); { clear result }
   r.sign := a.sign div b.sign; { place result sign }
   place(one, 1); { set 1 constant }
   { repetitive subtract to find result }
   while not gtr(b, a) do begin sub(a, b); add(r, one) end;
   b := a; { place remander in b }
   a := r { place result }

end;

{ print number }

procedure print(var a: value);

var i:   valinx; { index for value }
    t:   value;  { temp }
    ten: value;  { store for 10 }   
    str: packed array [1..100] of char; { result ascii storage }
    oi:  1..100; { index for same }

begin

   place(ten, 10); { set 10 constant }
   for oi := 100 downto 1 do begin { extract digits }

      t := ten; { place ten constant }
      divi(a, t); { find a div 10 }
      str[oi] := chr(t.val[maxlen]+ord('0')) { place digit }

   end;
   for oi := 1 to 100 do write(str[oi]) { output resulting number }

end;   

{ read number }

procedure readv(var a: value);

var ten: value; { holder for ten }
    t:   value; { holder for new digit }

begin

   place(a, 0); { clear result }
   place(ten, 10); { set 10 constant }
   while input^ in ['0'..'9'] do begin { add digit }

      mult(a, ten); { move up number }
      place(t, ord(input^)-ord('0')); { place new digit in temp }
      add(a, t); { add in }
      get(input) { next digit }

   end;
   readln { next line }

end;   
   
begin

   readv(a); { get the user input value }
   prtdig(a);
   writeln;
   writeln('The value is: ');
   print(a);
   writeln;

end.   

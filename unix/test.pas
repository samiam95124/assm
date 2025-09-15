program test(output);

uses strlib;

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

begin

   prthex(8, -2)

end.
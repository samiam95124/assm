{*******************************************************************************
*                                                                              *
*                            SYSTEM CALL DB MODULE                             *
*                                                                              *
* Adds a "system call" to DB code. This is a general purpose I/O instruction   *
* that a simulated program can use to access operating system I/O capability.  *
*                                                                              *
*******************************************************************************}

module syscall(output);

uses stddef,   { standard defines }
     strlib,   { string library }
     extlib,   { extention library }
     dbdef,    { db defines }
     defi8080, { cpu specific defines }
     simi8080, { simulator defines }
     db,       { db module }
     main;     { main module }

type

   syscod = (scterm,   { terminate program }
             scopen,   { open file }
             scclose,  { close file }
             scread,   { read data }
             scwrite); { write data }
   syserr = (senull,  { no error }
             secal,   { invalid call }
             semod,   { invalid file mode }
             sefull,  { file table full }
             seivfn,  { invalid filename }
             seexist, { non-existant file }
             sefiln,  { file descriptor passed is bad }
             seeof);  { end of file reached }

var

   fstap:  boolean; { allow system tap calls }

private

type 

   fidinx = 1..maxfid; { index for program open files }
   fid    = 0..maxfid; { fid, including 0 (null) case }
   fidrec = record { program file entry }

               { status of file }
               sta: (fmfree, { file entry is free }
                     fmfil); { file entry is open file }
               fil: bytfil   { the file }

            end;

var

   fidtab:  array [fidinx] of fidrec; { open program files id table }
   fi:      fidinx; { file table index }

{*******************************************************************************

Perform system call

In 8080, the system tap is tripped by the sequence "ed ff xx", where "xx" is
the system call code in byte form. This number is used because "ed ff" is an
illegal instruction in 8080, 8085, Z80, Z64180, and Z280 processors. This means
that a program using this system tap can be migrated upwards to any simulator
without change.

The Portable Call Set is used, with 8080 registers. The following calls and
formats are implemented:

        0 - Terminate program. A contains the termination error code,
            with A = 0 = no error, and A <> 0 means error. The error code,
            if <> 0, is printed in text on the console, and the program run
            halted.
            When a program gets an error, it may handle the error itself,
            or pass the error back for printout and termination.

        1 - Open file. A contains the length of the filename, HL points to
            the first character of the filename, and B contains the file
            mode flags as follows:

               bit 0 - Open for read.
               bit 1 - open for write. Both read and write set means open for
                       read and write, which is not implemented at present. 
                       No read or write is an error.

            Returns an error code in A, where A <> 0 = error.
            Returns the file ID of the open file in C, which is 0 if no file
            was opened.

        2 - Close file. C contains the file ID. Closes the file. Returns the
            error code in A.

        3 - Read. C contains the file ID, HL contains the block address, and
            DE contains the block count. Returns the error code in A.

        4 - Write. C contains the file ID, HL contains the block address, and
            DE contains the block count. Returns the error code in A.

On all system calls, the carry AND zero flags also reflect the error status of
the call. If an error occured, carry is set and zero is reset. If not, carry is
reset and zero is set.
In addition to DOS provided filenames, we implement the following special
assignments:

        _command - Contains the command line.
        _input   - Receives input from the debug console.
        _output  - Prints on the debug console.
        _error   - Prints on the debug console.
        _list    - Prints on the printer.

*******************************************************************************}

procedure syscal(e: evttyp; var insaddr: integer; accaddr: integer; 
                  var data: integer; len: integer);

var cc:   byte;      { system call code }
    fidn: 0..maxfid; { program fid }
    fnam: filnam;    { file name buffer }
    b:    byte;

{ find free fid }

function frefid: fid;

var fi:   fidinx;    { index for program files table }
    fidn: 0..maxfid; { program fid }

begin

   { search for first open file. We search down in the table so that
     we will allways return the lowest number possible }
   fidn := 0; { set fid invalid }
   for fi := maxfid downto 1 do { search for open fid }
      if fidtab[fi].sta = fmfree then fidn := fi; { found a free entry }
   frefid := fidn { return found or null entry }

end;

{ load filename from memory }

procedure lodfil;

var t, l: integer; { temps }
    ni:   filinx;  { index for filename }

begin

   l := getreg(rega); { get length of filename }
   putreg(rega, 0); { set no error }
   { load filename }
   for ni := 1 to maxfil do fnam[ni] := ' '; { clear filename }
   ni := 1; { set 1st filename character }
   t := getreg(regh)*256+getreg(regl); { get address of filename }
   while (l > 0) and (getmem(t, 1) = ord(' ')) do begin { skip spaces }

      t := t+1; { next character }
      l := l-1

   end;
   { read non-space characters into filename }
   while (l > 0) and (getmem(t, 1) <> ord(' ')) do begin

      if ni < maxfil then begin { place filename characters }

         fnam[ni] := chr(getmem(t, 1)); { place character }
         ni := ni+1 { next character }

      end;
      t := t+1; { next character }
      l := l-1

   end;
   { skip trailing spaces }
   while (l > 0) and (getmem(t, 1) = ord(' ')) do begin { skip spaces }

      t := t+1; { next character }
      l := l-1

   end;
   { if filename is null (after spaces), or too large, or had
     embedded blanks, it's invalid }
   if (ni = 1) or (ni = maxfil) or (l <> 0) then 
      putreg(rega, ord(seivfn)) { invalid filename }

end;

{ syscal }
   
begin

   refer(e, accaddr, data, len); { these parameters are unused }

   if (getmem(insaddr+1, 1) = $ff) and fstap then begin { valid system call }

      cc := getmem(insaddr+2, 1); { get system call code }
      insaddr := insaddr+3; { advance PC past system call }
      if cc > ord(scwrite) then { invalid system call }
         putreg(rega, ord(secal)) { flag invalid call }
      else case syscod(cc) of { system call }
   
         scterm: begin { terminate program }
   
            runcpu := false; { stop execution }
            if getreg(rega) <> 0 then begin { an error was passed }
   
               write('*** Program error: ');
               if getreg(rega) > ord(seeof) then { unknown error }
                  writeln('Unspecified')
               else case syserr(getreg(rega)) of { error code }
   
                  senull:  ;
                  secal:   write('Invalid system call');
                  semod:   write('Invalid file mode');
                  sefull:  write('Maximum files already open');
                  seivfn:  write('Invalid filename');
                  seexist: write('Non-existant file');
                  sefiln:  write('Invalid file number');
                  seeof:   write('End of file reached')
   
               end;
               writeln
   
            end;
            putreg(regpc, getreg(regpc)-3); { back up to start of instruction }
            if getreg(regpc) < 0 then putreg(regpc, getreg(regpc)+maxmem+1)
   
         end;
   
         scopen: begin { open file }
   
            putreg(regc, 0); { set file unopened by default }
            if (getreg(regb) <> 1) and (getreg(regb) <> 2) then 
               putreg(rega, ord(semod)) { flag invalid open mode }
            else begin { open mode is read or write }
   
               { search for first open file. We search down in the table so that
                 we will allways return the lowest number possible }
               fidn := frefid; { find free fid }
               if fidn = 0 then putreg(rega, ord(sefull)) { flag table full }
               else begin { open the file }
   
                  lodfil; { load filename from memory }
                  if getreg(rega) = 0 then begin { filename loaded ok }
   
                     fidtab[fidn].sta := fmfil; { default to normal file }
                     { open file for read }
                     if getreg(regb) = 1 then begin
   
                        assign(fidtab[fidn].fil, fnam);
                        reset(fidtab[fidn].fil)
   
                     end else begin
   
                        assign(fidtab[fidn].fil, fnam);
                        rewrite(fidtab[fidn].fil)
   
                     end;
                     putreg(regc, fidn) { place opened file }
   
                  end
   
               end
   
            end
   
         end;
   
         scclose: begin { close file }
   
            { check file descriptor is non-zero and within limits }
            if (getreg(regc) = 0) or (getreg(regc) > maxfid) then 
               putreg(rega, ord(sefiln))
            { check file at descriptor is open }
            else if fidtab[getreg(regc)].sta = fmfree then 
               putreg(rega, ord(sefiln))
            else if fidtab[getreg(regc)].sta = fmfil then begin { close file }
   
               close(fidtab[getreg(regc)].fil); { close the file }
               fidtab[getreg(regc)].sta := fmfree; { free fid entry }
               putreg(regc, 0); { set no longer a valid file }
               putreg(rega, 0) { set no error }
   
            end
   
         end;
   
         scread: begin { read from file }
   
            { check file descriptor is non-zero and within limits }
            if (getreg(regc) = 0) or (getreg(regc) > maxfid) then 
               putreg(rega, ord(sefiln))
            { check file at descriptor is open }
            else if fidtab[getreg(regc)].sta = fmfree then 
               putreg(rega, ord(sefiln))
            else if fidtab[getreg(regc)].sta = fmfil then begin { read file }
   
               repeat { read bytes }
   
                  { read a byte }
                  read(fidtab[getreg(regc)].fil, b);
                  putmem(getreg(regh)*256+getreg(regl), b, 1);
                  { increment destination }
                  putreg(regl, (getreg(regl)+1) and $ff);
                  if getreg(regl) = 0 then 
                     putreg(regh, (getreg(regh)+1) and $ff);
                  putreg(rege, getreg(rege)-1);
                  if getreg(rege) < 0 then begin 
   
                     putreg(rege, getreg(rege)+$100);
                     putreg(regd, getreg(regd)-1);
                     if getreg(regd) < 0 then putreg(regd, getreg(regd)+$100)
   
                  end
   
               until getreg(regd)+getreg(rege) = 0; { until count satisfied }
               putreg(rega, 0) { set no error }
   
            end
   
         end;
   
         scwrite: begin { write to file }
   
            { check file descriptor is non-zero and within limits }
            if (getreg(regc) = 0) or (getreg(regc) > maxfid) then 
               putreg(rega, ord(sefiln))
            { check file at descriptor is open }
            else if fidtab[getreg(regc)].sta = fmfree then 
               putreg(rega, ord(sefiln))
            else if fidtab[getreg(regc)].sta = fmfil then begin { write file }
   
               repeat { write bytes }
   
                  { write a byte }       
                  write(fidtab[getreg(regc)].fil, 
                        getmem(getreg(regh)*256+getreg(regl), 1));
                  { increment destination }
                  putreg(regl, (getreg(regl)+1) and $ff);
                  if getreg(regl) = 0 then 
                     putreg(regh, (getreg(regh)+1) and $ff);
                  putreg(rege, getreg(rege)-1);
                  if getreg(rege) < 0 then begin 
   
                     putreg(rege, getreg(rege)+$100);
                     putreg(regd, getreg(regd)-1);
                     if getreg(regd) < 0 then putreg(regd, getreg(regd)+$100)
   
                  end
   
               until getreg(regd)+getreg(rege) = 0; { until count satisfied }
               putreg(rega, 0) { set no error }
   
            end
   
         end
   
      end;
      { set flags for "good" termination }
      if getreg(rega) = 0 then 
         begin putreg(regfc, ord(false)); putreg(regfz, ord(true)) end
      { set flags for "bad" termination }
      else begin putreg(regfc, ord(true)); putreg(regfz, ord(false)) end

   end else prterr(einvins) { otherwise flag invalid instruction }

end;

{*******************************************************************************

Initialize syscal module

*******************************************************************************}

begin

   fstap := true; { allow system tap calls }

   { clear open program files table }
   for fi := 1 to maxfid do fidtab[fi].sta := fmfree; { set file closed }

   reserve(rsinvied, 0, 0, syscal) { reserve $ed illegal instruction }

end.
rem @echo off
rem
rem compile mpc7400 assembler
rem
rem The files have the following meanings:
rem
rem macdef.pas - Contains the processor specific declarations.
rem macutl.pas - Contains processor specific utilities.
rem opcode.pas - Contains processor specific instruction handlers.
rem

parse macutl=macutl #u=.\,..\as\
parse opcode=opcode #u=.\,..\as\
parse machine=machine #u=.\,..\as\
ce macutl=macutl
ce opcode=opcode
ce machine=machine
ln runfile=c:\pascomp\lib\serlib c:\assm\as\common ..\as\utl ..\as\direct macutl opcode machine c:\pascomp\lib\main ..\as\main c:\pascomp\lib\cap
genpe as7400=runfile c:\windows\system\kernel32 c:\windows\system\user32 c:\windows\system\gdi32 c:\windows\system\winmm/v/sc
del runfile.*
rem copy as7400.exe ..

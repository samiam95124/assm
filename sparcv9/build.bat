rem @echo off
rem
rem compile SPARC V9 assembler
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

rem
rem Its not nice, but right now there are tricks with signed/unsigned numbers
rem that don't work if we enable overflow checking.
rem
ec macutl=macutl/noc/nrc/nac/nclcl
ec opcode=opcode/noc/nrc/nac/nclcl
ec machine=machine/noc/nrc/nac/nclcl

rem
rem Link Windows
rem

ln runfile=c:\ip\windows\i80386\lib\serlib c:\assm\as\common ..\as\utl ..\as\direct macutl opcode machine c:\ip\windows\i80386\lib\main ..\as\asmain c:\ip\windows\i80386\lib\cap/nu
rem ln runfile=c:\ip\windows\i80386\lib\serlib c:\assm\as\common ..\as\utl ..\as\direct macutl opcode machine c:\ip\windows\i80386\lib\main ..\as\asmain c:\ip\windows\i80386\lib\cap #ps=$401000 #ll #lv #lm > as80586.lst
genpe assparcv9=runfile/v/sc
del runfile.*

@echo off
rem
rem Link Linux
rem

rem ln runfile=c:\ip\linux\i80386\lib\serlib c:\assm\as\common ..\as\utl ..\as\direct macutl opcode machine c:\ip\windows\i80386\lib\main ..\as\main c:\ip\linux\i80386\lib\cap
rem genelf assparcv9=runfile/v
rem del runfile.*

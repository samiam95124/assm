@echo off
rem
rem Update the assembler to binary
rem
rem This file writes the assembler in this directory to the IP binaries
rem directory. There are two possible copies of the assembler. One is the
rem assembler specifically named for the machine it assembles for. Another is
rem the generic assembler, "as". This copy is commented out, and should only
rem be enabled if the machine type is active for this host, since the generic
rem assembler command is the one that assembles for the present host machine.
rem

copy as.exe \ip\windows\i80386\bin\as80586.exe
copy as.exe \ip\windows\i80386\bin\as.exe

rem copy as.exe \iprel\windows\i80386\binas80586.exe
rem copy as.exe \iprel\windows\i80386\bin\as.exe

rem copy as \ip\linux\i80386\bin\as80586
rem copy as \ip\linux\i80386\bin\as

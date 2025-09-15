@echo off
rem @echo off
rem
rem Compile a program for use with the serial level library
rem
parse db=db
parse simcpu=simcpu
ce db=db
ce simcpu=simcpu
ln runfile=d:\pascomp\lib\serlib simcpu db d:\pascomp\lib\cap
genpe db=runfile c:\windows\system\kernel32 c:\windows\system\user32 c:\windows\system\gdi32 c:\windows\system\winmm/v
del runfile.*
del db.int
del db.obj
del db.sym

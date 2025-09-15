rem
rem Construct pedump
rem

parse pedump=pedump
ec pedump=pedump/noc

rem
rem perform link for windows
rem

ln runfile=c:\pascomp\windows\serlib c:\pascomp\comlib\strlib c:\pascomp\windows\extlib c:\pascomp\comlib\parlib pedump c:\ip\windows\i80386\lib\cap
genpe pedump=runfile c:\windows\system32\kernel32 c:\windows\system32\user32 c:\windows\system32\gdi32 c:\windows\system32\winmm/v
del runfile.*

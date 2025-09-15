rem
rem Construct pecat
rem

parse pecat=pecat
ec pecat=pecat/noc

rem
rem perform link for windows
rem

ln runfile=c:\ip\windows\i80386\lib\serlib c:\ip\windows\i80386\lib\strlib c:\ip\windows\i80386\lib\extlib c:\ip\windows\i80386\lib\parlib pecat c:\ip\windows\i80386\lib\cap
genpe pecat=runfile c:\windows\system32\kernel32 c:\windows\system32\user32 c:\windows\system32\gdi32 c:\windows\system32\winmm/v
del runfile.*

rem
rem Construct ELF generator
rem

parse genelf=genelf
ce genelf=genelf

rem
rem perform link for windows
rem

ln runfile=c:\ip\windows\i80386\lib\serlib c:\ip\windows\i80386\lib\strlib c:\ip\windows\i80386\lib\extlib genelf c:\ip\windows\i80386\lib\cap
genpe genelf=runfile c:\windows\system\kernel32 c:\windows\system\user32 c:\windows\system\gdi32 c:\windows\system\winmm/v
del runfile.*

rem
rem perform link for linux
rem

ln runfile=c:\ip\linux\i80386\lib\serlib c:\ip\linux\i80386\lib\strlib c:\ip\linux\i80386\lib\extlib genelf c:\ip\linux\i80386\lib\cap
genelf genelf=runfile/v
del runfile.*

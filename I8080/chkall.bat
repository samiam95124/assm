rem
rem This file assembles the allins.asm instruction test file, generates a
rem listing, and compares it to the saved reference file.
rem
.\as allins=allins/l
al allins/e > allins.lst
comp allins.obj allins.cmp

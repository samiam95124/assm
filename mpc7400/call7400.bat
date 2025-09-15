rem
rem This file assembles the all 7400 instruction test file, generates a
rem listing, and compares it to the saved reference file.
rem
as7400 all7400=all7400/l
al all7400/e > all7400.lst
comp all7400.obj all7400.cmp

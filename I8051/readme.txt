                        ASSEMBLER TARGET README FILE

This is an assembler target specific directory. The main assembler files are
located in ../as. This directory creates a machine specific assembler. The
build in the ../as directory should have already have been done.

This is the target specific directory for:

I8051 8 bit microprocessor

The following are files in this directory.

build.bat    - Creates the target specific assembler.
chkall.bat   - Assembles the assembler checker file, and compares the .obj
               created against the reference file, allins.cmp.
update.bat   - Updates the assembler to the IP binaries directory.
clean.bat    - Cleans out all built files, ie., .obj, .sym, etc.
opcdef.pas   - Defines the opcode enumeration type. This is an automatically
               generated file, by hashtab.
opcini.pas   - Gives the initialization for the reserved symbols table. This is
               an automatically generated file, by hashtab.
macdef.pas   - Machine specific definitions file. Contains all definitions needed
               for this target.
macutl.pas   - Machine specific utility routines. Contains the initalization,
               parameter parsing, machine specific expression handling, and other
               routines for the target.
machine.pas  - Defines the rouines used by the general as code in ../as that are
               called to run the assembly. This is the same file for each target.
opcode.pas   - Contains the opcode parsing for this target.
allins.asm   - A list of all instructions, or individual formats for all 
               instructions. This serves as a test for the target specific
               assembler.
allins.cmp   - Contains a reference copy of the object for the all instruction 
               file. This should never or rarely need updating, and should only
               be copied from allins.obj when the complete listing, allins.lst is
               manually verified to be correct.
allins.lst   - Contains the listing file for allins.asm. This gives all of the
               target instructions, and the code they generate. It is manually
               checked for correctness when the assembler target is created.
chrtrn.xlt   - Contains the character transliteration file for the target. This
               is used to encode characters in the binary, a reverse engineering
               countermeasure (one of several typically in use).
def8051.asm  - Defines register names and onboard peripherals in the I8051.
format.txt   - A description of the format of 8051 instructions.

To build the assembler, perform the following.

build  - to build the target specific assembler.
chkall - This will assemble and check the assembler test file. Any miscompares
         must be corrected.
update - To send the final assembler to the general IP binaries directory.

For each new version, the version number should be incremented, and the 
copyright year should be updated as required. This string is found in 
macutl.pas.

The format of a version number is:

<major>.<minor>.<build>

The first two numbers are a general release. The major version number is 
incremented when a release is "sold", or released to the public. It is 0 only
during development, and turns to 1 when the first release occurs.

The minor number is the number of a between offical versions copy, and is used
to distribute fixes.

The last number, the build number, is always two digits, and is incremented each
time the program is built.
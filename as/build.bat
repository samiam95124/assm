@echo off
rem
rem Compile all the generic assembler components.
rem The actual contruction of the assembler is done in
rem the specific processor directory.
rem
rem The assembler generic module has the following format:
rem
rem asmain.pas  - Contains the main startup procedure, initalization,
rem               and overall assembly parsing code.
rem direct.pas  - Contains the generic assembler pseudo-op handling code.
rem utl.pas     - Contains the support procedures for the assembler.
rem basicio.pas - Contains the compiler and system dependent routines.
rem
rem The assembler types are defined in asdef.pas, and the common variables
rem defined in common.pas.
rem
rem The assember processor specific module follows, and is usually of
rem the basic format:
rem
rem ocxxxx.pas  - Contains the processor specific opcode handlers.
rem utxxxx.pas  - Contains the processor specific support routines.
rem
rem The processor specific types are defined in mdxxxx.pas.
rem
rem The layout of the processor specific module, can of course, vary.
rem The assembler only makes two calls to the processor specific module:
rem one to initalize it and one to process an opcode.
rem The Processor specific module can and does call many routines in
rem the generic utl.pas module.
rem 

parse common=common /u=.,..\as,c:\ip\windows\i80386\lib
parse utl=utl /u=.,..\as,c:\ip\windows\i80386\lib
parse direct=direct /u=.,..\as,c:\ip\windows\i80386\lib
parse asmain=asmain /u=.,..\as,c:\ip\windows\i80386\lib

rem
rem Its not nice, but right now there are tricks with signed/unsigned numbers
rem that don't work if we enable overflow checking.
rem

rem
rem Build the charcter translation version.
rem
rem ec common=common/noc/nrc/nac/nclcl/scxt/discm
rem ec utl=utl/noc/nrc/nac/nclcl/scxt/discm
rem ec direct=direct/noc/nrc/nac/nclcl/scxt/discm
rem ec asmain=asmain/noc/nrc/nac/nclcl/scxt/discm

rem
rem Build the "plain" charcter version. Use this ONLY for debugging.
rem
ec common=common/noc/nrc/nac/nclcl
ec utl=utl/noc/nrc/nac/nclcl
ec direct=direct/noc/nrc/nac/nclcl
ec asmain=asmain/noc/nrc/nac/nclcl

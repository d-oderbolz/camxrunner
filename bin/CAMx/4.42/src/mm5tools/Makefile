#------------------------------------------------------------
#            This is the Linux Makefile for some MM5 tools
#            We use the pgf90 compiler
#
#     Modified by Daniel C. Oderbolz <CAMxRunner@psi.ch>
#     for use with the CAMxRunner and its installer.
#
#     Works well in Multiplatform-environments where many
#     machines share the same filesystems
#
#     Syntax is: "make <PLATFORM=string> <DESTINATION=directory>"
#
#     This will make a program for Linux machines using the
#     Portland Group Compiler.
#     Is is recommended to supply a PLATFORM string to be
#     able to tell different binaries from each other.
#
#     Note that binaries will get their name from the environment variable
#     $MACHTYPE instead of $PLATFORM because the environment does not know about
#     pgf77 -tp flags
#
#     Example for a IA32/EM64T processor in 64-bit mode:
#
#        make PLATFORM=p7-64
#
#     If you are unsure about this, do not supply the variable, it will then compile for the 
#     current platform.
#
#     The string will be used as an argument to pgf77,
#     here are acceptable values:
#                  amd64
#                      AMD Opteron or Athlon-64 in 64-bit mode
#
#                  amd64e
#                      AMD Opteron revision E or later, in 64-bit mode;
#                      includes SSE3 instructions
#
#                  athlon
#                      AMD Athlon processor
#
#                  athlonxp
#                      AMD AthlonXP and AthlonMP processor
#
#                  barcelona
#                      AMD Barcelona processor in 32-bit mode
#
#                  barcelona-64
#                      AMD Barcelona processor in 64-bit mode
#
#                  core2
#                      Intel core2 processor in 32-bit mode
#
#                  core2-64
#                      Intel core2 processor in 64-bit mode
#
#                  k7  AMD Athlon processor
#
#                  k8-32
#                      AMD Opteron or Athlon-64 in 32-bit mode
#
#                  k8-64
#                      AMD Opteron or Athlon-64 in 64-bit mode
#
#                  k8-64e
#                      AMD Opteron revision E or later, in 64-bit mode;
#                      includes SSE3 instructions
#
#                  p5  Pentium processor
#
#                  p6  Pentium Pro / Pentium II
#
#                  p7  Pentium 4 processor
#
#                  p7-64
#                      IA32/EM64T processor in 64-bit mode
#
#                  piii
#                      Pentium III processor
#
#                  piv 
#                      Pentium 4 processor
#
#                  px  
#                      blended code generation that will work on any
#                      x86-compatible processor
#
#                  x64 
#                      Single binary where each procedure is optimized for both
#                      the AMD Opteron and the Intel IA32/EM64T in 64-bit mode;
#                      the selection of which optimized copy to execute is made
#                      at run time depending on the machine executing the code.
#
#------------------------------------------------------------

# This is much better than a check for ifndef
# make has an elephants memory...
ifeq ($(strip $(PLATFORM)),)
	PLATFORM = 
	TPOPTION =
else
	TPOPTION = -tp $(PLATFORM)
endif

# Compiler
ifeq ($(strip $(FC)),)
FC=pgf90
endif

ifeq ($(strip $(DESTINATION)),)
	DESTINATION = ../..
endif

FCFLGS = $(TPOPTION) -Mfreeform -Mbyteswapio

all: readv3 v3toasc rplcdatv3 rplcdatv3_const

# This can potentially be simplified, but its all we got now...

readv3: readv3.o 
	$(FC)  -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) readv3.o
	
v3toasc: v3toasc.o 
	$(FC)  -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) v3toasc.o

rplcdatv3: rplcdatv3.o 
	$(FC)  -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) rplcdatv3.o
	
rplcdatv3_const: rplcdatv3_const.o 
	$(FC)  -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) rplcdatv3_const.o

.f.o: 
	$(FC)  -c $(FCFLGS) $< 

clean:
	-rm -f *.o
	-rm -f $(DESTINATION)/readv3-$(HOSTTYPE)
	-rm -f $(DESTINATION)/v3toasc-$(HOSTTYPE)
	-rm -f $(DESTINATION)/rplcdatv3-$(HOSTTYPE)
	-rm -f $(DESTINATION)/rplcdatv3_const-$(HOSTTYPE)
#------------------------------------------------------------
#            This is the Linux Makefile for split_emissions
#            We use the pgf77 compiler
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
#        make FC=pgf77 PLATFORM=p7-64 DESTINATION=../..
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
FC=pgf77
endif

ifeq ($(strip $(DESTINATION)),)
	DESTINATION = ../..
endif

OBJCTS_A = makearea_2006.o 
OBJCTS_P = makepoint_2006.o

# Original
# FLGS = -O2 -tp p6 -pc 64 -Kieee -Mdalign -Mextend -Mnoframe -byteswapio -Wl,-Bstatic
# FLGS = -O2 -g -tp p6 -pc 64 -Kieee -Mdalign -Mextend -Mnoframe -byteswapio -Wl,-Bstatic
# FLGS = -O2 -g $(TPOPTION) -Kieee -Mdalign -Mextend -Mnoframe -byteswapio -Wl,-Bstatic

# The standard we use from CAMxRunner stuff
# FLGS = -O2 $(TPOPTION) -Mbyteswapio -Bstatic

# This way it works, but it is not static...
FLGS = -O2 $(TPOPTION)

all: split_emissions_area split_emissions_point

split_emissions_area : $(OBJCTS_A)
	$(FC) $(FLGS) -o $(DESTINATION)/$@-$(HOSTTYPE) $(OBJCTS_A)
	
split_emissions_point : $(OBJCTS_P)
	$(FC) $(FLGS) -o $(DESTINATION)/$@-$(HOSTTYPE) $(OBJCTS_P)

.f.o :
	$(FC) -c -o $@ $(FLGS) $<

clean :
	-rm *.o
	-rm $(DESTINATION)/split_emissions_area-$(HOSTTYPE)
	-rm $(DESTINATION)/split_emissions_point-$(HOSTTYPE)
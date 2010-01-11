#------------------------------------------------------------
#            This is the Linux Makefile for add_volatility_bins
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
FC=pgf77
endif

ifeq ($(strip $(DESTINATION)),)
	DESTINATION = ../..
endif

# FLGS = -O2 -tp p6 -pc 64 -Kieee -Mdalign -Mextend -Mnoframe -byteswapio -Wl,-Bstatic
# FLGS = -O2 -g -tp p6 -pc 64 -Kieee -Mdalign -Mextend -Mnoframe -byteswapio -Wl,-Bstatic
#FLGS = -O2 -g $(TPOPTION) -Kieee -Mdalign -Mextend -Mnoframe -byteswapio -Wl,-Bstatic

# Defensive set of options
FLGS = -O2 $(TPOPTION) 

OBJCTS = prep.sp.o bswap.cvt.o 

all: add_volatility_bins

add_volatility_bins : $(OBJCTS)
	$(FC) $(FLGS) -o $(DESTINATION)/$@-$(HOSTTYPE) $(OBJCTS)

prep: prep.o bswap.cvt.o
	$(FC) $(FLGS) -o prep prep.o bswap.cvt.o

invprep.sp: invprep.sp.o bswap.cvt.o
	$(FC) $(FLGS) -o invprep.sp invprep.sp.o bswap.cvt.o

prep.sp: prep.sp.o bswap.cvt.o
	$(FC) $(FLGS) -o prep.sp prep.sp.o bswap.cvt.o

prep.point : prep.point.o bswap.cvt.o
	$(FC) $(FLGS) -o prep.point prep.point.o bswap.cvt.o

prep.Jan.lowvol : prep.Jan.lowvol.o bswap.cvt.o
	$(FC) $(FLGS) -o prep.Jan.lowvol prep.Jan.lowvol.o bswap.cvt.o

sum_grid : sum_grid.o bswap.cvt.o lcpgeo.o
	$(FC) $(FLGS) -o sum_grid  sum_grid.o bswap.cvt.o lcpgeo.o

fix.nighttime.no.max: fix.nighttime.no.max.f
	$(FC) $(FLGS) -o fix.nighttime.no.max fix.nighttime.no.max.f

fix.nighttime.no.double: fix.nighttime.no.double.f
	$(FC) $(FLGS) -o fix.nighttime.no.double fix.nighttime.no.double.f

nh3_factor: nh3_factor.f
	$(FC) $(FLGS) -o nh3_factor  nh3_factor.f

emis_factor: emis_factor.f
	$(FC) $(FLGS) -o emis_factor  emis_factor.f


diurnal_nh3: diurnal_nh3.f
	$(FC) $(FLGS) -o diurnal_nh3  diurnal_nh3.f

init_cond: init_cond.f
	$(FC) $(FLGS) -o init_cond init_cond.f

bndr_cond: bndr_cond.f
	$(FC) $(FLGS) -o bndr_cond bndr_cond.f

sum_sources: sum_sources.f
	$(FC) $(FLGS) -o sum_sources sum_sources.f

sumECOC: sumECOC.f
	$(FC) $(FLGS) -o sumECOC sumECOC.f

.f.o :
	$(FC) -c -o $@ $(FLGS) $<

clean :
	-rm -f *.o
	-rm -f $(DESTINATION)/add_volatility_bins-$(HOSTTYPE)
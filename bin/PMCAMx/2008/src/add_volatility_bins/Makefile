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
#     Syntax is: "make <DESTINATION=directory>"
#
#     This will make a program for Linux machines using the
#     Portland Group Compiler.
#
#     Note that binaries will get their name from the environment variable
#     $HOSTTYPE
#------------------------------------------------------------

# Compiler
ifeq ($(strip $(FC)),)
FC=pgf77
endif

ifeq ($(strip $(DESTINATION)),)
	DESTINATION = ../..
endif

# FLGS = -O2 -tp p6 -pc 64 -Kieee -Mdalign -Mextend -Mnoframe -byteswapio -Wl,-Bstatic
# FLGS = -O2 -g -tp p6 -pc 64 -Kieee -Mdalign -Mextend -Mnoframe -byteswapio -Wl,-Bstatic
#FLGS = -O2 -g -Kieee -Mdalign -Mextend -Mnoframe -byteswapio -Wl,-Bstatic

# Defensive set of options
FLGS = -O2

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
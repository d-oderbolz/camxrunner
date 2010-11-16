# Make file for zeroClouds for CAMxRunner

ifeq ($(strip $(DESTINATION)),)
	DESTINATION = ../..
endif

#PGF compiler
FC = pgf90
#32-bit:
#FLGS = -O2 -pc 64 -Mlfs -Mnoframe -byteswapio -Wl,-Bstatic
#64-bit:
FLGS = -O2 -pc 64 -Mlfs -Mnoframe -byteswapio -Bstatic_pgi

LIBS = 

OBJCTS = \
juldate.o \
zeroClouds.o

all: zeroClouds

zeroClouds.o 	: $(OBJCTS)
	zeroClouds.f param.inc fields.inc

zeroClouds: 
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(FLGS) $(OBJCTS)
	
.f.o	:
	$(FC) -c -o $@ $(FLGS) $<


clean:
	rm -f $(OBJCTS) $(DESTINATION)/zeroClouds-$(HOSTTYPE)


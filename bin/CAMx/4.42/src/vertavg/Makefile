#------------------------------------------------------------
#            This is the Linux Makefile for vertavg
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
FC=pgf90
endif

ifeq ($(strip $(DESTINATION)),)
	DESTINATION = ../..
endif

FLGS = -byteswapio -Bstatic -Mbounds

LIBS = 

TARGT =  all

all: vertavg diagnose_pbl

vertavg: vertavg.o
	$(FC) -o $(DESTINATION)/vertavg-$(HOSTTYPE) $(FLGS) vertavg.o $(LIBS)

diagnose_pbl: diagnose_pbl.o
	$(FC) -o $(DESTINATION)/diagnose_pbl-$(HOSTTYPE) $(FLGS) diagnose_pbl.o $(LIBS)

.f.o	:
	$(FC) -c -o $@ $(FLGS) $<

clean :
	-rm -f *.o
	-rm -f $(DESTINATION)/vertavg-$(HOSTTYPE)
	-rm -f $(DESTINATION)/diagnose_pbl-$(HOSTTYPE)
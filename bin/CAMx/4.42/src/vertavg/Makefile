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
FC=pgf77
endif

ifeq ($(strip $(DESTINATION)),)
	DESTINATION = ../..
endif

FLGS = -byteswapio -Bstatic

LIBS = 

TARGT =  all

all: vertavg diagnose_pbl

OBJCTS = \
vertavg.o \
diagnose_pbl.o

$(TARGT): $(OBJCTS)
	$(FC) -o $(DESTINATION)/$(TARGT)-$(HOSTTYPE) $(FLGS) $(OBJCTS) $(LIBS)
.f.o	:
	$(FC) -c -o $@ $(FLGS) $<

clean :
	-rm -f *.o
	-rm -f $(DESTINATION)/$(TARGT)-$(HOSTTYPE)
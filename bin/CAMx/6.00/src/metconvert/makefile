#------------------------------------------------------------
#            This is the Linux Makefile for metconvert
#            We use the pgf77 compiler
#
#     Modified by Giancarlo Ciarelli <CAMxRunner@psi.ch>
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


FLGS = -O2 -Kieee -Mbyteswapio -Mextend

.SUFFIXES: .o .f90

LIBS = 



OBJCTS = \
metconvert.o

metconvert: $(OBJCTS)
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(FLGS) $(OBJCTS) $(LIBS) 
.f90.o:
	$(FC) -c -o $@ $(FLGS) $<

clean:
	rm -f *.o
	rm -f $(DESTINATION)/metconvert-$(HOSTTYPE)
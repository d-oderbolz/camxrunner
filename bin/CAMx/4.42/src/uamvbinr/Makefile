#------------------------------------------------------------
#            This is the Linux Makefile for uamvbinr
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

FCFLGS = -O3 -Mbyteswapio -Bstatic
OBJCTS = \
uamvbinr.o \
mdfbin.o \
mixhtbin.o \
itempbin.o \
iuh2obin.o \
isurfbin.o \
iterrbin.o \
ikvbin.o \
outccbin.o \
outfcbin.o \
outfabin.o

uamvbinr: $(OBJCTS) 
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(OBJCTS) $(FCFLGS)

.f.o: 
	$(FC) -c $(FCFLGS) $< 
	
clean:
	-rm *.o
	-rm $(DESTINATION)/uamvbinr-$(HOSTTYPE)


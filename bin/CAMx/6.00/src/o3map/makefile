#------------------------------------------------------------
#            This is the Linux Makefile for o3map
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
FC=pgf77
endif

ifeq ($(strip $(DESTINATION)),)
	DESTINATION = ../..
endif


# FLGS =  -byteswapio -Wl,-Bstatic  -g

FLGS =  -Mbyteswapio -Bstatic

LIBS = 



OBJCTS = \
TOMSprep.o \
o3map.o \
lcpgeo.o \
pspgeo.o \
utmgeo.o \
juldate.o \
strlen.o

o3map: $(OBJCTS)
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(FLGS) $(OBJCTS) $(LIBS) 
.f.o	:
	$(FC) -c -o $@ $(FLGS) $<

clean:
	rm -f *.o
	rm -f $(DESTINATION)/o3map-$(HOSTTYPE)
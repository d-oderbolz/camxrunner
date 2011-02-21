#------------------------------------------------------------
#            This is the Linux Makefile for camxpost
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

FLGS = -byteswapio

LIBS = 

all: obscat combine2 camxstat avgcat camxpost extstat

obscat: obscat.o
	$(FC) -o $(DESTINATION)/obscat-$(HOSTTYPE) $(FLGS) obscat.o $(LIBS)
	
combine2: combine2.o
	$(FC) -o $(DESTINATION)/combine2-$(HOSTTYPE) $(FLGS) combine2.o $(LIBS)
	
camxstat: camxstat.o
	$(FC) -o $(DESTINATION)/camxstat-$(HOSTTYPE) $(FLGS) camxstat.o $(LIBS)
	
avgcat: avgcat.o
	$(FC) -o $(DESTINATION)/avgcat-$(HOSTTYPE) $(FLGS) avgcat.o $(LIBS)
	
camxpost: camxpost.o
	$(FC) -o $(DESTINATION)/camxpost-$(HOSTTYPE) $(FLGS) camxpost.o $(LIBS)
	
extstat: extstat.o
	$(FC) -o $(DESTINATION)/extstat-$(HOSTTYPE) $(FLGS) extstat.o $(LIBS)
	
.f.o	:
	$(FC) -c -o $@ $(FLGS) $<

clean :
	-rm -f *.o
	-rm -f $(DESTINATION)/obscat-$(HOSTTYPE)
	-rm -f $(DESTINATION)/combine2-$(HOSTTYPE)
	-rm -f $(DESTINATION)/camxstat-$(HOSTTYPE)
	-rm -f $(DESTINATION)/avgcat-$(HOSTTYPE)
	-rm -f $(DESTINATION)/camxpost-$(HOSTTYPE)
	-rm -f $(DESTINATION)/extstat-$(HOSTTYPE)
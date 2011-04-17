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

FCFLGS = -byteswapio -g

LIBS = 

all: obscat combine2 camxstat avgcat camxpost extstat

obscat: 
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) $@.f
	
combine2: 
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) $@.f
	
camxstat: 
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) $@.f
	
avgcat: 
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) $@.f
	
camxpost: 
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) $@.f
	
extstat: 
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) $@.f

clean :
	-rm -f *.o
	-rm -f $(DESTINATION)/obscat-$(HOSTTYPE)
	-rm -f $(DESTINATION)/combine2-$(HOSTTYPE)
	-rm -f $(DESTINATION)/camxstat-$(HOSTTYPE)
	-rm -f $(DESTINATION)/avgcat-$(HOSTTYPE)
	-rm -f $(DESTINATION)/camxpost-$(HOSTTYPE)
	-rm -f $(DESTINATION)/extstat-$(HOSTTYPE)
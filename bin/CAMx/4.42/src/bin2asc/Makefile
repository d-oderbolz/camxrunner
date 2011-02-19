#------------------------------------------------------------
#            This is the Linux Makefile for asc2bin/bin2asc
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

FCFLGS = -byteswapio -Bstatic

all: bin2asc asc2bin

bin2asc: 
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) $@.f
	
asc2bin:
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) $@.f
	
clean:
	-rm *.o
	-rm -f $(DESTINATION)/bin2asc-$(HOSTTYPE) $(DESTINATION)/asc2bin-$(HOSTTYPE)


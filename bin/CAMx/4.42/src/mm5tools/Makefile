#------------------------------------------------------------
#            This is the Linux Makefile for some MM5 tools
#            We use the pgf90 compiler
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

FCFLGS = -Mfreeform -Mbyteswapio

all: readv3 v3toasc rplcdatv3 rplcdatv3_const

# This can potentially be simplified, but its all we got now...

readv3: readv3.o 
	$(FC)  -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) readv3.o
	
v3toasc: v3toasc.o 
	$(FC)  -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) v3toasc.o

rplcdatv3: rplcdatv3.o 
	$(FC)  -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) rplcdatv3.o
	
rplcdatv3_const: rplcdatv3_const.o 
	$(FC)  -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) rplcdatv3_const.o

.f.o: 
	$(FC)  -c $(FCFLGS) $< 

clean:
	-rm -f *.o
	-rm -f $(DESTINATION)/readv3-$(HOSTTYPE)
	-rm -f $(DESTINATION)/v3toasc-$(HOSTTYPE)
	-rm -f $(DESTINATION)/rplcdatv3-$(HOSTTYPE)
	-rm -f $(DESTINATION)/rplcdatv3_const-$(HOSTTYPE)
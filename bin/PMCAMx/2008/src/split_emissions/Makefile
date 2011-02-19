#------------------------------------------------------------
#            This is the Linux Makefile for split_emissions
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

OBJCTS_A = makearea_2006.o 
OBJCTS_P = makepoint_2006.o

# Original
# FLGS = -O2 -tp p6 -pc 64 -Kieee -Mdalign -Mextend -Mnoframe -byteswapio -Wl,-Bstatic
# FLGS = -O2 -g -tp p6 -pc 64 -Kieee -Mdalign -Mextend -Mnoframe -byteswapio -Wl,-Bstatic
# FLGS = -O2 -g -Kieee -Mdalign -Mextend -Mnoframe -byteswapio -Wl,-Bstatic

# The standard we use from CAMxRunner stuff
# FLGS = -O2 -Mbyteswapio -Bstatic

# This way it works, but it is not static...
FLGS = -O2 

all: split_emissions_area split_emissions_point

split_emissions_area : $(OBJCTS_A)
	$(FC) $(FLGS) -o $(DESTINATION)/$@-$(HOSTTYPE) $(OBJCTS_A)
	
split_emissions_point : $(OBJCTS_P)
	$(FC) $(FLGS) -o $(DESTINATION)/$@-$(HOSTTYPE) $(OBJCTS_P)

.f.o :
	$(FC) -c -o $@ $(FLGS) $<

clean :
	-rm *.o
	-rm $(DESTINATION)/split_emissions_area-$(HOSTTYPE)
	-rm $(DESTINATION)/split_emissions_point-$(HOSTTYPE)
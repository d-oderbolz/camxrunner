#------------------------------------------------------------
#            This is the Linux Makefile for airconv
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
aircon.o \
airconv.o \
avgcon.o \
avgcns.o \
bndcon.o \
checon.o \
dfbcon.o \
emicon.o \
metcon.o \
ptscon.o \
regcon.o \
simcon.o \
tercon.o \
tmpcon.o \
tmpcn3.o \
tpccon.o \
wndcon.o 

airconv: $(OBJCTS) 
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) $(OBJCTS)

.f.o: 
	$(FC) -c $(FCFLGS) $< 

clean:
	-rm -f *.o
	-rm -f $(DESTINATION)/airconv-$(HOSTTYPE)

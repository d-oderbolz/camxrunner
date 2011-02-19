#------------------------------------------------------------
#            This is the Linux Makefile for airascii
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
airascii.o \
aircon.o \
avgcon.o \
avgcns.o \
avgsrf.o \
avgstf.o \
avgtim.o \
bndcon.o \
bndfrm.o \
checon.o \
datavg.o \
dfbcon.o \
emicon.o \
metcon.o \
ptscon.o \
regcon.o \
simcon.o \
tercon.o \
terfrm.o \
tmpcon.o \
tmpcn3.o \
tpccon.o \
wndcon.o \
wndcnd.o \
averge.o

airascii: $(OBJCTS) 
	$(FC)  -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) $(OBJCTS)

.f.o: 
	$(FC)  -c $(FCFLGS) $< 

wndcnd.o: wndcnd.f winds.cmd
	$(FC)  -c $(FCFLGS) wndcnd.f

averge.o: averge.f winds.cmd
	$(FC)  -c $(FCFLGS) averge.f

clean:
	-rm *.o
	-rm $(DESTINATION)/airascii-$(HOSTTYPE)
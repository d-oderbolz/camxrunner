#------------------------------------------------------------
#            This is the Linux Makefile for uamvascii
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
uamvascii.o \
mdfcon.o \
mixhtcon.o \
itempcon.o \
iuh2ocon.o \
isurfcon.o \
iterrcon.o \
ikvcon.o \
outcccon.o \
outfccon.o \
outfacon.o

uamvascii: $(OBJCTS) 
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(OBJCTS) $(FCFLGS)

.f.o: 
	$(FC) -c $(FCFLGS) $< 

clean:
	-rm *.o
	-rm $(DESTINATION)/uamvascii-$(HOSTTYPE)
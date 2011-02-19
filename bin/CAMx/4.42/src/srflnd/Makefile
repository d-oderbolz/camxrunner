#------------------------------------------------------------
#            This is the Linux Makefile for srflnd
#            We use the pgf90 compiler (exit)
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

FCFLGS =  -O3 -Mbyteswapio -Bstatic

LIBS = 

OBJCTS = \
strlen.o \
srflnd.o

srflnd: $(OBJCTS)
	$(FC) -o $(DESTINATION)/$@-$(HOSTTYPE) $(FCFLGS) $(OBJCTS) $(LIBS)
	
.f.o	:
	$(FC) -c -o $@ $(FCFLGS) $<

clean:
	-rm -f *.o
	-rm -f $(DESTINATION)/srflnd-$(HOSTTYPE)
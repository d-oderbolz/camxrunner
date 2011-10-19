#------------------------------------------------------------
#            This is the Linux Makefile for tuv
#            We use the pgf77 compiler.
#            Check the Makefile.ENVIREN for more options.
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

INCLUDES = params 

# loading routines to include
RSUBS =        rn.o

USE_INCL = tuv.o \
           gridck.o gridw.o gridz.o \
           rdetfl.o read1.o read2.o \
           rdo2xs.o rdso2xs.o rdno2xs.o rdo3xs.o \
           pchem.o $(RSUBS) \
           pbiol1.o  \
	   sjo2.o schu.o lymana.o \
           setair.o settmp.o \
	   seto2.o setozo.o setno2.o setso2.o \
           setaer.o setcld.o setalb.o \
           sphers.o rtlink.ps2str.o ps2str.o \
           rtlink.psndo.o psndo.o

FOBJS = addpnt.o zero1.o zero2.o inter1.o inter2.o inter3.o inter4.o fsum.o           \
        futr.o fery.o fchap.o zenith.o sundis.o refrac.o

# Setings for pgf77
FCFLGS = -Kieee -Bstatic_pgi 

all:    tuv tuvcompr

tuv:   $(FOBJS) $(USE_INCL) 
		$(FC) $(FCFLGS) $(FOBJS) $(USE_INCL)  -o $(DESTINATION)/$@-$(HOSTTYPE)



tuvcompr:		tuvcompr.o 
		$(FC) $(FCFLGS) tuvcompr.o -o $(DESTINATION)/$@-$(HOSTTYPE)

$(USE_INCL):	$(INCLUDES)

.f.o: 
	$(FC)  -c $(FCFLGS) $< 

clean:
	-rm -f *.o
	-rm -f $(DESTINATION)/tuv-$(HOSTTYPE)
	-rm -f $(DESTINATION)/tuvcompr-$(HOSTTYPE)
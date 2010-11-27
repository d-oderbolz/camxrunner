#------------------------------------------------------------
#            This is the Linux Makefile for ext_ipr
#            We use the pgf77 compiler
#
#     Modified by Daniel C. Oderbolz <CAMxRunner@psi.ch>
#     for use with the CAMxRunner and its installer.
#
#     Works well in Multiplatform-environments where many
#     machines share the same filesystems
#
#     Syntax is: "make <PLATFORM=string> <DESTINATION=directory>"
#
#     This will make a program for Linux machines using the
#     Portland Group Compiler.
#     Is is recommended to supply a PLATFORM string to be
#     able to tell different binaries from each other.
#
#     Note that binaries will get their name from the environment variable
#     $MACHTYPE instead of $PLATFORM because the environment does not know about
#     pgf77 -tp flags
#
#     Example for a IA32/EM64T processor in 64-bit mode:
#
#        make PLATFORM=p7-64
#
#     If you are unsure about this, do not supply the variable, it will then compile for the 
#     current platform.
#
#     The string will be used as an argument to pgf77,
#     check the compilers manual for acceptable values.
#------------------------------------------------------------


# This is much better than a check for ifndef
# make has an elephants memory...
ifeq ($(strip $(PLATFORM)),)
	PLATFORM = 
	TPOPTION =
else
	TPOPTION = -tp $(PLATFORM)
endif

# Compiler
ifeq ($(strip $(FC)),)
FC=pgf77
endif

ifeq ($(strip $(DESTINATION)),)
	DESTINATION = ../..
endif

FLGS = $(TPOPTION) -byteswapio -Bstatic

LIBS = 

TARGT =  ext_ipr

OBJCTS = \
ext_ipr.o

$(TARGT): $(OBJCTS)
	$(FC) -o $(DESTINATION)/$(TARGT)-$(HOSTTYPE) $(FLGS) $(OBJCTS) $(LIBS)
.f.o	:
	$(FC) -c -o $@ $(FLGS) $<

clean :
	-rm -f *.o
	-rm -f $(DESTINATION)/$(TARGT)-$(HOSTTYPE)
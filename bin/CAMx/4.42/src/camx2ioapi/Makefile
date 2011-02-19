#------------------------------------------------------------
#            This is the Linux Makefile for CAMx2IOAPI
#            We use the pgf77 compiler
#
#   NOTE: This Makefile assumes the NetCDF and IO/API libraries
#         have already been built using CAMxRunner -I
#
#   DIRECTORIES:
#
#     IOAPI_INC  - I/O-API include file directory
#     IOAPI_LIB  - I/O-API library file directory
#     NETCDF_LIB - NetCDF library file directory
#
#     Modified by Daniel C. Oderbolz <CAMxRunner@psi.ch>
#     for use with the CAMxRunner and its installer.
#
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


IOAPI_INC  = /models/CMAQ/lib/ioapi_3/ioapi/fixed_src
IOAPI_LIB  = /models/CMAQ/lib/ioapi_3/Linux2_x86pg
NETCDF_LIB = /models/CMAQ/lib/netcdf-3.6.0-p1/lib

######################################################################

FC  = pgf90
OPT = -O2 -pc 64 -Mdalign -Mnoframe -byteswapio -Mlfs -Wl,-Bstatic
LIB = -L$(IOAPI_LIB) -lioapi -L$(NETCDF_LIB) -lnetcdf
INC = -I$(IOAPI_INC)
EXE = camx2ioapi


.PHONY: all clean

all : $(EXE)


$(EXE) : $(EXE).o
	$(FC) $(OPT) -o $@ $^ $(LIB)

.f.o :
	$(FC) $(INC) $(OPT) -c $<

clean :
	-rm -f *.o



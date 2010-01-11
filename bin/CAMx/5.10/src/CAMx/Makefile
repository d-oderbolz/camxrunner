#-------------------------------------------------------------------------------
#            This is the Makefile for CAMx v5.10
#
#     Syntax is: "make platform <DOMAIN=string> <MPI=true> <HDF=true>"
#
#     The Makefile will generate a CAMx exectuable program called:
#
#     CAMx.<string>.MPI_configruation.platform
#
#     where MPI_configuration is either "MPI" or "noMPI" according to
#     the MPI flag.  Generally, to compile CAMx you do not need to use
#     the optional DOMAIN keyword. For example:
#
#     make pg_linux
#
#     will make a program for Linux machines using the Portland Group
#     Compiler and using the default include file 
#     "./Includes/camx.prm.v5.10".  The executable will be named:
#
#     "CAMx.v5.10.noMPI.linux".
#
#     In this case, OMP, MPI, and HDF would be disabled.
#
#     As another example:
#
#     make pg_linuxomp DOMAIN=mydomain MPI=true HDF=true
#
#     will make a program for Linux machines using the
#     Portland Group Compiler and using the include file
#     "./Includes/camx.prm.mydomain" for the domain parameters.
#     The executable will be named: 
#
#     CAMx.mydomain.MPI.linuxomp
#
#     In this case, the CAMx exectuable would be able to run both
#     MPI and OMP and would be able to output HDF5 files.
#
#-------------------------------------------------------------------------------
#

.SUFFIXES: .c .f .o .f90 .F90

#ifndef DOMAIN
   DOMAIN = v5.10
#endif

#ifndef MPI
   MPI = false
#endif

#ifndef HDF
   HDF = false
#endif

CAMx      = ./CAMx
CMC       = ./CMC
CF_AERO   = ./CF_AERO
CMU_AERO  = ./CMU_AERO
SOAP      = ./SOAP
HG        = ./HG
PIG       = ./PiG
IOBIN     = ./IO_bin
IOHDF     = ./IO_hdf
OSAT      = ./OSAT
DDM       = ./DDM
PA        = ./PA
RTRAC     = ./RTRAC
CAMx_INC  = ./Includes
MOD_SRC   = ./Mod_src
MOD_DIR   = ./Modules
CAMX_MPI  = ./MPI

ifeq ($(MPI),true)
    MPI_INC   = /usr/local/mpich/include
    MPI_LIBS =  -lparlib \
         -lmpich
    TARGT_STRING = MPI
else
    MPI_INC   = ./Dummy
    DUM_C = Dummy/mpi_dummy_c.o
    DUM_F = Dummy/mpi_dummy.o
    TARGT_STRING = noMPI
endif

ifeq ($(HDF),true)
   HDF_LIB  = -lhdf5 -lz
   HDF_INC  = /usr/local/include/hdf5
   DEFHDF   = -DUSEHDF
   INCLUDES = -I$(CAMx_INC) -I$(MPI_INC) -I$(IOHDF) -I$(HDF_INC)
   HDF_OBJ  = \
              $(IOHDF)/camx_includes_hdf.o \
              $(IOHDF)/rams_head.o \
              $(IOHDF)/hdf5_utils.o \
              $(IOHDF)/hdf5_f2c.o \
              $(IOHDF)/hdf5_file_out.o
   LIBS     = $(MPI_LIBS) $(HDF_OBJ) $(HDF_LIB)
else
   HDF_LIB  =
   HDF_INC  =
   DEFHDF   =
   HDF_OBJ  = $(IOHDF)/camx_includes_hdf.o \
              $(IOHDF)/hdf5_file_out.o
   LIBS     = $(MPI_LIBS) $(HDF_OBJ)
   INCLUDES = -I$(CAMx_INC) -I$(MPI_INC)
endif

default:
	@echo '-----------------------------------------------------------------'
	@echo 'This is the Makefile for CAMx v5.10'
	@echo ' '
	@echo 'Syntax is: "make platform <DOMAIN=string> <MPI=true> <HDF=true>"'
	@echo ' '
	@echo 'Acceptable platforms are:'
	@echo '    dec         -- configure for DEC/Alpha platform'
	@echo '    pg_linux    -- configure for Linux using pgf90'
	@echo '    pg_linuxomp -- configure for Linux using pgf90 w/OMP'
	@echo '    i_linux     -- configure for Linux using ifort'
	@echo '    i_linuxomp  -- configure for Linux using ifort w/OMP'
	@echo '    ab_linux    -- configure for MAC OSX using ABSoft'
	@echo '    sgi         -- configure for SGI platform'
	@echo '    sgiomp      -- configure for SGI platform w/OMP'
	@echo '    sun         -- configure for SUN platform'
	@echo '    hp          -- configure for HP platform'
	@echo '    ibm         -- configure for IBM platform'
	@echo ' '
	@echo 'DOMAIN=string points the make utility at the specific'
	@echo 'CAMx parameters file called "./Includes/camx.prm.string"'
	@echo 'which you have created for your specific CAMx application.'
	@echo 'If DOMAIN is not set on the command line, the default'
	@echo 'CAMx parameters file will be used: "Includes/camx.prm.v5.10"'
	@echo ' '
	@echo 'MPI=true will compile CAMx to include MPI so that the model'
	@echo 'can be run in parallel on distributed memory systems.  MPI'
	@echo 'can be run in tandem with OMP parallelization on shared memory'
	@echo 'systems. MPI requires that the MPICH utility has been installed'
	@echo 'on the system that is running CAMx.'
	@echo ' '
	@echo 'HDF=true will compile CAMx to include HDF5 I/O libraries.'
	@echo 'This requires that HDF5 libraries have been installed and'
	@echo 'built on the machine that is running this Makefile.  You'
	@echo 'should check that the variables HDF_LIB and HDF_INC in the'
	@echo 'Makefile are set correctly.  If HDF is not set on the'
	@echo 'command line, CAMx will not be able to write HDF5 files.'
	@echo ' '
	@echo '-----------------------------------------------------------------'
	@echo ' '

dec:
	@rm -f $(CAMx_INC)/camx.prm
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make modules \
	   FC=f90 \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="$(INCLUDES) -convert big_endian -O3 -align dcommons"
	make hdflib \
	   HDF=$(HDF) \
	   FC=f90 \
	   CC=cc \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="$(INCLUDES) -DALPHA $(DEFHDF) -convert big_endian -O3 -align dcommons"
	make model \
	   FC=f90 \
	   TARGT=CAMx.$(DOMAIN).$(TARGT_STRING).dec \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -I$(MPI_INC) -convert big_endian -O3 -align dcommons"


sgi:
	@rm -f $(CAMx_INC)/camx.prm
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make modules \
	   FC=f90 \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-O2 -Wf,-I$(CAMx_INC) -I$(MPI_INC)"
	make hdflib \
	   HDF=$(HDF) \
	   FC=f90 \
	   CC=cc \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-DSGI $(DEFHDF) -O2 -Wf,$(INCLUDES)"
	make model \
	   FC=f90 \
	   TARGT=CAMx.$(DOMAIN).$(TARGT_STRING).sgi \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-O2 -Wf,-I$(CAMx_INC) -I$(MPI_INC)"

sgiomp:
	@rm -f $(CAMx_INC)/camx.prm
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make modules \
	   FC=f90 \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-mp -Wf,-I$(CAMx_INC) -I$(MPI_INC)"
	make hdflib \
	   HDF=$(HDF) \
	   FC=f90 \
	   CC=cc \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-DSGI $(DEFHDF) -mp -Wf,$(INCLUDES)"
	make model \
	   FC=f90 \
	   TARGT=CAMx.$(DOMAIN).$(TARGT_STRING).sgiomp \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-mp -Wf,-I$(CAMx_INC) -I$(MPI_INC)"

sun:
	@rm -f $(CAMx_INC)/camx.prm
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make modules \
	   FC=f90 \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -I$(MPI_INC) -Nq900 -fast"
	make hdflib \
	   HDF=$(HDF) \
	   FC=f90 \
	   CC=cc \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="$(INCLUDES) -DSUN $(DEFHDF) -Nq900 -fast"
	make model \
	   FC=f90 \
	   TARGT=CAMx.$(DOMAIN).$(TARGT_STRING).sun \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -I$(MPI_INC) -Nq900 -fast"

hp:
	@rm -f $(CAMx_INC)/camx.prm
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make modules \
	   FC=f90 \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -I$(MPI_INC) -O +U77 +FPVZODu"
	make hdflib \
	   HDF=$(HDF) \
	   FC=f90 \
	   CC=cc \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="$(INCLUDES) -DHP $(DEFHDF) -O +U77 +FPVZODu"
	make model \
	   FC=f90 \
	   TARGT=CAMx.$(DOMAIN).$(TARGT_STRING).hp \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -I$(MPI_INC) -O +U77 +FPVZODu"

ibm:
	@rm -f $(CAMx_INC)/camx.prm
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make modules \
	   FC=xlf \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -I$(MPI_INC) -O2 -brename:.flush,.flush_ -brename:.etime,.etime_ -brename:.dtime,.dtime_ -brename:.fdate,.fdate_"
	make hdflib \
	   HDF=$(HDF) \
	   FC=xlf \
	   CC=cc \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="$(INCLUDES) -DIBM $(DEFHDF) -O2 -brename:.flush,.flush_ -brename:.etime,.etime_ -brename:.dtime,.dtime_ -brename:.fdate,.fdate_"
	make model \
	   FC=xlf \
	   TARGT=CAMx.$(DOMAIN).$(TARGT_STRING).ibm \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -I$(MPI_INC) -O2 -brename:.flush,.flush_ -brename:.etime,.etime_ -brename:.dtime,.dtime_ -brename:.fdate,.fdate_"

pg_linux:
	@rm -f $(CAMx_INC)/camx.prm
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make modules \
	   FC=pgf90 \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="$(INCLUDES) -O2 -pc 64 -Mdalign -mcmodel=medium -Mextend -Mnoframe -byteswapio"
	make hdflib \
	   HDF=$(HDF) \
	   FC=pgf90 \
	   CC=gcc \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="$(INCLUDES) -DPC_LINUX1 $(DEFHDF) -O2 -pc 64 -mcmodel=medium -Mdalign -Mextend -Mnoframe -byteswapio"
	make model \
	   FC=pgf90 \
	   TARGT=CAMx.$(DOMAIN).$(TARGT_STRING).pg_linux \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="$(INCLUDES) -O2 -pc 64 -mcmodel=medium -Mdalign -Mextend -Mnoframe -byteswapio"

pg_linuxomp:
	@rm -f $(CAMx_INC)/camx.prm
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make modules \
	   FC=pgf90 \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -I$(MPI_INC) -mp -O2 -pc 64 -Mconcur=nonuma -mcmodel=medium -Mdalign -Mextend -Mnoframe -byteswapio "
	make hdflib \
	   HDF=$(HDF) \
	   FC=pgf90 \
	   CC=gcc \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="$(INCLUDES) -DPC_LINUX1 $(DEFHDF) -O2 -pc 64 -Mconcur=nonuma -mcmodel=medium -Mdalign -Mextend -Mnoframe -byteswapio"
	make model \
	   FC=pgf90 \
	   TARGT=CAMx.$(DOMAIN).$(TARGT_STRING).pg_linuxomp \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -I$(MPI_INC) -mp -O2 -pc 64 -Mconcur=nonuma -mcmodel=medium -Mdalign -Mextend -Mnoframe -byteswapio"

i_linux:
	@rm -f $(CAMx_INC)/camx.prm
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make modules \
	   FC=ifort \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -I$(MPI_INC) -O2 -align dcommons -extend_source -convert big_endian -lpthread"
	make hdflib \
	   HDF=$(HDF) \
	   FC=ifort \
	   CC=gcc \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="$(INCLUDES) -DPC_LINUX1 $(DEFHDF) -O2 -align dcommons -extend_source -convert big_endian -lpthread"
	make model \
	   FC=ifort \
	   TARGT=CAMx.$(DOMAIN).$(TARGT_STRING).i_linux \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -I$(MPI_INC) -O2 -align dcommons -extend_source -convert big_endian -lpthread"

i_linuxomp:
	@rm -f $(CAMx_INC)/camx.prm
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make modules \
	   FC=ifort \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -I$(MPI_INC) -openmp -O2 -align dcommons -extend_source -convert big_endian -ipo"
	make hdflib \
	   HDF=$(HDF) \
	   FC=ifort \
	   CC=gcc \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="$(INCLUDES) -DPC_LINUX1 $(DEFHDF) -O2 -align dcommons -extend_source -convert big_endian -ipo"
	make model \
	   FC=ifort \
	   TARGT=CAMx.$(DOMAIN).$(TARGT_STRING).i_linuxomp \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -I$(MPI_INC) -openmp -O2 -align dcommons -extend_source -convert big_endian -ipo"

ab_linux:
	@rm -f $(CAMx_INC)/camx.prm
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make modules \
	   FC=ifort \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -V -O3 -m64 -N11 -YEXT_NAMES=LCS -YEXT_SFX=_ -s -YCFRL=1 -W132 -lU77 "
	make hdflib \
	   HDF=$(HDF) \
	   FC=f90 \
	   CC=cc \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -I$(MPI_INC) $(INCLUDES) -DPC_LINUX1 $(DEFHDF) -O3 -V -m64 -N11 -YEXT_NAMES=LCS -YEXT_SFX=_ -s -YCFRL=1 -W132 -lU77 "
	make model \
	   FC=ifort \
	   TARGT=CAMx.$(DOMAIN).$(TARGT_STRING).ab_linux \
           MODULES="-I${MOD_DIR} -module ${MOD_DIR}" \
	   FLGS="-I$(CAMx_INC) -V -O3 -m64 -N11 -YEXT_NAMES=LCS -YEXT_SFX=_ -s -YCFRL=1 -W132 -lU77 "

clean:	
	rm -f $(OBJCTS) $(MOD_OBJCTS) $(IOHDF)/*.o Dummy/*.o *.mod $(MOD_DIR)/*.mod $(CAMX_MPI)/*.o *.a
	cd MPI/util; make clean


linux:
	@echo ' '
	@echo '---------------------------------------------------------'
	@echo 'This version of the CAMx makefile supports more than'
	@echo 'one compiler on the Linux platform.'
	@echo 'Acceptable keywords for Linux are:'
	@echo '          pg_linux    -- configure for Linux using pgf90'
	@echo '          i_linux     -- configure for Linux using ifort'
	@echo '          ab_linux    -- configure for Linux using ABSOFT'
	@echo '---------------------------------------------------------'
	@echo ' '

linuxomp:
	@echo ' '
	@echo '---------------------------------------------------------'
	@echo 'This version of the CAMx makefile supports more than'
	@echo 'one compiler on the Linux OMP platform.'
	@echo 'Acceptable keywords for OMP on Linux are:'
	@echo '          pg_linuxomp  -- configure for Linux using pgf90 w/OMP'
	@echo '          i_linuxomp   -- configure for Linux using ifort w/OMP'
	@echo '---------------------------------------------------------'
	@echo ' '

MOD_OBJCTS = \
$(MOD_SRC)/camx_includes.o \
$(MOD_SRC)/grid_dims_mod.o \
$(MOD_SRC)/node_mod.o \
$(MOD_SRC)/ahomap.o \
$(MOD_SRC)/bndary.o \
$(MOD_SRC)/camxcom.o \
$(MOD_SRC)/camxfld.o \
$(MOD_SRC)/chmstry.o \
$(MOD_SRC)/filunit.o \
$(MOD_SRC)/grid.o \
$(MOD_SRC)/grid_nodes.o \
$(MOD_SRC)/lsbox.o \
$(MOD_SRC)/master_mod.o \
$(MOD_SRC)/pigsty.o \
$(MOD_SRC)/ptemiss.o \
$(MOD_SRC)/procan.o \
$(MOD_SRC)/rtracchm.o \
$(MOD_SRC)/rtcmcchm.o \
$(MOD_SRC)/tracer.o

OBJCTS = \
$(CAMx)/CAMx.o \
$(CAMx)/aerochem_cf.o \
$(CAMx)/aerochem_cmu.o \
$(CAMx)/aeroset.o \
$(CAMx)/aggdep.o \
$(CAMx)/aggr00.o \
$(CAMx)/aggreg.o \
$(CAMx)/ahoprep.o \
$(CAMx)/ahoz_upd.o \
$(CAMx)/average.o \
$(CAMx)/avgall.o \
$(CAMx)/bc1grd.o \
$(CAMx)/bcmodfy.o \
$(CAMx)/bndry_updt.o \
$(CAMx)/caldate.o \
$(CAMx)/camxerr.o \
$(CAMx)/chem10.o \
$(CAMx)/chemdriv.o \
$(CAMx)/chemrxn.o \
$(CAMx)/chmdat.o \
$(CAMx)/chrtime.o \
$(CAMx)/cpivot.o \
$(CAMx)/cvtdate.o \
$(CAMx)/cvtwind.o \
$(CAMx)/dateerr.o \
$(CAMx)/depsmry.o \
$(CAMx)/diffus.o \
$(CAMx)/dlsode.o \
$(CAMx)/drydep.o \
$(CAMx)/ebisolv.o \
$(CAMx)/emassign.o \
$(CAMx)/emiss.o \
$(CAMx)/emistrns.o \
$(CAMx)/emiss_updt.o \
$(CAMx)/expndlay.o \
$(CAMx)/exptbl.o \
$(CAMx)/fgavrg.o \
$(CAMx)/finwind.o \
$(CAMx)/getdelt.o \
$(CAMx)/getunit.o \
$(CAMx)/getznth.o \
$(CAMx)/grdprep.o \
$(CAMx)/hadvbot.o \
$(CAMx)/hadvppm.o \
$(CAMx)/henryfnc.o \
$(CAMx)/iassgn2d.o \
$(CAMx)/iehsolv.o \
$(CAMx)/iniptr.o \
$(CAMx)/initnml.o \
$(CAMx)/interp2d.o \
$(CAMx)/interpv.o \
$(CAMx)/intrpcnc.o \
$(CAMx)/intrpdat.o \
$(CAMx)/istrln.o \
$(CAMx)/jstlft.o \
$(CAMx)/juldate.o \
$(CAMx)/khetero.o \
$(CAMx)/khorz.o \
$(CAMx)/kphoto.o \
$(CAMx)/ktherm.o \
$(CAMx)/lcpgeo.o \
$(CAMx)/linpac.o \
$(CAMx)/lscall.o \
$(CAMx)/lsode.o \
$(CAMx)/luassgn.o \
$(CAMx)/massum.o \
$(CAMx)/micromet.o \
$(CAMx)/nesting.o \
$(CAMx)/nstprep.o \
$(CAMx)/parntchd.o \
$(CAMx)/plumeris.o \
$(CAMx)/pntprep.o \
$(CAMx)/pspgeo.o \
$(CAMx)/raddrivr.o \
$(CAMx)/rassgn3d.o \
$(CAMx)/rassgn4d.o \
$(CAMx)/readaho.o \
$(CAMx)/readchm.o \
$(CAMx)/readnml.o \
$(CAMx)/readpht.o \
$(CAMx)/scavrat.o \
$(CAMx)/setbc1d.o \
$(CAMx)/setbc.o \
$(CAMx)/sim_init.o \
$(CAMx)/srfruf.o \
$(CAMx)/startup.o \
$(CAMx)/timestep.o \
$(CAMx)/timrates.o \
$(CAMx)/toupper.o \
$(CAMx)/tridiag.o \
$(CAMx)/tstep_init.o \
$(CAMx)/updtmet.o \
$(CAMx)/uptime.o \
$(CAMx)/utmgeo.o \
$(CAMx)/vd_aer.o \
$(CAMx)/vd_gas.o \
$(CAMx)/vdiffacm2.o \
$(CAMx)/vdiffimp.o \
$(CAMx)/vnmshcal.o \
$(CAMx)/vrtslv.o \
$(CAMx)/wetdep.o \
$(CAMx)/wrtmass.o \
$(CAMx)/xerf.o \
$(CAMx)/xyadvec.o \
$(CAMx)/zadvec.o \
$(CAMx)/zeros.o \
$(CAMx)/zrates.o \
$(IOBIN)/areaprep.o \
$(IOBIN)/bndprep.o \
$(IOBIN)/clciwt.o \
$(IOBIN)/cncprep.o \
$(IOBIN)/depprep.o \
$(IOBIN)/getdepth.o \
$(IOBIN)/openfils.o \
$(IOBIN)/metinit.o \
$(IOBIN)/rdfgcon.o \
$(IOBIN)/rdpthdr.o \
$(IOBIN)/rdsumbc.o \
$(IOBIN)/readar.o \
$(IOBIN)/readbnd.o \
$(IOBIN)/readcnc.o \
$(IOBIN)/readinp.o \
$(IOBIN)/readpt.o \
$(IOBIN)/srfprep.o \
$(IOBIN)/wrfgcon.o \
$(IOBIN)/wrtcon.o \
$(IOBIN)/wrtdep.o \
$(CF_AERO)/hlconst.o \
$(CF_AERO)/hlindex.o \
$(CF_AERO)/isocom_v1.6.o \
$(CF_AERO)/isofwd_v1.6.o \
$(CF_AERO)/isorev_v1.6.o \
$(CF_AERO)/raqchem.o \
$(CMC)/ddmjac3.o \
$(CMC)/ddmjac4.o \
$(CMC)/ddmjac5.o \
$(CMC)/ddmjac6.o \
$(CMC)/ebirate3.o \
$(CMC)/ebirate4.o \
$(CMC)/ebirate5.o \
$(CMC)/ebirate6.o \
$(CMC)/ebirxn3.o \
$(CMC)/ebirxn4.o \
$(CMC)/ebirxn5.o \
$(CMC)/ebirxn6.o \
$(CMC)/hddmjac3.o \
$(CMC)/hddmjac4.o \
$(CMC)/hddmjac5.o \
$(CMC)/hddmjac6.o \
$(CMC)/hr_hox3.o \
$(CMC)/hr_hox4.o \
$(CMC)/hr_hox5.o \
$(CMC)/hr_hox6.o \
$(CMC)/hr_nox3.o \
$(CMC)/hr_nox4.o \
$(CMC)/hr_nox5.o \
$(CMC)/hr_nox6.o \
$(CMC)/hr_nxy3.o \
$(CMC)/hr_nxy4.o \
$(CMC)/hr_nxy5.o \
$(CMC)/hr_nxy6.o \
$(CMC)/hr_pan3.o \
$(CMC)/hr_pan4.o \
$(CMC)/hr_pan5.o \
$(CMC)/hr_pan6.o \
$(CMC)/iejac3.o \
$(CMC)/iejac4.o \
$(CMC)/iejac5.o \
$(CMC)/iejac6.o \
$(CMC)/ierate3.o \
$(CMC)/ierate4.o \
$(CMC)/ierate5.o \
$(CMC)/ierate6.o \
$(CMC)/ierxn3.o \
$(CMC)/ierxn4.o \
$(CMC)/ierxn5.o \
$(CMC)/ierxn6.o \
$(CMC)/ieslow3.o \
$(CMC)/ieslow4.o \
$(CMC)/ieslow5.o \
$(CMC)/ieslow6.o \
$(CMC)/lsrate3.o \
$(CMC)/lsrate4.o \
$(CMC)/lsrate5.o \
$(CMC)/lsrate6.o \
$(CMC)/lsrxn3.o \
$(CMC)/lsrxn4.o \
$(CMC)/lsrxn5.o \
$(CMC)/lsrxn6.o \
$(CMU_AERO)/areag.o \
$(CMU_AERO)/addit.o \
$(CMU_AERO)/aerchem.o \
$(CMU_AERO)/aero_err.o \
$(CMU_AERO)/aqchem.o \
$(CMU_AERO)/aqdist.o \
$(CMU_AERO)/aqfex1.o \
$(CMU_AERO)/aqfex2.o \
$(CMU_AERO)/aqintegr1.o \
$(CMU_AERO)/aqintegr2.o \
$(CMU_AERO)/aqjex.o \
$(CMU_AERO)/aqoperator1.o \
$(CMU_AERO)/aqoperator2.o \
$(CMU_AERO)/aqrates1.o \
$(CMU_AERO)/aqrates2.o \
$(CMU_AERO)/baddit.o \
$(CMU_AERO)/block.o \
$(CMU_AERO)/bmass.o \
$(CMU_AERO)/coagul.o \
$(CMU_AERO)/constants.o \
$(CMU_AERO)/decisions.o \
$(CMU_AERO)/diameter.o \
$(CMU_AERO)/diff.o \
$(CMU_AERO)/differ.o \
$(CMU_AERO)/diffund.o \
$(CMU_AERO)/dropinit.o \
$(CMU_AERO)/electro.o \
$(CMU_AERO)/eqpart.o \
$(CMU_AERO)/eqparto.o \
$(CMU_AERO)/equaer.o \
$(CMU_AERO)/fullequil.o \
$(CMU_AERO)/hybrd.o \
$(CMU_AERO)/hybrid.o \
$(CMU_AERO)/linint.o \
$(CMU_AERO)/madm.o \
$(CMU_AERO)/mass.o \
$(CMU_AERO)/negchk.o \
$(CMU_AERO)/newdist.o \
$(CMU_AERO)/nucl.o \
$(CMU_AERO)/qsaturation.o \
$(CMU_AERO)/react.o \
$(CMU_AERO)/state.o \
$(CMU_AERO)/steady.o \
$(CMU_AERO)/step.o \
$(CMU_AERO)/svode.o \
$(CMU_AERO)/values.o \
$(CMU_AERO)/vsrm.o \
$(CMU_AERO)/slsode.o \
$(DDM)/adjbcddm.o \
$(DDM)/adjddmc0.o \
$(DDM)/avgrcpddm.o \
$(DDM)/bottddm.o \
$(DDM)/clrbdyddm.o \
$(DDM)/cvticddm.o \
$(DDM)/ddmebi.o \
$(DDM)/filspddm.o \
$(DDM)/hddmadj.o \
$(DDM)/hddmchem.o \
$(DDM)/hdrrcpddm.o \
$(DDM)/loaddm.o \
$(DDM)/rdarddm.o \
$(DDM)/rdbcddm.o \
$(DDM)/rdicddm.o \
$(DDM)/rdoptddm.o \
$(DDM)/rdptddm.o \
$(DDM)/specddm.o \
$(DDM)/startddm.o \
$(OSAT)/addrcp.o \
$(OSAT)/adjstc0.o \
$(OSAT)/adjstsa.o \
$(OSAT)/apcasa.o \
$(OSAT)/avgrcp.o \
$(OSAT)/avgwal.o \
$(OSAT)/clcbwt.o \
$(OSAT)/clcewt.o \
$(OSAT)/clrbdysa.o \
$(OSAT)/cyctpnsa.o \
$(OSAT)/emprepsa.o \
$(OSAT)/filaqsa.o \
$(OSAT)/filbdysa.o \
$(OSAT)/filvdsa.o \
$(OSAT)/goatsa.o \
$(OSAT)/hdrrcp.o \
$(OSAT)/hdrdepsa.o \
$(OSAT)/hdrwsa.o \
$(OSAT)/initsa.o \
$(OSAT)/osatsa.o \
$(OSAT)/o3prdsa.o \
$(OSAT)/psatsa.o \
$(OSAT)/pigdrysa.o \
$(OSAT)/pigdumpsa.o \
$(OSAT)/pigwetsa.o \
$(OSAT)/rdargrp.o \
$(OSAT)/rdfgsa.o \
$(OSAT)/rdinstsa.o \
$(OSAT)/rdoptsa.o \
$(OSAT)/rdptgrp.o \
$(OSAT)/readarsa.o \
$(OSAT)/readptsa.o \
$(OSAT)/recalib.o \
$(OSAT)/repartsa.o \
$(OSAT)/rercp.o \
$(OSAT)/spccbivsa.o \
$(OSAT)/spcsprcsa.o \
$(OSAT)/spccb05sa.o \
$(OSAT)/resmap.o \
$(OSAT)/specsa.o \
$(OSAT)/stabsa.o \
$(OSAT)/startsa.o \
$(OSAT)/sum1grd.o \
$(OSAT)/sum1pnt.o \
$(OSAT)/sumgrps.o \
$(OSAT)/sumicwt.o \
$(OSAT)/sumwt4.o \
$(OSAT)/timadv.o \
$(OSAT)/wconsa.o \
$(OSAT)/wfconsa.o \
$(OSAT)/wrtdepsa.o \
$(OSAT)/wsfcsa.o \
$(OSAT)/xfluxsa.o \
$(OSAT)/yfluxsa.o \
$(PA)/cpamech3.o \
$(PA)/cpamech4.o \
$(PA)/cpamech5.o \
$(PA)/cpamech6.o \
$(PA)/cparad.o \
$(PA)/initipr.o \
$(PA)/pagrids.o \
$(PA)/pasetup.o \
$(PA)/pazero.o \
$(PA)/rdoptpa.o \
$(PA)/wrtcpa.o \
$(PA)/wrtipr.o \
$(PA)/wrtiprhdr.o \
$(PA)/wrtirr.o \
$(PA)/wrtirrhdr.o \
$(PIG)/avepig.o \
$(PIG)/irondry.o \
$(PIG)/ironchem.o \
$(PIG)/ironwet.o \
$(PIG)/noxchem.o \
$(PIG)/pigcoord.o \
$(PIG)/pigdiag.o \
$(PIG)/pigdrive.o \
$(PIG)/pigevol.o \
$(PIG)/piggrow.o \
$(PIG)/piginit.o \
$(PIG)/pigmscl.o \
$(PIG)/pigprep.o \
$(PIG)/pigwalk.o \
$(PIG)/pigsampl.o \
$(PIG)/siggrow.o \
$(PIG)/smpl1puf.o \
$(PIG)/smpprep.o \
$(PIG)/virtdump.o \
$(PIG)/walk1puf.o \
$(PIG)/wrtpig.o \
$(PIG)/wrtsmp.o \
$(RTRAC)/chemrt.o \
$(RTRAC)/cvticrt.o \
$(RTRAC)/deqmcmc.o \
$(RTRAC)/djaccmc.o \
$(RTRAC)/dlsdrv.o \
$(RTRAC)/dprepslo.o \
$(RTRAC)/dratecmc.o \
$(RTRAC)/drydeprt.o \
$(RTRAC)/dslocmc.o \
$(RTRAC)/empreprt.o \
$(RTRAC)/gencmc.o \
$(RTRAC)/hdrcprt.o \
$(RTRAC)/idxspc.o \
$(RTRAC)/is1alpha.o \
$(RTRAC)/is1num.o \
$(RTRAC)/krtc.o \
$(RTRAC)/ksci.o \
$(RTRAC)/pigchmrt.o \
$(RTRAC)/rbjaccmc.o \
$(RTRAC)/rbkdrv.o \
$(RTRAC)/rbratcmc.o \
$(RTRAC)/rdarrt.o \
$(RTRAC)/rdbcrt.o \
$(RTRAC)/rdchmrt.o \
$(RTRAC)/rdicrt.o \
$(RTRAC)/rdozmch.o \
$(RTRAC)/rdptrt.o \
$(RTRAC)/rdrcprt.o \
$(RTRAC)/rdschm.o \
$(RTRAC)/rodas.o \
$(RTRAC)/seqmcmc.o \
$(RTRAC)/sjaccmc.o \
$(RTRAC)/slsdrv.o \
$(RTRAC)/sprepslo.o \
$(RTRAC)/sratecmc.o \
$(RTRAC)/sslocmc.o \
$(RTRAC)/startrt.o \
$(RTRAC)/wetdeprt.o \
$(RTRAC)/wrrcprt.o \
$(SOAP)/soachem.o \
$(SOAP)/soapdat.o \
$(SOAP)/soap.o \
$(SOAP)/spfcn.o \
$(HG)/hggaschem.o \
$(HG)/hgaqschem.o \
$(CAMX_MPI)/after_massum.o \
$(CAMX_MPI)/before_massum.o \
$(CAMX_MPI)/isbound.o \
$(CAMX_MPI)/master_send_1species_data.o \
$(CAMX_MPI)/master_send_gridded_data.o \
$(CAMX_MPI)/master_update.o \
$(CAMX_MPI)/newgrid.o \
$(CAMX_MPI)/node_get_feed.o \
$(CAMX_MPI)/node_get_lbc.o \
$(CAMX_MPI)/node_get_nbc.o \
$(CAMX_MPI)/node_recv_1species_data.o \
$(CAMX_MPI)/node_recv_gridded_data.o \
$(CAMX_MPI)/nodes_ahoz.o \
$(CAMX_MPI)/nodes_alloc.o \
$(CAMX_MPI)/nodes_emiss.o \
$(CAMX_MPI)/node_send_feed.o \
$(CAMX_MPI)/node_send_lbc.o \
$(CAMX_MPI)/node_send_nbc.o \
$(CAMX_MPI)/nodes_init.o \
$(CAMX_MPI)/nodes_ipr.update.o \
$(CAMX_MPI)/nodes_irr.update.o \
$(CAMX_MPI)/nodes_met.o \
$(CAMX_MPI)/nodes_met_pig.o \
$(CAMX_MPI)/nodes_pass.o \
$(CAMX_MPI)/nodes_pass_sapnts.o \
$(CAMX_MPI)/nodes_pig_pass.o \
$(CAMX_MPI)/nodes_send_pig_sample.o \
$(CAMX_MPI)/nodes_send_rt_sample.o \
$(CAMX_MPI)/nodes_tstep.o \
$(CAMX_MPI)/nodemass.o \
$(CAMX_MPI)/broadcast_grid_dimens.o \
$(CAMX_MPI)/broadcast_procid.o \
$(CAMX_MPI)/domain_decomp.o \
$(CAMX_MPI)/iniptr_node.o \
$(CAMX_MPI)/init_fields_node.o \
$(CAMX_MPI)/master_pass_xmass.o \
$(CAMX_MPI)/master_recv_1species_data.o \
$(CAMX_MPI)/myoffset.o \
$(CAMX_MPI)/node_send_1species_data.o \
$(CAMX_MPI)/nodes_send_lslice_back.o \
$(CAMX_MPI)/nodes_send_pig_back.o \
$(CAMX_MPI)/nodes_send_pig_misc.o \
$(CAMX_MPI)/nodes_send_pig_misc_real8.o \
$(CAMX_MPI)/nodes_send_puffmass_back.o \
$(CAMX_MPI)/nodes_pass1.o \
$(CAMX_MPI)/mpi_feedback.o \
$(CAMX_MPI)/par_decomp.o \
$(CAMX_MPI)/par_decomp_bounds.o


model:  $(OBJCTS)
ifeq ($(MPI),true)
	cd MPI/util; make; cd ../..
else
	cc -c -o Dummy/mpi_dummy_c.o Dummy/mpi_dummy_c.c
	$(FC) -c -o Dummy/mpi_dummy.o Dummy/mpi_dummy.f
endif
	$(FC) -o $(TARGT) $(FLGS) $(MOD_OBJCTS) $(OBJCTS) $(LIBS) $(DUM_F) $(DUM_C)

modules:  $(MOD_OBJCTS)


hdflib: $(HDF_OBJ)
	ar -rv libcamxhdf.a $(HDF_OBJ)

.F90.o  :
	$(FC) -c -o $@ $(FLGS) $(MODULES) $<
.f90.o  :
	$(FC) -c -o $@ $(FLGS) $(MODULES) $<
.f.o    :
	$(FC) -c -o $@ $(FLGS) $(MODULES) $<
.c.o    :
	cc -c -o $@ -I$(INCLUDES) $<

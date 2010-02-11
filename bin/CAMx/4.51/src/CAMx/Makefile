#------------------------------------------------------------
#            This is the Makefile for CAMx v4.50
#
#     Modified by Daniel Oderbolz for use with the CAMxRunner Installer
#     Only changed HDF_LIB and HDF_INC
#
#     Syntax is: "make platform <DOMAIN=string> <HDF=true>"
#
#     For example:
#
#     make pg_linux DOMAIN=crca5 HDF=true
#
#     will make a program for Linux machines using the
#     Portland Group Compiler and using the include file 
#     "./Includes/camx.prm.crca25" for the domain parameters.
#     The CAMx exectuable will be able to output HDF5 files.
#
#     To compile CAMx for the test problem you do not
#     need to use the DOMAIN keyword. For example:
#
#     make pg_linux 
#
#     will make a program for Linux machines using the
#     Portland Group Compiler and using the include file 
#     for the test problem (midwest.36.12) and HDF disabled.
#
#------------------------------------------------------------

.SUFFIXES: .F90 .f90 .f .c .f .o .a

#ifndef DOMAIN
   DOMAIN = midwest.36.12
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
MOD_DIR   = ./Modules

MODULES   = -I${MOD_DIR} -module ${MOD_DIR}

ifeq ($(HDF),true)
   HDF_LIB  = -L/usr/lib -L/usr/local/lib  -L${MYLIBDIR}/hdf -lhdf5 -lz
   HDF_INC  = ${MYLIBDIR}/hdf/include
   DEFHDF   = -DUSEHDF
   INCLUDES = -I$(CAMx_INC) -I$(IOHDF) -I$(HDF_INC)
   HDF_OBJ  = \
              $(IOHDF)/camx_includes.o \
              $(IOHDF)/rams_head.o \
              $(IOHDF)/hdf5_utils.o \
              $(IOHDF)/hdf5_f2c.o \
              $(IOHDF)/hdf5_file_out.o
   LIBS     = $(HDF_OBJ) $(HDF_LIB)
else
   HDF_LIB  = 
   HDF_INC  =
   DEFHDF   =
   INCLUDES = -I$(CAMx_INC)
   HDF_OBJ  = $(IOHDF)/camx_includes.o \
              $(IOHDF)/hdf5_file_out.o
   LIBS     = $(HDF_OBJ)
endif

default:
	@echo ' '
	@echo '---------------------------------------------------------'
	@echo ' '
	@echo 'This is the Makefile for CAMx v4.5beta'
	@echo ' '
	@echo 'Syntax is: "make platform <DOMAIN=string> <HDF=true>"'
	@echo ' '
	@echo 'Acceptable platforms are:'
	@echo '    dec         -- configure for DEC/Alpha platform'
	@echo '    pg_linux    -- configure for Linux using pgf77'
	@echo '    pg_linuxomp -- configure for Linux using pgf77 w/OMP'
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
	@echo 'file for the CAMx test case will be used:'
	@echo '"Includes/camx.prm.mrpo.36.12"'
	@echo ' '
	@echo 'HDF=true will compile CAMx to include HDF5 I/O libraries.'
	@echo 'This requires that HDF5 libraries have been installed and'
	@echo 'built on the machine that is running this Makefile.  You'
	@echo 'should check that the variables HDF_LIB and HDF_INC in the'
	@echo 'Makefile are set correctly.  If HDF is not set on the'
	@echo 'command line, CAMx will not be able to write HDF5 files.'
	@echo ' '
	@echo '----------------------------------------------------------'
	@echo ' '

clean:	
	-rm -f $(OBJCTS) $(IOHDF)/*.o $(MOD_DIR)/*.mod *.a

dec:
	@rm -f $(IOHDF)/*.o $(MOD_DIR)/*.mod
	@rm -f $(CAMx_INC)/camx.prm
	@csh chktracer camx.prm.$(DOMAIN) CAMx.$(DOMAIN).dec
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make hdflib \
           HDF=$(HDF) \
           FC=f90 \
           CC=cc \
           FLGS="$(INCLUDES) -DALPHA $(DEFHDF) -convert big_endian -O3 -align dcommons"
	make model \
           FC=f90 \
           TARGT=CAMx.$(DOMAIN).dec \
           FLGS="-I$(CAMx_INC) -convert big_endian -O3 -align dcommons"

sgi:
	@rm -f $(IOHDF)/*.o $(MOD_DIR)/*.mod
	@rm -f $(CAMx_INC)/camx.prm
	@csh chktracer camx.prm.$(DOMAIN) CAMx.$(DOMAIN).sgi
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make hdflib \
           HDF=$(HDF) \
           FC=f90 \
           CC=cc \
           FLGS="-DSGI $(DEFHDF) -O2 -Wf,$(INCLUDES)"
	make model \
           FC=f90 \
           TARGT=CAMx.$(DOMAIN).sgi \
           FLGS="-O2 -Wf,-I$(CAMx_INC)"

sgiomp:
	@rm -f $(IOHDF)/*.o $(MOD_DIR)/*.mod
	@rm -f $(CAMx_INC)/camx.prm
	@csh chktracer camx.prm.$(DOMAIN) CAMx.$(DOMAIN).sgiomp
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make hdflib \
           HDF=$(HDF) \
           FC=f90 \
           CC=cc \
           FLGS="-DSGI $(DEFHDF) -mp -Wf,$(INCLUDES)"
	make model \
           FC=f90 \
           TARGT=CAMx.$(DOMAIN).sgiomp \
           FLGS="-mp -Wf,-I$(CAMx_INC)"

sun:
	@rm -f $(IOHDF)/*.o $(MOD_DIR)/*.mod
	@rm -f $(CAMx_INC)/camx.prm
	@csh chktracer camx.prm.$(DOMAIN) CAMx.$(DOMAIN).sun
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make hdflib \
           HDF=$(HDF) \
           FC=f90 \
           CC=cc \
           FLGS="$(INCLUDES) -DSUN $(DEFHDF) -Nq900 -fast"
	make model \
           FC=f90 \
           TARGT=CAMx.$(DOMAIN).sun \
           FLGS="-I$(CAMx_INC) -Nq900 -fast"

hp:
	@rm -f $(IOHDF)/*.o $(MOD_DIR)/*.mod
	@rm -f $(CAMx_INC)/camx.prm
	@csh chktracer camx.prm.$(DOMAIN) CAMx.$(DOMAIN).hp
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make hdflib \
           HDF=$(HDF) \
           FC=f90 \
           CC=cc \
           FLGS="$(INCLUDES) -DHP $(DEFHDF) -O +U77 +FPVZODu"
	make model \
           FC=f90 \
           TARGT=CAMx.$(DOMAIN).hp \
           FLGS="-I$(CAMx_INC) -O +U77 +FPVZODu"

ibm:
	@rm -f $(IOHDF)/*.o $(MOD_DIR)/*.mod
	@rm -f $(CAMx_INC)/camx.prm
	@csh chktracer camx.prm.$(DOMAIN) CAMx.$(DOMAIN).ibm
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make hdflib \
           HDF=$(HDF) \
           FC=xlf \
           CC=cc \
           FLGS="$(INCLUDES) -DIBM $(DEFHDF) -O2 -brename:.flush,.flush_ -brename:.etime,.etime_ -brename:.dtime,.dtime_ -brename:.fdate,.fdate_"
	make model \
           FC=xlf \
           TARGT=CAMx.$(DOMAIN).ibm \
           FLGS="-I$(CAMx_INC) -O2 -brename:.flush,.flush_ -brename:.etime,.etime_ -brename:.dtime,.dtime_ -brename:.fdate,.fdate_"

pg_linux:
	@rm -f $(IOHDF)/*.o $(MOD_DIR)/*.mod
	@rm -f $(CAMx_INC)/camx.prm
	@csh chktracer camx.prm.$(DOMAIN) CAMx.$(DOMAIN).pg_linux
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make hdflib \
           HDF=$(HDF) \
           FC=pgf90 \
           CC=gcc \
           FLGS="$(INCLUDES) -DPC_LINUX1 $(DEFHDF) -O2 -pc 64 -Mdalign -Mextend -Mnoframe -byteswapio"
	make model \
           FC=pgf90 \
           TARGT=CAMx.$(DOMAIN).pg_linux \
           FLGS="-I$(CAMx_INC) -O2 -pc 64 -Mdalign -Mextend -Mnoframe -byteswapio"

pg_linuxomp:
	@rm -f $(IOHDF)/*.o $(MOD_DIR)/*.mod
	@rm -f $(CAMx_INC)/camx.prm
	@csh chktracer camx.prm.$(DOMAIN) CAMx.$(DOMAIN).pg_linuxomp
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make hdflib \
           HDF=$(HDF) \
           FC=pgf90 \
           CC=gcc \
           FLGS="$(INCLUDES) -DPC_LINUX1 $(DEFHDF) -O2 -pc 64 -Mdalign -Mextend -Mnoframe -byteswapio"
	make model \
           FC=pgf90 \
           TARGT=CAMx.$(DOMAIN).pg_linuxomp \
           FLGS="-I$(CAMx_INC) -O2 -mp -pc 64 -Mdalign -Mextend -Mnoframe -byteswapio"

i_linux:
	@rm -f $(IOHDF)/*.o $(MOD_DIR)/*.mod
	@rm -f $(CAMx_INC)/camx.prm
	@csh chktracer camx.prm.$(DOMAIN) CAMx.$(DOMAIN).i_linux
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make hdflib \
           HDF=$(HDF) \
           FC=ifort \
           CC=gcc \
           FLGS="$(INCLUDES) -DPC_LINUX1 $(DEFHDF) -O2 -align dcommons -extend_source -convert big_endian -ipo"
	make model \
           FC=ifort \
           TARGT=CAMx.$(DOMAIN).i_linux \
           FLGS="-I$(CAMx_INC) -O2 -align dcommons -extend_source -convert big_endian -ipo"

i_linuxomp:
	@rm -f $(IOHDF)/*.o $(MOD_DIR)/*.mod
	@rm -f $(CAMx_INC)/camx.prm
	@csh chktracer camx.prm.$(DOMAIN) CAMx.$(DOMAIN).i_linuxomp
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make hdflib \
           HDF=$(HDF) \
           FC=ifort \
           CC=gcc \
           FLGS="$(INCLUDES) -DPC_LINUX1 $(DEFHDF) -O2 -align dcommons -extend_source -convert big_endian -ipo -i_dynamic -fpp -openmp"
	make model \
           FC=ifort \
           TARGT=CAMx.$(DOMAIN).i_linuxomp \
           FLGS="-I$(CAMx_INC) -O2 -align dcommons -mcmodel=medium -shared-intel -extend_source -convert big_endian -ipo -i_dynamic -fpp -openmp"

ab_linux:
	@rm -f $(IOHDF)/*.o $(MOD_DIR)/*.mod
	@rm -f $(CAMx_INC)/camx.prm
	@csh chktracer camx.prm.$(DOMAIN) CAMx.$(DOMAIN).ab_linux
	@ln -s camx.prm.$(DOMAIN) $(CAMx_INC)/camx.prm
	make hdflib \
           HDF=$(HDF) \
           FC=f90 \
           CC=cc \
           FLGS="-I$(CAMx_INC) $(INCLUDES) -DPC_LINUX1 $(DEFHDF) -O3 -V -m64 -N11 -YEXT_NAMES=LCS -YEXT_SFX=_ -s -YCFRL=1 -W132 -lU77 -L/Applications/Absoft/lib64"
	make model \
           FC=ifort \
           TARGT=CAMx.$(DOMAIN).ab_linux \
           FLGS="-I$(CAMx_INC) -V -O3 -m64 -N11 -YEXT_NAMES=LCS -YEXT_SFX=_ -s -YCFRL=1 -W132 -lU77 -L/Applications/Absoft/lib64"

linux:
	@echo ' '
	@echo '---------------------------------------------------------'
	@echo 'This version of the CAMx makefile supports more than'
	@echo 'one compiler on the Linux platform.'
	@echo 'Acceptable keywords for Linux are:'
	@echo '     pg_linux    -- configure for Linux using pgf90'
	@echo '     i_linux     -- configure for Linux using ifort'
	@echo '     ab_linux    -- configure for MAC OSX using ABSoft'
	@echo '---------------------------------------------------------'
	@echo ' '

linuxomp:
	@echo ' '
	@echo '---------------------------------------------------------'
	@echo 'This version of the CAMx makefile supports more than'
	@echo 'one compiler on the Linux OMP platform.'
	@echo 'Acceptable keywords for OMP on Linux are:'
	@echo '     pg_linuxomp -- configure for Linux using pgf90 w/OMP'
	@echo '     i_linuxomp  -- configure for Linux using ifort w/OMP'
	@echo '---------------------------------------------------------'
	@echo ' '

OBJCTS = \
$(CAMx)/CAMx.o \
$(CAMx)/aerochem_cf.o \
$(CAMx)/aerochem_cmu.o \
$(CAMx)/aeroset.o \
$(CAMx)/aggdep.o \
$(CAMx)/aggr00.o \
$(CAMx)/aggreg.o \
$(CAMx)/ahoprep.o \
$(CAMx)/average.o \
$(CAMx)/bc1grd.o \
$(CAMx)/bcmodfy.o \
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
$(CAMx)/emassign.o \
$(CAMx)/emiss.o \
$(CAMx)/emistrns.o \
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
$(CAMx)/interp2d.o \
$(CAMx)/interpv.o \
$(CAMx)/intrpcnc.o \
$(CAMx)/intrpdat.o \
$(CAMx)/istrln.o \
$(CAMx)/jstlft.o \
$(CAMx)/juldate.o \
$(CAMx)/khorz.o \
$(CAMx)/kphoto.o \
$(CAMx)/ktherm.o \
$(CAMx)/khetero.o \
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
$(CAMx)/startup.o \
$(CAMx)/timestep.o \
$(CAMx)/timrates.o \
$(CAMx)/toupper.o \
$(CAMx)/trap.o \
$(CAMx)/trdiag.o \
$(CAMx)/updtmet.o \
$(CAMx)/uptime.o \
$(CAMx)/utmgeo.o \
$(CAMx)/vd_aer.o \
$(CAMx)/vd_gas.o \
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
$(CMC)/ddmjac1.o \
$(CMC)/ddmjac3.o \
$(CMC)/ddmjac4.o \
$(CMC)/ddmjac5.o \
$(CMC)/ddmjac6.o \
$(CMC)/iejac1.o \
$(CMC)/iejac3.o \
$(CMC)/iejac4.o \
$(CMC)/iejac5.o \
$(CMC)/iejac6.o \
$(CMC)/ierate1.o \
$(CMC)/ierate3.o \
$(CMC)/ierate4.o \
$(CMC)/ierate5.o \
$(CMC)/ierate6.o \
$(CMC)/ierxn1.o \
$(CMC)/ierxn3.o \
$(CMC)/ierxn4.o \
$(CMC)/ierxn5.o \
$(CMC)/ierxn6.o \
$(CMC)/ieslow1.o \
$(CMC)/ieslow3.o \
$(CMC)/ieslow4.o \
$(CMC)/ieslow5.o \
$(CMC)/ieslow6.o \
$(CMC)/radslvr1.o \
$(CMC)/radslvr3.o \
$(CMC)/radslvr4.o \
$(CMC)/radslvr5.o \
$(CMC)/radslvr6.o \
$(CMC)/ratejac1.o \
$(CMC)/ratejac3.o \
$(CMC)/ratejac4.o \
$(CMC)/ratejac5.o \
$(CMC)/ratejac6.o \
$(CMC)/rateslo1.o \
$(CMC)/rateslo3.o \
$(CMC)/rateslo4.o \
$(CMC)/rateslo5.o \
$(CMC)/rateslo6.o \
$(CMC)/rxnrate1.o \
$(CMC)/rxnrate3.o \
$(CMC)/rxnrate4.o \
$(CMC)/rxnrate5.o \
$(CMC)/rxnrate6.o \
$(CMC)/lsrate1.o \
$(CMC)/lsrate3.o \
$(CMC)/lsrate4.o \
$(CMC)/lsrate5.o \
$(CMC)/lsrate6.o \
$(CMC)/lsrxn1.o \
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
$(CMU_AERO)/hybrid.o \
$(CMU_AERO)/hybrd.o \
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
$(DDM)/adjddmc0.o \
$(DDM)/avgrcpddm.o \
$(DDM)/bottddm.o \
$(DDM)/clrbdyddm.o \
$(DDM)/cvticddm.o \
$(DDM)/ddmchem.o \
$(DDM)/filspddm.o \
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
$(OSAT)/hdrwsa.o \
$(OSAT)/initsa.o \
$(OSAT)/osatsa.o \
$(OSAT)/o3prdsa.o \
$(OSAT)/psatsa.o \
$(OSAT)/pekrcp.o \
$(OSAT)/pigdumpsa.o \
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
$(RTRAC)/drydeprt.o \
$(RTRAC)/empreprt.o \
$(RTRAC)/hdrcprt.o \
$(RTRAC)/pigchmrt.o \
$(RTRAC)/rdarrt.o \
$(RTRAC)/rdbcrt.o \
$(RTRAC)/rdchmrt.o \
$(RTRAC)/rdicrt.o \
$(RTRAC)/rdptrt.o \
$(RTRAC)/rdrcprt.o \
$(RTRAC)/startrt.o \
$(RTRAC)/wetdeprt.o \
$(RTRAC)/wrrcprt.o \
$(SOAP)/soachem.o \
$(SOAP)/soapdat.o \
$(SOAP)/soap.o \
$(SOAP)/spfcn.o \
$(HG)/hggaschem.o \
$(HG)/hgaqschem.o


model: 	$(OBJCTS)
	$(FC) -o $(TARGT) $(FLGS) $(OBJCTS) $(LIBS)

hdflib:	$(HDF_OBJ)
	ar -rv libcamxhdf.a $(HDF_OBJ)

.F90.o  :
	$(FC) -c -o $@ $(FLGS) $(MODULES) $<
.f90.o  :
	$(FC) -c -o $@ $(FLGS) $(MODULES) $<
.f.o    :    
	$(FC) -c -o $@ $(FLGS) $(MODULES) $<
.c.o    :
	$(CC) -c -o $@ $(INCLUDES) $<


$(CAMx)/CAMx.o 		: $(CAMx)/CAMx.f                                       \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/grid.com           \
                        $(CAMx_INC)/ptemiss.com $(CAMx_INC)/bndary.com         \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/ahomap.com  $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/pigsty.com $(CAMx_INC)/tracer.com          \
                        $(CAMx_INC)/rtracchm.com $(CAMx_INC)/procan.com

$(CAMx)/aerochem_cf.o	: $(CAMx)/aerochem_cf.f                                \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/procan.com

$(CAMx)/aerochem_cmu.o	: $(CAMx)/aerochem_cmu.f                               \
			$(CAMx_INC)/aerpar.inc $(CAMx_INC)/dynamic.inc         \
			$(CAMx_INC)/droppar.inc $(CAMx_INC)/camx_aero.inc      \
                        $(CAMx_INC)/camx_aero.com

$(CAMx)/aeroset.o	: $(CAMx)/aeroset.f                                    \
			$(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/section.inc        \
                        $(CAMx_INC)/camx_aero.inc $(CAMx_INC)/camx.com         \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/section_aq.inc        \
                        $(CAMx_INC)/camx_aero.com

$(CAMx)/aggr00.o 	: $(CAMx)/aggr00.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/grid.com           \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/procan.com

$(CAMx)/ahoprep.o 	: $(CAMx)/ahoprep.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/ahomap.com            \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/grid.com           \
                        $(CAMx_INC)/flags.com

$(CAMx)/average.o 	: $(CAMx)/average.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/chmstry.com         \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/rtracchm.com        \
                        $(CAMx_INC)/procan.com

$(CAMx)/bcmodfy.o 	: $(CAMx)/bcmodfy.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com

$(CAMx)/camxerr.o 	: $(CAMx)/camxerr.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com

$(CAMx)/chem10.o	: $(CAMx)/chem10.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com

$(CAMx)/chemdriv.o 	: $(CAMx)/chemdriv.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/chmstry.com         \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/ahomap.com         \
                        $(CAMx_INC)/procan.com $(CAMx_INC)/tracer.com          \
                        $(CAMx_INC)/rtracchm.com

$(CAMx)/chemrxn.o 	: $(CAMx)/chemrxn.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/grid.com           \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/filunit.com          \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/procan.com

$(CAMx)/chmdat.o 	: $(CAMx)/chmdat.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/camx.com $(CAMx_INC)/deposit.com           \
                        $(CAMx_INC)/camx_aero.com

$(CAMx)/dateerr.o 	: $(CAMx)/dateerr.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com

$(CAMx)/depsmry.o 	: $(CAMx)/depsmry.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/filunit.com

$(CAMx)/diffus.o 	: $(CAMx)/diffus.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/chmstry.com         \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/procan.com

$(CAMx)/drydep.o 	: $(CAMx)/drydep.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/deposit.com         \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/camx_aero.com

$(CAMx)/emiss.o 	: $(CAMx)/emiss.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/ptemiss.com           \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/flags.com           \
                        $(CAMx_INC)/procan.com $(CAMx_INC)/tracer.com

$(CAMx)/emistrns.o 	: $(CAMx)/emistrns.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/chmstry.com        \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/flags.com             \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/ptemiss.com        \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/pigsty.com          \
                        $(CAMx_INC)/procan.com $(CAMx_INC)/tracer.com          \
                        $(CAMx_INC)/rtracchm.com

$(CAMx)/exptbl.o 	: $(CAMx)/exptbl.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CAMx)/fgavrg.o 	: $(CAMx)/fgavrg.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/grid.com           \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/pigsty.com $(CAMx_INC)/tracer.com          \
                        $(CAMx_INC)/rtracchm.com $(CAMx_INC)/procan.com

$(CAMx)/getdelt.o 	: $(CAMx)/getdelt.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/bndary.com

$(CAMx)/getunit.o 	: $(CAMx)/getdelt.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com

$(CAMx)/grdprep.o 	: $(CAMx)/grdprep.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/flags.com

$(CAMx)/hadvbot.o 	: $(CAMx)/hadvbot.f                                    \
                        $(CAMx_INC)/camx.prm

$(CAMx)/hadvppm.o 	: $(CAMx)/hadvppm.f                                    \
                        $(CAMx_INC)/camx.prm

$(CAMx)/iehsolv.o 	: $(CAMx)/iehsolv.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/lsbox.com $(CAMx_INC)/filunit.com

$(CAMx)/iniptr.o 	: $(CAMx)/iniptr.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/tracer.com

$(CAMx)/interpv.o 	: $(CAMx)/interpv.f                                    \
                        $(CAMx_INC)/camx.prm

$(CAMx)/intrpdat.o 	: $(CAMx)/intrpdat.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/grid.com

$(CAMx)/khorz.o 	: $(CAMx)/khorz.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/filunit.com

$(CAMx)/kphoto.o 	: $(CAMx)/kphoto.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CAMx)/ktherm.o 	: $(CAMx)/ktherm.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com

$(CAMx)/khetero.o	: $(CAMx)/khetero.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com

$(CAMx)/lscall.o 	: $(CAMx)/lscall.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/ddmchm.com $(CAMx_INC)/lsbox.com           \
                        $(CAMx_INC)/filunit.com

$(CAMx)/massum.o 	: $(CAMx)/massum.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com

$(CAMx)/nesting.o 	: $(CAMx)/nesting.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/camx.com $(CAMx_INC)/flags.com

$(CAMx)/nstprep.o 	: $(CAMx)/nstprep.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/tracer.com

$(CAMx)/parntchd.o 	: $(CAMx)/parntchd.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com

$(CAMx)/pntprep.o 	: $(CAMx)/pntprep.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/ptemiss.com        \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/bndary.com

$(CAMx)/raddrivr.o 	: $(CAMx)/raddrivr.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/camxfld.com         \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/chmstry.com
 
$(CAMx)/readaho.o 	: $(CAMx)/readaho.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/ahomap.com

$(CAMx)/readchm.o 	: $(CAMx)/readchm.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/ddmchm.com $(CAMx_INC)/iehchem.com

$(CAMx)/readnml.o	: $(CAMx)/readnml.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/chmstry.com        \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/ahomap.com         \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/pigsty.com            \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/namelist.com         \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/procan.com

$(CAMx)/readpht.o 	: $(CAMx)/readpht.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/ahomap.com

$(CAMx)/setbc1d.o 	: $(CAMx)/setbc1d.f                                    \
                        $(CAMx_INC)/camx.prm

$(CAMx)/setbc.o 	: $(CAMx)/setbc.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/chmstry.com        \
                        $(CAMx_INC)/tracer.com

$(CAMx)/startup.o 	: $(CAMx)/startup.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/ahomap.com         \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/pigsty.com            \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/tracer.com           \
                        $(CAMx_INC)/procan.com

$(CAMx)/timestep.o 	: $(CAMx)/timestep.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/bndary.com         \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/pigsty.com

$(CAMx)/timrates.o 	: $(CAMx)/timrates.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/bndary.com

$(CAMx)/trap.o 		: $(CAMx)/trap.f                                       \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com
 
$(CAMx)/updtmet.o 	: $(CAMx)/updtmet.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com

$(CAMx)/vdiffimp.o 	: $(CAMx)/vdiffimp.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/procan.com

$(CAMx)/vnmshcal.o 	: $(CAMx)/vnmshcal.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com

$(CAMx)/vrtslv.o 	: $(CAMx)/vrtslv.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/tracer.com

$(CAMx)/wetdep.o 	: $(CAMx)/wetdep.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com         \
                        $(CAMx_INC)/procan.com

$(CAMx)/wrtmass.o 	: $(CAMx)/wrtmass.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camxfld.com           \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/flags.com

$(CAMx)/xyadvec.o 	: $(CAMx)/xyadvec.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/tracer.com           \
                        $(CAMx_INC)/procan.com

$(CAMx)/zadvec.o 	: $(CAMx)/zadvec.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/rtracchm.com        \
                        $(CAMx_INC)/procan.com

$(CAMx)/zrates.o 	: $(CAMx)/zrates.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/flags.com

$(IOBIN)/areaprep.o 	: $(IOBIN)/areaprep.f                                  \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/flags.com

$(IOBIN)/bndprep.o 	: $(IOBIN)/bndprep.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/grid.com            \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/flags.com

$(IOBIN)/clciwt.o 	: $(IOBIN)/clciwt.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/bndary.com         \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/flags.com             \
                        $(CAMx_INC)/tracer.com

$(IOBIN)/cncprep.o 	: $(IOBIN)/cncprep.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/flags.com

$(IOBIN)/depprep.o 	: $(IOBIN)/depprep.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/flags.com

$(IOBIN)/getdepth.o 	: $(IOBIN)/getdepth.f                                  \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com

$(IOBIN)/openfils.o 	: $(IOBIN)/openfils.f                                  \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/ahomap.com         \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/flags.com             \
                        $(CAMx_INC)/pigsty.com $(CAMx_INC)/tracer.com          \
                        $(CAMx_INC)/procan.com $(CAMx_INC)/namelist.com

$(IOBIN)/metinit.o       : $(IOBIN)/metinit.f                                  \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/flags.com

$(IOBIN)/rdfgcon.o 	: $(IOBIN)/rdfgcon.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/grid.com           \
                        $(CAMx_INC)/chmstry.com

$(IOBIN)/rdpthdr.o 	: $(IOBIN)/rdpthdr.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/ptemiss.com        \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/tracer.com

$(IOBIN)/rdsumbc.o 	: $(IOBIN)/rdsumbc.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/chmstry.com        \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/tracer.com

$(IOBIN)/readar.o 	: $(IOBIN)/readar.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/flags.com

$(IOBIN)/readbnd.o 	: $(IOBIN)/readbnd.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                       	$(CAMx_INC)/camxfld.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/grid.com            \
                        $(CAMx_INC)/chmstry.com

$(IOBIN)/readcnc.o 	: $(IOBIN)/readcnc.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/grid.com            \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/flags.com

$(IOBIN)/readinp.o 	: $(IOBIN)/readinp.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/camxfld.com

$(IOBIN)/readpt.o 	: $(IOBIN)/readpt.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/ptemiss.com        \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/grid.com

$(IOBIN)/srfprep.o 	: $(IOBIN)/srfprep.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/bndary.com

$(IOBIN)/wrfgcon.o 	: $(IOBIN)/wrfgcon.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/filunit.com          \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/chmstry.com        \
                        $(CAMx_INC)/grid.com

$(IOBIN)/wrtcon.o 	: $(IOBIN)/wrtcon.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/flags.com

$(IOBIN)/wrtdep.o 	: $(IOBIN)/wrtdep.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/chmstry.com

$(CF_AERO)/isocom_v1.6.o 	: $(CF_AERO)/isocom_v1.6.f	               \
                        $(CAMx_INC)/isrpia.inc

$(CF_AERO)/isofwd_v1.6.o 	: $(CF_AERO)/isofwd_v1.6.f	               \
                        $(CAMx_INC)/isrpia.inc

$(CF_AERO)/isorev_v1.6.o 	: $(CF_AERO)/isorev_v1.6.f	               \
                        $(CAMx_INC)/isrpia.inc

$(CMC)/ddmjac1.o 	: $(CMC)/ddmjac1.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/ddmchm.com            \
                        $(CAMx_INC)/filunit.com

$(CMC)/ddmjac3.o 	: $(CMC)/ddmjac3.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/ddmchm.com            \
                        $(CAMx_INC)/filunit.com

$(CMC)/ddmjac4.o 	: $(CMC)/ddmjac4.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/ddmchm.com            \
                        $(CAMx_INC)/filunit.com

$(CMC)/ddmjac5.o 	: $(CMC)/ddmjac5.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/ddmchm.com            \
                        $(CAMx_INC)/filunit.com

$(CMC)/ddmjac6.o 	: $(CMC)/ddmjac6.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/ddmchm.com            \
                        $(CAMx_INC)/filunit.com

$(CMC)/iejac1.o 	: $(CMC)/iejac1.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/iejac3.o 	: $(CMC)/iejac3.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/iejac4.o 	: $(CMC)/iejac4.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/iejac5.o 	: $(CMC)/iejac5.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/iejac6.o 	: $(CMC)/iejac6.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ierate1.o 	: $(CMC)/ierate1.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ierate3.o 	: $(CMC)/ierate3.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ierate4.o 	: $(CMC)/ierate4.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ierate5.o 	: $(CMC)/ierate5.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ierate6.o 	: $(CMC)/ierate6.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ierxn1.o 	: $(CMC)/ierxn1.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ierxn3.o 	: $(CMC)/ierxn3.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ierxn4.o 	: $(CMC)/ierxn4.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ierxn5.o 	: $(CMC)/ierxn5.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ierxn6.o 	: $(CMC)/ierxn6.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ieslow1.o 	: $(CMC)/ieslow1.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ieslow3.o 	: $(CMC)/ieslow3.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ieslow4.o 	: $(CMC)/ieslow4.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ieslow5.o 	: $(CMC)/ieslow5.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/ieslow6.o 	: $(CMC)/ieslow6.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/iehchem.com

$(CMC)/radslvr1.o 	: $(CMC)/radslvr1.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com

$(CMC)/radslvr3.o 	: $(CMC)/radslvr3.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com

$(CMC)/radslvr4.o 	: $(CMC)/radslvr4.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com

$(CMC)/radslvr5.o 	: $(CMC)/radslvr5.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com

$(CMC)/radslvr6.o 	: $(CMC)/radslvr6.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com

$(CMC)/ratejac1.o 	: $(CMC)/ratejac1.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/ratejac3.o 	: $(CMC)/ratejac3.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/ratejac4.o 	: $(CMC)/ratejac4.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/ratejac5.o 	: $(CMC)/ratejac5.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/ratejac6.o 	: $(CMC)/ratejac6.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/rateslo1.o 	: $(CMC)/rateslo1.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/rateslo3.o 	: $(CMC)/rateslo3.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/rateslo4.o 	: $(CMC)/rateslo4.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/rateslo5.o 	: $(CMC)/rateslo5.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/rateslo6.o 	: $(CMC)/rateslo6.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/rxnrate1.o 	: $(CMC)/rxnrate1.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/rxnrate3.o 	: $(CMC)/rxnrate3.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/rxnrate4.o 	: $(CMC)/rxnrate4.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/rxnrate5.o 	: $(CMC)/rxnrate5.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/rxnrate6.o 	: $(CMC)/rxnrate6.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(CMC)/lsrate1.o 	: $(CMC)/lsrate1.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/lsbox.com             \
                        $(CAMx_INC)/ddmchm.com

$(CMC)/lsrate3.o 	: $(CMC)/lsrate3.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/lsbox.com             \
                        $(CAMx_INC)/ddmchm.com

$(CMC)/lsrate4.o 	: $(CMC)/lsrate4.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/lsbox.com             \
                        $(CAMx_INC)/ddmchm.com

$(CMC)/lsrate5.o 	: $(CMC)/lsrate5.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/lsbox.com             \
                        $(CAMx_INC)/ddmchm.com

$(CMC)/lsrate6.o 	: $(CMC)/lsrate6.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/lsbox.com             \
                        $(CAMx_INC)/ddmchm.com

$(CMC)/lsrxn1.o 	: $(CMC)/lsrxn1.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/ddmchm.com

$(CMC)/lsrxn3.o 	: $(CMC)/lsrxn3.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/ddmchm.com

$(CMC)/lsrxn4.o 	: $(CMC)/lsrxn4.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/ddmchm.com

$(CMC)/lsrxn5.o 	: $(CMC)/lsrxn5.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/ddmchm.com

$(CMC)/lsrxn6.o 	: $(CMC)/lsrxn6.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/ddmchm.com

$(CMU_AERO)/aerchem.o	: $(CMU_AERO)/aerchem.f                                \
                        $(CAMx_INC)/dynamic.inc

$(CMU_AERO)/aero_err.o	: $(CMU_AERO)/aero_err.f                               \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/grid.com

$(CMU_AERO)/aqchem.o	: $(CMU_AERO)/aqchem.f                                 \
                        $(CAMx_INC)/aerpar.inc $(CAMx_INC)/droppar.inc         \
                        $(CAMx_INC)/dropcom.inc

$(CMU_AERO)/aqfex1.o	: $(CMU_AERO)/aqfex1.f                                 \
                        $(CAMx_INC)/aerpar.inc $(CAMx_INC)/droppar.inc

$(CMU_AERO)/aqfex2.o	: $(CMU_AERO)/aqfex2.f                                 \
                        $(CAMx_INC)/aerpar.inc $(CAMx_INC)/droppar.inc

$(CMU_AERO)/aqintegr1.o	: $(CMU_AERO)/aqintegr1.f                              \
                        $(CAMx_INC)/aerpar.inc $(CAMx_INC)/droppar.inc         \
                        $(CAMx_INC)/math.inc

$(CMU_AERO)/aqintegr2.o	: $(CMU_AERO)/aqintegr2.f                              \
                        $(CAMx_INC)/aerpar.inc $(CAMx_INC)/droppar.inc         \
                        $(CAMx_INC)/math.inc

$(CMU_AERO)/aqoperator1.o: $(CMU_AERO)/aqoperator1.f                           \
                        $(CAMx_INC)/aerpar.inc $(CAMx_INC)/droppar.inc         \
                        $(CAMx_INC)/dropcom.inc

$(CMU_AERO)/aqoperator2.o: $(CMU_AERO)/aqoperator2.f                           \
                        $(CAMx_INC)/aerpar.inc $(CAMx_INC)/droppar.inc         \
                        $(CAMx_INC)/dropcom.inc

$(CMU_AERO)/aqrates1.o	: $(CMU_AERO)/aqrates1.f                               \
                        $(CAMx_INC)/aerpar.inc $(CAMx_INC)/droppar.inc         \
                        $(CAMx_INC)/dropcom.inc $(CAMx_INC)/math.inc

$(CMU_AERO)/aqrates2.o	: $(CMU_AERO)/aqrates2.f                               \
                        $(CAMx_INC)/aerpar.inc $(CAMx_INC)/droppar.inc         \
                        $(CAMx_INC)/dropcom.inc $(CAMx_INC)/math.inc

$(CMU_AERO)/block.o	: $(CMU_AERO)/block.f                                  \
                        $(CAMx_INC)/dynamic.inc

$(CMU_AERO)/coagul.o	: $(CMU_AERO)/coagul.f                                 \
                        $(CAMx_INC)/dynamic.inc

$(CMU_AERO)/constants.o	: $(CMU_AERO)/constants.f                              \
                        $(CAMx_INC)/aerpar.inc $(CAMx_INC)/droppar.inc         \
                        $(CAMx_INC)/dropcom.inc

$(CMU_AERO)/decisions.o	: $(CMU_AERO)/decisions.f                              \
                        $(CAMx_INC)/aerpar.inc $(CAMx_INC)/droppar.inc         \
                        $(CAMx_INC)/dropcom.inc

$(CMU_AERO)/diameter.o	: $(CMU_AERO)/diameter.f                               \
                        $(CAMx_INC)/dynamic.inc

$(CMU_AERO)/diff.o	: $(CMU_AERO)/diff.f                                   \
                        $(CAMx_INC)/dynamic.inc

$(CMU_AERO)/diffund.o	: $(CMU_AERO)/diffund.f                                \
                        $(CAMx_INC)/dynamic.inc $(CAMx_INC)/equaer.inc

$(CMU_AERO)/dropinit.o	: $(CMU_AERO)/dropinit.f                               \
                        $(CAMx_INC)/aerpar.inc $(CAMx_INC)/droppar.inc         \
                        $(CAMx_INC)/dropcom.inc $(CAMx_INC)/section.inc        \
			$(CAMx_INC)/camx_aero.inc

$(CMU_AERO)/eqpart.o	: $(CMU_AERO)/eqpart.f                                 \
                        $(CAMx_INC)/dynamic.inc $(CAMx_INC)/equaer.inc

$(CMU_AERO)/eqparto.o	: $(CMU_AERO)/eqparto.f                                \
                        $(CAMx_INC)/dynamic.inc $(CAMx_INC)/soap.com

$(CMU_AERO)/equaer.o	: $(CMU_AERO)/equaer.f                                 \
                        $(CAMx_INC)/dynamic.inc $(CAMx_INC)/equaer.inc

$(CMU_AERO)/hybrid.o	: $(CMU_AERO)/hybrid.f                                 \
                        $(CAMx_INC)/dynamic.inc

$(CMU_AERO)/madm.o	: $(CMU_AERO)/madm.f                                   \
                        $(CAMx_INC)/dynamic.inc

$(CMU_AERO)/negchk.o	: $(CMU_AERO)/negchk.f                                 \
                        $(CAMx_INC)/dynamic.inc

$(CMU_AERO)/newdist.o	: $(CMU_AERO)/newdist.f                                \
                        $(CAMx_INC)/dynamic.inc

$(CMU_AERO)/nucl.o	: $(CMU_AERO)/nucl.f                                   \
                        $(CAMx_INC)/dynamic.inc

$(CMU_AERO)/react.o	: $(CMU_AERO)/react.f                                  \
                        $(CAMx_INC)/aerpar.inc $(CAMx_INC)/droppar.inc

$(CMU_AERO)/step.o	: $(CMU_AERO)/step.f                                   \
                        $(CAMx_INC)/dynamic.inc

$(CMU_AERO)/vsrm.o	: $(CMU_AERO)/vsrm.f                                   \
                        $(CAMx_INC)/aerpar.inc $(CAMx_INC)/droppar.inc         \
                        $(CAMx_INC)/dropcom.inc

$(DDM)/adjddmc0.o 	: $(DDM)/adjddmc0.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/tracer.com

$(DDM)/avgrcpddm.o 	: $(DDM)/avgrcpddm.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/filunit.com

$(DDM)/bottddm.o 	: $(DDM)/bottddm.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/tracer.com

$(DDM)/clrbdyddm.o 	: $(DDM)/clrbdyddm.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com

$(DDM)/cvticddm.o 	: $(DDM)/cvticddm.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/camx.com

$(DDM)/filspddm.o 	: $(DDM)/filspddm.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com

$(DDM)/hdrrcpddm.o 	: $(DDM)/hdrrcpddm.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com         \
                        $(CAMx_INC)/filunit.com

$(DDM)/loaddm.o 	: $(DDM)/loaddm.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/tracer.com

$(DDM)/rdarddm.o 	: $(DDM)/rdarddm.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/grid.com           \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/tracer.com

$(DDM)/rdbcddm.o 	: $(DDM)/rdbcddm.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/chmstry.com        \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/flags.com             \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/tracer.com

$(DDM)/rdicddm.o 	: $(DDM)/rdicddm.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/chmstry.com        \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/flags.com             \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/tracer.com

$(DDM)/rdoptddm.o 	: $(DDM)/rdoptddm.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/tracer.com            \
                        $(CAMx_INC)/procan.com $(CAMx_INC)/namelist.com

$(DDM)/rdptddm.o 	: $(DDM)/rdptddm.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/grid.com           \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/tracer.com

$(DDM)/specddm.o 	: $(DDM)/specddm.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com

$(DDM)/startddm.o       : $(DDM)/startddm.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/tracer.com           \
                        $(CAMx_INC)/procan.com $(CAMx_INC)/namelist.com

$(OSAT)/addrcp.o 	: $(OSAT)/addrcp.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/tracer.com 

$(OSAT)/adjstc0.o 	: $(OSAT)/adjstc0.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/tracer.com

$(OSAT)/adjstsa.o 	: $(OSAT)/adjstsa.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/tracer.com

$(OSAT)/apcasa.o 	: $(OSAT)/apcasa.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com

$(OSAT)/avgrcp.o 	: $(OSAT)/avgrcp.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/filunit.com

$(OSAT)/avgwal.o 	: $(OSAT)/avgwal.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/tracer.com

$(OSAT)/clcbwt.o 	: $(OSAT)/clcbwt.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/chmstry.com        \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/tracer.com

$(OSAT)/clcewt.o 	: $(OSAT)/clcewt.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/grid.com            \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com

$(OSAT)/clrbdysa.o 	: $(OSAT)/clrbdysa.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/tracer.com

$(OSAT)/cyctpnsa.o 	: $(OSAT)/cyctpnsa.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(OSAT)/emprepsa.o 	: $(OSAT)/emprepsa.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/flags.com             \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com

$(OSAT)/filaqsa.o 	: $(OSAT)/filaqsa.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/tracer.com

$(OSAT)/filbdysa.o 	: $(OSAT)/filbdysa.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/tracer.com

$(OSAT)/filvdsa.o 	: $(OSAT)/filvdsa.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/tracer.com

$(OSAT)/goatsa.o 	: $(OSAT)/goatsa.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com

$(OSAT)/hdrrcp.o 	: $(OSAT)/hdrrcp.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/filunit.com

$(OSAT)/hdrwsa.o 	: $(OSAT)/hdrwsa.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/flags.com             \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/filunit.com

$(OSAT)/initsa.o 	: $(OSAT)/initsa.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/tracer.com

$(OSAT)/osatsa.o 	: $(OSAT)/osatsa.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com

$(OSAT)/o3prdsa.o 	: $(OSAT)/o3prdsa.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com

$(OSAT)/psatsa.o 	: $(OSAT)/psatsa.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com

$(OSAT)/pekrcp.o 	: $(OSAT)/pekrcp.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com

$(OSAT)/pigdumpsa.o 	: $(OSAT)/pigdumpsa.f                                  \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/pigsty.com $(CAMx_INC)/tracer.com

$(OSAT)/rdargrp.o 	: $(OSAT)/rdargrp.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/tracer.com

$(OSAT)/rdfgsa.o 	: $(OSAT)/rdfgsa.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/filunit.com

$(OSAT)/rdinstsa.o 	: $(OSAT)/rdinstsa.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/tracer.com           \
                        $(CAMx_INC)/filunit.com

$(OSAT)/rdoptsa.o       : $(OSAT)/rdoptsa.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/tracer.com            \
                        $(CAMx_INC)/procan.com $(CAMx_INC)/namelist.com

$(OSAT)/rdptgrp.o 	: $(OSAT)/rdptgrp.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/ptemiss.com           \
                        $(CAMx_INC)/tracer.com

$(OSAT)/readarsa.o 	: $(OSAT)/readarsa.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/filunit.com

$(OSAT)/readptsa.o 	: $(OSAT)/readptsa.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/grid.com             \
                        $(CAMx_INC)/ptemiss.com $(CAMx_INC)/tracer.com         \
                        $(CAMx_INC)/filunit.com

$(OSAT)/recalib.o 	: $(OSAT)/recalib.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/tracer.com

$(OSAT)/repartsa.o 	: $(OSAT)/repartsa.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com

$(OSAT)/rercp.o 	: $(OSAT)/rercp.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/filunit.com

$(OSAT)/resmap.o 	: $(OSAT)/resmap.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/tracer.com            \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/grid.com

$(OSAT)/specsa.o 	: $(OSAT)/specsa.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/tracer.com            \
                        $(CAMx_INC)/filunit.com

$(OSAT)/stabsa.o 	: $(OSAT)/stabsa.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/filunit.com

$(OSAT)/startsa.o       : $(OSAT)/startsa.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/tracer.com           \
                        $(CAMx_INC)/procan.com $(CAMx_INC)/namelist.com

$(OSAT)/sum1grd.o 	: $(OSAT)/sum1grd.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/chmstry.com

$(OSAT)/sum1pnt.o 	: $(OSAT)/sum1pnt.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/tracer.com           \
                        $(CAMx_INC)/chmstry.com

$(OSAT)/sumgrps.o 	: $(OSAT)/sumgrps.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/grid.com            \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com

$(OSAT)/sumwt4.o 	: $(OSAT)/sumwt4.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/tracer.com

$(OSAT)/timadv.o 	: $(OSAT)/timadv.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/tracer.com

$(OSAT)/wconsa.o 	: $(OSAT)/wconsa.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/filunit.com         \
                        $(CAMx_INC)/flags.com

$(OSAT)/wfconsa.o 	: $(OSAT)/wfconsa.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/tracer.com            \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/flags.com

$(OSAT)/wsfcsa.o 	: $(OSAT)/wsfcsa.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/filunit.com         \
                        $(CAMx_INC)/flags.com

$(OSAT)/xfluxsa.o 	: $(OSAT)/xfluxsa.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/tracer.com

$(OSAT)/yfluxsa.o 	: $(OSAT)/yfluxsa.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/tracer.com

$(OSAT)/spccbivsa.o 	: $(OSAT)/spccbivsa.f                                  \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/tracer.com

$(OSAT)/spcsprcsa.o 	: $(OSAT)/spcsprcsa.f                                  \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/tracer.com

$(OSAT)/spccb05sa.o 	: $(OSAT)/spccb05sa.f                                  \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/tracer.com

$(PA)/cpamech3.o 	: $(PA)/cpamech3.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/procan.com

$(PA)/cpamech4.o 	: $(PA)/cpamech4.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/procan.com

$(PA)/cpamech5.o 	: $(PA)/cpamech5.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/procan.com

$(PA)/cpamech6.o 	: $(PA)/cpamech6.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/procan.com

$(PA)/cparad.o 		: $(PA)/cparad.f                                       \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/tracer.com         \
                        $(CAMx_INC)/procan.com $(CAMx_INC)/chmstry.com

$(PA)/initipr.o 	: $(PA)/initipr.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/procan.com

$(PA)/pagrids.o 	: $(PA)/pagrids.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/procan.com

$(PA)/pasetup.o 	: $(PA)/pasetup.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/tracer.com            \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/procan.com

$(PA)/pazero.o 		: $(PA)/pazero.f                                       \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com         \
                        $(CAMx_INC)/procan.com

$(PA)/rdoptpa.o         : $(PA)/rdoptpa.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/procan.com            \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/namelist.com

$(PA)/wrtcpa.o 		: $(PA)/wrtcpa.f                                       \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/filunit.com         \
                        $(CAMx_INC)/flags.com

$(PA)/wrtipr.o 		: $(PA)/wrtipr.f                                       \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/procan.com

$(PA)/wrtiprhdr.o 	: $(PA)/wrtiprhdr.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/procan.com

$(PA)/wrtirr.o 		: $(PA)/wrtirr.f                                       \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/grid.com           \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/procan.com

$(PA)/wrtirrhdr.o 	: $(PA)/wrtirrhdr.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/procan.com

$(PIG)/avepig.o 	: $(PIG)/avepig.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/chmstry.com         \
                        $(CAMx_INC)/pigsty.com $(CAMx_INC)/filunit.com

$(PIG)/irondry.o 	: $(PIG)/irondry.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/pigsty.com            \
                        $(CAMx_INC)/chmstry.com

$(PIG)/ironchem.o 	: $(PIG)/ironchem.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com

$(PIG)/ironwet.o 	: $(PIG)/ironwet.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/pigsty.com $(CAMx_INC)/chmstry.com

$(PIG)/pigcoord.o 	: $(PIG)/pigcoord.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/bndary.com

$(PIG)/pigdiag.o 	: $(PIG)/pigdiag.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/pigsty.com            \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/flags.com

$(PIG)/pigdrive.o 	: $(PIG)/pigdrive.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/chmstry.com        \
                        $(CAMx_INC)/ahomap.com $(CAMx_INC)/pigsty.com          \
                        $(CAMx_INC)/deposit.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/rtracchm.com

$(PIG)/pigevol.o 	: $(PIG)/pigevol.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/grid.com           \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/filunit.com          \
                        $(CAMx_INC)/procan.com

$(PIG)/piggrow.o 	: $(PIG)/piggrow.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/pigsty.com            \
                        $(CAMx_INC)/filunit.com

$(PIG)/piginit.o 	: $(PIG)/piginit.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/ptemiss.com $(CAMx_INC)/pigsty.com         \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/tracer.com         \
                        $(CAMx_INC)/rtracchm.com

$(PIG)/pigmscl.o 	: $(PIG)/pigmscl.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/pigsty.com            \
                        $(CAMx_INC)/chmstry.com

$(PIG)/pigprep.o 	: $(PIG)/pigprep.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/ptemiss.com           \
                        $(CAMx_INC)/pigsty.com $(CAMx_INC)/chmstry.com         \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/filunit.com          \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/tracer.com         \
                        $(CAMx_INC)/rtracchm.com

$(PIG)/pigsampl.o	: $(PIG)/pigsampl.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/pigsty.com         \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/rtracchm.com

$(PIG)/pigwalk.o 	: $(PIG)/pigwalk.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/camxfld.com        \
                        $(CAMx_INC)/pigsty.com $(CAMx_INC)/flags.com

$(PIG)/smpprep.o	: $(PIG)/smpprep.f                                     \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/flags.com             \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/pigsty.com         \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/tracer.com         \
                        $(CAMx_INC)/rtracchm.com

$(PIG)/virtdump.o 	: $(PIG)/virtdump.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/pigsty.com $(CAMx_INC)/filunit.com

$(PIG)/walk1puf.o 	: $(PIG)/walk1puf.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com

$(PIG)/wrtpig.o 	: $(PIG)/wrtpig.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/pigsty.com $(CAMx_INC)/flags.com           \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com         \
                        $(CAMx_INC)/rtracchm.com

$(PIG)/wrtsmp.o		: $(PIG)/wrtsmp.f                                      \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com

$(RTRAC)/chemrt.o 	: $(RTRAC)/chemrt.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/grid.com           \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/rtracchm.com

$(RTRAC)/cvticrt.o 	: $(RTRAC)/cvticrt.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/camx.com $(CAMx_INC)/tracer.com            \
                        $(CAMx_INC)/rtracchm.com

$(RTRAC)/drydeprt.o 	: $(RTRAC)/drydeprt.f                                  \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/deposit.com         \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/filunit.com        \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/rtracchm.com

$(RTRAC)/empreprt.o 	: $(RTRAC)/empreprt.f                                  \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/flags.com             \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com         \
                        $(CAMx_INC)/rtracchm.com

$(RTRAC)/hdrcprt.o 	: $(RTRAC)/hdrcprt.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/rtracchm.com        \
                        $(CAMx_INC)/filunit.com

$(RTRAC)/pigchmrt.o	: $(RTRAC)/pigchmrt.f                                  \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/rtracchm.com

$(RTRAC)/rdarrt.o 	: $(RTRAC)/rdarrt.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/grid.com           \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/tracer.com          \
                        $(CAMx_INC)/rtracchm.com

$(RTRAC)/rdbcrt.o 	: $(RTRAC)/rdbcrt.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/chmstry.com        \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/flags.com             \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/tracer.com          \
                        $(CAMx_INC)/rtracchm.com

$(RTRAC)/rdchmrt.o 	: $(RTRAC)/rdchmrt.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/chmstry.com           \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/tracer.com           \
                        $(CAMx_INC)/rtracchm.com

$(RTRAC)/rdicrt.o 	: $(RTRAC)/rdicrt.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/chmstry.com        \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/flags.com             \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/tracer.com          \
                        $(CAMx_INC)/rtracchm.com

$(RTRAC)/rdptrt.o 	: $(RTRAC)/rdptrt.f                                    \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/filunit.com $(CAMx_INC)/flags.com          \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/grid.com           \
                        $(CAMx_INC)/bndary.com $(CAMx_INC)/tracer.com          \
                        $(CAMx_INC)/rtracchm.com

$(RTRAC)/rdrcprt.o 	: $(RTRAC)/rdrcprt.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/rtracchm.com        \
                        $(CAMx_INC)/procan.com $(CAMx_INC)/filunit.com

$(RTRAC)/startrt.o	: $(RTRAC)/startrt.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/grid.com              \
                        $(CAMx_INC)/flags.com $(CAMx_INC)/pigsty.com           \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/namelist.com        \
                        $(CAMx_INC)/procan.com

$(RTRAC)/wetdeprt.o 	: $(RTRAC)/wetdeprt.f                                  \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/bndary.com            \
                        $(CAMx_INC)/chmstry.com $(CAMx_INC)/tracer.com         \
                        $(CAMx_INC)/rtracchm.com

$(RTRAC)/wrrcprt.o 	: $(RTRAC)/wrrcprt.f                                   \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/tracer.com $(CAMx_INC)/rtracchm.com        \
                        $(CAMx_INC)/filunit.com

$(SOAP)/soachem.o 	: $(SOAP)/soachem.f                                    \
                        $(CAMx_INC)/soap.com

$(SOAP)/soapdat.o 	: $(SOAP)/soapdat.f                                    \
                        $(CAMx_INC)/soap.com

$(SOAP)/soap.o 		: $(SOAP)/soap.f                                       \
                        $(CAMx_INC)/soap.com
 
$(HG)/hggaschem.o 	: $(HG)/hggaschem.f                                    \
                        $(CAMx_INC)/hggaschm.prm
 
$(HG)/hgaqschem.o 	: $(HG)/hgaqschem.f                                    \
                        $(CAMx_INC)/hgaqschm.prm

$(IOHDF)/camx_includes.o: $(IOHDF)/camx_includes.f                             \
                        $(CAMx_INC)/camx.prm $(CAMx_INC)/camx.com              \
                        $(CAMx_INC)/grid.com $(CAMx_INC)/filunit.com           \
                        $(CAMx_INC)/camxfld.com $(CAMx_INC)/chmstry.com        \
                        $(CAMx_INC)/pigsty.com $(CAMx_INC)/tracer.com

$(IOHDF)/hdf5_f2c.o	: $(IOHDF)/hdf5_f2c.c                                  \
                        $(IOHDF)/utils_sub_names.h

$(IOHDF)/hdf5_file_out.o: $(IOHDF)/hdf5_file_out.F90                           \
                        $(IOHDF)/camx_includes.f $(IOHDF)/rams_head.f90        \
                        $(IOHDF)/hdf5_utils.F90

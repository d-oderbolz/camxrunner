      subroutine camxerr()
      use filunit
c   
c----CAMx v5.30 101223
c   
c     CAMXERR writes the final message whenever CAMx terminates
c             due to an error
c                             
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c             
c     Modifications:   
c        none 
c 
c     Input arguments:
c        none 
c 
c     Output arguments: 
c        none 
c               
c     Routines Called:   
c        none 
c               
c     Called by:   
c        AHOPREP
c        AREAPREP
c        AVGRCP
c        BCMODFY
c        BNDPREP
c        CHEMDRIV
c        CLCBWT
c        CLCEWT
c        CLCIWT
c        CNCPREP
c        DDMJAC
c        DIEHSOLV
c        DRYDEP
c        EMPREPSA
c        GETDEPTH
c        HDRRCP
c        HDRWSA
c        IEHSOLV
c        INIPTR
c        INSTSA
c        KTHERM
c        OPENFILS
c        PARNTCHD
c        IRONDRIVE
c        IRONGROW
c        GRESDRIVE
c        GRESGROW
c        PIGINIT
c        PIGPREP
c        PIGWALK
c        RADINIT
c        RADSLVR
c        RDARGRP
c        RDFGCON
c        RDFGSA
c        RDINSTSA
c        RDOPTSA
c        RDPTGRP
c        RDPTHDR
c        RDSUMBC
c        READAHO
c        READAR
c        READARSA
c        READBND
c        READCHM
c        READCNC
c        READINP
c        READPT
c        READPTSA
c        METINIT
c        RERCP
c        RESMAP
c        SPECSA
c        STARTSA
c        STARTUP
c        SUMGRPS
c        TRAP
c        VNMSHCAL
c        VRTSLV
c 
      include "camx.prm"
      include "flags.inc"
      include "mpif.h"
c
c-----Entry point
c
      write(*,*) 
      write(*,*) 
      write(*,*) ' CAMx is stopping because an error has occured'
      write(*,*) ' See the .out output file for details'
      write(*,*) 
      write(*,*) 
      call flush(6)
c
      write(iout,*) 
      write(iout,*) 
      write(iout,*) ' CAMx is stopping because of the error(s) ',
     &              'described above'
      write(iout,*) 
      write(iout,*) 
      call flush(iout)
c
      if( lmpi ) call MPI_Abort(MPI_COMM_WORLD,1,ierr)
c
      call exit(1)
c
      end

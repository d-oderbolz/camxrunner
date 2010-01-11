      subroutine camxerr()
c   
c---CAMx v4.42 070603
c   
c     CAMXERR writes the final message whenever CAMx terminates
c             due to an error
c                             
c     Copyright 1996-2007
c     ENVIRON International Corporation
c             
c     Modifications:   
c        Replaced stop with call exit(1) , daniel.oderbolz@psi.ch (2009-06-28)
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
c        KTHERM
c        OPENFILS
c        PARNTCHD
c        PIGDRIVE
c        PIGGROW
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
c        WCONSA
c        WSFCSA
c        WFCONSA
c        WFSFCSA
c 
      include "camx.prm"
      include "filunit.com"
c
c-----Entry point
c
      write(*,*) 
      write(*,*) 
      write(*,*) ' CAMx is stopping because an error has occured'
      write(*,*) ' See the .out output file for details'
      write(*,*) 
      write(*,*) 
c
      write(iout,*) 
      write(iout,*) 
      write(iout,*) ' CAMx is stopping because of the error(s) ',
     &              'described above'
      write(iout,*) 
      write(iout,*) 
c
c call exit(1) sets a return value of 1
      call exit(1)
c
      end

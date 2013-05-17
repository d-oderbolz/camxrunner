c
c----CAMx v6.00 130506
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c
c     Include file for passing aerosol module parameters
c     to AERO routines
c
      real*8 dsec_c(NSECP1)
      real*8 dsecf_c(NSECP1)
      real*8 dmin
      real*8 dmax
c
      common /sections/ dmin, dmax, dsec_c, dsecf_c

c
c-----Optional flags for the PM routines
c
      character*4 chaero
      character*4 chaq
      common /aeroopt/ chaero, chaq

      real aqcwmin
      real aqtamin
      common /aqcond/ aqcwmin, aqtamin
   
      logical lfrst
      common /init/ lfrst

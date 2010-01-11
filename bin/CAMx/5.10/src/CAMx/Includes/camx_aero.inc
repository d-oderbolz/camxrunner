c
c----CAMx v5.10 090918
c
c     Copyright 1996 - 2009
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
c
c-----Adjustment factors for aerosol molecular weights
c     for interface between aerosol modules (= CAMx M/Aerosol M)
c
c      real*4 wtfac_ae(MXSPEC)
       real*4 wtfac_aq(MXSPEC)
c
c      common /wtfac/ wtfac_ae, wtfac_aq

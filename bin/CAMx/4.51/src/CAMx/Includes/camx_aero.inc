c
c-----CAMx v4.51 080522
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c     Include file for passing aerosol module parameters
c     to AERO routines
c
      real*8 dsec_c(nsecp1),dsecf_c(nsecp1)
      real*8 dmin,dmax
c
      common /sections/ dmin,dmax,dsec_c,dsecf_c

c
c-----Optional flags for the PM routines
c
      character*4 chaero,chaq
      common /aeroopt/chaero,chaq

      real aqcwmin,aqtamin
      common /aqcond/aqcwmin,aqtamin
   
      logical lfrst
      common /init/ lfrst
c
c-----Adjustment factors for aerosol molecular weights
c     for interface between aerosol modules (= CAMx M/Aerosol M)
c
c      real*4 wtfac_ae(MXSPEC), wtfac_aq(MXSPEC)
c
c      common /wtfac/ wtfac_ae,wtfac_aq

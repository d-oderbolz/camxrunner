c----CAMx v6.00 130506
c
c     LSBOX.COM passes information to LSODE for gas phase chemistry
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
      real             rrk(MXREACT)
      common /rrate/   rrk
c$omp threadprivate(/rrate/)
c
      real*8           dbrk(MXREACT)
      common /drate/   dbrk
c$omp threadprivate(/drate/)
c
      double precision dH2O
      double precision dM
      double precision dO2
      double precision dCH4
      double precision dH2
      common /dwat/    dH2O, dM, dO2, dCH4, dH2
c$omp threadprivate(/dwat/)

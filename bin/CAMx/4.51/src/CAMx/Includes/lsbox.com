c-----CAMx v4.51 080522
c
c     LSBOX.COM passes information to LSODE for gas phase chemistry
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c 
      real             rrk(MXRXN) 
      common /rrate/   rrk
c$omp threadprivate(/rrate/)
c
      real*8           dbrk(MXRXN) 
      common /drate/   dbrk
c$omp threadprivate(/drate/)
c
      real*8           dH2O, dM, dO2, dCH4, dH2
      common /dwat/    dH2O, dM, dO2, dCH4, dH2
c$omp threadprivate(/dwat/)

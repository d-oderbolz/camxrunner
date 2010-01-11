c----CAMx v5.10 090918
c
c     LSBOX.COM passes information to LSODE for gas phase chemistry
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c 
      double precision dH2O
      double precision dM
      double precision dO2
      double precision dCH4
      double precision dH2
      common /dwat/    dH2O, dM, dO2, dCH4, dH2
c$omp threadprivate(/dwat/)

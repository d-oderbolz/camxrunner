c-----CAMx v4.42 070514
c
c     LSBOX.COM passes information to LSODE for gas phase chemistry
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c 
      double precision dbrk(mxrxn), 
     &                 dH2O, dM, dO2, dCH4, dH2,
     &                 jac(mxspec+mxradcl+1,mxspec+mxradcl+1)
      common /drate/   dbrk
      common /dwat/    dH2O, dM, dO2, dCH4, dH2
      common /ljac/    jac

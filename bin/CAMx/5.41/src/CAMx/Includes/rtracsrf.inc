c**** RTRACSRF
c
c----CAMx v5.41 121109
c
c
c----------------------------------------------------------------------
c
c    Include file for variables used in the RTRAC surface model.
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     10/29/09   -cemery-   original development
c
c-----------------------------------------------------------------------
c   Variables for the RTRAC surface and partitioning models
c-----------------------------------------------------------------------
c
c   lsrfmod   L   flag to invoke the RTRAC surface model 
c   lparttn   L   flag to invoke RTRAC gas/PM partitioning
c   fsoil     R   fraction of soil as f(LU)
c   fsoiloc   R   fraction of soil that is organic matter as f(LU)
c   fshad     R   fraction of shading as f(LU)
c
      logical      lsrfmod
      logical      lparttn
      real         fsoil(NLUW89)
      real         fsoiloc(NLUW89)
      real         fshad(NLUW89)
c
      common /rtsrfpart/ lsrfmod, lparttn, fsoil, fsoiloc, fshad
c

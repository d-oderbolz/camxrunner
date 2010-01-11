c-----CAMx v4.51 080522
c  
c     PTEMISS.COM contains all information regarding elevated point sources
c                            
c     Copyright 1996-2008
c     ENVIRON International Corporation
c            
c     Modifications:  
c        none
c 
c-----------------------------------------------------------------------
c     Variables for point source parameters:
c
c     nptsrc  -- number of point source stacks
c     xstk    -- stack x-location (km or degrees)
c     ystk    -- stack y-location (km or degrees)
c     lpiglet -- flag indicating a PiG point source
c     idstk   -- stack ID
c     hstk    -- stack height (meters)
c     dstk    -- stack diameter (meters)
c     tstk    -- stack exit temperature (K)
c     vstk    -- stack exit velocity (meters/s)
c     effph   -- effective plume height (meters)
c                normally ignored; if negative, PLUMERIS is overridden
c     ptemis  -- point source emission rates (mol/s)
c-----------------------------------------------------------------------
c
      character*20 idstk(MXPTSRC)
      integer      nptsrc
      real         xstk(MXPTSRC,MXGRID)
      real         ystk(MXPTSRC,MXGRID)
      real         hstk(MXPTSRC)
      real         dstk(MXPTSRC)
      real         tstk(MXPTSRC)
      real         vstk(MXPTSRC)
      real         effph(MXPTSRC)
      real         ptemis(MXPTSRC,MXSPEC)
      logical      lpiglet(MXPTSRC)
c
      common /ptechr/ idstk
      common /ptemiss/ nptsrc, xstk, ystk, lpiglet, hstk,
     &                 dstk, tstk, vstk, effph, ptemis

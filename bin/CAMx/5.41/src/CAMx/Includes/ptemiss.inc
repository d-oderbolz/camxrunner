c----CAMx v5.41 121109
c  
c     PTEMISS.COM contains all information regarding elevated point sources
c                            
c     Copyright 1996 - 2012
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
c     hstk    -- stack height (meters)
c     dstk    -- stack diameter (meters)
c     tstk    -- stack exit temperature (K)
c     vstk    -- stack exit velocity (meters/s)
c     effph   -- effective plume height (meters)
c                normally ignored; if negative, PLUMERIS is overridden
c     ptemis  -- point source emission rates (mol/s)
c     xloc    -- temporary array for stack locations
c     yloc    -- temporary array for stack locations
c-----------------------------------------------------------------------
c
      real,         allocatable, dimension(:,:) :: xstk
      real,         allocatable, dimension(:,:) :: ystk
      real,         allocatable, dimension(:)   :: hstk
      real,         allocatable, dimension(:)   :: dstk
      real,         allocatable, dimension(:)   :: tstk
      real,         allocatable, dimension(:)   :: vstk
      real,         allocatable, dimension(:)   :: effph
      real,         allocatable, dimension(:,:) :: ptemis
      logical,      allocatable, dimension(:)   :: lpiglet
      real,         allocatable, dimension(:)   ::  xloc
      real,         allocatable, dimension(:)   ::  yloc
c
      integer      nptsrc
c
      common /ptesdat/ nptsrc

c----CAMx v5.41 121109
c  
c     AHOMAP.COM contains albedo/haze/ozone index maps and value classes
c                            
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c            
c     Modifications:  
c        none  
c
c-----------------------------------------------------------------------
c     Variables to contain the albedo/haze/ozone data:
c
c     albcl   -- albedo column lookup table
c     hazcl   -- haze column lookup table
c     ozcl    -- ozone column lookup table
c     ruflen  -- optional input roughness length table
c     nhdraho -- number of lines to start of time-varying data
c     icdalb  -- index by cell/grid for albedo
c     icdhaz  -- index by cell/grid for haze 
c     icdozn  -- index by cell/grid for ozone column
c     icdsno  -- optional index by cell/grid for snow cover
c     icdocn  -- optional index by cell/grid for ocean
c     icddrt  -- optional index by cell/grid for drought
c     icdruf  -- optional index by cell/grid for roughness
c     lrdalb  -- flag by grid to show data read for albedo
c     lrdsno  -- flag to show data read for snow cover
c     lrdocn  -- flag by grid to show data read for ocean
c     lrddrt  -- flag by grid to show data read for drought index
c     lrdruf  -- flag by grid to show data read for roughness
c
c-----------------------------------------------------------------------
c
      real,    allocatable, dimension(:) :: albcl
      real,    allocatable, dimension(:) :: hazcl
      real,    allocatable, dimension(:) :: ozcl
      real,    allocatable, dimension(:) :: ruflen
      integer, allocatable, dimension(:) :: icdalb
      integer, allocatable, dimension(:) :: icdhaz
      integer, allocatable, dimension(:) :: icdozn
      integer, allocatable, dimension(:) :: icdsno
      integer, allocatable, dimension(:) :: icdocn
      integer, allocatable, dimension(:) :: icddrt
      integer, allocatable, dimension(:) :: icdruf
      logical, allocatable, dimension(:) :: lrdalb
      logical, allocatable, dimension(:) :: lrdocn
      logical, allocatable, dimension(:) :: lrddrt
      logical, allocatable, dimension(:) :: lrdruf
c
      integer   nhdraho
      logical   lrdsno
c 
      common /ahodat/ nhdraho, lrdsno

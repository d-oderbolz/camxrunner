c----CAMx v5.41 121109
c 
c     GRID.COM contains all grid characteristic and relational information
c                           
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c           
c     Modifications: 
c        none 
c
c-----------------------------------------------------------------------
c     Variables defining grid sizes/extents/projections:
c
c     ncol    -- number of columns for each grid
c     nrow    -- number of rows for each grid
c     nlay    -- number of layers for each grid
c     deltax  -- grid cell size in x-direction for each grid (m)
c     deltay  -- grid cell size in y-direction for each grid (m)
c     xorg    -- input grid x-origin (SW corner) (km or degrees)
c     yorg    -- input grid y-origin (SW corner) (km or degrees)
c     delx    -- input grid cell size in x-direction (km or degrees)
c     dely    -- input grid cell size in y-direction (km or degrees)
c     iuzon   -- UTM zone
c     itzon   -- time zone (5=EST,8=PST,0=UTC)
c     ngrid   -- total number of grids in simulation
c     nnest   -- number of fine grid nests in simulation
c     polelon -- longitude of Polar Stereographic projection pole (degrees)
c     polelat -- latitude of Polar Stereographic projection pole (degrees)
c     xlonc   -- longitude of center of Lambert Conformal projection (degrees)
c     ylatc   -- latitude of center of Lambert Conformal projection (degrees)
c     tlat1   -- 1st true latitude of Lamber Conformal projection (degrees)
c     tlat2   -- 2nd true latitude of Lamber Conformal projection (degrees)
c-----------------------------------------------------------------------
c
      integer, allocatable, dimension(:)   :: ncol
      integer, allocatable, dimension(:)   :: nrow
      integer, allocatable, dimension(:)   :: nlay
      real,    allocatable, dimension(:,:) :: deltax
      real,    allocatable, dimension(:)   :: deltay
      integer iuzon
      integer itzon
      integer ngrid
      integer nnest
      real    xorg
      real    yorg
      real    delx
      real    dely
      real    polelon
      real    polelat
      real    xlonc
      real    ylatc
      real    tlat1
      real    tlat2
c
      common /grdinp/ xorg, yorg, delx, dely, iuzon, itzon, ngrid, 
     &                nnest, polelon, polelat, xlonc, ylatc, tlat1, 
     &                tlat2
c
c-----------------------------------------------------------------------
c     Variables for defining the grid nests:
c
c     idfin   -- flag that identifies the nests in the current grid
c     ntim    -- Number of timesteps per parent step in current grid
c     ntimcrs -- Number of timesteps per coarse grid step in current grid
c     nadv    -- Number of sub-steps per timestep by layer in current grid
c     i1      -- i-starting index relative to parent grid
c     j1      -- j-starting index relative to parent grid
c     i2      -- i-ending index relative to parent grid
c     j2      -- j-ending index relative to parent grid
c     nmesh   -- meshing factor relative to parent grid
c     nmshv   -- vertical meshing factor for current grid
c     nchdrn  -- number of children fine grid nests for current grid
c     idchdrn -- ID of children fine grid nests for current grid
c     nosrc   -- number of point sources in current grid
c     idsrc   -- point source ID for current grid
c     isrc    -- i-location of point source
c     jsrc    -- j-location of point source
c     inst1   -- i-starting index relative to coarse grid
c     inst2   -- i-ending index relative to coarse grid
c     jnst1   -- j-starting index relative to coarse grid
c     jnst2   -- j-ending index relative to coarse grid
c     meshold -- input meshing factor relative to coarse grid
c     mapgrd  -- parent grid to which current grid maps
c     l3davg  -- flag for 3-D output average fields
c     iptr2d  -- pointer array for 2-D variables
c     iptr2d_full  -- pointer array for 2-D variables for entire grid
c     iptr3d  -- pointer array for 3-D variables
c     iptr3d_full  -- pointer array for 3-D variables for entire grid
c     iptr4d  -- pointer array for 4-D variables
c     iptrav  -- pointer array for averaging variables
c     iptrem  -- pointer array for emissions
c     iptrad  -- pointer array for radicals
c     iptrlu  -- pointer array for landuse
c     iptrdp  -- pointer array for deposition fields
c     ipsa3d  -- pointer array for 3-D source apportionment variables
c     ipsa2d  -- pointer array for 2-D source apportionment variables
c     ipsadep -- pointer array for source apportionment deposition variables
c-----------------------------------------------------------------------
c
      integer, allocatable, dimension(:)   :: idfin
      integer, allocatable, dimension(:)   :: ntim
      integer, allocatable, dimension(:)   :: ntimcrs
      integer, allocatable, dimension(:,:) :: nadv
      integer, allocatable, dimension(:)   :: i1
      integer, allocatable, dimension(:)   :: j1
      integer, allocatable, dimension(:)   :: i2
      integer, allocatable, dimension(:)   :: j2
      integer, allocatable, dimension(:)   :: nmesh
      integer, allocatable, dimension(:,:) :: nmshv
      integer, allocatable, dimension(:)   :: nchdrn
      integer, allocatable, dimension(:,:) :: idchdrn
      integer, allocatable, dimension(:)   :: nosrc
      integer, allocatable, dimension(:,:) :: idsrc
      integer, allocatable, dimension(:,:) :: isrc
      integer, allocatable, dimension(:,:) :: jsrc
c
      integer, allocatable, dimension(:)   :: inst1
      integer, allocatable, dimension(:)   :: inst2
      integer, allocatable, dimension(:)   :: jnst1
      integer, allocatable, dimension(:)   :: jnst2
      integer, allocatable, dimension(:)   :: meshold
      integer, allocatable, dimension(:)   :: mapgrd
c
      logical, allocatable, dimension(:)   :: l3davg
c
      integer, allocatable, dimension(:)   :: iptr2d
      integer, allocatable, dimension(:)   :: iptr2d_full
      integer, allocatable, dimension(:)   :: iptr3d
      integer, allocatable, dimension(:)   :: iptr3d_full
      integer, allocatable, dimension(:)   :: iptr4d
      integer, allocatable, dimension(:)   :: iptrav
      integer, allocatable, dimension(:)   :: iptrem
      integer, allocatable, dimension(:)   :: iptrad
      integer, allocatable, dimension(:)   :: iptrlu
      integer, allocatable, dimension(:)   :: iptrdp
      integer, allocatable, dimension(:)   :: ipsa3d
      integer, allocatable, dimension(:)   :: ipsa2d
      integer, allocatable, dimension(:)   :: ipsadep
c
c-----------------------------------------------------------------------
c     Variable for the darkness flag:
c
c     ldark   --  set to true when darkness is upon us
c-----------------------------------------------------------------------
c 
      logical, allocatable, dimension(:) :: ldark

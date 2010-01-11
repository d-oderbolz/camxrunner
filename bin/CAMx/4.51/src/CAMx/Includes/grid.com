c-----CAMx v4.51 080522
c 
c     GRID.COM contains all grid characteristic and relational information
c                           
c     Copyright 1996-2008
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
      integer ncol(MXGRID),nrow(MXGRID),nlay(MXGRID)
      real deltax(MXROWA,MXGRID),deltay(MXGRID)
c
      common /grdsz/  ncol,nrow,nlay,deltax,deltay
c
      real xorg,yorg,delx,dely,polelon,polelat,xlonc,ylatc,tlat1,tlat2
      integer iuzon,itzon,ngrid,nnest
c
      common /grdinp/ xorg,yorg,delx,dely,iuzon,itzon,ngrid,nnest,
     &                polelon,polelat,xlonc,ylatc,tlat1,tlat2
c
c-----------------------------------------------------------------------
c     Variables for defining the grid nests:
c
c     idfin   -- flag that identifies the nests in the current grid
c     ntim    -- Number of timesteps per master step in current grid
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
c     iptr2d  -- pointer array for 2-D variables
c     iptr3d  -- pointer array for 3-D variables
c     iptr4d  -- pointer array for 4-D variables
c     iptrem  -- pointer array for emissions
c     iptrad  -- pointer array for radicals
c     iptrlu  -- pointer array for landuse
c     iptrdp  -- pointer array for deposition fields
c     ipsa3d  -- pointer array for 3-D source apportionment variables
c     ipsa2d  -- pointer array for 2-D source apportionment variables
c-----------------------------------------------------------------------
c
      integer   idfin(MXVEC2D)
      integer   ntim(MXGRID)
      integer   nadv(MXLAYA,MXGRID)
      integer   i1(MXGRID),j1(MXGRID),i2(MXGRID),j2(MXGRID)
      integer   nmesh(MXGRID),nmshv(MXLAYA,MXGRID)
      integer   nchdrn(MXGRID),idchdrn(MXCHDRN,MXGRID)
      integer   nosrc(MXGRID),idsrc(MXPTSRC,MXGRID)
      integer   isrc(MXPTSRC,MXGRID),jsrc(MXPTSRC,MXGRID)
c
      common /nest1/ idfin,ntim,nadv,i1,j1,i2,j2,nmesh,nmshv,
     &               nchdrn,idchdrn,nosrc,idsrc,isrc,jsrc
c
      integer inst1(MXGRID),inst2(MXGRID),jnst1(MXGRID),
     &        jnst2(MXGRID),meshold(MXGRID),mapgrd(MXGRID)
c
      common /nest2/inst1,inst2,jnst1,jnst2,meshold,mapgrd
c
      integer iptr2d(MXGRID), iptr3d(MXGRID), iptr4d(MXGRID),
     &        iptrem(MXGRID), iptrad(MXGRID), iptrlu(MXGRID),
     &        iptrdp(MXGRID), ipsa3d(MXGRID), ipsa2d(MXGRID)
c
      common /ptrdat/ iptr2d, iptr3d, iptr4d,
     &                iptrem, iptrad, iptrlu,
     &                iptrdp, ipsa3d, ipsa2d
c
c-----------------------------------------------------------------------
c     Variable for the darkness flag:
c
c     ldark   --  set to true when darkness is upon us
c-----------------------------------------------------------------------
c 
      logical ldark(MXVEC2D)
c
      common /pigchm/ ldark

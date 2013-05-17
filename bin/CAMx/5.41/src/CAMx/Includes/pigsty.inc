c----CAMx v5.41 121109
c
c     PIGSTY.COM contains general Plume-in-Grid variables
c                           
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c           
c     Modifications: 
c        9/3/03        Updated with IRON PiG variables 
c        7/20/05       Moved PiG sampling grid variables from TRACER.COM
c        2/02/06       Removed GREASD-PiG specific variables, renamed
c                      PUFFPOL to PUFFMASS
c        3/31/08       Added metrics for average age of killed puffs
c        6/04/09       Added sigmax
c
c-----------------------------------------------------------------------
c     Variables for PiG submodel:
c
c     npig     -- number of active PiG puffs
c     nreactr  -- number of chemical reactors in each puff
c     loverlap -- puff chemistry overlap flag
c     ipigint  -- grid index possessing the minimum timestep of all grids
c                 for which new puffs will be released and all puffs will
c                 be transported
c     ingrd    -- grid ID in which puff resides
c     idpig    -- stack ID from which puff is released
c     xpigf    -- current x-location of puff front (km or deg)
c     xpigb    -- current x-location of puff back (km or deg)
c     ypigf    -- current y-location of puff front (km or deg)
c     ypigb    -- current y-location of puff back (km or deg)
c     zpig     -- current z-location of puff (meters)
c     axisy    -- width of puff (meters)
c     axisz    -- depth of puff (meters)
c     sigy     -- Gaussian standard deviation width (meters)
c     sigx     -- Gaussian standard deviation length (meters)
c     sigz     -- Gaussian standard deviation depth (meters)
c     lnewt    -- flag indicating newly released puff for transport
c     lnewg    -- flag indicating newly released puff for growth
c     puffmass -- puff pollutant mass
c     fmspig   -- puff volume parameter
c     agepigf  -- puff age of front end (seconds)
c     agepigb  -- puff age of back end (seconds)
c     npigon   -- number of active puffs in grid
c     pufftop  -- height of top of puff (m)
c     puffbot  -- height of bottom of puff (m)
c     pufffad  -- initial guess for radicals in puff reactors
c     htfms    -- horizontal turbulent flux moment, shear (m2/s)
c     htfmb    -- horizontal turbulent flux moment, buoyancy (m2/s)
c     vtfms    -- vertical turbulent flux moment, shear (m2/s)
c     vtfmb    -- vertical turbulent flux moment, buoyancy (m2/s)
c     nkill    -- number of puffs killed by the following processes:
c                 1 = size
c                 2 = fraction of remaining mass (or age)
c                 3 = convergence error in LSODE (background step)
c                 4 = convergence error in LSODE (total puff step)
c                 5 = puff entered Stage 3 chem
c                 6 = Neg concs in aqueous PM chem (background step)
c                 7 = Neg concs in aqueous PM chem (total puff step)
c                 8 = Neg concs in PM chem (background step)
c                 9 = Neg concs in PM chem (total puff step)
c     pigage   -- mean puff age when killed by grid
c     nage     -- counter for puff age by grid
c     ipufmap  -- region from which each PiG puff originated
c     ipufgrp  -- group from which each PiG puff originated
c     LSlice   -- keeps track of which MPI slice a puff is in
c     vpconc   -- gridded array to keep track of concentrations with puffs
c
c-----------------------------------------------------------------------
c
      logical, allocatable, dimension(:)     :: lnewt
      logical, allocatable, dimension(:)     :: lnewg
c
      integer, allocatable, dimension(:)     :: ingrd
      integer, allocatable, dimension(:)     :: idpig
      integer, allocatable, dimension(:)     :: npigon
      integer, allocatable, dimension(:)     :: nage
c
      real,    allocatable, dimension(:)     :: xpigf
      real,    allocatable, dimension(:)     :: xpigb
      real,    allocatable, dimension(:)     :: ypigf
      real,    allocatable, dimension(:)     :: ypigb
      real,    allocatable, dimension(:)     :: zpig
      real,    allocatable, dimension(:)     :: axisy
      real,    allocatable, dimension(:)     :: axisz
      real,    allocatable, dimension(:)     :: sigy
      real,    allocatable, dimension(:)     :: sigx
      real,    allocatable, dimension(:)     :: sigz
      real,    allocatable, dimension(:,:,:) :: puffmass
      real,    allocatable, dimension(:)     :: fmspig
      real,    allocatable, dimension(:)     :: agepigf
      real,    allocatable, dimension(:)     :: agepigb
      real,    allocatable, dimension(:)     :: pufftop
      real,    allocatable, dimension(:)     :: puffbot
      real,    allocatable, dimension(:)     :: htfms
      real,    allocatable, dimension(:)     :: htfmb
      real,    allocatable, dimension(:)     :: vtfms
      real,    allocatable, dimension(:)     :: vtfmb
      real,    allocatable, dimension(:)     :: pigage
c
      integer, allocatable, dimension(:)   :: ipufmap
      integer, allocatable, dimension(:)   :: ipufgrp

      integer, allocatable, dimension(:,:,:) :: Lslice
c
      real, allocatable, dimension(:,:,:,:) :: vpconc
c
      logical loverlap
      integer npig
      integer nreactr
      integer ipigint
      integer nkill(9)
c
      common /pig1/ loverlap, npig, nreactr, ipigint, nkill
c
c-----------------------------------------------------------------------
c  Parameters for PiG sampling grids:
c-----------------------------------------------------------------------
c
c   lsample  L  sampling grid flag
c   lbckgrd  L  include background concs in sampling grid output
c   nsample  I  number of sampling grids
c   nsmpcels I  number of total sampling grids (size of smpgrd array)
c   ismp1,2  I  x-dir beginning/ending master grid cells
c   jsmp1,2  I  y-dir beginning/ending master grid cells
c   meshsmp  I  sampling grid meshing factor relative to master grid
c   ncolsmp  I  number of sampling grid columns (based on modeling grid)
c   nrowsmp  I  number of sampling grid rows (based on modeling grid)
c   ismpgrd  I  pointer to finest nest containing sampling grid
c   ipsmp    I  pointer array for 2-D PiG sampling grid variables
c   xorgsmp  R  x-origin of sampling grid (deg or km)
c   yorgsmp  R  y-origin of sampling grid (deg or km)
c   smpcnc   R  time-averaged concentration array on sampling grid
c               (gasses=ppm, PM=ug/m3)
c
      integer, allocatable, dimension(:)     :: ismp1
      integer, allocatable, dimension(:)     :: ismp2
      integer, allocatable, dimension(:)     :: jsmp1
      integer, allocatable, dimension(:)     :: jsmp2
      integer, allocatable, dimension(:)     :: meshsmp
      integer, allocatable, dimension(:)     :: ncolsmp
      integer, allocatable, dimension(:)     :: nrowsmp
      integer, allocatable, dimension(:)     :: ismpgrd
      integer, allocatable, dimension(:)     :: ipsmp
      real,    allocatable, dimension(:)     :: xorgsmp
      real,    allocatable, dimension(:)     :: yorgsmp
      real,    allocatable, dimension(:)     :: smpcnc
c
      integer nsmpcels
      logical lsample
      logical lbckgrd
      integer nsample
c
      common /pigsamp/ nsmpcels, lsample, lbckgrd, nsample

c----CAMx v6.00 130506
c  
c     CHMSTRY.COM contains all chemistry variables 
c                            
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c            
c     Modifications:  
c       4/4/00     Added aerosol deposition variables to /aerochm/
c       1/9/02     Aerosol size cut points and density now a function of
c                  species
c       8/20/02    Added minimum CWC to define presence of clouds
c      12/12/02    Expanded species list for Mechanism 4
c       1/10/03    Added array for deposition output species names
c      10/14/04    Modifications for mechanism 10
c      11/30/06    Split into two include files to accamodate block-data
c      12/29/06    Expanded species list for the updated SOA scheme
c      01/08/06    Expanded species list for Mechanism 6 (CB05)
c                  Now ipigrxn is a scalar pointer to NO self-reaction
c      07/04/07    Added pointer to hydrolysis of N2O5
c      12/15/08    Added code to handle averaging of radicals
c      03/29/11    Support in-line TUV with aerosol optical depth
c 
c-----------------------------------------------------------------------
c    Variables for the number of species in input files:
c
c    narspc --  number of species in the surface emissions file
c    lgas   --  TRUE if species is a gas species
c
c-----------------------------------------------------------------------
c
       integer, allocatable, dimension(:) :: narspc
       logical, allocatable, dimension(:) :: lgas
c
c-----------------------------------------------------------------------
c     Variables for storing chemical reaction data:
c
c     rk     -- reaction rate constant (ppm/hr)
c     ltdep  -- flag to determine if rate constant is temperature dependent
c     lpdep  -- flag to determine if rate constant is pressure dependent
c     bdnl   -- lower vound value for each modeled species (ppm)
c
c-----------------------------------------------------------------------
c
      logical, allocatable, dimension(:) :: ltdep
      logical, allocatable, dimension(:) :: lpdep
      real,    allocatable, dimension(:) :: bdnl
c
      real    rk(10000)
      common /chmratep/ rk
c$omp threadprivate(/chmratep/)
c
c-----------------------------------------------------------------------
c     Variables for photolysis data:
c
c     idphot1  -- ID of primary photolysis reactions
c     idphot2  -- ID of secondary (scaled) photolysis reactions 
c     idphot3  -- ID of primary photolysis reaction to scale to obtain
c                 the secondary photolysis reaction
c     phtscl   -- photolysis reaction scaling factor
c
c-----------------------------------------------------------------------
c
      integer, allocatable, dimension(:) :: idphot1
      integer, allocatable, dimension(:) :: idphot2
      integer, allocatable, dimension(:) :: idphot3
      real,    allocatable, dimension(:) :: phtscl
c 
c-----------------------------------------------------------------------
c     Variables for species names:
c
c     spname  --  name of each modeled species
c     spavg   --  name of each species to be written to the output file
c     depsp   --  name of each deposition species output to file
c
c-----------------------------------------------------------------------
c
      character*10, allocatable, dimension(:) :: spname
      character*10, allocatable, dimension(:) :: spavg
      character*10, allocatable, dimension(:) :: depsp
c 
c-----------------------------------------------------------------------
c     Variables for mapping input species to internal model order:
c
c     lbcmap   -- mapping of species in the boundary condition file
c     lavmap   -- mapping of species written to average file
c     lptmap   -- mapping of species in the point source emissions file
c     larmap   -- mapping of species in the surface emissions file
c     licmap   -- mapping of species in the initial conditions file
c     ldepmap  -- mapping of species written to deposition file
c
c-----------------------------------------------------------------------
c
      integer, allocatable, dimension(:)   :: lbcmap
      integer, allocatable, dimension(:)   :: lavmap
      integer, allocatable, dimension(:)   :: lptmap
      integer, allocatable, dimension(:,:) :: larmap
      integer, allocatable, dimension(:,:) :: licmap
      integer, allocatable, dimension(:)   :: ldepmap
c
c-----------------------------------------------------------------------
c     Variables for chemistry lookup tables:
c
c     tempr  -- temperature table
c     presr  -- pressure table
c     rktbl  -- temperature/pressure-dependent rate constant table
c     htint  -- height AGL table
c     zenint -- zenith angle table
c     prkn   -- reaction rate table
c
c-----------------------------------------------------------------------
c      
      real, allocatable, dimension(:)           :: tempr
      real, allocatable, dimension(:)           :: presr
      real, allocatable, dimension(:)           :: htint
      real, allocatable, dimension(:)           :: zenint
      real, allocatable, dimension(:,:,:)       :: rktbl
      real, allocatable, dimension(:,:,:,:,:,:) :: prkn
c
c-----------------------------------------------------------------------
c     Variables to define parameters for each chemical species:
c
c     henry0   -- Henry's Law constant at STP (molar/atm)
c     tfact    -- Temperature dependence of Henry's Law constant (1/K)
c     diffrat  -- Species diffusivity
c     f0       -- Species reactivity parameter
c     rscale   -- Species scaling factor for surface resistance
c     roprt    -- Aerosol density (g/m3)
c     bext     -- Aerosol dry extinction efficiency (m2/ug)
c     ssa      -- Aerosol single scatter albedo
c     rhadj    -- Aerosol hygroscopic growth flag (1=apply RH adjustment)
c     dcut     -- Aerosol size bin cut points (um)
c
c-----------------------------------------------------------------------
c
      real, allocatable, dimension(:) :: henry0
      real, allocatable, dimension(:) :: tfact
      real, allocatable, dimension(:) :: diffrat
      real, allocatable, dimension(:) :: f0
      real, allocatable, dimension(:) :: rscale
c
      real, allocatable, dimension(:)   :: roprt
      real, allocatable, dimension(:,:) :: dcut
      real, allocatable, dimension(:)   :: bext
      real, allocatable, dimension(:)   :: ssa
      integer, allocatable, dimension(:) :: rhadj
c
c----------------------------------------------------------------------
c    Variables for controlling calls to aerosol routines
c
c     time_aero  -- next time to call aerosol routines for each grid (HHMM)
c     date_aero  -- next date to call aerosol routines for each grid (YYJJJ)
c     aero_dt    -- incremented (actual) time (hr) between calls to aerosol 
c                   routines for each grid
c
      integer, allocatable, dimension(:) :: date_aero
      real,    allocatable, dimension(:) :: time_aero
      real,    allocatable, dimension(:) :: aero_dt

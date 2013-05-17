c**** RTRACCHM
c
c----CAMx v6.00 130506
c
c
c----------------------------------------------------------------------
c
c    Include file for parameters and data structures used in the source 
c    apportionment version of the CAMx.  This file is specific to the
c    RTRAC technology.
c    The file TRACER.COM must be included prior to including this file.
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     01/23/02   -gwilson-  original development
c     11/12/03   -cemery-   added LREG logical for each RTRAC species
c     10/29/09   -cemery-   added surface model and gas/PM partitioning options
c
c-----------------------------------------------------------------------
c   Arrays for defining species in the RTRAC technology:
c-----------------------------------------------------------------------
c
c   nrtgas    I   number of gas species used
c   lrtgas    L   .TRUE. if any gas species are used
c   nrtaero   I   number of aerosol (PM) species used
c   lrtaero   L   .TRUE. if any aerosol (PM) species are used
c   nrtrac    I   number of species total
c   nrtphot   I   number of photolysis reactions
c   nrtherm   I   number of thermal reactions
c   lsecnd    L   flag for determining if species is primary or secondary
c   lreg      L   flag for determining is secondary species maps to regular
c                 model species
c   ksec      I   index into regular model arrays of species which acts 
c                 primary to the secondary species
c   rtlbnd    R   lower bound for RTRAC species
c   rthlaw    R   Henry's Law constant for RTRAC gas species
c   rttfact   R   temperature dependence of Henry's Law constant
c   rtdrate   R   diffusivity ratio for RTRAC gas species
c   rtreact   R   reactivity parameter for RTRAC gas species
c   rtscale   R   surface resistance scaling factor for RTRAC gas species
c   rtdens    R   density of RTRAC PM species
c   rtlcut    R   lower size cut for RTRAC PM species
c   rtucut    R   upper size cut for RTRAC PM species
c
      logical,      allocatable, dimension(:) :: lsecnd
      logical,      allocatable, dimension(:) :: lreg
      integer,      allocatable, dimension(:) :: ksec
      real,         allocatable, dimension(:) :: rtlbnd
      real,         allocatable, dimension(:) :: rthlaw
      real,         allocatable, dimension(:) :: rttfact
      real,         allocatable, dimension(:) :: rtdrate
      real,         allocatable, dimension(:) :: rtreact
      real,         allocatable, dimension(:) :: rtscale
      real,         allocatable, dimension(:) :: rtdens
      real,         allocatable, dimension(:) :: rtlcut
      real,         allocatable, dimension(:) :: rtucut
c
      integer      nrtgas
      logical      lrtgas
      integer      nrtaero
      logical      lrtaero
      integer      nrtrac
      integer      nrtphot
      integer      nrtherm
c
      common /rtspdat/ nrtgas, lrtgas, nrtaero, lrtaero, nrtrac, 
     &                 nrtphot, nrtherm
c
c-----------------------------------------------------------------------
c   Parameters for reactions in RTRAC technology:
c-----------------------------------------------------------------------
c
c  NAMOH    C   string for identifying the OH species
c  NAMNO3   C   string for identifying the NO3 species
c  NAMO3    C   string for identifying the O3 species
c
      character*10 NAMOH
      character*10 NAMNO3
      character*10 NAMO3
c
      parameter( NAMOH  = 'OH        ' )
      parameter( NAMNO3 = 'NO3       ' )
      parameter( NAMO3  = 'O3        ' )
c
c-----------------------------------------------------------------------
c   Variables for gas-phase reactions in RTRAC technology:
c      thermal rate constant = A * (T/Tref)**B * exp(Ea/T)
c-----------------------------------------------------------------------
c
c  jnum     I   photolysis reaction number
c  rtjfact  R   scale factor used in phololysis reaction
c  aoh      R   array of A  coefficients for OH
c  eaoh     R   array of Ea coefficients for OH
c  boh      R   array of B  coefficients for OH
c  troh     R   array of Tref coefficients for OH
c  ano3     R   array of A  coefficients for NO3
c  eano3    R   array of Ea coefficients for NO3
c  bno3     R   array of B  coefficients for NO3
c  trno3    R   array of Tref coefficients for NO3
c  ao3      R   array of A  coefficients for O3
c  eao3     R   array of Ea coefficients for O3
c  bo3      R   array of B  coefficients for O3
c  tro3     R   array of Tref coefficients for O3
c
      integer, allocatable, dimension(:) :: jnum
      real,    allocatable, dimension(:) :: rtjfact
      real,    allocatable, dimension(:) :: aoh
      real,    allocatable, dimension(:) :: eaoh
      real,    allocatable, dimension(:) :: boh
      real,    allocatable, dimension(:) :: troh
      real,    allocatable, dimension(:) :: ano3
      real,    allocatable, dimension(:) :: eano3
      real,    allocatable, dimension(:) :: bno3
      real,    allocatable, dimension(:) :: trno3
      real,    allocatable, dimension(:) :: ao3
      real,    allocatable, dimension(:) :: eao3
      real,    allocatable, dimension(:) :: bo3
      real,    allocatable, dimension(:) :: tro3
c
c-----------------------------------------------------------------------
c   Arrays for gridded fields for RTRAC species:
c-----------------------------------------------------------------------
c
c   vdeprt   R   gridded array of deposition velocities 
c   rtsolmas  R   gridded array of surface soil mass
c   rtvegmas  R   gridded array of surface vegetation mass
c
      real,    allocatable, dimension(:) :: vdeprt
      real,    allocatable, dimension(:) :: rtsolmas
      real,    allocatable, dimension(:) :: rtvegmas
c
c-----------------------------------------------------------------------
c   Variables for the RTRAC surface and partitioning models
c-----------------------------------------------------------------------
c
c   eqkoa     R   octanol-air equilibrium partitioning constant (unitless)
c   khydro    R   hydrolysis rate (1/day)
c   kleach    R   soil leaching rate (1/day)
c   kpen      R   vegetation penetration rate (1/day)
c   kphot     R   photolysis rate (1/day)
c
      real,    allocatable, dimension(:) :: eqkoa
      real,    allocatable, dimension(:) :: khydro
      real,    allocatable, dimension(:) :: kleach
      real,    allocatable, dimension(:) :: kpen
      real,    allocatable, dimension(:) :: kphot
c
c-----------------------------------------------------------------------
c   Parameters and Arrays for receptor definitions:
c-----------------------------------------------------------------------
c
c    MXRTCEL  I  maximum number of receptor cells 
c
      integer MXRTCEL
c
      parameter( MXRTCEL = 100 )
c
c    ircprt   I  cell index in X direction for receptor location
c    jrcprt   I  cell index in Y direction for receptor location
c    krcprt   I  cell index in Z direction for receptor location
c    idomrt   I  grid containing the receptor
c    nrcprt   I  number of receptors requested
c    rcpdcy   R  gas tracer decay rates at each receptor location
c
      integer, allocatable, dimension(:)   :: ircprt
      integer, allocatable, dimension(:)   :: jrcprt
      integer, allocatable, dimension(:)   :: krcprt
      integer, allocatable, dimension(:)   :: idomrt
      real*4,  allocatable, dimension(:,:) :: rcpdcy
c
      integer nrcprt
c
      common /rtrcp/ nrcprt

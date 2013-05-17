c----CAMx v5.41 121109
c
c     CAMxFLD.COM contains all multidimensional fields for CAMx
c 
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c     Modifications:
c        1/10/03    Added deposition field array
c        1/23/03    New cloud parameters from cloud/rain file
c       10/12/04    Added time-dependent fields for water vapor and
c                   vertical diffusivity
c        6/21/05    Added fields for rain, snow, and graupel cloud water
c       10/24/05    Added cloud pH field
c        4/27/06    Added topo field
c
c-----------------------------------------------------------------------
c     Variables for 2-dimensional fields:
c
c     cellon --  array of longitudes for the cell centroids (deg)
c     cellat --  array of lattitudes for the cell centroids (deg)
c     mapscl --  array of map-scale factors at cell centroids
c     topo   --  array of topographic elevation (m MSL)
c     lai    --  array of leaf area index
c     lrdlai --  flag indicating that LAI was read
c     tsurf  --  array for surface temperature field (K)
c     pspt   --  array for time-rate change of surface temperature (K/s) 
c     sfcz0  --  array of surface roughness (m)
c-----------------------------------------------------------------------
c
      real, allocatable, dimension(:) :: cellon
      real, allocatable, dimension(:) :: cellat
      real, allocatable, dimension(:) :: mapscl
      real, allocatable, dimension(:) :: topo
      real, allocatable, dimension(:) :: lai
      real, allocatable, dimension(:) :: tsurf
      real, allocatable, dimension(:) :: pspt
      real, allocatable, dimension(:) :: sfcz0
      logical, allocatable, dimension(:) :: lrdlai
c
c-----------------------------------------------------------------------
c     Variables for 3-dimensional fields:
c
c      windu  --  U-component of the wind field (m/s)
c      windv  --  V-component of the wind field (m/s)
c      pupt   --  time-rate change of U-component wind (m/s2)
c      pvpt   --  time-rate change of V-component wind (m/s2)
c      tempk  --  temperature field (K) 
c      ptpt   --  time-rate change of temperature (K/s)
c      press  --  pressure field (mb)
c      pppt   --  time-rate change of pressure (mb/s)
c      height --  layer interface height field (m)
c      phpt   --  time-rate change of layer interface height (m/s)
c      rkv    --  vertical diffusion coefficient field (m2/s)
c      pkpt   --  time-rate change of Kv (m2/s2)
c      water  --  water vapor concentration field (ppm)
c      pwpt   --  time-rate change of water vapor (ppm/s)
c      fcloud --  vertically-accumulating cloud coverage field (fraction)
c      depth  --  layer depth field (m)
c      rkx    --  horizontal diffusion coefficient in X direction (m2/s)
c      rky    --  horizontal diffusion coefficient in Y direction (m2/s)
c      aremis --  surface emissions (moles/hour or g/hour)
c      ctop   --  top concentrations
c      cwc    --  cloud water content (g/m3)
c      pwr    --  rain water content (g/m3)
c      pws    --  snow water content (g/m3)
c      pwg    --  graupel water content (g/m3)
c      cod    --  cloud optical depth
c      cldtrns -  Cloud energy transmission coefficient (fraction)
c      cph    --  cloud water pH
c      fluxtmp -- temporary gridded variable used to store fluxes for diffusion
c
c-----------------------------------------------------------------------
c
      real, allocatable, dimension(:) :: windu
      real, allocatable, dimension(:) :: windv
      real, allocatable, dimension(:) :: pupt
      real, allocatable, dimension(:) :: pvpt
      real, allocatable, dimension(:) :: tempk
      real, allocatable, dimension(:) :: ptpt
      real, allocatable, dimension(:) :: press
      real, allocatable, dimension(:) :: pppt
      real, allocatable, dimension(:) :: height
      real, allocatable, dimension(:) :: phpt
      real, allocatable, dimension(:) :: rkv
      real, allocatable, dimension(:) :: pkpt
      real, allocatable, dimension(:) :: water
      real, allocatable, dimension(:) :: pwpt
      real, allocatable, dimension(:) :: fcloud
      real, allocatable, dimension(:) :: depth
      real, allocatable, dimension(:) :: rkx
      real, allocatable, dimension(:) :: rky
      real, allocatable, dimension(:) :: aremis
      real, allocatable, dimension(:) :: ctop
      real, allocatable, dimension(:) :: cwc
      real, allocatable, dimension(:) :: pwr
      real, allocatable, dimension(:) :: pws
      real, allocatable, dimension(:) :: pwg
      real, allocatable, dimension(:) :: cod
      real, allocatable, dimension(:) :: cldtrns
      real, allocatable, dimension(:) :: cph
      real, allocatable, dimension(:,:,:,:) :: fluxtmp
c
c-----------------------------------------------------------------------
c     Variables for 4-dimensional fields:
c
c      conc   --  species concentrations field (umol/m3)
c      avcnc  --  average species concentration (gas=ppm,other=ug/m3)
c
c-----------------------------------------------------------------------
c
      real, allocatable, dimension(:) :: conc
      real, allocatable, dimension(:) :: avcnc
c
c-----------------------------------------------------------------------
c     Variables for boundary condition concentrastions
c
c      bndconc -- concentrations on the edge cells
c
c-----------------------------------------------------------------------
c
      real, allocatable, dimension(:,:,:,:) :: bndconc
c
c-----------------------------------------------------------------------
c     Variables for calculating vertical transport:
c     NOTE:  These fields are over-written when processing each grid.
c
c     entrn  --  entrainment rate (m/s)
c     dilut  --  dilution rate (m/s)
c-----------------------------------------------------------------------
c   
      real, allocatable, dimension(:) :: entrn
      real, allocatable, dimension(:) :: dilut
c
c-----------------------------------------------------------------------
c     Variables for calculating depostion rates:
c
c     vdep   --  species-dependent deposition velocity field (m/s)
c     fsurf  --  fractional landuse cover field (fraction)
c     depfld --  2-D array containing dry, wet dep mass (mol/ha, g/ha) and
c                precip liquid concentration (mol/l, g/l)
c-----------------------------------------------------------------------
c
      real, allocatable, dimension(:) :: vdep
      real, allocatable, dimension(:) :: fsurf
      real, allocatable, dimension(:) :: depfld
c
      integer, parameter :: EX_SCRATCH=100
      integer  mvecscr
      integer  mvecscr_dp
      real, allocatable, dimension(:) :: scr1
      real, allocatable, dimension(:) :: scr1_dp
      integer  :: dp_identifier = 111 
      integer  :: rad_identifier = 711
c
c-----------------------------------------------------------------------
c     Variables for mass flux calculations:
c
c     xmass   -- current total grid mass by species (moles)
c     xmass0  -- initial total grid mass by species (moles)
c     armass  -- total area emissions mass by species (moles)
c     ptmass  -- total point emissions mass by species (moles)
c     fluxes  -- array of mass tranport by species (moles)
c     xmschem -- mass change due to chemistry by species (moles)
c     xmsold  -- total grid mass by species before last process (moles)
c     resid   -- residual (error) mass by species (moles)
c     xmsfin  -- current total fine grid mass by species (moles)
c     xmstmp  -- temporary total fine grid mass by species (moles)
c     pigdump -- total mass transferred from PiG to grid by species (umol)
c     pigmass -- total mass in PiG by species (umol)
c     pgmserr -- error in mass due to aborted pig dumping (umol)
c
c-----------------------------------------------------------------------
c
      real*8, allocatable, dimension(:,:) :: xmass
      real*8, allocatable, dimension(:,:) :: xmass0
      real*8, allocatable, dimension(:,:) :: armass
      real*8, allocatable, dimension(:,:) :: ptmass
      real*8, allocatable, dimension(:,:) :: fluxes
      real*8, allocatable, dimension(:,:) :: xmschem
      real*8, allocatable, dimension(:,:) :: xmsold
      real*8, allocatable, dimension(:,:) :: resid
      real*8, allocatable, dimension(:,:) :: xmsfin
      real*8, allocatable, dimension(:,:) :: xmstmp
      real*8, allocatable, dimension(:,:) :: pigdump
      real*8, allocatable, dimension(:,:) :: pigmass
      real*8, allocatable, dimension(:,:) :: pgmserr
c
c-----------------------------------------------------------------------
c     Variables for mass balance calculations for the extent of the
c     simulation:
c
c     tarmass  -- total area emissions mass by species (moles)
c     tptmass  -- total point emissions mass by species (moles)
c     tfluxes  -- array of mass tranport by species (moles)
c     txmschem -- mass change due to chemistry by species (moles)
c     tresid   -- residual (error) mass by species (moles)
c     txmsfin  -- current total fine grid mass by species (moles)
c-----------------------------------------------------------------------
c
      real*8, allocatable, dimension(:,:) :: tarmass
      real*8, allocatable, dimension(:,:) :: tptmass
      real*8, allocatable, dimension(:,:) :: tfluxes
      real*8, allocatable, dimension(:,:) :: tresid
      real*8, allocatable, dimension(:,:) :: txmschem
      real*8, allocatable, dimension(:,:) :: txmsfin
c
c-----------------------------------------------------------------------
c     Variables for storing the next hours meteorological fields:
c
c     hnxt   --  layer interface height at next hour (m)
c     pnxt   --  pressure field at next hour (mb)
c     unxt   --  U-component of the wind field at next hour (m/s)
c     vnxt   --  V-component of the wind field at next hour (m/s)
c     tnxt   --  temperature field at next hour (K) 
c     tsnxt  --  surface temperature field at next hour (K)
c     knxt   --  vertical diffusivity field at next hour (m2/s)
c     wnxt   --  water vapor field at next hour (ppm)
c-----------------------------------------------------------------------
c
      real, allocatable, dimension(:) :: hnxt
      real, allocatable, dimension(:) :: pnxt
      real, allocatable, dimension(:) :: unxt
      real, allocatable, dimension(:) :: vnxt
      real, allocatable, dimension(:) :: tnxt
      real, allocatable, dimension(:) :: tsnxt
      real, allocatable, dimension(:) :: knxt
      real, allocatable, dimension(:) :: wnxt

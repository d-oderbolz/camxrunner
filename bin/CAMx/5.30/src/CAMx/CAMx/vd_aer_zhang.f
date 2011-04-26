      subroutine vd_aer_zhang(z2,zl,z0_f,ustar,diam,rhop,ts,t2,mlu,
     &                        lai_f,vd)
c
c----CAMx v5.30 101223
c 
c     VD_AER_ZHANG is the driver to interface with the dry deposition module
c     of Zhang et al. (2001) developed by the Meteorological Service of Canada.
c     This routine prepares arguments to be used in the module.
c 
c     The DRYVD module considers fractions of multiple land uses.  Since we
c     call DRYVD for a single landuse and weight appropriately in DRYPEP later,
c     the land use fraction set to be 1 for the current land use category.
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c           
c     Modifications:
c        None
c
c     Input arguments:
c        z2                  reference height (m)
c        zl                  Z/L stability parameter
c        z0_f                surface roughness (m)
c        ustar               friction velocity (m/s)
c        diam                log-mean sectional aerosol diameter (m)
c        rhop                aerosol density (g/m3)
c        ts                  surface temperature (K)
c        t2                  temperature at z2 (K)
c        mlu                 land use index
c        lai_f               Leaf area index
c
c     Output arguments: 
c        vd                  deposition velocity (cm/s)
c             
c     Routines called: 
c        DRYVD_PM
c             
c     Called by: 
c        DRYDEP
c
      implicit none
c
c-----Arguments
c
      integer mlu
      real z2
      real zl
      real z0_f
      real ustar
      real diam
      real rhop
      real ts
      real t2
      real lai_f
      real vd
c
c-----Local variables
c
      real vdp
c
c-----Entry point
c
c-----Call dry deposition velocity algorithm
c
      call dryvd_pm(z2,zl,z0_f,ustar,t2,ts,lai_f,mlu,vdp,diam,rhop)
      vd = vdp
c
      return
      end
C
C=======================================================================
C
      subroutine dryvd_pm(z2,zl,z0_f,ustar,t2,ts,lai_f,mlu,vdp,
     &                    diam,rhoprt)
C
C-----------------------------------------------------------------------
C    References: 
C      Zhang et al.,2001.  Atmos. Environ. 35, 4463-4470
C
C
C     LUC No.     Vegetation type
C     =====       ===============
C       1         water
C       2         ice
C       3         inland lake
C       4         evergreen needleleaf trees
C       5         evergreen broadleaf trees
C       6         deciduous needleleaf trees
C       7         deciduous broadleaf trees
C       8         tropical broadleaf trees
C       9         drought deciduous trees
C       10        evergreen broadleaf shrub
C       11        deciduous shrubs
C       12        thorn shrubs
C       13        short grass and forbs
C       14        long grass
C       15        crops
C       16        rice
C       17        sugar
C       18        maize
C       19        cotton
C       20        irrigated crops
C       21        urban
C       22        tundra
C       23        swamp
C       24        desert
C       25        mixed wood forests
C       26        transitional forest 
C-----------------------------------------------------------------------
C        Key   variables
C-----------------------------------------------------------------------
C  ARGUMENTS:
C  z2       | Met reference height (m)
C  zl       | Z/L stability parameter
C  z0_f     | Surface roughness (m)
C  ustar    | friction velocity (m/s)
C  t2       | Temperature at Z2 (K)
C  ts       | Surface temperature (K)
C  lai_f    | Leaf area index
C  mlu      | Land Use type
C  vdp      | particle dry deposition velocity (m/s)
C  diam     | log-mean sectional aerosol diameter (m)
C  rhoprt   | aerosol density (g/m3)
C
C  LOCAL VARIABLES:
C  ra       | Aerodynamic resistance (s/m)         
C  aest     | parameter for calculating eim 
C  binsize  ! radius of a size bin
C  eb       | Brownian collection efficiency 
C  eim      | impaction collection efficiency 
C  ein      | interception collection efficiency 
C  gama     | parameter for calculating EB 
C  pllp     | leaf dimenision for calculating EIN 
C
C-----------------------------------------------------------------------
C
      implicit none
c 
      include 'camx.prm'
      include 'deposit.inc'
c
c-----Arguments
c
      integer mlu
      real z2
      real zl
      real z0_f
      real ustar
      real t2
      real ts
      real vdp
      real diam
      real rhoprt
      real lai_f
c
c-----Local variables
c      
      integer i
      real aa1,aa2,aa3,amfp,roarow,boltzk,rhop,dair,dh2o,ra,pllp,
     &     binsize,tave,amu,anu,prii,priiv,vphil,cfac,taurel,amob,diff,
     &     schm,pdepv,st,eb,eim,ein,r1,rs,vdsize
      real aest(NLUZ03), pllp1(NLUZ03), pllp2(NLUZ03), gama(NLUZ03)
C
C   Parameters for collection efficiency 
C   PLLP1 and PLLP2 are minimum and maximum leaf dimension for each LUC. 
C
      data   aest       /
     & 100. , 50.  , 100. ,  1.0 ,  0.8 ,  
     &  1.1 , 0.8  ,  0.6 ,  1.0 ,  1.1 ,
     &  1.1 , 1.2  ,  1.2 ,  1.2 ,  1.2 ,
     &  1.2 , 1.2  ,  1.1 ,  1.2 ,  1.2 ,
     &  1.5 , 50.  ,  2.0 ,  50. ,  0.8 ,
     &  0.8         /
      data   gama       /
     &  0.5 , 0.54 ,  0.5 , 0.56 , 0.56 ,
     & 0.56 , 0.56 , 0.58 , 0.56 , 0.55 ,
     & 0.55 , 0.54 , 0.54 , 0.55 , 0.54 ,
     & 0.54 , 0.54 , 0.55 , 0.54 , 0.54 ,
     & 0.56 , 0.54 , 0.54 , 0.54 , 0.56 ,
     & 0.56        / 
      data   pllp1         /
     & -0.9 , -0.9 , -0.9 ,  2.0 ,  5.0 ,
     &  2.0 ,  5.0 ,  5.0 ,  5.0 ,  5.0 ,
     &  5.0 ,  2.0 ,  2.0 ,  2.0 ,  2.0 ,
     &  2.0 ,  5.0 ,  5.0 ,  5.0 ,  2.0 ,
     & 10.0 , -0.9 , 10.0 , -0.9 ,  5.0 ,
     &  5.0        /
      data   pllp2         /
     & -0.9 , -0.9 , -0.9 ,  2.0 ,  5.0 ,
     &  5.0 , 10.0 ,  5.0 , 10.0 ,  5.0 ,
     & 10.0 ,  5.0 ,  5.0 ,  5.0 ,  5.0 ,
     &  5.0 , 10.0 , 10.0 , 10.0 ,  5.0 ,
     & 10.0 , -0.9 , 10.0 , -0.9 ,  5.0 ,
     &  5.0        /
C
C ---  parameters for air dynamic properties 
      data aa1/1.257/, aa2/0.4/, aa3/1.1/
      data amfp /6.53E-8/, roarow/1.19/,
     &     boltzk/1.3806044503487214E-23/   
c
c --- Particle density
      RHOP = rhoprt * 1.0E-3 ! convert a unit from g/m3 to kg/m3

C   Some constants 
C
      dair=0.369*29.+6.29  
      dh2o=0.369*18.+6.29

      VDP = 0.0
      I = MLU
C
C     Aerodynamic resistance above canopy
C
      IF(ZL.GE.0.) THEN
          Ra=(.74*ALOG(Z2/Z0_F)+4.7*ZL)/0.4/USTAR 
      ELSE
          Ra=0.74/0.4/USTAR*(ALOG(Z2/Z0_F)-
     &          2*ALOG((1+SQRT(1-9.*ZL))*0.5))
      ENDIF
      Ra=amax1(RA,5.0)
      if (I.EQ.1.OR.I.EQ.3) THEN
        Ra=amin1(RA,2000.)
      else
        Ra=amin1(RA,1000.)
      end if 
C
C Loop 150 for particle species 
C
      PLLP = PLLP2(I)-(LAI_F-lai_ref(I,14))/(lai_ref(I,15)
     &         -lai_ref(I,14)+1.E-10)*(PLLP2(I)-PLLP1(I)) 
      BINSIZE = diam/2.   ! radius, m

      TAVE=0.5*(T2+TS)
C
C     * AIR'S DYNAMIC VISCOSITY
C
      AMU=145.8*1.E-8*TAVE**1.5/(TAVE+110.4)
C
C     * AIR'S KINEMATIS VISCOSITY
C
      ANU=AMU/ROAROW
C
C     * CUNNINGHAM SLIP CORRECTION FACTOR AND RELAXATION TIME = vg/Grav.
C
      PRII=2./9.*9.81/AMU
      PRIIV=PRII*(RHOP-ROAROW)
      VPHIL=0.
      CFAC=1.+AMFP/BINSIZE*(AA1+AA2*EXP(-AA3*BINSIZE/AMFP))
      TAUREL=AMAX1(PRIIV*BINSIZE**2*CFAC/9.81,0.0)
C
C     * STOKES FRICTION AND DIFFUSION COEFFICIENTS.
C
      AMOB=6.*3.14*AMU*BINSIZE/CFAC
      DIFF=BOLTZK*TAVE/AMOB
      SCHM=ANU/DIFF
C
C     * GRAVITATIONAL SETTLING VELOCITY.
C
      PDEPV =TAUREL*9.81
C
C     * Efficiency by diffusion, impaction, interception and particle rebound.
C
      if (PLLP .le. 0.) then ! smooth surface
         St=TAUREL*USTAR*USTAR/ANU
      else ! vegetated surface
         St=TAUREL*USTAR/PLLP*1000.
      end if
      EB= SCHM**(-gama(I)) 
      EIM=( St/(St+AEST(I)))**2
      EIN=0.0
      if (PLLP .gt. 0.00) THEN
         EIN=(1000.*2.*BINSIZE/PLLP)**2*0.5 
      end if
      R1= EXP(-ST**0.5)
      IF (R1.LT.0.5) R1=0.5
      RS= 1./3./USTAR/(EB+EIM+EIN)/R1
C
C -- Deposition velocity 
C
      VDSIZE = PDEPV + 1./(RA+RS) 
      VDP = VDSIZE 

      RETURN
      END

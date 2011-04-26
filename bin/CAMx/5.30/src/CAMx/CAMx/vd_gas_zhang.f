      subroutine vd_gas_zhang(z2,zl,z0_f,ustar,t2,ts,srad,rh,fcld,pp,
     &                        coszen,mlu,isesn,henry,henso2,
     &                        lsp,lai_f,vd)
      use chmstry
c
c----CAMx v5.30 101223
c 
c     VD_GAS_ZHANG is the driver to interface with the dry deposition module
c     of Zhang et al. (2003) developed by the Meteorological Service of Canada.
c     This routine prepares arguments to be used in the module.
c 
c     The DRYVD module considers fractions of multiple land uses.  Since we
c     call DRYVD for a single landuse and weight appropriately in DRYPEP later,
c     the land use fraction set to be 1 for the current land use category.
c
c     CAMx does not track snow depth explicitly, which is required in
c     the Canadian model. Therefore, the snow fraction is set 0.8 for
c     season = 4 (defined as winter with snow present), and 0.0 in other seasons.
c
c     The list of gas species between CAMx and the Canadian model are 
c     not identical.  Any CAMx species not handled by the Canadian model
c     need to have their mesophyll resistances calculated.
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c           
c     Modifications:
c        None
c
c     Input arguments:
c        z2                  met reference height (m)
c        zl                  Z/L stability parameter
c        z0_f                surface roughness (m)
c        ustar               friction velocity (m/s)
c        t2                  temperature at z2 (K)
c        ts                  surface temperature (K)
c        srad                solar irradiance (W/m2)   
c        rh                  relative humidity (0-1) 
c        fcld                cloud fraction (0-1)
c        pp                  precipitation water content (g/m3)
c        coszen              cosine of solar zenith angle
c        mlu                 land use index (1-26)
c        isesn               season index (1-5, 4=winter with snow)
c        henry               Henry's Law constant (M/atm)
c        henso2              Henry's Law constant of SO2 (M/atm)
c        lsp                 gas species index
c        lai_f               leaf area index
c
c     Output arguments: 
c        vd                  deposition velocity (m/s)
c             
c     Routines called: 
c        DRYVD 
c             
c     Called by: 
c        DRYDEP
c
      implicit none
      include 'camx.prm'
c
c-----Arguments
c
      integer mlu
      integer isesn
      integer lsp
      real z2
      real zl
      real z0_f
      real ustar
      real t2
      real ts
      real*8 srad
      real rh
      real fcld
      real pp
      real*8 coszen
      real henry
      real henso2
      real lai_f
      real vd
c
c-----Local variables
c
      integer i
      real rhoh2o,diffh2o,rmin,rmax,fsnow,volrat,prec,alpha,beta,rm,di,
     &     rscalsp
      real*8 vdg

      real*8 z2_tmp,zl_tmp,z0_f_tmp,ustar_tmp,fsnow_tmp,t2_tmp
      real*8 ts_tmp,rh_tmp,fcld_tmp,prec_tmp
      real*8 rm_tmp,alpha_tmp,beta_tmp,di_tmp,lai_f_tmp,rscalsp_tmp
c
c-----Data statements
c 
      data rhoh2o  /1.e6/         ! water density (g/m3)
      data diffh2o /2.30e-05/     ! water diffusivity (m2/s)
      data rmin/1.0/, rmax/100./  !min/max resistances (s/m)
c
c-----Entry point
c
c-----Set a snow fraction
c
      fsnow = 0.0
      if (isesn.eq.4) fsnow = 0.8
c
c-----Set precipitation rate
c
      volrat = pp/rhoh2o
      prec = (volrat/1.0e-7)**1.27    ! precipitation rate (mm/hr) 
c
c-----Set deposition params for species.
c     Mesophyll resistance follows Wesely (1989)
c
      alpha = henry/henso2
      beta = f0(lsp)
      rm = 1./(henry/3000. + 100.*f0(lsp)) 
      rm = max1(rmin,rm)
      rm = min1(rmax,rm)
c
c-----Set diffusivity of gas species
c
      di = diffh2o/diffrat(lsp) ! m2/s
      di = di*1.e4              ! cm2/s
      rscalsp = rscale(lsp)
c
c-----Call dry deposition velocity algorithm
c
      z2_tmp = z2
      zl_tmp = zl
      z0_f_tmp = z0_f
      ustar_tmp = ustar
      fsnow_tmp = fsnow
      t2_tmp = t2
      ts_tmp = ts
      rh_tmp = rh
      fcld_tmp = fcld
      prec_tmp = prec
      rm_tmp = rm
      alpha_tmp = alpha
      beta_tmp = beta
      di_tmp = di
      lai_f_tmp = lai_f
      rscalsp_tmp = rscalsp
      call dryvd(z2_tmp,zl_tmp,z0_f_tmp,ustar_tmp,fsnow_tmp,
     &         t2_tmp,ts_tmp,srad,rh_tmp,fcld_tmp,prec_tmp,coszen,
     &                        mlu,rm_tmp,alpha_tmp,beta_tmp,di_tmp,vdg,
     &                                             lai_f_tmp,rscalsp_tmp)
      vd = vdg
c
      return
      end
C
C=======================================================================
C
      subroutine dryvd(z2,zl,z0_f,ustar,fsnow,t2,ts,srad,rh,fcld,prec,
     &                 coszen,mlu,rm,alpha,beta,di,vdg,lai_f,
     &                 rscalsp) 
C
C-----------------------------------------------------------------------
C    References: 
C      Zhang et al., 2003.  Atmos. Chem. Phys. 3, 2067-2082 
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
C        KEY   VARIABLES
C-----------------------------------------------------------------------
C  ARGUMENTS:
C  z2       | Met reference height (m)
C  zl       | Z/L stability parameter
C  z0_f     | Surface roughness (m)
C  ustar    | friction velocity (m/s)
C  fsnow    | Snow fraction  (0-1)              
C  t2       | Temperature at Z2 (K)      
C  ts       | Surface temperature (K)             
C  srad     | Solar irradiance (w/m2)          
C  rh       | relative humidity (0.0-1.0)                
C  fcld     | Cloud fraction (0-1)              
C  prec     | hourly precipitation (mm/hour) 
C  coszen   | Cosine of solar zenith angle
C  mlu      | Land Use type          
C  rm       | mesophyll resistance (s/m)           
C  alpha    | Scaling factor based on SO2 (no unit) 
C  beta     | Scaling factor based on O3  (no unit) 
C  di       | Gas diffusivity (cm2/s)
C  vdg      | gaseous dry deposition velocity (m/s) 
C  lai_f    | Leaf area index
C  rscalesp | Scaling factor for highly soluble species
C
C  LOCAL VARIABLES:
C  brs      | Constant for stomatal resistance(W/m2)
C  bvpd     | Constant for water vapor pressure deficit  (kPa^-1)                   
C  fsun     | fraction of sunlit leaves (0.0-LAI)  
C  pardir   | visible beam radiation (W/m2)         
C  pardif   | diffuse visible radiation (W/m2)     
C  psi1     | Constant for leaf water potential(Mpa)
C  psi2     | Constant for leaf water potential(Mpa)
C  ra       | Aerodynamic resistance (s/m)         
C  racz     | IN-canopy aerodynamic resistance (s/m)         
C  rb       | quasi-laminar resistance (s/m)      
C  rc       | total surface resistance (s/m)     
C  rcut     | cuticle resistance (s/m )              
C  rcutdo   | Dry cuticle resistance for O3 (s/m)  
C  rcutds   | Dry cuticle resistance for SO2 (s/m)
C  rcutwo   | Wet cuticle resistance for O3 (s/m)  
C  rg       | Ground  resistance (s/m )                
C  rgo      | Ground  resistance for O3 (s/m)  
C  rgs      | Ground  resistance for SO2 (s/m) 
C  rsmin    | minimum stomatal resistance (s/m)    
C  rst      | Stomatal resistance (s/m)
C  tmin     | Minimum temperature for stomatal opening (C)
C  tmax     | Maxmum temperature for stomatal opening (C)
C  topt     | Optimum temperature for stomatal opening (C) 
C  wst      | fraction of stomatal closure under wet conditions (0.0-0.5)
C  vdg      | gas dry deposition velocity (m/s)
C
C-----------------------------------------------------------------------
C
      implicit none
      include 'camx.prm'
      include 'deposit.inc'
c
c-----Arguments 
c
      integer mlu
      real*8 z2
      real*8 zl
      real*8 z0_f
      real*8 ustar
      real*8 fsnow
      real*8 t2
      real*8 ts
      real*8 srad
      real*8 rh
      real*8 fcld 
      real*8 prec
      real*8 coszen
      real*8 rm
      real*8 alpha
      real*8 beta
      real*8 di
      real*8 rscalsp
      real*8 lai_f
      real*8 vdg
c
c-----Local variables for gaseous Vd submoudle 
c
      integer i
      real*8 es,temp,dair,dh2o,ra,rst,rdu,rdv,ww,rdm,
     &     rdn,rv,rn,ratio,sv,fv,pardir,pardif
      real*8 pshad,psun,rshad,rsun,gshad,gsun,fsun,fshad,gspar,t,bt,gt,d0,
     &     gd,psi,gw,coedew,dq,usmin,wst,racz,rgo_f,rgs_f
      real*8 rcuto_f,rcuts_f,rsnows,vi,rb,dvh2o,rs,rcut,rg,rc
      real*8 tmax(NLUZ03),tmin(NLUZ03),topt(NLUZ03),rsmin(NLUZ03),
     &     brs(NLUZ03),bvpd(NLUZ03),psi1(NLUZ03),psi2(NLUZ03),
     &     rac1(NLUZ03),rac2(NLUZ03),rgo(NLUZ03),rgs(NLUZ03),
     &     rcutdo(NLUZ03),rcutwo(NLUZ03),rcutds(NLUZ03)
      logical is_rain,is_dew
C
C   In-canopy aerodynamic resistance  [s/m].
C   Rac1 and Rac2 are minimum and maximum Rac0 for each LUC. 
C
      DATA   Rac1            /
     &  0   ,  0   ,  0   ,  100 ,  250 , 
     &  60  ,  100 ,  300 ,  100 ,  60  ,
     &  20  ,  40  ,  20  ,  10  ,  10  ,
     &  10  ,  10  ,  10  ,  10  ,  20  ,
     &  40  ,  0   ,  20  ,  0   ,  100 ,
     &  100    /
      DATA   Rac2            /
     &  0   ,  0   ,  0   ,  100 ,  250 , 
     &  100 ,  250 ,  300 ,  100 ,  60  ,
     &  60  ,  40  ,  20  ,  40  ,  40  ,
     &  40  ,  40  ,  50  ,  40  ,  20  ,
     &  40  ,  0   ,  20  ,  0   ,  100 ,
     &  100    /
C
C   Dry and wet cuticle resistance for O3  [s/m].
C
      DATA   RcutdO          /
     & -999 , -999 , -999 , 4000 , 6000 , 
     & 4000 , 6000 , 6000 , 8000 , 6000 ,
     & 5000 , 5000 , 4000 , 4000 , 4000 ,
     & 4000 , 4000 , 5000 , 5000 , 4000 ,
     & 6000 , 8000 , 5000 , -999 , 4000 ,
     & 4000    /
      DATA   RcutwO          /
     & -999 , -999 , -999 ,  200 ,  400 , 
     &  200 ,  400 ,  400 ,  400 ,  400 ,
     &  300 ,  300 ,  200 ,  200 ,  200 ,
     &  200 ,  200 ,  300 ,  300 ,  200 ,
     &  400 ,  400 ,  300 , -999 ,  200 ,
     &  200    /
C
C   Ground resistance for O3  [s/m].
C
      DATA   RgO            /
     & 2000 , 2000 , 2000 ,  200 ,  200 , 
     &  200 ,  200 ,  200 ,  200 ,  200 ,
     &  200 ,  200 ,  200 ,  200 ,  200 ,
     &  200 ,  200 ,  200 ,  200 ,  500 ,
     &  500 ,  500 ,  500 ,  500 ,  200 ,
     &  200    /
C
C   Dry cuticle resistance for SO2  [s/m].
C
      DATA   RcutdS          /
     & -999 , -999 , -999 , 2000 , 2500 , 
     & 2000 , 2500 , 2500 , 6000 , 2000 ,
     & 2000 , 2000 , 1000 , 1000 , 1500 ,
     & 1500 , 2000 , 2000 , 2000 , 2000 ,
     & 4000 , 2000 , 1500 , -999 , 2500 ,
     & 2500    /
C
C   Ground resistance for SO2  [s/m].
C
      DATA   RgS            /
     &   20 ,   70 ,  20  ,  200 ,  100 , 
     &  200 ,  200 ,  100 ,  300 ,  200 ,
     &  200 ,  200 ,  200 ,  200 ,  200 ,
     &   50 ,  200 ,  200 ,  200 ,   50 ,
     &  300 ,  300 ,   50 ,  700 ,  200 ,
     &  200    /
C
C   Stomatal resistance related parameters. 
C   In sequence: rsmin, brs, tmin, tmax, topt, bvpd, psi1, psi2
C
      DATA   rsmin            /
     & -999 , -999 , -999 ,  250 ,  150 , 
     &  250 ,  150 ,  150 ,  250 ,  150 , 
     &  150 ,  250 ,  150 ,  100 ,  120 , 
     &  120 ,  120 ,  250 ,  125 ,  150 ,
     &  200 ,  150 ,  150 , -999 ,  150 , 
     &  150   /
      DATA   brs              /
     & -999 , -999 , -999 ,   44 ,   40 , 
     &   44 ,   43 ,   40 ,   44 ,   40 , 
     &   44 ,   44 ,   50 ,   20 ,   40 , 
     &   40 ,   50 ,   65 ,   65 ,   40 , 
     &   42 ,   25 ,   40 , -999 ,   44 , 
     &   43   /
      DATA   tmin             /
     & -999 , -999 , -999 ,   -5 ,    0 , 
     &   -5 ,    0 ,    0 ,    0 ,    0 , 
     &   -5 ,    0 ,    5 ,    5 ,    5 , 
     &    5 ,    5 ,    5 ,   10 ,    5 , 
     &    0 ,   -5 ,    0 , -999 ,   -3 , 
     &    0   /
      DATA   tmax             /
     & -999 , -999 , -999 ,   40 ,   45 , 
     &   40 ,   45 ,   45 ,   45 ,   45 , 
     &   40 ,   45 ,   40 ,   45 ,   45 , 
     &   45 ,   45 ,   45 ,   45 ,   45 , 
     &   45 ,   40 ,   45 , -999 ,   42 , 
     &   45   /
      DATA   topt             /
     & -999 , -999 , -999 ,   15 ,   30 , 
     &   15 ,   27 ,   30 ,   25 ,   30 , 
     &   15 ,   25 ,   30 ,   25 ,   27 , 
     &   27 ,   25 ,   25 ,   30 ,   25 , 
     &   22 ,   20 ,   20 , -999 ,   21 , 
     &   25   /
      DATA   bvpd             /
     & -999 , -999 , -999 ,  0.31,  0.27, 
     &  0.31,  0.36,  0.27,  0.31,  0.27,  
     &  0.27,  0.27,  0.0 ,  0.0 ,  0.0 ,  
     &  0.0 ,  0.0 ,  0.0 ,  0.0 ,  0.0 ,  
     &  0.31,  0.24,  0.27, -999 ,  0.34, 
     &  0.31   /
      DATA   psi1            /
     & -999 , -999 , -999 , -2.0 , -1.0 , 
     & -2.0 , -1.9 , -1.0 , -1.0 , -2.0 ,
     & -2.0 , -2.0 , -1.5 , -1.5 , -1.5 ,
     & -1.5 , -1.5 , -1.5 , -1.5 , -1.5 ,
     & -1.5 ,    0 , -1.5 , -999 , -2.0 ,
     & -2.0    /
      DATA   psi2            /
     & -999 , -999 , -999 , -2.5 , -5.0 , 
     & -2.5 , -2.5 , -5.0 , -4.0 , -4.0 ,
     & -4.0 , -3.5 , -2.5 , -2.5 , -2.5 ,
     & -2.5 , -2.5 , -2.5 , -2.5 , -2.5 ,
     & -3.0 , -1.5 , -2.5 , -999 , -2.5 ,
     & -3.0    /
C
C --- Define the function for saturation vapor pressure (mb) 
C
      ES(TEMP) = 6.108*EXP(17.27*(TEMP - 273.16)/(TEMP - 35.86))
C
C   Some constants 
C
      dair=0.369*29.+6.29  
      dh2o=0.369*18.+6.29

      VDG = 0.0
      I = MLU
C
C     Aerodynamic resistance above canopy
C
      IF(ZL.GE.0.) THEN
          Ra=(.74*DLOG(Z2/Z0_F)+4.7*ZL)/0.4/USTAR 
      ELSE
          Ra=0.74/0.4/USTAR*(DLOG(Z2/Z0_F)-
     &          2*DLOG((1+SQRT(1-9.*ZL))*0.5))
      ENDIF
      Ra=dmax1(Ra,dble(5.0))
      if (I.EQ.1.OR.I.EQ.3) THEN
        Ra=dmin1(Ra,dble(2000.))
      else
        Ra=dmin1(Ra,dble(1000.))
      end if 
C
C --- STOMATAL RESISTANCE FOR WATER VAPOR ONLY. STEPS FOR CALCULATING:
C     1. Calculate direct and diffuse PAR from solar radiation
C     2. Calculate sunlit and shaded leaf area, PAR for sunlit and shaded leafs 
C     3. Calculate stomatal conductance 
C     4. Calculate stomatal resistance for water vapor
C
C --- Set a big value for stomatal resistance when stomata are closed
      RST=99999.9        
C
C --  Only calculate stomatal resistance if there is solar radiation, 
C     leaf area index is not zero, and within reasonable temperature range
C
      IF ( SRAD.GE.0.1             .AND.
     &     TS.LT.(Tmax(I)+273.15)  .AND. 
     &     TS.GT.(Tmin(I)+273.15)  .AND.  
     &     LAI_F.GT.0.001           .AND.
     &     COSZEN.GT.0.001               ) THEN

C --  Calculate direct and diffuse PAR from solar radiation and solar zenith angle
 
      RDU=600.*EXP(-0.185/COSZEN)*COSZEN
      RDV=0.4*(600.-RDU)*COSZEN
      WW=-DLOG(COSZEN)/2.302585  
      WW=-1.195+0.4459*WW-0.0345*WW**2
      WW=1320*10**WW
      RDM=(720.*EXP(-0.06/COSZEN)-WW)*COSZEN
      RDN=0.6*(720-RDM-WW)*COSZEN
      RV=dmax1(dble(0.1),RDU+RDV) 
      RN=dmax1(dble(0.01),RDM+RDN) 
      RATIO=dmin1(dble(0.9),SRAD/(RV+RN))
      SV=RATIO*RV                            ! Total PAR 
      FV=dmin1(dble(0.99), (0.9-RATIO)/0.7)  
      FV=dmax1(dble(0.01),RDU/RV*(1.0-FV**0.6667))  !fraction of PAR in the direct beam 
      PARDIR=FV*SV                           ! PAR from direct radiation 
      PARDIF=SV-PARDIR                       ! PAR from diffuse radiation 
c  
C -- Calculate sunlit and shaded leaf area, PAR for sunlit and shaded leaves 
C
      IF (LAI_F.GT.2.5.AND.SRAD.GT.200.) THEN
      PSHAD=PARDIF*EXP(-0.5*LAI_F**0.8)
     & +0.07*PARDIR*(1.1-0.1*LAI_F)*EXP(-COSZEN)
      PSUN=PARDIR**0.8*.5/COSZEN+PSHAD
      ELSE
      PSHAD=PARDIF*EXP(-0.5*LAI_F**0.7)
     & +0.07*PARDIR*(1.1-0.1*LAI_F)*EXP(-COSZEN)
      PSUN=PARDIR*.5/COSZEN+PSHAD
      END IF
      RSHAD=RSmin(I)+BRS(I)*RSMIN(I)/PSHAD 
      RSUN=RSmin(I)+BRS(I)*RSMIN(I)/PSUN
      GSHAD=1./RSHAD
      GSUN=1./RSUN
      FSUN=2*COSZEN*(1.-EXP(-0.5*LAI_F/COSZEN))  ! Sunlit leaf area 
      FSHAD=LAI_F-FSUN                    ! Shaded leaf area 

C -- Stomatal conductance before including effects of temperature, 
C                   vapor pressure defict and water stress.

      GSPAR=FSUN*GSUN+FSHAD*GSHAD     

C --  function for temperature effect
      T=TS-273.15      
      BT=(Tmax(I)-TOPT(I))/(TMAX(I)-Tmin(I))
      GT=(Tmax(I)-T)/(TMAX(I)-TOPT(I))
      GT=GT**BT
      GT=GT*(T-Tmin(I))/(TOPT(I)-TMIN(I))  
C --  function for vapor pressure deficit 
      D0= ES(TS)*(1.- RH)/10.           !kPa 
      GD=1.-BVPD(I)*D0
C --  function for water stress 
      PSI=(-0.72-0.0013*SRAD)
c      PSI_S=(-0.395-0.043*(TS-273.15))*102.
      GW=(PSI-PSI2(I))/(PSI1(I)-PSI2(I))
      IF (GW.GT.1.0) GW=1.0
      IF (GW.LT.0.1) GW=0.1
      IF (GD.GT.1.0) GD=1.0
      IF (GD.LT.0.1) GD=0.1
C --  Stomatal resistance for water vapor 
      RST=1.0/(GSPAR*GT*GD*GW)    

      END IF
C
c   Decide if dew or rain occurs.
C
      IF (FCLD.LT.0.25) THEN
        Coedew=0.3  
      ELSE  IF (FCLD.GE.0.25.AND.FCLD.LT.0.75) THEN 
         Coedew=0.2  
      ELSE
        Coedew=0.1  
      END IF
         DQ=0.622/1000. * ES(TS)*(1.- RH)*1000.    ! unit g/kg 
	 DQ=dmax1(dble(0.0001),DQ)
         USMIN=1.5/DQ*Coedew  
 
      IF (TS.GT.273.15 .AND. PREC.GT.0.20) then
        is_rain = .true. 
      ELSE IF (TS.GT.273.15 .AND. USTAR.LT.USMIN)THEN 
        is_dew = .true. 
      ELSE
        is_rain = .false. 
        is_dew = .false. 
      END IF
C
C   Decide fraction of stomatal blocking due to wet conditions 
C 
      Wst=0.
      if ((is_dew.or.is_rain).and.SRAD.GT.200.) then
      Wst=(SRAD-200.)/800.
      Wst=dmin1(Wst, dble(0.5)) 
      end if 
C
C -- In-canopy aerodynamic resistance 
C
        Racz = Rac1(I)+(LAI_F-lai_ref(I,14))/(lai_ref(I,15)
     &        -lai_ref(I,14)+1.E-10)*(Rac2(I)-Rac1(I)) 
        Racz = Racz*LAI_F**0.25/USTAR/USTAR 
C
C -- Ground resistance for O3 
C
      IF (I.GE.4.AND.TS.LT.272.15) THEN 
        RgO_F = dmin1( RgO(I)*2., RgO(I) * exp(0.2*(272.15-TS))) 
      ELSE
        RgO_F = RgO(I)
      END IF
C
C -- Ground resistance for SO2 
C
      IF (I.EQ.2) THEN 
	RgS_F = DMIN1(RgS(I)*(275.15-TS), dble(500.)) 
	RgS_F = DMAX1(RgS(I), dble(100.)) 
      ELSE IF (I.GE.4.AND.is_rain) THEN
        RgS_F = 50. 
      ELSE IF (I.GE.4.AND.is_dew) THEN
        RgS_F = 100. 
      ELSE IF (I.GE.4.AND.TS.LT.272.15) THEN
        RgS_F = dmin1( RgS(I)*2., RgS(I) * exp(0.2*(272.15-TS))) 
      ELSE 
        RgS_F =  RgS(I)
      END IF
C
C -- Cuticle resistance for O3 AND SO2  
C
      IF (RcutdO(I).LE.-1) THEN 
	RcutO_F = 1.E25 
	RcutS_F = 1.E25 
      ELSE IF (is_rain) THEN
	RcutO_F = RcutwO(I)/LAI_F**0.5/USTAR 
	RcutS_F = 50./LAI_F**0.5/USTAR 
	RcutS_F = DMAX1(RcutS_F, dble(20.)) 
      ELSE IF (is_dew) THEN
	RcutO_F = RcutwO(I)/LAI_F**0.5/USTAR 
	RcutS_F = 100./LAI_F**0.5/USTAR 
	RcutS_F = DMAX1(RcutS_F, dble(20.)) 
      ELSE IF (TS.LT.272.15) THEN
	RcutO_F = RcutdO(I)/exp(3.*RH)/LAI_F**0.25/USTAR 
	RcutS_F = RcutdS(I)/exp(3.*RH)/LAI_F**0.25/USTAR 
        RcutO_F = dmin1( RcutO_F*2., RcutO_F * exp(0.2*(272.15-TS))) 
        RcutS_F = dmin1( RcutS_F*2., RcutS_F * exp(0.2*(272.15-TS))) 
	RcutO_F = DMAX1(RcutO_F,dble(100.)) 
	RcutS_F = DMAX1(RcutS_F,dble(100.)) 
      ELSE 
	RcutO_F = RcutdO(I)/exp(3.*RH)/LAI_F**0.25/USTAR 
	RcutS_F = RcutdS(I)/exp(3.*RH)/LAI_F**0.25/USTAR 
	RcutO_F = DMAX1(RcutO_F,dble(100.)) 
	RcutS_F = DMAX1(RcutS_F,dble(100.)) 
      END IF
C
C If snow occurs, Rg and Rcut are adjusted by snow cover fraction
C
      fsnow= dmin1(dble(1.0), fsnow)   !snow cover fraction for leaves 
      If (fsnow.GT.0.0001.and.I.GE.4) THEN
        RsnowS= DMIN1(70.*(275.15-TS), dble(500.)) 
        RsnowS= DMAX1(RSnowS, dble(100.)) 
        RcutS_F=1.0/((1.-fsnow)/RcutS_F+fsnow/RsnowS) 
        RcutO_F=1.0/((1.-fsnow)/RcutO_F+fsnow/2000.) 
        fsnow= dmin1(dble(1.0), fsnow*2.)   !snow cover fraction for ground 
        RgS_F=1.0/((1.-fsnow)/RgS_F+fsnow/RsnowS) 
        RgO_F=1.0/((1.-fsnow)/RgO_F+fsnow/2000.) 
      END IF

C
C -- Calculate diffusivity for each gas species
C
      VI=145.8*1.E-4*(TS*0.5+T2*0.5)**1.5/
     &                  (TS*0.5+T2*0.5+110.4)
C
C -- Calculate quasi-laminar resistance 
C
      Rb =5./USTAR*(VI/DI)**.666667   
C
C -- Calculate stomatal resistance for each species from the ratio of
C       diffusity of water vapor to the gas species
C
      DVh2o=0.001*TS**1.75*SQRT((29.+18.)/29./18.)
      DVh2o=DVh2o/(dair**0.3333+dh2o**0.3333)**2
      RS=RST*DVh2o/DI+RM 
C
C -- Scale cuticle and ground resistances for each species 
C
      Rcut = 1./(ALPHA/RcutS_F+BETA/RcutO_F)
      Rg = 1./(ALPHA/RgS_F+BETA/RgO_F)
C
C -- Calculate total surface resistance  
C
      Rc = (1.-Wst)/Rs+1./(Racz+Rg)+1./Rcut 
      Rc=dmax1(dble(10.0),1./Rc)        !Set minimum surface resistance as 10 s/m 
      Rc=Rc*rscalsp               !Scale for the extremely soluble gas
C
C -- Deposition velocity 
C
      VDG = 1./(RA+RB+RC)    

      RETURN
      END

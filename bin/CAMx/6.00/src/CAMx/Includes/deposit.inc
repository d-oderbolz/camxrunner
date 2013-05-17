c----CAMx v6.00 130506
c  
c     DEPOSIT.COM contains arrays for dry deposition 
c                            
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c            
c     Modifications:  
c        7/21/03     removed istress as it can now be optionally specified
c                    via input file
c 
c-----------------------------------------------------------------------
c     Arrays defining the dry deposition model:
c
c     nlu     -- number of landuse categories for choses dry dep model
c     z0lu    -- surface roughness length by landuse and season (meter)
c     lai_ref -- reference leaf area index (LAI) by landuse/season
c     iseason -- season index map by latitude band and month
c     rj      -- baseline minimum stomatal resistance (s/m) 
c     rlu     -- baseline upper canopy (cuticle) resistance (s/m) 
c     rac     -- baseline canopy height/density resistance (s/m) 
c     rlcs    -- baseline SO2 lower canopy resistance (s/m) 
c     rlco    -- baseline O3 lower canopy resistance (s/m) 
c     rgss    -- baseline SO2 ground surface resistance (s/m) 
c     rgso    -- baseline O3 ground surface resistance (s/m)
c     dstress -- drought stress adjustment factors to stomatal resistance
c-----------------------------------------------------------------------
c
      integer nlu
      integer iseason(5,12)
      real    z0lu(NLUW89,5)
      real    rj(NLUW89,5)
      real    rlu(NLUW89,5)
      real    rac(NLUW89,5)
      real    rlcs(NLUW89,5)
      real    rlco(NLUW89,5)
      real    rgss(NLUW89,5)
      real    rgso(NLUW89,5)
      real    dstress(0:5)
      real    lai_ref(NLUZ03,15)
      real    z01(NLUZ03)
      real    z02(NLUZ03)
c
      common /deposit/ nlu, z0lu, rj, rlu, rac, rlcs, rlco, rgss, rgso, 
     &                 iseason, dstress, lai_ref, z01, z02

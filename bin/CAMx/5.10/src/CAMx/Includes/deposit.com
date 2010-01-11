c----CAMx v5.10 090918
c  
c     DEPOSIT.COM contains arrays for dry deposition 
c                            
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c            
c     Modifications:  
c        7/21/03     removed istress as it can now be optionally specified
c                    via input file
c 
c-----------------------------------------------------------------------
c     Arrays defining the Wesely (1989) resistance model:
c
c     z0lu    -- surface roughness length by landuse and season (meter)
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
      integer iseason(5,12)
      real    z0lu(NLU,5)
      real    rj(NLU,5)
      real    rlu(NLU,5)
      real    rac(NLU,5)
      real    rlcs(NLU,5)
      real    rlco(NLU,5)
      real    rgss(NLU,5)
      real    rgso(NLU,5)
      real    dstress(0:5)
c
      common /deposit/ z0lu, rj, rlu, rac, rlcs, rlco, rgss, rgso, 
     &                 iseason, dstress

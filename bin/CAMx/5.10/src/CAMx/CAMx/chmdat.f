      block data
      use camxcom
c
c----CAMx v5.10 090918
c  
c     BLOCK DATA contains the following:
c        (1) specifies heights and zenith angles for the lookup table
c            of photolysis rate constants
c        (2) sets gas-phase deposition parameters
c 
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c 
c     Modifications:  
c        4/4/00    Added default aerosol density and size cut points
c        1/9/02    Aerosol cut points and density now read from chemistry 
c                  parameters file in READCHEM
c        8/30/02   Added minimum CWC to define clouds
c        7/21/03   Removed drought stress index, as it can now be optionally
c                  specified via input file; added season map by latitude and
c                  month
c        4/21/04   Added RH parameters for aerosol chemistry
c        11/19/04  Incorporated season-dependent roughness length
c        11/30/04  Removed height dimension for photolysis rates
c        10/07/05  Reduced tamin to 243 K (no liquid cloud/precip below tamin)
c
      include 'camx.prm'
      include 'deposit.com'
      include 'chmdat.com'
      include 'camx_aero.com'
c
c-----Lower bound for radical concentrations
c
      data bdlrad /1.0e-25/
c
c-----CWMIN is the minimum threshold of cloud water content (g/m3) that
c     defines the presence of a cloud
c     TAMIN is the freezing threshold of cloud water for use in aqueous
c     chemistry and wet deposition
c
      data cwmin /0.05/
      data tamin /243./
c
c-----Vertical profiles of HCl and Cl2 (ppm) used by the Hg chemistry
c
      data htcl    /0.06, 0.15, 0.45, 0.85, 2.00, 6.00/
      data hclprof 
     &         /1.0E-03, 1.0E-03, 1.0E-03, 1.0E-03, 1.0E-03, 1.0E-03/
      data cl2day  
     &         /1.0E-06, 1.0E-06, 1.0E-06, 1.0E-06, 1.0E-06, 1.0E-06/
      data cl2nite 
     &         /1.5E-04, 1.0E-04, 7.5E-05, 5.0E-05, 5.0E-05, 5.0E-05/
c
c-----The following are deposition variables
c
c-----Default SO2 Henry's Law coefficients and temperature factor
c
      data henso20  /1.0e+5/
      data tfactso2 /-3156./
      data nbin /1/
c
c-----Surface roughness (m) as a function of 11 landuse categories
c     and 5 seasons; based on AERMET model (ref EPA SCRAM website)
c
      data z0lu
     & /1.0,0.20,0.100,1.3,1.3,1.30,0.0001,0.002,0.20,0.150,0.30,
     &  1.0,0.05,0.010,0.8,1.3,1.05,0.0001,0.002,0.20,0.030,0.30,
     &  1.0,0.05,0.010,0.8,1.3,1.05,0.0001,0.002,0.20,0.030,0.30,
     &  1.0,0.01,0.001,0.5,1.3,0.90,0.0001,0.002,0.05,0.006,0.15,
     &  1.0,0.03,0.050,1.0,1.3,1.15,0.0001,0.002,0.20,0.040,0.30/
c
c-----Season indices by month and latitude band
c     Season Indices            Latitude Bands
c     1 = summer                1 = <20    Tropical
c     2 = autumn                2 = 20-35  Sub-tropical
c     3 = winter w/o snow       3 = 35-50  Temperate
c     4 = winter w/ snow        4 = 50-75  Cool
c     5 = spring                5 = >75    Polar
c                    Latitude Band
      data iseason / 1, 3, 3, 3, 3, ! Jan
     &               1, 5, 3, 3, 3, ! Feb
     &               1, 5, 5, 3, 3, ! Mar
     &               1, 5, 5, 5, 3, ! Apr
     &               1, 1, 5, 5, 3, ! May
     &               1, 1, 1, 1, 5, ! Jun
     &               1, 1, 1, 1, 1, ! Jul
     &               1, 1, 1, 1, 2, ! Aug
     &               1, 1, 2, 2, 3, ! Sep
     &               1, 2, 2, 2, 3, ! Oct 
     &               1, 2, 2, 3, 3, ! Nov
     &               1, 2, 3, 3, 3/ ! Dec
c
c-----Drought stress adjustment factors for stomatal resistance
c     From the GloBEIS3 adjustment based on the Palmer Drought
c     Index (PDI).  PDI ranges from +2 (wet) to -6 (severe drought).
c     The index input to CAMx is minus the PDI from 0 to 5. Values
c     below are the adjustment factors for index 0 to 5. 
c
      data dstress / 1.0, 1.1, 1.5, 2.0, 3.5, 10.0 /
c
c-----Baseline resistances are from Wesely (1989)
c
      data rj  /9999.,  60., 120.,  70., 130., 100.,9999.,9999.,  
     &            80., 100., 150.,9999.,9999.,9999.,9999., 250., 
     &           500.,9999.,9999.,9999.,9999.,9999.,9999.,9999.,
     &          9999.,9999., 250., 500.,9999.,9999.,9999.,9999.,
     &          9999.,9999.,9999.,9999.,9999., 400., 800.,9999.,
     &          9999.,9999.,9999.,9999.,9999., 120., 240., 140., 
     &           250., 190.,9999.,9999., 160., 200., 300./ 
c
      data rlu /9999.,2000.,2000.,2000.,2000.,2000.,9999.,9999.,
     &          2500.,2000.,4000.,9999.,9000.,9000.,9000.,4000.,
     &          8000.,9999.,9999.,9000.,9000.,9000.,9999.,9999.,
     &          9000.,9000.,4000.,8000.,9999.,9999.,9000.,9000.,
     &          9000.,9999.,9999.,9999.,9999.,6000.,9000.,9999.,
     &          9999.,9000.,9000.,9000.,9999.,4000.,4000.,4000.,
     &          2000.,3000.,9999.,9999.,4000.,4000.,8000./ 
c
      data rac / 100., 200., 100.,2000.,2000.,2000.,0.001,0.001,
     &           300., 150., 200., 100., 150., 100.,1500.,2000.,
     &          1700.,0.001,0.001, 200., 120., 140., 100.,  10.,
     &           100.,1000.,2000.,1500.,0.001,0.001, 100.,  50.,
     &           120., 100.,  10.,  10.,1000.,2000.,1500.,0.001,
     &          0.001,  50.,  10.,  50., 100.,  50.,  80.,1200.,
     &          2000.,1500.,0.001,0.001, 200.,  60., 120./ 
c
      data rgss/ 400., 150., 350., 500., 500., 100.,0.001,1000.,
     &          0.001, 220., 400., 400., 200., 350., 500., 500.,
     &           100.,0.001,1000.,0.001, 300., 400., 400., 150.,
     &           350., 500., 500., 200.,0.001,1000.,0.001, 200.,
     &           400., 100., 100., 100., 100., 100., 100.,0.001,
     &          1000., 100., 100.,  50., 500., 150., 350., 500.,
     &           500., 200.,0.001,1000.,0.001, 250., 400./ 
c
      data rgso/ 300., 150., 200., 200., 200., 300.,2000., 400.,
     &          1000., 180., 200., 300., 150., 200., 200., 200.,
     &           300.,2000., 400., 800., 180., 200., 300., 150.,
     &           200., 200., 200., 300.,2000., 400.,1000., 180.,
     &           200., 600.,3500.,3500.,3500.,3500.,3500.,2000.,
     &           400.,3500.,3500.,3500., 300., 150., 200., 200.,
     &           200., 300.,2000., 400.,1000., 180., 200./ 
c
      data rlcs/9999.,2000.,2000.,2000.,2000.,2000.,9999.,9999.,
     &          2500.,2000.,4000.,9999.,9000.,9000.,9000.,2000.,
     &          4000.,9999.,9999.,9000.,9000.,9000.,9999.,9999.,
     &          9000.,9000.,3000.,6000.,9999.,9999.,9000.,9000.,
     &          9000.,9999.,9999.,9999.,9000., 200., 400.,9999.,
     &          9999.,9000.,9999.,9000.,9999.,4000.,4000.,4000.,
     &          2000.,3000.,9999.,9999.,4000.,4000.,8000./ 
c
      data rlco/9999.,1000.,1000.,1000.,1000.,1000.,9999.,9999.,
     &          1000.,1000.,1000.,9999., 400., 400., 400.,1000.,
     &           600.,9999.,9999., 400., 400., 400.,9999.,1000.,
     &           400., 400.,1000., 600.,9999.,9999., 800., 600.,
     &           600.,9999.,1000.,1000., 400.,1500., 600.,9999.,
     &          9999., 800.,1000., 800.,9999.,1000., 500., 500.,
     &          1500., 700.,9999.,9999., 600., 800., 800./ 
c
c-----Background values for RADM
c
      data co2 /330./      ! carbon dioxide, ppm
      data foa /1.e-6/     ! formic acid, ppm
      data mhp /1.e-6/     ! MHP, ppm
      data paa /1.e-6/     ! PAA, ppm
      data nacl /0.05/     ! sea salt, ug/m3
      data caco3 /0./      ! calcium carbonate, ug/m3
      data mgco3 /0./      ! magnesium carbonate, ug/m3
      data a3fe /0.010/    ! Fe+++, ug/m3
      data b2mn /0.005/    ! Mn++, ug/m3
      data potcl /0./      ! potassium chloride, ug/m3
c
c-----Parameters for relative humidity calculation
c
      data eps/0.622/, e0/6.11/, lv/2.5e6/, rv/461./
c
      end

      block data
      use camxcom
c
c----CAMx v6.00 130506
c  
c     BLOCK DATA contains the following:
c        (1) specifies heights and zenith angles for the lookup table
c            of photolysis rate constants
c        (2) sets gas-phase deposition parameters
c 
c     Copyright 1996 - 2013
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
c        8/10/09   Added Zhang (2003) LAI and surface roughness parameters for
c                  26 landuse categories
c        10/29/09  Added optional RTRAC surface model
c        05/24/11  Updated Hg chemistry: added Br species and removed
c                  HCl profile (HCl is now a model species)
c        09/28/12  Reduced tamin to 233 K (no liquid cloud/precip below tamin)
c        09/28/12  Reduced cwmin to 0.01 g/m3
c
      include 'camx.prm'
      include 'deposit.inc'
      include 'chmdat.inc'
      include 'camx_aero.inc'
      include 'rtracsrf.inc'
c
c-----CWMIN is the minimum threshold of cloud water content (g/m3) that
c     defines the presence of a cloud
c     TAMIN is the freezing threshold of cloud water for use in aqueous
c     chemistry and wet deposition
c
      data cwmin /0.01/
      data tamin /233./
c
c-----Vertical profiles of Cl2, Br and BrO (ppm) used by the Hg chemistry
c
      data hthal    /0.06, 0.15, 0.45, 0.85, 2.00, 6.00/
      data brlprof
     &         /5.0E-09, 5.0E-09, 5.0E-09, 5.0E-09, 5.0E-09, 5.0E-09/
      data brwprof
     &         /2.9E-08, 2.9E-08, 2.9E-08, 2.9E-08, 2.9E-08, 2.9E-08/
      data brolprof
     &         /5.0E-08, 5.0E-08, 5.0E-08, 5.0E-08, 5.0E-08, 5.0E-08/
      data browprof
     &         /2.9E-07, 2.9E-07, 2.9E-07, 2.9E-07, 2.9E-07, 2.9E-07/
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
c-----Reference Leaf Area Index (LAI)
c
c     The Zhang (2003) scheme is a function of 26 landuse
c     categories and 12 months, Jan, and MIN and MAX
c
      data lai_ref
     & /0.0, 0.0, 0.0, 5.0, 6.0, 0.1, 0.1, 6.0, 4.0, 3.0,
     &  0.5, 3.0, 1.0, 0.5, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0,
     &  0.1, 1.0, 4.0, 0.0, 3.0, 3.0, !Jan
     &  0.0, 0.0, 0.0, 5.0, 6.0, 0.1, 0.1, 6.0, 4.0, 3.0,
     &  0.5, 3.0, 1.0, 0.5, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0,
     &  0.1, 1.0, 4.0, 0.0, 3.0, 3.0, !Feb
     &  0.0, 0.0, 0.0, 5.0, 6.0, 0.5, 0.5, 6.0, 4.0, 3.0,
     &  1.0, 3.0, 1.0, 0.5, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0,
     &  0.1, 0.5, 4.0, 0.0, 3.0, 3.0, !Mar
     &  0.0, 0.0, 0.0, 5.0, 6.0, 1.0, 1.0, 6.0, 4.0, 3.0,
     &  1.0, 3.0, 1.0, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 1.0,
     &  0.1, 0.1, 4.0, 0.0, 4.0, 4.0, !Apr
     &  0.0, 0.0, 0.0, 5.0, 6.0, 2.0, 2.0, 6.0, 4.0, 3.0,
     &  1.5, 3.0, 1.0, 0.5, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0,
     &  0.5, 0.1, 4.0, 0.0, 4.5, 4.5, !May
     &  0.0, 0.0, 0.0, 5.0, 6.0, 4.0, 4.0, 6.0, 4.0, 3.0,
     &  2.0, 3.0, 1.0, 0.5, 2.0, 2.5, 3.0, 2.0, 3.0, 1.0,
     &  1.0, 0.1, 4.0, 0.0, 5.0, 5.0, !Jun
     &  0.0, 0.0, 0.0, 5.0, 6.0, 5.0, 5.0, 6.0, 4.0, 3.0,
     &  3.0, 3.0, 1.0, 1.0, 3.0, 4.0, 4.0, 3.0, 4.0, 1.0,
     &  1.0, 0.1, 4.0, 0.0, 5.0, 5.0, !Jul
     &  0.0, 0.0, 0.0, 5.0, 6.0, 5.0, 5.0, 6.0, 4.0, 3.0,
     &  3.0, 3.0, 1.0, 2.0, 3.5, 5.0, 4.5, 3.5, 4.5, 1.0,
     &  1.0, 1.0, 4.0, 0.0, 5.0, 5.0, !Aug
     &  0.0, 0.0, 0.0, 5.0, 6.0, 4.0, 4.0, 6.0, 4.0, 3.0,
     &  2.0, 3.0, 1.0, 2.0, 4.0, 6.0, 5.0, 4.0, 5.0, 1.0,
     &  1.0, 2.0, 4.0, 0.0, 4.0, 4.0, !Sep
     &  0.0, 0.0, 0.0, 5.0, 6.0, 2.0, 2.0, 6.0, 4.0, 3.0,
     &  1.5, 3.0, 1.0, 1.5, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0,
     &  1.0, 1.5, 4.0, 0.0, 3.0, 3.0, !Oct
     &  0.0, 0.0, 0.0, 5.0, 6.0, 1.0, 1.0, 6.0, 4.0, 3.0,
     &  1.0, 3.0, 1.0, 1.0, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0,
     &  0.4, 1.5, 4.0, 0.0, 3.0, 3.0, !Nov
     &  0.0, 0.0, 0.0, 5.0, 6.0, 0.1, 0.1, 6.0, 4.0, 3.0,
     &  0.5, 3.0, 1.0, 1.0, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0,
     &  0.1, 1.0, 4.0, 0.0, 3.0, 3.0, !Dec
     &  0.0, 0.0, 0.0, 5.0, 6.0, 0.1, 0.1, 6.0, 4.0, 3.0,
     &  0.5, 3.0, 1.0, 0.5, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0,
     &  0.1, 1.0, 4.0, 0.0, 3.0, 3.0, !Jan
     &  0.0, 0.0, 0.0, 5.0, 6.0, 0.1, 0.1, 6.0, 4.0, 3.0,
     &  0.5, 3.0, 1.0, 0.5, 0.1, 0.1, 0.1, 0.1, 0.1, 1.0,
     &  0.1, 0.1, 4.0, 0.0, 3.0, 3.0, !MIN
     &  0.0, 0.0, 0.0, 5.0, 6.0, 5.0, 5.0, 6.0, 4.0, 3.0,
     &  3.0, 3.0, 1.0, 2.0, 4.0, 6.0, 5.0, 4.0, 5.0, 1.0,
     &  1.0, 2.0, 4.0, 0.0, 5.0, 5.0/ !MAX
c
c-----Surface roughness length (m) for 26 LU categories
c     for the Zhang (2003) dry dep scheme.
c     z01 and z02 are minimum and maximum z0 for each luc.
c
      data   z01             /
     &  0.0 ,  0.01,  0.0 ,  0.9 ,  2.0 ,
     &  0.4 ,  0.4 ,  2.5 ,  0.6 ,  0.2 ,
     &  0.05,  0.2 ,  0.04,  0.02,  0.02,
     &  0.02,  0.02,  0.02,  0.02,  0.05,
     &  1.0 ,  0.03,  0.1 ,  0.04,  0.6 ,
     &  0.6    /
      data   z02             /
     &  0.0 ,  0.01,  0.0 ,  0.9 ,  2.0 ,
     &  0.9 ,  1.0 ,  2.5 ,  0.6 ,  0.2 ,
     &  0.2 ,  0.2 ,  0.04,  0.1 ,  0.1 ,
     &  0.1 ,  0.1 ,  0.1 ,  0.2 ,  0.05,
     &  1.0 ,  0.03,  0.1 ,  0.04,  0.9 ,
     &  0.9    /
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
c============================= Begin Probing Tool=============================
c
c-----Landuse-dependent fractions for RTRAC soil model
c     fsoil   = fraction of soil area
c     fsoiloc = fraction of soil organic material
c     fshad   = fraction of shading (for photolysis)
c
      data fsoil   /0.7,0.4,0.7,0.3,0.2,0.3,1.0,0.9,0.5,0.5,0.7/
      data fsoiloc /0.3,0.9,0.5,0.9,0.9,0.9,0.0,0.1,0.9,0.7,0.4/
      data fshad   /0.2,0.3,0.2,0.4,0.5,0.4,0.0,0.0,0.3,0.3,0.2/
c
c============================= End Probing Tool===============================
c
      end

c-----CAMx v4.42 070603
c  
c     CHMSTRY.COM contains all chemistry variables 
c                            
c     Copyright 1996-2007
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
c 
c-----------------------------------------------------------------------
c     Parameters for some of the switches:
c
c     CDCMC  -- string for requesting the CMC (standard) chemistry solver
c     CDIEH  -- string for requesting the IEH chemistry solver
c     CDLSOD -- string for requesting the LSODE chemistry solver
c     IDCMC  -- code for using the CMC (standard) chemistry solver
c     IDIEH  -- code for using the IEH chemistry solver
c     IDLSOD -- code for using the LSODE chemistry solver
c-----------------------------------------------------------------------
c
      character*10 CDCMC
      character*10 CDIEH
      character*10 CDLSOD
      integer      IDCMC
      integer      IDIEH
      integer      IDLSOD
c
      parameter( CDCMC  = "CMC       " )
      parameter( CDIEH  = "IEH       " )
      parameter( CDLSOD = "LSODE     " )
      parameter( IDCMC  = 1 )
      parameter( IDIEH  = 2 )
      parameter( IDLSOD = 3 )
c 
c-----------------------------------------------------------------------
c    Variables for the number of species in input files:
c
c    ngas   --  number of gas species being modeled
c    naero  --  number of aersol species being modeled
c    nspec  --  total number of modeled species
c    nrad   --  number of radical species being modeled
c    nreact --  number of chemical reactions
c    nspfst --  number of "fast" species -- handled by the fast solver
c    iessrad--  number of radicals in steady state for IEH solver
c    idmech --  the code which determines which chemical mechanism is used
c    idsolv --  the code which determines which chemstry solver to use
c    navspc --  number of species to write to output average file
c    nicspc --  number of species in the initial conditions file
c    nbcspc --  number of species in the boundary conditions file
c    nptspc --  number of species in the point source emissions file
c    narspc --  number of species in the surface emissions file
c    lgas   --  TRUE if species is a gas species
c-----------------------------------------------------------------------
c
       integer   ngas
       integer   naero
       integer   nspec
       integer   nrad
       integer   nreact
       integer   nspfst
       integer   iessrad
       integer   idmech
       integer   idsolv
       integer   navspc
       integer   nicspc
       integer   nbcspc
       integer   nptspc
       integer   narspc(MXGRID)
       logical   lgas(MXSPEC)
c
      common /chm1/ ngas, naero, nspec, nrad, nreact, nspfst, iessrad,
     &              idmech, idsolv, navspc, nicspc, nbcspc, nptspc, 
     &              narspc, lgas
c
c-----------------------------------------------------------------------
c    aeropt --  keyword for aerosol scheme ('CF' or 'CMU')
c-----------------------------------------------------------------------
c
      character*10 aeropt
      common /chm2/ aeropt
c
c-----------------------------------------------------------------------
c     Variables for keeping track of where chmistry is being performed:
c     NOTE:  Used for diagnostic and error messages.
c
c     igrdchm  --  grid number of current chemistry step
c     ichm     --  column for the current chemistry step
c     jchm     --  row for the current chemistry step
c     kchm     --  layer for the current chemistry step
c     tchm     --  temperature in current chemistry step
c     wchm     --  water in current chemstry step
c     ldchm    --  ldark flag in current chemistry step
c-----------------------------------------------------------------------
c
      integer   igrdchm
      integer   ichm
      integer   jchm
      integer   kchm
      real      tchm
      real      wchm
      logical   ldchm
c
      common /ijkgrd/ igrdchm, ichm, jchm, kchm, tchm, wchm, ldchm
c$omp threadprivate(/ijkgrd/)
c
c-----------------------------------------------------------------------
c     Variables for storing chemical reaction data:
c
c     rk     -- reaction rate constant (ppm/hr)
c     ltdep  -- flag to determine if rate constant is temperature dependent
c     lpdep  -- flag to determine if rate constant is pressure dependent
c     bdnl   -- lower vound value for each modeled species (ppm)
c     bdlrad -- lower bound value for each radical species (ppm)
c-----------------------------------------------------------------------
c
      real    rk(MXRXN)
      logical ltdep(MXRXN)
      logical lpdep(MXRXN)
      real    bdnl(MXSPEC+1)
      real    bdlrad
c
      common /chmratep/ rk
c$omp threadprivate(/chmratep/)
      common /chmrate/ ltdep, lpdep, bdnl, bdlrad
c
c-----------------------------------------------------------------------
c     Variables for photolysis data:
c
c     nphot1   -- number of primary photolysis reactions
c     nphot2   -- number of secondary (scaled) photolysis reactions
c     idphot1  -- ID of primary photolysis reactions
c     idphot2  -- ID of secondary (scaled) photolysis reactions 
c     idphot3  -- ID of primary photolysis reaction to scale to obtain
c                 the secondary photolysis reaction
c     phtscl   -- photolysis reaction scaling factor
c-----------------------------------------------------------------------
c
      integer   nphot1
      integer   nphot2
      integer   idphot1(MXPHT1)
      integer   idphot2(MXPHT2)
      integer   idphot3(MXPHT2)
      real      phtscl(MXPHT2)
c
      common /photmap/ nphot1, nphot2, idphot1, idphot2, idphot3, phtscl
c 
c-----------------------------------------------------------------------
c     Variables for vertical profiles of HCl and CL2 for Hg chemistry
c
c     NHTCL   --  number of heights for concentrations 
c     htcl    --  heights (km) for the top of each concentration range
c     hclprof --  profile of HCl (ppm) with height
c     cl2day  --  profile of Cl2 (ppm) with height during day over ocean
c     cl2nite --  profile of Cl2 (ppm) with height at night over ocean
c-----------------------------------------------------------------------
c
      integer   NHTCL
      parameter ( NHTCL = 6 )
      real      htcl(NHTCL)
      real      hclprof(NHTCL)
      real      cl2day(NHTCL)
      real      cl2nite(NHTCL)
c
      common /clprofs/ htcl, hclprof, cl2day, cl2nite
c 
c-----------------------------------------------------------------------
c     Variables for species names:
c
c     spname  --  name of each modeled species
c     spavg   --  name of each species to be written to the output file
c     nmrad   --  name of each radical species
c     depsp   --  name of each deposition species output to file
c-----------------------------------------------------------------------
c
      character*10 spname(MXSPEC+1)
      character*10 spavg(MXSPEC)
      character*10 nmrad(MXRADCL+1)
      character*10 depsp(4*MXSPEC)
c
      common /cname/ spname, spavg, nmrad, depsp
c 
c-----------------------------------------------------------------------
c     Variables for mapping input species to internal model order:
c
c     krad     -- mapping of radical species to specific mechanism order
c     kmap     -- mapping of species on chemistry parameters file to
c                 internal order
c     lbcmap   -- mapping of species in the boundary condition file
c     lavmap   -- mapping of species written to average file
c     lptmap   -- mapping of species in the point source emissions file
c     larmap   -- mapping of species in the surface emissions file
c     licmap   -- mapping of species in the initial conditions file
c-----------------------------------------------------------------------
c
      integer   krad(NRADNM)
      integer   kmap(NSPNAM)
      integer   lbcmap(MXSPEC)
      integer   lavmap(MXSPEC)
      integer   lptmap(MXSPEC)
      integer   larmap(MXSPEC,MXGRID) 
      integer   licmap(MXSPEC,MXGRID) 
c
      common /kname/ krad, kmap, lbcmap, lavmap, lptmap, larmap, licmap
c
      integer        kno,    kno2,     ko3
      integer       kpan,   kcres,   kpan2
      integer      kmpan,   kpbzn,   knphe
      integer      krno3,   kdcb2,   kdcb3
      integer      khno4,   kacet,   kald2
      integer      kalk1,   kalk2,   kalk3
      integer      kalk4,   kalk5,   karo1
      integer      karo2,   kbacl,   kbald
      integer      kbcl1,   kbcl2,   kbuta
      integer      kccho,   kccrs,    kcg1
      integer       kcg2,    kcg3,    kcg4
      integer       kcg5,    kcl2,     kco
      integer      kco2h,   kco3h,   kcooh
      integer      kcprm,   kdcb1,    keth
      integer      kethe,   ketoh,   kfcrs
      integer      kfmcl,   kform,   kfprm
      integer       kgly,   kh2o2,   khc2h
      integer      khcho,    khcl,    khg0
      integer       khg2,    khgp,   khno3
      integer      kho2h,   khocl,   khono
      integer      kicl1,   kicl2,   kisop
      integer      kispd,   kmbut,    kmek
      integer      kmeoh,   kmeth,   kmgly
      integer      kmtbe,    kmvk,     kna
      integer       knh3,    kntr,   knxoy
      integer       kole,   kole1,   kole2
      integer      kopen,    kpar,    kpcl
      integer       kpec,   kph2o,   kphen
      integer       kpna,   kpnh4,   kpno3
      integer       kpoa,   kprod,   kpso4
      integer      krc2h,   krc3h,   krcho
      integer      krooh,    kso2,   ksoa1
      integer      ksoa2,   ksoa3,   ksoa4
      integer      ksoa5,   ksulf,   kterp
      integer       ktol,     kxn,    kxyl
      integer    ksoa1_1, ksoa1_2, ksoa1_3
      integer    ksoa1_4, ksoa1_5, ksoa1_6
      integer    ksoa1_7, ksoa1_8, ksoa1_9
      integer   ksoa1_10, ksoa2_1, ksoa2_2
      integer    ksoa2_3, ksoa2_4, ksoa2_5
      integer    ksoa2_6, ksoa2_7, ksoa2_8
      integer    ksoa2_9,ksoa2_10, ksoa3_1
      integer    ksoa3_2, ksoa3_3, ksoa3_4
      integer    ksoa3_5, ksoa3_6, ksoa3_7
      integer    ksoa3_8, ksoa3_9,ksoa3_10
      integer    ksoa4_1, ksoa4_2, ksoa4_3
      integer    ksoa4_4, ksoa4_5, ksoa4_6
      integer    ksoa4_7, ksoa4_8, ksoa4_9
      integer   ksoa4_10, ksoa5_1, ksoa5_2
      integer    ksoa5_3, ksoa5_4, ksoa5_5
      integer    ksoa5_6, ksoa5_7, ksoa5_8
      integer    ksoa5_9,ksoa5_10,  kpoa_1
      integer     kpoa_2,  kpoa_3,  kpoa_4
      integer     kpoa_5,  kpoa_6,  kpoa_7
      integer     kpoa_8,  kpoa_9, kpoa_10
      integer     kpec_1,  kpec_2,  kpec_3
      integer     kpec_4,  kpec_5,  kpec_6
      integer     kpec_7,  kpec_8,  kpec_9
      integer    kpec_10, kcrst_1, kcrst_2
      integer    kcrst_3, kcrst_4, kcrst_5
      integer    kcrst_6, kcrst_7, kcrst_8
      integer    kcrst_9,kcrst_10, kph2o_1
      integer    kph2o_2, kph2o_3, kph2o_4
      integer    kph2o_5, kph2o_6, kph2o_7
      integer    kph2o_8, kph2o_9,kph2o_10
      integer     kpcl_1,  kpcl_2,  kpcl_3
      integer     kpcl_4,  kpcl_5,  kpcl_6
      integer     kpcl_7,  kpcl_8,  kpcl_9
      integer    kpcl_10,   kna_1,   kna_2
      integer      kna_3,   kna_4,   kna_5
      integer      kna_6,   kna_7,   kna_8
      integer      kna_9,  kna_10, kpnh4_1
      integer    kpnh4_2, kpnh4_3, kpnh4_4
      integer    kpnh4_5, kpnh4_6, kpnh4_7
      integer    kpnh4_8, kpnh4_9,kpnh4_10
      integer    kpno3_1, kpno3_2, kpno3_3
      integer    kpno3_4, kpno3_5, kpno3_6
      integer    kpno3_7, kpno3_8, kpno3_9
      integer   kpno3_10, kpso4_1, kpso4_2
      integer    kpso4_3, kpso4_4, kpso4_5
      integer    kpso4_6, kpso4_7, kpso4_8
      integer    kpso4_9,kpso4_10
c
      equivalence
     &   (kmap(  1),     kno),(kmap(  2),    kno2),(kmap(  3),     ko3),
     &   (kmap(  4),    kpan),(kmap(  5),   kcres),(kmap(  6),   kpan2),
     &   (kmap(  7),   kmpan),(kmap(  8),   kpbzn),(kmap(  9),   knphe),
     &   (kmap( 10),   krno3),(kmap( 11),   kdcb2),(kmap( 12),   kdcb3),
     &   (kmap( 13),   khno4),(kmap( 14),   kacet),(kmap( 15),   kald2),
     &   (kmap( 16),   kalk1),(kmap( 17),   kalk2),(kmap( 18),   kalk3),
     &   (kmap( 19),   kalk4),(kmap( 20),   kalk5),(kmap( 21),   karo1),
     &   (kmap( 22),   karo2),(kmap( 23),   kbacl),(kmap( 24),   kbald),
     &   (kmap( 25),   kbcl1),(kmap( 26),   kbcl2),(kmap( 27),   kbuta),
     &   (kmap( 28),   kccho),(kmap( 29),   kccrs),(kmap( 30),    kcg1),
     &   (kmap( 31),    kcg2),(kmap( 32),    kcg3),(kmap( 33),    kcg4),
     &   (kmap( 34),    kcg5),(kmap( 35),    kcl2),(kmap( 36),     kco),
     &   (kmap( 37),   kco2h),(kmap( 38),   kco3h),(kmap( 39),   kcooh),
     &   (kmap( 40),   kcprm),(kmap( 41),   kdcb1),(kmap( 42),    keth),
     &   (kmap( 43),   kethe),(kmap( 44),   ketoh),(kmap( 45),   kfcrs),
     &   (kmap( 46),   kfmcl),(kmap( 47),   kform),(kmap( 48),   kfprm),
     &   (kmap( 49),    kgly),(kmap( 50),   kh2o2),(kmap( 51),   khc2h),
     &   (kmap( 52),   khcho),(kmap( 53),    khcl),(kmap( 54),    khg0),
     &   (kmap( 55),    khg2),(kmap( 56),    khgp),(kmap( 57),   khno3),
     &   (kmap( 58),   kho2h),(kmap( 59),   khocl),(kmap( 60),   khono),
     &   (kmap( 61),   kicl1),(kmap( 62),   kicl2),(kmap( 63),   kisop),
     &   (kmap( 64),   kispd),(kmap( 65),   kmbut),(kmap( 66),    kmek),
     &   (kmap( 67),   kmeoh),(kmap( 68),   kmeth),(kmap( 69),   kmgly),
     &   (kmap( 70),   kmtbe),(kmap( 71),    kmvk),(kmap( 72),     kna),
     &   (kmap( 73),    knh3),(kmap( 74),    kntr),(kmap( 75),   knxoy),
     &   (kmap( 76),    kole),(kmap( 77),   kole1),(kmap( 78),   kole2),
     &   (kmap( 79),   kopen),(kmap( 80),    kpar),(kmap( 81),    kpcl),
     &   (kmap( 82),    kpec),(kmap( 83),   kph2o),(kmap( 84),   kphen),
     &   (kmap( 85),    kpna),(kmap( 86),   kpnh4),(kmap( 87),   kpno3),
     &   (kmap( 88),    kpoa),(kmap( 89),   kprod),(kmap( 90),   kpso4),
     &   (kmap( 91),   krc2h),(kmap( 92),   krc3h),(kmap( 93),   krcho),
     &   (kmap( 94),   krooh),(kmap( 95),    kso2),(kmap( 96),   ksoa1),
     &   (kmap( 97),   ksoa2),(kmap( 98),   ksoa3),(kmap( 99),   ksoa4),
     &   (kmap(100),   ksoa5),(kmap(101),   ksulf),(kmap(102),   kterp),
     &   (kmap(103),    ktol),(kmap(104),     kxn),(kmap(105),    kxyl),
     &   (kmap(106), ksoa1_1),(kmap(107), ksoa1_2),(kmap(108), ksoa1_3),
     &   (kmap(109), ksoa1_4),(kmap(110), ksoa1_5),(kmap(111), ksoa1_6),
     &   (kmap(112), ksoa1_7),(kmap(113), ksoa1_8),(kmap(114), ksoa1_9),
     &   (kmap(115),ksoa1_10),(kmap(116), ksoa2_1),(kmap(117), ksoa2_2),
     &   (kmap(118), ksoa2_3),(kmap(119), ksoa2_4),(kmap(120), ksoa2_5),
     &   (kmap(121), ksoa2_6),(kmap(122), ksoa2_7),(kmap(123), ksoa2_8),
     &   (kmap(124), ksoa2_9),(kmap(125),ksoa2_10),(kmap(126), ksoa3_1),
     &   (kmap(127), ksoa3_2),(kmap(128), ksoa3_3),(kmap(129), ksoa3_4),
     &   (kmap(130), ksoa3_5),(kmap(131), ksoa3_6),(kmap(132), ksoa3_7),
     &   (kmap(133), ksoa3_8),(kmap(134), ksoa3_9),(kmap(135),ksoa3_10),
     &   (kmap(136), ksoa4_1),(kmap(137), ksoa4_2),(kmap(138), ksoa4_3),
     &   (kmap(139), ksoa4_4),(kmap(140), ksoa4_5),(kmap(141), ksoa4_6),
     &   (kmap(142), ksoa4_7),(kmap(143), ksoa4_8),(kmap(144), ksoa4_9),
     &   (kmap(145),ksoa4_10),(kmap(146), ksoa5_1),(kmap(147), ksoa5_2),
     &   (kmap(148), ksoa5_3),(kmap(149), ksoa5_4),(kmap(150), ksoa5_5),
     &   (kmap(151), ksoa5_6),(kmap(152), ksoa5_7),(kmap(153), ksoa5_8),
     &   (kmap(154), ksoa5_9),(kmap(155),ksoa5_10),(kmap(156),  kpoa_1),
     &   (kmap(157),  kpoa_2),(kmap(158),  kpoa_3),(kmap(159),  kpoa_4),
     &   (kmap(160),  kpoa_5),(kmap(161),  kpoa_6),(kmap(162),  kpoa_7),
     &   (kmap(163),  kpoa_8),(kmap(164),  kpoa_9),(kmap(165), kpoa_10),
     &   (kmap(166),  kpec_1),(kmap(167),  kpec_2),(kmap(168),  kpec_3),
     &   (kmap(169),  kpec_4),(kmap(170),  kpec_5),(kmap(171),  kpec_6),
     &   (kmap(172),  kpec_7),(kmap(173),  kpec_8),(kmap(174),  kpec_9),
     &   (kmap(175), kpec_10),(kmap(176), kcrst_1),(kmap(177), kcrst_2),
     &   (kmap(178), kcrst_3),(kmap(179), kcrst_4),(kmap(180), kcrst_5),
     &   (kmap(181), kcrst_6),(kmap(182), kcrst_7),(kmap(183), kcrst_8),
     &   (kmap(184), kcrst_9),(kmap(185),kcrst_10),(kmap(186), kph2o_1),
     &   (kmap(187), kph2o_2),(kmap(188), kph2o_3),(kmap(189), kph2o_4),
     &   (kmap(190), kph2o_5),(kmap(191), kph2o_6),(kmap(192), kph2o_7),
     &   (kmap(193), kph2o_8),(kmap(194), kph2o_9),(kmap(195),kph2o_10),
     &   (kmap(196),  kpcl_1),(kmap(197),  kpcl_2),(kmap(198),  kpcl_3),
     &   (kmap(199),  kpcl_4),(kmap(200),  kpcl_5),(kmap(201),  kpcl_6),
     &   (kmap(202),  kpcl_7),(kmap(203),  kpcl_8),(kmap(204),  kpcl_9),
     &   (kmap(205), kpcl_10),(kmap(206),   kna_1),(kmap(207),   kna_2),
     &   (kmap(208),   kna_3),(kmap(209),   kna_4),(kmap(210),   kna_5),
     &   (kmap(211),   kna_6),(kmap(212),   kna_7),(kmap(213),   kna_8),
     &   (kmap(214),   kna_9),(kmap(215),  kna_10),(kmap(216), kpnh4_1),
     &   (kmap(217), kpnh4_2),(kmap(218), kpnh4_3),(kmap(219), kpnh4_4),
     &   (kmap(220), kpnh4_5),(kmap(221), kpnh4_6),(kmap(222), kpnh4_7),
     &   (kmap(223), kpnh4_8),(kmap(224), kpnh4_9),(kmap(225),kpnh4_10),
     &   (kmap(226), kpno3_1),(kmap(227), kpno3_2),(kmap(228), kpno3_3),
     &   (kmap(229), kpno3_4),(kmap(230), kpno3_5),(kmap(231), kpno3_6),
     &   (kmap(232), kpno3_7),(kmap(233), kpno3_8),(kmap(234), kpno3_9),
     &   (kmap(235),kpno3_10),(kmap(236), kpso4_1),(kmap(237), kpso4_2),
     &   (kmap(238), kpso4_3),(kmap(239), kpso4_4),(kmap(240), kpso4_5),
     &   (kmap(241), kpso4_6),(kmap(242), kpso4_7),(kmap(243), kpso4_8),
     &   (kmap(244), kpso4_9),(kmap(245),kpso4_10)
c
      integer   ko1d  ,ko    ,kclo 
      integer   kcl   ,kn2o5 ,kno3 
      integer   koh   ,kho2  ,kc2o3
      integer   kxo2  ,kxo2n ,kto2 
      integer   kror  ,kcro  ,kro2r
      integer   kr2o2 ,kro2n ,kcco3
      integer   krco3 ,kmco3 ,kbzco
      integer   kcxo2 ,khco3 ,ktbuo
      integer   kbzo  ,kbzno
c
      equivalence (krad(1), ko1d ), (krad(2), ko   ), (krad(3), kclo ),
     &            (krad(4), kcl  ), (krad(5), kn2o5), (krad(6), kno3 ),
     &            (krad(7), koh  ), (krad(8), kho2 ), (krad(9), kc2o3),
     &            (krad(10),kxo2 ), (krad(11),kxo2n), (krad(12),kto2 ),
     &            (krad(13),kror ), (krad(14),kcro ), (krad(15),kro2r),
     &            (krad(16),kr2o2), (krad(17),kro2n), (krad(18),kcco3),
     &            (krad(19),krco3), (krad(20),kmco3), (krad(21),kbzco),
     &            (krad(22),kcxo2), (krad(23),khco3), (krad(24),ktbuo),
     &            (krad(25),kbzo ), (krad(26),kbzno)
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
c-----------------------------------------------------------------------
c      
      real tempr(NTEMPR)
      real presr(NPRESR)
      real rktbl(MXRXN,NTEMPR,NPRESR)
      real htint(NHGHT)
      real zenint(NZEN)
      real prkn(NZEN,MXPHT1,NHGHT,NHAZE,NALB,NOZN)
c
      common /tables/ tempr, presr, rktbl, htint, zenint, prkn
c
c-----------------------------------------------------------------------
c     Variables to define parameters for each chemical species:
c
c     henry0   -- Henry's Law constant at STP (molar/atm)
c     tfact    -- Temperature dependence of Henry's Law constant (1/K)
c     diffrat  -- Species diffusivity
c     f0       -- Species reactivity parameter
c     rscale   -- Species scaling factor for surface resistance
c     henso20  -- Henry's Law constant at STP for SO2 (molar/atm)
c     tfactso2 -- Temperature dependence of SO2 Henry's Law constant (1/K)
c     nbin     -- Number of aerosol size bins
c     roprt    -- Aerosol density (g/m3)
c     dcut     -- Aerosol size bin cut points (um)
c     cwmin    -- Minimum cloud water threshold (g/m3)
c     tamin    -- Cloud water freezing threshold (K)
c-----------------------------------------------------------------------
c
      real henry0(MXSPEC)
      real tfact(MXSPEC)
      real diffrat(MXSPEC)
      real f0(MXSPEC)
      real rscale(MXSPEC)
      real henso20
      real tfactso2
      real cwmin,tamin
c
      common /depchm/ henry0, tfact, diffrat, f0, rscale, henso20,
     &                tfactso2, cwmin, tamin
c
      integer nbin
      real roprt(MXSPEC)
      real dcut(MXSPEC,2)
c
      common /aerochm/ nbin, roprt, dcut
c
c-----------------------------------------------------------------------
c     Pointers used to lookup pig chemistry rate constants
c
c     ipigrxn  -- pointers to the nine reactions
c                 (1)   NO2 + O3 -> NO3
c                 (2)         O3 -> O(1D)
c                 (3)      O(1D) -> O(3P)
c                 (4)      O(1D) -> 2 OH
c                 (5)  NO3 + NO2 -> NO + NO2
c                 (6)  NO3 + NO2 -> N2O5
c                 (7) N2O5 + H2O -> 2 HNO3
c                 (8)       N2O5 -> NO3 + NO2
c                 (9)    NO + NO -> 2 NO2
c
      integer ipigrxn(9)
c 
      common /pigrxn/ ipigrxn
c
c----------------------------------------------------------------------
c    Variables for controlling calls to aerosol routines
c
c     time_aero  -- next time to call aerosol routines for each grid (HHMM)
c     date_aero  -- next date to call aerosol routines for each grid (YYJJJ)
c     dtaero     -- user input time interval (min) between calls to aerosol 
c                   routines
c     dt_aero    -- adjusted (for I/O frequency) time interval (min) between
c                   calls to aerosol routines
c     aero_dt    -- incremented (actual) time (hr) between calls to aerosol 
c                   routines for each grid
      real time_aero(MXGRID)
      real dtaero
      real dt_aero
      real aero_dt(MXGRID)
      integer date_aero(MXGRID)
c
      common /aero_t/ time_aero,date_aero,dtaero,dt_aero,aero_dt

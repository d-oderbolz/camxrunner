c-----CAMx v4.51 080522
c  
c     CHMSTRY.COM contains all chemistry variables 
c                            
c     Copyright 1996-2008
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
c      12/29/06    Expanded species list for the updated SOA scheme
c      01/08/06    Expanded species list for Mechanism 6 (CB05)
c                  Now ipigrxn is a scalar pointer to NO self-reaction
c      07/04/07	   Added pointer to hydrolysis of N2O5
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
      integer       kpan,   kpanx,   kcres
      integer      kpan2,   kmpan,   kpbzn
      integer      knphe,   krno3,   kdcb2
      integer      kdcb3,   khno4,   kaacd
      integer      kacet,   kald2,   kaldx
      integer      kalk1,   kalk2,   kalk3
      integer      kalk4,   kalk5,   karo1
      integer      karo2,   kbacl,   kbald
      integer      kbcl1,   kbcl2,   kbuta
      integer      kccho,   kccrs,    kcg1
      integer       kcg2,    kcg3,    kcg4
      integer       kcg5,    kcg6,    kcg7
      integer       kcl2,     kco,   kco2h
      integer      kco3h,   kcooh,   kcprm
      integer      kdcb1,    keth,   ketha
      integer      kethe,   ketoh,   kfacd
      integer      kfcrs,   kfmcl,   kform
      integer      kfprm,    kgly,   kh2o2
      integer      khc2h,   khcho,    khcl
      integer       khg0,    khg2,    khgp
      integer      khno3,   kho2h,   khocl
      integer      khono,   kicl1,   kicl2
      integer      kiole,   kisop,    kisp
      integer      kispd,   kmbut,    kmek
      integer      kmeoh,   kmepx,   kmeth
      integer      kmgly,   kmtbe,    kmvk
      integer        kna,    knh3,    kntr
      integer      knxoy,    kole,   kole1
      integer      kole2,   kopen,   kpacd
      integer       kpar,    kpcl,    kpec
      integer      kph2o,   kphen,    kpna
      integer      kpnh4,   kpno3,    kpoa
      integer      kprod,   kpso4,   krc2h
      integer      krc3h,   krcho,   krooh
      integer       kso2,   ksoa1,   ksoa2
      integer      ksoa3,   ksoa4,   ksoa5
      integer      ksoa6,   ksoa7,   ksopa
      integer      ksopb,    ksqt,   ksulf
      integer      kterp,    ktol,   ktola
      integer       ktrp,     kxn,    kxyl
      integer      kxyla, ksoa1_1, ksoa1_2
      integer    ksoa1_3, ksoa1_4, ksoa1_5
      integer    ksoa1_6, ksoa1_7, ksoa1_8
      integer    ksoa1_9,ksoa1_10, ksoa2_1
      integer    ksoa2_2, ksoa2_3, ksoa2_4
      integer    ksoa2_5, ksoa2_6, ksoa2_7
      integer    ksoa2_8, ksoa2_9,ksoa2_10
      integer    ksoa3_1, ksoa3_2, ksoa3_3
      integer    ksoa3_4, ksoa3_5, ksoa3_6
      integer    ksoa3_7, ksoa3_8, ksoa3_9
      integer   ksoa3_10, ksoa4_1, ksoa4_2
      integer    ksoa4_3, ksoa4_4, ksoa4_5
      integer    ksoa4_6, ksoa4_7, ksoa4_8
      integer    ksoa4_9,ksoa4_10, ksoa5_1
      integer    ksoa5_2, ksoa5_3, ksoa5_4
      integer    ksoa5_5, ksoa5_6, ksoa5_7
      integer    ksoa5_8, ksoa5_9,ksoa5_10
      integer    ksoa6_1, ksoa6_2, ksoa6_3
      integer    ksoa6_4, ksoa6_5, ksoa6_6
      integer    ksoa6_7, ksoa6_8, ksoa6_9
      integer   ksoa6_10, ksoa7_1, ksoa7_2
      integer    ksoa7_3, ksoa7_4, ksoa7_5
      integer    ksoa7_6, ksoa7_7, ksoa7_8
      integer    ksoa7_9,ksoa7_10, ksopa_1
      integer    ksopa_2, ksopa_3, ksopa_4
      integer    ksopa_5, ksopa_6, ksopa_7
      integer    ksopa_8, ksopa_9,ksopa_10
      integer    ksopb_1, ksopb_2, ksopb_3
      integer    ksopb_4, ksopb_5, ksopb_6
      integer    ksopb_7, ksopb_8, ksopb_9
      integer   ksopb_10,  kpoa_1,  kpoa_2
      integer     kpoa_3,  kpoa_4,  kpoa_5
      integer     kpoa_6,  kpoa_7,  kpoa_8
      integer     kpoa_9, kpoa_10,  kpec_1
      integer     kpec_2,  kpec_3,  kpec_4
      integer     kpec_5,  kpec_6,  kpec_7
      integer     kpec_8,  kpec_9, kpec_10
      integer    kcrst_1, kcrst_2, kcrst_3
      integer    kcrst_4, kcrst_5, kcrst_6
      integer    kcrst_7, kcrst_8, kcrst_9
      integer   kcrst_10, kph2o_1, kph2o_2
      integer    kph2o_3, kph2o_4, kph2o_5
      integer    kph2o_6, kph2o_7, kph2o_8
      integer    kph2o_9,kph2o_10,  kpcl_1
      integer     kpcl_2,  kpcl_3,  kpcl_4
      integer     kpcl_5,  kpcl_6,  kpcl_7
      integer     kpcl_8,  kpcl_9, kpcl_10
      integer      kna_1,   kna_2,   kna_3
      integer      kna_4,   kna_5,   kna_6
      integer      kna_7,   kna_8,   kna_9
      integer     kna_10, kpnh4_1, kpnh4_2
      integer    kpnh4_3, kpnh4_4, kpnh4_5
      integer    kpnh4_6, kpnh4_7, kpnh4_8
      integer    kpnh4_9,kpnh4_10, kpno3_1
      integer    kpno3_2, kpno3_3, kpno3_4
      integer    kpno3_5, kpno3_6, kpno3_7
      integer    kpno3_8, kpno3_9,kpno3_10
      integer    kpso4_1, kpso4_2, kpso4_3
      integer    kpso4_4, kpso4_5, kpso4_6
      integer    kpso4_7, kpso4_8, kpso4_9
      integer   kpso4_10
c
      equivalence
     &   (kmap(  1),     kno),(kmap(  2),    kno2),(kmap(  3),     ko3),
     &   (kmap(  4),    kpan),(kmap(  5),   kpanx),(kmap(  6),   kcres),
     &   (kmap(  7),   kpan2),(kmap(  8),   kmpan),(kmap(  9),   kpbzn),
     &   (kmap( 10),   knphe),(kmap( 11),   krno3),(kmap( 12),   kdcb2),
     &   (kmap( 13),   kdcb3),(kmap( 14),   khno4),(kmap( 15),   kaacd),
     &   (kmap( 16),   kacet),(kmap( 17),   kald2),(kmap( 18),   kaldx),
     &   (kmap( 19),   kalk1),(kmap( 20),   kalk2),(kmap( 21),   kalk3),
     &   (kmap( 22),   kalk4),(kmap( 23),   kalk5),(kmap( 24),   karo1),
     &   (kmap( 25),   karo2),(kmap( 26),   kbacl),(kmap( 27),   kbald),
     &   (kmap( 28),   kbcl1),(kmap( 29),   kbcl2),(kmap( 30),   kbuta),
     &   (kmap( 31),   kccho),(kmap( 32),   kccrs),(kmap( 33),    kcg1),
     &   (kmap( 34),    kcg2),(kmap( 35),    kcg3),(kmap( 36),    kcg4),
     &   (kmap( 37),    kcg5),(kmap( 38),    kcg6),(kmap( 39),    kcg7),
     &   (kmap( 40),    kcl2),(kmap( 41),     kco),(kmap( 42),   kco2h),
     &   (kmap( 43),   kco3h),(kmap( 44),   kcooh),(kmap( 45),   kcprm),
     &   (kmap( 46),   kdcb1),(kmap( 47),    keth),(kmap( 48),   ketha),
     &   (kmap( 49),   kethe),(kmap( 50),   ketoh),(kmap( 51),   kfacd),
     &   (kmap( 52),   kfcrs),(kmap( 53),   kfmcl),(kmap( 54),   kform),
     &   (kmap( 55),   kfprm),(kmap( 56),    kgly),(kmap( 57),   kh2o2),
     &   (kmap( 58),   khc2h),(kmap( 59),   khcho),(kmap( 60),    khcl),
     &   (kmap( 61),    khg0),(kmap( 62),    khg2),(kmap( 63),    khgp),
     &   (kmap( 64),   khno3),(kmap( 65),   kho2h),(kmap( 66),   khocl),
     &   (kmap( 67),   khono),(kmap( 68),   kicl1),(kmap( 69),   kicl2),
     &   (kmap( 70),   kiole),(kmap( 71),   kisop),(kmap( 72),    kisp),
     &   (kmap( 73),   kispd),(kmap( 74),   kmbut),(kmap( 75),    kmek),
     &   (kmap( 76),   kmeoh),(kmap( 77),   kmepx),(kmap( 78),   kmeth),
     &   (kmap( 79),   kmgly),(kmap( 80),   kmtbe),(kmap( 81),    kmvk),
     &   (kmap( 82),     kna),(kmap( 83),    knh3),(kmap( 84),    kntr),
     &   (kmap( 85),   knxoy),(kmap( 86),    kole),(kmap( 87),   kole1),
     &   (kmap( 88),   kole2),(kmap( 89),   kopen),(kmap( 90),   kpacd),
     &   (kmap( 91),    kpar),(kmap( 92),    kpcl),(kmap( 93),    kpec),
     &   (kmap( 94),   kph2o),(kmap( 95),   kphen),(kmap( 96),    kpna),
     &   (kmap( 97),   kpnh4),(kmap( 98),   kpno3),(kmap( 99),    kpoa),
     &   (kmap(100),   kprod),(kmap(101),   kpso4),(kmap(102),   krc2h),
     &   (kmap(103),   krc3h),(kmap(104),   krcho),(kmap(105),   krooh),
     &   (kmap(106),    kso2),(kmap(107),   ksoa1),(kmap(108),   ksoa2),
     &   (kmap(109),   ksoa3),(kmap(110),   ksoa4),(kmap(111),   ksoa5),
     &   (kmap(112),   ksoa6),(kmap(113),   ksoa7),(kmap(114),   ksopa),
     &   (kmap(115),   ksopb),(kmap(116),    ksqt),(kmap(117),   ksulf),
     &   (kmap(118),   kterp),(kmap(119),    ktol),(kmap(120),   ktola),
     &   (kmap(121),    ktrp),(kmap(122),     kxn),(kmap(123),    kxyl),
     &   (kmap(124),   kxyla),(kmap(125), ksoa1_1),(kmap(126), ksoa1_2),
     &   (kmap(127), ksoa1_3),(kmap(128), ksoa1_4),(kmap(129), ksoa1_5),
     &   (kmap(130), ksoa1_6),(kmap(131), ksoa1_7),(kmap(132), ksoa1_8),
     &   (kmap(133), ksoa1_9),(kmap(134),ksoa1_10),(kmap(135), ksoa2_1),
     &   (kmap(136), ksoa2_2),(kmap(137), ksoa2_3),(kmap(138), ksoa2_4),
     &   (kmap(139), ksoa2_5),(kmap(140), ksoa2_6),(kmap(141), ksoa2_7),
     &   (kmap(142), ksoa2_8),(kmap(143), ksoa2_9),(kmap(144),ksoa2_10),
     &   (kmap(145), ksoa3_1),(kmap(146), ksoa3_2),(kmap(147), ksoa3_3),
     &   (kmap(148), ksoa3_4),(kmap(149), ksoa3_5),(kmap(150), ksoa3_6),
     &   (kmap(151), ksoa3_7),(kmap(152), ksoa3_8),(kmap(153), ksoa3_9),
     &   (kmap(154),ksoa3_10),(kmap(155), ksoa4_1),(kmap(156), ksoa4_2),
     &   (kmap(157), ksoa4_3),(kmap(158), ksoa4_4),(kmap(159), ksoa4_5),
     &   (kmap(160), ksoa4_6),(kmap(161), ksoa4_7),(kmap(162), ksoa4_8),
     &   (kmap(163), ksoa4_9),(kmap(164),ksoa4_10),(kmap(165), ksoa5_1),
     &   (kmap(166), ksoa5_2),(kmap(167), ksoa5_3),(kmap(168), ksoa5_4),
     &   (kmap(169), ksoa5_5),(kmap(170), ksoa5_6),(kmap(171), ksoa5_7),
     &   (kmap(172), ksoa5_8),(kmap(173), ksoa5_9),(kmap(174),ksoa5_10),
     &   (kmap(175), ksoa6_1),(kmap(176), ksoa6_2),(kmap(177), ksoa6_3),
     &   (kmap(178), ksoa6_4),(kmap(179), ksoa6_5),(kmap(180), ksoa6_6),
     &   (kmap(181), ksoa6_7),(kmap(182), ksoa6_8),(kmap(183), ksoa6_9),
     &   (kmap(184),ksoa6_10),(kmap(185), ksoa7_1),(kmap(186), ksoa7_2),
     &   (kmap(187), ksoa7_3),(kmap(188), ksoa7_4),(kmap(189), ksoa7_5),
     &   (kmap(190), ksoa7_6),(kmap(191), ksoa7_7),(kmap(192), ksoa7_8),
     &   (kmap(193), ksoa7_9),(kmap(194),ksoa7_10),(kmap(195), ksopa_1),
     &   (kmap(196), ksopa_2),(kmap(197), ksopa_3),(kmap(198), ksopa_4),
     &   (kmap(199), ksopa_5),(kmap(200), ksopa_6),(kmap(201), ksopa_7),
     &   (kmap(202), ksopa_8),(kmap(203), ksopa_9),(kmap(204),ksopa_10),
     &   (kmap(205), ksopb_1),(kmap(206), ksopb_2),(kmap(207), ksopb_3),
     &   (kmap(208), ksopb_4),(kmap(209), ksopb_5),(kmap(210), ksopb_6),
     &   (kmap(211), ksopb_7),(kmap(212), ksopb_8),(kmap(213), ksopb_9),
     &   (kmap(214),ksopb_10),(kmap(215),  kpoa_1),(kmap(216),  kpoa_2),
     &   (kmap(217),  kpoa_3),(kmap(218),  kpoa_4),(kmap(219),  kpoa_5),
     &   (kmap(220),  kpoa_6),(kmap(221),  kpoa_7),(kmap(222),  kpoa_8),
     &   (kmap(223),  kpoa_9),(kmap(224), kpoa_10),(kmap(225),  kpec_1),
     &   (kmap(226),  kpec_2),(kmap(227),  kpec_3),(kmap(228),  kpec_4),
     &   (kmap(229),  kpec_5),(kmap(230),  kpec_6),(kmap(231),  kpec_7),
     &   (kmap(232),  kpec_8),(kmap(233),  kpec_9),(kmap(234), kpec_10),
     &   (kmap(235), kcrst_1),(kmap(236), kcrst_2),(kmap(237), kcrst_3),
     &   (kmap(238), kcrst_4),(kmap(239), kcrst_5),(kmap(240), kcrst_6),
     &   (kmap(241), kcrst_7),(kmap(242), kcrst_8),(kmap(243), kcrst_9),
     &   (kmap(244),kcrst_10),(kmap(245), kph2o_1),(kmap(246), kph2o_2),
     &   (kmap(247), kph2o_3),(kmap(248), kph2o_4),(kmap(249), kph2o_5),
     &   (kmap(250), kph2o_6),(kmap(251), kph2o_7),(kmap(252), kph2o_8),
     &   (kmap(253), kph2o_9),(kmap(254),kph2o_10),(kmap(255),  kpcl_1),
     &   (kmap(256),  kpcl_2),(kmap(257),  kpcl_3),(kmap(258),  kpcl_4),
     &   (kmap(259),  kpcl_5),(kmap(260),  kpcl_6),(kmap(261),  kpcl_7),
     &   (kmap(262),  kpcl_8),(kmap(263),  kpcl_9),(kmap(264), kpcl_10),
     &   (kmap(265),   kna_1),(kmap(266),   kna_2),(kmap(267),   kna_3),
     &   (kmap(268),   kna_4),(kmap(269),   kna_5),(kmap(270),   kna_6),
     &   (kmap(271),   kna_7),(kmap(272),   kna_8),(kmap(273),   kna_9),
     &   (kmap(274),  kna_10),(kmap(275), kpnh4_1),(kmap(276), kpnh4_2),
     &   (kmap(277), kpnh4_3),(kmap(278), kpnh4_4),(kmap(279), kpnh4_5),
     &   (kmap(280), kpnh4_6),(kmap(281), kpnh4_7),(kmap(282), kpnh4_8),
     &   (kmap(283), kpnh4_9),(kmap(284),kpnh4_10),(kmap(285), kpno3_1),
     &   (kmap(286), kpno3_2),(kmap(287), kpno3_3),(kmap(288), kpno3_4),
     &   (kmap(289), kpno3_5),(kmap(290), kpno3_6),(kmap(291), kpno3_7),
     &   (kmap(292), kpno3_8),(kmap(293), kpno3_9),(kmap(294),kpno3_10),
     &   (kmap(295), kpso4_1),(kmap(296), kpso4_2),(kmap(297), kpso4_3),
     &   (kmap(298), kpso4_4),(kmap(299), kpso4_5),(kmap(300), kpso4_6),
     &   (kmap(301), kpso4_7),(kmap(302), kpso4_8),(kmap(303), kpso4_9),
     &   (kmap(304),kpso4_10)
c
      integer   ko1d  ,ko    ,kclo 
      integer   kcl   ,kn2o5 ,kno3 
      integer   koh   ,kho2  ,kc2o3
      integer   kxo2  ,kxo2n ,kcxo3
      integer   kmeo2 ,kto2  ,kror
      integer   kcro  ,kro2r ,kr2o2
      integer   kro2n ,kcco3 ,krco3
      integer   kmco3 ,kbzco ,kcxo2
      integer   khco3 ,ktbuo ,kbzo
      integer   kbzno
c
      equivalence (krad(1), ko1d ), (krad(2), ko   ), (krad(3), kclo ),
     &            (krad(4), kcl  ), (krad(5), kn2o5), (krad(6), kno3 ),
     &            (krad(7), koh  ), (krad(8), kho2 ), (krad(9), kc2o3),
     &            (krad(10),kxo2 ), (krad(11),kxo2n), (krad(12),kcxo3),
     &            (krad(13),kmeo2), (krad(14),kto2 ), (krad(15),kror ),
     &            (krad(16),kcro ), (krad(17),kro2r), (krad(18),kr2o2),
     &            (krad(19),kro2n), (krad(20),kcco3), (krad(21),krco3),
     &            (krad(22),kmco3), (krad(23),kbzco), (krad(24),kcxo2),
     &            (krad(25),khco3), (krad(26),ktbuo), (krad(27),kbzo ),
     &            (krad(28),kbzno)
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
c     Pointer to hydrolysis of N2O5
c                    N2O5 + H2O -> 2 HNO3
c
      integer ihydrxn
      common /hydrxn/ ihydrxn
c
c-----------------------------------------------------------------------
c     Pointer used to lookup pig chemistry rate constant
c
c     ipigrxn  -- pointer to NO self-reaction
c                    NO + NO -> 2 NO2
c
      integer ipigrxn
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

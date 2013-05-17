c----CAMx v6.00 130506
c  
c     CHMDAT.COM contains all chemistry variables 
c                            
c     Copyright 1996 - 2013
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
c      11/30/06    Created by taking Static variables out of chmstry.com
c      12/29/06 -bkoo-     Expanded species list for the updated SOA scheme
c      01/08/07 -bkoo-     Expanded species list for Mechanism 6 (CB05)
c                          Now ipigrxn is a scalar pointer to NO self-reaction
c      07/04/07 -bkoo-     Added pointer to hydrolysis of N2O5
c      04/28/08 -gyarwood- Added EBI solver
c      04/12/12 -cemery-   Added T and P adjustments to photolysis rates
c      07/20/12 -ou-       Added Mechanism 1 (CB06 with Iodine chemistry)
c      10/08/12 -jjung-    Added Mechanism 2 (CB6r1)
c 
c-----------------------------------------------------------------------
c     Parameters for some of the switches:
c
c     CDCMC  -- string for requesting the CMC chemistry solver
c     CDEBI  -- string for requesting the EBI chemistry solver
c     CDIEH  -- string for requesting the IEH chemistry solver
c     CDLSOD -- string for requesting the LSODE chemistry solver
c     IDCMC  -- code for using the CMC chemistry solver
c     IDEBI  -- code for using the EBI chemistry solver
c     IDIEH  -- code for using the IEH chemistry solver
c     IDLSOD -- code for using the LSODE chemistry solver
c
c-----------------------------------------------------------------------
c
      character*10 CDCMC
      character*10 CDEBI
      character*10 CDIEH
      character*10 CDLSOD
      integer      IDCMC
      integer      IDEBI
      integer      IDIEH
      integer      IDLSOD
c
      parameter( CDCMC  = "CMC       " )
      parameter( CDEBI  = "EBI       " )
      parameter( CDIEH  = "IEH       " )
      parameter( CDLSOD = "LSODE     " )
      parameter( IDCMC  = 1 )
      parameter( IDEBI  = 2 )
      parameter( IDIEH  = 3 )
      parameter( IDLSOD = 4 )
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
c    ndepspc--  number of species to write to deposition file
c    nicspc --  number of species in the initial conditions file
c    nbcspc --  number of species in the boundary conditions file
c    nptspc --  number of species in the point source emissions file
c
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
       integer   ndepspc
       integer   nicspc
       integer   nbcspc
       integer   nptspc
c
      common /chm1/ ngas, naero, nspec, nrad, nreact, nspfst, iessrad,
     &                         idmech, idsolv, navspc, ndepspc, nicspc,
     &                                                    nbcspc, nptspc
c
c-----------------------------------------------------------------------
c
c    aeropt --  keyword for aerosol scheme ('NONE', 'INERT', 'CF' or 'CMU')
c
c-----------------------------------------------------------------------
c
      character*10 aeropt
c
      common /chm2/ aeropt
c
c-----------------------------------------------------------------------
c     Variables for photolysis data:
c
c     nphot1   -- number of primary photolysis reactions
c     nphot2   -- number of secondary (scaled) photolysis reactions
c
c-----------------------------------------------------------------------
c
      integer nphot1
      integer nphot2
c
      common /photmap/ nphot1, nphot2
c 
c-----------------------------------------------------------------------
c     Mercury chemistry variables 
c     Variables for vertical profiles of Cl2, Br and BrO for Hg chemistry
c
c     NHTHAL   --  number of heights for halogen concentrations 
c     hthal    --  heights (km) for the top of each concentration range
c     cl2day  --  profile of Cl2 (ppm) with height during day over ocean
c     cl2nite --  profile of Cl2 (ppm) with height at night over ocean
c     brlprof  --  profile of Br (ppm) with height over land
c     brwprof  --  profile of Br (ppm) with height over ocean
c     brolprof --  profile of BrO (ppm) with height over land
c     browprof --  profile of BrO (ppm) with height over ocean
c
c     MWHG     --  Hg molecular weight 
c-----------------------------------------------------------------------
c
      integer, parameter :: NHTHAL = 6
      real, parameter :: MWHG = 200.59
c
      real, dimension(NHTHAL) :: hthal,brlprof,brwprof,brolprof,
     &                           browprof,cl2day,cl2nite
c
      common /halprofs/ hthal, brlprof, brwprof, brolprof, browprof,
     &                  cl2day, cl2nite
c 
c-----------------------------------------------------------------------
c     Parameters for mapping input species to internal model order:
c
c      NSPNAM  -- number of recognizable state species
c
      integer   NSPNAM
c
      parameter ( NSPNAM  =   386 )
c 
c-----------------------------------------------------------------------
c     Variables for mapping input species to internal model order:
c
c     kmap     -- mapping of species on chemistry parameters file to
c                 internal order
c
c-----------------------------------------------------------------------
c
      integer kmap(NSPNAM)
c
      common /kname/ kmap
c
      integer       ko1d,      ko,     kcl
      integer       kclo,     koh,    kho2
      integer      kc2o3,    kxo2,   kxo2n
      integer      kcxo3,   kmeo2,    kto2
      integer       kror,    kcro,   kro2r
      integer      kr2o2,   kro2n,   kcco3
      integer      krco3,   kmco3,   kbzco
      integer      kcxo2,   khco3,   ktbuo
      integer       kbzo,   kbzno,      ki
      integer        kno,    kno2,     ko3
      integer       kpan,   kpanx,   kcres
      integer      kpan2,   kmpan,   kpbzn
      integer      knphe,   krno3,   kdcb2
      integer      kdcb3,   khno4,   kbzo2
      integer      kcao2,   kcrn2,   kcrno
      integer      kepx2,   kiso2,   kopan
      integer      kopo3,    kro2,   kxlo2
      integer      kxo2h,   kaacd,   kacet
      integer      kald2,   kaldx,   kalk1
      integer      kalk2,   kalk3,   kalk4
      integer      kalk5,   karo1,   karo2
      integer      kbacl,   kbald,   kbcl1
      integer      kbcl2,   kbenz,   kbuta
      integer     kc3h7i,   kcat1,   kccho
      integer      kccrs,    kcg1,    kcg2
      integer       kcg3,    kcg4,    kcg5
      integer       kcg6,    kcg7,  kch2i2
      integer    kch2ibr, kch2icl,   kch3i
      integer       kcl2,     kco,   kco2h
      integer      kco3h,   kcooh,   kcprm
      integer      kcron,   kcrpx,   kdcb1
      integer      kepox,    keth,   ketha
      integer      kethe,   kethy,   ketoh
      integer      kfacd,   kfcrs,   kfmcl
      integer      kform,   kfprm,    kgly
      integer      kglyd,   kh2o2,   khc2h
      integer      khcho,    khcl,    khg0
      integer       khg2,    khgp,     khi
      integer      khio3,   khno3,   kho2h
      integer      khocl,    khoi,  khoio2
      integer      khono,   khpld,     ki2
      integer      ki2o2,   kialk,   kibac
      integer      kibuo,   kicl1,   kicl2
      integer       kino,   kino2,   kino3
      integer      kintr,     kio,   kiole
      integer     kiono2,   kisop,    kisp
      integer      kispd,   kispx,   kixoy
      integer       kket,   kmbut,    kmek
      integer      kmeoh,   kmepx,   kmeth
      integer      kmgly,    kmib,    kmic
      integer       kmi2,   kmtbe,    kmvk
      integer      kn2o5,   kn3cl,     kna
      integer      knbui,    knh3,    kno3
      integer      kntcl,    kntr,    koio
      integer       kole,   kole1,   kole2
      integer      kopen,   kpacd,    kpar
      integer       kpcl,    kpec,   kph2o
      integer      kphen,    kpna,   kpnh4
      integer      kpno3,    kpoa,   kprod
      integer      kprpa,   kpso4,   krc2h
      integer      krc3h,   krcho,   krooh
      integer       kso2,   ksoa1,   ksoa2
      integer      ksoa3,   ksoa4,   ksoa5
      integer      ksoa6,   ksoa7,   ksopa
      integer      ksopb,    ksqt,     kss
      integer      ksulf,   kterp,    ktol
      integer      ktola,    ktrp,     kxn
      integer      kxopn,    kxyl,   kxyla
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
      integer    ksoa5_9,ksoa5_10, ksoa6_1
      integer    ksoa6_2, ksoa6_3, ksoa6_4
      integer    ksoa6_5, ksoa6_6, ksoa6_7
      integer    ksoa6_8, ksoa6_9,ksoa6_10
      integer    ksoa7_1, ksoa7_2, ksoa7_3
      integer    ksoa7_4, ksoa7_5, ksoa7_6
      integer    ksoa7_7, ksoa7_8, ksoa7_9
      integer   ksoa7_10, ksopa_1, ksopa_2
      integer    ksopa_3, ksopa_4, ksopa_5
      integer    ksopa_6, ksopa_7, ksopa_8
      integer    ksopa_9,ksopa_10, ksopb_1
      integer    ksopb_2, ksopb_3, ksopb_4
      integer    ksopb_5, ksopb_6, ksopb_7
      integer    ksopb_8, ksopb_9,ksopb_10
      integer     kpoa_1,  kpoa_2,  kpoa_3
      integer     kpoa_4,  kpoa_5,  kpoa_6
      integer     kpoa_7,  kpoa_8,  kpoa_9
      integer    kpoa_10,  kpec_1,  kpec_2
      integer     kpec_3,  kpec_4,  kpec_5
      integer     kpec_6,  kpec_7,  kpec_8
      integer     kpec_9, kpec_10, kcrst_1
      integer    kcrst_2, kcrst_3, kcrst_4
      integer    kcrst_5, kcrst_6, kcrst_7
      integer    kcrst_8, kcrst_9,kcrst_10
      integer    kph2o_1, kph2o_2, kph2o_3
      integer    kph2o_4, kph2o_5, kph2o_6
      integer    kph2o_7, kph2o_8, kph2o_9
      integer   kph2o_10,  kpcl_1,  kpcl_2
      integer     kpcl_3,  kpcl_4,  kpcl_5
      integer     kpcl_6,  kpcl_7,  kpcl_8
      integer     kpcl_9, kpcl_10,   kna_1
      integer      kna_2,   kna_3,   kna_4
      integer      kna_5,   kna_6,   kna_7
      integer      kna_8,   kna_9,  kna_10
      integer    kpnh4_1, kpnh4_2, kpnh4_3
      integer    kpnh4_4, kpnh4_5, kpnh4_6
      integer    kpnh4_7, kpnh4_8, kpnh4_9
      integer   kpnh4_10, kpno3_1, kpno3_2
      integer    kpno3_3, kpno3_4, kpno3_5
      integer    kpno3_6, kpno3_7, kpno3_8
      integer    kpno3_9,kpno3_10, kpso4_1
      integer    kpso4_2, kpso4_3, kpso4_4
      integer    kpso4_5, kpso4_6, kpso4_7
      integer    kpso4_8, kpso4_9,kpso4_10
      integer     khgiip, khgiipc
c
      equivalence
     &   (kmap(  1),    ko1d),(kmap(  2),      ko),(kmap(  3),     kcl),
     &   (kmap(  4),    kclo),(kmap(  5),     koh),(kmap(  6),    kho2),
     &   (kmap(  7),   kc2o3),(kmap(  8),    kxo2),(kmap(  9),   kxo2n),
     &   (kmap( 10),   kcxo3),(kmap( 11),   kmeo2),(kmap( 12),    kto2),
     &   (kmap( 13),    kror),(kmap( 14),    kcro),(kmap( 15),   kro2r),
     &   (kmap( 16),   kr2o2),(kmap( 17),   kro2n),(kmap( 18),   kcco3),
     &   (kmap( 19),   krco3),(kmap( 20),   kmco3),(kmap( 21),   kbzco),
     &   (kmap( 22),   kcxo2),(kmap( 23),   khco3),(kmap( 24),   ktbuo),
     &   (kmap( 25),    kbzo),(kmap( 26),   kbzno),(kmap( 27),      ki),
     &   (kmap( 28),     kno),(kmap( 29),    kno2),(kmap( 30),     ko3),
     &   (kmap( 31),    kpan),(kmap( 32),   kpanx),(kmap( 33),   kcres),
     &   (kmap( 34),   kpan2),(kmap( 35),   kmpan),(kmap( 36),   kpbzn),
     &   (kmap( 37),   knphe),(kmap( 38),   krno3),(kmap( 39),   kdcb2),
     &   (kmap( 40),   kdcb3),(kmap( 41),   khno4),(kmap( 42),   kbzo2),
     &   (kmap( 43),   kcao2),(kmap( 44),   kcrn2),(kmap( 45),   kcrno),
     &   (kmap( 46),   kepx2),(kmap( 47),   kiso2),(kmap( 48),   kopan),
     &   (kmap( 49),   kopo3),(kmap( 50),    kro2),(kmap( 51),   kxlo2),
     &   (kmap( 52),   kxo2h),(kmap( 53),   kaacd),(kmap( 54),   kacet),
     &   (kmap( 55),   kald2),(kmap( 56),   kaldx),(kmap( 57),   kalk1),
     &   (kmap( 58),   kalk2),(kmap( 59),   kalk3),(kmap( 60),   kalk4),
     &   (kmap( 61),   kalk5),(kmap( 62),   karo1),(kmap( 63),   karo2),
     &   (kmap( 64),   kbacl),(kmap( 65),   kbald),(kmap( 66),   kbcl1),
     &   (kmap( 67),   kbcl2),(kmap( 68),   kbenz),(kmap( 69),   kbuta),
     &   (kmap( 70),  kc3h7i),(kmap( 71),   kcat1),(kmap( 72),   kccho),
     &   (kmap( 73),   kccrs),(kmap( 74),    kcg1),(kmap( 75),    kcg2),
     &   (kmap( 76),    kcg3),(kmap( 77),    kcg4),(kmap( 78),    kcg5),
     &   (kmap( 79),    kcg6),(kmap( 80),    kcg7),(kmap( 81),  kch2i2),
     &   (kmap( 82), kch2ibr),(kmap( 83), kch2icl),(kmap( 84),   kch3i),
     &   (kmap( 85),    kcl2),(kmap( 86),     kco),(kmap( 87),   kco2h),
     &   (kmap( 88),   kco3h),(kmap( 89),   kcooh),(kmap( 90),   kcprm),
     &   (kmap( 91),   kcron),(kmap( 92),   kcrpx),(kmap( 93),   kdcb1),
     &   (kmap( 94),   kepox),(kmap( 95),    keth),(kmap( 96),   ketha),
     &   (kmap( 97),   kethe),(kmap( 98),   kethy),(kmap( 99),   ketoh),
     &   (kmap(100),   kfacd),(kmap(101),   kfcrs),(kmap(102),   kfmcl),
     &   (kmap(103),   kform),(kmap(104),   kfprm),(kmap(105),    kgly),
     &   (kmap(106),   kglyd),(kmap(107),   kh2o2),(kmap(108),   khc2h),
     &   (kmap(109),   khcho),(kmap(110),    khcl),(kmap(111),    khg0),
     &   (kmap(112),    khg2),(kmap(113),    khgp),(kmap(114),     khi),
     &   (kmap(115),   khio3),(kmap(116),   khno3),(kmap(117),   kho2h),
     &   (kmap(118),   khocl),(kmap(119),    khoi),(kmap(120),  khoio2),
     &   (kmap(121),   khono),(kmap(122),   khpld),(kmap(123),     ki2),
     &   (kmap(124),   ki2o2),(kmap(125),   kialk),(kmap(126),   kibac),
     &   (kmap(127),   kibuo),(kmap(128),   kicl1),(kmap(129),   kicl2),
     &   (kmap(130),    kino),(kmap(131),   kino2),(kmap(132),   kino3),
     &   (kmap(133),   kintr),(kmap(134),     kio),(kmap(135),   kiole),
     &   (kmap(136),  kiono2),(kmap(137),   kisop),(kmap(138),    kisp),
     &   (kmap(139),   kispd),(kmap(140),   kispx),(kmap(141),   kixoy),
     &   (kmap(142),    kket),(kmap(143),   kmbut),(kmap(144),    kmek),
     &   (kmap(145),   kmeoh),(kmap(146),   kmepx),(kmap(147),   kmeth),
     &   (kmap(148),   kmgly),(kmap(149),    kmib),(kmap(150),    kmic),
     &   (kmap(151),    kmi2),(kmap(152),   kmtbe),(kmap(153),    kmvk),
     &   (kmap(154),   kn2o5),(kmap(155),   kn3cl),(kmap(156),     kna),
     &   (kmap(157),   knbui),(kmap(158),    knh3),(kmap(159),    kno3),
     &   (kmap(160),   kntcl),(kmap(161),    kntr),(kmap(162),    koio),
     &   (kmap(163),    kole),(kmap(164),   kole1),(kmap(165),   kole2),
     &   (kmap(166),   kopen),(kmap(167),   kpacd),(kmap(168),    kpar),
     &   (kmap(169),    kpcl),(kmap(170),    kpec),(kmap(171),   kph2o),
     &   (kmap(172),   kphen),(kmap(173),    kpna),(kmap(174),   kpnh4),
     &   (kmap(175),   kpno3),(kmap(176),    kpoa),(kmap(177),   kprod),
     &   (kmap(178),   kprpa),(kmap(179),   kpso4),(kmap(180),   krc2h),
     &   (kmap(181),   krc3h),(kmap(182),   krcho),(kmap(183),   krooh),
     &   (kmap(184),    kso2),(kmap(185),   ksoa1),(kmap(186),   ksoa2),
     &   (kmap(187),   ksoa3),(kmap(188),   ksoa4),(kmap(189),   ksoa5),
     &   (kmap(190),   ksoa6),(kmap(191),   ksoa7),(kmap(192),   ksopa),
     &   (kmap(193),   ksopb),(kmap(194),    ksqt),(kmap(195),     kss),
     &   (kmap(196),   ksulf),(kmap(197),   kterp),(kmap(198),    ktol),
     &   (kmap(199),   ktola),(kmap(200),    ktrp),(kmap(201),     kxn),
     &   (kmap(202),   kxopn),(kmap(203),    kxyl),(kmap(204),   kxyla),
     &   (kmap(205), ksoa1_1),(kmap(206), ksoa1_2),(kmap(207), ksoa1_3),
     &   (kmap(208), ksoa1_4),(kmap(209), ksoa1_5),(kmap(210), ksoa1_6),
     &   (kmap(211), ksoa1_7),(kmap(212), ksoa1_8),(kmap(213), ksoa1_9),
     &   (kmap(214),ksoa1_10),(kmap(215), ksoa2_1),(kmap(216), ksoa2_2),
     &   (kmap(217), ksoa2_3),(kmap(218), ksoa2_4),(kmap(219), ksoa2_5),
     &   (kmap(220), ksoa2_6),(kmap(221), ksoa2_7),(kmap(222), ksoa2_8),
     &   (kmap(223), ksoa2_9),(kmap(224),ksoa2_10),(kmap(225), ksoa3_1),
     &   (kmap(226), ksoa3_2),(kmap(227), ksoa3_3),(kmap(228), ksoa3_4),
     &   (kmap(229), ksoa3_5),(kmap(230), ksoa3_6),(kmap(231), ksoa3_7),
     &   (kmap(232), ksoa3_8),(kmap(233), ksoa3_9),(kmap(234),ksoa3_10),
     &   (kmap(235), ksoa4_1),(kmap(236), ksoa4_2),(kmap(237), ksoa4_3),
     &   (kmap(238), ksoa4_4),(kmap(239), ksoa4_5),(kmap(240), ksoa4_6),
     &   (kmap(241), ksoa4_7),(kmap(242), ksoa4_8),(kmap(243), ksoa4_9),
     &   (kmap(244),ksoa4_10),(kmap(245), ksoa5_1),(kmap(246), ksoa5_2),
     &   (kmap(247), ksoa5_3),(kmap(248), ksoa5_4),(kmap(249), ksoa5_5),
     &   (kmap(250), ksoa5_6),(kmap(251), ksoa5_7),(kmap(252), ksoa5_8),
     &   (kmap(253), ksoa5_9),(kmap(254),ksoa5_10),(kmap(255), ksoa6_1),
     &   (kmap(256), ksoa6_2),(kmap(257), ksoa6_3),(kmap(258), ksoa6_4),
     &   (kmap(259), ksoa6_5),(kmap(260), ksoa6_6),(kmap(261), ksoa6_7),
     &   (kmap(262), ksoa6_8),(kmap(263), ksoa6_9),(kmap(264),ksoa6_10),
     &   (kmap(265), ksoa7_1),(kmap(266), ksoa7_2),(kmap(267), ksoa7_3),
     &   (kmap(268), ksoa7_4),(kmap(269), ksoa7_5),(kmap(270), ksoa7_6),
     &   (kmap(271), ksoa7_7),(kmap(272), ksoa7_8),(kmap(273), ksoa7_9),
     &   (kmap(274),ksoa7_10),(kmap(275), ksopa_1),(kmap(276), ksopa_2),
     &   (kmap(277), ksopa_3),(kmap(278), ksopa_4),(kmap(279), ksopa_5),
     &   (kmap(280), ksopa_6),(kmap(281), ksopa_7),(kmap(282), ksopa_8),
     &   (kmap(283), ksopa_9),(kmap(284),ksopa_10),(kmap(285), ksopb_1),
     &   (kmap(286), ksopb_2),(kmap(287), ksopb_3),(kmap(288), ksopb_4),
     &   (kmap(289), ksopb_5),(kmap(290), ksopb_6),(kmap(291), ksopb_7),
     &   (kmap(292), ksopb_8),(kmap(293), ksopb_9),(kmap(294),ksopb_10),
     &   (kmap(295),  kpoa_1),(kmap(296),  kpoa_2),(kmap(297),  kpoa_3),
     &   (kmap(298),  kpoa_4),(kmap(299),  kpoa_5),(kmap(300),  kpoa_6),
     &   (kmap(301),  kpoa_7),(kmap(302),  kpoa_8),(kmap(303),  kpoa_9),
     &   (kmap(304), kpoa_10),(kmap(305),  kpec_1),(kmap(306),  kpec_2),
     &   (kmap(307),  kpec_3),(kmap(308),  kpec_4),(kmap(309),  kpec_5),
     &   (kmap(310),  kpec_6),(kmap(311),  kpec_7),(kmap(312),  kpec_8),
     &   (kmap(313),  kpec_9),(kmap(314), kpec_10),(kmap(315), kcrst_1),
     &   (kmap(316), kcrst_2),(kmap(317), kcrst_3),(kmap(318), kcrst_4),
     &   (kmap(319), kcrst_5),(kmap(320), kcrst_6),(kmap(321), kcrst_7),
     &   (kmap(322), kcrst_8),(kmap(323), kcrst_9),(kmap(324),kcrst_10),
     &   (kmap(325), kph2o_1),(kmap(326), kph2o_2),(kmap(327), kph2o_3),
     &   (kmap(328), kph2o_4),(kmap(329), kph2o_5),(kmap(330), kph2o_6),
     &   (kmap(331), kph2o_7),(kmap(332), kph2o_8),(kmap(333), kph2o_9),
     &   (kmap(334),kph2o_10),(kmap(335),  kpcl_1),(kmap(336),  kpcl_2),
     &   (kmap(337),  kpcl_3),(kmap(338),  kpcl_4),(kmap(339),  kpcl_5),
     &   (kmap(340),  kpcl_6),(kmap(341),  kpcl_7),(kmap(342),  kpcl_8),
     &   (kmap(343),  kpcl_9),(kmap(344), kpcl_10),(kmap(345),   kna_1),
     &   (kmap(346),   kna_2),(kmap(347),   kna_3),(kmap(348),   kna_4),
     &   (kmap(349),   kna_5),(kmap(350),   kna_6),(kmap(351),   kna_7),
     &   (kmap(352),   kna_8),(kmap(353),   kna_9),(kmap(354),  kna_10),
     &   (kmap(355), kpnh4_1),(kmap(356), kpnh4_2),(kmap(357), kpnh4_3),
     &   (kmap(358), kpnh4_4),(kmap(359), kpnh4_5),(kmap(360), kpnh4_6),
     &   (kmap(361), kpnh4_7),(kmap(362), kpnh4_8),(kmap(363), kpnh4_9),
     &   (kmap(364),kpnh4_10),(kmap(365), kpno3_1),(kmap(366), kpno3_2),
     &   (kmap(367), kpno3_3),(kmap(368), kpno3_4),(kmap(369), kpno3_5),
     &   (kmap(370), kpno3_6),(kmap(371), kpno3_7),(kmap(372), kpno3_8),
     &   (kmap(373), kpno3_9),(kmap(374),kpno3_10),(kmap(375), kpso4_1),
     &   (kmap(376), kpso4_2),(kmap(377), kpso4_3),(kmap(378), kpso4_4),
     &   (kmap(379), kpso4_5),(kmap(380), kpso4_6),(kmap(381), kpso4_7),
     &   (kmap(382), kpso4_8),(kmap(383), kpso4_9),(kmap(384),kpso4_10),
     &   (kmap(385),  khgiip),(kmap(386), khgiipc)
c
c-----------------------------------------------------------------------
c     Variables to define parameters for each chemical species:
c
c     henso20  -- Henry's Law constant at STP for SO2 (molar/atm)
c     tfactso2 -- Temperature dependence of SO2 Henry's Law constant (1/K)
c     nbin     -- Number of aerosol size bins
c     cwmin    -- Minimum cloud water threshold (g/m3)
c     tamin    -- Cloud water freezing threshold (K)
c
c-----------------------------------------------------------------------
c
      real henso20
      real tfactso2
      real cwmin
      real tamin
c
      common /depchm/ henso20, tfactso2, cwmin, tamin
c
      integer nbin
c
      common /aerochm/ nbin
c
c-----------------------------------------------------------------------
c     Pointers to photolysis reactions that receive T,P adjustments
c
      integer jno2rxn,jo3rxn,jhcho1rxn,jhcho2rxn,jch3chorxn
      common /phtrxn/ jno2rxn,jo3rxn,jhcho1rxn,jhcho2rxn,jch3chorxn
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
c     dtaero     -- user input time interval (min) between calls to aerosol 
c                   routines
c     dt_aero    -- adjusted (for I/O frequency) time interval (min) between
c                   calls to aerosol routines
c
      real dtaero
      real dt_aero
c
      common /aero_t/ dtaero, dt_aero

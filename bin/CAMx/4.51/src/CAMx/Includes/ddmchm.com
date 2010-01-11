c-----CAMx v4.51 080522
c
c     DDMCHM.COM sets species pointers for the DDM chemistry
c     These equivalences must be consistent with the internal
c     species lists defined in data statements in READCHM
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c               
c 
      integer lmap(NSPNAM)
      integer lrad(NRADNM)
      common /lname/ lmap, lrad
c
      integer        lno,    lno2,     lo3
      integer       lpan,   lpanx,   lcres
      integer      lpan2,   lmpan,   lpbzn
      integer      lnphe,   lrno3,   ldcb2
      integer      ldcb3,   lhno4,   laacd
      integer      lacet,   lald2,   laldx
      integer      lalk1,   lalk2,   lalk3
      integer      lalk4,   lalk5,   laro1
      integer      laro2,   lbacl,   lbald
      integer      lbcl1,   lbcl2,   lbuta
      integer      lccho,   lccrs,    lcg1
      integer       lcg2,    lcg3,    lcg4
      integer       lcg5,    lcg6,    lcg7
      integer       lcl2,     lco,   lco2h
      integer      lco3h,   lcooh,   lcprm
      integer      ldcb1,    leth,   letha
      integer      lethe,   letoh,   lfacd
      integer      lfcrs,   lfmcl,   lform
      integer      lfprm,    lgly,   lh2o2
      integer      lhc2h,   lhcho,    lhcl
      integer       lhg0,    lhg2,    lhgp
      integer      lhno3,   lho2h,   lhocl
      integer      lhono,   licl1,   licl2
      integer      liole,   lisop,    lisp
      integer      lispd,   lmbut,    lmek
      integer      lmeoh,   lmepx,   lmeth
      integer      lmgly,   lmtbe,    lmvk
      integer        lna,    lnh3,    lntr
      integer      lnxoy,    lole,   lole1
      integer      lole2,   lopen,   lpacd
      integer       lpar,    lpcl,    lpec
      integer      lph2o,   lphen,    lpna
      integer      lpnh4,   lpno3,    lpoa
      integer      lprod,   lpso4,   lrc2h
      integer      lrc3h,   lrcho,   lrooh
      integer       lso2,   lsoa1,   lsoa2
      integer      lsoa3,   lsoa4,   lsoa5
      integer      lsoa6,   lsoa7,   lsopa
      integer      lsopb,    lsqt,   lsulf
      integer      lterp,    ltol,   ltola
      integer       ltrp,     lxn,    lxyl
      integer      lxyla, lsoa1_1, lsoa1_2
      integer    lsoa1_3, lsoa1_4, lsoa1_5
      integer    lsoa1_6, lsoa1_7, lsoa1_8
      integer    lsoa1_9,lsoa1_10, lsoa2_1
      integer    lsoa2_2, lsoa2_3, lsoa2_4
      integer    lsoa2_5, lsoa2_6, lsoa2_7
      integer    lsoa2_8, lsoa2_9,lsoa2_10
      integer    lsoa3_1, lsoa3_2, lsoa3_3
      integer    lsoa3_4, lsoa3_5, lsoa3_6
      integer    lsoa3_7, lsoa3_8, lsoa3_9
      integer   lsoa3_10, lsoa4_1, lsoa4_2
      integer    lsoa4_3, lsoa4_4, lsoa4_5
      integer    lsoa4_6, lsoa4_7, lsoa4_8
      integer    lsoa4_9,lsoa4_10, lsoa5_1
      integer    lsoa5_2, lsoa5_3, lsoa5_4
      integer    lsoa5_5, lsoa5_6, lsoa5_7
      integer    lsoa5_8, lsoa5_9,lsoa5_10
      integer    lsoa6_1, lsoa6_2, lsoa6_3
      integer    lsoa6_4, lsoa6_5, lsoa6_6
      integer    lsoa6_7, lsoa6_8, lsoa6_9
      integer   lsoa6_10, lsoa7_1, lsoa7_2
      integer    lsoa7_3, lsoa7_4, lsoa7_5
      integer    lsoa7_6, lsoa7_7, lsoa7_8
      integer    lsoa7_9,lsoa7_10, lsopa_1
      integer    lsopa_2, lsopa_3, lsopa_4
      integer    lsopa_5, lsopa_6, lsopa_7
      integer    lsopa_8, lsopa_9,lsopa_10
      integer    lsopb_1, lsopb_2, lsopb_3
      integer    lsopb_4, lsopb_5, lsopb_6
      integer    lsopb_7, lsopb_8, lsopb_9
      integer   lsopb_10,  lpoa_1,  lpoa_2
      integer     lpoa_3,  lpoa_4,  lpoa_5
      integer     lpoa_6,  lpoa_7,  lpoa_8
      integer     lpoa_9, lpoa_10,  lpec_1
      integer     lpec_2,  lpec_3,  lpec_4
      integer     lpec_5,  lpec_6,  lpec_7
      integer     lpec_8,  lpec_9, lpec_10
      integer    lcrst_1, lcrst_2, lcrst_3
      integer    lcrst_4, lcrst_5, lcrst_6
      integer    lcrst_7, lcrst_8, lcrst_9
      integer   lcrst_10, lph2o_1, lph2o_2
      integer    lph2o_3, lph2o_4, lph2o_5
      integer    lph2o_6, lph2o_7, lph2o_8
      integer    lph2o_9,lph2o_10,  lpcl_1
      integer     lpcl_2,  lpcl_3,  lpcl_4
      integer     lpcl_5,  lpcl_6,  lpcl_7
      integer     lpcl_8,  lpcl_9, lpcl_10
      integer      lna_1,   lna_2,   lna_3
      integer      lna_4,   lna_5,   lna_6
      integer      lna_7,   lna_8,   lna_9
      integer     lna_10, lpnh4_1, lpnh4_2
      integer    lpnh4_3, lpnh4_4, lpnh4_5
      integer    lpnh4_6, lpnh4_7, lpnh4_8
      integer    lpnh4_9,lpnh4_10, lpno3_1
      integer    lpno3_2, lpno3_3, lpno3_4
      integer    lpno3_5, lpno3_6, lpno3_7
      integer    lpno3_8, lpno3_9,lpno3_10
      integer    lpso4_1, lpso4_2, lpso4_3
      integer    lpso4_4, lpso4_5, lpso4_6
      integer    lpso4_7, lpso4_8, lpso4_9
      integer   lpso4_10
c
      equivalence
     &   (lmap(  1),     lno),(lmap(  2),    lno2),(lmap(  3),     lo3),
     &   (lmap(  4),    lpan),(lmap(  5),   lpanx),(lmap(  6),   lcres),
     &   (lmap(  7),   lpan2),(lmap(  8),   lmpan),(lmap(  9),   lpbzn),
     &   (lmap( 10),   lnphe),(lmap( 11),   lrno3),(lmap( 12),   ldcb2),
     &   (lmap( 13),   ldcb3),(lmap( 14),   lhno4),(lmap( 15),   laacd),
     &   (lmap( 16),   lacet),(lmap( 17),   lald2),(lmap( 18),   laldx),
     &   (lmap( 19),   lalk1),(lmap( 20),   lalk2),(lmap( 21),   lalk3),
     &   (lmap( 22),   lalk4),(lmap( 23),   lalk5),(lmap( 24),   laro1),
     &   (lmap( 25),   laro2),(lmap( 26),   lbacl),(lmap( 27),   lbald),
     &   (lmap( 28),   lbcl1),(lmap( 29),   lbcl2),(lmap( 30),   lbuta),
     &   (lmap( 31),   lccho),(lmap( 32),   lccrs),(lmap( 33),    lcg1),
     &   (lmap( 34),    lcg2),(lmap( 35),    lcg3),(lmap( 36),    lcg4),
     &   (lmap( 37),    lcg5),(lmap( 38),    lcg6),(lmap( 39),    lcg7),
     &   (lmap( 40),    lcl2),(lmap( 41),     lco),(lmap( 42),   lco2h),
     &   (lmap( 43),   lco3h),(lmap( 44),   lcooh),(lmap( 45),   lcprm),
     &   (lmap( 46),   ldcb1),(lmap( 47),    leth),(lmap( 48),   letha),
     &   (lmap( 49),   lethe),(lmap( 50),   letoh),(lmap( 51),   lfacd),
     &   (lmap( 52),   lfcrs),(lmap( 53),   lfmcl),(lmap( 54),   lform),
     &   (lmap( 55),   lfprm),(lmap( 56),    lgly),(lmap( 57),   lh2o2),
     &   (lmap( 58),   lhc2h),(lmap( 59),   lhcho),(lmap( 60),    lhcl),
     &   (lmap( 61),    lhg0),(lmap( 62),    lhg2),(lmap( 63),    lhgp),
     &   (lmap( 64),   lhno3),(lmap( 65),   lho2h),(lmap( 66),   lhocl),
     &   (lmap( 67),   lhono),(lmap( 68),   licl1),(lmap( 69),   licl2),
     &   (lmap( 70),   liole),(lmap( 71),   lisop),(lmap( 72),    lisp),
     &   (lmap( 73),   lispd),(lmap( 74),   lmbut),(lmap( 75),    lmek),
     &   (lmap( 76),   lmeoh),(lmap( 77),   lmepx),(lmap( 78),   lmeth),
     &   (lmap( 79),   lmgly),(lmap( 80),   lmtbe),(lmap( 81),    lmvk),
     &   (lmap( 82),     lna),(lmap( 83),    lnh3),(lmap( 84),    lntr),
     &   (lmap( 85),   lnxoy),(lmap( 86),    lole),(lmap( 87),   lole1),
     &   (lmap( 88),   lole2),(lmap( 89),   lopen),(lmap( 90),   lpacd),
     &   (lmap( 91),    lpar),(lmap( 92),    lpcl),(lmap( 93),    lpec),
     &   (lmap( 94),   lph2o),(lmap( 95),   lphen),(lmap( 96),    lpna),
     &   (lmap( 97),   lpnh4),(lmap( 98),   lpno3),(lmap( 99),    lpoa),
     &   (lmap(100),   lprod),(lmap(101),   lpso4),(lmap(102),   lrc2h),
     &   (lmap(103),   lrc3h),(lmap(104),   lrcho),(lmap(105),   lrooh),
     &   (lmap(106),    lso2),(lmap(107),   lsoa1),(lmap(108),   lsoa2),
     &   (lmap(109),   lsoa3),(lmap(110),   lsoa4),(lmap(111),   lsoa5),
     &   (lmap(112),   lsoa6),(lmap(113),   lsoa7),(lmap(114),   lsopa),
     &   (lmap(115),   lsopb),(lmap(116),    lsqt),(lmap(117),   lsulf),
     &   (lmap(118),   lterp),(lmap(119),    ltol),(lmap(120),   ltola),
     &   (lmap(121),    ltrp),(lmap(122),     lxn),(lmap(123),    lxyl),
     &   (lmap(124),   lxyla),(lmap(125), lsoa1_1),(lmap(126), lsoa1_2),
     &   (lmap(127), lsoa1_3),(lmap(128), lsoa1_4),(lmap(129), lsoa1_5),
     &   (lmap(130), lsoa1_6),(lmap(131), lsoa1_7),(lmap(132), lsoa1_8),
     &   (lmap(133), lsoa1_9),(lmap(134),lsoa1_10),(lmap(135), lsoa2_1),
     &   (lmap(136), lsoa2_2),(lmap(137), lsoa2_3),(lmap(138), lsoa2_4),
     &   (lmap(139), lsoa2_5),(lmap(140), lsoa2_6),(lmap(141), lsoa2_7),
     &   (lmap(142), lsoa2_8),(lmap(143), lsoa2_9),(lmap(144),lsoa2_10),
     &   (lmap(145), lsoa3_1),(lmap(146), lsoa3_2),(lmap(147), lsoa3_3),
     &   (lmap(148), lsoa3_4),(lmap(149), lsoa3_5),(lmap(150), lsoa3_6),
     &   (lmap(151), lsoa3_7),(lmap(152), lsoa3_8),(lmap(153), lsoa3_9),
     &   (lmap(154),lsoa3_10),(lmap(155), lsoa4_1),(lmap(156), lsoa4_2),
     &   (lmap(157), lsoa4_3),(lmap(158), lsoa4_4),(lmap(159), lsoa4_5),
     &   (lmap(160), lsoa4_6),(lmap(161), lsoa4_7),(lmap(162), lsoa4_8),
     &   (lmap(163), lsoa4_9),(lmap(164),lsoa4_10),(lmap(165), lsoa5_1),
     &   (lmap(166), lsoa5_2),(lmap(167), lsoa5_3),(lmap(168), lsoa5_4),
     &   (lmap(169), lsoa5_5),(lmap(170), lsoa5_6),(lmap(171), lsoa5_7),
     &   (lmap(172), lsoa5_8),(lmap(173), lsoa5_9),(lmap(174),lsoa5_10),
     &   (lmap(175), lsoa6_1),(lmap(176), lsoa6_2),(lmap(177), lsoa6_3),
     &   (lmap(178), lsoa6_4),(lmap(179), lsoa6_5),(lmap(180), lsoa6_6),
     &   (lmap(181), lsoa6_7),(lmap(182), lsoa6_8),(lmap(183), lsoa6_9),
     &   (lmap(184),lsoa6_10),(lmap(185), lsoa7_1),(lmap(186), lsoa7_2),
     &   (lmap(187), lsoa7_3),(lmap(188), lsoa7_4),(lmap(189), lsoa7_5),
     &   (lmap(190), lsoa7_6),(lmap(191), lsoa7_7),(lmap(192), lsoa7_8),
     &   (lmap(193), lsoa7_9),(lmap(194),lsoa7_10),(lmap(195), lsopa_1),
     &   (lmap(196), lsopa_2),(lmap(197), lsopa_3),(lmap(198), lsopa_4),
     &   (lmap(199), lsopa_5),(lmap(200), lsopa_6),(lmap(201), lsopa_7),
     &   (lmap(202), lsopa_8),(lmap(203), lsopa_9),(lmap(204),lsopa_10),
     &   (lmap(205), lsopb_1),(lmap(206), lsopb_2),(lmap(207), lsopb_3),
     &   (lmap(208), lsopb_4),(lmap(209), lsopb_5),(lmap(210), lsopb_6),
     &   (lmap(211), lsopb_7),(lmap(212), lsopb_8),(lmap(213), lsopb_9),
     &   (lmap(214),lsopb_10),(lmap(215),  lpoa_1),(lmap(216),  lpoa_2),
     &   (lmap(217),  lpoa_3),(lmap(218),  lpoa_4),(lmap(219),  lpoa_5),
     &   (lmap(220),  lpoa_6),(lmap(221),  lpoa_7),(lmap(222),  lpoa_8),
     &   (lmap(223),  lpoa_9),(lmap(224), lpoa_10),(lmap(225),  lpec_1),
     &   (lmap(226),  lpec_2),(lmap(227),  lpec_3),(lmap(228),  lpec_4),
     &   (lmap(229),  lpec_5),(lmap(230),  lpec_6),(lmap(231),  lpec_7),
     &   (lmap(232),  lpec_8),(lmap(233),  lpec_9),(lmap(234), lpec_10),
     &   (lmap(235), lcrst_1),(lmap(236), lcrst_2),(lmap(237), lcrst_3),
     &   (lmap(238), lcrst_4),(lmap(239), lcrst_5),(lmap(240), lcrst_6),
     &   (lmap(241), lcrst_7),(lmap(242), lcrst_8),(lmap(243), lcrst_9),
     &   (lmap(244),lcrst_10),(lmap(245), lph2o_1),(lmap(246), lph2o_2),
     &   (lmap(247), lph2o_3),(lmap(248), lph2o_4),(lmap(249), lph2o_5),
     &   (lmap(250), lph2o_6),(lmap(251), lph2o_7),(lmap(252), lph2o_8),
     &   (lmap(253), lph2o_9),(lmap(254),lph2o_10),(lmap(255),  lpcl_1),
     &   (lmap(256),  lpcl_2),(lmap(257),  lpcl_3),(lmap(258),  lpcl_4),
     &   (lmap(259),  lpcl_5),(lmap(260),  lpcl_6),(lmap(261),  lpcl_7),
     &   (lmap(262),  lpcl_8),(lmap(263),  lpcl_9),(lmap(264), lpcl_10),
     &   (lmap(265),   lna_1),(lmap(266),   lna_2),(lmap(267),   lna_3),
     &   (lmap(268),   lna_4),(lmap(269),   lna_5),(lmap(270),   lna_6),
     &   (lmap(271),   lna_7),(lmap(272),   lna_8),(lmap(273),   lna_9),
     &   (lmap(274),  lna_10),(lmap(275), lpnh4_1),(lmap(276), lpnh4_2),
     &   (lmap(277), lpnh4_3),(lmap(278), lpnh4_4),(lmap(279), lpnh4_5),
     &   (lmap(280), lpnh4_6),(lmap(281), lpnh4_7),(lmap(282), lpnh4_8),
     &   (lmap(283), lpnh4_9),(lmap(284),lpnh4_10),(lmap(285), lpno3_1),
     &   (lmap(286), lpno3_2),(lmap(287), lpno3_3),(lmap(288), lpno3_4),
     &   (lmap(289), lpno3_5),(lmap(290), lpno3_6),(lmap(291), lpno3_7),
     &   (lmap(292), lpno3_8),(lmap(293), lpno3_9),(lmap(294),lpno3_10),
     &   (lmap(295), lpso4_1),(lmap(296), lpso4_2),(lmap(297), lpso4_3),
     &   (lmap(298), lpso4_4),(lmap(299), lpso4_5),(lmap(300), lpso4_6),
     &   (lmap(301), lpso4_7),(lmap(302), lpso4_8),(lmap(303), lpso4_9),
     &   (lmap(304),lpso4_10)
c
      integer   lo1d  ,lo    ,lclo
      integer   lcl   ,ln2o5 ,lno3 
      integer   loh   ,lho2  ,lc2o3
      integer   lxo2  ,lxo2n ,lcxo3
      integer   lmeo2 ,lto2  ,lror
      integer   lcro  ,lro2r ,lr2o2
      integer   lro2n ,lcco3 ,lrco3
      integer   lmco3 ,lbzco ,lcxo2
      integer   lhco3 ,ltbuo ,lbzo
      integer   lbzno
c     
      equivalence (lrad(1), lo1d ), (lrad(2), lo   ), (lrad(3), lclo ),
     &            (lrad(4), lcl  ), (lrad(5), ln2o5), (lrad(6), lno3 ),
     &            (lrad(7), loh  ), (lrad(8), lho2 ), (lrad(9), lc2o3),
     &            (lrad(10),lxo2 ), (lrad(11),lxo2n), (lrad(12),lcxo3),
     &            (lrad(13),lmeo2), (lrad(14),lto2 ), (lrad(15),lror ),
     &            (lrad(16),lcro ), (lrad(17),lro2r), (lrad(18),lr2o2),
     &            (lrad(19),lro2n), (lrad(20),lcco3), (lrad(21),lrco3),
     &            (lrad(22),lmco3), (lrad(23),lbzco), (lrad(24),lcxo2),
     &            (lrad(25),lhco3), (lrad(26),ltbuo), (lrad(27),lbzo ),
     &            (lrad(28),lbzno)

c-----CAMx v4.42 070603
c
c     DDMCHM.COM sets species pointers for the DDM chemistry
c     These equivalences must be consistent with the internal
c     species lists defined in data statements in READCHM
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c               
c 
      integer lmap(NSPNAM)
      integer lrad(NRADNM)
      common /lname/ lmap, lrad
c
      integer        lno,    lno2,     lo3
      integer       lpan,   lcres,   lpan2
      integer      lmpan,   lpbzn,   lnphe
      integer      lrno3,   ldcb2,   ldcb3
      integer      lhno4,   lacet,   lald2
      integer      lalk1,   lalk2,   lalk3
      integer      lalk4,   lalk5,   laro1
      integer      laro2,   lbacl,   lbald
      integer      lbcl1,   lbcl2,   lbuta
      integer      lccho,   lccrs,    lcg1
      integer       lcg2,    lcg3,    lcg4
      integer       lcg5,    lcl2,     lco
      integer      lco2h,   lco3h,   lcooh
      integer      lcprm,   ldcb1,    leth
      integer      lethe,   letoh,   lfcrs
      integer      lfmcl,   lform,   lfprm
      integer       lgly,   lh2o2,   lhc2h
      integer      lhcho,    lhcl,    lhg0
      integer       lhg2,    lhgp,   lhno3
      integer      lho2h,   lhocl,   lhono
      integer      licl1,   licl2,   lisop
      integer      lispd,   lmbut,    lmek
      integer      lmeoh,   lmeth,   lmgly
      integer      lmtbe,    lmvk,     lna
      integer       lnh3,    lntr,   lnxoy
      integer       lole,   lole1,   lole2
      integer      lopen,    lpar,    lpcl
      integer       lpec,   lph2o,   lphen
      integer       lpna,   lpnh4,   lpno3
      integer       lpoa,   lprod,   lpso4
      integer      lrc2h,   lrc3h,   lrcho
      integer      lrooh,    lso2,   lsoa1
      integer      lsoa2,   lsoa3,   lsoa4
      integer      lsoa5,   lsulf,   lterp
      integer       ltol,     lxn,    lxyl
      integer    lsoa1_1, lsoa1_2, lsoa1_3
      integer    lsoa1_4, lsoa1_5, lsoa1_6
      integer    lsoa1_7, lsoa1_8, lsoa1_9
      integer   lsoa1_10, lsoa2_1, lsoa2_2
      integer    lsoa2_3, lsoa2_4, lsoa2_5
      integer    lsoa2_6, lsoa2_7, lsoa2_8
      integer    lsoa2_9,lsoa2_10, lsoa3_1
      integer    lsoa3_2, lsoa3_3, lsoa3_4
      integer    lsoa3_5, lsoa3_6, lsoa3_7
      integer    lsoa3_8, lsoa3_9,lsoa3_10
      integer    lsoa4_1, lsoa4_2, lsoa4_3
      integer    lsoa4_4, lsoa4_5, lsoa4_6
      integer    lsoa4_7, lsoa4_8, lsoa4_9
      integer   lsoa4_10, lsoa5_1, lsoa5_2
      integer    lsoa5_3, lsoa5_4, lsoa5_5
      integer    lsoa5_6, lsoa5_7, lsoa5_8
      integer    lsoa5_9,lsoa5_10,  lpoa_1
      integer     lpoa_2,  lpoa_3,  lpoa_4
      integer     lpoa_5,  lpoa_6,  lpoa_7
      integer     lpoa_8,  lpoa_9, lpoa_10
      integer     lpec_1,  lpec_2,  lpec_3
      integer     lpec_4,  lpec_5,  lpec_6
      integer     lpec_7,  lpec_8,  lpec_9
      integer    lpec_10, lcrst_1, lcrst_2
      integer    lcrst_3, lcrst_4, lcrst_5
      integer    lcrst_6, lcrst_7, lcrst_8
      integer    lcrst_9,lcrst_10, lph2o_1
      integer    lph2o_2, lph2o_3, lph2o_4
      integer    lph2o_5, lph2o_6, lph2o_7
      integer    lph2o_8, lph2o_9,lph2o_10
      integer     lpcl_1,  lpcl_2,  lpcl_3
      integer     lpcl_4,  lpcl_5,  lpcl_6
      integer     lpcl_7,  lpcl_8,  lpcl_9
      integer    lpcl_10,   lna_1,   lna_2
      integer      lna_3,   lna_4,   lna_5
      integer      lna_6,   lna_7,   lna_8
      integer      lna_9,  lna_10, lpnh4_1
      integer    lpnh4_2, lpnh4_3, lpnh4_4
      integer    lpnh4_5, lpnh4_6, lpnh4_7
      integer    lpnh4_8, lpnh4_9,lpnh4_10
      integer    lpno3_1, lpno3_2, lpno3_3
      integer    lpno3_4, lpno3_5, lpno3_6
      integer    lpno3_7, lpno3_8, lpno3_9
      integer   lpno3_10, lpso4_1, lpso4_2
      integer    lpso4_3, lpso4_4, lpso4_5
      integer    lpso4_6, lpso4_7, lpso4_8
      integer    lpso4_9,lpso4_10
c
      equivalence
     &   (lmap(  1),     lno),(lmap(  2),    lno2),(lmap(  3),     lo3),
     &   (lmap(  4),    lpan),(lmap(  5),   lcres),(lmap(  6),   lpan2),
     &   (lmap(  7),   lmpan),(lmap(  8),   lpbzn),(lmap(  9),   lnphe),
     &   (lmap( 10),   lrno3),(lmap( 11),   ldcb2),(lmap( 12),   ldcb3),
     &   (lmap( 13),   lhno4),(lmap( 14),   lacet),(lmap( 15),   lald2),
     &   (lmap( 16),   lalk1),(lmap( 17),   lalk2),(lmap( 18),   lalk3),
     &   (lmap( 19),   lalk4),(lmap( 20),   lalk5),(lmap( 21),   laro1),
     &   (lmap( 22),   laro2),(lmap( 23),   lbacl),(lmap( 24),   lbald),
     &   (lmap( 25),   lbcl1),(lmap( 26),   lbcl2),(lmap( 27),   lbuta),
     &   (lmap( 28),   lccho),(lmap( 29),   lccrs),(lmap( 30),    lcg1),
     &   (lmap( 31),    lcg2),(lmap( 32),    lcg3),(lmap( 33),    lcg4),
     &   (lmap( 34),    lcg5),(lmap( 35),    lcl2),(lmap( 36),     lco),
     &   (lmap( 37),   lco2h),(lmap( 38),   lco3h),(lmap( 39),   lcooh),
     &   (lmap( 40),   lcprm),(lmap( 41),   ldcb1),(lmap( 42),    leth),
     &   (lmap( 43),   lethe),(lmap( 44),   letoh),(lmap( 45),   lfcrs),
     &   (lmap( 46),   lfmcl),(lmap( 47),   lform),(lmap( 48),   lfprm),
     &   (lmap( 49),    lgly),(lmap( 50),   lh2o2),(lmap( 51),   lhc2h),
     &   (lmap( 52),   lhcho),(lmap( 53),    lhcl),(lmap( 54),    lhg0),
     &   (lmap( 55),    lhg2),(lmap( 56),    lhgp),(lmap( 57),   lhno3),
     &   (lmap( 58),   lho2h),(lmap( 59),   lhocl),(lmap( 60),   lhono),
     &   (lmap( 61),   licl1),(lmap( 62),   licl2),(lmap( 63),   lisop),
     &   (lmap( 64),   lispd),(lmap( 65),   lmbut),(lmap( 66),    lmek),
     &   (lmap( 67),   lmeoh),(lmap( 68),   lmeth),(lmap( 69),   lmgly),
     &   (lmap( 70),   lmtbe),(lmap( 71),    lmvk),(lmap( 72),     lna),
     &   (lmap( 73),    lnh3),(lmap( 74),    lntr),(lmap( 75),   lnxoy),
     &   (lmap( 76),    lole),(lmap( 77),   lole1),(lmap( 78),   lole2),
     &   (lmap( 79),   lopen),(lmap( 80),    lpar),(lmap( 81),    lpcl),
     &   (lmap( 82),    lpec),(lmap( 83),   lph2o),(lmap( 84),   lphen),
     &   (lmap( 85),    lpna),(lmap( 86),   lpnh4),(lmap( 87),   lpno3),
     &   (lmap( 88),    lpoa),(lmap( 89),   lprod),(lmap( 90),   lpso4),
     &   (lmap( 91),   lrc2h),(lmap( 92),   lrc3h),(lmap( 93),   lrcho),
     &   (lmap( 94),   lrooh),(lmap( 95),    lso2),(lmap( 96),   lsoa1),
     &   (lmap( 97),   lsoa2),(lmap( 98),   lsoa3),(lmap( 99),   lsoa4),
     &   (lmap(100),   lsoa5),(lmap(101),   lsulf),(lmap(102),   lterp),
     &   (lmap(103),    ltol),(lmap(104),     lxn),(lmap(105),    lxyl),
     &   (lmap(106), lsoa1_1),(lmap(107), lsoa1_2),(lmap(108), lsoa1_3),
     &   (lmap(109), lsoa1_4),(lmap(110), lsoa1_5),(lmap(111), lsoa1_6),
     &   (lmap(112), lsoa1_7),(lmap(113), lsoa1_8),(lmap(114), lsoa1_9),
     &   (lmap(115),lsoa1_10),(lmap(116), lsoa2_1),(lmap(117), lsoa2_2),
     &   (lmap(118), lsoa2_3),(lmap(119), lsoa2_4),(lmap(120), lsoa2_5),
     &   (lmap(121), lsoa2_6),(lmap(122), lsoa2_7),(lmap(123), lsoa2_8),
     &   (lmap(124), lsoa2_9),(lmap(125),lsoa2_10),(lmap(126), lsoa3_1),
     &   (lmap(127), lsoa3_2),(lmap(128), lsoa3_3),(lmap(129), lsoa3_4),
     &   (lmap(130), lsoa3_5),(lmap(131), lsoa3_6),(lmap(132), lsoa3_7),
     &   (lmap(133), lsoa3_8),(lmap(134), lsoa3_9),(lmap(135),lsoa3_10),
     &   (lmap(136), lsoa4_1),(lmap(137), lsoa4_2),(lmap(138), lsoa4_3),
     &   (lmap(139), lsoa4_4),(lmap(140), lsoa4_5),(lmap(141), lsoa4_6),
     &   (lmap(142), lsoa4_7),(lmap(143), lsoa4_8),(lmap(144), lsoa4_9),
     &   (lmap(145),lsoa4_10),(lmap(146), lsoa5_1),(lmap(147), lsoa5_2),
     &   (lmap(148), lsoa5_3),(lmap(149), lsoa5_4),(lmap(150), lsoa5_5),
     &   (lmap(151), lsoa5_6),(lmap(152), lsoa5_7),(lmap(153), lsoa5_8),
     &   (lmap(154), lsoa5_9),(lmap(155),lsoa5_10),(lmap(156),  lpoa_1),
     &   (lmap(157),  lpoa_2),(lmap(158),  lpoa_3),(lmap(159),  lpoa_4),
     &   (lmap(160),  lpoa_5),(lmap(161),  lpoa_6),(lmap(162),  lpoa_7),
     &   (lmap(163),  lpoa_8),(lmap(164),  lpoa_9),(lmap(165), lpoa_10),
     &   (lmap(166),  lpec_1),(lmap(167),  lpec_2),(lmap(168),  lpec_3),
     &   (lmap(169),  lpec_4),(lmap(170),  lpec_5),(lmap(171),  lpec_6),
     &   (lmap(172),  lpec_7),(lmap(173),  lpec_8),(lmap(174),  lpec_9),
     &   (lmap(175), lpec_10),(lmap(176), lcrst_1),(lmap(177), lcrst_2),
     &   (lmap(178), lcrst_3),(lmap(179), lcrst_4),(lmap(180), lcrst_5),
     &   (lmap(181), lcrst_6),(lmap(182), lcrst_7),(lmap(183), lcrst_8),
     &   (lmap(184), lcrst_9),(lmap(185),lcrst_10),(lmap(186), lph2o_1),
     &   (lmap(187), lph2o_2),(lmap(188), lph2o_3),(lmap(189), lph2o_4),
     &   (lmap(190), lph2o_5),(lmap(191), lph2o_6),(lmap(192), lph2o_7),
     &   (lmap(193), lph2o_8),(lmap(194), lph2o_9),(lmap(195),lph2o_10),
     &   (lmap(196),  lpcl_1),(lmap(197),  lpcl_2),(lmap(198),  lpcl_3),
     &   (lmap(199),  lpcl_4),(lmap(200),  lpcl_5),(lmap(201),  lpcl_6),
     &   (lmap(202),  lpcl_7),(lmap(203),  lpcl_8),(lmap(204),  lpcl_9),
     &   (lmap(205), lpcl_10),(lmap(206),   lna_1),(lmap(207),   lna_2),
     &   (lmap(208),   lna_3),(lmap(209),   lna_4),(lmap(210),   lna_5),
     &   (lmap(211),   lna_6),(lmap(212),   lna_7),(lmap(213),   lna_8),
     &   (lmap(214),   lna_9),(lmap(215),  lna_10),(lmap(216), lpnh4_1),
     &   (lmap(217), lpnh4_2),(lmap(218), lpnh4_3),(lmap(219), lpnh4_4),
     &   (lmap(220), lpnh4_5),(lmap(221), lpnh4_6),(lmap(222), lpnh4_7),
     &   (lmap(223), lpnh4_8),(lmap(224), lpnh4_9),(lmap(225),lpnh4_10),
     &   (lmap(226), lpno3_1),(lmap(227), lpno3_2),(lmap(228), lpno3_3),
     &   (lmap(229), lpno3_4),(lmap(230), lpno3_5),(lmap(231), lpno3_6),
     &   (lmap(232), lpno3_7),(lmap(233), lpno3_8),(lmap(234), lpno3_9),
     &   (lmap(235),lpno3_10),(lmap(236), lpso4_1),(lmap(237), lpso4_2),
     &   (lmap(238), lpso4_3),(lmap(239), lpso4_4),(lmap(240), lpso4_5),
     &   (lmap(241), lpso4_6),(lmap(242), lpso4_7),(lmap(243), lpso4_8),
     &   (lmap(244), lpso4_9),(lmap(245),lpso4_10)
c
      integer   lo1d  ,lo    ,lclo 
      integer   lcl   ,ln2o5 ,lno3 
      integer   loh   ,lho2  ,lc2o3
      integer   lxo2  ,lxo2n ,lto2 
      integer   lror  ,lcro  ,lro2r
      integer   lr2o2 ,lro2n ,lcco3
      integer   lrco3 ,lmco3 ,lbzco
      integer   lcxo2 ,lhco3 ,ltbuo
      integer   lbzo  ,lbzno
c
      equivalence (lrad(1), lo1d ), (lrad(2), lo   ), (lrad(3), lclo ),
     &            (lrad(4), lcl  ), (lrad(5), ln2o5), (lrad(6), lno3 ),
     &            (lrad(7), loh  ), (lrad(8), lho2 ), (lrad(9), lc2o3),
     &            (lrad(10),lxo2 ), (lrad(11),lxo2n), (lrad(12),lto2 ),
     &            (lrad(13),lror ), (lrad(14),lcro ), (lrad(15),lro2r),
     &            (lrad(16),lr2o2), (lrad(17),lro2n), (lrad(18),lcco3),
     &            (lrad(19),lrco3), (lrad(20),lmco3), (lrad(21),lbzco),
     &            (lrad(22),lcxo2), (lrad(23),lhco3), (lrad(24),ltbuo),
     &            (lrad(25),lbzo ), (lrad(26),lbzno)

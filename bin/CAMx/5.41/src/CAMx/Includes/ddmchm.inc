c----CAMx v5.41 121109
c
c     DDMCHM.COM sets species pointers for the DDM chemistry
c     These equivalences must be consistent with the internal
c     species lists defined in data statements in READCHM
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c               
c 
      integer lmap(NSPNAM)
c
      common /lname/ lmap
c
      integer       lo1d,      lo,     lcl
      integer       lclo,     loh,    lho2
      integer      lc2o3,    lxo2,   lxo2n
      integer      lcxo3,   lmeo2,    lto2
      integer       lror,    lcro,   lro2r
      integer      lr2o2,   lro2n,   lcco3
      integer      lrco3,   lmco3,   lbzco
      integer      lcxo2,   lhco3,   ltbuo
      integer       lbzo,   lbzno,      li
      integer        lno,    lno2,     lo3
      integer       lpan,   lpanx,   lcres
      integer      lpan2,   lmpan,   lpbzn
      integer      lnphe,   lrno3,   ldcb2
      integer      ldcb3,   lhno4,   lbzo2
      integer      lcao2,   lcrn2,   lcrno
      integer      lepx2,   liso2,   lopan
      integer      lopo3,    lro2,   lxlo2
      integer      lxo2h,   laacd,   lacet
      integer      lald2,   laldx,   lalk1
      integer      lalk2,   lalk3,   lalk4
      integer      lalk5,   laro1,   laro2
      integer      lbacl,   lbald,   lbcl1
      integer      lbcl2,   lbenz,   lbuta
      integer     lc3h7i,   lcat1,   lccho
      integer      lccrs,    lcg1,    lcg2
      integer       lcg3,    lcg4,    lcg5
      integer       lcg6,    lcg7,  lch2i2
      integer    lch2ibr, lch2icl,   lch3i
      integer       lcl2,     lco,   lco2h
      integer      lco3h,   lcooh,   lcprm
      integer      lcron,   lcrpx,   ldcb1
      integer      lepox,    leth,   letha
      integer      lethe,   lethy,   letoh
      integer      lfacd,   lfcrs,   lfmcl
      integer      lform,   lfprm,    lgly
      integer      lglyd,   lh2o2,   lhc2h
      integer      lhcho,    lhcl,    lhg0
      integer       lhg2,    lhgp,     lhi
      integer      lhio3,   lhno3,   lho2h
      integer      lhocl,    lhoi,  lhoio2
      integer      lhono,   lhpld,     li2
      integer      li2o2,   lialk,   libac
      integer      libuo,   licl1,   licl2
      integer       lino,   lino2,   lino3
      integer      lintr,     lio,   liole
      integer     liono2,   lisop,    lisp
      integer      lispd,   lispx,   lixoy
      integer       lket,   lmbut,    lmek
      integer      lmeoh,   lmepx,   lmeth
      integer      lmgly,    lmib,    lmic
      integer       lmi2,   lmtbe,    lmvk
      integer      ln2o5,   ln3cl,     lna
      integer      lnbui,    lnh3,    lno3
      integer      lntcl,    lntr,    loio
      integer       lole,   lole1,   lole2
      integer      lopen,   lpacd,    lpar
      integer       lpcl,    lpec,   lph2o
      integer      lphen,    lpna,   lpnh4
      integer      lpno3,    lpoa,   lprod
      integer      lprpa,   lpso4,   lrc2h
      integer      lrc3h,   lrcho,   lrooh
      integer       lso2,   lsoa1,   lsoa2
      integer      lsoa3,   lsoa4,   lsoa5
      integer      lsoa6,   lsoa7,   lsopa
      integer      lsopb,    lsqt,     lss
      integer      lsulf,   lterp,    ltol
      integer      ltola,    ltrp,     lxn
      integer      lxopn,    lxyl,   lxyla
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
      integer    lsoa5_9,lsoa5_10, lsoa6_1
      integer    lsoa6_2, lsoa6_3, lsoa6_4
      integer    lsoa6_5, lsoa6_6, lsoa6_7
      integer    lsoa6_8, lsoa6_9,lsoa6_10
      integer    lsoa7_1, lsoa7_2, lsoa7_3
      integer    lsoa7_4, lsoa7_5, lsoa7_6
      integer    lsoa7_7, lsoa7_8, lsoa7_9
      integer   lsoa7_10, lsopa_1, lsopa_2
      integer    lsopa_3, lsopa_4, lsopa_5
      integer    lsopa_6, lsopa_7, lsopa_8
      integer    lsopa_9,lsopa_10, lsopb_1
      integer    lsopb_2, lsopb_3, lsopb_4
      integer    lsopb_5, lsopb_6, lsopb_7
      integer    lsopb_8, lsopb_9,lsopb_10
      integer     lpoa_1,  lpoa_2,  lpoa_3
      integer     lpoa_4,  lpoa_5,  lpoa_6
      integer     lpoa_7,  lpoa_8,  lpoa_9
      integer    lpoa_10,  lpec_1,  lpec_2
      integer     lpec_3,  lpec_4,  lpec_5
      integer     lpec_6,  lpec_7,  lpec_8
      integer     lpec_9, lpec_10, lcrst_1
      integer    lcrst_2, lcrst_3, lcrst_4
      integer    lcrst_5, lcrst_6, lcrst_7
      integer    lcrst_8, lcrst_9,lcrst_10
      integer    lph2o_1, lph2o_2, lph2o_3
      integer    lph2o_4, lph2o_5, lph2o_6
      integer    lph2o_7, lph2o_8, lph2o_9
      integer   lph2o_10,  lpcl_1,  lpcl_2
      integer     lpcl_3,  lpcl_4,  lpcl_5
      integer     lpcl_6,  lpcl_7,  lpcl_8
      integer     lpcl_9, lpcl_10,   lna_1
      integer      lna_2,   lna_3,   lna_4
      integer      lna_5,   lna_6,   lna_7
      integer      lna_8,   lna_9,  lna_10
      integer    lpnh4_1, lpnh4_2, lpnh4_3
      integer    lpnh4_4, lpnh4_5, lpnh4_6
      integer    lpnh4_7, lpnh4_8, lpnh4_9
      integer   lpnh4_10, lpno3_1, lpno3_2
      integer    lpno3_3, lpno3_4, lpno3_5
      integer    lpno3_6, lpno3_7, lpno3_8
      integer    lpno3_9,lpno3_10, lpso4_1
      integer    lpso4_2, lpso4_3, lpso4_4
      integer    lpso4_5, lpso4_6, lpso4_7
      integer    lpso4_8, lpso4_9,lpso4_10
c
      equivalence
     &   (lmap(  1),    lo1d),(lmap(  2),      lo),(lmap(  3),     lcl),
     &   (lmap(  4),    lclo),(lmap(  5),     loh),(lmap(  6),    lho2),
     &   (lmap(  7),   lc2o3),(lmap(  8),    lxo2),(lmap(  9),   lxo2n),
     &   (lmap( 10),   lcxo3),(lmap( 11),   lmeo2),(lmap( 12),    lto2),
     &   (lmap( 13),    lror),(lmap( 14),    lcro),(lmap( 15),   lro2r),
     &   (lmap( 16),   lr2o2),(lmap( 17),   lro2n),(lmap( 18),   lcco3),
     &   (lmap( 19),   lrco3),(lmap( 20),   lmco3),(lmap( 21),   lbzco),
     &   (lmap( 22),   lcxo2),(lmap( 23),   lhco3),(lmap( 24),   ltbuo),
     &   (lmap( 25),    lbzo),(lmap( 26),   lbzno),(lmap( 27),      li),
     &   (lmap( 28),     lno),(lmap( 29),    lno2),(lmap( 30),     lo3),
     &   (lmap( 31),    lpan),(lmap( 32),   lpanx),(lmap( 33),   lcres),
     &   (lmap( 34),   lpan2),(lmap( 35),   lmpan),(lmap( 36),   lpbzn),
     &   (lmap( 37),   lnphe),(lmap( 38),   lrno3),(lmap( 39),   ldcb2),
     &   (lmap( 40),   ldcb3),(lmap( 41),   lhno4),(lmap( 42),   lbzo2),
     &   (lmap( 43),   lcao2),(lmap( 44),   lcrn2),(lmap( 45),   lcrno),
     &   (lmap( 46),   lepx2),(lmap( 47),   liso2),(lmap( 48),   lopan),
     &   (lmap( 49),   lopo3),(lmap( 50),    lro2),(lmap( 51),   lxlo2),
     &   (lmap( 52),   lxo2h),(lmap( 53),   laacd),(lmap( 54),   lacet),
     &   (lmap( 55),   lald2),(lmap( 56),   laldx),(lmap( 57),   lalk1),
     &   (lmap( 58),   lalk2),(lmap( 59),   lalk3),(lmap( 60),   lalk4),
     &   (lmap( 61),   lalk5),(lmap( 62),   laro1),(lmap( 63),   laro2),
     &   (lmap( 64),   lbacl),(lmap( 65),   lbald),(lmap( 66),   lbcl1),
     &   (lmap( 67),   lbcl2),(lmap( 68),   lbenz),(lmap( 69),   lbuta),
     &   (lmap( 70),  lc3h7i),(lmap( 71),   lcat1),(lmap( 72),   lccho),
     &   (lmap( 73),   lccrs),(lmap( 74),    lcg1),(lmap( 75),    lcg2),
     &   (lmap( 76),    lcg3),(lmap( 77),    lcg4),(lmap( 78),    lcg5),
     &   (lmap( 79),    lcg6),(lmap( 80),    lcg7),(lmap( 81),  lch2i2),
     &   (lmap( 82), lch2ibr),(lmap( 83), lch2icl),(lmap( 84),   lch3i),
     &   (lmap( 85),    lcl2),(lmap( 86),     lco),(lmap( 87),   lco2h),
     &   (lmap( 88),   lco3h),(lmap( 89),   lcooh),(lmap( 90),   lcprm),
     &   (lmap( 91),   lcron),(lmap( 92),   lcrpx),(lmap( 93),   ldcb1),
     &   (lmap( 94),   lepox),(lmap( 95),    leth),(lmap( 96),   letha),
     &   (lmap( 97),   lethe),(lmap( 98),   lethy),(lmap( 99),   letoh),
     &   (lmap(100),   lfacd),(lmap(101),   lfcrs),(lmap(102),   lfmcl),
     &   (lmap(103),   lform),(lmap(104),   lfprm),(lmap(105),    lgly),
     &   (lmap(106),   lglyd),(lmap(107),   lh2o2),(lmap(108),   lhc2h),
     &   (lmap(109),   lhcho),(lmap(110),    lhcl),(lmap(111),    lhg0),
     &   (lmap(112),    lhg2),(lmap(113),    lhgp),(lmap(114),     lhi),
     &   (lmap(115),   lhio3),(lmap(116),   lhno3),(lmap(117),   lho2h),
     &   (lmap(118),   lhocl),(lmap(119),    lhoi),(lmap(120),  lhoio2),
     &   (lmap(121),   lhono),(lmap(122),   lhpld),(lmap(123),     li2),
     &   (lmap(124),   li2o2),(lmap(125),   lialk),(lmap(126),   libac),
     &   (lmap(127),   libuo),(lmap(128),   licl1),(lmap(129),   licl2),
     &   (lmap(130),    lino),(lmap(131),   lino2),(lmap(132),   lino3),
     &   (lmap(133),   lintr),(lmap(134),     lio),(lmap(135),   liole),
     &   (lmap(136),  liono2),(lmap(137),   lisop),(lmap(138),    lisp),
     &   (lmap(139),   lispd),(lmap(140),   lispx),(lmap(141),   lixoy),
     &   (lmap(142),    lket),(lmap(143),   lmbut),(lmap(144),    lmek),
     &   (lmap(145),   lmeoh),(lmap(146),   lmepx),(lmap(147),   lmeth),
     &   (lmap(148),   lmgly),(lmap(149),    lmib),(lmap(150),    lmic),
     &   (lmap(151),    lmi2),(lmap(152),   lmtbe),(lmap(153),    lmvk),
     &   (lmap(154),   ln2o5),(lmap(155),   ln3cl),(lmap(156),     lna),
     &   (lmap(157),   lnbui),(lmap(158),    lnh3),(lmap(159),    lno3),
     &   (lmap(160),   lntcl),(lmap(161),    lntr),(lmap(162),    loio),
     &   (lmap(163),    lole),(lmap(164),   lole1),(lmap(165),   lole2),
     &   (lmap(166),   lopen),(lmap(167),   lpacd),(lmap(168),    lpar),
     &   (lmap(169),    lpcl),(lmap(170),    lpec),(lmap(171),   lph2o),
     &   (lmap(172),   lphen),(lmap(173),    lpna),(lmap(174),   lpnh4),
     &   (lmap(175),   lpno3),(lmap(176),    lpoa),(lmap(177),   lprod),
     &   (lmap(178),   lprpa),(lmap(179),   lpso4),(lmap(180),   lrc2h),
     &   (lmap(181),   lrc3h),(lmap(182),   lrcho),(lmap(183),   lrooh),
     &   (lmap(184),    lso2),(lmap(185),   lsoa1),(lmap(186),   lsoa2),
     &   (lmap(187),   lsoa3),(lmap(188),   lsoa4),(lmap(189),   lsoa5),
     &   (lmap(190),   lsoa6),(lmap(191),   lsoa7),(lmap(192),   lsopa),
     &   (lmap(193),   lsopb),(lmap(194),    lsqt),(lmap(195),     lss),
     &   (lmap(196),   lsulf),(lmap(197),   lterp),(lmap(198),    ltol),
     &   (lmap(199),   ltola),(lmap(200),    ltrp),(lmap(201),     lxn),
     &   (lmap(202),   lxopn),(lmap(203),    lxyl),(lmap(204),   lxyla),
     &   (lmap(205), lsoa1_1),(lmap(206), lsoa1_2),(lmap(207), lsoa1_3),
     &   (lmap(208), lsoa1_4),(lmap(209), lsoa1_5),(lmap(210), lsoa1_6),
     &   (lmap(211), lsoa1_7),(lmap(212), lsoa1_8),(lmap(213), lsoa1_9),
     &   (lmap(214),lsoa1_10),(lmap(215), lsoa2_1),(lmap(216), lsoa2_2),
     &   (lmap(217), lsoa2_3),(lmap(218), lsoa2_4),(lmap(219), lsoa2_5),
     &   (lmap(220), lsoa2_6),(lmap(221), lsoa2_7),(lmap(222), lsoa2_8),
     &   (lmap(223), lsoa2_9),(lmap(224),lsoa2_10),(lmap(225), lsoa3_1),
     &   (lmap(226), lsoa3_2),(lmap(227), lsoa3_3),(lmap(228), lsoa3_4),
     &   (lmap(229), lsoa3_5),(lmap(230), lsoa3_6),(lmap(231), lsoa3_7),
     &   (lmap(232), lsoa3_8),(lmap(233), lsoa3_9),(lmap(234),lsoa3_10),
     &   (lmap(235), lsoa4_1),(lmap(236), lsoa4_2),(lmap(237), lsoa4_3),
     &   (lmap(238), lsoa4_4),(lmap(239), lsoa4_5),(lmap(240), lsoa4_6),
     &   (lmap(241), lsoa4_7),(lmap(242), lsoa4_8),(lmap(243), lsoa4_9),
     &   (lmap(244),lsoa4_10),(lmap(245), lsoa5_1),(lmap(246), lsoa5_2),
     &   (lmap(247), lsoa5_3),(lmap(248), lsoa5_4),(lmap(249), lsoa5_5),
     &   (lmap(250), lsoa5_6),(lmap(251), lsoa5_7),(lmap(252), lsoa5_8),
     &   (lmap(253), lsoa5_9),(lmap(254),lsoa5_10),(lmap(255), lsoa6_1),
     &   (lmap(256), lsoa6_2),(lmap(257), lsoa6_3),(lmap(258), lsoa6_4),
     &   (lmap(259), lsoa6_5),(lmap(260), lsoa6_6),(lmap(261), lsoa6_7),
     &   (lmap(262), lsoa6_8),(lmap(263), lsoa6_9),(lmap(264),lsoa6_10),
     &   (lmap(265), lsoa7_1),(lmap(266), lsoa7_2),(lmap(267), lsoa7_3),
     &   (lmap(268), lsoa7_4),(lmap(269), lsoa7_5),(lmap(270), lsoa7_6)
      equivalence
     &   (lmap(271), lsoa7_7),(lmap(272), lsoa7_8),(lmap(273), lsoa7_9),
     &   (lmap(274),lsoa7_10),(lmap(275), lsopa_1),(lmap(276), lsopa_2),
     &   (lmap(277), lsopa_3),(lmap(278), lsopa_4),(lmap(279), lsopa_5),
     &   (lmap(280), lsopa_6),(lmap(281), lsopa_7),(lmap(282), lsopa_8),
     &   (lmap(283), lsopa_9),(lmap(284),lsopa_10),(lmap(285), lsopb_1),
     &   (lmap(286), lsopb_2),(lmap(287), lsopb_3),(lmap(288), lsopb_4),
     &   (lmap(289), lsopb_5),(lmap(290), lsopb_6),(lmap(291), lsopb_7),
     &   (lmap(292), lsopb_8),(lmap(293), lsopb_9),(lmap(294),lsopb_10),
     &   (lmap(295),  lpoa_1),(lmap(296),  lpoa_2),(lmap(297),  lpoa_3),
     &   (lmap(298),  lpoa_4),(lmap(299),  lpoa_5),(lmap(300),  lpoa_6),
     &   (lmap(301),  lpoa_7),(lmap(302),  lpoa_8),(lmap(303),  lpoa_9),
     &   (lmap(304), lpoa_10),(lmap(305),  lpec_1),(lmap(306),  lpec_2),
     &   (lmap(307),  lpec_3),(lmap(308),  lpec_4),(lmap(309),  lpec_5),
     &   (lmap(310),  lpec_6),(lmap(311),  lpec_7),(lmap(312),  lpec_8),
     &   (lmap(313),  lpec_9),(lmap(314), lpec_10),(lmap(315), lcrst_1),
     &   (lmap(316), lcrst_2),(lmap(317), lcrst_3),(lmap(318), lcrst_4),
     &   (lmap(319), lcrst_5),(lmap(320), lcrst_6),(lmap(321), lcrst_7),
     &   (lmap(322), lcrst_8),(lmap(323), lcrst_9),(lmap(324),lcrst_10),
     &   (lmap(325), lph2o_1),(lmap(326), lph2o_2),(lmap(327), lph2o_3),
     &   (lmap(328), lph2o_4),(lmap(329), lph2o_5),(lmap(330), lph2o_6),
     &   (lmap(331), lph2o_7),(lmap(332), lph2o_8),(lmap(333), lph2o_9),
     &   (lmap(334),lph2o_10),(lmap(335),  lpcl_1),(lmap(336),  lpcl_2),
     &   (lmap(337),  lpcl_3),(lmap(338),  lpcl_4),(lmap(339),  lpcl_5),
     &   (lmap(340),  lpcl_6),(lmap(341),  lpcl_7),(lmap(342),  lpcl_8),
     &   (lmap(343),  lpcl_9),(lmap(344), lpcl_10),(lmap(345),   lna_1),
     &   (lmap(346),   lna_2),(lmap(347),   lna_3),(lmap(348),   lna_4),
     &   (lmap(349),   lna_5),(lmap(350),   lna_6),(lmap(351),   lna_7),
     &   (lmap(352),   lna_8),(lmap(353),   lna_9),(lmap(354),  lna_10),
     &   (lmap(355), lpnh4_1),(lmap(356), lpnh4_2),(lmap(357), lpnh4_3),
     &   (lmap(358), lpnh4_4),(lmap(359), lpnh4_5),(lmap(360), lpnh4_6),
     &   (lmap(361), lpnh4_7),(lmap(362), lpnh4_8),(lmap(363), lpnh4_9),
     &   (lmap(364),lpnh4_10),(lmap(365), lpno3_1),(lmap(366), lpno3_2),
     &   (lmap(367), lpno3_3),(lmap(368), lpno3_4),(lmap(369), lpno3_5),
     &   (lmap(370), lpno3_6),(lmap(371), lpno3_7),(lmap(372), lpno3_8),
     &   (lmap(373), lpno3_9),(lmap(374),lpno3_10),(lmap(375), lpso4_1),
     &   (lmap(376), lpso4_2),(lmap(377), lpso4_3),(lmap(378), lpso4_4),
     &   (lmap(379), lpso4_5),(lmap(380), lpso4_6),(lmap(381), lpso4_7),
     &   (lmap(382), lpso4_8),(lmap(383), lpso4_9),(lmap(384),lpso4_10)

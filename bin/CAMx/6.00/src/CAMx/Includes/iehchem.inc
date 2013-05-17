c----CAMx v6.00 130506
c
c     IEHCHEM.COM sets species pointers for the IEH chemistry solver
c     These equivalences must be consistent with the internal   
c     species lists defined in data statements in READCHM
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c 
      integer imap(NSPNAM)
      common /iname/ imap
c
      integer       io1d,      io,     icl
      integer       iclo,     ioh,    iho2
      integer      ic2o3,    ixo2,   ixo2n
      integer      icxo3,   imeo2,    ito2
      integer       iror,    icro,   iro2r
      integer      ir2o2,   iro2n,   icco3
      integer      irco3,   imco3,   ibzco
      integer      icxo2,   ihco3,   itbuo
      integer       ibzo,   ibzno,      ii
      integer        ino,    ino2,     io3
      integer       ipan,   ipanx,   icres
      integer      ipan2,   impan,   ipbzn
      integer      inphe,   irno3,   idcb2
      integer      idcb3,   ihno4,   ibzo2
      integer      icao2,   icrn2,   icrno
      integer      iepx2,   iiso2,   iopan
      integer      iopo3,    iro2,   ixlo2
      integer      ixo2h,   iaacd,   iacet
      integer      iald2,   ialdx,   ialk1
      integer      ialk2,   ialk3,   ialk4
      integer      ialk5,   iaro1,   iaro2
      integer      ibacl,   ibald,   ibcl1
      integer      ibcl2,   ibenz,   ibuta
      integer     ic3h7i,   icat1,   iccho
      integer      iccrs,    icg1,    icg2
      integer       icg3,    icg4,    icg5
      integer       icg6,    icg7,  ich2i2
      integer    ich2ibr, ich2icl,   ich3i
      integer       icl2,     ico,   ico2h
      integer      ico3h,   icooh,   icprm
      integer      icron,   icrpx,   idcb1
      integer      iepox,    ieth,   ietha
      integer      iethe,   iethy,   ietoh
      integer      ifacd,   ifcrs,   ifmcl
      integer      iform,   ifprm,    igly
      integer      iglyd,   ih2o2,   ihc2h
      integer      ihcho,    ihcl,    ihg0
      integer       ihg2,    ihgp,     ihi
      integer      ihio3,   ihno3,   iho2h
      integer      ihocl,    ihoi,  ihoio2
      integer      ihono,   ihpld,     ii2
      integer      ii2o2,   iialk,   iibac
      integer      iibuo,   iicl1,   iicl2
      integer       iino,   iino2,   iino3
      integer      iintr,     iio,   iiole
      integer     iiono2,   iisop,    iisp
      integer      iispd,   iispx,   iixoy
      integer       iket,   imbut,    imek
      integer      imeoh,   imepx,   imeth
      integer      imgly,    imib,    imic
      integer       imi2,   imtbe,    imvk
      integer      in2o5,   in3cl,     ina
      integer      inbui,    inh3,    ino3
      integer      intcl,    intr,    ioio
      integer       iole,   iole1,   iole2
      integer      iopen,   ipacd,    ipar
      integer       ipcl,    ipec,   iph2o
      integer      iphen,    ipna,   ipnh4
      integer      ipno3,    ipoa,   iprod
      integer      iprpa,   ipso4,   irc2h
      integer      irc3h,   ircho,   irooh
      integer       iso2,   isoa1,   isoa2
      integer      isoa3,   isoa4,   isoa5
      integer      isoa6,   isoa7,   isopa
      integer      isopb,    isqt,     iss
      integer      isulf,   iterp,    itol
      integer      itola,    itrp,     ixn
      integer      ixopn,    ixyl,   ixyla
      integer    isoa1_1, isoa1_2, isoa1_3
      integer    isoa1_4, isoa1_5, isoa1_6
      integer    isoa1_7, isoa1_8, isoa1_9
      integer   isoa1_10, isoa2_1, isoa2_2
      integer    isoa2_3, isoa2_4, isoa2_5
      integer    isoa2_6, isoa2_7, isoa2_8
      integer    isoa2_9,isoa2_10, isoa3_1
      integer    isoa3_2, isoa3_3, isoa3_4
      integer    isoa3_5, isoa3_6, isoa3_7
      integer    isoa3_8, isoa3_9,isoa3_10
      integer    isoa4_1, isoa4_2, isoa4_3
      integer    isoa4_4, isoa4_5, isoa4_6
      integer    isoa4_7, isoa4_8, isoa4_9
      integer   isoa4_10, isoa5_1, isoa5_2
      integer    isoa5_3, isoa5_4, isoa5_5
      integer    isoa5_6, isoa5_7, isoa5_8
      integer    isoa5_9,isoa5_10, isoa6_1
      integer    isoa6_2, isoa6_3, isoa6_4
      integer    isoa6_5, isoa6_6, isoa6_7
      integer    isoa6_8, isoa6_9,isoa6_10
      integer    isoa7_1, isoa7_2, isoa7_3
      integer    isoa7_4, isoa7_5, isoa7_6
      integer    isoa7_7, isoa7_8, isoa7_9
      integer   isoa7_10, isopa_1, isopa_2
      integer    isopa_3, isopa_4, isopa_5
      integer    isopa_6, isopa_7, isopa_8
      integer    isopa_9,isopa_10, isopb_1
      integer    isopb_2, isopb_3, isopb_4
      integer    isopb_5, isopb_6, isopb_7
      integer    isopb_8, isopb_9,isopb_10
      integer     ipoa_1,  ipoa_2,  ipoa_3
      integer     ipoa_4,  ipoa_5,  ipoa_6
      integer     ipoa_7,  ipoa_8,  ipoa_9
      integer    ipoa_10,  ipec_1,  ipec_2
      integer     ipec_3,  ipec_4,  ipec_5
      integer     ipec_6,  ipec_7,  ipec_8
      integer     ipec_9, ipec_10, icrst_1
      integer    icrst_2, icrst_3, icrst_4
      integer    icrst_5, icrst_6, icrst_7
      integer    icrst_8, icrst_9,icrst_10
      integer    iph2o_1, iph2o_2, iph2o_3
      integer    iph2o_4, iph2o_5, iph2o_6
      integer    iph2o_7, iph2o_8, iph2o_9
      integer   iph2o_10,  ipcl_1,  ipcl_2
      integer     ipcl_3,  ipcl_4,  ipcl_5
      integer     ipcl_6,  ipcl_7,  ipcl_8
      integer     ipcl_9, ipcl_10,   ina_1
      integer      ina_2,   ina_3,   ina_4
      integer      ina_5,   ina_6,   ina_7
      integer      ina_8,   ina_9,  ina_10
      integer    ipnh4_1, ipnh4_2, ipnh4_3
      integer    ipnh4_4, ipnh4_5, ipnh4_6
      integer    ipnh4_7, ipnh4_8, ipnh4_9
      integer   ipnh4_10, ipno3_1, ipno3_2
      integer    ipno3_3, ipno3_4, ipno3_5
      integer    ipno3_6, ipno3_7, ipno3_8
      integer    ipno3_9,ipno3_10, ipso4_1
      integer    ipso4_2, ipso4_3, ipso4_4
      integer    ipso4_5, ipso4_6, ipso4_7
      integer    ipso4_8, ipso4_9,ipso4_10
c
      equivalence
     &   (imap(  1),    io1d),(imap(  2),      io),(imap(  3),     icl),
     &   (imap(  4),    iclo),(imap(  5),     ioh),(imap(  6),    iho2),
     &   (imap(  7),   ic2o3),(imap(  8),    ixo2),(imap(  9),   ixo2n),
     &   (imap( 10),   icxo3),(imap( 11),   imeo2),(imap( 12),    ito2),
     &   (imap( 13),    iror),(imap( 14),    icro),(imap( 15),   iro2r),
     &   (imap( 16),   ir2o2),(imap( 17),   iro2n),(imap( 18),   icco3),
     &   (imap( 19),   irco3),(imap( 20),   imco3),(imap( 21),   ibzco),
     &   (imap( 22),   icxo2),(imap( 23),   ihco3),(imap( 24),   itbuo),
     &   (imap( 25),    ibzo),(imap( 26),   ibzno),(imap( 27),      ii),
     &   (imap( 28),     ino),(imap( 29),    ino2),(imap( 30),     io3),
     &   (imap( 31),    ipan),(imap( 32),   ipanx),(imap( 33),   icres),
     &   (imap( 34),   ipan2),(imap( 35),   impan),(imap( 36),   ipbzn),
     &   (imap( 37),   inphe),(imap( 38),   irno3),(imap( 39),   idcb2),
     &   (imap( 40),   idcb3),(imap( 41),   ihno4),(imap( 42),   ibzo2),
     &   (imap( 43),   icao2),(imap( 44),   icrn2),(imap( 45),   icrno),
     &   (imap( 46),   iepx2),(imap( 47),   iiso2),(imap( 48),   iopan),
     &   (imap( 49),   iopo3),(imap( 50),    iro2),(imap( 51),   ixlo2),
     &   (imap( 52),   ixo2h),(imap( 53),   iaacd),(imap( 54),   iacet),
     &   (imap( 55),   iald2),(imap( 56),   ialdx),(imap( 57),   ialk1),
     &   (imap( 58),   ialk2),(imap( 59),   ialk3),(imap( 60),   ialk4),
     &   (imap( 61),   ialk5),(imap( 62),   iaro1),(imap( 63),   iaro2),
     &   (imap( 64),   ibacl),(imap( 65),   ibald),(imap( 66),   ibcl1),
     &   (imap( 67),   ibcl2),(imap( 68),   ibenz),(imap( 69),   ibuta),
     &   (imap( 70),  ic3h7i),(imap( 71),   icat1),(imap( 72),   iccho),
     &   (imap( 73),   iccrs),(imap( 74),    icg1),(imap( 75),    icg2),
     &   (imap( 76),    icg3),(imap( 77),    icg4),(imap( 78),    icg5),
     &   (imap( 79),    icg6),(imap( 80),    icg7),(imap( 81),  ich2i2),
     &   (imap( 82), ich2ibr),(imap( 83), ich2icl),(imap( 84),   ich3i),
     &   (imap( 85),    icl2),(imap( 86),     ico),(imap( 87),   ico2h),
     &   (imap( 88),   ico3h),(imap( 89),   icooh),(imap( 90),   icprm),
     &   (imap( 91),   icron),(imap( 92),   icrpx),(imap( 93),   idcb1),
     &   (imap( 94),   iepox),(imap( 95),    ieth),(imap( 96),   ietha),
     &   (imap( 97),   iethe),(imap( 98),   iethy),(imap( 99),   ietoh),
     &   (imap(100),   ifacd),(imap(101),   ifcrs),(imap(102),   ifmcl),
     &   (imap(103),   iform),(imap(104),   ifprm),(imap(105),    igly),
     &   (imap(106),   iglyd),(imap(107),   ih2o2),(imap(108),   ihc2h),
     &   (imap(109),   ihcho),(imap(110),    ihcl),(imap(111),    ihg0),
     &   (imap(112),    ihg2),(imap(113),    ihgp),(imap(114),     ihi),
     &   (imap(115),   ihio3),(imap(116),   ihno3),(imap(117),   iho2h),
     &   (imap(118),   ihocl),(imap(119),    ihoi),(imap(120),  ihoio2),
     &   (imap(121),   ihono),(imap(122),   ihpld),(imap(123),     ii2),
     &   (imap(124),   ii2o2),(imap(125),   iialk),(imap(126),   iibac),
     &   (imap(127),   iibuo),(imap(128),   iicl1),(imap(129),   iicl2),
     &   (imap(130),    iino),(imap(131),   iino2),(imap(132),   iino3),
     &   (imap(133),   iintr),(imap(134),     iio),(imap(135),   iiole),
     &   (imap(136),  iiono2),(imap(137),   iisop),(imap(138),    iisp),
     &   (imap(139),   iispd),(imap(140),   iispx),(imap(141),   iixoy),
     &   (imap(142),    iket),(imap(143),   imbut),(imap(144),    imek),
     &   (imap(145),   imeoh),(imap(146),   imepx),(imap(147),   imeth),
     &   (imap(148),   imgly),(imap(149),    imib),(imap(150),    imic),
     &   (imap(151),    imi2),(imap(152),   imtbe),(imap(153),    imvk),
     &   (imap(154),   in2o5),(imap(155),   in3cl),(imap(156),     ina),
     &   (imap(157),   inbui),(imap(158),    inh3),(imap(159),    ino3),
     &   (imap(160),   intcl),(imap(161),    intr),(imap(162),    ioio),
     &   (imap(163),    iole),(imap(164),   iole1),(imap(165),   iole2),
     &   (imap(166),   iopen),(imap(167),   ipacd),(imap(168),    ipar),
     &   (imap(169),    ipcl),(imap(170),    ipec),(imap(171),   iph2o),
     &   (imap(172),   iphen),(imap(173),    ipna),(imap(174),   ipnh4),
     &   (imap(175),   ipno3),(imap(176),    ipoa),(imap(177),   iprod),
     &   (imap(178),   iprpa),(imap(179),   ipso4),(imap(180),   irc2h),
     &   (imap(181),   irc3h),(imap(182),   ircho),(imap(183),   irooh),
     &   (imap(184),    iso2),(imap(185),   isoa1),(imap(186),   isoa2),
     &   (imap(187),   isoa3),(imap(188),   isoa4),(imap(189),   isoa5),
     &   (imap(190),   isoa6),(imap(191),   isoa7),(imap(192),   isopa),
     &   (imap(193),   isopb),(imap(194),    isqt),(imap(195),     iss),
     &   (imap(196),   isulf),(imap(197),   iterp),(imap(198),    itol),
     &   (imap(199),   itola),(imap(200),    itrp),(imap(201),     ixn),
     &   (imap(202),   ixopn),(imap(203),    ixyl),(imap(204),   ixyla),
     &   (imap(205), isoa1_1),(imap(206), isoa1_2),(imap(207), isoa1_3),
     &   (imap(208), isoa1_4),(imap(209), isoa1_5),(imap(210), isoa1_6),
     &   (imap(211), isoa1_7),(imap(212), isoa1_8),(imap(213), isoa1_9),
     &   (imap(214),isoa1_10),(imap(215), isoa2_1),(imap(216), isoa2_2),
     &   (imap(217), isoa2_3),(imap(218), isoa2_4),(imap(219), isoa2_5),
     &   (imap(220), isoa2_6),(imap(221), isoa2_7),(imap(222), isoa2_8),
     &   (imap(223), isoa2_9),(imap(224),isoa2_10),(imap(225), isoa3_1),
     &   (imap(226), isoa3_2),(imap(227), isoa3_3),(imap(228), isoa3_4),
     &   (imap(229), isoa3_5),(imap(230), isoa3_6),(imap(231), isoa3_7),
     &   (imap(232), isoa3_8),(imap(233), isoa3_9),(imap(234),isoa3_10),
     &   (imap(235), isoa4_1),(imap(236), isoa4_2),(imap(237), isoa4_3),
     &   (imap(238), isoa4_4),(imap(239), isoa4_5),(imap(240), isoa4_6),
     &   (imap(241), isoa4_7),(imap(242), isoa4_8),(imap(243), isoa4_9),
     &   (imap(244),isoa4_10),(imap(245), isoa5_1),(imap(246), isoa5_2),
     &   (imap(247), isoa5_3),(imap(248), isoa5_4),(imap(249), isoa5_5),
     &   (imap(250), isoa5_6),(imap(251), isoa5_7),(imap(252), isoa5_8),
     &   (imap(253), isoa5_9),(imap(254),isoa5_10),(imap(255), isoa6_1),
     &   (imap(256), isoa6_2),(imap(257), isoa6_3),(imap(258), isoa6_4),
     &   (imap(259), isoa6_5),(imap(260), isoa6_6),(imap(261), isoa6_7),
     &   (imap(262), isoa6_8),(imap(263), isoa6_9),(imap(264),isoa6_10),
     &   (imap(265), isoa7_1),(imap(266), isoa7_2),(imap(267), isoa7_3),
     &   (imap(268), isoa7_4),(imap(269), isoa7_5),(imap(270), isoa7_6)
      equivalence
     &   (imap(271), isoa7_7),(imap(272), isoa7_8),(imap(273), isoa7_9),
     &   (imap(274),isoa7_10),(imap(275), isopa_1),(imap(276), isopa_2),
     &   (imap(277), isopa_3),(imap(278), isopa_4),(imap(279), isopa_5),
     &   (imap(280), isopa_6),(imap(281), isopa_7),(imap(282), isopa_8),
     &   (imap(283), isopa_9),(imap(284),isopa_10),(imap(285), isopb_1),
     &   (imap(286), isopb_2),(imap(287), isopb_3),(imap(288), isopb_4),
     &   (imap(289), isopb_5),(imap(290), isopb_6),(imap(291), isopb_7),
     &   (imap(292), isopb_8),(imap(293), isopb_9),(imap(294),isopb_10),
     &   (imap(295),  ipoa_1),(imap(296),  ipoa_2),(imap(297),  ipoa_3),
     &   (imap(298),  ipoa_4),(imap(299),  ipoa_5),(imap(300),  ipoa_6),
     &   (imap(301),  ipoa_7),(imap(302),  ipoa_8),(imap(303),  ipoa_9),
     &   (imap(304), ipoa_10),(imap(305),  ipec_1),(imap(306),  ipec_2),
     &   (imap(307),  ipec_3),(imap(308),  ipec_4),(imap(309),  ipec_5),
     &   (imap(310),  ipec_6),(imap(311),  ipec_7),(imap(312),  ipec_8),
     &   (imap(313),  ipec_9),(imap(314), ipec_10),(imap(315), icrst_1),
     &   (imap(316), icrst_2),(imap(317), icrst_3),(imap(318), icrst_4),
     &   (imap(319), icrst_5),(imap(320), icrst_6),(imap(321), icrst_7),
     &   (imap(322), icrst_8),(imap(323), icrst_9),(imap(324),icrst_10),
     &   (imap(325), iph2o_1),(imap(326), iph2o_2),(imap(327), iph2o_3),
     &   (imap(328), iph2o_4),(imap(329), iph2o_5),(imap(330), iph2o_6),
     &   (imap(331), iph2o_7),(imap(332), iph2o_8),(imap(333), iph2o_9),
     &   (imap(334),iph2o_10),(imap(335),  ipcl_1),(imap(336),  ipcl_2),
     &   (imap(337),  ipcl_3),(imap(338),  ipcl_4),(imap(339),  ipcl_5),
     &   (imap(340),  ipcl_6),(imap(341),  ipcl_7),(imap(342),  ipcl_8),
     &   (imap(343),  ipcl_9),(imap(344), ipcl_10),(imap(345),   ina_1),
     &   (imap(346),   ina_2),(imap(347),   ina_3),(imap(348),   ina_4),
     &   (imap(349),   ina_5),(imap(350),   ina_6),(imap(351),   ina_7),
     &   (imap(352),   ina_8),(imap(353),   ina_9),(imap(354),  ina_10),
     &   (imap(355), ipnh4_1),(imap(356), ipnh4_2),(imap(357), ipnh4_3),
     &   (imap(358), ipnh4_4),(imap(359), ipnh4_5),(imap(360), ipnh4_6),
     &   (imap(361), ipnh4_7),(imap(362), ipnh4_8),(imap(363), ipnh4_9),
     &   (imap(364),ipnh4_10),(imap(365), ipno3_1),(imap(366), ipno3_2),
     &   (imap(367), ipno3_3),(imap(368), ipno3_4),(imap(369), ipno3_5),
     &   (imap(370), ipno3_6),(imap(371), ipno3_7),(imap(372), ipno3_8),
     &   (imap(373), ipno3_9),(imap(374),ipno3_10),(imap(375), ipso4_1),
     &   (imap(376), ipso4_2),(imap(377), ipso4_3),(imap(378), ipso4_4),
     &   (imap(379), ipso4_5),(imap(380), ipso4_6),(imap(381), ipso4_7),
     &   (imap(382), ipso4_8),(imap(383), ipso4_9),(imap(384),ipso4_10)

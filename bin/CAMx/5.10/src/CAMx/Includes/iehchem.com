c----CAMx v5.10 090918
c
c     IEHCHEM.COM sets species pointers for the IEH chemistry solver
c     These equivalences must be consistent with the internal   
c     species lists defined in data statements in READCHM
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c 
      integer imap(NSPNAM)
      integer irad(NRADNM)
      common /iname/ imap, irad
c
      integer        ino,    ino2,     io3
      integer       ipan,   ipanx,   icres
      integer      ipan2,   impan,   ipbzn
      integer      inphe,   irno3,   idcb2
      integer      idcb3,   ihno4,   iaacd
      integer      iacet,   iald2,   ialdx
      integer      ialk1,   ialk2,   ialk3
      integer      ialk4,   ialk5,   iaro1
      integer      iaro2,   ibacl,   ibald
      integer      ibcl1,   ibcl2,   ibuta
      integer      iccho,   iccrs,    icg1
      integer       icg2,    icg3,    icg4
      integer       icg5,    icg6,    icg7
      integer       icl2,     ico,   ico2h
      integer      ico3h,   icooh,   icprm
      integer      idcb1,    ieth,   ietha
      integer      iethe,   ietoh,   ifacd
      integer      ifcrs,   ifmcl,   iform
      integer      ifprm,    igly,   ih2o2
      integer      ihc2h,   ihcho,    ihcl
      integer       ihg0,    ihg2,    ihgp
      integer      ihno3,   iho2h,   ihocl
      integer      ihono,   iicl1,   iicl2
      integer      iiole,   iisop,    iisp
      integer      iispd,   imbut,    imek
      integer      imeoh,   imepx,   imeth
      integer      imgly,   imtbe,    imvk
      integer        ina,    inh3,    intr
      integer      inxoy,    iole,   iole1
      integer      iole2,   iopen,   ipacd
      integer       ipar,    ipcl,    ipec
      integer      iph2o,   iphen,    ipna
      integer      ipnh4,   ipno3,    ipoa
      integer      iprod,   ipso4,   irc2h
      integer      irc3h,   ircho,   irooh
      integer       iso2,   isoa1,   isoa2
      integer      isoa3,   isoa4,   isoa5
      integer      isoa6,   isoa7,   isopa
      integer      isopb,    isqt,   isulf
      integer      iterp,    itol,   itola
      integer       itrp,     ixn,    ixyl
      integer      ixyla, isoa1_1, isoa1_2
      integer    isoa1_3, isoa1_4, isoa1_5
      integer    isoa1_6, isoa1_7, isoa1_8
      integer    isoa1_9,isoa1_10, isoa2_1
      integer    isoa2_2, isoa2_3, isoa2_4
      integer    isoa2_5, isoa2_6, isoa2_7
      integer    isoa2_8, isoa2_9,isoa2_10
      integer    isoa3_1, isoa3_2, isoa3_3
      integer    isoa3_4, isoa3_5, isoa3_6
      integer    isoa3_7, isoa3_8, isoa3_9
      integer   isoa3_10, isoa4_1, isoa4_2
      integer    isoa4_3, isoa4_4, isoa4_5
      integer    isoa4_6, isoa4_7, isoa4_8
      integer    isoa4_9,isoa4_10, isoa5_1
      integer    isoa5_2, isoa5_3, isoa5_4
      integer    isoa5_5, isoa5_6, isoa5_7
      integer    isoa5_8, isoa5_9,isoa5_10
      integer    isoa6_1, isoa6_2, isoa6_3
      integer    isoa6_4, isoa6_5, isoa6_6
      integer    isoa6_7, isoa6_8, isoa6_9
      integer   isoa6_10, isoa7_1, isoa7_2
      integer    isoa7_3, isoa7_4, isoa7_5
      integer    isoa7_6, isoa7_7, isoa7_8
      integer    isoa7_9,isoa7_10, isopa_1
      integer    isopa_2, isopa_3, isopa_4
      integer    isopa_5, isopa_6, isopa_7
      integer    isopa_8, isopa_9,isopa_10
      integer    isopb_1, isopb_2, isopb_3
      integer    isopb_4, isopb_5, isopb_6
      integer    isopb_7, isopb_8, isopb_9
      integer   isopb_10,  ipoa_1,  ipoa_2
      integer     ipoa_3,  ipoa_4,  ipoa_5
      integer     ipoa_6,  ipoa_7,  ipoa_8
      integer     ipoa_9, ipoa_10,  ipec_1
      integer     ipec_2,  ipec_3,  ipec_4
      integer     ipec_5,  ipec_6,  ipec_7
      integer     ipec_8,  ipec_9, ipec_10
      integer    icrst_1, icrst_2, icrst_3
      integer    icrst_4, icrst_5, icrst_6
      integer    icrst_7, icrst_8, icrst_9
      integer   icrst_10, iph2o_1, iph2o_2
      integer    iph2o_3, iph2o_4, iph2o_5
      integer    iph2o_6, iph2o_7, iph2o_8
      integer    iph2o_9,iph2o_10,  ipcl_1
      integer     ipcl_2,  ipcl_3,  ipcl_4
      integer     ipcl_5,  ipcl_6,  ipcl_7
      integer     ipcl_8,  ipcl_9, ipcl_10
      integer      ina_1,   ina_2,   ina_3
      integer      ina_4,   ina_5,   ina_6
      integer      ina_7,   ina_8,   ina_9
      integer     ina_10, ipnh4_1, ipnh4_2
      integer    ipnh4_3, ipnh4_4, ipnh4_5
      integer    ipnh4_6, ipnh4_7, ipnh4_8
      integer    ipnh4_9,ipnh4_10, ipno3_1
      integer    ipno3_2, ipno3_3, ipno3_4
      integer    ipno3_5, ipno3_6, ipno3_7
      integer    ipno3_8, ipno3_9,ipno3_10
      integer    ipso4_1, ipso4_2, ipso4_3
      integer    ipso4_4, ipso4_5, ipso4_6
      integer    ipso4_7, ipso4_8, ipso4_9
      integer   ipso4_10
c
      equivalence
     &   (imap(  1),     ino),(imap(  2),    ino2),(imap(  3),     io3),
     &   (imap(  4),    ipan),(imap(  5),   ipanx),(imap(  6),   icres),
     &   (imap(  7),   ipan2),(imap(  8),   impan),(imap(  9),   ipbzn),
     &   (imap( 10),   inphe),(imap( 11),   irno3),(imap( 12),   idcb2),
     &   (imap( 13),   idcb3),(imap( 14),   ihno4),(imap( 15),   iaacd),
     &   (imap( 16),   iacet),(imap( 17),   iald2),(imap( 18),   ialdx),
     &   (imap( 19),   ialk1),(imap( 20),   ialk2),(imap( 21),   ialk3),
     &   (imap( 22),   ialk4),(imap( 23),   ialk5),(imap( 24),   iaro1),
     &   (imap( 25),   iaro2),(imap( 26),   ibacl),(imap( 27),   ibald),
     &   (imap( 28),   ibcl1),(imap( 29),   ibcl2),(imap( 30),   ibuta),
     &   (imap( 31),   iccho),(imap( 32),   iccrs),(imap( 33),    icg1),
     &   (imap( 34),    icg2),(imap( 35),    icg3),(imap( 36),    icg4),
     &   (imap( 37),    icg5),(imap( 38),    icg6),(imap( 39),    icg7),
     &   (imap( 40),    icl2),(imap( 41),     ico),(imap( 42),   ico2h),
     &   (imap( 43),   ico3h),(imap( 44),   icooh),(imap( 45),   icprm),
     &   (imap( 46),   idcb1),(imap( 47),    ieth),(imap( 48),   ietha),
     &   (imap( 49),   iethe),(imap( 50),   ietoh),(imap( 51),   ifacd),
     &   (imap( 52),   ifcrs),(imap( 53),   ifmcl),(imap( 54),   iform),
     &   (imap( 55),   ifprm),(imap( 56),    igly),(imap( 57),   ih2o2),
     &   (imap( 58),   ihc2h),(imap( 59),   ihcho),(imap( 60),    ihcl),
     &   (imap( 61),    ihg0),(imap( 62),    ihg2),(imap( 63),    ihgp),
     &   (imap( 64),   ihno3),(imap( 65),   iho2h),(imap( 66),   ihocl),
     &   (imap( 67),   ihono),(imap( 68),   iicl1),(imap( 69),   iicl2),
     &   (imap( 70),   iiole),(imap( 71),   iisop),(imap( 72),    iisp),
     &   (imap( 73),   iispd),(imap( 74),   imbut),(imap( 75),    imek),
     &   (imap( 76),   imeoh),(imap( 77),   imepx),(imap( 78),   imeth),
     &   (imap( 79),   imgly),(imap( 80),   imtbe),(imap( 81),    imvk),
     &   (imap( 82),     ina),(imap( 83),    inh3),(imap( 84),    intr),
     &   (imap( 85),   inxoy),(imap( 86),    iole),(imap( 87),   iole1),
     &   (imap( 88),   iole2),(imap( 89),   iopen),(imap( 90),   ipacd),
     &   (imap( 91),    ipar),(imap( 92),    ipcl),(imap( 93),    ipec),
     &   (imap( 94),   iph2o),(imap( 95),   iphen),(imap( 96),    ipna),
     &   (imap( 97),   ipnh4),(imap( 98),   ipno3),(imap( 99),    ipoa),
     &   (imap(100),   iprod),(imap(101),   ipso4),(imap(102),   irc2h),
     &   (imap(103),   irc3h),(imap(104),   ircho),(imap(105),   irooh),
     &   (imap(106),    iso2),(imap(107),   isoa1),(imap(108),   isoa2),
     &   (imap(109),   isoa3),(imap(110),   isoa4),(imap(111),   isoa5),
     &   (imap(112),   isoa6),(imap(113),   isoa7),(imap(114),   isopa),
     &   (imap(115),   isopb),(imap(116),    isqt),(imap(117),   isulf),
     &   (imap(118),   iterp),(imap(119),    itol),(imap(120),   itola),
     &   (imap(121),    itrp),(imap(122),     ixn),(imap(123),    ixyl),
     &   (imap(124),   ixyla),(imap(125), isoa1_1),(imap(126), isoa1_2),
     &   (imap(127), isoa1_3),(imap(128), isoa1_4),(imap(129), isoa1_5),
     &   (imap(130), isoa1_6),(imap(131), isoa1_7),(imap(132), isoa1_8),
     &   (imap(133), isoa1_9),(imap(134),isoa1_10),(imap(135), isoa2_1),
     &   (imap(136), isoa2_2),(imap(137), isoa2_3),(imap(138), isoa2_4),
     &   (imap(139), isoa2_5),(imap(140), isoa2_6),(imap(141), isoa2_7),
     &   (imap(142), isoa2_8),(imap(143), isoa2_9),(imap(144),isoa2_10),
     &   (imap(145), isoa3_1),(imap(146), isoa3_2),(imap(147), isoa3_3),
     &   (imap(148), isoa3_4),(imap(149), isoa3_5),(imap(150), isoa3_6),
     &   (imap(151), isoa3_7),(imap(152), isoa3_8),(imap(153), isoa3_9),
     &   (imap(154),isoa3_10),(imap(155), isoa4_1),(imap(156), isoa4_2),
     &   (imap(157), isoa4_3),(imap(158), isoa4_4),(imap(159), isoa4_5),
     &   (imap(160), isoa4_6),(imap(161), isoa4_7),(imap(162), isoa4_8),
     &   (imap(163), isoa4_9),(imap(164),isoa4_10),(imap(165), isoa5_1),
     &   (imap(166), isoa5_2),(imap(167), isoa5_3),(imap(168), isoa5_4),
     &   (imap(169), isoa5_5),(imap(170), isoa5_6),(imap(171), isoa5_7),
     &   (imap(172), isoa5_8),(imap(173), isoa5_9),(imap(174),isoa5_10),
     &   (imap(175), isoa6_1),(imap(176), isoa6_2),(imap(177), isoa6_3),
     &   (imap(178), isoa6_4),(imap(179), isoa6_5),(imap(180), isoa6_6),
     &   (imap(181), isoa6_7),(imap(182), isoa6_8),(imap(183), isoa6_9),
     &   (imap(184),isoa6_10),(imap(185), isoa7_1),(imap(186), isoa7_2),
     &   (imap(187), isoa7_3),(imap(188), isoa7_4),(imap(189), isoa7_5),
     &   (imap(190), isoa7_6),(imap(191), isoa7_7),(imap(192), isoa7_8),
     &   (imap(193), isoa7_9),(imap(194),isoa7_10),(imap(195), isopa_1),
     &   (imap(196), isopa_2),(imap(197), isopa_3),(imap(198), isopa_4),
     &   (imap(199), isopa_5),(imap(200), isopa_6),(imap(201), isopa_7),
     &   (imap(202), isopa_8),(imap(203), isopa_9),(imap(204),isopa_10),
     &   (imap(205), isopb_1),(imap(206), isopb_2),(imap(207), isopb_3),
     &   (imap(208), isopb_4),(imap(209), isopb_5),(imap(210), isopb_6),
     &   (imap(211), isopb_7),(imap(212), isopb_8),(imap(213), isopb_9),
     &   (imap(214),isopb_10),(imap(215),  ipoa_1),(imap(216),  ipoa_2),
     &   (imap(217),  ipoa_3),(imap(218),  ipoa_4),(imap(219),  ipoa_5),
     &   (imap(220),  ipoa_6),(imap(221),  ipoa_7),(imap(222),  ipoa_8),
     &   (imap(223),  ipoa_9),(imap(224), ipoa_10),(imap(225),  ipec_1),
     &   (imap(226),  ipec_2),(imap(227),  ipec_3),(imap(228),  ipec_4),
     &   (imap(229),  ipec_5),(imap(230),  ipec_6),(imap(231),  ipec_7),
     &   (imap(232),  ipec_8),(imap(233),  ipec_9),(imap(234), ipec_10),
     &   (imap(235), icrst_1),(imap(236), icrst_2),(imap(237), icrst_3),
     &   (imap(238), icrst_4),(imap(239), icrst_5),(imap(240), icrst_6),
     &   (imap(241), icrst_7),(imap(242), icrst_8),(imap(243), icrst_9),
     &   (imap(244),icrst_10),(imap(245), iph2o_1),(imap(246), iph2o_2),
     &   (imap(247), iph2o_3),(imap(248), iph2o_4),(imap(249), iph2o_5),
     &   (imap(250), iph2o_6),(imap(251), iph2o_7),(imap(252), iph2o_8),
     &   (imap(253), iph2o_9),(imap(254),iph2o_10),(imap(255),  ipcl_1),
     &   (imap(256),  ipcl_2),(imap(257),  ipcl_3),(imap(258),  ipcl_4),
     &   (imap(259),  ipcl_5),(imap(260),  ipcl_6),(imap(261),  ipcl_7),
     &   (imap(262),  ipcl_8),(imap(263),  ipcl_9),(imap(264), ipcl_10),
     &   (imap(265),   ina_1),(imap(266),   ina_2),(imap(267),   ina_3),
     &   (imap(268),   ina_4),(imap(269),   ina_5),(imap(270),   ina_6),
     &   (imap(271),   ina_7),(imap(272),   ina_8),(imap(273),   ina_9),
     &   (imap(274),  ina_10),(imap(275), ipnh4_1),(imap(276), ipnh4_2),
     &   (imap(277), ipnh4_3),(imap(278), ipnh4_4),(imap(279), ipnh4_5),
     &   (imap(280), ipnh4_6),(imap(281), ipnh4_7),(imap(282), ipnh4_8),
     &   (imap(283), ipnh4_9),(imap(284),ipnh4_10),(imap(285), ipno3_1),
     &   (imap(286), ipno3_2),(imap(287), ipno3_3),(imap(288), ipno3_4),
     &   (imap(289), ipno3_5),(imap(290), ipno3_6),(imap(291), ipno3_7),
     &   (imap(292), ipno3_8),(imap(293), ipno3_9),(imap(294),ipno3_10),
     &   (imap(295), ipso4_1),(imap(296), ipso4_2),(imap(297), ipso4_3),
     &   (imap(298), ipso4_4),(imap(299), ipso4_5),(imap(300), ipso4_6),
     &   (imap(301), ipso4_7),(imap(302), ipso4_8),(imap(303), ipso4_9),
     &   (imap(304),ipso4_10)
c
      integer   io1d  ,io    ,iclo
      integer   icl   ,in2o5 ,ino3 
      integer   ioh   ,iho2  ,ic2o3
      integer   ixo2  ,ixo2n ,icxo3
      integer   imeo2 ,ito2  ,iror
      integer   icro  ,iro2r ,ir2o2
      integer   iro2n ,icco3 ,irco3
      integer   imco3 ,ibzco ,icxo2
      integer   ihco3 ,itbuo ,ibzo
      integer   ibzno
c     
      equivalence (irad(1), io1d ), (irad(2), io   ), (irad(3), iclo ),
     &            (irad(4), icl  ), (irad(5), in2o5), (irad(6), ino3 ),
     &            (irad(7), ioh  ), (irad(8), iho2 ), (irad(9), ic2o3),
     &            (irad(10),ixo2 ), (irad(11),ixo2n), (irad(12),icxo3),
     &            (irad(13),imeo2), (irad(14),ito2 ), (irad(15),iror ),
     &            (irad(16),icro ), (irad(17),iro2r), (irad(18),ir2o2),
     &            (irad(19),iro2n), (irad(20),icco3), (irad(21),irco3),
     &            (irad(22),imco3), (irad(23),ibzco), (irad(24),icxo2),
     &            (irad(25),ihco3), (irad(26),itbuo), (irad(27),ibzo ),
     &            (irad(28),ibzno)

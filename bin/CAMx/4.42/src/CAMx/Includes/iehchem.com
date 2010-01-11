c-----CAMx v4.42 070514
c
c     IEHCHEM.COM sets species pointers for the IEH chemistry solver
c     These equivalences must be consistent with the internal   
c     species lists defined in data statements in READCHM
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c 
      integer imap(NSPNAM)
      integer irad(NRADNM)
      common /iname/ imap, irad
c
      integer        ino,    ino2,     io3
      integer       ipan,   icres,   ipan2
      integer      impan,   ipbzn,   inphe
      integer      irno3,   idcb2,   idcb3
      integer      ihno4,   iacet,   iald2
      integer      ialk1,   ialk2,   ialk3
      integer      ialk4,   ialk5,   iaro1
      integer      iaro2,   ibacl,   ibald
      integer      ibcl1,   ibcl2,   ibuta
      integer      iccho,   iccrs,    icg1
      integer       icg2,    icg3,    icg4
      integer       icg5,    icl2,     ico
      integer      ico2h,   ico3h,   icooh
      integer      icprm,   idcb1,    ieth
      integer      iethe,   ietoh,   ifcrs
      integer      ifmcl,   iform,   ifprm
      integer       igly,   ih2o2,   ihc2h
      integer      ihcho,    ihcl,    ihg0
      integer       ihg2,    ihgp,   ihno3
      integer      iho2h,   ihocl,   ihono
      integer      iicl1,   iicl2,   iisop
      integer      iispd,   imbut,    imek
      integer      imeoh,   imeth,   imgly
      integer      imtbe,    imvk,     ina
      integer       inh3,    intr,   inxoy
      integer       iole,   iole1,   iole2
      integer      iopen,    ipar,    ipcl
      integer       ipec,   iph2o,   iphen
      integer       ipna,   ipnh4,   ipno3
      integer       ipoa,   iprod,   ipso4
      integer      irc2h,   irc3h,   ircho
      integer      irooh,    iso2,   isoa1
      integer      isoa2,   isoa3,   isoa4
      integer      isoa5,   isulf,   iterp
      integer       itol,     ixn,    ixyl
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
      integer    isoa5_9,isoa5_10,  ipoa_1
      integer     ipoa_2,  ipoa_3,  ipoa_4
      integer     ipoa_5,  ipoa_6,  ipoa_7
      integer     ipoa_8,  ipoa_9, ipoa_10
      integer     ipec_1,  ipec_2,  ipec_3
      integer     ipec_4,  ipec_5,  ipec_6
      integer     ipec_7,  ipec_8,  ipec_9
      integer    ipec_10, icrst_1, icrst_2
      integer    icrst_3, icrst_4, icrst_5
      integer    icrst_6, icrst_7, icrst_8
      integer    icrst_9,icrst_10, iph2o_1
      integer    iph2o_2, iph2o_3, iph2o_4
      integer    iph2o_5, iph2o_6, iph2o_7
      integer    iph2o_8, iph2o_9,iph2o_10
      integer     ipcl_1,  ipcl_2,  ipcl_3
      integer     ipcl_4,  ipcl_5,  ipcl_6
      integer     ipcl_7,  ipcl_8,  ipcl_9
      integer    ipcl_10,   ina_1,   ina_2
      integer      ina_3,   ina_4,   ina_5
      integer      ina_6,   ina_7,   ina_8
      integer      ina_9,  ina_10, ipnh4_1
      integer    ipnh4_2, ipnh4_3, ipnh4_4
      integer    ipnh4_5, ipnh4_6, ipnh4_7
      integer    ipnh4_8, ipnh4_9,ipnh4_10
      integer    ipno3_1, ipno3_2, ipno3_3
      integer    ipno3_4, ipno3_5, ipno3_6
      integer    ipno3_7, ipno3_8, ipno3_9
      integer   ipno3_10, ipso4_1, ipso4_2
      integer    ipso4_3, ipso4_4, ipso4_5
      integer    ipso4_6, ipso4_7, ipso4_8
      integer    ipso4_9,ipso4_10
c
      equivalence
     &   (imap(  1),     ino),(imap(  2),    ino2),(imap(  3),     io3),
     &   (imap(  4),    ipan),(imap(  5),   icres),(imap(  6),   ipan2),
     &   (imap(  7),   impan),(imap(  8),   ipbzn),(imap(  9),   inphe),
     &   (imap( 10),   irno3),(imap( 11),   idcb2),(imap( 12),   idcb3),
     &   (imap( 13),   ihno4),(imap( 14),   iacet),(imap( 15),   iald2),
     &   (imap( 16),   ialk1),(imap( 17),   ialk2),(imap( 18),   ialk3),
     &   (imap( 19),   ialk4),(imap( 20),   ialk5),(imap( 21),   iaro1),
     &   (imap( 22),   iaro2),(imap( 23),   ibacl),(imap( 24),   ibald),
     &   (imap( 25),   ibcl1),(imap( 26),   ibcl2),(imap( 27),   ibuta),
     &   (imap( 28),   iccho),(imap( 29),   iccrs),(imap( 30),    icg1),
     &   (imap( 31),    icg2),(imap( 32),    icg3),(imap( 33),    icg4),
     &   (imap( 34),    icg5),(imap( 35),    icl2),(imap( 36),     ico),
     &   (imap( 37),   ico2h),(imap( 38),   ico3h),(imap( 39),   icooh),
     &   (imap( 40),   icprm),(imap( 41),   idcb1),(imap( 42),    ieth),
     &   (imap( 43),   iethe),(imap( 44),   ietoh),(imap( 45),   ifcrs),
     &   (imap( 46),   ifmcl),(imap( 47),   iform),(imap( 48),   ifprm),
     &   (imap( 49),    igly),(imap( 50),   ih2o2),(imap( 51),   ihc2h),
     &   (imap( 52),   ihcho),(imap( 53),    ihcl),(imap( 54),    ihg0),
     &   (imap( 55),    ihg2),(imap( 56),    ihgp),(imap( 57),   ihno3),
     &   (imap( 58),   iho2h),(imap( 59),   ihocl),(imap( 60),   ihono),
     &   (imap( 61),   iicl1),(imap( 62),   iicl2),(imap( 63),   iisop),
     &   (imap( 64),   iispd),(imap( 65),   imbut),(imap( 66),    imek),
     &   (imap( 67),   imeoh),(imap( 68),   imeth),(imap( 69),   imgly),
     &   (imap( 70),   imtbe),(imap( 71),    imvk),(imap( 72),     ina),
     &   (imap( 73),    inh3),(imap( 74),    intr),(imap( 75),   inxoy),
     &   (imap( 76),    iole),(imap( 77),   iole1),(imap( 78),   iole2),
     &   (imap( 79),   iopen),(imap( 80),    ipar),(imap( 81),    ipcl),
     &   (imap( 82),    ipec),(imap( 83),   iph2o),(imap( 84),   iphen),
     &   (imap( 85),    ipna),(imap( 86),   ipnh4),(imap( 87),   ipno3),
     &   (imap( 88),    ipoa),(imap( 89),   iprod),(imap( 90),   ipso4),
     &   (imap( 91),   irc2h),(imap( 92),   irc3h),(imap( 93),   ircho),
     &   (imap( 94),   irooh),(imap( 95),    iso2),(imap( 96),   isoa1),
     &   (imap( 97),   isoa2),(imap( 98),   isoa3),(imap( 99),   isoa4),
     &   (imap(100),   isoa5),(imap(101),   isulf),(imap(102),   iterp),
     &   (imap(103),    itol),(imap(104),     ixn),(imap(105),    ixyl),
     &   (imap(106), isoa1_1),(imap(107), isoa1_2),(imap(108), isoa1_3),
     &   (imap(109), isoa1_4),(imap(110), isoa1_5),(imap(111), isoa1_6),
     &   (imap(112), isoa1_7),(imap(113), isoa1_8),(imap(114), isoa1_9),
     &   (imap(115),isoa1_10),(imap(116), isoa2_1),(imap(117), isoa2_2),
     &   (imap(118), isoa2_3),(imap(119), isoa2_4),(imap(120), isoa2_5),
     &   (imap(121), isoa2_6),(imap(122), isoa2_7),(imap(123), isoa2_8),
     &   (imap(124), isoa2_9),(imap(125),isoa2_10),(imap(126), isoa3_1),
     &   (imap(127), isoa3_2),(imap(128), isoa3_3),(imap(129), isoa3_4),
     &   (imap(130), isoa3_5),(imap(131), isoa3_6),(imap(132), isoa3_7),
     &   (imap(133), isoa3_8),(imap(134), isoa3_9),(imap(135),isoa3_10),
     &   (imap(136), isoa4_1),(imap(137), isoa4_2),(imap(138), isoa4_3),
     &   (imap(139), isoa4_4),(imap(140), isoa4_5),(imap(141), isoa4_6),
     &   (imap(142), isoa4_7),(imap(143), isoa4_8),(imap(144), isoa4_9),
     &   (imap(145),isoa4_10),(imap(146), isoa5_1),(imap(147), isoa5_2),
     &   (imap(148), isoa5_3),(imap(149), isoa5_4),(imap(150), isoa5_5),
     &   (imap(151), isoa5_6),(imap(152), isoa5_7),(imap(153), isoa5_8),
     &   (imap(154), isoa5_9),(imap(155),isoa5_10),(imap(156),  ipoa_1),
     &   (imap(157),  ipoa_2),(imap(158),  ipoa_3),(imap(159),  ipoa_4),
     &   (imap(160),  ipoa_5),(imap(161),  ipoa_6),(imap(162),  ipoa_7),
     &   (imap(163),  ipoa_8),(imap(164),  ipoa_9),(imap(165), ipoa_10),
     &   (imap(166),  ipec_1),(imap(167),  ipec_2),(imap(168),  ipec_3),
     &   (imap(169),  ipec_4),(imap(170),  ipec_5),(imap(171),  ipec_6),
     &   (imap(172),  ipec_7),(imap(173),  ipec_8),(imap(174),  ipec_9),
     &   (imap(175), ipec_10),(imap(176), icrst_1),(imap(177), icrst_2),
     &   (imap(178), icrst_3),(imap(179), icrst_4),(imap(180), icrst_5),
     &   (imap(181), icrst_6),(imap(182), icrst_7),(imap(183), icrst_8),
     &   (imap(184), icrst_9),(imap(185),icrst_10),(imap(186), iph2o_1),
     &   (imap(187), iph2o_2),(imap(188), iph2o_3),(imap(189), iph2o_4),
     &   (imap(190), iph2o_5),(imap(191), iph2o_6),(imap(192), iph2o_7),
     &   (imap(193), iph2o_8),(imap(194), iph2o_9),(imap(195),iph2o_10),
     &   (imap(196),  ipcl_1),(imap(197),  ipcl_2),(imap(198),  ipcl_3),
     &   (imap(199),  ipcl_4),(imap(200),  ipcl_5),(imap(201),  ipcl_6),
     &   (imap(202),  ipcl_7),(imap(203),  ipcl_8),(imap(204),  ipcl_9),
     &   (imap(205), ipcl_10),(imap(206),   ina_1),(imap(207),   ina_2),
     &   (imap(208),   ina_3),(imap(209),   ina_4),(imap(210),   ina_5),
     &   (imap(211),   ina_6),(imap(212),   ina_7),(imap(213),   ina_8),
     &   (imap(214),   ina_9),(imap(215),  ina_10),(imap(216), ipnh4_1),
     &   (imap(217), ipnh4_2),(imap(218), ipnh4_3),(imap(219), ipnh4_4),
     &   (imap(220), ipnh4_5),(imap(221), ipnh4_6),(imap(222), ipnh4_7),
     &   (imap(223), ipnh4_8),(imap(224), ipnh4_9),(imap(225),ipnh4_10),
     &   (imap(226), ipno3_1),(imap(227), ipno3_2),(imap(228), ipno3_3),
     &   (imap(229), ipno3_4),(imap(230), ipno3_5),(imap(231), ipno3_6),
     &   (imap(232), ipno3_7),(imap(233), ipno3_8),(imap(234), ipno3_9),
     &   (imap(235),ipno3_10),(imap(236), ipso4_1),(imap(237), ipso4_2),
     &   (imap(238), ipso4_3),(imap(239), ipso4_4),(imap(240), ipso4_5),
     &   (imap(241), ipso4_6),(imap(242), ipso4_7),(imap(243), ipso4_8),
     &   (imap(244), ipso4_9),(imap(245),ipso4_10)
c
      integer   io1d  ,io    ,iclo 
      integer   icl   ,in2o5 ,ino3 
      integer   ioh   ,iho2  ,ic2o3
      integer   ixo2  ,ixo2n ,ito2 
      integer   iror  ,icro  ,iro2r
      integer   ir2o2 ,iro2n ,icco3
      integer   irco3 ,imco3 ,ibzco
      integer   icxo2 ,ihco3 ,itbuo
      integer   ibzo  ,ibzno
c
      equivalence (irad(1), io1d ), (irad(2), io   ), (irad(3), iclo ),
     &            (irad(4), icl  ), (irad(5), in2o5), (irad(6), ino3 ),
     &            (irad(7), ioh  ), (irad(8), iho2 ), (irad(9), ic2o3),
     &            (irad(10),ixo2 ), (irad(11),ixo2n), (irad(12),ito2 ),
     &            (irad(13),iror ), (irad(14),icro ), (irad(15),iro2r),
     &            (irad(16),ir2o2), (irad(17),iro2n), (irad(18),icco3),
     &            (irad(19),irco3), (irad(20),imco3), (irad(21),ibzco),
     &            (irad(22),icxo2), (irad(23),ihco3), (irad(24),itbuo),
     &            (irad(25),ibzo ), (irad(26),ibzno)

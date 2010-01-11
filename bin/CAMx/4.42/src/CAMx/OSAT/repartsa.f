C*** REPARTSA.F
c
      subroutine repartsa(igrid,icell,jcell,kcell,modcon)
      implicit none
c
c----CAMx v4.42 070603
c
c-----------------------------------------------------------------------
c   Description:
c     This routine repartitions the particulate and gaseous portions
c     nitric acid and ammonia tracers based on the ratio found in the
c     regular model.
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       igrid   I  grid number
c       icell   I  the X grid location of current cell
c       jcell   I  the X grid location of current cell
c       kcell   I  the vertical grid location of current layer
c       modcon  R  array of species concentrations
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     09/28/03   --gwilson--  Original development
c     05/05/07   --gwilson--  Added code to make an adjustment to
c                             tracer species if there is stray from 
c                             the regular - added to handle mass errors
c                             in regular model chemistry routines
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'grid.com'
      include 'chmstry.com'
      include 'tracer.com'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer igrid
      integer icell
      integer jcell
      integer kcell
      real    modcon(MXSPEC)
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c    FUZZ - fuzz value for comparing against lower bound
c
      real FUZZ
c
      parameter( FUZZ = 10.0 )
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer idxcel, idxhno3, idxpno3, idxnh3, idxpnh4
      integer ipno3, ipnh4, ipo1, ipo2, ipo3, ipo4, ipo5
      integer idxpo1, idxpo2, idxpo3, idxpo4, idxpo5, idx
      integer idxcg1, idxcg2, idxcg3, idxcg4, idxcg5, i
      real    wthno3, wtnh3, conhno3, conpno3, connh3, conpnh4
      real    concg1, concg2, concg3, concg4, concg5
      real    conpo1, conpo2, conpo3, conpo4, conpo5
      real    wtcg1, wtcg2, wtcg3, wtcg4, wtcg5
      real    ratio, sumtrac, sumall
      real    contrac, conadjust
c
c-----------------------------------------------------------------------
c    Data statements:
c-----------------------------------------------------------------------
c
      data wtnh3  /18.0/
      data wthno3 /62.0/
      data wtcg1 /150.0/
      data wtcg2 /150.0/
      data wtcg3 /150.0/
      data wtcg4 /180.0/
      data wtcg5 /150.0/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- calculate the index of the cell in the grid ---
c
      idxcel =  ipsa3d(igrid)-1+ icell + ncol(igrid)*(jcell-1) + 
     &                      ncol(igrid)*nrow(igrid)*(kcell-1)
c
      if( lnitrate ) then
c
c  --- calculate the ratio of gas to  particluate nitric acid ---
c
         conhno3 = modcon(khno3) * wthno3
         conpno3 = modcon(kpno3)
         ratio = 0.
         if( conhno3+conpno3 .GT. 0. )
     &                    ratio = conhno3 / (conpno3 + conhno3)
c
c  --- get the sum all all tracer species ---
c
         contrac = 0.0
         do i=iptcls(idxipt(ITRHN3)),nptcls(idxipt(ITRHN3))
           idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
           if( ptconc(idx) .GT. FUZZ*BNDLPT ) 
     &                          contrac = contrac + ptconc(idx) * wthno3
         enddo
         do i=iptcls(idxipt(ITRPN3)),nptcls(idxipt(ITRPN3))
           idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
           if( ptconc(idx) .GT. FUZZ*BNDLPT ) 
     &                                  contrac = contrac + ptconc(idx)
         enddo
         if( contrac .GT. 0. ) then
             conadjust = (conhno3+conpno3) / contrac
         else
             conadjust = 1.0
         endif
c
c  --- adjust the nitric acid tracers ----
c
         ipno3 = iptcls(idxipt(ITRPN3)) - 1
         do i=iptcls(idxipt(ITRHN3)),nptcls(idxipt(ITRHN3))
            idxhno3 = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            ipno3 = ipno3 + 1
            idxpno3 = idxcel + ncol(igrid)*nrow(igrid)*
     &                                             nlay(igrid)*(ipno3-1)
c
c  --- redistribute the gas and particulates ---
c
            sumtrac = ptconc(idxhno3)*wthno3 + ptconc(idxpno3)
            ptconc(idxhno3) = (ratio * sumtrac ) / wthno3
            ptconc(idxhno3) = MAX(ptconc(idxhno3),BNDLPT)
            ptconc(idxpno3) = sumtrac - ptconc(idxhno3) * wthno3
            ptconc(idxpno3) = MAX(ptconc(idxpno3),BNDLPT)
c
c  --- adjust for mass imbalance, if needed ---
c
            if( ptconc(idxhno3) .GT. FUZZ*BNDLPT ) 
     &                  ptconc(idxhno3) = conadjust * ptconc(idxhno3)
            if( ptconc(idxpno3) .GT. FUZZ*BNDLPT )  
     &                  ptconc(idxpno3) = conadjust * ptconc(idxpno3)
            ptconc(idxhno3) = MAX(ptconc(idxhno3),BNDLPT)
            ptconc(idxpno3) = MAX(ptconc(idxpno3),BNDLPT)
         enddo
c
c  --- calculate the ratio of gas to particluate ammonia ---
c
         connh3 = modcon(knh3) * wtnh3
         conpnh4 = modcon(kpnh4)
         ratio = 0.
         if( connh3+conpnh4 .GT. 0. )
     &                    ratio = connh3 / (conpnh4 + connh3)
c
c  --- get the sum all all tracer species ---
c
         contrac = 0.0
         do i=iptcls(idxipt(ITRNH3)),nptcls(idxipt(ITRNH3))
           idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
           if( ptconc(idx) .GT. FUZZ*BNDLPT )
     &                          contrac = contrac + ptconc(idx) * wtnh3
         enddo
         do i=iptcls(idxipt(ITRPN4)),nptcls(idxipt(ITRPN4))
           idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
           if( ptconc(idx) .GT. FUZZ*BNDLPT )
     &                                  contrac = contrac + ptconc(idx)
         enddo
         if( contrac .GT. 0. ) then
             conadjust = (connh3+conpnh4) / contrac
         else
             conadjust = 1.0
         endif
c
c  --- adjust the ammonia tracers ----
c
         ipnh4 = iptcls(idxipt(ITRPN4)) - 1
         do i=iptcls(idxipt(ITRNH3)),nptcls(idxipt(ITRNH3))
            idxnh3 = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            ipnh4 = ipnh4 + 1
            idxpnh4 = idxcel + ncol(igrid)*nrow(igrid)*
     &                                             nlay(igrid)*(ipnh4-1)
c
c  --- redistribute the gas and particulates ---
c
            sumtrac = ptconc(idxnh3)*wtnh3 + ptconc(idxpnh4)
            ptconc(idxnh3) = ratio * sumtrac / wtnh3
            ptconc(idxnh3) = MAX(ptconc(idxnh3),BNDLPT)
            ptconc(idxpnh4) = sumtrac - ptconc(idxnh3) * wtnh3
            ptconc(idxpnh4) = MAX(ptconc(idxpnh4),BNDLPT)
c
c  --- adjust for mass imbalance, if needed ---
c
            if( ptconc(idxhno3) .GT. FUZZ*BNDLPT )
     &                  ptconc(idxnh3) = conadjust * ptconc(idxnh3)
            if( ptconc(idxpnh4) .GT. FUZZ*BNDLPT )
     &                  ptconc(idxpnh4) = conadjust * ptconc(idxpnh4)
            ptconc(idxnh3) = MAX(ptconc(idxnh3),BNDLPT)
            ptconc(idxpnh4) = MAX(ptconc(idxpnh4),BNDLPT)
         enddo
      endif
c
c  --- secondary aerosols ---
c
      if( lsoa ) then
c
c  --- calculate the ratio of CG1 to SOA1 ---
c
         concg1 = modcon(kcg1) * wtcg1
         conpo1 = modcon(ksoa1)
         ratio = 0.
         if( concg1+conpo1 .GT. 0. )
     &                    ratio = concg1 / (conpo1 + concg1)
c
c  --- adjust the CG1/PO1 ratio ---
c
         ipo1 = iptcls(idxipt(ITRPO1)) - 1
         do i=iptcls(idxipt(ITRCG1)),nptcls(idxipt(ITRCG1))
            idxcg1 = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            ipo1 = ipo1 + 1
            idxpo1 = idxcel + ncol(igrid)*nrow(igrid)*
     &                                             nlay(igrid)*(ipo1-1)
            sumtrac = ptconc(idxcg1)*wtcg1 + ptconc(idxpo1)
            ptconc(idxcg1) = (ratio * sumtrac ) / wtcg1
            ptconc(idxcg1) = MAX(ptconc(idxcg1),BNDLPT)
            ptconc(idxpo1) = sumtrac - ptconc(idxcg1) * wtcg1
            ptconc(idxpo1) = MAX(ptconc(idxpo1),BNDLPT)
         enddo
c
c  --- calculate the ratio of CG2 to SOA2 ---
c
         concg2 = modcon(kcg2) * wtcg2
         conpo2 = modcon(ksoa2)
         ratio = 0.
         if( concg2+conpo2 .GT. 0. )
     &                    ratio = concg2 / (conpo2 + concg2)
c
c  --- adjust the CG2/PO2 ratio ---
c
         ipo2 = iptcls(idxipt(ITRPO2)) - 1
         do i=iptcls(idxipt(ITRCG2)),nptcls(idxipt(ITRCG2))
            idxcg2 = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            ipo2 = ipo2 + 1
            idxpo2 = idxcel + ncol(igrid)*nrow(igrid)*
     &                                             nlay(igrid)*(ipo2-1)
            sumtrac = ptconc(idxcg2)*wtcg2 + ptconc(idxpo2)
            ptconc(idxcg2) = (ratio * sumtrac ) / wtcg2
            ptconc(idxcg2) = MAX(ptconc(idxcg2),BNDLPT)
            ptconc(idxpo2) = sumtrac - ptconc(idxcg2) * wtcg2
            ptconc(idxpo2) = MAX(ptconc(idxpo2),BNDLPT)
         enddo
c
c  --- calculate the ratio of CG3 to SOA3 ---
c
         concg3 = modcon(kcg3) * wtcg3
         conpo3 = modcon(ksoa3)
         ratio = 0.
         if( concg3+conpo3 .GT. 0. )
     &                    ratio = concg3 / (conpo3 + concg3)
c
c  --- adjust the CG3/PO3 ratio ---
c
         ipo3 = iptcls(idxipt(ITRPO3)) - 1
         do i=iptcls(idxipt(ITRCG3)),nptcls(idxipt(ITRCG3))
            idxcg3 = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            ipo3 = ipo3 + 1
            idxpo3 = idxcel + ncol(igrid)*nrow(igrid)*
     &                                             nlay(igrid)*(ipo3-1)
            sumtrac = ptconc(idxcg3)*wtcg3 + ptconc(idxpo3)
            ptconc(idxcg3) = (ratio * sumtrac ) / wtcg3
            ptconc(idxcg3) = MAX(ptconc(idxcg3),BNDLPT)
            ptconc(idxpo3) = sumtrac - ptconc(idxcg3) * wtcg3
            ptconc(idxpo3) = MAX(ptconc(idxpo3),BNDLPT)
         enddo
c
c  --- calculate the ratio of CG4 to SOA4 ---
c
         concg4 = modcon(kcg4) * wtcg4
         conpo4 = modcon(ksoa4)
         ratio = 0.
         if( concg4+conpo4 .GT. 0. )
     &                    ratio = concg4 / (conpo4 + concg4)
c
c  --- adjust the CG4/PO4 ratio ---
c
         ipo4 = iptcls(idxipt(ITRPO4)) - 1
         do i=iptcls(idxipt(ITRCG4)),nptcls(idxipt(ITRCG4))
            idxcg4 = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            ipo4 = ipo4 + 1
            idxpo4 = idxcel + ncol(igrid)*nrow(igrid)*
     &                                             nlay(igrid)*(ipo4-1)
            sumtrac = ptconc(idxcg4)*wtcg4 + ptconc(idxpo4)
            ptconc(idxcg4) = (ratio * sumtrac ) / wtcg4
            ptconc(idxcg4) = MAX(ptconc(idxcg4),BNDLPT)
            ptconc(idxpo4) = sumtrac - ptconc(idxcg4) * wtcg4
            ptconc(idxpo4) = MAX(ptconc(idxpo4),BNDLPT)
         enddo
c
c  --- calculate the ratio of CG5 to SOA5 ---
c
         concg5 = modcon(kcg5) * wtcg5
         conpo5 = modcon(ksoa5)
         ratio = 0.
         if( concg5+conpo5 .GT. 0. )
     &                    ratio = concg5 / (conpo5 + concg5)
c
c  --- adjust the CG5/PO5 ratio ---
c
         ipo5 = iptcls(idxipt(ITRPO5)) - 1
         do i=iptcls(idxipt(ITRCG5)),nptcls(idxipt(ITRCG5))
            idxcg5 = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            ipo5 = ipo5 + 1
            idxpo5 = idxcel + ncol(igrid)*nrow(igrid)*
     &                                             nlay(igrid)*(ipo5-1)
            sumtrac = ptconc(idxcg5)*wtcg5 + ptconc(idxpo5)
            ptconc(idxcg5) = (ratio * sumtrac ) / wtcg5
            ptconc(idxcg5) = MAX(ptconc(idxcg5),BNDLPT)
            ptconc(idxpo5) = sumtrac - ptconc(idxcg5) * wtcg5
            ptconc(idxpo5) = MAX(ptconc(idxpo5),BNDLPT)
         enddo
      endif
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

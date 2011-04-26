c*** REPARTSA.F
c
      subroutine repartsa(numcol,numrow,numlay,igrid,
     &                                  icell,jcell,kcell,modcon,delcon)
      use grid
      use chmstry
      use tracer
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c   Description:
c     This routine repartitions the particulate and gaseous portions
c     nitric acid and ammonia tracers based on the ratio found in the
c     regular model.
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       numcol  I number of columns in this slice
c       numrow  I number of rows in this slice
c       numlay  I number of layers in this slice
c       igrid   I  grid number
c       icell   I  the X grid location of current cell
c       jcell   I  the X grid location of current cell
c       kcell   I  the vertical grid location of current layer
c       modcon  R  array of model species concentrations
c       delcon  R  array of change in model species concentrations
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     09/28/03   --gwilson--  Original development
c     12/29/06   --bkoo--     Revised for the updated SOA scheme
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
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer numcol
      integer numrow
      integer numlay
      integer igrid
      integer icell
      integer jcell
      integer kcell
      real    modcon(*)
      real    delcon(5,MXSPEC)
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
      integer i, idxcel, idxhno3, idxpno3, idxnh3, idxpnh4
      integer ipno3, ipnh4, ipo1, ipo2, ipo3, ipo4, ipo5, ipo6, ipo7
      integer idxpo1, idxpo2, idxpo3, idxpo4, idxpo5, idxpo6, idxpo7
      integer idxcg1, idxcg2, idxcg3, idxcg4, idxcg5, idxcg6, idxcg7
      integer ippa, ippb, idxppa, idxppb
      real    wthno3, wtnh3, conhno3, conpno3, connh3, conpnh4
      real    concg1, concg2, concg3, concg4, concg5, concg6, concg7
      real    conpo1, conpo2, conpo3, conpo4, conpo5, conpo6, conpo7
      real    wtcg1, wtcg2, wtcg3, wtcg4, wtcg5, wtcg6, wtcg7
      real    ratio, sumtrac, pratio, contrac, conadjust
c
c-----------------------------------------------------------------------
c    Data statements:
c     Note: MWs for CG species should be consistent with those defined
c           in /SOAP/soapdat.f
c-----------------------------------------------------------------------
c
      data wtnh3  /18.0/
      data wthno3 /62.0/
      data wtcg1 /150.0/
      data wtcg2 /150.0/
      data wtcg3 /130.0/
      data wtcg4 /130.0/
      data wtcg5 /180.0/
      data wtcg6 /180.0/
      data wtcg7 /210.0/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- calculate the index of the cell in the grid ---
c
      idxcel =  ipsa3d(igrid)-1+ icell + numcol *(jcell-1) + 
     &                      numcol * numrow *(kcell-1)
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
c  --- get the sum of all tracer species ---
c
         contrac = 0.0
         ipno3 = iptcls(idxipt(ITRPN3)) - 1
         do i=iptcls(idxipt(ITRHN3)),nptcls(idxipt(ITRHN3))
            idxhno3 = idxcel + numcol * numrow *numlay * (i-1)
            ipno3 = ipno3 + 1
            idxpno3 = idxcel + numcol * numrow * numlay * (ipno3-1)
            sumtrac = ptconc(idxhno3)*wthno3 + ptconc(idxpno3)
            if( sumtrac .GT. FUZZ*BNDLPT ) contrac = contrac + sumtrac
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
            idxhno3 = idxcel + numcol * numrow * numlay * (i-1)
            ipno3 = ipno3 + 1
            idxpno3 = idxcel + numcol * numrow * numlay * (ipno3-1)
c
c  --- adjust for mass imbalance, if needed ---
c
            sumtrac = ptconc(idxhno3)*wthno3 + ptconc(idxpno3)
            if( sumtrac .GT. FUZZ*BNDLPT ) sumtrac = conadjust * sumtrac
c
c  --- redistribute the gas and particulates ---
c
            ptconc(idxhno3) = (ratio * sumtrac ) / wthno3
            ptconc(idxhno3) = MAX(ptconc(idxhno3),BNDLPT)
            ptconc(idxpno3) = sumtrac - ptconc(idxhno3) * wthno3
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
c  --- get the sum of all tracer species ---
c
         contrac = 0.0
         ipnh4 = iptcls(idxipt(ITRPN4)) - 1
         do i=iptcls(idxipt(ITRNH3)),nptcls(idxipt(ITRNH3))
            idxnh3 = idxcel + numcol * numrow * numlay * (i-1)
            ipnh4 = ipnh4 + 1
            idxpnh4 = idxcel + numcol * numrow * numlay * (ipnh4-1)
            sumtrac = ptconc(idxnh3)*wtnh3 + ptconc(idxpnh4)
            if( sumtrac .GT. FUZZ*BNDLPT ) contrac = contrac + sumtrac
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
            idxnh3 = idxcel + numcol * numrow * numlay * (i-1)
            ipnh4 = ipnh4 + 1
            idxpnh4 = idxcel + numcol * numrow * numlay * (ipnh4-1)
c        
c  --- adjust for mass imbalance, if needed ---
c  
            sumtrac = ptconc(idxnh3)*wtnh3 + ptconc(idxpnh4)
            if( sumtrac .GT. FUZZ*BNDLPT ) sumtrac = conadjust * sumtrac
c
c  --- redistribute the gas and particulates ---
c
            ptconc(idxnh3) = ratio * sumtrac / wtnh3
            ptconc(idxnh3) = MAX(ptconc(idxnh3),BNDLPT)
            ptconc(idxpnh4) = sumtrac - ptconc(idxnh3) * wtnh3
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
         conpo1 = modcon(ksoa1) - delcon(5,ksoa1) ! add polym'd frac to get con at equil
         ratio = 0.
         if( concg1+conpo1 .GT. 0. )
     &                    ratio = concg1 / (conpo1 + concg1)
         pratio = 0.
         if ( conpo1 .GT. 0. ) pratio = -delcon(5,ksoa1) / conpo1
c
c  --- adjust the CG1/PO1 ratio ---
c
         ipo1 = iptcls(idxipt(ITRPO1)) - 1
         ippa = iptcls(idxipt(ITRPPA)) - 1
         do i=iptcls(idxipt(ITRCG1)),nptcls(idxipt(ITRCG1))
            idxcg1 = idxcel + numcol * numrow * numlay * (i-1)
            ipo1 = ipo1 + 1
            ippa = ippa + 1
            idxpo1 = idxcel + numcol * numrow * numlay * (ipo1-1)
            idxppa = idxcel + numcol * numrow * numlay * (ippa-1)
            sumtrac = ptconc(idxcg1)*wtcg1 + ptconc(idxpo1)
            ptconc(idxcg1) = (ratio * sumtrac ) / wtcg1
            ptconc(idxpo1) = (1.0-ratio) * sumtrac
            ptconc(idxppa) = ptconc(idxppa) + pratio*ptconc(idxpo1) ! SOPA tracers
            ptconc(idxpo1) = (1.0-pratio) * ptconc(idxpo1) ! after polymerization
            ptconc(idxcg1) = MAX(ptconc(idxcg1),BNDLPT)
            ptconc(idxpo1) = MAX(ptconc(idxpo1),BNDLPT)
            ptconc(idxppa) = MAX(ptconc(idxppa),BNDLPT)
         enddo
c
c  --- calculate the ratio of CG2 to SOA2 ---
c
         concg2 = modcon(kcg2) * wtcg2
         conpo2 = modcon(ksoa2) - delcon(5,ksoa2) ! add polym'd frac to get con at equil
         ratio = 0.
         if( concg2+conpo2 .GT. 0. )
     &                    ratio = concg2 / (conpo2 + concg2)
         pratio = 0.
         if ( conpo2 .GT. 0. ) pratio = -delcon(5,ksoa2) / conpo2
c
c  --- adjust the CG2/PO2 ratio ---
c
         ipo2 = iptcls(idxipt(ITRPO2)) - 1
         ippa = iptcls(idxipt(ITRPPA)) - 1
         do i=iptcls(idxipt(ITRCG2)),nptcls(idxipt(ITRCG2))
            idxcg2 = idxcel + numcol * numrow * numlay * (i-1)
            ipo2 = ipo2 + 1
            ippa = ippa + 1
            idxpo2 = idxcel + numcol * numrow * numlay * (ipo2-1)
            idxppa = idxcel + numcol * numrow * numlay * (ippa-1)
            sumtrac = ptconc(idxcg2)*wtcg2 + ptconc(idxpo2)
            ptconc(idxcg2) = (ratio * sumtrac ) / wtcg2
            ptconc(idxpo2) = (1.0-ratio) * sumtrac
            ptconc(idxppa) = ptconc(idxppa) + pratio*ptconc(idxpo2) ! SOPA tracers
            ptconc(idxpo2) = (1.0-pratio) * ptconc(idxpo2) ! after polymerization
            ptconc(idxcg2) = MAX(ptconc(idxcg2),BNDLPT)
            ptconc(idxpo2) = MAX(ptconc(idxpo2),BNDLPT)
            ptconc(idxppa) = MAX(ptconc(idxppa),BNDLPT)
         enddo
c
c  --- calculate the ratio of CG3 to SOA3 ---
c
         concg3 = modcon(kcg3) * wtcg3
         conpo3 = modcon(ksoa3) - delcon(5,ksoa3) ! add polym'd frac to get con at equil
         ratio = 0.
         if( concg3+conpo3 .GT. 0. )
     &                    ratio = concg3 / (conpo3 + concg3)
         pratio = 0.
         if ( conpo3 .GT. 0. ) pratio = -delcon(5,ksoa3) / conpo3
c
c  --- adjust the CG3/PO3 ratio ---
c
         ipo3 = iptcls(idxipt(ITRPO3)) - 1
         ippb = iptcls(idxipt(ITRPPB)) - 1
         do i=iptcls(idxipt(ITRCG3)),nptcls(idxipt(ITRCG3))
            idxcg3 = idxcel + numcol * numrow * numlay * (i-1)
            ipo3 = ipo3 + 1
            ippb = ippb + 1
            idxpo3 = idxcel + numcol * numrow * numlay * (ipo3-1)
            idxppb = idxcel + numcol * numrow * numlay * (ippb-1)
            sumtrac = ptconc(idxcg3)*wtcg3 + ptconc(idxpo3)
            ptconc(idxcg3) = (ratio * sumtrac ) / wtcg3
            ptconc(idxpo3) = (1.0-ratio) * sumtrac
            ptconc(idxppb) = ptconc(idxppb) + pratio*ptconc(idxpo3) ! SOPB tracers
            ptconc(idxpo3) = (1.0-pratio) * ptconc(idxpo3) ! after polymerization
            ptconc(idxcg3) = MAX(ptconc(idxcg3),BNDLPT)
            ptconc(idxpo3) = MAX(ptconc(idxpo3),BNDLPT)
            ptconc(idxppb) = MAX(ptconc(idxppb),BNDLPT)
         enddo
c
c  --- calculate the ratio of CG4 to SOA4 ---
c
         concg4 = modcon(kcg4) * wtcg4
         conpo4 = modcon(ksoa4) - delcon(5,ksoa4) ! add polym'd frac to get con at equil
         ratio = 0.
         if( concg4+conpo4 .GT. 0. )
     &                    ratio = concg4 / (conpo4 + concg4)
         pratio = 0.
         if ( conpo4 .GT. 0. ) pratio = -delcon(5,ksoa4) / conpo4
c
c  --- adjust the CG4/PO4 ratio ---
c
         ipo4 = iptcls(idxipt(ITRPO4)) - 1
         ippb = iptcls(idxipt(ITRPPB)) - 1
         do i=iptcls(idxipt(ITRCG4)),nptcls(idxipt(ITRCG4))
            idxcg4 = idxcel + numcol * numrow * numlay * (i-1)
            ipo4 = ipo4 + 1
            ippb = ippb + 1
            idxpo4 = idxcel + numcol * numrow * numlay * (ipo4-1)
            idxppb = idxcel + numcol * numrow * numlay * (ippb-1)
            sumtrac = ptconc(idxcg4)*wtcg4 + ptconc(idxpo4)
            ptconc(idxcg4) = (ratio * sumtrac ) / wtcg4
            ptconc(idxpo4) = (1.0-ratio) * sumtrac
            ptconc(idxppb) = ptconc(idxppb) + pratio*ptconc(idxpo4) ! SOPB tracers
            ptconc(idxpo4) = (1.0-pratio) * ptconc(idxpo4) ! after polymerization
            ptconc(idxcg4) = MAX(ptconc(idxcg4),BNDLPT)
            ptconc(idxpo4) = MAX(ptconc(idxpo4),BNDLPT)
            ptconc(idxppb) = MAX(ptconc(idxppb),BNDLPT)
         enddo
c
c  --- calculate the ratio of CG5 to SOA5 ---
c
         concg5 = modcon(kcg5) * wtcg5
         conpo5 = modcon(ksoa5) - delcon(5,ksoa5) ! add polym'd frac to get con at equil
         ratio = 0.
         if( concg5+conpo5 .GT. 0. )
     &                    ratio = concg5 / (conpo5 + concg5)
         pratio = 0.
         if ( conpo5 .GT. 0. ) pratio = -delcon(5,ksoa5) / conpo5
c
c  --- adjust the CG5/PO5 ratio ---
c
         ipo5 = iptcls(idxipt(ITRPO5)) - 1
         ippb = iptcls(idxipt(ITRPPB)) - 1
         do i=iptcls(idxipt(ITRCG5)),nptcls(idxipt(ITRCG5))
            idxcg5 = idxcel + numcol * numrow * numlay * (i-1)
            ipo5 = ipo5 + 1
            ippb = ippb + 1
            idxpo5 = idxcel + numcol * numrow * numlay * (ipo5-1)
            idxppb = idxcel + numcol * numrow * numlay * (ippb-1)
            sumtrac = ptconc(idxcg5)*wtcg5 + ptconc(idxpo5)
            ptconc(idxcg5) = (ratio * sumtrac ) / wtcg5
            ptconc(idxpo5) = (1.0-ratio) * sumtrac
            ptconc(idxppb) = ptconc(idxppb) + pratio*ptconc(idxpo5) ! SOPB tracers
            ptconc(idxpo5) = (1.0-pratio) * ptconc(idxpo5) ! after polymerization
            ptconc(idxcg5) = MAX(ptconc(idxcg5),BNDLPT)
            ptconc(idxpo5) = MAX(ptconc(idxpo5),BNDLPT)
            ptconc(idxppb) = MAX(ptconc(idxppb),BNDLPT)
         enddo
c
c  --- calculate the ratio of CG6 to SOA6 ---
c
         concg6 = modcon(kcg6) * wtcg6
         conpo6 = modcon(ksoa6) - delcon(5,ksoa6) ! add polym'd frac to get con at equil
         ratio = 0.
         if( concg6+conpo6 .GT. 0. )
     &                    ratio = concg6 / (conpo6 + concg6)
         pratio = 0.
         if ( conpo6 .GT. 0. ) pratio = -delcon(5,ksoa6) / conpo6
c
c  --- adjust the CG6/PO6 ratio ---
c
         ipo6 = iptcls(idxipt(ITRPO6)) - 1
         ippb = iptcls(idxipt(ITRPPB)) - 1
         do i=iptcls(idxipt(ITRCG6)),nptcls(idxipt(ITRCG6))
            idxcg6 = idxcel + numcol * numrow * numlay * (i-1)
            ipo6 = ipo6 + 1
            ippb = ippb + 1
            idxpo6 = idxcel + numcol * numrow * numlay * (ipo6-1)
            idxppb = idxcel + numcol * numrow * numlay * (ippb-1)
            sumtrac = ptconc(idxcg6)*wtcg6 + ptconc(idxpo6)
            ptconc(idxcg6) = (ratio * sumtrac ) / wtcg6
            ptconc(idxpo6) = (1.0-ratio) * sumtrac
            ptconc(idxppb) = ptconc(idxppb) + pratio*ptconc(idxpo6) ! SOPB tracers
            ptconc(idxpo6) = (1.0-pratio) * ptconc(idxpo6) ! after polymerization
            ptconc(idxcg6) = MAX(ptconc(idxcg6),BNDLPT)
            ptconc(idxpo6) = MAX(ptconc(idxpo6),BNDLPT)
            ptconc(idxppb) = MAX(ptconc(idxppb),BNDLPT)
         enddo
c
c  --- calculate the ratio of CG7 to SOA7 ---
c
         concg7 = modcon(kcg7) * wtcg7
         conpo7 = modcon(ksoa7) ! no polymerization for SOA from SQT
         ratio = 0.
         if( concg7+conpo7 .GT. 0. )
     &                    ratio = concg7 / (conpo7 + concg7)
c
c  --- adjust the CG7/PO7 ratio ---
c
         ipo7 = iptcls(idxipt(ITRPO7)) - 1
         do i=iptcls(idxipt(ITRCG7)),nptcls(idxipt(ITRCG7))
            idxcg7 = idxcel + numcol * numrow * numlay * (i-1)
            ipo7 = ipo7 + 1
            idxpo7 = idxcel + numcol * numrow * numlay * (ipo7-1)
            sumtrac = ptconc(idxcg7)*wtcg7 + ptconc(idxpo7)
            ptconc(idxcg7) = (ratio * sumtrac ) / wtcg7
            ptconc(idxpo7) = (1.0-ratio) * sumtrac
            ptconc(idxcg7) = MAX(ptconc(idxcg7),BNDLPT)
            ptconc(idxpo7) = MAX(ptconc(idxpo7),BNDLPT)
         enddo
      endif
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

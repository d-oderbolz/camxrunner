c*** PSATSA
c
      subroutine psatsa(igrid,icell,jcell,kcell,convfac,
     &                                         cold,delcon,rrxn_irr)
      implicit none
c
c----CAMx v4.42 070603
c
c-----------------------------------------------------------------------
c   Description:
c     This routine makes the "chemistry" adjustments to the tracer 
c     species for PSAT.  The adjustments are based on the differences in 
c     concentrations of the regular model species before and after
c     the regular model chemistry.  This is essentially an adjustment
c     for production or decay.
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       igrid     I  grid number
c       icell     I  the X grid location of current cell
c       jcell     I  the X grid location of current cell
c       kcell     I  the vertical grid location of current layer
c       convfac   R  conversion factor used for lower bound value
c       cold      R  array of concentrations at last time step
c       delcon    R  array of change in concentrations total
c       rrxn_irr  R  array of reactions from last chemistry step
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     09/28/03   --gwilson--  Original development
c     05/04/07   --gwilson--  Changed call to cyctnsa so that conversion
c                             factor is set for lower bound values -
c                             the conversion is passed here from chemdriv
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
      integer   igrid
      integer   icell
      integer   jcell
      integer   kcell
      real      convfac
      real      cold(MXSPEC)
      real      delcon(4,MXSPEC)
      real      rrxn_irr(MXRXN)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer idxcel, idx, jdx, idx0, idx2, ihg0, iso2, ialk, itrp, icre
      integer iaro, ihn3, itpn, intr, idx_itpn, idx_intr, idx_ihn3
      integer icg1, icg2, icg3, icg4, icg5, i
      real    delhg2, delALK, delARO, delTRP
      real    delCG1, delCG2, delCG3, delCG4, delCG5
      real    delso2, delps4, sumtpn
      real    sumhg0, sumhg2, sumalk1, sumaro, sumtrp, sumhn3
      real    sumso2, sumps4, sumalk, sumcre, sumntr, sumrgn
      real    rgnconc, tpnconc, cntrconc, hn3conc
      real    alphaNTR, alphaHN3, betaTPN, delRGN
      real    cycTPN, betaRGN, alphaCRES, betaCRES
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
c  --- call routine to calculate the cycle coefficients ---
c
      call cyctpnsa(convfac,cold,delcon,rrxn_irr,alphaNTR,alphaHN3,
     &     betaTPN,cycTPN,betaRGN,alphaCRES,delRGN,delALK,delARO,
     &       delTRP,delCG1,delCG2,delCG3,delCG4,delCG5)
      betaCRES = delcon(1,kcres) - alphaCRES
c
c----------------------------------------------------------------------
c  ---  SOA tracers ----
c----------------------------------------------------------------------
c
      if( lsoa ) then
c
c  --- get the sum of the tracer species for ARO/CG1 yield ---
c
         icg1 = iptcls(idxipt(ITRCG1)) - 1
         sumaro = 0.
         do i=iptcls(idxipt(ITRARO)),nptcls(idxipt(ITRARO))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            icg1 = icg1 + 1
            sumaro = sumaro + ptconc(idx) * yrates(icg1)
         enddo
c
c  --- make adjustment to CG1 tracers based on change in CG1 and
c      yield rates for ARO ---
c
         iaro = iptcls(idxipt(ITRARO)) - 1
         do i=iptcls(idxipt(ITRCG1)),nptcls(idxipt(ITRCG1))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            iaro = iaro + 1
            jdx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(iaro-1)
            ptconc(idx) = ptconc(idx) + delCG1 * 
     &                              (ptconc(jdx)*yrates(i)) / sumaro
            ptconc(idx) = MAX(BNDLPT,ptconc(idx))
         enddo
c
c  --- get the sum of the tracer species for ARO/CG2 yield ---
c
         icg2 = iptcls(idxipt(ITRCG2)) - 1
         sumaro = 0.
         do i=iptcls(idxipt(ITRARO)),nptcls(idxipt(ITRARO))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            icg2 = icg2 + 1
            jdx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(icg2-1)
            sumaro = sumaro + ptconc(idx) * yrates(icg2)
         enddo
c
c  --- make adjustment to CG2 tracers based on change in CG2 and
c      yield rates for ARO ---
c
         iaro = iptcls(idxipt(ITRARO)) - 1
         do i=iptcls(idxipt(ITRCG2)),nptcls(idxipt(ITRCG2))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            iaro = iaro + 1
            jdx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(iaro-1)
            ptconc(idx) = ptconc(idx) + delCG2 * 
     &                              (ptconc(jdx)*yrates(i)) / sumaro
            ptconc(idx) = MAX(BNDLPT,ptconc(idx))
         enddo
c
c  --- get the sum of the tracer species for ALK/CG3 yield ---
c
         icg3 = iptcls(idxipt(ITRCG3)) - 1
         sumalk = 0.
         do i=iptcls(idxipt(ITRALK)),nptcls(idxipt(ITRALK))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            icg3 = icg3 + 1
            jdx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(icg3-1)
            sumalk = sumalk + ptconc(idx) * yrates(icg3)
         enddo
c
c  --- make adjustment to CG3 tracers based on change in CG3 and
c      yield rates for ALK ---
c
         ialk = iptcls(idxipt(ITRALK)) - 1
         do i=iptcls(idxipt(ITRCG3)),nptcls(idxipt(ITRCG3))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            ialk = ialk + 1
            jdx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(ialk-1)
            ptconc(idx) = ptconc(idx) + delCG3 * 
     &                              (ptconc(jdx)*yrates(i)) / sumalk
            ptconc(idx) = MAX(BNDLPT,ptconc(idx))
         enddo
c
c  --- get the sum of the tracer species for TRP ---
c
         sumtrp = 0.
         do i=iptcls(idxipt(ITRTRP)),nptcls(idxipt(ITRTRP))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            sumtrp = sumtrp + ptconc(idx)
         enddo
c
c  --- make adjustment to CG4 tracers based on change in CG4 and
c      distribution of TRP ----
c
         itrp = iptcls(idxipt(ITRTRP)) - 1
         do i=iptcls(idxipt(ITRCG4)),nptcls(idxipt(ITRCG4))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            itrp = itrp + 1
            jdx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(itrp-1)
            ptconc(idx) = ptconc(idx) + delCG4 * ptconc(jdx) / sumtrp
            ptconc(idx) = MAX(BNDLPT,ptconc(idx))
         enddo
c
c  --- get the sum of the tracer species for CRE ---
c
         sumcre = 0.
         do i=iptcls(idxipt(ITRCRE)),nptcls(idxipt(ITRCRE))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            sumcre = sumcre + ptconc(idx) 
         enddo
c
c  --- get the sum of the tracer species for ARO ---
c
         icre = iptcls(idxipt(ITRCRE)) - 1
         sumaro = 0.
         do i=iptcls(idxipt(ITRARO)),nptcls(idxipt(ITRARO))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            icre = icre + 1
            sumaro = sumaro + ptconc(idx)*yrates(icre)
         enddo
c
c  --- make adjustment to CG5 tracers based on change in CG5 and
c      distribution of CRE ----
c
         icre = iptcls(idxipt(ITRCRE)) - 1
         do i=iptcls(idxipt(ITRCG5)),nptcls(idxipt(ITRCG5))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            icre = icre + 1
            jdx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(icre-1)
            ptconc(idx) = ptconc(idx) + delCG5 * ptconc(jdx) / sumcre
            ptconc(idx) = MAX(BNDLPT,ptconc(idx))
         enddo
c
c  --- make adjustment to CRE tracers based on distrubution 
c      of CRE (destruction) and ARO (production) ---
c
         iaro = iptcls(idxipt(ITRARO)) - 1
         do i=iptcls(idxipt(ITRCRE)),nptcls(idxipt(ITRCRE))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            iaro = iaro + 1
            jdx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(iaro-1)
            ptconc(idx) = ptconc(idx) +
     &              alphaCRES * ptconc(idx) / sumcre +
     &                 betaCRES * ptconc(jdx)*yrates(i) / sumaro
         enddo
c
c  --- get the sum of the tracer species for ALK ---
c
         sumalk1 = 0.
         do i=iptcls(idxipt(ITRALK)),nptcls(idxipt(ITRALK))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            sumalk1 = sumalk1 + ptconc(idx)*wtkoh(i)
         enddo
c
c  --- make adjustment to ALK based on change in PAR and kOH ---
c
         do i=iptcls(idxipt(ITRALK)),nptcls(idxipt(ITRALK))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            ptconc(idx) = ptconc(idx) + delALK * 
     &                             ptconc(idx)*wtkoh(i)  / sumalk1
            ptconc(idx) = MAX(BNDLPT,ptconc(idx))
         enddo
c
c  --- get the sum of the tracer species for ARO ---
c
         sumaro = 0.
         do i=iptcls(idxipt(ITRARO)),nptcls(idxipt(ITRARO))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            sumaro = sumaro + ptconc(idx)*wtkoh(i) 
         enddo
c
c  --- make adjustment to ARO based on change in TOL/XYL and kOH ---
c
         do i=iptcls(idxipt(ITRARO)),nptcls(idxipt(ITRARO))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            ptconc(idx) = ptconc(idx) + delARO * 
     &                                  ptconc(idx)*wtkoh(i) / sumaro
            ptconc(idx) = MAX(BNDLPT,ptconc(idx))
         enddo
c
c  --- get the sum of the tracer species for TRP ---
c
         sumtrp = 0.
         do i=iptcls(idxipt(ITRTRP)),nptcls(idxipt(ITRTRP))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            sumtrp = sumtrp + ptconc(idx)
         enddo
c
c  --- make adjustment to TRP based on change in OLE2 ---
c
         do i=iptcls(idxipt(ITRTRP)),nptcls(idxipt(ITRTRP))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            ptconc(idx) = ptconc(idx) + delTRP * ptconc(idx) / sumtrp
            ptconc(idx) = MAX(BNDLPT,ptconc(idx))
         enddo
      endif
c
c----------------------------------------------------------------------
c  ---  Sulfate tracers ---
c----------------------------------------------------------------------
c
      if( lsulfate) then
         delso2 = delcon(3,kso2)
         delps4 = delcon(3,kpso4)
c
c  --- get the sum of the tracer species for SO2 ---
c
         sumso2 = 0.
         do i=iptcls(idxipt(ITRSO2)),nptcls(idxipt(ITRSO2))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            sumso2 = sumso2 + ptconc(idx)
         enddo
c
c  --- if change is positive, make adjustment to PS4 
c      tracers based on distribtion of SO2 ---
c
         if( delps4 .GT. 0. ) then
            iso2 = iptcls(idxipt(ITRSO2)) - 1
            do i=iptcls(idxipt(ITRPS4)),nptcls(idxipt(ITRPS4))
               idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
               iso2 = iso2 + 1
               jdx = idxcel + 
     &                    ncol(igrid)*nrow(igrid)*nlay(igrid)*(iso2-1)
               ptconc(idx) = ptconc(idx) + delps4 * ptconc(jdx) / sumso2
               ptconc(idx) = MAX(BNDLPT,ptconc(idx))
            enddo
c
c  --- if change is negative, make adjustment to PS4 
c      tracers based on distribtion of PS4 ---
c
         else
c
c  --- get the sum of the tracer species for PS4 ---
c
            sumps4 = 0.
            do i=iptcls(idxipt(ITRPS4)),nptcls(idxipt(ITRPS4))
               idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
               sumps4 = sumps4 + ptconc(idx)
            enddo
            do i=iptcls(idxipt(ITRPS4)),nptcls(idxipt(ITRPS4))
               idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
               ptconc(idx) = ptconc(idx) + delps4 * ptconc(idx) / sumps4
               ptconc(idx) = MAX(BNDLPT,ptconc(idx))
            enddo
         endif
c
c  --- make adjustment to SO2 tracers based on change in SO2 ---
c
         do i=iptcls(idxipt(ITRSO2)),nptcls(idxipt(ITRSO2))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            ptconc(idx) = ptconc(idx) + delso2 * ptconc(idx) / sumso2
            ptconc(idx) = MAX(BNDLPT,ptconc(idx))
         enddo
      endif
c
c----------------------------------------------------------------------
c  ---  Nitrate tracers ---
c----------------------------------------------------------------------
c
      if( lnitrate) then
c
c  --- get the sum of the tracer species for HN3 ----
c
         sumhn3 = 0.
         do i=iptcls(idxipt(ITRHN3)),nptcls(idxipt(ITRHN3))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            sumhn3 = sumhn3 + ptconc(idx)
         enddo
c
c  --- get the sum of the tracer species for TPN ----
c
         sumtpn = 0.
         do i=iptcls(idxipt(ITRTPN)),nptcls(idxipt(ITRTPN))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            sumtpn = sumtpn + ptconc(idx)
         enddo
c
c  --- get the sum of the tracer species for NTR ----
c
         sumntr = 0.
         do i=iptcls(idxipt(ITRNTR)),nptcls(idxipt(ITRNTR))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            sumntr = sumntr + ptconc(idx)
         enddo
c
c  --- get the sum of the tracer species for RGN ----
c
         sumrgn = 0.
         do i=iptcls(idxipt(ITRRGN)),nptcls(idxipt(ITRRGN))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            sumrgn = sumrgn + ptconc(idx)
         enddo
c
c  --- make adjustment to RGN tracers based on saved reaction
c      and distrubution of HN3, TPN and NTR ---
c
         ihn3 = iptcls(idxipt(ITRHN3)) - 1
         itpn = iptcls(idxipt(ITRTPN)) - 1
         intr = iptcls(idxipt(ITRNTR)) - 1
c
         do i=iptcls(idxipt(ITRRGN)),nptcls(idxipt(ITRRGN))
            idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            rgnconc = ptconc(idx)
c
            itpn = itpn + 1
            idx_itpn = idxcel + 
     &                     ncol(igrid)*nrow(igrid)*nlay(igrid)*(itpn-1)
            tpnconc = ptconc(idx_itpn)
c
            intr = intr + 1
            idx_intr = idxcel + 
     &                     ncol(igrid)*nrow(igrid)*nlay(igrid)*(intr-1)
            cntrconc = ptconc(idx_intr)
c
            ihn3 = ihn3 + 1
            idx_ihn3 = idxcel + 
     &                     ncol(igrid)*nrow(igrid)*nlay(igrid)*(ihn3-1)
            hn3conc = ptconc(idx_ihn3)
c
            ptconc(idx) = ptconc(idx) 
     &         + delRGN * rgnconc / sumrgn
     &           + alphaNTR * cntrconc / sumntr
     &             - alphaNTR * rgnconc / sumrgn
     &               + alphaHN3 * hn3conc / sumhn3
     &                 - alphaHN3 * rgnconc / sumrgn
     &                   - cycTPN * rgnconc / sumrgn
     &                     + cycTPN * (rgnconc + tpnconc) / 
     &                                             (sumrgn + sumtpn)
c
            ptconc(idx_ihn3) = ptconc(idx_ihn3)
     &         + (delcon(4,kHNO3) + alphaHN3) * rgnconc / sumrgn
     &                                    - alphaHN3 * hn3conc / sumhn3
c
            ptconc(idx_itpn) = ptconc(idx_itpn)
     &         + betaRGN * rgnconc / sumrgn
     &           - betaTPN * tpnconc / sumtpn
     &              + cycTPN * (rgnconc + tpnconc) / (sumrgn + sumtpn)
     &                                      - cycTPN * tpnconc / sumtpn
c
            ptconc(idx_intr) = ptconc(idx_intr)
     &         + (delcon(2,kNTR) + alphaNTR) * rgnconc / sumrgn
     &                                   - alphaNTR * cntrconc / sumntr
c
            ptconc(idx) = MAX(BNDLPT,ptconc(idx))
            ptconc(idx_ihn3) = MAX(BNDLPT,ptconc(idx_ihn3))
            ptconc(idx_itpn) = MAX(BNDLPT,ptconc(idx_itpn))
            ptconc(idx_intr) = MAX(BNDLPT,ptconc(idx_intr))
         enddo
      endif
c
c----------------------------------------------------------------------
c  ---  Mercury tracers ----
c----------------------------------------------------------------------
c
      if( lmercury ) then
         delhg2 = delcon(3,khg2)

c  --- get the sum of the tracer species ---
c
         sumhg0 = 0.
         do i=iptcls(idxipt(ITRHG0)),nptcls(idxipt(ITRHG0))
            idx0 = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            sumhg0 = sumhg0 + ptconc(idx0)
         enddo
         sumhg2 = 0.
         do i=iptcls(idxipt(ITRHG2)),nptcls(idxipt(ITRHG2))
            idx2 = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
            sumhg2 = sumhg2 + ptconc(idx2)
         enddo
c
c  --- HG0 being oxidized to HG2 ----
c
         if( delhg2 .GT. 0 ) then
            ihg0 = iptcls(idxipt(ITRHG0)) - 1
            do i=iptcls(idxipt(ITRHG2)),nptcls(idxipt(ITRHG2))
               idx2 = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
               ihg0 = ihg0 + 1
               idx0 = idxcel + ncol(igrid)*nrow(igrid)*
     &                                             nlay(igrid)*(ihg0-1)
               ptconc(idx2) = ptconc(idx2) + 
     &                                   delhg2 * ptconc(idx0) / sumhg0
               ptconc(idx2) = MAX(BNDLPT,ptconc(idx2))
               ptconc(idx0) = ptconc(idx0) - 
     &                                   delhg2 * ptconc(idx2) / sumhg2
               ptconc(idx0) = MAX(BNDLPT,ptconc(idx0))
            enddo
c
c  --- HG2 beign reduced to HG0 ----
c
         else
            ihg0 = iptcls(idxipt(ITRHG0)) - 1
            do i=iptcls(idxipt(ITRHG2)),nptcls(idxipt(ITRHG2))
               idx2 = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
               ihg0 = ihg0 + 1
               idx0 = idxcel + ncol(igrid)*nrow(igrid)*
     &                                             nlay(igrid)*(ihg0-1)
               ptconc(idx2) = ptconc(idx2) + 
     &                                  delhg2 * ptconc(idx2) / sumhg2
               ptconc(idx2) = MAX(BNDLPT,ptconc(idx2))
               ptconc(idx0) = ptconc(idx0) - 
     &                                  delhg2 * ptconc(idx0) / sumhg0
               ptconc(idx0) = MAX(BNDLPT,ptconc(idx0))
            enddo
         endif
      endif
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

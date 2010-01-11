      subroutine ratejac5(nstrt,neq1,cncrad,conc,r,rate,loss,jac)
c
c----CAMx v4.42 070603
c
c     RATEJAC computes reaction rate and its Jacobian matrix for fast
c     state species
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Routines Called:
c        none
c
c     Called by:
c        TRAP
c
      implicit none
      include "camx.prm"
      include "chmstry.com"
c
      real loss(MXSPEC+1),gain(MXSPEC+1),rate(MXSPEC+1),
     &     jac(MXSPEC,MXSPEC),conc(MXSPEC+1),
     &     cncrad(MXRADCL),r(MXRXN)
      integer i, j, l, nstrt, neq1, neqtmp
c
      neqtmp = neq1
      neqtmp = min0(neqtmp, nspfst)
      do l=1,neqtmp
        Loss(l) = 0.
        Gain(l) = 0.
      enddo
c
      do i=1,neqtmp
        do j=1,neqtmp
          jac(i,j) = 0.
        enddo
      enddo
c
c  decide whether NO is solved here
c
c
c      NO2, and O3 are solved together
c
      if(neq1.ge.3) then
c
c   NO2    O3
c
        Loss(kNO2  )= +( 1.000)*r(  1)+( 1.000)*r(  5)+( 1.000)*r(  6)
     &                +( 1.000)*r(  8)+( 1.000)*r( 11)+( 1.000)*r( 14)
     &                +( 1.000)*r( 25)+( 1.000)*r( 32)+( 1.000)*r( 69)
     &                +( 1.000)*r( 79)+( 1.000)*r( 90)+( 1.000)*r(102)
     &                +( 1.000)*r(115)+( 1.000)*r(117)+( 1.000)*r(120)
        Gain(kNO2  )= +( 1.000)*r(  4)+( 1.000)*r(  7)+( 2.000)*r(  9)
     &                +( 2.000)*r( 10)+( 1.000)*r( 12)+( 1.000)*r( 14)
     &                +( 1.000)*r( 16)+( 1.000)*r( 23)+( 1.000)*r( 24)
     &                +( 1.000)*r( 26)+( 1.000)*r( 28)+( 1.000)*r( 31)
     &                +( 1.000)*r( 33)+( 0.610)*r( 34)+( 1.000)*r( 35)
     &                +( 0.800)*r( 39)+( 2.000)*r( 40)+( 1.000)*r( 46)
     &                +( 1.000)*r( 48)+( 1.000)*r( 51)+( 1.000)*r( 53)
     &                +( 1.000)*r( 56)+( 1.000)*r( 58)+( 1.000)*r( 65)
     &                +( 1.000)*r( 70)+( 1.000)*r( 71)+( 1.000)*r( 73)
        Gain(kNO2  ) = Gain(kNO2  )
     &                +( 1.000)*r( 80)+( 1.000)*r( 81)+( 1.000)*r( 83)
     &                +( 1.000)*r( 91)+( 1.000)*r( 92)+( 1.000)*r( 94)
     &                +( 1.000)*r(103)+( 1.000)*r(104)+( 1.000)*r(106)
     &                +( 1.000)*r(128)+( 0.338)*r(176)+( 1.000)*r(177)
     &                +( 0.187)*r(191)+( 0.474)*r(195)+( 0.391)*r(210)
        Loss(kO3   )= +( 1.000)*r(  3)+( 1.000)*r(  7)+( 1.000)*r(  8)
     &                +( 1.000)*r( 17)+( 1.000)*r( 18)+( 1.000)*r( 30)
     &                +( 1.000)*r( 36)+( 1.000)*r(162)+( 1.000)*r(167)
     &                +( 1.000)*r(171)+( 1.000)*r(179)+( 1.000)*r(186)
     &                +( 1.000)*r(190)+( 1.000)*r(194)+( 1.000)*r(205)
     &                +( 1.000)*r(209)+( 1.000)*r(215)
        Gain(kO3   )= +( 1.000)*r(  2)+( 0.250)*r( 72)+( 0.250)*r( 82)
     &                +( 0.250)*r( 93)+( 0.250)*r(105)

          JAC(kNO2 ,kNO2 )= +( 1.000)*r(  1)+( 1.000)*r(  5)
     &                      +( 1.000)*r(  6)+( 1.000)*r(  8)
     &                      +( 1.000)*r( 11)+( 1.000)*r( 14)
     &                      +(-1.000)*r( 14)+( 1.000)*r( 25)
     &                      +( 1.000)*r( 32)+( 1.000)*r( 69)
     &                      +( 1.000)*r( 79)+( 1.000)*r( 90)
     &                      +( 1.000)*r(102)+( 1.000)*r(115)
     &                      +( 1.000)*r(117)+( 1.000)*r(120)
          JAC(kNO2 ,kO3  )= +(-1.000)*r(  7)+( 1.000)*r(  8)
          JAC(kO3  ,kNO2 )= +( 1.000)*r(  8)
          JAC(kO3  ,kO3  )= +( 1.000)*r(  3)+( 1.000)*r(  7)
     &                      +( 1.000)*r(  8)+( 1.000)*r( 17)
     &                      +( 1.000)*r( 18)+( 1.000)*r( 30)
     &                      +( 1.000)*r( 36)+( 1.000)*r(162)
     &                      +( 1.000)*r(167)+( 1.000)*r(171)
     &                      +( 1.000)*r(179)+( 1.000)*r(186)
     &                      +( 1.000)*r(190)+( 1.000)*r(194)
     &                      +( 1.000)*r(205)+( 1.000)*r(209)
     &                      +( 1.000)*r(215)
      endif
c
c     PAN is added to the coupled species
c
      if(neq1.ge.4) then
c
c   PAN  PAN2  MPAN  PBZN  NPHE  RNO3  CRES  DCB2  DCB3
c
        Loss(kPAN  )= +( 1.000)*r( 70)
        Gain(kPAN  )= +( 1.000)*r( 69)
        Loss(kPAN2 )= +( 1.000)*r( 80)
        Gain(kPAN2 )= +( 1.000)*r( 79)
        Loss(kMPAN )= +( 1.000)*r(103)
        Gain(kMPAN )= +( 1.000)*r(102)
        Loss(kPBZN )= +( 1.000)*r( 91)
        Gain(kPBZN )= +( 1.000)*r( 90)
        Loss(kNPHE )= +( 1.000)*r(157)
        Gain(kNPHE )= +( 1.000)*r(117)+( 1.000)*r(121)+( 1.000)*r(122)
        Loss(kRNO3 )= +( 1.000)*r(176)+( 1.000)*r(177)
        Gain(kRNO3 )= +( 1.000)*r( 62)+( 1.000)*r(115)+( 0.572)*r(172)
     &                +( 0.310)*r(176)+( 0.813)*r(191)+( 0.276)*r(195)
     &                +( 0.511)*r(206)+( 0.321)*r(210)+( 1.000)*r(216)
        Loss(kCRES )= +( 1.000)*r(155)+( 1.000)*r(156)
        Gain(kCRES )= +( 0.207)*r(202)+( 0.187)*r(203)
        Loss(kDCB2 )= +( 1.000)*r(180)+( 1.000)*r(181)
        Gain(kDCB2 )= +( 0.108)*r(202)+( 0.099)*r(203)
        Loss(kDCB3 )= +( 1.000)*r(182)+( 1.000)*r(183)
        Gain(kDCB3 )= +( 0.051)*r(202)+( 0.093)*r(203)

          JAC(kNO2 ,kPAN )= +(-1.000)*r( 70)
          JAC(kNO2 ,kPAN2)= +(-1.000)*r( 80)
          JAC(kNO2 ,kMPAN)= +(-1.000)*r(103)
          JAC(kNO2 ,kPBZN)= +(-1.000)*r( 91)
          JAC(kNO2 ,kRNO3)= +(-0.338)*r(176)+(-1.000)*r(177)

          JAC(kPAN ,kNO2 )= +(-1.000)*r( 69)
          JAC(kPAN2,kNO2 )= +(-1.000)*r( 79)
          JAC(kMPAN,kNO2 )= +(-1.000)*r(102)
          JAC(kPBZN,kNO2 )= +(-1.000)*r( 90)
          JAC(kNPHE,kNO2 )= +(-1.000)*r(117)
          JAC(kRNO3,kNO2 )= +(-1.000)*r(115)

          JAC(kPAN ,kPAN )= +( 1.000)*r( 70)
          JAC(kPAN2,kPAN2)= +( 1.000)*r( 80)
          JAC(kMPAN,kMPAN)= +( 1.000)*r(103)
          JAC(kPBZN,kPBZN)= +( 1.000)*r( 91)
          JAC(kNPHE,kNPHE)= +( 1.000)*r(157)
          JAC(kRNO3,kRNO3)= +( 1.000)*r(176)+(-0.310)*r(176)
     &                      +( 1.000)*r(177)
          JAC(kCRES,kCRES)= +( 1.000)*r(155)+( 1.000)*r(156)
          JAC(kDCB2,kDCB2)= +( 1.000)*r(180)+( 1.000)*r(181)
          JAC(kDCB3,kDCB3)= +( 1.000)*r(182)+( 1.000)*r(183)
      endif
c
c  add NO during day time or night time when NO is not zero
c
      if(nstrt.eq.1) then
        if(neq1.ge.3) then
c
c    NO
c
        Loss(kNO   )= +( 1.000)*r(  4)+( 1.000)*r(  7)+( 1.000)*r(  9)
     &                +( 2.000)*r( 10)+( 1.000)*r( 21)+( 1.000)*r( 31)
     &                +( 1.000)*r( 46)+( 1.000)*r( 51)+( 1.000)*r( 56)
     &                +( 1.000)*r( 62)+( 1.000)*r( 71)+( 1.000)*r( 81)
     &                +( 1.000)*r( 92)+( 1.000)*r(104)+( 1.000)*r(128)
        Gain(kNO   )= +( 1.000)*r(  1)+( 1.000)*r(  5)+( 1.000)*r( 14)
     &                +( 1.000)*r( 15)+( 1.000)*r( 22)

          JAC(kNO  ,kNO  )= +( 1.000)*r(  4)+( 1.000)*r(  7)
     &                      +( 1.000)*r(  9)+( 4.000)*r( 10)
     &                      +( 1.000)*r( 21)+( 1.000)*r( 31)
     &                      +( 1.000)*r( 46)+( 1.000)*r( 51)
     &                      +( 1.000)*r( 56)+( 1.000)*r( 62)
     &                      +( 1.000)*r( 71)+( 1.000)*r( 81)
     &                      +( 1.000)*r( 92)+( 1.000)*r(104)
     &                      +( 1.000)*r(128)

          JAC(kNO  ,kNO2 )= +(-1.000)*r(  1)+(-1.000)*r(  5)
     &                      +(-1.000)*r( 14)
          JAC(kNO  ,kO3  )= +( 1.000)*r(  7)

          JAC(kNO2 ,kNO  )= +(-1.000)*r(  4)+(-1.000)*r(  7)
     &                      +(-2.000)*r(  9)+(-4.000)*r( 10)
     &                      +(-1.000)*r( 31)+(-1.000)*r( 46)
     &                      +(-1.000)*r( 51)+(-1.000)*r( 56)
     &                      +(-1.000)*r( 71)+(-1.000)*r( 81)
     &                      +(-1.000)*r( 92)+(-1.000)*r(104)
     &                      +(-1.000)*r(128)
          JAC(kO3  ,kNO  )= +( 1.000)*r(  7)
        endif
        if(neq1.ge.4) then


          JAC(kRNO3,kNO  )= +(-1.000)*r( 62)
        endif
      endif
c
c  complete Jacobian terms
c
      do j=1,neqtmp
        do i=1,neqtmp
          jac(i,j) = jac(i,j)/conc(j)
        enddo
      enddo
c
c
      do l=1,neq1
        rate(l) = Gain(l) - Loss(l)
      enddo
c
      return
      end

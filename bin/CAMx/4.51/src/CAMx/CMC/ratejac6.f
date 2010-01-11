      subroutine ratejac6(nstrt,neq1,cncrad,conc,r,rate,loss,jac)
c
c----CAMx v4.51 080522
c
c     RATEJAC computes reaction rate and its Jacobian matrix for fast
c     state species
c
c     Copyright 1996-2008
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
        Loss(kNO2  )= +( 1.000)*r(  1)+( 1.000)*r(  4)+( 1.000)*r(  5)
     &                +( 1.000)*r(  7)+( 1.000)*r( 17)+( 1.000)*r( 18)
     &                +( 1.000)*r( 23)+( 1.000)*r( 28)+( 1.000)*r( 31)
     &                +( 1.000)*r( 89)+( 1.000)*r(104)+( 1.000)*r(118)
     &                +( 1.000)*r(136)+( 1.000)*r(148)
        Gain(kNO2  )= +( 1.000)*r(  3)+( 1.000)*r(  6)+( 1.000)*r( 14)
     &                +( 2.000)*r( 16)+( 1.000)*r( 17)+( 1.000)*r( 21)
     &                +( 2.000)*r( 22)+( 1.000)*r( 26)+( 1.000)*r( 27)
     &                +( 1.000)*r( 30)+( 1.000)*r( 32)+( 1.000)*r( 33)
     &                +( 1.000)*r( 46)+( 1.000)*r( 47)+( 1.000)*r( 49)
     &                +( 2.000)*r( 50)+( 0.610)*r( 51)+( 1.000)*r( 52)
     &                +( 1.000)*r( 53)+( 1.000)*r( 54)+( 1.000)*r( 62)
     &                +( 1.000)*r( 68)+( 1.000)*r( 81)+( 1.000)*r( 88)
     &                +( 1.000)*r( 90)+( 1.000)*r( 91)+( 1.000)*r(103)
        Gain(kNO2  ) = Gain(kNO2  )
     &                +( 1.000)*r(105)+( 1.000)*r(106)+( 1.000)*r(107)
     &                +( 1.000)*r(122)+( 1.000)*r(126)+( 1.000)*r(130)
     &                +( 0.900)*r(132)+( 0.200)*r(147)+( 0.470)*r(156)
        Loss(kO3   )= +( 1.000)*r(  3)+( 1.000)*r(  7)+( 1.000)*r(  8)
     &                +( 1.000)*r(  9)+( 1.000)*r( 12)+( 1.000)*r( 13)
     &                +( 1.000)*r( 49)+( 1.000)*r(121)+( 1.000)*r(125)
     &                +( 1.000)*r(129)+( 1.000)*r(140)+( 1.000)*r(146)
     &                +( 1.000)*r(150)+( 1.000)*r(155)
        Gain(kO3   )= +( 1.000)*r(  2)+( 0.200)*r( 92)+( 0.200)*r(108)

          JAC(kNO2 ,kNO2 )= +( 1.000)*r(  1)+( 1.000)*r(  4)
     &                      +( 1.000)*r(  5)+( 1.000)*r(  7)
     &                      +( 1.000)*r( 17)+(-1.000)*r( 17)
     &                      +( 1.000)*r( 18)+( 1.000)*r( 23)
     &                      +( 1.000)*r( 28)+( 1.000)*r( 31)
     &                      +( 1.000)*r( 89)+( 1.000)*r(104)
     &                      +( 1.000)*r(118)+( 1.000)*r(136)
     &                      +( 1.000)*r(148)
          JAC(kNO2 ,kO3  )= +(-1.000)*r(  3)+( 1.000)*r(  7)
     &                      +(-1.000)*r( 49)
          JAC(kO3  ,kNO2 )= +( 1.000)*r(  7)
          JAC(kO3  ,kO3  )= +( 1.000)*r(  3)+( 1.000)*r(  7)
     &                      +( 1.000)*r(  8)+( 1.000)*r(  9)
     &                      +( 1.000)*r( 12)+( 1.000)*r( 13)
     &                      +( 1.000)*r( 49)+( 1.000)*r(121)
     &                      +( 1.000)*r(125)+( 1.000)*r(129)
     &                      +( 1.000)*r(140)+( 1.000)*r(146)
     &                      +( 1.000)*r(150)+( 1.000)*r(155)
      endif
c
c     PAN is added to the coupled species
c
      if(neq1.ge.4) then
c
c   PAN  PANX
c
        Loss(kPAN  )= +( 1.000)*r( 90)+( 1.000)*r( 91)
        Gain(kPAN  )= +( 1.000)*r( 89)
        Loss(kPANX )= +( 1.000)*r(105)+( 1.000)*r(106)+( 1.000)*r(107)
        Gain(kPANX )= +( 1.000)*r(104)

          JAC(kNO2 ,kPAN )= +(-1.000)*r( 90)+(-1.000)*r( 91)
          JAC(kNO2 ,kPANX)= +(-1.000)*r(105)+(-1.000)*r(106)
     &                      +(-1.000)*r(107)

          JAC(kPAN ,kNO2 )= +(-1.000)*r( 89)
          JAC(kPANX,kNO2 )= +(-1.000)*r(104)

          JAC(kPAN ,kPAN )= +( 1.000)*r( 90)+( 1.000)*r( 91)
          JAC(kPANX,kPANX)= +( 1.000)*r(105)+( 1.000)*r(106)
     &                      +( 1.000)*r(107)
      endif
c
c  add NO during day time or night time when NO is not zero
c
      if(nstrt.eq.1) then
        if(neq1.ge.3) then
c
c    NO
c
        Loss(kNO   )= +( 1.000)*r(  3)+( 1.000)*r(  6)+( 1.000)*r( 16)
     &                +( 2.000)*r( 22)+( 1.000)*r( 23)+( 1.000)*r( 24)
     &                +( 1.000)*r( 30)+( 1.000)*r( 54)+( 1.000)*r( 55)
     &                +( 1.000)*r( 68)+( 1.000)*r( 81)+( 1.000)*r( 88)
     &                +( 1.000)*r(103)+( 1.000)*r(132)
        Gain(kNO   )= +( 1.000)*r(  1)+( 1.000)*r(  4)+( 1.000)*r( 15)
     &                +( 1.000)*r( 17)+( 1.000)*r( 25)+( 1.000)*r( 27)
     &                +( 0.200)*r(148)

          JAC(kNO  ,kNO  )= +( 1.000)*r(  3)+( 1.000)*r(  6)
     &                      +( 1.000)*r( 16)+( 4.000)*r( 22)
     &                      +( 1.000)*r( 23)+( 1.000)*r( 24)
     &                      +( 1.000)*r( 30)+( 1.000)*r( 54)
     &                      +( 1.000)*r( 55)+( 1.000)*r( 68)
     &                      +( 1.000)*r( 81)+( 1.000)*r( 88)
     &                      +( 1.000)*r(103)+( 1.000)*r(132)

          JAC(kNO  ,kNO2 )= +(-1.000)*r(  1)+(-1.000)*r(  4)
     &                      +(-1.000)*r( 17)+( 1.000)*r( 23)
     &                      +(-0.200)*r(148)
          JAC(kNO  ,kO3  )= +( 1.000)*r(  3)

          JAC(kNO2 ,kNO  )= +(-1.000)*r(  3)+(-1.000)*r(  6)
     &                      +(-2.000)*r( 16)+(-4.000)*r( 22)
     &                      +( 1.000)*r( 23)+(-1.000)*r( 30)
     &                      +(-1.000)*r( 54)+(-1.000)*r( 68)
     &                      +(-1.000)*r( 81)+(-1.000)*r( 88)
     &                      +(-1.000)*r(103)+(-0.900)*r(132)
          JAC(kO3  ,kNO  )= +( 1.000)*r(  3)
        endif
        if(neq1.ge.4) then


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

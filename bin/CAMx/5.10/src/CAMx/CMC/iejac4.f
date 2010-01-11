      subroutine iejac4(neq,t,y,ml,mu,jac_ie,njac)
      use lsbox
      implicit none
c
c-----CAMx v5.10 090918
c
c     IEJAC4 computes a Jacobian for the IEH solver
c
c     Copyright 1996 - 2009ENVIRON International Corp.
c     Created by the CMC version 5.2
c
c     Routines Called:
c        IERXN4
c
c     Called by:
c        LSODE
c
      include "camx.prm"
      include "chmdat.com"
      include "iehchem.com"
      include "lsbox.com"
c
      integer ml, mu, njac, neq(3), ny, nk, i, j, l
      real t, tmp, H2O, M, O2, CH4, H2, N2
      real y(neq(2)+6)
      real jac_ie(neq(1),neq(1))
      real r(neq(3))
      real loss(MXSPEC+MXRADCL+1)
      real gain(MXSPEC+MXRADCL+1)
c
c --- Entry point
c
      ny = neq(2)
      nk = neq(3)
      H2O = y(ny+2)
      M   = y(ny+3)
      O2  = y(ny+4)
      CH4 = y(ny+5)
      H2  = y(ny+6)
      N2  = M - O2
c
      do l=1,ny
        Loss(l) = 0.0
        Gain(l) = 0.0
      enddo
c
c --- Get the reaction rates
c
      call ierxn4(y,ny,r,rrk,nk)
c
c --- Solve the steady state species
c
c
c   O1D
c
        Loss(iO1D  )= +( 1.000)*r( 10)+( 1.000)*r( 11)+( 1.000)*r( 97)
c
        Gain(iO1D  )= +( 1.000)*r(  9)
c
      if (loss(iO1D).gt.1.0e-25.or.loss(iO1D).lt.-1.0e-25) then
        y(iO1D) = gain(iO1D)/loss(iO1D)*y(iO1D)
      else
        y(iO1D) = 0.0
      endif
      r( 10) = rrk( 10)*y(iO1D)
      r( 11) = rrk( 11)*y(iO1D)*H2O
      r( 97) = rrk( 97)*y(iO1D)*H2
c
c     O
c
        Loss(iO    )= +( 1.000)*r(  2)+( 1.000)*r(  4)+( 1.000)*r(  5)
     &                +( 1.000)*r(  6)+( 1.000)*r( 40)+( 1.000)*r( 42)
     &                +( 1.000)*r( 56)+( 1.000)*r( 60)+( 1.000)*r( 75)
     &                +( 1.000)*r( 99)+( 1.000)*r(102)+( 1.000)*r(103)
     &                +( 1.000)*r(104)
c
        Gain(iO    )= +( 1.000)*r(  1)+( 1.000)*r(  8)+( 1.000)*r( 10)
     &                +( 0.890)*r( 14)+( 1.000)*r(100)
c
      if (loss(iO).gt.1.0e-25.or.loss(iO).lt.-1.0e-25) then
        y(iO) = gain(iO)/loss(iO)*y(iO)
      else
        y(iO) = 0.0
      endif
      r(  2) = rrk(  2)*y(iO)
      r(  4) = rrk(  4)*y(iO)*y(iNO2)
      r(  5) = rrk(  5)*y(iO)*y(iNO2)
      r(  6) = rrk(  6)*y(iO)*y(iNO)
      r( 40) = rrk( 40)*y(iFORM)*y(iO)
      r( 42) = rrk( 42)*y(iALD2)*y(iO)
      r( 56) = rrk( 56)*y(iO)*y(iOLE)
      r( 60) = rrk( 60)*y(iO)*y(iETH)
      r( 75) = rrk( 75)*y(iO)*y(iISOP)
      r( 99) = rrk( 99)*y(iOH)*y(iO)
      r(102) = rrk(102)*y(iHO2)*y(iO)
      r(103) = rrk(103)*y(iH2O2)*y(iO)
      r(104) = rrk(104)*y(iNO3)*y(iO)
c
c --- Calculate the Jacobian
c
c
c  N2O5   NO3    OH   HO2  C2O3   XO2  XO2N   TO2   ROR   CRO
c    NO   NO2    O3   PAN
c

          jac(iN2O5,iN2O5)= +( 1.000)*r( 18)+( 1.000)*r( 19)
     &                      +( 1.000)*r(111)
          jac(iNO3 ,iN2O5)= +(-1.000)*r( 19)+(-1.000)*r(111)
          jac(iNO2 ,iN2O5)= +(-1.000)*r( 19)+(-1.000)*r(111)

          jac(iN2O5,iNO3 )= +(-1.000)*r( 17)
          jac(iNO3 ,iNO3 )= +( 1.000)*r( 14)+( 1.000)*r( 15)
     &                      +( 1.000)*r( 16)+( 1.000)*r( 17)
     &                      +( 1.000)*r( 41)+( 1.000)*r( 44)
     &                      +( 1.000)*r( 59)+( 1.000)*r( 67)
     &                      +( 1.000)*r( 78)+( 1.000)*r( 94)
     &                      +( 1.000)*r(104)+( 1.000)*r(105)
     &                      +( 1.000)*r(106)+( 1.000)*r(107)
     &                      +( 4.000)*r(108)
          jac(iOH  ,iNO3 )= +( 1.000)*r(105)
          jac(iHO2 ,iNO3 )= +(-1.000)*r( 41)+(-0.800)*r( 78)
     &                      +(-0.925)*r( 94)+(-1.000)*r(105)
     &                      +( 1.000)*r(106)
          jac(iC2O3,iNO3 )= +(-1.000)*r( 44)+(-0.075)*r( 94)
          jac(iXO2 ,iNO3 )= +(-0.910)*r( 59)+(-1.000)*r( 78)
     &                      +(-0.075)*r( 94)
          jac(iXO2N,iNO3 )= +(-0.090)*r( 59)
          jac(iCRO ,iNO3 )= +(-1.000)*r( 67)
          jac(iNO  ,iNO3 )= +(-0.110)*r( 14)+( 1.000)*r( 15)
     &                      +(-1.000)*r( 16)
          jac(iNO2 ,iNO3 )= +(-0.890)*r( 14)+(-2.000)*r( 15)
     &                      +( 1.000)*r( 16)+(-1.000)*r( 16)
     &                      +( 1.000)*r( 17)+(-1.000)*r( 59)
     &                      +(-0.200)*r( 78)+(-1.000)*r(104)
     &                      +(-1.000)*r(105)+(-1.000)*r(107)
     &                      +(-4.000)*r(108)
          jac(iO3  ,iNO3 )= +( 1.000)*r(107)

          jac(iNO3 ,iOH  )= +(-1.000)*r( 27)+( 1.000)*r(105)
          jac(iOH  ,iOH  )= +( 1.000)*r( 12)+( 1.000)*r( 22)
     &                      +( 1.000)*r( 24)+( 1.000)*r( 26)
     &                      +( 1.000)*r( 27)+( 1.000)*r( 31)
     &                      +( 1.000)*r( 35)+( 1.000)*r( 36)
     &                      +( 1.000)*r( 37)+( 1.000)*r( 43)
     &                      +( 1.000)*r( 51)+( 1.000)*r( 52)
     &                      +( 1.000)*r( 57)+( 1.000)*r( 61)
     &                      +( 1.000)*r( 63)+( 1.000)*r( 66)
     &                      +( 1.000)*r( 70)+( 1.000)*r( 72)
          jac(iOH  ,iOH  )=jac(iOH  ,iOH  )
     &                      +( 1.000)*r( 73)+( 1.000)*r( 76)
     &                      +( 1.000)*r( 82)+( 1.000)*r( 84)
     &                      +( 1.000)*r( 85)+( 1.000)*r( 90)
     &                      +( 1.000)*r( 92)+( 1.000)*r( 98)
     &                      +( 1.000)*r( 99)+( 4.000)*r(100)
     &                      +( 4.000)*r(101)+( 1.000)*r(105)
          jac(iHO2 ,iOH  )= +(-1.000)*r( 12)+(-1.000)*r( 35)
     &                      +(-1.000)*r( 36)+(-1.000)*r( 37)
     &                      +(-1.000)*r( 51)+(-0.110)*r( 52)
     &                      +(-1.000)*r( 57)+(-1.000)*r( 61)
     &                      +(-0.440)*r( 63)+(-0.600)*r( 66)
     &                      +(-2.000)*r( 70)+(-0.700)*r( 72)
     &                      +(-0.912)*r( 76)+(-1.000)*r( 82)
     &                      +(-1.000)*r( 84)+(-1.000)*r( 85)
     &                      +( 1.000)*r( 90)+(-0.503)*r( 92)
          jac(iHO2 ,iOH  )=jac(iHO2 ,iOH  )
     &                      +(-1.000)*r( 98)+(-1.000)*r( 99)
     &                      +(-1.000)*r(105)
          jac(iC2O3,iOH  )= +(-1.000)*r( 43)+(-1.000)*r( 70)
     &                      +(-1.000)*r( 73)+(-0.498)*r( 92)
          jac(iXO2 ,iOH  )= +(-1.000)*r( 51)+(-0.870)*r( 52)
     &                      +(-1.000)*r( 57)+(-1.000)*r( 61)
     &                      +(-0.080)*r( 63)+(-0.600)*r( 66)
     &                      +(-1.000)*r( 70)+(-0.500)*r( 72)
     &                      +(-1.000)*r( 73)+(-0.991)*r( 76)
     &                      +(-0.713)*r( 92)
          jac(iXO2N,iOH  )= +(-0.130)*r( 52)+(-0.088)*r( 76)
          jac(iTO2 ,iOH  )= +(-0.560)*r( 63)+(-0.300)*r( 72)
          jac(iROR ,iOH  )= +(-0.760)*r( 52)
          jac(iCRO ,iOH  )= +(-0.400)*r( 66)
          jac(iNO  ,iOH  )= +( 1.000)*r( 22)
          jac(iNO2 ,iOH  )= +(-1.000)*r( 24)+( 1.000)*r( 26)
     &                      +(-1.000)*r( 31)+(-1.000)*r(105)
          jac(iO3  ,iOH  )= +( 1.000)*r( 12)

          jac(iNO3 ,iHO2 )= +( 1.000)*r(106)
          jac(iOH  ,iHO2 )= +(-1.000)*r( 13)+(-1.000)*r( 28)
     &                      +(-0.790)*r( 50)+( 1.000)*r( 90)
     &                      +(-1.000)*r(102)
          jac(iHO2 ,iHO2 )= +( 1.000)*r( 13)+( 1.000)*r( 28)
     &                      +( 1.000)*r( 29)+( 4.000)*r( 32)
     &                      +( 4.000)*r( 33)+( 1.000)*r( 50)
     &                      +(-0.790)*r( 50)+( 1.000)*r( 86)
     &                      +( 1.000)*r( 87)+( 1.000)*r( 90)
     &                      +( 1.000)*r(102)+( 1.000)*r(106)
          jac(iC2O3,iHO2 )= +( 1.000)*r( 50)
          jac(iXO2 ,iHO2 )= +(-0.790)*r( 50)+( 1.000)*r( 86)
          jac(iXO2N,iHO2 )= +( 1.000)*r( 87)
          jac(iNO  ,iHO2 )= +( 1.000)*r( 28)
          jac(iNO2 ,iHO2 )= +(-1.000)*r( 28)+( 1.000)*r( 29)
          jac(iO3  ,iHO2 )= +( 1.000)*r( 13)

          jac(iOH  ,iC2O3)= +(-0.790)*r( 50)
          jac(iHO2 ,iC2O3)= +(-1.000)*r( 46)+(-4.000)*r( 49)
     &                      +( 1.000)*r( 50)+(-0.790)*r( 50)
          jac(iC2O3,iC2O3)= +( 1.000)*r( 46)+( 1.000)*r( 47)
     &                      +( 4.000)*r( 49)+( 1.000)*r( 50)
          jac(iXO2 ,iC2O3)= +(-1.000)*r( 46)+(-4.000)*r( 49)
     &                      +(-0.790)*r( 50)
          jac(iNO  ,iC2O3)= +( 1.000)*r( 46)
          jac(iNO2 ,iC2O3)= +(-1.000)*r( 46)+( 1.000)*r( 47)
          jac(iPAN ,iC2O3)= +(-1.000)*r( 47)

          jac(iHO2 ,iXO2 )= +( 1.000)*r( 86)
          jac(iXO2 ,iXO2 )= +( 1.000)*r( 79)+( 4.000)*r( 80)
     &                      +( 1.000)*r( 86)+( 1.000)*r( 89)
          jac(iXO2N,iXO2 )= +( 1.000)*r( 89)
          jac(iNO  ,iXO2 )= +( 1.000)*r( 79)
          jac(iNO2 ,iXO2 )= +(-1.000)*r( 79)

          jac(iHO2 ,iXO2N)= +( 1.000)*r( 87)
          jac(iXO2 ,iXO2N)= +( 1.000)*r( 89)
          jac(iXO2N,iXO2N)= +( 1.000)*r( 81)+( 1.000)*r( 87)
     &                      +( 4.000)*r( 88)+( 1.000)*r( 89)
          jac(iNO  ,iXO2N)= +( 1.000)*r( 81)

          jac(iHO2 ,iTO2 )= +(-0.900)*r( 64)+(-1.000)*r( 65)
          jac(iTO2 ,iTO2 )= +( 1.000)*r( 64)+( 1.000)*r( 65)
          jac(iNO  ,iTO2 )= +( 1.000)*r( 64)
          jac(iNO2 ,iTO2 )= +(-0.900)*r( 64)

          jac(iHO2 ,iROR )= +(-0.940)*r( 53)+(-1.000)*r( 54)
          jac(iXO2 ,iROR )= +(-0.960)*r( 53)
          jac(iXO2N,iROR )= +(-0.040)*r( 53)
          jac(iROR ,iROR )= +( 1.000)*r( 53)+( 1.000)*r( 54)
     &                      +( 1.000)*r( 55)
          jac(iNO2 ,iROR )= +( 1.000)*r( 55)

          jac(iCRO ,iCRO )= +( 1.000)*r( 68)+( 1.000)*r( 91)
          jac(iNO2 ,iCRO )= +( 1.000)*r( 68)

          jac(iNO3 ,iNO  )= +( 1.000)*r( 15)
          jac(iOH  ,iNO  )= +( 1.000)*r( 22)+(-1.000)*r( 28)
          jac(iHO2 ,iNO  )= +( 1.000)*r( 28)+(-1.000)*r( 46)
     &                      +(-0.900)*r( 64)
          jac(iC2O3,iNO  )= +( 1.000)*r( 46)
          jac(iXO2 ,iNO  )= +(-1.000)*r( 46)+( 1.000)*r( 79)
          jac(iXO2N,iNO  )= +( 1.000)*r( 81)
          jac(iTO2 ,iNO  )= +( 1.000)*r( 64)
          jac(iNO  ,iNO  )= +( 1.000)*r(  3)+( 1.000)*r(  6)
     &                      +( 1.000)*r( 15)+( 4.000)*r( 20)
     &                      +( 1.000)*r( 21)+( 1.000)*r( 22)
     &                      +( 1.000)*r( 28)+( 1.000)*r( 46)
     &                      +( 1.000)*r( 64)+( 1.000)*r( 79)
     &                      +( 1.000)*r( 81)
          jac(iNO2 ,iNO  )= +(-1.000)*r(  3)+(-1.000)*r(  6)
     &                      +(-2.000)*r( 15)+(-4.000)*r( 20)
     &                      +( 1.000)*r( 21)+(-1.000)*r( 28)
     &                      +(-1.000)*r( 46)+(-0.900)*r( 64)
     &                      +(-1.000)*r( 79)
          jac(iO3  ,iNO  )= +( 1.000)*r(  3)

          jac(iN2O5,iNO2 )= +(-1.000)*r( 17)
          jac(iNO3 ,iNO2 )= +(-1.000)*r(  5)+(-1.000)*r(  7)
     &                      +( 1.000)*r( 16)+( 1.000)*r( 17)
          jac(iOH  ,iNO2 )= +( 1.000)*r( 26)
          jac(iHO2 ,iNO2 )= +( 1.000)*r( 29)+(-0.800)*r( 96)
          jac(iC2O3,iNO2 )= +( 1.000)*r( 47)
          jac(iXO2 ,iNO2 )= +(-1.000)*r( 96)
          jac(iROR ,iNO2 )= +( 1.000)*r( 55)
          jac(iCRO ,iNO2 )= +( 1.000)*r( 68)
          jac(iNO  ,iNO2 )= +(-1.000)*r(  1)+(-1.000)*r(  4)
     &                      +(-1.000)*r( 16)+( 1.000)*r( 21)
     &                      +(-0.200)*r( 96)
          jac(iNO2 ,iNO2 )= +( 1.000)*r(  1)+( 1.000)*r(  4)
     &                      +( 1.000)*r(  5)+( 1.000)*r(  7)
     &                      +( 1.000)*r( 16)+(-1.000)*r( 16)
     &                      +( 1.000)*r( 17)+( 1.000)*r( 21)
     &                      +( 1.000)*r( 26)+( 1.000)*r( 29)
     &                      +( 1.000)*r( 47)+( 1.000)*r( 55)
     &                      +( 1.000)*r( 68)+( 1.000)*r( 96)
          jac(iO3  ,iNO2 )= +( 1.000)*r(  7)
          jac(iPAN ,iNO2 )= +(-1.000)*r( 47)

          jac(iNO3 ,iO3  )= +(-1.000)*r(  7)+( 1.000)*r(107)
          jac(iOH  ,iO3  )= +( 1.000)*r( 12)+(-1.000)*r( 13)
     &                      +(-0.100)*r( 58)+(-0.080)*r( 71)
     &                      +(-0.266)*r( 77)+(-0.268)*r( 93)
          jac(iHO2 ,iO3  )= +(-1.000)*r( 12)+( 1.000)*r( 13)
     &                      +(-0.440)*r( 58)+(-0.120)*r( 62)
     &                      +(-0.760)*r( 71)+(-0.066)*r( 77)
     &                      +(-0.154)*r( 93)
          jac(iC2O3,iO3  )= +(-0.620)*r( 71)+(-0.200)*r( 77)
     &                      +(-0.114)*r( 93)
          jac(iXO2 ,iO3  )= +(-0.220)*r( 58)+(-0.030)*r( 71)
     &                      +(-0.200)*r( 77)+(-0.064)*r( 93)
          jac(iNO  ,iO3  )= +( 1.000)*r(  3)
          jac(iNO2 ,iO3  )= +(-1.000)*r(  3)+( 1.000)*r(  7)
     &                      +(-1.000)*r(107)
          jac(iO3  ,iO3  )= +( 1.000)*r(  3)+( 1.000)*r(  7)
     &                      +( 1.000)*r(  8)+( 1.000)*r(  9)
     &                      +( 1.000)*r( 12)+( 1.000)*r( 13)
     &                      +( 1.000)*r( 58)+( 1.000)*r( 62)
     &                      +( 1.000)*r( 71)+( 1.000)*r( 77)
     &                      +( 1.000)*r( 93)+( 1.000)*r(107)

          jac(iC2O3,iPAN )= +(-1.000)*r( 48)+(-1.000)*r(109)
          jac(iNO2 ,iPAN )= +(-1.000)*r( 48)+(-1.000)*r(109)
          jac(iPAN ,iPAN )= +( 1.000)*r( 48)+( 1.000)*r(109)
c
c --- Put the Jacobian in right form for LSODE
c
      do j = 1,neq(1)
        tmp = y(j)
        if( tmp.LT.1.0e-25 .AND. tmp.GT.-1.0e-25 ) then
          tmp = 1.0
        endif
        do i = 1,neq(1)
          jac_ie(i,j) = -jac(i,j)/tmp
        enddo
      enddo
c
      return
      end

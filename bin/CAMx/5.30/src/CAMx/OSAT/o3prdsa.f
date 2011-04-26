c*** O3PRDSA
c
      subroutine o3prdsa(rrxn_irr,convfac,delo3,prdo3n,prdo3v,desto3)
      use filunit
      use chmstry
      implicit none
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c   Description:
c     This routine calculates the production/destruction of ozone
c     based on the chemical mechanism.  The production is in two
c     parts NOx-attributed and VOC-attributed.
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       rrxn_irr   R  array of reactions from last chemistry step
c       delo3      R  change in Ozone
c       convfac    R  conversion factor (Mmoles to ug/m^3)
c     Outputs:
c       prdo3n     R  ozone production attributed to NOx
c       prdo3v     R  ozone production attributed to VOC
c       desto3     R  ozone destruction
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     07/20/04   --gwilson--  Original development
c     02/21/05   --gyarwood-  Add SAPRC mechanism
c     10/06/05   --gyarwood-  Removed mechanism 2
c     01/08/06   --bkoo--     Added Mechanism 6 (CB05)
c                             Added MBUT rxns to SAPRC99 code
c     08/22/06   --gyarwood-  Revised ozone destruction calculation 
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
      real rrxn_irr(*)
      real delo3
      real convfac
      real prdo3n
      real prdo3v
      real desto3
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      real prodo3, phno3, pho2h, ho2term
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- Estimate daytime ozone destruction due to:
c       (1) O1D + water 
c       (2) HO2 + O3
c       (3) OH + O3 --> HO2 + O2 (subtracting HO2 reacting with NO)
c       (4) O(3P) + VOC 
c       (5) O3 + VOC
c     Get the rates for OH + NO2 and HO2 + HO2
c
      if( idmech .EQ. 1 .OR. idmech .EQ. 3 .OR. idmech .EQ. 4 ) then
c
c --- CB4 mechanism 3 ---
c     
         desto3 =  - rrxn_irr( 11)
c
         desto3 = desto3 - rrxn_irr( 12)
c
         ho2term= 2.*(rrxn_irr( 32)+rrxn_irr( 33))
     &              + rrxn_irr( 86)+rrxn_irr( 87)+rrxn_irr( 90)
     &              + rrxn_irr( 50)
         desto3 = desto3 - ( rrxn_irr( 13) 
     &               * ho2term / ( ho2term + rrxn_irr( 28) ) )
c
         desto3 = desto3 -
     &             ( rrxn_irr( 40)+rrxn_irr( 42)+rrxn_irr( 56)+
     &               rrxn_irr( 60)+rrxn_irr( 75) )
c
         desto3 = desto3 -
     &             ( rrxn_irr( 58)+rrxn_irr( 62)+rrxn_irr( 71)+
     &               rrxn_irr( 77)+rrxn_irr( 93) )
         phno3 = rrxn_irr( 26)
         pho2h = rrxn_irr( 32) + rrxn_irr( 33)
      elseif( idmech .EQ. 5 ) then
c
c --- SAPRC99 Mechanism 5 ---
c
         desto3 =  - rrxn_irr( 19)
c
         desto3 = desto3 - rrxn_irr( 36)
c
         ho2term= 2.*(rrxn_irr( 37)+rrxn_irr( 38))
     &              + rrxn_irr( 43)+rrxn_irr( 47)+rrxn_irr( 52)
     &              + rrxn_irr( 57)+rrxn_irr( 63)+rrxn_irr(118)
     &              + rrxn_irr(121)+rrxn_irr(126)
     &         +0.75*(rrxn_irr( 72)+rrxn_irr( 82)+rrxn_irr( 93)
     &              + rrxn_irr(105))
         desto3 = desto3 - ( rrxn_irr( 30) * ho2term
     &          / ( ho2term + rrxn_irr( 31) + 0.8*rrxn_irr( 39) ) )
c
         desto3 = desto3 -
     &             ( rrxn_irr(164)+rrxn_irr(168)+rrxn_irr(188)+
     &               rrxn_irr(192)+rrxn_irr(196)+rrxn_irr(207)+
     &               rrxn_irr(211)+rrxn_irr(217) )
c
         desto3 = desto3 -
     &             ( rrxn_irr(162)+rrxn_irr(167)+rrxn_irr(171)+
     &               rrxn_irr(179)+rrxn_irr(186)+rrxn_irr(190)+
     &               rrxn_irr(194)+rrxn_irr(205)+rrxn_irr(209)+
     &               rrxn_irr(215) )
         phno3 = rrxn_irr(25)
         pho2h = rrxn_irr(37) + rrxn_irr(38)
      elseif( idmech .EQ. 6 ) then
c
c --- CB05 Mechanism 6 ---
c
         desto3 =  - rrxn_irr( 11)
c
         desto3 = desto3 - rrxn_irr( 13)
c
         ho2term=     2.0*(rrxn_irr( 34)+rrxn_irr( 35))
     &              + rrxn_irr( 43)+rrxn_irr( 48)+rrxn_irr( 56)
     &              + rrxn_irr( 57)+rrxn_irr( 69)+rrxn_irr( 79)
     &              + rrxn_irr( 82)
     &              + 0.8*(rrxn_irr( 92)+rrxn_irr(108))
         desto3 = desto3 - ( rrxn_irr( 12) * ho2term
     &          / ( ho2term + rrxn_irr( 30) + rrxn_irr( 44) ) )
c
         desto3 = desto3 -
     &             ( rrxn_irr( 77)+rrxn_irr( 84)+rrxn_irr( 99)+
     &               rrxn_irr(119)+rrxn_irr(123)+rrxn_irr(127)+
     &               rrxn_irr(144)+rrxn_irr(153) )
c
         desto3 = desto3 -
     &             ( rrxn_irr(121)+rrxn_irr(125)+rrxn_irr(129)+
     &               rrxn_irr(140)+rrxn_irr(146)+rrxn_irr(150)+
     &               rrxn_irr(155) )
         phno3 = rrxn_irr(28)
         pho2h = rrxn_irr(34) + rrxn_irr(35)
      else
         write(iout,'(//,a)') 'ERROR in O3PRDSA:'
         write(iout,'(/,1X,A,I10)')
     &             'Unknown chemical mechanism ID number: ',idmech
         write(iout,'(/,1X,2A)')
     &             'Ozone source apportionment is not available ',
     &             'for this chemical mechanism'
         call camxerr()
      endif
c
c --- Assign ozone production to VOC or NOx ---
c
      desto3 = AMIN1(desto3*convfac,delo3)
      prodo3 = delo3 - desto3
      if( pho2h/phno3 .LE. 0.35) then
        prdo3n = 0.0
        prdo3v = prodo3
      else
        prdo3n = prodo3
        prdo3v = 0.0
      endif

c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

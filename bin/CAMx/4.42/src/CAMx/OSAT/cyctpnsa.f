c*** CYCTPNSA
c
      subroutine cyctpnsa(convfac,cold,delcon,rrxn_irr,alphaNTR,alphaHN3,
     &          betaTPN,cycTPN,betaRGN,alphaCRES,delRGN,delALK,delARO,
     &              delTRP,delCG1,delCG2,delCG3,delCG4,delCG5)
c
c----CAMx v4.42 070603
c
c-----------------------------------------------------------------------
c   Description:
c     This routine calculates the coefficients used in the chemistry
c     calculations of the nitrate species in PSAT.
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       convfac    R  conversion factor used for lower bound value
c       cold       R  array of concentrations at last time step
c       delcon     R  array of change in concentrations total
c       rrxn_irr   R  array of reactions from last chemistry step
c     Outputs:
c       alphaHN3   R  coefficient used in chemistry adjustment 
c                     calculations
c       alphaNTR   R  coefficient used in chemistry adjustment 
c                     calculations
c       betaTPN    R  coefficient used in chemistry adjustment 
c                     calculations
c       cycTPN     R  coefficient used in chemistry adjustment 
c                     calculations
c       betaRGN    R  coefficient used in chemistry adjustment 
c                     calculations
c       alphaCRES  R  coefficient used in chemistry adjustment 
c                     calculations
c       delRGN     R  change in concentration over RGN species
c       delALK     R  change in concentration over ALK species
c       delARO     R  change in concentration over ARO species
c       delTRP     R  change in concentration over TRP species
c       delCG1     R  change in concentration over CG1 species
c       delCG2     R  change in concentration over CG2 species
c       delCG3     R  change in concentration over CG3 species
c       delCG4     R  change in concentration over CG4 species
c       delCG5     R  change in concentration over CG5 species
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     09/28/03   --gwilson--  Original development
c     05/17/07	 --gwilson--  Changes to improve handling of small
c                             concentrations near lower bounds
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'chmstry.com'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real convfac
      real cold(MXSPEC)
      real delcon(4,MXSPEC)
      real rrxn_irr(MXRXN)
      real alphaHN3
      real alphaNTR
      real betaTPN
      real cycTPN
      real betaRGN
      real alphaCRES
      real delRGN
      real delALK
      real delARO
      real delTRP
      real delCG1
      real delCG2
      real delCG3
      real delCG4
      real delCG5
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      real FUZZ
      parameter (FUZZ = 10.0)
      real lossPAN, lossPNA, lossPAN2, lossPBZN, lossMPAN, lossHNO4
      real prodPAN, prodPNA, prodPAN2, prodPBZN, prodMPAN, prodHNO4
      real cycPAN, cycPNA, cycPAN2, cycPBZN, cycMPAN, cycHNO4
      real delTPN
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  ---- caclulate the cycle TPN and delta values for the
c       SAPRC99 mechanism ------
c
      if( idmech .EQ. 5 ) then
c
         delALK = delcon(1,kalk4) + delcon(1,kalk5)
         delARO = delcon(1,karo1) + delcon(1,karo2)
         delTRP = delcon(1,kterp)
         delCG1 = delcon(1,kcg1)
         delCG2 = delcon(1,kcg2)
         delCG3 = delcon(1,kcg3)
         delCG4 = delcon(1,kcg4)
         delCG5 = delcon(1,kcg5)
c
         delRGN = delcon(2,kno) + delcon(2,kno2) +
     &                             delcon(2,knxoy) + delcon(2,khono)
c
         alphaHN3 = rrxn_irr(27) + rrxn_irr(28)
         if( cold(khno3) .LT. FUZZ*convfac*bdnl(khno3) ) alphaHN3 = 0.0
c
         alphaNTR = rrxn_irr(176) + rrxn_irr(177)
         if( cold(krno3) .LT. FUZZ*convfac*bdnl(krno3) ) alphaNTR = 0.0
c
         alphaCRES = -AMIN1( rrxn_irr(155) + rrxn_irr(156), cold(kcres))
c
         lossPAN = cold(kpan) * (1 - EXP(-rrxn_irr(70)/cold(kpan)) )
         prodPAN = cold(kno2) * (1 - EXP(-rrxn_irr(69)/cold(kpan)) )
         cycPAN = 0.5 * AMIN1(lossPAN, prodPAN)
c
         lossPAN2 = cold(kpan2) * (1 - EXP(-rrxn_irr(80)/cold(kpan2)) )
         prodPAN2 = cold(kno2) * (1 - EXP(-rrxn_irr(79)/cold(kpan2)) )
         cycPAN2 = 0.5 * AMIN1(lossPAN2, prodPAN2)
c
         lossPBZN = cold(kpbzn) * (1 - EXP(-rrxn_irr(91)/cold(kpbzn)) )
         prodPBZN = cold(kno2) * (1 - EXP(-rrxn_irr(90)/cold(kpbzn)) )
         cycPBZN = 0.5 * AMIN1(lossPBZN, prodPBZN)
c
         lossMPAN = cold(kmpan) * (1 - EXP(-rrxn_irr(103)/cold(kmpan)) )
         prodMPAN = cold(kno2) * (1 - EXP(-rrxn_irr(102)/cold(kmpan)) )
         cycMPAN = 0.5 * AMIN1(lossMPAN, prodMPAN)
c
         lossHNO4 = cold(khno4) * 
     &          (1 - EXP(-(rrxn_irr(33)+rrxn_irr(34)+rrxn_irr(35))
     &                                                  /cold(kpna)) )
         prodHNO4 = cold(kno2) * (1 - EXP(-rrxn_irr(32)/cold(khno4)) )
         cycHNO4 = 0.5 * AMIN1(lossHNO4, prodHNO4)
c
         sumTPN = cold(kpan) + cold(kpan2) + cold(kpbzn) + 
     &                                        cold(kmpan) + cold(khno4)
         cycTPN = cold(kpan)/sumTPN * cycPAN + 
     &               cold(kpan2)/sumTPN * cycPAN2 + 
     &                     cold(kpbzn)/sumTPN * cycPBZN + 
     &                        cold(kmpan)/sumTPN * cycMPAN + 
     &                           cold(khno4)/sumTPN * cycHNO4 
         if( sumTPN .LT. FUZZ*convfac*
     &        ( bdnl(kpan)+bdnl(kpan2)+bdnl(kpbzn)
     &                       +bdnl(kmpan)+bdnl(khno4) )
     &                                                  ) cycTPN = 0.0
c
         delTPN = delcon(2,kpan) + delcon(2,kpan2) + delcon(2,kpbzn) +
     &                                delcon(2,kmpan) + delcon(2,khno4)
c
c  ---- caclulate the cycle TPN and delta values for the
c       CBIV mechanism ------
c
      else
        delALK = delcon(1,kpar)
        delARO = delcon(1,ktol) + delcon(1,kxyl)
        delTRP = delcon(1,kole2)
        delCG1 = delcon(1,kcg1)
        delCG2 = delcon(1,kcg2)
        delCG3 = delcon(1,kcg3)
        delCG4 = delcon(1,kcg4)
        delCG5 = delcon(1,kcg5)
c
        delRGN = delcon(2,kno) + delcon(2,kno2) +
     &                             delcon(2,knxoy) + delcon(2,khono)

        alphaCRES = -AMIN1( rrxn_irr(66) + rrxn_irr(67), cold(kcres) )
c
        alphaNTR = rrxn_irr(116)
        if( cold(kntr) .LT. FUZZ*convfac*bdnl(kntr) ) alphaNTR = 0.0
c
        alphaHN3 = rrxn_irr(27) + rrxn_irr(114)
        if( cold(khno3) .LT. FUZZ*convfac*bdnl(khno3) ) alphaHN3 = 0.0
c
        lossPAN = cold(kpan) * 
     &         (1 - EXP(-(rrxn_irr(48)+rrxn_irr(113))/cold(kpan)) )
        prodPAN = cold(kno2) * (1 - EXP(-rrxn_irr(47)/cold(kno2)) )
        cycPAN = 0.5 * AMIN1(lossPAN, prodPAN)
c
        lossPNA = cold(kpna) * 
     &         (1 - EXP(-(rrxn_irr(30)+rrxn_irr(31)+rrxn_irr(117))
     &                                               /cold(kpna)) )
        prodPNA = cold(kno2) * (1 - EXP(-rrxn_irr(29)/cold(kno2)) )
        cycPNA = 0.5 * AMIN1(lossPNA, prodPNA)
c
        sumTPN = cold(kpan) + cold(kpna)
        cycTPN = cold(kpan)/sumTPN * cycPAN + cold(kpna)/sumTPN * cycPNA
        sumTPN = cold(kpan) + cold(kpna)
        if( sumTPN .LT. FUZZ*convfac*
     &                            (bdnl(kpan)+bdnl(kpna)) ) cycTPN = 0.0
c
        delTPN = delcon(2,kpan) + delcon(2,kpna)
      endif
c
c   --- set the output variables and return ---
c
      if( ( cold(kno)+cold(kno2)+cold(khono)+cold(knxoy) ) .LT.
     &                        FUZZ*convfac*( bdnl(kno)+bdnl(kno2)+
     &                                 bdnl(khono)+bdnl(knxoy) ) ) then
         alphaNTR = 0.0
         alphaHN3 = 0.0
         cycTPN   = 0.0
      endif
c
c   --- set the output variables and return ---
c
      cycTPN  = AMIN1( cold(kno2)-delTPN, sumTPN+delTPN, cycTPN )
      betaTPN = AMAX1( -delTPN, 0.) + cycTPN
      betaRGN = AMAX1(  delTPN, 0.) + cycTPN
      cycTPN  = 0.
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

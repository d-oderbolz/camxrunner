c**** KRTC
c
      subroutine krtc(temp, pres)
      use rtcmcchm
c 
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c 
c     KRTC calculates temperature/pressure-dependent rate constants
c     for the RTRAC CMC solver.  The rate expressions follow the
c     CAMx conventions
c
c     The expression types supported are:
c       Type 1 Temperature independent
c       Type 2 UAM/OZIPM format Arrhenius
c       Type 3 Generalized temperature dependent
c       Type 4 Troe teperature/pressure dependent
c       Type 5 Equilibrium constant ratio to another reaction
c       Type 6 Lindemann-Hinshelwood used for OH + HNO3
c       Type 7 k = k1 + k2[M] used for OH + CO and HO2 self-reaction
c 
c    Copyright 1996-2007
c    ENVIRON International Corporation
c
c     Argument description:
c      Inputs:
c       temp         R     temperature (K)
c       pres         R     pressure (mbar)
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    07/06/07   --gyarwood--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c    Argument declaration:
c-----------------------------------------------------------------------
c
      real    temp, pres
c 
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      double precision darren
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      real*8   cf1
      parameter (cf1 = 2.462d13)
c
      integer  i, j, k, iord, ityp, iref
      real*8   cf, cfm, ppmpres, factor, dtemp, dpres, t2h
      real*8   rktmp, rk0, rk1, rk2, rk3, ratio
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      dtemp = DBLE(temp)
      dpres = DBLE(pres)
c
c --- Conversions from molecule cm-3 to ppm and from hr to min to sec
c       cf1 from gas law: (n/V) = NP/RT
c       cf1 = (6.022e23 * 1.013e5 * 1e-12 ) / (8.314 * 298.0)
c     The cf1 conversion is at 298 K and 1013 mb and the
c     adjustment "factor" below generalises to other conditions
c
      if( itunit .EQ. 1 ) then
         t2h = 3600.0D0
      elseif( itunit .EQ. 2 ) then
         t2h = 60.0D0
      else
         t2h = 1.0D0
      endif
c
      do i = 1,nrxnrtc
        ityp = ityprtc(i)
        iord = MAX(1, nrct(i))
        if (icunit .EQ. 1) then
          cf  = cf1**(iord-1)
          cfm = cf1**(iord)
        else
          cf  = 1.0D0
          cfm = 1.0D0
        endif
        ppmpres = 1.0D6*dpres/1013.0D0
        factor = ( (298.D0/dtemp)*(pres/1013.D0) )**(iord-1)
c
c --- For reference: function darren(a,ea,b,tref,temp)
c                    darren = a*((temp/tref)**b)*dexp(-ea/temp)
c
        if (ityp.EQ.1) then
c         Type 1 Temperature independent
          rkrtc(i) = rkprmrtc(i,1)*cf*t2h
        elseif (ityp.EQ.2) then
c         Type 2 UAM/OZIPM format Arrhenius
          rktmp = rkprmrtc(i,2)*(1.0D0/298.0D0 - 1.0D0/dtemp)
          rkrtc(i) = rkprmrtc(i,1)*DEXP(rktmp)*cf*t2h
        elseif (ityp.eq.3) then
c         Type 3 Generalized temperature dependent
          rkrtc(i) = darren(DBLE(rkprmrtc(i,1)), DBLE(rkprmrtc(i,2)),
     &            DBLE(rkprmrtc(i,3)), DBLE(rkprmrtc(i,4)),dtemp)*cf*t2h
        elseif (ityp.eq.4) then
c         Type 4 Troe teperature/pressure dependent
          rk0 = darren(DBLE(rkprmrtc(i,1)), DBLE(rkprmrtc(i,2)),
     &      DBLE(rkprmrtc(i,3)), DBLE(rkprmrtc(i,4)),dtemp)*cfm*ppmpres
          ratio = rk0/(darren(DBLE(rkprmrtc(i,5)), DBLE(rkprmrtc(i,6)),
     &      DBLE(rkprmrtc(i,7)), DBLE(rkprmrtc(i,8)),dtemp)*cf)
          rkrtc(i) = 
     &      (rk0/(1.D0+ratio))*DBLE(rkprmrtc(i,9))**(1.D0/(1.D0+
     &      (0.43429D0*log(ratio)/DBLE(rkprmrtc(i,10)))**2))*t2h
        elseif (ityp.eq.5) then
c         Type 5 Equilibrium constant ratio to another reaction
          factor = 1.D0
          iref = nint(rkprmrtc(i,1))
          if (icunit .EQ. 1)
     &       cf  = cf1**(MAX(1, nrct(iref))-iord)
          rktmp = darren(DBLE(rkprmrtc(i,2)), DBLE(rkprmrtc(i,3)),
     &            DBLE(rkprmrtc(i,4)), DBLE(rkprmrtc(i,5)),dtemp)*cf
          rkrtc(i) = rkrtc(iref) / rktmp 
        elseif (ityp.eq.6) then
c         Type 6 Lindemann-Hinshelwood used for OH + HNO3
          rk1 = darren(DBLE(rkprmrtc(i,1)), DBLE(rkprmrtc(i,2)),
     &          DBLE(rkprmrtc(i,3)), DBLE(rkprmrtc(i,4)),dtemp)*cf
          rk2 = darren(DBLE(rkprmrtc(i,5)), DBLE(rkprmrtc(i,6)),
     &          DBLE(rkprmrtc(i,7)), DBLE(rkprmrtc(i,8)),dtemp)*cf
          rk3 = darren(DBLE(rkprmrtc(i,9)), DBLE(rkprmrtc(i,10)),
     &          DBLE(rkprmrtc(i,11)), DBLE(rkprmrtc(i,12)),dtemp)*cfm
          rkrtc(i) = ( rk1 + (rk3*ppmpres /
     &          (1.D0 + (rk3*ppmpres / rk2) ) ) )*t2h
        elseif (ityp.eq.7) then
c         Type 7 k = k1 + k2[M] used for OH + CO and HO2 self-reaction
          rk1 = darren(DBLE(rkprmrtc(i,1)), DBLE(rkprmrtc(i,2)),
     &          DBLE(rkprmrtc(i,3)), DBLE(rkprmrtc(i,4)),dtemp)*cf
          rk2 = darren(DBLE(rkprmrtc(i,5)), DBLE(rkprmrtc(i,6)),
     &          DBLE(rkprmrtc(i,7)), DBLE(rkprmrtc(i,8)),dtemp)*cfm
          rkrtc(i) = ( rk1 + rk2*ppmpres )*t2h
        endif
        rkrtc(i) =  rkrtc(i)*factor
        srkrtc(i) =  SNGL( rkrtc(i) )
      enddo
c
      return
      end

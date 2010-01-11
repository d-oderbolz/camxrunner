c**** KSCI
c
      subroutine ksci(temp, pres)
      use rtcmcchm
c 
c----CAMx v5.10 090918
c 
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     KSCI calculates temperature/pressure-dependent rate constants
c     for the RTRAC CMC solver.  The rate expressions follow the
c     SCICHEM conventions
c
c     The SCICHEM expression types supported are:
c
c     ID_RAD  = 0 Radiation dependent reaction rate identifier
c     ID_CONS = 1 Constant reaction rate identifier
c     ID_TEMP = 2 Temperature dependent reaction rate identifier
c     ID_PRES = 3 Pressure dependent reaction rate identifier
c     ID_H2O  = 4 H2O dependent reaction rate identifier
c     ID_M    = 5 M dependent reaction rate identifier
c     ID_LWC  = 6 LWC (and drop diam) dependent reaction rate identifier
c     ID_PRES2= 7 Pressure (type 2) dependent reaction rate identifier
c     ID_EQM  = 8 reverse decomposition rate dependent on forward rate
c     ID_O2   = 9 O2 dependent reaction rate identifier
c     ID_N2   =10 N2 dependent reaction rate identifier
c     ID_FOFF1=11 Falloff (type 1) reaction rate identifier
c     ID_FOFF2=12 Falloff (type 2) reaction rate identifier
c     ID_FOFF3=13 Falloff (type 3) reaction rate identifier
c     ID_CH4  =14 CH4 dependent reaction rate identifier
c     ID_H2OB =15 H2O dependent (type 2) reaction rate identifier
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
      real*8     cf1
      parameter (cf1 = 2.462D13)
c
      integer    i, iord, ityp, iref
      real*8     cf, cfm, rktmp, ppmpres, factor, t2h
      real*8     dpres, dtemp, dtemp2, dtemp3
      real*8     ratio, rk0, rk1, rk2, rk3
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      dpres = DBLE(pres)
      dtemp = DBLE(temp)
      dtemp2 = dtemp*dtemp
      dtemp3 = dtemp2*dtemp
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
        iord = max(1, nrct(i))
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
c --- Note that SCICHEM rate expression types are offset by +100
c     For reference: function darren(a,ea,b,tref,temp)
c                    darren = a*((temp/tref)**b)*dexp(-ea/temp)
c
        rkrtc(i) = 0.0D0
c
        if (ityp.EQ.101) then
          rkrtc(i) = rkprmrtc(i,1)*cf*t2h
c
        elseif (ityp.EQ.102) then
          rkrtc(i) = darren(DBLE(rkprmrtc(i,1)),DBLE(-rkprmrtc(i,2)),
     &                      DBLE(-rkprmrtc(i,3)),1.0D0,dtemp)*cf*t2h
c
        elseif (ityp.EQ.103) then
          rk0 = darren(DBLE(rkprmrtc(i,1)),0.0D0,DBLE(-rkprmrtc(i,2)),
     &                               1.0D0,dtemp)*cfm*ppmpres
          ratio = rk0/(darren(DBLE(rkprmrtc(i,3)),0.0D0,
     &                             DBLE(-rkprmrtc(i,4)),1.0D0,dtemp)*cf)
          rkrtc(i) = (rk0/(1.0D0+ratio))*0.6D0**(1.0D0/(1.0D0+
     &                                        LOG10(ratio)**2))*t2h
c
        elseif (ityp.EQ.104) then
          rkrtc(i) = darren(DBLE(rkprmrtc(i,1)),DBLE(-rkprmrtc(i,2)),
     &                      DBLE(-rkprmrtc(i,3)),1.0D0,dtemp)*t2h*cf/cf1
c
        elseif (ityp.EQ.105 .OR. ityp.EQ.109 
     &     .OR. ityp.EQ.110 .OR. ityp.EQ.114 .OR. ityp.EQ.115) then
          rkrtc(i) = darren(DBLE(rkprmrtc(i,1)),DBLE(-rkprmrtc(i,2)),
     &                      DBLE(-rkprmrtc(i,3)),1.0D0,dtemp)*cf*t2h
c
        elseif (ityp.EQ.107) then
          rkrtc(i) = DBLE(rkprmrtc(i,1))
     &                    * ( 1.0D0 + 0.6D0*ppmpres/1.0D6 )*t2h
c
        elseif (ityp.EQ.108) then
          factor = 1.0D0
          iref = NINT(rkprmrtc(i,1))
          if (icunit .EQ. 2) 
     &       cf  = cf1**(nrct(iref)-iord)
          rktmp = darren(DBLE(rkprmrtc(i,2)),
     &               DBLE(-rkprmrtc(i,3)),0.0D0,1.0D0,dtemp)*cf
          rkrtc(i) = rkrtc(iref) / rktmp 
c
        elseif (ityp.EQ.111 .OR. ityp.EQ.112) then
          rk0 = darren(DBLE(rkprmrtc(i,1)),DBLE(-rkprmrtc(i,2)),
     &              DBLE(-rkprmrtc(i,3)),1.0D0,dtemp)*cfm*ppmpres
          ratio = rk0/(darren(DBLE(rkprmrtc(i,4)),DBLE(-rkprmrtc(i,5)),
     &                        DBLE(-rkprmrtc(i,6)),1.0D0,dtemp)*cf)
          rkrtc(i) = (rk0/(1.0D0+ratio))*0.6D0**(1.0D0/(1.0D0+
     &                                        LOG10(ratio)**2))*t2h
c
        elseif (ityp.EQ.113) then
          rk1 = darren(DBLE(rkprmrtc(i,1)),DBLE(-rkprmrtc(i,2)),
     &                      0.0D0,1.0D0,dtemp)*cf
          rk2 = darren(DBLE(rkprmrtc(i,3)),DBLE(-rkprmrtc(i,4)),
     &                      0.0D0,1.0D0,dtemp)*cf
          rk3 = darren(DBLE(rkprmrtc(i,5)),DBLE(-rkprmrtc(i,6)),
     &                      0.0D0,1.0D0,dtemp)*cfm
          rkrtc(i) = ( rk1 + (rk3*ppmpres /
     &                    (1.0D0 + (rk3*ppmpres / rk2) ) ) )*t2h
c
        elseif (ityp.EQ.116) then
          rk0 = darren(DBLE(rkprmrtc(i,1)),DBLE(-rkprmrtc(i,2)),
     &                               0.0D0,1.0D0,dtemp)*cfm*ppmpres
          ratio = rk0/(darren(DBLE(rkprmrtc(i,3)),DBLE(-rkprmrtc(i,4)),
     &                                          0.0D0,1.0D0,dtemp)*cf)
          rk1 = darren(DBLE(rkprmrtc(i,5)),DBLE(-rkprmrtc(i,6)),
     &                      0.0D0,1.0D0,dtemp)*cf
          rkrtc(i) = rk1*(rk0/(1.0D0+ratio))*0.6D0**(1.0D0/(1.0D0+
     &                                        LOG10(ratio)**2))*t2h
c
        elseif (ityp.EQ.117) then
          rkrtc(i) = (DBLE(rkprmrtc(i,1))+DBLE(rkprmrtc(i,2))*dtemp+
     &         DBLE(rkprmrtc(i,3))*dtemp2+DBLE(rkprmrtc(i,4))*dtemp3+
     &           DBLE(rkprmrtc(i,5))/dtemp+DBLE(rkprmrtc(i,6))/dtemp2+
     &                               DBLE(rkprmrtc(i,7))/dtemp3)*cf*t2h
c
        endif
        rkrtc(i) =  rkrtc(i)*factor
        srkrtc(i) =  SNGL( rkrtc(i) )
      enddo
c
      return
      end

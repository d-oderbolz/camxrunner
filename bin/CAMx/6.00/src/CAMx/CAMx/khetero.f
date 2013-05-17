      subroutine khetero(tcell,H2O,con)
      use chmstry
      use filunit
      implicit none
c
c----CAMx v6.00 130506
c
c     KHETERO estimates the rate constant for the heterogeneous
c     hydrolysis of N2O5 which replaces the gas-phase kN2O5 if
c     the former is greater than the latter.
c
c     minimum/maximum N2O5 uptake coefficients are taken from
c     Brown et al. (2006) Science, 311, 67-70.
c
c     mean molecular speed formula - Seinfeld and Pandis (1998)
c     Atmospheric Chemistry and Physics, p453.
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Modifications:
c        01/30/2006  - bkoo -     Original development
c        07/04/2007  - bkoo -     Modified to use pointer to N2O5 hydrolysis
c                                 Extended for CMU_AERO
c        03/29/2011  -cemery-     Revised to allow for inert PM with gas-phase
c                                 chemistry
c        03/28/2012  -cemery-     Revised to set rate of 0.25/hr in the
c                                 absence of PM chemistry
c
c     Input arguments:
c        tcell          temperature (K)
c        H2O            water vapor concentration (ppm)
c        con            species concentrations (ppm or ug/m3)
c
c     Output arguments:
c        NONE
c
c     Routines called:
c        NONE
c
c     Called by:
c        CHEMDRIV
c
      include 'camx.prm'
c
c-----Arguments
c
      real  tcell, H2O
      real  con(*)
c
c-----Local variables:
c
      real  Rgc, Pi, mwN2O5, gamma1, gamma2
      parameter ( Rgc    = 8.314 )        ! gas constant [J/mol-K]
      parameter ( Pi     = 3.1415927 )    ! Pi
      parameter ( mwN2O5 = 0.108 )        ! MW of N2O5 [kg/mol]
      parameter ( gamma1 = 0.001 )        ! minimum uptake coefficient
      parameter ( gamma2 = 0.017 )        ! maximum uptake coefficient

      real  vt          ! total dry volume of fine particles
      real  diam        ! diameter of a fine particle [um]
      real  nden        ! number density of fine particles [#/cm3_air]
      real  aratio      ! acidic ratio (1 <= [NH4+]/[SO4=] <= 2)
      real  gamma       ! N2O5 uptake coefficient
      real  cbarN2O5    ! mean molecular speed of N2O5 [m/s]
      real  saden       ! surface area density [um2/cm3_air]
      real  khetN2O5    ! rate constant of N2O5 heterogeneous rxn [1/min]

      integer     l, isempty
      integer     kwtr,isec,iaero
c
c-----Entry point
c
      khetN2O5 = 0.0
      cbarN2O5 = sqrt( 8.0*Rgc*tcell / (Pi*mwN2O5) )
c
c-----CF PM chemistry
c
      if (aeropt.eq.'CF') then
        if (con(kph2o).le.bdnl(kph2o)) goto 800

        isempty = 1
        vt = 0.
        do l = ngas+1,nspec
          if (dcut(l,2).lt.(dcut(kph2o,2)+1.e-5)) then      ! fine
            if (l.ne.kph2o) then                            ! dry
              if (con(l).gt.bdnl(l)) isempty = 0
              vt = vt + con(l)/roprt(l)                     ! [ug/m3_air]/[g/m3]
            endif
          endif
        enddo
        if (isempty.eq.1) goto 800

        diam = sqrt(dcut(kph2o,1)*dcut(kph2o,2))            ! geometric mean dry diameter
        nden = vt*1.E6 / (diam*diam*diam*Pi/6.0)            ! [#/cm3_air]
        diam = diam * ( 1.0 +
     &         con(kph2o)/roprt(kph2o)/vt )**0.33333        ! wet diameter [um]
        aratio = con(kpnh4)/con(kpso4)
        aratio = amax1(amin1(aratio, 2.0), 1.0)
        gamma = (gamma1-gamma2)*(aratio-1.0) + gamma2
        saden = Pi*diam*diam*nden                           ! [um2/cm3_air]
        khetN2O5 = gamma*cbarN2O5*saden*6.E-5 / 4.0         ! [1/min]
c
c-----CMU PM chemistry
c
      elseif (aeropt.eq.'CMU') then
        kwtr = (kph2o_1 - ngas)/nbin + 1
        if (nbin.eq.1) kwtr = kph2o_1 - ngas
        do isec = 1, nbin
          if (con(kph2o_1+isec-1).le.bdnl(kph2o_1+isec-1)) cycle
          isempty = 1
          vt = 0.
          do iaero = 1, naero
            if (iaero.ne.kwtr) then
              l = ngas + (iaero - 1)*nbin + isec
              if (con(l).gt.bdnl(l)) isempty = 0
              vt = vt + con(l)/roprt(l)
            endif
          enddo
          if (isempty.eq.1) cycle

          diam = sqrt(dcut(ngas+isec,1)*dcut(ngas+isec,2))
          nden = vt*1.E6 / (diam*diam*diam*Pi/6.0)
          diam = diam * ( 1.0 +
     &           con(kph2o_1+isec-1)/roprt(kph2o_1+isec-1)/vt )**0.33333
          aratio = con(kpnh4_1+isec-1)/con(kpso4_1+isec-1)
          aratio = amax1(amin1(aratio, 2.0), 1.0)
          gamma = (gamma1-gamma2)*(aratio-1.0) + gamma2
          saden = Pi*diam*diam*nden
          khetN2O5 = khetN2O5 + gamma*cbarN2O5*saden*6.E-5 / 4.0
        enddo
c
c-----No PM chemistry; use IUPAC 2006 N2O5 + H2O rate
c
      else
        khetN2O5 = 2.5e-22*8.87e16*H2O/60.
      endif
c
c --- adjust rk(N2O5); rk(N2O5) will be reset by KTHERM every timestep
c
 800  khetN2O5 = amin1( khetN2O5, 0.1 )                     ! check against lower bound
      khetN2O5 = khetN2O5 * 60.0 / H2O                      ! convert to [1/ppm-hr]

      rk(ihydrxn) = amax1( rk(ihydrxn), khetN2O5 )

      return

      end

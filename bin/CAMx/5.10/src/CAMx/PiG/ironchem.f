      subroutine ironchem(dt,dtpig,aero_flag,wpuff,tpuff,ppuff,cwpuff,
     &                    phpuff,conpin,crad,avgcnc,avgrad,convfac,
     &                    ierr1,ierr2,ierr3,pig_flag)
      use chmstry
      implicit none
c
c----CAMx v5.10 090918
c
c     IRONCHEM performs chemistry on the current puff reactor for one
c     time step.  It uses the LSODE solver exclusively for the gas-phase.
c
c     Local array element con(nspec+1) is used to store the concentrations
c     of non-used species, i.e., species that are in the chem solver but
c     not on the species list for this run.
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Modifications:
c        10/06/05        Removed mechanism 2
c        01/12/06        Added PM chemistry calls for AERO_CF
c        12/26/06        Removed AEROCHEM_AQ (merged into AEROCHEM_CF)
c        01/08/07        Added mechanism 6 (CB05)
c        02/21/08        Added coupling time for puff aerosol chemistry
c        03/31/08        Puff killed before PM chemistry for concs < lower bound
c        05/13/08        Modified to skip ISOROPIA for background chemistry.
c
c     Input arguments:
c        dt                  gas/aqueous chemistry timestep (s)
c        dtpig               PiG coupling timestep (hr)
c        aero_flag           flag to determine which PM algorithm to call
c        wpuff               water vapor (ppm)
c        tpuff               temperature (K)
c        ppuff               pressure (mb)
c        cwpuff              cloud liquid water content (g/m3)
c        phpuff              cloud pH
c        conpin              puff species concentration (ppm,ug/m3)
c        crad                radical concentration (ppm)
c        convfac             conversion factor: umol/m3 = ppm * convfac
c        pig_flag            T - skip isorropia chemistry for PiG
c
c     Output arguments:
c        conpin              puff species concentration (ppm,ug/m3)
c        crad                radical concentration (ppm)
c        avgcnc              time-step average puff concentrations
c        avgrad              time-step average radical concentrations
c        ierr1               error code (=1 if LSODE reports error)
c        ierr2               error code (=1 if neg concs for AEROCHEM_AQ)
c        ierr3               error code (=1 if neg concs for AEROCHEM_CF)
c
c     Routines called:
c        LSCALL
c        AEROCHEM_CF
c
c     Called by:
c        PIGDRIVE
c
      include 'camx.prm'
c
c-----Subroutine names to be called by LSODE
c
      external lsrate3
      external lsrate4
      external lsrate5
      external lsrate6
c
      real conpin(*)
      real crad(*)
      real avgcnc(*)
      real avgrad(*)
c
      real rrxn(MXREACT)
      real dt,dtpig,wpuff,tpuff,ppuff,cwpuff,phpuff,convfac,dtchem
      real atm,o2,ch4,h2,cliq,dumarr(5,MXSPEC)
      integer is,l,ierr1,ierr2,ierr3,aero_flag,ierr
      logical pig_flag
c
      real con(MXSPEC+1)
c
c-----Entry point
c
      dtchem = dt/3600.
      con(nspec+1) = 0.
      do is = 1,nspec
        con(is) = conpin(is)
      enddo
c
c-----Chemistry integration, pass subroutines for mechanism used
c
      atm = 1.e6
      O2  = 2.095e5
      CH4 = 1.75
      H2  = 0.50
c
      if (idmech.eq.3) then
        call lscall(lsrate3,dtchem,wpuff,atm,O2,CH4,H2,con,crad,
     &              avgcnc,avgrad,nreact,rrxn,ierr1,.FALSE.,.TRUE.)
      elseif (idmech.eq.4) then
        call lscall(lsrate4,dtchem,wpuff,atm,O2,CH4,H2,con,crad,
     &              avgcnc,avgrad,nreact,rrxn,ierr1,.FALSE.,.TRUE.)
      elseif (idmech.eq.5) then
        call lscall(lsrate5,dtchem,wpuff,atm,O2,CH4,H2,con,crad,
     &              avgcnc,avgrad,nreact,rrxn,ierr1,.FALSE.,.TRUE.)
      elseif (idmech.eq.6) then
        call lscall(lsrate6,dtchem,wpuff,atm,O2,CH4,H2,con,crad,
     &              avgcnc,avgrad,nreact,rrxn,ierr1,.FALSE.,.TRUE.)
      endif
      if (ierr1 .ne. 0) return
c
c-----Perform aerosol chemistry if it's time
c
      if (aero_flag .gt. 0) then
        cliq = cwpuff
        if (tpuff.lt.273.)
     &    cliq = amax1(0.,cliq*(tpuff - tamin)/(273. - tamin))
c
c-----Minimum conc check: kill the puff if below BDNL values
c
        ierr = 0
        if (con(kso2) .lt. 0. .or.
     &      con(khno3).lt. 0. .or.
     &      con(knxoy).lt. 0. .or.
     &      con(knh3) .lt. 0. .or.
     &      con(kh2o2).lt. 0. .or.
     &      con(ko3)  .lt. 0. .or.
     &      con(ksulf).lt. 0. .or.
     &      con(kcg1) .lt. 0. .or.
     &      con(kcg2) .lt. 0. .or.
     &      con(kcg3) .lt. 0. .or.
     &      con(kcg4) .lt. 0. .or.
     &      con(kcg5) .lt. 0. .or.
     &      con(kcg6) .lt. 0. .or.
     &      con(kcg7) .lt. 0.) ierr = 1
        do is = ngas+1,nspec
          if (is.ne.kph2o .and. con(is).lt.0.) ierr = 1
        enddo

        if (ierr.eq.1) then
          if (cliq.ge.cwmin .and. tpuff.ge.tamin) then
            ierr2 = 1
          else
            ierr3 = 1
          endif
          return
        endif

        call aerochem_cf(wpuff,tpuff,ppuff,cliq,phpuff,con,
     &                  convfac,dtchem,dtpig,dumarr,.false.,1,aero_flag,
     &                  pig_flag)

      endif
c
c-----Update input species arrays
c
      do l = 1,nrad
        crad(l) = amax1(crad(l),bdlrad)
      enddo
      do is = 1,nspec
        conpin(is) = con(is)
      enddo
      conpin(nspec+1) = 0.
c
      return
      end

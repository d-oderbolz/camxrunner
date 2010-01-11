      subroutine ironchem(dt,aero_flag,wpuff,tpuff,ppuff,cwpuff,phpuff,
     &                    conpin,crad,convfac,ierr1,ierr2,ierr3)
c
c----CAMx v4.42 070603
c
c     IRONCHEM performs chemistry on the current puff reactor for one
c     time step.  It uses the LSODE solver exclusively for the gas-phase.
c
c     Local array element con(nspec+1) is used to store the concentrations
c     of non-used species, i.e., species that are in the chem solver but
c     not on the species list for this run.
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Modifications:
c        10/06/05        Removed mechanism 2
c        01/12/06        Added PM chemistry calls for AERO_CF
c
c     Input arguments:
c        dt                  timestep (s)
c        aero_flag           flag to determine which PM algorithm to call
c        wpuff               water vapor (ppm)
c        tpuff               temperature (K)
c        ppuff               pressure (mb)
c        cwpuff              cloud liquid water content (g/m3)
c        phpuff              cloud pH
c        conpin              puff species concentration (ppm,ug/m3)
c        crad                radical concentration (ppm)
c        convfac             conversion factor: umol/m3 = ppm * convfac
c
c     Output arguments:
c        conpin              puff species concentration (ppm,ug/m3)
c        crad                radical concentration (ppm)
c        ierr1               error code (=1 if LSODE reports error)
c        ierr2               error code (=1 if neg concs for AEROCHEM_AQ)
c        ierr3               error code (=1 if neg concs for AEROCHEM_CF)
c
c     Routines called:
c        LSCALL
c        AEROCHEM_AQ
c        AEROCHEM_CF
c
c     Called by:
c        PIGDRIVE
c
      implicit none
      include 'camx.prm'
      include 'chmstry.com'
c
c-----Subroutine names to be called by LSODE
c
      external lsrate1,lsjac1
      external lsrate3,lsjac3
      external lsrate4,lsjac4
      external lsrate5,lsjac5
c
      real conpin(MXSPEC+1),crad(MXRADCL),con(MXSPEC+1)
      real dt,wpuff,tpuff,ppuff,cwpuff,phpuff,convfac,dtchem
      real atm,o2,ch4,h2,cliq,dumarr(4,MXSPEC)
      integer is,l,ierr1,ierr2,ierr3,aero_flag
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
      if (idmech.eq.1) then
        call lscall(lsrate1,lsjac1,dtchem,wpuff,atm,O2,CH4,H2,con,crad,
     &              ierr1,.true.)
      elseif (idmech.eq.3) then
        call lscall(lsrate3,lsjac3,dtchem,wpuff,atm,O2,CH4,H2,con,crad,
     &              ierr1,.true.)
      elseif (idmech.eq.4) then
        call lscall(lsrate4,lsjac4,dtchem,wpuff,atm,O2,CH4,H2,con,crad,
     &              ierr1,.true.)
      elseif (idmech.eq.5) then
        call lscall(lsrate5,lsjac5,dtchem,wpuff,atm,O2,CH4,H2,con,crad,
     &              ierr1,.true.)
      endif
c
c-----Perform aerosol chemistry if it's time
c       
      cliq = cwpuff
      if (tpuff.lt.273.) then
        cliq = amax1(0.,cliq*(tpuff - tamin)/(273. - tamin))
      endif
      if (aero_flag.EQ.1 .AND.
     &   (cliq.ge.cwmin .and. tpuff.ge.tamin)) then
        if (con(kso2) .lt. bdnl(kso2)  .or.
     &      con(khno3).lt. bdnl(khno3) .or.
     &      con(knxoy).lt. bdnl(knxoy) .or.
     &      con(knh3) .lt. bdnl(knh3)  .or.
     &      con(kh2o2).lt. bdnl(kh2o2) .or.
     &      con(ko3)  .lt. bdnl(ko3)   .or.
     &      con(ksulf).lt. bdnl(ksulf) .or.
     &      con(kpso4).lt. bdnl(kpso4) .or.
     &      con(kpnh4).lt. bdnl(kpnh4) .or.
     &      con(kpno3).lt. bdnl(kpno3) .or.
     &      con(kna)  .lt. bdnl(kna)   .or.
     &      con(kpcl) .lt. bdnl(kpcl)  .or.
     &      con(khcl) .lt. bdnl(khcl)  .or.
     &      con(kph2o).lt. bdnl(kph2o)) then
          ierr2 = 1
        else
          call aerochem_aq(MXSPEC,nspec,ngas,aeropt,dtchem,wpuff,tpuff,
     &                     ppuff,cliq,phpuff,con,bdnl,convfac,
     &                     .false.,dumarr,.false.,1)
        endif
      elseif (aero_flag.EQ.2) then
        if (con(kso2) .lt. bdnl(kso2)  .or.
     &      con(khno3).lt. bdnl(khno3) .or.
     &      con(knxoy).lt. bdnl(knxoy) .or.
     &      con(knh3) .lt. bdnl(knh3)  .or.
     &      con(kh2o2).lt. bdnl(kh2o2) .or.
     &      con(ko3)  .lt. bdnl(ko3)   .or.
     &      con(ksulf).lt. bdnl(ksulf) .or.
     &      con(kpso4).lt. bdnl(kpso4) .or.
     &      con(kpnh4).lt. bdnl(kpnh4) .or.
     &      con(kpno3).lt. bdnl(kpno3) .or.
     &      con(kna)  .lt. bdnl(kna)   .or.
     &      con(kpcl) .lt. bdnl(kpcl)  .or.
     &      con(khcl) .lt. bdnl(khcl)  .or.
     &      con(kcg1) .lt. bdnl(kcg1)  .or.
     &      con(kcg2) .lt. bdnl(kcg2)  .or.
     &      con(kcg3) .lt. bdnl(kcg3)  .or.
     &      con(kcg4) .lt. bdnl(kcg4)  .or.
     &      con(kcg5) .lt. bdnl(kcg5)  .or.
     &      con(ksoa1).lt. bdnl(ksoa1) .or.
     &      con(ksoa2).lt. bdnl(ksoa2) .or.
     &      con(ksoa3).lt. bdnl(ksoa3) .or.
     &      con(ksoa4).lt. bdnl(ksoa4) .or.
     &      con(ksoa5).lt. bdnl(ksoa5) .or.
     &      con(kpoa) .lt. bdnl(kpoa)  .or.
     &      con(kph2o).lt. bdnl(kph2o)) then
          ierr3 = 1
        else
          call aerochem_cf(wpuff,tpuff,ppuff,cliq,phpuff,con,
     &                     convfac,dtchem,dumarr,.false.,1)
        endif
      endif
c
c-----Update input species arrays
c
      if (ierr1.eq.0 .and. ierr2.eq.0 .and. ierr3.eq.0) then
        do l = 1,nrad
          crad(l) = amax1(crad(l),bdlrad)
        enddo
        do is = 1,nspec
          conpin(is) = con(is)
        enddo
        conpin(nspec+1) = 0.
      endif
c
      return
      end

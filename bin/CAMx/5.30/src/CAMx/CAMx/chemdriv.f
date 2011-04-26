      subroutine chemdriv(m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon,igrd,
     &                    ltuv,ncol,nrow,nlay,dt,itzon,idfin,fcloud,
     &                    cldtrns,water,tempk,press,height,cwc,cph,
     &                    conc,cncrad,cellat,cellon,ldark,l3dflag,
     &                    iptr2d,iptrsa,iptrrad,ipa_cel,iproc_id)
      use filunit
      use chmstry
      use ahomap
      use bndary
      use camxcom
      use procan
      use tracer
      use rtracchm
      use rtcmcchm
c
c----CAMx v5.30 101223
c
c     CHEMDRIV performs chemistry on the current grid for one time step.
c     It calls one of three ODE solvers: EBISOLV, IEHSOLV, or LSCALL.
c       EBISOLV is the driver for the EBI solver with Hertel's solutions
c       IEHSOLV is the driver for the IEH solver
c       LSCALL  is the driver for the LSODE solver
c
c     Mechanism specific subroutines are passed to the driver routines.
c
c     Local array element con(nspec+1) is used to store the concentrations
c     of non-used species, i.e., species that are in the chem solver but
c     not on the species list for this run
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c     Modifications:
c        1/9/02        A minor bug fix related to fast aero routine
c                      (units conversion)
c        1/15/02       Added code to handle RTRAC
c        01/30/02      Added code for RTRAC probing tool
c        10/18/02      Updated AEROCHEM routine; added CWC for aqueous
c                      PM chemistry
c        03/14/03      Implemented Hg chemistry (Prakash Karamchandani, AER)
c        03/21/03      Removed the OSAT technology type OPPAT
c        04/2/03       Removed option for UAM-V type cloud adjustment
c        07/21/03      Added optional snow effect on UV albedo
c        04/21/04      Added CMU sectional aerosol chemistry
c        10/14/04      Added Mechanism 10
c        10/05/05      Added PA calls for Mech 4
c        10/06/05      Removed Mech 2
c        10/07/05      Cloud water content below 273 K is partitioned to
c                      liquid fraction like in wet dep
c        10/27/05      Added cloud water pH as a 3-D argument
c        01/12/06      Aqueous PM now called each timestep
c        11/28/06      Fixed bug in assigning ldchm flag
c        07/04/07      Added heterogeneous hydrolysis of N2O5
c        07/11/07      Added code for RTCMC probing tool
c        12/26/06 -bkoo-     Removed AEROCHEM_AQ (merged into AEROCHEM_CF/CMU)
c        12/29/06 -bkoo-     Added SOACHEM
c        01/08/07 -bkoo-     Added Mechanism 6 (CB05)
c                            Fixed titration rxn # for SAPRC99 (PA)
c        07/04/07 -bkoo-     Added heterogeneous hydrolysis of N2O5
c        07/16/07 -bkoo-     Revised for HDDM
c        04/24/08 -gyarwood- Added EBI solver
c        06/01/08 -bkoo-     Modified call to EBI for DDM
c        06/11/08 -bkoo-     Revised for rate constant sensitivity
c        07/16/08 -bkoo-     Added DDM turn-off flag
c        01/30/09 -bkoo-     Removed the CMC fast solver
c        7/14/10       Added code for in-line TUV cloud adjustment
c
c     Input arguments:
c        igrd                grid index
c        ltuv                In-line TUV flag
c        ncol                number of columns
c        nrow                number of rows
c        nlay                number of layers
c        dt                  timestep (s)
c        itzon               time zone
c        idfin               map of nested grids in this grid
c        fcloud              cloud coverage (fraction)
c        cldtrns             energy transmission coefficient (fraction)
c        water               water vapor (ppm)
c        tempk               temperature (K)
c        press               pressure (mb)
c        height              layer interface height (m)
c        cwc                 cloud water content (g/m3)
c        cph                 cloud water pH
c        conc                species concentration (umol/m3,ug/m3)
c        cncrad              radical concentration (ppm)
c        cellat              cell centroid latitude (deg)
c        cellon              cell centroid longitude (deg)
c        ldark               darkness flag (T=dark)
c        l3dflag             save 3-D average concentrations
c        iptr2d              pointers into vectors for 2-D fields
c        iptrsa              pointers into vectors for tracer conc
c        iptrrad             pointers into vectors for DDM radicals
c        ipa_cel             gridded array to identify if cell is
c                            in a IPRM sub-domain
c        iproc_id            process ID for this slice
c
c     Output arguments:
c        conc                species concentration (umol/m3,ug/m3)
c
c     Routines called:
c        CHEM10
c        KTHERM
c        KHETERO
c        GETZNTH
c        KPHOTO
c        EBISOLV
c        IEHSOLV
c        LSCALL
c        HDDMCHEM
c        SOACHEM
c        AEROCHEM
c        HGGASCHEM
c        HGAQSCHEM
c        UPTIME
c
c     Called by:
c        CHEMRXN
c
c----Argument declarations
c
      integer :: m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon

      integer igrd
      integer ncol
      integer nrow
      integer nlay
      real    dt
      integer itzon
      integer idfin(m1,m2)
      integer iproc_id
c
      real, dimension(m1,m2,m3) :: fcloud,cldtrns,cph,
     &                            water,tempk,press,cwc

      real, dimension(ncol,nrow,nlay) :: height
c
      real    conc(m1,m2,m3,nspec)
c
      real    cncrad(m1,m2,m3,nrad)
c
      real    cellat(m1,m2)
      real    cellon(m1,m2)
c
      logical ldark(m1,m2)
      logical ltuv
      logical l3dflag
      integer iptr3d
      integer iptrsa
      integer iptrrad
      integer ipa_cel(ncol,nrow,nlay)
c
      include "camx.prm"
      include 'chmdbg.inc'
c
c======================== DDM Begin =======================
c
      real sddm(MXTRSP), sradddm(MXTRSP)
c
c======================== DDM End =======================
c
c======================== Process Analysis Begin =======================
c
      logical ldoirr, ldoipr, ltrapirr
      real    titrt
      real    chg0, chg2
c
      real rrxn_irr(MXREACT)
      real patmp(MXTRSP)
c
c======================== Process Analysis End =========================
c
c========================= Hg Chemistry Begin ==========================
c
      real hclc, cl2c        ! HCl and Cl2 concentrations (ppm)
      real henso2,tfso2      ! Henry's Law for SO2 (M/atm)
      real heno3,tfo3        ! Henry's Law for O3 (M/atm)
      real pm10              ! Total PM10 concentrations (ug/m3)
      integer iocean         ! Ocean flag: 1 = ocean, 0 = other
c
c========================== Hg Chemistry End ===========================
c
c-----variables for SOACHEM
c
      real avgox(4)          ! Average oxidant conc. (ppm)
      real con_prec(5)       ! Precursor conc. (ppm)
      real con_CG(7)         ! CG conc. (ppm)
c
c-----subroutine names to be called by TRAP and IEHSOLV
c
      external ebirxn3, ebirate3, hr_hox3, hr_nox3, hr_nxy3, hr_pan3
      external ebirxn4, ebirate4, hr_hox4, hr_nox4, hr_nxy4, hr_pan4
      external ebirxn5, ebirate5, hr_hox5, hr_nox5, hr_nxy5, hr_pan5
      external ebirxn6, ebirate6, hr_hox6, hr_nox6, hr_nxy6, hr_pan6
      external ierxn3, ierate3, iejac3, ieslow3
      external ierxn4, ierate4, iejac4, ieslow4
      external ierxn5, ierate5, iejac5, ieslow5
      external ierxn6, ierate6, iejac6, ieslow6
      external lsrate3
      external lsrate4
      external lsrate5
      external lsrate6
      external hddmjac3
      external hddmjac4
      external hddmjac5
      external hddmjac6
c
c-----Local variables
c
      logical laero_upd
      integer ierr
      integer aero_flag
c
      real con(MXSPEC+1)
      real avgcnc(MXSPEC+1)
      real crad(MXRADCL)
      real avgrad(MXRADCL)
c
c --- the delcon array stores the delta concentrations at
c     various stages of the chemistry process.
c
c    delcon(1,*)  -- change in concentration after just gas phase
c                    chemistry (no aerosal chemistry applied)
c    delcon(2,*)  -- change in concentration after gas phase
c                    and aeqeous phase chemistry
c    delcon(3,*)  -- change in concentration after all chemistry
c                    is complete
c    delcon(4,*)  -- special instance to capture the change in
c                    concentration due to gas phase and aqueous 
c                    phase but also includes the transfer of
c                    some nitrate species during the aqueous phase
c    delcon(5,*)  -- special instance to capture the change in
c                    concentration due to SOA polymerization
c
      integer ispc, jspc
      real    o3old, o3new
c
      real delcon(5,MXSPEC)
      real cold(MXSPEC)
      real cnew(MXSPEC)
      real rtcon(MXTRSP)
c
c-----Entry point
c
      call flush(6)
      call flush(iout)
c
      dtchem = dt/3600.
      con(nspec+1) = 0.
      ltrapirr = .FALSE.
c
c-----Aerosol timing parameters:
c     time_aero : time to call aerosol routine (HHMM)
c     date_aero : date to call aerosol routine (YYJJJ)
c     dt_aero   : time interval between calls to aerosol routines (min)
c     aero_dt   : actual time interval for each grid (hr) (accumulated dtchem)
c
      aero_flag = 0
      if( naero.GT.0 .AND. (aeropt.EQ.'CF' .OR. aeropt.EQ.'CMU')) then
         aero_flag = 1
         aero_dt(igrd) = aero_dt(igrd) + dtchem
         if ( (timec(igrd)-time_aero(igrd)) .ge. -0.01 .and.
     &        (datec(igrd) .eq. date_aero(igrd)) ) then
           aero_flag = 2
         endif
      endif
c
      igrdchm = igrd
      do 91 k = 1,nlay
c
c$omp parallel default(shared)
c$omp&  private(i,j,l,is,ispc,i1,i2,con,
c$omp&  crad,ij,iozon,ihaze,ialb,hght,iabov,ctrns,fcld,
c$omp&  zenith,delcon,tcell,pcell,cliq,cldph,atm,O2,H2,CH4,
c$omp&  sradddm,sddm,ldoipr,ldoirr,ipa_idx,titrt,irxn,rrxn_irr,
c$omp&  ierr,convfac,patmp,idx,nn,cold,cnew,avgcnc,avgrad,o3old,
c$omp&  o3new,irt_cel,iocean,hclc,cl2c,henso2,tfso2,heno3,tfo3,
c$omp&  pm10,iht,ihg,cldadj,ltrapirr,chg0,chg2,avgox,con_prec,
c$omp&  con_CG,nj,nz,rtcon,desto3,prdo3n,prdo3v)
c$omp&  copyin(/ijkgrd/)
c
c$omp do schedule(dynamic)
c
        do 90 j =  ja,jz   !2,nrow-1
          do 89 i = ia,iz   !2,ncol-1
            ichm = i
            jchm = j
            kchm = k
c
c-----skip chemistry if fine grid exists in this cell
c
            if (idfin(i,j).gt.igrd) goto 89
c
c-----For the gas phase species (numbered 1 to ngas)
c     Pass concentration to CON, converting from umol/m3 to ppm
c
            tcell = tempk(i,j,k)
            pcell = press(i,j,k)
            convfac = densfac*(273./tcell)*(pcell/1013.)
            cldph = cph(i,j,k)
            tchm = tcell
            wchm = water(i,j,k)
            do is = 1,ngas
              con(is) = conc(i,j,k,is)/convfac
              if (con(is).lt.0.) then
                write(iout,'(//,a)') 'ERROR in CHEMDRIV:'
                write(iout,*) 'Negative concentration before chem'
                write(iout,*) 'igrd, i, j, k = ', igrd,i,j,k
                do l = 1,nspec
                  write(iout,'(i3,2x,a7,e10.3)') l,spname(l),con(l)
                enddo
                call camxerr()
              endif
              con(is) = amax1(bdnl(is),con(is))
            enddo
c
c-----Load any aerosols
c
            if (ngas.lt.nspec) then
              do is=ngas+1,nspec
                con(is) = conc(i,j,k,is)
                if (con(is).lt.0.) then
                  write(iout,'(//,a)') 'ERROR in CHEMDRIV:'
                  write(iout,*) 'Negative concentration before chem'
                  write(iout,*) 'igrd, i, j, k = ', igrd,i,j,k
                  do l = 1,nspec
                    write(iout,'(i3,2x,a7,e10.3)') l,spname(l),con(l)
                  enddo
                  call camxerr()
                endif
                con(is) = amax1(bdnl(is),con(is))
              enddo
            endif
c
c-----Mechanism 10 is done here
c
            if (idmech.eq.10) then
               call chem10(dtchem,tcell,pcell,water(i,j,k),con)
               goto 88
            endif
c
c-----Load radicals from last time step to use as initial guess
c
            do l=1,nrad
              crad(l) = cncrad(i,j,k,l)
            enddo
c
c-----Determine thermal rate constants
c
            call ktherm(tcell,pcell)
c
c-----Calculate the rate constant for heterogeneous hydrolysis of N2O5
c
            if (aero_flag.GT.0) call khetero(tcell,water(i,j,k),con)
c
c-----Load local values of ozone, haze, albedo and zenith angle
c
            ij = i + (j-1)*m1
            iozon = icdozn(iptr2d-1+ij)
            ihaze = icdhaz(iptr2d-1+ij)
            ialb = icdalb(iptr2d-1+ij)
            iocean = icdocn(iptr2d-1+ij)
c
c-----Set albedo to max if there is snow cover
c
            if (lrdsno .and. icdsno(iptr2d-1+ij).eq.1) ialb = NALB
c
            hght = height(i+i0,j+j0,k)/2000.
            if (k.gt.1) hght = (height(i+i0,j+j0,k) + 
     &                                    height(i+i0,j+j0,k-1))/2000.
            if (ltuv) then
              iabov = 0
              ctrns = cldtrns(i,j,k)
              fcld = fcloud(i,j,k)
            else
              if (cldtrns(i,j,k).ne.1.) then
                iabov = 0 
                ctrns = cldtrns(i,j,k)
                fcld = fcloud(i,j,k)
              else
                iabov = 1
                ctrns = cldtrns(i,j,1)
                fcld = fcloud(i,j,1)
              endif
           endif
           call getznth(cellat(i,j),cellon(i,j),timec(igrd),
     &                  datec(igrd),itzon,zenith,ldark(i,j))
            ldchm = ldark(i,j)
c
c-----Determine photolysis rates through interpolation of look-up table
c
            cldadj = 0.0
            call kphoto(iozon,ialb,ihaze,hght,zenith,fcld,
     &                  ctrns,cldadj,ldark(i,j),ltuv,iabov)
c
c======================== Source Apportion Begin =======================
c
c  --- store the current value of the species needed for tracer "chemistry" ----
c
            if( ltrace ) then
               o3old = con(ko3)
               do ispc=1,ngas
                  delcon(1,ispc) = -con(ispc)*convfac
                  delcon(2,ispc) = -con(ispc)*convfac
                  delcon(3,ispc) = -con(ispc)*convfac
                  delcon(4,ispc) = -con(ispc)*convfac
                  delcon(5,ispc) = 0.0
                  cold(ispc) = con(ispc)*convfac
               enddo
               do ispc=ngas+1,nspec
                  delcon(1,ispc) = -con(ispc)
                  delcon(2,ispc) = -con(ispc)
                  delcon(3,ispc) = -con(ispc)
                  delcon(4,ispc) = -con(ispc)
                  cold(ispc) = con(ispc)
                  delcon(5,ispc) = 0.0
               enddo
c
c  --- call routine to recalibrate so that tracer species stays
c      on track with model species: needed because the numerics
c      cause tracer species to drift
c
               if( tectyp .NE. RTRAC .AND. tectyp .NE. RTCMC )
     &                call recalib(m1,m2,m3,ntotsp,
     &                                      ptconc(iptrsa),i,j,k,cold)
c
c   --- if doing Ozone and daylight hours or oding PSAT and
c       nitrate species are requested,
c       set the flag for process analysis -- this will save needed
c       information from the solver routines ---
c
c  --- set variables for OSAT2 ----
c
               if( (lozone .AND. .NOT. ldark(i,j)) .OR. lnitrate ) then
                 ltrapirr = .TRUE.
                 do irxn=1,nreact
                     rrxn_irr(irxn) = 0.0
                 enddo
               endif
            endif
c
c========================= Source Apportion End ========================
c
c
c======================== DDM Begin ====================================
c
c  ---- load the sensitivities for this cell into 2-D array ---
c
            if ( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
              call loaddm(.FALSE.,m1,m2,m3,ntotsp,ptconc(iptrsa),
     &                         nradddm,senrad(iptrrad),i,j,k,nddmsp,
     &                                nspec,nrad,sddm,sradddm,convfac)
              if ( lhddm )
     &              call hddmadj(.FALSE.,nddmsp,con,crad,sddm,sradddm)
            endif
c
c======================== DDM End =======================================
c
c-----Chemistry integration, pass subroutines for mechanism used
c
            atm = 1.e6
            O2  = 2.095e5
            CH4 = 1.75
            H2  = 0.60
c
c======================== Process Analysis Begin =======================
c
            ldoipr = .FALSE.
            ldoirr = .FALSE.
            if( lproca ) then
               if( lipr ) then
                  if( ipa_cel(i+i0,j+j0,k) .GT. 0 )  then
                     ipa_idx = ipa_cel(i+i0,j+j0,k)
                     ldoipr = .TRUE.
                  endif
               endif
               if( lirr ) then
                  ltrapirr = .TRUE.
                  if( ipa_cel(i+i0,j+j0,k) .GT. 0 ) then
                     ipa_idx = ipa_cel(i+i0,j+j0,k)
                     ldoirr = .TRUE.
                  endif
                  titrt = 0.0
                  do irxn=1,nreact
                     rrxn_irr(irxn) = 0.0
                  enddo
               endif
            endif
c
c======================== Process Analysis End =========================
c
            if ( idsolv .EQ. IDEBI ) then
               if (idmech.eq.3) then
                   call ebisolv(ebirxn3,ebirate3,hr_hox3,hr_nox3,
     &                  hr_nxy3,hr_pan3,dtchem,water(i,j,k),atm,O2,
     &                  CH4,H2,tcell,ldark(i,j),con,crad,avgcnc,avgrad,
     &                  hddmjac3,nddmsp,sddm,sradddm,
     &                  (lddm.AND.lddmcalc(igrd)),
     &                  nirrrxn,rrxn_irr,ltrapirr)
               elseif (idmech.eq.4) then
                   call ebisolv(ebirxn4,ebirate4,hr_hox4,hr_nox4,
     &                  hr_nxy4,hr_pan4,dtchem,water(i,j,k),atm,O2,
     &                  CH4,H2,tcell,ldark(i,j),con,crad,avgcnc,avgrad,
     &                  hddmjac4,nddmsp,sddm,sradddm,
     &                  (lddm.AND.lddmcalc(igrd)),
     &                  nirrrxn,rrxn_irr,ltrapirr)
               elseif (idmech.eq.5) then
                   call ebisolv(ebirxn5,ebirate5,hr_hox5,hr_nox5,
     &                  hr_nxy5,hr_pan5,dtchem,water(i,j,k),atm,O2,
     &                  CH4,H2,tcell,ldark(i,j),con,crad,avgcnc,avgrad,
     &                  hddmjac5,nddmsp,sddm,sradddm,
     &                  (lddm.AND.lddmcalc(igrd)),
     &                  nirrrxn,rrxn_irr,ltrapirr)
               elseif (idmech.eq.6) then
                   call ebisolv(ebirxn6,ebirate6,hr_hox6,hr_nox6,
     &                  hr_nxy6,hr_pan6,dtchem,water(i,j,k),atm,O2,
     &                  CH4,H2,tcell,ldark(i,j),con,crad,avgcnc,avgrad,
     &                  hddmjac6,nddmsp,sddm,sradddm,
     &                  (lddm.AND.lddmcalc(igrd)),
     &                  nirrrxn,rrxn_irr,ltrapirr)
               endif
c
            elseif ( idsolv .EQ. IDIEH ) then
               if (idmech.eq.3) then
                   call iehsolv(ierxn3,ierate3,iejac3,ieslow3,
     &                  dtchem,water(i,j,k),atm,O2,CH4,H2,con,crad,
     &                  avgcnc,avgrad,nirrrxn,rrxn_irr,ltrapirr)
               elseif (idmech.eq.4) then
                   call iehsolv(ierxn4,ierate4,iejac4,ieslow4,
     &                  dtchem,water(i,j,k),atm,O2,CH4,H2,con,crad,
     &                  avgcnc,avgrad,nirrrxn,rrxn_irr,ltrapirr)
               elseif (idmech.eq.5) then
                   call iehsolv(ierxn5,ierate5,iejac5,ieslow5,
     &                  dtchem,water(i,j,k),atm,O2,CH4,H2,con,crad,
     &                  avgcnc,avgrad,nirrrxn,rrxn_irr,ltrapirr)
               elseif (idmech.eq.6) then
                   call iehsolv(ierxn6,ierate6,iejac6,ieslow6,
     &                  dtchem,water(i,j,k),atm,O2,CH4,H2,con,crad,
     &                  avgcnc,avgrad,nirrrxn,rrxn_irr,ltrapirr)
               endif
c
            elseif ( idsolv .EQ. IDLSOD ) then
               if (idmech.eq.3) then
                  call lscall(lsrate3,dtchem,water(i,j,k),atm,
     &                        O2,CH4,H2,con,crad,avgcnc,avgrad,nirrrxn,
     &                        rrxn_irr,ierr,ltrapirr,.FALSE.)
               elseif (idmech.eq.4) then
                  call lscall(lsrate4,dtchem,water(i,j,k),atm,
     &                        O2,CH4,H2,con,crad,avgcnc,avgrad,nirrrxn,
     &                        rrxn_irr,ierr,ltrapirr,.FALSE.)
               elseif (idmech.eq.5) then
                  call lscall(lsrate5,dtchem,water(i,j,k),atm,
     &                        O2,CH4,H2,con,crad,avgcnc,avgrad,nirrrxn,
     &                        rrxn_irr,ierr,ltrapirr,.FALSE.)
               elseif (idmech.eq.6) then
                  call lscall(lsrate6,dtchem,water(i,j,k),atm,
     &                        O2,CH4,H2,con,crad,avgcnc,avgrad,nirrrxn,
     &                        rrxn_irr,ierr,ltrapirr,.FALSE.)
               endif
            endif
c
c======================== HDDM Begin ====================================
c
            if ( lhddm .AND. lddmcalc(igrd) ) then
              if (idmech.eq.3) then
                call hddmchem(hddmjac3,ebirxn3,ebirate3,
     &                        nreact,nreact,ngas,nrad,nrad+ngas,
     &                        nddmsp,nrateddm,nhddm,iprate,iphddm,
     &                        dtchem,water(i,j,k),atm,O2,CH4,H2,
     &                        rk,avgcnc,avgrad,sddm,sradddm,ierr)
              elseif (idmech.eq.4) then
                call hddmchem(hddmjac4,ebirxn4,ebirate4,
     &                        nreact,nreact,ngas,nrad,nrad+ngas,
     &                        nddmsp,nrateddm,nhddm,iprate,iphddm,
     &                        dtchem,water(i,j,k),atm,O2,CH4,H2,
     &                        rk,avgcnc,avgrad,sddm,sradddm,ierr)
              elseif (idmech.eq.5) then
                call hddmchem(hddmjac5,ebirxn5,ebirate5,
     &                        nreact,nreact,ngas,nrad,nrad+ngas,
     &                        nddmsp,nrateddm,nhddm,iprate,iphddm,
     &                        dtchem,water(i,j,k),atm,O2,CH4,H2,
     &                        rk,avgcnc,avgrad,sddm,sradddm,ierr)
              elseif (idmech.eq.6) then
                call hddmchem(hddmjac6,ebirxn6,ebirate6,
     &                        nreact,nreact,ngas,nrad,nrad+ngas,
     &                        nddmsp,nrateddm,nhddm,iprate,iphddm,
     &                        dtchem,water(i,j,k),atm,O2,CH4,H2,
     &                        rk,avgcnc,avgrad,sddm,sradddm,ierr)
              endif
              if (ierr.ne.0) then
                write(iout,'(//,a)') 'ERROR in CHEMDRIV:'
                write(iout,*)
     &                'Zero determinant in SGEFA in HDDMCHEM at ',ierr
                write(iout,*) 'igrd, i, j, k = ', igrd,i,j,k
                call camxerr()
              endif
            endif
c
c======================== HDDM End =======================================
c
c-----Perform gas-phase chemistry for SOA precursors
c
            if( aero_flag .GT. 0 ) then

              avgox(1) = amax1(avgrad(kO), bdlrad)
              avgox(2) = amax1(avgrad(kOH), bdlrad)
              avgox(3) = amax1(avgcnc(kO3), bdnl(kO3))
              avgox(4) = amax1(avgcnc(kNO3), bdnl(kNO3))

              con_prec(1) = con(kTOLA)
              con_prec(2) = con(kXYLA)
              con_prec(3) = con(kISP)
              con_prec(4) = con(kTRP)
              con_prec(5) = con(kSQT)

              con_CG(1) = con(kCG1)
              con_CG(2) = con(kCG2)
              con_CG(3) = con(kCG3)
              con_CG(4) = con(kCG4)
              con_CG(5) = con(kCG5)
              con_CG(6) = con(kCG6)
              con_CG(7) = con(kCG7)

              call soachem(iout,4,5,7,tcell,pcell,dtchem,
     &                          avgox,con_prec,con_CG)

              con(kTOLA) = amax1( con_prec(1), bdnl(kTOLA) )
              con(kXYLA) = amax1( con_prec(2), bdnl(kXYLA) )
              con(kISP)  = amax1( con_prec(3), bdnl(kISP) )
              con(kTRP)  = amax1( con_prec(4), bdnl(kTRP) )
              con(kSQT)  = amax1( con_prec(5), bdnl(kSQT) )

              con(kCG1) = con_CG(1)
              con(kCG2) = con_CG(2)
              con(kCG3) = con_CG(3)
              con(kCG4) = con_CG(4)
              con(kCG5) = con_CG(5)
              con(kCG6) = con_CG(6)
              con(kCG7) = con_CG(7)
            endif
c
c======================== Source Apportion Begin =======================
c
c  --- subtract the current values of the species from the before values
c      to get the delta values ---
c
            if( ltrace ) then
               do ispc=1,ngas
                 delcon(1,ispc) = delcon(1,ispc) +
     &                     AMAX1(bdnl(ispc),con(ispc)) * convfac
                 delcon(4,ispc) = delcon(4,ispc) +
     &                     AMAX1(bdnl(ispc),con(ispc)) * convfac
                 if( aeropt .NE. 'CF' .OR. aero_flag .EQ. 0 ) then
                     delcon(2,ispc) = delcon(2,ispc) +
     &                     AMAX1(bdnl(ispc),con(ispc)) * convfac
                 endif
               enddo
               do ispc=ngas+1,nspec
                 delcon(1,ispc) = delcon(1,ispc) +
     &                               AMAX1(bdnl(ispc),con(ispc))
                 delcon(4,ispc) = delcon(4,ispc) +
     &                               AMAX1(bdnl(ispc),con(ispc))
                 if( aeropt .NE. 'CF' .OR. aero_flag .EQ. 0 ) then
                     delcon(2,ispc) = delcon(2,ispc) +
     &                               AMAX1(bdnl(ispc),con(ispc))
                 endif
               enddo
            endif
c
c======================== Source Apportion End =======================
c
c======================= Process Analysis Begin ========================
c
            if( ldoipr ) then
c
c --- Chemistry change in umol/m3 units (except for HG chemistry) ----
c
               do is=1,ngas
                  cipr(IPR_CHEM, ipa_idx, is) =
     &               cipr(IPR_CHEM, ipa_idx, is) +
     &                  con(is)*convfac-conc(i,j,k,is)
               enddo
            endif
c
c======================== Process Analysis End =========================
c
            do l=1,nrad
              crad(l) = amax1(crad(l),bdlrad)
              cncrad(i,j,k,l) = crad(l)
            enddo
c
c-----Perform aerosol chemistry if it's time
c
            if ( aero_flag.gt.0 ) then
              cliq = cwc(i,j,k)
              if (tcell.lt.273.)
     &          cliq = amax1(0.,cliq*(tcell - tamin)/(273. - tamin))
              if( aeropt.eq.'CF' ) then
                call aerochem_cf(water(i,j,k),tcell,pcell,cliq,cldph,
     &                           con,convfac,dtchem,aero_dt(igrd),
     &                           delcon,ldoipr,ipa_idx,aero_flag,
     &                           .FALSE.)
c
              elseif( aeropt.eq.'CMU' ) then
                call aerochem_cmu(water(i,j,k),tcell,pcell,cliq,cldph,
     &                            MXSPEC,MXRADCL,con,crad,bdnl,convfac,
     &                            timec(igrd),dtchem,aero_dt(igrd),
     &                            aero_flag)
              endif
            endif
c
c========================= Hg Chemistry Begin ==========================
c
c-----Call Hg chemistry modules
c
            if (khg0.lt.nspec+1) then
c
c-----Assign HCl and Cl2 concentrations based on height(km), ocean
c     and day/night
c
              do ihg = 1,NHTCL
                if (hght .le. htcl(ihg)) then
                  iht = ihg
                  go to 115
                endif
              enddo
              iht = NHTCL
 115          continue
              hclc = hclprof(iht)
              if (iocean .eq. 1) then
                if (ldark(i,j)) then
                  cl2c = cl2nite(iht)
                else
                  cl2c = cl2day(iht)
                endif
              else
                cl2c =  0.0
              endif
c
c-----Gas-phase chemistry
c
              call hggaschem(con(khg0),con(khg2),con(ko3),
     &                       con(kh2o2),crad(koh),hclc,cl2c,
     &                       tcell,pcell,dt)
              con(khg0) = max(bdnl(khg0),con(khg0))
              con(khg2) = max(bdnl(khg2),con(khg2))
c
c======================= Process Analysis Begin ========================
c
              if( ldoipr ) then
                chg0 = con(khg0)*convfac ! store HG0 in umol/m3
                chg2 = con(khg2)*convfac ! store HG2 in umol/m3
                cipr(IPR_CHEM, ipa_idx, khg0) =
     &            cipr(IPR_CHEM, ipa_idx, khg0) +
     &              chg0 - conc(i,j,k,khg0)
                cipr(IPR_CHEM, ipa_idx, khg2) =
     &            cipr(IPR_CHEM, ipa_idx, khg2) +
     &              chg2 - conc(i,j,k,khg2)
              endif
c
c======================== Process Analysis End =========================
c
c-----Aqueous-phase chemistry (conditional)
c
              cliq = cwc(i,j,k)
              if (tcell.lt.273.) then
                cliq = amax1(0.,cliq*(tcell - tamin)/(273. - tamin))
              endif
              if (cliq.ge.cwmin .and. tcell.ge.tamin ) then
c
c-----Calculate total PM concentrations
c
                henso2 = henry0(kso2)
                tfso2  = tfact(kso2)
                heno3  = henry0(ko3)
                tfo3   = tfact(ko3)
                pm10   = 0.
                do is = ngas+1,nspec
                  if (is.ne.kph2o) pm10 = pm10 + con(is)
                enddo
                call hgaqschem(con(khg0),con(khg2),con(ko3),
     &                         con(kso2),crad(koh),crad(kho2),
     &                         hclc,cl2c,pm10,cldph,cliq,
     &                         tcell,pcell,dt,henso2,tfso2,
     &                         heno3,tfo3)
                con(khg0) = max(bdnl(khg0),con(khg0))
                con(khg2) = max(bdnl(khg2),con(khg2))
c
c======================= Process Analysis Begin ========================
c
                if( ldoipr ) then
                  cipr(IPR_AQCHEM, ipa_idx, khg0) =
     &              cipr(IPR_AQCHEM, ipa_idx, khg0) +
     &                con(khg0)*convfac - chg0
                  cipr(IPR_AQCHEM, ipa_idx, khg2) =
     &              cipr(IPR_AQCHEM, ipa_idx, khg2) +
     &                con(khg2)*convfac - chg2
                endif
c
c======================== Process Analysis End =========================
c
              endif
            endif
c
c========================== Hg Chemistry End ===========================
c
c
c======================== Source Apportion Begin =======================
c
c  --- subtract the current values of the species from the before values
c      to get the delta values ---
c
            if( ltrace ) then
               do ispc=1,ngas
                  cnew(ispc) = con(ispc)*convfac
                  delcon(3,ispc) = delcon(3,ispc) +
     &                     AMAX1(bdnl(ispc),con(ispc)) * convfac
               enddo
               do ispc=ngas+1,nspec
                  cnew(ispc) = con(ispc)
                  delcon(3,ispc) = delcon(3,ispc) +
     &                               AMAX1(bdnl(ispc),con(ispc))
               enddo
               o3new = AMAX1(bdnl(ko3),con(ko3))
c
               if( ldark(i,j) ) then
                 desto3 = delcon(2,ko3)
                 prdo3n = 0.0
                 prdo3v = 0.0
               else
                 if( tectyp .EQ. OSAT .OR.
     &               tectyp .EQ. GOAT .OR. tectyp .EQ. APCA .OR.
     &              (tectyp .EQ. PSAT .AND. lozone) ) then
                    call o3prdsa(rrxn_irr,convfac,delcon(2,ko3),
     &                                         prdo3n,prdo3v,desto3)
                    if( delcon(2,ko3) .lt. desto3 ) then
                       desto3 = delcon(2,ko3)
                       prdo3n = 0.0
                       prdo3v = 0.0
                    endif
                 endif
               endif
c
c  --- OSAT: call routine to do the tracer species "chemistry", just makes
c      adjustments for production or decay of the regular model species ----
c
               if( tectyp .EQ. OSAT .OR.
     &                            (tectyp .EQ. PSAT .AND. lozone) ) then
                  call osatsa(m1,m2,m3,igrd,i,j,k,
     &                          prdo3n,prdo3v,desto3,nspec,delcon,dtchem)
               else if( tectyp .EQ. GOAT ) then
                  call goatsa(m1,m2,m3,igrd,i,j,k,i0,j0,
     &                          prdo3n,prdo3v,desto3,nspec,delcon,dtchem)
               else if( tectyp .EQ. APCA ) then
                  call apcasa(m1,m2,m3,igrd,i,j,k,
     &                          prdo3n,prdo3v,desto3,nspec,delcon,dtchem)
               endif
c
c  --- PSAT
c
               if( tectyp .EQ. PSAT ) then
                  if( ltrapirr ) then
                     do irxn=1,nreact
                        rrxn_irr(irxn) = rrxn_irr(irxn) * convfac
                     enddo
                  endif
                  call psatsa(m1,m2,m3,igrd,i,j,k,convfac,cold,nspec,
     &                                                 delcon,rrxn_irr)
                  call repartsa(m1,m2,m3,igrd,i,j,k,cnew,delcon)
               endif
            endif
c
c  --- RTRAC chemistry
c
            if( tectyp .EQ. RTRAC .AND.
     &                 (nrtherm .GT. 0 .OR. nrtphot .GT. 0 )) then
               if( ipa_cel(i+i0,j+j0,k) .GT. 0 )  then
                  irt_cel = ipa_cel(i+i0,j+j0,k)
               else
                  irt_cel = -9
               endif
               call chemrt(m1,m2,m3,igrd,i,j,k,pcell,tcell,cold,cnew,
     &                 avgrad(koh),avgcnc(ko3),avgcnc(kno3),dtchem,
     &                 convfac,irt_cel)
c
c  --- RTCMC chemistry
c
            else if( tectyp .EQ. RTCMC ) then
c
c  --- set rate constants
c
               if( ktype .EQ. 1 ) then
                  call krtc(tcell,pcell)
               else
                  call ksci(tcell,pcell)
               endif
               if( njschm .GT. 0 ) then
                  do nj = 1,njschm
                     srkrtc(ijschm(nj)) = 0.
                     do nz = nzschm,1,-1
                        if( zenith .LE. zenschm(nz) )
     &                        srkrtc(ijschm(nj)) = rjschm(nz,nj)
                     enddo
                     rkrtc(ijschm(nj))  = DBLE( srkrtc(ijschm(nj)) )
                  enddo
               endif
c
c  --- do chemistry
c
               do l = 1,ntotsp
                  idx = i + m1*(j-1) + m1*m2*(k-1) +
     &                                    m1*m2*m3*(l-1)
                  rtcon(l) = ptconc(iptrsa-1+idx)/convfac
               enddo
               if( isolv .EQ. 1) then
                  call dlsdrv(dtchem,water(i,j,k),atm,O2,CH4,H2,
     &                           avgcnc,avgrad,rtcon,ierr,.false.)
               elseif( isolv .EQ. 2) then
                  call slsdrv(dtchem,water(i,j,k),atm,O2,CH4,H2,
     &                           avgcnc,avgrad,rtcon,ierr,.false.)
               elseif( isolv .EQ. 3) then
                  call rbkdrv(dtchem,water(i,j,k),atm,O2,CH4,H2,
     &                           avgcnc,avgrad,rtcon,ierr,.false.)
               endif
               do l = 1,ntotsp
                  idx = i + m1*(j-1) + m1*m2*(k-1) +
     &                                    m1*m2*m3*(l-1)
                  ptconc(iptrsa-1+idx) = rtcon(l)*convfac
               enddo
            endif
c
c========================= Source Apportion End ========================
c
c
c========================= DDM Begin ===================================
c
c  --- put the changed values back into the gridded array ---
c
            if( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
              if( lhddm )
     &              call hddmadj(.TRUE.,nddmsp,con,crad,sddm,sradddm)
              call loaddm(.TRUE.,m1,m2,m3,ntotsp,ptconc(iptrsa),
     &                        nradddm,senrad(iptrrad),i,j,k,nddmsp,
     &                                  nspec,nrad,sddm,sradddm,convfac)
            endif
c
c========================= DDM End =====================================
c
c
c======================= Process Analysis Begin ========================
c
c --- Chemical reaction tracking in ppm units
c     Account for titration reaction in TRAP at night ---
c
            if( ldoirr ) then
              do irxn=1,nirrrxn
                cirr(ipa_idx, irxn) =
     &                   cirr(ipa_idx, irxn) + rrxn_irr(irxn)
              enddo
              if ( titrt .GT. 0.0 ) then
                if ( idmech.EQ.5 ) then
                  cirr(ipa_idx, 7) =
     &                     cirr(ipa_idx, 7) + titrt
                else
                  cirr(ipa_idx, 3) =
     &                     cirr(ipa_idx, 3) + titrt
                endif
              endif
            endif
c
c   --- calculate the chemical process analysis (CPA) variables
c       time weight CPA variables that are not cumulative by dtfact ---
c
            if( lirr .AND. (l3dflag .OR. k .EQ. 1) ) then
               nn = 0
               dtfact = dt/dtout/60.
               if( idmech.EQ.3 ) then
                 call cpamech3(rrxn_irr,rk,dtfact,MXREACT,patmp,ntotsp,
     &                                                    nn,ldark(i,j))
               elseif( idmech.EQ.4 ) then
                 call cpamech4(rrxn_irr,rk,dtfact,MXREACT,patmp,ntotsp,
     &                                                    nn,ldark(i,j))
               elseif( idmech.EQ.5 ) then
                 call cpamech5(rrxn_irr,rk,dtfact,MXREACT,patmp,ntotsp,
     &                                                    nn,ldark(i,j))
               elseif( idmech.EQ.6 ) then
                 call cpamech6(rrxn_irr,rk,dtfact,MXREACT,patmp,ntotsp,
     &                                                    nn,ldark(i,j))
               endif
c
c  --- save the cloud adjustment to photolysis rates ---
c
               nn = nn + 1
               ptname(nn)  = 'J_CLDADJ'
               patmp(nn) = cldadj * dtfact
c    
c  --- save radical concentrations unless accumulating throught run ---
c
               if( .NOT. lcpacum)
     &           call cparad(crad, nrad, patmp, ntotsp, nn, dtfact)
c
c  --- add CPA values to gridded array in native units ---
c
               do l=1,ntotsp
                 idx = i + m1*(j-1) + m1*m2*(k-1) + m1*m2*nlay*(l-1)
                 ptconc(iptrsa-1+idx) = ptconc(iptrsa-1+idx) + patmp(l)
               enddo
            endif
c
c======================== Process Analysis End =========================
c
c-----Pass CON back to concentration, convert gases from ppm to umol/m3
c
 88         continue
            cph(i,j,k) = cldph
            do is=1,ngas
              conc(i,j,k,is) = con(is)*convfac
            enddo
            if (ngas.lt.nspec) then
              do is=ngas+1,nspec
                conc(i,j,k,is) = amax1(con(is),bdnl(is))
              enddo
            endif
c
  89      continue
  90    continue
c
c$omp end parallel
c
  91  continue
c
c-----If aerosol chemistry was called
c     - reset aero_dt
c     - increase time_aero (& date_aero) by dt_aero (or multiple of dt_aero)
c
      if (aero_flag.EQ.2) then
        aero_dt(igrd) = 0.0
  92    continue
        call uptime(time_aero(igrd),date_aero(igrd),60.*dt_aero)
        if ( time_aero(igrd) .le. timec(igrd) .and.
     &       date_aero(igrd) .eq. datec(igrd) ) goto 92
      endif
c
      return
      end

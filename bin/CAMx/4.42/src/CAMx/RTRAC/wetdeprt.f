      subroutine wetdeprt(igrid,ncol,nrow,nlay,nrtsp,deltat,deltax,
     &                    mapscl,deltay,depth,tempk,press,cwc,pwr,pws,
     &                    pwg,cph,densfac,idfin,conc)
c
c----CAMx v4.42 070603
c 
c     This version is for the RTRAC species.
c     WETDEP modifies vertical concentration profiles for a given grid via 
c     precipitation processes.  This subroutine has been completely rewritten
c     for CAMx v4.
c 
c     Copyright 1996-2007
c     ENVIRON International Corporation
c           
c     Modifications:
c        05/01/03   Taken from WETDEP for regular model
c        06/15/03   Fixed bug in scaling of tmass
c        02/11/04   Updated empirical rainfall relationships
c        05/03/05   Revised gas scavenging calculation
c        06/20/05   Revised to handle liquid/frozen cloud and precip water
c        10/21/05   Added dissociation to Henry's Law for NH3, HNO3, SO2
c        08/31/06   Added map scale factor
c
c     Input arguments:
c        igrid               grid index 
c        ncol                number of columns 
c        nrow                number of rows 
c        nlay                number of layers 
c        nrtsp               number of species in RTRAC array
c        deltat              time step (s)
c        deltax              cell size in x-direction (m)
c        deltay              cell size in y-direction (m)
c        mapscl              map scale factor
c        depth               cell depth (m)
c        tempk               temperature field (K)
c        press               pressure field (mb)
c        cwc                 cloud water content (g/m3)
c        pwr                 rain water content (g/m3)
c        pws                 snow water content (g/m3)
c        pwg                 graupel water content (g/m3)
c        cph                 cloud water pH
c        densfac             factor to convert to umol/m3
c        idfin               map of nested grids in this grid
c        conc                concentration field (umol/m3, ug/m3)
c             
c     Output arguments: 
c        conc                concentration field (umol/m3, ug/m3)
c             
c     Routines called: 
c        SCAVRAT
c        HENRYFNC
c             
c     Called by: 
c        EMISTRNS
c 
      implicit none
      include 'camx.prm'
      include 'bndary.com'
      include 'chmstry.com'
      include 'tracer.com'
      include 'rtracchm.com'
c
      integer ncol,nrow,nlay,nrtsp,igrid,j,i1,i2,i,kbot,ktop,
     &        k,ncnt,l
      integer idfin(ncol,nrow)
      real deltax(nrow),tempk(ncol,nrow,nlay),press(ncol,nrow,nlay),
     &     cwc(ncol,nrow,nlay),pwr(ncol,nrow,nlay),pws(ncol,nrow,nlay),
     &     pwg(ncol,nrow,nlay),cph(ncol,nrow,nlay),depth(ncol,nrow,nlay)
      real mapscl(ncol,nrow)
      real conc(ncol,nrow,nlay,nrtsp)
      real c0(MXTRSP),pp(MXLAYA),rr(MXLAYA),volrat(MXLAYA),
     &     tmass(MXTRSP)
      real rd,rhoh2o,deltat,deltay,densfac,cellvol,rainvol,rhoair,
     &     delc,delm,convfac,cmin,hlaw,gscav,ascav,c00,totc,totw,ceq,
     &     psize,cwat,pwat,rconst
      logical lcloud,ltop,lgraupl
c
      data rd /287./         ! Dry air gas constant (J/K/kg)
      data rhoh2o /1.e6/     ! water density (g/m3)
      data rconst /8.206e-2/ ! gas constant (l.atm/mol.K)
c
c-----Entry point
c
c-----Loop over rows and columns
c
      do 10 j = 2,nrow-1 
        i1 = 2 
        i2 = ncol-1 
        if (igrid.eq.1) then 
          if (ibeg(j).eq.-999) goto 10 
          i1 = ibeg(j) 
          i2 = iend(j) 
        endif 
        do 20 i = i1,i2
          if (idfin(i,j).gt.igrid) goto 20
c
c-----Scan column for layers containing precipitation bottom/top
c
          kbot = 0
          ktop = 0
          do k = 1,nlay
            if (pwr(i,j,k).ge.cwmin .or. pws(i,j,k).ge.cwmin .or.
     &          pwg(i,j,k).ge.cwmin) then
              kbot = k
              goto 25
            endif
          enddo
          goto 20

  25      continue
          if (kbot.eq.nlay) goto 20
          ncnt = 1
          do k = kbot+1,nlay
            if (pwr(i,j,k).lt.cwmin .and. pws(i,j,k).lt.cwmin .and.
     &          pwg(i,j,k).lt.cwmin) then
              ktop = k-1
              goto 26
            endif
            ncnt = ncnt + 1
          enddo
          ktop = nlay
  26      continue
          if (kbot.gt.1 .and. ncnt.eq.1) goto 20
c
c-----Load precip and cloud profiles, and determine precip rate
c
          do k = 1,nlay
            volrat(k) = 0.
            rr(k) = 0.
            pp(k) = pwr(i,j,k) + pws(i,j,k) + pwg(i,j,k)
          enddo
          do k = kbot,ktop
            volrat(k) = pp(k)/rhoh2o                 ! drop volume/air volume
            rr(k) = (volrat(k)/1.0e-7)**1.27         ! rainfall rate (mm/hr)
          enddo
c
c-----Loop over layers and species for precipitating columns
c
          ltop = .true.
          do 30 k = ktop,kbot,-1
            lcloud = .false.
            lgraupl = .false.
            if (cwc(i,j,k).ge.cwmin) lcloud = .true.
            if (pwg(i,j,k).ge.cwmin) lgraupl = .true.
            cellvol = deltax(j)*deltay*depth(i,j,k)/mapscl(i,j)**2
            rainvol = volrat(k)*cellvol
            rhoair = 100.*press(i,j,k)/(rd*tempk(i,j,k))
c
c-----Calculate scavenging for soluble gas species
c
            do 40 l = 1,nrtgas
              if( rthlaw(l).LT.1.e-6 ) goto 40

              call henryfnc(0,rthlaw(l),rttfact(l),tempk(i,j,k),
     &                      cph(i,j,k),knh3,khno3,kso2,hlaw)
              hlaw = hlaw*rconst*tempk(i,j,k)

              cwat = cwc(i,j,k)
              pwat = pp(k)
              if (tempk(i,j,k).lt.273. .and. rtscale(l).gt.0.) then
                cwat = amax1(0.,cwc(i,j,k)*
     &                          (tempk(i,j,k) - tamin)/(273. - tamin))
                pwat = pwr(i,j,k)
              endif

              delc = 0.
              delm = 0.
              if (ltop) tmass(l) = 0.
              c0(l) = tmass(l)/rainvol
              convfac = densfac*(273./tempk(i,j,k))*(press(i,j,k)/1013.)
              cmin = rtlbnd(l)*convfac
              conc(i,j,k,l) = amax1(cmin,conc(i,j,k,l))
c
              call scavrat(.false.,lcloud,lgraupl,tamin,rr(k),
     &                     tempk(i,j,k),cwat,depth(i,j,k),rhoair,
     &                     conc(i,j,k,l),hlaw,rtdrate(l),rtscale(l),
     &                     0.,0.,gscav,ascav)
c
              c00 = c0(l)*volrat(k)
              totc = conc(i,j,k,l) + c00
              totw = cwat + pwat
              ceq = totc/(1. + hlaw*totw/rhoh2o)
              ceq = totc - ceq
              delc = (ceq - c00)*(1. - exp(-gscav*deltat))
              if (delc.gt.0.) delc = amin1(delc,conc(i,j,k,l)-cmin)
c
c-----Update the cell concentration and rain mass
c
              conc(i,j,k,l) = conc(i,j,k,l) - delc
              delm = delc*cellvol
              tmass(l) = tmass(l) + delm
 40         continue
c
c-----Calculate scavenging for particulate species
c
            if (nrtaero .gt. 0) then
              do 50 l = nrtgas+1,nrtrac
                delc = 0.
                delm = 0.
                if (ltop) tmass(l) = 0.
                cmin = rtlbnd(l)
                conc(i,j,k,l) = amax1(cmin,conc(i,j,k,l))
                psize = 1.e-6*sqrt(rtlcut(l)*rtucut(l))

                call scavrat(.TRUE.,lcloud,lgraupl,tamin,rr(k),
     &                       tempk(i,j,k),0.,depth(i,j,k),
     &                       rhoair,0.,0.,0.,0.,psize,rtdens(l),
     &                       gscav,ascav)

                delc = conc(i,j,k,l)*(1. - exp(-ascav*deltat))
                delc = amin1(delc,conc(i,j,k,l)-cmin)
c
                conc(i,j,k,l) = conc(i,j,k,l) - delc
                delm = delc*cellvol
                tmass(l) = tmass(l) + delm
 50           continue
            endif
            ltop = .false.
 30       continue
c
c-----If rain evaporates before reaching the ground, return all mass
c     back to layer KBOT
c
          if (kbot.gt.1) then
            cellvol = deltax(j)*deltay*depth(i,j,kbot)/mapscl(i,j)**2
            do l = 1,nrtrac
              conc(i,j,kbot,l) = conc(i,j,kbot,l) + tmass(l)/cellvol
              tmass(l) = 0.
            enddo
          endif
c
 20     continue
 10   continue
c
      return
      end

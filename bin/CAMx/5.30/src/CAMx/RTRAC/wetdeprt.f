      subroutine wetdeprt(m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon,
     &                    igrid,ncol,nrow,nlay,nrtsp,deltat,deltax,
     &                    deltay,mapscl,depth,tempk,press,cwc,pwr,
     &                    pws,pwg,cph,densfac,idfin,conc)
      use bndary
      use chmstry
      use tracer
      use rtracchm
c
c----CAMx v5.30 101223
c 
c     This version is for the RTRAC species.
c     WETDEP modifies vertical concentration profiles for a given grid via 
c     precipitation processes.  This subroutine has been completely rewritten
c     for CAMx v4.
c 
c     Copyright 1996 - 2010
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
c        12/22/09   Improved treatment of "c0" array
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
c
      integer ncol,nrow,nlay,nrtsp,igrid,j,i1,i2,i,kbot,ktop,
     &        k,ncnt,l
      integer idfin(m1,m2)
      real deltax(nrow)
      real, dimension(m1,m2,m3) :: tempk,press,
     &                             cwc,pwr,pws,pwg,cph
      real, dimension(ncol,nrow,nlay) :: depth
      real conc(m1,m2,m3,nrtsp)
      real mapscl(m1,m2)
      real rd,rhoh2o,deltat,deltay,densfac,cellvol,rainvol,rhoair,
     &     delc,delm,convfac,cmin,hlaw,gscav,ascav,totc,totw,ceq,
     &     psize,cwat,pwat,rconst,dtfall,drpvel,delc0
      logical lcloud,ltop,lgraupl
c
      real c0(MXTRSP)
      real pp(MXLAYER)
      real rr(MXLAYER)
      real volrat(MXLAYER)
      real tmass(MXTRSP)
c
      integer :: m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon
      integer :: m_xi1,m_xi2,m_xj1,m_xj2
      integer :: m_yi1,m_yi2,m_yj1,m_yj2
      integer :: m_zi1,m_zi2,m_zj1,m_zj2
c
      data rd /287./         ! Dry air gas constant (J/K/kg)
      data rhoh2o /1.e6/     ! water density (g/m3)
      data rconst /8.206e-2/ ! gas constant (l.atm/mol.K)
c
c-----Entry point
c

! Set grid point limits
      m_zi1= ia-1
      m_zi2= iz+1
      m_zj1= ja-1
      m_zj2= jz+1

      if(btest(ibcon,0)) m_zi1 = ia
      if(btest(ibcon,1)) m_zi2 = iz
      if(btest(ibcon,2)) m_zj1 = ja
      if(btest(ibcon,3)) m_zj2 = jz
c
c-----Loop over rows and columns
c
      do 10 j = m_zj1, m_zj2
        do 20 i = m_zi1, m_zi2
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
            cellvol = deltax(j+j0)*deltay*
     &                        depth(i+i0,j+j0,k)/mapscl(i,j)**2
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
              if (ltop) then
                tmass(l) = 0.
                c0(l) = 0.
              endif
              convfac = densfac*(273./tempk(i,j,k))*(press(i,j,k)/1013.)
              cmin = rtlbnd(l)*convfac
              conc(i,j,k,l) = amax1(cmin,conc(i,j,k,l))
c
              call scavrat(.false.,lcloud,lgraupl,tamin,rr(k),
     &                     tempk(i,j,k),cwat,rhoair,conc(i,j,k,l),
     &                     hlaw,rtdrate(l),rtscale(l),0.,0.,gscav,ascav,
     &                     drpvel)
c
              totc = conc(i,j,k,l) + c0(l)
              totw = cwat + pwat
              ceq = totc/(1. + hlaw*totw/rhoh2o)
              ceq = totc - ceq
              if( totw .NE. 0.0 ) then
                 ceq = ceq*pwat/totw
              else
                 ceq = 0.0
              endif
              delc = (ceq - c0(l))*(1. - exp(-gscav*deltat))
              if (delc.gt.0.) delc = amin1(delc,conc(i,j,k,l)-cmin)
c
              dtfall = depth(i+i0,j+j0,k)/drpvel
              delc0 = (ceq - c0(l))*(1. - exp(-gscav*dtfall))
              if (delc0.gt.0.) delc0 = amin1(delc0,conc(i,j,k,l)-cmin)
c
c-----Update the cell concentration and rain mass
c
              conc(i,j,k,l) = conc(i,j,k,l) - delc
              delm = delc*cellvol
              tmass(l) = tmass(l) + delm
              c0(l) = c0(l) + delc0
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
     &                       tempk(i,j,k),0.,rhoair,0.,0.,0.,0.,
     &                       psize,rtdens(l),gscav,ascav,drpvel)

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
            cellvol = deltax(j+j0)*deltay*
     &                        depth(i+i0,j+j0,kbot)/mapscl(i,j)**2
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

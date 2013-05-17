      subroutine reemisrt(m1,m2,m3,i0,j0,ncols,nrows,nlays,nrtsp,delt,
     &                    tsurf,cellat,cellon,fsurf,height,press,windu,
     &                    windv,tempk,lrdruf,icdruf,ruflen,lrdsno,
     &                    icdsno,smasfld,vmasfld,saconc)
      use camxcom
      use rtracchm
c
c----CAMx v5.41 121109
c 
c     REEMISS determines re-emission fluxes of volatile gas tracers from the
c     RTRAC surface model.  The gridded atmospheric RTRAC tracer concentrations 
c     are incremented and surface model mass is decremented accordingly.
c 
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c           
c     Modifications:
c        12/08/09   ---jjung---    Original development
c
c     Input arguments:
c        ncols               number of columns 
c        nrows               number of rows 
c        nlays               number of layers 
c        nrtsp               number of species
c        delt                time step size (s)
c        tsurf               surface temperature field (K)
c        cellat              cell centroid latitude (deg)
c        cellon              cell centroid longitude (deg)
c        fsurf               fractional landuse cover field (fraction)
c        height              layer interface height field (m)
c        press               layer pressure field (mb)
c        windu               layer U-component wind field (m/s)
c        windv               layer V-component wind field (m/s)
c        tempk               layer temperature field (K)
c        lrdruf              flag that gridded roughness is available
c        icdruf              optional surface roughness length index
c        ruflen              optional surface roughness length value (m)
c        lrdsno              flag that gridded snow cover is available
c        icdsno              optional snow index
c        smasfld             RTRAC surface soil mass field (mol/ha)
c        vmasfld             RTRAC surface vegetation mass field (mol/ha)
c
c     Output arguments
c        saconc              RTRAC tracer concentration field (umol/m3,ug/m3)
c        smasfld             RTRAC surface soil mass field (mol/ha)
c        vmasfld             RTRAC surface vegetation mass field (mol/ha)
c             
c     Routines called: 
c        CALDATE
c        MICROMET
c        HENRYFNC
c        VE_GAS_RT
c             
c     Called by: 
c        EMISTRNS
c 
      implicit none
c
      include 'camx.prm'
      include 'deposit.inc'
      include 'rtracsrf.inc'
c
      integer :: m1,m2,m3,i0,j0
      integer :: ncols,nrows,nlays
      integer :: nrtsp
      real delt
      logical lrdruf,lrdsno

      real, dimension(m1,m2) :: tsurf, cellat, cellon
      integer, dimension(m1,m2) :: icdruf, icdsno
      real, dimension(ncols,nrows,nlays) :: height
      real, dimension(m1,m2,m3) :: press, windu, windv, tempk
      real, dimension(m1,m2,NLU) :: fsurf
      real ruflen(NRUF)
      real saconc(m1,m2,m3,nrtsp)
      real smasfld(m1,m2,nrtsp) ! soil deposition
                                ! gas [=] mol/hectare, aerosol[=] g/hectare 
      real vmasfld(m1,m2,nrtsp) ! vegitation deposition
                                ! gas [=] mol/hectare, aerosol[=] g/hectare 

      integer idate,month
      integer i,j,l,m
      integer mbin,latbin,isesn
      real deltaz,temp0,prss0,ucomp,vcomp,wind
      real pbl,ustar,el,psih,wstar
      real henry,ve,rconst
      real soldepth,vegdepth
      real z0
      real Ksa
      real ptsolconc ! soil surface concentration, umol/m3, ug/m3
      real ptvegconc ! veg. surface concentration, umol/m3, ug/m3
      real reemis    ! overall reemission, umol/m2-s, ug/m2-s
      real resolemis ! soil reemission, umol/m2-s, ug/m2-s
      real revegemis ! vegitation reemission, umol/m2-s, ug/m2-s
      real dconc     ! umol/m3, ug/m3
      real tdconc(MXTRSP)
      real tdsolconc(MXTRSP)
      real tdvegconc(MXTRSP)
      real eps       ! default minimum value for deposition value
      real crrctn    ! air conc. correction in case of using eps
      logical lstable
      logical lcrrs  ! correction flag for soil
      logical lcrrv  ! correction flag for veg.
c
      data rconst /8.206e-2/                  ! gas constant (l.atm/mol.K)
      data soldepth/1.0e-3/, vegdepth/1.0e-4/ ! depth of deposit field, m
      data eps/1.0e-20/
c
c-----Entry point
c
      idate = date
      call caldate(idate)
      month = (idate - 10000*int(idate/10000.))/100

c-----Loop over rows and columns
c
      do 30 j = 2,m2-1 
        do 20 i = 2,m1-1
c
          do l = 1,nrtrac
             tdconc(l) = 0.
             tdsolconc(l) = 0.
             tdvegconc(l) = 0.
          enddo
c
c-----Determine season
c
          mbin = month
          if (cellat(i,j).lt.0.) then
            mbin = mod(month+6,12)
            if (mbin.eq.0) mbin = 12
          endif
          latbin = 1
          if (abs(cellat(i,j)).gt.20.) then
            latbin = 2
          elseif (abs(cellat(i,j)).gt.35.) then
            latbin = 3
          elseif (abs(cellat(i,j)).gt.50.) then
            latbin = 4
          elseif (abs(cellat(i,j)).gt.75.) then
            latbin = 5
          endif
          if ((cellat(i,j).gt.50. .and. cellat(i,j).lt.75.) .and.
     &        (cellon(i,j).gt.-15. .and. cellon(i,j).lt.15.)) latbin = 3
          isesn = iseason(latbin,mbin)
c
c-----Use input snow cover to set season, if specified
c
          if (lrdsno .and. icdsno(i,j).eq.1) isesn = 4
c
c-----Load local met variables
c
          deltaz = height(i+i0,j+j0,1)/2.
          temp0 = tsurf(i,j)
          prss0 = press(i,j,1) - 2.*deltaz*(press(i,j,2) - 
     &                            press(i,j,1))/height(i+i0,j+j0,2)
          ucomp = (windu(i,j,1) + windu(i-1,j,1))/2.
          vcomp = (windv(i,j,1) + windv(i,j-1,1))/2.
          wind = sqrt(ucomp**2 + vcomp**2)
          wind = amax1(0.1,wind)
c
c-----Loop over land use; surface roughness for water is dependent on
c     wind speed
c
          do 10 m = 1,NLU
            if (fsurf(i,j,m).lt.0.01) goto 10
            z0 = z0lu(m,isesn)
            if (m.eq.7) z0 = amax1(z0,2.0e-6*wind**2.5)
c
c-----Use input surface roughness, if specified
c
            if (lrdruf .and. icdruf(i,j).gt.0) then
              z0 = ruflen(icdruf(i,j))
            endif
c
c-----Get surface layer micrometeorological parameters for this cell and
c     landuse type
c 
            call micromet(tempk(i,j,1),temp0,press(i,j,1),prss0,
     &                    deltaz,wind,z0,pbl,ustar,el,psih,wstar,lstable)
c
c-----Loop over GAS species, and calculate re-emission velocity for this cell,
c     landuse, and current species.
c
            do 40 l = 1,nrtgas
              call henryfnc(0,rthlaw(l),rttfact(l),temp0,7.,1,1,1,henry)
              call ve_gas_rt(m,z0,deltaz,psih,ustar,rtdrate(l),henry,
     &                       rtscale(l),temp0,wind,ve)
              if (m.eq.7) then
cnowtr          Kaw = 1./(henry*rconst*tsurf(i,j))
cnowtr          ptsolconc = (smasfld(i,j,l)*1.0e-4/soldepth)*Kaw
cnowtr          resolemis = (vd-ve)*saconc(i,j,1,l)*1.0e-6 + 
cnowtr     &                ve*ptsolconc
                resolemis = 0.0
                revegemis = 0.0
                reemis = 0.0
              else
                Ksa = 0.411*1.53*0.59*fsoiloc(m)*eqkoa(l)
                ptsolconc = (smasfld(i,j,l)*1.0e2/soldepth)
                ptvegconc = (vmasfld(i,j,l)*1.0e2/vegdepth)
                resolemis = ve*(saconc(i,j,1,l)+ptsolconc)/(1.+Ksa)
                revegemis = ve*(saconc(i,j,1,l)+ptvegconc)/(1.+eqkoa(l))
                reemis = resolemis + revegemis
              endif
              dconc = reemis*delt/height(i+i0,j+j0,1)
              tdconc(l) = tdconc(l) + dconc*fsurf(i,j,m)
              if (reemis.gt.0.0) then
                tdsolconc(l) = tdsolconc(l) +
     &                dconc*fsurf(i,j,m)*(resolemis/reemis)
                tdvegconc(l) = tdvegconc(l) +
     &                dconc*fsurf(i,j,m)*(revegemis/reemis)
              endif   
 40         continue
 10       continue
          do l = 1, nrtgas
            crrctn = 0.
            lcrrs = .false.
            lcrrv = .false.
            if (smasfld(i,j,l).gt.eps) then
               smasfld(i,j,l) = smasfld(i,j,l) -
     &                       tdsolconc(l)*height(i+i0,j+j0,1)*1.0e-2
            else
               crrctn = crrctn + 
     &            (smasfld(i,j,l) - eps)*1.0e2*soldepth/height(i+i0,j+j0,1)
               smasfld(i,j,l) = eps
               lcrrs = .true.
            endif
            if (vmasfld(i,j,l).gt.eps) then
               vmasfld(i,j,l) = vmasfld(i,j,l) -
     &                       tdvegconc(l)*height(i+i0,j+j0,1)*1.0e-2
            else
               crrctn = crrctn + 
     &            (vmasfld(i,j,l) - eps)*1.0e2*vegdepth/height(i+i0,j+j0,1)
               vmasfld(i,j,l) = eps
               lcrrv = .true.
            endif
            if (.not. lcrrs .and. .not. lcrrv) then
               saconc(i,j,1,l) = saconc(i,j,1,l) + tdconc(l)
            elseif (.not. lcrrs .and. lcrrv) then
               saconc(i,j,1,l) = saconc(i,j,1,l) + tdsolconc(l) +
     &                          crrctn
            elseif (lcrrs .and. .not.lcrrv) then
               saconc(i,j,1,l) = saconc(i,j,1,l) + tdvegconc(l) +
     &                          crrctn
            elseif (lcrrs .and. lcrrv) then
               saconc(i,j,1,l) = saconc(i,j,1,l) + crrctn
            endif
            saconc(i,j,1,l) = MAX(rtlbnd(l),saconc(i,j,1,l))
          enddo
 20     continue
 30   continue
      return
      end

      subroutine drydep(igrid,ncol,nrow,nlay,itzon,tsurf,cellat,cellon,
     &                  pwc,cwc,height,press,windu,windv,fcloud,cldtrns,
     &                  water,fsurf,tempk,lrdruf,icdruf,ruflen,lrddrt,
     &                  icddrt,lrdsno,icdsno,conc,vdep)
c
c-----CAMx v4.51 080522
c 
c     DRYDEP is the driver for the calculation of gridded dry deposition 
c     velocities for a given grid. Deposition velocities are calculated for
c     each gas species and for each aerosol size bin, weighted by the
c     fractional land use specified for each cell.
c 
c     Copyright 1996-2008
c     ENVIRON International Corporation
c           
c     Modifications:
c        4/4/00    Added aerosol deposition as f(size)
c        4/4/01    Fixed a few bugs in the call for VD_AER
c        1/9/02    Aerosol size and density now species-dependent
c        3/26/03   Added scaling factor to surface resistance (provided
c                  on chemparam file), and zero dry deposition for 
c                  insoluble gases
c        4/9/03    Removed rain, added precip and cloud water contents:
c                  surfaces can now be rain-wetted, fog-wetted, or dew-wetted
c        6/6/03    Protect against divide by zero with totland
c        6/11/03   Use optional surface roughness length, if available
c        7/21/03   Use optional drought stress and snow cover, if available;
c                  Introduced latitude-dependent specification of season;
c                  Revised solar flux calculation to use RADM cloud adjustment
c        8/18/03   Relate drought stress to Palmer Drought Index
c        4/21/04   Incorporated sectional PM
c        11/19/04  Incorporated season-dependent roughness length
c
c     Input arguments:
c        igrid               grid index 
c        ncol                number of columns 
c        nrow                number of rows 
c        nlay                number of layers 
c        itzon               time zone
c        tsurf               surface temperature field (K)
c        cellat              cell centroid latitude (deg)
c        cellon              cell centroid longitude (deg)
c        pwc                 precipitation water content (g/m3)
c        cwc                 cloud water content (g/m3)
c        height              layer interface height field (m)
c        press               layer pressure field (mb)
c        windu               layer U-component wind field (m/s)
c        windv               layer V-component wind field (m/s)
c        fcloud              fractional cloud cover field (fraction)
c        cldtrns             cloud energy transmission coefficient (fraction)
c        water               layer water vapor field (ppm)
c        fsurf               fractional landuse cover field (fraction)
c        tempk               layer temperature field (K)
c        lrdruf              flag that gridded roughness is available
c        icdruf              optional surface roughness length index
c        ruflen              optional surface roughness length value (m)
c        lrddrt              flag that gridded drought stress is available
c        icddrt              optional drought index
c        lrdsno              flag that gridded snow cover is available
c        icdsno              optional snow index
c        conc                concentration field (umol/m3, ug/m3)
c             
c     Output arguments: 
c        vdep                species-dependent deposition velocity field (m/s)
c             
c     Routines called: 
c        CALDATE
c        GETZNTH
c        MICROMET
c        VD_GAS
c        VD_AER
c        HENRYFNC
c             
c     Called by: 
c        CAMx 
c 
      include 'camx.prm'
      include 'camx.com'
      include 'bndary.com'
      include 'deposit.com'
      include 'chmstry.com'
      include 'filunit.com'
      include 'camx_aero.com'
c
      real tsurf(ncol,nrow),cellat(ncol,nrow),cellon(ncol,nrow),
     &     ruflen(NRUF)
      integer icdruf(ncol,nrow),icddrt(ncol,nrow),icdsno(ncol,nrow)
      real height(ncol,nrow,nlay),press(ncol,nrow,nlay),
     &     windu(ncol,nrow,nlay),windv(ncol,nrow,nlay),
     &     fcloud(ncol,nrow,nlay),cldtrns(ncol,nrow,nlay),
     &     water(ncol,nrow,nlay),fsurf(ncol,nrow,NLU),
     &     tempk(ncol,nrow,nlay),pwc(ncol,nrow,nlay),cwc(ncol,nrow,nlay)
      real vdep(ncol,nrow,nspec)
      real conc(ncol,nrow,nlay,nspec)
      logical lstable
      logical ldark,lrdruf,lrddrt,lrdsno
c
      data pi/3.1415927/
c
c-----Entry point
c
      idate = date
      call caldate(idate)
      month = (idate - 10000*int(idate/10000.))/100
c
c-----Loop over rows and columns
c
      do 30 j = 2,nrow-1 
        i1 = 2 
        i2 = ncol-1 
        if (igrid.eq.1) then 
          if (ibeg(j).eq.-999) goto 30 
          i1 = ibeg(j) 
          i2 = iend(j) 
        endif 
        do 20 i = i1,i2 
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
c-----Calculate solar flux
c
          call getznth(cellat(i,j),cellon(i,j),time,date,itzon,
     &                 zenith,ldark)
          coszen = cos(zenith*pi/180.)
          solflux = (990.*coszen - 30.)*
     &              (1. - fcloud(i,j,1)*(1. - cldtrns(i,j,1)))
          solflux = amax1(0.,solflux)
c
c-----Load local met variables
c
          deltaz = height(i,j,1)/2.
          temp0 = tsurf(i,j) - 273.15
          prss0 = press(i,j,1) - 
     &            2.*deltaz*(press(i,j,2) - press(i,j,1))/height(i,j,2)
          ucomp = (windu(i,j,1) + windu(i-1,j,1))/2.
          vcomp = (windv(i,j,1) + windv(i,j-1,1))/2.
          wind = sqrt(ucomp**2 + vcomp**2)
          wind = amax1(0.1,wind)
c
c-----Determine surface wetness
c
          iwet = 0
          if (pwc(i,j,1).gt.cwmin) then
            iwet = 2
          elseif (cwc(i,j,1).gt.cwmin) then
            iwet = 1
          else
            qwatr = 1.e-6*water(i,j,1)*18./28.8
            ev = qwatr*prss0/(qwatr + eps) 
            es = e0*exp((lv/rv)*(1./273. - 1./tsurf(i,j))) 
            rh = amin1(1.,ev/es)
            dew = (1. - rh)*(wind + 0.6) 
            if (dew.lt.0.19) iwet = 1
          endif
c
c-----Use input drought stress, if specified
c
          istress = 0
          if (lrddrt .and. icddrt(i,j).gt.0) istress = icddrt(i,j)
c
c-----Loop over land use; surface roughness for water is dependent on
c     wind speed
c
          do l = 1,nspec
            vdep(i,j,l) = 0.
          enddo
          totland = 0.
c
          do 10 m = 1,NLU
            if (fsurf(i,j,m).lt.0.01) goto 10
            totland = totland + fsurf(i,j,m)
            z0 = z0lu(m,isesn)
            if (m.eq.7) z0 = amax1(z0,2.0e-6*wind**2.5)
c
c-----Use input surface roughness, if specified
c
            if (lrdruf .and. icdruf(i,j).gt.0) z0 = ruflen(icdruf(i,j))
c
c-----Get surface layer micrometeorological parameters for this cell and
c     landuse type
c 
            if (prss0.lt.0) then
              write(iout,'(//,a)') 'ERROR in DRYDEP:'
              write(iout,*) 'Invalid pressure value'
              write(iout,*) 'Cell   Height  Deltaz'
              write(iout,*) i,j,height(i,j,1),deltaz
              call camxerr()
            endif
            call micromet(tempk(i,j,1),tsurf(i,j),press(i,j,1),prss0,
     &                    deltaz,wind,z0,pbl,ustar,psih,wstar,lstable) 
c
c-----Loop over GAS species, and calculate deposition velocity for this cell,
c     landuse, and current species.
c     Use input drought stress code, if specified
c
            call henryfnc(0,henso20,tfactso2,tsurf(i,j),7.,knh3,khno3,
     &                    kso2,henso2)
            do 40 l = 1,ngas
              if (henry0(l).gt.1.e-6) then
                call henryfnc(l,henry0(l),tfact(l),tsurf(i,j),7.,knh3,
     &                        khno3,kso2,henry)
                iflgso2 = 0
                iflgo3 = 0
                if (l.eq.kso2) then
                  iflgso2 = 1
                  henry = henso2 
                endif
                if (l.eq.ko3) iflgo3 = 1
                call vd_gas(m,istress,iwet,iflgso2,iflgo3,z0,
     &                     deltaz,psih,ustar,diffrat(l),henry,henso2,
     &                     f0(l),rscale(l),temp0,dstress(0),solflux,
     &                     rj(m,isesn),rlu(m,isesn),rac(m,isesn),
     &                     rlcs(m,isesn),rlco(m,isesn),rgss(m,isesn),
     &                     rgso(m,isesn),vd)
              else
                vd = 0.
              endif
              vdep(i,j,l) = vdep(i,j,l) + vd*fsurf(i,j,m)
 40         continue
c
c-----Loop over AEROSOL size bins, and calculate deposition velocity for
c     this cell and landuse
c
            if (naero .gt. 0) then
c
c-----Recalculate particle size for wet diameter
c
c
c-----CF 2-bin scheme
c
              if (aeropt.eq.'CF') then
                isemptyf = 1
                isemptyc = 1
                qtf = 0. ! fine dry total mass
                qtc = 0. ! coarse dry total mass
                vtf = 0. ! fine dry total volume
                vtc = 0. ! coarse dry total volume
                do l = ngas+1,nspec
                  if (dcut(l,2).lt.(dcut(kph2o,2)+1.e-5)) then     !fine
                    if (l.ne.kph2o) then
                      if (conc(i,j,1,l).gt.bdnl(l)) isemptyf = 0
                      qtf = qtf + conc(i,j,1,l)
                      vtf = vtf + conc(i,j,1,l)/roprt(l)
                    endif
                  else                                             !coarse
                    if (conc(i,j,1,l).gt.bdnl(l)) isemptyc = 0
                    qtc = qtc + conc(i,j,1,l)
                    vtc = vtc + conc(i,j,1,l)/roprt(l)
                    diamc = sqrt(dcut(l,1)*dcut(l,2))
                  endif
                enddo
                vdf = 0.
                if (isemptyf.eq.0) then
                  roprta = (qtf + conc(i,j,1,kph2o)) / 
     &                     (vtf + conc(i,j,1,kph2o)/roprt(kph2o))
                  diam = sqrt(dcut(kph2o,1)*dcut(kph2o,2))
                  diam = 1.e-6*diam*(1. + 
     &                   conc(i,j,1,kph2o)/roprt(kph2o)/vtf)**0.33333
                  call vd_aer(z0,deltaz,psih,ustar,diam,roprta,
     &                        tsurf(i,j),vdf)
                endif
                vdc = 0.
                if (isemptyc.eq.0) then
                  roprta = qtc/vtc
                  diam = 1.e-6*diamc
                  call vd_aer(z0,deltaz,psih,ustar,diam,roprta,
     &                        tsurf(i,j),vdc)
                endif
                do l = ngas+1,nspec
                  if (dcut(l,2).lt.(dcut(kph2o,2)+1.e-5)) then
                    vdep(i,j,l) = vdep(i,j,l) + vdf*fsurf(i,j,m)
                  else
                    vdep(i,j,l) = vdep(i,j,l) + vdc*fsurf(i,j,m)
                  endif
                enddo
c
c-----CMU multi-section scheme
c
              elseif (aeropt.eq.'CMU') then
                kwtr = (kph2o_1 - ngas)/nbin + 1
                if (nbin.eq.1) kwtr = kph2o_1 - ngas
                do isec = 1,nbin
                  isempty = 1
                  qt = 0. ! dry total mass
                  vt = 0. ! dry total volume
                  do iaero = 1,naero
                    if (iaero.ne.kwtr) then
                      l = ngas + (iaero - 1)*nbin + isec
                      if (conc(i,j,1,l).gt.bdnl(l)) isempty = 0
                      qt = qt + conc(i,j,1,l)
                      vt = vt + conc(i,j,1,l)/roprt(l)
                    endif
                  enddo
                  vd = 0.
                  if (isempty.eq.0) then
                    roprta = (qt + conc(i,j,1,kph2o_1-1+isec)) /
     &                       (vt + conc(i,j,1,kph2o_1-1+isec)/
     &                        roprt(kph2o_1))
                    diam = sqrt(dcut(ngas+isec,1)*dcut(ngas+isec,2))
                    diam = 1.e-6*diam*(1. + conc(i,j,1,kph2o_1-1+isec)/
     &                     roprt(kph2o_1)/vt)**0.33333
                    call vd_aer(z0,deltaz,psih,ustar,diam,roprta,
     &                          tsurf(i,j),vd)
                  endif
                  do iaero = 1,naero
                    l = ngas + (iaero - 1)*nbin + isec
                    vdep(i,j,l) = vdep(i,j,l) + vd*fsurf(i,j,m)
                  enddo
                enddo
c
c-----Other scheme
c
              else
                do l = ngas+1,nspec
                  diam = 1.e-6*sqrt(dcut(l,1)*dcut(l,2))
                  call vd_aer(z0,deltaz,psih,ustar,diam,roprt(l),
     &                        tsurf(i,j),vd)
                  vdep(i,j,l) = vdep(i,j,l) + vd*fsurf(i,j,m)
                enddo
              endif
c
            endif
 10       continue
c
c-----Calculate final landuse-weighted deposition velocities
c
          totland = amax1(totland, 0.01)
          do l = 1,nspec
            vdep(i,j,l) = vdep(i,j,l)/totland
          enddo
c
 20     continue
 30   continue
c
      return
      end

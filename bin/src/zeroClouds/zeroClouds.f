      program zeroClouds
c
c-----zeroClouds (derived from MM5CAMx v 4.8) creates a Cloud rain file without 
c     clouds or rain.
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c 
c  
c This program is free software; you can redistribute it and/or 
c modify it under the terms of the GNU General Public License 
c as published by the Free Software Foundation; either version 2 
c of the License, or (at your option) any later version. 
c  
c This program is distributed in the hope that it will be useful, 
c but WITHOUT ANY WARRANTY; without even the implied warranty of 
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
c GNU General Public License for more details. 
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc 
c
      implicit none
      
      include 'param.inc'
      include 'fields.inc'
c
      integer addday,subday

      integer kz1(mnzc),kz2(mnzc),kzin(mnzc),isnow(mnxc,mnyc)
      integer sdate,edate,stime,etime,jdate,jhr,itzon,nxc,nyc,nzc,
     &        izone,ioffset,joffset,k,iunit,nfiles,nf,ierr,nx,ny,nz,
     &        mm5date,mm5hr,jdatep,jhrp,jmnp,i,j,kk,junit,dtout
      integer kup,kdn,nlu
      real zm(mnzc),zz(mnzc),thetav(mnzc),uwind(mnzc),vwind(mnzc),
     &     tkep(mnzc),rkc(mnzc),tt(mnzc),pp(mnzc),qv(mnzc),cc(mnzc)
      real lwat,zero,tkemin,gamma,dxcamx,dycamx,x0camx,y0camx,
     &     deltax,hr,hrp,th,qq,press0,wind,ustar,eli,wstar,
     &     sumtkz,sumtk,xinf,q,z,dz,eee,al1,rwat,swat,gwat,delz,volrat,
     &     clonin,clatin,tlat1in,tlat2in,sumf,sumff
      real qhi,qlo
      character*100 fname 
      character*20 cldhdr
      character*10 kvmeth,project
      logical lfirst,lfill,lstagw,ltopo,lsnow,lsubcld
      data zero /0./
      data lfirst/.true./
      data ltopo /.true./
      data lfill /.true./
      data cldhdr /'CAMx_V4.3 CLOUD_RAIN'/
      data tkemin /1.e-6/
      data gamma /0.286/
c
c-----Initialize variables
c
      jdate = 0
      jhr = 0
      ioffset = -1
      joffset = -1
c
c-----Get user-supplied inputs
c
      read(*,'(20x,a10)') kvmeth
      write(*,'(a,1x,a10)') '                      Kv Method:',kvmeth
      if (kvmeth.ne.'OB70' .and. kvmeth.ne.'TKE' .and. kvmeth.ne.'CMAQ'
     &    .and. kvmeth.ne.'ACM2') then
        write(*,*) 'Unknown Kv Method keyword: ',kvmeth
        stop
      endif
      read(*,'(20x,f)') kvmin
      write(*,'(a,f10.2)')  '                     Minimum Kv:',kvmin
      read(*,'(20x,a10)') project
      write(*,'(a,1x,a10)') '                     Projection:',project
      read(*,'(20x,l10)') lsnow 
      write(*,'(a,1x,l10)') '              Output snow cover:',lsnow
c     read(*,'(20x,l10)') lz03 
c     write(*,'(a,1x,l10)') '       Output Zhang03 26-cat LU:',lz03
      nlu = 11
c     if (lz03) nlu = 26
      read(*,'(20x,l10)') lsubcld
      write(*,'(a,1x,l10)') '        Process sub-grid clouds:',lsubcld
      read(*,'(20x,a)') fname
      read(fname,*) sdate,edate
      stime = mod(sdate,100)
      etime = mod(edate,100)
      sdate = sdate/100
      edate = edate/100
      call juldate(sdate)
      call juldate(edate)
      if (stime.gt.23) then
        stime = stime - 24
        sdate = addday(sdate)
      endif
      if (etime.gt.23) then
        etime = etime - 24
        edate = addday(edate)
      endif
      stime = 100*stime
      etime = 100*etime
      write(*,'(a,i6.5,i5.4)') '   Start date/time (YYJJJ HHMM):',
     &                          sdate,stime
      write(*,'(a,i6.5,i5.4)') '     End date/time (YYJJJ HHMM):',
     &                          edate,etime
      read(*,'(20x,a)') fname
      read(fname,*) dtout
      write(*,'(a,i10)')       '          MM5 output freq (min):',dtout
      if (dtout.lt.60 .and. amod(60.,float(dtout)).ne.0.) then
        write(*,*)'Met output frequency does not divide an hour evenly'
        stop
      endif
      read(*,'(20x,i10)') itzon
      write(*,'(a,i10)')       '                      Time zone:',itzon
      read(*,'(20x,a)') fname
      read(fname,*) nxc,nyc,nzc
      write(*,'(a,3i10)')'                 CAMx grid size:',nxc,nyc,nzc
      if (nxc.gt.mnxc .or. nyc.gt.mnyc .or. nzc.gt.mnzc) then
        write(*,*)'CAMx dimensions too large for arrays'
        write(*,*)'Increase array dimensions in param.inc and recompile'
        stop
      endif
      read(*,'(20x,a)') fname
      read(fname,*) dxcamx,dycamx
      write(*,'(a,2f10.3)')
     &                  '              CAMx grid spacing:',dxcamx,dycamx
      if (project.eq.'UTM       ') then
        read(*,'(20x,a)') fname
        read(fname,*) x0camx,y0camx,izone
        write(*,'(a,2f10.3)')
     &                  '    CAMx UTM Origin (SW corner):',x0camx,y0camx
        write(*,'(a,i10,/)') '                     UTM zone:',izone
      elseif (project.eq.'LATLON    ') then
        read(*,'(20x,a)') fname
        read(fname,*) x0camx,y0camx
        write(*,'(a,2f10.3,/)') 
     &      'CAMx Lat/Lon Origin (SW corner):',x0camx,y0camx
      elseif (project.eq.'LCP       ') then
        read(*,'(20x,a)') fname
        read(fname,*) x0camx,y0camx,clonin,clatin,tlat1in,tlat2in
        write(*,'(a,2f10.3)')
     &                  '    CAMx LCP Origin (SW corner):',x0camx,y0camx
        write(*,'(a,4f10.3,/)')
     &  '    CAMx LCP Projection Params :',clonin,clatin,tlat1in,tlat2in
      else
        write(*,'(a,a)') 'Unknown Projection Keyword: ',project
        stop
      endif
      read(*,'(20x,a)') fname
      read(fname,*) (kzin(k),k=1,nzc)

      read(*,'(20x,a)') fname
      iunit = 9
      open(unit=iunit,file=fname,form='unformatted')
      write(*,*)'Opened CAMx LU file: ',fname

      read(*,'(20x,a)') fname
      iunit = 10
      open(unit=iunit,file=fname,form='unformatted')
      write(*,*)'Opened CAMx Z-P file: ',fname

      read(*,'(20x,a)') fname
      iunit = iunit + 1
      open(unit=iunit,file=fname,form='unformatted')
      write(*,*)'Opened CAMx wind file: ',fname

      read(*,'(20x,a)') fname
      iunit = iunit + 1
      open(unit=iunit,file=fname,form='unformatted')
      write(*,*)'Opened CAMx temp file: ',fname

      read(*,'(20x,a)') fname
      iunit = iunit + 1
      open(unit=iunit,file=fname,form='unformatted')
      write(*,*)'Opened CAMx Kv file: ',fname

      read(*,'(20x,a)') fname
      iunit = iunit + 1
      open(unit=iunit,file=fname,form='unformatted')
      write(*,*)'Opened CAMx moisture file: ',fname

      read(*,'(20x,a)') fname
      iunit = iunit + 1
      open(unit=iunit,file=fname,form='unformatted')
      write(*,*)'Opened CAMx cloud/rain file: ',fname
      write(iunit) cldhdr,nxc,nyc,nzc

      read(*,'(20x,a)') fname
      if (lsnow) then
        iunit = iunit + 1
        open(unit=iunit,file=fname)
        write(*,*)'Opened CAMx/AHO snow cover file: ',fname
        do j = 1,nyc
          do i = 1,nxc
            isnow(i,j) = 0
          enddo
        enddo
      endif
c
c-----Determine MM5 layer mapping in CAMx layers
c
      do k = 1,nzc
        kz2(k) = kzin(k)
        if (k.eq.1) then
          kz1(k) = 1
        else
          kz1(k) = kz2(k-1) + 1
        endif
        write(*,'(a,i2,a,i2,a,i3)')'CAMx layer: ',k,
     &        ' contains MM5 layers ',kz1(k),' to',kz2(k)
      enddo
c
c-----Loop over number of input MM5 files
c
      read(*,'(20x,i10)') nfiles
      do 200 nf = 1,nfiles
        iunit = 20 + nf 
        ierr = 0
        read(*,'(20x,a)') fname
        open(unit=iunit,file=fname,status='old',form='unformatted')
        write(*,*)
        write(*,*)'Opened input MM5 file: ',fname
c
c-----Read raw MM5 data and convert to CAMx index/units convention
c
 100    call readmm5(iunit,lfirst,lsubcld,project,kvmeth,nxc,nyc,nzc,
     &               ioffset,joffset,dxcamx,dycamx,x0camx,y0camx,izone,
     &               clonin,clatin,tlat1in,tlat2in,mm5date,mm5hr,itzon,
     &               dtout,nx,ny,nz,deltax,ierr)
        if (ierr.eq.1) goto 200
c 
        if (mm5date.lt.jdate .or.
     &     (mm5date.eq.jdate .and. mm5hr.le.jhr)) goto 100
        jdate = mm5date
        jhr = mm5hr
        hr = float(jhr)
        if (jdate.lt.sdate .or. 
     &     (jdate.eq.sdate .and. jhr.lt.stime)) goto 100
        write(*,'(a,t30,i6.5,i5.4,/)')'CAMx date/time (YYJJJ HHMM):',
     &                                 jdate,jhr
c
c-----Special date/time variables for precip and clouds
c
        jdatep = jdate
        jhrp = jhr/100
        jmnp = mod(jhr,100)
        if (dtout.lt.60) then
          jmnp = jmnp - dtout
          if (jmnp.lt.0) then
            jmnp = jmnp + 60
            jhrp = jhrp - 1
            if (jhrp.lt.0) then
              jhrp = jhrp + 24
              jdatep = subday(jdatep)
            endif
          endif
        else
          jhrp = jhrp - dtout/60
          if (jhrp.lt.0) then
            jhrp = jhrp + 24
            jdatep = subday(jdatep)
          endif
        endif
        jhrp = 100*jhrp + jmnp
        hrp = float(jhrp)
c
c-----Horizontally interpolate, vertically aggregate, MM5 data to CAMx grid
c
c     This is where the user should substitute new routines that handle
c     interpolation to a specific grid type/projection
c
        if (project.eq.'UTM       ' .or. project.eq.'LCP2      ') then
          lstagw = .false.
          call interp_cart(nx,ny,nz,nxc,nyc,nzc,kz1,kz2,nlu,deltax,
     &                     kvmeth)
        elseif (project.eq.'LATLON    ') then
          lstagw = .false.
          call interp_geo(nx,ny,nz,nxc,nyc,nzc,kz1,kz2,nlu,deltax,
     &                    kvmeth)
        elseif (project.eq.'LCP1      ') then
          lstagw = .true.
          call interp_lcp(nx,ny,nz,nxc,nyc,nzc,kz1,kz2,
     &                    ioffset,joffset,nlu,deltax,dxcamx,kvmeth)
        endif
c
        do j = 1,nyc
          do i = 1,nxc
c
c-----Diagnose Kv profiles: OB70, TKE, CMAQ options
c
            do k = 1,nzc
              zz(k) = zhc(i,j,k)
              zm(k) = zz(k)/2.
              if (k.gt.1) zm(k) = (zz(k) + zz(k-1))/2.

              th = tac(i,j,k)*(1000./pac(i,j,k))**gamma
              qq = qac(i,j,k)*18./28.8/1.e6
              thetav(k) = th*(1. + 0.61*qq)

              uwind(k) = uac(i,j,k)
              if (lstagw .and. i.gt.1)
     &          uwind(k) = (uac(i,j,k) + uac(i-1,j,k))/2.
              vwind(k) = vac(i,j,k)
              if (lstagw .and. j.gt.1)
     &          vwind(k) = (vac(i,j,k) + vac(i,j-1,k))/2.
  
              tkep(k) = tkc(i,j,k)
            enddo
            press0 = pac(i,j,1) - zm(1)*(pac(i,j,2) - pac(i,j,1))/
     &                                  (zm(2) - zm(1))
            wind = sqrt(uwind(1)**2 + vwind(1)**2)

            if (kvmeth.eq.'OB70') then
              call kv_ob70(nzc,kvmin,pblc(i,j),zz,thetav,uwind,vwind,
     &                     rkc)

            elseif (kvmeth.eq.'CMAQ') then
              call micromet(tac(i,j,1),tsfc(i,j),pac(i,j,1),press0,
     &                      zm(1),wind,z0c(i,j),pblc(i,j),ustar,eli,
     &                      wstar)
              call kv_cmaq(nzc,pblc(i,j),ustar,eli,wstar,zm,zz,thetav,
     &                     uwind,vwind,kvmin,rkc)

            elseif (kvmeth.eq.'TKE') then
              sumtkz = 0.
              sumtk = 0.
              xinf = 0.
              do k = 1,nz
                q = sqrt(2.*tktmp(i,j,k))
                z = ztmp(i,j,k)
                if (tktmp(i,j,k).gt.tkemin) then
                  dz = ztmp(i,j,k)
                  if (k.gt.1) dz = ztmp(i,j,k) - ztmp(i,j,k-1)
                  eee = q*dz
                  sumtk = sumtk + eee
                  sumtkz = sumtkz + eee*z
                endif
              enddo
              if (sumtk.gt.0.) xinf = sumtkz/sumtk
              al1 = 0.1
              if (thetav(2).lt.thetav(1)) al1 = 0.075
              xinf = al1*xinf
              xinf = min(xinf,2000.)
              xinf = max(xinf,5.)
              call kv_tke(nzc,thetav,uwind,vwind,zm,zz,xinf,tkep,
     &                    kvmin,rkc)
            endif
c
c-----Prepare cloud fields
c
            do k = 1,nzc
              cwc(i,j,k) = 0.
              pwr(i,j,k) = 0.
              pws(i,j,k) = 0.
              pwg(i,j,k) = 0.
              cod(i,j,k) = 0.
            enddo
            do k = 1,nzc
              if (kz2(k)+1.gt.nz) then
                cod(i,j,k) = 0.
              else
                cod(i,j,k) = odtmp(i,j,kz2(k)+1)
              endif
            enddo
c
c-----Calculate mean CWC and PWR/C for each CAMx layer
c
            do k = 1,nzc
              lwat = 0.
              rwat = 0.
              swat = 0.
              gwat = 0.
              delz = zhc(i,j,k)
              if (k.gt.1) delz = zhc(i,j,k) - zhc(i,j,k-1)
              do kk = kz1(k),kz2(k)
                dz = ztmp(i,j,kk)
                if (kk.gt.1) dz = ztmp(i,j,kk) - ztmp(i,j,kk-1)        
                lwat = lwat + cwtmp(i,j,kk)*dz
                rwat = rwat + prtmp(i,j,kk)*dz
                swat = swat + pstmp(i,j,kk)*dz
                gwat = gwat + pgtmp(i,j,kk)*dz
              enddo
              if (lwat.gt.0.) cwc(i,j,k) = lwat/delz
              if (rwat.gt.0.) pwr(i,j,k) = rwat/delz
              if (swat.gt.0.) pws(i,j,k) = swat/delz
              if (gwat.gt.0.) pwg(i,j,k) = gwat/delz
            enddo
c
c-----Diagnose Kv profiles using ACM2 option
c
            if (kvmeth.eq.'ACM2') then
              do k = 1,nzc
                zz(k) = zhc(i,j,k)
                tt(k) = tac(i,j,k)
                pp(k) = pac(i,j,k)
                qv(k) = qac(i,j,k)
                cc(k) = cwc(i,j,k)
                uwind(k) = uac(i,j,k)
                if (lstagw .and. i.gt.1)
     &            uwind(k) = (uac(i,j,k) + uac(i-1,j,k))/2.
                vwind(k) = vac(i,j,k)
                if (lstagw .and. j.gt.1)
     &            vwind(k) = (vac(i,j,k) + vac(i,j-1,k))/2.
              enddo
              call kv_acm2(nzc,zz,uwind,vwind,tt,qv,cc,pp,tsfc(i,j),
     &                     z0c(i,j),pblc(i,j),kvmin,rkc)
            endif
            do k = 1,nzc
              rkv(i,j,k) = rkc(k)
            enddo
c
c-----Identify cells covered in snow
c
            if (snow(i,j) .gt. 0.) isnow(i,j) = 1
          enddo
        enddo
c
c-----Write to CAMx files
c
c-----Landuse and topography: renormalize landuse fractions
c
        if (ltopo) then
          do j = 1,nyc
            do i = 1,nxc
              sumf = 0.
              sumff = 0.
              do k = 1,nlu
                if (lucx(i,j,k).lt.0.) lucx(i,j,k) = 0.
                sumf = sumf + lucx(i,j,k)
              enddo
              do k = 1,nlu
                lucx(i,j,k) = lucx(i,j,k)/sumf
                sumff = sumff + lucx(i,j,k)
              enddo
              if (sumff.lt.0.99 .or. sumff.gt.1.01) then
                write(*,*)'Land cover total not = 1!',i,j,sumff
              endif
            enddo
          enddo
          junit = 9
c         if (lz03) then
c           write(junit)'LUCAT26'
c         else
c           write(junit)'LUCAT11'
c         endif
          write(junit) (((lucx(i,j,k),i=1,nxc),j=1,nyc),k=1,nlu)
c         write(junit)'TOPO'
          write(junit) ((topcx(i,j),i=1,nxc),j=1,nyc) 
          call stats('Landuse fraction    ',lucx,nxc,nyc,nlu)
          write(*,*)
          call stats('Topography (m)      ',topcx,nxc,nyc,1)
          write(*,*)
          ltopo = .false.
        endif
c
c-----Time-varying met fields
c
        junit = 10
        do k = 1,nzc 
          write(junit) hr,jdate,((zhc(i,j,k),i=1,nxc),j=1,nyc) 
          write(junit) hr,jdate,((pac(i,j,k),i=1,nxc),j=1,nyc) 
        enddo
        call stats('Height (m)          ',zhc,nxc,nyc,nzc)
        write(*,*)
        call stats('Pressure (mb)       ',pac,nxc,nyc,nzc)
        write(*,*)
c
        junit = 11
        write(junit) hr,jdate,lstagw
        do k = 1,nzc
          write(junit) ((uac(i,j,k),i=1,nxc),j=1,nyc) 
          write(junit) ((vac(i,j,k),i=1,nxc),j=1,nyc) 
        enddo 
        write(junit) zero
        call stats('U-comp (m/s)        ',uac,nxc,nyc,nzc)
        write(*,*)
        call stats('V-comp (m/s)        ',vac,nxc,nyc,nzc)
        write(*,*)
c
        junit = 12
        write(junit) hr,jdate,((tsfc(i,j),i=1,nxc),j=1,nyc) 
        do k = 1,nzc 
          write(junit) hr,jdate,((tac(i,j,k),i=1,nxc),j=1,nyc) 
        enddo
        call stats('Sfc Temp (K)        ',tsfc,nxc,nyc,1)
        call stats('Temperature (K)     ',tac,nxc,nyc,nzc)
        write(*,*)
c
        junit = 13
        do k = 1,nzc 
          write(junit) hr,jdate,((rkv(i,j,k),i=1,nxc),j=1,nyc) 
        enddo 
        call stats('Kv (m2/s)           ',rkv,nxc,nyc,nzc)
        write(*,*)
c
c-----Check for (and fill) humidity holes (Qa < 1 ppm)
c
        do 300 j = 1,nyc
          do 300 i = 1,nxc
            do k = 1,nzc
              if (qac(i,j,k).le.1.) then
                write(*,'(a,i3,a,i3,a)') 'Column (',i,',',j,
     &                ') has unreasonably low humidity! Check MM5 Data'
                if (lfill) then
                  qhi = 0.
                  qlo = 0.
                  if (k.gt.1) qlo = qac(i,j,k-1)
                  do kup = k+1,nzc
                    if (qac(i,j,kup).gt.1.) then
                      qhi = qac(i,j,kup)
                      goto 301
                    endif
                  enddo
 301              if (qhi.le.1. .and. qlo.le.1.) then
                    write(*,'(a,i3,a,i3,a)') 'Entire column (',i,',',j,
     &                                ') has unreasonably low humidty!'
                    write(*,'(a)') 'Stopping! Check MM5 Data'
                    stop
                  elseif (qhi.gt.1. .and. qlo.le.1.) then
                    do kk = 1,kup-1
                      qac(i,j,kk) = qhi
                    enddo
                  elseif (qhi.le.1. .and. qlo.gt.1) then
                    do kk = k,nzc
                      qac(i,j,kk) = qlo
                    enddo
                  else 
                    do kk = k,kup-1
                      qac(i,j,kk) = (qhi + qlo)*0.5
                    enddo
                  endif
                else
                  write(*,'(a)') 'You can fix this using the lfill flag'
                  write(*,'(a)') 'See the README file'
                  stop
                endif
                goto 300
              endif
            enddo
 300    continue
c
        junit = 14
        do k = 1,nzc 
          write(junit) hr,jdate,((qac(i,j,k),i=1,nxc),j=1,nyc) 
        enddo 
        call stats('Humidity (ppm)      ',qac,nxc,nyc,nzc)
        write(*,*)
c
        if (jdatep.lt.sdate .or.
     &     (jdatep.eq.sdate .and. jhrp.lt.stime)) goto 100
        write(*,'(a,t30,i6.5,i5.4,/)')'Cld/rn date/time (YYJJJ HHMM):',
     &                                 jdatep,jhrp
        junit = 15
        write(junit) hrp,jdatep
        do k = 1,nzc
          write(junit) ((cwc(i,j,k),i=1,nxc),j=1,nyc) 
          write(junit) ((pwr(i,j,k),i=1,nxc),j=1,nyc) 
          write(junit) ((pws(i,j,k),i=1,nxc),j=1,nyc) 
          write(junit) ((pwg(i,j,k),i=1,nxc),j=1,nyc) 
          write(junit) ((cod(i,j,k),i=1,nxc),j=1,nyc) 
          do j = 1,nyc
            do i = 1,nxc
              pwtr(i,j,k) = pwr(i,j,k) + pws(i,j,k) + pwg(i,j,k)
            enddo
          enddo
        enddo 
        call stats('Cloud Water (g/m3)  ',cwc,nxc,nyc,nzc)
        write(*,*)
        call stats('Precip Water (g/m3) ',pwtr,nxc,nyc,nzc)
        write(*,*)
        call stats('Cloud Optical Depth ',cod,nxc,nyc,nzc)
        write(*,*)
        do j = 1,nyc
          do i = 1,nxc
            volrat = pwtr(i,j,1)/1.e6
            rain(i,j) = (volrat/1.0e-7)**1.27
          enddo
        enddo
        call stats('Precip Rate (mm/hr) ',rain,nxc,nyc,1)
c
        if (jdate.eq.edate .and. jhr.eq.etime) then
          if (lsnow) then
            junit = 16
            write(junit,'(a10,2(i10.5,f10.2))') 
     &            'SNOW      ',sdate,float(stime),edate,float(etime)
            do j = nyc,1,-1
              write(junit,'(9999i1)') (isnow(i,j),i=1,nxc)
            enddo
          endif
          write(*,'(/,a,/)')'Reached CAMx end date/hour; run completed'
          goto 999
        endif
        goto 100
c
 200  continue
      write(*,'(/,a,/)')'End of MM5 data; program run incomplete'
 999  continue
      stop
      end
c
c-----Date functions
c
      integer function addday(idate)
      implicit none
      integer idate,iyr,idy
      iyr = idate/1000
      idy = idate - iyr*1000
      if ((mod(iyr,4).eq.0 .and. idy.eq.366) .or.
     &    (mod(iyr,4).ne.0 .and. idy.eq.365)) then
        iyr = iyr + 1
        if (iyr.gt.99) iyr = 0
        addday = iyr*1000 + 1
      else
        addday = idate + 1
      endif
      end
c
      integer function subday(idate)
      implicit none
      integer idate,iyr,idy
      iyr = idate/1000
      idy = idate - iyr*1000
      if (idy.eq.1) then
        iyr = iyr - 1
        if (iyr.lt.0) iyr = 99
        if (mod(iyr,4).eq.0) then
          idy = 366
        else
          idy = 365
        endif
        subday = iyr*1000 + idy
      else
        subday = idate - 1
      endif
      end

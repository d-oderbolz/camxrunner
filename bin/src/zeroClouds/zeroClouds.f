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
      
      read(*,'(20x,a)') fname
      read(fname,*) nxc,nyc,nzc
      
      write(*,'(a,3i10)')'                 CAMx grid size:',nxc,nyc,nzc
      if (nxc.gt.mnxc .or. nyc.gt.mnyc .or. nzc.gt.mnzc) then
        write(*,*)'CAMx dimensions too large for arrays'
        write(*,*)'Increase array dimensions in param.inc and recompile'
        stop
      endif
      
      read(*,'(20x,a)') fname

      iunit = 15
      open(unit=iunit,file=fname,form='unformatted')
      write(*,*)'Opened CAMx cloud/rain file: ',fname
      write(iunit) cldhdr,nxc,nyc,nzc

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

        do j = 1,nyc
          do i = 1,nxc

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

          enddo
        enddo
c
c-----Write to CAMx files
c

c
c-----Time-varying met fields
c

c
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

      write(*,'(/,a,/)')' program run incomplete'

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

      subroutine metinit(igrd,ncol,nrow,nlay,endtim,enddate,orgx,orgy,
     &                   dxmod,dymod,height,press,depth,windu,windv,
     &                   tempk,tsurf,water,rkv,icdsno)
      use filunit
      use camxcom
c
c----CAMx v6.00 130506
c
c     METINIT reads the grid-dependent meteorological variables that
c     are to be time-interpolated, until the model start time/date has
c     been reached
c                          
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c          
c     Modifications:
c       11/06/01   CAMx now assumes that all input file dates are in
c                  Julian format (YYJJJ) if the simulation year is 2000
c                  or greater.  Added call to DATEERR for time mismatches.
c       10/12/04   Changed name from READZPWT to METINIT and added read of
c                  water vapor and vertical diffusivity
c       11/28/05   Added read of staggered wind flag on wind date/time record
c       01/04/11   Revised for new met input format
c       02/11/11   Moved snow field from AHO to 2D met file
c
c     Input arguments:
c        igrd                grid index
c        ncol                number of columns
c        nrow                number of rows
c        nlay                number of layers
c        endtim              simulation end time (HHMM)
c        enddate             simulation end date (YYJJJ)
c        orgx                grid x-origin (km or deg)
c        orgy                grid y-origin (km or deg)
c        dxmod               grid spacing in x-direction (km or deg)
c        dymod               grid spacing in y-direction (km or deg)
c
c     Output arguments:
c        height              layer interface height field (m)
c        press               layer midpoint pressure field (mb)
c        depth               layer depth (m)
c        windu               u-component wind field (m/s)
c        windv               u-component wind field (m/s)
c        tempk               temperature field (K)
c        tsurf               surface temperature field (K)
c        water               water vapor field (ppm)
c        rkv                 vertical diffusivity field (m2/s)
c        icdsno              snow cover field
c
c     Routines Called:
c        RDMETHDR
c        CVTWIND
c
c     Called by:
c        STARTUP
c
      implicit none
      include 'camx.prm'
      include 'flags.inc'
c
      integer istrln
c
      integer igrd,ncol,nrow,nlay
      integer enddate
      real endtim
      real orgx,orgy,dxmod,dymod
      integer icdsno(ncol,nrow)
      real height(ncol,nrow,nlay)
      real press(ncol,nrow,nlay)
      real depth(ncol,nrow,nlay)
      real windu(ncol,nrow,nlay)
      real windv(ncol,nrow,nlay)
      real tempk(ncol,nrow,nlay)
      real tsurf(ncol,nrow)
      real water(ncol,nrow,nlay)
      real rkv(ncol,nrow,nlay)
c
      character*200 fname
      character*60 string
      character*10 namevar
      character*4 namvar(10)
      integer iunit,idt,n,m,i,j,k,idum
      real hr
      real arr3d(MXCELLS,MXCELLS,MXLAYER)
      real arr2d(MXCELLS,MXCELLS)
c
c-----Entry point
c
c-----Read 3D met file
c
      iunit = i3dmet(igrd)
      if (iunit .GT. 0) then
c
        call rdmethdr(iunit,'3DMET     ',igrd,begtim,begdate,endtim,
     &                enddate,ncol,nrow,nlay,orgx,orgy,dxmod,dymod,
     &                iout,n3dmet(igrd))
c
 100    continue
        read(iunit,end=7000) idt,hr
        hr = 100*int(hr) + 60.*amod(hr,1.)
        if (hr.ge.2400.) then
          hr = hr - 2400.
          idt = idt + 1
          if( MOD(idt,1000) .GT. 365 ) then
             if( MOD(INT(idt/1000),4) .EQ. 0 ) then
                if( MOD(idt,1000) .EQ. 367 )
     &             idt = (INT(idt/1000)+1)*1000 + 1
             else
                idt = (INT(idt/1000)+1)*1000 + 1
             endif
          endif
        endif
        write(iout,'(a40,f7.0,i8.5,a,i3)')
     &        'Read 3D met file at ',hr,idt,' grid',igrd
c
        if (idt.lt.date .or. (idt.eq.date .and. hr.lt.time)) then
          do n = 1,n3dmet(igrd)
            do k = 1,nlay
              read(iunit)
            enddo
          enddo
          goto 100
        endif
        if (idt.gt.date .or. (idt.eq.date .and. hr.gt.time)) goto 7001
c
        do n = 1,n3dmet(igrd)
          do k = 1,nlay
            read(iunit,end=7000) idum,(namvar(m),m=1,10),
     &                            ((arr3d(i,j,k),i=1,ncol),j=1,nrow)
          enddo
          write(namevar,'(10a1)') (namvar(m),m=1,10)
c
          if (namevar.eq.'ZGRID_M') then
            do k = 1,nlay
              do j = 1,nrow
                do i = 1,ncol
                  height(i,j,k) = arr3d(i,j,k)
                enddo
              enddo
            enddo
          elseif (namevar.eq.'PRESS_MB') then
            do k = 1,nlay
              do j = 1,nrow
                do i = 1,ncol
                  press(i,j,k) = arr3d(i,j,k)
                enddo
              enddo
            enddo
          elseif (namevar.eq.'TEMP_K') then
            do k = 1,nlay
              do j = 1,nrow
                do i = 1,ncol
                  tempk(i,j,k) = arr3d(i,j,k)
                enddo
              enddo
            enddo
          elseif (namevar.eq.'HUMID_PPM') then
            do k = 1,nlay
              do j = 1,nrow
                do i = 1,ncol
                  water(i,j,k) = arr3d(i,j,k)
                enddo
              enddo
            enddo
          elseif (namevar.eq.'UWIND_MpS') then
            do k = 1,nlay
              do j = 1,nrow
                do i = 1,ncol
                  windu(i,j,k) = arr3d(i,j,k)
                enddo
              enddo
            enddo
          elseif (namevar.eq.'VWIND_MpS') then
            do k = 1,nlay
              do j = 1,nrow
                do i = 1,ncol
                  windv(i,j,k) = arr3d(i,j,k)
                enddo
              enddo
            enddo
          endif
        enddo
c
c-----Calculate layer depth
c
        do k = 1,nlay
          do j = 1,nrow
            do i = 1,ncol
              if (k.eq.1) then
                depth(i,j,k) = height(i,j,k) 
              else
                depth(i,j,k) = height(i,j,k) - height(i,j,k-1)
              endif
              if (depth(i,j,k).le.0.0) then
                write(iout,'(//,a)') 'ERROR in METINIT:'
                write(iout,'(a,3i3)')
     &                    'Negative depth in metinit, i,j,k = ',i,j,k
                call camxerr()
              endif
            enddo
          enddo
        enddo
c             
c-----Convert windu and windv if they are not staggered 
c             
        if (.not.lstagw) call cvtwind(ncol,nrow,nlay,windu,windv)
      endif
c
c-----Read 2D met file
c
      iunit = i2dmet(igrd)
      if (iunit.gt.0) then
c
        call rdmethdr(iunit,'2DMET     ',igrd,begtim,begdate,endtim,
     &                enddate,ncol,nrow,nlay,orgx,orgy,dxmod,dymod,
     &                iout,n2dmet(igrd))
c
 300    continue 
        read(iunit,end=7000) idt,hr
        hr = 100*int(hr) + 60.*amod(hr,1.)
        if (hr.ge.2400.) then
          hr = hr - 2400.
          idt = idt + 1
          if( MOD(idt,1000) .GT. 365 ) then
             if( MOD(INT(idt/1000),4) .EQ. 0 ) then
                if( MOD(idt,1000) .EQ. 367 )
     &             idt = (INT(idt/1000)+1)*1000 + 1
             else
                idt = (INT(idt/1000)+1)*1000 + 1
             endif
          endif
        endif
        write(iout,'(a40,f7.0,i8.5,a,i3)')
     &        'Read 2D met file at ',hr,idt,' grid',igrd
c
        if (idt.lt.date .or. (idt.eq.date .and. hr.lt.time)) then
          do n = 1,n2dmet(igrd)
            read(iunit)
          enddo
          goto 300
        endif
        if (idt.gt.date .or. (idt.eq.date .and. hr.gt.time)) goto 7001
c
        do n = 1,n2dmet(igrd)
          read(iunit,end=7000) idum,(namvar(m),m=1,10),
     &                          ((arr2d(i,j),i=1,ncol),j=1,nrow)
          write(namevar,'(10a1)') (namvar(m),m=1,10)
          if (namevar.eq.'TSURF_K') then
            do j = 1,nrow
              do i = 1,ncol
                tsurf(i,j) = arr2d(i,j)
              enddo
            enddo
          elseif (namevar.eq.'SNOWCOVER') then
            do j = 1,nrow
              do i = 1,ncol
                icdsno(i,j) = int(arr2d(i,j))
              enddo
            enddo
          endif
        enddo
      endif
c
c-----Read vertical diffusivity file
c
      iunit = ikv(igrd)
      if (iunit.gt.0) then
c
        call rdmethdr(iunit,'KVMET     ',igrd,begtim,begdate,endtim,
     &                enddate,ncol,nrow,nlay,orgx,orgy,dxmod,dymod,
     &                iout,nkvmet(igrd))
c
 500    continue
        read(iunit,end=7000) idt,hr
        hr = 100*int(hr) + 60.*amod(hr,1.)
        if (hr.ge.2400.) then
          hr = hr - 2400.
          idt = idt + 1
          if( MOD(idt,1000) .GT. 365 ) then
             if( MOD(INT(idt/1000),4) .EQ. 0 ) then
                if( MOD(idt,1000) .EQ. 367 )
     &             idt = (INT(idt/1000)+1)*1000 + 1
             else
                idt = (INT(idt/1000)+1)*1000 + 1
             endif
          endif
        endif
        write(iout,'(a40,f7.0,i8.5,a,i3)')
     &        'Read 3D VDiff file at ',hr,idt,' grid',igrd
c
        if (idt.lt.date .or. (idt.eq.date .and. hr.lt.time)) then
          do n = 1,nkvmet(igrd)
            do k=1,nlay
               read(iunit)
            enddo
          enddo
          goto 500
        endif
        if (idt.gt.date .or. (idt.eq.date .and. hr.gt.time)) goto 7001
c
        do n = 1,nkvmet(igrd)
          do k = 1,nlay
            read(iunit,end=7000) idum,(namvar(m),m=1,10),
     &                            ((arr3d(i,j,k),i=1,ncol),j=1,nrow)
          enddo
          write(namevar,'(10a1)') (namvar(m),m=1,10)
          if (namevar.eq.'KV_M2pS') then
            do k = 1,nlay
              do j = 1,nrow
                do i = 1,ncol
                  rkv(i,j,k) = arr3d(i,j,k)
                enddo
              enddo
            enddo
          endif
        enddo
      endif
c
c-----Read cloud/rain file header
c
      iunit = icld(igrd)
      if (iunit.gt.0) then
        call rdmethdr(iunit,'CLDMET    ',igrd,begtim,begdate,endtim,
     &                enddate,ncol,nrow,nlay,orgx,orgy,dxmod,dymod,
     &                iout,ncldmet(igrd))
      endif
c
      return
c
 7000 continue
      write(iout,'(//,a)')'ERROR in METINIT:'
      write(iout,*)'End of input file reached.  Make sure the file '
      write(iout,*)'is for the correct day and contains all hours.'
      inquire(unit=iunit,name=fname)
      write(iout,*) 'Filename = ',fname(:istrln(fname))
      write(iout,*)
      call camxerr()
c
 7001 continue
      write(iout,'(//,a)') 'ERROR in METINIT:'
      write(iout,'(a,f10.1,i10.5)') 'Past current time/date ',time,date
      inquire(unit=iunit,name=fname)
      write(iout,*) 'Filename = ',fname(:istrln(fname))
      write(iout,*)
      call camxerr()
c
      end

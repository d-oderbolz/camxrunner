      subroutine readinp(igrd,ngcol,ngrow,nglay,hght,phptim,hnext,
     &                   presure,ppptim,pnext,wndu,puptim,unext,wndv,
     &                   pvptim,vnext,tsrf,psptim,tsnext,temper,ptptim,
     &                   tnext,vapor,pwptim,wnext,rkvgrd,pkptim,knext,
     &                   cldwtr,ranwtr,snowtr,gplwtr,cldod,
     &                   cldph,icdsno)
      use filunit
      use grid
      use camxfld
      use camxcom
c 
c----CAMx v6.00 130506
c 
c     READINP cycles through and reads all time-variant meteorological 
c     input files.  The following files are read:
c     - 3D met fields        at next update date/time, nested grids optional
c     - 2D met fields        at next update date/time, nested grids optional
c     - 3D Kv fields         at next update date/time, nested grids optional
c     - 3D Cloud/rain fields     at current date/time, nested grids optional
c     The time-rate of change for all time-interpolated fields are also determined.
c 
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c           
c     Modifications: 
c        1/13/99   Added checks to convert input hours at 2400 to 0 on the
c                  following day
c       11/06/01   CAMx now assumes that all input file dates are in
c                  Julian format (YYJJJ) if the simulation year is 2000
c                  or greater.  Added call to DATEERR for time mismatches.
c        8/30/02   Cloud and rain files combined into one file (CAMx formatted
c                  file only), and now cloud/rain and water vapor files can be 
c                  read in for each nest
c        1/23/03   Cloud file contains new parameters:
c                  -cloud water content (cwc)
c                  -precip water content (pwc)
c                  -optical depth (cod)
c        4/2/03    Removed option for UAM-V type cloud file
c        3/3/04    Added checks for reading water files
c                  if either chemistry, dry dep or wet dep is on
c       10/12/04   Water vapor and vertical diffusivity fields are now
c                  time-interpolated
c        6/21/05   Cloud file contains new parameters:
c                  -cloud water content (cwc)
c                  -rain water content (pwr)
c                  -snow water content (pws)
c                  -graupel water content (pwg)
c                  -optical depth (cod)
c       10/24/05   Cloud water pH initialized at 5
c       12/12/05   Date/time handling improved to handle small clock drift
c        7/14/10   Added code for in-line TUV cloud adjustment
c       01/04/11   Revised for new met input format
c       02/11/11   Moved snow field from AHO to 2D met file
c       03/29/11   Support in-line TUV with aerosol optical depth
c       04/02/12   Removed RADM cloud adjustment option, cloud/aerosol 
c                  adjustments now always done with in-line TUV
c 
c     Input arguments: 
c        igrd                grid index
c        ngcol               number of columns
c        ngrow               number of rows
c        nglay               number of layers
c        hght                current layer interface field (m)
c        presure             current pressure field (mb)
c        wndu                current u-component wind field (m/s)
c        wndv                current v-component wind field (m/s)
c        tsrf                current surface temperature field (K)
c        temper              current temperature field (K)
c        vapor               current water vapor field (ppm)
c        rkvgrd              current vertical exchange coefficient (m2/s)
c 
c     Output arguments: 
c        phptim              time-rate change of layer interface height (m/s)
c        hnext                next layer interface height field (m)
c        ppptim              time-rate change of pressure (mb/s)
c        pnext                next pressure field (mb)
c        puptim              time-rate change of u-component wind (m/s2)
c        unext                next u-component wind field (m/s)
c        pvptim              time-rate change of v-component wind (m/s2)
c        vnext                next v-component wind field (m/s)
c        psptim              time-rate change of surface temperature (K/s)
c        tsnext               next surface temperature field (m/s)
c        ptptim              time-rate change of 3-D temperature (K/s)
c        tnext                next 3-D temperature field (m/s)
c        pwptim              time-rate change of 3-D water vapor (ppm/s)
c        wnext                next 3-D water vapor field (ppm)
c        pkptim              time-rate change of 3-D vertical diff (m2/s2)
c        knext                next 3-D vertical diffusivity field (m2/s)
c        cldwtr              cloud water content (g/m3)
c        ranwtr              rain water content (g/m3)
c        snowtr              snow water content (g/m3)
c        gplwtr              graupel water content (g/m3)
c        cldod               cloud optical depth 
c        cldph               cloud water pH
c        icdsno              snow cover field
c 
c     Routines Called: 
c        TIMRATES
c        CVTWIND
c        ZEROS
c        RASSGN3D
c        INTRP2D
c        INTRPV
c 
c     Called by: 
c        TSTEP_INIT
c 
      implicit none
      include 'camx.prm'
      include 'flags.inc'
c
      integer igrd,ngcol,ngrow,nglay
      integer icdsno(ngcol,ngrow)
      real hnext(ngcol,ngrow,nglay),pnext(ngcol,ngrow,nglay),
     &     unext(ngcol,ngrow,nglay),vnext(ngcol,ngrow,nglay),
     &     tnext(ngcol,ngrow,nglay),wnext(ngcol,ngrow,nglay),
     &     knext(ngcol,ngrow,nglay)
      real hght(ngcol,ngrow,nglay),phptim(ngcol,ngrow,nglay),
     &     presure(ngcol,ngrow,nglay),ppptim(ngcol,ngrow,nglay),
     &     wndu(ngcol,ngrow,nglay),puptim(ngcol,ngrow,nglay),
     &     wndv(ngcol,ngrow,nglay),pvptim(ngcol,ngrow,nglay),
     &     temper(ngcol,ngrow,nglay),ptptim(ngcol,ngrow,nglay),
     &     vapor(ngcol,ngrow,nglay),pwptim(ngcol,ngrow,nglay),
     &     rkvgrd(ngcol,ngrow,nglay),pkptim(ngcol,ngrow,nglay)
      real cldwtr(ngcol,ngrow,nglay),
     &     ranwtr(ngcol,ngrow,nglay),snowtr(ngcol,ngrow,nglay),
     &     gplwtr(ngcol,ngrow,nglay),cldod(ngcol,ngrow,nglay),
     &     cldph(ngcol,ngrow,nglay)
      real tsrf(ngcol,ngrow),psptim(ngcol,ngrow),tsnext(ngcol,ngrow)
c
      integer hdate,iunit,idt,i,j,k,m,n,idum
      real whr,wmn,atim,htim,hr
      real arr3d(MXCELLS,MXCELLS,MXLAYER)
      real arr2d(MXCELLS,MXCELLS)
      character*60 string
      character*10 namevar
      character*4 namvar(10)
c
c-----Entry point
c
c-----Find the date/time at the next update interval
c
      whr = aint(time/100.)
      wmn = anint(amod(time,100.))
      atim = 100.*whr + wmn
      htim = 100.*(whr + aint((wmn + dtinp)/60.)) +
     &             amod((wmn + dtinp),60.)
      hdate = date
      if (htim.ge.2400.) then
        htim = anint(htim - 2400.)
        hdate = hdate + 1
        if( MOD(hdate,1000) .GT. 365 ) then
           if( MOD(INT(hdate/1000),4) .EQ. 0 ) then
              if( MOD(hdate,1000) .EQ. 367 )
     &           hdate = (INT(hdate/1000)+1)*1000 + 1
           else
              hdate = (INT(hdate/1000)+1)*1000 + 1
           endif
        endif
      endif
c
c-----Read 3D met file
c
      iunit = i3dmet(igrd)
      if( iunit .GT. 0 ) then
         write(string,'(A,I5)') 'Reading 3D met file for grid:',igrd
 101     continue
         read(iunit,end=7000) idt,hr
         hr = 100*int(hr) + 60.*amod(hr,1.)
         if (hr.ge.2400.) then
           hr = hr - 2400.
           idt = idt + 1
           if( MOD(idt,1000) .GT. 365 ) then
              if( MOD(INT(idt/1000),4) .EQ. 0 ) then
                 if( MOD(idt,1000) .EQ. 367 )
     &              idt = (INT(idt/1000)+1)*1000 + 1
              else
                 idt = (INT(idt/1000)+1)*1000 + 1
              endif
           endif
         endif
         write(iout,'(a40,f7.0,i8.5,a,i3)')
     &        'Read 3D met file at ',hr,idt,' grid',igrd
         call flush(iout)
c
         if (idt.lt.hdate .or. (idt.eq.hdate .and. hr.lt.htim)) then
           do n = 1,n3dmet(igrd)
             do k = 1,nglay
               read(iunit)
             enddo
           enddo
           goto 101
         endif
         if (idt.gt.hdate .or. (idt.eq.hdate .and. hr.gt.htim))
     &     goto 7001
c
         do n = 1,n3dmet(igrd)
           do k = 1,nglay
            read(iunit,end=7000) idum,(namvar(m),m=1,10),
     &                           ((arr3d(i,j,k),i=1,ngcol),j=1,ngrow)
           enddo
           write(namevar,'(10a1)') (namvar(m),m=1,10)
           if (namevar.eq.'ZGRID_M') then
             do k = 1,nglay
               do j = 1,ngrow
                 do i = 1,ngcol
                   hnext(i,j,k) = arr3d(i,j,k)
                 enddo
               enddo
             enddo
           elseif (namevar.eq.'PRESS_MB') then
             do k = 1,nglay
               do j = 1,ngrow
                 do i = 1,ngcol
                   pnext(i,j,k) = arr3d(i,j,k)
                 enddo
               enddo
             enddo
           elseif (namevar.eq.'TEMP_K') then
             do k = 1,nglay
               do j = 1,ngrow
                 do i = 1,ngcol
                   tnext(i,j,k) = arr3d(i,j,k)
                 enddo
               enddo
             enddo
           elseif (namevar.eq.'HUMID_PPM') then
             do k = 1,nglay
               do j = 1,ngrow
                 do i = 1,ngcol
                   wnext(i,j,k) = arr3d(i,j,k)
                 enddo
               enddo
             enddo
           elseif (namevar.eq.'UWIND_MpS') then
             do k = 1,nglay
               do j = 1,ngrow
                 do i = 1,ngcol
                   unext(i,j,k) = arr3d(i,j,k)
                 enddo
               enddo
             enddo
           elseif (namevar.eq.'VWIND_MpS') then
             do k = 1,nglay
               do j = 1,ngrow
                 do i = 1,ngcol
                   vnext(i,j,k) = arr3d(i,j,k)
                 enddo
               enddo
             enddo
           endif
         enddo
c
c-----Convert wind vectors if they are not staggered
c
         if (.not.lstagw) call cvtwind(ngcol,ngrow,nglay,unext,vnext)
c
c-----Calculate time rates of change for 3D met fields
c
         call timrates(ngcol,ngrow,nglay,hght,hnext,phptim)
         call timrates(ngcol,ngrow,nglay,presure,pnext,ppptim)
         call timrates(ngcol,ngrow,nglay,temper,tnext,ptptim)
         call timrates(ngcol,ngrow,nglay,vapor,wnext,pwptim)
         call timrates(ngcol,ngrow,nglay,wndu,unext,puptim)
         call timrates(ngcol,ngrow,nglay,wndv,vnext,pvptim)
      endif
c
c-----Read 2D met file
c
      iunit = i2dmet(igrd)
      if( iunit .GT. 0 ) then
         write(string,'(A,I5)') 'Reading 2D met file for grid:',igrd
 102     continue
         read(iunit,end=7000) idt,hr
         hr = 100*int(hr) + 60.*amod(hr,1.)
         if (hr.ge.2400.) then
           hr = hr - 2400.
           idt = idt + 1
           if( MOD(idt,1000) .GT. 365 ) then
              if( MOD(INT(idt/1000),4) .EQ. 0 ) then
                 if( MOD(idt,1000) .EQ. 367 )
     &              idt = (INT(idt/1000)+1)*1000 + 1
              else
                 idt = (INT(idt/1000)+1)*1000 + 1
              endif
           endif
         endif
         write(iout,'(a40,f7.0,i8.5,a,i3)')
     &        'Read 2D met file at ',hr,idt,' grid',igrd
         call flush(iout)
c
         if (idt.lt.hdate .or. (idt.eq.hdate .and. hr.lt.htim)) then
           do n = 1,n2dmet(igrd)
             read(iunit)
           enddo
           goto 102
         endif
         if (idt.gt.hdate .or. (idt.eq.hdate .and. hr.gt.htim))
     &     goto 7001
c
         do n = 1,n2dmet(igrd)
           read(iunit,end=7000) idum,(namvar(m),m=1,10),
     &                          ((arr2d(i,j),i=1,ngcol),j=1,ngrow)
           write(namevar,'(10a1)') (namvar(m),m=1,10)
           if (namevar.eq.'TSURF_K') then
             do j = 1,ngrow
               do i = 1,ngcol
                 tsnext(i,j) = arr2d(i,j)
               enddo
             enddo
           elseif (namevar.eq.'SNOWCOVER') then
             do j = 1,ngrow
               do i = 1,ngcol
                 icdsno(i,j) = int(arr2d(i,j))
               enddo
             enddo
           endif
         enddo
c
c-----Calculate time rates of change for 2D met fields
c
         call timrates(ngcol,ngrow,1,tsrf,tsnext,psptim)
      endif
c
c-----Read 3D VDiff file
c
      iunit = ikv(igrd)
      if( iunit .GT. 0 ) then
         write(string,'(A,I5)') 'Reading 3D VDiff file for grid:',igrd
 103     continue
         read(iunit,end=7000) idt,hr
         hr = 100*int(hr) + 60.*amod(hr,1.)
         if (hr.ge.2400.) then
           hr = hr - 2400.
           idt = idt + 1
           if( MOD(idt,1000) .GT. 365 ) then
              if( MOD(INT(idt/1000),4) .EQ. 0 ) then
                 if( MOD(idt,1000) .EQ. 367 )
     &              idt = (INT(idt/1000)+1)*1000 + 1
              else
                 idt = (INT(idt/1000)+1)*1000 + 1
              endif
           endif
         endif
         write(iout,'(a40,f7.0,i8.5,a,i3)')
     &        'Read 3D VDiff file at ',hr,idt,' grid',igrd
         call flush(iout)
c
         if (idt.lt.hdate .or. (idt.eq.hdate .and. hr.lt.htim)) then
           do n = 1,nkvmet(igrd)
             do k = 1,nglay
               read(iunit)
             enddo
           enddo
           goto 103
         endif
         if (idt.gt.hdate .or. (idt.eq.hdate .and. hr.gt.htim))
     &     goto 7001
c
         do n = 1,nkvmet(igrd)
           do k = 1,nglay
            read(iunit,end=7000) idum,(namvar(m),m=1,10),
     &                           ((arr3d(i,j,k),i=1,ngcol),j=1,ngrow)
           enddo
           write(namevar,'(10a1)') (namvar(m),m=1,10)
           if (namevar.eq.'KV_M2pS') then
             do k = 1,nglay
               do j = 1,ngrow
                 do i = 1,ngcol
                   knext(i,j,k) = arr3d(i,j,k)
                 enddo
               enddo
             enddo
           endif
         enddo
c
c-----Calculate time rates of change for 3D vertical diffusivity
c
         call timrates(ngcol,ngrow,nglay,rkvgrd,knext,pkptim)
      endif
c
c-----Read 3D cloud file
c
      iunit = icld(igrd)
      if (igrd.eq.1 .and. iunit.eq.0) then
         call zeros(cldwtr,ngcol*ngrow*nglay)
         call zeros(ranwtr,ngcol*ngrow*nglay)
         call zeros(snowtr,ngcol*ngrow*nglay)
         call zeros(gplwtr,ngcol*ngrow*nglay)
         call zeros(cldod, ngcol*ngrow*nglay)
      elseif (iunit.gt.0) then
         write(string,'(A,I5)') 'Reading 3D Cloud file for grid:',igrd
 104     continue
         read(iunit,end=7000) idt,hr
         hr = 100*int(hr) + 60.*amod(hr,1.)
         if (hr.ge.2400.) then
           hr = hr - 2400.
           idt = idt + 1
           if( MOD(idt,1000) .GT. 365 ) then
              if( MOD(INT(idt/1000),4) .EQ. 0 ) then
                 if( MOD(idt,1000) .EQ. 367 )
     &              idt = (INT(idt/1000)+1)*1000 + 1
              else
                 idt = (INT(idt/1000)+1)*1000 + 1
              endif
           endif
         endif
         write(iout,'(a40,f7.0,i8.5,a,i3)')
     &        'Read 3D Cloud file at ',hr,idt,' grid',igrd
         call flush(iout)
c
         if (idt.lt.date .or. (idt.eq.date .and. hr.lt.atim)) then
           do n = 1,ncldmet(igrd)
             do k = 1,nglay
               read(iunit)
             enddo
           enddo
           goto 104
         endif
         if (idt.gt.date .or. (idt.eq.date .and. hr.gt.atim))
     &     goto 7002
c
         do n = 1,ncldmet(igrd)
           do k = 1,nglay
            read(iunit,end=7000) idum,(namvar(m),m=1,10),
     &                           ((arr3d(i,j,k),i=1,ngcol),j=1,ngrow)
           enddo
           write(namevar,'(10a1)') (namvar(m),m=1,10)
           if (namevar.eq.'CLODW_GpM3') then
             do k = 1,nglay
               do j = 1,ngrow
                 do i = 1,ngcol
                   cldwtr(i,j,k) = arr3d(i,j,k)
                 enddo
               enddo
             enddo
           endif
           if (namevar.eq.'RAINW_GpM3') then
             do k = 1,nglay
               do j = 1,ngrow
                 do i = 1,ngcol
                   ranwtr(i,j,k) = arr3d(i,j,k)
                 enddo
               enddo
             enddo
           endif
           if (namevar.eq.'SNOWW_GpM3') then
             do k = 1,nglay
               do j = 1,ngrow
                 do i = 1,ngcol
                   snowtr(i,j,k) = arr3d(i,j,k)
                 enddo
               enddo
             enddo
           endif
           if (namevar.eq.'GRPLW_GpM3') then
             do k = 1,nglay
               do j = 1,ngrow
                 do i = 1,ngcol
                   gplwtr(i,j,k) = arr3d(i,j,k)
                 enddo
               enddo
             enddo
           endif
           if (namevar.eq.'CLOUDOD') then
             do k = 1,nglay
               do j = 1,ngrow
                 do i = 1,ngcol
                   cldod(i,j,k) = arr3d(i,j,k)
                 enddo
               enddo
             enddo
           endif
         enddo
c
c-----Initialize cloud pH
c
         do k = 1,nglay
           do j = 1,ngrow
             do i = 1,ngcol
               cldph(i,j,k) = 5.
             enddo
           enddo
         enddo
c
      endif
c
      return
c
 7000 continue
      write(iout,'(//,a)')'ERROR in READINP:'
      write(iout,'(a)') string
      write(iout,*)'End of input file reached.  Make sure the file '
      write(iout,*)'is for the correct day and contains all hours.'
      write(iout,*)
      call camxerr()
c
 7001 continue
      write(iout,'(//,a)') 'ERROR in READINP:'
      write(iout,'(a)') string
      write(iout,'(a,f10.1,i10.5)') 
     &      'Past expected time/date ',htim,hdate
      write(iout,*)
      call camxerr()
c
 7002 continue
      write(iout,'(//,a)') 'ERROR in READINP:'
      write(iout,'(a)') string
      write(iout,'(a,f10.1,i10.5)') 
     &      'Past expected time/date ',atim,date
      write(iout,*)
      call camxerr()
c
      end

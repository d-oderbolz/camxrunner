      subroutine metinit(igrd,ihtp,iwnd,itmp,ivap,ikv,iout,ncol,nrow,
     &                   nlay,height,press,depth,windu,windv,tempk,
     &                   tsurf,water,rkv)
c
c----CAMx v4.51 080522
c
c     METINIT reads the grid-dependent meteorological variables that
c     are to be time-interpolated, until the model start time/date has
c     been reached
c                          
c     Copyright 1996-2008
c     ENVIRON International Corporation
c          
c     Modifications:
c       11/06/01   CAMx now assumes that all input file dates are in
c                  Julian format (YYJJJ) if the simulation year is 2000
c                  or greater.  Added call to DATEERR for time mismatches.
c       10/12/04   Changed name from READZPWT to METINIT and added read of
c                  water vapor and vertical diffusivity
c       11/28/05   Added read of staggered wind flag on wind date/time record
c
c     Input arguments:
c        igrd                grid index
c        ihtp                height file unit number
c        iwnd                wind file unit number
c        itmp                temperature file unit number
c        ivap                water vapor file unit number
c        ikv                 vertical diffusivity unit number
c        iout                simulation output file number
c        ncol                number of columns
c        nrow                number of rows
c        nlay                number of layers
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
c
c     Routines Called:
c        JULDATE
c        DATEERR
c        CVTWIND
c
c     Called by:
c        STARTUP
c
      include 'camx.prm'
      include 'camx.com'
      include 'flags.com'
c
      dimension height(ncol,nrow,nlay),press(ncol,nrow,nlay),
     &          depth(ncol,nrow,nlay),windu(ncol,nrow,nlay),
     &          windv(ncol,nrow,nlay),tempk(ncol,nrow,nlay),
     &          tsurf(ncol,nrow),water(ncol,nrow,nlay),
     &          rkv(ncol,nrow,nlay)
      character*60 string
      logical lstagin
c
c-----Entry point
c
c-----Read height/pressure file
c
      if( ihtp .GT. 0 ) then
 100     continue
         do k = 1,nlay
           read(ihtp,end=7000) hr,idt,
     &                ((height(i,j,k),i=1,ncol),j=1,nrow)
           read(ihtp,end=7000) hr,idt,
     &                ((press(i,j,k),i=1,ncol),j=1,nrow)
         enddo
c
         if (.not.ly2k .and. idt.gt.100000) call juldate(idt)
         if (hr.ge.2400.) then
           hr = hr - 2400.
           idt = idt + 1
         endif
         write(iout,'(a40,f7.0,i8.5,a,i3)')
     &        'Read height/pressure file at ',hr,idt,' grid',igrd
c
         if (idt.lt.date .or. (idt.eq.date .and. hr.lt.time)) goto 100
         if (idt.gt.date .or. (idt.eq.date .and. hr.gt.time)) then
           write(iout,'(//,a)') 'ERROR in METINIT:'
           write(iout,'(a,f10.1,i10.5)')
     &           'Past current time/date ',time,date
           write(string,'(A,I5)')
     &                   'Reading height/pressure file for grid:',igrd
           call dateerr(ly2k,string)
         endif
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
     &                     'Negative depth in readzp, i,j,k = ',i,j,k
                 call camxerr()
               endif
             enddo
           enddo
         enddo
      endif
c
c-----Read wind file
c
      if (iwnd.gt.0) then
        read(iwnd,err=201,end=201) hr,idt,lstagin
        rewind(iwnd)
        write(iout,'(a40,l15,a,i3)')
     &        'Read staggered wind flag: ',lstagin,' grid',igrd
        lstagw = lstagin
        goto 200
 201    rewind(iwnd)
        write(iout,'(a40,l15,a,i3)')
     &  'Using staggered wind flag from CAMx.in: ',lstagw,' grid',igrd

 200    read(iwnd,end=7000) hr,idt
        do k = 1,nlay
          read(iwnd,end=7000) ((windu(i,j,k),i=1,ncol),j=1,nrow)
          read(iwnd,end=7000) ((windv(i,j,k),i=1,ncol),j=1,nrow)
        enddo
        read(iwnd,end=7000)

        if (.not.ly2k .and. idt.gt.100000) call juldate(idt)
        if (hr.ge.2400.) then
          hr = hr - 2400.
          idt = idt + 1
        endif
        write(iout,'(a40,f7.0,i8.5,a,i3)')
     &        'Read wind file at ',hr,idt,' grid',igrd
c
        if (idt.lt.date .or. (idt.eq.date .and. hr.lt.time)) goto 200
        if (idt.gt.date .or. (idt.eq.date .and. hr.gt.time)) then
          write(iout,'(//,a)') 'ERROR in METINIT:'
          write(iout,'(a,f10.1,i10.5)')
     &          'Past current time/date ',time,date
          write(string,'(A,I5)')'Reading wind file for grid:',igrd
          call dateerr(ly2k,string)
        endif
c             
c-----Convert windu and windv if they are not staggered 
c             
        if (.not.lstagw) call cvtwind(ncol,nrow,nlay,windu,windv)
      endif
c
c-----Read temperature file
c
      if (itmp.gt.0) then
 300    read(itmp,end=7000) hr,idt,
     &                ((tsurf(i,j),i=1,ncol),j=1,nrow) 
        do k = 1,nlay 
          read(itmp,end=7000) hr,idt,
     &                ((tempk(i,j,k),i=1,ncol),j=1,nrow) 
        enddo

        if (.not.ly2k .and. idt.gt.100000) call juldate(idt)
        if (hr.ge.2400.) then
          hr = hr - 2400.
          idt = idt + 1
        endif
        write(iout,'(a40,f7.0,i8.5,a,i3)')
     &        'Read temperature file at ',hr,idt,' grid',igrd
c
        if (idt.lt.date .or. (idt.eq.date .and. hr.lt.time)) goto 300
        if (idt.gt.date .or. (idt.eq.date .and. hr.gt.time)) then
          write(iout,'(//,a)') 'ERROR in METINIT:'
          write(iout,'(a,f10.1,i10.5)')
     &          'Past current time/date ',time,date
          write(string,'(A,I5)')
     &                   'Reading temperature file for grid:',igrd
          call dateerr(ly2k,string)
        endif
      endif
c
c-----Read water vapor file
c
      if (ivap.gt.0) then
 400    do k = 1,nlay
          read(ivap,end=7000) hr,idt,
     &                ((water(i,j,k),i=1,ncol),j=1,nrow)
        enddo

        if (.not.ly2k .and. idt.gt.100000) call juldate(idt)
        if (hr.ge.2400.) then
          hr = hr - 2400.
          idt = idt + 1
        endif
        write(iout,'(a40,f7.0,i8.5,a,i3)')
     &        'Read water vapor file at ',hr,idt,' grid',igrd
c
        if (idt.lt.date .or. (idt.eq.date .and. hr.lt.time)) goto 400
        if (idt.gt.date .or. (idt.eq.date .and. hr.gt.time)) then
          write(iout,'(//,a)') 'ERROR in METINIT:'
          write(iout,'(a,f10.1,i10.5)')
     &          'Past current time/date ',time,date
          write(string,'(A,I5)')
     &                   'Reading water vapor file for grid:',igrd
          call dateerr(ly2k,string)
        endif
      endif
c
c-----Read vertical diffusivity file
c
      if (ikv.gt.0) then
 500    do k = 1,nlay
          read(ikv,end=7000) hr,idt,
     &                ((rkv(i,j,k),i=1,ncol),j=1,nrow)
        enddo

        if (.not.ly2k .and. idt.gt.100000) call juldate(idt)
        if (hr.ge.2400.) then
          hr = hr - 2400.
          idt = idt + 1
        endif
        write(iout,'(a40,f7.0,i8.5,a,i3)')
     &        'Read diffusivity file at ',hr,idt,' grid',igrd
c
        if (idt.lt.date .or. (idt.eq.date .and. hr.lt.time)) goto 500
        if (idt.gt.date .or. (idt.eq.date .and. hr.gt.time)) then
          write(iout,'(//,a)') 'ERROR in METINIT:'
          write(iout,'(a,f10.1,i10.5)')
     &          'Past current time/date ',time,date
          write(string,'(A,I5)')
     &                   'Reading diffusivity file for grid:',igrd
          call dateerr(ly2k,string)
        endif
      endif
      goto 9999
c
 7000 continue
      write(iout,'(//,a)')'ERROR in METINIT:'
      write(iout,*)'End of input file reached.  Make sure the file '
      write(iout,*)'is for the correct day and contains all hours.'
      write(iout,*)
      if (ly2k) then
        write(iout,*)'You are modeling year 2000 or later --' 
        write(iout,*)'CAMx assumes that all input file dates',
     &               ' are in Julian format (YYJJJ)'
      endif
      call camxerr()
c
 9999 continue
      return
      end

      subroutine readaho(ncol,nrow,time,date,ly2k,name,ahotim,ahodate,
     &                   indx)
c
c----CAMx v4.51 080522
c
c     READAHO reads the time varying records for 1 parameter from 
c     the albedo/haze/ozone file and and sets the next read time
c
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c     Modifications:
c        11/06/01  CAMx now assumes that all input file dates are in
c                  Julian format (YYJJJ) if the simulation year is 2000
c                  or greater.
c        04/26/03  Add capability for snow cover and make more robust
c
c     Input arguments:
c        ncol                number of columns
c        nrow                number of rows
c        time                simulation time (HHMM)
c        date                simulation date (YYJJJ)
c        ly2k                Year 2000 flag (T is >=2000)
c        name                name of variable(HAZE, OZONE COL, SNOW)
c
c     Output arguments:
c        ahotim              next update time for AHO map (HHMM)
c        ahodate             next update date for AHO map (YYJJJ)
c        indx                coarse grid codes for variable
c
c     Routines called:
c        JULDATE
c
c     Called by:
c        CAMx
c
      include 'camx.prm'
      include 'filunit.com'
      include 'ahomap.com'
c
      integer ahodate, date
      integer indx(ncol,nrow)
      character*10 title, namoz, namhz, namsno, name
      logical ly2k, lrewind
c
      data namoz /'OZONE COL '/
      data namhz /'HAZE      '/
      data namsno/'SNOW      '/
c
c-----Entry point
c
      lrewind = .FALSE.
c
c-----Read coarse grid ozone column and haze indices
c
 100  read(iaho,'(a10,i10,f10.0,i10,f10.0)',end=800) title,id1,t1,id2,t2
c     write(iout,'(a10,i10,f10.0,i10,f10.0)') title,id1,t1,id2,t2 
      if (title.ne.namoz .and. title.ne.namhz .and. 
     &                                        title.ne.namsno ) then 
        write(iout,'(//,a)') 'ERROR in READAHO:'
        write(iout,'(3a,i10,f10.3)')
     &                'Looking for: ',name,' at: ',date,time
        write(iout,*) 'Expecting keyword HAZE, OZONE COL or SNOW' 
        write(iout,*) 'Found: ',title
        call camxerr()
      endif 
      if (.not.ly2k .and. id1.gt.100000) call juldate(id1)
      if (.not.ly2k .and. id2.gt.100000) call juldate(id2)
c
c-----Check title against requested name
c
      if (title.eq.name) then
        do j = nrow,1,-1
          read(iaho,'(9999i1)',end=900) (indx(i,j),i=1,ncol)
        enddo
      else
        do j = nrow,1,-1
          read(iaho,*,end=900) 
        enddo
        write(iout,'(a40,2(f7.0,i8.5),1x,A9)')
     &    'Skipped albedo/haze/ozone file at ',t1,id1,t2,id2,title
        goto 100
      endif
c
c-----Check for correct time/date and set next update time/date
c
      if ((id1.lt.date .or. (id1.eq.date .and. t1.le.time)) .and.
     &    (id2.gt.date .or. (id2.eq.date .and. t2.gt.time))) then
        ahodate = id2
        ahotim = t2
        if (ahotim.ge.2400.) then
          ahotim = ahotim - 2400.
          ahodate = ahodate + 1
          if( MOD(ahodate,1000) .GT. 365 ) then
            if( MOD(INT(ahodate/1000),4) .EQ. 0 ) then
               if( MOD(ahodate,1000) .EQ. 367 )
     &                     ahodate = (INT(ahodate/1000)+1)*1000 + 1
            else
               ahodate = (INT(ahodate/1000)+1)*1000 + 1
            endif
         endif
        endif
        write(iout,'(a40,2(f7.0,i8.5),1x,A9)')
     &       'Read albedo/haze/ozone file at ',t1,id1,t2,id2,title
        goto 999
      else
        write(iout,'(a40,2(f7.0,i8.5),1x,A9)')
     &    'Skipped albedo/haze/ozone file at ',t1,id1,t2,id2,title
        goto 100
      endif
c
c-----Expected end of AHO file; rewind and skip header/constant lines
c     using the number of header lines remembered from AHOPREP
c
 800  continue
      if (lrewind) then
        write(iout,'(//,a)') 'ERROR in READAHO:'
        write(iout,'(//,a)') 'Searched the entire AHO file'
        write(iout,'(3a,i10,f10.3)')
     &     'Unable to find valid data for: ',name,' at: ',date,time
        call camxerr()
      endif
      write(iout,'(2a)') 'In READAHO: rewinding the input file ',
     &                   'because data not in most efficient order'
      lrewind = .TRUE.
      rewind(iaho)
      do n = 1,nhdraho
        read(iaho,*,end=900)
      enddo
      goto 100
c
c-----Unexpected end of AHO file
c
 900  continue
      write(iout,'(//,a)') 'ERROR in READAHO:'
      write(iout,'(//,a)') 'Unexpected end of AHO file'
      write(iout,'(3a,i10,f10.3)')
     &                'Looking for: ',name,' at: ',date,time
      write(iout,*)' Make sure the number of columns and rows ',
     &                     'matches the coarse grid dimensions.'
      write(iout,'(//,a)') 
     &                'Look at the READAHO messages in the out file'
      call camxerr()
c
c-----Normal return
c
 999  return
      end


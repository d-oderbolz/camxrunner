      subroutine reado3col(ncol,nrow,time,date,name,o3coltim,o3coldat,
     &                     indx)
      use filunit
      implicit none
c
c----CAMx v6.00 130506
c
c     READO3COL reads the time varying records for 1 parameter from 
c     the ozone column file and and sets the next read time
c
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Modifications:
c        11/06/01  CAMx now assumes that all input file dates are in
c                  Julian format (YYJJJ) if the simulation year is 2000
c                  or greater.
c        04/26/03  Add capability for snow cover and make more robust
c        02/22/11  Moved snow panel to 2D surface met file
c        03/31/12  Reads only time-varying ozone column
c
c     Input arguments:
c        ncol                number of columns
c        nrow                number of rows
c        time                simulation time (HHMM)
c        date                simulation date (YYJJJ)
c        name                name of variable(OZONE COL)
c
c     Output arguments:
c        o3coltim            next update time for O3 column map (HHMM)
c        o3coldat            next update date for O3 column map (YYJJJ)
c        indx                coarse grid codes for variable
c
c     Routines called:
c        NONE
c
c     Called by:
c        O3COL_UPD
c
      integer o3coldat, date
      integer id1,id2,i,j,ncol,nrow
      integer indx(ncol,nrow)
      real o3coltim,time
      real t1,t2
      character*10 title, namoz, name
c
      data namoz /'OZONE COL '/
c
c-----Entry point
c
c-----Read coarse grid ozone column
c
 100  read(io3col,'(a10,i10,f10.0,i10,f10.0)',end=800) title,id1,t1,id2,t2
      if (title.ne.namoz) then 
        write(iout,'(//,a)') 'ERROR in READO3COL:'
        write(iout,'(3a,i10,f10.3)')
     &                'Looking for: ',name,' at: ',date,time
        write(iout,*) 'Expecting keyword OZONE COL' 
        write(iout,*) 'Found: ',title
        call camxerr()
      endif 
c
      do j = nrow,1,-1
        read(io3col,'(9999i1)',end=900) (indx(i,j),i=1,ncol)
      enddo
c
c-----Check for correct time/date and set next update time/date
c
      if ((id1.lt.date .or. (id1.eq.date .and. t1.le.time)) .and.
     &    (id2.gt.date .or. (id2.eq.date .and. t2.gt.time))) then
        o3coldat = id2
        o3coltim = t2
        if (o3coltim.ge.2400.) then
          o3coltim = o3coltim - 2400.
          o3coldat = o3coldat + 1
          if( MOD(o3coldat,1000) .GT. 365 ) then
            if( MOD(INT(o3coldat/1000),4) .EQ. 0 ) then
               if( MOD(o3coldat,1000) .EQ. 367 )
     &                     o3coldat = (INT(o3coldat/1000)+1)*1000 + 1
            else
               o3coldat = (INT(o3coldat/1000)+1)*1000 + 1
            endif
         endif
        endif
        write(iout,'(a40,2(f7.0,i8.5))')
     &    'Read ozone column file at ',t1,id1,t2,id2
        goto 999
      else
        write(iout,'(a40,2(f7.0,i8.5))')
     &    'Skipped ozone column file at ',t1,id1,t2,id2
        goto 100
      endif
c
c-----Unexpected end of O3 column file
c
 800  continue
      write(iout,'(//,a)') 'ERROR in READO3COL:'
      write(iout,'(a,i10,f10.3)')
     &     'Unable to find O3 column data at: ',date,time
      call camxerr()
c
c-----Unexpected end of O3 column file
c
 900  continue
      write(iout,'(//,a)') 'ERROR in READO3COL:'
      write(iout,'(//,a)') 'Unexpected end O3 column file'
      write(iout,'(3a,i10,f10.3)')
     &                'Looking for: ',name,' at: ',date,time
      write(iout,*)' Make sure the number of columns and rows ',
     &                     'matches the coarse grid dimensions.'
      call camxerr()
c
c-----Normal return
c
 999  return
      end

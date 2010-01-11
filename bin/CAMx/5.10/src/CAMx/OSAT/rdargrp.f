C***** RDARGRP
c
      subroutine rdargrp(igrid,ndate,ttime,nox,noy,numcls,igroup,emscls)
      use filunit
      use grid
      use bndary
      use camxcom
      use tracer
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c      10/24/01  Removed BSWAP and converted integer strings to character*4
c      05/01/03  Time span of emissions must now match emiss update interval
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.com'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer igrid
      integer ndate
      real    ttime
      integer nox
      integer noy
      integer numcls
      integer igroup
      real    emscls(MXTRCLS,MXCELLS,MXCELLS)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*200 fname
      character*4   iname(10)
      integer       ispc
      integer       ibgdat, iendat, iounit, idx, iseg, i, j
      real          bgtim, edtim
      logical       lfound
c
      real emsgrd(MXCELLS,MXCELLS)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- initalize the array ---
c
      do icls=1,ntrcls
        do i=1,nox
          do j=1,noy
              emscls(icls,i,j) = 0.
          enddo
        enddo
      enddo 
c
c   --- skip if filename not supplied ---
c
      if( .NOT. ltemfl(igrid,igroup) .OR. .NOT. larsrc ) goto 9999
c
c   --- set the unit number for surface emissions file ---
c
      if( igroup .EQ. 0 ) then
          iounit = iarem(igrid)
          write(fname,'(A,I3)') 'EMISSIONS -- UNIT ',iarem(igrid)
c
c    --- if emissions is regular model emissions file, backup one hour ---
c
          do i=1,nspcem(igrid,igroup)
              backspace(iounit)
          enddo
          backspace(iounit)
      else
          iounit = iortem(igrid,igroup)
          fname = temfil(igrid,igroup)
      endif
c
c   --- read the date and time, again ---
c
      lfound = .FALSE.
  111 continue
      read(iounit,END=222) ibgdat, bgtim, iendat, edtim
c
      ichktm1 = NINT( 1000*(bgtim) )
      if( le1day ) then
         ichktm2 = NINT( 1000*(edtim) )
       else
         ichktm2 = NINT( 1000*(edtim)+24000*(iendat-ibgdat) )
      endif
      if( NINT(edtim) .EQ. 0 ) ichktm2 = 24000
      ichkems = NINT( 1000*(dtems/60.) )
      if( (ichktm2 - ichktm1) .NE. ichkems ) then
         write(iout,'(//,a)')'ERROR in RDARGRP:'
         write(iout,*) 'Time interval in surface emissions file does'
         write(iout,*)  ' not match emissions update time interval.'
         write(iout,*) '   Beginning Date/Time (HHMM): ',ibgdat,bgtim
         write(iout,*) '   Ending Date/Time    (HHMM): ',iendat,edtim
         write(iout,*) '   Emiss Input interval (min): ',dtems
         call camxerr()
      endif
      if(NINT(edtim) .EQ. 0) then
         edtim = 24.
         iendat = iendat - 1
      endif
c
c   --- read the emissions for this hour ---
c
      do 10 ispc=1,nspcem(igrid,igroup)
          read(iounit,ERR=7000) iseg, (iname(i),i=1,10), 
     &                         ((emsgrd(i,j),i=1,nox),j=1,noy)
c
c   --- if date and time does not match this hour, skip this record ---
c
          if( le1day ) then
              if( bgtim .NE. ttime ) goto 10
          else
              if( ndate .NE. ibgdat .OR. bgtim .NE. ttime ) goto 10
          endif
          lfound  = .TRUE.
c
c   --- if the species is a not modelled or not a VOC species skip it ---
c
          idx = idxems(igrid,igroup,ispc)
          if( idx .LE. 0 ) goto 10
          if( .NOT. lusespc(idx) ) goto 10
c
c   --- convert to PPM (PPMC) ----
c
          do j=2,noy-1
            do i=2,nox-1
              emsgrd(i,j) = emsgrd(i,j)/(60.*dtems)
            enddo
          enddo
c
c   --- load the species into the cells in tracer array ----
c
          do 20 j=2,noy-1
             do i=2,nox-1
                do icls=1,ntrcls
                   emscls(icls,i,j) = emscls(icls,i,j) + 
     &                           emsgrd(i,j) * trspmap(idx,icls)
                enddo
             enddo
   20     continue
c
c   --- next species ---
c
  10  continue
c
c   --- if the correct hour has not been found, 
c       go back and read some more else read next file ---
c
      if( .NOT. lfound ) then
         goto 111
      else
         goto 9999
      endif
c
c   --- if using 1 day emissions, we need to rewind the file to
c       get the current hour ---
c
  222 continue
      if( le1day ) then
          rewind(iounit)
          read(iounit)
          read(iounit)
          read(iounit)
          read(iounit)
          goto 111
      else
         goto 7001
      endif
c
      goto 9999
c
c----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in RDARGRP:'
      write(iout,'(/,1X,2A,I8.5,F8.1,2A)') 'Reading emissions ',
     &   'after hour ',ibgdat, bgtim,' in file: ',fname(:istrln(fname))
      call camxerr()
c
 7001 continue
      write(iout,'(//,a)') 'ERROR in RDARGRP:'
      write(iout,'(/,1X,2A,I8.5,F4.1,2A)') 'Premature end-of-file',
     &              ' in emissions file after hour ',ibgdat, bgtim,
     &              ' in file: ',fname(:istrln(fname))
      call camxerr()
c
 9999 continue
c
      return
      end

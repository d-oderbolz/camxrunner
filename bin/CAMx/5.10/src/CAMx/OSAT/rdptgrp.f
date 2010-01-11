C***** RDPTGRP
c
      subroutine rdptgrp(ndate,ttime,igroup,numcls,emscls,izcel)
      use filunit
      use grid
      use camxcom
      use ptemiss
      use tracer
c
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c
c----CAMx v5.10 090918
c
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     08/18/99  --gwilson--   Added code to implement the override
c                             flag for the source area of point sources
c     10/24/01  Removed BSWAP and converted integer strings to character*4
c     07/05/02  Changed to account for new type of the PiG flag
c     05/01/03  Time span of emissions must now match emiss update interval
c     09/25/03  Significant changes to handle PSAT
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
      integer ndate
      real    ttime
      integer igroup
      integer numcls
      real    emscls(MXTRCLS,*)
      integer izcel(*)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*200 fname
      character*4   iname(10)
      integer       ibgdat, iendat, iounit, idx, iseg
      integer       npoint, ispc, i, idum
      real          bgtim, edtim, rdum
      logical       lfound
c
      real emspts(MXPTSRC)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- initialize the array to zero ----
c
      do icls=1,numcls
        do j=1,nptsrc
           emscls(icls,j) = 0.
        enddo
      enddo
c
c   --- skip if filename not supplied ---
c
      if( .NOT. ltptfl(igroup) ) goto 9999
c
c   --- set the unit number for file ---
c
      if( igroup .EQ. 0 ) then
          iounit = iptem
          write(fname,'(A,I3)') 'PTSOURCE -- UNIT ',iptem
          do i=1,nspcpt(igroup)
              backspace(iounit)
          enddo
          backspace(iounit)
          backspace(iounit)
          backspace(iounit)
      else
          iounit = iortpt(igroup)
          fname = tptfil(igroup)
      endif
c
c   --- read the date and time, again ---
c
      lfound = .FALSE.
  111 continue
      read(iounit,END=222) ibgdat, bgtim, iendat, edtim
      ichktm1 = NINT( 1000*(bgtim) )
      if( le1day ) then
         ichktm2 = NINT( 1000*(edtim) )
      else
         ichktm2 = NINT( 1000*(edtim)+24000*(iendat-ibgdat) )
      endif
      if( NINT(edtim) .EQ. 0 ) ichktm2 = 24000
      ichkems = NINT( 1000*(dtems/60.) )
      if( (ichktm2 - ichktm1) .NE. ichkems ) then
          write(iout,'(//,a)')'ERROR in RDPTGRP:'
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
c   --- read the number of points and point locations ---
c
       read(iounit,ERR=7000) iseg, npoint
       if( npoint .NE. nptsrc ) goto 7002
       if( npoint .LE. 0 ) goto 9999
       read(iounit,ERR=7000,END=7001)  (idum, idum,
     &                                izcel(i), rdum, rdum,i=1,npoint)
c
c   --- read the emissions for this hour ---
c
       do 10 ispc=1,nspcpt(igroup)
          read(iounit,ERR=7000,END=7001) iseg, (iname(i),i=1,10),
     &                                            (emspts(i),i=1,nptsrc)
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
c   --- if the species is a not modelled or not a tracer species skip it ---
c
           idx = idxpts(igroup,ispc)
           if( idx .LE. 0 ) goto 10
           if( .NOT. lusespc(idx) ) goto 10
c
c   --- convert to PPM (PPMC) ----
c
           do i=1,nptsrc
             emspts(i) = emspts(i)/(60.*dtems)
c
c   --- load into the tracer emissions array ---
c
              do 20 icls=1,ntrcls
c
c   --- if this is the NOx emissions tracer and this is a 
c       PiG source, skip it (PiG treated elsewhere) ---
c
                  if( lpigsa(i) ) goto 20
                  emscls(icls,i) = emscls(icls,i) + 
     &                                    emspts(i) * trspmap(idx,icls)
   20         continue
           enddo
c
c   --- next species ---
c
  10   continue
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
          read(iounit)
          read(iounit)
          goto 111
      else
          goto 7001
      endif
      goto 9999
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in RDPTGRP:'
      write(iout,'(/,1X,A,I8.5,F8.1,2A)') 
     &      'Reading emissions after hour ',ibgdat, bgtim,' in file: ',
     &                                            fname(:istrln(fname))
      call camxerr()
c
 7001 continue
      write(iout,'(//,a)') 'ERROR in RDPTGRP:'
      write(iout,'(/,1X,3A)') 'Premature end-of-file reading ',
     &                  'emissions from file: ',fname(:istrln(fname))
      call camxerr()
c
 7002 continue
      write(iout,'(//,a)') 'ERROR in RDPTGRP:'
      write(iout,'(/,1X,A,I10,2A)') 'Number of points: ',npoint,
     &     ' is not consistent with regular emissions in file: ',
     &                                            fname(:istrln(fname))
      call camxerr()
c
 9999 continue
c
      return
      end

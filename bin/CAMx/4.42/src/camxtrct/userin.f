c**** USERIN.F
c
      subroutine userin(ierr)          
c
c-----------------------------------------------------------------------
c
c   This routine reads the user inputs and I/O files for the 
c   CAMXTRCT program.
c     Argument description:
c      Outputs:
c        ierr   I  error code
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camxtrct.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      integer*4 ierr
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
      integer*4 strlen
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*200 line
      character*200 fname, messag
      logical*4     lexist, lfound, ltemp
      integer*4     i, iflzp
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c  --- initialize the error code ---
c
      ierr = IFAIL
      write(IOWSTD,'(T30,A)') 'User Input Parameters'
c
c  --- get the type of extraction flag ---
c
      read(IORSTD,'(A)',ERR=7000) line
      exttyp = line(21:36)
      call lftjst( exttyp )
      call low2up( exttyp )
      lfound = .FALSE.
      if( exttyp .EQ. XYHOUR  ) lfound = .TRUE.
      if( exttyp .EQ. XZHOUR  ) lfound = .TRUE.
      if( exttyp .EQ. YZHOUR  ) lfound = .TRUE.
      if( exttyp .EQ. XYMAX   ) lfound = .TRUE.
      if( exttyp .EQ. XZMAX   ) lfound = .TRUE.
      if( exttyp .EQ. YZMAX   ) lfound = .TRUE.
      if( exttyp .EQ. TIMSER  ) lfound = .TRUE.
      if( exttyp .EQ. EMISS   ) lfound = .TRUE.
      if( exttyp .EQ. PTSRCE  ) lfound = .TRUE.
      if( .NOT. lfound ) goto 7001
      write(IOWSTD,'(A,T20,A,A)') 'Extraction type',': ',
     &                                       exttyp(:strlen(exttyp))
c
c  --- get the type of average file ----
c
      read(IORSTD,'(A)',ERR=7000) line
      avgtyp = line(21:36)
      call lftjst( avgtyp )
      call low2up( avgtyp )
      lfound = .FALSE.
      if( exttyp .EQ. PTSRCE .OR. exttyp .EQ. EMISS ) avgtyp = COARSE
      if( avgtyp(1:4) .EQ. FINE  ) then
         lfound = .TRUE.
         read(avgtyp(5:10),*,ERR=7009) grdnum
         avgtyp(5:10) = '      '
         write(IOWSTD,'(A,T20,A,2A,I2)') 'Grid type',': ',
     &                 avgtyp(:strlen(avgtyp)),' -- Grid #',grdnum
         if( grdnum .EQ. 0 ) goto 7009
      endif
      if( avgtyp .EQ. COARSE  ) then
         lfound = .TRUE.
         write(IOWSTD,'(A,T20,A,A)') 'Grid type',': ',
     &                 avgtyp(:strlen(avgtyp))
      endif
      if( avgtyp .EQ. ALL  ) then
         lfound = .TRUE.
         write(IOWSTD,'(A,T20,A,A)') 'Grid type',': ',
     &                 avgtyp(:strlen(avgtyp))
      endif
      if( .NOT. lfound ) goto 7004
c
c  --- get the type of output file format ----
c
      read(IORSTD,'(A)',ERR=7000) line
      if( exttyp .EQ. XYHOUR .OR. exttyp .EQ. XYMAX ) then
         outtyp = line(21:36)
         call lftjst( outtyp )
         call low2up( outtyp )
         lfound = .FALSE.
         if( outtyp .EQ. ASCII ) lfound = .TRUE.
         if( outtyp .EQ. BINARY ) lfound = .TRUE.
         if( .NOT. lfound ) goto 7006
         if( outtyp .EQ. ASCII ) then
            write(IOWSTD,'(A,T20,3A)') 'Output file type',': ',
     &                  outtyp(:strlen(outtyp)),' -- Surfer GRD format'
         else
            write(IOWSTD,'(A,T20,3A)') 'Output file type',': ',
     &            outtyp(:strlen(outtyp)),' -- CAMx Average file format'
         endif
      else
         outtyp = ASCII
         if( exttyp .EQ. PTSRCE ) then
            write(IOWSTD,'(A,T20,3A)') 'Output file type',': ',
     &                  outtyp(:strlen(outtyp)),' -- Surfer DAT format'
         else
            write(IOWSTD,'(A,T20,3A)') 'Output file type',': ',
     &                  outtyp(:strlen(outtyp)),' -- Surfer GRD format'
         endif
      endif
c
c  --- get the output file name ---
c
      read(IORSTD,'(A)',ERR=7000) line
      fname = line(21:)
      call lftjst( fname )
      write(IOWSTD,'(A,T20,A,A)') 'Output file',': ',
     &                                       fname(:strlen(fname))
      if( outtyp .EQ. ASCII ) then
         open(IOWGRD,file=fname,status='UNKNOWN',ERR=7007)
      else 
         open(IOWGRD,file=fname,status='UNKNOWN',
     &                                form='UNFORMATTED',ERR=7007)
      endif
c
c  --- get the input file name ---
c
      read(IORSTD,'(A)',ERR=7000) line
      fname = line(21:)
      if( fname .EQ. ' ' ) goto 7011
      call lftjst( fname )
      if( exttyp .EQ. EMISS .OR. exttyp .EQ. PTSRCE ) then
         write(IOWSTD,'(A,T20,A,A)') 'Emission file',': ',
     &                                       fname(:strlen(fname))
      else
         write(IOWSTD,'(A,T20,A,A)') 'Coarse Average file',': ',
     &                                       fname(:strlen(fname))
      endif
      messag = 'Emission or coarse grid file must be supplied.'
      if( fname .EQ. ' ' ) goto 7005
      inquire(file=fname,exist=lexist)
      if( .NOT. lexist ) goto 7002
      open(IORCAV,file=fname,form='UNFORMATTED',status='OLD',ERR=7003)
c
      read(IORSTD,'(A)',ERR=7000) line
      if( avgtyp .EQ. ALL .OR. avgtyp .EQ. FINE ) then
         fname = line(21:)
         call lftjst( fname )
         write(IOWSTD,'(A,T20,A,A)') 'Fine Average file',': ',
     &                                       fname(:strlen(fname))
         messag = 'Fine grid average file must be supplied for '//
     &                                     'FINE or ALL extraction.'
         if( fname .EQ. ' ' ) goto 7005
         inquire(file=fname,exist=lexist)
         if( .NOT. lexist ) goto 7002
         open(IORFAV,file=fname,form='UNFORMATTED',
     &                                       status='OLD',ERR=7003)
       endif
c
c  --- get the boundary file name ----
c
      read(IORSTD,'(A)',ERR=7000) line
      fname = line(21:)
      call lftjst( fname )
      if( fname .EQ. ' ' ) then
          lbndry = .FALSE.
          write(IOWSTD,'(A,T20,2A)' )'Boundary file ',': Not supplied ',
     &                            '-- Assuming 1-cell regular boundary'
      else
          lbndry = .TRUE.
          write(IOWSTD,'(A,T20,A,A)') 'Boundary file',': ',
     &                                       fname(:strlen(fname))
          inquire(file=fname,exist=lexist)
          if( .NOT. lexist ) goto 7002
          open(IORBND,file=fname,form='UNFORMATTED',
     &                                           status='OLD',ERR=7003)
      endif
c
c  --- get the station id file, skip if not TIMSER extraction ---
c
      read(IORSTD,'(A)',ERR=7000) line
      if( exttyp .EQ. TIMSER ) then
         fname = line(21:)
         call lftjst( fname )
         write(IOWSTD,'(A,T20,A,A)') 'Station file',': ',
     &                                       fname(:strlen(fname))
         inquire(file=fname,exist=lexist)
         if( .NOT. lexist ) goto 7002
         open(IORSTN,file=fname,status='OLD',ERR=7003)
      endif
c
c  --- get the type of layer to extract ---
c
      read(IORSTD,'(A)',ERR=7000) line
      laytyp = line(21:36)
      call lftjst( laytyp )
      call low2up( laytyp )
      lfound = .FALSE.
      if( exttyp .EQ. EMISS .OR. exttyp .EQ. PTSRCE ) then
         klayer = 1
         laytyp = SURFAC
      endif
      if( laytyp(1:5) .EQ. ALOFT  ) then
         lfound = .TRUE. 
         read(laytyp(6:10),*,ERR=7010) klayer
         laytyp(6:10) = '     ' 
         if( klayer .EQ. 0 ) goto 7010
      endif
      if( laytyp .EQ. SURFAC ) then
         lfound = .TRUE.
         klayer = 1
      endif
      if( .NOT. lfound ) goto 7008
      write(IOWSTD,'(A,T20,A,2A,i2)') 'Type of layer ',': ',
     &                 laytyp(:strlen(laytyp)),' -- Layer #',klayer
c
c  --- get the time interval (skip if not hourly extraction) ---
c
      read(IORSTD,'(A)',ERR=7000) line
      if( exttyp .EQ. XYHOUR .OR. exttyp .EQ. XZHOUR .OR. 
     &              exttyp .EQ. YZHOUR .OR. exttyp .EQ. TIMSER ) then
         read(line(21:30),'(I10)',ERR=7000) inter
         write(IOWSTD,'(A,T20,A,I5)') 'Time interval',': ',inter
      else
         inter = 1
      endif
c
c  --- get the first hour (skip if not hourly extraction) ---
c
      read(IORSTD,'(A)',ERR=7000) line
      if( exttyp .EQ. XYHOUR .OR. exttyp .EQ. XZHOUR .OR. 
     &              exttyp .EQ. YZHOUR .OR. exttyp .EQ. TIMSER ) then
         read(line(21:30),'(I10)',ERR=7000) istart
         write(IOWSTD,'(A,T20,A,I5)') 'First hour',': ',istart
      endif
c
c   --- get the column to extract (skip if not YZ extraction) ----
c
      read(IORSTD,'(A)',ERR=7000) line
      if( exttyp .EQ. YZHOUR .OR. exttyp .EQ. YZMAX ) then
         read(line(21:30),'(I10)',ERR=7000) jcol
         write(IOWSTD,'(A,T20,A,I5)') 'Extract Column',': ',jcol
      endif
c
c   --- get the row to extract (skip if not XZ extraction) ----
c
      read(IORSTD,'(A)',ERR=7000) line
      if( exttyp .EQ. XZHOUR .OR. exttyp .EQ. XZMAX ) then
         read(line(21:30),'(I10)',ERR=7000) irow
         write(IOWSTD,'(A,T20,A,I5)') 'Extract Row',': ',irow
      endif
c
c   --- get the species name ----
c
      read(IORSTD,'(A)',ERR=7000) line
      spname = line(21:41)
      call lftjst( spname )
      call low2up( spname )
      write(IOWSTD,'(A,T20,2A)') 'Species to Extract',': ',spname
c
c   --- get the units string ---
c
      read(IORSTD,'(A)',ERR=7000) line
      untstr = line(21:41)
      call lftjst( untstr )
      call low2up( untstr )
      write(IOWSTD,'(A,T20,2A)') 'Output units',': ',untstr
c
c   --- get the units conversion factor  ---
c
      read(IORSTD,'(A)',ERR=7000) line
      read(line(21:30),'(F10.0)',ERR=7000) untfac
      write(IOWSTD,'(A,T20,A,F20.8)') 'Units conv. factor',': ',untfac
c
c   --- get the number of decimal places for output  ---
c
      ndec = 2
      read(IORSTD,'(A)',ERR=7000) line
      if( outtyp .EQ. ASCII ) then
        read(line(21:30),'(I10)',ERR=7000) ndec
        write(IOWSTD,'(A,T20,A,I5)') 'Number of decimals',': ',ndec
      endif
c
c   --- get the minimum value for output  ---
c
      read(IORSTD,'(A)',ERR=7000) line
      if( outtyp .EQ. ASCII .AND. 
     &              (exttyp .EQ. EMISS .OR. exttyp .EQ. PTSRCE) ) then
        read(line(21:30),'(F10.0)',ERR=7000) epslon
        write(IOWSTD,'(A,T20,A,F10.5)') 'Minimum output value',': ',
     &                                                          epslon
      endif
c
c  --- check if temperature file is needed and open ---
c
      ltemp = .false.
      if ( (exttyp .EQ. XYHOUR .OR. exttyp .EQ. XYMAX)
     &     .AND. avgtyp .EQ. ALL .AND. laytyp .EQ. ALOFT )  then
         read(IORSTD,'(A)',ERR=7000) line
         fname = line(21:)
         call lftjst( fname )
         write(IOWSTD,'(A,T20,A,A)') 'Temperature file',': ',
     &                                       fname(:strlen(fname))
         messag = 'Temperature file required for ALL ALOFT extraction.'
         if( fname .EQ. ' ' ) goto 7005
         inquire(file=fname,exist=lexist)
         if( .NOT. lexist ) goto 7002
         open(IORTMP,file=fname,form='UNFORMATTED',
     &                                    status='OLD',ERR=7003)
         ltemp = .true.
      endif
c
c  --- check if height/pressure (ZP) files are needed and open ---
c
      if( ltemp .OR. exttyp .EQ. XZHOUR .OR. exttyp .EQ. XZMAX
     &    .OR. exttyp .EQ. YZHOUR .OR. exttyp .EQ. YZMAX )  then
c
c  --- number of grids ---
c
         read(IORSTD,'(A)',ERR=7000) line
         read(line(21:30),'(I5)',ERR=7000) nzpfil
         write(IOWSTD,'(A,T20,A,I5)') 'Number of grids',': ',
     &                                                 nzpfil 
         if( ltemp ) then
            messag = 'ZP files required for XY ALL ALOFT '//
     &              'extractions.  Invalid number of grids specified.'
            if( nzpfil .LE. 0 .OR. nzpfil .GT. MXGRID ) goto 7005
         else
            if( avgtyp .EQ. FINE .OR. avgtyp .EQ. COARSE ) then
               messag = 'ZP files required for XZ/YZ FINE/COARSE '//
     &              'extractions.  Number of grids must be 1.'
               if( nzpfil .NE. 1 ) goto 7005
            else
               messag = 'ZP files required for XZ/YZ ALL '//
     &              'extractions.  Invalid number of grids specified.'
               if( nzpfil .LE. 0 .OR. nzpfil .GT. MXGRID ) goto 7005
            endif
         endif
c
c  --- get the height/pressure file names ----
c
         do i = 1, nzpfil
            read(IORSTD,'(A)',ERR=7000) line
            fname = line(21:)
            call lftjst( fname )
            write(IOWSTD,'(A,T20,A,A)') 'ZP file fine',': ',
     &                                       fname(:strlen(fname))
            messag = 'ZP file required for this extraction.'
            if( fname .EQ. ' ' ) goto 7005
            inquire(file=fname,exist=lexist)
            if( .NOT. lexist ) goto 7002
            iflzp = IORZP + i - 1
            open(iflzp,file=fname,form='UNFORMATTED',
     &                                       status='OLD',ERR=7003)
         enddo
      endif
c
c   --- set error code and return ---
c
      ierr = ISUCES
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(1X,2A,/,A)') 'ERROR:  Reading user inputs.  ',
     &                            'Last line read: ',line(:strlen(line))
      goto 9999
c
 7001 continue
      write(IOWSTD,'(2A)') 'ERROR: Invalid entry for extraction type: ',
     &                                                       line(21:36)
      write(IOWSTD,'(A)') 'Should be one of the following: '
      write(IOWSTD,'(10X,A)') XYHOUR
      write(IOWSTD,'(10X,A)') XZHOUR
      write(IOWSTD,'(10X,A)') YZHOUR
      write(IOWSTD,'(10X,A)') XYMAX 
      write(IOWSTD,'(10X,A)') XZMAX
      write(IOWSTD,'(10X,A)') YZMAX
      write(IOWSTD,'(10X,A)') TIMSER
      write(IOWSTD,'(10X,A)') EMISS
      write(IOWSTD,'(10X,A)') PTSRCE
      goto 9999
c
 7002 continue
      write(IOWSTD,'(2A)') 'ERROR: Input file not found: ',
     &                                             fname(:strlen(fname))
      goto 9999
c
 7003 continue
      write(IOWSTD,'(2A)') 'ERROR: Opening input file: ',
     &                                             fname(:strlen(fname))
      goto 9999
c
 7004 continue
      write(IOWSTD,'(2A)') 
     &           'ERROR: Invalid entry for type of average file: ',
     &                                                     line(21:36)
      goto 9999
c
 7005 continue
      write(IOWSTD,'(2A)') 'ERROR: ',messag
      goto 9999
c
 7006 continue
      write(IOWSTD,'(2A)') 
     &           'ERROR: Invalid entry for type of output file: ',
     &                                                     line(21:36)
      goto 9999
c
 7007 continue
      write(IOWSTD,'(2A)') 'ERROR:  Cannot open output file: ',
     &                                             fname(:strlen(fname))
      goto 9999
c
 7008 continue
      write(IOWSTD,'(2A)') 'ERROR:  Invalid entry for type of layer: ',
     &                                             line(21:36)
      goto 9999
c
 7009 continue
      write(IOWSTD,'(2A)') 'ERROR:  Must supply non-zero grid number ',
     &                                             line(21:36)
      goto 9999
c
 7010 continue
      write(IOWSTD,'(2A)') 'ERROR:  Must supply non-zero layer number ',
     &                                             line(21:36)
      goto 9999
c
 7011 continue
      write(IOWSTD,'(2A)') 
     &           'ERROR:  Must supply coarse grid average filename.'
      goto 9999
c
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

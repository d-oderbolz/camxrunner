c**** FILBND.F
c
      subroutine filbnd(ierr)          
c
c-----------------------------------------------------------------------
c
c   This routine reads the header of the boundary file and stores the
c   idexes of the boundary cells in common to be used by the 
c   CAMXTRCT program.
c   If no boundary file is supplied, the indexes are set to the 
c   domain edge cells. 
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
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 irec, igrd, i, j, idum, ncells
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c  --- initialize the error code ---
c
      ierr = IFAIL
c
c  --- initialize the boundary definition for the fine grids ---
c
      if( avgtyp .EQ. ALL .OR. avgtyp .EQ. FINE) then
         do 10 igrd=1,ngrid
            do 20 j=1,nygrid(igrd)
               iwest(j,igrd) = 2
               ieast(j,igrd) = nxgrid(igrd) - 1
   20       continue
            do 30 i=1,nxgrid(igrd)
               isouth(i,igrd) = 2
               inorth(i,igrd) = nygrid(igrd) - 1
   30       continue
            iwest(1,igrd) = -9
            iwest(nygrid(igrd),igrd) = -9
            ieast(1,igrd) = -9
            ieast(nygrid(igrd),igrd) = -9
            isouth(1,igrd) = -9
            isouth(nxgrid(igrd),igrd) = -9
            inorth(1,igrd) = -9
            inorth(nxgrid(igrd),igrd) = -9
   10    continue
      endif
c
c  --- if no boundary file is supplied, just set to the edge cells ----
c
      if( .NOT. lbndry ) then
c
c  --- East and West boundaries ----
c
         do 40 j=1,nycell
           ieast(j,0) = nxcell - 1
           iwest(j,0) = 2
   40    continue
c
c  --- North and South boundaries ---
c
         do 50 i=1,nxcell
           inorth(i,0) = nycell - 1
           isouth(i,0) = 2
   50    continue
         ierr = ISUCES
c
c  --- set the error flag and return ---
c
         goto 9999
      endif
c
c  --- skip the header records, this assumes that the correct boundary
c      file was used ---
c
      do 60 irec=1,4
         read(IORBND,ERR=7000)
   60 continue
c
c   --- read the cells on the west boundary ---
c
      read(IORBND,ERR=7001) idum, idum, ncells, 
     &                       (iwest(i,0), idum, idum, idum, i=1,ncells)
c
c   --- read the cells on the east boundary ---
c
      read(IORBND,ERR=7002) idum, idum, ncells, 
     &                       (ieast(i,0), idum, idum, idum, i=1,ncells)
c
c   --- read the cells on the south boundary ---
c
      read(IORBND,ERR=7003) idum, idum, ncells, 
     &                       (isouth(i,0), idum, idum, idum, i=1,ncells)
c
c   --- read the cells on the north boundary ---
c
      read(IORBND,ERR=7004) idum, idum, ncells, 
     &                       (inorth(i,0), idum, idum, idum, i=1,ncells)
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
      write(IOWSTD,'(2A,I5)',ERR=9999) 'ERROR: Reading header of ',
     &                                 'boundary file at record: ',irec
      goto 9999
c
 7001 continue
      write(IOWSTD,'(2A,I5)',ERR=9999) 'ERROR: Reading WEST boundary ',
     &                              'cells of boundary file at record.'
      goto 9999
c
 7002 continue
      write(IOWSTD,'(2A,I5)',ERR=9999) 'ERROR: Reading EAST boundary ',
     &                              'cells of boundary file at record.'
      goto 9999
c
 7003 continue
      write(IOWSTD,'(2A,I5)',ERR=9999) 'ERROR: Reading SOUTH boundary ',
     &                              'cells of boundary file at record.'
      goto 9999
c
 7004 continue
      write(IOWSTD,'(2A,I5)',ERR=9999) 'ERROR: Reading NORTH boundary ',
     &                              'cells of boundary file at record.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

c**** RDSTN.F
c
      subroutine rdstn(gridin, ierr)
c
c-----------------------------------------------------------------------
c
c   This routine returns an array of concentration values at the ground
c   layer (first layer in the XY plane), one for each
c   station requested.  The input to this routine is a 3-dimensional 
c   concentration matrix for a certain hour.
c     Argument description:
c      Outputs:
c        ierr   I  error code
c        gridin   3-dimensional matrix for one hour 
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
      real*4 gridin(MXCELL, MXCELL, MXLAYR)
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
c    grdtrp  R   returns the bi-linear interpolated value at a point
c
      real*4 grdtrp
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*200 stnname 
      integer*4 i, numstn, statsymb 
      real*4 lat(MXSTN), long(MXSTN), stnx, stny, celli, cellj,
     &       stnconc(MXSTN)
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c  --- initialize the error code ---
c
      ierr = IFAIL
      rewind(IORSTN) 

c  --- reads in the lat/longs of each monitoring station ---
      do i = 1, MXSTN 
        read (IORSTN, 1001, ERR=7000, END=1000) stnname, lat(i),
     &        long(i), statsymb
      enddo
   
 1000 numstn = i - 1
 1001 FORMAT (A10, 2F10.0, I4)

c  --- cycles through each monitoring station ---
 
      do i = 1, numstn
        
        stnx = long(i) - xorig
        stny = lat(i) - yorig
        celli = INT(stnx/deltax)
        cellj = INT(stny/deltay)
       
c   --- when this condition is true the station is out of bounds 
        if ((celli .LT. iwest(celli,0) .OR. 
     &         celli .GT. ieast(celli,0)) .OR.
     &    (cellj .LT. isouth(cellj,0) .OR. 
     &         cellj .GT. inorth(cellj,0))) then
          stnconc(i) = -9999 
          write(IOWSTD) 'Station out of bounds: rdstn.f'
          goto 2000
        end if 

c   --- when this condition is true the station is in a boundary cell
        if ((celli .EQ. iwest(celli,0) .OR. 
     &                       celli .EQ. ieast(celli,0)) .AND.
     &    (cellj .EQ. isouth(cellj,0) .OR. 
     &                       cellj .EQ. inorth(cellj,0))) then
          stnconc(i) = gridin(celli, cellj, 1) 
          goto 2000
        end if 

        stnconc(i) = grdtrp(ierr, stnx, stny, gridin)
        
 2000   continue
      enddo

      write(IOWGRD, 2001) (stnconc(i), i = 1, numstn)
 2001 FORMAT(1000(:,f10.3))

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
      write(IOWSTD,'(2A,I5)',ERR=9999) 'ERROR: Reading station ',
     &                                 'file at record: ',i
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

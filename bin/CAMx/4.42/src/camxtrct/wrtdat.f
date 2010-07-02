c**** WRTDAT.F
c
      subroutine wrtdat(ierr, outgrd, iounit, orgx, orgy, ncols, nrows,
     &                                      delx, dely, mesh, cvtfac )
c
c-----------------------------------------------------------------------
c
c   This routine writes the output data in surfer DAT format. The edge 
c   cells are not written.
c     Argument description:
c       Outputs:
c        ierr     I   error code
c       Inputs:
c        outgrd   R   2-dimensional grid representing one hour concs
c        iounit   I   the output unit number
c        orgx     R   X coordinate of the origin of the 2d grid
c        orgy     R   Y coordinate of the origin of the 2d grid
c        ncols    I   the number of cells in the x direction
c        nrows    I   the number of cells in the y direction
c        delx     R   the width of one X cell in the coordinate system
c        dely     R   the width of one Y cell in the coordinate system
c        mesh     I   meshing factor in fine grid (used for calculating
c                     the interior,  1 gives single cell boundary )
c        cvtfat   R   conversion factor to units we want
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
      integer*4    ierr
      real*4       outgrd(MXCELL,MXCELL)
      integer*4    iounit
      real*4       orgx
      real*4       orgy
      integer*4    ncols
      integer*4    nrows
      real*4       delx
      real*4       dely
      integer*4    mesh
      real*4       cvtfac
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
      integer*4     i, j
      real*4        tmpval, xutm, yutm, divfac
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c  ---- initialize the error code ----
c
      ierr = IFAIL
c
c  --- set conversion if the coordinates are in meters ----
c
      if( ABS(orgx) .GT. 9999 ) then
        divfac = 1000.0
      else
        divfac = 1.0
      endif
c
c  --- write the concentration grid to the output to the DAT file ---
c
      do j = mesh+1,nrows-mesh
        yutm = FLOAT(j-1)*dely + orgy + dely/2.0
        yutm = yutm / divfac
        do i = mesh+1,ncols-mesh
c
c   --- calculate the center of the cell ---
c
           xutm = FLOAT(i-1)*delx + orgx + delx/2.0
           xutm = xutm / divfac
           tmpval = outgrd(i,j) * cvtfac
           if( tmpval .GT. epslon ) 
     &               write(iounit,'(3F20.5)',ERR=7000) xutm, yutm, 
     &                                                 tmpval
        enddo
      enddo
c
c  --- set error code to success and return ---
c
      ierr = ISUCES 
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(IOWSTD,'(1X,A)') 'ERROR: Writing output DAT file.' 
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

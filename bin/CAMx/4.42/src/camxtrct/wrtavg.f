c**** WRTAVG.F
c
      subroutine wrtavg(ierr, outgrd, iounit, ncols, nrows, species, 
     &                                      jdate, ibeghr, cnvfac )
c
c-----------------------------------------------------------------------
c
c   This routine writes the output data in surfer GRD format. The edge 
c   cells are not written.
c     Argument description:
c       Outputs:
c        ierr     I   error code
c       Inputs:
c        outgrd   R   2-dimensional grid representing one hour concs
c        iounit   I   the output unit number
c        ncols    I   the number of cells in the x direction
c        nrows    I   the number of cells in the y direction
c        species  C   the name of the species
c        jdate    I   the julian date
c        ibeghr   I   the beginning hour
c        cnvfac   R   conversion factor 
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
      integer*4    ncols
      integer*4    nrows
      character*10 species
      integer*4    jdate
      integer*4    ibeghr
      real*4       cnvfac
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 iname(10), ione, i, j
c
c-----------------------------------------------------------------------
c    Data statements:
c-----------------------------------------------------------------------
c
      data ione /1/
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c  ---- initialize the error code ----
c
      ierr = IFAIL
      write(iounit,ERR=7000) jdate, FLOAT(ibeghr), 
     &                                           jdate, FLOAT(ibeghr+1)
c
c  --- write the concentration field ----
c
       read(species,'(10A1)') (iname(i),i=1,10)
       call bswap(iname,10,0)
       write(iounit,ERR=7000) ione, iname, 
     &                     ((outgrd(i,j)*cnvfac,i=1,ncols),j=1,nrows)
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
      write(IOWSTD,'(1X,A)') 'ERROR: Writing output AVERAGE file.' 
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

c**** WRTHDR.F
c
      subroutine wrthdr(ierr, iounit, orgx, orgy, ncols, nrows,
     &                                           delx, dely, species)
c
c-----------------------------------------------------------------------
c
c   This routine writes the output data in surfer GRD format. The edge 
c   cells are not written.
c     Argument description:
c       Outputs:
c        ierr     I   error code
c       Inputs:
c        iounit   I   the output unit number
c        orgx     R   X coordinate of the origin of the 2d grid
c        orgy     R   Y coordinate of the origin of the 2d grid
c        ncols    I   the number of cells in the x direction
c        nrows    I   the number of cells in the y direction
c        delx     R   the width of one X cell in the coordinate system
c        dely     R   the width of one Y cell in the coordinate system
c        species  C   the name of the species
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
      integer*4    iounit
      real*4       orgx
      real*4       orgy
      integer*4    ncols
      integer*4    nrows
      real*4       delx
      real*4       dely
      character*10 species
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
      character*10  cname
      integer*4     iname(10), inote(60), ione, izero, i
      real*4        zero
c
c-----------------------------------------------------------------------
c    Data statements:
c-----------------------------------------------------------------------
c
      data cname /'AVERAGE'/
      data ione /1/
      data zero /0/
      data izero /0/
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c  ---- initialize the error code ----
c
      ierr = IFAIL
      read(cname,'(10A1)' ) (iname(i),i=1,10)
      read(cnote,'(60A1)' ) (inote(i),i=1,60)
      call bswap(iname,60,0)
      write(iounit,ERR=7000) iname, inote, ione, ione,
     &                                 idtbeg, timbeg, idtend, timend
c
      write(iounit,ERR=7000) zero, zero, iutmzn, orgx, orgy, delx, dely, 
     &                ncols, nrows, ione, izero, izero, zero, zero, zero
c
      write(iounit,ERR=7000) izero, izero, ncols, nrows
c
      read(species,'(10A1)' ) (iname(i),i=1,10)
      call bswap(iname,10,0)
      write(iounit,ERR=7000) iname
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
      write(IOWSTD,'(1X,A)') 
     &                'ERROR: Writing header of output AVERAGE file.' 
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

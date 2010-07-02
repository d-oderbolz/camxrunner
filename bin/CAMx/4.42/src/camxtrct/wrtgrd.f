c**** WRTGRD.F
c
      subroutine wrtgrd(ierr, outgrd, iounit, orgx, orgy, ncols, nrows,
     &                delx, dely, mesh, species, units, cvtfac, ndecs,
     &                                     jdate, ibeghr, level, ltype )
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
c        orgx     R   X coordinate of the origin of the 2d grid
c        orgy     R   Y coordinate of the origin of the 2d grid
c        ncols    I   the number of cells in the x direction
c        nrows    I   the number of cells in the y direction
c        delx     R   the width of one X cell in the coordinate system
c        dely     R   the width of one Y cell in the coordinate system
c        species  C   the name of the species
c        units    C   the units of the species
c        cvtfac   R   conversion factor for output units
c        ndecs    I   number of decimal places on output
c        jdate    I   the julian date
c        ibeghr   I   the beginning hour
c        level    I   the layer number of the 2-d grid
c        ltype    C   can be "layer", "column", or "row"
c        mesh     I   meshing factor in fine grid (used for calculating
c                     the interior,  1 gives single cell boundary )
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
      character*10 species
      character*10 units
      real*4       cvtfac
      integer*4    ndecs
      integer*4    jdate
      integer*4    ibeghr
      integer*4    level
      character*10 ltype  
      integer*4    mesh
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
      character*10  fmtstr
      integer*4     iendhr, nxint, nyint, nlines, i, j, k
      real*4        divfac, tmpval, xmax, xmin, ymax, ymin
      real*4        zmin, zmax, zminx, zminy, zmaxx, zmaxy, totems
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c  ---- initialize the error code ----
c
      ierr = IFAIL
      iendhr = ibeghr + 1
c
c  --- set conversion if the coordinates are in meters ----
c
      if( ABS(orgy) .GT. 9999 ) then 
        divfac = 1000.0
      else 
        divfac = 1.0
      endif
      totems = 0.
c
c  --- calculates the number of cells without boundaries ----
c
      nxint = ncols - 2*mesh
      nyint = nrows - 2*mesh
c
c  --- calculate the number of lines it takes to output the grid
c
      nlines = nyint * INT(nxint/10)
      if( MOD( nxint, 10 ) .NE. 0 ) then
        nlines = nlines + nyint
      endif
c
c  --- calculate the centroid of the bottom left cell of the grid ---
c
      if( exttyp .NE. EMISS .AND. exttyp .NE. PTSRCE) then
         xmin = (orgx + (FLOAT(mesh) + 0.5) * delx) / divfac 
         ymin = (orgy + (FLOAT(mesh) + 0.5) * dely) / divfac 
c
c  --- calculate the centroid of the top right cell of the grid
c
         xmax = (orgx + (FLOAT(ncols-mesh) - 0.5) * delx ) / divfac
         ymax = (orgy + (FLOAT(nrows-mesh) - 0.5) * dely ) / divfac
      else
         xmin = (orgx + FLOAT(mesh) * delx) / divfac 
         ymin = (orgy + FLOAT(mesh) * dely) / divfac 
         xmax = (orgx + FLOAT(ncols-mesh) * delx ) / divfac
         ymax = (orgy + FLOAT(nrows-mesh) * dely ) / divfac
      endif
c
c  --- inititalize the min/max parameters ---
c
      zmaxx = -9.0
      zmaxy = -9.0
      zmax = -9.0
      zminx = -9.0
      zminy = -9.0
      zmin = 9999999.0
c
c  --- calculate the min and max parameters ----
c
      do i = 1+mesh,ncols-mesh
        do j = 1+mesh,nrows-mesh
          totems = totems + outgrd(i,j) * cvtfac
          if( outgrd(i,j)*cvtfac .GT. zmax ) then
            zmaxx = FLOAT( i )
            zmaxy = FLOAT( j )
            zmax = outgrd(i,j) * cvtfac
          endif
          if( outgrd(i,j)*cvtfac .LT. zmin ) then
            zminx = FLOAT( i )
            zminy = FLOAT( j )
            zmin = outgrd(i,j) * cvtfac
          endif
        enddo
      enddo
c
c  ---- if field is constant, just return ----
c
      if( zmax .EQ. zmin ) then
          write(IOWSTD,'(/,1X,2A,/)') 'WARNING:  Uniform ',
     &                      'concentration field.  Skipping this hour.'
          goto 9999
      endif
c
c  --- write the header data to the output to the GRD file ---
c
      write(iounit,'(A)',ERR=7000) species
      write(iounit,'(A)',ERR=7000) units(1:3)
      write(iounit,'(I6)',ERR=7000) jdate
      write(iounit,'(I6)',ERR=7000) ibeghr
      write(iounit,'(I6)',ERR=7000) iendhr 
      write(iounit,'(I6)',ERR=7000) level 
cgmw      write(iounit,'(A)',ERR=7000) ltype
      write(iounit,'(I6)',ERR=7000) nlines 
      write(iounit,'(A)',ERR=7000) 'DSAA' 
      write(iounit,'(I6)',ERR=7000) nxint 
      write(iounit,'(I6)',ERR=7000) nyint 
      if( ABS(xmin) .GT. 99999.0 ) then
          write(iounit,'(F10.2)',ERR=7000) xmin
      else 
          write(iounit,'(F10.4)',ERR=7000) xmin
      endif
      if( ABS(xmax) .GT. 99999.0 ) then
          write(iounit,'(F10.2)',ERR=7000) xmax
      else 
          write(iounit,'(F10.4)',ERR=7000) xmax
      endif
      if( ABS(ymin) .GT. 99999.0 ) then
          write(iounit,'(F10.2)',ERR=7000) ymin
      else 
          write(iounit,'(F10.4)',ERR=7000) ymin
      endif
      if( ABS(ymax) .GT. 99999.0 ) then
          write(iounit,'(F10.2)',ERR=7000) ymax
      else 
          write(iounit,'(F10.4)',ERR=7000) ymax
      endif
      if( ABS(zmin) .GT. 99999.0 ) then
          write(iounit,'(F10.2)',ERR=7000) zmin
      else 
          write(iounit,'(F10.4)',ERR=7000) zmin
      endif
      if( ABS(zmax) .GT. 99999.0 ) then
          write(iounit,'(F10.2)',ERR=7000) zmax
      else 
          write(iounit,'(F10.4)',ERR=7000) zmax
      endif
c
c  --- write the concentration grid to the output to the GRD file ---
c
      write(fmtstr,'(A,I1,A)')'(F10.',ndecs,')'
      do j = mesh+1,nrows-mesh
        line = ' '
        k = 0
        do i = mesh+1,ncols-mesh
           k = k + 1
           tmpval = outgrd(i,j)
           if( tmpval .NE. -9.0 ) tmpval = tmpval * cvtfac
           if( tmpval .EQ. 0. ) then
               write(line((k-1)*10+1:k*10),'(F10.0)') tmpval
           else if( tmpval .LT. epslon ) then
               write(line((k-1)*10+1:k*10),fmtstr) epslon
           else
               write(line((k-1)*10+1:k*10),fmtstr) tmpval
           endif
           if( k .EQ. 10 ) then
              write(iounit,'(A)') line(:strlen(line))
              line = ' '
              k = 0
           endif
        enddo
        if( k .NE. 0 ) write(iounit,'(A)') line(:strlen(line))
        line = ' '
      enddo
      write(iounit,'(A)',ERR=7000) 'ENDGRD'
c
c  --- write the max parameters to the output to the GRD file ---
c
      if( exttyp .NE. EMISS .AND. exttyp .NE. PTSRCE) then
         zminx = (orgx + (zminx-0.5) * delx ) / divfac
         zminy = (orgy + (zminy-0.5) * dely ) / divfac
         zmaxx = (orgx + (zmaxx-0.5) * delx ) / divfac
         zmaxy = (orgy + (zmaxy-0.5) * dely ) / divfac
         line = ' '
         if( zmaxx .GT. 99999.0 ) then
             write(line(1:10),'(F10.2)',ERR=7000) zmaxx
         else 
             write(line(1:10),'(F10.4)',ERR=7000) zmaxx
         endif
         if( zmaxy .GT. 99999.0 ) then
             write(line(11:20),'(F10.2)',ERR=7000) zmaxy
         else 
             write(line(11:20),'(F10.4)',ERR=7000) zmaxy
         endif
         if( zmax .GT. 99999.0 ) then
             write(line(21:30),'(F10.2)',ERR=7000) zmax
         else 
             write(line(21:30),'(F10.4)',ERR=7000) zmax
         endif
         line(31:40) = '    1    2'
         write(iounit,'(A)') line(:strlen(line))
c
c  --- write the min parameters to the output to the GRD file ---
c
         if( zminx .GT. 99999.0 ) then
             write(line(1:10),'(F10.2)',ERR=7000) zminx
         else 
             write(line(1:10),'(F10.4)',ERR=7000) zminx
         endif
         if( zminy .GT. 99999.0 ) then
             write(line(11:20),'(F10.2)',ERR=7000) zminy
         else 
             write(line(11:20),'(F10.4)',ERR=7000) zminy
         endif
         if( zmin .GT. 99999.0 ) then
             write(line(21:30),'(F10.2)',ERR=7000) zmin
         else 
             write(line(21:30),'(F10.4)',ERR=7000) zmin
         endif
         line(31:40) = '    1    5'
         write(iounit,'(A)') line(:strlen(line))
      else
c
c  --- write the location of the max and the total for emissions plots ---
c
         write(iounit,'(I5)') INT( zmaxx )
         write(iounit,'(I5)') INT( zmaxy )
         write(iounit,'(F20.8)')  zmax
         write(iounit,'(F20.8)')  totems
      endif
c
      write(iounit,'(A)',ERR=7000) 'ENDDAT'
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
      write(IOWSTD,'(1X,A)') 'ERROR: Writing output GRD file.' 
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

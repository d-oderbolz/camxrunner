c**** RDHDFG.F
c
      subroutine rdhdfg(ierr)          
c
c-----------------------------------------------------------------------
c
c   This routine reads the header of the fine grid average file
c   and stores the data in common to be used bu the CAMXTRCT program.
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
      character*10 spcnam(MXSPEC)
      integer*4    ixfe, jyfe, nvf, ifglvl
      integer*4    irec, i, j, nspc
      integer*4    mshmax
      logical*4    lmatch
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c  --- initialize the error code ---
c
      ierr = IFAIL
      write(IOWSTD,'(/,T30,A)') 'Fine Grid Average File Parameters'
      rewind(IORFAV)
c
c  --- read the run id ---
c
      irec = 1
cgmw      read(IORFAV,ERR=7000) cnote
      read(IORFAV) cnote
      write(IOWSTD,'(A,T20,A,A)') 'Average File Id',':',
     &                                           cnote(:strlen(cnote))
c
c  --- read the number of grids and the number of species ---
c
      irec = irec + 1
      read(IORFAV,ERR=7000) ngrid, nspc
      if( nspc .NE. nspec ) goto 7005
      write(IOWSTD,'(A,T20,A,I2)',ERR=9999) 'Number of grids',': ',
     &                                                           ngrid
      if( nzpfil .GT. 1 .AND. nzpfil .LT. ngrid+1 ) goto 7009 
c
c   --- read the species names ---
c
      irec = irec + 1
      read(IORFAV,ERR=7000) (spcnam(j),j=1,nspc)
      write(IOWSTD,'(A,T20,A)',ERR=9999) 'Species in file',':'
      lmatch = .TRUE.
      do 10 j=1,nspec
         write(IOWSTD,'(T20,A)',ERR=9999) spcnam(j)(:strlen(spcnam(j)))
         if( spcnam(j) .NE. spclst(j) ) lmatch = .FALSE.
   10 continue
      if( .NOT. lmatch ) goto 7006
c
c   ---- read the domain definition of each grid ---
c
      do 20 i=1,ngrid
          irec = irec + 1
          read(IORFAV,ERR=7000) iclbeg(i), jclbeg(i), ixfe, jyfe, 
     &                          mshfac(i), nvf, nxgrid(i), nygrid(i), 
     &                          nlgrid(i), iparnt(i), ifglvl
  20  continue
c
c   ---- calculate the size of the newly resolved grid and the mesh
c        factor into this new resolution ----
c
      if( avgtyp .EQ. FINE ) then
         i = grdnum
         nmesh(i) = 1
         grdelx(i) = deltax / FLOAT( mshfac(i) )
         grdely(i) = deltay / FLOAT( mshfac(i) )
         grxorg(i) = FLOAT(iclbeg(i) - 1) * deltax + xorig - grdelx(i)
         gryorg(i) = FLOAT(jclbeg(i) - 1) * deltay + yorig - grdely(i)
         iclbeg(i) = 1
         jclbeg(i) = 1
         write(IOWSTD,'(/,T30,A,I2)') ' Dimensions of Grid #',i
         if( iparnt(i) .EQ. 0 ) then
            write(IOWSTD,'(A,T20,A)') 'Parent grid',': COARSE GRID'
         else
            write(IOWSTD,'(A,T20,A,I2)') 'Parent grid',': #',iparnt(i)
         endif
         write(IOWSTD,'(A,T20,A,I2)') 'Meshing factor',': ',mshfac(i)
         write(IOWSTD,'(A,T20,A,2(I5,A))',ERR=9999) 'Number of cells',
     &                                 ': (',nxgrid(i),',',nygrid(i),')'
         write(IOWSTD,'(A,T20,A,2(F15.2,A))',ERR=9999) 'Grid origin',
     &                                 ': (',grxorg(i),',',gryorg(i),')'
         write(IOWSTD,'(A,T20,A,2(F15.5,A))',ERR=9999) 'Cell width',
     &                                 ': (',grdelx(i),',',grdely(i),')'
         write(IOWSTD,'(A,T20,A,2(I5,A))',ERR=9999) 
     &           'Coarse grid offset',': (',iclbeg(i),',',jclbeg(i),')'
         write(IOWSTD,'(A,T20,A,I2)',ERR=9999) 'Number of layers',': ',
     &                                                        nlgrid(i)
         if(nxgrid(i) .GT. MXCELL .OR. nygrid(i) .GT. MXCELL) goto 7003
         if(nlgrid(i) .GT. MXLAYR) goto 7004
         if(klayer .GT. nlgrid(i)) goto 7008
      else
         mshmax = -9
         do 30 i=1,ngrid
            mshmax = MAX( mshfac(i), mshmax )
   30    continue
         nmesh(0) = mshmax
         grdelx(0) = deltax / FLOAT( mshmax )
         grdely(0) = deltay / FLOAT( mshmax )
         if( MAX(nmesh(0)*nxcell,nmesh(0)*nycell) .GT. MXCELL ) 
     &      goto 7007
c
c   --- calculate the grid origin and cell widths ---
c
         do 40 i=1,ngrid
           nmesh(i) = mshmax / mshfac(i) 
           grdelx(i) = deltax / FLOAT( mshfac(i) )
           grdely(i) = deltay / FLOAT( mshfac(i) )
           grxorg(i) = FLOAT(iclbeg(i) - 1) * deltax + xorig - grdelx(i)
           gryorg(i) = FLOAT(jclbeg(i) - 1) * deltay + yorig - grdely(i)
c
c   --- echo some of the parameters ----
c
           write(IOWSTD,'(/,T30,A,I2)') ' Dimensions of Grid #',i
           if( iparnt(i) .EQ. 0 ) then
               write(IOWSTD,'(A,T20,A)') 'Parent grid',': COARSE GRID'
           else
               write(IOWSTD,'(A,T20,A,I2)') 'Parent grid',': #',
     &                                         iparnt(i)
           endif
           write(IOWSTD,'(A,T20,A,I2)') 'Meshing factor',': ',mshfac(i)
           write(IOWSTD,'(A,T20,A,2(I5,A))',ERR=9999) 'Number of cells',
     &                                 ': (',nxgrid(i),',',nygrid(i),')'
           write(IOWSTD,'(A,T20,A,2(F15.2,A))',ERR=9999) 'Grid origin',
     &                                 ': (',grxorg(i),',',gryorg(i),')'
           write(IOWSTD,'(A,T20,A,2(F15.5,A))',ERR=9999) 'Cell width',
     &                                 ': (',grdelx(i),',',grdely(i),')'
           write(IOWSTD,'(A,T20,A,2(I5,A))',ERR=9999) 
     &           'Coarse grid offset',': (',iclbeg(i),',',jclbeg(i),')'
           write(IOWSTD,'(A,T20,A,I2)',ERR=9999) 'Number of layers',
     &                                                 ': ',nlgrid(i)
           if( nxgrid(i) .GT. MXCELL .OR. nygrid(i) .GT. MXCELL ) 
     &           goto 7003
           if( nlgrid(i) .GT. MXLAYR ) goto 7004
           if(klayer .GT. nlgrid(i)) goto 7008
   40    continue 
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
      write(IOWSTD,'(2A,I5)',ERR=9999) 'ERROR: Reading header of ',
     &                        'fine grid average file at record: ',irec
      goto 9999
c
 7003 continue
      write(IOWSTD,'(2A,I5,A,I5,A)',ERR=9999) 'ERROR: Number of ',
     &       'cells in fine grid average file exceeds max: (',
     &                                     nxgrid(i),',',nygrid(i),')'
      goto 9999
c
 7004 continue
      write(IOWSTD,'(2A,I5)',ERR=9999) 'ERROR: Number of ',
     &                  'layers in average file exceeds max: ',nygrid(i)
      goto 9999
c
 7005 continue
      write(IOWSTD,'(2A,I5)',ERR=9999) 'ERROR:  Number of species in ',
     &      'fine grid file not consistent with coarse grid: ',nspc
      goto 9999
c
 7006 continue
      write(IOWSTD,'(2A)',ERR=9999) 'ERROR:  Species list in fine ', 
     &                  'grid file does not match coarse grid file. '
      goto 9999
c
 7007 continue
      write(IOWSTD,'(2A,I5,A,I5,A)',ERR=9999) 'ERROR: Number of ',
     &       'cells needed for new resolution exceeds max: (',
     &                         nmesh(0)*nxcell,',',nmesh(0)*nycell,')'
      goto 9999
c
 7008 continue
      write(IOWSTD,'(2A,I5)',ERR=9999) 'ERROR: Layer requested in ',
     &                  'not in average file'
      goto 9999
c
 7009 continue
      write(IOWSTD,'(2A,I5)',ERR=9999) 'ERROR: Insufficient number ',
     &           'of input ZP files; must be',ngrid+1
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

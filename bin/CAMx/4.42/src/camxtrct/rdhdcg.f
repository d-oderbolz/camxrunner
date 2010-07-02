c**** RDHDCG.F
c
      subroutine rdhdcg(ierr)          
c
c-----------------------------------------------------------------------
c
c   This routine reads the header of the coarse grid average file
c   and stores the data in common to be used by the CAMXTRCT program.
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
      integer*4    filnam(10), fileid(60), noseg, idatb, idate, irec
      integer*4    ispec(10,MXSPEC), i, j
      real*4       xref, yref
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c  --- initialize the error code ---
c
      ierr = IFAIL
      write(IOWSTD,'(/,T30,A)') 'File Parameters'
      rewind(IORCAV)
c
c  --- read the first record ---
c
      irec = 1
      read(IORCAV,ERR=7000) filnam, fileid, noseg, nspec, 
     &                                    idatb, timbeg, idate, timend
      idtbeg = idatb
      idtend = idate
      call bswap(fileid,60,0)
      write(cnote,'(60A1)') (fileid(i),i=1,60)
      call bswap(fileid,60,0)
      write(IOWSTD,'(A,T20,A,A)') 'File Id',':',cnote(:strlen(cnote))
      if( nspec .GT. MXSPEC ) goto 7001
c
c  --- read the domain definition record ---
c
      irec = irec + 1
      read(IORCAV,ERR=7000) xref, yref, iutmzn, xorig, yorig, 
     &                           deltax, deltay, nxcell, nycell, nlayer
      if( xorig .LT. 999999.0 .AND. yorig .LT. 999999.0 ) then
          write(IOWSTD,'(A,T20,A,F10.3,A,F10.3,A)',ERR=9999) 
     &                        'Domain origin ',': (',xorig,',',yorig,')'
      else
          write(IOWSTD,'(A,T20,A,F10.1,A,F10.1,A)',ERR=9999) 
     &                        'Domain origin ',': (',xorig,',',yorig,')'
      endif
      write(IOWSTD,'(A,T20,A,I5,A,I5,A)',ERR=9999) 'Number of cells',
     &                                       ': (',nxcell,',',nycell,')'
      write(IOWSTD,'(A,T20,A,I2)',ERR=9999) 'Number of layers',': ',
     &                                                           nlayer
      write(IOWSTD,'(A,T20,A,F10.3,A,F10.3,A)',ERR=9999) 'Cell width ',
     &                                       ': (',deltax,',',deltay,')'
      if( nxcell .GT. MXCELL .OR. nycell .GT. MXCELL ) goto 7002
      if( exttyp .EQ. EMISS .OR. nlayer .EQ. 0 ) nlayer = 1
      if( nlayer .GT. MXLAYR ) goto 7003
      if( avgtyp .EQ. COARSE .AND. klayer .GT. nlayer ) goto 7004
c
c   --- skip next record ---
c
      irec = irec + 1
      read(IORCAV,ERR=7000)
c
c   --- read the species names ---
c
      irec = irec + 1
      read(IORCAV,ERR=7000) ((ispec(i,j),i=1,10),j=1,nspec)
      call bswap(ispec,10,MXSPEC)
      write(IOWSTD,'(A,T20,A)',ERR=9999) 'Species in file',':'
      do 10 j=1,nspec
         write(spclst(j),'(10A1)') (ispec(i,j),i=1,10)
         write(IOWSTD,'(T20,A)',ERR=9999) spclst(j)(:strlen(spclst(j)))
   10 continue
      call bswap(ispec,10,MXSPEC)
c
c   --- if doing a PTSOURCE file, skip the next couple records ---
c
      if( exttyp .EQ. PTSRCE ) then
          read(IORCAV,ERR=7000)
          read(IORCAV,ERR=7000)
      endif
c
c   --- initialize the variables needed for the fine grid data ---
c
      nxgrid(0) = nxcell
      nygrid(0) = nycell
      nlgrid(0) = nlayer
      iclbeg(0) = 1
      jclbeg(0) = 1
      nmesh(0) = 1
      iparnt(0) = 0
      grxorg(0) = xorig
      gryorg(0) = yorig
      grdelx(0) = deltax
      grdely(0) = deltay
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
     &                                 'average file at record: ',irec
      goto 9999
c
 7001 continue
      write(IOWSTD,'(2A,I5)',ERR=9999) 'ERROR: Number of species in ',
     &                               'average file exceeds max: ',nspec
      goto 9999
c
 7002 continue
      write(IOWSTD,'(2A,I5,A,I5,A)',ERR=9999) 'ERROR: Number of ',
     &      'cells in average file exceeds max: (',nxcell,',',nycell,')'
      goto 9999
c
 7003 continue
      write(IOWSTD,'(2A,I5)',ERR=9999) 'ERROR: Number of ',
     &                  'layers in average file exceeds max: ',nlayer
      goto 9999
c
 7004 continue
      write(IOWSTD,'(2A,I5)',ERR=9999) 'ERROR: Layer requested is not ',
     &                                           'in the average file. '
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

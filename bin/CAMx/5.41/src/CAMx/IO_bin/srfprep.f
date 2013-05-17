      subroutine srfprep(igrd,ncol,nrow,fsurf,topo,lai,lrdlai)
      use filunit
      use bndary
      use camx_includes
c
c----CAMx v5.41 121109
c
c     SRFPREP reads the landuse files for all grids and initializes the
c     landuse field arrays.  Landuse is mapped to all nested grids that
c     are not supplied with a surface file
c                          
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c          
c     Modifications:
c        6/6/03       check input data for consistency
c        7/7/03       now skips everthing if file not supplied
c        4/27/06      Added read of optional topo field
c        7/5/07       Added read of optional LAI field
c
c     Input arguments:
c        igrd                grid index
c        ncol                number of columns
c        nrow                number of rows
c
c     Output arguments:
c        fsurf               fractional landuse field
c        topo                topographic elevation (m MSL, optional)
c        lai                 lead area index field (optional)
c        lrdlai              flag indicating if LAI was read
c
c     Routines Called:
c        JSTLFT
c        TOUPPER
c
c     Called by:
c        STARTUP
c
      integer irdlu,irdtopo,irdlai
      real fsurf(ncol,nrow,NLU),topo(ncol,nrow),lai(ncol,nrow)
      character*8 inrec
      logical lrdlai
c
c-----Entry point
c
      lrdlai = .false.
      irdlu = 0
      irdtopo = 0
      irdlai = 0
c
c-----Skip if file not provided
c
      iunit = isurf(igrd)
      if (iunit .LE. 0) then 
        do j = 1,nrow
          do i = 1,ncol
            topo(i,j) = 0.
            lai(i,j)  = 0.
          enddo
        enddo
        goto 9999
      endif
c
c-----Attempt to read new data format
c
 99   read(iunit,end=100) inrec
      call jstlft(inrec)
      call toupper(inrec)
      if (inrec(1:5) .EQ. 'LUCAT') then
        if (.not.ldry) then
          read(inrec(6:7),'(i2)') nlu
          if (nlu .NE. 11 .or. nlu .NE. 26) then
            write(iout,'(/,2A)') 'Number of landuse categories on',
     &                           ' landuse file is not 11 or 26.'
            write(iout,'(A)') 'Found: ',nlu
            call camxerr()
          endif
        endif
        if ((inrec(6:7) .EQ. '11' .and. nlu.eq.11).or.
     &      (inrec(6:7) .EQ. '26' .and. nlu.eq.26)) then
          read(iunit) (((fsurf(i,j,l),i=1,ncol),j=1,nrow),l=1,nlu)
          irdlu = 1
          write(iout,'(a39,a21,i3,a)')
     &         'Read LU field','grid',igrd,' new format'
          goto 99
        else
          write(iout,'(/,2A)') 'Number of landuse categories on',
     &                         ' landuse file does not equal number'
          write(iout,'(2A)')   'of categories for the chosen dry',
     &                         ' deposition scheme:'
          write(iout,'(2A)') 'Number in landuse file:          ',
     &                       inrec(6:7)
          write(iout,'(A,i3)') 'Number in dry deposition scheme: ',nlu
          write(iout,'(2A)') 'Choose a different deposition option',
     &                     ' or generate a new landuse file with'
          write(iout,'(A)') 'the correct number of landuse categories.'
          call camxerr()
        endif
      elseif (inrec .EQ. 'TOPO') then
        read(iunit) ((topo(i,j),i=1,ncol),j=1,nrow)
        irdtopo = 1
        write(iout,'(a39,a21,i3,a)')
     &         'Read TOPO field','grid',igrd,' new format'
        goto 99
      elseif (inrec .EQ. 'LAI') then
        read(iunit) ((lai(i,j),i=1,ncol),j=1,nrow)
        irdlai = 1
        lrdlai = .true.
        write(iout,'(a39,a21,i3,a)')
     &         'Read LAI field','grid',igrd,' new format'
        goto 99
      else
c
c-----Must be old format
c
        if (.not.ldry) nlu = 11
        if (nlu.ne.11) then
          write(iout,'(/,A)') '*** Reading OLD landuse format! ***'
          write(iout,'(/,2A)') 'Number of landuse categories on',
     &                         ' landuse file is assumed to be 11'
          write(iout,'(A,i3)') 'Number in dry deposition scheme: ',nlu
          write(iout,'(2A)') 'Choose a different deposition option',
     &                     ' or generate a new landuse file format with'
          write(iout,'(A)') 'the correct number of landuse categories.'
          call camxerr()
        endif

        rewind(iunit)
        read(iunit) (((fsurf(i,j,l),i=1,ncol),j=1,nrow),l=1,nlu)
        irdlu = 1
        write(iout,'(a39,a21,i3,a)')
     &         'Read LU field','grid',igrd,' old format'
        read(iunit,end=100,err=100) ((topo(i,j),i=1,ncol),j=1,nrow)
        irdtopo = 1
        write(iout,'(a39,a21,i3,a)')
     &         'Read TOPO field ','grid',igrd,' old format'
      endif
c
c-----Check which optional data fields were read
c
 100  if (irdtopo .EQ. 0) then
        do j = 1,nrow
          do i = 1,ncol
            topo(i,j) = 0.
          enddo
        enddo
      endif
      if (irdlai .EQ. 0) then
        do j = 1,nrow
          do i = 1,ncol
            lai(i,j) = 0.
          enddo
        enddo
      endif
c
c-----Check that the LU data are reasonable and adjust out
c     minor inconsistencies
c
 101  continue
      do 10 j = 2,nrow-1
         i1 = 2
         i2 = ncol - 1
         if (igrd .eq. 1) then
           if (ibeg(j) .EQ. -999) goto 10
           i1 = ibeg(j)
           i2 = iend(j)
         endif
         do i = i1,i2
           areatot = 0.0
           do l = 1,nlu
             areatot = areatot + fsurf(i,j,l)
           enddo
           if (areatot.lt.0.95 .or. areatot.gt.1.05) goto 900
           do l = 1,nlu
             fsurf(i,j,l) = fsurf(i,j,l)/areatot
           enddo
         enddo
 10   continue
      return
c
c-----Error in landuse fractions
c
 900  write(iout,'(//,A)') 'ERROR in SRFPREP'
      write(iout,'(/,2A)') 'Sum of landuse fractions differs from 1.0 ',
     &                     'by more than 5%'
      write(iout,'(A,i3,a,2i4)') 
     &       'Grid = ', igrd, '  Cell(i,j) = ', i, j
      write(iout,'(A)') 'Table of input landuse data follows:'
      write(iout,'(/,A)') ' Class    Fraction'
      write(iout,'(i6,F10.3)') (l,fsurf(i,j,l),l=1,nlu)
      write(iout,'(A6,F10.3)') 'Total', areatot
      write(iout,'(/,A)') 'Check your input landuse data file'
      call camxerr()
c
 9999 continue
      return
      end

      subroutine o3colprep
      use filunit
      use grid
      use o3colmap
      implicit none
c
c----CAMx v6.00 130506
c
c     O3COLPREP reads the header records of the ozone column file,
c     and any optional constant codes
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Modifications:
c        06/13/03   Added optional snow cover, land-ocean, drought, 
c                   and roughness maps; only snow cover is time-varying
c        02/11/11   Moved snow panel to 2D met file, removed roughness,
c                   removed albedo panel (now determined in SRFPREP)
c        03/30/12   Removed haze and drought stress codes
c
c     Input arguments:
c        none
c
c     Output arguments:
c        none
c
c     Routines called:
c        none
c
c     Called by:
c        STARTUP
c
      include 'camx.prm'
      include 'flags.inc'
c
      character*10  title, name, optconst
      character*80  action
      character*180 line
      real          rdozn(NOZN)
      integer       nx, ny
      integer       i, j, idd, iff, nhdro3col
c
      data name    /'OZONE COL '/
      data optconst/'OCEAN     '/
c
c-----Entry point
c
      read(io3col,*)
      nhdro3col = 1
c
c-----Read ozone class and check inputs
c
      action = 'looking for ozone column header'
      read(io3col,'(a10,5f10.0)',err=900) title,(rdozn(i),i=1,NOZN)
      nhdro3col = nhdro3col + 1
      if (title.ne.name) then
        write(iout,'(//,a)') 'ERROR in O3COLPREP:'
        write(iout,*) 'After reading line: ',nhdro3col
        write(iout,*) 'Expecting keyword: ',name
        write(iout,*) 'Read from ozone column file: ',title
        write(iout,'(/,2A)') 'This version uses a different format for ',
     &                             'what was the Albedo/Haze/Ozone file.'
        write(iout,'(/,2A,/,A)') 'For more information refer to the User Guide ',
     &                         'and Release Notes included with ',
     &                          'this CAMx distribution.'
        call camxerr()
      endif
      if (lchem) then
        do i = 1,NOZN
          if (rdozn(i).ne.ozcl(i)) then
            write(iout,'(//,a)') 'ERROR in O3COLPREP:'
            write(iout,*) 'After reading line: ',nhdro3col
            write(iout,*) 'Mismatch in ozone class'
            write(iout,*) 'Photolysis rates  file: ',(ozcl(j),j=1,NOZN)
            write(iout,*) 'Ozone column file: ',(rdozn(j),j=1,NOZN)
            write(iout,'(/,2A)') 'This version uses a different format for ',
     &                             'what was the Albedo/Haze/Ozone file.'
            write(iout,'(/,2A,/,A)') 'For more information refer to the User Guide ',
     &                         'and Release Notes included with ',
     &                          'this CAMx distribution.'
            call camxerr()
          endif
        enddo
      endif
c
c-----Read any optional records
c
      action = 'looking for headers for any optional constant inputs'
      read(io3col,'(a)',err=900) line
      nhdro3col = nhdro3col + 1
      read(line,'(a10)') title
      if (title.eq.name) then
        backspace(io3col)
        nhdro3col = nhdro3col - 1
        goto 51
      endif
      if (title.ne.optconst) then
        write(iout,'(//,a)') 'ERROR in O3COLPREP:'
        write(iout,*) 'Cannot matchup variable: ',title
        write(iout,*) 'with any variable name.'
        write(iout,*) 'Review your ozone column file.'
        write(iout,*) 'Acceptable names are:'
        write(iout,*) 'OZONE COL'
        write(iout,*) 'OCEAN'
        write(iout,'(/,2A)') 'This version uses a different format for ',
     &                             'what was the Albedo/Haze/Ozone file.'
        write(iout,'(/,2A,/,A)') 'For more information refer to the User Guide ',
     &                         'and Release Notes included with ',
     &                          'this CAMx distribution.'
        call camxerr()
      endif
c
c-----Read optional constant index (ocean mask)
c
      action = 'reading master grid ocean data'
      read(line,'(a10,3i10)',err=901) title,idd,nx,ny
      if (nx.ne.ncol(1) .or. ny.ne.nrow(1)) then
         write(iout,'(//,2a)') 'ERROR in O3COLPREP reading: ', title
         write(iout,*) 'After reading line: ',nhdro3col
         write(iout,*) 'Mismatch in number of master grid cells'
         write(iout,'(A,2I5)') 'Cells in ozone column file: ',nx,ny
         write(iout,'(A,2I5)') 'Cells in master grid: ',ncol(1),nrow(1)
         write(iout,'(/,2A)') 'This version uses a different format for ',
     &                             'what was the Albedo/Haze/Ozone file.'
         write(iout,'(/,2A,/,A)') 'For more information refer to the User Guide ',
     &                         'and Release Notes included with ',
     &                          'this CAMx distribution.'
         call camxerr()
      endif
      do j = ny,1,-1
        read(io3col,'(9999i1)',err=900) (icdocn(i+(j-1)*nx),i=1,nx)
        nhdro3col = nhdro3col + 1
      enddo
      lrdocn(1) = .TRUE.
c
 50   action = 'reading nested grid ocean data'
      read(io3col,'(a)',err=900) line
      read(line,'(a10,3i10)',err=901) title,iff,nx,ny
      nhdro3col = nhdro3col + 1
      if (title.ne.optconst) then
        write(iout,'(//,a)') 'ERROR in O3COLPREP:'
        write(iout,*) 'After reading line: ',nhdro3col
        write(iout,*) 'Unexpected keyword: ', title
        write(iout,*) 'Read from ozone column file '
        write(iout,*) 'When looking for:',optconst
        write(iout,'(/,2A)') 'This version uses a different format for ',
     &                             'what was the Albedo/Haze/Ozone file.'
        write(iout,'(/,2A,/,A)') 'For more information refer to the User Guide ',
     &                         'and Release Notes included with ',
     &                          'this CAMx distribution.'
        call camxerr()
      endif
      if (iff.eq.0) goto 51
      if (iff.gt.ngrid) then
        write(iout,'(//,2a)') 'ERROR in O3COLPREP reading: ', title
        write(iout,*) 'After reading line: ',nhdro3col
        write(iout,*) 'Codes are given for more grids'
        write(iout,*) 'than have been specified in the control file'
        write(iout,*) 'Grid # read from ozone column file: ',iff
        write(iout,*) 'Total # of grids from control file: ',ngrid
        write(iout,'(/,2A)') 'This version uses a different format for ',
     &                             'what was the Albedo/Haze/Ozone file.'
        write(iout,'(/,2A,/,A)') 'For more information refer to the User Guide ',
     &                         'and Release Notes included with ',
     &                          'this CAMx distribution.'
        call camxerr()
      endif
      if (nx.ne.ncol(iff) .or. ny.ne.nrow(iff)) then
        write(iout,'(//,2a)') 'ERROR in O3COLPREP reading: ', title
        write(iout,*) 'After reading line: ',nhdro3col
        write(iout,'(A,I5)') 
     &            'Mismatch in number of fine cells for grid: ',iff
        write(iout,'(A,2I5)') 'Cells in ozone column file: ',nx,ny
        write(iout,'(A,2I5)') 'Cells in grid: ',ncol(iff),nrow(iff)
        write(iout,'(/,2A)') 'This version uses a different format for ',
     &                             'what was the Albedo/Haze/Ozone file.'
        write(iout,'(/,2A,/,A)') 'For more information refer to the User Guide ',
     &                         'and Release Notes included with ',
     &                          'this CAMx distribution.'
        call camxerr()
      endif
      do j = ny,1,-1
        read(io3col,'(9999i1)',err=900) 
     &                 (icdocn(iptr2d(iff)-1+i+(j-1)*nx), i=1,nx)
        nhdro3col = nhdro3col + 1
      enddo
      lrdocn(iff) = .TRUE.
      goto 50
c
c --- Summarize what information was read
c
 51   return
c
c --- Trap read errors
c
 900  write(iout,'(//,a,i4)') 'ERROR in O3COLPREP reading line ',
     &                                                     nhdro3col+1
      write(iout,'(2a)') 'While ', action
      write(iout,'(/,2A)') 'This version uses a different format for ',
     &                             'what was the Albedo/Haze/Ozone file.'
      write(iout,'(/,2A,/,A)') 'For more information refer to the User Guide ',
     &                         'and Release Notes included with ',
     &                          'this CAMx distribution.'
      call camxerr()
c
c --- For case of reading from character array line
c
 901  write(iout,'(//,a,i4)') 'ERROR in O3COLPREP reading line ',nhdro3col
      write(iout,'(2a)') 'Line read was:',line
      write(iout,'(2a)') 'While ', action
      write(iout,'(/,2A)') 'This version uses a different format for ',
     &                             'what was the Albedo/Haze/Ozone file.'
      write(iout,'(/,2A,/,A)') 'For more information refer to the User Guide ',
     &                         'and Release Notes included with ',
     &                          'this CAMx distribution.'
      call camxerr()
c
      end

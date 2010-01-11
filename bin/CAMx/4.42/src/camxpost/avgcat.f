      program avgcat
c
c-----Concatenate several average files together
c
      parameter(mx=300,my=300,ms=30,md=20)
      character*80 iopath
      character*4 ifile(10),note(60),mspec(10,ms),ispec(10,ms)
      dimension conc(mx,my),id1(md),id2(md),t1(md),t2(md)

      read(*,'(20x,a)') iopath
      open(9,file=iopath,form='unformatted')
      write(*,*) 'Opened output file: ',iopath
      read (*,'(20x,i5)') nday
      write(*,*) 'Processing ',nday,' input files'
      if (nday.gt.md) then
        write(*,*)'Number of days exceeds internal dimension'
        write(*,*)'Increase MD and recompile'
        stop
      endif
c
c-----Loop over number of input files and read header info
c
      iunit = 9
      do n = 1,nday
        iunit = iunit + 1
        read(*,'(20x,a)') iopath
        open(iunit,file=iopath,form='unformatted')
        write(*,*) 'Opened input file: ',iopath

        read(iunit) ifile,note,nseg,nspecs,id1(n),t1(n),id2(n),t2(n)
        read(iunit) orgx,orgy,izone,utmx,utmy,deltax,deltay,nx,ny,
     &              nz,nzlowr,nzuppr,htsur,htlow,htupp
        read(iunit) ix,iy,nxcll,nycll
        read(iunit) ((mspec(i,j),i=1,10),j=1,nspecs)
c
        if (t2(n).eq.0.) then
          t2(n) = 24.
          id2(n) = id2(n) - 1
        endif
      enddo
c
c-----Write header of output file
c
      write(9) ifile,note,nseg,nspecs,id1(1),t1(1),id2(nday),t2(nday)
      write(*,*)
      write(*,*) 'Output file header info:'
      write(*,'(10a1)') ifile
      write(*,'(60a1)') note
      write(*,*) 'NSEG:       ',nseg
      write(*,*) 'NSPEC:      ',nspecs
      write(*,*) 'Start date: ',id1(1)
      write(*,*) 'Start hour: ',t1(1)
      write(*,*) 'End date:   ',id2(nday)
      write(*,*) 'End hour:   ',t2(nday)
      if (nspecs.gt.ms) then
        write(*,*)'Number of species exceeds internal dimensions'
        write(*,*)'Increase MS and recompile'
        stop
      endif

      write(9) orgx,orgy,izone,utmx,utmy,deltax,deltay,nx,ny,
     &         nz,nzlowr,nzuppr,htsur,htlow,htupp
      write(*,*) 'X0:         ',orgx
      write(*,*) 'Y0:         ',orgy
      write(*,*) 'IZONE:      ',izone
      write(*,*) 'XORG:       ',utmx
      write(*,*) 'YORG:       ',utmy
      write(*,*) 'DELTAX:     ',deltax
      write(*,*) 'DELTAY:     ',deltay
      write(*,*) 'NX:         ',nx
      write(*,*) 'NY:         ',ny
      write(*,*) 'NZ:         ',nz
      if (nx.gt.mx .or. ny.gt.my) then
        write(*,*)'Grid dimensions exceed internal dimensions'
        write(*,*)'Increase MX and/or MY and recompile'
        stop
      endif

      write(9) ix,iy,nxcll,nycll
      write(*,*) 'IX:         ',ix
      write(*,*) 'IY:         ',iy
      write(*,*) 'NXCELL:     ',nxcll
      write(*,*) 'NYCELL:     ',nycll
      if (nx.ne.nxcll .or. ny.ne.nycll) then
        write(*,*)'Inconsistent grid dimensions in AVG file'
        stop
      endif

      write (9) ((mspec(i,j),i=1,10),j=1,nspecs)
      write(*,*) 'Species:'
      do j = 1,nspecs
        write(*,'(13x,10a1)') (mspec(i,j),i=1,10)
      enddo
c
c-----Loop over hourly data in each input file
c
      iunit = 9
      do 1000 n = 1,nday
        iunit = iunit + 1
 1001   read(iunit,end=1000) ibgdat,begtim,iendat,endtim
        if (endtim.eq.0.) then
          endtim = 24.
          iendat = iendat - 1
        endif
        write(9) ibgdat,begtim,iendat,endtim
        write(*,*) 'Process date/time: ',ibgdat,begtim,iendat,endtim
c
c-----Loop on species - read and write
c
        do l = 1,nspecs
          read(iunit) iseg,(ispec(m,l),m = 1,10),
     &                ((conc(i,j),i=1,nx),j=1,ny)
          do m = 1,10
            if (ispec(m,l).ne.mspec(m,l)) then
              write(*,*)'Species order mismatch'
              write(*,'(10a1,5x,10a1)') 
     &             (ispec(mm,l),mm=1,10),(mspec(mm,l),mm=1,10)
              stop
            endif
          enddo
          write(9) iseg,(ispec(m,l),m = 1,10),
     &             ((conc(i,j),i=1,nx),j=1,ny)
        enddo
        goto 1001
 1000 continue
      stop
      end

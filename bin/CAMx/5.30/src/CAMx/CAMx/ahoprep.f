      subroutine ahoprep
      use filunit
      use grid
      use ahomap
      implicit none
c
c----CAMx v5.30 101223
c
c     AHOPREP reads the header records of the albedo/haze/ozone file,
c     reads the time-invariant albedo codes, and any optional constant codes
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c     Modifications:
c        06/13/03   Added optional snow cover, land-ocean, drought, 
c                   and roughness maps; only snow cover is time-varying
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
      integer NCONST
      integer NVARY
c
      parameter (NCONST = 3)
      parameter (NVARY  = 1)
c
      character*10  title, name(3), optconst(NCONST), optvary(NVARY)
      character*80  action
      character*180 line
      real          rdalb(NALB), rdhaz(NHAZE), rdozn(NOZN)
      integer       noptcon, noptvar, nxalb, nyalb, irdopt, nx, ny
      integer       i, j, idd, iff
      logical       lerror
c
      data name    /'ALBEDO    ','HAZE      ','OZONE COL '/
      data optconst/'OCEAN     ','DROUGHT   ','ROUGHNESS '/
      data optvary /'SNOW      '/
c
c-----Entry point
c
      lrdsno = .FALSE.
      lerror = .FALSE.
      read(iaho,*)
      nhdraho = 1
c
c-----Read albedo class and check inputs
c
      action = 'looking for albedo header'
      read(iaho,'(a10,5f10.0)',err=900) title,(rdalb(i),i=1,NALB)
      nhdraho = nhdraho + 1
      if (title.ne.name(1)) then
        write(iout,'(//,a)') 'ERROR in AHOPREP:'
        write(iout,*) 'After reading line: ',nhdraho
        write(iout,*) 'Expecting keyword: ',name(1)
        write(iout,*) 'Read from Albedo/haze/ozone file: ',title
        call camxerr()
      endif
      if (lchem) then
        do i = 1,NALB
          if (rdalb(i).ne.albcl(i)) lerror = .TRUE.
        enddo
      endif
      if (lerror) then
        write(iout,'(//,a)') 'ERROR in AHOPREP:'
        write(iout,*) 'After reading line: ',nhdraho
        write(iout,*) 'Mismatch in albedo'
        write(iout,*) 'Photolysis rates  file: ',(albcl(i),i=1,NALB)
        write(iout,*) 'Albedo/haze/ozone file: ',(rdalb(i),i=1,NALB)
        call camxerr()
      endif
c
c-----Read haze class and check inputs
c
      action = 'looking for haze header'
      read(iaho,'(a10,3f10.0)',err=900) title,(rdhaz(i),i=1,NHAZE)
      nhdraho = nhdraho + 1
      if (title.ne.name(2)) then
        write(iout,'(//,a)') 'ERROR in AHOPREP:'
        write(iout,*) 'After reading line: ',nhdraho
        write(iout,*) 'Expecting keyword: ',name(2)
        write(iout,*) 'Read from Albedo/haze/ozone file: ',title
        call camxerr()
      endif
      if (lchem) then
        do i = 1,NHAZE
          if (rdhaz(i).ne.hazcl(i)) lerror = .TRUE.
        enddo
      endif
      if (lerror) then
        write(iout,'(//,a)') 'ERROR in AHOPREP:'
        write(iout,*) 'After reading line: ',nhdraho
        write(iout,*) 'Mismatch in haze class'
        write(iout,*) 'Photolysis rates  file: ',(hazcl(i),i=1,NHAZE)
        write(iout,*) 'Albedo/haze/ozone file: ',(rdhaz(i),i=1,NHAZE)
        call camxerr()
      endif
c
c-----Read ozone class and check inputs
c
      action = 'looking for ozone column header'
      read(iaho,'(a10,5f10.0)',err=900) title,(rdozn(i),i=1,NOZN)
      nhdraho = nhdraho + 1
      if (title.ne.name(3)) then
        write(iout,'(//,a)') 'ERROR in AHOPREP:'
        write(iout,*) 'After reading line: ',nhdraho
        write(iout,*) 'Expecting keyword: ',name(3)
        write(iout,*) 'Read from Albedo/haze/ozone file: ',title
        call camxerr()
      endif
      if (lchem) then
        do i = 1,NOZN
          if (rdozn(i).ne.ozcl(i)) lerror = .TRUE.
        enddo
      endif
      if (lerror) then
        write(iout,'(//,a)') 'ERROR in AHOPREP:'
        write(iout,*) 'After reading line: ',nhdraho
        write(iout,*) 'Mismatch in ozone class'
        write(iout,*) 'Photolysis rates  file: ',(ozcl(i),i=1,NOZN)
        write(iout,*) 'Albedo/haze/ozone file: ',(rdozn(i),i=1,NOZN)
        call camxerr()
      endif
c
c-----Read any optional classes and count them
c
      noptcon = 0
      noptvar = 0
      action = 'looking for headers for any optional constant inputs'
 50   read(iaho,'(a)',err=900) line
      nhdraho = nhdraho + 1
      read(line,'(a10)') title
      do i = 1, NCONST
        if (title.eq.optconst(i)) then
          noptcon = noptcon + 1
          if (title.eq.optconst(3)) 
     &       read(line,'(10x,9f10.0)',err=901) (ruflen(j),j=1,NRUF)
          goto 50
        endif
      enddo
      do i = 1, NVARY
        if (title.eq.optvary(i)) then
          noptvar = noptvar + 1
          if (title.eq.optvary(1)) lrdsno = .TRUE.
          goto 50
        endif
      enddo
c
c-----Read coarse grid albedo index
c
      action = 'looking for coarse grid albedo'
      read(line,'(a10,3i10)',err=901) title,idd,nxalb,nyalb
      if (title.ne.name(1)) then
        write(iout,'(//,a)') 'ERROR in AHOPREP:'
        write(iout,*) 'After reading line: ',nhdraho
        write(iout,*) 'Expecting keyword: ',name(1)
        write(iout,*) 'Read from Albedo/haze/ozone file: ',title
        call camxerr()
      endif
      if (nxalb.ne.ncol(1) .or. nyalb.ne.nrow(1)) then
        write(iout,'(//,a)') 'ERROR in AHOPREP:'
        write(iout,*) 'After reading line: ',nhdraho
        write(iout,*) 'Mismatch in number of master grid cells'
        write(iout,'(A,2I5)') 'Cells in AHO file   : ',nxalb,nyalb
        write(iout,'(A,2I5)') 'Cells in master grid: ',ncol(1),nrow(1)
        call camxerr()
      endif
      action = 'reading coarse grid albedo'
      do j = nyalb,1,-1
        read(iaho,'(9999i1)',err=900) (icdalb(i+(j-1)*nxalb),i=1,nxalb)
        nhdraho = nhdraho + 1
      enddo
      lrdalb(1) = .TRUE.
c 
c-----Read fine grid albedo index (if any)
c              
      action = 'looking for fine grid albedo'
 100  read(iaho,'(a10,3i10)',err=900) title,iff,nxalb,nyalb 
      nhdraho = nhdraho + 1
      if (title.ne.name(1)) then 
        write(iout,'(//,a)') 'ERROR in AHOPREP:'
        write(iout,*) 'After reading line: ',nhdraho
        write(iout,*) 'Expecting keyword: ',name(1)
        write(iout,*) 'Read from Albedo/haze/ozone file: ',title
        call camxerr()
      endif 
      if (iff.eq.0) goto 110
      if (iff.gt.ngrid) then
        write(iout,'(//,a)') 'ERROR in AHOPREP:'
        write(iout,*) 'After reading line: ',nhdraho
        write(iout,*) 'Albedo codes are given for more grids'
        write(iout,*) 'than have been specified in the control file'
        write(iout,*) 'Grid # read from AHO file: ',iff
        write(iout,*) 'Total # of grids from control file: ',ngrid
        call camxerr()
      endif
      if (nxalb.ne.ncol(iff) .or. nyalb.ne.nrow(iff)) then 
        write(iout,'(//,a)') 'ERROR in AHOPREP:'
        write(iout,*) 'After reading line: ',nhdraho
        write(iout,'(A,I5)') 
     &              'Mismatch in number of fine cells for grid: ',iff
        write(iout,'(A,2I5)') 'Cells in AHO file: ',nxalb,nyalb
        write(iout,'(A,2I5)') 'Cells in the grid: ',ncol(iff),nrow(iff)
        call camxerr()
      endif 
      action = 'reading fine grid albedo'
      do j = nyalb,1,-1   
        read(iaho,'(9999i1)',err=900) 
     &         (icdalb(iptr2d(iff)-1+i+(j-1)*nxalb), i=1,nxalb)
        nhdraho = nhdraho + 1
      enddo 
      lrdalb(iff) = .TRUE.
      goto 100
 110  write(iout,'(11x,a)')'Read albedo/haze/ozone file - ALBEDO'
c
c-----Read any optional constant index (ocean, drought, roughness)
c
      if (noptcon.gt.0) then
      do irdopt = 1,noptcon
        action = 'reading coarse grid optional constant data type'
        read(iaho,'(a10,3i10)',err=900) title,idd,nx,ny
        nhdraho = nhdraho + 1
        lerror = .TRUE.
        do i = 1, NCONST
          if (title.eq.optconst(i)) lerror = .FALSE.
        enddo
        if (lerror) then
          write(iout,'(//,a)') 'ERROR in AHOPREP:'
        write(iout,*) 'After reading line: ',nhdraho
          write(iout,*) 'Unexpected keyword: ', title
          write(iout,*) 'Read from Albedo/haze/ozone file '
          write(iout,*) 'When looking for one of:',
     &                   (optconst(i),i=1,NCONST)
          call camxerr()
        endif
        if (nx.ne.ncol(1) .or. ny.ne.nrow(1)) then
          write(iout,'(//,2a)') 'ERROR in AHOPREP reading: ', title
          write(iout,*) 'After reading line: ',nhdraho
          write(iout,*) 'Mismatch in number of master grid cells'
          write(iout,'(A,2I5)') 'Cells in AHO file   : ',nxalb,nyalb
          write(iout,'(A,2I5)') 'Cells in master grid: ',ncol(1),nrow(1)
          call camxerr()
        endif
        if (title.eq.optconst(1)) then
          action = 'reading coarse grid ocean data'
          do j = ny,1,-1
            read(iaho,'(9999i1)',err=900) (icdocn(i+(j-1)*nx),i=1,nx)
            nhdraho = nhdraho + 1
          enddo
          lrdocn(1) = .TRUE.
        elseif (title.eq.optconst(2)) then
          action = 'reading coarse grid drought data'
          do j = ny,1,-1
            read(iaho,'(9999i1)',err=900) (icddrt(i+(j-1)*nx),i=1,nx)
            nhdraho = nhdraho + 1
          enddo
          lrddrt(1) = .TRUE.
        elseif (title.eq.optconst(3)) then
          action = 'reading coarse grid roughness data'
          do j = ny,1,-1
            read(iaho,'(9999i1)',err=900) (icdruf(i+(j-1)*nx),i=1,nx)
            nhdraho = nhdraho + 1
          enddo
          lrdruf(1) = .TRUE.
        endif
c
c-----Read fine grids, if exist
c
 200    action = 'reading fine grid optional constant data type'
        read(iaho,'(a10,3i10)',err=900) title,iff,nx,ny
        nhdraho = nhdraho + 1
        lerror = .TRUE.
        do i = 1, NCONST
          if (title.eq.optconst(i)) lerror = .FALSE.
        enddo
        if (lerror) then
          write(iout,'(//,a)') 'ERROR in AHOPREP:'
        write(iout,*) 'After reading line: ',nhdraho
          write(iout,*) 'Unexpected keyword: ', title
          write(iout,*) 'Read from Albedo/haze/ozone file '
          write(iout,*) 'When looking for one of:',
     &                                   (optconst(i),i=1,NCONST)
          call camxerr()
        endif
        if (iff.eq.0) goto 210
        if (iff.gt.ngrid) then
          write(iout,'(//,2a)') 'ERROR in AHOPREP reading: ', title
        write(iout,*) 'After reading line: ',nhdraho
          write(iout,*) 'Codes are given for more grids'
          write(iout,*) 'than have been specified in the control file'
          write(iout,*) 'Grid # read from AHO file: ',iff
          write(iout,*) 'Total # of grids from control file: ',ngrid
          call camxerr()
        endif
        if (nx.ne.ncol(iff) .or. ny.ne.nrow(iff)) then
          write(iout,'(//,2a)') 'ERROR in AHOPREP reading: ', title
          write(iout,*) 'After reading line: ',nhdraho
          write(iout,'(A,I5)') 
     &               'Mismatch in number of fine cells for grid: ',iff
          write(iout,'(A,2I5)') 'Cells in AHO file: ',nxalb,nyalb
          write(iout,'(A,2I5)') 'Cells in the grid: ',ncol(iff),
     &                                                      nrow(iff)
          call camxerr()
        endif
        if (title.eq.optconst(1)) then
          do j = ny,1,-1
            action = 'reading fine grid ocean data'
            read(iaho,'(9999i1)',err=900) 
     &                  (icdocn(iptr2d(iff)-1+i+(j-1)*nx), i=1,nx)
            nhdraho = nhdraho + 1
          enddo
          lrdocn(iff) = .TRUE.
        elseif (title.eq.optconst(2)) then
          do j = ny,1,-1
            action = 'reading fine grid drought data'
            read(iaho,'(9999i1)',err=900) 
     &                  (icddrt(iptr2d(iff)-1+i+(j-1)*nx), i=1,nx)
            nhdraho = nhdraho + 1
          enddo
          lrddrt(iff) = .TRUE.
        elseif (title.eq.optconst(3)) then
          do j = ny,1,-1
            action = 'reading fine grid roughness data'
            read(iaho,'(9999i1)',err=900) 
     &                  (icdruf(iptr2d(iff)-1+i+(j-1)*nx), i=1,nx)
            nhdraho = nhdraho + 1
          enddo
          lrdruf(iff) = .TRUE.
        endif
        goto 200
 210    write(iout,'(11x,2a)')'Read albedo/haze/ozone file - ', title
      enddo
      endif
c
c --- Check validity of roughness classes, if input
c
      if (lrdruf(1)) then
        do i = 1,NRUF
          if (ruflen(i).lt.0.000001 .or. ruflen(i).gt.20.) then
            write(iout,'(//,a)') 'ERROR in AHOPREP:'
            write(iout,*) 'After reading line: ',nhdraho
            write(iout,*) 'An input roughness length is not reasonable'
            write(iout,*)
     &                  'Roughness must between 0.000001 and 20 meters'
            write(iout,*) 'You specified the following 9 classes'
            write(iout,*) 'Class    Length (m)'
            write(iout,'(i6,f10.4)') (j,ruflen(j),j=1,NRUF)
            call camxerr()
          endif
        enddo
      endif
c
c --- Summarize what information was read
c
      write(idiag,'(/,a)') 'Completed reading time-invariant AHO data'
      write(idiag,'(/,a)') 'The information read by grid was:'
      write(idiag,'(/,a44)') 'Grid Index (1 = Master Grid)'
      write(idiag,'(a15,99i3)') '             ', (i,i=1,ngrid)
      write(idiag,'(a15,99l3)') 'Albedo    (M)', (lrdalb(i),i=1,ngrid)
      write(idiag,'(a15,99l3)') 'Ocean     (O)', (lrdocn(i),i=1,ngrid)
      write(idiag,'(a15,99l3)') 'Drought   (O)', (lrddrt(i),i=1,ngrid)
      write(idiag,'(a15,99l3)') 'Roughness (O)', (lrdruf(i),i=1,ngrid)
      write(idiag,'(/,a)') '  M means mandatory input'
      write(idiag,'(a)')   '  O means optional input'
      write(idiag,'(a)')   '  T means input data were read'
      write(idiag,'(a)')   '  F means input data were not read'
      write(idiag,'(a,l3,//)') ' The flag for SNOW input is ', lrdsno
c
      return
c
c --- Trap read errors
c
 900  write(iout,'(//,a,i4)') 'ERROR in AHOPREP reading line ',
     &                                                     nhdraho+1
      write(iout,'(2a)') 'While ', action
      call camxerr()
c
c --- For case of reading from character array line
c
 901  write(iout,'(//,a,i4)') 'ERROR in AHOPREP reading line ', nhdraho
      write(iout,'(2a)') 'Line read was:',line
      write(iout,'(2a)') 'While ', action
      call camxerr()
c
      end

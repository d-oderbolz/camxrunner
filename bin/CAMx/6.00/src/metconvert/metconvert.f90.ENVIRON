      program metconvert

!-----Converts CAMx v4.3-v5.3 met input files to v6 CAMx format.
!
!     Met files can be processed for all inputs, or for individual v6 files
!     (2-D surface, 2/3-D met, Kv, and clouds)
!     This program does not alter any grid parameters or data fields, it
!     simply reformats the data to the new file structure.
!
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
! Copyright (C) 2011  ENVIRON
!
! This program is free software; you can redistribute it and/or
! modify it under the terms of the GNU General Public License
! as published by the Free Software Founhrion; either version 2
! of the License, or (at your option) any later version.
!
! This program is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!
      implicit none

      integer,parameter :: MXLU = 26
      
      character*200 filname,filname1,filname2,filname3,filname4
      character*40 project
      character*20 cldhdr,cldver43,cldver53
      character*8 lucat,inrec
      character*4 fname(10)
      character*4 nam3d(60),nam2d(60),namkv(60),namcld(60),namsrf(60)
      character*4 namvar(10,MXLU+2)
      character*4 lunam(10,MXLU)
      character*4 toponam(10),lainam(10)
      character*4 met3dnam(10,6),met2dnam(10,2)
      character*4 kv3dnam(10,1),cld3dnam(10,5)
 
      integer ione,izero,itzon,n3dmet,n2dmet,n3dkv,n3dcld,n2dout
      integer o2dsrf,o2dmet,o3dmet,o3dkv,o3dcld
      integer ilu,izp,itp,iqa,iuv,ikv,icld,isno
      integer iutm,iproj,istag
      integer nlu,ntopo,nlai,n,nx,ny,nz,nvar,i,j,k,l
      integer idate,ibdate,iedate,id
      integer nxsno,nysno,ibsdat,iesdat

      integer lumap(11)
      integer,allocatable,dimension(:,:) :: isnow

      real zero,plon,plat,tlat1,tlat2,xorg,yorg,dx,dy
      real hhmm,mm,hh,btime,etime,hr
      real bstim,estim

      logical lexist
      integer istrln

      real,allocatable,dimension(:,:,:) :: zz,pp,tt,qq,uu,vv,kv
      real,allocatable,dimension(:,:,:) :: cldwtr,ranwtr,snowtr,gplwtr,cldod
      real,allocatable,dimension(:,:,:) :: fsurf,fsrf11
      real,allocatable,dimension(:,:)   :: topo,lai,ts,snow
      real,allocatable,dimension(:)     :: tau

      logical lstagger,l2dsrf,l3dmet,l3dkv,l3dcld,lsnow
     
      data fname    /'A','V','E','R','A','G','E',' ',' ',' '/
      data nam3d    /'3','D','M','E','T',55*' '/
      data nam2d    /'2','D','M','E','T',55*' '/
      data namkv    /'K','V','M','E','T',55*' '/
      data namcld   /'C','L','D','M','E','T',54*' '/
      data namsrf   /'S','U','R','F','A','C','E',53*' '/
      data lunam    /'W','A','T','E','R',' ',' ',' ',' ',' ', &
                     'I','C','E',' ',' ',' ',' ',' ',' ',' ', &
                     'L','A','K','E',' ',' ',' ',' ',' ',' ', &
                     'E','N','E','E','D','L',' ',' ',' ',' ', &
                     'E','B','R','O','A','D',' ',' ',' ',' ', &
                     'D','N','E','E','D','L',' ',' ',' ',' ', &
                     'D','B','R','O','A','D',' ',' ',' ',' ', &
                     'T','B','R','O','A','D',' ',' ',' ',' ', &
                     'D','D','E','C','I','D',' ',' ',' ',' ', &
                     'E','S','H','R','U','B',' ',' ',' ',' ', &
                     'D','S','H','R','U','B',' ',' ',' ',' ', &
                     'T','S','H','R','U','B',' ',' ',' ',' ', &
                     'S','G','R','A','S','S',' ',' ',' ',' ', &
                     'L','G','R','A','S','S',' ',' ',' ',' ', &
                     'C','R','O','P','S',' ',' ',' ',' ',' ', &
                     'R','I','C','E',' ',' ',' ',' ',' ',' ', &
                     'S','U','G','A','R',' ',' ',' ',' ',' ', &
                     'M','A','I','Z','E',' ',' ',' ',' ',' ', &
                     'C','O','T','T','O','N',' ',' ',' ',' ', &
                     'I','C','R','O','P','S',' ',' ',' ',' ', &
                     'U','R','B','A','N',' ',' ',' ',' ',' ', &
                     'T','U','N','D','R','A',' ',' ',' ',' ', &
                     'S','W','A','M','P',' ',' ',' ',' ',' ', &
                     'D','E','S','E','R','T',' ',' ',' ',' ', &
                     'M','W','O','O','D',' ',' ',' ',' ',' ', &
                     'T','F','O','R','E','S','T',' ',' ',' '/
      data toponam  /'T','O','P','O','_','M',' ',' ',' ',' '/
      data lainam   /'L','A','I',' ',' ',' ',' ',' ',' ',' '/
      data met3dnam /'Z','G','R','I','D','_','M',' ',' ',' ', &
                     'P','R','E','S','S','_','M','B',' ',' ', &
                     'T','E','M','P','_','K',' ',' ',' ',' ', &
                     'H','U','M','I','D','_','P','P','M',' ', &
                     'U','W','I','N','D','_','M','p','S',' ', &
                     'V','W','I','N','D','_','M','p','S',' '/
      data met2dnam /'T','S','U','R','F','_','K',' ',' ',' ', & 
                     'S','N','O','W','C','O','V','E','R',' '/
      data kv3dnam  /'K','V','_','M','2','p','S',' ',' ',' '/
      data cld3dnam /'C','L','O','D','W','_','G','p','M','3', &
                     'R','A','I','N','W','_','G','p','M','3', &
                     'S','N','O','W','W','_','G','p','M','3', &
                     'G','R','P','L','W','_','G','p','M','3', &
                     'C','L','O','U','D','O','D',' ',' ',' '/
      data cldver43 /'CAMx_V4.3 CLOUD_RAIN'/
      data cldver53 /'CAMx_V5.3 CLOUD_RAIN'/

      data lumap /21,15,13,7,4,25,1,24,23,14,24/

      data ione   /1/
      data izero  /0/
      data zero   /0./
      data n3dmet /6/
      data n2dmet /1/
      data n3dkv  /1/
      data n3dcld /5/
      data o2dsrf /10/
      data o2dmet /11/
      data o3dmet /12/
      data o3dkv  /13/
      data o3dcld /14/
      data ilu    /20/
      data izp    /21/
      data itp    /22/
      data iqa    /23/
      data iuv    /24/
      data ikv    /25/
      data icld   /26/
!
!-----Entry point
!
      plon  = 0.
      plat  = 0.
      iutm  = 0
      iproj = 99
      istag = 99
      tlat1 = 0.
      tlat2 = 0.
      nlu   = 0
      ntopo = 0
      nlai  = 0
      l2dsrf = .false.
      l3dmet = .false.
      l3dkv  = .false.
      l3dcld = .false.
      lsnow  = .false.
      n2dout = n2dmet
!
!-----Get grid and projection definition
!
      read(*,'(20x,a)') filname
      read(filname,*) itzon
      write(*,'(a,3i10)') 'Time zone                 : ',itzon

      read(*,'(20x,a)') filname
      read(filname,*) nx,ny,nz
      write(*,'(a,3i10)') 'Grid dimensions (nx,ny,nz): ',nx,ny,nz

      read(*,'(20x,a)') filname
      read(filname,*) xorg,yorg
      write(*,'(a,2f10.3)') 'Grid SW coordinates (xorg,yorg):',xorg,yorg
      xorg = 1000.*xorg
      yorg = 1000.*yorg

      read(*,'(20x,a)') filname
      read(filname,*) dx,dy
      write(*,'(a,2f10.3)') 'Grid spacing (dx,dy):',dx,dy
      dx = 1000.*dx
      dy = 1000.*dy

      read(*,'(20x,a)') project
      if (project.eq.'LATLON') then
        iproj = 0
        write(*,*) 'Processing LATLON'
      elseif (project(1:3).eq.'UTM') then
        iproj = 1
        write(*,*) 'Processing UTM'
        read(project(4:),*) iutm
        write(*,'(a,i10)') 'UTM zone:',iutm
      elseif (project(1:3).eq.'LCP') then
        iproj = 2
        write(*,*) 'Processing LCP'
        read(project(4:),*) plon,plat,tlat1,tlat2
        write(*,'(a,4f10.0)') 'LCP parameters (plon,plat,tlat1,tlat2):', &
                               plon,plat,tlat1,tlat2
      elseif (project(1:4).eq.'PSP') then
        iproj = 3
        write(*,*) 'Processing PSP'
        read(project(4:),*) plon,plat
        write(*,'(a,2f10.0)') 'PSP parameters (plon,plat):',plon,plat
      else
        write(*,*) 'Unrecognized map projection:'
        write(*,*) project
        write(*,*) 'Use: LATLON, UTM, LCP, PSP'
        stop
      endif
!
!-----Get which output files to process
!
      write(*,*)
      read(*,'(20x,a)') filname
      if (filname.ne.'') then
        open(o2dsrf,file=filname,form='unformatted')
        write(*,*) 'Opened output 2-D surface file: ',filname
        l2dsrf = .true.
      endif

      read(*,'(20x,a)') filname
      if (filname.ne.'') then
        open(o3dmet,file=filname,form='unformatted')
        write(*,*) 'Opened output 3-D met file: ',filname
        l3dmet = .true.
      endif
      read(*,'(20x,a)') filname
      if (l3dmet) then
        if (filname.eq.'') then
          write(*,*) 'Output 2-D met file is not specified'
          write(*,*) 'Both 3-D and 2-D met files must be provided'
          stop
        endif
        open(o2dmet,file=filname,form='unformatted')
        write(*,*) 'Opened output 2-D met file: ',filname
      endif

      read(*,'(20x,a)') filname
      if (filname.ne.'') then
        open(o3dkv,file=filname,form='unformatted')
        write(*,*) 'Opened output 3-D Kv file: ',filname
        l3dkv = .true.
      endif

      read(*,'(20x,a)') filname
      if (filname.ne.'') then
        open(o3dcld,file=filname,form='unformatted')
        write(*,*) 'Opened output 3-D cloud file: ',filname
        l3dcld = .true.
      endif
!
!-----Open input files based on the output files to process
!
      read(*,'(20x,a)') filname
      if (l2dsrf) then
        if (filname.eq.'') then
          write(*,*)'Input surface file is not specified'
          stop
        endif
        inquire(file=filname,exist=lexist)
        if( .NOT. lexist ) goto 902
        open(ilu,file=filname,err=900,form='unformatted')
        write(*,*) 'Opened input surface file: ',filname
      endif

      read(*,'(20x,a)') filname1
      read(*,'(20x,a)') filname2
      read(*,'(20x,a)') filname3
      read(*,'(20x,a)') filname4
      if (l3dmet) then
        if (filname1.eq.'') then
          write(*,*)'Input height/pressure file is not specified'
          stop
        endif
        filname = filname1
        inquire(file=filname,exist=lexist)
        if( .NOT. lexist ) goto 902
        open(izp,file=filname,err=900,form='unformatted')
        write(*,*) 'Opened input height/pressure file: ',filname1

        if (filname2.eq.'') then
          write(*,*)'Input wind file is not specified'
          stop
        endif
        filname = filname2
        inquire(file=filname,exist=lexist)
        if( .NOT. lexist ) goto 902
        open(iuv,file=filname,err=900,form='unformatted')
        write(*,*) 'Opened input wind file: ',filname2

        if (filname3.eq.'') then
          write(*,*)'Input temperature file is not specified'
          stop
        endif
        filname = filname3
        inquire(file=filname,exist=lexist)
        if( .NOT. lexist ) goto 902
        open(itp,file=filname,err=900,form='unformatted')
        write(*,*) 'Opened input temperature file: ',filname3

        if (filname4.eq.'') then
          write(*,*)'Input humidity file is not specified'
          stop
        endif
        filname = filname4
        inquire(file=filname,exist=lexist)
        if( .NOT. lexist ) goto 902
        open(iqa,file=filname,err=900,form='unformatted')
        write(*,*) 'Opened input humidity file: ',filname4
      endif

      read(*,'(20x,a)') filname
      if (l3dkv) then
        if (filname.eq.'') then
          write(*,*)'Input Kv file is not specified'
          stop
        endif
        inquire(file=filname,exist=lexist)
        if( .NOT. lexist ) goto 902
        open(ikv,file=filname,err=900,form='unformatted')
        write(*,*) 'Opened input Kv file: ',filname
      endif

      read(*,'(20x,a)') filname
      if (l3dcld) then
        if (filname.eq.'') then
          write(*,*)'Input cloud file is not specified'
          stop
        endif
        inquire(file=filname,exist=lexist)
        if( .NOT. lexist ) goto 902
        open(icld,file=filname,err=900,form='unformatted')
        write(*,*) 'Opened input cloud file: ',filname
      endif
!
!-----Process 2-D surface file
!
      if (l2dsrf) then
!
!-----Allocate arrays
!
        allocate(fsurf(nx,ny,MXLU))
        allocate(fsrf11(nx,ny,MXLU))
        allocate(topo(nx,ny))
        allocate(lai(nx,ny))
!
!-----Read original or v5.30 LU file formats
!
        write(*,*)
        write(*,*)'Creating 2-D surface file'
        nvar = 0
 100    read(ilu,end=101) inrec
        call jstlft(inrec)
        call toupper(inrec)
        if (inrec(1:5) .EQ. 'LUCAT') then
          if (inrec(6:7) .EQ. '11') then
            read(ilu) (((fsrf11(i,j,l),i=1,nx),j=1,ny),l=1,11)
             write(*,*) 'Read 11-cat LU from v5.30 file format'
            nlu = 11
          elseif (inrec(6:7) .EQ. '26') then
            read(ilu) (((fsurf(i,j,l),i=1,nx),j=1,ny),l=1,26)
          write(*,*) 'Read 26-cat LU from v5.30 file format'
            nlu = 26
          else
            write(*,*) 'Attempting to read v5.30 LU file format'
            write(*,*) 'Expecting 11 or 26 LU categories, but found: '
            write(*,*) inrec(6:7)
            stop
          endif
          goto 100
        elseif (inrec .EQ. 'TOPO') then
          read(ilu) ((topo(i,j),i=1,nx),j=1,ny)
          write(*,*) 'Read TOPO from v5.30 file format'
          ntopo = 1
          goto 100
        elseif (inrec .EQ. 'LAI') then
          read(ilu) ((lai(i,j),i=1,nx),j=1,ny)
          write(*,*) 'Read LAI from v5.30 file format'
          nlai = 1
          goto 100
        else
          rewind(ilu)
          read(ilu) (((fsrf11(i,j,l),i=1,nx),j=1,ny),l=1,11)
          write(*,*) 'Read 11-cat LU from pre-v5.30 file format'
          nlu = 11
          read(ilu,end=101,err=101) ((topo(i,j),i=1,nx),j=1,ny)
          write(*,*) 'Read TOPO from pre-v5.30 file format'
          ntopo = 1
        endif
 101    continue

        do n = 1,MXLU
          do i = 1,10
            namvar(i,n) = lunam(i,n)
          enddo
        enddo
        nvar = MXLU
        if (ntopo.eq.1) then
          nvar = nvar + 1
            do i = 1,10
              namvar(i,nvar) = toponam(i)
            enddo
        endif
        if (nlai.eq.1) then
          nvar = nvar + 1
            do i = 1,10
              namvar(i,nvar) = lainam(i)
            enddo
        endif
!
!-----Write file header: Date is assigned arbitrarily as these are static fields
!
        write(o2dsrf) fname,namsrf,ione,nvar,izero,zero,izero,zero
        write(o2dsrf) plon,plat,iutm,xorg,yorg,dx,dy,nx,ny,ione,iproj, &
                      izero,tlat1,tlat2,zero
        write(o2dsrf) ione,ione,nx,ny
        write(o2dsrf) ((namvar(i,n),i=1,10),n=1,nvar)
!
!-----Write data portion
!
        nvar = 0
        write(o2dsrf) izero,zero,izero,zero
        if (nlu.eq.11) then
          fsurf = 0.
          do n = 1,nlu
            do j = 1,ny
              do i = 1,nx
                fsurf(i,j,lumap(n)) = fsurf(i,j,lumap(n)) + fsrf11(i,j,n)
              enddo
            enddo
          enddo
        endif
        do n = 1,MXLU
          write(o2dsrf) ione,(namvar(i,n),i=1,10),((fsurf(i,j,n),i=1,nx),j=1,ny)
        enddo
        nvar = nlu
        if (ntopo.eq.1) then
          nvar = nvar + 1
          write(o2dsrf) ione,(namvar(i,nvar),i=1,10),((topo(i,j),i=1,nx),j=1,ny)
        endif
        if (nlai.eq.1) then
          nvar = nvar + 1
          write(o2dsrf) ione,(namvar(i,nvar),i=1,10),((lai(i,j),i=1,nx),j=1,ny)
        endif

        close(o2dsrf)
        close(ilu)
        write(*,*)'Finished creating 2-D surface file'
      endif
!
!-----Process 3-D met files
!
      if (l3dmet) then
!
!-----Allocate arrays
!
        allocate(zz(nx,ny,nz))
        allocate(pp(nx,ny,nz))
        allocate(tt(nx,ny,nz))
        allocate(qq(nx,ny,nz))
        allocate(uu(nx,ny,nz))
        allocate(vv(nx,ny,nz))
        allocate(ts(nx,ny))
        allocate(isnow(nx,ny))
        allocate(snow(nx,ny))
!
!-----Read through Z/P file to find date/time range
!
        write(*,*)
        write(*,*)'Creating 2-D and 3-D met files'
        n = 0
 200    do k = 1,nz
          read(izp,end=201) hhmm,idate
          read(izp,end=201) hhmm,idate
        enddo
        n = n + 1
        if (n.eq.1) then
          ibdate = idate
          mm = amod(hhmm,100.)
          hh = int(hhmm/100.)
          btime = anint(1000.*(hh + mm/60.))/1000.
          write(*,*) 'Input starting date/hour:',ibdate,btime
        endif
        goto 200
 201    continue
        iedate = idate
        mm = amod(hhmm,100.)
        hh = int(hhmm/100.)
        etime = anint(1000.*(hh + mm/60.))/1000.
        write(*,*) 'Input ending date/hour  :',iedate,etime
        rewind(izp)
!
!-----Read wind file to get wind stagger flag
!
        read(iuv,err=901) hhmm,idate,lstagger
        istag = 0
        if (lstagger) istag = 1
        rewind(iuv)
!
!-----Is there a snow file to process?
!
        read(*,'(20x,a)') filname
        if (filname.ne.'') then
          lsnow = .true.
          inquire(file=filname,exist=lexist)
          if( .NOT. lexist ) goto 902
          open(isno,file=filname,err=900)
          write(*,*)
          write(*,*) 'Opened input snow file: ',filname
          read(isno,'(20x,2i10)') nxsno,nysno
          if (nxsno.ne.nx .or. nysno.ne.ny) then
            write(*,*)'Snow file NX/NY does not match grid dimensions'
            write(*,*)'Snow file: ',nxsno,nysno
            write(*,*)'Program  : ',nx,ny
            stop
          endif
          n2dout = n2dmet + 1
          read(isno,'(10x,2(i10,f10.0))') ibsdat,bstim,iesdat,estim
          if (ibsdat.lt.ibdate .or. iesdat.gt.iedate) then
            write(*,*)'Snow date range is outside of met file range:'
            write(*,*)'Snow date range: ',ibsdat,iesdat
            write(*,*)'Met file range : ',ibdate,iedate
            stop
          endif
          if ((ibsdat.eq.ibdate .and. bstim.lt.btime) .or. &
              (iesdat.eq.iedate .and. estim.gt.etime)) then
            write(*,*)'Snow time range is outside of met file range:'
            write(*,*)'Snow time range: ',ibsdat,bstim,iesdat,estim
            write(*,*)'Met file range : ',ibdate,btime,iedate,etime
            stop
          endif
          do j = ny,1,-1
            read(isno,'(9999i1)') (isnow(i,j),i=1,nx)
            do i = 1,nx
              snow(i,j) = float(isnow(i,j))
            enddo
          enddo
        endif
!
!-----Write file headers
!
        write(o3dmet) fname,nam3d,itzon,n3dmet,ibdate,btime,iedate,etime
        write(o2dmet) fname,nam2d,itzon,n2dout,ibdate,btime,iedate,etime
        write(o3dmet) plon,plat,iutm,xorg,yorg,dx,dy,nx,ny,nz,iproj, &
                      istag,tlat1,tlat2,zero
        write(o2dmet) plon,plat,iutm,xorg,yorg,dx,dy,nx,ny,ione,iproj, &
                      istag,tlat1,tlat2,zero
        write(o3dmet) ione,ione,nx,ny
        write(o3dmet) ((met3dnam(i,n),i=1,10),n=1,n3dmet)
        write(o2dmet) ione,ione,nx,ny
        write(o2dmet) ((met2dnam(i,n),i=1,10),n=1,n2dout)
!
!-----Loop over hours and read gridded data
!
 300    do k = 1,nz
          read(izp,end=301) hhmm,idate,((zz(i,j,k),i=1,nx),j=1,ny)
          read(izp,end=301) hhmm,idate,((pp(i,j,k),i=1,nx),j=1,ny)
        enddo
        write(*,*) 'Read ZP file'

        read(iuv) hr,id
        do k = 1,nz
          read(iuv) ((uu(i,j,k),i=1,nx),j=1,ny)
          read(iuv) ((vv(i,j,k),i=1,nx),j=1,ny)
        enddo
        read(iuv)
        write(*,*) 'Read UV file'

        read(itp) hr,id,((ts(i,j),i=1,nx),j=1,ny)
        do k = 1,nz
          read(itp) hr,id,((tt(i,j,k),i=1,nx),j=1,ny)
        enddo
        write(*,*) 'Read TT file'

        do k = 1,nz
          read(iqa) hr,id,((qq(i,j,k),i=1,nx),j=1,ny)
        enddo
        write(*,*) 'Read QA file'
!
!-----Write data portion
!
        mm = amod(hhmm,100.)
        hh = int(hhmm/100.)
        etime = anint(1000.*(hh + mm/60.))/1000.
        write(*,*) 'Processing date/hour:',idate,etime

        write(o2dmet) idate,etime,idate,etime
        write(o2dmet) ione,(met2dnam(i,1),i=1,10),((ts(i,j),i=1,nx),j=1,ny)
        if (lsnow) then
          write(o2dmet) ione,(met2dnam(i,2),i=1,10),((snow(i,j),i=1,nx),j=1,ny)
        endif

        write(o3dmet) idate,etime,idate,etime
        do k = 1,nz
          write(o3dmet) ione,(met3dnam(i,1),i=1,10),((zz(i,j,k),i=1,nx),j=1,ny)
        enddo
        do k = 1,nz
          write(o3dmet) ione,(met3dnam(i,2),i=1,10),((pp(i,j,k),i=1,nx),j=1,ny)
        enddo
        do k = 1,nz
          write(o3dmet) ione,(met3dnam(i,3),i=1,10),((tt(i,j,k),i=1,nx),j=1,ny)
        enddo
        do k = 1,nz
          write(o3dmet) ione,(met3dnam(i,4),i=1,10),((qq(i,j,k),i=1,nx),j=1,ny)
        enddo
        do k = 1,nz
          write(o3dmet) ione,(met3dnam(i,5),i=1,10),((uu(i,j,k),i=1,nx),j=1,ny)
        enddo
        do k = 1,nz
          write(o3dmet) ione,(met3dnam(i,6),i=1,10),((vv(i,j,k),i=1,nx),j=1,ny)
        enddo

        goto 300
 301    continue
        close(o2dmet)
        close(o3dmet)
        close(izp)
        close(iuv)
        close(itp)
        close(iqa)
        write(*,*)'Finished creating 2-D and 3-D met files'
      endif
!
!-----Process 3-D Kv file
!
      if (l3dkv) then
!
!-----Allocate arrays
!
        allocate(kv(nx,ny,nz))
!
!-----Read through Kv file to find date/time range
!
        write(*,*)
        write(*,*)'Creating 3-D Kv file'
        n = 0
 400    do k = 1,nz
          read(ikv,end=401) hhmm,idate
        enddo
        n = n + 1
        if (n.eq.1) then
          ibdate = idate
          mm = amod(hhmm,100.)
          hh = int(hhmm/100.)
          btime = anint(1000.*(hh + mm/60.))/1000.
          write(*,*) 'Input starting date/hour:',ibdate,btime
        endif
        goto 400
 401    continue
        iedate = idate
        mm = amod(hhmm,100.)
        hh = int(hhmm/100.)
        etime = anint(1000.*(hh + mm/60.))/1000.
        write(*,*) 'Input ending date/hour  :',iedate,etime
        rewind(ikv)
!
!-----Write file header
!
        write(o3dkv) fname,namkv,itzon,n3dkv,ibdate,btime,iedate,etime
        write(o3dkv) plon,plat,iutm,xorg,yorg,dx,dy,nx,ny,nz,iproj, &
                     izero,tlat1,tlat2,zero
        write(o3dkv) ione,ione,nx,ny
        write(o3dkv) ((kv3dnam(i,n),i=1,10),n=1,n3dkv)
!       
!-----Loop over hours and read gridded data
! 
 500    do k = 1,nz
          read(ikv,end=501) hhmm,idate,((kv(i,j,k),i=1,nx),j=1,ny)
        enddo
        write(*,*) 'Read KV file'
!
!-----Write data portion
!
        mm = amod(hhmm,100.)
        hh = int(hhmm/100.)
        etime = anint(1000.*(hh + mm/60.))/1000.
        write(*,*) 'Processing date/hour:',idate,etime

        write(o3dkv) idate,etime,idate,etime
        do k = 1,nz
          write(o3dkv) ione,(kv3dnam(i,1),i=1,10),((kv(i,j,k),i=1,nx),j=1,ny)
        enddo

        goto 500
 501    continue
        close(o3dkv)
        close(ikv)
        write(*,*)'Finished creating 3-D Kv file'
      endif
!
!-----Process 3-D cloud file
!
      if (l3dcld) then
!
!-----Allocate arrays
!
        allocate(cldwtr(nx,ny,nz))
        allocate(ranwtr(nx,ny,nz))
        allocate(snowtr(nx,ny,nz))
        allocate(gplwtr(nx,ny,nz))
        allocate(cldod(nx,ny,nz))
        allocate(tau(nz))
!
!-----Read through cloud file to find date/time range
!
        write(*,*)
        write(*,*)'Creating 3-D cloud file'
        n = 0
        read(icld) cldhdr
        if (cldhdr.ne.cldver43 .and. cldhdr.ne.cldver53) then
          write(*,*)'Cloud file format is not recognized'
          write(*,*)'Cannot translate this file'
          stop
        endif
 600    read(icld,end=601) hhmm,idate 
        do k = 1,nz
          read(icld)
          read(icld)
          read(icld)
          read(icld)
          read(icld)
        enddo
        n = n + 1
        if (n.eq.1) then
          ibdate = idate
          mm = amod(hhmm,100.)
          hh = int(hhmm/100.)
          btime = anint(1000.*(hh + mm/60.))/1000.
          write(*,*) 'Input starting date/hour:',ibdate,btime
        endif
        goto 600
 601    continue
        iedate = idate
        mm = amod(hhmm,100.)
        hh = int(hhmm/100.)
        etime = anint(1000.*(hh + mm/60.))/1000.
        write(*,*) 'Input ending date/hour  :',iedate,etime
        rewind(icld)
!
!-----Write file header
!
        write(o3dcld) fname,namcld,itzon,n3dcld,ibdate,btime,iedate,etime
        write(o3dcld) plon,plat,iutm,xorg,yorg,dx,dy,nx,ny,nz,iproj, &
                      izero,tlat1,tlat2,zero
        write(o3dcld) ione,ione,nx,ny
        write(o3dcld) ((cld3dnam(i,n),i=1,10),n=1,n3dcld)
!
!-----Loop over hours and read gridded data
!
        read(icld)
 700    read(icld,end=701) hhmm,idate 
        do k = 1,nz
          read(icld) ((cldwtr(i,j,k),i=1,nx),j=1,ny)
          read(icld) ((ranwtr(i,j,k),i=1,nx),j=1,ny)
          read(icld) ((snowtr(i,j,k),i=1,nx),j=1,ny)
          read(icld) ((gplwtr(i,j,k),i=1,nx),j=1,ny)
          read(icld) ((cldod(i,j,k),i=1,nx),j=1,ny)
        enddo
        write(*,*) 'Read CR file'
!
!-----In the case of a v4.3 cloud/rain file, convert cloud optical depth
!     to layer-specific optical depth equivalent to v5.3
!
        if (cldhdr.eq.cldver43) then
          do j = 1,ny
            do i = 1,nx
              tau(1) = 0.
              do k = 2,nz
                tau(k) = cldod(i,j,k-1) - cldod(i,j,k)
              enddo
              do k = 1,nz
                cldod(i,j,k) = tau(k)
              enddo
            enddo
          enddo
        endif
!
!-----Write data portion
!
        mm = amod(hhmm,100.)
        hh = int(hhmm/100.)
        etime = anint(1000.*(hh + mm/60.))/1000.
        write(*,*) 'Processing date/hour:',idate,etime

        write(o3dcld) idate,etime,idate,etime
        do k = 1,nz
          write(o3dcld) ione,(cld3dnam(i,1),i=1,10), &
                        ((cldwtr(i,j,k),i=1,nx),j=1,ny)
        enddo
        do k = 1,nz
          write(o3dcld) ione,(cld3dnam(i,2),i=1,10), &
                        ((ranwtr(i,j,k),i=1,nx),j=1,ny)
        enddo
        do k = 1,nz
          write(o3dcld) ione,(cld3dnam(i,3),i=1,10), &
                        ((snowtr(i,j,k),i=1,nx),j=1,ny)
        enddo
        do k = 1,nz
          write(o3dcld) ione,(cld3dnam(i,4),i=1,10), &
                        ((gplwtr(i,j,k),i=1,nx),j=1,ny)
        enddo
        do k = 1,nz
          write(o3dcld) ione,(cld3dnam(i,5),i=1,10), &
                        ((cldod(i,j,k),i=1,nx),j=1,ny)
        enddo

        goto 700
 701    continue
        close(o3dcld)
        close(icld)
        write(*,*)'Finished creating 3-D cloud file'
      endif
      stop
!
 900  write(*,*) 'Could not open input file: ',filname(:istrln(filname))
      stop
 901  write(*,*) 'Error reading stagger flag on input wind file header'
      write(*,*) 'This might be a pre-v4.3 wind file'
      stop
 902  write(*,*) 'Input file not found: ',filname(:istrln(filname))
      end

!=======================================================================

      subroutine jstlft( string )
!
!     Copyright 2013
!     ENVIRON International Corporation
!
!-----------------------------------------------------------------------
!
!   Description:
!     Left justifies a string
!
!   Arguments:
!     Inputs/Outputs: (the string arguments serves as both input and
!                      output)
!       string   C   string to left justify
!
!-----------------------------------------------------------------------
!   LOG:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!   Argument declaration:
!-----------------------------------------------------------------------
!
      character*(*) string
!
!-----------------------------------------------------------------------
!   Local variables:
!-----------------------------------------------------------------------
!
      integer ibeg, i
!
!-----------------------------------------------------------------------
!   Entry point:
!-----------------------------------------------------------------------
!
!   ---- it may already be left-justified ---
!
      if( string(1:1) .NE. ' ' ) goto 9999
!
!   ---- find the first non-blank character ---
!
      do 10 i=1,LEN( string )
         if( string(i:i) .NE. ' ' ) then
             ibeg = i
             goto 111
         endif
   10 continue
!
!   --- no non-blanks found, it's a blank string, nothing to do ----
!
      goto 9999
!
!   ---- move the string over, 2 char at a time ---
!
  111 continue
      do 20 i=1,LEN( string )-ibeg+1
         string(i:i) = string(i+ibeg-1:i+ibeg-1)
         string(i+ibeg-1:i+ibeg-1) = ' '
   20 continue
      goto 9999
!
!-----------------------------------------------------------------------
!   Return point:
!-----------------------------------------------------------------------
!
 9999 continue
      return
      end

!=======================================================================

      subroutine toupper(string)
!
!----CAMx v5.30 101223
!
!-----------------------------------------------------------------------
!    Description:
!-----------------------------------------------------------------------
!
!      Converts the string to upper case.
!
!     Copyright 2013
!     ENVIRON International Corporation
!
!       Argument description:
!         Inputs:
!           string  C  string to convert
!
!-----------------------------------------------------------------------
!    LOG:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Include files:
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
!    Argument declaration:
!-----------------------------------------------------------------------
!
      character*(*) string
!
!-----------------------------------------------------------------------
!    Local variables:
!-----------------------------------------------------------------------
!
      character*26 lower, upper
      integer idx, i
!
!-----------------------------------------------------------------------
!    Data statments:
!-----------------------------------------------------------------------
!
      data lower /'abcdefghijklmnopqrstuvwxyz'/
      data upper /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
!
!-----------------------------------------------------------------------
!    Entry point:
!-----------------------------------------------------------------------
!
      do i=1,LEN( string )
         idx = INDEX( lower, string(i:i) )
         if( idx .GT. 0 ) string(i:i) = upper(idx:idx)
      enddo
!
!-----------------------------------------------------------------------
!    Return point:
!-----------------------------------------------------------------------
!
      return
      end

!**** ISTRLN
!
!     Copyright 2013
!     ENVIRON International Corporation
!
      function istrln( string )
      integer   istrln
!
!-----------------------------------------------------------------------
!
!     This routine returns the non-blank length of a string.
!
!   Arguments:
!     Inputs:
!       string   C   string for determining length
!
!-----------------------------------------------------------------------
!   Argument declaration:
!-----------------------------------------------------------------------
!
      character*(*) string
!
!-----------------------------------------------------------------------
!   Local variables:
!-----------------------------------------------------------------------
!
      integer   i
!
!-----------------------------------------------------------------------
!   Entry point:
!-----------------------------------------------------------------------
!
!   ---- initialize length to zero ----
!
      istrln = 0
      do 10 i=LEN( string ),1,-1
         if( string(i:i) .NE. ' ' ) then
             istrln = i
             goto 9999
         endif
   10 continue
!
!-----------------------------------------------------------------------
!   Return point:
!-----------------------------------------------------------------------
!
 9999 continue
      return
      end

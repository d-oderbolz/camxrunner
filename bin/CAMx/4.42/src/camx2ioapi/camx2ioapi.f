      program camx2ioapi
      implicit none
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c   Copyright (C) 2003-2008  ENVIRON
c
c
c   This program is free software; you can redistribute it and/or
c   modify it under the terms of the GNU General Public License
c   as published by the Free Software Foundation; either version 2
c   of the License, or (at your option) any later version.
c
c   This program is distributed in the hope that it will be useful,
c   but WITHOUT ANY WARRANTY; without even the implied warranty of
c   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c   GNU General Public License for more details.
c
c   To obtain a copy of the GNU General Public License
c   write to the Free Software Foundation, Inc.,
c   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
c
c
c   For comments and questions, send to bkoo@environcorp.com
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c     CAMx2IOAPI converts CAMx outputs (avrg & depn) to I/O API format.
c     This program assumes hourly data and supports limited horizontal
c     and vertical coordinate systems (Lambert/UTM; non-h sigma-P).
c
c
c     HISTORY:
c       created by bwang@cert.ucr.edu
c       modified by tcao@cert.ucr.edu (07/15/2003)
c       modified by bkoo (02/19/2004)
c       modified by bkoo (06/27/2006)
c       modified by bkoo (08/16/2006) - added SOP
c       modified by bkoo (11/29/2007) - added option to extract surface layer only
c       modified by bkoo (01/29/2008) - added UTM support
c       name change to camx2ioapi (01/29/2008)
c
      include 'PARMS3.EXT'
      include 'IODECL3.EXT'
      include 'FDESC3.EXT'

      integer :: LOGUNIT
      integer :: JUNIT, in
      integer :: JDATE,JTIME
      logical :: ENVYN, LAYFLAG

      character(16), parameter :: OUTFILE = 'IOAPI_OUT'
      character(16), parameter :: PGNAME = 'CAMx2IOAPI'

      character(256) :: MESG



      character(4), dimension(10) :: name
      character(4), dimension(60) :: note
      integer :: nseg,nspec,ibdate,iedate,iutm,nx,ny,nz,
     &           nzlowr,nzuppr,ixseg,iyseg,nxseg,nyseg,iseg
      real :: btime,etime,refx,refy,xorg,yorg,
     &        delx,dely,htsur,htlow,htupp

      character(4), allocatable :: mspec(:,:)
      integer, allocatable :: idx(:)
      real, allocatable :: buff(:,:,:)
      integer :: istat

      character(10) :: tmpnam
      character(4) :: dtyp
      integer :: iyear,itzone,nstep

      integer, parameter :: npm = 16
      character(10), dimension(npm) :: pmnam
      data pmnam /'PSO4','PNO3','PNH4','POA' ,'PEC' ,'SOA' ,
     &            'FPRM','FCRS','CPRM','CCRS','CRST','PH2O',
     &            'NA'  ,'PCL' ,'HGP' ,'SOP' /

      integer :: i,j,k,l,m,n,nzo
c     
c     Initialize I/O-API
c
      LOGUNIT = INIT3()
c
c     Get LAYFLAG
c
      LAYFLAG = ENVYN('SURFACE_LAYER_ONLY','Output Surface Layer Only',
     &                                                   .false.,istat)
      if (istat.gt.0) then
        MESG = 'Bad value for SURFACE_LAYER_ONLY'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Input file name
c
      write(*,*) 'Enter name of CAMx file (AVRG or DEPN):'
      read(*,'(20x,a)') MESG
      write(*,*) TRIM(MESG)
      in = JUNIT()
      open(in,file=MESG,status='OLD',form='UNFORMATTED')
c
c     Read CAMx header
c
      read(in) name,note,nseg,nspec,ibdate,btime,iedate,etime
      read(in) refx,refy,iutm,xorg,yorg,delx,dely,nx,ny,nz,
     &         nzlowr,nzuppr,htsur,htlow,htupp
      read(in) ixseg,iyseg,nxseg,nyseg
c
c     Data type
c
      write(*,*) 'Enter data type (AVRG, DDEP, or WDEP):'
      read(*,'(20x,a)') dtyp
      write(*,*) dtyp
      if (dtyp.ne.'AVRG' .and. dtyp.ne.'DDEP' .and. dtyp.ne.'WDEP') then
        MESG = 'Invalid data type - ' // dtyp
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Input year
c
      write(*,*) 'Enter data year (YYYY):'
      read(*,'(20x,i)') iyear
      write(*,*) iyear
      if (JMOD(iyear,100).ne.ibdate/1000) then
        write(*,*) 'Input Start Date: ',ibdate
        MESG = 'Inconsistent year'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Input time zone
c
      write(*,*) 'Enter input time zone (e.g., 8 for PST):'
      read(*,'(20x,i)') itzone
      write(*,*) itzone
c
c     Map projection parameters
c
      write(*,*) 'Enter map projection parameters ' //
     &           '(P_ALP3D, P_BET3D, P_GAM3D, XCENT3D, YCENT3D):'
      read(*,'(20x,a)') MESG
      read(MESG,*) P_ALP3D,P_BET3D,P_GAM3D,XCENT3D,YCENT3D
      write(*,*) P_ALP3D,P_BET3D,P_GAM3D,XCENT3D,YCENT3D
      if (iutm.eq.0) then
        write(*,*) 'Lambert coordinate system: '
        write(*,*) '1st true latitude - ',P_ALP3D
        write(*,*) '2nd true latitude - ',P_BET3D
        write(*,*) 'central meridian  - ',P_GAM3D
        write(*,*) 'center longitude  - ',XCENT3D
        write(*,*) 'center latitude   - ',YCENT3D
      else
        write(*,*) 'UTM coordinate system: '
        if (iutm.ne.NINT(P_ALP3D)) then
          MESG = 'Input file UTM zone is inconsistent with user input'
          call M3EXIT(PGNAME,0,0,MESG,2)
        endif
        write(*,*) 'UTM zone     - ',iutm
        write(*,*) 'UTM X offset - ',XCENT3D
        write(*,*) 'UTM Y offset - ',YCENT3D
      endif
c
c     Vertical grid parameters - sigma levels
c
      nzo = nz
      if (LAYFLAG) nzo = 1
      write(*,*) 'Enter sigma levels'
      read(*,'(20x,a)') MESG
      read(MESG,*) (VGLVS3D(k),k=2,nzo+1)
      VGLVS3D(1) = 1.0
      do k = 1, nzo+1
        write(*,'(i3,2x,f)') k-1, VGLVS3D(k)
      enddo
c
c     Read species list
c
      allocate (mspec(10,nspec), stat = istat)
      if (istat.ne.0) then
        MESG = 'Memory allocation failed: MSPEC'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
      allocate (idx(nspec), stat = istat)
      if (istat.ne.0) then
        MESG = 'Memory allocation failed: IDX'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
      idx = 0
      read(in) ((mspec(n,l),n=1,10),l=1,nspec)
      NVARS3D = 0
      do l = 1, nspec
        write(tmpnam,'(10a1)') (mspec(n,l),n=1,10)
        if (dtyp.eq.'DDEP' .and. INDEX(tmpnam,'_DD').eq.0) cycle
        if (dtyp.eq.'WDEP' .and. INDEX(tmpnam,'_WD').eq.0) cycle
        NVARS3D = NVARS3D + 1
        idx(l) = NVARS3D
        VNAME3D(idx(l)) = tmpnam
        VDESC3D(idx(l)) = 'VARIABLE ' // TRIM(tmpnam)
        VTYPE3D(idx(l)) = M3REAL
        j = 0
        do i = 1, npm
          k = INDEX(pmnam(i),' ')
          if (pmnam(i)(:k-1).eq.tmpnam(:k-1)) j = 1 ! PM species
        enddo
        if (dtyp.eq.'AVRG') then
          UNITS3D(idx(l)) = 'ppmV'
          if (j.eq.1) UNITS3D(idx(l)) = 'micrograms/m**3' ! PM species
        else
          UNITS3D(idx(l)) = 'mol/hectare'
          if (j.eq.1) UNITS3D(idx(l)) = 'g/hectare' ! PM species
        endif
      enddo
      write(*,*) 'No. Name            Unit'
      do l = 1, NVARS3D
        write(*,'(i3,2x,2a16)') l, VNAME3D(l), UNITS3D(l)
      enddo
c
c     CMAQ haeder
c
      FTYPE3D = GRDDED3 ! Gridded
      FDESC3D(1) = 'I/O API formatted CAMx ' // TRIM(dtyp) // ' output'

      if (iutm.eq.0) then
        GDTYP3D = LAMGRD3 ! Lambert
      else
        GDTYP3D = UTMGRD3 ! UTM
      endif
      GDNAM3D = CMISS3  ! '????????????????'

      VGTYP3D = VGSGPN3 ! non-h sigma-P
      VGTOP3D = 10000.0 ! Pa

      NCOLS3D = nx
      NROWS3D = ny
      NLAYS3D = nz
      if (LAYFLAG) NLAYS3D = 1
      NTHIK3D = 1

      XORIG3D = xorg    ! [m]
      YORIG3D = yorg    ! [m]
         
      XCELL3D = delx    ! [m]
      YCELL3D = dely    ! [m]

      MXREC3D = (iedate-ibdate)*24 + NINT(etime-btime)
      TSTEP3D = 10000   ! Assume hourly data
      SDATE3D = iyear*1000 + JMOD(ibdate,1000)
      STIME3D = NINT( btime * 10000. )

      if (dtyp.ne.'AVRG') STIME3D = STIME3D + TSTEP3D ! Use end time of each interval
      call NEXTIME(SDATE3D, STIME3D, itzone*10000) ! Convert to GMT
c
c     Open output file
c
      if (.not.OPEN3(OUTFILE,FSNEW3,PGNAME)) then
        MESG = 'Cannot open ' // TRIM(OUTFILE)
        call M3EXIT(PGNAME,0,0,MESG,1)
      endif
c
c     Memory allocation
c
      allocate (buff(nx,ny,nz), stat = istat)
      if (istat.ne.0) then
        MESG = 'Memory allocation failed: BUFF'
        call M3EXIT(PGNAME,0,0,MESG,2)
      endif
c
c     Read/write hourly data
c
      JDATE = SDATE3D
      JTIME = STIME3D

      do m = 1, MXREC3D
        read(in,end=999) ibdate,btime,iedate,etime
        do l = 1, nspec
          do k = 1, nz
            read(in,end=999) iseg,(mspec(n,l),n=1,10),
     &                                  ((buff(i,j,k),i=1,nx),j=1,ny)
          enddo

          if (idx(l).gt.0) then
            if (LAYFLAG) then
              if (.not.WRITE3(OUTFILE,VNAME3D(idx(l)),JDATE,JTIME,
     &                                               buff(:,:,1))) then
                MESG = 'Cannot write data to ' // TRIM(OUTFILE)
                call M3EXIT(PGNAME,JDATE,JTIME,MESG,1)
              endif
            else
              if (.not.WRITE3(OUTFILE,VNAME3D(idx(l)),JDATE,JTIME,
     &                                               buff)) then
                MESG = 'Cannot write data to ' // TRIM(OUTFILE)
                call M3EXIT(PGNAME,JDATE,JTIME,MESG,1)
              endif
            endif
          endif

        enddo ! nspec
        call NEXTIME(JDATE, JTIME, TSTEP3D)
      enddo

      goto 1000

999   MESG = 'Premature end of CAMx file'
      call M3EXIT(PGNAME,JDATE,JTIME,MESG,1)

1000  continue
      close(in)

      MESG = 'Successful completion of ' // PGNAME
      call M3EXIT(PGNAME,0,0,MESG,0)

      end


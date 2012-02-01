      program vertavg
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copyright (C) 2011  ENVIRON
c
c This program is free software; you can redistribute it and/or
c modify it under the terms of the GNU General Public License
c as published by the Free Software Foundation; either version 2
c of the License, or (at your option) any later version.
c
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c
c To obtain a copy of the GNU General Public License
c write to the Free Software Foundation, Inc.,
c 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c This program reads in a CAMx 3D average file (*.avrg, *.favrg, *.cpa)
c and vertically averages grid cell values for each species on file
c over the mixing layer for each hour on file.  The mixing layer is
c determined by the input CAMx Kv file.  Density weighting is performed.
c The output file as in the binary coarse grid average format.


      real, allocatable ::  rkv(:,:,:)           ! kv 
      real, allocatable ::  zh(:,:,:)            ! ht
      real, allocatable ::  tp(:,:,:), tsfc(:,:) ! temp 
      real, allocatable ::  pr(:,:,:)            ! pressure
      integer,allocatable :: ipbl(:,:)           ! top layer in PBL
      integer,allocatable :: zpbl(:,:)           ! PBL depth

      real,allocatable :: conc(:,:,:,:),         ! conc(nx,ny,nz,nspec)
     +                    concavg(:,:,:)         ! (nx,ny,nspec+2)
      real,allocatable ::  dz(:),pres(:)         ! nz


      character*4, allocatable :: mspec(:,:)     ! (10,nspec+2)
      character*4 name(10),note(60)
      character*256 infile,outfile,line
      character*60 message
      character*10 cspec,namei,namez

      data ione /1/
      data rd   /287./
      data namei /'PBL_I     '/
      data namez /'PBL_Z     '/

c     READ IN INPUTS

      read (*,'(20x,a)')infile
      open (10,file=infile,status='old',form='unformatted')
      print *, 'open CAMx Kv file ', infile
      
      read (*,'(20x,a)')infile
      open (11,file=infile,status='old',form='unformatted')
      print *, 'open CAMx ZP file ', infile

      read (*,'(20x,a)')infile
      open (12,file=infile,status='old',form='unformatted')
      print *, 'open CAMx temperature file ', infile

      read (*,'(20x,a)')infile
      open (13,file=infile,status='old',form='unformatted')
      print *, 'open PA CPA file ', infile


      read (*,'(20x,a)')outfile
      open (20,file=outfile,status='unknown',form='unformatted')
      print *, 'open output file ', outfile


c     READ HEADERS OF CPA FILE
      read (13) name,note,ijunk,nspec,ibdate,btime,iedate,etime 
      read (13) rdum,rdum,iutm,xorg,yorg,dx,dy,nx,ny,
     +  nz,ncell1,ncell2,surfht,htmin1,htmin2
      read (13) ijunk,ijunk, nx,ny

      allocate (mspec(10,nspec+2), rkv(nx,ny,nz),zh(nx,ny,nz),
     +         tp(nx,ny,nz),tsfc(nx,ny),pr(nx,ny,nz),
     +         ipbl(nx,ny),zpbl(nx,ny),
     +         conc(nx,ny,nz,nspec),concavg(nx,ny,nspec+2),
     +         dz(nz),pres(nz))

      read (13) ((mspec(i,l),i=1,10),l=1,nspec)


c     PRINT DOMAIN DEFINITIONS
      print *, xorg,yorg
      print *, nx,ny,nz
      print *, dx, dy


c     ADD THE NAMES FOR PBL LAYER TOP AND DEPTH
      do i=1,10
        write(mspec(i,nspec+1),'(a1)') namei(i:i)
        write(mspec(i,nspec+2),'(a1)') namez(i:i)
      enddo

c     WRITE HEADER FOR NEW CPA FILE 
      write(20) name,note,ione,nspec+2,ibdate,btime,iedate,etime 
      write(20) rdum,rdum,iutm,xorg,yorg,dx,dy,nx,ny,ione,
     +  ncell1,ncell2,surfht,htmin1,htmin2
      write(20) ione,ione, nx,ny
      write(20) ((mspec(i,l),i=1,10),l=1,nspec+2)


c     LOOP FOR HOURLY DATA ---

      do ih = 1,24
12345   continue
c       READ IN MET VARIABLES FIRST
        read (12,end=130) hour3,idate3,((tsfc(i,j),i=1,nx),j=1,ny)
        do k=1,nz
          read (10,end=130) hour,idate,((rkv(i,j,k),i=1,nx),j=1,ny)
          read (11,end=130) hour1,idate1,((zh(i,j,k),i=1,nx),j=1,ny)
          read (11,end=130) hour2,idate2,((pr(i,j,k),i=1,nx),j=1,ny)
          read (12,end=130) hour3,idate3,((tp(i,j,k),i=1,nx),j=1,ny)
          
          if (hour1 .ne. hour .or. idate1 .ne. idate .or.
     +        hour2 .ne. hour .or. idate2 .ne. idate .or.
     +        hour3 .ne. hour .or. idate3 .ne. idate ) then
              print *, 'times do not match'
              stop
          endif

        enddo

       if (ibdate.gt.idate.or.(ibdate.eq.idate.and.btime.gt.hour)) then
           write(*,*) 'SKIPHR:',ibdate,idate,btime,hour
           goto 12345
       endif
       
c-----  PBL depth: calculate depth of mixing based on Kv profile
c
        do j = 1,ny
          do i = 1,nx
            kpbl = 1
            do k = 2,nz
              if (kpbl.ne.k-1) goto 100
              dz1 = zh(i,j,k-1)
              if (k.gt.2) dz1 = zh(i,j,k-1) - zh(i,j,k-2)
              dz2 = zh(i,j,k) - zh(i,j,k-1)
              zr = dz2/dz1
              critk = 0.03*dz1*dz1*(1. + zr)/200.
              if (rkv(i,j,k-1) .gt. critk) kpbl = k
            enddo
 100        zpbl(i,j) = zh(i,j,kpbl)
            ipbl(i,j) = kpbl
          enddo
        enddo

c       READ IN CONCS
        read (13,end=200) ibdate,btime,iedate,etime
        if (ibdate .ne. idate .or. nint(btime) .ne. nint(hour/100.)) 
     +  then
c          write(*,*) ibdate, idate, btime, nint(hour/100.), etime
          print *, 'Avrg/PA time  does not match met time'
          stop
        endif
        do isp=1,nspec
          do k=1,nz
            read (13) ione,(mspec(i,isp),i=1,10),
     +        ((conc(i,j,k,isp),i=1,nx),j=1,ny)
          enddo
        enddo


c       FOR EACH COLUMN, WEIGHT EACH GRID CELL IN THE MIXED LAYER
C       BY DENSITY AND CELL THICKNESS. 
c         sum(CONCi[PPB] * RHOi[KG/M3] * DX[M]* DY[M] * DZi[M]) = ppb*kg[air]
c         then divide by air mass in column:
c         sum(RHOi[KG/M3] * DX * DY * DZi) = kg[air]
c         note: dx and dy are not really needed
c         save PBL top layer index and height for output

        do j=1,ny
        do i=1,nx
          do k=1,ipbl(i,j)
            if (k .eq. 1) then
              dz(k) = zh(i,j,k)
              pres(k) = pr(i,j,k)
            else
              dz(k) = zh(i,j,k) - zh(i,j,k-1)
              pres(k) = 0.5*(pr(i,j,k)+pr(i,j,k-1))
            endif
          enddo

          do isp = 1,nspec
            sum = 0.
            rmass = 0.
            concavg(i,j,isp) = 0.0
            do k=1,ipbl(i,j)
              rho = pres(k)*100./rd/tp(i,j,k)
              sum = sum + conc(i,j,k,isp) * rho * dz(k)
              rmass = rmass + rho * dz(k)
            enddo
            concavg(i,j,isp) = sum/rmass
          enddo

          concavg(i,j,nspec+1) = ipbl(i,j)
          concavg(i,j,nspec+2) = zpbl(i,j)

        enddo
        enddo
            
c       WRITE VERTICALLY AVERAGED HOURLY CONCS/RATES
        write (20) ibdate,btime,iedate,etime
        do isp = 1,nspec+2
          write (20) ione,(mspec(i,isp),i=1,10),
     +      ((concavg(i,j,isp),i=1,nx),j=1,ny)
        enddo

        print *, 'finished processing on ', ibdate,btime,iedate,etime
      enddo
130   continue
200   continue
      
      stop
      end

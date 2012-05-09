      program diagnose_pbl
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copyright (C) 2011  ENVIRON
c
c $Id$
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
c and a kv file and determines the PBL height.
c The output file as in the binary 2D coarse grid average format.
c With one species (PBL_Z)


      real, allocatable ::  rkv(:,:,:)           ! kv 
      real, allocatable ::  zh(:,:,:)            ! ht
      integer,allocatable :: zpbl(:,:)           ! PBL depth
      integer :: nz

      real,allocatable ::  dz(:),pres(:)         ! nz


      character*4 name(10),note(60)
      character*256 infile,outfile,line
      character*60 message
      character*40 namez

      data ione /1/
      data namez /'P   B   L   _   Z                       '/

c     READ IN INPUTS

      read (*,'(20x,a)')infile
      open (10,file=infile,status='old',form='unformatted')
      print *, 'open CAMx Kv file ', infile

      read (*,'(20x,a)')infile
      open (11,file=infile,status='old',form='unformatted')
      print *, 'open CAMx ZP file ', infile

      read (*,'(20x,a)')infile
      open (13,file=infile,status='old',form='unformatted')
      print *, 'open average file ', infile


      read (*,'(20x,a)')outfile
      open (20,file=outfile,status='unknown',form='unformatted')
      print *, 'open output file ', outfile
      
      read (*,'(20x,i)')nz
      print *, 'Assuming nz', nz

c     READ HEADERS OF AVG FILE
      read (13) name,note,ijunk,nspec,ibdate,btime,iedate,etime 
      read (13) rdum,rdum,iutm,xorg,yorg,dx,dy,nx,ny,
     +  ijunk,ncell1,ncell2,surfht,htmin1,htmin2
      read (13) ijunk,ijunk, nx,ny
       
      allocate (rkv(nx,ny,nz),zpbl(nx,ny),zh(nx,ny,nz))


c     PRINT DOMAIN DEFINITIONS
      print *, xorg,yorg
      print *, nx,ny,nz
      print *, dx, dy

c     WRITE HEADER FOR NEW FILE 
      write(20) name,note,ione,1,ibdate,btime,iedate,etime 
      write(20) rdum,rdum,iutm,xorg,yorg,dx,dy,nx,ny,ione,
     +  ncell1,ncell2,surfht,htmin1,htmin2
      write(20) ione,ione, nx,ny
      write(20) namez


c     LOOP FOR HOURLY DATA ---

      do ih = 1,24
12345   continue
c       READ IN MET VARIABLES FIRST

        do k=1,nz
          read (10,end=130) hour,idate,((rkv(i,j,k),i=1,nx),j=1,ny)
          read (11,end=130) hour1,idate1,((zh(i,j,k),i=1,nx),j=1,ny)
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
          enddo
        enddo

c       WRITE VERTICALLY AVERAGED HOURLY CONCS/RATES
        write (20) ibdate,btime,iedate,etime
        write (20) ione,namez,
     +      ((zpbl(i,j),i=1,nx),j=1,ny)


        print *, 'finished processing on ', ibdate,btime,iedate,etime
       

      enddo
130   continue
200   continue
      
      stop
      end

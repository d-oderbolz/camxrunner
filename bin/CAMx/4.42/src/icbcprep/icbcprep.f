      program icbcprep
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copyright (C) 1999  ENVIRON
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
c-----Reads a CAMx TOPCON file and prepares initial and boundary
c     condition files with the same constant values.
c     Domain definition is specified from user input.
c
      parameter(mxx=380, mxy=380, mxz=25, mxspec=50)
c
      character*240 ifile,line
      character*60 ictitle, bctitle
      character*10 tpspec
      real conc(mxx,mxy,mxspec), caloft(mxspec)
      real bconcx(mxx,mxz,mxspec), bconcy(mxy,mxz,mxspec)
      integer indx(4,mxx), indy(4,mxy)
      character*4 icname(10), bcname(10)
      character*4 icnote(60), bcnote(60)
      character*4 ispec(10,mxspec)
      data icname/'A','I','R','Q','U','A','L','I','T','Y'/
      data bcname/'B','O','U','N','D','A','R','Y',' ',' '/
      data ione /1/
      data izero /0/
      data zero /0./
c
c-----Read control input and open files
c
      write(*,*) 'Input path of CAMx TOPCON file: '
      read(*,'(10x,a)') ifile
      open(10,file=ifile,status='old')
      write(*,*) 'Opened CAMx TOPCON file: ',ifile
c 
      write(*,*) 'Input path of ic file: '
      read(*,'(10x,a)') ifile
      open(20,file=ifile,form='unformatted',status='new')
      write(*,*) 'Opened ic file: ',ifile
      write(*,*) 'Input 60-character ic file note'
      read(*,'(a)') ifile
      read(ifile(11:70),'(a)') ictitle
      write(*,*) ictitle
c
      write(*,*) 'Input path of bc file: '
      read(*,'(10x,a)') ifile
      open(30,file=ifile,form='unformatted',status='new')
      write(*,*) 'Opened bc file: ',ifile
      write(*,*) 'Input 60-character bc file note'
      read(*,'(a)') ifile
      read(ifile(11:70),'(a)') bctitle
      write(*,*) bctitle
c
      write(*,*) 'Input grid cell dimensions (nx,ny,nz)'
      read(*,'(a)') line
      read(line(11:),*) nx,ny,nz
      write(*,*) nx,ny,nz 
      write(*,*) 'Input grid information (xorg,yorg,dx,dy)'
      read(*,'(a)') line
      read(line(11:),*) xorg,yorg,dx,dy
      write(*,*) xorg,yorg,dx,dy
      write(*,*) 'Input UTM zone (zero if not using UTM)'
      read(*,'(a)') line
      read(line(11:),*) iutm
      write(*,*) iutm
      write(*,*) 'Input starting date/hour:'
      read(*,'(a)') line
      read(line(11:),*) idate,istart
      write(*,*) idate,istart
      write(*,*) 'Input ending date/hour:'
      read(*,'(a)') line
      read(line(11:),*) iedate,iend
      write(*,*) iedate,iend
c
c-----Check some dimensions
c
      if(nx .gt. mxx ) then
        write(*,*) 'Increase MXX and recompile'
        stop
      endif
      if(ny .gt. mxy ) then
        write(*,*) 'Increase MXY and recompile'
        stop
      endif
      if(nz .gt. mxz ) then
        write(*,*) 'Increase MXZ and recompile'
        stop
      endif
c
c-----Check for julian dates
c
      if (idate.gt.100000) call juldate(idate)
      if (iedate.gt.100000) call juldate(iedate)
c
c-----Read TOPCON file
c
      write(*,*) 'Reading TOPCON file'
      nspec = 0
      nskip = 0
 200  read(10,'(a10,f10.0)',end=210) tpspec,ctin
        write(*,'(a10,f15.10)') tpspec,ctin
        if (ctin .eq. 0.) then
          nskip = nskip + 1
          goto 200
        endif
        nspec = nspec+1
        if(nspec .gt. mxspec) then
           write(*,*) 'Increase MXSPEC and recompile'
           stop
        endif
        read(tpspec,'(10a1)') (ispec(i,nspec),i=1,10)
        caloft(nspec) = ctin
        goto 200
 210  continue
      if (nspec .eq. 0) then
        write(*,*) ' Zero species found in TOPCON file'
        stop
      endif
      write(*,*) ' Species on TOPCON file = ', nskip+nspec
      write(*,*) ' Skipped ', nskip, ' species with zero concentration'
c    
c-----Write file headers
c     Initial conc file
c
      write(*,*) 'Writing file ioutput headers'
      read(ictitle,'(60a1)') (icnote(n),n=1,60)
      write(20) icname,icnote,ione,nspec,idate,float(istart),iedate,
     &          float(iend)
      write(20) zero,zero,iutm,xorg,yorg,dx,dy,
     &          nx,ny,nz,izero,izero,zero,zero,zero
      write(20) izero,izero,nx,ny
      write(20) ((ispec(n,l),n=1,10),l=1,nspec)
c
c-----Boundary conc file
c
      read(bctitle,'(60a1)') (bcnote(n),n=1,60)
      write(30) bcname,bcnote,ione,nspec,idate,float(istart),iedate,
     &          float(iend)
      write(30) zero,zero,iutm,xorg,yorg,dx,dy,
     &          nx,ny,nz,izero,izero,zero,zero,zero
      write(30) izero,izero,nx,ny
      write(30) ((ispec(n,l),n=1,10),l=1,nspec)
c
c-----west edge
c
      iedge = 1
      do i =1,ny
        indx(1,i) = 2
        indx(2,i) = 0
        indx(3,i) = 0
        indx(4,i) = 0
      enddo
      indx(1,1) = 0
      indx(1,ny) = 0 
      write(30) ione, iedge, ny,((indx(n,i),n=1,4),i=1,ny)
c
c-----east edge
c
      iedge = 2
      do i =1,ny
        indx(1,i) = nx-1
      enddo
      indx(1,1) = 0
      indx(1,ny) = 0 
      write(30) ione, iedge, ny,((indx(n,i),n=1,4),i=1,ny)
c
c-----south edge
c
      iedge = 3
      do i =1,nx
        indy(1,i) = 2
        indy(2,i) = 0
        indy(3,i) = 0
        indy(4,i) = 0
      enddo
      indy(1,1) = 0
      indy(1,nx) = 0 
      write(30) ione, iedge, nx,((indy(n,i),n=1,4),i=1,nx)
c
c-----north edge
c
      iedge = 4
      do i =1,nx
        indy(1,i) = ny-1
      enddo
      indy(1,1) = 0
      indy(1,nx) = 0 
      write(30) ione, iedge, nx,((indy(n,i),n=1,4),i=1,nx)
c
c-----Write concentration data to files
c     Initial conc
c
      write(*,*) 'Writing initial concentrations'
      write(*,*) idate, float(istart), iedate, float(iend)
      write(20) idate, float(istart), iedate, float(iend)
      do l = 1,nspec
        do i = 1,nx
          do j = 1,ny
            conc(i,j,l) = caloft(l)
          enddo
        enddo
        do k = 1,nz
          write(20) ione, (ispec(m,l),m=1,10),
     &              ((conc(i,j,l),i=1,nx),j=1,ny)
        enddo
      enddo
c
c-----Boundary conc
c     Fill bconcx and bconcy
c
      write(*,*) 'Writing boundary concentrations'
      do l = 1,nspec
        do k = 1,nz
          do i = 2,nx-1
            bconcx(i,k,l) = caloft(l)
          enddo
          bconcx(1,k,l) = 0.
          bconcx(nx,k,l) = 0.
        enddo
        do k = 1,nz
          do j = 2,ny-1
            bconcy(j,k,l) = caloft(l)
          enddo
          bconcy(1,k,l) = 0.
          bconcy(ny,k,l) = 0.
        enddo
      enddo
c
c-----Write concentrations once for entire period
c
      write(30) idate, float(istart), iedate, float(iend)
      write(*,*) idate, float(istart), iedate, float(iend)
      do l = 1,nspec
        do iedge = 1,4
          if (iedge.le.2) then
            write(30) ione,(ispec(m,l),m=1,10),iedge,
     &              ((bconcy(j,k,l),k=1,nz),j=1,ny) 
          else
            write(30) ione,(ispec(m,l),m=1,10),iedge,
     &              ((bconcx(i,k,l),k=1,nz),i=1,nx) 
          endif
        enddo
      enddo
c
c-----Done
c
      end

      program asc2bin
c
c---  Convert UAM-IV format CAMx files from ascii to binary
c     Slightly changed by Daniel Oderbolz to allow non-interactive call
c
c     Call it like this bin2asc sourcefile destfile
c     the filetype is determined automatcally
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copyright (C) 2006  ENVIRON
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
      parameter(MXY=300, MXZ=16, MXSPEC=150, MXPT=25000)
c

      character*255 source
      character*255 dest
      
      character*80 record
      character*10 ftype
      character*4 fname(10), spname(10,MXSPEC)
      character*4 note(60)
      integer iloc(4,MXY)
      integer idumx(MXPT), idumy(MXPT)
      integer icell(MXPT), jcell(MXPT), kcell(MXPT)
      real    temp(MXY,MXY), bconc(MXZ,MXY)
      real    emiss(MXPT), flow(MXPT), plumht(MXPT)
      real    xc(MXPT),yc(MXPT),sh(MXPT)
      real    sd(MXPT),st(MXPT),sv(MXPT)
      logical lhdrspec, l3d, lbndry, lptsrc
c
c---  ENTRY POINT

c---  Read arguments   
c---  This is pgf77 specific code!     
      call getarg(1,source)
      call getarg(2,dest)

c
c---  read filenames and open files ---
c
      write(*,*) 'Convert UAM-IV format CAMx files from ascii to binary'
      write(*,*) 'The following file types are recognized:'
      write(*,*) ' AVERAGE'
      write(*,*) ' AIRQUALITY'
      write(*,*) ' BOUNDARY'
      write(*,*) ' EMISSIONS'
      write(*,*) ' PTSOURCE'
      write(*,*) ' TEMPERATUR'
      write(*,*) ' WIND'
      write(*,*)
      write(*,*) 'The file type will be determined automatically'
      write(*,*)
c      
c      write(*,*) 'Input file name: '
c      read (*,'(a)') record
      open (10,file=source,status='old')
      write(*,*) 'Opened file: ',source
c
c      write(*,*) 'Output file name: '
c      read (*,'(a)') record
      open (11,file=dest,form='unformatted',status='new')
      write(*,*) 'Opened file: ',dest
c
c---  read/write headers ---
c
      read (10,900) fname,note,nseg,nspec,idate,begtim,jdate,endtim
      write(11) fname,note,nseg,nspec,idate,begtim,jdate,endtim
      write(*,900) fname,note,nseg,nspec,idate,begtim,jdate,endtim
      write(ftype,'(10a1)') (fname(i),i=1,10)
      write(*,*) 'file type is ',ftype
c
c---  set parameters according to file type ---
c
      lhdrspec = .false.
      l3d      = .false.
      lbndry   = .false.
      lptsrc   = .false.
c
      if (ftype.eq.'INSTANT   ') then
        lhdrspec = .true.
        l3d = .true.
      elseif (ftype.eq.'AVERAGE   ') then
        lhdrspec = .true.
        l3d = .true.
      elseif (ftype.eq.'AIRQUALITY') then
        lhdrspec = .true.
        l3d = .true.
      elseif (ftype.eq.'EMISSIONS ') then
        lhdrspec = .true.
      elseif (ftype.eq.'TEMPERATUR') then
        l3d = .true.
      elseif (ftype.eq.'WIND      ') then
        l3d = .true.
      elseif (ftype.eq.'BOUNDARY  ') then
        lhdrspec = .true.
        lbndry = .true.
      elseif (ftype.eq.'PTSOURCE  ') then
        lhdrspec = .true.
        lptsrc = .true.
      else
        write(*,*)'Warning: file type not recognized'
      endif
c
c---  region definition ---
c
      read (10,901) orgx,orgy,iutm,utmx,utmy,dx,dy,nx,ny,
     &          nz,nzlo,nzup,hts,htl,htu
      write(11) orgx,orgy,iutm,utmx,utmy,dx,dy,nx,ny,
     &               nz,nzlo,nzup,hts,htl,htu
      write(*,901) orgx,orgy,iutm,utmx,utmy,dx,dy,nx,ny,
     &               nz,nzlo,nzup,hts,htl,htu
      read (10,902) i1,j1,nx1,ny1
      write(11) i1,j1,nx1,ny1
c
c---  species records ---
c
      if (lhdrspec) then
        read (10,903) ((spname(m,l),m=1,10),l=1,nspec)
        write(11) ((spname(m,l),m=1,10),l=1,nspec)
      endif
c
c---  Check some dimensions ---
c
      if(nx .gt. MXY) then
        write(*,*) 'Increase MXY to ',nx,' and recompile'
        stop
      endif
      if(ny .gt. MXY) then
        write(*,*) 'Increase MXY to ',ny,' and recompile'
        stop
      endif
      if(lbndry.and.nz .gt. MXZ) then
        write(*,*) 'Increase MXZ to ',nz,' and recompile'
        stop
      endif
      if(nspec .gt. MXSPEC) then
        write(*,*) 'Increase MXSPEC to ',nspec,'  and recompile'
        stop
      endif
      if(nspec .lt. 1) then
        write(*,*) 'no of species less than 1'
        stop
      endif
c
c---  read boundary definition header, if boundary file
c
      if (lbndry) then
        do i=1,4
          read (10,908) iseg, iedge, ncells,
     &            ((iloc(ii,j),ii=1,4),j=1,ncells)
          write(11) iseg, iedge, ncells,
     &            ((iloc(ii,j),ii=1,4),j=1,ncells)
        enddo
      endif
c
c---  read point source location and stack parameters, if 
c     point source file ---
c
      if (lptsrc) then
        read (10,910) iseg,npmax
        write(11) iseg,npmax
        if (npmax.gt.MXPT) then
          write(*,*) 'Increase MXPT to ',npmax,' and recompile'
          stop
        endif
        if (npmax.gt.0) then
          do ip=1,npmax
            read (10,911) xc(ip),yc(ip),sh(ip),sd(ip),st(ip),sv(ip)
          enddo
          write(11) (xc(ip),yc(ip),sh(ip),sd(ip),st(ip),sv(ip),
     &               ip=1,npmax)
        endif
      endif
c
c---  loop over hours ---
c
      do 100 ihr = 1,999
        read (10,904,end=999) ibgdat,begtim,iendat,endtim
        write(*,904) ibgdat,begtim,iendat,endtim
        write(11) ibgdat,begtim,iendat,endtim
c
c---  boundary file ---
c
        if (lbndry) then
          do 130 l=1,nspec
            ncel = ny
            do 140 i=1,4
              if (i.ge.3) ncel= nx
              read (10,909,err=800) iseg,(spname(m,l),m=1,10),nedgno,
     &            ((bconc(k,ij),k=1,nz),ij=1,ncel)
              write(11) iseg,(spname(m,l),m=1,10),nedgno,
     &            ((bconc(k,ij),k=1,nz),ij=1,ncel)
  140       continue
  130    continue
c
c---  point source file ---
c
        elseif (lptsrc) then
          read (10,910) iseg,numpts
          write(11) iseg,numpts
          if (numpts.gt.0) then
            read (10,912) (icell(ip),jcell(ip),kcell(ip),flow(ip),
     &                     plumht(ip),ip=1,numpts)
            write(11)     (icell(ip),jcell(ip),kcell(ip),flow(ip),
     &                     plumht(ip),ip=1,numpts)
            do l=1,nspec
              read (10,905) isegnm, (spname(m,l),m=1,10)
              read (10,913) (emiss(ip),ip=1,numpts)
              write(11) iseg, (spname(m,l),m=1,10), (emiss(ip),
     &                          ip=1,numpts)
            enddo
          endif
c
c---  all other file types ---
c
        else
          if(.not.l3d) then
            nlay = 1
          else
            nlay = nz
          endif
          do 110 l = 1,nspec
            do 120 k = 1,nlay
              read (10,905,err=800) idum, (spname(m,l),m=1,10)
              read (10,906,err=800) ((temp(i,j),i=1,nx),j=1,ny)
              write(11) idum, (spname(m,l),m = 1,10), 
     &                  ((temp(i,j),i=1,nx),j=1,ny)
 120        continue    
 110      continue    
        endif
c
c---  end loop over hours
c
 100  continue    
c
      goto 999
c
c---  I/O error messages ---
c
 800  write(*,*) 'Error reading hourly data block'
c
 900  format(10a1,60a1,/,i2,1x,i2,1x,i6,f6.0,i6,f6.0)
 901  format(2(f16.5,1x),i3,1x,4(f16.5,1x),5i4,3f7.0)
 902  format(4i5) 
 903  format(10a1)
 904  format(5x,2(i10,f10.2))
 905  format(i4,10a1)
 906  format(5e14.7)
 908  format(3i10/(5i14))
 909  format(i10,10a1,i10/(5e14.7))
 910  format(2i10)
 911  format(2(f16.5,1x),4e14.7)
 912  format(3i12,2e14.7)
 913  format(5e14.7)
c
 999  stop
      end

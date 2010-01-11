      program avgdif
c 
c***  compares two binary coarse grid average files and prints a 
c     summary of differences
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copyright (C) 1998  ENVIRON
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
      parameter (maxsp=60, maxx=100, maxy=100, maxz=10)
c
      character*4 filnam(10),fileid(60),mspec1(10,maxsp),
     &            mspec2(10,maxsp)
      character*80 ipath 
      dimension conc1(maxsp,maxz,maxx,maxy),conc2(maxsp,maxz,maxx,maxy) 
      dimension maxhr(maxsp),maxi(maxsp),maxj(maxsp),maxk(maxsp),
     &          spmax(maxsp),ki(maxsp),sumdif(maxsp),ndif(maxsp)
c
c
      data spmax /maxsp*0./
      data sumdif/maxsp*0./
      data ndif  /maxsp*0./
      data ncellhrs /0/
c
c***  open input and output files from command line arguments 
c
      write (*,'(a)') ' enter the name of the output file: '
      read (*,'(a)') ipath
      write (*,'(a)') ipath
      open (9,file=ipath,status='new') 
      write (*,'(a)') ' enter the name of the first average file: '
      read (*,'(a)') ipath
      write (*,'(a)') ipath
      write (9,'(a)') ' file 1: ', ipath
      open (7,file=ipath,form='unformatted',status='old')
      write (*,'(a)') ' enter the name of the second average file: '
      read (*,'(a)') ipath
      write (*,'(a)') ipath
      write (9,'(a)') ' file 2: ', ipath
      open (8,file=ipath,form='unformatted',status='old')
      write (*,'(a)') ' min and max layers to diff? (2 integers): '
      read (*,*) nozmin, nozmax
      write (*,*) nozmin, nozmax
      if (nozmin .gt. nozmax) then
         write(*,*) ' requested nozmin > nozmax '
         stop
      endif
      
c
c***  read the header records on file 1
c
      write(*,*)
      write(*,*) 'Reading file 1 header'
      read(7)filnam,fileid,noseg,nospec1,idatb,timbeg,jdate,timend
      write(*,1000) filnam,fileid,noseg,nospec1,idatb,timbeg,jdate,timend
      read(7) xref,yref,iutm,xorg,yorg,deltax,deltay,noxg,
     &    noyg,nozg,ncell1,ncell2,surfht,htmin1,htmin2
      write(*,1001) xref,yref,iutm,xorg,yorg,deltax,deltay,noxg,
     &    noyg,nozg,ncell1,ncell2,surfht,htmin1,htmin2
     
      if (noxg .gt. maxx) then
         write(*,*) ' increase the parameter maxx and re-compile'
         stop
      elseif (noyg .gt. maxy) then
         write(*,*) ' increase the parameter maxy and re-compile'
         stop
      elseif (nozg .gt. maxz) then
         write(*,*) ' increase the parameter maxz and re-compile'
         stop
      elseif (nozmax .gt. nozg) then
         write(*,*) ' requested nozmax > noz on input file'
         stop
      endif
c
c***  read the header records on file 2
c
      write(*,*)
      write(*,*) 'Reading file 2 header'
      read(8)filnam,fileid,noseg,nospec2,idatb,timbeg,jdate,timend
      write(*,1000) filnam,fileid,noseg,nospec2,idatb,timbeg,jdate,timend
      read(8) xref,yref,iutm,xorg,yorg,deltax,deltay,noxg,
     &    noyg,nozg,ncell1,ncell2,surfht,htmin1,htmin2
      write(*,1001) xref,yref,iutm,xorg,yorg,deltax,deltay,noxg,
     &    noyg,nozg,ncell1,ncell2,surfht,htmin1,htmin2
c
      if(noseg.ne.1) stop 6
c
c***  read the segment dimensions (redundant, from UAM-IV)
c
      read(7) ilocx,ilocy, nox,noy
c      write(*,1002) ilocx,ilocy, nox,noy
      read(8) ilocx,ilocy, nox,noy
c      write(*,1002) ilocx,ilocy, nox,noy
c
c***  read the species lists on each file
c     avgdif can handle different numbers of species on
c     each file, but file 1 must have fewer species than file 2
c
      if (nospec1 .gt. nospec2) then
         write(*,*) ' flip the order of files 1 and 2'
         stop
      endif
      if (nospec2 .gt. maxsp) then
         write(*,*) ' increase parameter maxsp to', nospec2
         stop
      endif

      read(7) ((mspec1(i,j),i=1,10),j=1,nospec1)
      read(8) ((mspec2(i,j),i=1,10),j=1,nospec2)
      write(*,*)
      write(*,*) 'Species on file 1 are'
      write(*,1003) ((mspec1(i,j),i=1,10),j=1,nospec1)
      write(*,*) 'Species on file 2 are'
      write(*,1003) ((mspec2(i,j),i=1,10),j=1,nospec2)
c
c***  check order of spec on both files and cross-ref via ki()
c
      write(*,*)
      write(*,*) 'Species on both files are'
      do 130 l=1,nospec1 
        do 120 ll=1,nospec2 
          do 110 i=1,10 
             if (mspec1(i,l) .ne. mspec2(i,ll)) go to 120 
  110     continue 
          ki(l) = ll 
          write(*,1015) (mspec1(i,l),i=1,10), (mspec2(i,ll),i=1,10)
          go to 130 
  120   continue 
  130 continue 
c
c***  read the time varying data
c
      write(*,*)
      write(*,*) 'Reading hourly data'
      ibhr = timbeg + 1
      iehr = timend
      write(9,1004)
      do 500 ll=1,100
        read(7,end=999) ibgdat, begtim, iendat, endtim
        read(8,end=999) ibgdat, begtim, iendat, endtim
        write(9,1005) ibgdat, begtim, iendat, endtim
        write(6,1005) ibgdat, begtim, iendat, endtim
c
c***  loop over species
c
        do 400 ispec=1,nospec1
c
c***  loop over layers
c
          do 401 iz=1,nozg
            read(7) iseg, filnam,
     &             ((conc1(ispec,iz,i,j),i=1,nox),j=1,noy)
401       continue
400     continue
c
        do 402 ispec=1,nospec2
c
c***  loop over layers
c
          do 403 iz=1,nozg
            read(8) iseg, filnam, 
     &           ((conc2(ispec,iz,i,j),i=1,nox),j=1,noy)
403       continue
402     continue
c
c***  find differences
c
        do 410 ispec=1,nospec1
          vmax = 0.
	  pk1 = 0.
	  pk2 = 0.
          do 411 iz=nozmin,nozmax
            do 412 j=1,noy
            do 412 i=1,nox
              v1 = conc1(ispec,iz,i,j)
              v2 = conc2(ki(ispec),iz,i,j)
              diff = abs(v1-v2)
c
c*** grand averages
c
              sumdif(ispec)=sumdif(ispec)+diff
              if (diff .ne. 0.0) ndif(ispec)=ndif(ispec)+1
              if (ispec.eq.1) ncellhrs = ncellhrs+1
c
              if(diff.gt.vmax) then
                vmax = diff
                ii = i
                jj = j
                kk = iz
                val1=v1
              endif
              if(v1.gt.pk1) then
                pk1 = v1
                i1 = i
                j1 = j
                k1 = iz
              endif
              if(v2.gt.pk2) then
                pk2 = v2
                i2 = i
                j2 = j
                k2 = iz
              endif
 412        continue
 411      continue
c
c***  write houly summaries
c
          write(9,1011) (mspec1(i,ispec),i=1,10),vmax,val1,ii,jj,kk,
     &         pk1,i1,j1,k1,pk2,i2,j2,k2
	  if(vmax.gt.spmax(ispec)) then
	    spmax(ispec) = vmax
	    maxhr(ispec) = ll
	    maxi( ispec) = ii
	    maxj( ispec) = jj
	    maxk( ispec) = kk
	  endif
410     continue
500   continue
c
c***  done reading data, write final summary
c
  999 continue
      write(9,1012) 
      write(9,1010) ((mspec1(i,j),i=1,10),spmax(j),maxhr(j),
     &              maxi(j),maxj(j),maxk(j),j=1,nospec1)
      write(9,1013) 
      write(9,1014) ((mspec1(i,j),i=1,10),
     &              sumdif(j)/max0(1,ndif(j)),ndif(j),j=1,nospec1)
      write(9,1016) ncellhrs
      write(6,1017)
      stop
c
 1000 format(' filnam =',10a1,/,
     &       ' fileid =',60a1,/,
     &       ' noseg =',i2,/,
     &       ' nspec =',i4,/,
     &       ' begdat =',i6,/,
     &       ' begtim =',f6.0,/,
     &       ' enddat =',i6,/,
     &       ' endtim =',f6.0)
 1001 format(' xref =',f12.3,/,
     &       ' yref =',f12.3,/,
     &       ' iutm =',i3,/,
     &       ' xorg =',f12.3,/,
     &       ' yorg =',f12.3,/,
     &       ' dx,dy =',2f10.4,/,
     &       ' nox,noy,noz,nc1,nc2 =',5i4,/,
     &       ' surfht,htmin1,htmin2 =',3f10.3)
 1002 format(1x,4i5)
 1003 format(1x,10a1)
 1004 format(1x,'spec      maxdiff  in val   at  i  j  k ',
     & 'file1 max  i  j  k file2 max  i  j  k',/)
 1005 format(5x,2(i10,f10.2))
 1006 format(i4,10a1)
 1010 format(1x,10a1,' max = ',1pg10.3' hr =', 4i4)
 1011 format(1x,10a1,2(1pe9.2,x),3i3,x,1pe9.2,3i3,x,1pe9.2,3i3)
 1012 format(/,1x,'max difference for all hours',6x,'hr   i   j   k')
 1013 format(/,1x,'avg difference for all hours')
 1014 format(1x,10a1,' avg = ',1pg10.3, ' in ', i10, ' cell*hrs')
 1015 format(1x,10a1,4x,10a1)
 1016 format(1x,'Number of cell*hrs compared =  ', i10)
 1017 format(/,1x,'Normal completion by AVGDIF')
c
      end

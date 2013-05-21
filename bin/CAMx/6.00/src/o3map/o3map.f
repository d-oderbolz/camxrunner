      program o3map
c
c-----This program generates an ASCII ozone column file for
c     direct input to CAMx.  TOMS data files must be supplied as input.
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copyright (C) 2012 ENVIRON
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
      implicit none
      integer ncol,nrow,nday,mlon,mlat
      parameter (ncol=500,nrow=500,nday=31)
      parameter (mlon=360,mlat=180)
      integer iout,izone,nx,ny,i,j,nfils,nda,iunit,icount,
     &        ibday,ieday,nlon,nlat,itmp,jtmp,idd1,idd2,iozone
      integer iozsim(ncol,nrow,nday),iozn(mlon,mlat)
      real xorg,yorg,xlonc,ylatc,tlat1,tlat2,polelon,polelat,
     &     dx,dy,xloc,yloc,dlat,dlon,frclat,frclon,dif1,dif2,
     &     ozmax,ozmin,doz,tim1,tim2
      real xlat(ncol,nrow),xlon(ncol,nrow)
      real xlatcl(mlat),xloncl(mlon)
      integer iyr(nday),mo(nday),ida(nday),
     &        eyr(nday),emo(nday),eda(nday)
      character*120 inrec,header
      character*200 inline
      character*8 lcoord
      data iout /10/
c
c-----Input grid specifics
c
      read(*,'(20x,a)') lcoord
      write(*,*) 'Projection: ',lcoord
      read(*,'(20x,a)') inrec
      if (lcoord.eq.'UTM') then
        read(inrec,*) xorg,yorg,izone
        write(*,*) xorg,yorg,izone
      elseif (lcoord.eq.'LATLON') then
        read(inrec,*) xorg,yorg
        write(*,*) xorg,yorg
      elseif (lcoord.eq.'LAMBERT') then
        read(inrec,*) xorg,yorg,xlonc,ylatc,tlat1,tlat2
        write(*,*) xorg,yorg,xlonc,ylatc,tlat1,tlat2
      elseif (lcoord.eq.'POLAR') then
        read(inrec,*) xorg,yorg,polelon,polelat
        write(*,*) xorg,yorg,polelon,polelat
      else
        write(*,*) 'Keyword ',lcoord,' not found!'
        write(*,*) 'Allowed projection keywords are:'
        write(*,*) 'UTM'
        write(*,*) 'LATLON'
        write(*,*) 'LAMBERT'
        write(*,*) 'POLAR'
        stop
      endif
      read(*,'(20x,a)') inrec
      read(inrec,*) dx,dy
      write(*,*) 'Master grid cell size: ',dx,dy
      read(*,'(20x,a)') inrec
      read(inrec,*) nx,ny
      write(*,*)'Domain size: ',nx,ny
c
c-----Open output file
c
      read(*,'(20x,a)') inrec
      open(iout,file=inrec,status='unknown')               
      write(*,*)'Opened output file: ',inrec
c
c-----Determine lat/lon of coarse grid cell centers
c
      do j = 1,ny
        yloc = yorg + (j-0.5)*dy 
        do i = 1,nx
          xloc = xorg + (i-0.5)*dx 
          if (lcoord.eq.'LATLON') then
            xlon(i,j) = xloc
            xlat(i,j) = yloc
          elseif (lcoord.eq.'UTM') then
            call utmgeo(1,izone,xloc,yloc,xlon(i,j),xlat(i,j)) 
          elseif (lcoord.eq.'POLAR') then 
            call pspgeo(1,polelon,polelat,xloc,yloc,xlon(i,j),xlat(i,j)) 
          elseif (lcoord.eq.'LAMBERT') then 
            call lcpgeo(1,ylatc,xlonc,tlat1,tlat2,xloc,yloc,xlon(i,j),
     &                  xlat(i,j)) 
          else
            write(*,*) 'Keyword ',lcoord,' not found'
            stop
          endif 
        enddo
      enddo
      write(*,*) 'lat/lon range: ',xlon(1,1),xlat(1,1),xlon(nx,ny),
     &           xlat(nx,ny)
c
c-----Loop over TOMS input files and read header data
c
      read(*,'(20x,a)') inrec
      read(inrec,*) nfils
      write(*,*)'Number of TOMS files: ',nfils

      do 50 nda = 1,nfils
        iunit = 20
        icount = 0
        read(*,'(20x,a)') inline
        read(inline,*) ibday,ieday
        do j=1,200
           if (inline(j:j).eq.',') icount=icount+1
           if (icount.eq.2) then
              inrec = inline(j+1:)
              go to 209
           endif
        enddo

209     iyr(nda) = ibday/10000
        mo(nda)  = (ibday-iyr(nda)*10000)/100
        ida(nda) = ibday-iyr(nda)*10000-mo(nda)*100

        eyr(nda) = ieday/10000
        emo(nda) = (ieday-eyr(nda)*10000)/100
        eda(nda) = ieday-eyr(nda)*10000-emo(nda)*100

        open(iunit,file=inrec,status='old')               
        write(*,*)'Opened: ',inrec

        write(*,'(3(A,i3,x))') 
     &  ' Beg time: year =',iyr(nda),'month =',mo(nda),'day =',ida(nda)
        write(*,'(3(A,i3,x))') 
     &  ' End time: year =',eyr(nda),'month =',emo(nda),'day =',eda(nda)

        read(iunit,'(a80)') header                             
        read(iunit,'(a80)') header                            
        read(header,'(14x,i3)') nlon
        read(iunit,'(a80)') header                           
        read(header,'(14x,i3)') nlat
c
c-----Determine lat/lon cells of TOMS data
c
        dlat = 180./nlat                                         
        frclat = dlat/2.
        do j = 1,nlat
          xlatcl(j) = -90. + frclat + (j-1)*dlat                      
        enddo
        dlon = 360./nlon                                   
        frclon = dlon/2.
        do i = 1,nlon
          xloncl(i) = -180. + frclon + (i-1)*dlon               
        enddo
        write(*,'(a,i4,2f6.3)')'TOMS Latitude grid : ',nlat,dlat,frclat
        write(*,'(a,i4,2f6.3)')'TOMS Longitude grid: ',nlon,dlon,frclon

        do j = 1,nlat                               
          read(iunit,'(1x,25i3)') (iozn(i,j),i=1,nlon)    
        enddo
        close(iunit)

        write(*,'(a,3i2.2,a)') ' Starting TOMSprep for date: ',
     &  iyr(nda),mo(nda),ida(nda),
     &  '. Please wait! It may take a few minutes...'
        call TOMSprep(nlon,nlat,dlon,dlat,frclon,frclat,iozn) 
        write(*,*) 'Finished TOMSprep for',iyr(nda),mo(nda),ida(nda)
c
c-----Process the ozone column data onto the CAMx grid                     
c                                                       
        do 40 j = 1,ny
          do 30 i = 1,nx
            do itmp = 1,nlon
              dif1 = abs(xlon(i,j) - xloncl(itmp))
              if (dif1.le.frclon) then
                do jtmp = 1,nlat
                  dif2 = abs(xlat(i,j) - xlatcl(jtmp))
                  if (dif2.le.frclat) then
                    iozsim(i,j,nda) = iozn(itmp,jtmp)
                    goto 30
                  endif
                enddo
              endif
            enddo
  30      continue
  40    continue
  50  continue
c
c-----Find the range of ozone column over all input days
c
      ozmax = -9999.
      ozmin = 10000.
      do nda = 1,nfils
        do j = 1,ny
          do i = 1,nx
            ozmax = amax1(ozmax,float(iozsim(i,j,nda)))
            ozmin = amin1(ozmin,float(iozsim(i,j,nda)))
          enddo
        enddo
      enddo

      if (ozmin.lt.250.0) then
         ozmin = 250.0
         write(*,*) 'Minimum bound of ozone column reset to 250'
      endif
      if (ozmax.gt.600.0) then
          ozmax = 600.0
         write(*,*) 'Maximum bound of ozone column reset to 600'
      endif

      doz = (ozmax - ozmin)/5
      write(*,*) 'Range of ozone column: ',ozmin,ozmax,doz
      doz = doz*1.001
c
c-----Write header of CAMx ozone column file
c
      write(*,*)
      write(*,*) 'Writing output file'
      idd1 = iyr(1)*10000 + mo(1)*100 + ida(1)
      idd2 = eyr(nfils)*10000 + emo(nfils)*100 + eda(nfils)
      write(iout,'(a,2i10)') 'Ozone column data',idd1,idd2
      write(iout,'(a10,5f10.3)') 'OZONE COL ',
     &                           ((ozmin + (i - .5)*doz)*.001,i=1,5)
c
c-----Write ozone column
c
      write(*,*)
      tim1 = 0.
      tim2 = 2400.
      do nda = 1,nfils
        idd1 = iyr(nda)*10000 + mo(nda)*100 + ida(nda)
        idd2 = eyr(nda)*10000 + emo(nda)*100 + eda(nda)
        call juldate(idd1)
        call juldate(idd2)
        write(*,'(2(a,i10,f10.2))') 
     &      ' Writing ozone for period',idd1,tim1,' to ',idd2,tim2
        write(iout,'(a10,2(i10,f10.2))') 
     &  'OZONE COL ',idd1,tim1,idd2,tim2
        do j = ny,1,-1

          do i=1,nx
             if (iozsim(i,j,nda).lt.250) iozsim(i,j,nda) = 250
             if (iozsim(i,j,nda).gt.600) iozsim(i,j,nda) = 600
          enddo ! i

          do i=1,nx
             if (doz.ge.1e-10) then
                iozone = 1 + int((iozsim(i,j,nda)-ozmin)/doz)
             else
                iozone = 1
             endif
             write(iout,'(i1,$)') iozone
          enddo
          write(iout,*)
        enddo
      enddo
c
      stop
      end

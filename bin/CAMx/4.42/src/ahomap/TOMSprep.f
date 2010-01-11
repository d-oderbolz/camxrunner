      subroutine TOMSprep(nlon,nlat,dlon,dlat,frclon,frclat,ozone)

C TOMSprep is a preprocessor for TOMS data. It is used to fill the 
C missing by 1/exp(d) interpolation

ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copyright (C) 2003  ENVIRON
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
      
      parameter (mlon=360,mlat=180)
      character ifile*180, data*80  ! file name and data string
      character*15 data15(mlat)     ! for the lat information
      integer ozone(mlon,mlat)      ! read in the ozone value
      real*8 rozone(mlon,mlat)      ! ozone value in real number
      real*8 rozone2(mlon,mlat)     ! ozone value in real number
      real*8 lat1, lat2, lon1, lon2 ! lat and lon of the ozone data point
      real*8 d                      ! distance 
      integer icount                ! Count number of valid ozone data

      icount = 0
      do i = 1,nlon
        do j = 1,nlat
          rozone(i,j)  = float(ozone(i,j))
          rozone2(i,j) = rozone(i,j)
          if (ozone(i,j).ge.250. .and. ozone(i,j).le.600.) then
            icount = icount + 1
          endif
        enddo
      enddo

      if (icount.eq.0) then
        write(*,*) 'No Valid Ozone Column Data. Ahomap Stopping! '
        stop
      endif

      do i = 1,nlon
        do j = 1,nlat
          if (rozone(i,j).lt.250. .or. rozone(i,j).gt.600.) then ! Level 1
            lon1 = (i-1)*dlon - (180. - frclon)
            lat1 = (j-1)*dlat - ( 90. - frclat)
            rozone2(i,j) = 0.
            ratio = 0

             do ii = 1,nlon
               do jj = 1,nlat
                 if (rozone(ii,jj).ge.250. .and. 
     &               rozone(ii,jj).le.600.) then                 ! Level 2
                    lon2 = (ii-1)*dlon - (180. - frclon) 
                    lat2 = (jj-1)*dlat - ( 90. - frclat)

                    if (lon2-lon1.gt. 180.) lon2 = lon2 - 360.
                    if (lon2-lon1.lt.-180.) lon2 = lon2 + 360.
                    d = (lon2-lon1)**2. + (lat2-lat1)**2.
                    d = dsqrt(d)

                    rozone2(i,j) = rozone2(i,j) + rozone(ii,jj)/dexp(d)
                    ratio = ratio + 1./dexp(d)

                 endif ! Level 2
               enddo ! ii
             enddo ! jj

            if (ratio.eq.0.) ratio = 1.
            rozone2(i,j) = rozone2(i,j)/ratio
            ozone(i,j) = idnint(rozone2(i,j))

          endif ! Level 1
        enddo ! i
      enddo ! j
       
      return
      end

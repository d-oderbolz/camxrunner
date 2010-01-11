      subroutine srfruf(m1,m2,m3,ncol,nrow,nlay,date,cellat,cellon,
     &                  windu,windv,fsurf,lrdruf,icdruf,ruflen,lrdsno,
     &                  icdsno,sfcz0)
c
c-----CAMx v5.10 090918
c 
c     SRFRUF calcualtes surface roughness for the specified grid, according
c     to landuse, season, optional surface fields from the AHO file (snow and
c     roughness code, and wind speed (water surfaces only).
c 
c     Copyright 1996-2008
c     ENVIRON International Corporation
c           
c     Modifications:
c        None
c
c     Input arguments:
c        m1                  number of columns (MPI slice)
c        m2                  number of rows (MPI slice)
c        m3                  number of layers (MPI slice)
c        ncol                number of columns 
c        nrow                number of rows 
c        nlay                number of layers 
c        date                model date
c        cellat              cell centroid latitude (deg)
c        cellon              cell centroid longitude (deg)
c        windu               layer U-component wind field (m/s)
c        windv               layer V-component wind field (m/s)
c        fsurf               fractional landuse cover field (fraction)
c        lrdruf              flag that gridded roughness is available
c        icdruf              optional surface roughness length index
c        ruflen              optional surface roughness length value (m)
c        lrdsno              flag that gridded snow cover is available
c        icdsno              optional snow index
c             
c     Output arguments: 
c        sfcz0               surface roughness (m)
c             
c     Routines called: 
c        None
c             
c     Called by: 
c        EMISTRNS
c 
      implicit none
      include 'camx.prm'
      include 'deposit.com'
c
      integer :: m1,m2,m3
      integer :: ncol,nrow,nlay,idate,date,month
      real, dimension(m1,m2) :: cellat, cellon, sfcz0
      integer, dimension(m1,m2) :: icdruf, icdsno
      real, dimension(m1,m2,m3) :: windu, windv
      real, dimension(m1,m2,NLU) :: fsurf
      real ruflen(NRUF)
      logical lrdruf,lrdsno
c
      integer i,j,m,mbin,latbin,isesn
      real ucomp,vcomp,wind,totland,z0,z0sum

      real z0min,sum,z0max
c
c-----Entry point
c
      idate = date
      call caldate(idate)
      month = (idate - 10000*int(idate/10000.))/100
c
c-----Loop over rows and columns
c
      do 30 j = 2,m2-1 
        do 20 i = 2,m1-1
c
c-----Determine season
c
          mbin = month
          if (cellat(i,j).lt.0.) then
            mbin = mod(month+6,12)
            if (mbin.eq.0) mbin = 12
          endif
          latbin = 1
          if (abs(cellat(i,j)).gt.20.) then
            latbin = 2
          elseif (abs(cellat(i,j)).gt.35.) then
            latbin = 3
          elseif (abs(cellat(i,j)).gt.50.) then
            latbin = 4
          elseif (abs(cellat(i,j)).gt.75.) then
            latbin = 5
          endif
          if ((cellat(i,j).gt.50. .and. cellat(i,j).lt.75.) .and.
     &        (cellon(i,j).gt.-15. .and. cellon(i,j).lt.15.)) latbin = 3
          isesn = iseason(latbin,mbin)
c
c-----Use input snow cover to set season, if specified
c
          if (lrdsno .and. icdsno(i,j).eq.1) isesn = 4
c
c-----Load local met variables
c
          ucomp = (windu(i,j,1) + windu(i-1,j,1))/2.
          vcomp = (windv(i,j,1) + windv(i,j-1,1))/2.
          wind = sqrt(ucomp**2 + vcomp**2)
          wind = amax1(0.1,wind)
c
c-----Loop over land use; surface roughness for water is dependent on
c     wind speed
c
          totland = 0.
          z0sum = 0.
          do 10 m = 1,NLU
            if (fsurf(i,j,m).lt.0.01) goto 10
            totland = totland + fsurf(i,j,m)
            z0 = z0lu(m,isesn)
            if (m.eq.7) z0 = amax1(z0,2.0e-6*wind**2.5)
c
c-----Use input surface roughness, if specified
c
            if (lrdruf .and. icdruf(i,j).gt.0) z0 = ruflen(icdruf(i,j))
c
            z0sum = z0sum + alog(z0)*fsurf(i,j,m)
 10       continue

          totland = amax1(totland, 0.01)
          sfcz0(i,j) = exp(z0sum)/totland

 20     continue
 30   continue
c
c     z0min = 1.e6
c     z0max = -1.e6
c     do j = 2,m2-1 
c       do i = 2,m1-1
c         sum = sum + sfcz0(i,j) 
c         z0min = min(z0min,sfcz0(i,j))
c         z0max = max(z0max,sfcz0(i,j))
c       enddo
c     enddo
c     sum = sum/((m2-2)*(m1-2))
      return
      end

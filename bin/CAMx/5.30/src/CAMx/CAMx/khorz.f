      subroutine khorz(igrid,ncol,nrow,nlay,numrows,joff,dx,dy,
     &                 deltat,windu,windv,idfin,rkx,rky)
      use filunit
      use bndary
c
c----CAMx v5.30 101223
c
c     KHORZ computes horizontal diffusion coefficients using a wind
c     deformation approach. RKX(i) is defined on the east wall of cell (i),
c     RKY(j) is defined on the northern wall of cell (j). RKX and RKY set 
c     to zero on grid boundaries, and for cell interfaces that are within 
c     child nested grids
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c          
c     Modifications:
c        11/30/99  Form of Kh revised to be:
c                  Kh = Kh0 + c*dx*dx*(deformation)
c                  where Kh0 is dependent on advection solver and grid size.
c                  Max Kh is now grid size and timestep dependent
c                  Echos min/avg/max stats by layer to diag file
c        10/31/03  Reduced value of Kh0 by factor of 4
c         5/28/09  Reduced minimum horiz diffusion
c
c     Input arguments:
c        igrid               grid index
c        ncol                number of columns in this slice
c        nrow                number of rows in this slice
c        nlay                number of layers in this slice
c        numrows             number of rows in entire grid
c        joff                row offset of this slice in entire grid
c        dx                  cell size in x-direction (m)
c        dy                  cell size in y-direction (m)
c        deltat              timestep (s)
c        windu               wind speed in x-direction (m/s)
c        windv               wind speed in y-direction (m/s)
c        idfin               map of nested grids in this grid 
c
c     Output arguments:
c        rkx                 horiz diffusion coefficient in x (m2/s)
c        rky                 horiz diffusion coefficient in y (m2/s)
c
c     Routines called:
c        none
c
c     Called by:
c        CAMx
c
      include "camx.prm"
c
      real    dx(numrows)
      real    windu(ncol,nrow,nlay)
      real    windv(ncol,nrow,nlay)
      real    rkx(ncol,nrow,nlay)
      real    rky(ncol,nrow,nlay)
      integer idfin(ncol,nrow)
c
c-----Entry point
c
      jmid = nrow/2
      diffac = 0.25/1.414
      difmin = 1.
      difmax = dx(jmid+joff)*dy/(32.*deltat)
      difmax = amin1(difmax,1.e5)
      kh0 = 0.003*dx(jmid+joff)*dy/deltat
c
      write(idiag,*)
      write(idiag,*)'Horizontal diffusion coefficients'
      write(idiag,*)'Stats (min, avg, max) for grid ',igrid
      do 50 k = 1,nlay
        xsum = 0.
        xcnt = 0.
        xmin = 1.e10
        xmax = 0.
        ysum = 0.
        ycnt = 0.
        ymin = 1.e10
        ymax = 0.
c
c-----Derive Kx
c
        do 20 j = 2,nrow-1
          do 10 i = 2,ncol-2
            if (idfin(i,j).gt.igrid .or. idfin(i+1,j).gt.igrid) then
              rkx(i,j,k) = 0.
              goto 10
            endif
c
            dudx = (windu(i+1,j,k) - windu(i-1,j,k))/(2.*dx(j+joff))
            dudy = (windu(i,j+1,k) - windu(i,j-1,k))/(2.*dy)
c
            dv1 = (windv(i,j,k) - windv(i,j-1,k))/dy
            dv2 = (windv(i+1,j,k) - windv(i+1,j-1,k))/dy
            dvdy = (dv1 + dv2)/2.
            dv1 = (windv(i+1,j,k) - windv(i,j,k))/dx(j+joff)
            dv2 = (windv(i+1,j-1,k) - windv(i,j-1,k))/dx(j+joff)
            dvdx = (dv1 + dv2)/2.
c
            deform = sqrt((dudy + dvdx)**2 + (dudx - dvdy)**2)
            rkx(i,j,k) = kh0 + diffac*dy*dx(j+joff)*deform
            rkx(i,j,k) = amin1(difmax,rkx(i,j,k))
            rkx(i,j,k) = amax1(difmin,rkx(i,j,k))
            xsum = xsum + rkx(i,j,k)
            xcnt = xcnt + 1.
            xmin = amin1(xmin,rkx(i,j,k))
            xmax = amax1(xmax,rkx(i,j,k))
  10      continue
          rkx(1,j,k) = 0.
          rkx(ncol-1,j,k) = 0.
  20    continue
c
c-----Derive Ky
c
        do 40 i = 2,ncol-1
          do 30 j = 2,nrow-2
            if (idfin(i,j).gt.igrid .or. idfin(i,j+1).gt.igrid) then
              rky(i,j,k) = 0.
              goto 30
            endif
c
            dvdy = (windv(i,j+1,k) - windv(i,j-1,k))/(2.*dy)
            dvdx = (windv(i+1,j,k) - windv(i-1,j,k))/(2.*dx(j+joff))
c
            du1 = (windu(i,j,k) - windu(i-1,j,k))/dx(j+joff)
            du2 = (windu(i,j+1,k) - windu(i-1,j+1,k))/dx(j+joff)
            dudx = (du1 + du2)/2.
            du1 = (windu(i,j+1,k) - windu(i,j,k))/dy
            du2 = (windu(i-1,j+1,k) - windu(i-1,j,k))/dy
            dudy = (du1 + du2)/2.
c
            deform = sqrt((dudy + dvdx)**2 + (dudx - dvdy)**2)
            rky(i,j,k) = kh0 + diffac*dy*dx(j+joff)*deform
            rky(i,j,k) = amin1(difmax,rky(i,j,k))
            rky(i,j,k) = amax1(difmin,rky(i,j,k))
            ysum = ysum + rky(i,j,k)
            ycnt = ycnt + 1.
            ymin = amin1(ymin,rky(i,j,k))
            ymax = amax1(ymax,rky(i,j,k))
  30      continue
          rky(i,1,k) = 0.
          rky(i,nrow-1,k) = 0.
  40    continue
        xsum = xsum/xcnt
        ysum = ysum/ycnt
        havg = (xsum + ysum)/2.
        hmin = amin1(xmin,ymin) 
        hmax = amax1(xmax,ymax)
        write(idiag,'(i2,3f10.0)') k,hmin,havg,hmax
  50  continue
c
      return
      end

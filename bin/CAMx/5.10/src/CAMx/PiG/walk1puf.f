C**** WALK1PUF
c
      subroutine walk1puf(dt,igrd,nx,ny,nz,iipig,jjpig,ingrd,
     &                    xpig,ypig,pufftop,puffbot,windu,windv,
     &                    height,press,tempk)
      use grid
      implicit none
c
c----CAMx v5.10 090918
c
c     WALK1PUF calculates a new puff location by integrating dx/u(x) = dt
c     where u(x) = u(i-1) + (u(i) - u(i-1))*(x/deltax).  Each puff is
c     transported the entire dt or to the nearest cell interface,
c     whichever occurs first.
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Modifications:
c        9/5/03              Modified to handle "chained" puff approach
c
c     Input arguments:
c        dt                  remaining timestep (s)
c        igrd                grid index
c        nx                  number of cells in x-direction
c        ny                  number of cells in y-direction
c        nz                  number of layers
c        iipig               i-cell index of puff location 
c        jjpig               j-cell index of puff location 
c        ingrd               grid index of puff location
c        xpig                x-coord of puff location (km or deg)
c        ypig                y-coord of puff location (km or deg)
c        pufftop             puff top height (m)
c        puffbot             puff bottom height (m)
c        windu               gridded wind speed in x-direction (m/s)
c        windv               gridded wind speed in y-direction (m/s)
c        height              gridded layer height (m)
c        press               gridded pressure (mb)
c        tempk               gridded temperature (K)
c
c     Output arguments:
c        dt                  remaining timestep (s)
c        xpig                x-coord of puff location (km or deg)
c        ypig                y-coord of puff location (km or deg)
c        iipig               i-cell index of puff location 
c        jjpig               j-cell index of puff location 
c        ingrd               grid index of puff location
c
c     Subroutines Called:
c        PIGCOORD
c
c     Called by:
c        PIGWALK
c
      include "camx.prm"
c
      integer igrd,nx,ny,nz,iipig,jjpig,ingrd,nlaya
      real windu(nx,ny,nz),windv(nx,ny,nz),height(nx,ny,nz),
     &     press(nx,ny,nz),tempk(nx,ny,nz)
      real dt,wedge,sedge,xcell,xcell0,ycell,ycell0,ax,bx,ay,by,
     &     dtx,dty,dttmp,xpig,ypig,uup,uum,vvp,vvm,
     &     pufftop,puffbot,sumwt,deplyr
      integer iedge,i,j,kk,kpb,kpt
c
      real wtfac(MXLAYER)
c 
c-----Entry point
c
      iedge = 0
c
c-----Find layers containing top and bottom of puff
c
      i = iipig
      j = jjpig
      kpb = 1
      kpt = nlay(igrd)
      do kk = 1,nlay(igrd)
        if (height(i,j,kk) .GE. pufftop) then
          kpt = kk
          goto 26
        endif
      enddo
  26  continue
      do kk = nlay(igrd),1,-1
        if (height(i,j,kk) .LE. puffbot) then
          kpb = kk + 1
          goto 27
        endif
      enddo
 27   continue
c
c-----Determine layer density-weighted u,v wind components over puff depth
c
      if (kpb.eq.kpt) then
        wtfac(kpb) = 1.
      else
        sumwt = 0.
        deplyr = pufftop - height(i,j,kpt-1)
        wtfac(kpt) = deplyr*press(i,j,kpt)/tempk(i,j,kpt)
        sumwt = sumwt + wtfac(kpt)
c
        deplyr = height(i,j,kpb) - puffbot
        wtfac(kpb) = deplyr*press(i,j,kpb)/tempk(i,j,kpb)
        sumwt = sumwt + wtfac(kpb)
c
        do kk = kpb+1,kpt-1
          deplyr = height(i,j,kk) - height(i,j,kk-1)
          wtfac(kk) = deplyr*press(i,j,kk)/tempk(i,j,kk)
          sumwt = sumwt + wtfac(kk)
        enddo
c
        do kk = kpb,kpt
          wtfac(kk) = wtfac(kk)/sumwt
        enddo
      endif
c
      uup = 0.
      uum = 0.
      vvp = 0.
      vvm = 0.
      do kk = kpb,kpt
        uup = uup + wtfac(kk)*windu(i,j,kk)
        uum = uum + wtfac(kk)*windu(i-1,j,kk)
        vvp = vvp + wtfac(kk)*windv(i,j,kk)
        vvm = vvm + wtfac(kk)*windv(i,j-1,kk)
      enddo
c
c-----Calculate puff distance from West/South cell edge
c
      if (igrd.eq.1) then
        wedge = (i - 1)*delx
        sedge = (j - 1)*dely
      else
        wedge = (inst1(igrd) - 1)*delx + (i - 2)*delx/meshold(igrd)
        sedge = (jnst1(igrd) - 1)*dely + (j - 2)*dely/meshold(igrd)
      endif
      xcell0 = (xpig - wedge)/delx*deltax(j,igrd)*meshold(igrd)
      ycell0 = (ypig - sedge)/dely*deltay(igrd)*meshold(igrd)
c
c-----Compute the new position after the whole time step
c
      ax = uum
      bx = (uup - uum)/deltax(j,igrd)
      if (abs(bx).lt.1.e-6) then
        xcell = xcell0 + ax*dt
      else
        xcell = (-ax + (ax + bx*xcell0)*exp(bx*dt))/bx
      endif
c
      ay = vvm
      by = (vvp - vvm)/deltay(igrd)
      if (abs(by).lt.1.e-6) then
        ycell = ycell0 + ay*dt
      else
        ycell = (-ay + (ay + by*ycell0)*exp(by*dt))/by
      endif
c
c-----If the new position is beyond the boundaries of cell, compute
c     the time needed to reach the boundary
c
      dtx = dt
      dty = dt
      if (xcell.gt.deltax(j,igrd)) then
        iedge = 1
        if (abs(bx).lt.1.e-6) then
          dtx = (deltax(j,igrd)*1.001 - xcell0)/ax
        else
          dtx = alog((ax + bx*deltax(j,igrd)*1.001)/(ax + bx*xcell0))/bx
        endif
      elseif (xcell.lt.0.) then
        iedge = 1
        if (abs(bx).lt.1.e-6) then
          dtx = (-deltax(j,igrd)*0.001 - xcell0)/ax
        else
          dtx = alog((ax - bx*deltax(j,igrd)*0.001)/(ax + bx*xcell0))/bx
        endif
      endif
      if (ycell.gt.deltay(igrd)) then
        iedge = 1
        if (abs(by).lt.1.e-6) then
          dty = (deltay(igrd)*1.001 - ycell0)/ay
        else
          dty = alog((ay + by*deltay(igrd)*1.001)/(ay + by*ycell0))/by
        endif
      elseif (ycell.lt.0.) then
        iedge = 1
        if (abs(by).lt.1.e-6) then
          dty = (-deltay(igrd)*0.001 - ycell0)/ay
        else
          dty = alog((ay - by*deltay(igrd)*0.001)/(ay + by*ycell0))/by
        endif
      endif
c
c-----If the puff reaches a boundary, compute the remaining time and new
c     position
c
      if (iedge.eq.1) then
        dttmp = amin1(dtx,dty,dt)
        if (abs(bx).lt.1.e-6) then
          xcell = xcell0 + ax*dttmp
        else
          xcell = (-ax + (ax + bx*xcell0)*exp(bx*dttmp))/bx
        endif
        if (abs(by).lt.1.e-6) then
          ycell = ycell0 + ay*dttmp
        else
          ycell = (-ay + (ay + by*ycell0)*exp(by*dttmp))/by
        endif
        dt = dt - dttmp
      else
        dt = 0.
      endif
      xpig = wedge + xcell/deltax(j,igrd)*delx/meshold(igrd)
      ypig = sedge + ycell/deltay(igrd)*dely/meshold(igrd)
c
c-----Compute new ingrd, iipig, jjpig
c
      call pigcoord(xpig,ypig,iipig,jjpig,ingrd)
      if (ingrd.eq.0) dt = 0.
c
      return
      end

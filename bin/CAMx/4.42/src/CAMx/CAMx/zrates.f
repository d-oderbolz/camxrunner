      subroutine zrates(igrid,xyordr,ncol,nrow,nlay,nadv,deltat,dx,dy,
     &                  depth,phpt,pppt,ptpt,windu,windv,tempk,press,
     &                  mapscl,dilut,entrn)
c 
c----CAMx v4.42 070603
c  
c     ZRATES calculates new vertical velocity and dilution/entrainment
c     rates resulting from the time-varying vertical grid.
c 
c     Copyright 1996-2007
c     ENVIRON International Corporation
c 
c     Modifications:
c        4/26/99   Added Piecewise Parabolic Method for horizontal advection
c        12/3/99   A dummy atmospheric density is added above the top of
c                  the model based upon an extrapolation to an overlying
c                  layer of the same thickness as the top model layer
c        9/26/01   Revised the calculation of vertical velocity (W) to be
c                  consistent with the way the implicit solver uses W
c        4/10/03   X/Y advection now uses layer-dependent timestep
c       10/13/03   area weighting applied to winds rather than density
c        3/06/06   Revised top BC approach; removed top dummy layer
c                            
c     Input arguments:  
c        igrid               grid index 
c        xyordr              x/y advection order
c        ncol                number of columns 
c        nrow                number of rows 
c        nlay                number of layers
c        nadv                number of sub-steps per timestep
c        deltat              timestep (s)
c        dx                  cell size in x direction (m)
c        dy                  cell size in y direction (m)
c        depth               layer depth (m) 
c        phpt                time-rate change of layer interface height (m/s)
c        pppt                time-rate change of pressure (mb/s)
c        ptpt                time-rate change of temperature (K/s)
c        windu               u-component windspeed (m/s)
c        windv               v-component windspeed (m/s)
c        tempk               temperature (k)
c        press               pressure (mb)
c        mapscl              map-scale factor at cell centroid
c             
c     Output arguments:  
c        dilut               dilution rate (m/s) 
c        entrn               entrainment rate (m/s) 
c             
c     Routines Called:  
c        none 
c             
c     Called by:  
c        EMISTRNS 
c 
      implicit none
      include 'camx.prm'
      include 'bndary.com'
      include 'flags.com'
c
      integer xyordr,ncol,nrow,nlay,igrid
      integer nadv(nlay)
      real phpt(ncol,nrow,nlay), 
     &     windu(ncol,nrow,nlay),windv(ncol,nrow,nlay),
     &     depth(ncol,nrow,nlay),tempk(ncol,nrow,nlay),
     &     press(ncol,nrow,nlay),dilut(ncol,nrow,nlay),
     &     entrn(ncol,nrow,nlay),pppt(ncol,nrow,nlay),
     &     ptpt(ncol,nrow,nlay),mapscl(ncol,nrow),dx(nrow)
      real rho(MXCOLA,MXROWA,MXLAYA),rhon(MXCOLA,MXROWA,MXLAYA),
     &     fluxx(MXCOLA),fluxy(MXROWA),drhou(MXCOLA,MXROWA,MXLAYA),
     &     drhov(MXCOLA,MXROWA,MXLAYA)
      real c1d(MX1D),v1d(MX1D),a1d(MX1D),area(MX1D),flxarr(MX1D)
      real rhonxt(MXLAYA)
      real fpc(MX1D),fmc(MX1D)
      real deltat,dy
c
      integer i,j,k,i1,i2,l,nn,j1,j2,istep
      real drodz,rhotop(MXCOLA,MXROWA),areamx,avarea,dtuse,pnxt,tnxt,
     &     rhow,div,drhodt,totrat,windw
      common /comzratres/ rho, rhon, drhou, drhov
c
c-----Entry point 
c
      do 30 k = 1,nlay
c
c-----Load current atmospheric density (rho) and advected density (rhon)
c
        do j = 1,nrow
          do i = 1,ncol
            rho(i,j,k) = press(i,j,k)/tempk(i,j,k)
            rhon(i,j,k) = rho(i,j,k)
            if (k.eq.nlay) then
              drodz = 2.*(rho(i,j,k) - rho(i,j,k-1))/
     &                (depth(i,j,k) + depth(i,j,k-1))
              rhotop(i,j) = rho(i,j,k) + drodz*depth(i,j,k)/2.
            endif
          enddo
        enddo
c
c-----Determine atmospheric density flux in x-direction
c
        if (xyordr.eq.0) goto 200
 100    continue
        do 10 j = 2,nrow-1
          i1 = 1
          i2 = ncol
          if (igrid.eq.1) then
            if (ibeg(j).eq.-999) goto 10
            i1 = ibeg(j) - 1
            i2 = iend(j) + 1
          endif
c
          areamx = 0.
          do i = i1,i2
            area(i) = dy*depth(i,j,k)
            areamx = amax1(areamx,area(i))
          enddo
          l = 0
          do i = i1,i2-1
            l = l + 1
            avarea = dy*(depth(i,j,k) + depth(i+1,j,k))/
     &                (mapscl(i,j) + mapscl(i+1,j))
            v1d(l) = windu(i,j,k)*avarea/areamx
            a1d(l) = mapscl(i,j)*mapscl(i,j)*areamx/area(i)
            c1d(l) = rhon(i,j,k)
          enddo
          l = l + 1
          avarea = dy*depth(i2,j,k)/mapscl(i2,j)
          v1d(l) = windu(i2,j,k)*avarea/areamx
          a1d(l) = mapscl(i2,j)*mapscl(i2,j)*areamx/area(i2)
          c1d(l) = rhon(i2,j,k)
          nn = i2 - i1 + 1
c
          do i = i1,i2-1
            fluxx(i) = 0.
          enddo
          dtuse = deltat/nadv(k)
          do istep = 1,nadv(k)
            if (iadvct.eq.2) then
              call hadvbot(nn,dtuse,dx(j),c1d,v1d,a1d,flxarr,fpc,fmc)
            elseif( iadvct .eq. 3) then
              call hadvppm(nn,dtuse,dx(j),c1d,v1d,a1d,flxarr)
            endif
c
            l = 0
            do i = i1,i2-1
              l = l + 1
              fluxx(i) = fluxx(i) + flxarr(l)
              if (i.gt.i1) rhon(i,j,k) = c1d(l)
            enddo
          enddo
c 
          l = 1
          do i = i1+1,i2-1
            l = l + 1
            drhou(i,j,k) = (fluxx(i) - fluxx(i-1))*a1d(l)/deltat
          enddo
 10     continue
        if (xyordr.eq.0) goto 30
c
c-----Determine atmospheric density flux in y-direction
c
 200    continue
        do 20 i = 2,ncol-1
          j1 = 1
          j2 = nrow
          if (igrid.eq.1) then
            if (jbeg(i).eq.-999) goto 20
            j1 = jbeg(i) - 1
            j2 = jend(i) + 1
          endif
c
          areamx = 0.
          do j = j1,j2
            area(j) = dx(j)*depth(i,j,k)
            areamx = amax1(areamx,area(j))
          enddo
          l = 0
          do j = j1,j2-1
            l = l + 1
            avarea = (dx(j)*depth(i,j,k) + dx(j+1)*depth(i,j+1,k))/
     &               (mapscl(i,j) + mapscl(i,j+1))
            v1d(l) = windv(i,j,k)*avarea/areamx
            a1d(l) = mapscl(i,j)*mapscl(i,j)*areamx/area(j)
            c1d(l) = rhon(i,j,k)
          enddo
          l = l + 1
          avarea = dx(j2)*depth(i,j2,k)/mapscl(i,j2)
          v1d(l) = windv(i,j2,k)*avarea/areamx
          a1d(l) = mapscl(i,j2)*mapscl(i,j2)*areamx/area(j2)
          c1d(l) = rhon(i,j2,k)
          nn = j2 - j1 + 1
c
          do j = j1,j2-1
            fluxy(j) = 0.
          enddo
          dtuse = deltat/nadv(k) 
          do istep = 1,nadv(k)
            if (iadvct.eq.2) then
              call hadvbot(nn,dtuse,dy,c1d,v1d,a1d,flxarr,fpc,fmc)
            elseif( iadvct .eq. 3) then
              call hadvppm(nn,dtuse,dy,c1d,v1d,a1d,flxarr)
            endif
c
            l = 0
            do j = j1,j2-1
              l = l + 1
              fluxy(j) = fluxy(j) + flxarr(l)
              if (j.gt.j1) rhon(i,j,k) = c1d(l)
            enddo
          enddo
c
          l = 1
          do j = j1+1,j2-1
            l = l + 1
            drhov(i,j,k) = (fluxy(j) - fluxy(j-1))*a1d(l)/deltat
          enddo
 20     continue
        if (xyordr.eq.0) goto 100
 30   continue
c
c-----Loop over grid to calculate vertical velocity, dilution and
c     entrainment rates
c             
      do 60 j = 2,nrow-1 
        i1 = 2 
        i2 = ncol - 1 
        if (igrid.eq.1) then 
          if(ibeg(j).eq.-999) goto 60 
          i1 = ibeg(j) 
          i2 = iend(j) 
        endif 
        do 50 i = i1,i2
c
c-----Diagnose density at end of timestep
c
          do k = 1,nlay
            pnxt = press(i,j,k) + deltat*pppt(i,j,k)
            tnxt = tempk(i,j,k) + deltat*ptpt(i,j,k)
            rhonxt(k) = pnxt/tnxt
          enddo
c
c-----Calculate horizontal divergence
c
          rhow = 0.
          do 40 k = 1,nlay 
            div = (drhou(i,j,k) + drhov(i,j,k))
c
c-----Calculate actual density tendency
c
            drhodt = (rhonxt(k) - rho(i,j,k))/deltat
            totrat = drhodt + div
            rhow = rhow - depth(i,j,k)*totrat
c
            if (rhow.ge.0.) then
              windw = rhow/rhonxt(k) 
            else
              if (k.eq.nlay) then
                windw = rhow/rhotop(i,j)
              else
                windw = rhow/rhonxt(k+1)
              endif
            endif
c             
c-----Calculate entrainment and dilution rates
c
            dilut(i,j,k) = phpt(i,j,k)
            if (k.gt.1) dilut(i,j,k) = phpt(i,j,k) - phpt(i,j,k-1) 
            entrn(i,j,k) = phpt(i,j,k) - windw
 40       continue 
 50     continue
 60   continue 
c
      return
      end

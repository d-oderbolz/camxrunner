      subroutine zrates(m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon,
     &                  igrid,xyordr,ncol,nrow,nlay,nadv,deltat,dx,dy,
     &                  depth,phpt,pppt,ptpt,windu,windv,tempk,press,
     &                  mapscl,dilut,entrn)
      use bndary
c 
c----CAMx v5.10 090918
c  
c     ZRATES calculates new vertical velocity and dilution/entrainment
c     rates resulting from the time-varying vertical grid.
c 
c     Copyright 1996 - 2009
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
c        4/09/09   Improved definition of atmospheric density at top of model
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
      include 'flags.com'
c
      integer :: m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon
      integer :: m_xi1,m_xi2,m_xj1,m_xj2
      integer :: m_yi1,m_yi2,m_yj1,m_yj2
      integer :: m_zi1,m_zi2,m_zj1,m_zj2
c
      integer xyordr,ncol,nrow,nlay,igrid
      integer nadv(nlay)

      real, dimension(m1,m2,m3) :: windu,windv,tempk,
     &                             press,dilut,entrn,pppt,ptpt
      real, dimension(ncol,nrow,nlay) :: depth, phpt
      real :: mapscl(m1,m2),dx(nrow)
      real deltat,dy
c
      integer i,j,k,l,nn,istep, num1d
      real areamx,avarea,dtuse,pnxt,tnxt,ttop,ptop,dtdz,tavg,
     &     rhow,drhodt,windw
c
      real rho(MXCELLS,MXCELLS,MXLAYER)
      real rhon(MXCELLS,MXCELLS,MXLAYER)
      real rhotop(MXCELLS,MXCELLS)
      real c1d(MXCELLS)
      real v1d(MXCELLS)
      real a1d(MXCELLS)
      real area(MXCELLS)
      real flxarr(MXCELLS)
      real rhonxt(MXLAYER)
      real fpc(MXCELLS)
      real fmc(MXCELLS)
c
      common /comzratres/ rho, rhon
c
c-----Entry point 
c
      num1d = MAX(m1,m2,m3)
c
! Set grid point limits
      if (xyordr == 1) then
         ! doing x-dir first
         m_xi1= ia-1           ! x-adv, west  i-bound
         m_xi2= iz+1           ! x-adv, east  i-bound
         m_xj1= 1              ! x-adv, south j-bound
         m_xj2= m2             ! x-adv, north j-bound
         m_yi1= ia-1           ! y-adv, west  i-bound
         m_yi2= iz+1           ! y-adv, east  i-bound
         m_yj1= ja-1           ! y-adv, south j-bound
         m_yj2= jz+1           ! y-adv, north j-bound
      elseif (xyordr == 0) then
         ! doing y-dir first
         m_xi1= ia-1
         m_xi2= iz+1
         m_xj1= ja-1
         m_xj2= jz+1
         m_yi1= 1
         m_yi2= m1
         m_yj1= ja-1
         m_yj2= jz+1
      endif
      m_zi1= ia-1
      m_zi2= iz+1
      m_zj1= ja-1
      m_zj2= jz+1
      if(btest(ibcon,0)) then
         m_xi1 = ia
         m_yi1 = ia
         m_zi1 = ia
      endif
      if(btest(ibcon,1)) then
         m_xi2 = iz
         m_yi2 = iz
         m_zi2 = iz
      endif
      if(btest(ibcon,2)) then
         m_xj1 = ja
         m_yj1 = ja
         m_zj1 = ja
      endif
      if(btest(ibcon,3)) then
         m_xj2 = jz
         m_yj2 = jz
         m_zj2 = jz
      endif
c
      do 30 k = 1,nlay
c
c-----Load current atmospheric density (rho) and advected density (rhon)
c
        do j = 1,m2
          do i = 1,m1
            rho(i,j,k) = press(i,j,k)/tempk(i,j,k)
            rhon(i,j,k) = rho(i,j,k)
            if (k.eq.nlay) then
              dtdz = 2.*(tempk(i,j,nlay) - tempk(i,j,nlay-1))/
     &               (depth(i+i0,j+j0,nlay) + depth(i+i0,j+j0,nlay-1))
              ttop = tempk(i,j,nlay) + dtdz*depth(i+i0,j+j0,nlay)/2.
              tavg = (ttop + tempk(i,j,nlay))/2.
              ptop = press(i,j,nlay)*
     &               exp(-9.8*depth(i+i0,j+j0,nlay)/(2.*287.*tavg))
              rhotop(i,j) = ptop/ttop
            endif
          enddo
        enddo
c
c-----Determine atmospheric density flux in x-direction
c
        if (xyordr.eq.0) goto 200
 100    continue
c
cgwilsonc$omp parallel default(shared)
cgwilsonc$omp&  private(j,i1,i2,areamx,i,area,l,avarea,v1d,a1d,c1d,nn,
cgwilsonc$omp&          dtuse,istep,flxarr,fpc,fmc)
c
cgwilsonc$omp do schedule(dynamic)
c
        do 10 j = m_xj1, m_xj2
          areamx = 0.
          do i = 1,ncol
            area(i) = dy*depth(i,j+j0,k)
            areamx = amax1(areamx,area(i))
          enddo
          do i = 1,m1-1 
            avarea = dy*(depth(i+i0,j+j0,k) + depth(i+i0+1,j+j0,k))/
     &                (mapscl(i,j) + mapscl(i+1,j))
            v1d(i) = windu(i,j,k)*avarea/areamx
            a1d(i) = mapscl(i,j)*mapscl(i,j)*areamx/area(i+i0)
            c1d(i) = rhon(i,j,k)
          enddo
          avarea = dy*depth(m1+i0,j+j0,k)/mapscl(m1,j)
          v1d(m1) = windu(m1,j,k)*avarea/areamx
          a1d(m1) = mapscl(m1,j)*mapscl(m1,j)*areamx/area(m1+i0)
          c1d(m1) = rhon(m1,j,k)
c
          dtuse = deltat/nadv(k)
          do istep = 1,nadv(k)
            if (iadvct.eq.2) then
              call hadvbot(m1,dtuse,dx(j+j0),c1d,v1d,a1d,flxarr,fpc,fmc,num1d) 
            elseif( iadvct .eq. 3) then
              call hadvppm(m1,dtuse,dx(j+j0),c1d,v1d,a1d,flxarr,num1d)
            endif
c
            do i = 1,m1-1
              if (i.gt.1) rhon(i,j,k) = c1d(i)
            enddo
          enddo
c 
 10     continue
c
c  --- end of parallelized loop ---
c
cgwilsonc$omp end parallel
c
        if (xyordr.eq.0) goto 30
c
c-----Determine atmospheric density flux in y-direction
c
 200    continue
c
cgwilsonc$omp parallel default(shared)
cgwilsonc$omp&  private(i,j1,j2,areamx,area,l,j,v1d,a1d,c1d,avarea,nn,
cgwilsonc$omp&          dtuse,istep,flxarr,fpc,fmc)
c
cgwilsonc$omp do schedule(dynamic)
c
        do 20 i = m_yi1, m_yi2 
c
          areamx = 0.
          do j = 1,nrow
            area(j) = dx(j)*depth(i+i0,j,k)
            areamx = amax1(areamx,area(j))
          enddo
          do j = 1,m2-1
            avarea = (dx(j+j0)*depth(i+i0,j+j0,k) + dx(j+j0+1)*
     &             depth(i+i0,j+j0+1,k))/(mapscl(i,j) + mapscl(i,j+1))
            v1d(j) = windv(i,j,k)*avarea/areamx
            a1d(j) = mapscl(i,j)*mapscl(i,j)*areamx/area(j+j0)
            c1d(j) = rhon(i,j,k)
          enddo
          avarea = dx(m2+j0)*depth(i+i0,m2+j0,k)/mapscl(i,m2)
          v1d(m2) = windv(i,m2,k)*avarea/areamx
          a1d(m2) = mapscl(i,m2)*mapscl(i,m2)*areamx/area(m2+j0)
          c1d(m2) = rhon(i,m2,k)
c
          dtuse = deltat/nadv(k) 
          do istep = 1,nadv(k)
            if (iadvct.eq.2) then
              call hadvbot(m2,dtuse,dy,c1d,v1d,a1d,flxarr,fpc,fmc,num1d)
            elseif( iadvct .eq. 3) then
              call hadvppm(m2,dtuse,dy,c1d,v1d,a1d,flxarr,num1d)
            endif
c 
            do j = 1,m2-1
              if (j.gt.1) rhon(i,j,k) = c1d(j)
            enddo
          enddo
c
 20     continue
c
c  --- end of parallelized loop ---
c
cgwilsonc$omp end parallel
c
        if (xyordr.eq.0) goto 100
 30   continue
c
c-----Loop over grid to calculate vertical velocity, dilution and
c     entrainment rates
c             
      do 60 j = m_zj1, m_zj2
        do 50 i = m_zi1, m_zi2
c
c-----Diagnose density at end of timestep
c
          do k = 1,nlay
            pnxt = press(i,j,k) + deltat*pppt(i,j,k)
            tnxt = tempk(i,j,k) + deltat*ptpt(i,j,k)
            rhonxt(k) = pnxt/tnxt
          enddo
c
c-----Calculate actual density tendency
c
          rhow = 0.
          do 40 k = 1,nlay 
            drhodt = (rhonxt(k) - rhon(i,j,k))/deltat
            rhow = rhow - depth(i+i0,j+j0,k)*drhodt
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
            dilut(i,j,k) = phpt(i+i0,j+j0,k)
            if (k.gt.1) dilut(i,j,k) = 
     &                     phpt(i+i0,j+j0,k) - phpt(i+i0,j+j0,k-1) 
            entrn(i,j,k) = phpt(i+i0,j+j0,k) - windw
 40       continue 
 50     continue
 60   continue 
c
      return
      end

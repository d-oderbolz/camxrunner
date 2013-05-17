      subroutine pigsampl(m1,m2,m3,ioff,joff,lrtrac,igrd,numgrid,
     &                    nsp,nav,nxs,nys,ncol,nrow,nlay,
     &                    meshnst,inst1,jnst1,dt,delx,dely,deltax,
     &                    deltay,height,tempk,press,conc,cncsmp)
      use camxcom
      use chmstry
      use pigsty
      use tracer
      use rtracchm
      use node_mod
      implicit none
c
c----CAMx v6.00 130506
c
c     PIGSAMPL increments the time-averaged concentrations on PiG/RTRAC
c     sampling grids.
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Modifications: 
c        8/2/05       Generalized to sample regular model species or
c                     RTRAC species.
c        6/22/07      Temperature/pressure arrays interpolated to sampling
c                     grid for smoother results in complex terrain.
c       06/04/09      Added sigmax
c
c     Input arguments:
c        m1                 number of columns in this slice
c        m2                 number of row in this slice
c        m3                 number of layers in this slice
c        ioff               X offset of slice in full domain
c        joff               Y offset of slice in full domain
c        lrtrac             Rtrac flag
c        igrd               sampling grid index
c        numgrid            number of modeling grids in the simulation
c        nsp                number of species in input field
c        nav                number of species in output average field
c        nxs                number of sampling grid columns
c        nys                number of sampling grid rows
c        ncol               number of computational grid columns
c        nrow               number of computational grid rows
c        nlay               number of computational grid layers
c        meshnst            meshing factor for computational grid
c        inst1              starting I index of comp grid rel to master grid 
c        jnst1              starting J index of comp grid rel to master grid 
c        dt                 time step for computational grid concentration (s)
c        delx               master x-grid spacing in input units (km or deg)
c        dely               master y-grid spacing in input units (km or deg)
c        deltax             computational grid spacing in x (m)
c        deltay             computational grid spacing in y (m)
c        height             computational grid layer heights (m)
c        tempk              computational grid temperature field (K)
c        press              computational grid pressure field (mb)
c        conc               instantaneous gridded concentration (gas=umol/m3,
c                                                                other=ug/m3)
c        cncsmp             average sampling grid concentration (gas=ppm,
c                                                                other=ug/m3)
c
c     Output arguments:
c        cncsmp             average sampling grid concentration (gas=ppm,
c                                                                other=ug/m3)
c
c     Routines Called:
c        PIGCOORD
c        SMPL1PUF
c
c     Called by:
c        CAMx
c
      include "camx.prm"
      include "flags.inc"
c
      integer MXOUT
      parameter (MXOUT = MAX(MXSPEC,MXTRSP))

      integer m1
      integer m2
      integer m3
      integer ioff
      integer joff
      logical lrtrac
      integer igrd
      integer numgrid
      integer nsp
      integer nav
      integer nxs
      integer nys
      integer ncol
      integer nrow
      integer nlay
      integer meshnst
      integer inst1
      integer jnst1
      real    dt
      real    delx
      real    dely
      real    deltax(nrow)
      real    deltay
      real    height(ncol,nrow,nlay)
      real    tempk(m1,m2,m3)
      real    press(m1,m2,m3)
      real    conc(m1,m2,m3,nsp)
      real    cncsmp(nxs,nys,nav)
c
      real convfac(MXCOLSMP,MXROWSMP,MXOUT)
      real avcnc(MXCOLSMP,MXROWSMP,MXOUT)
      real pmass(MXOUT)
      real bndlow(MXOUT)
c
      integer l,mfac,jc,j,ic,i,n,nr,idum,jj,ng,ll
      real dtfact,yc,xc,yf,xf,
     &     dcdx1,dcdx2,c1,c2,dcdy,xpig,ypig,fpigx,fpigy,bpigx,bpigy,
     &     xpmin,xpmax,ypmin,ypmax,xmax,ymax,xlen,sigxx,pdepth
      real deltaxs,deltays,tsmp,psmp
c
      common /compigsampl/ convfac, avcnc
c
c-----Entry point
c
      dtfact = dt/(dtout*60.)
      mfac = meshsmp(igrd)/meshnst
      jj = (jsmp1(igrd) - jnst1)*meshnst + 2
      deltaxs = deltax(jj)/mfac
      jj = jj-joff
      deltays = deltay/mfac
c
c-----Interpolate gridded concentration fields from host computational 
c     grid to sampling grid, and define conc units conversion factor 
c
      jc = (jsmp1(igrd) - jnst1)*meshnst + 2
      jc = jc-joff
      yc = 0.5
      do 10 j = 1,nys
        ic = (ismp1(igrd) - inst1)*meshnst + 2
        ic = ic-ioff
        xc = 0.5
        yf = (j - 0.5)/float(mfac)
        if (yc.le.yf) then
          jc = jc + 1
          yc = yc + 1.
        endif
        do 20 i = 1,nxs
          xf = (i - 0.5)/float(mfac)
          if (xc.le.xf) then
            ic = ic + 1
            xc = xc + 1.
          endif
c
c  --- calculate the conversion factor for only cells in 
c      this slice ----
c
          if( ic .GT. 1 .AND. ic .LT. m1 .AND. jc .GT. 1
     &                                   .AND. jc .LT. m2 ) then
             dcdx1 = (tempk(ic,jc-1,1) - tempk(ic-1,jc-1,1))
             dcdx2 = (tempk(ic,jc,1) - tempk(ic-1,jc,1))
             c1 = tempk(ic,jc-1,1) - dcdx1*(xc - xf)
             c2 = tempk(ic,jc,1) - dcdx2*(xc - xf)
             dcdy = (c2 - c1)
             tsmp = c2 - dcdy*(yc - yf)
             dcdx1 = (press(ic,jc-1,1) - press(ic-1,jc-1,1))
             dcdx2 = (press(ic,jc,1) - press(ic-1,jc,1))
             c1 = press(ic,jc-1,1) - dcdx1*(xc - xf)
             c2 = press(ic,jc,1) - dcdx2*(xc - xf)
             dcdy = (c2 - c1)
             psmp = c2 - dcdy*(yc - yf)
             do l = 1,nav
               if (lrtrac) then
                 ng = nrtgas
                 ll = l
               else
                 ng = ngas
                 ll = lavmap(l)
               endif
               if (ll.gt.ng) then 
                 convfac(i,j,l) = 1.
               else
                 convfac(i,j,l) = densfac*(273./tsmp)*(psmp/1013.)
               endif
c
c  --- add background for cells within the computation domain
c      of this slice ----
c
              if( ic .GE. ia .AND. ic .LE. iz .AND. jc .GE. ja 
     &                                        .AND. jc .LE. jz ) then
                 if (lbckgrd) then
                   dcdx1 = (conc(ic,jc-1,1,ll) - conc(ic-1,jc-1,1,ll))
                   dcdx2 = (conc(ic,jc,1,ll) - conc(ic-1,jc,1,ll))
                   c1 = conc(ic,jc-1,1,ll) - dcdx1*(xc - xf)
                   c2 = conc(ic,jc,1,ll) - dcdx2*(xc - xf)
                   dcdy = (c2 - c1)
                   avcnc(i,j,l) = (c2 - dcdy*(yc - yf))/convfac(i,j,l)
                 else
                   avcnc(i,j,l) = 0.
                 endif
              endif
            enddo
          endif
 20     continue
 10   continue
c
c-----Start puff loop
c
      do 50 n = 1,npig
c
c-----Locate the pig in the grid; consider only puffs extending into layer 1
c
        xpig = (xpigf(n) + xpigb(n))/2.
        ypig = (ypigf(n) + ypigb(n))/2.
        call pigcoord(xpig,ypig,i,j,idum)
c
c--- adjust cell index and skip if PiG not in this slice ---
c
        i = i-ioff
        j = j-joff
       
        if( i .LT. 1 .OR. i .GT. m1 ) goto 50
        if( j .LT. 1 .OR. j .GT. m2 ) goto 50
c
        if( puffbot(n) .GT. height(i,j,1) ) goto 50
c
c-----Determine puff location relative to origin of sampling grid
c     and convert to meters
c
        fpigx = xpigf(n) - (ismp1(igrd) - 1)*delx
        fpigy = ypigf(n) - (jsmp1(igrd) - 1)*dely
        bpigx = xpigb(n) - (ismp1(igrd) - 1)*delx
        bpigy = ypigb(n) - (jsmp1(igrd) - 1)*dely

        fpigx = fpigx/delx*meshnst*deltax(j+joff)
        fpigy = fpigy/dely*meshnst*deltay
        bpigx = bpigx/delx*meshnst*deltax(j+joff)
        bpigy = bpigy/dely*meshnst*deltay
c
c-----Check if puff is not in or near the current sampling grid
c
        xpmax =  3.*sigx(n) + amax1(fpigx,bpigx)
        xpmin = -3.*sigx(n) + amin1(fpigx,bpigx)
        ypmax =  3.*sigy(n) + amax1(fpigy,bpigy)
        ypmin = -3.*sigy(n) + amin1(fpigy,bpigy)
        xmax = nxs*deltaxs
        ymax = nys*deltays
        if (xpmin .GT. xmax .OR. xpmax .LT. 0. .OR.
     &      ypmin .GT. ymax .OR. ypmax .LT. 0.) goto 50
c
c-----We should sample this one; load additional parameters for sampling
c
        xlen  = sqrt((fpigx - bpigx)**2 + (fpigy - bpigy)**2)
        sigxx = xlen/3. + sigx(n)
        pdepth = pufftop(n) - puffbot(n)
        do l = 1,nav
          pmass(l) = 0.
          do nr = 1,nreactr
            if (lrtrac) then
              pmass(l) = pmass(l) + puffrt(l,nr,n)
              bndlow(l) = rtlbnd(l)
            else
              ll = lavmap(l)
              pmass(l) = pmass(l) + puffmass(ll,nr,n)
              bndlow(l) = bdnl(ll)
            endif
          enddo
        enddo
c
        call smpl1puf(lbckgrd,MXCOLSMP,MXROWSMP,MXOUT,nxs,nys,
     &                nav,ismp1(igrd),jsmp1(igrd),mfac,meshnst,
     &                inst1,jnst1,ia,iz,ja,jz,ioff,joff,deltaxs,
     &                deltays,sigxx,sigy(n),fpigx,bpigx,fpigy,bpigy,
     &                pdepth,pmass,bndlow,convfac,avcnc)
  50  continue
c
c-----Increment running average 
c
      do l = 1,nav
        do 60 j = 1,nys
          do i = 1,nxs
            cncsmp(i,j,l) = cncsmp(i,j,l) + dtfact*avcnc(i,j,l)
          enddo
  60    continue
      enddo
c 
      return
      end

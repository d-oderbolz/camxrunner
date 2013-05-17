      subroutine smpl1puf(lbckgrd,maxcol,maxrow,maxspec,ncol,
     &                    nrow,navspc,ismpbeg,jsmpbeg,mfac,meshnst,
     &                    inst1,jnst1,ia,iz,ja,jz,ioff,joff,dx,dy,sigx,
     &                    sigy,xpigf,xpigb,ypigf,ypigb,pdepth,pmass,
     &                    bdnl,convfac,avcnc)
c
c----CAMx v6.00 130506
c
c     SMPL1PUF calculates a single puff's concentration distribution on
c     the given sampling grid.  A 2-D Gaussian distribution is assumed
c     in the horizontal; uniform (well mixed) concentrations are assumed
c     in the vertical.
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Modifications:
c        08/08/05       Moved from Rtrac, added option to include
c                       computational background concentrations
c
c     Input arguments:
c        lbckgrd             flag for adding background concs
c        maxcol              max number of sampling grid columns
c        maxrow              max number of sampling grid rows
c        maxspec             number of averaging species
c        ncol                number of sampling grid columns
c        nrow                number of sampling grid rows
c        navspc              number of averaging species
c        mfac                meshing factor for sampling grid in this nest
c        ismpbeg             starting I index of sample grid rel to master grid
c        jsmpbeg             starting J index of sample grid rel to master grid
c        meshnst             meshing factor for computational grid
c        inst1               starting I index of comp grid rel to master grid
c        jnst1               starting J index of comp grid rel to master grid
c        ia                  X beginning of computational part of this slice
c        iz                  X ending of computational part of this slice
c        ja                  Y beginning of computational part of this slice
c        jz                  Y ending of computational part of this slice
c        ioff                X offset of slice in full domain
c        joff                Y offset of slice in full domain
c        dx                  cell size in x-direction (m)
c        dy                  cell size in y-direction (m)
c        sigx                along-puff Gaussian standard deviation (m)
c        sigy                cross-puff Gaussian standard deviation (m)
c        xpigf               x-coord of puff front (m)
c        xpigb               x-coord of puff back (m)
c        ypigf               y-coord of puff front (m)
c        ypigb               y-coord of puff back (m)
c        pdepth              puff depth (m)
c        pmass               puff mass (umol)
c        bdnl                lower bounds conc (ppm)
c        convfac             units conversion factor for gasses
c        avcnc               background conc (ppm)
c                            (Initially interpolated gridded concs)
c
c     Output arguments:
c        avcnc               incremented background conc (ppm)
c
c     Subroutines Called:
c        none
c
c     Called by:
c        PIGSAMPL
c
      implicit none
      logical lbckgrd
      integer maxcol,maxrow,maxspec,ncol,nrow,navspc
      integer mfac
      integer ismpbeg
      integer jsmpbeg
      integer meshnst
      integer inst1
      integer jnst1
      integer ia
      integer iz
      integer ja
      integer jz
      integer ioff
      integer joff
      real dx,dy,sigx,sigy,xpigf,xpigb,ypigf,ypigb,pdepth,
     &     pmass(maxspec),bdnl(maxspec),convfac(maxcol,maxrow,maxspec),
     &     avcnc(maxcol,maxrow,maxspec)
      integer i,j,l,ic,jc
      real sigx2,sigy2,xpig,ypig,pi,phi,theta,xgrd,ygrd,xloc,yloc,dist,
     &     xloc2,yloc2,couple,cnc,sigmax
      real xc, yc, xf, yf
c
      data pi /3.1415927/
c
c-----Entry point
c
      sigx2 = sigx*sigx
      sigy2 = sigy*sigy
      sigmax = amax1(sigx,sigy)
c
c-----Determine puff orientation relative to sampling grid
c
      xpig = (xpigf + xpigb)/2.
      ypig = (ypigf + ypigb)/2.
      if (xpigf.eq.xpigb) then
        phi = pi/2.
      else
        phi = atan((ypigf - ypigb)/(xpigf - xpigb))
      endif
c
c-----Loop over rows/columns; calculate grid point orientation
c     relative to puff center point
c
      jc = (jsmpbeg - jnst1)*meshnst + 2
      jc = jc-joff
      yc = 0.5
      do 10 j = 1,nrow
        ic = (ismpbeg - inst1)*meshnst + 2
        ic = ic-ioff
        xc = 0.5
        yf = (j - 0.5)/float(mfac)
        if (yc.le.yf) then
          jc = jc + 1
          yc = yc + 1.
        endif
c
c  --- if this row is not in this slice, skip it all ---
c
        if( jc .LT. ja .OR. jc .GT. jz ) goto 10
        ygrd = (j - 0.5)*dy
        do 20 i = 1,ncol
          xf = (i - 0.5)/float(mfac)
          if (xc.le.xf) then
            ic = ic + 1
            xc = xc + 1.
          endif
c
c  --- if this row is not in this slice, skip it all ---
c
          if( ic .LT. ia .OR. ic .GT. iz ) goto 20
          xgrd = (i - 0.5)*dx
          dist = sqrt((xgrd - xpig)**2 + (ygrd - ypig)**2)
          if (xgrd.eq.xpig) then
            theta = pi/2.
          else
            theta = atan((ygrd - ypig)/(xgrd - xpig))
          endif
          theta = theta - phi
          xloc = dist*cos(theta)
          yloc = dist*sin(theta)
c
c-----Sample puff if grid point is within +/- 3*sigma of center point
c
          if (abs(xloc).le.3.*sigmax .and. abs(yloc).le.3.*sigmax) then
            xloc2 = xloc*xloc
            yloc2 = yloc*yloc
            couple = exp(-xloc2/(2.*sigx2))*exp(-yloc2/(2.*sigy2))
            do l = 1,navspc
              cnc = pmass(l)*couple/(2.*pi*sigx*sigy*pdepth)
              avcnc(i,j,l) = cnc/convfac(i,j,l) + avcnc(i,j,l)
              if (lbckgrd) avcnc(i,j,l) = amax1(avcnc(i,j,l),bdnl(l))
            enddo
          endif
   20   continue
   10 continue
      return
      end

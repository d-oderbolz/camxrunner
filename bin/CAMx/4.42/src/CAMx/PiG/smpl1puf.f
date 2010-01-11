      subroutine smpl1puf(lbckgrd,mxcol,mxrow,mxspec,ncol,nrow,navspc,
     &                    dx,dy,sigx,sigy,xpigf,xpigb,ypigf,ypigb,
     &                    pdepth,pmass,bdnl,convfac,avcnc)
c
c----CAMx v4.42 070603
c
c     SMPL1PUF calculates a single puff's concentration distribution on
c     the given sampling grid.  A 2-D Gaussian distribution is assumed
c     in the horizontal; uniform (well mixed) concentrations are assumed
c     in the vertical.
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Modifications:
c        08/08/05       Moved from Rtrac, added option to include
c                       computational background concentrations
c
c     Input arguments:
c        lbckgrd             flag for adding background concs
c        mxcol               max number of sampling grid columns
c        mxrow               max number of sampling grid rows
c        mxspec              max number of averaging species
c        ncol                number of sampling grid columns
c        nrow                number of sampling grid rows
c        navspc              number of averaging species
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
      integer mxcol,mxrow,mxspec,ncol,nrow,navspc
      real dx,dy,sigx,sigy,xpigf,xpigb,ypigf,ypigb,pdepth,
     &     pmass(mxspec),bdnl(mxspec),convfac(mxcol,mxrow,mxspec),
     &     avcnc(mxcol,mxrow,mxspec)
      integer i,j,l
      real sigx2,sigy2,xpig,ypig,pi,phi,theta,xgrd,ygrd,xloc,yloc,dist,
     &     xloc2,yloc2,couple,cnc,sigmax
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
      do j = 1,nrow
        ygrd = (j - 0.5)*dy
        do i = 1,ncol
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
        enddo
      enddo

      return
      end

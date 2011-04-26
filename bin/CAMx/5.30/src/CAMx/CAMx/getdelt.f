      subroutine getdelt(ncol,nrow,nlay,dx,dy,windu,windv,height,
     &                   dtmx,kmax)
      use bndary
      use camxcom
c
c----CAMx v5.30 101223
c
c     GETDELT computes the time step size according to a maximum allowable
c     Courant number
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c     Modifications:
c        11/30/99  Removed timestep calculation for horizontal diffusion
c        1/25/02   Made the CFL criterion a parameter
c        4/10/03   Now calculating max timestep by layer AND
c                  returning layer index of 'critical height'
c        1/16/06   Increased CFL to 0.9, reduced hmax to 10 m, revised kmax
c                  calculation to handle kmax = 1
c        5/25/06   CFL set according to advection solver
c        3/12/10   Increased Hmax for super-stepping to 3000 m (from 10 m)
c
c     Input arguments:
c        ncol                number of columne
c        nrow                number of rows
c        nlay                number of layers
c        dx                  cell sixe in x-direction (m)
c        dy                  cell size in y-direction (m)
c        windu               wind speed in x-direction (m/s)
c        windv               wind speed in y-direction (m/s)
c        height              layer height (m)
c
c     Output arguments:
c        dtmx                layer maximum time step size (s)
c        kmax                layer index of critical height hmax
c
c     Routines called:
c        none
c
c     Called by:
c        TIMESTEP
c
      include "camx.prm"
      include "flags.inc"
c
      real dx(nrow),windu(ncol,nrow,nlay),windv(ncol,nrow,nlay),
     &     height(ncol,nrow,nlay)
      real dtmx(nlay)
c
c-----Entry point
c
c
c  --- set height for super-stepping ---
c
      hmax = 9999999.
      if( lsuper ) hmax = 2000.
c
      cfl = 0.9
      if (iadvct.eq.3) cfl = 0.5
      do 30 k = 1,nlay
        dtmx(k) = 3600.
        tmpx = dtmx(k)
        tmpy = dtmx(k)
        do 20 j = 1,nrow-1
          do 10 i = 1,ncol-1
c
c-----Calculate maximum time step based on wind speed
c
            if (j.gt.1 .and. windu(i,j,k).ne.0.) 
     &        tmpx = cfl*abs(dx(j)/windu(i,j,k))
            if (i.gt.1 .and. windv(i,j,k).ne.0.) 
     &        tmpy = cfl*abs(dy/windv(i,j,k))
            dtmx(k) = amin1(dtmx(k),tmpx,tmpy)
 10       continue
 20     continue
 30   continue
c
c-----Determine layer index of 'critical height', which defines the depth
c     wherein the driving timestep for current grid will be determined 
c
      imid = ncol/2
      jmid = nrow/2
      do k = 2,nlay
        if (height(imid,jmid,k).gt.hmax) then 
          kmax = k-1
          kmax = max(1,kmax)
          goto 100
        endif
      enddo
      kmax = nlay
c
 100  return
      end

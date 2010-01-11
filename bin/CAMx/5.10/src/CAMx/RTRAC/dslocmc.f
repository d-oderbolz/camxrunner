c*** DSLOCMC
c
      subroutine dslocmc(y, rr, dt)
      use rtcmcchm
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Advance the slow species in the RTRAC CMC solver
c
c    Copyright 1996-2007
c    ENVIRON International Corporation
c
c    Argument descriptions:
c     Inputs:
c      rr    D  rate of each reaction
c      dt    D  time step
c     Outputs:
c      y     D  species concentrations
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    07/06/07   --gyarwood--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      implicit none
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real*8    dt
      real*8    rr(MXRX), y(MXTRSP+MXSPEC+MXRADCL)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i, j, isp
      real*8    ydot
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      do i = 1,nslortc
         isp = idslo(i)
         ydot = 0.0d0
c
c --- accumulate ydot terms for reactant loss
c
         if( nslloss(i) .GT. 0 ) then
            do j = 1,nslloss(i)
               ydot = ydot - rr(islloss(i,j))
            enddo
         endif
c
c --- accumulate ydot terms for product gain
c
         if( nslgain(i) .GT. 0 ) then
            do j = 1,nslgain(i)
               ydot = ydot + rr(islgain(i,j))*prdcoslo(i,j)
            enddo
         endif
c
c --- advance y(i)
c
         y(isp) = y(isp) + dt*ydot
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

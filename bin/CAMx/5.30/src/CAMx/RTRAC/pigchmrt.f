C*** PIGCHMRT
c
      subroutine pigchmrt(pcell,tcell,oh,o3,no3,dtime,rtcon)
      use chmstry
      use rtracchm
      use tracer
      implicit none
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c   Description:
c     This routine performs IRON PiG chemistry adjustments on the reactive
c     tracer gasses using information from the host chemistry solution.  
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       pcell   R  cell pressure (mb)
c       tcell   R  cell temperature (K)
c       oh      R  OH radical concentration (ppm)
c       o3      R  ozone concentration (ppm)
c       no3     R  no3 radical concentration (ppm)
c       dtime   R  time step (hours)
c       rtcon   R  RTRAC concentration (ppm)
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real    pcell
      real    tcell
      real    oh
      real    o3
      real    no3
      real    dtime
      real    rtcon(*)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i
      real      factor
      real      prod
      real      rkoh
      real      rko3
      real      rkno3
      real      rkspc
      real      arrhen
c
      real drate(MXTRSP)
      real rtnew(MXTRSP)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- factor is a temp/press adjustment for second order
c     rate constants that were converted from cm3 molec-1 sec-1 to
c     ppm-1 min-1 at STP, also convert to per hour ---
c
      factor = (298.0/tcell)*(pcell/1013.) * 60.0
c
c --- calculate the total decay rate and any secondary production
c     rate for each tracer.  get host photolysis rates from rk in the
c     chmstry.inc include file ---
c
      do i=1,nrtgas
         if( jnum(i) .GT. 0 ) then
            rkspc = rk(jnum(i))
         else
            rkspc = 0.0
         endif 
         rkoh  = arrhen(aoh(i),eaoh(i),boh(i),troh(i),tcell) * factor
         rko3  = arrhen(ao3(i),eao3(i),bo3(i),tro3(i),tcell) * factor
         rkno3 = arrhen(ano3(i),eano3(i),bno3(i),trno3(i),tcell)
     &           * factor
         drate(i) = oh*rkoh + o3*rko3 + no3*rkno3 + rkspc*rtjfact(i)
         rtnew(i) = rtcon(i)*EXP(-drate(i)*dtime)
         rtnew(i) = AMAX1(rtlbnd(i),rtnew(i))
         if ( lsecnd(i) ) then
            prod = AMAX1(rtcon(ksec(i)) - rtnew(ksec(i)),0.0)
            rtnew(i) = rtnew(i) + prod
         endif
      enddo
c
      do i=1,nrtgas
         rtcon(i) = rtnew(i)
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

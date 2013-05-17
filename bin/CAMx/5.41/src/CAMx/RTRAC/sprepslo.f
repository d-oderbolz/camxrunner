c*** SPREPSLO
c
      subroutine sprepslo(y, rr)
      use rtcmcchm
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Save concentration and reaction rate values to later advance the
c     slow species in the RTRAC CMC solver
c
c    Copyright 1996 - 2012
c    ENVIRON International Corporation
c
c    Argument descriptions:
c     Inputs:
c      y     R  species concentrations
c     Outputs:
c      rr    R  rate of each reaction
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
      real      y(MXTRSP+MXSPEC), rr(MXRX)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   k, n
      real      yh(MXTRSP+MXSPEC)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      do k = 1,ngasrtc
         yh(k) = max(0.0, y(k))
      enddo
c
c --- calculate reaction rates
c
      do n = 1,nrxnrtc
         rr(n) = srkrtc(n)
         if( nrct(n) .GT. 0) then
            do k = 1,nrct(n)
               rr(n) = rr(n)*yh(idxrct(n,k))
            enddo
         endif
      enddo
c
c --- update any equilibrium species and rr
c
      if( neqmrtc .GT. 0) call seqmcmc(yh,rr,.true.,.true.)
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

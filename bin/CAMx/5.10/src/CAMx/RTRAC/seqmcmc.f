c*** SEQMCMC
c
      subroutine seqmcmc(y, rr, lrr, lpr)
      use rtcmcchm
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Calculate equilibrium (steady-state) species concentrations and 
c     update reaction rates for SLSODE in the RTRAC CMC solver
c
c    Copyright 1996-2007
c    ENVIRON International Corporation
c
c    Argument descriptions:
c     Inputs:
c      y     R  species concentrations (ppm)
c      rr    R  time rate of change of species concentrations
c      lrr   L  only update rr if true
c      lpr   L  availability of prior rr
c     Outputs:
c      y     R  species concentrations (ppm)
c      rr    R  time rate of change of species concentrations
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
      real      y(MXTRSP+MXSPEC+MXRADCL), rr(MXRX)
      logical   lrr, lpr
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i, j, k, isp, irx, nitr
      real      gain, loss, rtmp
      logical   l1
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c
c ---  calculate rr if prior is not available
c
      if( .NOT. lpr ) then
         do i = 1,nrxnrtc
            irx = irxupdt(i)
            rr(i) = srkrtc(i)
            if( nrct(i) .GT. 0) then   ! this check should be superfluous
               do k = 1,nrct(i)
                     rr(i) = rr(i)*y(idxrct(i,k))
               enddo
            endif
         enddo
      endif
c
c ---  solve equilibrium species y(i)
c      accumulate production terms (gain)
c      accumulate destruction terms (loss) excluding y(i)
c      y(i) = gain/loss
c
      do i = 1,neqmrtc
         isp = ideqm(i)
         gain = 0.0
         do j = 1,nrxgain(i)
            irx = irxgain(i,j)
            rtmp = srkrtc(irx)*spdcoeqm(i,j)
            if( nrct(irx) .GT. 0 ) then
               do k = 1,nrct(irx)
                  rtmp = rtmp*y(idxrct(irx,k))
               enddo
            endif
            gain = gain + rtmp
         enddo
         rtmp = 0.0
         loss = 0.0
         do j = 1,nrxloss(i)
            irx = irxloss(i,j)
            rtmp = srkrtc(irx)
            l1 = .true.
            if( nrct(irx) .GT. 0 ) then
               do k = 1,nrct(irx)
                  if( idxrct(irx,k) .EQ. isp .AND. l1 )  then
                     l1 = .false.
                  else
                     rtmp = rtmp*y(idxrct(irx,k))
                  endif
               enddo
            endif
            loss = loss + rtmp
         enddo
         if( ABS(loss) .GT. 1.0E-25 ) then
            y(isp) = gain/loss
         else
            y(isp) = 0.0
         endif
      enddo
c
      if( .NOT. lrr) return
c
c ---  update rr using new equilibrium species concentrations
c
      do i = 1,nrxupdt
         irx = irxupdt(i)
         rr(irx) = srkrtc(irx)
         if( nrct(irx) .GT. 0) then   ! this check should be superfluous
            do k = 1,nrct(irx)
                  rr(irx) = rr(irx)*y(idxrct(irx,k))
            enddo
         endif
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

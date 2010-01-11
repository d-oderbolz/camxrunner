c*** SRATECMC
c
      subroutine sratecmc(neq, t, y, ydot, nrr, rr)
      use rtcmcchm
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Calculate fast species rates of change (ydot = dy/dt) for 
c     SLSODE in the RTRAC CMC solver
c
c    Copyright 1996-2007
c    ENVIRON International Corporation
c
c    Argument descriptions:
c     Inputs:
c      neq   I  number of fast species (dimension of ydot)
c      t     R  current time
c      y     R  species concentrations
c      nrr   I  dimension of rr
c     Outputs:
c      ydot  R  time rate of change of species concentrations
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
      integer   neq, nrr
      real      t
      real      y(MXTRSP+MXSPEC+MXRADCL), ydot(neq), rr(nrr)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   k, l, n
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      do k = 1,nfstrtc
         ydot(k) = 0.0
      enddo
c
c --- calculate reaction rates
c
      do n = 1,nrxnrtc
         rr(n) = srkrtc(n)
         if( nrct(n) .GT. 0) then
            do k = 1,nrct(n)
               rr(n) = rr(n)*y(idxrct(n,k))
            enddo
         endif
      enddo
c
c --- update any equilibrium species and rr
c
      if( neqmrtc .GT. 0) call seqmcmc(y,rr,.true.,.true.)
c
c --- accumulate ydot terms for reactant loss - only fast species
c
      do n = 1,nrxnrtc
         if( nrctfst(n) .GT. 0) then
            do k = 1,nrctfst(n)
               ydot(idxrctfst(n,k)) = ydot(idxrctfst(n,k)) - rr(n)
            enddo
         endif
c
c --- accumulate ydot terms for product gain  - only fast species
c
         if( nprdfst(n) .GT. 0) then
            do k = 1,nprdfst(n)
               ydot(idxprdfst(n,k)) = ydot(idxprdfst(n,k)) 
     &                                        + rr(n)*prdcofst(n,k)
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

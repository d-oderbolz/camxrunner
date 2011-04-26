c*** RBRATCMC
c
      subroutine rbratcmc(neq,t,y,ydot,rpar,ipar)
      use rtcmcchm
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Calculate fast species rates of change (ydot = dy/dt) for 
c     DLSODE in the RTRAC CMC solver
c
c    Copyright 1996 - 2010
c    ENVIRON International Corporation
c
c    Argument descriptions:
c     Inputs:
c      neq   I  number of fast species (dimension of ydot)
c      t     D  current time
c      y     D  species concentrations
c      rpar  D  space to pass information
c      ipar  I  space to pass information
c     Outputs:
c      ydot  D  time rate of change of species concentrations
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
      integer   neq, ipar
      real*8    t, rate,rpar
      real*8    y(MXTRSP+MXSPEC+MXRADCL), ydot(neq)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c     rr     D  rate of each reaction
c
      integer   k, l, n
      real*8    rr(MXRX)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      do k = 1,nfstrtc
         ydot(k) = 0.0d0
      enddo
c
c --- calculate reaction rates
c
      do n = 1,nrxnrtc
         rr(n) = rkrtc(n)
         if( nrct(n) .GT. 0) then
            do k = 1,nrct(n)
               rr(n) = rr(n)*y(idxrct(n,k))
            enddo
         endif
      enddo
c
c --- update any equilibrium species and rr
c
      if( neqmrtc .GT. 0) call deqmcmc(y,rr,.true.,.true.)
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

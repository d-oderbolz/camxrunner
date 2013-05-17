c*** RBJACCMC
c
      subroutine rbjaccmc(neq,t,y,pd,ldjac,rpar,ipar)
      use rtcmcchm
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Calculate fast species Jacobian (pd = df/dy) for RODAS in the
c     RTRAC CMC solver
c
c     RODAS does NOT initialize pd() to zero
c
c    Copyright 1996 - 2013
c    ENVIRON International Corporation
c
c    Argument descriptions:
c     Inputs:
c      neq   I  number of fast species (second dimension of pd)
c      t     D  current time
c      y     D  species concentrations
c      ldjac I  leading (first) dimension of pd
c      rpar  D  not used
c      ipar  I  not used
c     Outputs:
c      pd    D  Jacobian (df/dy)
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
      integer   neq, ipar, ldjac
      real*8    t, rpar
      real*8    y(MXTRSP+MXSPEC), pd(ldjac,neq)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i, j, k, n
      real*8    tmp
      real*8    rr(1)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- update any equilibrium species but not rr
c
      if( neqmrtc .GT. 0) call deqmcmc(y,rr,.false.,.true.)
c
c --- fill in the Jacobian 
c
      do i = 1,ldjac
         do j = 1,neq
            pd(i,j) = 0.0D0
         enddo
      enddo
c
      do n = 1,njactrm
         tmp = coefjac(n)*rkrtc(idrxjac(n))
         if( nspjac(n) .GT. 0 ) then
            do k = 1,nspjac(n)
               tmp = tmp*y(idspjac(n,k))
            enddo
         endif
            pd(ipd(n),jpd(n)) = pd(ipd(n),jpd(n))+tmp
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

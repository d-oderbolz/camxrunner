c*** RBKDRV
c
      subroutine rbkdrv(dtin,H2O,atm,O2,CH4,H2,conc,rtconc,
     &                  ierr,lpig)
      use chmstry
      use filunit
      use rtcmcchm
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Call the Rosenbrock solver (RODAS) for RTRAC CMC chemistry
c
c
c    Copyright 1996 - 2013
c    ENVIRON International Corporation
c
c     Argument descriptions:
c      Input arguments:
c
c       dtin              time duration to be integrated (hr)
c       H2O               water vapor concentration (ppm)
c       atm               total gas concentration (M) in ppm
c       O2                oxygen concentration (ppm)
c       CH4               methane concentration (ppm)
c       H2                hydrogen concentration (ppm)
c       conc              state species concentrations (ppm)
c       rtconc            RTRAC species concentrations (ppm)
c       lpig              PiG chemistry flag (T=puff chemistry)
c
c      Output arguments:
c       rtconc            RTRAC species concentrations (ppm)
c       ierr              error flag (=1 if RODAS reports error)
c
c     Routines called:
c      RODAS
c      DPREPSLO
c      DSLOCMC
c      DEQMCMC
c      CAMXERR
c
c     Called by:
c      CHEMDRIV
c      PIGDRIVE
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
      include "camx.prm"
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real     dtin, H2O, atm, O2, CH4, H2
      real     conc(MXSPEC+1), rtconc(MXTRSP)
      integer  ierr
      logical  lpig
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      external rbratcmc, rbjaccmc, solout
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
      integer   LDIM, LIW, LRW, ITOL
      parameter (LDIM = MXTRSP)
      parameter (LIW = LDIM + 20)
      parameter (LRW = 2*LDIM*LDIM + 14*LDIM + 20)
      parameter (ITOL = 0)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i, j, izero, ione, mf
      integer   ipar, idid, neq
      integer   iwork(LIW) 
      real*8    datol, drtol
      real*8    t, tout, dstep, dpar, fixed(6+mxspec)
      real*8    y(MXTRSP+MXSPEC), y0(MXTRSP+MXSPEC),
     &          dwork(LRW), rr(MXRX)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ione = 1
      izero = 0
c
c --- Zero the working array elements that can pass options to RODAS
c
      dwork(1) = 0.D0
      dwork(2) = 0.D0
      dwork(3) = 0.D0
      dwork(4) = 0.D0
      dwork(5) = 0.D0
      iwork(1) = 0
      iwork(2) = 0
      iwork(3) = 0
      iwork(9) = 0
      iwork(10) = 0
c
c --- Load concentrations for RODAS
c
      do i = 1,ngasrtc
         y(i) = DBLE( rtconc(i) )
         y0(i) = y(i)
      enddo
c
      if( nfixrtc. GT. 0 ) then
         fixed(1) = atm
         fixed(2) = O2
         fixed(3) = atm-O2
         fixed(4) = H2O
         fixed(5) = H2
         fixed(6) = CH4
         if( ngas. GT. 0 ) then
            do i = 1,ngas
               fixed(6+i) = conc(i)
            enddo
         endif
         do i = 1,nfixrtc
            y(i+ngasrtc) = DBLE( fixed( idxfix(i+ngasrtc)) )
            y0(i+ngasrtc) = y(i+ngasrtc)
         enddo
      endif
c
c --- Set integration parameters for RODAS
c     mf sets Jacobian method: 0 = numeric, 1 = algebraic
c
      mf = ijac-1
      t = 0.0D0 
      tout = DBLE( dtin )
      dstep = tout/1.0D-4
      neq = nfstrtc
      datol = DBLE(atolrtc)
      drtol = DBLE(rtolrtc)
c
c --- Call RODAS to advance the fast species
c     Equilibrium species (if any) are solved within RODAS 
c     Slow species (if any) are advanced using the end-point fast/eqm 
c     concentrations
c
      call rodas(neq,rbratcmc,izero,t,y,tout,dstep,drtol,datol,ITOL,
     &     rbjaccmc,MF,neq,izero,rbratcmc,izero,rbratcmc,izero,izero,
     &     izero,solout,izero,dwork,LRW,iwork,LIW,dpar,ipar,idid)
      if( idid .LT. 0) go to 900 
      if( nslortc .LT. 1) then
        call dprepslo(y,rr)
        call dslocmc(y,rr,DBLE( dtin ))
      endif
      if( neqmrtc .GT. 0) call deqmcmc(y,rr,.false.,.false.)
c
c --- Save species concentrations in CAMx arrays
c
      do i = 1,ngasrtc
         rtconc(i) = MAX(1.0E-18, SNGL( y(i) ))
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
c
c-----------------------------------------------------------------------
c    Error handling:
c-----------------------------------------------------------------------
c
 900  if (lpig) then 
        ierr = 1
        return
      else
        write(iout,'(//,A)') ' ERROR in RBKDRV:'
        write(iout,'(/,A,I3,/,A)') ' RODAS reporting IDID = ',
     &     idid, ' Look up this IDID code in the RODAS source code'
        write(iout,901) dstep, tout, dtin
        write(iout,902) iwork(16),iwork(14),iwork(15)
 901    format(/,'   last step size  = ', D12.3,/
     &           '   current time    = ', D12.3,/
     &           '   total time step = ', D12.3)
 902    format('   # of steps ',i4,' # of f-s ',i4,' # of j-s ',i4)
        write(iout,'(/,A7,A10,2A12)')
     &        'Species','Name','C(t)','C(0)'
        write(iout,'(I5,3X,A,2X,1P2E12.3)')
     &       (j,spnmrt(j),y(j),y0(j),j=1,ngasrtc+nfixrtc)
        call camxerr()
      endif
c
      end 

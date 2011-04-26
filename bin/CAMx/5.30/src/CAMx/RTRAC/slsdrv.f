c*** SLSDRV
c
      subroutine slsdrv(dtin,H2O,atm,O2,CH4,H2,conc,cncrad,rtconc,
     &                  ierr,lpig)
      use chmstry
      use filunit
      use rtcmcchm
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Call the SLSODE solver for RTRAC CMC chemistry
c
c
c    Copyright 1996 - 2010
c    ENVIRON International Corporation
c
c     Argument descriptions:
c      Input arguments:
c       dtin              time duration to be integrated (hr)
c       H2O               water vapor concentration (ppm)
c       atm               total gas concentration (M) in ppm
c       O2                oxygen concentration (ppm)
c       CH4               methane concentration (ppm)
c       H2                hydrogen concentration (ppm)
c       conc              state species concentrations (ppm)
c       cncrad            radical species concentrations (ppm)
c       lpig              PiG chemistry flag (T=puff chemistry)
c
c      Output arguments:
c       conc              state species concentrations (ppm)
c       cncrad            radical species concentrations (ppm)
c       ierr              error flag (=1 if LSODE reports error)
c
c     Routines called:
c      LSODE
c      SPREPSLO
c      SSLOCMC
c      SEQMCMC
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
c --- lsode common block
c
      integer   mesflg, lunit
      common    /ehd001/ mesflg, lunit
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real     dtin, H2O, atm, O2, CH4, H2
      real     conc(MXSPEC+1), cncrad(MXRADCL), rtconc(MXTRSP)
      integer  ierr
      logical  lpig
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      external sratecmc, sjaccmc
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
      integer   LSDIM, LIW, LRW, ITOL
      parameter (LSDIM = MXTRSP)
      parameter (LIW = LSDIM + 20)
      parameter (LRW = LSDIM*LSDIM + 9*LSDIM + 22)
      parameter (ITOL = 1)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i, j, mf
      integer   itask, istate, iopt, neq
      integer   iwork(LIW) 
      real      rrdt(MXRX), rrxn(MXRX)
      real      t, tout, fixed(6+mxspec+mxradcl)
      real      y(MXTRSP+MXSPEC+MXRADCL), y0(MXTRSP+MXSPEC+MXRADCL),
     &          rwork(LRW), rr(MXRX), ydd(LSDIM)
      logical   lrr
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- Optional diagnostic calculation of integrated reaction rates
c
      lrr = .false.
      if (lrr) then
         do i = 1,nrxnrtc
            rrdt(i) = 0.0
         enddo
      endif
c
c --- Zero the working array elements that can pass options to LSODE
c
      do i=1,12
        rwork(i) = 0.0
        iwork(i) = 0
      enddo
c
c --- Load concentrations for LSODE
c
      do i = 1,ngasrtc
         y(i) =  rtconc(i)
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
         if( nrad. GT. 0 ) then
            do i = 1,nrad
               fixed(6+ngas+i) = cncrad(i)
            enddo
         endif
         do i = 1,nfixrtc
            y(i+ngasrtc) = fixed( idxfix(i+ngasrtc))
            y0(i+ngasrtc) = y(i+ngasrtc)
         enddo
      endif
c
c --- Set integration parameters for LSODE
c     mf sets Jacobian method: 21 = numeric, 22 = algebraic
c
      mf = 20 + ijac
      t = 0.0 
      tout = dtin
      itask = 1 
      istate = 1 
      iopt = 0 
      lunit = idiag
      neq = nfstrtc
c
c --- Call LSODE to advance the fast species
c     Equilibrium species (if any) are solved within LSODE 
c     Slow species (if any) are advanced using the mid-point fast/eqm 
c     concentrations
c
      if( nslortc .LT. 1 ) then
        call lsode(sratecmc,neq,y,t,tout,ITOL,rtolrtc,atolrtc,itask,
     1       istate,iopt,rwork,LRW,iwork,LIW,sjaccmc,mf,lrr,nrxnrtc,
     2       rrxn,rrdt,ydd)
        if (istate .lt. 0) go to 900 
      else
        tout = tout/2.0
        call lsode(sratecmc,neq,y,t,tout,ITOL,rtolrtc,atolrtc,itask,
     1       istate,iopt,rwork,LRW,iwork,LIW,sjaccmc,mf,lrr,nrxnrtc,
     2       rrxn,rrdt,ydd)
        if( istate .LT. 0) go to 900 
        call sprepslo(y,rr)
        tout = dtin
        call lsode(sratecmc,neq,y,t,tout,ITOL,rtolrtc,atolrtc,itask,
     1       istate,iopt,rwork,LRW,iwork,LIW,sjaccmc,mf,lrr,nrxnrtc,
     2       rrxn,rrdt,ydd)
        if( istate .LT. 0) go to 900 
        call sslocmc(y,rr,tout)
      endif
      if( neqmrtc .GT. 0) call seqmcmc(y,rr,.false.,.false.)
c
c --- Save species concentrations in CAMx arrays
c
      do i = 1,ngasrtc
         rtconc(i) = MAX(1.0E-18, y(i) )
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
        write(iout,'(//,A)') ' ERROR in SLSDRV:'
        write(iout,'(/,A,I3,/,A)') ' LSODE reporting ISTATE = ',
     &     istate, ' Look up this ISTATE code in the LSODE source code'
        write(iout,901) rwork(11),rwork(12),rwork(13),dtin
        write(iout,902) iwork(12),iwork(13),iwork(11)
 901    format(/,'   last step size  = ', E12.3,/
     &           '   next step size  = ', E12.3,/
     &           '   current time    = ', E12.3,/
     &           '   total time step = ', E12.3)
 902    format('   # of steps ',i4,' # of f-s ',i4,' # of j-s ',i4)
        write(iout,'(/,A7,A10,2A12)')
     &        'Species','Name','C(t)','C(0)'
        write(iout,'(I5,3X,A10,2X,1P2E12.3)')
     &       (j,spnmrt(j),y(j),y0(j),j=1,ngasrtc+nfixrtc)
        call camxerr()
      endif
c
      end 

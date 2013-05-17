      subroutine iehsolv(ierxn,ierate,iejac,ieslow,dtin,
     &                   H2O,atm,O2,CH4,H2,conc,avgcnc,
     &                   nirrrxn,rrxn_irr,ldoirr)
      use chmstry
      use filunit
      implicit none
c
c----CAMx v5.41 121109
c
c     IEHSOLV is the driver for the implicit-explicit hybrid chemistry solver.
c     Steady state for some radicals, LSODE for fast species, first order for
c     slow state species. 
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c     Modifications:
c        none
c
c     Input arguments:
c
c        ierxn             name of the routine which computes the
c                          reaction rates
c        ierate            name of the routine which computes the
c                          species rates for lsode
c        iejac             name of the routine which computes the
c                          Jacobian for lsode
c        ieslow            name of the routine which computes the
c                          slow species rates
c        dtin              time duration to be integrated (hr)
c        H2O               water vapor concentration (ppm)
c        atm               total gas concentration (M) in ppm
c        O2                oxygen concentration (ppm)
c        CH4               methane concentration (ppm)
c        H2                hydrogen concentration (ppm)
c        conc              state species concentrations (ppm)
c        nirrrxn           number of reactions for IPRM
c        ldoirr            flag to calculate IRR data
c
c     Output arguments:
c        conc              state species concentrations (ppm)
c        avgcnc            average state species concentrations (ppm)
c        rrxn_irr          array for reaction data for IPRM
c
c     Routines called:
c        LSODE
c
c     Called by:
c        CHEMDRIV
c
      include "camx.prm"
      include 'chmdbg.inc'
      include 'lsbox.inc'
c
c---- Argument declarations ----
c
      real conc(*)
c
c --- set parameters for LSODE
c
      integer   LSDIM, LIW, LRW, MF, ITOLER, ITASK
      real      DTIEH
      parameter (LSDIM = MXSPEC + 1)
      parameter (LIW = LSDIM + 20)
      parameter (LRW = LSDIM*LSDIM + 9*LSDIM + 22)
      parameter (MF=21, ITOLER=1)
      parameter (ITASK=1)
      parameter (DTIEH = 0.03)
c
c --- arguments
c
      real      dtin, H2O, atm, O2, CH4, H2
      real      avgcnc(MXSPEC+1)
c
c --- local variables
c
      integer i, j, l, nstep, istep, istate, iopt
      integer neq(3)
      real    rtol, atol, rstep, t, tout, dtuse
      real    rwork(LRW)
      integer iwork(LIW)
      real    conc0(MXSPEC+1)
      real    y(LSDIM+5)
      real    yh(LSDIM+5)
      real    yavg(LSDIM)
      real    ydd(LSDIM)
      real    rrxn(MXREACT)
      real    rate(LSDIM)
      real    gain(LSDIM)
      real    loss(LSDIM)
c
c --- lsode common block
c
      integer   mesflg, lunit
      common    /eh0001/ mesflg, lunit
c
c --- external functions
c
      external  ierxn, ierate, iejac, ieslow
c
c========================== Process Analysis Begin ==================================
c
      integer   nirrrxn, irxn
      real      rrxn_irr(*)
      real      yirr(LSDIM+5)
      logical   ldoirr
c
c=========================== Process Analysis End ===================================
c
c
c --- Entry point
c
c --- zero the working array elements that can pass options to LSODE
c     then set the max iterations to 150 and set error tolerances
c
      do i = 5,10
        rwork(i) = 0.
        iwork(i) = 0
      enddo
      iopt = 1
      iwork(6) = 150      ! max iterations
      iwork(7) = 1
      rtol = 5.e-5
      atol = 1.e-7
      istate = 1 
cgy      lunit  = idiag   ! comment out data statement in LSODE if do this
cgy      mesflg  = 1      ! comment out data statement in LSODE if do this
c
c --- pass some values to ierate and iejac
c
      do i = 1,nreact
        rrk(i) = rk(i)
      enddo
      neq(2) = ngas
      neq(3) = nreact
      y(neq(2)+1) = 0.
      y(neq(2)+2) = H2O
      y(neq(2)+3) = atm
      y(neq(2)+4) = O2
      y(neq(2)+5) = CH4 
      y(neq(2)+6) = H2
      do i = neq(2)+1, neq(2)+6
        yh(i) = y(i)
        yirr(i) = y(i)
      enddo
c
 101  continue
c
c --- load concentrations for IEH solver
c
      j = 0
      do i = iessrad+1, nspfst
        j = j+1
        y(j) = conc(i)
        conc0(i) = conc(i)
        yavg(j) = 0.0
      enddo
      neq(1) =  j
      do i = 1,iessrad
        j = j+1
        y(j) = conc(i)
        conc0(i) = conc(i)
        yavg(j) = 0.0
      enddo
      do i = nspfst+1, ngas
        j = j+1
        y(j) = conc(i)
        conc0(i) = conc(i)
        yavg(j) = 0.0
      enddo
c
c---  step size
c
      t = 0.
      nstep = INT((dtin+0.0001)/DTIEH) 
      nstep = MAX(nstep,1)
      dtuse = dtin/nstep
      rwork(6) = dtuse/2.0
c
c --- integrate in steps
c
      do istep = 1, nstep
        do l = nspfst+1, ngas
          yh(l) = y(l)
          yirr(l) = y(l)/2.0
        enddo
c
c --- solve radicals and fast species
c     save concentrations at mid-point
c
        rstep = FLOAT(istep-1) + 0.5
        tout = rstep*dtuse
        call lsode(ierate,neq,y,t,tout,ITOLER,rtol,atol,ITASK, 
     &             istate,iopt,rwork,LRW,iwork,LIW,iejac,MF,
     &             .FALSE.,nirrrxn,rrxn,rrxn_irr,ydd)
        if (istate.lt.0) goto 102
        do l = 1, nspfst
          yh(l) = MAX(0.0, y(l))
          yirr(l) = yh(l)
        enddo
        rstep = rstep + 0.5
        tout = rstep*dtuse
        call lsode(ierate,neq,y,t,tout,ITOLER,rtol,atol,ITASK, 
     &             istate,iopt,rwork,LRW,iwork,LIW,iejac,MF,
     &             .FALSE.,nirrrxn,rrxn,rrxn_irr,ydd)
c
c --- check for errors from LSODE
c
 102    if (istate.eq.-2.or.istate.eq.-3.or.istate.eq.-6) then
           go to 700 
        elseif (istate.eq.-1.or.istate.eq.-4.or.istate.eq.-5) then
           rtol = rtol * 0.1
           atol = MAX(1.e-8, atol * 0.1)
           write(idiag,103) istate, rstep, dtuse
 103       format('Information message from the IEH chemistry solver'/
     +            'LSODE reports istate = ',i3, ' on step ', f3.1,
     +            ' with dtuse = ',f6.3)
           write(idiag,'(a,4i4)') 'igrd, i, j, k = ', 
     +          igrdchm,ichm,jchm,kchm 
           write(idiag,104) rtol*10,rtol,atol*10,atol
 104       format('The tolerances for LSODE will be tightened'/
     +          'Relative Tolerance: ',e10.4,'(old) ',e10.4,'(new)'/
     +          'Absolute Tolerance: ',e10.4,'(old) ',e10.4,'(new)'/
     +          'and the calculation will be repeated.'/)
           istate = 1
           iwork(6) = 1000
           go to 101 
        endif
c
c --- update slow species concentrations
c     save concentrations at mid-point
c
        call ierxn(yh,neq(2),rrxn,rrk,nreact)
        call ieslow(rrxn,rate,gain,loss,nreact,neq(2),
     &              nspfst+1,ngas)
        do l = nspfst+1, ngas
          y(l) = MAX( (y(l) + dtuse*rate(l)), bdnl(l))
          yirr(l) = yirr(l) + y(l)/2.0
          yavg(l) = yavg(l) + yirr(l)/FLOAT(nstep)
        enddo
        do l = 1, nspfst
          yavg(l) = yavg(l) + yirr(l)/FLOAT(nstep)
        enddo
c
c========================== Process Analysis Begin ==================================
c
        if (ldoirr) then
          call ierxn(yirr,neq(2),rrxn,rrk,nreact)
          if( istep.EQ.1) then
             do irxn = 1, nirrrxn
               rrxn_irr(irxn) = dtuse*rrxn(irxn)
             enddo
          else
             do irxn = 1, nirrrxn
               rrxn_irr(irxn) = rrxn_irr(irxn) +
     &                          dtuse*rrxn(irxn)
             enddo
          endif
        endif
c
c=========================== Process Analysis End ===================================
c
      enddo
c
c --- save concentrations
c
 700  j = 0
      do i = iessrad+1, nspfst
        j = j+1
        conc(i) = y(j)
        avgcnc(i) = yavg(j)
      enddo
      do i = 1,iessrad
        j = j+1
        conc(i) = y(j)
        avgcnc(i) = yavg(j)
      enddo
      do i = nspfst+1, ngas
        j = j+1
        conc(i) = y(j)
        avgcnc(i) = yavg(j)
      enddo
      if (istate .lt. 0) go to 900
c
c --- check lower bounds
c
      do i = 1, ngas
        conc(i)   = MAX(conc(i),bdnl(i))
        avgcnc(i) = MAX(avgcnc(i),bdnl(i))
      enddo
c
      return
c
c --- Error handling
c
 900  write(iout,*)
      write(iout,*) ' ERROR reported in IEHSOLV by the LSODE solver'
      write(iout,*) ' LSODE reports ISTATE = ', istate
      write(iout,*) ' look up this error code in the LSODE source code'
      write(iout,901) rwork(11),rwork(12),rwork(13),dtin
      write(iout,903) iwork(12),iwork(13),iwork(11)
      write(iout,*)
      write(iout,*) ' Taking step ',rstep,' with dtuse = ',dtuse
      write(iout,*) 'ldark, temp(K), water = ',
     &            ldchm, tchm, wchm
      write(iout,*) 'M, O2, CH4, H2  = ',
     &            atm, O2, CH4, H2
      write(iout,'(a,i10,3i4)')' igrd, i, j, k =',igrdchm,ichm,jchm,kchm
      write(iout,*) 'No  Name    New Conc  Init Conc '
      do l=1,ngas
        write(iout,905) l, spname(l), conc(l), conc0(l)
      enddo
      write(iout,*) 'No   Rate Con  Rxn Rate'
      do l=1,nreact
        write(iout,910) l, rrk(l), rrxn(l)
      enddo
      write(iout,*)
c
      call camxerr()
c
 901  format('   last step size  = ', e12.3,/
     &       '   next step size  = ', e12.3,/
     &       '   current time    = ', e12.3,/
     &       '   total time step = ', e12.3)
 903  format('   # of steps ',i4,' # of f-s ',i4,' # of j-s ',i4) 
 905  format(i3,2x,a7,1p3e10.3)
 910  format(i3,2x,1p3e10.3)
c
      end 

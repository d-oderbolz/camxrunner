      subroutine lscall(lsrate,dtin,H2O,atm,O2,CH4,H2,conc,
     &                  avgcnc,nirrrxn,rrxn_irr,
     &                  ierr,ldoirr,lpig)
      use chmstry
      use filunit
      implicit none
c
c
c----CAMx v5.41 121109
c
c     LSCALL calls DLSODE for gas phase chemistry
c     DLSODE is a double precision Gear solver
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c     Modifications:
c        8/4/08        Altered error tolerances according to weather
c                      LSODE is called for grid chemistry or PiG
c
c
c     Input arguments:
c
c        lsrate            name of the routine which computes the
c                          reaction rates
c        dtin              time duration to be integrated (hr)
c        H2O               water vapor concentration (ppm)
c        atm               total gas concentration (M) in ppm
c        O2                oxygen concentration (ppm)
c        CH4               methane concentration (ppm)
c        H2                hydrogen concentration (ppm)
c        conc              state species concentrations (ppm)
c        nirrrxn           number of reactions for IPRM
c        ldoirr            logical flag to calculate IRR data
c        lpig              PiG chemistry flag (T=puff chemistry)
c
c     Output arguments:
c        conc              state species concentrations (ppm)
c        avgcnc            average state species concentrations (ppm)
c        rrxn_irr          integrated reaction rates for IRR
c        ierr              error flag (=1 if LSODE reports error)
c
c     Routines called:
c        DLSODE
c        CAMXERR
c
c     Called by:
c        CHEMDRIV
c        IRONCHEM
c
      include "camx.prm"
      include "ddmchm.inc"
      include 'chmdbg.inc'
      include 'lsbox.inc'
c
c-----argument delclarations ---
c
      real    conc(*)
      integer ierr
c
c --- Parameters for LSODE
c
      integer   LSDIM, LIW, LRW, ITOL, MF
      real*8    DATOL, DRTOL
      parameter (LSDIM = MXSPEC + 1)
      parameter (LIW = LSDIM + 20)
      parameter (LRW = LSDIM*LSDIM + 9*LSDIM + 22)
      parameter (ITOL = 1)
      parameter (MF = 22)
c
c --- Variables
c
      integer   i, l
      integer   itask, istate, iopt, neq
      integer   iwork(LIW) 
      real      dtin, H2O, atm, O2, CH4, H2
      real      avgcnc(MXSPEC + 1)
      real*8    t, tout 
      real*8    y(LSDIM), y0(LSDIM), rwork(LRW)
      logical   lpig
c
c --- lsode common block
c
      integer   mesflg, lunit
      common    /ehd001/ mesflg, lunit
c
c --- External subroutines
c     jdum is a dummy routine defined below
c
      external lsrate, jdum
c
c========================== Process Analysis Begin =============================
c
      integer irxn, nirrrxn
      real    rrxn_irr(nirrrxn)
      real*8  rrxn(MXREACT), rrdt(nirrrxn), ydd(LSDIM)
      logical ldoirr
c
c=========================== Process Analysis End ==============================
c
c --- Entry point
c
c
c --- Set error tolerances according to grid or PiG chem
c
      DRTOL = 1.0D-7
      DATOL = 1.0D-10
      if( lpig ) then
        DRTOL = 1.0D-5
        DATOL = 1.0D-7
      endif
c
c --- Fill some double precision values for LSODE
c
      do i = 1,nreact
        dbrk(i) = rk(i)
      enddo
      dH2O = H2O
      dM   = atm
      dO2  = O2
      dCH4 = CH4
      dH2  = H2
      neq  = ngas
c
c --- Load concentrations
c
      do i = 1,ngas
        y(i)  = DBLE(conc(i))
        y0(i) = DBLE(conc(i))
      enddo
      y(neq+1) = 0.0D0 
c
c --- Zero the working array elements that can pass options to LSODE
c
      do i = 1,12
        rwork(i) = 0.0D0
        iwork(i) = 0
      enddo
c
c --- Set variables that control LSODE
c
      t      = 0.0D0 
      itask  = 1 
      istate = 1 
      iopt   = 1 
      iwork(6) = 1000     ! max iterations
cgy      lunit  = idiag   ! comment out data statement in DLSODE if do this
cgy      mesflg  = 1      ! comment out data statement in DLSODE if do this
c
c========================== Process Analysis Begin =============================
c
      if (ldoirr) then
        do irxn = 1, nirrrxn
          rrdt(irxn) = 0.0
        enddo
      endif
c
c=========================== Process Analysis End ==============================
c
c --- Do chemistry for timestep using LSODE
c     Step out of LSODE at dt/2 and capture concentrations
c
      tout   = DBLE(dtin/2.0)
      call dlsode(lsrate,neq,y,t,tout,ITOL,DRTOL,DATOL,itask,istate, 
     1      iopt,rwork,LRW,iwork,LIW,jdum,MF,ldoirr,nirrrxn,rrxn,
     2      rrdt,ydd)
      if (istate .lt. 0) go to 900 
      do i = 1,ngas
        avgcnc(i) = MAX(DBLE(bdnl(i)),y(i))
      enddo
      tout = DBLE(dtin)
      call dlsode(lsrate,neq,y,t,tout,ITOL,DRTOL,DATOL,itask,istate, 
     1      iopt,rwork,LRW,iwork,LIW,jdum,MF,ldoirr,nirrrxn,rrxn,
     2      rrdt,ydd)
      if (istate .lt. 0) go to 900 
c
c========================== Process Analysis Begin =============================
c
      if (ldoirr) then
        do irxn = 1, nirrrxn
          rrxn_irr(irxn) = rrdt(irxn) 
        enddo
      endif
c
c=========================== Process Analysis End ==============================
c
c
c --- Save species concentrations in CAMx arrays
c
      if (lpig) then
        do i = 1,ngas
          conc(i) = y(i)
        enddo
      else
        do i = 1,ngas
          conc(i) = MAX(DBLE(bdnl(i)),y(i))
        enddo
      endif
c
      return
c
c --- Handle error codes from LSODE
c
 900  if (lpig) then 
        ierr = 1
        return
      else
        write(iout,*)
        write(iout,*)' ERROR reported in LSCALL by the LSODE solver'
        write(iout,*)' LSODE reporting ISTATE = ', istate
        write(iout,*)' look up this error code in the LSODE source code'
        write(iout,901) rwork(11),rwork(12),rwork(13),dtin
        write(iout,903) iwork(12),iwork(13),iwork(11)
        write(iout,*)
        write(iout,*) 'ldark, temp(K), water = ',
     &                 ldchm, tchm, wchm
        write(iout,*) 'M, O2, CH4, H2  = ',
     &                 atm, O2, CH4, H2
        if( igrdchm.GT.0 ) then
           write(iout,'(a,i10,3i4)')' igrd, i, j, k =',
     &                             igrdchm,ichm,jchm,kchm
        else
           write(iout,'(a,i10,3i4)')' PiG puff number, i, j =',
     &                            -igrdchm,ichm,jchm
        endif
        write(iout,*) 'No  Name    New Conc    Init Conc '
        do l = 1,ngas
          write(iout,905) l, spname(l), y(l), y0(l)
        enddo
        write(iout,*) 'No   Rate Constant'
        do l = 1,nreact
          write(iout,910) l, rk(l)
        enddo
        write(iout,*)
c
        call camxerr()
c
 905    format(i3,2x,a7,1p3d12.3)
 910    format(i3,2x,1p3e12.3)
 901    format('   last step size  = ', d12.3,/
     &         '   next step size  = ', d12.3,/
     &         '   current time    = ', d12.3,/
     &         '   total time step = ', d12.3)
 903    format('   # of steps ',i4,' # of f-s ',i4,' # of j-s ',i4)
      endif
c
      end 
c
      subroutine jdum (neq, t, y, ml, mu, pd, nrpd)
      use filunit
c
c --- This is a dummy subroutine that should never be called
c
      implicit none
      integer neq, ml, mu, nrpd
      real*8  t, y(neq), pd(nrpd,neq)
      include "camx.prm"
c
      write(iout,'(//,A)') ' ERROR in LSCALL:'
      write(iout,'(A)') ' Set LSODE parameter MF to 22, not 21'
      call camxerr()
c
      return
      end

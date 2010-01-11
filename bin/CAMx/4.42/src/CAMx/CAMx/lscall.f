      subroutine lscall(lsrate,lsjac,dtin,H2O,atm,O2,CH4,H2,conc,
     &                  cncrad,ierr,lpig)
c
c----CAMx v4.42 070603
c
c     LSCALL calls dlsode for gas phase chemistry
c     dlsode is a double precision Gear solver
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Input arguments:
c
c        lsrate            name of the routine which computes the
c                          reaction rates
c        lsjac             name of the routine which computes the
c                          Jacobian
c        dtin              time duration to be integrated (hr)
c        H2O               water vapor concentration (ppm)
c        atm               total gas concentration (M) in ppm
c        O2                oxygen concentration (ppm)
c        CH4               methane concentration (ppm)
c        H2                hydrogen concentration (ppm)
c        conc              state species concentrations (ppm)
c        cncrad            radical species concentrations (ppm)
c        lpig              PiG chemistry flag (T=puff chemistry)
c
c     Output arguments:
c        conc              state species concentrations (ppm)
c        cncrad            radical species concentrations (ppm)
c        ierr              error flag (=1 if LSODE reports error)
c
c     Routines called:
c        DLSODE
c        CAMXERR
c
c     Called by:
c        CHEMDRIV
c
      include "camx.prm"
      include "chmstry.com"
      include "ddmchm.com"
      include "lsbox.com"
      include "filunit.com"
c
c-----Parameters for LSODE
c
      integer   lsdim, liw, lrw, itol, mf, ierr
      logical   lpig
      double precision datol, drtol
      parameter (lsdim = mxspec + mxradcl + 1)
      parameter (liw = lsdim + 20)
      parameter (lrw = lsdim*lsdim + 9*lsdim + 22)
      parameter (itol = 1)
      parameter (datol = 1.0D-7)
      parameter (drtol = 1.0D-10)
      parameter (mf = 22)
c
      real conc(mxspec+1), cncrad(mxradcl)
      real scl
      real rrxn(MXRXN), gain(LSDIM), loss(LSDIM)
      double precision y(lsdim), y0(lsdim), rwork(lrw)
      double precision t, tout 
      integer iwork(liw) 
      integer i, j
c
      external lsrate, lsjac
c
c-----Entry point
c
      do i = 1,lsdim
        do j = 1,lsdim
          jac(i,j) = 0.d0
        enddo
      enddo
c
c-----Fill some double precision values for LSODE
c
      do i=1,mxrxn
        dbrk(i)=rk(i)
      enddo
      dH2O=H2O
      dM=atm
      dO2=O2
      dCH4=CH4
      dH2=H2
      neq = nrad+ngas
c
c-----Conserve nitrogen within the NXOY scheme
c
      scl = conc(kNXOY) / ( cncrad(kNO3) + 2.0*cncrad(kN2O5) )
      cncrad(kNO3)  = scl * cncrad(kNO3)
      cncrad(kN2O5) = scl * cncrad(kN2O5)
c
c-----Load concentrations
c
      do i=1,nrad
        y(i)=dble(cncrad(i))
        y0(i)=dble(cncrad(i))
      enddo
      do i=1, ngas
        y(i+nrad)=dble(conc(i))
        y0(i+nrad)=dble(conc(i))
      enddo
      y(neq+1) = 0.d0 
c
c-----set parameters for call to LSODE
c
      t = 0.d0 
      tout = dble(dtin)
      itask = 1 
      istate = 1 
      iopt = 0 
c
c-----Zero the working array elements that can pass options to LSODE
c
      do i=1,12
        rwork(i) = 0.d0
        iwork(i) = 0
      enddo
c
c-----Call LSODE
c
      call dlsode(lsrate,neq,y,t,tout,itol,drtol,datol,itask,istate, 
     1      iopt,rwork,lrw,iwork,liw,lsjac,mf,rk,rrxn,gain,loss) 
      if (istate .lt. 0) go to 900 
c
c-----Save species concentrations in CAMx arrays
c
      do i=1,nrad
        cncrad(i) = dmax1(dble(bdlrad),y(i))
c       cncrad(i) = y(i)
      enddo
      do i=1,ngas
c       conc(i) = dmax1(dble(bdnl(i)),y(i+nrad))
        conc(i) = y(i+nrad)
      enddo
      conc(kNXOY) = cncrad(kNO3) + 2.0*cncrad(kN2O5)
c
      return
c
c-----Handle error codes from LSODE
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
        write(iout,'(a,i10,3i4)')' igrd, i, j, k =',
     &                             igrdchm,ichm,jchm,kchm
        write(iout,*) 'No  Name    New Conc    Init Conc '
        y(kNXOY) = y(kNO3+nrad) + 2.0*y(kN2O5+nrad)
        y0(kNXOY) = y0(kNO3+nrad) + 2.0*y0(kN2O5+nrad)
        do l=1,ngas
          write(iout,905) l, spname(l), y(l+nrad), y0(l+nrad)
        enddo
        do l=1,nrad
          write(iout,905) l, nmrad(l), y(l), y0(l)
        enddo
        write(iout,*) 'No   Rate Constant'
        do l=1,nreact
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

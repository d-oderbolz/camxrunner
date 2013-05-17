      subroutine ddmebi(hddmjac,nrxn,nfam,njac,dt,H2O,atm,O2,CH4,H2,
     &                  rk,y1,ysen,ierr)
c
c----CAMx v6.00 130506
c
c     DDMEBI advances the 1st order sensitivty coefficients one time
c     step using Backward Euler method.
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     History:
c        06/01/08   --bkoo--       Original development
c
c     Input arguments:
c        hddmjac           name of subroutine to calc full Jacobian
c        nrxn              number of reactions
c        nfam              number of sensitivity families
c        njac              number of gas and radical species (ngas)
c        dt                EBI solver time step (hr)
c        H2O               water vapor concentration (ppm)
c        atm               total gas concentration (M) in ppm
c        O2                oxygen concentration (ppm)
c        CH4               methane concentration (ppm)
c        H2                hydrogen concentration (ppm)
c        rk                reaction rate constant (ppm/hr)
c        y1                concentrations (radical+gas) at t+dt (ppm)
c        ysen              sensitivity (radical+gas) matrix (ppm)
c
c     Output arguments:
c        ysen              sensitivity (radical+gas) matrix (ppm)
c        ierr              error flag from LU decomposition
c
c     Routines called:
c        HDDMJAC
c        SGEFA
c        SGESL
c
c     Called by:
c        EBISOLV
c
      implicit none

      integer  nrxn,nfam,njac,ierr
      real     dt,H2O,atm,O2,CH4,H2
      real     rk(nrxn),y1(njac+1),ysen(nfam,njac)

      external hddmjac

      real     eps
      parameter ( eps = 1.e-25 )

      real     rjac(njac+1,njac+1),aa(njac,njac),bb(njac)
      real     flg

      integer  ipvt(njac)
      integer  i,j,ip
c
c---  Entry point
c
      ierr = 0
      y1(njac+1) = 0.0
c
c---  Get the Jacobian
c
      flg = 1.0   ! keep (effective) 1st-order rxns
      rjac = 0.0  ! initialize the Jacobian
      call hddmjac(njac,nrxn,H2O,atm,O2,CH4,H2,flg,rjac,y1,rk)
c
c---  Fill the coeff matrix
c
      do i = 1, njac
        do j = 1, njac
          aa(i,j) = -dt * rjac(i,j)
        enddo
        aa(i,i) = aa(i,i) + 1.0
      enddo
c
c---  Factor the coeff matrix
c
      call sgefa(aa,njac,njac,ipvt,ierr)
      if (ierr.ne.0) goto 990 ! zero determinant
c
c---  Solve for 1st order sensitivities
c
      do ip = 1, nfam

        do i = 1, njac
          bb(i) = ysen(ip,i)
          if ( ABS(bb(i)) .LT. eps ) bb(i) = 0.0
        enddo

        call sgesl(aa,njac,njac,ipvt,bb,0)

        do i = 1, njac
          if ( ABS(bb(i)) .LT. eps ) bb(i) = 0.0
          ysen(ip,i) = bb(i)
        enddo

      enddo
c
c---  Return point
c
990   return
      end

      subroutine vrtslv(nlay,ii,jj,igrd,dtin,entrn,dilut,depth,conc,
     &                  fluxtop,sens,spec,fc1,fc2,fc3,ldoipts)
c
c----CAMx v4.42 070603
c 
c     VRTSLV performs column mass adjustments due to vertical
c     advection and changes in layer structure that result
c     from the time- and space-varying vertical coordinate system.
c     This version performs the mass adjustments for both the concentrations
c     and the sensitivities.
c     The system is solved using a Crank-Nicholson approach.
c
c     The difference equations are generalized to allow for three options,
c     depending on the value of the parameter "mu":
c        1) Crank-Nicholson (mu=0.5)
c        2) Fully Explicit (mu=0) !May be unstable for input timestep!
c        3) Fully Implicit (mu=1) !Recommended!
c     Upper boundary conditions:
c        This routine accomodates non-zero vertical velocities at the top of
c        the column; the top boundary condition is held in the conc vector
c        at location (nlay+1).
c     Lower boundary conditions:
c        Zero flux is specified at the ground
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Modifications: 
c        12/3/99   This routine is now fully implicit, and calculation
c                  of sub-timesteps has been removed.
c        3/06/06   Revised top BC approach; removed top dummy layer
c 
c     Input arguments: 
c        nlay                number of layers
c        ii                  column index
c        jj                  row index
c        dtin                timestep (s)
c        entrn               entrainment rate (m/s) 
c        dilut               dilution rate (m/s)
c        depth               layer depth (m)
c        conc                species concentration (umol/m3)
c        sens                sensitivity coefficients(umol/m3/parameter unit)
c        spec                species name
c        ldoipts             flag to calculate Process Analysis data
c 
c     Output arguments: 
c        conc                species concentration (umol/m3)
c        fluxtop             flux across the top boundary (umol/(m2*s))
c        sens                sensitivity coefficients(umol/m3/parameter unit)
c        fc1                 conc entrained through bottom of layer (umol/m3)
c        fc2                 conc entrained through top of layer (umol/m3)
c        fc3                 conc diluted by layer expansion/contraction
c                            (umol/m3)
c 
c     Routines Called: 
c        TRDIAG
c 
c     Called by: 
c        ZADVEC 
c
      implicit none
      include "camx.prm"
      include "filunit.com"
c
      integer nlay
c
c===========================DDM Begin=================================
c
      include "tracer.com"
      integer ls,lrr,isen
      real sens((nlay+1)*nddmsp)
c
c     Note:  nddmsp is the total number of DDM parameters for which 
c            sensitivities are calculated.
c
c===========================DDM End===================================
c
c===================== Process Analysis Begin ========================
c
      real fc1(MXLAYA), fc2(MXLAYA), fc3(MXLAYA)
      logical ldoipts
c
c===================== Process Analysis End ==========================
c
      real mu
      parameter (mu=1.0)

      character*10 spec
      integer ii,jj,igrd
      real conc(nlay+1),entrn(nlay),dilut(nlay),depth(nlay)
      real pn(MXLAYA),pnp1(MXLAYA)
      real aa(MXLAYA),bb(MXLAYA),cc(MXLAYA)
      real rr(MXLAYA*(MXTRSP+1))
      real aa1(MXLAYA),bb1(MXLAYA),cc1(MXLAYA)
      real dtin
      real*8 fluxtop
c
      integer nsteps,k,n,kk
      real dt
c
c-----Entry point
c
      dt = dtin
      nsteps = 1
c
c-----Calculate some constants
c
      fluxtop = 0.
      do k = 1,nlay
        pn(k) = (1.0-mu)*dt/depth(k)
        pnp1(k) = mu*dt/depth(k)
      enddo
c
c-----Calculate matrix coefficients A,B & C, and array R for the equation
c        n+1     n+1     n+1   n
c     A*c   + B*c   + C*c   = R
c        k-1     k       k+1
c
c-----If sensitivities are requested, additional right-hand
c     side arrays r are calculated for the equation
c        n+1     n+1     n+1    n
c     A*s   + B*s   + C*s   = r
c        k-1     k       k+1
c     The right-hand side arrays for both the concentration and
c     sensitivity equations are loaded into the same array rr, and 
c     the equations are solved together.
c
      do n = 1,nsteps
c
        do k = 2,nlay-1
          if (n.eq.1) then
            if (entrn(k).ge.0. .and. entrn(k-1).ge.0.) then
              aa1(k) = 0.
              bb1(k) = entrn(k-1) + dilut(k)
              cc1(k) = entrn(k)
            elseif (entrn(k).lt.0. .and. entrn(k-1).lt.0.) then
              aa1(k) = -entrn(k-1)
              bb1(k) = -entrn(k) + dilut(k)
              cc1(k) = 0.
            elseif (entrn(k).ge.0. .and. entrn(k-1).lt.0.) then
              aa1(k) = -entrn(k-1)
              bb1(k) = dilut(k)
              cc1(k) =  entrn(k)
            else
              aa1(k) = 0.
              bb1(k) = entrn(k-1) - entrn(k) + dilut(k)
              cc1(k) = 0.
            endif
            aa(k) = -pnp1(k)*aa1(k) 
            bb(k) = 1. + pnp1(k)*bb1(k)
            cc(k) = -pnp1(k)*cc1(k) 
          endif
          rr(k) = pn(k)*aa1(k)*conc(k-1) + 
     &            (1. - pn(k)*bb1(k))*conc(k) + 
     &            pn(k)*cc1(k)*conc(k+1)
        enddo
c
c-----Lower boundary conditions
c
        if (n.eq.1) then
          aa1(1) = 0.
          if (entrn(1).ge.0.) then
            bb1(1) = dilut(1)
            cc1(1) = entrn(1)
          else
            bb1(1) = -entrn(1) + dilut(1)
            cc1(1) = 0.
          endif
          aa(1) = 0.
          bb(1) = 1. + pnp1(1)*bb1(1)
          cc(1) = -pnp1(1)*cc1(1) 
        endif
        rr(1) = (1. - pn(1)*bb1(1))*conc(1) + 
     &          pn(1)*cc1(1)*conc(2)
c
c-----Upper boundary conditions
c
        k = nlay
        if (n.eq.1) then
          cc1(k) = 0.
          if (entrn(k).ge.0. .and. entrn(k-1).ge.0.) then
            aa1(k) = 0.
            bb1(k) = entrn(k-1) + dilut(k)
            rr(k) = pnp1(k)*entrn(k)*conc(k+1)
          elseif (entrn(k).lt.0. .and. entrn(k-1).lt.0.) then
            aa1(k) = -entrn(k-1)
            bb1(k) = -entrn(k) + dilut(k)
            rr(k) = 0.
          elseif (entrn(k).ge.0. .and. entrn(k-1).lt.0.) then
            aa1(k) = -entrn(k-1)
            bb1(k) = dilut(k)
            rr(k) = pnp1(k)*entrn(k)*conc(k+1)
          else
            aa1(k) = 0.
            bb1(k) = entrn(k-1) - entrn(k) + dilut(k)
            rr(k) = 0.
          endif
          aa(k) = -pnp1(k)*aa1(k)
          bb(k) = 1. + pnp1(k)*bb1(k)
          cc(k) = 0.
        endif
        rr(k) = rr(k) + 
     &          pn(k)*aa1(k)*conc(k-1) +
     &          (1. - pn(k)*bb1(k))*conc(k)
        if (entrn(nlay).ge.0.) then
          fluxtop =  fluxtop - conc(nlay+1)*entrn(nlay)
        else
          fluxtop =  fluxtop - conc(nlay)*entrn(nlay)
        endif
c
c===========================DDM Begin=================================
c
c-----Load additional arrays into rr, one array for each sensitivity
c     coefficient.  Load the lower boundary condition, then layers
c     2 through nlay-1, finally the upper boundary condition.
c
        if (lddm) then
          ls = 0
          lrr = nlay
          do isen = 1,nddmsp
            ls = ls + 1
            lrr = lrr + 1
            rr(lrr) = (1. - pn(1)*bb1(1))*sens(ls) + 
     &                pn(1)*cc1(1)*sens(ls+1)
c
            do k = 2,nlay-1
              ls = ls + 1
              lrr = lrr + 1
              rr(lrr) = pn(k)*aa1(k)*sens(ls-1) + 
     &                  (1. - pn(k)*bb1(k))*sens(ls) + 
     &                  pn(k)*cc1(k)*sens(ls+1)
            enddo
c
            ls = ls + 1
            lrr = lrr + 1
            rr(lrr) = 0.
            if (entrn(nlay).ge.0.) 
     &        rr(lrr) = pnp1(nlay)*entrn(nlay)*sens(ls+1)
            rr(lrr) = rr(lrr) +
     &                pn(nlay)*aa1(nlay)*sens(ls-1) +
     &                (1. - pn(nlay)*bb1(nlay))*sens(ls)
            ls = ls + 1
          enddo
        endif
c
c===========================DDM End===================================
c
c-----Solve the equations
c
        if( lddm ) then
           call trdiag(aa,bb,cc,rr,nlay,nddmsp+1)
        else
           call trdiag(aa,bb,cc,rr,nlay,1)
        endif
c             
        do k = 1,nlay
          if (rr(k).le.0.) then 
            write(iout,'(//,a)') 'ERROR in VRTSLV:'
            write(iout,*) 'Negative concentration ',
     &                    'when doing advection in z-direction'
            write(iout,*) 'Grid: ',igrd
            write(iout,*) 'Location (I,J): ',ii,jj
            write(iout,*) 'Species: ',spec 
            write(iout,'(a,i3,a,i3)')'   Taking ',n,' steps of ',nsteps
            do kk = 1,nlay 
              write(iout,'(i3,6e12.4,f5.1,6f7.3)') kk,rr(kk), 
     &              conc(kk),depth(kk),entrn(kk),dilut(kk)
            enddo 
            call camxerr()
          endif
          conc(k) = rr(k)
        enddo
c
c===================== Process Analysis Begin ========================
c
        if (ldoipts) then
          do k = 1,nlay
            fc3(k) = -pnp1(k)*dilut(k)*rr(k)
          enddo
          do k = 2,nlay
            if (entrn(k).ge.0. .and. entrn(k-1).ge.0.) then
              fc1(k) = -pnp1(k)*entrn(k-1)*conc(k)
              fc2(k) =  pnp1(k)*entrn(k)*conc(k+1)
            elseif (entrn(k).lt.0. .and. entrn(k-1).lt.0.) then
              fc1(k) = -pnp1(k)*entrn(k-1)*conc(k-1)
              fc2(k) =  pnp1(k)*entrn(k)*conc(k)
            elseif (entrn(k).ge.0. .and. entrn(k-1).lt.0.) then
              fc1(k) = -pnp1(k)*entrn(k-1)*conc(k-1)
              fc2(k) =  pnp1(k)*entrn(k)*conc(k+1)
            else
              fc1(k) = -pnp1(k)*entrn(k-1)*conc(k)
              fc2(k) =  pnp1(k)*entrn(k)*conc(k)
            endif
          enddo
          fc1(1) = 0.
          if (entrn(1).ge.0.) then
            fc2(k) =  pnp1(1)*entrn(1)*conc(2)
          else
            fc2(1) =  pnp1(1)*entrn(1)*conc(1)
          endif
        endif
c
c===================== Process Analysis End ==========================
c
c
c===========================DDM Begin=================================
c
        if (lddm) then
          ls = -1
          lrr = nlay 
          do isen = 1,nddmsp
            ls = ls + 1
            do k = 1, nlay
              ls = ls + 1
              lrr = lrr + 1
              sens(ls) = rr(lrr)
            enddo
          enddo
        endif
c
c===========================DDM End===================================
c
      enddo
c
      return
      end

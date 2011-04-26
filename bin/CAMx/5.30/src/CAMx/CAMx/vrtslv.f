      subroutine vrtslv(nlay,ii,jj,igrd,dtin,entrn,dilut,depth,conc,
     &                   lvupsolv,fluxtop,fluxlay,sens,spec,fc1,fc2,
     &                                                    fc3,ldoipts)
      use filunit
      use tracer
c
c----CAMx v5.30 101223
c 
c     VRTSLV performs column mass adjustments due to vertical advection and
c     changes in layer structure that result from the time- and space-varying
c     vertical coordinate system.  This version performs the mass adjustments
c     for both the concentrations and the sensitivities.
c     The system is solved using a fully implicit backward Euler approach in
c     time and a hybrid centered/upstream-donor approach in space.  The latter
c     (upstream) is used if the vertical CFL>1, or if centered difference results in
c     a local negative concentrations locally.
c
c     The difference equations are generalized to allow for three time 
c     differencing options, depending on the value of the parameter "mu":
c        1) Crank-Nicholson semi-implicit (mu=0.5)
c        2) Fully explicit forward Euler (mu=0)
c        3) Fully implicit backward Euler (mu=1) !Current implementation!
c     Upper boundary conditions:
c        This routine accomodates non-zero vertical velocities at the top of
c        the column; the top boundary condition is held in the conc vector
c        at location (nlay+1).
c     Lower boundary conditions:
c        Zero flux is specified at the ground
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c     Modifications: 
c        12/3/99   This routine is now fully implicit, and calculation
c                  of sub-timesteps has been removed.
c        3/06/06   Revised top BC approach; removed top dummy layer
c       07/16/07 -bkoo-     Added check for HDDM
c       07/16/08 -bkoo-     Added DDM turn-off flag
c       11/04/09   Revised for hybrid centered/upstream-donor technique
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
c        lvupsolv            flag to use upstream donor solver
c        sens                sensitivity coefficients(umol/m3/parameter unit)
c        spec                species name
c        ldoipts             flag to calculate Process Analysis data
c 
c     Output arguments: 
c        conc                species concentration (umol/m3)
c        fluxtop             flux across the top boundary (umol/(m2*s))
c        fluxlay             flux across top of each layer (umol/(m2*s))
c        sens                sensitivity coefficients(umol/m3/parameter unit)
c        fc1                 conc entrained through bottom of layer (umol/m3)
c        fc2                 conc entrained through top of layer (umol/m3)
c        fc3                 conc diluted by layer expansion/contraction
c                            (umol/m3)
c 
c     Routines Called: 
c        TRIDIAG
c 
c     Called by: 
c        ZADVEC 
c
      implicit none
      include "camx.prm"
c
      real mu
      parameter (mu = 1.0)

      character*10 spec
      integer nlay
      integer ii,jj,igrd
      real conc(nlay+1),entrn(nlay),dilut(nlay),depth(nlay)
c
      real dtin,deltac
      real*8 fluxtop
c
      integer nsteps,k,kk,ineg,k1,k2
      real dt
      logical lvupsolv(nlay)
c
      real pn(MXLAYER)
      real pnp1(MXLAYER)
      real aa(MXLAYER)
      real bb(MXLAYER)
      real cc(MXLAYER)
      real rr(MXLAYER*(MXTRSP+1))
      real zrp(MXLAYER)
      real zrm(MXLAYER)
c
c===========================DDM Begin=================================
c
      integer ls,lrr,isen
      real sens((nlay+1)*nddmsp)
c
c     Note:  nddmsp is the total number of DDM parameters for which
c            sensitivities are calculated.
c
c===========================DDM End===================================
c
c
c================== Source Apportionmment Begin ======================
c
      real fluxlay(MXLAYER)
c
c=================== Source Apportionment End ========================
c
c
c================== Process Analysis Begin ===========================
c
      logical ldoipts
      real fc1(*)
      real fc2(*)
      real fc3(*)
c
c=================== Process Analysis End ============================
c
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
        pn(k) = (1. - mu)*dt/depth(k)
        pnp1(k) = mu*dt/depth(k)
      enddo
      do k = 1,nlay-2
        zrp(k) = depth(k+1)/(depth(k+1) + depth(k))
        zrm(k) = depth(k)/(depth(k+1) + depth(k))
      enddo
      do k = nlay-1,nlay
        zrp(k) = 0.
        zrm(k) = 1.
        if (entrn(k).lt.0.) then
          zrp(k) = 1.
          zrm(k) = 0.
        endif
      enddo
c
c-----Check upstream donor flag for hybrid case
c
      do k = 1,nlay-2
        if (lvupsolv(k)) then
          zrp(k) = 0.
          zrm(k) = 1.
          if (entrn(k).lt.0.) then
            zrp(k) = 1.
            zrm(k) = 0.
          endif
        endif
      enddo
c
c-----Calculate matrix coefficients A,B & C, and array R for the equation
c        n+1     n+1     n+1   n
c     A*c   + B*c   + C*c   = R
c        k-1     k       k+1
c
 100  continue
c
      do k = 2,nlay-1
        aa(k) = pnp1(k)*zrp(k-1)*entrn(k-1)
        bb(k) = 1. + pnp1(k)*
     &          (zrm(k-1)*entrn(k-1) - zrp(k)*entrn(k) + dilut(k))
        cc(k) = -pnp1(k)*zrm(k)*entrn(k)
        rr(k) = -pn(k)*zrp(k-1)*entrn(k-1)*conc(k-1) +
     &          conc(k)*(1. - pn(k)*
     &          (zrm(k-1)*entrn(k-1) - zrp(k)*entrn(k) + dilut(k))) +
     &          pn(k)*zrm(k)*entrn(k)*conc(k+1)
      enddo
c
c-----Lower boundary conditions
c
      k = 1
      aa(k) = 0.
      bb(k) = 1. + pnp1(k)*(-zrp(k)*entrn(k) + dilut(k))
      cc(k) = -pnp1(k)*zrm(k)*entrn(k)
      rr(k) = conc(k)*(1. - pn(k)*(-zrp(k)*entrn(k) + dilut(k))) +
     &        pn(k)*zrm(k)*entrn(k)*conc(k+1)
c
c-----Upper boundary conditions
c
      k = nlay
      aa(k) = pnp1(k)*zrp(k-1)*entrn(k-1)
      bb(k) = 1. + pnp1(k)*
     &        (zrm(k-1)*entrn(k-1) - zrp(k)*entrn(k) + dilut(k))
      cc(k) = 0.
      rr(k) = -pn(k)*zrp(k-1)*entrn(k-1)*conc(k-1) +
     &        conc(k)*(1. - pn(k)*
     &        (zrm(k-1)*entrn(k-1) - zrp(k)*entrn(k) + dilut(k))) +
     &        zrm(k)*entrn(k)*conc(k+1)*dt/depth(k)
c
c============================DDM Begin================================
c
c-----If sensitivities are requested, load additional arrays into rr,
c     one array for each sensitivity coefficient.  Additional right-hand
c     side arrays rr are calculated for the equation
c        n+1     n+1     n+1    n
c     A*s   + B*s   + C*s   = r
c        k-1     k       k+1
c     The right-hand side arrays for both the concentration and
c     sensitivity equations are loaded into the same array rr, and
c     the equations are solved together.
c
c     Load the lower boundary condition, then layers
c     2 through nlay-1, finally the upper boundary condition.
c
        if( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
          ls = 0
          lrr = nlay
          do isen = 1,nddmsp
            ls = ls + 1
            lrr = lrr + 1
            k = 1
            rr(lrr) = sens(ls)*
     &                (1. - pn(k)*(-zrp(k)*entrn(k) + dilut(k))) +
     &                pn(k)*zrm(k)*entrn(k)*sens(ls+1)
            do k = 2,nlay-1
              ls = ls + 1
              lrr = lrr + 1
              rr(lrr) = -pn(k)*zrp(k-1)*entrn(k-1)*sens(ls-1) +
     &                  sens(ls)*(1. - pn(k)*
     &                           (zrm(k-1)*entrn(k-1) -
     &                            zrp(k)*entrn(k) +
     &                            dilut(k))) +
     &                  pn(k)*zrm(k)*entrn(k)*sens(ls+1)
            enddo
 
            ls = ls + 1
            lrr = lrr + 1
            k = nlay
            rr(lrr) = -pn(k)*zrp(k-1)*entrn(k-1)*sens(ls-1) +
     &                sens(ls)*(1. - pn(k)*
     &                         (zrm(k-1)*entrn(k-1) - 
     &                          zrp(k)*entrn(k) + 
     &                          dilut(k))) +
     &                zrm(k)*entrn(k)*sens(ls+1)*dt/depth(k)
            ls = ls + 1
          enddo
        endif
c
c=============================DDM End=================================
c
c-----Solve the equations
c
      if( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
         call tridiag(aa,bb,cc,rr,nlay,nddmsp+1)
      else
         call tridiag(aa,bb,cc,rr,nlay,1)
      endif
c
      if (nsteps.lt.nlay) then
        ineg = 0
        do k = 1,nlay
          deltac = (rr(k) - conc(k))/conc(k)
          if (deltac.le.-0.99) then
            if( k.gt.1 ) then
              if( entrn(k-1).gt.0.) then
                zrp(k-1) = 0.
                zrm(k-1) = 1.
              endif
            endif
            if (entrn(k).lt.0.) then
              zrp(k) = 1.
              zrm(k) = 0.
            endif
            ineg = 1
          endif
        enddo
        if (ineg.eq.1) then
          nsteps = nsteps + 1
          goto 100
        endif
      endif
c
c===================== Source Apportionment Begin =====================
c
      fluxlay(1) = -(rr(1) - conc(1))*depth(1)/dt
      do k = 2,nlay
        fluxlay(k) = -(rr(k) - conc(k))*depth(k)/dt + fluxlay(k-1)
      enddo
c
c====================== Source Apportionment End ======================
c
c
c===================== Process Analysis Begin =========================
c
      if (ldoipts) then
        do k = 1,nlay
          fc3(k) = -dt/depth(k)*dilut(k)*conc(k)
        enddo
        fc1(1) = 0.
        do k = 2,nlay
          fc1(k) = fluxlay(k-1)/depth(k)*dt
        enddo
        do k = 1,nlay
          fc2(k) = -fluxlay(k)/depth(k)*dt
        enddo
      endif
c
c====================== Process Analysis End ==========================
c
      fluxtop =  fluxlay(nlay)
      do k = 1,nlay
        if (rr(k).le.0.) then 
          write(iout,'(//,a)') 'ERROR in VRTSLV:'
          write(iout,*) 'Negative concentration ',
     &                  'when doing advection in z-direction'
          write(iout,*) 'Grid: ',igrd
          write(iout,*) 'Location (I,J,K): ',ii,jj,k
          write(iout,*) 'Species: ',spec 
          write(iout,'(a,i3,a,i3)')'Number of steps: ',nsteps
          do kk = 1,nlay 
            write(iout,'(i3,6e12.4,f5.1,6f7.3)') kk,rr(kk), 
     &            conc(kk),depth(kk),entrn(kk),dilut(kk)
          enddo 
          call camxerr()
        endif
        conc(k) = rr(k)
      enddo
c
c==============================DDM Begin===============================
c
      if( (lddm .OR. lhddm) .AND. lddmcalc(igrd) ) then
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
c===============================DDM End================================
c
      return
      end

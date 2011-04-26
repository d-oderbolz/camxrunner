      subroutine vdiffimp(ns,nn,dt,vdep,depth,rho,rkv,con,nparddm,
     &                    fluxbot,fcup,fcdn,ldoipts)
      use procan
c
c----CAMx v5.30 101223
c
c     VDIFFIMP performs vertical diffusion of concentrations using 
c     an implicit method, where a tri-diagonal matrix is solved.
c     This version also performs vertical diffusion of sensitivities
c     if DDM is enabled.
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c          
c     Modifications:
c        4/17/00   Revised diffusion equations to weight fluxes by density
c       10/10/08   Revised to move species loop into this routine for
c                  better efficiency
c
c     Input arguments:
c        ns                number of species
c        nn                number of layers
c        dt                time step (s)
c        vdep              deposition velocity (m/s)
c        depth             layer depth (m)
c        rho               atmospheric density (mb/K)
c        rkv               vertical diffusion coefficient (mb m2/s/K)
c        con               species concentrations (umol/m3) followed by 
c                          sensitivities (umol/m3/parameter unit).
c        nparddm           number of parameters for which sensitivities
c                          are calculated.  Should be zero if DDM is not
c                          enabled.
c        ldoipts           flag to calculate data for Process Analysis
c
c     Output arguments:
c        con               species concentrations (umol/m3) followed by
c                          sensitivities (umol/m3/parameter unit)
c        fcup              change in layer concentration due to flux across
c                          upper interface (umol/m3) -- FOR Process Analysis
c        fcdn              change in layer concentration due to flux across
c                          lower interface (umol/m3) -- FOR Process Analysis
c        fluxbot           Concentration removed by dry deposition (umol/m3)
c
c     Routines Called:
c        TRIDIAG
c
c     Called by:
c        DIFFUS
c
      implicit none
      include "camx.prm"
c
      integer ns,nn,nparddm
      real dt
      real vdep(ns)
      real con(MXLAYER+MXLAYER*MXTRSP,ns)
      real depth(nn)
      real rkv(nn)
      real rho(nn)
      real*8 fluxbot(ns)
c
c======================== Process Analysis Begin ====================================
c
      logical ldoipts
c
      real aa_old(MXLAYER)
      real cc_old(MXLAYER)
      real fcup(MXLAYER,MXSPEC)
      real fcdn(MXLAYER,MXSPEC)
c
c========================= Process Analysis End =====================================
c
      integer k,l
      real rr(MXLAYER+MXLAYER*MXTRSP)
      real aa(MXLAYER)
      real bb(MXLAYER)
      real cc(MXLAYER)
c
c-----Entry point
c
      do l = 1,ns
        do k = 1,nn+nn*nparddm
          rr(k) = con(k,l)
        enddo
c
c-----Lower boundary condition
c
        aa(1) = 0.
        bb(1) = 1. + dt/depth(1)*
     &               (vdep(l) + 2.*rkv(1)/(depth(2)+depth(1))/rho(1))
c
c-----Upper boundary condition
c
        cc(nn) = 0.
        bb(nn) = 1. + dt/depth(nn)*
     &                2.*rkv(nn-1)/(depth(nn-1)+depth(nn))/rho(nn)
c
        do k = 2,nn
         aa(k) = -dt/depth(k)*2.*rkv(k-1)/(depth(k-1)+depth(k))/rho(k-1)
        enddo
        do k = 1,nn-1
         cc(k) = -dt/depth(k)*2.*rkv(k)/(depth(k+1)+depth(k))/rho(k+1)
        enddo
        do k = 2,nn-1
          bb(k) = 1.
     &            + dt/depth(k)*2.*rkv(k-1)/(depth(k-1)+depth(k))/rho(k)
     &            + dt/depth(k)*2.*rkv(k)/(depth(k+1)+depth(k))/rho(k)
        enddo
c
c======================== Process Analysis Begin ====================================
c
        if (ldoipts) then
          do k = 1,nn
            aa_old(k) = 0.
            cc_old(k) = 0.
            if (k.gt.1)  aa_old(k) = aa(k)*rho(k-1)
            if (k.lt.nn) cc_old(k) = cc(k)*rho(k+1)
          enddo
        endif
c 
c========================= Process Analysis End =====================================
c
c
c-----Solve the equations
c
        call tridiag(aa,bb,cc,rr,nn,1+nparddm)
c
c
c======================== Process Analysis Begin ====================================
c
        if (ldoipts) then
          do k = 2,nn-1
            fcup(k,l) = (rr(k)/rho(k) - rr(k+1)/rho(k+1))*cc_old(k)
            fcdn(k,l) = (rr(k)/rho(k) - rr(k-1)/rho(k-1))*aa_old(k)
          enddo
c
c-----Lower boundary
c
          fcup(1,l) = (rr(1)/rho(1) - rr(2)/rho(2))*cc_old(1)
          fcdn(1,l) = -rr(1)*dt/depth(1)*vdep(l)
c
c-----Upper boundary
c
          fcup(nn,l) =  0.0
          fcdn(nn,l) =  (rr(nn)/rho(nn) - rr(nn-1)/rho(nn-1))*aa_old(nn)
        endif
c 
c========================= Process Analysis End =====================================
c
        do k = 1,nn+nn*nparddm
          con(k,l) = rr(k)
        enddo
        fluxbot(l) = -con(1,l)*vdep(l)*dt/depth(1)
      enddo
c
      return
      end

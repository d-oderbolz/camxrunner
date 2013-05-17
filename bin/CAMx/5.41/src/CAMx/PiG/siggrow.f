C**** SIGGROW
c
      subroutine siggrow(delt,height,pbl,ustar,wstar,lstable,rkzeq,
     &                   htfms,htfmb,vtfms,vtfmb,shrflg,hshear,vshear,
     &                   pufang,shrang,sigy,sigx,sigz)
      implicit none
c
c----CAMx v5.41 121109
c
c     SIGGROW advances PiG puff spread (sigma) for one timestep 
c     due to turbulent diffusion.  Approach based on SCIPUFF (EPRI,2002)
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c     Modifications:
c        06/17/09        Restructured puff sigmas to improve impacts of
c                        vertical directional shear on puff growth
c
c     Input arguments:
c        delt                timestep (s)
c        height              puff height (m)
c        pbl                 PBL height (m)
c        ustar               friction velocity (m/s)
c        wstar               convective velocity scale (m/s)
c        lstable             flag for PBL stability (T=stable)
c        rkzeq               equilibrium vertical diffusivity (m2/s)
c        htfms               horiz turb flux moment, shear (m2/s)
c        htfmb               horiz turb flux moment, buoyancy (m2/s)
c        vtfms               vert turb flux moment, shear (m2/s)
c        vtfmb               vert turb flux moment, buoyancy (m2/s)
c        shrflg              shear flag:
c                               0 = shear not applied;
c                               1 = shear applied during neutral/unstable;
c                               2 = shear applied always
c        hshear              grid-resolved horz shear of horz wind (1/s)
c        vshear              grid-resolved vert shear of horz wind (1/s)
c        pufang              puff directional angle (rad)
c        shrang              shear directional angle (rad)
c        sigy                lateral puff spread (m)
c        sigx                longitudinal puff spread (m)
c        sigz                vertical puff spread (m)
c
c     Output arguments:
c        sigy                lateral puff spread (m)
c        sigx                longitudinal puff spread (m)
c        sigz                vertical puff spread (m)
c
c     Subroutines called:
c        none
c
c     Called by:
c        PIGGROW
c 
      integer shrflg

      real delt,height,pbl,ustar,wstar,rkzeq,hshear,vshear
      real htfms,htfmb,vtfms,vtfmb
      real pufang,shrang,vshearx,vsheary
      real sigy,sigx,sigz
      logical lstable
c
      real htlss,htlsb,vtls,q2s,q2b,rkx,rkz
      real aa,xpbl,pbleq
c
      data aa /0.75/
      data pbleq /1000./
c
c-----Entry point
c
cae   htlss = 300.
      htlss = 1000.
      htlsb = 1.
      vtls = 10.
      rkzeq = amax1(rkzeq,0.1)

      xpbl = pbl
      if (height.gt.pbl) then
cae     xpbl = amax1(pbleq,pbl)
        xpbl = amax1(100.,pbl)
        lstable = .true.
      endif
c
c-----Calculate horizontal diffusivity
c
cae   q2s = 0.10
      q2s = 0.25
      q2b = 0.
      if (height.le.xpbl) then
        htlss = 1./((0.3*xpbl)**2) + 1./((0.65*height)**2)
        htlss = sqrt(1./htlss)
        q2s = amax1(q2s,2.5*(1. - height/xpbl)*ustar**2)
        if (.not.lstable) then
          htlsb = 0.3*xpbl
          q2b = 0.13*(1. + 1.5*exp(height/xpbl))*wstar**2
        endif
      endif

      htfms = (htfms + delt*q2s)/(1. + delt*aa*sqrt(q2s)/htlss)
      htfms = amax1(htfms,1.)
      htfmb = (htfmb + delt*q2b)/(1. + delt*aa*sqrt(q2b)/htlsb)
      rkx = htfms + htfmb
c
c-----Calculate vertical diffusivity
c
      q2s = 0.01  
      q2b = 0.
      if (height.le.xpbl) then
        vtls = htlss
        q2s = amax1(q2s,1.5*(1. - height/xpbl)*ustar**2)
        if (.not.lstable) then
          q2b = 1.1*(height/xpbl)**(2./3.)*
     &              (1.05 - height/xpbl)*wstar**2
        endif
      endif

      vtfms = (vtfms + delt*aa*sqrt(q2s)*rkzeq/vtls) /
     &        (1. + delt*aa*sqrt(q2s)/vtls)
      vtfms = amax1(vtfms,0.1)
      vtfmb = (vtfmb + delt*aa*sqrt(q2b)*rkzeq/vtls) /
     &        (1. + delt*aa*sqrt(q2b)/vtls)
      rkz = vtfms + vtfmb
c
c-----Solve for new sigmas
c
      if (shrflg.eq.0 .or. (shrflg.eq.1 .and. lstable)) then
        vshear = 0.
        hshear = 0.
      endif
      vshearx = vshear*abs(cos(pufang - shrang))
      vsheary = vshear*abs(sin(pufang - shrang))
      sigy = sqrt(sigy*sigy + 2.*delt*
     &           (sigy*sigy*hshear + sigy*sigz*vsheary + rkx))
      sigx = sqrt(sigx*sigx + 2.*delt*
     &           (sigx*sigx*hshear + sigx*sigz*vshearx + rkx))
      sigz = sqrt(sigz*sigz + 2.*delt*rkz) 
      sigy = amax1(sigy,sigz)
      sigx = amax1(sigx,sigz)
c
      return
      end

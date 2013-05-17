      subroutine ve_gas_rt(ilu,z0,deltaz,psih,ustar,diffrat,henry,
     &                     rscale,tsurf,u10,ve)
c
c----CAMx v5.41 121109
c
c     VE_GAS_RT calculates an exchange/re-emission velocity for a specific gas 
c     species, grid cell, and land use category. 
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c 
c     Modifications: 
c        12/08/09   ---jjung---    Original development
c
c     Input arguments: 
c        ilu                 land use index
c        z0                  surface roughness length (m)
c        deltaz              Layer 1 midpoint height (m)
c        psih                similarity stability correction term 
c        ustar               friction velocity (m/s)
c        diffrat             ratio of molecular diffusivity of water to species
c        henry               Henry's Law constant (M/atm)
c        rscale              user-defined surface resistance scaling factor
c        tsurf               surface temperature (K)
c        u10                 Wind speed at 10 m reference height (m/s)
c      
c     Output arguments: 
c        ve                  re-emission velocity (m/s)
c      
c     Routines called: 
c        none 
c      
c     Called by: 
c        REEMISRT
c 
      implicit none

      integer ilu
      real z0,deltaz,psih,ustar,diffrat,henry,rscale,tsurf,u10,ve
c
      real kwCO2
      real MwCO2,MwH2O
      real ka,Kaw,kw
      real vk,rmin,d1,d2,vair,diffh2o,rconst
      real ra,rd,re,rs
      real schmidt
c
      data vk/0.4/, rmin/1.0/, d1/2./, d2/0.667/
      data vair/1.5e-5/, diffh2o/2.3e-5/
      data MwCO2/44.0/, MwH2O/18.0/
      data rconst /8.206e-2/          ! gas constant (l.atm/mol.K)
c
c-----Entry point
c
c-----Compute atmospheric resistance, RA
c
      ra = (alog(deltaz/z0) - psih)/(vk*ustar)
      ra = amax1(ra,rmin)
c
c-----Compute the deposition layer resistance, RD
c
      schmidt = vair*diffrat/diffh2o
      rd = d1*schmidt**d2/(vk*ustar)
      rd = amax1(rd,rmin)
c
c-----Surface exchange resistance is determined only for water, RE
c
      if (ilu.ne.7) then
        re = 0.0
        goto 100
      endif
c
c-----Compute water side transfer coefficient (kw),
c     air side transfer coefficient (ka), Henry's Law (Kaw)
c
      kwCO2 = 0.45*u10**1.64
      kw = kwCO2*(MwCO2/MwH2O)**(1./4.)*sqrt(1./diffrat)
c
      ka = 1.0e-3 + 4.62*1.0e-4*sqrt(6.1+0.63*u10)*u10*
     &     (schmidt**(-0.67))
c
      Kaw = 1./(henry*rconst*tsurf)
c
c-----Compute the exchange resistance between air and water interface
c
      re = 1./ka + Kaw/kw
c
c-----Compute the surface exchange resistance over water, RS
c
      rs = 1./(3.9e-5*henry*ustar*tsurf)
      rs = amax1(rs,rmin)
      rs = rs*rscale
      re = amax1(rs,re)
c
c-----Final deposition velocity for this cell, land use, and species
c
 100  continue
      ve = 1./(ra + rd + re)
c
      return
      end

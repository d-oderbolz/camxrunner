      subroutine scavrat(laero,lcloud,lgraupl,tamin,pmm,temp,cwat,
     &                   rhoair,conc,hlaw,difrat,rscale,prtdia,rhoprt,
     &                   gscav,ascav,drpvel)
c
c----CAMx v6.00 130506
c 
c     SCAVRAT calculates wet scavenging rates for gases and aerosols.
c     Rates are determined for:
c       1) Uptake of cloud water with dissolved gasses
c       2) Uptake of ambient gasses into precip
c       3) Uptake of cloud water with PM (all PM in cloudy layers is assumed
c          to reside in cloud water)
c       4) Uptake of ambient PM into precip, dependent on particle size and 
c          ice form
c     Super-cooled liquid cloud water is assumed to exist in the temperature
c     range tamin < T < 273K using a ramp function (100% liquid at 273 to 
c     0% liquid at tamin).
c 
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c           
c     Modifications:
c        02/11/04   Updated empirical rainfall relationships
c        05/03/05   Revised gas scavenging calculations
c        06/20/05   Revised to handle liquid/frozen cloud and precip water
c        10/21/05   Henry's Law calculation moved out to WETDEP
c        12/22/09   Simplified characterization of "gscav"
c
c     Input arguments:
c        laero               aerosol flag
c        lcloud              in-cloud flag (F=no cloud water)
c        lgraupl             graupel frozen precip flag
c        tamin               minimum temperature for liquid cloud water
c        pmm                 precip rate (mm/hr)
c        temp                temperature (K)
c        cwat                cloud water content (g/m3)
c        rhoair              atmospheric density (kg/m3)
c        conc                cell gas concentration (umol/m3)
c        hlaw                Henry's Law constant (M/M)
c        difrat              Ratio of H2O to gas diffusivity
c        prtdia              mean aerosol size (m)
c        rhoprt              aerosol density (g/m3)
c             
c     Output arguments: 
c        gscav               Gas scavenging rate (1/s)
c        ascav               Aerosol scavenging rate (1/s)
c             
c     Routines called: 
c        none
c             
c     Called by: 
c        WETDEP
c 
      implicit none
      real tamin,pmm,temp,cwat,rhoair,conc,difrat,rscale,
     &     prtdia,rhoprt,hlaw,gscav,ascav
      logical laero,lcloud,lgraupl
c
      real pi,rhoh2o,difh2o,boltz,xmfp,cldeff,dscav,
     &     drpdia,drpvel,cscav,cgas,caq,diff,term1,term2,
     &     reynold,power,scf,difbrwn,schmidt,stoke,top,bot,star,
     &     terma,termb,phi,term3,eff,drpmas
      real nuair,muair,muh2o,kc
c
c-----Constants
c
      data pi /3.1415927/
      data rhoh2o /1.e6/     !g/m3
      data difh2o /2.3e-5/   !m2/s
      data muair /1.8e-5/    !kg/ms
      data muh2o /1.e-3/     !kg/ms
      data boltz /1.38e-23/  !J/K
      data xmfp /6.5e-8/     !m
      data cldeff /0.9/
c
c-----Entry point
c
      dscav = 0.
      gscav = 0.
      ascav = 0.
      eff   = 1.
c
c-----Calculate environmental parameters
c
      nuair = muair/rhoair                 !air molecular diffusivity (m2/s)
      drpdia = 9.0e-4*(pmm**0.21)          !rain drop diameter (m)
      drpmas = 1.e6*(pi/6.)*(drpdia)**3    !rain drop mass (g)
      if (temp.gt.273.) then
        drpvel = 3.1e3*drpdia              !rain drop fall speed (m/s)
      elseif (lgraupl) then
        drpdia = (17.*1000.*drpmas)**0.38  !graupel diameter (mm)
        drpvel = 1.10*drpdia**0.61         !graupel fall speed (m/s)
        drpdia = 1.e-3*drpdia              !(m)
      else
        drpdia = (29.*1000.*drpmas)**0.56  !snow diameter (mm)
        drpvel = 0.83*drpdia**0.20         !snow fall speed (m/s)
        drpdia = 1.e-3*drpdia              !(m)
      endif
      cscav = 4.2e-7*pmm*cldeff/drpdia     !cloud scavenging rate (1/s)

      if (laero) goto 1000 
c
c-----Gas scavenging
c
      if (temp.le.tamin) return
      cgas = conc
      caq  = 0.
c
c-----If in cloud, partition total gas into equilibrium aqueous and gas phase,
c     and calculate scavenging rate for dissolved gasses in cloud water
c
      if (lcloud) then
        cgas = conc/(1. + hlaw*cwat/rhoh2o)
        caq  = conc - cgas
        dscav = cscav*caq/conc
      endif
c
c-----Calculate scavenging rate for ambient gas dissolving into precip
c
      if (temp.gt.273. .or. (temp.le.273. .and. rscale.eq.0.)) then
        diff  = difh2o/difrat
        term1 = (drpvel*drpdia/nuair)**0.5
        term2 = (nuair/diff)**0.333
        kc    = diff/drpdia*(2. + 0.6*term1*term2)
        gscav = 1.67e-6*pmm*kc/(drpdia*drpvel)
      endif
      gscav = gscav + dscav
c
      return
c
c-----Aerosol scavenging
c
 1000 continue
c
c-----Scavenging rate for aerosol in cloud water is set equal to
c     cloud water scavenging rate
c
      if (lcloud) then
        ascav = cscav
      else
c
c-----Calculate scavenging rate of dry aerosols below cloud as f(size)
c
        reynold = drpdia*drpvel/(2.*nuair)
        power = amin1(7.6,0.55*prtdia/xmfp)
        scf = 1. + (2.514 + 0.8*exp(-power))*xmfp/prtdia
        difbrwn = boltz*temp*scf/(3.*pi*muair*prtdia)
        schmidt = nuair/difbrwn
        stoke = drpvel*prtdia*prtdia*rhoprt*scf/(9000.*muair*drpdia)

        top = 1.2 + alog(1. + reynold)/12.
        bot = 1.0 + alog(1. + reynold)
        star = amin1(top/bot,stoke)

        terma = reynold**0.5 * schmidt**0.333
        termb = reynold**0.5 * schmidt**0.5
        term1 = 4./(reynold*schmidt)*(1. + 0.4*terma + 0.16*termb)
        phi = prtdia/drpdia
        term2 = 4.*phi*(muair/muh2o + (1. + 2.*reynold**0.5)*phi)
        term3 = (stoke - star)/(stoke - star + 2./3.)
        term3 = term3**1.5
        eff = term1 + term2 + term3
        if (temp.le.273. .and. .not.lgraupl) eff = amax1(eff,1.e-3)

        ascav = cscav*eff/cldeff
      endif

      return
      end

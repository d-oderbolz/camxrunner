      subroutine henryfnc(ispc,hlaw0,tfact,temp,ph,knh3,khno3,kso2,hlaw)
c
c----CAMx v5.41 121109
c 
c     HENRYFNC calculates temperature and dissociation adjustments to
c     baseline Henry's Law constants. 
c 
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c           
c     Modifications:
c        None
c
c     Input arguments:
c        ispc                Species index
c        hlaw0               Baseline Henry's Law constant @298K (M/atm)
c        tfact               temperature factor
c        temp                ambient temperature (K)
c        ph                  pH of liquid
c        knh3                pointer to NH3
c        khno3               pointer to HNO3
c        kso2                pointer to SO2
c             
c     Output arguments: 
c        hlaw                Adjusted Henry's Law constant (M/atm)
c             
c     Routines called: 
c        None
c             
c     Called by: 
c        WETDEP
c        DRYDEP
c 
      implicit none
      integer ispc,knh3,khno3,kso2
      real hlaw0,tfact,temp,ph,hlaw

      real diss1,diss2
c
      hlaw = hlaw0*exp(tfact*(1./298. - 1./temp))
      if (ispc.eq.knh3) then
        diss1 = 10.**(-189.1/temp - 4.117)
        diss2 = 10.**(-5839.5/temp - 9.7618*alog(temp) + 61.206)
        hlaw = hlaw*(1. + (diss1/diss2)*10.**(-ph))
      elseif (ispc.eq.khno3) then
        diss1 = 15.4
        hlaw = hlaw*(1. + diss1/(10.**(-ph)))
      elseif (ispc.eq.kso2) then
        diss1 = 10.**(853./temp)/54950.
        diss2 = 10.**(621.9/temp)/1.897e+9
        hlaw = hlaw*(1. + diss1/(10.**(-ph)) + 
     &                    diss1*diss2/(10.**(-2.*ph)))
      endif

      return
      end

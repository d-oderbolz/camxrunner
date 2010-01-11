C**** NOXCHEM
c
      subroutine noxchem(dt,bdnlo3,bdnlno,cno,cno2,co3,rnono)
      implicit none
c
c----CAMx v5.10 090918
c
c     NOXCHEM updates NOx and O3 concentrations in very highly concentrated
c     NOx puffs. It applies the NO + NO self-reaction and titrates O3 against 
c     NO. The titration is essentially complete because NO >> O3.
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Modifications:
c        none
c
c     Input arguments:
c        dt                  time step (s)
c        bdnlo3              lower bound ozone concentration (ppm)
c        bdnlno              lower bound NO concentration (ppm)
c        cno                 NO concentration (ppm)
c        cno2                NO2 concentration (ppm)
c        co3                 O3 concentration (ppm)
c        rnono               NO self-reaction rate constant (ppm-1 hr-1)
c  
c     Output arguments:
c        cno                 NO concentration (ppm)
c        cno2                NO2 concentration (ppm)
c        co3                 O3 concentration (ppm)
c
c     Routines Called:
c        none
c
c     Called by:
c        PIGDRIVE
c
      real cno,cno2,co3,dt,bdnlo3,bdnlno
      real rnono,tmp
c
c-----Entry point
c
c-----NO-NO self reaction
c
      tmp = cno/(1. + rnono*dt*cno/3600.)
      cno2 = cno2 + (cno - tmp)
      cno = tmp 
c
c-----NO-O3 titration is essentially complete because NO is very high
c
      if (cno.le.bdnlno .or. co3.le.bdnlo3) goto 999
      if (co3.le.cno) then
        tmp = co3 - bdnlo3
        co3 = bdnlo3
        cno2 = cno2 + tmp
        cno = amax1((cno-tmp),bdnlno)
      else
        tmp = cno - bdnlno
        cno = bdnlno
        cno2 = cno2 + tmp
        co3 = amax1((co3-tmp),bdnlo3)
      endif
 999  continue
c
      return
      end

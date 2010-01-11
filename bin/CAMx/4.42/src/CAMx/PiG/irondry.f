      subroutine irondry(n,dx,dy,dz,mapscl,dt,vdep,depfld,flxdry)
c
c----CAMx v4.42 070603
c
c     IRONDRY calculates dry deposition for a given IRON PiG puff.
c     Material is depleted (+ puff increment) or added (- puff increment)
c     for the fraction of the puff that resides in the surface layer.
c
c     Copyright 2006-2007
c     ENVIRON International Corporation
c
c     Modifications:
c        8/31/06         Added map scale factor
c
c     Input arguments:
c        n                   puff index
c        dx                  cell size in x-direction (m)
c        dy                  cell size in y-direction (m)
c        dz                  thickness of deposition layer (m)
c        mapscl              map scale factor
c        dt                  time step (s)
c        vdep                dry deposition velocity (m/s)
c        depfld              dry deposited mass (mol/ha, g/ha)
c        flxdry              dry deposition flux (umol)
c  
c     Output arguments:
c        depfld              dry deposited mass (mol/ha, g/ha)
c        flxdry              dry deposition flux (umol)
c
c     Routines Called:
c        none
c
c     Called by:
c        PIGDRIVE
c
      implicit none
      include "camx.prm"
      include "pigsty.com"
      include "chmstry.com"
c
      integer n
      real dz,dt,dx,dy,mapscl,vdep(MXSPEC),depfld(MXSPEC)
      real*8 flxdry(MXSPEC)
c
      integer nr,is
      real pmass,fdep,dmass,flxbot
c
c-----Entry point
c
      do is = 1,nspec
        depfld(is) = 0.
        flxdry(is) = 0.
      enddo
c
c-----Loop over puff reactors and species
c
      do nr = 1,nreactr
        do is = 1,nspec
c
c-----Get fraction of puff mass that deposits
c
          pmass = puffmass(is,nr,n)*amin1(1.,dz/axisz(n))
          fdep = 1. - exp(-vdep(is)*dt/dz)
c
c-----Calculate deposited mass and increment tracking arrays
c
          dmass = fdep*pmass
          puffmass(is,nr,n) = puffmass(is,nr,n) - dmass
          flxdry(is) = flxdry(is) - dmass
          flxbot = dmass/(dx*dy/mapscl**2)
          depfld(is) = depfld(is) + 1.e-2*flxbot
        enddo
      enddo
c
      return
      end

      subroutine irondry(nspec,nreactr,dx,dy,dz,mapscl,dt,puffmass,
     &                   axisz,vdep,depfld,flxdry)
c
c----CAMx v5.10 090918
c
c     IRONDRY calculates dry deposition for a given IRON PiG puff.
c     Material is depleted (+ puff increment) or added (- puff increment)
c     for the fraction of the puff that resides in the surface layer.
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c     Modifications:
c        8/31/06         Added map scale factor
c        7/11/07         Added RTRAC/RTCMC
c
c     Input arguments:
c        nspec               number of species
c        nreactr             number of puff reactor cells
c        dx                  cell size in x-direction (m)
c        dy                  cell size in y-direction (m)
c        dz                  thickness of deposition layer (m)
c        mapscl              map scale factor
c        dt                  time step (s)
c        puffmass            puff mass
c        axisz               puff size in z-direction (m)
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
c
      integer n,nspec,nreactr
      real dz,dt,dx,dy,mapscl,axisz
      real puffmass(nspec,nreactr)
      real vdep(*)
      real depfld(*)
      real*8 flxdry(*)
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
          pmass = puffmass(is,nr)*amin1(1.,dz/axisz)
          fdep = 1. - exp(-vdep(is)*dt/dz)
c
c-----Calculate deposited mass and increment tracking arrays
c
          dmass = fdep*pmass
          puffmass(is,nr) = puffmass(is,nr) - dmass
          flxdry(is) = flxdry(is) - dmass
          flxbot = dmass/(dx*dy/mapscl**2)
          depfld(is) = depfld(is) + 1.e-2*flxbot
        enddo
      enddo
c
      return
      end

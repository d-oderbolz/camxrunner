      subroutine virtdump(npuf,ncol,nrow,nlay,nspec,ipuf,jpuf,dx,dy,
     &                    mapscl,height,tempk,press,vdmpvec)
c
c----CAMx v4.51 080522
c
c     VIRTDUMP computes the 'virtual' contribution of a single puff
c     through a vertical grid column
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c     Modifications: 
c        8/31/06         Added map scale factor
c
c     Input arguments:
c        npuf               puff number
c        ncol               number of grid columns
c        nrow               number of grid rows
c        nlay               number of grid layers
c        ipuf               i-cell index of puff location
c        jpuf               j-cell index of puff location
c        dx                 grid spacing in x (m)
c        dy                 grid spacing in y (m)
c        mapscl             map scale factor
c        height             gridded layer heights (m)
c        nspec              number of species in conc array
c        tempk              temperature field (K)
c        press              pressure field (mb)
c
c     Output arguments:
c        vdmpvec            gridded species concentration (umol/m3)
c                           from one puff 
c
c     Routines Called:
c        none
c
c     Called by:
c        PIGDRIVE
c        AVEPIG
c
      implicit none
      include "camx.prm"
      include "camx.com"
      include "pigsty.com"
      include "filunit.com"
c
      integer npuf,ncol,nrow,nlay,ipuf,jpuf,nspec
      real tempk(ncol,nrow,nlay),press(ncol,nrow,nlay),
     &     virtdmp(MXSPEC),height(ncol,nrow,nlay),mapscl(ncol,nrow),
     &     vdmpvec(MXLAYA,MXSPEC),wtfac(MXLAYA)
      real dx,dy,sumwt,deplyr,volcel
      integer i,j,kk,kpb,kpt,is,nr
c
c-----Entry point
c
      i = ipuf
      j = jpuf
c
c-----Initialize some vectors
c
      do kk = 1,nlay
        wtfac(kk) = 0.
        do is = 1,nspec
          vdmpvec(kk,is) = 0.
        enddo
      enddo
c
c-----Find layers containing top and bottom of puff
c
      kpb = 1
      kpt = nlay
      do kk = 1, nlay
        if (height(i,j,kk) .GE. pufftop(npuf)) then
          kpt = kk
          goto 16
        endif
      enddo
  16  continue
      do kk = nlay,1,-1
        if (height(i,j,kk) .LE. puffbot(npuf)) then
          kpb = kk + 1
          goto 17
        endif
      enddo
 17   continue
c
c-----Define layer weighting (depth of puff coverage weighted by density)     
c
      if (kpb.eq.kpt) then
        wtfac(kpb) = 1.
      else
        sumwt = 0.
        deplyr = pufftop(npuf) - height(i,j,kpt-1)
        wtfac(kpt) = deplyr*press(i,j,kpt)/tempk(i,j,kpt)
        sumwt = sumwt + wtfac(kpt)

        deplyr = height(i,j,kpb) - puffbot(npuf)
        wtfac(kpb)= deplyr*press(i,j,kpb)/tempk(i,j,kpb)
        sumwt = sumwt + wtfac(kpb)

        do kk = kpb+1,kpt-1
          deplyr = height(i,j,kk) - height(i,j,kk-1)
          wtfac(kk) = deplyr*press(i,j,kk)/tempk(i,j,kk)
          sumwt = sumwt + wtfac(kk)
        enddo

        do kk = kpb,kpt
          wtfac(kk) = wtfac(kk)/sumwt
        enddo
      endif
c
c----Combine reactor mass in this puff
c
      do is = 1,nspec
        virtdmp(is) = 0.
        do nr = 1,nreactr
          virtdmp(is) = virtdmp(is) + puffmass(is,nr,npuf)
        enddo
      enddo
c
c-----Puff mass is assigned to layers according to the fraction of
c     puff coverage, and converted to concentration (umol/m3) 
c
      do kk = kpb,kpt
        deplyr = height(i,j,1)
        if (kk.gt.1) deplyr = height(i,j,kk) - height(i,j,kk-1)
        volcel = deplyr*dx*dy/mapscl(i,j)**2
        do is = 1,nspec
          vdmpvec(kk,is) = virtdmp(is)*wtfac(kk)/volcel
        enddo
      enddo
c
      return
      end

      subroutine virtdump(m1,m2,m3,ioff,joff,npuf,ncol,nrow,nlay,nspec,
     &                    ipuf,jpuf,dx,dy,mapscl,height,tempk,
     &                    press,vdmpvec)
      use camxcom
      use pigsty
      use filunit
      use filunit
      use node_mod
      implicit none
c
c----CAMx v5.10 090918
c
c     VIRTDUMP computes the 'virtual' contribution of a single puff
c     through a vertical grid column
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Modifications: 
c        8/31/06         Added map scale factor
c
c     Input arguments:
c        m1                 number of columns for this slice
c        m2                 number of columns for this slice
c        m1                 number of columns for this slice
c        ioff               X offset of this slice in full domain
c        joff               Y offset of this slice in full domain
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
      include "camx.prm"
c
      integer m1
      integer m2
      integer m3
      integer ioff
      integer joff
      integer npuf
      integer ncol
      integer nrow
      integer nlay
      integer nspec
      integer ipuf
      integer jpuf
      real    dx
      real    dy
      real    mapscl(m1,m2)
      real    height(ncol,nrow,nlay)
      real    tempk(m1,m2,m3)
      real    press(m1,m2,m3)
      real    vdmpvec(MXLAYER,MXSPEC)

      real virtdmp(MXSPEC)
      real wtfac(MXLAYER)
      real sumwt,deplyr,volcel
      integer i,j,kk,kpb,kpt,is,nr
c
c-----Entry point
c
      i = ipuf
      j = jpuf
c
c ---- if not in this slice, just return ---
c
      if( i .LT. ia .OR. i .GT. iz ) goto 9999
      if( j .LT. ja .OR. j .GT. jz ) goto 9999
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
        if (height(i+ioff,j+joff,kk) .GE. pufftop(npuf)) then
          kpt = kk
          goto 16
        endif
      enddo
  16  continue
      do kk = nlay,1,-1
        if (height(i+ioff,j+joff,kk) .LE. puffbot(npuf)) then
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
        deplyr = pufftop(npuf) - height(i+ioff,j+joff,kpt-1)
        wtfac(kpt) = deplyr*press(i,j,kpt)/tempk(i,j,kpt)
        sumwt = sumwt + wtfac(kpt)

        deplyr = height(i+ioff,j+joff,kpb) - puffbot(npuf)
        wtfac(kpb)= deplyr*press(i,j,kpb)/tempk(i,j,kpb)
        sumwt = sumwt + wtfac(kpb)

        do kk = kpb+1,kpt-1
          deplyr = height(i+ioff,j+joff,kk) - height(i+ioff,j+joff,kk-1)
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
        deplyr = height(i+ioff,j+joff,1)
        if (kk.gt.1) deplyr = height(i+ioff,j+joff,kk) - 
     &                                     height(i+ioff,j+joff,kk-1)
        volcel = deplyr*dx*dy/mapscl(i,j)**2
        do is = 1,nspec
          vdmpvec(kk,is) = virtdmp(is)*wtfac(kk)/volcel
        enddo
      enddo
c
 9999 continue
      return
      end

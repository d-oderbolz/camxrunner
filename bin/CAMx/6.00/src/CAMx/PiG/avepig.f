      subroutine avepig(iproc_id,igrd,dt,m1,m2,m3,ioff,joff,
     &                  ncol,nrow,nlay,nlayav,dx,dy,mapscl,height,
     &                              nspav,nspc,lmap,tempk,press,avcnc)
      use filunit
      use chmstry
      use bndary
      use camxcom
      use pigsty
      use node_mod
      implicit none
c
c----CAMx v6.00 130506
c
c     AVEPIG computes the 'virtual' contribution of all puffs to the
c     gridded concentration fields and adds them to time-averaged 
c     concentrations
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Modifications: 
c        none
c
c     Input arguments:
c        iproc_id           process ID for this slice (MPI)
c        igrd               grid index
c        dt                 time step for present grid concentration (s)
c        m1                 number of cells in X direction for this slice
c        m2                 number of cells in Y direction for this slice
c        m3                 number of layers for this slice
c        ioff               X offset of slice in full domain
c        joff               Y offset of slice in full domain
c        ncol               number of columns
c        nrow               number of rows
c        nlay               number of layers in instantaneous array
c        nlayav             number of layers in average array
c        dx                 grid spacing in x (m)
c        dy                 grid spacing in y (m)
c        mapscl             map scale factor
c        height             gridded layer heights (m)
c        nspav              number of average species
c        nspc               number of species in conc array
c        lmap               mapping array for average species
c        tempk              temperature field (K)
c        press              pressure field (mb)
c        avcnc              average species concentration (gas=ppm,
c                                                          other=ug/m3)
c
c     Output arguments:
c        avcnc              average species concentration (gas=ppm,
c                                                          other=ug/m3)
c
c     Routines Called:
c        VIRTDUMP
c
c     Called by:
c        CAMx
c        FGAVRG
c
      include "camx.prm"
      include "flags.inc"
c
      integer m1,m2,m3
      integer ioff, joff
      integer iproc_id,ncol,nrow,nlay,nlayav,nspav,nspc,igrd
      real tempk(m1,m2,m3),press(m1,m2,m3),
     &     avcnc(m1,m2,nlayav,nspav),height(m1,m2,m3),
     &     dx(nrow),dy,mapscl(m1,m2)
      real pcon
      integer lmap(nspc)
      integer i,j,k,n,kpb,kpt,kk,l,lsp,idum
      real dt,dtfact,convfac,xpig,ypig

      real vdmpvec(MXLAYER,MXSPEC)
c
c-----Entry point
c
      dtfact = dt/(dtout*60.)
c
c-----Start puff loop
c
      do 50 n = 1,npig
c
c-----Skip puffs that are not in the current grid
c
        if (ingrd(n).ne.igrd) goto 50
c
c-----If running MPI, Skip puffs that are not in this slice
c
        if( lmpi .AND. Lslice(igrd,iproc_id,n) .NE. 1 ) goto 50
c
c-----Locate the pig in the grid
c
        xpig = (xpigf(n) + xpigb(n))/2.
        ypig = (ypigf(n) + ypigb(n))/2.
        call pigcoord(xpig,ypig,i,j,idum)
        i = i-ioff
        j = j-joff
        do k = 1,nlay
          if (height(i,j,k).gt.zpig(n)) goto 15
        enddo
        k = nlay
  15    continue
c
c-----Find layers containing top and bottom of puff
c
        kpb = 1
        kpt = nlay
        do kk = 1,nlay
          if (height(i,j,kk) .GE. pufftop(n)) then
            kpt = kk
            goto 16
          endif
        enddo
  16    continue
        do kk = nlay,1,-1
           if (height(i,j,kk) .LE. puffbot(n)) then
             kpb = kk+1
             goto 17
          endif
        enddo
  17    continue
c
c-----Virtual dump of PUFFMASS into VDMPVEC (mass remains in PUFFMASS)
c
        call virtdump(m1,m2,m3,n,ncol,nrow,nlay,nspc,i,j,
     &                    dx(j+j0),dy,mapscl,height,tempk,press,vdmpvec)
c
c-----Put VDMPVEC into appropriate cells of AVCNC converting units from 
c     umol/m3 to ppm (gas) or ug/m3 (PM) 
c
        do l = 1,nspav
          lsp = lmap(l)
          if (lsp.le.nrad) CYCLE
          do kk = kpb,kpt
            if (lsp.gt.ngas) then 
              convfac = 1.
            else
              convfac = densfac*273./tempk(i,j,kk)*
     &                  press(i,j,kk)/1013.
            endif
            pcon = dtfact*vdmpvec(kk,lsp)/convfac
            if( kk .LE. nlayav ) then
               avcnc(i,j,kk,l) = amax1(bdnl(lsp),avcnc(i,j,kk,l)+pcon)
            endif
          enddo
        enddo
  50  continue
c 
      return
      end

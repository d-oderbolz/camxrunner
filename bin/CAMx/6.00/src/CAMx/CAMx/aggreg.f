      subroutine aggreg(ncolf,nrowf,nlayf,ncol,nrow,nlay,nspec,i1,j1,
     &                  i2,j2,nmesh,dxfin,dyfin,depfin,
     &                  dxcrs,dycrs,depcrs,concf,conc)
c
c----CAMx v6.00 130506
c  
c     AGGREG aggregates child (fine) grid concentrations to its parent (coarse)
c     grid using an arithmitic average
c                            
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c            
c     Modifications:  
c        1/29/99   Fixed a bug in layer indexing of fine layer depth
c                  (kp vs. kg)  
c       11/05/12   Removed vertical nesting
c
c     Input arguments:
c        ncolf               number of columns on fine grid
c        nrowf               number of rows on fine grid
c        nlayf               number of layers on fine grid
c        ncol                number of columns on parent grid
c        nrow                number of rows on parent grid
c        nlay                number of layers on parent grid
c        nspec               number of species
c        i1,j1               starting indices of fine grid on parent grid
c        i2,j2               ending indices of fine grid on parent grid
c        nmesh               fine grid meshing factor relative to parent
c        dxfin               cell size in x-direction on fine grid
c        dyfin               cell size in y-direction on fine grid
c        depfin              layer depths on fine grid
c        dxcrs               cell size in x-direction on parent grid
c        dycrs               cell size in y-direction on parent grid
c        depcrs              layer depths on parent grid
c        concf               concentration array on fine grid
c
c     Output arguments:
c        conc                concentration array on parent grid
c
c     Routines called:
c        none
c
c     Called by:
c        AGGR00
c
      real    dxfin(nrowf)
      real    dxcrs(nrow)
      real    depfin(ncolf,nrowf,nlayf)
      real    depcrs(ncol,nrow,nlay)
      real    conc(ncol,nrow,nlay,nspec)
      real    concf(ncolf,nrowf,nlayf,nspec)
c
c-----Entry point
c
c
c$omp parallel default(shared)
c$omp&  private(ispc,j,i,kp,ctmp,kg,jtmp,j0,itmp,i0,vol)
c
c$omp do schedule(dynamic)
c
      do 60 ispc = 1,nspec
        do 50 j = j1,j2
          do 40 i = i1,i2
            do 30 kp = 1,nlay
              ctmp = 0.
              kg = kp
                do jtmp = 1,nmesh
                 j0 = (j-j1)*nmesh+jtmp + 1
                  do itmp = 1,nmesh
                    i0 = (i-i1)*nmesh+itmp + 1
                    vol = dxfin(j0)*dyfin*depfin(i0,j0,kg)
                    ctmp = ctmp + concf(i0,j0,kg,ispc)*vol
                  enddo
                enddo
              conc(i,j,kp,ispc) = ctmp/
     &                         ( dxcrs(j)*dycrs*depcrs(i,j,kp) )
  30        continue
  40      continue
  50    continue
  60  continue
c
c  --- end of parallelized loop ---
c
c$omp end parallel
c
      return
      end

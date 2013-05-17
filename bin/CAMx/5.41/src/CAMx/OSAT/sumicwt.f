      subroutine sumicwt(ncolx,nrowy,nlays,deltax,depth,congrd,consum)
c
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c
c----CAMx v5.41 121109
c
c     SUMICWT sums up species concentrations from initial condition
c
c     Input arguments:
c        ncolx           the number of coarse grid columns
c        nrowy           the number of coarse grid rows
c        nlays           the number of coarse grid layers
c        depth           layer depth
c        congrd          species concentration for the grid
c
c     Output argumnets:
c        consum          sum of the species concentrations
c
      real    depth(ncolx,nrowy,nlays)
      real    congrd(ncolx,nrowy,nlays)
      real    deltax(nrowy)
c
c   --- calculate the average over all vertical cells,
c       weight by layer depth and cell width ---
c
      do 80 j=2,nrowy-1
         do 90 i=2,ncolx-1
            sumthk = 0
            sumcon = 0
            do 11 izcl=1,nlays
               sumthk = sumthk + depth(i,j,izcl) * deltax(j)
               sumcon = sumcon + 
     &              congrd(i,j,izcl) * depth(i,j,izcl) * deltax(j)
   11       continue
            if( sumthk .GT. 0. ) consum = consum + sumcon / sumthk
   90    continue
   80 continue
c
      return
      end

      subroutine vertag(igrd,ncol,nrow,nlay,cnctmp)
c 
c----------------------------------------------------------------------- 
c 
c   VERTAG aggregates fine grid data in multiple fine layers to the
c   coarse grid layer structure
c     Argument description: 
c      Inputs:
c        igrd   I  fine grid number
c        ncol   I  number of cells in x-direction
c        nrow   I  number of cells in y-direction
c        nlay   I  number of layers
c        cnctmp R  input concentration array
c      Outputs:
c        cnctmp R  output concentration array
c 
c----------------------------------------------------------------------- 
c   Include files: 
c----------------------------------------------------------------------- 
c
      include 'camxtrct.inc'
c 
c----------------------------------------------------------------------- 
c   Argument declaration: 
c----------------------------------------------------------------------- 
c
      integer*4 igrd, ncol, nrow, nlay
      real*4 cnctmp(MXCELL,MXCELL,MXLAYR)
c 
c----------------------------------------------------------------------- 
c    Local variables: 
c----------------------------------------------------------------------- 
c 
      integer*4 i, j, k,kf,kc,kfin
      real*4 tav, pav, tdepth, conc
c 
c----------------------------------------------------------------------- 
c    Entry point: 
c----------------------------------------------------------------------- 
c
      do j = 2,nrow-1
        do i = 2,ncol-1
          kf = 1 
          do 10 kc = 1,nlgrid(0)
            if (nmshv(kc,igrd).eq.1) then 
              cnctmp(i,j,kc) = cnctmp(i,j,kf) 
              kf = kf + 1 
              goto 10 
            else 
              pav = 0.
              tav = 0.
              tdepth = 0.
              conc = 0.
              do kfin = kf,kf+nmshv(kc,igrd)-1 
                tdepth = tdepth + depth(i,j,kfin,igrd)
                pav = pav + press(i,j,kfin,igrd)*depth(i,j,kfin,igrd)
                tav = tav + temp(i,j,kfin,igrd)*depth(i,j,kfin,igrd)
                conc = conc + cnctmp(i,j,kfin)*depth(i,j,kfin,igrd)*
     &                        press(i,j,kfin,igrd)/temp(i,j,kfin,igrd)
              enddo
              cnctmp(i,j,kc) = conc*tav/pav/tdepth
              kf = kf + nmshv(kc,igrd) 
            endif 
 10       continue 
        enddo 
      enddo 
c
c----------------------------------------------------------------------- 
c   Return point: 
c----------------------------------------------------------------------- 
c 
 9999 continue 
      return 
      end 

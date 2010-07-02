      subroutine interp2d(ncol,nrow,nlay,io,jo,mesh,ncolf,nrowf,
     &                    cval,fval)
c
c     INTERP2D horizontally interpolates a coarse grid field to a
c     fine grid
c
c     Modifications:
c        none
c
c     Input arguments:
c        ncol              number of columns in parent grid
c        nrow              number of rows in parent grid 
c        nlay              number of layers in parent grid
c        io                starting i index for the fine grid
c        jo                starting j index for the fine grid
c        mesh              mesh number
c        ncolf             number of columns in fine grid
c        nrowf             number of rows in fine grid
c        cval              cell centered value on coarse grid
c
c     Output arguments:
c        fval              cell centered value on fine grid
c
c     Subroutine called:
c        none
c
      include 'camxtrct.inc'
      real*4 cval(MXCELL,MXCELL,MXLAYR),fval(MXCELL,MXCELL,MXLAYR)
c
      integer*4 ncol,nrow,nlay,io,jo,ncolf,nrowf,k,j1,j,i1,i,j2,i2,
     &          ifin,jfin,mesh
      real*4 t11,t12,t21,t22,dy,dx,w11,w12,w21,w22
c
c-----Entry point
c
      do 50 k = 1,nlay
        do 40 j1 = 1,(nrowf-2)/mesh
          j = j1 + jo - 1
          do 30 i1 = 1,(ncolf-2)/mesh
            i = i1 + io - 1
            t11 = (cval(i-1,j-1,k) + cval(i-1,j,k) 
     &           + cval(i,j-1,k)   + cval(i,j,k))/4.
            t12 = (cval(i-1,j,k)   + cval(i-1,j+1,k) 
     &           + cval(i,j,k)     + cval(i,j+1,k))/4.
            t21 = (cval(i,j-1,k)   + cval(i,j,k) 
     &           + cval(i+1,j-1,k) + cval(i+1,j,k))/4.
            t22 = (cval(i,j,k)     + cval(i,j+1,k) 
     &           + cval(i+1,j,k)   + cval(i+1,j+1,k))/4.
            do j2 = 1,mesh
              dy = (j2 - 0.5)/mesh
              dy = dy*2 - 1
              do i2 = 1,mesh
                dx = (i2 - 0.5)/mesh
                dx = dx*2 - 1
c
c-----Bilinear interpolation 
c
                w11 = (1.0 - dx)*(1.0 - dy)/4.
                w12 = (1.0 - dx)*(1.0 + dy)/4.
                w21 = (1.0 + dx)*(1.0 - dy)/4.
                w22 = (1.0 + dx)*(1.0 + dy)/4.
c
                ifin = (i1 - 1)*mesh + i2 + 1
                jfin = (j1 - 1)*mesh + j2 + 1
                fval(ifin,jfin,k) = w11*t11 + w12*t12 + w21*t21 
     &                            + w22*t22
              enddo
            enddo
  30      continue
  40    continue
c
c-----Set boundary (1, ncol, and nrow) values to those just inside
c
        do j = 1,nrowf
          fval(1,j,k) = fval(2,j,k)
          fval(ncolf,j,k) = fval(ncolf-1,j,k)
        enddo
        do i = 1,ncolf
          fval(i,1,k) = fval(i,2,k)
          fval(i,nrowf,k) = fval(i,nrowf-1,k)
        enddo
c
  50  continue
c
      return
      end

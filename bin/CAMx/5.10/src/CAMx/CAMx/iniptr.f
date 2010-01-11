      subroutine iniptr(numcols,numrows)
      use filunit
      use grid
      use chmstry
      use tracer
      use procan
      implicit none
c
c----CAMx v5.10 090918
c
c     This routine calculates the pointers into the concentration
c     and meterology arrays for each grid.  The arrays are vectors
c     so this routine calculates the pointer that stores the 
c     first element of the 2-D or 3-D or 4-D field.  Some checks
c     are also made to ensure there is no array overflow of the
c     vectors.
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c          
c     Modifications:
c        10/8/99   Fixed a bug in assigning pointers for OSAT arrays for
c                  grids >= 3
c         9/3/02   Removed IPTRCL
c        1/13/03   Added IPTRDP for deposition output fields
c       11/10/03   Added IPSMP for RTRAC/PiG sampling grid output fields
c        7/29/05   Removed pointers for PiG sampling grid fields to READNML
c
c     Input arguments:
c        numcols   -- number of columns for each grid
c        numrows   -- number of columns for each grid
c
c     Output arguments:
c        nglay   -- maximum number of columns for each grid
c
c     Subroutine called:
c        none
c
c     Called by:
c        READNML
c
c-----Include files
c
      include 'camx.prm'
      include 'flags.com'
c
c-----Argument declarations
c
      integer numcols(*)
      integer numrows(*)
c
c-----Local variables
c
      integer ngmax, i
c
c-----Entry point
c
c   --- check that the parameters for each grid are large
c       enough to hold the grid dimensions --- 
c
      ngmax = -9
      do i=1,ngrid
        ngmax  = MAX(numrows(i),ngmax)
        ngmax  = MAX(numcols(i),ngmax)
        ngmax  = MAX(nlay(i),ngmax)
        if( numrows(i) .LT. numrows(i) .OR. numcols(i) .LT. numcols(i)
     &                                .OR. nlay(i) .LT. nlay(i) ) then
           write(iout,'(//,a)') 'ERROR in INIPTR:'
           write(iout,'(1X,A,I2,A)') 'Parameters for grid ',i,
     &                               ' are not large enough.'
           write(iout,'(10X,A,5X,A)') 'Parameters','Grid Definition'
           write(iout,'(A10,5X,I5,10X,I5)') 'Rows    :',numrows(i),
     &                                                      numrows(i)
           write(iout,'(A10,5X,I5,10X,I5)') 'Columns :',numcols(i),
     &                                                      numcols(i)
           write(iout,'(A10,5X,I5,10X,I5)') 'Layers  :',nlay(i),nlay(i)
           write(iout,*) 'Increase the parameters and recompile.'
           call camxerr()
        endif
      enddo
c
c  ---- everything checks out, set the pointers ---
c
      iptr2d(1) = 1
      iptr2d_full(1) = 1
      iptr3d(1) = 1
      iptr3d_full(1) = 1
      iptr4d(1) = 1
      iptrav(1) = 1
      iptrem(1) = 1
      iptrad(1) = 1
      iptrlu(1) = 1
      iptrdp(1) = 1
      ipsa2d(1) = 1
      ipsa3d(1) = 1
      ipsa3d_rad(1) = 1
      do i=2,ngrid
         iptr2d(i) = iptr2d(i-1) + numcols(i-1)*numrows(i-1)
         iptr2d_full(i) = iptr2d(i-1) + numcols(i-1)*numrows(i-1)
         iptr3d(i) = iptr3d(i-1) + numcols(i-1)*numrows(i-1)*nlay(i-1)
         iptr3d_full(i) = iptr3d(i)
         iptr4d(i) = iptr4d(i-1) + 
     &                       numcols(i-1)*numrows(i-1)*nlay(i-1)*nspec
         iptrem(i) = iptrem(i-1) + numcols(i-1)*numrows(i-1)*nspec
         iptrad(i) = iptrad(i-1) + 
     &                        numcols(i-1)*numrows(i-1)*nlay(i-1)*nrad
         iptrlu(i) = iptrlu(i-1) + numcols(i-1)*numrows(i-1)*NLU
         iptrdp(i) = iptrdp(i-1) + numcols(i-1)*numrows(i-1)*ndepspc*3
         if( .NOT. l3davg(i-1) ) then 
             iptrav(i) = iptrav(i-1) + numcols(i-1)*numrows(i-1)*navspc
         else
             iptrav(i) = iptrav(i-1) + 
     &                       numcols(i-1)*numrows(i-1)*nlay(i-1)*navspc
         endif
         if( ltrace .OR. lddm .OR. lhddm .OR. lirr ) then
             ipsa2d(i) = ipsa2d(i-1) + numcols(i-1)*numrows(i-1)*ntotsp
             ipsa3d(i) = ipsa3d(i-1) + 
     &                        numcols(i-1)*numrows(i-1)*nlay(i-1)*ntotsp
             ipsa3d_rad(i) = ipsa3d_rad(i-1) + 
     &                        numcols(i-1)*numrows(i-1)*nlay(i-1)*nradddm
         else
             ipsa2d(i) = 1
             ipsa3d(i) = 1
         endif
      enddo
c
c----Return point
c
      return
      end

c**** OPERATE.F
c
      subroutine operate(concget,concnum,concden,opcode,factor)          
c
c-----------------------------------------------------------------------
c
c   This adds or divides two 3-dimensional matrices.
c     Argument description:
c      Outputs:
c        concget R  The result of the division or addition of
c                   the two matrices.
c      Inputs:   
c        concnum R  array of concentrations to add or the numerator
c                   of a division operation
c        concden R  array of concentrations containing the denominator
c                   of division operation (Not used for addition oper)
c        opcode  C  code that indicates which operation to perform
c                   (valid values are "ADD" or "DIV")
c        factor  C  coefficient of linear combination to apply
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
      character*4 opcode
      real*4      concden(MXCELL,MXCELL,MXLAYR)
      real*4      concnum(MXCELL,MXCELL,MXLAYR)
      real*4      concget(MXCELL,MXCELL,MXLAYR) 
      real*4      factor
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 i, j, k 
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c  --- loop over the layers ---
c
      do k=1,MXLAYR
c
c  --- loop over the rows ----
c
        do j=1,MXCELL
c
c  --- loop over the columns ----
c
          do i=1,MXCELL
c
c  --- if operation is addition, just add to output array ---
c
            if( opcode(1:3) .EQ. "ADD" ) then
                concget(i,j,k) = concget(i,j,k) + 
     &                                       concnum(i,j,k) * factor
c
c  --- if operation is division, check for divide by zero ---
c
            else if( opcode(1:3) .EQ. "DIV" ) then
               if( concden(i,j,k) .NE. 0 ) then
                   concget(i,j,k) = concnum(i,j,k) / concden(i,j,k)
               else
                   concget(i,j,k) = 9.0E19
               endif
            endif
c
c  --- next column ---
c
          enddo
c
c  --- next row ---
c
        enddo
c
c  --- next layer ----
c
      enddo
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
      return
      end

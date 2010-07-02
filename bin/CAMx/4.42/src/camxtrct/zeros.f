c**** ZEROS.F
c
      subroutine zeros(array,ncount)
c
c-----------------------------------------------------------------------
c   This routine fills an array with zeros.
c     Argument description:
c        array   R  array to zero out
c        ncount  I  size of array (product of size in each dimension)
c
c-----------------------------------------------------------------------
c   Argument declaration: 
c-----------------------------------------------------------------------
c
      integer*4 ncount
      real*4    array(ncount)
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer*4 i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
      do i=1,ncount
        array(i) = 0.
      enddo
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
      return
      end

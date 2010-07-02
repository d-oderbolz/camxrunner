C
C  Dummy routine that can be linked with the source code on 
C  a machine that is big endian.  This is necessary to minimize
C  the amount of platform dependant code.
C
C arguments: integer string array to be converted 
C            number of elements in array along 1st dimension
C            number of elements along second dimension
C
      Subroutine bswap (instring,num1,num2)
C
      logical*1 instring(*), tmp1, tmp2
      integer*4 num1,num2
      integer*4 i,j,k
C
C   --- just return for the machine that is big endian already ---
C
      return
      end

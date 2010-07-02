C
C function to convert big-endian character strings stored as integers
C to little endian character strings stored as integers
C
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
C process in row major order
C
      if (num2 .ne. 0) goto 200
C
C Single dimension array
C
 100  do 150 i=1,num1
      j=(i-1)*4+1
      tmp1 = instring(j)
      instring(j) = instring(j+3)
      instring(j+3) = tmp1
      tmp2 = instring(j+1)
      instring(j+1) = instring(j+2)
      instring(j+2) = tmp2
 150  continue

      goto 300
C
C Double dimension array
C
 200  continue
      do 250 i=1,num2
      do 240 j=1,num1
      k = (i-1)*(num1*4)+(j-1)*4+1
      tmp1 = instring(k)
      instring(k) = instring(k+3)
      instring(k+3) = tmp1
      tmp2 = instring(k+1)
      instring(k+1) = instring(k+2)
      instring(k+2) = tmp2
 240  continue
 250  continue

 300  return
C
      end

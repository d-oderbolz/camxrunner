      subroutine bswap (instring,num1,num2)
c
c-----PMCAMx v3.01 020531
c
c     BSWAP converts big-endian character strings stored as integers
c     to little endian character strings stored as integers
c
c     Copyright 1996, 1997, 1998, 1999, 2000, 2001
c     ENVIRON International Corporation
c
c     Modifications:
c        none
c
c     Input arguments:
c        instring            integer string array to be converted 
c        num1                number of elements in array along 1st dimension
c        num2                number of elements along second dimension
c
c     Output arguments:
c        instring            converted integer string array 
c
      logical*1 instring(*), tmp1, tmp2
      integer   num1,num2
      integer   i,j,k
c
c-----Entry point
c
c-----Process in row major order
c
      if (num2 .ne. 0) goto 200
c
c-----Single dimension array
c
      do 150 i = 1,num1
        j = (i-1)*4+1
        tmp1 = instring(j)
        instring(j) = instring(j+3)
        instring(j+3) = tmp1
        tmp2 = instring(j+1)
        instring(j+1) = instring(j+2)
        instring(j+2) = tmp2
 150  continue

      goto 300
c
c-----Double dimension array
c
 200  continue
      do 250 i = 1,num2
        do 240 j = 1,num1
          k = (i-1)*(num1*4)+(j-1)*4+1
          tmp1 = instring(k)
          instring(k) = instring(k+3)
          instring(k+3) = tmp1
          tmp2 = instring(k+1)
          instring(k+1) = instring(k+2)
          instring(k+2) = tmp2
 240    continue
 250  continue

 300  return
c
      end

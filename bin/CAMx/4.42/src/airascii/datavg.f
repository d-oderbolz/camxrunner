      SUBROUTINE DATAVG (IOUT,NX,NY)
C
C
C      PROGRAM READS THE BINARY TEMPERATURE FILE THAT WILL INPUT INTO
C      THE SAI AIRSHED MODEL AND REFORMATS THIS FILE
C
      INCLUDE 'nampar.cmd'
      COMMON /LCM/ EMOB(MXX,MXY)
C
C
C
C   READ AND WRITE OUT FILE HEADER INFORMATION
C
C
C
C    LOOP ON INDIVIDUAL SPECIES -  READ AND WRITE OUT IN ASCII
C
          WRITE (IOUT,1006) ISEG, (MNAME(M),M=1,10)
 1006            FORMAT(I4,10A1, '    MIN    MAX    AVG')
        nav = 0
        sum = 0.
        xmin = 1.E20
        xmax = -1.E20
        do 100 j=1,ny
          do 90 i=1,nx
            if (emob(i,j) .ne. -9.) then
              sum = sum + emob(i,j)
              nav = nav + 1
              xmin = amin1(xmin,emob(i,j))
              xmax = amax1(xmax,emob(i,j))
            endif
   90     continue
  100   continue
        if (nav .gt. 0) then
          sum = sum / float(nav)
        else
          sum = -9.
        endif
          WRITE (IOUT,1007) xmin, xmax, sum
 1007            FORMAT(3(E14.7,2x))
  999 RETURN
       END

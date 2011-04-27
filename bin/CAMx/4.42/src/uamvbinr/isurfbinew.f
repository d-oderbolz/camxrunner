      subroutine isurfbinew(mlus)

      include 'uamvbinr.inc'

   10 continue


c
c  In the 5.30+ file, there is a header of 8 bytes we must read as well

      read(InNum,err=200) luheader
      write(OutNum,err=210) luheader

      do k = 1, mlus
        read(InNum,30,end=100,err=200) 
     &      ((fland(i,j,k), i=1,nox),j=1,noy)
      enddo
      write(OutNum,err=210)  
     &    (((fland(i,j,k), i=1,nox),j=1,noy),k=1,mlus)

   30 format(9E14.7)

      goto 10

  100 continue
      return

  200 continue
      write(ErrNum, 220) 'Error reading isurf file'
      write(*, 220) 'Error reading isurf file'
      stop

  210 continue
      write(ErrNum, 220) 'Error writing isurf file'
      write(*, 220) 'Error writing isurf file'
      stop

  220 format(A)

      end

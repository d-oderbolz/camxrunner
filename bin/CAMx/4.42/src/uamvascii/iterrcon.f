      subroutine iterrcon

      include 'uamvascii.inc'

   10 continue

      read(InNum,end=100,err=200) 
     &    ((emob(i,j), i=1,nox),j=1,noy)
      write(OutNum,30,err=210)  
     &    ((emob(i,j), i=1,nox),j=1,noy)


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

      subroutine iuh2ocon

      include 'uamvascii.inc'

   10 continue
      do 20 k=1, nlayer
          read(InNum,end=100,err=200) ttim, iidat,
     &        ((emob(i,j), i=1,nox),j=1,noy)
          write(OutNum,30,err=210)  ttim, iidat
          write(ErrNum,30,err=210)  ttim, iidat
          write(OutNum,35,err=210)
     &        ((emob(i,j), i=1,nox),j=1,noy)
   30     format(F10.3, 1X, I10)
   35     format(9E14.7)
   20 enddo

      goto 10

  100 continue
      return

  200 continue
      write(ErrNum, 220) 'Error reading iuh2o file'
      write(*, 220) 'Error reading iuh2o file'
      stop

  210 continue
      write(ErrNum, 220) 'Error writing iuh2o file'
      write(*, 220) 'Error writing iuh2o file'
      stop

  220 format(A)

      end

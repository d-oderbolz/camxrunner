      subroutine mixhtbin

      include 'uamvbinr.inc'

   10 continue

      do 20 k=1, nlayer
          read(InNum,30,end=100,err=200) ttim, iidat
          read(InNum,35,end=100,err=200)
     &        ((emob(i,j), i=1,nox),j=1,noy)
          write(ErrNum,30) ttim, iidat
          write(OutNum,err=210)  ttim, iidat,
     &        ((emob(i,j), i=1,nox),j=1,noy)
          read(InNum,30,end=200,err=200) ttim, iidat
          read(InNum,35,end=200,err=200)
     &        ((emob(i,j), i=1,nox),j=1,noy)
          write(OutNum,err=210)  ttim, iidat,
     &        ((emob(i,j), i=1,nox),j=1,noy)
   30     format(F10.3, 1X, I10)
   35     format(9E14.7)
   20 enddo

      goto 10

  100 continue
      return

  200 continue
      write(ErrNum, 220) 'Error reading mixht file'
      write(*, 220) 'Error reading mixht file'
      stop

  210 continue
      write(ErrNum, 220) 'Error writing mixht file'
      write(*, 220) 'Error writing mixht file'
      stop

  220 format(A)

      end

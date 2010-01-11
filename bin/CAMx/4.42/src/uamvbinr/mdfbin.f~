      subroutine mdfbin

      include 'uamvbinr.inc'

    1 read(InNum,10,end=100,err=200) tdata, idate
      write(ErrNum,10) tdata, idate
      write(OutNum) tdata, idate
   10 format(F10.1, 1X, I10)

      do 20 k=1, nlayer
          read(InNum,30,end=200,err=200) ((emob(i,j), i=1,nox),j=1,noy)
          write(OutNum,err=210)  ((emob(i,j), i=1,nox),j=1,noy)
          read(InNum,30,end=200,err=200) ((emob(i,j), i=1,nox),j=1,noy)
          write(OutNum,err=210)  ((emob(i,j), i=1,nox),j=1,noy)
   30     format(9E14.7)
   20 enddo

      read(InNum,30,end=200,err=200) ((emob(i,j), i=1,nox),j=1,noy)
      write(OutNum,err=210)  ((emob(i,j), i=1,nox),j=1,noy)
      
      goto 1

  100 continue
      return

  200 continue
      write(ErrNum, 220) 'Error reading mdf file'
      write(*, 220) 'Error reading mdf file'
      stop

  210 continue
      write(ErrNum, 220) 'Error writing mdf file'
      write(*, 220) 'Error writing mdf file'
      stop

  220 format(A)

      end

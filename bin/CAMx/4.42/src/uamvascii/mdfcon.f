      subroutine mdfcon

      include 'uamvascii.inc'
csebnem
      data zero /0./
csebnem
    1 read(InNum,end=100,err=200) tdata, idate
      write(OutNum, 10) tdata, idate
      write(ErrNum, 10) tdata, idate
   10 format(F10.1, 1X, I10)

      do 20 k=1, nlayer
          read(InNum,end=200,err=200) ((emob(i,j), i=1,nox),j=1,noy)
          write(OutNum,30,err=210)  ((emob(i,j), i=1,nox),j=1,noy)
          read(InNum,end=200,err=200) ((emob(i,j), i=1,nox),j=1,noy)
          write(OutNum,30,err=210)  ((emob(i,j), i=1,nox),j=1,noy)
   30     format(9E14.7)
   20 enddo

csebnem      read(InNum,end=200,err=200) ((emob(i,j), i=1,nox),j=1,noy)
csebnem      write(OutNum,30,err=210)  ((emob(i,j), i=1,nox),j=1,noy)
          read(InNum,end=200,err=200) zero  
          write(OutNum,30,err=210) zero
csebnem
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

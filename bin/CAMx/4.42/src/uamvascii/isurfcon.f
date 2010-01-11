      subroutine isurfcon

      include 'uamvascii.inc'

      mlus = 11

   10 continue

c
c   surface roughness removed from surf file (tcm, 3/8/93)
c
c     read(InNum,end=100,err=200) 
c    &    ((emob(i,j), i=1,nox),j=1,noy)
c     write(OutNum,30,err=210)  
c    &    ((emob(i,j), i=1,nox),j=1,noy)

      read(InNum,end=100,err=200) 
     &    (((fland(i,j,k), i=1,nox),j=1,noy),k=1,mlus)
      do k = 1, mlus
          write(OutNum,30,err=210)  
     &        ((fland(i,j,k), i=1,nox),j=1,noy)
      enddo

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

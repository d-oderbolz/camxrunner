      subroutine zeros(a,n)
c
c----CAMx v6.00 130506
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     ZEROS sets input array to zero
c
      dimension a(n)
c
      do i=1,n
        a(i) = 0.
      enddo
c
      return
      end

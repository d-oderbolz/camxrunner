      subroutine zeros(a,n)
c
c----CAMx v5.30 101223
c
c     Copyright 1996 - 2010
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

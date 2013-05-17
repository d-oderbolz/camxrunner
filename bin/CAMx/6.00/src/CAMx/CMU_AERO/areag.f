      function areag(x1, x2, d, sigma)
      implicit none

c----CAMx v6.00 130506
c
c --- calculate the fractional area of a Gaussian 
c     within the interval x1 to x2
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     corrected by bkoo (11/14/03)
c
      real areag, x1, x2, d, sigma
      real t, xerf, sqr2

      data sqr2 /1.414214/

c --- enforce x2 > x1

      if (x1 .gt. x2) then
        t = x1
        x1 = x2
        x2 = t
      elseif (x1 .eq. x2) then
        areag = 0.0
        return
      endif

c --- calculate area

      areag = 0.5*(xerf((x2-d)/sigma/sqr2) - xerf((x1-d)/sigma/sqr2))

      return
      end

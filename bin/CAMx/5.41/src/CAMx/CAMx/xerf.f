      function xerf(x)
c
c     Returns the complementary error function erfc(x) with fractional error
c     everywhere less than 1.2e-7 (Numerical Recipes in Fortran, 2nd Ed., 1992)
c
c     modified by bkoo to return the error function erf(x) - (11/14/03)
c
      real xerf
      real erfcc,x
      real t,z
      z=abs(x)
      t=1./(1.+0.5*z)
      erfcc=t*exp(-z*z-1.26551223+t*(1.00002368+t*(.37409196+
     1     t*(.09678418+t*(-.18628806+t*(.27886807+t*(-1.13520398+
     2     t*(1.48851587+t*(-.82215223+t*.17087277)))))))))
      if (x.lt.0.) erfcc=2.-erfcc

      xerf = 1.-erfcc ! return erf(x)

      return
      end


      subroutine chem10(dt,tcell,pcell,H2O,con)
      use filunit
      use chmstry
      implicit none
c
c----CAMx v5.10 090918
c
c     CHEM10 performs simple chemistry defined by the user.
c     You must hard code the reactions you want below.
c     Follow the guidelines and safety checks given as examples.
c                          
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Modifications:
c        07/25/05      Corrected the exponential decay example
c
c     Input arguments:
c        dt                time duration to be integrated (hr)
c        H2O               water vapor concentration (ppm)
c        tcell             temperature (K)
c        pcell             pressure (mbar)
c        con               state species concentrations (ppm or ug/m3)
c
c     Output arguments:
c        con               state species concentrations (ppm or ug/m3)
c
c     Routines called:
c        NONE
c
c     Called by:
c        CHEMDRIV
c
      include "camx.prm"
      include 'chmdbg.com'
c
c-----Arguments
c
      real dt
      real tcell
      real pcell
      real H2O
      real con(*)
c
c-----Local variables:
c
      real    rate, dc
      integer l
c
      real concin(MXSPEC+1)
c
c-----Entry point
c
      do l=1,nspec
        concin(l) = con(l)
      enddo
c
c=====Follow these guidelines for implementing reactions================
c
c --- For a reaction A --> B with rate in units hr-1
c
c     The species A and B are completely defined by the user, and therefore
c     do not appear in the universe of known CAMx species.  You must
c     be very careful to access specific species pointers in the CON array
c     in the order that the species appear in the chemistry parameters file.
c
c --- You can calculate a rate constant (k) using the expression:
c               k = A*(Tcell/Tref)^B*exp(Ea/Tcell)
c     by calling the CAMx function arren(Aa,Ea,B,Tref,Tcell).
c     See the subroutine EXPTBL for more complex expressions.
c
c --- Set the reaction rate in units hr-1, 
c
c        rate = 1.0 (for example)
c     or
c        rate = arren(Aa,Ea,B,tref,tcell)
c
c --- Be sure that the species A is the first species listed in the
c     chemistry parameters file.  Then decay A = species 1.

c        dc = con(1)*(1.0 - EXP(-rate*dt))
c        con(1) = con(1) - dc
c
c --- Be sure that the species B is the second species listed in the
c     chemistry parameters file.  Then increase B = species 2.
c
c        con(2) = con(2) + dc
c=======================================================================
c
      rate = 1.0
      dc = con(1)*(1.0 - EXP(-rate*dt))
      con(1) = con(1) - dc
      con(2) = con(2) + dc
c
c --- Check for negative concentrations
c
      do l=1,nspec
        if (con(l).lt.0.0) goto 900
        con(l) = amax1(con(l),bdnl(l))
      enddo
c
      return
c
c-----Error reporting on negative concentration
c
 900  write(iout,'(//,a)') ' ERROR in User Defined subroutine CHEM10:'
      write(iout,'(/,a)')  ' Negative Concentraion'
      write(iout,*) 'dt, temp(K), pressure(mbar), water(ppm) = ',
     &     dt, tcell, pcell, H2O
      write(iout,'(a,4i4)') ' igrd, i, j, k = ', igrdchm,ichm,jchm,kchm 
      write(iout,*) 'No  Name     New Conc  Conc In'
      do l=1,nspec
        write(iout,800) l,spname(l),con(l),concin(l)
      enddo
      call camxerr()
 800  format(i3,2x,a7,1p3e10.3)
      end

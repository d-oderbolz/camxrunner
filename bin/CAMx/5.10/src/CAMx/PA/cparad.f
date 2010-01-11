      subroutine cparad( rad, nr, pa, npa, nn, dtfact )
      use filunit
      use chmstry
      use camxcom
      use procan
      use tracer
c
c----CAMx v5.10 090918
c
c     CPARAD saves radical concentrations for CPA output
c     and sets the CPA names and position numbers on the 
c     first call from PASETUP
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Modifications:
c        none
c
c     Input arguments:
c        rad                 radical concentrations array (ppm)
c        nr                  dimension of radical array
c        pa                  CPA parameter array (ppb units)
c        npa                 dimension of pa array
c        nn                  counter of CPA parameters filled
c        dtfact              ratio of time step to averaging interval
c
c     Output arguments:
c        pa                  CPA parameter array (ppb units)
c        nn                  counter of CPA parameters filled
c
c     Routines called:
c        none
c
c     Called by:
c        CHEMDRIV
c        PASETUP
c
      implicit none
      include 'camx.prm'
c
      integer npa, nr, nn, in_nn
      real    dtfact, ppbfact
      real    pa(npa), rad(nr)
c
c-----Entry point
c
      ppbfact = 1000.
      in_nn = nn
c
      nn = nn + 1
      ptname(nn)  = 'OH'
      pa(nn) = dtfact*rad(kOH)*ppbfact
c
      nn = nn + 1
      ptname(nn)  = 'HO2'
      pa(nn) = dtfact*rad(kHO2)*ppbfact
c
      nn = nn + 1
      ptname(nn)  = 'NO3'
      pa(nn) = dtfact*rad(kNO3)*ppbfact
c
      nn = nn + 1
      ptname(nn)  = 'N2O5'
      pa(nn) = dtfact*rad(kN2O5)*ppbfact
c
c
      if( nn .GT. MXCPA ) then
         write(iout,'(//,a)') 'ERROR in CPARAD:'
         write(iout,*) 'Number of outputs requested exceeds limit.'
         write(iout,*) 
     &     'Increase the parameter MXCPA in include file procan.com'
         write(iout,'(1X,A,I5)')
     &         'You need room for at least this many species: ',in_nn+4
         call camxerr()
      end if
c
      return
      end

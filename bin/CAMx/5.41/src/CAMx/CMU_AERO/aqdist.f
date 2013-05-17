      subroutine aqdist(nsect, cut, fdist, fdist2)
c
c----CAMx v5.41 121109
c
c --- Calculate the factors for allocating aerosol production
c     in the aqueous chemistry to the PMCAMx size sections.
c     A coarse mode (2.5 um) and a fine mode (0.4 um) are
c     formed.
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c     Modifications:
c        none
c
c     Input arguments:
c
c        nsect  -  number of size sections
c        cut    -  cut points for the size sections
c
c     Output arguments:
c
c        fdist  -  fractions for bulk AQ chemistry
c        fdist2 -  fractions for size resolved AQ chemistry
c
c     Routines Called:
c
c        AREAG
c
c     Called by:
c        
c        AQCHEM
c

      real cut(nsect+1), fdist(nsect), fdist2(nsect)
      real x(1000)
      real dfin, dcrs, sfin, scrs

      real adj, xadj                      ! adj

c --- These data define the fine and coarse modes
c     of the aqueous phase aerosol production

      data dfin /0.884/                   ! fix
      data sfin /1.5/                     ! fix
      data dcrs /3.54/                    ! fix
      data scrs /1.5/                     ! fix

c --- Entry point

c --- locally expand the ends of the size distribution
c     so that the fractions always sum to 1.0

      xadj = 0.                           ! adj
      do i=1,nsect+1
        x(i) = cut(i)
        if (cut(i).ge.2.51.and.cut(i).lt.10.01) xadj = xadj + 1. ! adj
      enddo
      x(1) = 1.0e-6
      x(nsect+1) = 1000.0

c --- calculate the fraction of Guassian in each section
c     working in log space for a log-normal distribution

      adj = 0.                            ! adj
      do i=1,nsect
        ffin = areag(alog10(x(i)),alog10(x(i+1)),
     &                             alog10(dfin),alog10(sfin))
        fcrs = areag(alog10(x(i)),alog10(x(i+1)),
     &                             alog10(dcrs),alog10(scrs))
        if (cut(i+1).lt.2.51) then        ! adj
          adj = adj + fcrs                ! adj
          fcrs = 0.0                      ! adj
        elseif (cut(i+1).lt.10.01) then   ! adj
          fcrs = fcrs + adj / xadj        ! adj
        endif                             ! adj
        fdist2(i) = ffin + fcrs
        fdist(i)  = fdist2(i)/2.0
      enddo

      return
      end


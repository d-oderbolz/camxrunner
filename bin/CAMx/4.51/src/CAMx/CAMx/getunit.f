      subroutine getunit(iounit)
c
c----CAMx v4.51 080522
c
c     GETUNIT incraments the I/O unit number, checks that it is not
c     already attached to a file and returns an unused unit number.
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c     Modifications:
c        none
c
c     Input arguments:
c        none
c
c     Output arguments:
c        iounit
c
c     Routines called:
c        none
c
c     Called by:
c         READNML
c         OPENFILS
c         STARTDDM
c         STARTSA
c         STARTRT
c
      include 'camx.prm'
      include 'filunit.com'
c
      integer iounit
c
      logical lused
c
c-----Entry point
c
c
c  --- incrament the global unit number variable ---
c
  111 continue
      icur_unit = icur_unit + 1
c
c  --- check if this unit is attached to a file ---
c
      inquire(unit=icur_unit,opened=lused)
c
c  --- if it is already used, go back and bump it up ---
c
      if( lused ) goto 111
c
c  --- set the output variable and return ---
c
      iounit = icur_unit
      return
      end

c*** CAMXCOM
c
      Module camxcom
      include 'camx.inc'
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the CAMX.COM
c        include file.
c
c    Argument descriptions:
c     Input:
c        numgrds    I  number of grids
c     Output:
c
c-----------------------------------------------------------------------
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
      Contains
c
         subroutine alloc_camxcom(numgrds)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
         use camx_includes
         implicit none
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numgrds
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- allocate the arrays dimensioned by number of grids ---
c
         allocate( datec  (numgrds) )
         allocate( timec  (numgrds) )
         allocate( deltat (numgrds) )
c
c-----DENSFAC is the moles/m3 of air at STP: (1293 g/m3)/(28.8 g/mol)
c
         densfac = 44.9
c
         return
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
         end subroutine
c
      end Module

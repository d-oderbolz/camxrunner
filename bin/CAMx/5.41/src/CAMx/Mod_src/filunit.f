      Module filunit
      include 'filunit.inc'
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the FILUNIT.COM
c        include file.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
      Contains
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_FILUNIT
c-----------------------------------------------------------------------
c
         subroutine alloc_filunit(numgrds)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
         use camx_includes
         implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c        numgrds    I  number of grids
c     Output:  
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
         allocate( iavg  (numgrds) )
         allocate( idep  (numgrds) )
         allocate( iarem (numgrds) )
         allocate( isurf (numgrds) )
         allocate( ihtp  (numgrds) )
         allocate( iwind (numgrds) )
         allocate( itemp (numgrds) )
         allocate( ikv   (numgrds) )
         allocate( ih2o  (numgrds) )
         allocate( icld  (numgrds) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_FILUNIT
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_FILUNIT_SAMPLE
c-----------------------------------------------------------------------
c
         subroutine alloc_filunit_sample(numsamples)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
         use camx_includes
         implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c        numsamples   I  number of sampling grids
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numsamples
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c         
         allocate( isample(numsamples) )
c
         return
         end subroutine
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_FILUNIT_SAMPLE
c-----------------------------------------------------------------------
c
      end Module

      Module lsbox
      include 'lsboxalloc.com'
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the LSBOX.COM
c        include file.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
      Contains
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_LSBOX
c-----------------------------------------------------------------------
c
         subroutine alloc_lsbox(numrxns,numspcs,numrads)
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
c        numrxns    I  ???
c        numspcs    I  ???
c        numrads    I  ???
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numrxns
         integer :: numspcs
         integer :: numrads
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c
         allocate( dbrk(numrxns) )
         allocate( rrk (numrxns) )
c
         allocate( jac(numspcs+numrads+1,numspcs+numrads+1) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_LSBOX
c-----------------------------------------------------------------------
c
      end Module

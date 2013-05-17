      Module ptemiss
      include 'ptemiss.inc'
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the PIGSTY.COM
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
      Contains
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_PTEMISS
c-----------------------------------------------------------------------
c
         subroutine alloc_ptemiss(numspcs,numgrids)
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
c        numspcs    I  number of species
c        numgrids   I  number of grids
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numspcs
         integer :: numgrids
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( hstk   (nptsrc) )
         allocate( dstk   (nptsrc) )
         allocate( tstk   (nptsrc) )
         allocate( vstk   (nptsrc) )
         allocate( effph  (nptsrc) )
         allocate( lpiglet(nptsrc) )
c
         allocate( xstk   (nptsrc,numgrids) )
         allocate( ystk   (nptsrc,numgrids) )
         allocate( ptemis (nptsrc,numspcs)  )

         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_PTEMISS
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_PTEMISS_NULL
c-----------------------------------------------------------------------
c
         subroutine alloc_ptemiss_null(numspcs)
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
c        numspcs    I  number of species
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numspcs
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( ptemis (1,numspcs)  )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_PTEMISS_NULL
c-----------------------------------------------------------------------
c
      end Module



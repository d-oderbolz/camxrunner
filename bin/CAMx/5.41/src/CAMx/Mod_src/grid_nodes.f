      Module grid_nodes
      include 'grid_nodes.inc'
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the GRID_NODES.COM
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
c   BEGIN SUBROUTINE ALLOC_GRID_NODES
c-----------------------------------------------------------------------
c
     
         subroutine alloc_grid_nodes(numgrds,numprocs)
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
c        numprocs   I  number of processors used
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numgrds
         integer :: numprocs
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( nvals_4d(numgrds,0:numprocs) )
         allocate( nvals_dp(numgrds,0:numprocs) )
         allocate( nvals_em(numgrds,0:numprocs) )
         allocate( istart_4d(numgrds,0:numprocs) )
         allocate( istart_dp(numgrds,0:numprocs) )
         allocate( istart_em(numgrds,0:numprocs) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_GRID_NODES
c-----------------------------------------------------------------------
c
      end Module

c*** BNDARY
c
      Module bndary
      include 'bndary.inc'
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the BNDARY.COM
c        include file.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:
c        numgrds    I  number of grids
c        numcols    I  number of cells in the X direction
c        numrows    I  number of cells in the Y direction
c        numlays    I  number of cells in the Z direction
c        numspcs    I  number of modeled species
c     Output:
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c     11/4/09 -cemery- Removed input top concentrations
c
c-----------------------------------------------------------------------
c
      Contains
c
         subroutine alloc_bndary(numgrds,numcols,numrows,numlays,numspcs)
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
         integer :: numcols(numgrds)
         integer :: numrows(numgrds)
         integer :: numlays(numgrds)
         integer :: numspcs
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: ncola
         integer :: nrowa
         integer :: nlaya
         integer :: nmx1d
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- calculate the bigggest dimension over all grids ---
c
         ncola  = MAXVAL( numcols(1:numgrds) )
         nrowa  = MAXVAL( numrows(1:numgrds) )
         nlaya  = MAXVAL( numlays(1:numgrds) )
         nmx1d  = MAX( nrowa, ncola )
c         
c  --- allocate arrays based on biggest dimension ---
c
         allocate( ibeg(nrowa) )
         allocate( iend(nrowa) )
         allocate( jbeg(ncola) )
         allocate( jend(ncola) )
c
c  --- allocate array that is used to read/store the boundary conditions ---
c
         allocate( bc(nmx1d,nlaya,numspcs,4) )
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c    
         return
         end subroutine
c
      end Module

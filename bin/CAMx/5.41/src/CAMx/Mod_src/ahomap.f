c*** AHPMAP
c
      Module ahomap
      include 'ahomap.inc'
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the AHOMAP.COM
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
c-----------------------------------------------------------------------
c    BEGIN SUBROUTINE ALLOC_AHO
c-----------------------------------------------------------------------
c
      Contains
c
         subroutine alloc_aho(numgrds, numcols, numrows)
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
c    This routine allocates the arrays that are dimensioned by
c    number of grids and grid size. This version is for the master
c    node, which needs space for the entire domain.
c
c    Argument descriptions:
c     Input:
c        numgrds    I   number of grids
c        numcols    I   number of cells in the X direction
c        numrows    I   number of cells in the Y direction
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numgrds
         integer :: numcols(numgrds)
         integer :: numrows(numgrds)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvec2d
         integer :: i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- calculate the size of the all domains ---
c
         mvec2d = 0
         do i=1,numgrds
            mvec2d = mvec2d + numcols(i) * numrows(i)
         enddo
c
c  --- allocate arrays that have constant length ---
c
         allocate( albcl  (NALB)   )
         allocate( hazcl  (NHAZE)  )
         allocate( ozcl   (NOZN)   )
         allocate( ruflen (NRUF)   )
c
c  --- allocate arrays that depend on grid sizes ---
c
         allocate( icdalb (mvec2d) )
         allocate( icdhaz (mvec2d) )
         allocate( icdozn (mvec2d) )
         allocate( icdsno (mvec2d) )
         allocate( icdocn (mvec2d) )
         allocate( icddrt (mvec2d) )
         allocate( icdruf (mvec2d) )
         allocate( lrdalb (numgrds)  )
         allocate( lrdocn (numgrds)  )
         allocate( lrddrt (numgrds)  )
         allocate( lrdruf (numgrds)  )
c
c  --- initialize some arrays ---
c
         do i = 1,numgrds
           lrdalb(i) = .FALSE.
           lrdocn(i) = .FALSE.
           lrddrt(i) = .FALSE.
           lrdruf(i) = .FALSE.
         enddo
         do i = 1,mvec2d
           icdocn(i) = 0.
           icddrt(i) = 0.
           icdruf(i) = 0.
           icdsno(i) = 0.
         enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c    END SUBROUTINE ALLOC_AHO
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN SUBROUTINE ALLOC_AHO_NODE
c-----------------------------------------------------------------------
c
        subroutine alloc_aho_node(ngrd, numgrds, numcols, numrows)
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
c    This routine allocates the arrays that are dimensioned by
c    number of grids and grid size. This version is for the compute
c    nodes, which need space for the just the slice.
c
c    Argument descriptions:
c     Input:
c        ngrd       I  number of usable grids
c        numgrds    I  number of potential grids
c        numcols    I  number of cells in the X direction
c        numrows    I  number of cells in the Y direction
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: ngrd
         integer :: numgrds
         integer :: numcols(numgrds)
         integer :: numrows(numgrds)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvec2d
         integer :: i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- calculate the size of the all domains ---
c
         mvec2d = 0
         do i=1,ngrd
            mvec2d = mvec2d + numcols(i) * numrows(i)
         enddo
c  
c  --- allocate arrays that have constant length ---
c
         allocate( albcl  (NALB)   )
         allocate( hazcl  (NHAZE)  )
         allocate( ozcl   (NOZN)   )
         allocate( ruflen (NRUF)   )
c  
c  --- allocate arrays that depend on grid sizes ---
c
         allocate( icdalb (mvec2d) )
         allocate( icdhaz (mvec2d) )
         allocate( icdozn (mvec2d) )
         allocate( icdsno (mvec2d) )
         allocate( icdocn (mvec2d) )
         allocate( icddrt (mvec2d) )
         allocate( icdruf (mvec2d) )
c
         allocate( lrdalb (ngrd)  )
         allocate( lrdocn (ngrd)  )
         allocate( lrddrt (ngrd)  )
         allocate( lrdruf (ngrd)  )
c
c  --- initialize some arrays ---
c
         do i = 1,ngrd
           lrdalb(i) = .FALSE.
           lrdocn(i) = .FALSE.
           lrddrt(i) = .FALSE.
           lrdruf(i) = .FALSE.
        enddo
         do i = 1,mvec2d
          icdocn(i) = 0.
           icddrt(i) = 0.
           icdruf(i) = 0.
           icdsno(i) = 0.
         enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c    
         return
         end subroutine
c
c-----------------------------------------------------------------------
c    END SUBROUTINE ALLOC_AHO_NODE
c-----------------------------------------------------------------------
c
      end Module

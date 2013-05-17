      Module grid
      include 'grid.inc'
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the GRID.COM
c        include file.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
      Contains
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_GRID
c-----------------------------------------------------------------------
c
         subroutine alloc_grid()
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
         use camx_includes
         implicit none
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( ncol(ngrid)            )
         allocate( nrow(ngrid)            )
         allocate( nlay(ngrid)            )
         allocate( deltay(ngrid)          )
         allocate( ntim(ngrid)            )
         allocate( ntimcrs(ngrid)         )
         allocate( i1(ngrid)              )
         allocate( j1(ngrid)              )
         allocate( i2(ngrid)              )
         allocate( j2(ngrid)              )
         allocate( nmesh(ngrid)           )
         allocate( nchdrn(ngrid)          )
         allocate( idchdrn(MXCHDRN,ngrid) )
         allocate( nosrc(ngrid)           )
         allocate( l3davg(ngrid)          )
c
         allocate( inst1(ngrid)   )
         allocate( inst2(ngrid)   )
         allocate( jnst1(ngrid)   )
         allocate( jnst2(ngrid)   )
         allocate( meshold(ngrid) )
         allocate( mapgrd(ngrid)  )
c
         do i=1, ngrid       
             ncol(i) = 0
             nrow(i) = 0
             nlay(i) = 0
             inst1(i) = 0
             inst2(i) = 0
             jnst1(i) = 0
             jnst2(i) = 0
         enddo 
c
         allocate( iptr2d(ngrid) )
         allocate( iptr2d_full(ngrid) )
         allocate( iptr3d(ngrid) )
         allocate( iptr3d_full(ngrid) )
         allocate( iptr4d(ngrid) )
         allocate( iptrav(ngrid) )
         allocate( iptrem(ngrid) )
         allocate( iptrlu(ngrid) )
         allocate( iptrdp(ngrid) )
         allocate( ipsa3d(ngrid) )
         allocate( ipsa2d(ngrid) )
         allocate( ipsadep(ngrid) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_GRID
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_GRID_ROW
c-----------------------------------------------------------------------
c
         subroutine alloc_grid_row(numrows)
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
c        numrows    I  number of cells in the Y direction
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numrows(*)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: nrowa
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         nrowa = maxval( numrows(1:ngrid) )
c         
         allocate( deltax(nrowa,ngrid) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_GRID_ROW
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_GRID_LAY
c-----------------------------------------------------------------------
c
         subroutine alloc_grid_lay()
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
         use camx_includes
         implicit none
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: nlaya
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         nlaya = maxval( nlay(1:ngrid) )
c         
         allocate( nadv(nlaya,ngrid)  )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_GRID_LAY
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_GRID_PTSRC
c-----------------------------------------------------------------------
c
         subroutine alloc_grid_ptsrc(numpoints)
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
c        numpoints  I  number of point sources
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numpoints
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( idsrc(numpoints,ngrid) )
         allocate( isrc(numpoints,ngrid)  )
         allocate( jsrc(numpoints,ngrid)  )
         call flush(6)
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_GRID_PTSRC
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_GRID_2D
c-----------------------------------------------------------------------
c
         subroutine alloc_grid_2d(numcols, numrows)
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
c        numcols    I  number of cells in the X direction
c        numrows    I  number of cells in the Y direction
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numcols(*)
         integer :: numrows(*)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvec2d, i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         mvec2d = 0
c
         do i=1,ngrid
            mvec2d = mvec2d + numrows(i) * numcols(i)
         enddo
c
         allocate( idfin(mvec2d) )
         allocate( ldark(mvec2d) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_GRID_2D
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_GRID_2D_NODE
c-----------------------------------------------------------------------
c
         subroutine alloc_grid_2d_node(numgrds, numcols, numrows)
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
c        numcols    I  number of cells in the X direction
c        numrows    I  number of cells in the Y direction
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
         mvec2d = 0
c
         do i=1,ngrid
            mvec2d = mvec2d + numrows(i) * numcols(i)
         enddo
c
         allocate( idfin(mvec2d) )
         allocate( ldark(mvec2d) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_GRID_2D_NODE
c-----------------------------------------------------------------------
c
      end Module

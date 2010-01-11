      Module procan
      include 'procan.com'
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the PROCAN.COM
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
c
      Contains
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_PROCAN
c-----------------------------------------------------------------------
c
         subroutine alloc_procan(numgrds,numcols,numrows,numlays)
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
c        numgrds       I  number of grids
c        numrows       I  number of cells in the X direction
c        numcols       I  number of cells in the Y direction
c        numlays       I  number of cells in the Z direction
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer      :: numgrds
         integer      :: numrows(numgrds)
         integer      :: numcols(numgrds)
         integer      :: numlays(numgrds)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvec2d
         integer :: mvec3d
         integer :: i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         mvec2d = 0
         mvec3d = 0
         do i=1,numgrds
            mvec2d = mvec2d + numcols(i) * numrows(i)
            mvec3d = mvec3d + numcols(i) * numrows(i) * numlays(i)
         enddo
c
         allocate( ipacl_2d (mvec2d) )
         do i = 1,mvec2d
           ipacl_2d(i) = -9
         enddo
c
         allocate( ipacl_3d (mvec3d) )
         do i = 1,mvec3D
           ipacl_3d(i) = -9
         enddo

         allocate( ipagrd ( npadom) )
         allocate( i_sw   ( npadom) )
         allocate( j_sw   ( npadom) )
         allocate( i_ne   ( npadom) )
         allocate( j_ne   ( npadom) )
         allocate( b_lay  ( npadom) )
         allocate( t_lay  ( npadom) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_PROCAN
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_PROCAN_CELLS
c-----------------------------------------------------------------------
c
         subroutine alloc_procan_cells(numcells,numspecs,numreact)
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
c        numcells   I  total number of PA cells 
c        numspecs   I  number of PA "species"
c        numreact   I  number of reactions
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numcells
         integer :: numspecs
         integer :: numreact
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( ipadom ( numcells) )
         allocate( ipax   ( numcells) )
         allocate( ipay   ( numcells) )
         allocate( ipaz   ( numcells) )
         allocate( ipanst ( numcells) )
c     
         allocate( cipr   ( NPAPRC, numcells, numspecs  ) )
c
         allocate( cirr   ( numcells, numreact ) )
         allocate( npastep( numcells, numspecs ) )

c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_PROCAN_CELLS
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_PROCAN_IPA
c-----------------------------------------------------------------------
c
         subroutine alloc_procan_ipa(numgrds,numcols,numrows,numlays,
     &                                        probing_tools,this_proc)
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
c        numgrds       I  number of grids
c        numrows       I  number of cells in the X direction
c        numcols       I  number of cells in the Y direction
c        numlays       I  number of cells in the Z direction
c        probing_tools C  keyword for current probing tools option
c        this_proc     I  process ID for this processor
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer      :: numgrds
         integer      :: numrows(numgrds)
         integer      :: numcols(numgrds)
         integer      :: numlays(numgrds)
         character*10    probing_tools
         integer      :: this_proc
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvec2d
         integer :: mvec3d
         integer :: i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         mvec2d = 0
         mvec3d = 0
         do i=1,numgrds
            mvec2d = mvec2d + numrows(i) * numcols(i)
            mvec3d = mvec3d + numrows(i) * numcols(i) * numlays(i)
         enddo
c
         allocate( ipacl_2d (mvec2d) )
         do i = 1,mvec2d
           ipacl_2d(i) = -9
         enddo
c
         if( this_proc .GT. 0 .OR. (probing_tools .NE. 'RTRAC' .AND.
     &                              probing_tools .NE. 'RTCMC') ) then
            allocate( ipacl_3d (mvec3d) )
            do i = 1,mvec3D
              ipacl_3d(i) = -9
            enddo
         endif
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_PROCAN_IPA
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_PROCAN_NULL
c-----------------------------------------------------------------------
c 
         subroutine alloc_procan_null()
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( cipr     (NPAPRC,1,1) )
c        
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_PROCAN_NULL
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_RTRAC_CEL
c-----------------------------------------------------------------------
c
         subroutine alloc_rtrac_cel(numgrds,numcols,numrows,numlays)
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
c        numlays    I  number of vertical layers
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numgrds
         integer :: numrows(numgrds)
         integer :: numcols(numgrds)
         integer :: numlays(numgrds)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvec3d
         integer :: i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         mvec3d = 0
         do i=1,numgrds
            mvec3d = mvec3d + numrows(i) * numcols(i) * numlays(i)
         enddo
c
         allocate( ipacl_3d (mvec3d) )
         do i = 1,mvec3D
           ipacl_3d(i) = -9
         enddo
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_RTRAC_CEL
c-----------------------------------------------------------------------
c
      end Module

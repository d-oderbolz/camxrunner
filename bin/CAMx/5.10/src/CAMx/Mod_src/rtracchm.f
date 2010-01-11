      Module rtracchm
      include 'rtracchm.com'
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the RTRACCHM.COM
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
c   BEGIN SUBROUTINE ALLOC_RTRACCHM
c-----------------------------------------------------------------------
c
         subroutine alloc_rtracchm(numgrds,numcols,numrows,numspcs,numtracs)
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
c        numspcs    I  number of model species
c        numtracs   I  number of tracer species
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numgrds
         integer :: numrows(numgrds)
         integer :: numcols(numgrds)
         integer :: numspcs
         integer :: numtracs
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( lsecnd  (numtracs) )
         allocate( lreg    (numtracs) )
         allocate( ksec    (numtracs) )
         allocate( rtlbnd  (numtracs) )
         allocate( rthlaw  (numtracs) )
         allocate( rttfact (numtracs) )
         allocate( rtdrate (numtracs) )
         allocate( rtreact (numtracs) )
         allocate( rtscale (numtracs) )
         allocate( rtdens  (numtracs) )
         allocate( rtlcut  (numtracs) )
         allocate( rtucut  (numtracs) )
c
         allocate( jnum    (numtracs) )
         allocate( rtjfact (numtracs) )
         allocate( aoh     (numtracs) )
         allocate( eaoh    (numtracs) )
         allocate( boh     (numtracs) )
         allocate( troh    (numtracs) )
         allocate( ano3    (numtracs) )
         allocate( eano3   (numtracs) )
         allocate( bno3    (numtracs) )
         allocate( trno3   (numtracs) )
         allocate( ao3     (numtracs) )
         allocate( eao3    (numtracs) )
         allocate( bo3     (numtracs) )
         allocate( tro3    (numtracs) )
c
         allocate( ircprt (MXRTCEL) )
         allocate( jrcprt (MXRTCEL) )
         allocate( krcprt (MXRTCEL) )
         allocate( idomrt (MXRTCEL) )
         allocate( rcpdcy (MXRTCEL, numspcs) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_RTRACCHM
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_RTRACCHM_NULL
c-----------------------------------------------------------------------
c
         subroutine alloc_rtracchm_null()
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
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c    
         allocate( vdeprt (3) )
c
         return
         end subroutine
c
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_TRACER_VDEP
c-----------------------------------------------------------------------
c
         subroutine alloc_tracer_vdep(numgrds,numcols,numrows,numtracs)
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
c        numgrds    I  number of grids
c        numcols    I  number of cells in the X direction
c        numrows    I  number of cells in the Y direction
c        numtracs   I  number of tracer species
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numgrds
         integer :: numrows(numgrds)
         integer :: numcols(numgrds)
         integer :: numtracs
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
         do i=1,numgrds
            mvec2d = mvec2d + numrows(i) * numcols(i)
         enddo
c
         allocate( vdeprt (mvec2d * numtracs) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_TRACER_VDEP
c-----------------------------------------------------------------------
c
      end Module

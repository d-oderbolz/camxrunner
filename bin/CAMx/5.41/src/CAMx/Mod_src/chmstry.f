      Module chmstry
      include 'chmdat.inc'
      include 'chmstry.inc'
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the CHMSTRY.COM
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
c        12/15/08    --gwilson--  Added code to handle averaging of
c                                 radicals
c        03/29/11    --cemery--   Support in-line TUV with aerosol optical depth
c
c-----------------------------------------------------------------------
c
      Contains
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_CHMSTRY
c-----------------------------------------------------------------------
c
         subroutine alloc_chmstry(numgrds,numspcs,numrxns,
     &                                                 numpht1,numpht2)
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
c        numspcs    I  number of species
c        numrxns    I  number of reactions
c        numpht1    I  number of primary photalysis reactions
c        numpht2    I  number of secondary photalysis reactions
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numgrds
         integer :: numspcs
         integer :: numrxns
         integer :: numpht1
         integer :: numpht2
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
         allocate( narspc  (numgrds) )
         allocate( lgas    (numspcs) )
c
         allocate( ltdep   (numrxns) )
         allocate( lpdep   (numrxns) )
         allocate( bdnl    (numspcs+1) )
c
         allocate( idphot1 (numpht1) )
         allocate( idphot2 (numpht2) )
         allocate( idphot3 (numpht2) )
         allocate( phtscl  (numpht2) )
c
         allocate( spname  (numspcs+1) )
         allocate( depsp   (4*(numspcs)) )
c
         allocate( lbcmap  (numspcs) )
         allocate( lavmap  (numspcs) )
         allocate( licmap  (numspcs,numgrds) )
         allocate( larmap  (numspcs,numgrds) )
         allocate( lptmap  (numspcs) )
         allocate( ldepmap (numspcs) )
c
         allocate( rktbl(numrxns,NTEMPR,NPRESR) )
         allocate( prkn(NZEN,numpht1,NHGHT,NHAZE,NALB,NOZN) )
c
         allocate( tempr  (NTEMPR) )
         allocate( presr  (NPRESR) )
         allocate( htint  (NHGHT)  )
         allocate( zenint (NZEN)   )
c
         allocate( henry0   (numspcs) )
         allocate( tfact    (numspcs) )
         allocate( diffrat  (numspcs) )
         allocate( f0       (numspcs) )
         allocate( rscale   (numspcs) )
c
         allocate( roprt    (numspcs)   )
         allocate( dcut     (numspcs,2) )
         allocate( bext     (numspcs)   )
         allocate( ssa      (numspcs)   )
         allocate( rhadj    (numspcs)   )
c
         allocate( time_aero (numgrds) )
         allocate( aero_dt   (numgrds) )
         allocate( date_aero (numgrds) )

         zenint(1) = 0.
         zenint(2) = 10.
         zenint(3) = 20.
         zenint(4) = 30.
         zenint(5) = 40.
         zenint(6) = 50.
         zenint(7) = 60.
         zenint(8) = 70.
         zenint(9) = 78.
         zenint(10) = 86.
c
         do i=1,numgrds
           narspc(i) = 0
         enddo
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_CHMSTRY
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_CHMSTRY_AVG
c-----------------------------------------------------------------------
c
         subroutine alloc_chmstry_avg(numspcs)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
         use camx_includes
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
         allocate( spavg   (numspcs)   )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_CHMSTRY_AVG
c-----------------------------------------------------------------------
c
      end Module

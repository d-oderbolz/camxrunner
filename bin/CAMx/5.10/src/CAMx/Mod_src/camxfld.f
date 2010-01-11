C*** CAMXFLD
c
      Module camxfld
      include 'camxfld.com'
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the CAMXFLD.COM
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
c    BEGIN SUBROUTINE ALLOC_CAMXFLD
c-----------------------------------------------------------------------
c      
         subroutine alloc_camxfld(numgrds,numcols,numrows,numlays,
     &                           numspcs,numavg,numdeps,numrads,avg_3d)                            
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
c    This routine allocates the met and concentration fields that depend
c    on grid size and number of species. This version is called by the
c    master node, which needs space for the entire domain.    

c    Argument descriptions:
c     Input:  
c        numgrds    I  number of grids
c        numcols    I  number of cells in the X direction
c        numrows    I  number of cells in the Y direction
c        numlays    I  number of cells in the Z direction
c        numspcs    I  number of modeled species
c        numavg     I  number of species requested for output
c        numrads    I  number of radicals
c        numdeps    I  number of species in deposition array
c        avg_3d     L  flag that determines if doing 3-D averages
c     Output:  
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
         integer :: numavg
         integer :: numdeps
         integer :: numrads
         logical :: avg_3d(numgrds)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvec2d
         integer :: mvec3d
         integer :: mvecem
         integer :: mvec4d
         integer :: mvecrd
         integer :: mvec2a
         integer :: mvec3a
         integer :: mveclu
         integer :: mvecdp
         integer :: mvecavg
         integer :: i, j, k

         integer :: ierr
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  -- calculate the size of the arrays ---
c
         mvec2d = 0
         mvec3d = 0
         mvecavg = 0
         do i=1,numgrds
            mvec2d = mvec2d + numrows(i) * numcols(i)
            mvec3d = mvec3d + numrows(i) * numcols(i) * numlays(i)
            if( avg_3d(i) ) then
               mvecavg = mvecavg + numrows(i) * numcols(i) *
     &                                          numlays(i) * numavg
            else
               mvecavg = mvecavg + numrows(i) * numcols(i) * numavg
            endif
         enddo
         mvecem = mvec2d * numspcs
         mvec4d = mvec3d * numspcs
         mvec2a = MAXVAL(numrows(1:numgrds))*MAXVAL(numcols(1:numgrds))
         mvec3a = MAXVAL(numrows(1:numgrds))*
     &              MAXVAL(numcols(1:numgrds))*MAXVAL(numlays(1:numgrds))
         mveclu = mvec2d * NLU
         mvecdp = mvec2d * numdeps * 3
         mvecrd = mvec3d * numrads
         mvecscr=mvec3a*numspcs+ex_scratch
         mvecscr_dp=mvec2a*3*numdeps+ex_scratch
c
c ---- allocate the arrays that are 2-D fields ---
c
         allocate( cellon (mvec2d) )
         allocate( cellat (mvec2d) )
         allocate( mapscl (mvec2d) )
         allocate( tsurf  (mvec2d) )
         allocate( topo   (mvec2d) )
         allocate( pspt   (mvec2d) )
         allocate( sfcz0  (mvec2d) )
c
c --- allocate the 3-D fields that do not depend on species ---
c
         allocate( windu  (mvec3d) )
         allocate( windv  (mvec3d) )
         allocate( pupt   (mvec3d) )
         allocate( pvpt   (mvec3d) )
         allocate( tempk  (mvec3d) )
         allocate( ptpt   (mvec3d) )
         allocate( press  (mvec3d) )
         allocate( pppt   (mvec3d) )
         allocate( height (mvec3d) )
         allocate( phpt   (mvec3d) )
         allocate( rkv    (mvec3d) )
         allocate( pkpt   (mvec3d) )
         allocate( water  (mvec3d) )
         allocate( pwpt   (mvec3d) )
         allocate( fcloud (mvec3d) )
         allocate( depth  (mvec3d) )
         allocate( rkx    (mvec3d) )
         allocate( rky    (mvec3d) )
         allocate( cwc    (mvec3d) )
         allocate( pwr    (mvec3d) )
         allocate( pws    (mvec3d) )
         allocate( pwg    (mvec3d) )
         allocate( cod    (mvec3d) )
         allocate( cldtrns(mvec3d) )
         allocate( cph    (mvec3d) )
c
c  --- allocate arrays that depend on some kine if species number ---
c
         allocate( aremis (mvecem) )
c
         allocate( conc   (mvec4d) )
c
         allocate( avcnc  (mvecavg) )
c
         if (lchem) then
            allocate( cncrad (mvecrd) )
         else
            allocate( cncrad (1) )
         end if
c
         allocate( entrn  (mvec3d) )
         allocate( dilut  (mvec3d) )
c
         allocate( vdep   (mvecem) )
         allocate( fsurf  (mveclu) )
         allocate( depfld (mvecdp) )
c
c  --- allocate the arrays used for scratch ---
c
         allocate( scr1(mvecscr) )
         allocate( scr1_dp(mvecscr_dp) )
c
c   --- allocate arrays used for mass summary ---
c
         allocate( xmass   (numspcs,numgrds)    )
         allocate( xmass0  (numspcs,numgrds)    )
         allocate( armass  (numspcs,numgrds)    )
         allocate( ptmass  (numspcs,numgrds)    )
         allocate( fluxes  (numspcs*11,numgrds) )
         allocate( xmschem (numspcs,numgrds)    )
         allocate( xmsold  (numspcs,numgrds)    )
         allocate( resid   (numspcs,numgrds)    )
         allocate( xmsfin  (numspcs,numgrds)    )
         allocate( xmstmp  (numspcs,numgrds)    )
         allocate( pigdump (numspcs,numgrds)    )
         allocate( pigmass (numspcs,numgrds)    )
         allocate( pgmserr (numspcs,numgrds)    )
c
         allocate( tarmass (numspcs,numgrds)    )
         allocate( tptmass (numspcs,numgrds)    )
         allocate( tfluxes (numspcs*12,numgrds) )
         allocate( tresid  (numspcs,numgrds)    )
         allocate( txmschem(numspcs,numgrds)    )
         allocate( txmsfin (numspcs,numgrds)    )
c
c  --- initialize to zero ---
c
         do j=1,numgrds
           do i=1,numspcs
             xmass(i,j) = 0.
             xmass0(i,j) = 0.
             armass(i,j) = 0.
             ptmass(i,j) = 0.
             xmschem(i,j) = 0.
             xmsold(i,j) = 0.
             resid(i,j) = 0.
             xmsfin(i,j) = 0.
             xmstmp(i,j) = 0.
             pigdump(i,j) = 0.
             pigmass(i,j) = 0.
             pgmserr(i,j) = 0.
             tarmass(i,j) = 0.
             tptmass(i,j) = 0.
             tresid(i,j) = 0.
             txmschem(i,j) = 0.
           enddo
           do i=1,numspcs*11
             fluxes(i,j) = 0.
           enddo
           do i=1,numspcs*12
             tfluxes(i,j) = 0.
           enddo
         enddo
c
c  --- allocate the fields used to walk the met ---
c
         allocate( hnxt    (mvec3d) )
         allocate( pnxt    (mvec3d) )
         allocate( unxt    (mvec3d) )
         allocate( vnxt    (mvec3d) )
         allocate( tnxt    (mvec3d) )
         allocate( tsnxt   (mvec3d) )
         allocate( knxt    (mvec3d) )
         allocate( wnxt    (mvec3d) )
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c    END SUBROUTINE ALLOC_CAMXFLD
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN SUBROUTINE ALLOC_CAMXFLD_NODE
c-----------------------------------------------------------------------
c
         subroutine alloc_camxfld_node(numspcs, numgrds, numavg,
     &             numdeps, numrads, numcols, numrows, numlays, avg_3d)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
         use camx_includes
         use node_mod
c
         implicit none
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    This routine allocates the met and concentration fields that depend
c    on grid size and number of species. This version is called by the
c    compute nodes, which need space for the just the slice.
c
c    Argument descriptions:
c     Input:  
c        numspcs    I  number of modeled species
c        numgrds    I  number of grids
c        numavg     I  number of species requested for output
c        numdeps    I  number of depositions species
c        numrads    I  number of radicals
c        numcols    I  number of cells in the X direction
c        numrows    I  number of cells in the Y direction
c        numlays    I  number of cells in the Z direction
c        avg_3d     L  flag that determines if doing 3-D averages
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c                    
         integer :: numspcs
         integer :: numgrds
         integer :: numavg
         integer :: numdeps
         integer :: numrads
         integer :: numcols(numgrds)
         integer :: numrows(numgrds)
         integer :: numlays(numgrds)
         logical :: avg_3d(numgrds)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvec2d
         integer :: mvec3d
         integer :: mvec3d_full
         integer :: mvecem
         integer :: mvec4d
         integer :: mvecrd
         integer :: mvec2a
         integer :: mvec3a
         integer :: mveclu
         integer :: mvecdp
         integer :: mvecavg
         integer :: i, j, k

         integer :: ierr
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  -- calculate the size of the arrays ---
c
         mvec2d = 0
         mvec3d = 0
         mvecavg = 0
         mvec3d_full = 0
         do i=1,numgrds
            mvec2d = mvec2d + mmyp(i) * mmxp(i)
            mvec3d = mvec3d + mmyp(i) * mmxp(i) * mmzp(i)
            mvec3d_full = mvec3d_full + 
     &                       numcols(i) * numrows(i) * numlays(i)
            if( avg_3d(i) ) then
               mvecavg = mvecavg + mmyp(i) * mmxp(i) * mmzp(i) * numavg
            else
               mvecavg = mvecavg + mmyp(i) * mmxp(i) * numavg
            endif
         enddo
         mvecem = mvec2d * numspcs
         mvec4d = mvec3d * numspcs
         mvec2a = MAXVAL(mmxp(1:numgrds))*MAXVAL(mmyp(1:numgrds))
         mvec3a = MAXVAL(mmxp(1:numgrds))*
     &                MAXVAL(mmyp(1:numgrds))*MAXVAL(mmzp(1:numgrds))
         mveclu = mvec2d * NLU
         mvecdp = mvec2d * numdeps * 3
         mvecrd = mvec3d * numrads
         mvecscr=mvec3a*numspcs+ex_scratch
         mvecscr_dp=mvec2a*3*numdeps+ex_scratch
c
c ---- allocate the arrays that are 2-D fields ---
c
         allocate( cellon (mvec2d) )
         allocate( cellat (mvec2d) )
         allocate( mapscl (mvec2d) )
         allocate( tsurf  (mvec2d) )
         allocate( topo   (mvec2d) )
         allocate( pspt   (mvec2d) )
         allocate( sfcz0  (mvec2d) )
c
c --- allocate the 3-D fields that do not depend on species ---
c
         allocate( windu  (mvec3d) )
         allocate( windv  (mvec3d) )
         allocate( pupt   (mvec3d) )
         allocate( pvpt   (mvec3d) )
         allocate( tempk  (mvec3d) )
         allocate( ptpt   (mvec3d) )
         allocate( press  (mvec3d) )
         allocate( pppt   (mvec3d) )
         allocate( height (mvec3d_full) )
         allocate( phpt   (mvec3d_full) )
         allocate( rkv    (mvec3d) )
         allocate( pkpt   (mvec3d) )
         allocate( water  (mvec3d) )
         allocate( pwpt   (mvec3d) )
         allocate( fcloud (mvec3d) )
         allocate( depth  (mvec3d_full) )
         allocate( rkx    (mvec3d) )
         allocate( rky    (mvec3d) )
         allocate( cwc    (mvec3d) )
         allocate( pwr    (mvec3d) )
         allocate( pws    (mvec3d) )
         allocate( pwg    (mvec3d) )
         allocate( cod    (mvec3d) )
         allocate( cldtrns(mvec3d) )
         allocate( cph    (mvec3d) )
c
c  --- allocate arrays that depend on some kine if species number ---
c
         allocate( aremis (mvecem) )
c
         allocate( conc   (mvec4d) )
c
         allocate( avcnc  (mvecavg) )

         if(lchem) then
            allocate( cncrad (mvecrd) )
         else
            allocate( cncrad (1) )
         end if
c
         allocate( entrn  (mvec3d) )
         allocate( dilut  (mvec3d) )
c
         allocate( vdep   (mvecem) )
         allocate( fsurf  (mveclu) )
         allocate( depfld (mvecdp) )
c
c  --- allocate the arrays used for scratch ---
c
         allocate( scr1(mvecscr) )
         allocate( scr1_dp(mvecscr_dp) )
c
c   --- allocate arrays used for mass summary ---
c
         allocate( xmass   (numspcs,numgrds)    )
         allocate( xmass0  (numspcs,numgrds)    )
         allocate( armass  (numspcs,numgrds)    )
         allocate( ptmass  (numspcs,numgrds)    )
         allocate( fluxes  (numspcs*11,numgrds) )
         allocate( xmschem (numspcs,numgrds)    )
         allocate( xmsold  (numspcs,numgrds)    )
         allocate( resid   (numspcs,numgrds)    )
         allocate( xmsfin  (numspcs,numgrds)    )
         allocate( xmstmp  (numspcs,numgrds)    )
         allocate( pigdump (numspcs,numgrds)    )
         allocate( pigmass (numspcs,numgrds)    )
         allocate( pgmserr (numspcs,numgrds)    )
c
         allocate( tarmass (numspcs,numgrds)    )
         allocate( tptmass (numspcs,numgrds)    )
         allocate( tfluxes (numspcs*12,numgrds) )
         allocate( tresid  (numspcs,numgrds)    )
         allocate( txmschem(numspcs,numgrds)    )
         allocate( txmsfin (numspcs,numgrds)    )
c
c  --- initialize to zero ---
c
         do j=1,numgrds
           do i=1,numspcs
             xmass(i,j) = 0.
             xmass0(i,j) = 0.
             armass(i,j) = 0.
             ptmass(i,j) = 0.
             xmschem(i,j) = 0.
             xmsold(i,j) = 0.
             resid(i,j) = 0.
             xmsfin(i,j) = 0.
             xmstmp(i,j) = 0.
             pigdump(i,j) = 0.
             pigmass(i,j) = 0.
             pgmserr(i,j) = 0.
             tarmass(i,j) = 0.
             tptmass(i,j) = 0.
             tresid(i,j) = 0.
             txmschem(i,j) = 0.
           enddo
           do i=1,numspcs*11
             fluxes(i,j) = 0.
           enddo
           do i=1,numspcs*12
             tfluxes(i,j) = 0.
           enddo
         enddo
c
c  --- allocate the fields used to walk the met ---
c
         allocate( hnxt    (mvec3d) )
         allocate( pnxt    (mvec3d) )
         allocate( unxt    (mvec3d) )
         allocate( vnxt    (mvec3d) )
         allocate( tnxt    (mvec3d) )
         allocate( tsnxt   (mvec3d) )
         allocate( knxt    (mvec3d) )
         allocate( wnxt    (mvec3d) )
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c    END SUBROUTINE ALLOC_CAMXFLD_NODE
c-----------------------------------------------------------------------
c
      end Module

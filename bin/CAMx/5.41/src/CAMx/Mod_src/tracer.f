      Module tracer
      include 'tracer.inc'
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c        This allocates the dynamic memory arrays in the TRACER.COM
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
c       03/15/09     Added code for deposition output for tracers
c       10/29/09     Added code for RTRAC surface model
c       11/4/09      Removed input top concentrations
c       11/9/09      Added new routine to deallocate and reallocate
c                    for timing tracers
c       11/06/12     Fixed Walls of Cells receptors for MPI
c
c-----------------------------------------------------------------------
c
      Contains
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_TRACER
c-----------------------------------------------------------------------
c
         subroutine alloc_tracer(numgrps,numgrds,numcols,numrows,numlays,numspcs)
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
c        numgrps    I  number of emissions groups
c        numgrds    I  number of grids
c        numcols    I  number of cells in the X direction
c        numrows    I  number of cells in the Y direction
c        numlays    I  number of cells in the Z direction
c        numspcs    I  number of model species
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numgrps
         integer :: numgrds
         integer :: numrows(numgrds)
         integer :: numcols(numgrds)
         integer :: numlays(numgrds)
         integer :: numspcs
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvcola
         integer :: mvrowa
         integer :: i, j, k
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         mvrowa = MAXVAL(numrows(1:numgrds))
         mvcola = MAXVAL(numcols(1:numgrds))
c
         allocate( iormap(numgrds) )
         allocate( iowsfc(numgrds) )
         allocate( iowptdep(numgrds) )
         allocate( iorrtsrf(numgrds) )
         allocate( iowrtsrf(numgrds) )
         allocate( sfcfil(numgrds) )
         allocate( ptdepfil(numgrds) )
         allocate( smpfil(numgrds) )
         allocate( mapfil(numgrds) )
         allocate( rtsrfin(numgrds) )
         allocate( rtsrfout(numgrds) )
         allocate( lmapfl(numgrds) )
         allocate( nxcell(numgrds) )
         allocate( nycell(numgrds) )
         if( .NOT. allocated(lddmcalc) )
     &                     allocate( lddmcalc(numgrds) )
c
         allocate( iortpt(numgrps+1)               )
         allocate( tptfil(numgrps+1)               )
         allocate( nspcpt(0:numgrps+1)             )
         allocate( ltptfl(0:numgrps+1)             )
         allocate( idxpts(0:numgrps+1,numspcs*100) )
c
         allocate( iortem(numgrds,numgrps+1)               )
         allocate( temfil(numgrds,numgrps+1)               )
         allocate( ltemfl(numgrds,0:numgrps+1)             )
         allocate( nspcem(numgrds,0:numgrps+1)             )
         allocate( idxems(numgrds,0:numgrps+1,numspcs*100) )
c
         allocate( lusespc(numspcs) )
         allocate( lvocsp (numspcs) )
         allocate( lvocsoa(numspcs) )
         allocate( lhrvoc(numspcs) )
         allocate( lnoxsp (numspcs) )
         allocate( lo3sp  (numspcs) )
         allocate( crbnum (numspcs) )
         allocate( mwspec (numspcs) )
         allocate( rkohrt (numspcs) )
         allocate( rmirrt (numspcs) )
c
c   --- initialize the species pointers ---
c
         do i=0,numgrps+1
            do j=1,numspcs*100
               idxpts(i,j) = -9
               do k=1,numgrds
                 idxems(k,i,j) = -9
               enddo
            enddo
         enddo
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_TRACER
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_TRACER_FULL
c-----------------------------------------------------------------------
c
         subroutine alloc_tracer_full(numgrds,numcols,numrows)
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
c        numrows    I  number of cells in the X direction
c        numcols    I  number of cells in the Y direction
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numgrds
         integer :: numrows(numgrds)
         integer :: numcols(numgrds)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvcola
         integer :: mvrowa
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         mvrowa = MAXVAL(numrows(1:numgrds))
         mvcola = MAXVAL(numcols(1:numgrds))
c
         allocate( igrmap(numgrds,mvcola,mvrowa) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_TRACER_FULL
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_TRACER_CLASS
c-----------------------------------------------------------------------
c
         subroutine alloc_tracer_class(numspcs)
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
c        numspcs    I  number of model species
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
         allocate( trspmap(numspcs,ntrcls) )
         allocate( fluxmap(numspcs,ntrcls) )
         allocate( yratmap(numspcs,ntrcls) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_TRACER_CLASS
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_TRACER_PTSRCE
c-----------------------------------------------------------------------
c
         subroutine alloc_tracer_ptsrce(numpts)
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
c        numpts     I  number of point sources
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numpts
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( ipigmap(numpts) )
         allocate( ipiggrp(numpts) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_TRACER_PTSRCE
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_TRACER_SPECS
c-----------------------------------------------------------------------
c
         subroutine alloc_tracer_specs(numgrds,numcols,numrows,numlays,
     &                                                pt_string,mvecedge)
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
c        numrows    I  number of cells in the X direction
c        numcols    I  number of cells in the Y direction
c        numlays    I  number of cells in the Z direction
c        pt_string  C  keyword for type of Probing Tool
c        mvecedge   I  max number of cells on edge of master grid
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
         character*(*) pt_string
         integer :: mvecedge
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvsa2d
         integer :: mvsa3d
         integer :: mvecem
         integer :: mvec4d
         integer :: mvcola
         integer :: mvrowa
         integer :: mvecdry
         integer :: mvec2a
         integer :: mvec3a
         integer :: i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         mvsa2d = 0
         mvsa3d = 0
         do i=1,numgrds
            mvsa2d = mvsa2d + numrows(i) * numcols(i)
            mvsa3d = mvsa3d + numrows(i) * numcols(i) * numlays(i)
         enddo
         mvecem  = mvsa2d * ntotsp
         mvecdry = mvsa2d * notimespc
         mvec4d  = mvsa3d * ntotsp
         mvrowa  = MAXVAL(numrows(1:numgrds))
         mvcola  = MAXVAL(numcols(1:numgrds))
         mvec2a  = mvcola * mvrowa
         mvec3a  = mvcola * mvrowa*MAXVAL(numlays(1:numgrds))
c
         mvecscr_pt = mvec3a*ntotsp+100
         allocate( scr1_pt(mvecscr_pt) )
c
         allocate( ptname(ntotsp) )
         allocate( lsamap(ntotsp) )
         allocate( lsagas(ntotsp) )
         allocate( loutsa(ntotsp) )
         allocate( wtkoh (ntotsp) )
         allocate( wtmir (ntotsp) )
         allocate( yrates(ntotsp) )
         allocate( ptlong(ntotsp) )
         allocate( ptop_fac(ntotsp) )
c
         allocate( conrcp(ntotsp,MXRECP) )
         allocate( volrcp(MXRECP) )
c
         allocate( saemis(mvecem) )
         allocate( pttop(mvecem) )
         allocate( ptavrg(mvecem) )
c
         if( pt_string .EQ. OSAT .OR. pt_string .EQ. GOAT .OR. 
     &            pt_string .EQ. APCA .OR. pt_string .EQ. PSAT ) then
            allocate( ptdryfld(mvecdry) )
            allocate( ptwetfld(mvecdry) )
         else
            allocate( ptdryfld(1) )
            allocate( ptwetfld(1) )
         endif
c
         allocate( ptvdep(mvec2a*ntotsp) )
c
         if( pt_string .EQ. RTRAC .OR. pt_string .EQ. RTCMC ) then
            allocate( puffrt (ntotsp,MXRECTR,MXPIG) )
            allocate( bndrt(4,mvecedge,numlays(1),ntotsp) )
         endif
c
         allocate( ptconc(mvec4d) )
c
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_TRACER_SPECS
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE REALLOC_TRACER_SPECS
c-----------------------------------------------------------------------
c
      subroutine realloc_tracer_specs(numgrds,numcols,numrows,
     &                                                 numlays,numold)
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
c        numrows    I  number of cells in the X direction
c        numcols    I  number of cells in the Y direction
c        numlays    I  number of cells in the Z direction
c        numold     I  number of old species (from restart file)
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
         integer :: numold
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvsa2d
         integer :: mvsa3d
         integer :: mvecem
         integer :: mvec4d
         integer :: mvcola
         integer :: mvrowa
         integer :: mvecdry
         integer :: mvec2a
         integer :: mvec3a
         integer :: i
         logical :: tmp_logical(MXTRSP)
         real    :: tmp_real(MXTRSP)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         mvsa2d = 0
         mvsa3d = 0
         do i=1,numgrds
            mvsa2d = mvsa2d + numrows(i) * numcols(i)
            mvsa3d = mvsa3d + numrows(i) * numcols(i) * numlays(i)
         enddo
         mvecem  = mvsa2d * ntotsp
         mvecdry = mvsa2d * notimespc
         mvec4d  = mvsa3d * ntotsp
         mvrowa  = MAXVAL(numrows(1:numgrds))
         mvcola  = MAXVAL(numcols(1:numgrds))
         mvec2a  = mvcola * mvrowa
         mvec3a  = mvcola * mvrowa*MAXVAL(numlays(1:numgrds))
c
         mvecscr_pt = mvec3a*ntotsp+100
         deallocate( scr1_pt )
         allocate( scr1_pt(mvecscr_pt) )
c
         deallocate( ptname )
         allocate( ptname(ntotsp) )

         deallocate( lsamap )
         allocate( lsamap(ntotsp) )
         do i=1,ntotsp
           lsamap(i) = i
         enddo

         do i=1,numold
           tmp_logical(i) = lsagas(i)
         enddo
         deallocate( lsagas )
         allocate( lsagas(ntotsp) )
         do i=1,numold
           lsagas(i) = tmp_logical(i)
         enddo
         do i=numold+1,ntotsp
           lsagas(i) = .FALSE.
         enddo

         do i=1,numold
           tmp_logical(i) = loutsa(i)
         enddo
         deallocate( loutsa )
         allocate( loutsa(ntotsp) )
         do i=1,numold
           loutsa(i) = tmp_logical(i)
         enddo
         do i=ipttim-1,ntotsp
           loutsa(i) = .TRUE.
         enddo
         do i=1,numold
           tmp_real(i) = wtkoh(i)
         enddo
         deallocate( wtkoh  )
         allocate( wtkoh (ntotsp) )
         do i=1,numold
           wtkoh(i) = tmp_real(i)
         enddo

         do i=1,numold
           tmp_real(i) = wtmir(i)
         enddo
         deallocate( wtmir  )
         allocate( wtmir (ntotsp) )
         do i=1,numold
           wtmir(i) = tmp_real(i)
         enddo

         do i=1,numold
           tmp_real(i) = yrates(i)
         enddo
         deallocate( yrates )
         allocate( yrates(ntotsp) )
         do i=1,numold
           yrates(i) = tmp_real(i)
         enddo

         do i=1,numold
           tmp_real(i) = ptop_fac(i)
         enddo
         deallocate( ptop_fac )
         allocate( ptop_fac(ntotsp) )
         do i=1,numold
           ptop_fac(i) = tmp_real(i)
         enddo
         do i=numold+1,ntotsp
           ptop_fac(i) = 0.
         enddo

         deallocate( ptlong )
         allocate( ptlong(ntotsp) )
c
         deallocate( conrcp )
         allocate( conrcp(ntotsp,MXRECP) )
c
         deallocate( volrcp )
         allocate( volrcp(MXRECP) )
c
         deallocate( saemis )
         deallocate( pttop )
         deallocate( ptavrg )
c
         allocate( saemis(mvecem) )
         allocate( pttop(mvecem) )
         allocate( ptavrg(mvecem) )
c
         deallocate( ptvdep )
         allocate( ptvdep(mvec2a*ntotsp) )
c
         deallocate( ptconc )
         allocate( ptconc(mvec4d) )
c
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE REALLOC_TRACER_SPECS
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_TRACER_PTS
c-----------------------------------------------------------------------
c
         subroutine alloc_tracer_pts(numpnts)
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
c        numpnts    I  number of point sources
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numpnts
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( xlocpt(numpnts) )
         allocate( ylocpt(numpnts) )
         allocate( lpigsa(numpnts) )
c
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_TRACER_PTS
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_TRACER_NULL
c-----------------------------------------------------------------------
c
         subroutine alloc_tracer_null(numspcs,ldoing_pa,
     &                                        numgrids,numcols,numrows)
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
c        numspcs    I  number of model species
c        ldoing_pa  L  flag that determines if PA is turned on
c        numgrids   I  number of grids
c        numrows    I  number of cells in the X direction
c        numcols    I  number of cells in the Y direction
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numspcs
         logical :: ldoing_pa
         integer :: numgrids
         integer :: numcols(numgrids)
         integer :: numrows(numgrids)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvcola
         integer :: mvrowa
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         mvrowa = MAXVAL(numrows(1:numgrids))
         mvcola = MAXVAL(numcols(1:numgrids))
c
         if( .NOT. allocated(pttop) )
     &                allocate( pttop(mvcola*mvrowa*1) )
c
         if( .NOT. ldoing_pa) allocate( ptconc(1) )
         allocate( iptddm(numspcs) )
         allocate( sns(1,1,1,1) )
         allocate( ptwetfld(1) )
         if( allocated(lddmcalc) ) deallocate (lddmcalc)
         allocate( lddmcalc(numgrids) )
c
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_TRACER_NULL
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_TRACER_SAMPLE
c-----------------------------------------------------------------------
c
         subroutine alloc_tracer_sample_io(numsamples,inode)
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
c        numsamples I  number of RTRAC sampling grids
c        inode      I  process ID for this node
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numsamples
         integer :: inode
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         if( inode .EQ. 0 ) allocate( iowsmp (numsamples) )
         allocate( iprtsmp(numsamples) )
c
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_TRACER_SAMPLE
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_TRACER_SAMPLE
c-----------------------------------------------------------------------
c
         subroutine alloc_tracer_sample(numsamples,numcolsmp,
     &                                          numrowsmp,numsmpcels)
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
c        numsamples I  number of RTRAC sampling grids
c        numcolsmp  I  number of columns in each sampling grid
c        numrowsmp  I  number of rows in each sampling grid
c        numsmpcels I  size of the sample conc array
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numsamples
         integer :: numcolsmp(numsamples)
         integer :: numrowsmp(numsamples)
         integer :: numsmpcels
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvsmp2d
         integer :: mvecsmp
         integer :: i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         mvsmp2d = 0
         do i=1,numsamples
            mvsmp2d = mvsmp2d + numrowsmp(i) * numcolsmp(i)
         enddo
         mvecsmp = mvsmp2d * ntotsp
c
         allocate( rtsmpcnc(mvecsmp) ) 
         numsmpcels = mvecsmp
c
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_TRACER_SAMPLE
c-----------------------------------------------------------------------
     
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_DDM_SPECIES
c-----------------------------------------------------------------------
c
         subroutine alloc_ddm_species(numspcs)
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c        
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
         allocate( icddmsp(numspcs) )
         allocate( bcddmsp(numspcs) )
         allocate( emddmsp(numspcs) )
         allocate( iptddm (numspcs) )
c
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_DDM_SPECIES
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_DDM
c-----------------------------------------------------------------------
c
         subroutine alloc_ddm(lalloc_sns,numgrds,numcols,numrows,
     &                   numlays,numspcs,numrxns,numddmrates,
     &                                numddmspcs,numhddmfams,mvecedge)
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
c        lalloc_sns  L  if .TRUE. then allocate the sns array
c        numgrds     I  number of grids
c        numcols     I  number of cells in the X direction
c        numrows     I  number of cells in the Y direction
c        numlays     I  number of cells in the Z direction
c        numspcs     I  number of modeled species
c        numrxns     I  number of reactions
c        numddmrates I  number of DDM rate constant groups
c        numddmspcs  I  number of DDM species
c        numhddmfams I  number of HDDM families
c        mvecedge    I  max number of cells on edge of master grid
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         logical :: lalloc_sns
         integer :: numgrds
         integer :: numrows(numgrds)
         integer :: numcols(numgrds)
         integer :: numlays(numgrds)
         integer :: numspcs
         integer :: numrxns
         integer :: numddmrates
         integer :: numddmspcs
         integer :: numhddmfams
         integer :: mvecedge
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvsa2d
         integer :: mvsa3d
         integer :: mvecem
         integer :: mvec4d
         integer :: mvcola
         integer :: mvrowa
         integer :: mvec2a
         integer :: mvec3a
         integer :: i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         mvsa2d = 0
         mvsa3d = 0
         do i=1,numgrds
            mvsa2d = mvsa2d + numrows(i) * numcols(i)
            mvsa3d = mvsa3d + numrows(i) * numcols(i) * numlays(i)
         enddo
         mvecem = mvsa2d * ntotsp
         mvec4d = mvsa3d * ntotsp
         mvrowa = MAXVAL(numrows(1:numgrds))
         mvcola = MAXVAL(numcols(1:numgrds))
         mvec2a =  mvcola * mvrowa
         mvec3a =  mvcola * mvrowa*MAXVAL(numlays(1:numgrds))
c
         mvecscr_pt = mvec3a*ntotsp+100
c
         allocate( scr1_pt(mvecscr_pt) )
c
         allocate( iphddm(2, numhddmfams ) )
c
         allocate( ptname(ntotsp) )
         allocate( lsamap(ntotsp) )
         allocate( lsagas(ntotsp) )
         allocate( loutsa(ntotsp) )
         allocate( wtkoh (ntotsp) )
         allocate( wtmir (ntotsp) )
         allocate( yrates(ntotsp) )
         allocate( ptlong(ntotsp) )
         allocate( ptop_fac(ntotsp) )
c
         allocate( conrcp(ntotsp,MXRECP) )
         allocate( volrcp(MXRECP) )
c
         allocate( saemis(mvecem) )
         allocate( pttop(mvecem) )
         allocate( ptavrg(mvecem) )
c
         allocate( ptvdep(mvec2a*ntotsp) )
c
         allocate( ptconc(mvec4d) )
c
c  --- allocate arrays for edge cells concentratons ---
c
         allocate( bndddm(4,mvecedge,numlays(1),ntotsp) )
c
         if( lalloc_sns )
     &              allocate( sns(mvcola,mvrowa,numddmspcs,numspcs) )
c
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_DDM
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_PROCAN_INIT
c-----------------------------------------------------------------------
c
         subroutine alloc_procan_init(numgrds,numpaspc)
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
c        numpaspc   I  number of PA "species"
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
         integer :: numgrds
         integer :: numpaspc
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( iowsfc(numgrds) )
         allocate( iowptdep(numgrds) )
         allocate( iorrtsrf(numgrds) )
         allocate( iowrtsrf(numgrds) )
c
         allocate( sfcfil(numgrds) )
         allocate( ptdepfil(numgrds) )
         allocate( smpfil(numgrds) )
         allocate( rtsrfin(numgrds) )
         allocate( rtsrfout(numgrds) )
c
         allocate( ptname(numpaspc) )
         allocate( ptop_fac(numpaspc) )
         allocate( loutsa(numpaspc) )
c
         allocate( lddmcalc(numgrds) )
c
         allocate( pttop(1) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_PROCAN_INIT
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_PROCAN_IRR
c-----------------------------------------------------------------------
c
         subroutine alloc_procan_irr(numgrds,numcols,numrows,
     &                                                 numlays,numspcs)
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
c        numrows    I  number of cells in the X direction
c        numcols    I  number of cells in the Y direction
c        numlays    I  number of cells in the Z direction
c        numspcs    I  number of species
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
         integer :: numspcs
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
         integer :: mvec4d
         integer :: mvcola
         integer :: mvrowa
         integer :: mvec3a
         integer :: mvecscr_pt
         integer :: i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         mvec4d = 0
         do i=1,numgrds
            mvec4d = mvec4d + numrows(i) * numcols(i) * numlays(i) * numspcs
         enddo
         mvcola = MAXVAL(numcols(1:numgrds))
         mvrowa = MAXVAL(numrows(1:numgrds))
         mvec3a =  mvcola * mvrowa*MAXVAL(numlays(1:numgrds))
c
         allocate( ptconc (mvec4d) )
c
         mvecscr_pt = mvec3a*ntotsp+100
         allocate( scr1_pt(mvecscr_pt) )
c
         return
         end subroutine
c
c-----------------------------------------------------------------------
c   BEGIN SUBROUTINE ALLOC_PTWET_NULL
c-----------------------------------------------------------------------
c
         subroutine alloc_ptwet_null()
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
c     Output:  
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
         allocate( ptwetfld(1) )
c
         end subroutine
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_PTWET_NULL
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   END SUBROUTINE ALLOC_PROCAN_IRR
c-----------------------------------------------------------------------
c
      end Module

      subroutine nodes_alloc(numprocs,iproc_id)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use grid
      use grid_nodes
      use chmstry
      use filunit
      use o3colmap
      use bndary
      use camxfld
      use camxcom
      use pigsty
      use ptemiss
      use procan
      use rtracchm
      use rtcmcchm
      use tracer
      use grid_dims, only : maxgrds,maxmach 
      use camxfld   
      use master_mod 
      use node_mod  
c
      implicit none
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c        This routine allocates all of the data structures for the 
c        compute nodes when running in MPI mode.  
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c        numprocs            the number of processes
c     Output:  
c
c    Called by:
c       SIM_INIT
c    Subroutines called:
c       NODES_PASS
c       ALLOC_FILUNIT
c       ALLOC_GRID
c       ALLOC_GRID_NODES
c       BROADCAST_PROCID
c       DOMAIN_DECOMP
c       MPI_BARRIER
c       BROADCAST_GRID_DIMENS
c       INIPTR_NODE
c       INIT_FIELDS_NODE
c       ALLOC_CAMXCOM
c       ALLOC_CHMSTRY
c       ALLOC_CHMSTRY_AVG
c       ALLOC_GRID_ROW
c       ALLOC_GRID_LAY
c       ALLOC_GRID_PTSRC
c       ALLOC_GRID_2D_NODE
c       ALLOC_CAMXFLD_NODE
c       ALLOC_BNDARY
c       ALLOC_PTEMISS
c       ALLOC_O3COL_NODE
c       ALLOC_PIGSTY
c       ALLOC_PROCAN_IPA
c       ALLOC_TRACER_NULL
c       ALLOC_TRACER_CLASS
c       ALLOC_TRACR_SPECS
c       ALLOC_TRACER
c       ALLOC_TRACER_FULL
c       ALLOC_TRACER_PTS
c       ALLOC_TRACER_PTSRCE
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c       12/15/08    --gwilson--  Added code to handle averaging of
c                                radicals
c       03/15/09     --gwilson-- Added code for deposition output 
c                                for tracers
c       10/29/09    --cemery--   Added code for RTRAC surface model
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
      include 'deposit.inc'
      include 'camx_aero.inc'
      include 'soap.inc'
      include 'rtracsrf.inc'
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: numprocs
      integer :: iproc_id
c     
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: ierr, mvecedge
c     
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (.not. lmpi .and. iproc_id .gt. 0) return
c
c  --- send the variables for array dimensioning ---
c
      call nodes_pass(ngrid, 1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nspec, 1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nptsrc,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(ngas,  1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(naero, 1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nspec ,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nrad,  1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nreact,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nphot1,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nphot2,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nlu,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(navspc,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(ndepspc,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nptspc,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(lproca,1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(ltrace,1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(lptdepout,1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(lddm,  1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(lhddm, 1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(lirr,1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(lptsrc,1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(ntotsp,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nsaspc,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(notimespc,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(ntrcls,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(ngroup,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(lsrfmod,1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(lparttn,1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass (nrtgas,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(lrtgas,1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass (nrtaero,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(lrtaero,1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass (nrtrac,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (nrtphot,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (nrtherm,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (nrcprt,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (isolv,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (ijac,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (rtolrtc,1,MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass (atolrtc,1,MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass (ktype,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (nrxnrtc,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (icunit,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (itunit,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (njschm,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (nzschm,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (mtype,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (ngasrtc,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (nfstrtc,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (nslortc,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (neqmrtc,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (nfixrtc,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (ngasschm,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (njactrm,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (nrxupdt,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass (nam_M,10,MPI_CHARACTER,itag,numprocs,iproc_id)
      call nodes_pass (nam_O2,10,MPI_CHARACTER,itag,numprocs,iproc_id)
      call nodes_pass (nam_N2,10,MPI_CHARACTER,itag,numprocs,iproc_id)
      call nodes_pass (nam_H2O,10,MPI_CHARACTER,itag,numprocs,iproc_id)
      call nodes_pass (nam_H2,10,MPI_CHARACTER,itag,numprocs,iproc_id)
      call nodes_pass (nam_CH4,10,MPI_CHARACTER,itag,numprocs,iproc_id)
c
c  --- if this is a compute process, allocate the data structures ---
c
      if (iproc_id .gt. 0) then
         call alloc_filunit(ngrid)
         call alloc_grid()
      endif
c
c  --- pass the allocated variables needed later  ---
c
      call nodes_pass(ncol,       ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nrow,       ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nlay,       ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(iptr2d,     ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(iptr2d_full,ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(iptr3d,     ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(iptr3d_full,ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(iptr4d,     ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(iptrav,     ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(iptrem,     ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(iptrlu,     ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(iptrdp,     ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(ipsa3d,     ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(ipsa2d,     ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(lchem,          1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(l3davg,     ngrid,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(nrateddm,       1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nhddm,          1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nddmsp,  1,MPI_INTEGER,itag,numprocs,iproc_id)
c
c  --- call the routines to setup the slices for each processor ---
c
      call alloc_grid_nodes(ngrid,numprocs)
      call broadcast_procid(iproc_id,numprocs)
      if (iproc_id .eq. 0) call domain_decomp()
      call MPI_Barrier(MPI_COMM_WORLD,ierr)
      call broadcast_grid_dimens(iproc_id,numprocs)
c
c  --- if this is a compute process, allocate the data
c      structures and wait for the data to arrive ---
c
      if (iproc_id .GT. 0) then
         call iniptr_node()         
         call init_fields_node(1)  
         call alloc_camxcom(ngrid)
         call alloc_chmstry(ngrid,nspec,nreact,nphot1,nphot2)
         call alloc_chmstry_avg(navspc)
         call alloc_grid_row(nrow) 
         call alloc_grid_lay()
         call alloc_grid_ptsrc(MAX(1,nptsrc))
         call alloc_grid_2d_node(maxgrds, mmxp, mmyp)
         call alloc_camxfld_node(nspec,ngrid,navspc,ndepspc,
     &                                            ncol,nrow,nlay,l3davg)
         call alloc_bndary(ngrid,mmxp,mmyp,nlay,nspec)
         call alloc_ptemiss(nspec,ngrid)
         call alloc_o3col_node(ngrid, maxgrds, mmxp,mmyp) 
      endif
      call nodes_pass(tectyp,10,MPI_CHARACTER,itag,numprocs,iproc_id)
      call nodes_pass(lpsat_apca,1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(ipigflg,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nreactr,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(npa_cels,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(lsample,1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(lsmptrc,1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(lbckgrd,1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(nsample,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nrtsmpcels,1,MPI_INTEGER,itag,numprocs,iproc_id)
      if( iproc_id .GT. 0 ) then
         if( ipigflg .NE. 0 ) then
            call alloc_pigsty(nspec,nreactr,ngrid,numprocs,ipigflg)
            call alloc_pigsty_vpconc(mmxp,mmyp,mmzp,nspec,ngrid)
            if( lsample ) call alloc_pigsty_sample()
         endif
         call alloc_procan_ipa(ngrid,ncol,nrow,nlay,tectyp,iproc_id)
         if( lproca ) then
           call alloc_procan_init(ngrid,MXCPA)
           call alloc_procan_cells(npa_cels,nspec,nreact)
         endif
         if( .NOT. ltrace .AND. .NOT. lddm .AND. .NOT. lhddm ) then
            call alloc_tracer_null(nspec,lirr,ngrid,ncol,nrow)
            if( lirr ) call alloc_procan_irr(ngrid,mmxp,mmyp,mmzp,ntotsp)
         else
            if( lddm .OR. lhddm ) then
               allocate( rateddm(nrateddm) )
               allocate( iprate(0:nreact, nrateddm ) )
               allocate( ptwetfld(1) )
               allocate( hddmsp(2,nhddm) )
               mvecedge = MAX(ncol(1),nrow(1))
               call alloc_ddm(.TRUE.,ngrid,mmxp,mmyp,nlay,nspec,
     &                           nreact,nrateddm,nddmsp,nhddm,mvecedge)
               call alloc_ddm_species(nspec)
            else
               mvecedge = MAX(ncol(1),nrow(1))
               call alloc_tracer_specs(ngrid,mmxp,mmyp,mmzp,tectyp,mvecedge)
            endif
            call alloc_tracer_class(nspec)
            call alloc_tracer_vdep(ngrid,mmxp,mmyp,ntotsp)
            call alloc_tracer(ngroup,ngrid,mmxp,mmyp,mmzp,nspec)
            call alloc_tracer_full(ngrid,ncol,nrow)
            call alloc_tracer_pts(nptsrc)
            if( lptsrc ) then
              call alloc_tracer_ptsrce(nptsrc)
              call alloc_tracer_sapnts(MXPTSRC,MXTRSP)
            endif
         endif
      endif
      call nodes_pass(lddmcalc,ngrid,MPI_LOGICAL,itag,numprocs,iproc_id)
c
c  --- pass the sampling grid information and allocate the 
c      the gridded arrays for the slices ----
c
      if( ipigflg .GT. 0 .AND. lsample ) then
         call nodes_pass(ismp1,nsample,MPI_INTEGER,itag,
     &                                                numprocs,iproc_id)
         call nodes_pass(ismp2,nsample,MPI_INTEGER,itag,
     &                                                numprocs,iproc_id)
         call nodes_pass(jsmp1,nsample,MPI_INTEGER,itag,
     &                                                numprocs,iproc_id)
         call nodes_pass(jsmp2,nsample,MPI_INTEGER,itag,
     &                                                numprocs,iproc_id)
         call nodes_pass(meshsmp,nsample,MPI_INTEGER,itag,
     &                                                numprocs,iproc_id)
         call nodes_pass(ncolsmp,nsample,MPI_INTEGER,itag,
     &                                                numprocs,iproc_id)
         call nodes_pass(nrowsmp,nsample,MPI_INTEGER,itag,
     &                                                numprocs,iproc_id)
         call nodes_pass(ismpgrd,nsample,MPI_INTEGER,itag,
     &                                                numprocs,iproc_id)
         call nodes_pass(ipsmp,nsample,MPI_INTEGER,itag,
     &                                                numprocs,iproc_id)
         call nodes_pass(xorgsmp,nsample,MPI_INTEGER,itag,
     &                                                numprocs,iproc_id)
         call nodes_pass(yorgsmp,nsample,MPI_INTEGER,itag,
     &                                                numprocs,iproc_id)
         if( iproc_id .GT. 0 ) then
            call alloc_pigsty_smpgrd(navspc,nsample,
     &                                          nrowsmp,ncolsmp,nsmpcels)
            if( lsmptrc ) then
               call alloc_tracer_sample_io(nsample,iproc_id)
               call alloc_tracer_sample(nsample,ncolsmp,nrowsmp,nrtsmpcels)
            endif
         endif
         if( lsmptrc ) then
             call nodes_pass(iprtsmp,nsample,MPI_INTEGER,itag,
     &                                                numprocs,iproc_id)
         endif
      endif
c
c  --- call routine to find the slices with edge cells and send
c      the information to the slices ---
c
      if( iproc_id .EQ. 0 ) call find_edge()
      call nodes_pass(nodeedge,nmachs,MPI_LOGICAL,itag,numprocs,iproc_id)
c
c  --- return to the calling routine ---
c
      end

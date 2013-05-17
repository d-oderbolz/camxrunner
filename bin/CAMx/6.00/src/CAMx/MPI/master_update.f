      subroutine master_update(numprocs,iproc_id)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use grid
      use grid_nodes
      use filunit
      use chmstry
      use o3colmap
      use bndary
      use camxfld
      use camxcom
      use pigsty
      use ptemiss
      use procan
      use rtracchm
      use tracer
      use node_mod
c
      implicit none
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       CAMX
c    Subroutines called:
c       NODE_SEND_1SPECIES_DATA
c       MASTER_RECV_1SPECIES_DATA
c       MASTER_PASS_REAL8
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Modifications:
c       03/15/09     Added code for deposition output for tracers
c       10/29/09     Added code for RTRAC surface model
c       11/06/12     Fixed Wall of Cells receptors for MPI
c       11/06/12     Added routine to pass RTRAC receptors
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
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
      integer :: ilay
      integer :: ispc
      integer :: ircp
      integer :: i
      integer :: nlayav
      real    :: rcptmp(MXTRSP)
c     
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (.NOT. lmpi) return
c
c  --- pass the fields that are dimensioned by mvec4d ---
c
      do i=1,ngrid
         nlayav = nlay(i)
         if( .NOT. l3davg(i) ) nlayav = 1
         do ilay=1,nlayav
            do ispc=1,navspc
               if (iproc_id .gt. 0) then
                  call node_send_1species_data(avcnc(iptrav(i)),i,
     &                                         nlayav,ilay,navspc,ispc,itag)
               else
                  call master_recv_1species_data(avcnc(iptrav(i)),i,
     &                                           nlayav,ilay,navspc,ispc,itag)
               endif
               itag=itag+1
            enddo
         enddo
         do ispc=1,ndepspc*3
            if (iproc_id .gt. 0) then
               call node_send_1species_data(depfld(iptrdp(i)),i,
     &                                      1,1,ndepspc*3,ispc,itag)
            else
               call master_recv_1species_data(depfld(iptrdp(i)),i,
     &                                        1,1,ndepspc*3,ispc,itag)
            endif
            itag=itag+1
         enddo
         do ispc=1,nspec
            if (iproc_id .gt. 0) then
               call node_send_1species_data(vdep(iptrem(i)),i,
     &                                      1,1,nspec,ispc,itag)
            else
               call master_recv_1species_data(vdep(iptrem(i)),i,
     &                                        1,1,nspec,ispc,itag)
            endif
            itag=itag+1
         enddo
      enddo
c
c  --- pass the probing tools concentration fields ---
c
      if( ltrace .OR. lddm .OR. lhddm .OR. lirr ) then
         do i=1,ngrid
c
c  --- pass the probing tool average concentration fields ---
c
            if( .NOT. lirr ) then
               do ispc=1,ntotsp
                  if( iproc_id .gt. 0 ) then
                    call node_send_1species_data  (ptavrg(ipsa2d(i)), i,
     &                                          1, 1, ntotsp, ispc, itag)
                  else
                    call master_recv_1species_data(ptavrg(ipsa2d(i)), i,
     &                                          1, 1, ntotsp, ispc, itag)
                  endif
                  itag=itag+1
                enddo
            endif
c
c  --- pass the probing tool dry deposition fields ----
c
            if( lptdepout ) then
                do ispc=1,notimespc
                  if( iproc_id .gt. 0 ) then
                    call node_send_1species_data  (ptdryfld(ipsadep(i)), i,
     &                                          1, 1, notimespc, ispc, itag)
                  else
                    call master_recv_1species_data(ptdryfld(ipsadep(i)), i,
     &                                          1, 1, notimespc, ispc, itag)
                  endif
                  itag=itag+1
                enddo
c
c  --- pass the probing tool wet deposition fields ----
c
                do ispc=1,notimespc
                  if( iproc_id .gt. 0 ) then
                    call node_send_1species_data  (ptwetfld(ipsadep(i)), i,
     &                                          1, 1, notimespc, ispc, itag)
                  else
                    call master_recv_1species_data(ptwetfld(ipsadep(i)), i,
     &                                          1, 1, notimespc, ispc, itag)
                  endif
                  itag=itag+1
                enddo
            endif
c
c  --- pass the RTRAC soil mass fields ----
c
            if( lsrfmod ) then
                do ispc=1,ntotsp
                  if( iproc_id .gt. 0 ) then
                    call node_send_1species_data  (rtsolmas(ipsa2d(i)), i,
     &                                          1, 1, ntotsp, ispc, itag)
                  else
                    call master_recv_1species_data(rtsolmas(ipsa2d(i)), i,
     &                                          1, 1, ntotsp, ispc, itag)
                  endif
                  itag=itag+1
                enddo
c
c  --- pass the RTRAC veg mass fields ----
c
                do ispc=1,ntotsp
                  if( iproc_id .gt. 0 ) then
                    call node_send_1species_data  (rtvegmas(ipsa2d(i)), i,
     &                                          1, 1, ntotsp, ispc, itag)
                  else
                    call master_recv_1species_data(rtvegmas(ipsa2d(i)), i,
     &                                          1, 1, ntotsp, ispc, itag)
                  endif
                  itag=itag+1
                enddo
            endif
         enddo
c
c  --- pass the Wall of Cells rectpors ---
c
         if( lwalls ) then
           do ircp=1,nrecep
              if( idrcp(ircp) .EQ. IDWAL ) then
                  call nodes_send_walls_back(numprocs,iproc_id,ircp,itag)
              endif
           enddo
         endif
c
c  --- pass the Wall of Cells rectpors ---
c
         if( tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC ) then
             call nodes_send_rtrcp_back(numprocs,iproc_id,itag)
         endif
c
c  --- pass the CPA fields every hour ---
c
         if( lirr ) then
            do i=1,ngrid
               do ilay=1,nlay(i)
                  do ispc=1,ntotsp
                     if (iproc_id .gt. 0) then
                        call node_send_1species_data(ptconc(ipsa3d(i)),i,
     &                                      nlay(i),ilay,ntotsp,ispc,itag)
                     else
                        call master_recv_1species_data(ptconc(ipsa3d(i)),i,
     &                                      nlay(i),ilay,ntotsp,ispc,itag)
                     endif
                     itag=itag+1
                  enddo
               enddo
            enddo
         endif
      endif
c
c========================= Process Analysis Begin ======================
c
c  ---- send back to master node ---
c
      if( lipr ) then
         if( iproc_id .GT. 0 ) then
            do i=1,ngrid
              call initipr(.FALSE.,iproc_id,i,nspec,mmxp(i),
     &                                mmyp(i),nlay(i),conc(iptr4d(i)))
            enddo
         endif
         call nodes_ipr_update(numprocs,iproc_id,npa_cels,nspec)
      endif
      if( lirr ) call nodes_irr_update(numprocs,iproc_id,npa_cels,nreact)
c
c   --- call routine to zero out all Process Analysis data structures ---
c
      if( (lirr .OR. lipr) .AND. iproc_id .GT. 0 ) then
         call pazero()
         if( lipr ) then
            do i=1,ngrid
              call initipr(.TRUE.,iproc_id,i,nspec,mmxp(i),
     &                                mmyp(i),nlay(i),conc(iptr4d(i)))
            enddo
         endif
      endif
c
c========================= Process Analysis End ======================
c
c  --- pass the mass flux arrays ---
c
      call master_pass_xmass(xmass,     nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(xmass0,    nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(armass,    nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(ptmass,    nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(fluxes, nspec*11*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(xmschem,   nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(xmsold,    nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(resid,     nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(xmsfin,    nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(xmstmp,    nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(pigdump,   nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(pigmass,   nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(pgmserr,   nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(tarmass,   nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(tptmass,   nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(tfluxes,nspec*12*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(tresid,    nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(txmschem,  nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
      call master_pass_xmass(txmsfin,   nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                       itag,numprocs,iproc_id                      )
c
c  --- pass the pig sampling grid data ---
c
      if( ipigflg .NE. 0 .AND. lsample ) then
        call nodes_send_pig_sample(numprocs,iproc_id)
        if( lsmptrc ) call nodes_send_rt_sample(numprocs,iproc_id)
      endif
c
      end

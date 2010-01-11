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
      use ahomap
      use bndary
      use camxfld
      use camxcom
      use pigsty
      use ptemiss
      use lsbox
      use procan
      use rtracchm
      use tracer
      use node_mod
c
      implicit none
c
c----CAMx v5.10 090918
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
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Modifications:
c       03/15/09     Added code for deposition output for tracers
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
      include 'flags.com'
      include 'camx_aero.com'
      include 'soap.com'
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
      integer :: ncola
      integer :: nrowa
      integer :: nlaya
      integer :: iprc
      integer :: ilay
      integer :: ispc
      integer :: i
      integer :: nlayav
      integer :: igrd
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
         do ilay=1,nlay(i)
            do ispc=1,nspec
               if (iproc_id .gt. 0) then
                  call node_send_1species_data(conc(iptr4d(i)),i,
     &                                         nlay(i),ilay,nspec,ispc,itag)
               else
                  call master_recv_1species_data(conc(iptr4d(i)),i,
     &                                           nlay(i),ilay,nspec,ispc,itag)
               endif
               itag=itag+1
            enddo
         enddo
c
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
c
c  --- pass the fields that are dimensioned by mvecrd ---
c
         if (lchem) then
            do ilay=1,nlay(i)
               do ispc=1,nrad
                  if (iproc_id .gt. 0) then
                     call node_send_1species_data(cncrad(iptrad(i)),i,
     &                                            nlay(i),ilay,nrad,ispc,itag)
                  else
                     call master_recv_1species_data(cncrad(iptrad(i)),i,
     &                                              nlay(i),ilay,nrad,
     &                                              ispc,itag           )
                  endif
                  itag=itag+1
               enddo
            enddo
         endif
      enddo
c
c  --- pass the probing tools concentration fields ---
c
      if( ltrace .OR. lddm .OR. lhddm .OR. lirr ) then
         do i=1,ngrid
            do ilay=1,nlay(i)
               do ispc=1,ntotsp
                  if (iproc_id .gt. 0) then
                     call node_send_1species_data(ptconc(ipsa3d(i)),i,nlay(i),
     &                                            ilay,ntotsp,ispc,itag       )
                  else
                     call master_recv_1species_data(ptconc(ipsa3d(i)),i,
     &                                              nlay(i),ilay,ntotsp,
     &                                              ispc,itag           )
                  endif
                  itag=itag+1
               enddo
            enddo
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
                do ispc=1,ntotsp
                  if( iproc_id .gt. 0 ) then
                    call node_send_1species_data  (ptdryfld(ipsa2d(i)), i,
     &                                          1, 1, ntotsp, ispc, itag)
                  else
                    call master_recv_1species_data(ptdryfld(ipsa2d(i)), i,
     &                                          1, 1, ntotsp, ispc, itag)
                  endif
                  itag=itag+1
                enddo
c
c  --- pass the probing tool wet deposition fields ----
c
                do ispc=1,ntotsp
                  if( iproc_id .gt. 0 ) then
                    call node_send_1species_data  (ptwetfld(ipsa2d(i)), i,
     &                                          1, 1, ntotsp, ispc, itag)
                  else
                    call master_recv_1species_data(ptwetfld(ipsa2d(i)), i,
     &                                          1, 1, ntotsp, ispc, itag)
                  endif
                  itag=itag+1
                enddo
            endif
         enddo
      endif
c
c========================= Process Analysis Begin ======================
c
c  ---- send back to master node ---
c
      if( lipr ) call nodes_ipr_update(numprocs,iproc_id,npa_cels,nspec)
      if( lirr ) call nodes_irr_update(numprocs,iproc_id,npa_cels,nreact)
c
c   --- call routine to zero out all Process Analysis data structures ---
c
      if( (lirr .OR. lipr) .AND. iproc_id .GT. 0 ) call pazero()
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

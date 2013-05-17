      subroutine bndry_updt(numcols,numrows,numlays,bndtim,bnddate,
     &                      nsteps,numprocs,iproc_id               )
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use filunit
      use camxcom
      use camxfld
      use chmstry
      use grid
      use bndary
      use node_mod
      use tracer
c
      implicit none
c
c----CAMx v5.41 121109
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
c       READBND
c       FLUSH
c       NODE_RECV_1SPECIES_DATA
c       MASTER_SEND_1SPECIES_DATA
c       RDBCRT
c       CLRBDYSA
c       FILBDYSA
c       CLRBDYDDM
c       ZEROS
c       RDBCDDM
c       NODES_PASS
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     11/4/09 -cemery- Removed input top concentrations
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
      include 'mpif.h'
c
c========================= Probing Tool End ============================
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: numcols(*)
      integer :: numrows(*)
      integer :: numlays(*)
c
      real    :: bndtim
c
      integer :: bnddate
      integer :: nsteps
      integer :: numprocs
      integer :: iproc_id
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: mvsa3d
      integer :: ncola
      integer :: nrowa
      integer :: nmx1d
      integer :: nedge
      integer :: i
c
      real    :: tcpu
      real    :: dtime
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- Check if master grid boundary data are to be read ---
c
      if (date .EQ. bnddate .AND. ABS(time-bndtim) .LT. 0.01) then
         if (iproc_id .EQ. 0) then
            write(*,'(a20,$)') 'readbnd ......'
            call readbnd(bndtim,bnddate)
            tcpu = dtime(tarray2)
            write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
            call flush(6)
         endif
c
c   --- if doing MPI, update edge cells on each slice ---
c
         if( lmpi ) then
           nedge = MAX(ncol(1),nrow(1))
           if( iproc_id .EQ. 0 ) 
     &                call master_bndconc(ncol(1),nrow(1),nlay(1),
     &                                          nspec,nedge,conc,bndconc)
            call edge_pass(nodeedge,bndconc,4*nedge*nlay(1)*nspec,MPI_REAL,
     &                                             itag,numprocs,iproc_id) 
            if( iproc_id .NE. 0 ) then
                   if( nodeedge(iproc_id) ) 
     &                     call nodes_bndconc(mi0(1),mj0(1),
     &                        mmxp(1),mmyp(1),ncol(1),nrow(1),nlay(1),
     &                                         nspec,nedge,conc,bndconc)
           endif
         endif
c
c======================== Source Apportion Begin =======================
c
c  --- call routine to clear the old boundary cells and then
c      call routine to fill with new boundary concentrations ---
c
c  --- For the Master Grid ---
c
         if (iproc_id .eq. 0) then
            if (ltrace) then
               if( tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC ) then
                  call rdbcrt(nsteps,numcols(1),numrows(1),numlays(1),
     &                        ntotsp,ptconc(1),tempk(1),press(1)      )
               else
                  call clrbdysa(1,numcols(1),numrows(1),
     &                          numlays(1),ntotsp,ptconc(1))
                  call filbdysa(1,numcols(1),numrows(1),numlays(1),
     &                          nspec,ntotsp,conc(1),ptconc(1)     )
               endif
c
c  --- make sure it is not below lower bound ---
c
               mvsa3d = 0
               ncola = 0
               nrowa = 0
               do i=1,ngrid
                  ncola = MAX(ncola,numcols(i))
                  nrowa = MAX(nrowa,numrows(i))
                  mvsa3d = mvsa3d + numrows(i) * numcols(i) * numlays(i)
               enddo
               nmx1d  = MAX( nrowa, ncola )
               do i=1,mvsa3d*ntotsp
                  ptconc(i) = AMAX1(ptconc(i),BNDLPT)
               enddo
            endif
c
c========================= Source Apportion End ========================
c
c
c============================= DDM Begin ===============================
c
c  --- call routine to clear the old boundary cells and then
c      call routine to fill with new boundary concentrations ---
c
c  --- For the Master Grid ---
c
            if((lddm.OR.lhddm) .AND. nbcddm .GT. 0) then
               call clrbdyddm(numcols(1),numrows(1),
     &                        numlays(1),ntotsp,ptconc(1))
               if (nbcddm .GT. 0) then
                  call rdbcddm(numcols(1),numrows(1),numlays(1),
     &                         ntotsp,ptconc(1),tempk(1),press(1))
               endif
            endif
         endif
c
c  --- update the DDM boundary sensitivites for the MPI slices ---
c
         if( (lddm.OR.lhddm) .AND. lmpi ) then
             nedge = MAX(ncol(1),nrow(1))
             if( iproc_id .EQ. 0 )
     &                call master_bndconc(ncol(1),nrow(1),nlay(1),
     &                                    ntotsp,nedge,ptconc,bndddm)
             call edge_pass(nodeedge,bndddm,4*nedge*nlay(1)*ntotsp,
     &                               MPI_REAL,itag,numprocs,iproc_id)
             if( iproc_id .NE. 0 ) then
               if( nodeedge(iproc_id) )
     &                call nodes_bndconc(mi0(1),mj0(1),
     &                      mmxp(1),mmyp(1),ncol(1),nrow(1),nlay(1),
     &                                     ntotsp,nedge,ptconc,bndddm)
             endif
         endif
c
c========================= Source Apportion Begin ========================
c
c  --- update the RTRAC boundary conditions for the MPI slices ---
c
         if( lmpi .AND. ltrace .AND.
     &           (tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) ) then
             nedge = MAX(ncol(1),nrow(1))
             if( iproc_id .EQ. 0 )
     &                call master_bndconc(ncol(1),nrow(1),nlay(1),
     &                                    ntotsp,nedge,ptconc,bndrt)
             call edge_pass(nodeedge,bndrt,4*nedge*nlay(1)*ntotsp,
     &                               MPI_REAL,itag,numprocs,iproc_id)
             if( iproc_id .NE. 0 ) then
               if( nodeedge(iproc_id) )
     &                call nodes_bndconc(mi0(1),mj0(1),
     &                      mmxp(1),mmyp(1),ncol(1),nrow(1),nlay(1),
     &                                     ntotsp,nedge,ptconc,bndrt)
             endif
         endif
c
c========================= Source Apportion End ========================
c
      endif
c
      end

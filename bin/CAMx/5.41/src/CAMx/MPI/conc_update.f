      subroutine conc_update(numprocs,iproc_id)
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
      use procan
      use rtracchm
      use tracer
      use node_mod
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
c       NODE_SEND_1SPECIES_DATA
c       MASTER_RECV_1SPECIES_DATA
c       MASTER_PASS_REAL8
c
c     Copyright 1996 - 2012
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
      include 'flags.inc'
      include 'camx_aero.inc'
      include 'soap.inc'
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
      integer :: i
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
         enddo
      endif
c
      end

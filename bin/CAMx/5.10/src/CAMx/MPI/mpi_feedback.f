      subroutine mpi_feedback(inest,iparent,icode)
      use grid
      use camxfld
      use tracer
      use procan
      implicit none
c
c----CAMx v5.10 090918
c
c     This routine does the nest to parent feeback calculations for
c     an MPI run. It just calls the routines that perform the tasks
c     necessary for this operation. These include mass balance summary
c     calculations, as well as the sending/receiving routines needed
c     do feedback for all necessary fields.
c                          
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c          
c     Modifications:
c       03/15/09     Added code for deposition output for tracers
c
c     Input arguments:
c        inest    grid ID for the nest
c        iparent  grid ID for the parent
c        icode               flag to select when to sum mass on parent grid
c                              0 = sum both before and after aggregation
c                              1 = sum before aggregation
c                              2 = sum after aggregation
c                             >2 = do not sum
c
c     Output arguments:
c        none
c
c     Subroutines Called:
c
c     Called by:
c        NESTING
c
c     Argument declarations:
c
      integer inest
      integer iparent
      integer icode
c
c     Local variables
c
c
c-----Entry point
c
c  ---- call routine to sum the mass on the parent prior to feedback ---
c
      call before_massum(iparent,icode)
c
c  ---- call routine to send nest values to slice containing parent ---
c
      call node_send_feed(inest,iparent,deltax(1,inest),
     &                          deltay(inest),depth(iptr3d_full(inest)))
c
c  ---- call routine so that parent slice recieves and loads the data ---
c
      call node_get_feed(inest,iparent,deltax(1,iparent),
     &                      deltay(iparent),depth(iptr3d_full(iparent)))
c
c  ---- call routine to sum the mass on the parent after feedback ---
c
      call after_massum(iparent,icode)
c
c  ---- now feedback the radical fields ---
c
      call node_send_feed_rad(inest,iparent,deltax(1,inest),
     &                          deltay(inest),depth(iptr3d_full(inest)))
      call node_get_feed_rad(inest,iparent,deltax(1,iparent),
     &                      deltay(iparent),depth(iptr3d_full(iparent)))
c
c  ---- now feedback the deposition fields ---
c
      call node_send_feed_dp(inest,iparent,
     &                             deltax(1,inest),deltay(inest))
      call node_get_feed_dp(inest,iparent,
     &                             deltax(1,iparent),deltay(iparent))
c
c======================== Probing Tool Begin ===========================
c
c  ---- do the feedback again for probing tools ----
c
      if( ltrace .OR. lddm .OR. lhddm .OR. lirr ) then
         call node_send_feed_pt(inest,iparent,deltax(1,inest),
     &                         deltay(inest),depth(iptr3d_full(inest)))
         call node_get_feed_pt(inest,iparent,deltax(1,iparent),
     &                     deltay(iparent),depth(iptr3d_full(iparent)))
         if( lptdepout ) then
           call node_send_feed_dry(inest,iparent,
     &                             deltax(1,inest),deltay(inest))
           call node_get_feed_dry(inest,iparent,
     &                             deltax(1,iparent),deltay(iparent))
           call node_send_feed_wet(inest,iparent,
     &                             deltax(1,inest),deltay(inest))
           call node_get_feed_wet(inest,iparent,
     &                             deltax(1,iparent),deltay(iparent))
         endif
      endif
c
c======================== Probing Tool End ===========================
c
      return
      end

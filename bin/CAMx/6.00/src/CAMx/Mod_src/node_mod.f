      Module node_mod                                               
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Variable descriptions:
c     Input:
c       nmachs          I  total number of procs
c       machs           I  processor IDs
c       mynum           I  current processor ID
c       master_num      I  processor ID of the master: default = 0
c
c       Grid index:
c
c    These variables are scalars that are set to the value 
c    for the current grid.
c       mxp             I  number of col in slice
c       myp             I  number of row in slice
c       mzp             I  number of layers in slice
c       ia              I  start index of col in computational slice
c       iz              I  end index of col in computational slice
c       ja              I  start index of row in computational slice
c       jz              I  end index of row in computational slice
c       i0              I  offset of start col of slice in full domain
c       j0              I  offset of start row of slice in full domain
c       ibcon           I  bcon flag value: max= 1+2+4+8=15
c                          bit 1=west, bit 2=east, bit 3=south, bit 4=north
c                          deternines if cell is on a true boundary
c
c    These variables are arrays that store values for all grids
c    for the current processor.
c       mmxp            I  number of col in slice
c       mmyp            I  number of row in slice
c       mmzp            I  number of layers in slice
c       mia             I  start index of col in computational slice
c       miz             I  end index of col in computational slice
c       mja             I  start index of row in computational slice
c       mjz             I  end index of row in computational slice
c       mi0             I  offset of start col of slice in full domain
c       mj0             I  offset of start row of slice in full domain
c       mibcon          I  bcon flag value: max= 1+2+4+8=15
c
c    These variables are 2-D arrays that store values for all grids
c    and all of the processor.
c       nodemxp         I  number of col in slice
c       nodemyp         I  number of row in slice
c       nodemzp         I  number of layers in slice
c       nodeia          I  start index of col in computational slice
c       nodeiz          I  end index of col in computational slice
c       nodeja          I  start index of row in computational slice
c       nodejz          I  end index of row in computational slice
c       nodei0          I  offset of start col of slice in full domain
c       nodej0          I  offset of start row of slice in full domain
c       nodeibcon       I  bcon flag value: max= 1+2+4+8=15
c       nodeedge        L  true if slice has an edge cell in master grid
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c       03/15/09     Added code for deposition output for tracers
c       10/29/09     Added code for RTRAC surface model
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use grid_dims, only : MAXGRDS,MAXMACH   ! MAXGRDS=10, MAXMACH=256
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer          :: nmachs
      integer          :: machs(MAXMACH)
      integer          :: mynum
      integer          :: master_num
c
c  ---- Variables: grid index ----
c
      integer          :: mxp
      integer          :: myp
      integer          :: mzp
      integer          :: ia
      integer          :: iz
      integer          :: ja
      integer          :: jz
      integer          :: i0
      integer          :: j0
      integer          :: ibcon
c
c  ---- One dimensional grid index, dimensioned by MAXGRDS ----
c
      integer, target  :: mmxp(MAXGRDS)
      integer, target  :: mmyp(MAXGRDS)
      integer, target  :: mmzp(MAXGRDS)
      integer          :: mia(MAXGRDS)
      integer          :: miz(MAXGRDS)
      integer          :: mja(MAXGRDS)
      integer          :: mjz(MAXGRDS)
      integer          :: mi0(MAXGRDS)
      integer          :: mj0(MAXGRDS)
      integer          :: mibcon(MAXGRDS)
c
c  ---- Two dimensional grid index, dimensioned by MAXMACH+1 and MAXGRDS ----
c
      integer          :: nodemxp(0:MAXMACH,MAXGRDS)
      integer          :: nodemyp(0:MAXMACH,MAXGRDS)
      integer          :: nodemzp(0:MAXMACH,MAXGRDS)
      integer          :: nodeia(0:MAXMACH,MAXGRDS)
      integer          :: nodeiz(0:MAXMACH,MAXGRDS)
      integer          :: nodeja(0:MAXMACH,MAXGRDS)
      integer          :: nodejz(0:MAXMACH,MAXGRDS)
      integer          :: nodei0(0:MAXMACH,MAXGRDS)
      integer          :: nodej0(0:MAXMACH,MAXGRDS)
      integer          :: nodeibcon(0:MAXMACH,MAXGRDS)
      logical          :: nodeedge(MAXMACH)
c
c     Neighborhood relationships 
c         5: beg_col, beg_row, end_col, end_row, destination_node_id
c         7: the 7th of the second dim of the ipaths(,7,,) array is used to determine
c            the loop limits in fdbackp (aggregation) for averaging the fm over the overlap
c            between the cm node and fm node, rather than always over the full
c            fm node.  It is not used for actually sending stuff.  The
c            ipaths(,6,,) part of the array is still used for sending the
c            block of averaged cm points from the fm node to the cm node.
c         6: means 6 isend_type(s), this 6 in iget_paths is the same as the 
c            first six elements in ipaths(,7,,)
c
      integer          :: ipaths(5,7,MAXGRDS,MAXMACH)
      integer          :: bpaths(5,7,MAXGRDS,MAXMACH)
      integer          :: iget_paths(6,MAXGRDS,MAXMACH)
c
c  ---- Could be the ierr flags for msg passing ----
c
      integer          :: irecv_req(MAXMACH)
      integer          :: isend_req(MAXMACH)
      integer          :: irecv_req_dp(MAXMACH)
      integer          :: isend_req_dp(MAXMACH)
      integer          :: irecv_req_pt(MAXMACH)
      integer          :: isend_req_pt(MAXMACH)
      integer          :: irecv_req_rad(MAXMACH)
      integer          :: isend_req_rad(MAXMACH)
      integer          :: irecv_req_dry(MAXMACH)
      integer          :: isend_req_dry(MAXMACH)
      integer          :: irecv_req_wet(MAXMACH)
      integer          :: isend_req_wet(MAXMACH)
      integer          :: irecv_req_rtsol(MAXMACH)
      integer          :: isend_req_rtsol(MAXMACH)
      integer          :: irecv_req_rtveg(MAXMACH)
      integer          :: isend_req_rtveg(MAXMACH)
cgwilsonc
cgwilsonc  --- this is for the 64-bit machines that use 
cgwilsonc      a 64-bit version of MPICH that is incompatible
cgwilsonc      with our implementation
cgwilsonc
cgwilson      integer(kind=8) :: irecv_req(MAXMACH)
cgwilson      integer(kind=8) :: isend_req(MAXMACH)
cgwilson      integer(kind=8) :: irecv_req_dp(MAXMACH)
cgwilson      integer(kind=8) :: isend_req_dp(MAXMACH)
cgwilson      integer(kind=8) :: irecv_req_pt(MAXMACH)
cgwilson      integer(kind=8) :: isend_req_pt(MAXMACH)
cgwilson      integer(kind=8) :: irecv_req_rad(MAXMACH)
cgwilson      integer(kind=8) :: isend_req_rad(MAXMACH)
cgwilson      integer(kind=8) :: irecv_req_dry(MAXMACH)
cgwilson      integer(kind=8) :: isend_req_dry(MAXMACH)
cgwilson      integer(kind=8) :: irecv_req_wet(MAXMACH)
cgwilson      integer(kind=8) :: isend_req_wet(MAXMACH)
c
c  ---- Values to measure the max length of array buffer ----
c
      integer          :: nbuff_feed
      integer          :: newbuff_nest
      integer          :: nbuff_nest
c
c  ---- An defined type to help transfer the bcon array buffer ----
c
      type nlbc_buffs
         real, pointer :: lbc_send_buff(:)
         real, pointer :: lbc_recv_buff(:)
         real, pointer :: lbc_dp_send_buff(:)
         real, pointer :: lbc_dp_recv_buff(:) 
         real, pointer :: lbc_pt_send_buff(:)
         real, pointer :: lbc_pt_recv_buff(:) 
         real, pointer :: lbc_rad_send_buff(:)
         real, pointer :: lbc_rad_recv_buff(:)
         real, pointer :: lbc_dry_send_buff(:)
         real, pointer :: lbc_dry_recv_buff(:) 
         real, pointer :: lbc_wet_send_buff(:)
         real, pointer :: lbc_wet_recv_buff(:) 
         real, pointer :: lbc_rtsol_send_buff(:)
         real, pointer :: lbc_rtsol_recv_buff(:)
         real, pointer :: lbc_rtveg_send_buff(:)
         real, pointer :: lbc_rtveg_recv_buff(:)
         integer       :: nsend
         integer       :: nrecv
      end type
c
      type (nlbc_buffs) :: node_buffs(MAXMACH)
c
      end module node_mod

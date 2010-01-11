c----CAMx v5.10 090918
c 
c     GRID_NODES.COM contains all grid definitions for the compute
c     nodes in MPI mode.
c                           
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c           
c     Modifications: 
c        none 
c
c
c-----------------------------------------------------------------------
c     Variables to define the computational domain:
c
c     nvals_4d    --  number of values in 4-D arrays for this slice
c     nvals_dp    --  number of values in deposition arrays for this slice
c     nvals_em    --  number of values in emissions arrays for this slice
c     istart_4d   --  starting position in 4-D arrays for this slice
c     istart_dp   --  starting position in depostion arrays for this slice
c     istart_em   --  starting position in emissions arrays for this slice
c
c-----------------------------------------------------------------------
c
      integer, allocatable, dimension(:,:) :: nvals_4d
      integer, allocatable, dimension(:,:) :: nvals_dp
      integer, allocatable, dimension(:,:) :: nvals_em
      integer, allocatable, dimension(:,:) :: istart_4d
      integer, allocatable, dimension(:,:) :: istart_dp
      integer, allocatable, dimension(:,:) :: istart_em

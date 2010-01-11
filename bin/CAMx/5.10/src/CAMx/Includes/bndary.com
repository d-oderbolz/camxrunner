c----CAMx v5.10 090918
c 
c     BNDARY.COM contains all coarse grid boundary information
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
c     ibeg     --  first modeled cell in each row on the West boundary
c     iend     --  last modeled cell in each row on the East boundary
c     jbeg     --  first modeled cell in each column on the South boundary
c     jend     --  last modeled cell in each column on the North boundary
c
c     NOTE:  A value of -999 indicates the entire row or column is in
c            the boundary, and is not modeled.
c
c-----------------------------------------------------------------------
c
      integer, allocatable, dimension(:) :: ibeg
      integer, allocatable, dimension(:) :: iend
      integer, allocatable, dimension(:) :: jbeg
      integer, allocatable, dimension(:) :: jend
c
c-----------------------------------------------------------------------
c     Variables to define concentrations on the TOP boundary:
c
c     caloft   -- invariant TOP boundary concentrations (ppm)
c
c-----------------------------------------------------------------------
c
      real, allocatable, dimension(:) :: caloft
c
c-----------------------------------------------------------------------
c     Variables to define concentrations on the LATERAL boundaries:
c
c     bc -- spatially varying boundary concentrations on each lateral
c           edge (ppm)
c
c-----------------------------------------------------------------------
c
      real, allocatable, dimension(:,:,:,:) :: bc

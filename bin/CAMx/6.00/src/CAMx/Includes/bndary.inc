c----CAMx v6.00 130506
c 
c     BNDARY.COM contains all coarse grid boundary information
c                           
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c           
c     Modifications: 
c        11/4/09 -cemery- Removed input top concentrations
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
c     Variables to define concentrations on the LATERAL boundaries:
c
c     bc -- spatially varying boundary concentrations on each lateral
c           edge (ppm)
c
c-----------------------------------------------------------------------
c
      real, allocatable, dimension(:,:,:,:) :: bc

c----CAMx v5.10 090918
c
c     LSBOXALLOC.COM passes information to LSODE for gas phase chemistry
c     This contains the allocatable arrays.
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c 
      real,             allocatable, dimension(:)   :: rrk
      double precision, allocatable, dimension(:)   :: dbrk
      double precision, allocatable, dimension(:,:) :: jac
c

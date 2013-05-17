      Module master_mod                                          
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Local variable descriptions:
c     Input:
c       nmachines   I  total number of procs
c       machnum     I  processor IDs
c       nxbeg       I  beginning I-cell for domain for the slice
c       nxend       I  ending I-cell for full domain for the slice
c       nybeg       I  beginning J-cell for full domain for the slice
c       nyend       I  ending J-cell for full domain for the slice
c       nxbegc      I  beginning I-cell of computational domain for the slice
c       nxendc      I  ending I-cell of computational domain for the slice
c       nybegc      I  beginning J-cell of computational domain for the slice
c       nyendc      I  ending J-cell of computational domain for the slice
c       ixoff       I  offset for I-cell origin in full domain
c       iyoff       I  offset for J-cell origin in full domain
c       ibcflg      I  bcon flag value: max= 1+2+4+8=15
c                      bit 1=west, bit 2=east, bit 3=south, bit 4=north    
c                      deternines if cell is on a true boundary
c       ixb         I  
c       ixe         I  
c       iyb         I  
c       iye         I  
c     Output:  
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use grid_dims   ! MAXGRDS=10, MAXMACH=256
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c
      integer :: nmachines
      integer, dimension(MAXMACH) :: machnum
c
c     Two dimensional grid index, dimensioned by MAXMACH+1 and MAXGRDS
c
      integer, dimension(0:MAXMACH,MAXGRDS) :: nxbeg
      integer, dimension(0:MAXMACH,MAXGRDS) :: nxend
      integer, dimension(0:MAXMACH,MAXGRDS) :: nybeg
      integer, dimension(0:MAXMACH,MAXGRDS) :: nyend
      integer, dimension(0:MAXMACH,MAXGRDS) :: nxbegc
      integer, dimension(0:MAXMACH,MAXGRDS) :: nxendc
      integer, dimension(0:MAXMACH,MAXGRDS) :: nybegc
      integer, dimension(0:MAXMACH,MAXGRDS) :: nyendc
      integer, dimension(0:MAXMACH,MAXGRDS) :: ixoff
      integer, dimension(0:MAXMACH,MAXGRDS) :: iyoff
      integer, dimension(0:MAXMACH,MAXGRDS) :: ibcflg
      integer, dimension(0:MAXMACH,MAXGRDS) :: ixb
      integer, dimension(0:MAXMACH,MAXGRDS) :: ixe
      integer, dimension(0:MAXMACH,MAXGRDS) :: iyb
      integer, dimension(0:MAXMACH,MAXGRDS) :: iye
c
c     Neighborhood relationships
c         5: beg_col, beg_row, end_col, end_row, destination_node_id
c         7: the 7th of the second dim of the inode_paths_master(,7,,,) array is used to determine
c            the loop limits in fdbackp (aggregation) for averaging the fm over the overlap
c            between the cm node and fm node, rather than always over the full
c            fm node.  It is not used for actually sending stuff.  The
c            inode_paths_master(,6,,,) part of the array is still used for sending the
c            block of averaged cm points from the fm node to the cm node.
c         6: means 6 isend_type(s), this 6 is the same as the first six elements 
c            in inode_paths_master(,7,,,)
c
      integer, dimension(5,7,MAXGRDS,MAXMACH,MAXMACH) :: inode_paths_master
      integer, dimension(6,MAXGRDS,MAXMACH,MAXMACH)   :: iget_paths_master
c
c     length of the array buffer for boundary data transfer
c
      integer, dimension(MAXMACH)                     :: nbuff_nest1
      integer, dimension(MAXMACH)                     :: newbuff_nest1
      integer, dimension(2,MAXMACH,MAXMACH)           :: lbc_buffs
c
c     Some additional nesting parameters
c
      integer, dimension(NXPMAX,MAXGRDS) :: ipm
      integer, dimension(NYPMAX,MAXGRDS) :: jpm
c
      end Module

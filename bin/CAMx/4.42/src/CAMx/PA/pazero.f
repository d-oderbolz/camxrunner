       subroutine pazero
c
c----CAMx v4.42 070603
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Initializes or re-initializes the Process Analysis data structures
c     to zero.  This routine is called at the beginning of each time
c     step.
c
c     Subroutines Called:
c
c
c     Called by:
c        CAMX
c
      implicit none
c
      include 'camx.prm'
      include 'grid.com'
      include 'chmstry.com'
      include 'tracer.com'
      include 'procan.com'
c
      integer i, j, k, igrd, nodes
c
c   ---- initialize the integrated process rates array to zero ---
c
      if( lipr ) then
         do i=1,NPAPRC
           do j=1,MXPACEL
              do k=1,MXSPEC
                 cipr(i,j,k) = 0.
              enddo
           enddo
         enddo 
c
c  ---- initialize the the number of steps to zero ---
c
         do i=1,MXPACEL
           do j=1,MXSPEC
             npastep(i,j) = 0
           enddo
         enddo 
      endif
c
c  --- zero out data structures for the integrated reaction rate data ---
c
      if( lirr ) then
c
c  ---- initialize the integrated reaction rates array to zero ---
c
        do i=1,MXPACEL
            do j=1,MXRXN
               cirr(i,j) = 0.
            enddo
        enddo
c
c  --- call routine to zero out the gridded chemical process analysis array ---
c
        do igrd=1,ngrid
          nodes=ncol(igrd)*nrow(igrd)*nlay(igrd)*ntotsp
          call zeros(ptconc(ipsa3d(igrd)),nodes)
        enddo
      endif
c
      return
      end

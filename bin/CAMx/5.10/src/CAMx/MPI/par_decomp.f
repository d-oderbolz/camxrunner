      subroutine par_decomp(nxp,nyp,nest_factor,nodes,                   
     &                      work,workrow,nblocks,workload,workblock,workcol,
     &                      jrows,jrow,ixb,ixe,iyb,iye                      )
c
      implicit none
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c        This routine decomposes grid domains of size (nnxp,nnyp) into
c        a number, specified by nodes, of rectangular subdomains.  The
c        convention is followed that any internal boundaries (between
c        subdomains) that are parallel to the x-axis run continuously
c        across the full domain, while boundaries parallel to the y-axis
c        may or may not run the full distance across the domain.  For
c        convenience, regions of the domain bounded by adjacent
c        east-west internal boundaries are termed "slabs", while smaller
c        divisions within each slab are termed "blocks".  Each block is
c        required to have a minimum dimension of 6 by 6 grid cells.  If
c        this cannot be satisfied with the given input parameters, the
c        subroutine stops.
c
c        Estimate the number of slabs to be used (aslabs), and compute a
c        final nearest integer value (nslabs) which is limited to
c        allowable values. Zero out array for accumulating number of
c        columns for each node.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c      integer :: jrows(*)   the number of rows in each slab
c      integer :: jrow (*)   the index of the southernmost row in each slab
c     Output:  
c
c    Called by:
c       DOMAIN_DECOMP
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c       03/15/09     Added code for deposition output for tracers
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: nxp
      integer :: nyp
      integer :: nest_factor
      integer :: nodes
c
      real    :: work(nxp,*)
      real    :: workrow(*)
c
      integer :: nblocks(*)
c
      real    :: workload(*)
      real    :: workblock(*)
      real    :: workcol(*)
c
      integer :: jrows(*)
      integer :: jrow (*)
      integer :: ixb(*)
      integer :: ixe(*)
      integer :: iyb(*)
      integer :: iye(*)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
c  --- default relspeed = 1.0 for nodes of uniform speed. ---
c
      real    :: relspeed(256)
      data       relspeed/256*1./
c
      integer :: inode
      integer :: i
      integer :: j
      integer :: islab
      integer :: jnodes
      integer :: nslabs
      integer :: min_blocks
      integer :: nbigslabs
      integer :: iblock
      integer :: jnode
      integer :: knode
      integer :: ii
      integer :: jj
c
      real    :: anodes
      real    :: aslabs
      real    :: totspeed
      real    :: workdom
      real    :: workaccum
      real    :: worksofar
      real    :: slabspeed
      real    :: workslab
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c            
      anodes = float(nodes)
      aslabs = sqrt(anodes * float(nyp) / float(nxp))
      nslabs = min(nodes,max(1,nint(aslabs)))
      totspeed = 0.
c
      do inode = 1,nodes
         ixe(inode) = 0
         totspeed = totspeed + relspeed(inode)
      enddo
c
c  --- Compute total work load over each row and over entire domain. ---
c
      workdom = 0.
      do j = 1,nyp
         workrow(j) = 0.
         do i = 1,nxp
            workrow(j) = workrow(j) + work(i,j)
         enddo
         workdom = workdom + workrow(j)
      enddo
      workrow(2) = workrow(2) + workrow(1)
      workrow(nyp-1) = workrow(nyp-1) + workrow(nyp)
c
c  --- Determine number of blocks and the average workload for each slab. ---
c
      min_blocks = nodes / nslabs
      nbigslabs = nodes - min_blocks * nslabs
      inode = 0
      do islab = 1,nslabs
         workload(islab) = 0.
         nblocks(islab) = min_blocks
         if (islab .le. nbigslabs) nblocks(islab) = min_blocks + 1
         do iblock = 1,nblocks(islab)
            inode = inode + 1
            workload(islab) = workload(islab) + 
     &                                     workdom * relspeed(inode) / totspeed
         enddo
      enddo
c
c  --- Assign all j-rows to their respective slabs in a way 
c      that balances the work load among slabs according to
c      their respective numbers of nodes (blocks). ---
c
      do islab = 1,nslabs
         jrows(islab) = 0
      enddo
      workaccum = 0.
      worksofar = 0.
      islab = 0
c
      do j = 2,nyp-1,nest_factor
         do jj=0,nest_factor-1
            workaccum = workaccum + workrow(j+jj)
         enddo
         if (workaccum - .5 * workrow(j+jj) .gt. worksofar .and.
     &                                                  islab .lt. nslabs) then
            islab = islab + 1
            jrow(islab) = j
            worksofar = worksofar + workload(islab)
         endif
         jrows(islab) = jrows(islab) + nest_factor
      enddo
c
      inode = 0
      jnode = 0
      knode = 0
      do islab = 1,nslabs
c
c  --- Compute the total work load for each slab
c      and for each i-column in the slab. ---
c
         slabspeed = 0.
         workslab = 0.
         do i = 1,nxp
            workcol(i) = 0.
            do j = jrow(islab),jrow(islab)+jrows(islab)-1
               workcol(i) = workcol(i) + work(i,j)
            enddo
            workslab = workslab + workcol(i)
         enddo
         workcol(2) = workcol(2) + workcol(1)
         workcol(nxp-1) = workcol(nxp-1) + workcol(nxp)
c
c  --- Determine average workload for each block. ---
c
         do iblock = 1,nblocks(islab)
            jnode = jnode + 1
            slabspeed = slabspeed + relspeed(jnode)
         enddo
         do iblock = 1,nblocks(islab)
            knode = knode + 1
            workblock(iblock) = workslab * relspeed(knode) / slabspeed
         enddo
c
c  --- Assign the i-columns of each slab to their respective blocks in
c      a way that balances the work load among the blocks.  The array
c      ncols counts the number of i-columns on each node, and the array
c      ncol is the index of the westernmost i-column on each node. ---
c
         workaccum = 0.
         worksofar = 0.
         iblock = 0
c
         do i = 2,nxp-1,nest_factor
            do ii=0,nest_factor-1
               workaccum = workaccum + workcol(i+ii)
            enddo
            if (workaccum - .5 * workcol(i+ii) .gt. worksofar .and.
     &                                         iblock .lt. nblocks(islab)) then
               iblock = iblock + 1
c
c  --- defining node variables here ---
c
               inode = inode + 1
               iyb(inode) = jrow(islab)
               ixb(inode) = i
               iye(inode) = iyb(inode) + jrows(islab) - 1
               worksofar = worksofar + workblock(iblock)
            endif
            ixe(inode) = ixe(inode) + nest_factor
         enddo
      enddo
c
c  --- defining node variable here ---
c
      do jnode = 1,nodes
         ixe(jnode) = ixb(jnode) + ixe(jnode) - 1
      enddo
c
c  --- Check to make sure that each subdomain has at least 2 interior
c      rows and columns. ---
c
      do jnode = 1,nodes
         if (iye(jnode) - iyb(jnode) .lt. 1 .or. 
     &                                     ixe(jnode) - ixb(jnode) .lt. 1) then
           write(*,*) 'grid:',nxp,nyp,
     &                '  subdomain too small on node ',jnode
           write(*,*) '(ixb,ixe,iyb,iye) = ',
     &                ixb(jnode),ixe(jnode),iyb(jnode),iye(jnode)
            stop 'small_nodes'
         endif
      enddo
c                                                                           
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine par_decomp:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine PAR_est_time:
c-----------------------------------------------------------------------
c
c    Called by:
c       DOMAIN_DECOMP
c
      subroutine PAR_est_time(nxp,nyp,isnest,work,cput,init)
c
      implicit none
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: nxp
      integer :: nyp
      integer :: isnest(nxp,nyp)
c
      real    :: work(nxp,nyp)
      real    :: cput(nxp,nyp)
c
      integer :: init
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i
      integer :: j
c
      real    :: bfact
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- Sample routine to fill work elements with values proportional
c      to the time required to perform model operations. ---
c
      if (init==1) then
         do j = 2,nyp-1
            do i = 2,nxp-1
               work(i,j) = 1.
            enddo
         enddo
      else
         do j = 2,nyp-1
            do i = 2,nxp-1
               work(i,j) = cput(i,j)
            enddo
         enddo
      endif
c
c  --- Fill real boundaries with .2 of interior points ---
c
      bfact=.2
      do j = 1,nyp
         work(1,j) = bfact * work(2,j)
         work(nxp,j) = bfact * work(nxp-1,j)
      enddo
      do i = 1,nxp
         work(i,1) = bfact * work(i,2)
         work(i,nyp) = bfact * work(i,nyp-1)
      enddo
c
c  --- If the cell is in a nest we don't as much work ---
c
      bfact=.2
      do i=2,nxp-1
         do j=2,nyp-1
            if (isnest(i,j) .gt. 0) then
               work(i,j) = bfact * work(i,j)
            endif
         enddo
      enddo
c
      return
      end
c
c-----------------------------------------------------------------------
c    END subroutine PAR_est_time:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    BEGIN subroutine PAR_node_paths:
c-----------------------------------------------------------------------
c
      subroutine PAR_node_paths(mxgrids,ngrids,nnxp,nnyp,
     &                          nxtnest,mxmachs,ibcflags,ipaths)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
c    Called by:
c       DOMAIN_DECOMP
c
      use master_mod
c
      implicit none
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: mxgrids
      integer :: ngrids
      integer :: nnxp(*)
      integer :: nnyp(*)
      integer :: nxtnest(*)
      integer :: mxmachs
      integer :: ibcflags(mxmachs,*)
      integer :: ipaths(5,7,mxgrids,mxmachs,mxmachs)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: is0t(3)
      integer :: is0u(3)
      integer :: is0v(3)
      integer :: js0t(3)
      integer :: js0u(3)
      integer :: js0v(3)
      integer :: ngr
      integer :: isend_type
      integer :: idn
      integer :: isn
      integer :: i
      integer :: j
      integer :: info
      integer :: icm
      integer :: indt
      integer :: indu
      integer :: indv
      integer :: nxp
      integer :: nyp
      integer :: id
      integer :: jd
      integer :: nijst
      integer :: nijsu
      integer :: nijsv
      integer :: iselft
      integer :: iselfu
      integer :: iselfv
      integer :: mijs
      integer :: is
      integer :: js
      integer :: ig
      integer :: ih
      integer :: ibeg
      integer :: jbeg
      integer :: iend
      integer :: jend
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- Zero out ipaths array ---
c
      do ngr=1,ngrids
         do isend_type = 1,6
            do idn=1,nmachines
               do isn = 1,nmachines
                  iget_paths_master(isend_type,ngr,isn,idn) = 0
                  do info = 1,5
                     ipaths(info,isend_type,ngr,idn,isn) = 0
                  enddo
               enddo
            enddo
         enddo
      enddo
      do ngr=1,ngrids
         icm = nxtnest(ngr)
         do idn=1,nmachines
            do isn = 1,nmachines
               if (isn .eq. idn) go to 6
c
c  --- Long timestep overlap regions ---
c               
               if (nxbeg(idn,ngr) .gt. ixe(isn,ngr) .or. 
     &                     nybeg(idn,ngr) .gt. iye(isn,ngr) .or. 
     &                             nxend(idn,ngr) .lt. ixb(isn,ngr) .or. 
     &                                nyend(idn,ngr) .lt. iyb(isn,ngr)) go to 6
               iget_paths_master(1,ngr,isn,idn) = machnum(isn)
               ipaths(1,1,ngr,idn,isn)=max(ixb(isn,ngr),nxbeg(idn,ngr))
               ipaths(2,1,ngr,idn,isn)=min(ixe(isn,ngr),nxend(idn,ngr))
               ipaths(3,1,ngr,idn,isn)=max(iyb(isn,ngr),nybeg(idn,ngr))
               ipaths(4,1,ngr,idn,isn)=min(iye(isn,ngr),nyend(idn,ngr))
               ipaths(5,1,ngr,idn,isn)=machnum(idn)
c
c  --- Expand ipaths to include [coarse] grid external boundary points. ---
c
               if (ipaths(1,1,ngr,idn,isn) .eq. 2)
     &                                      ipaths(1,1,ngr,idn,isn) = 1
               if (ipaths(2,1,ngr,idn,isn) .eq. nnxp(ngr)-1)
     &                                      ipaths(2,1,ngr,idn,isn) = nnxp(ngr)
               if (ipaths(3,1,ngr,idn,isn) .eq. 2)
     &                                      ipaths(3,1,ngr,idn,isn) = 1
               if (ipaths(4,1,ngr,idn,isn) .eq. nnyp(ngr)-1)
     &                                      ipaths(4,1,ngr,idn,isn) = nnyp(ngr)
    6 continue
c
c  --- Coarse grid to fine grid interpolation communication ---
c
               if (icm == 0) goto 16
               if (ipm(ixb(idn,ngr)-1,ngr)-2 .gt. ixe(isn,icm).or.  
     &               jpm(iyb(idn,ngr)-1,ngr)-2 .gt. iye(isn,icm).or.  
     &                  ipm(ixe(idn,ngr)+1,ngr)+1 .lt. ixb(isn,icm).or.  
     &                    jpm(iye(idn,ngr)+1,ngr)+1 .lt. iyb(isn,icm)) go to 16
               iget_paths_master(5,ngr,isn,idn) = machnum(isn)
               ipaths(1,5,ngr,idn,isn) = max(ixb(isn,icm),  
     &                                            ipm(ixb(idn,ngr)-1,ngr)-2)
               ipaths(2,5,ngr,idn,isn) = min(ixe(isn,icm),
     &                                            ipm(ixe(idn,ngr)+1,ngr)+1)
               ipaths(3,5,ngr,idn,isn) = max(iyb(isn,icm),
     &                                            jpm(iyb(idn,ngr)-1,ngr)-2)
               ipaths(4,5,ngr,idn,isn) = min(iye(isn,icm),
     &                                            jpm(iye(idn,ngr)+1,ngr)+1)
               ipaths(5,5,ngr,idn,isn) = machnum(idn)
   16 continue
c
               if (icm .GT. 0) then
                  ibeg = nxbegc(idn,icm)+ixoff(idn,icm)
                  jbeg = nybegc(idn,icm)+iyoff(idn,icm)
                  iend = nxendc(idn,icm)+ixoff(idn,icm)
                  jend = nyendc(idn,icm)+iyoff(idn,icm)
                  if (ibeg .gt. ipm(ixe(isn,ngr)+1,ngr).or.  
     &                  jbeg .gt. jpm(iye(isn,ngr)+1,ngr).or.  
     &                    iend .lt. ipm(ixb(isn,ngr)-1,ngr).or.  
     &                      jend .lt. jpm(iyb(isn,ngr)-1,ngr)) go to 26
                  iget_paths_master(6,ngr,isn,idn) = machnum(isn)
                  ipaths(1,6,ngr,idn,isn) = max(ipm(ixb(isn,ngr),ngr),
     &                                                        nxbeg(idn,icm)+1)
                  ipaths(2,6,ngr,idn,isn) = min(ipm(ixe(isn,ngr),ngr),
     &                                                        nxend(idn,icm)-1)
                  ipaths(3,6,ngr,idn,isn) = max(jpm(iyb(isn,ngr),ngr),
     &                                                        nybeg(idn,icm)+1)
                  ipaths(4,6,ngr,idn,isn) = min(jpm(iye(isn,ngr),ngr),
     &                                                        nyend(idn,icm)-1)
                  ipaths(5,6,ngr,idn,isn) = machnum(idn)
               else
                  iget_paths_master(6,ngr,isn,idn) = machnum(isn)
                  ipaths(1,6,ngr,idn,isn) = ipm(ixb(isn,ngr),ngr) 
                  ipaths(2,6,ngr,idn,isn) = ipm(ixe(isn,ngr),ngr)  
                  ipaths(3,6,ngr,idn,isn) = jpm(iyb(isn,ngr),ngr)  
                  ipaths(4,6,ngr,idn,isn) = jpm(iye(isn,ngr),ngr)  
                  ipaths(5,6,ngr,idn,isn) = machnum(idn)
               endif
c
               do i = ixb(isn,ngr),ixe(isn,ngr)
                  if (ipm(i,ngr) .eq. ipaths(1,6,ngr,idn,isn)) then
                     ipaths(1,7,ngr,idn,isn) = i
                     go to 21
                  endif
               enddo
   21 continue
c
               do i = ixe(isn,ngr),ixb(isn,ngr),-1
                  if (ipm(i,ngr) .eq. ipaths(2,6,ngr,idn,isn)) then
                     ipaths(2,7,ngr,idn,isn) = i
                     go to 22
                  endif
               enddo
   22 continue
c
               do j = iyb(isn,ngr),iye(isn,ngr)
                  if (jpm(j,ngr) .eq. ipaths(3,6,ngr,idn,isn)) then
                     ipaths(3,7,ngr,idn,isn) = j
                     go to 23
                  endif
               enddo
   23 continue
c
               do j = iye(isn,ngr),iyb(isn,ngr),-1
                  if (jpm(j,ngr) .eq. ipaths(4,6,ngr,idn,isn)) then
                     ipaths(4,7,ngr,idn,isn) = j
                     go to 24
                  endif
               enddo
   24 continue
c
   26 continue
            enddo
         enddo
      enddo
c
      return
      end

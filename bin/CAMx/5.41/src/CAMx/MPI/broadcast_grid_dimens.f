      subroutine broadcast_grid_dimens(iproc_id,num_procs)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use master_mod
      use node_mod
      use camx_includes
      use grid
c
c  --- for itag only ---
c
      use filunit
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
c       NODES_ALLOC
c    Subroutines called:
c       NODES_PASS
c       NODES_PASS1
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: iproc_id
      integer :: num_procs
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: nm
      integer :: nm2
      integer :: ngr
      integer :: nints
      integer :: i
      integer :: j
c
      logical :: debug = .false.
c
c  --- local buffers for send. ---
c
      integer :: bufferCol(ngrid)
      integer :: bufferRow(ngrid)
      integer :: bufferLay(ngrid)
      integer :: bufferAll(0:num_procs,ngrid, 10)
      integer :: myBuff(2*num_procs)
c
      integer, dimension(5,7,ngrid,num_procs) :: buffer_ipaths
      integer, dimension(5,7,ngrid,num_procs) :: buffer_bpaths
      integer, dimension(6,  ngrid,num_procs) :: buffer_iget_paths
c
      integer :: iunit = 199
c
      logical :: activate_bpaths = .true.
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- set up var values for master node ---
c
      if (iproc_id .eq. 0) then
         do nm=1, num_procs
            machs(nm) = nm
         enddo
c
         do ngr=1, ngrid
c
c  --- regular boundary ---
c
            nxbeg (0, ngr) = 1 
            nxend (0, ngr) = ncol(ngr)
            nybeg (0, ngr) = 1
            nyend (0, ngr) = nrow(ngr)
c
c  --- calculated boundary ---
c
            nxbegc(0, ngr) = 2
            nxendc(0, ngr) = ncol(ngr) - 1
            nybegc(0, ngr) = 2
            nyendc(0, ngr) = nrow(ngr) - 1
c
c  --- offset to the parent grid ---
c
            ixoff (0, ngr) = 0 
            iyoff (0, ngr) = 0
c
c  --- boundary flag bit ---
c
            ibcflg(0, ngr) = 1+2+4+8
c
            do nm=0, num_procs
               nodemxp(nm, ngr) = nxend(nm,ngr)-nxbeg(nm, ngr)+1
               nodemyp(nm, ngr) = nyend(nm,ngr)-nybeg(nm, ngr)+1
               nodemzp(nm, ngr) = nlay(ngr)      
               bufferAll(nm,ngr,1)  = nodemxp(nm,ngr)
               bufferAll(nm,ngr,2)  = nodemyp(nm,ngr)
               bufferAll(nm,ngr,3)  = nodemzp(nm,ngr)
               bufferAll(nm,ngr,4)  = nxbegc (nm,ngr)
               bufferAll(nm,ngr,5)  = nxendc (nm,ngr)
               bufferAll(nm,ngr,6)  = nybegc (nm,ngr)
               bufferAll(nm,ngr,7)  = nyendc (nm,ngr)
               bufferAll(nm,ngr,8)  = ixoff  (nm,ngr)
               bufferAll(nm,ngr,9)  = iyoff  (nm,ngr)
               bufferAll(nm,ngr,10) = ibcflg (nm,ngr)
            enddo 
            bufferCol(ngr) = ncol(ngr)
            bufferRow(ngr) = nrow(ngr)
            bufferLay(ngr) = nlay(ngr)
         enddo 
      endif
c
      call nodes_pass(machs,num_procs,MPI_INTEGER,itag,num_procs,iproc_id)
      call nodes_pass(ngrid,        1,MPI_INTEGER,itag,num_procs,iproc_id)
      call nodes_pass(bufferCol,ngrid,MPI_INTEGER,itag,num_procs,iproc_id)
      call nodes_pass(bufferRow,ngrid,MPI_INTEGER,itag,num_procs,iproc_id)
      call nodes_pass(bufferLay,ngrid,MPI_INTEGER,itag,num_procs,iproc_id)
      nints =ngrid*(num_procs+1)*10 
      call nodes_pass(bufferAll,nints,MPI_INTEGER,itag,num_procs,iproc_id)
c
      if (iproc_id .ne. 0) then
         do ngr = 1, ngrid
c
c  --- ncol, nrow, nlay were dynamically allocated ---
c
            ncol(ngr) = bufferCol(ngr)
            nrow(ngr) = bufferRow(ngr)
            nlay(ngr) = bufferLay(ngr)
            do nm=0, num_procs
               nodemxp  (nm,ngr) = bufferAll(nm,ngr,1)
               nodemyp  (nm,ngr) = bufferAll(nm,ngr,2)
               nodemzp  (nm,ngr) = bufferAll(nm,ngr,3)
               nodeia   (nm,ngr) = bufferAll(nm,ngr,4)
               nodeiz   (nm,ngr) = bufferAll(nm,ngr,5)
               nodeja   (nm,ngr) = bufferAll(nm,ngr,6)
               nodejz   (nm,ngr) = bufferAll(nm,ngr,7)
               nodei0   (nm,ngr) = bufferAll(nm,ngr,8)
               nodej0   (nm,ngr) = bufferAll(nm,ngr,9)
               nodeibcon(nm,ngr) = bufferAll(nm,ngr,10)
            enddo
         enddo
         if (nmachs .eq. 0) stop 'nmachs problem in broadcast...dim.f'
         nmachs = num_procs
      endif
c
      do ngr=1,ngrid
         mmxp(ngr)=nodemxp(iproc_id,ngr)        
         mmyp(ngr)=nodemyp(iproc_id,ngr)
         mmzp(ngr)=nodemzp(iproc_id,ngr)
         mia(ngr) =nodeia (iproc_id,ngr)
         miz(ngr) =nodeiz (iproc_id,ngr)
         mja(ngr) =nodeja (iproc_id,ngr)
         mjz(ngr) =nodejz (iproc_id,ngr)
         mi0(ngr) =nodei0 (iproc_id,ngr)
         mj0(ngr) =nodej0 (iproc_id,ngr)
         mibcon(ngr)=nodeibcon(iproc_id,ngr)
      enddo
c
      if (activate_bpaths) then
         do nm=1,num_procs
            if (iproc_id .eq. 0) then
               do i=1, 5
                  do j=1, 7
                     do ngr=1, ngrid
                        do nm2=1, num_procs
                           buffer_bpaths(i,j,ngr,nm2) = 
     &                                       inode_paths_master(i,j,ngr,nm,nm2)
                        enddo
                     enddo
                  enddo
               enddo
            endif
            call nodes_pass1(buffer_bpaths,5*7*ngrid*num_procs,
     &                       MPI_INTEGER,itag,iproc_id,nm      )
         enddo
c
         do i=1, 5
            do j=1, 7
               do ngr=1, ngrid
                  do nm2=1, num_procs
                     bpaths(i,j,ngr,nm2) = buffer_bpaths(i,j,ngr,nm2)
                  enddo
               enddo
            enddo
         enddo
c
         do ngr=2, ngrid
            do nm2=1, num_procs
               bpaths(5,5,ngr,nm2) = nm2
c
c  --- target node ---
c
            enddo
         enddo
      endif
c
      do nm=1,num_procs
         if (iproc_id .eq. 0) then
            do i=1, 5
               do j=1, 7
                  do ngr=1, ngrid
                     do nm2=1, num_procs
                        buffer_ipaths(i,j,ngr,nm2) =
     &                                       inode_paths_master(i,j,ngr,nm2,nm)
                     enddo
                  enddo
               enddo
            enddo
c
            do i=1, 6
               do ngr=1, ngrid
                  do nm2=1, num_procs
                     buffer_iget_paths(i,ngr,nm2) =
     &                                          iget_paths_master(i,ngr,nm2,nm)
                  enddo
               enddo
            enddo
         endif
         call nodes_pass1(buffer_ipaths,  5*7*ngrid*num_procs,
     &                    MPI_INTEGER,itag,iproc_id,nm        )
         call nodes_pass1(buffer_iget_paths,6*ngrid*num_procs,
     &                    MPI_INTEGER,itag,iproc_id,nm        )
       enddo  
c
      do i=1, 5
         do j=1, 7
            do ngr=1, ngrid
               do nm2=1, num_procs
                  ipaths(i,j,ngr,nm2) = buffer_ipaths(i,j,ngr,nm2)
               enddo
            enddo
         enddo
      enddo
c
      do i=1, 6
         do ngr=1, ngrid
            do nm2=1, num_procs
               iget_paths(i,ngr,nm2) = buffer_iget_paths(i,ngr,nm2)
            enddo
         enddo
      enddo
c
      do nm=1, num_procs
         if (iproc_id .eq. 0) then
            do i=1, 2   
               do j=1, num_procs
                  myBuff((i-1)*num_procs+j) = lbc_buffs(i,j,nm) 
               enddo 
            enddo
         endif
         call nodes_pass1(myBuff,2*num_procs,MPI_INTEGER,itag,iproc_id,nm)
         do j=1, num_procs
c
c  --- add 90000 for nbc overflow ---
c
            node_buffs(j)%nsend = 10*myBuff(j) + 90000
c
c  --- multiply 10 to meet the memory size needed by DDM, etc.
c      should change 10 to a variable later ---
c
            node_buffs(j)%nrecv = 10*myBuff(num_procs+j) + 90000
         enddo
      enddo
c
      do nm=1, num_procs
         if (iproc_id .eq. 0) then
            newbuff_nest = newbuff_nest1(nm)
            nbuff_nest   = nbuff_nest1(nm)
         endif
         call nodes_pass1(newbuff_nest,1,MPI_INTEGER,itag,iproc_id,nm)
         call nodes_pass1(nbuff_nest,  1,MPI_INTEGER,itag,iproc_id,nm)
      enddo
c
      if (debug .and. iproc_id .gt. 0) then
         do i=1, 5
            do j=1, 7
               do ngr=1, ngrid
                  do nm=1, num_procs
                     write(iunit,*) 'ipaths: ', i, j, 
     &                                           ngr, nm, ipaths(i,j,ngr,nm)
                  enddo
               enddo
            enddo
         enddo
c
         if (activate_bpaths) then
         do i=1, 5
            do j=1, 7
               do ngr=1, ngrid
                  do nm=1, num_procs
                     write(iunit,*) 'bpaths: ', i, j,
     &                                           ngr, nm, bpaths(i,j,ngr,nm)
                  enddo
               enddo
            enddo
         enddo
         endif
c
         do i=1, 6
            do ngr=1, ngrid
               do nm=1, num_procs
                  write(iunit,*) 'iget_paths: ', i,
     &                                         ngr, nm, iget_paths(i,ngr,nm)
               enddo
            enddo
         enddo
c
         do nm=1, num_procs
            write(iunit,*) "node_buffs(nm)%nsend: ", nm, node_buffs(nm)%nsend
            write(iunit,*) "node_buffs(nm)%nrecv: ", nm, node_buffs(nm)%nrecv
         enddo
         write(iunit,*) "newbuff_nest: ", newbuff_nest
         write(iunit,*) "nbuff_nest  : ", nbuff_nest
         close(iunit)
      endif
c
      return
      end

      subroutine domain_decomp()
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use master_mod
      use camx_includes
      use grid_dims
      use grid
      use tracer
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
c    Called by: 
c       NODES_ALLOC
c    Subroutines called:
c       PAR_EST_TIME
c       PAR_DECOMP
c       PAR_DECOMP_BOUNDS
c       PAR_NODE_PATHS
c
c     Copyright 1996 - 2009
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
      include 'filunit.com'
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: ngr
      integer :: idn
      integer :: isn
      integer :: ng
      integer :: mp_nzp
      integer :: numbuff
      integer :: jnode
      integer :: ncols
      integer :: nestvar
      integer :: nc
      integer :: nf
      integer :: nv
      integer :: num_lbc_buff
      integer :: num_nest_buff
      integer :: num_feed_buff
      integer :: itype
      integer :: ii1
      integer :: ii2
      integer :: jj1
      integer :: jj2
      integer :: ixy
      integer :: ixyz
      integer :: memf
      integer :: iinc
      integer :: icnt
      integer :: jinc
      integer :: jcnt
      integer :: icm
      integer :: ifm
      integer :: if1
      integer :: jf
      integer :: mfact
      integer :: i
c
      real,    dimension(maxdim2)       :: vctr1
      real,    dimension(maxdim2)       :: vctr2
      real,    dimension(maxdim2)       :: vctr3
      real,    dimension(maxdim2)       :: vctr4
      real,    dimension(maxdim2)       :: vctr5
      real,    dimension(maxdim2)       :: vctr6
      real,    dimension(maxdim2)       :: vctr7
      real,    dimension(maxdim2)       :: vctr8
      real,    dimension(maxdim2)       :: vctr9
      real,    dimension(nxpmax*nypmax) :: scr2d1
c
      integer, dimension(maxgrds)       :: ninest
      integer, dimension(maxgrds)       :: njnest
      integer, dimension(maxgrds)       :: iparent
c
c  --- Dummy nxtnest fill assuming telescoping grids ---
c
      integer, dimension(maxgrds)       :: nxtnest
      data nxtnest /0,1,2,3,4,5,6,7,8,9/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- Decompose all grids into subdomains ---
c
c  --- set the variable that stores the parent grid index ---
c
      do icm=1,ngrid
         iparent(icm) = 1
      enddo
      do icm=1,ngrid
         do ifm=1,nchdrn(icm)
            iparent(idchdrn(ifm,icm)) = icm
         enddo
      enddo
c
      do ifm=2,ngrid
          ninest(ifm) = i1(ifm)
          njnest(ifm) = j1(ifm)
      enddo
c
c  --- Fill IPM, JPM arrays with parent grid
c      index values for all fine grids. ---
c
      do ifm = 2,ngrid
         ipm(1,ifm) = ninest(ifm)
         iinc = 0
         icnt = 0
         do if1 = 2,ncol(ifm)
            ipm(if1,ifm) = ipm(if1-1,ifm) + iinc
            icnt = icnt + 1
            if (icnt >= nmesh(ifm)) then
               icnt = 0
               iinc = 1
            else
               iinc = 0
            endif
         enddo
         ipm(1,ifm) = ninest(ifm)-1
         jpm(1,ifm) = njnest(ifm)
         jinc = 0
         jcnt = 0
         do jf = 2,nrow(ifm)
            jpm(jf,ifm) = jpm(jf-1,ifm) + jinc
            jcnt = jcnt + 1
            if (jcnt >= nmesh(ifm)) then
               jcnt = 0
               jinc = 1
            else
               jinc = 0
            endif
         enddo
         jpm(1,ifm) = njnest(ifm)-1
      enddo   
c
c  --- Obtain estimates of the fraction of computational time (work)
c      required for each column in the region of the domain. ---
c
      do ngr = 1,ngrid
         do i=1,nxpmax*nypmax
c
c  --- Pass in dummy cputime ---
c
            scr2d1(i)=1.
         enddo
c
c  --- 1 is the previous "init" ---
c
         call PAR_est_time(ncol(ngr),nrow(ngr),idfin(iptr2d(ngr)),
     &                     scr2d1(1),scr2d1(1),1                  )
c
c  --- Decompose the grid taking into account the work numbers. ---
c
         call PAR_decomp(ncol(ngr),nrow(ngr),nmesh(ngr),nmachines,
     &                   scr2d1(1),vctr1(1),vctr2(1),vctr3(1),
     &                   vctr4(1),vctr5(1),vctr6(1),vctr7(1),
     &                   ixb(1,ngr),ixe(1,ngr),iyb(1,ngr),iye(1,ngr))
         write(idiag,*)
     &                 '!------------------------------------------------'
         write(idiag,*)
     &                 '!            Domain decomposition'
         write(idiag,*)
     &                 '!   grid# node# x-beg x-end y-beg y-end #cells'
         write(idiag,*)
     &                 '!------------------------------------------------'
         do jnode = 1,nmachines
            ncols= (1+ixe(jnode,ngr)-ixb(jnode,ngr))*
     &                                        (1+iye(jnode,ngr)-iyb(jnode,ngr))
            write(idiag, '('' ! '',7i6,f12.4)'),ngr,jnode,
     &                              ixb(jnode,ngr),ixe(jnode,ngr),
     &                                      iyb(jnode,ngr),iye(jnode,ngr),ncols
         enddo
         write(idiag,*)
     &                 '!------------------------------------------------'
      enddo
c
c  --- Compute various bounds for the subdomains ---
c
      call PAR_decomp_bounds(ngrid,ncol,nrow,NOVERLP,NOVERLP)
c
c  --- Determine node sending paths and numbers of receiving nodes ---
c
c  --- Dummy ipm,jpm dimensioned above for now ---
c
      call PAR_node_paths(maxgrds,ngrid,ncol,nrow,
     &                    nxtnest,maxmach,ibcflg,
     &                    inode_paths_master      )
c
c  --- Compute send and receive buffer sizes. These will be maximum of
c      long timestep, turbulence, nest boundaries, and nest feedback.
c      Small timestep will use same buffers as they are always smaller. ---
c
      do idn=1,nmachines
         do isn=1,nmachines
            lbc_buffs(1,idn,isn)=0
            lbc_buffs(2,idn,isn)=0
         enddo
      enddo
      mp_nzp=0
      do ng=1,ngrid
         mp_nzp=max(mp_nzp,nlay(ng))
      enddo
c
c  --- Find number of nested variables to be communicated. ---
c
      do ng=1,ngrid
         nestvar=MAX(nspec,ntotsp)
c
c  --- Find number of lbc variables to be communicated. ---
c
         do isn=1,nmachines
            do idn=1,nmachines
               num_lbc_buff=0
               num_nest_buff=0
               num_feed_buff=0
               itype=1
               ii1=inode_paths_master(1,itype,ng,idn,isn)
               ii2=inode_paths_master(2,itype,ng,idn,isn)
               jj1=inode_paths_master(3,itype,ng,idn,isn)
               jj2=inode_paths_master(4,itype,ng,idn,isn)
               if (ii1.ne.0) then
                  ixy=(ii2-ii1+1)*(jj2-jj1+1)
                  ixyz=(ii2-ii1+1)*(jj2-jj1+1)*(mp_nzp)
                  num_lbc_buff=ixyz*MAX(nspec,ntotsp) + 200
               endif
               itype=5
               ii1=inode_paths_master(1,itype,ng,idn,isn)
               ii2=inode_paths_master(2,itype,ng,idn,isn)
               jj1=inode_paths_master(3,itype,ng,idn,isn)
               jj2=inode_paths_master(4,itype,ng,idn,isn)
               if (ii1.ne.0) then
                  ixyz=(ii2-ii1+1)*(jj2-jj1+1)*(mp_nzp)
                  num_nest_buff=ixyz*nestvar+2*(nestvar+100)
               endif
               itype=6
               ii1=inode_paths_master(1,itype,ng,idn,isn)
               ii2=inode_paths_master(2,itype,ng,idn,isn)
               jj1=inode_paths_master(3,itype,ng,idn,isn)
               jj2=inode_paths_master(4,itype,ng,idn,isn)
               if (ii1.ne.0) then
                  ixyz=(ii2-ii1+1)*(jj2-jj1+1)*(mp_nzp)
                  num_feed_buff=ixyz*nestvar+2*(nestvar+100)
               endif
               lbc_buffs(1,idn,isn)= max(lbc_buffs(1,idn,isn),
     &                                num_lbc_buff,num_nest_buff,num_feed_buff)
            enddo
         enddo
c
         do isn=1,nmachines
            do idn=1,nmachines
               num_lbc_buff=0
               num_nest_buff=0
               num_feed_buff=0
               itype=1
               ii1=inode_paths_master(1,itype,ng,isn,idn)
               ii2=inode_paths_master(2,itype,ng,isn,idn)
               jj1=inode_paths_master(3,itype,ng,isn,idn)
               jj2=inode_paths_master(4,itype,ng,isn,idn)
               if (ii1.ne.0) then
                  ixy=(ii2-ii1+1)*(jj2-jj1+1)
                  ixyz=(ii2-ii1+1)*(jj2-jj1+1)*(mp_nzp)
                  num_lbc_buff=ixyz*MAX(nspec,ntotsp) + 200
               endif
               itype=5
               ii1=inode_paths_master(1,itype,ng,isn,idn)
               ii2=inode_paths_master(2,itype,ng,isn,idn)
               jj1=inode_paths_master(3,itype,ng,isn,idn)
               jj2=inode_paths_master(4,itype,ng,isn,idn)
               if (ii1.ne.0) then
                  ixyz=(ii2-ii1+1)*(jj2-jj1+1)*(mp_nzp)
                  num_nest_buff=ixyz*nestvar+2*(nestvar+100)
               endif
               itype=6
               ii1=inode_paths_master(1,itype,ng,isn,idn)
               ii2=inode_paths_master(2,itype,ng,isn,idn)
               jj1=inode_paths_master(3,itype,ng,isn,idn)
               jj2=inode_paths_master(4,itype,ng,isn,idn)
               if (ii1.ne.0) then
                  ixyz=(ii2-ii1+1)*(jj2-jj1+1)*(mp_nzp)
                  num_feed_buff=ixyz*nestvar+2*(nestvar+100)
               endif
               lbc_buffs(2,idn,isn)= max(lbc_buffs(2,idn,isn),
     &                                num_lbc_buff,num_nest_buff,num_feed_buff)
            enddo
         enddo
      enddo
c
c  --- Check nest boundary receive buffer size ---
c
      itype=5
      do idn=1,nmachines
         newbuff_nest1(idn)=1
         nbuff_nest1(idn)=0
         do ng=1,ngrid
            numbuff=0
            do isn=1,nmachines
               ii1=inode_paths_master(1,itype,ng,idn,isn)
               ii2=inode_paths_master(2,itype,ng,idn,isn)
               jj1=inode_paths_master(3,itype,ng,idn,isn)
               jj2=inode_paths_master(4,itype,ng,idn,isn)
               memf=(ii2-ii1+1)*(jj2-jj1+1)*(mp_nzp)*nestvar
               numbuff=numbuff+memf
            enddo
            nbuff_nest1(idn)=max(nbuff_nest1(idn),numbuff)
         enddo
      enddo
c
      return
      end

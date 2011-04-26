      subroutine bndry_updt(numcols,numrows,numlays,bndtim,bnddate,
     &                      nsteps,numprocs,iproc_id               )
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use filunit
      use camxcom
      use camxfld
      use chmstry
      use grid
      use bndary
      use tracer
c
      implicit none
c
c----CAMx v5.30 101223
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
c       CAMX
c    Subroutines called:
c       READBND
c       FLUSH
c       NODE_RECV_1SPECIES_DATA
c       MASTER_SEND_1SPECIES_DATA
c       RDBCRT
c       CLRBDYSA
c       FILBDYSA
c       CLRBDYDDM
c       ZEROS
c       RDBCDDM
c       NODES_PASS
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     11/4/09 -cemery- Removed input top concentrations
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
      include 'mpif.h'
c
c========================= Probing Tool End ============================
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: numcols(*)
      integer :: numrows(*)
      integer :: numlays(*)
c
      real    :: bndtim
c
      integer :: bnddate
      integer :: nsteps
      integer :: numprocs
      integer :: iproc_id
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: inpdate
      integer :: emsdate
      integer :: hazdate
      integer :: ozndate
      integer :: wrtdate
      integer :: enddate 
      integer :: snodate
      integer :: mvsa3d
      integer :: ncola
      integer :: nrowa
      integer :: nlaya
      integer :: nmx1d
      integer :: ilay
      integer :: ispc
      integer :: i
c
      real    :: inptim
      real    :: emstim
      real    :: haztim
      real    :: ozntim
      real    :: wrttim
      real    :: endtim 
      real    :: snotim
      real    :: tcpu
      real    :: dtime
c
      character*20 :: version
      character*10 :: name
      character*8  :: chtime
      character*8  :: chdate
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- Check if master grid boundary data are to be read ---
c
      if (date .EQ. bnddate .AND. ABS(time-bndtim) .LT. 0.01) then
         if (iproc_id .EQ. 0) then
            write(*,'(a20,$)') 'readbnd ......'
            call readbnd(bndtim,bnddate)
            tcpu = dtime(tarray2)
            write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
            call flush(6)
         endif
c
c  --- pass the concentration fields that have been update ---
c
         do ilay=1,nlay(1)
            do ispc = 1,nspec
               if (iproc_id .gt. 0) then
                  call node_recv_1species_data(conc(iptr4d(1)),1,
     &                                         nlay(1),ilay,nspec,ispc,itag)
               else
                  call master_send_1species_data(conc(iptr4d(1)),1,
     &                                           nlay(1),ilay,nspec,ispc,itag)
               endif
               itag = itag+1
            enddo
         enddo
c
c======================== Source Apportion Begin =======================
c
c  --- call routine to clear the old boundary cells and then
c      call routine to fill with new boundary concentrations ---
c
c  --- For the Master Grid ---
c
         if (iproc_id .eq. 0) then
            if (ltrace) then
               if( tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC ) then
                  call rdbcrt(nsteps,numcols(1),numrows(1),numlays(1),
     &                        ntotsp,ptconc(1),tempk(1),press(1)      )
               else
                  call clrbdysa(1,numcols(1),numrows(1),
     &                          numlays(1),ntotsp,ptconc(1))
                  call filbdysa(1,numcols(1),numrows(1),numlays(1),
     &                          nspec,ntotsp,conc(1),ptconc(1)     )
               endif
c
c  --- pass the boundary arrays ---
c
               mvsa3d = 0
               ncola = 0
               nrowa = 0
               do i=1,ngrid
                  ncola = MAX(ncola,numcols(i))
                  nrowa = MAX(nrowa,numrows(i))
                  mvsa3d = mvsa3d + numrows(i) * numcols(i) * numlays(i)
               enddo
               nmx1d  = MAX( nrowa, ncola )
               do i=1,mvsa3d*ntotsp
                  ptconc(i) = AMAX1(ptconc(i),BNDLPT)
               enddo
            endif
c
c========================= Source Apportion End ========================
c
c
c============================= DDM Begin ===============================
c
c  --- call routine to clear the old boundary cells and then
c      call routine to fill with new boundary concentrations ---
c
c  --- For the Master Grid ---
c
            if((lddm.OR.lhddm) .AND. nbcddm .GT. 0) then
               call clrbdyddm(numcols(1),numrows(1),
     &                        numlays(1),ntotsp,ptconc(1))
               if (nbcddm .GT. 0) then
                  call rdbcddm(numcols(1),numrows(1),numlays(1),
     &                         ntotsp,ptconc(1),tempk(1),press(1))
               endif
            endif
c
         endif
c
c  --- pass the concentration fields that have been updated ---
c
         if( lmpi .AND. (ltrace .OR. lddm .OR. lhddm) ) then
            do ilay=1,nlay(1)
               do ispc = 1,ntotsp
                  if (iproc_id .gt. 0) then
                     call node_recv_1species_data(ptconc(ipsa3d(1)),1,nlay(1),
     &                                            ilay,ntotsp,ispc,itag       )
                  else
                     call master_send_1species_data(ptconc(ipsa3d(1)),1,
     &                                              nlay(1),ilay,ntotsp,
     &                                              ispc,itag           )
                  endif
                  itag = itag+1
               enddo
            enddo
         endif
c
      endif
c
      end

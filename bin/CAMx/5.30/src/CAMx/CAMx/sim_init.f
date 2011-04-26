      subroutine sim_init(version,inptim,inpdate,emstim,emsdate,haztim,
     &                    hazdate,ozntim,ozndate,snotim,snodate,
     &                    bndtim,bnddate,wrttim,wrtdate,endtim,enddate,
     &                    numprocs,iproc_id                            )
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use node_mod
      use filunit
      use grid
      use chmstry
      use ahomap
      use bndary
      use camxfld
      use camxcom
      use ptemiss
      use pigsty
      use procan
      use rtracchm
      use rtcmcchm
      use tracer
      use node_mod
c
      implicit none
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    This routine calls all of the routines needed to initialize the
c    model at the begining of the simulation. The startup routine is
c    called as well as number of routines to allocate and pass data
c    for MPI.
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       CAMX
c    Subroutines called:
c       STARTUP
c       NODES_ALLOC
c       ZEROES
c       NODES_PASS
c       MPI_BARRIER
c       NODES_INIT
c       MASTER_SEND_GRIDDED_DATA
c       NODE_RECV_GRIDDED_DATA
c       CAMXERR
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c       03/15/09     Added code for deposition output for tracers
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      character*20  :: version
c
      real    :: inptim
      real    :: emstim
      real    :: haztim
      real    :: ozntim
      real    :: snotim
      real    :: bndtim
      real    :: wrttim
      real    :: endtim
c
      integer :: inpdate
      integer :: emsdate
      integer :: hazdate
      integer :: ozndate
      integer :: snodate
      integer :: bnddate
      integer :: wrtdate
      integer :: enddate
      integer :: numprocs
      integer :: iproc_id
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: ilast
      integer :: istrt
      integer :: ism
      integer :: igrd
      integer :: nodes
      integer :: i
      integer :: ierr
      integer :: mvec2d
      integer :: iii
      integer :: jjj
      integer :: nlayav
      integer :: istrln
c
      character*200 :: filtmp
      character*10  :: name
      character*8   :: chtime
      character*8   :: chdate
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (iproc_id .EQ. 0) then
         call startup(version,inptim,inpdate,emstim,emsdate,haztim,
     &                hazdate,ozntim,ozndate,snotim,snodate,bndtim,bnddate,
     &                wrttim,wrtdate,endtim,enddate,numprocs               )
c
c  --- Set some MPI variables ---
c
         do igrd=1,ngrid
            mmxp(igrd) = ncol(igrd)
            mmyp(igrd) = nrow(igrd)
            mmzp(igrd) = nlay(igrd)
         enddo
      endif
c
c  --- allocate the date for the nodes ---
c
      if (lmpi) then
         itag = 1
         numprocs = numprocs - 1
         call nodes_alloc(numprocs,iproc_id)
      endif
c
c  --- Set average concentrations to zero ---
c
      if (iproc_id .EQ. 0) then
         do igrd = 1,ngrid
            nlayav = nlay(igrd)
            if( .NOT. l3davg(igrd) ) nlayav = 1
            nodes=ncol(igrd)*nrow(igrd)*nlayav*navspc
            call zeros(avcnc(iptrav(igrd)),nodes)
            nodes = ncol(igrd)*nrow(igrd)*3*ndepspc
            call zeros(depfld(iptrdp(igrd)),nodes)
            if( lptdepout ) then
               nodes = ncol(igrd)*nrow(igrd)*notimespc
               call zeros(ptdryfld(ipsadep(igrd)),nodes)
               call zeros(ptwetfld(ipsadep(igrd)),nodes)
            endif
         enddo
         if (lsample) then
            do ism = 1,nsample
               nodes = ncolsmp(ism)*nrowsmp(ism)*navspc
               call zeros(smpcnc(ipsmp(ism)),nodes)
            enddo
         endif
c
c======================== Source Apportion Begin =======================
c
c  --- call routine to initialize the running averages ---
c
         if (ltrace .OR. lddm .OR. lhddm ) then
            do igrd=1,ngrid
               nodes=ncol(igrd)*nrow(igrd)*ntotsp
               call zeros(ptavrg(ipsa2d(igrd)),nodes)
            enddo
c
            if (tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) then
               nodes = MXRTCEL * nspec
               call zeros(rcpdcy,nodes)
            endif
c
            if ((tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) .AND.
     &                                       lsample .AND. lsmptrc) then
               do igrd = 1,nsample
                  nodes = ncolsmp(igrd)*nrowsmp(igrd)*nrtrac
                  call zeros(rtsmpcnc(iprtsmp(igrd)),nodes)
               enddo
            endif
         endif
c
c======================== Source Apportion End =========================
c
      endif
c
      if (lmpi) then
c
c  --- open a diag file and an out file for  
c
         call nodes_pass(iout,     1,  MPI_INTEGER,itag,numprocs,iproc_id)
         call nodes_pass(idiag,    1,  MPI_INTEGER,itag,numprocs,iproc_id)
         call nodes_pass(filroot,200,MPI_CHARACTER,itag,numprocs,iproc_id)
         if (iproc_id .GT. 0) then
            ilast = istrln(filroot)
            istrt = ilast
c
  111 continue
            if (istrt .EQ. 1 .OR. filroot(istrt:istrt) .EQ. '/') then
               if (istrt .EQ. 1) istrt = 0
               write(filtmp,'(2A,I2.2,A)') 
     &                           filroot(istrt+1:ilast),'.node',iproc_id,'.out'
               open(unit=iout,file=filtmp,status='UNKNOWN',ERR=7000)
               write(filtmp,'(2A,I2.2,A)') 
     &                          filroot(istrt+1:ilast),'.node',iproc_id,'.diag'
               open(unit=idiag,file=filtmp,status='UNKNOWN',ERR=7001)
            else
               istrt = istrt - 1
               if (istrt .GT. 0) goto 111
            endif
         endif
c
c  --- call routine to allocate all of the
c      data structures for the nodes ---
c
         call MPI_Barrier(MPI_COMM_WORLD, ierr)
         call nodes_init(numprocs,iproc_id)
c
         call nodes_pass(icur_unit,    1,MPI_INTEGER,itag,numprocs,iproc_id)
         call nodes_pass(inptim,       1,   MPI_REAL,itag,numprocs,iproc_id)
         call nodes_pass(inpdate,      1,MPI_INTEGER,itag,numprocs,iproc_id)
         call nodes_pass(emstim,       1,   MPI_REAL,itag,numprocs,iproc_id)
         call nodes_pass(emsdate,      1,MPI_INTEGER,itag,numprocs,iproc_id)
         call nodes_pass(haztim,       1,   MPI_REAL,itag,numprocs,iproc_id)
         call nodes_pass(hazdate,      1,MPI_INTEGER,itag,numprocs,iproc_id)
         call nodes_pass(ozntim,       1,   MPI_REAL,itag,numprocs,iproc_id)
         call nodes_pass(ozndate,      1,MPI_INTEGER,itag,numprocs,iproc_id)
         call nodes_pass(snotim,       1,   MPI_REAL,itag,numprocs,iproc_id)
         call nodes_pass(snodate,      1,MPI_INTEGER,itag,numprocs,iproc_id)
         call nodes_pass(bndtim,       1,   MPI_REAL,itag,numprocs,iproc_id)
         call nodes_pass(bnddate,      1,MPI_INTEGER,itag,numprocs,iproc_id)
         call nodes_pass(wrttim,       1,   MPI_REAL,itag,numprocs,iproc_id)
         call nodes_pass(wrtdate,      1,MPI_INTEGER,itag,numprocs,iproc_id)
         call nodes_pass(endtim,       1,   MPI_REAL,itag,numprocs,iproc_id)
         call nodes_pass(enddate,      1,MPI_INTEGER,itag,numprocs,iproc_id)
         call nodes_pass(time_aero,ngrid,   MPI_REAL,itag,numprocs,iproc_id)
         call nodes_pass(aero_dt,  ngrid,   MPI_REAL,itag,numprocs,iproc_id)
         call nodes_pass(date_aero,ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
         call nodes_pass(dt_aero,      1,MPI_INTEGER,itag,numprocs,iproc_id)
c
         do igrd=1,ngrid
            if (iproc_id .eq. 0) then    
               call master_send_gridded_data(icdalb(iptr2d(igrd)),igrd,1,1,itag)  
            else
               call node_recv_gridded_data(icdalb(iptr2d(igrd)),igrd,1,1,itag)
            endif
            itag = itag+1
            if (iproc_id .eq. 0) then  
               call master_send_gridded_data(icdocn(iptr2d(igrd)),igrd,1,1,itag)
            else
               call node_recv_gridded_data(icdocn(iptr2d(igrd)),igrd,1,1,itag)
            endif
            itag = itag+1
            if (iproc_id .eq. 0) then
               call master_send_gridded_data(icddrt(iptr2d(igrd)),igrd,1,1,itag)
            else
               call node_recv_gridded_data(icddrt(iptr2d(igrd)),igrd,1,1,itag)
            endif
            itag = itag+1
            if (iproc_id .eq. 0) then
               call master_send_gridded_data(icdruf(iptr2d(igrd)),igrd,1,1,itag)
            else
               call node_recv_gridded_data(icdruf(iptr2d(igrd)),igrd,1,1,itag)
            endif
            itag = itag+1
         enddo
c
c  --- not doing MPI, set some variables ---
c
      endif
c
      return
c
c  --- Error messages ---
c
 7000 continue
      write(*,'(//,A)') 'ERROR in SIM_INIT:'
      write(*,'(A)') 'Compute node cannot open its .out file.'
      write(*,'(A)') 'Compute node: ',iproc_id
      write(*,'(2A,//)') ' Could not open file: ', filtmp(:istrln(filtmp))
      call camxerr()
c
 7001 continue
      write(*,'(//,A)') 'ERROR in SIM_INIT:'
      write(*,'(A)') 'Compute node cannot open its .diag file.'
      write(*,'(A)') 'Compute node: ',iproc_id
      write(*,'(2A,//)') ' Could not open file: ', filtmp(:istrln(filtmp))
      call camxerr()
c
      end

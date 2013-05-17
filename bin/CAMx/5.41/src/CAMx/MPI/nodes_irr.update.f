      subroutine nodes_irr_update(numprocs,iproc_id,numcells,numreact)
      use procan
      use filunit
      implicit none
c
c
c----CAMx v5.41 121109
c
c     This routine will pass the cirr data for Process Analysis
c     from the compute nodes, where it is calculated to the master
c     where it will be written to the output file. The data can be
c     summed over all compute nodes because it will be zero in a PA
c     cell outside of the computational slice. This routine will be
c     called at each output time.
c     
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c
c     Modifications:
c
c     Input arguments:
c        numprocs            the number of processes
c        iproc_id            the process ID for this processor
c        numcells            then number of total Process Aanalysis cells
c        numreact            then number of total Process Aanalysis reactions
c
c     Output arguments:
c        none
c
c     Routines called:
c     
c     Called by:
c
      include 'camx.prm'
      include 'flags.inc'
      include 'mpif.h'
c     
c-----Argmument declarations
c
      integer numprocs
      integer iproc_id
      integer numcells
      integer numreact
c     
c-----Local variables
c
      integer icl, irxn
      integer nbytes, ierr, status(MPI_STATUS_SIZE), i
      real    tmp_cirr(numcells,numreact)
c     
c-----Entry point
c
      if( .NOT. lmpi ) return
c
c  --- calculate the  number of values ---
c
      nbytes = numcells * numreact
c
c  --- process ID zero recieves data into temp array ---
c
      if( iproc_id .EQ. 0 ) then
         do icl=1,numcells
           do irxn=1,numreact
             cirr(icl,irxn) = 0.
           enddo
         enddo
         do i=1,numprocs 
            call MPI_RECV( tmp_cirr, nbytes, MPI_REAL, i, itag,
     &                                 MPI_COMM_WORLD, status, ierr)
            do icl=1,numcells
               do irxn=1,numreact
                   cirr(icl,irxn) = cirr(icl,irxn) +
     &                                       tmp_cirr(icl,irxn)
               enddo 
            enddo 
         enddo
c
c  --- if this is a compute process, send the global array ---
c
      else
         call MPI_SEND( cirr, nbytes, MPI_REAL, 0, itag,
     &                                      MPI_COMM_WORLD, ierr)
      endif
c
c  --- return to the calling routine ---
c
      itag = itag + 1
      end

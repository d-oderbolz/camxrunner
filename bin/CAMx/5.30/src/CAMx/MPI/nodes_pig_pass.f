      subroutine nodes_pig_pass(igrd,numprocs,iproc_id)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use grid
      use chmstry
      use filunit
      use ahomap
      use bndary
      use camxfld
      use camxcom
      use pigsty
      use ptemiss
      use procan
      use rtracchm
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
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       CAMX
c       NESTING
c    Subroutines called:
c       NODES_SEND_LSLICE_BACK
c       MPI_BARRIER
c       NODES_SEND_PIG_MISC_REAL8
c       NODES_SEND_PIG_MISC
c       NODES_PASS
c       NODES_SEND_PIG_BACK
c       NODES_SEND_PUFFMASS_BACK
c
c     Copyright 1996 - 2010
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
      include 'camx.prm'
      include 'flags.inc'
      include 'camx_aero.inc'
      include 'soap.inc'
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer :: igrd
      integer :: numprocs
      integer :: iproc_id
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: ierr
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- pass the array that determines which slice the PiG is in ---
c
      call nodes_send_Lslice_back(Lslice,ngrid,npig,
     &                            MPI_REAL,itag,igrd,numprocs,iproc_id)
      call MPI_Barrier(MPI_COMM_WORLD,ierr)
c
c  --- pass the mass flux array ---
c
      call master_pass_xmass(pigmass,nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                               itag,numprocs,iproc_id                   )
      call master_pass_xmass(pgmserr,nspec*ngrid,MPI_DOUBLE_PRECISION,
     &                               itag,numprocs,iproc_id                   )
c
c  --- pass the variables in the pigsty include file ---
c
      call nodes_send_pig_misc(npig,   1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(npigon,ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(nage,  ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(pigage,ngrid,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_send_pig_misc(ipigint,1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_send_pig_misc(nkill,  4,MPI_INTEGER,itag,numprocs,iproc_id)
c
      if (npig .eq. 0) goto 999
c
      call nodes_send_pig_back(ingrd,  npig,MPI_INTEGER,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(idpig,  npig,MPI_INTEGER,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(xpigf,  npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(xpigb,  npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(ypigf,  npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(ypigb,  npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(zpig,   npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(axisy,  npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(axisz,  npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(sigy,   npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(sigx,   npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(sigz,   npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(htfms,  npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(htfmb,  npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(vtfms,  npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(vtfmb,  npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(agepigf,npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(agepigb,npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(fmspig, npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(lnewt,  npig,MPI_LOGICAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(lnewg,  npig,MPI_LOGICAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(ipufmap,npig,MPI_INTEGER,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(ipufgrp,npig,MPI_INTEGER,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(pufftop,npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
      call nodes_send_pig_back(puffbot,npig,   MPI_REAL,itag,numprocs,
     &                                          iproc_id,lslice,npig,ngrid,igrd)
c
      call nodes_send_puffmass_back(puffmass,nspec,nreactr,npig,MPI_REAL,itag,
     &                              numprocs,iproc_id,lslice,npig,ngrid,igrd  )
      call nodes_send_puffmass_back(puffrad,nrad,nreactr,npig,MPI_REAL,itag,
     &                              numprocs,iproc_id,lslice,npig,ngrid,igrd  )
c
      if( ltrace .AND. (tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) ) then
         call nodes_send_puffmass_back(puffrt,ntotsp,MXRECTR,npig,MPI_REAL,itag,
     &                                 numprocs,iproc_id,lslice,npig,ngrid,igrd  )
      endif
c
 999  continue
      return
      end 

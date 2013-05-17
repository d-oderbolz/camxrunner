      subroutine nodes_met_pig(numprocs,iproc_id)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use grid
      use chmstry
      use filunit
      use o3colmap
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
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c        This routine passes all of the PiG data that is time-step
c        dependent to the compute nodes when in MPI mode.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       EMISTRNS
c    Subroutines called:
c       NODES_PASS
c
c     Copyright 1996 - 2013
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
      integer :: numprocs
      integer :: iproc_id 
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: nslice
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- pass the mass flux array
c
      call nodes_pass(pigmass,nspec*ngrid,
     &                           MPI_DOUBLE_PRECISION,itag,numprocs,iproc_id)
      call nodes_pass(pgmserr,nspec*ngrid,
     &                           MPI_DOUBLE_PRECISION,itag,numprocs,iproc_id)
      call nodes_pass(nreactr,          1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(loverlap,         1,MPI_LOGICAL,itag,numprocs,iproc_id)
c
c  --- pass the variables in the pigsty include file ---
c
      call nodes_pass(npig,      1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(npigon,ngrid,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(ipigint,   1,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(ingrd,  npig,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(idpig,  npig,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(xpigf,  npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(xpigb,  npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(ypigf,  npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(ypigb,  npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(zpig,   npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(axisy,  npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(axisz,  npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(sigy,   npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(sigx,   npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(sigz,   npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(htfms,  npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(htfmb,  npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(vtfms,  npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(vtfmb,  npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(agepigf,npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(agepigb,npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(fmspig, npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(lnewt,  npig,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(lnewg,  npig,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(ipufmap,npig,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(ipufgrp,npig,MPI_INTEGER,itag,numprocs,iproc_id)
      call nodes_pass(pufftop,npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(puffbot,npig,   MPI_REAL,itag,numprocs,iproc_id)
      call nodes_pass(lsample,   1,MPI_LOGICAL,itag,numprocs,iproc_id)
      call nodes_pass(lsmptrc,   1,MPI_LOGICAL,itag,numprocs,iproc_id)
      nslice =  ngrid*(numprocs-1)*MXPIG
      call nodes_pass(Lslice,nslice,MPI_LOGICAL,itag,numprocs,iproc_id)
c
      call nodes_pass(puffmass,nspec*nreactr*npig,
     &                MPI_REAL,itag,numprocs,iproc_id)
c
      if( ltrace .AND. (tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) ) then
         call nodes_pass(puffrt,ntotsp*MXRECTR*npig,
     &                   MPI_REAL,itag,numprocs,iproc_id)
      endif
c
      return
      end

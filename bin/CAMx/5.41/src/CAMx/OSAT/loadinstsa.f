c*** LOADINSTSA
c
      subroutine loadinstsa(nox,noy,noz,nspsa,kcell,ispc,saconc,cnctmp)
      implicit none
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c   Description:
c     This routine loads the tracer concentrations from the restart 
c     file, stored in the argument cnctmp into the global array saconc.
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c    Argument description:
c     Outputs:
c        saconc  R   intitial concentrations
c     Inputs: 
c        nox     I   number of X cells in grid
c        noy     I   number of Y cells in grid
c        noz     I   number of layers in grid
c        nspsa   I   number of species in conc array
c        kcell   I   current layer to load
c        ispc    I   current species to load
c        cnctmp  R   concentration field read from the restart file
c
c-----------------------------------------------------------------------
c   LOG:
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer   nox
      integer   noy
      integer   noz
      integer   nspsa
      integer   kcell
      integer   ispc
      real      saconc(nox,noy,noz,nspsa)
      real      cnctmp(MXCELLS,MXCELLS)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer i, j
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      do j=1,noy
        do i=1,nox
           saconc(i,j,kcell,ispc) = cnctmp(i,j)
        enddo
      enddo
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
c
      return
      end

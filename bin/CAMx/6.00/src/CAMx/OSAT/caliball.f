c**** CALIBALL
c
      subroutine caliball(nox,noy,noz,nspc,nspsa,concmod,saconc)
      implicit none
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine drives the recalibration routine that ensures that
c   the sum of the tracer species is exactly the same as the regular
c   model species.
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c      Argument description:
c       Outputs:
c           saconc      R  tracer concentrations
c       Inputs:
c           nox         I  number of X cells in the grid
c           noy         I  number of Y cells in the grid
c           noz         I  number of vertical layers
c           nspc        I  number of species in the grid
c           nspsa       I  number of tracer species
c           concmod     R  regular model concentrations
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     06/06/96   --gwilson--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Arguement declarations:
c-----------------------------------------------------------------------
c
      integer   nox
      integer   noy
      integer   noz
      integer   nspc
      integer   nspsa
      real      concmod(nox,noy,noz,nspc)
      real      saconc(nox,noy,noz,nspsa)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer icl, jcl, kzcl, ispc
      real    con(MXSPEC)
c
c----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- loop over cells ---
c
      do kzcl=1,noz
        do jcl=2,noy-1
           do icl=2,nox-1
c
c   --- loop over model species, putting values in local array ---
c
              do ispc=1,nspc
                 con(ispc) = concmod(icl,jcl,kzcl,ispc)
              enddo
c
c   --- call routine to recalibrate the tracers ---
c
             call recalib(nox,noy,noz,nspsa,saconc,icl,jcl,kzcl,con)
c
           enddo
        enddo
      enddo
c
c  --- return to the calling routine ---
c
      goto 9999
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
      return
      end

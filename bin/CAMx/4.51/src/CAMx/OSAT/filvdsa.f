c**** FILVDSA.F
c
      subroutine filvdsa(nox,noy,noz,nspcs,nspsa,
     &                                     conc,vdep,vdepsa,vdeprt)
c
c----CAMx v4.51 080522
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine fills the depostion velocity from the regular model
c   depostion velocities.  The tracer vdeps are a concentration
c   weighted average of the regular model vdeps.
c   
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c      Argument description:
c       Outputs:
c           vdepsa   R  depostion velocity for tracer species
c       Inputs:
c           nox      I  number of X cells in the grid
c           noy      I  number of Y cells in the grid
c           noz      I  number of layers in the grid
c           nspcs    I  number of species in the grid
c           nspsa    I  number of tracer species
c           conc     R  regular model concentrations
c           vdep     R  diffusion velocity for regular model species
c           vdeprt   R  diffusion velocities for RTRAC
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     06/06/96   --gwilson--    Original development
c     10/11/97   --gwilson--    Removed unused argument SACONC
c     01/30/02   --gwilson--    Added code for RTRAC probing tool
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'chmstry.com'
      include 'tracer.com'
c
c-----------------------------------------------------------------------
c    Arguement declarations:
c-----------------------------------------------------------------------
c
      integer   nox
      integer   noy
      integer   noz
      integer   nspcs
      integer   nspsa
      real      vdep(nox,noy,nspcs)
      real      conc(nox,noy,noz,nspcs)
      real      vdepsa(nox,noy,nspsa)
      real      vdeprt(nox,noy,MXTRSP)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer icl, jcl, ispc
      real    consum(MXCOLA,MXROWA,MXTRCLS)
      real    vdsum(MXCOLA,MXROWA,MXTRCLS), val
c
      common /comfilvdsa/ consum, vdsum
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- call routine to zero out the arrays ---
c
      call zeros(vdepsa,nox*noy*nspsa)
      call zeros(consum,MXCOLA*MXROWA*MXTRCLS)
      call zeros(vdsum,MXCOLA*MXROWA*MXTRCLS)
c
c   --- if doing RTRAC, load from the gridded arrays 
c       filled by DRYDEPRT ---
c
      if( tectyp .EQ. RTRAC ) then
          do ispc=1,nspsa
             do icl=1,nox
                do jcl=1,noy
                  vdepsa(icl,jcl,ispc) = vdeprt(icl,jcl,ispc)
                enddo
             enddo
          enddo
          goto 9999
      endif
c
c  ---- read concentrations for each species ----
c
      do 10 ispc=1,nspcs
c
c   --- if the species is a not a tracer species, skip it ---
c
          if( .NOT. lusespc(ispc) ) goto 10
c
c   --- loop over cells ---
c
          do icl=1,nox
              do jcl=1,noy
c
c   --- loop over all classes ----
c
                 do icls=1,ntrcls
c
c   --- add up the wieghted average deposition velocity ---
c
                    consum(icl,jcl,icls) = consum(icl,jcl,icls) + 
     &                         conc(icl,jcl,1,ispc) * fluxmap(ispc,icls)
                    vdsum(icl,jcl,icls) = vdsum(icl,jcl,icls) + 
     &                     conc(icl,jcl,1,ispc) * fluxmap(ispc,icls) *
     &                                                vdep(icl,jcl,ispc)
                 enddo
c
c  --- next cell ---
c
            enddo
          enddo
c
c  --- next species --
c
   10 continue
c
c  --- loop over cells and calculate the tracer vdeps ---
c
      do icl=1,nox
         do jcl=1,noy
c
c   --- loop over all classes ----
c
            do icls=1,ntrcls
c
c   --- loop over all classes ----
c
              val = 0.
              if( consum(icl,jcl,icls) .NE. 0. ) val =
     &                     vdsum(icl,jcl,icls) /  consum(icl,jcl,icls)
               if( npttrc(icls) .GT. 0 ) then
                   do ispc=ipttrc(icls),npttrc(icls)
                      vdepsa(icl,jcl,ispc) = val
                   enddo
               endif
            enddo
c
c  ---- next cell ---
c
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

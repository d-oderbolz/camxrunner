c**** FILVDSA.F
c
      subroutine filvdsa(nox,noy,noz,nspcs,nspsa,nox_rt,noy_rt,nsprt,
     &                                     conc,vdep,vdepsa,vdeprt)
      use chmstry
      use tracer
      use rtcmcchm
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine fills the depostion velocity from the regular model
c   depostion velocities.  The tracer vdeps are a concentration
c   weighted average of the regular model vdeps.
c   
c     Copyright 1996 - 2012
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
c           nox_rt   I  number of X cells in the grid (used for RTRAC array)
c           noy_rt   I  number of Y cells in the grid (used for RTRAC array)
c           nsprt    I  number of RTRAC species
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
c     07/11/07   --gwilson--    Added code for RTCMC probing tool
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
      integer   nspcs
      integer   nspsa
      integer   nox_rt
      integer   noy_rt
      integer   nsprt
      real      vdep(nox,noy,nspcs)
      real      conc(nox,noy,noz,nspcs)
      real      vdepsa(nox,noy,nspsa)
      real      vdeprt(nox_rt,noy_rt,nsprt)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer icl, jcl, ispc
      real    val
c
      real, allocatable, dimension(:,:,:) :: consum
      real, allocatable, dimension(:,:,:) :: vdsum
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- call routine to zero out the arrays ---
c
      call zeros(vdepsa,nox*noy*nspsa)
c
c   --- if doing RTRAC, load from the gridded arrays 
c       filled by DRYDEPRT ---
c
      if( tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC ) then
          do ispc=1,nspsa
             do icl=1,nox
                do jcl=1,noy
                  if( tectyp .EQ. RTCMC )
     &               vdeprt(icl,jcl,ispc) = dryrtc(ispc)
                  vdepsa(icl,jcl,ispc) = vdeprt(icl,jcl,ispc)
                enddo
             enddo
          enddo
          goto 9999
      endif
c
c  --- allocate the temporary arrays ---
c
      allocate( consum(nox,noy,MXTRCLS) )
      call zeros(consum,nox*noy*MXTRCLS)
c
      allocate( vdsum(nox,noy,MXTRCLS) )
      call zeros(vdsum,nox*noy*MXTRCLS)
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
c  --- seallocate the local arrays ----
c
      deallocate( consum )
      deallocate( vdsum )
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
c
      return
      end

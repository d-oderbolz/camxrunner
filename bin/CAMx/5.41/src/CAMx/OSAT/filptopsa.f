c**** FILPTOPSA.F
c
      subroutine filptopsa(igrid,nox,noy,nspec,nspsa,
     &                                          conctop,saconctop)
      use bndary
      use tracer
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine loads the new top concentration values from the regular
c   model into the appropriate tracer array locations. This is only done
c   for the boundary condition tracers. The global array ptop_fac contains
c   a value of 1.0 for all boundary tracers and 0.0 for all others.
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c      Argument description:
c       Outputs:
c           saconctop   R  tracer concentrations
c       Inputs:
c           igrid       I  grid number 
c           nox         I  number of X cells in the grid
c           noy         I  number of Y cells in the grid
c           nspec       I  number of species in the grid
c           nspsa       I  number of tracer species
c           conctop     R  regular model concentrations
c       
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
      integer   igrid
      integer   nox
      integer   noy
      integer   nspec
      integer   nspsa
      real      conctop(nox,noy,nspec)
      real      saconctop(nox,noy,nspsa)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer icls, icl, jcl, ispc
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c ---- initialize to zero ----
c
      call zeros(saconctop,nox*noy*nspsa)
c
c  ---- read concentrations for each species ----
c
      do 10 ispc=1,nspec
c
c   --- if the species is a not being used, skip it ---
c
          if( .NOT. lusespc(ispc) ) goto 10
c
c   --- put concentractions in arrays ---
c
           do 20 icl=2,nox-1
c
c   --- loop over cells ---
c
             do jcl=2,noy-1
c
c   --- load species ---
c
               do icls=1,ntrcls
                  if( ipttrc(icls) .NE. 0 ) then 
                     do itr=ipttrc(icls),npttrc(icls)
                       saconctop(icl,jcl,itr) = saconctop(icl,jcl,itr) +
     &                       conctop(icl,jcl,ispc) * trspmap(ispc,icls) *
     &                                                       ptop_fac(itr)
                     enddo
                  endif
               enddo
c
c  --- next species ---
c
             enddo
   20     continue
c
c  --- next species --
c
   10 continue
c
c  --- make sure top conc is at least lower bound ---
c
      do jcl=2,noy-1
        do icl=2,nox-1
           do itr=1,ntotsp
              saconctop(icl,jcl,itr) = 
     &                       MAX( saconctop(icl,jcl,itr), BNDLPT )
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

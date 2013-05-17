c**** FILAQSA.F
c
      subroutine filaqsa(igrid,nox,noy,noz,nspec,nspsa,conc,saconc)
      use bndary
      use tracer
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine fils one hour of initial conditions and calculates 
c   the NOx and VOC levels.  It then places these concentrations in the 
c   appropriate place in the gridded array used for tracer concentrations.  
c   The O3 concentrations are placed into the concentration arrays 
c   for the Ozone tracer species. 
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c      Argument description:
c       Outputs:
c           saconc   R  tracer concentrations
c       Inputs:
c           igrid     I  grid number 
c           nox      I  number of X cells in the grid
c           noy      I  number of Y cells in the grid
c           noz      I  number of layers in the grid
c           nspec    I  number of species in the grid
c           nspsa    I  number of tracer species
c           conc     R  regular model concentrations
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
      integer   noz
      integer   nspec
      integer   nspsa
      real      conc(nox,noy,noz,nspec)
      real      saconc(nox,noy,noz,nspsa)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer icls, icl, jcl, izcl, ispc
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- call routine to zero out the array ---
c
      call zeros(saconc,nox*noy*noz*nspsa)
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
                 do izcl=1,noz
                   do icls=1,ntrcls
                       saconc(icl,jcl,izcl,iptcls(icls)) =  
     &                       saconc(icl,jcl,izcl,iptcls(icls)) + 
     &                               conc(icl,jcl,izcl,ispc) * 
     &                                                trspmap(ispc,icls)
                   enddo
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

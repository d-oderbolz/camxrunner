c**** FILAQSA.F
c
      subroutine filaqsa(igrid,nox,noy,noz,nspec,nspas,conc,saconc)
c
c----CAMx v4.51 080522
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
c     Copyright 1996-2008
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
c           nspas    I  number of tracer species
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
      include 'bndary.com'
      include 'tracer.com'
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
      integer   nspas
      real      conc(nox,noy,noz,nspec)
      real      saconc(nox,noy,noz,nspas)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer icls, icl, jcl, izcl, jclbeg, jclend, ispc
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- call routine to zero out the array ---
c
      call zeros(saconc,nox*noy*noz*nspas)
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
c  --- set the beginning and ending of the interior of grid ---
c
             if( igrid .EQ. 1 ) then
               if( jbeg(icl) .EQ. -999 ) goto 20 
               jclbeg = jbeg(icl) 
               jclend = jend(icl) 
             else 
               jclbeg = 2
               jclend = noy-1 
             endif 
c
c   --- loop over cells ---
c
             do jcl=jclbeg,jclend
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

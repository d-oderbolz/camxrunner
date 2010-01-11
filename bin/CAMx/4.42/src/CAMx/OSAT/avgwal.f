c*** AVGWAL
c
      subroutine avgwal(igrd,ncol,nrow,nlay,nspec,nspsa,dtime,
     &                    deltax,deltay,depth,tempk,press,saconc,conc)
c
c----CAMx v4.42 070603
c
c-----------------------------------------------------------------------
c   Description:
c     This routine calculates the average concentrations for the
c     WALL OF CELLS types of receptors.  It is not called unless
c     at least one receptor of this type is specified.  The average
c     is taken over all cells in the wall (including layers).  The 
c     concentraions in each cell are weighted by cell volume.
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c         nrow   I  number of X cells in grid
c         ncol   I  number of Y cells in grid
c         nlay   I  number of layers
c         nspsa  I  number of tracer species
c         nspec  I  number of species in regular model
c         dtime  R  time step for present concs
c         deltax R  cell width in X direction        
c         deltay R  cell width in Y direction        
c         depth  R  layer depths
c         tempk  R  temperature
c         press  R  pressure
c         saconc R  tracer concentrations 
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     1.  Fixed bug by eliminating unused argument (avtime)
c     2.  Added grid number to recptors defined by cell index
c     3.  Fixed bug in calculating the timing weight factor
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'camx.com'
      include 'bndary.com'
      include 'tracer.com'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer   nrow
      integer   ncol
      integer   nlay
      integer   nspsa
      integer   nspec
      real      dtime
      real      deltax(nrow)
      real      deltay
      real      depth(ncol,nrow,nlay)
      real      tempk(ncol,nrow,nlay)
      real      press(ncol,nrow,nlay)
      real      saconc(ncol,nrow,nlay,nspsa)
      real      conc(ncol,nrow,nlay,nspec)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   icl, jcl, kcl, ircp, ispc
      real      volume, volsum, sasum(MXTRSP), cncsum(MXSPEC)
      real      cncnow, cnvfac, timewt
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- if no receptor file is provided, just return ---
c
      if( .NOT. lrcpfil ) goto 9999
c
c  --- calculte the time average weighting for running average
c
      timewt = (dtime / dtout) / 60.
c
c  --- loop over all receptors ---
c
      do 10 ircp=1,nrecep
c
c  --- if receptor is not a WALL OF CELLS type, skip it ---
c
          if( idrcp(ircp) .NE. IDWAL ) goto 10
c
c   --- skip if receptor grid is not this grid ---
c
          if( igrdrcp(ircp) .NE. igrd ) goto 10
c
c  --- initialize the sums to zero ---
c
          volsum = 0.
          do ispc=1,nspsa
             sasum(ispc) = 0.
          enddo
          do ispc=1,nspec
             cncsum(ispc) = 0.
          enddo
c
c  --- loop over layers ---
c
          do kcl=kwalbg(ircp),kwalnd(ircp)
c
c  ---- loop over all cells ---
c
              do jcl=jwalbg(ircp),jwalnd(ircp)
                 do 20 icl=iwalbg(ircp),iwalnd(ircp)
c
c  --- slip the boundary cells ---
c
                    if( ibeg(jcl) .EQ. -999 ) goto 20
                    if( icl .LT. ibeg(jcl) .OR. 
     &                                icl .GT. iend(jcl) ) goto 20
c
c  ---- calculate the total volume for this cell and the 
c       conversion factor to ppm ---
c
                    volume = deltax(jcl)/1000. * deltay/1000. * 
     &                                              depth(icl,jcl,kcl)
                    volsum = volsum + volume
                    cnvfac = densfac*( 273./tempk(icl,jcl,kcl)*
     &                                         press(icl,jcl,kcl)/1013) 
c
c   --- loop over tracer species --
c
                    do ispc=1,nspsa
                       sasum(ispc) = sasum(ispc) + volume * 
     &                                saconc(icl,jcl,kcl,ispc) / cnvfac
                    enddo
c
c   --- loop over regular mode species ----
c
                    do ispc=1,nspec
                       cncsum(ispc) = cncsum(ispc) + volume * 
     &                                conc(icl,jcl,kcl,ispc) / cnvfac
                    enddo
c
c   --- next cell ---
c
   20            continue
              enddo
c
c   --- next layer ---
c
          enddo
c
c  ---- add concs to the running average ---
c
          do ispc=1,nspsa
              cncnow = 0.
              if( volsum .GT. 0. ) cncnow = sasum(ispc) / volsum
              conrcp(ispc,ircp) = cncnow * timewt + conrcp(ispc,ircp)
          enddo
c
c  --- next receptor ----
c
   10 continue
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

c*** AVGWAL
c
      subroutine avgwal(igrd,numcol,numrow,numlay,ncols,nrows,nlays,
     &                  i0,j0,nspsa,dtime,deltax,deltay,depth,mapscl,
     &                  tempk,press,saconc)
      use bndary
      use camxcom
      use tracer
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c   Description:
c     This routine calculates the average concentrations for the
c     WALL OF CELLS types of receptors.  It is not called unless
c     at least one receptor of this type is specified.  The average
c     is taken over all cells in the wall (including layers).  The 
c     concentraions in each cell are weighted by cell volume.
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c         igrd   I  grid number
c         numrow I  number of X cells in grid for this slice
c         numcol I  number of Y cells in grid for this slice
c         numlay I  number of layers for this slice
c         ncols  I  number of columns in entire grid
c         nrows  I  number of rows in entire grid
c         nlays  I  number of layers in entire grid
c         i0     I  offset of slice in entire grid in X direction
c         j0     I  offset of slice in entire grid in Y direction
c         nspsa  I  number of tracer species
c         dtime  R  time step for present concs
c         deltax R  cell width in X direction        
c         deltay R  cell width in Y direction        
c         depth  R  layer depths
c         mapscl R  map scale factor
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
c     4.  Added map scale factor (08/02/2007)
c         Removed model spc conc (08/02/2007)
c     5.  Fixed bug that was applying gas conversion factor to
c         all tracer species.
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
      integer   igrd
      integer   numcol
      integer   numrow
      integer   numlay
      integer   ncols
      integer   nrows
      integer   nlays
      integer   i0
      integer   j0
      integer   nspsa
      real      dtime
      real      deltax(nrows)
      real      deltay
      real      depth(ncols,nrows,nlays)
      real      mapscl(numcol,numrow)
      real      tempk(numcol,numrow,numlay)
      real      press(numcol,numrow,numlay)
      real      saconc(numcol,numrow,numlay,nspsa)
c
c----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer icl, jcl, kcl, ircp, ispc
      real    volume, volsum
      real    cncnow, cnvfac, timewt
c
      real sasum(MXTRSP)
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
                    if( icl-i0 .LE. 1 .OR. icl-i0 .GE. numcol ) goto 20
                    if( jcl-j0 .LE. 1 .OR. jcl-j0 .GE. numrow ) goto 20
c
c  ---- calculate the total volume for this cell and the 
c       conversion factor to ppm ---
c
                    volume = deltax(jcl) * deltay * depth(icl,jcl,kcl)
     &                                          / (mapscl(icl,jcl)**2)
                    volsum = volsum + volume
c
c   --- loop over tracer species --
c
                    do ispc=1,nspsa
                       cnvfac = 1.0
                       if( lsagas(ispc) ) cnvfac = 
     &                     densfac*( 273./tempk(icl-i0,jcl-j0,kcl)*
     &                                    press(icl-i0,jcl-j0,kcl)/1013) 
                       sasum(ispc) = sasum(ispc) + volume * 
     &                           saconc(icl-i0,jcl-j0,kcl,ispc) / cnvfac
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

c*** RECALIB2
c
      subroutine recalib2(numcol,numrow,numlay,nspc,
     &                                   saconc,icl,jcl,cnccls)
      use chmstry
      use tracer
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c   Description:
c     This routine recalibrates the tracer concentrations back to
c     the levels calculated in the regular model.  This is necessary
c     because the numerical algorithms used in the transport steps
c     cause the tracer species to stray from the regular model.
c     This is due to the sharp gradients at model boundary cells
c     and between source regions.  The regular model does not stratify
c     the concentrations by source region so these gradients do not 
c     appear in the regular model.
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       numcol  I  number of cells in X direction
c       numrow  I  number of cells in Y direction
c       numlay  I  number of layers
c       nspc    I  number of tracer species
c       saconc  R  tracer concentration array
c       icl     I  the X grid location of current cell
c       jcl     I  the Y grid location of current cell
c       cnccls  R  regular model concentrations for this class
c
c     Modifications:
c       05/03/07   Added code to check for lower bound before 
c                  applying adjustment -- adjustment now only applied
c                  to the "significant" sources     
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
      integer numcol
      integer numrow
      integer numlay
      integer nspc
      real    saconc(numcol,numrow,numlay,nspc)
      integer icl
      integer jcl
      real    cnccls(MXTRCLS,*)
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c    FUZZ - fuzz value for comparing against lower bound
c
      real FUZZ
c
      parameter( FUZZ = 10.0 )
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer i
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- loop over layers ---
c
      do ilay = 1,numlay
c
c  --- calculate the sum of the tracer species ----
c
         do icls=1,ntrcls
            trval = 0.
            if( npttrc(icls) .GT. 0. ) then
               do i=ipttrc(icls),npttrc(icls) 
                  if( saconc(icl,jcl,ilay,i) .GT. FUZZ*BNDLPT )
     &                       trval = trval + saconc(icl,jcl,ilay,i)
               enddo
c
c   --- recalibrate the tracers, if necessary ---
c
               if( trval .NE. cnccls(icls,ilay) .AND. trval .GT. 0 ) then
                  do i=ipttrc(icls),npttrc(icls)
                     if( saconc(icl,jcl,ilay,i) .GT. FUZZ*BNDLPT )
     &                  saconc(icl,jcl,ilay,i) = saconc(icl,jcl,ilay,i) +
     &                                    ( cnccls(icls,ilay) - trval ) *
     &                                     saconc(icl,jcl,ilay,i) / trval
                     saconc(icl,jcl,ilay,i) = 
     &                                MAX(saconc(icl,jcl,ilay,i),BNDLPT)
                   enddo
               endif
            endif
         enddo
c
c   --- next layer ---
c
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

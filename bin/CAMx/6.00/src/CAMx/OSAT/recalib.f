c*** RECALIB
c
      subroutine recalib(numcol,numrow,numlay,nspc,saconc,icl,jcl,kzcl,modcon)
      use chmstry
      use tracer
c
c----CAMx v6.00 130506
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
c     Copyright 1996 - 2013
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
c       kzcl    I  the vertical grid location of current layer
c       modcon  R  regular model concentrations
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
      integer kzcl
      real    modcon(*)
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
      real    modcls(MXALCLS)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- caluclate the conentrations for each tracer class ---
c
      do i=1,ntrcls
        modcls(i) = 0.
      enddo
      do ispc=1,nspec
        do i=1,ntrcls
           modcls(i) = modcls(i) + modcon(ispc) * fluxmap(ispc,i)
        enddo
      enddo
c
c  --- calculate the sum of the tracer species ----
c
      do icls=1,ntrcls
         trval = 0.
         if( npttrc(icls) .GT. 0. ) then
            do i=ipttrc(icls),npttrc(icls) 
               if( saconc(icl,jcl,kzcl,i) .GT. FUZZ*BNDLPT )
     &                       trval = trval + saconc(icl,jcl,kzcl,i)
            enddo
c
c   --- recalibrate the tracers, if necessary ---
c
            if( trval .NE. modcls(icls) .AND. trval .GT. 0 ) then
               do i=ipttrc(icls),npttrc(icls)
                  if( saconc(icl,jcl,kzcl,i) .GT. FUZZ*BNDLPT )
     &               saconc(icl,jcl,kzcl,i) = saconc(icl,jcl,kzcl,i) +
     &                                      ( modcls(icls) - trval ) *
     &                                  saconc(icl,jcl,kzcl,i) / trval
                  saconc(icl,jcl,kzcl,i) = 
     &                             MAX(saconc(icl,jcl,kzcl,i),BNDLPT)
                enddo
            endif
         endif
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

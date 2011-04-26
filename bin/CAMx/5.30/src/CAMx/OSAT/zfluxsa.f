c*** ZFLUXSA
c
      subroutine zfluxsa(numcol,numrow,numlay,nspc,saconc,satop,
     &           icell,jcell,rhofac,deltat,d1d,fluxcls,cnccls,cncafter)
      use tracer
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c   Description:
c     This routine updates the tracer concentrations arrays in the
c     common block variables by applying the appropriate flux value
c     calculated from the regular model transport routine. This is 
c     for the vertical transport.
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       numcol   I  number of columns in the grid
c       numrow   I  number of rows in the grid
c       numlay   I  number of layers in the grid
c       nspc     I  number of species
c       saconc   R  3-D concentration array for tracers
c       satop    R  top boundary concentrations for tracers
c       icell    I  cell index in the X direction
c       jcell    I  cell index in the Y direction
c       rhofac   R  density extrapolation factor to top of model
c       deltat   I  change in time for this time step
c       d1d      I  layer depths
c       fluxcls  R  flux for the each tracer class
c       cnccls   R  initial concentration for each tracer class
c       cncafter R  final concentration for each tracer class
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     11/15/09   --gwilson--  Orignal development
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
      integer     numcol
      integer     numrow
      integer     numlay
      integer     nspc
      real        saconc(numcol,numrow,numlay,nspc)
      real        satop(numcol,numrow,nspc)
      integer     icell
      integer     jcell
      real        rhofac
      real        deltat
      real        d1d(MXLAYER)
      real        fluxcls(MXTRCLS,*)
      real        cnccls(MXTRCLS,*)
      real        cncafter(MXTRCLS,*)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   ilay, ispc, icls
      real      sa_avg, cls_avg, fluxtmp(MXLAYER), cnctmp(MXLAYER)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- Loop over each tracer class ---
c
      do icls=1,ntrcls
c
c   --- loop over each tracer species in this class ---
c
          do ispc=iptcls(icls),nptcls(icls)
c
c   --- make sure all values are above lower bound, to start ----
c
            do ilay=1,numlay
              saconc(icell,jcell,ilay,ispc) =
     &                      AMAX1(saconc(icell,jcell,ilay,ispc),BNDLPT)
            enddo
c
c  --- handle the top layer ---
c
            if( fluxcls(icls,numlay) .LT. 0. ) then
                fluxtmp(numlay) = fluxcls(icls,numlay) * 
     &             satop(icell,jcell,ispc) * ptop_fac(ispc) * rhofac /
     &                                          cnccls(icls,numlay+1)
            else
                fluxtmp(numlay) = fluxcls(icls,numlay) * 
     &                          satop(icell,jcell,ispc) * rhofac / 
     &                                          cnccls(icls,numlay+1)
            endif
c
c   --- loop over all interior cells in this row ---
c
            do ilay=1,numlay-1
c
c   ---  apply flux to the tracer species ---
c
                zfacp = d1d(ilay)/( d1d(ilay) + d1d(ilay+1) )
                zfacm = d1d(ilay+1)/( d1d(ilay) + d1d(ilay+1) )
                sa_avg = saconc(icell,jcell,ilay,ispc)*zfacm +
     &                   saconc(icell,jcell,ilay+1,ispc)*zfacp 
                cls_avg = cnccls(icls,ilay)*zfacm +
     &                    cnccls(icls,ilay+1)*zfacp
                fluxtmp(ilay) = fluxcls(icls,ilay) * sa_avg / cls_avg 
            enddo
            do ilay=2,numlay
                cnctmp(ilay) = saconc(icell,jcell,ilay,ispc) - 
     &            (fluxtmp(ilay) - fluxtmp(ilay-1)) * deltat/d1d(ilay)
            enddo
            cnctmp(1) = saconc(icell,jcell,1,ispc) - 
     &                                        fluxtmp(1)*deltat/d1d(1)
c
c  --- put new concentrations back into 3-D array ---
c
            do ilay=1,numlay
              saconc(icell,jcell,ilay,ispc) = AMAX1(cnctmp(ilay),BNDLPT)
            enddo
c
c  --- next tracer species ---
c
          enddo
c
c   --- next tracer class
c
      enddo
c
c
c  --- call routine to recalibrate so that tracer species stays
c      on track with model species: needed because the numerics
c      cause tracer species to drift
c
      call recalib2(numcol,numrow,numlay,nspc,
     &                              saconc,icell,jcell,cncafter)
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

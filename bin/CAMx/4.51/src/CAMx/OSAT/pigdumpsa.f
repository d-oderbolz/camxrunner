c*** PIGDUMPSA
c
      subroutine pigdumpsa(ncolx,nrowy,nlays,nspc,icell,jcell,kclbeg,
     &                              kclend,idxspc,idxpig,delconc,saconc)
      implicit none
c
c----CAMx v4.51 080522
c
c-----------------------------------------------------------------------
c   Description:
c     This routine puts the mass from the PiG into the tracer
c     concentration array.  The mass is added to the tracer species
c     from the region/group from which the PiG originated.
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       ncolx   I  number of columns
c       nrowy   I  number of rows
c       nlays   I  number of layers
c       nspc    I  number of species
c       icell   I  the X grid location of current cell
c       jcell   I  the X grid location of current cell
c       kclbeg  I  the bottom vertical layer in the affected column
c       kclend  I  the top vertical layer in the affected column
c       idxspc  I  the index of the species
c       idxpig  I  the index of the puff in PiG arrays
c       delconc R  change in model concentrations 
c       saconc  R  gridded array of concentrations
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c       12/08/96  --gwilson--   Original development
c       11/10/97  --gwilson--   Removed unused argument: IGRID
c       08/25/05  --cemery--    Revamped to use puff-specific source 
c                               region/group pointers
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'grid.com'
      include 'pigsty.com'
      include 'tracer.com'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer   ncolx
      integer   nrowy
      integer   nlays
      integer   nspc
      integer   icell
      integer   jcell
      integer   kclbeg
      integer   kclend
      integer   idxpig
      integer   idxspc
      real      delconc(nlays)
      real      saconc(ncolx,nrowy,nlays,nspc)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer icls, itrc, i, kcel
      real    sumcls(MXALCLS)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- loop over all layers in the affected column ---
c  
      do kcel=kclbeg,kclend
c
c  --- if concentration change is positive, attribute to the puff ---
c
         if( delconc(kcel) .GT. 0. ) then
c
c  --- attribute the increase to the tracer for this plume ---
c
            do icls=1,ntrcls
              if( nptcls(icls) .GT. 0 .AND. 
     &                            fluxmap(idxspc,icls) .GT. 0. ) then
                   itrc = iemcls(icls) - 1 + ipufmap(idxpig) +
     &                                    ipufgrp(idxpig)*nregin
c
c  --- have to handle O3 differently because of O3V and O3N ---
c
                   if( icls .NE. idxipt(ITRO3V) .AND. 
     &                                  icls .NE. idxipt(ITRO3N) ) then
                       saconc(icell,jcell,kcel,itrc) = 
     &                      saconc(icell,jcell,kcel,itrc) + 
     &                              delconc(kcel) * fluxmap(idxspc,icls)
c
c  --- if GOAT, split to O3V and O3V ---
c
                    else
                       if( tectyp .EQ. GOAT ) then
                           saconc(icell,jcell,kcel,itrc) = 
     &                          saconc(icell,jcell,kcel,itrc) + 
     &                              delconc(kcel) * trspmap(idxspc,icls)
c
c  --- if not GOAT, it all goes to O3V ---
c
                       else
                           if( icls .EQ. idxipt(ITRO3V) ) then
                               saconc(icell,jcell,kcel,itrc) = 
     &                            saconc(icell,jcell,kcel,itrc) + 
     &                              delconc(kcel) * fluxmap(idxspc,icls)
                           endif
                       endif
                    endif
               endif
            enddo
c
c  --- find the tracer classes this species contributes to ---
c
          else if(delconc(kcel) .LT. 0. ) then
            do icls=1,ntrcls
                sumcls(icls) = 0.
                if( nptcls(icls) .GT. 0 .AND. 
     &                    icls .NE. idxipt(ITRO3V) .AND. 
     &                            fluxmap(idxspc,icls) .GT. 0. ) then
                  do i=iptcls(icls),nptcls(icls)
                     sumcls(icls) = sumcls(icls) +  
     &                                    saconc(icell,jcell,kcel,i) 
                  enddo
               endif
             enddo
c
c  ---- reduce the tracer based on it's relative contribution ---
c
             do icls=1,ntrcls
               if( sumcls(icls) .GT. 0. ) then
                 do i=iptcls(icls),nptcls(icls)
                    saconc(icell,jcell,kcel,i) = 
     &                  saconc(icell,jcell,kcel,i) + 
     &                     delconc(kcel) * fluxmap(idxspc,icls) *
     &                         saconc(icell,jcell,kcel,i) / sumcls(icls)
                    saconc(icell,jcell,kcel,i) =
     &                         MAX( BNDLPT,saconc(icell,jcell,kcel,i) )
                 enddo
               endif
             enddo
         endif
c
c  --- next affected layer ---
c
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

c*** PIGWETSA
c
      subroutine pigwetsa(ncolx,nrowy,nspc,ntrac,icell,jcell,
     &                                         idxpig,delwet,wetfld)
      use grid
      use pigsty
      use tracer
      implicit none
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c   Description:
c     This routine puts the wet deposition from the PiG into the tracer
c     wet deposition rray.  
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       ncolx   I  number of columns
c       nrowy   I  number of rows
c       nspc    I  number of model species
c       ntrac   I  number of tracers
c       icell   I  the X grid location of current cell
c       jcell   I  the X grid location of current cell
c       idxpig  I  the index of the puff in PiG arrays
c       delwet  R  change in model wet depostion
c       wetfld  R  gridded array of tracer wet depositions
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
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
      integer   ncolx
      integer   nrowy
      integer   nlays
      integer   nspc
      integer   ntrac
      integer   icell
      integer   jcell
      integer   kclbeg
      integer   kclend
      integer   idxpig
      real      delwet(nspc)
      real      wetfld(ncolx,nrowy,ntrac)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer icls, itrc, ispc, i
      real    sumcls(MXALCLS)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- loop over all model species ---
c
      do ispc=1,nspc
c
c  --- if concentration change is positive, attribute to the puff ---
c
          if( delwet(ispc) .GT. 0. ) then
c
c  --- attribute the increase to the tracer for this plume ---
c
             do icls=1,ntrcls
               if( nptcls(icls) .GT. 0 .AND. 
     &                         fluxmap(ispc,icls) .GT. 0. ) then
                  itrc = iemcls(icls) - 1 + ipufmap(idxpig) +
     &                                    ipufgrp(idxpig)*nregin
c
c  --- have to handle O3 differently because of O3V and O3N ---
c
                  if( icls .NE. idxipt(ITRO3V) .AND. 
     &                               icls .NE. idxipt(ITRO3N) ) then
                      wetfld(icell,jcell,itrc) = 
     &                     wetfld(icell,jcell,itrc) + 
     &                           delwet(ispc) * fluxmap(ispc,icls)
c
c  --- if GOAT, split to O3V and O3V ---
c
                   else
                      if( tectyp .EQ. GOAT ) then
                          wetfld(icell,jcell,itrc) = 
     &                       wetfld(icell,jcell,itrc) + 
     &                           delwet(ispc) * trspmap(ispc,icls)
c
c  --- if not GOAT, it all goes to O3V ---
c
                      else
                          if( icls .EQ. idxipt(ITRO3V) ) then
                              wetfld(icell,jcell,itrc) = 
     &                           wetfld(icell,jcell,itrc) + 
     &                             delwet(ispc) * fluxmap(ispc,icls)
                          endif
                      endif
                   endif
               endif
             enddo
c
c  --- find the tracer classes this species contributes to ---
c
          else if(delwet(ispc) .LT. 0. ) then
             do icls=1,ntrcls
                 sumcls(icls) = 0.
                 if( nptcls(icls) .GT. 0 .AND. 
     &                 icls .NE. idxipt(ITRO3V) .AND. 
     &                         fluxmap(ispc,icls) .GT. 0. ) then
                   do i=iptcls(icls),nptcls(icls)
                      sumcls(icls) = sumcls(icls) +  
     &                                 wetfld(icell,jcell,i) 
                   enddo
                endif
              enddo
c
c  ---- reduce the tracer based on it's relative contribution ---
c
              do icls=1,ntrcls
                if( sumcls(icls) .GT. 0. ) then
                  do i=iptcls(icls),nptcls(icls)
                     wetfld(icell,jcell,i) = 
     &                   wetfld(icell,jcell,i) + 
     &                      delwet(ispc) * fluxmap(ispc,icls) *
     &                          wetfld(icell,jcell,i) / sumcls(icls)
                  enddo
                endif
              enddo
          endif
c
c   --- next model species ---
c
        enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

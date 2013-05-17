      subroutine paconv(m1,m2,m3,dt,ncol,nrow,nlay,nspav,nspc,lmap,
     &                                   lgassp,tempk,press,ipa_cel)
      use chmstry
      use bndary
      use camxcom
      use procan
      use rtracchm
      use tracer
      use node_mod
c
c----CAMx v6.00 130506
c
c     This routine calculates and stores the concentration conversion 
c     factor and stores it in the process analysis array.
c     
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Modifications: 
c
c     Input arguments:
c        dt                 time step for present grid concentration (s)
c        ncol               number of columns
c        nrow               number of rows
c        nlay               number of layers in instantaneous array
c        nspav              number of average species
c        nspc               number of species in conc array
c        lmap               mapping array for average species
c        lgassp             true if species is gas
c        tempk              temperature field (K)
c        press              pressure field (mb)
c        ipa_cel            gridded array to identify if cell is
c                            in a IPRM sub-domain
c
c     Output arguments:
c                                                          other=ug/m3)
c
c     Routines Called:
c        none
c
c     Called by:
c        CAMx
c        FGAVRG
c
      include "camx.prm"
 
      integer m1,m2,m3
c
c========================= Process Analysis Begin ==============================
c
      integer ipa_cel(ncol,nrow,nlay)
c
c========================= Process Analysis End ==============================
c
      logical lgassp(nspc)
      real    tempk(m1,m2,m3),press(m1,m2,m3)
      integer lmap(*)
c
c-----Entry point
c
c-----Increment running average
c
      dtfact = dt/(dtout*60.)
      do 40 l = 1,nspav
        lsp = lmap(l) 
        convfac = 1.
        do j = 2,m2-1
          do i = 2,m1-1
c
            do k=1,nlay
               if( lgassp(lsp) ) then
                   tmp = 273./tempk(i,j,k)*press(i,j,k)/1013.
                   convfac = 1./(densfac*tmp)
               endif
               if( i .GE. ia .and. i .LE. iz .AND.      
     &                           j .GE. ja .AND. j .LE. jz ) then
                    if( ipa_cel(i+i0,j+j0,k) .GT. 0 ) then
                           ipa_idx = ipa_cel(i+i0,j+j0,k) 
c
c-----Save the units conversion factor for use in IPR post-processing
c
                           cipr(IPR_CONV, ipa_idx, lsp) = 
     &                             cipr(IPR_CONV, ipa_idx, lsp)
     &                                           + convfac * dtfact
                    endif
              endif
            enddo
          enddo
        enddo
  40  continue
c
      return
      end

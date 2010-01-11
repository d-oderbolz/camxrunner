      subroutine average(m1,m2,m3,losat,igrd,dt,ncol,nrow,nlay,
     &                   nlayav,nspav,nspc,nsprad,lmap,lmaprad,lgassp,
     &                   tempk,press,conc,radcnc,avcnc,ipa_cel)
      use chmstry
      use bndary
      use camxcom
      use procan
      use rtracchm
      use tracer
      use node_mod
c
c-----CAMx v5.10 090918
c
c     AVERAGE computes time-averaged concentrations
c
c     Copyright 1996-2005
c     ENVIRON International Corporation
c
c     Modifications: 
c        01/30/02    --gwilson--  Added code for RTRAC probing tool
c        12/15/08    --gwilson--  Added code to handle averaging of
c                                 radicals
c
c     Input arguments:
c        losat              .TRUE. if concentrations are tracer species
c        igrd               grid index
c        dt                 time step for present grid concentration (s)
c        ncol               number of columns
c        nrow               number of rows
c        nlay               number of layers in instantaneous array
c        nlayav             number of layers in average array
c        nspav              number of average species
c        nspc               number of species in conc array
c        lmap               mapping array for average species
c        lmaprad            mapping array for average species for radicals
c        lgassp             true if species is gas
c        tempk              temperature field (K)
c        press              pressure field (mb)
c        conc               instant species concentration (umol/m3)
c        radcnc             radical species concentration (PPM)
c        avcnc              average species concentration (gas=ppm,
c                                                          other=ug/m3)
c        ipa_cel            gridded array to identify if cell is
c                            in a IPRM sub-domain
c
c     Output arguments:
c        avcnc              average species concentration (gas=ppm,
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
      logical losat
      logical lrad
      real    tempk(m1,m2,m3),press(m1,m2,m3)
      real    avcnc(m1,m2,nlayav,nspav)
      real    conc(m1,m2,m3,nspc)
      real    radcnc(m1,m2,m3,nsprad)
      integer lmap(*)
      integer lmaprad(*)
c
c-----Entry point
c
c-----Increment running average
c
      dtfact = dt/(dtout*60.)
      do 40 l = 1,nspav
        lrad = .FALSE.
        lsp = lmap(l) 
        if( lsp .EQ. 0 ) then
           lrad = .TRUE.
           lsp = lmaprad(l)
        endif
        convfac = 1.
        do 30 j = 2,m2-1
          do i = 2,m1-1
c
            do k=1,nlayav
                if( .NOT. lrad ) then
                  if( lgassp(lsp) ) then
                      tmp = 273./tempk(i,j,k)*press(i,j,k)/1013.
                      convfac = 1./(densfac*tmp)
                  endif
                endif
                if( .NOT. lrad ) then
                    avcnc(i,j,k,l) = convfac*conc(i,j,k,lsp)*dtfact +
     &                                                  avcnc(i,j,k,l)



                else
                    avcnc(i,j,k,l) = radcnc(i,j,k,lsp)*dtfact +
     &                                                  avcnc(i,j,k,l)
                endif
c
c========================= Process Analysis Begin ==============================
c
                if( .NOT. losat .AND. lipr ) then
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
                 endif
c
c========================= Process Analysis End ==============================
c
            enddo
          enddo
  30    continue
  40  continue
c
      dtfact = dt/(dtout*60.)
      return
      end

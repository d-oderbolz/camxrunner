      subroutine sumwt4(idx,ncolx,nrowy,nlays,depth,conwst,conest,
     &                  consth,connth,consum)
c
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c
c----CAMx v4.51 080522
c     
c     SUMWT4 sums up a species concentration for the four boundaries
c
c     Input arguments:
c        idx             species ID
c        ncolx           the number of coarse grid columns
c        nrowy           the number of coarse grid rows
c        nlays           the number of coarse grid layers
c        depth           layer depth
c        conwst          west species concentration at west boundary
c        conest          east species concentration at east boundary
c        consth          south species concentration at south boundary
c        connth          north species concentration at north boundary
c
c     Output argumnets:
c        consum          sum of the species concentrations from the four
c                        boundaries
c
      include "camx.prm"
      include "bndary.com"
      include "tracer.com"
c
      real      depth(ncolx,nrowy,nlays)
      real      consum(MXSPEC,0:IDXBTP)
      real      conwst(MXLAYA,MXROWA), conest(MXLAYA,MXROWA)
      real      consth(MXLAYA,MXCOLA), connth(MXLAYA,MXCOLA)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- do the west and east boundaries first ----
c
      do jcl=1,nrowy
c
c   --- add contribution from WEST boundary ---
c
        icl = ibeg(jcl) - 1
        if( icl .GT. 0 .AND. icl .LE. ncolx ) then
           sumthk = 0
           sumcon = 0
c
c   --- add the concentrations, weighting by layer thickness ---
c
           do izcl=1,nlays
               if( conwst(izcl,jcl) .GT. 0 ) then
                 sumthk = sumthk + depth(icl,jcl,izcl)
                 sumcon = sumcon + 
     &                       conwst(izcl,jcl) * depth(icl,jcl,izcl)
               endif
           enddo
           if( sumthk .GT. 0. ) then
               consum(idx,IDXBWS) = consum(idx,IDXBWS) + sumcon / sumthk
               consum(idx,0) = consum(idx,0) + sumcon / sumthk
           endif
        endif
c
c   --- add contribution from EAST boundary ---
c
        icl = iend(jcl) + 1
        if( icl .GT. 0 .AND. icl .LE. ncolx ) then
           sumthk = 0
           sumcon = 0
c
c   --- add the concentrations, weighting by layer thickness ---
c
           do izcl=1,nlays
               if( conest(izcl,jcl) .GT. 0. ) then
                  sumthk = sumthk + depth(icl,jcl,izcl)
                  sumcon = sumcon + 
     &                     conest(izcl,jcl) * depth(icl,jcl,izcl)
               endif
           enddo
           if( sumthk .GT. 0. ) then
               consum(idx,IDXBES) = consum(idx,IDXBES) + sumcon / sumthk
               consum(idx,0) = consum(idx,0) + sumcon / sumthk
           endif
        endif
      enddo
c
c   --- do the south and north bundarues ----
c
      do icl=1,ncolx
c
c   --- add contribution from SOUTH boundary ---
c
         jcl = jbeg(icl) - 1
         if( jcl .GT. 0 .AND. jcl .LE. nrowy ) then
            sumthk = 0
            sumcon = 0
c
c   --- add the concentrations, weighting by layer thickness ---
c
            do izcl=1,nlays
                if( consth(izcl,icl) .GT. 0 ) then
                   sumthk = sumthk + depth(icl,jcl,izcl)
                   sumcon = sumcon + 
     &                     consth(izcl,icl) * depth(icl,jcl,izcl)
                endif
            enddo
            if( sumthk .GT. 0. ) then
              consum(idx,IDXBST) = consum(idx,IDXBST) + sumcon / sumthk
              consum(idx,0) = consum(idx,0) + sumcon / sumthk
            endif
         endif
c
c   --- add contribution from NORTH boundary ---
c
         jcl = jend(icl) + 1
         if( jcl .GT. 0 .AND. jcl .LE. nrowy ) then
            sumthk = 0
            sumcon = 0
c
            do izcl=1,nlays
                if( connth(izcl,icl) .GT. 0. ) then
                   sumthk = sumthk + depth(icl,jcl,izcl)
                   sumcon = sumcon + 
     &                    connth(izcl,icl) * depth(icl,jcl,izcl)
                endif
            enddo
            if( sumthk .GT. 0. ) then
              consum(idx,IDXBNT) = consum(idx,IDXBNT) + sumcon / sumthk
              consum(idx,0) = consum(idx,0) + sumcon / sumthk
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

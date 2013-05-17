      subroutine sumwt4(idx,ncolx,nrowy,nlays,depth,conwst,conest,
     &                  consth,connth,consum)
      use bndary
      use tracer
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c
c----CAMx v5.41 121109
c     
c     SUMWT4 sums up a species concentration for the four boundaries
c
c     11/12/09   --gwilson--    Now calculates the top concentration
c                               by averaging the top layer values
c
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
c
      real depth(ncolx,nrowy,nlays)
      real consum(MXSPEC,0:IDXBTP)
      real conwst(MXLAYER,MXCELLS)
      real conest(MXLAYER,MXCELLS)
      real consth(MXLAYER,MXCELLS)
      real connth(MXLAYER,MXCELLS)
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
        sumthk = 0
        sumcon = 0
c
c   --- add the concentrations, weighting by layer thickness ---
c
        do izcl=1,nlays
            if( conwst(izcl,jcl) .GT. 0 ) then
              sumthk = sumthk + depth(1,jcl,izcl)
              sumcon = sumcon + 
     &                       conwst(izcl,jcl) * depth(1,jcl,izcl)
            endif
        enddo
        if( sumthk .GT. 0. ) then
            consum(idx,IDXBWS) = consum(idx,IDXBWS) + sumcon / sumthk
            consum(idx,0) = consum(idx,0) + sumcon / sumthk
        endif
        if( depth(1,jcl,nlays) .GT. 0. ) then
            consum(idx,IDXBTP) = consum(idx,IDXBTP) + 
     &                            conwst(nlays,jcl) / depth(1,jcl,nlays)
        endif
c
c   --- add contribution from EAST boundary ---
c
        sumthk = 0
        sumcon = 0
c
c   --- add the concentrations, weighting by layer thickness ---
c
        do izcl=1,nlays
            if( conest(izcl,jcl) .GT. 0. ) then
               sumthk = sumthk + depth(ncolx,jcl,izcl)
               sumcon = sumcon + 
     &                     conest(izcl,jcl) * depth(ncolx,jcl,izcl)
            endif
        enddo
        if( sumthk .GT. 0. ) then
            consum(idx,IDXBES) = consum(idx,IDXBES) + sumcon / sumthk
            consum(idx,0) = consum(idx,0) + sumcon / sumthk
        endif
        if( depth(ncolx,jcl,nlays) .GT. 0. ) then
            consum(idx,IDXBTP) = consum(idx,IDXBTP) + 
     &                       conest(nlays,jcl) / depth(ncolx,jcl,nlays)
        endif
      enddo
c
c   --- do the south and north bundarues ----
c
      do icl=1,ncolx
c
c   --- add contribution from SOUTH boundary ---
c
         sumthk = 0
         sumcon = 0
c
c   --- add the concentrations, weighting by layer thickness ---
c
         do izcl=1,nlays
             if( consth(izcl,icl) .GT. 0 ) then
                sumthk = sumthk + depth(icl,1,izcl)
                sumcon = sumcon + 
     &                     consth(izcl,icl) * depth(icl,1,izcl)
             endif
         enddo
         if( sumthk .GT. 0. ) then
           consum(idx,IDXBST) = consum(idx,IDXBST) + sumcon / sumthk
           consum(idx,0) = consum(idx,0) + sumcon / sumthk
         endif
         if( depth(icl,1,nlays) .GT. 0. ) then
            consum(idx,IDXBTP) = consum(idx,IDXBTP) + 
     &                       consth(nlays,icl) / depth(icl,1,nlays)
         endif
c
c   --- add contribution from NORTH boundary ---
c
         sumthk = 0
         sumcon = 0
c
         do izcl=1,nlays
             if( connth(izcl,icl) .GT. 0. ) then
                sumthk = sumthk + depth(icl,nrowy,izcl)
                sumcon = sumcon + 
     &                    connth(izcl,icl) * depth(icl,nrowy,izcl)
             endif
         enddo
         if( sumthk .GT. 0. ) then
           consum(idx,IDXBNT) = consum(idx,IDXBNT) + sumcon / sumthk
           consum(idx,0) = consum(idx,0) + sumcon / sumthk
         endif
         if( depth(icl,nrowy,nlays) .GT. 0. ) then
            consum(idx,IDXBTP) = consum(idx,IDXBTP) + 
     &                     connth(nlays,icl) / depth(icl,nrowy,nlays)
         endif
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

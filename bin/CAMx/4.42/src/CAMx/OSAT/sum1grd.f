      subroutine sum1grd(igroup,igrd,idx,ibeg,iend,emssum,emsgrd,
     &                   emsbas,emsoth,emslft,emstot,lemit)
c
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c
c----CAMx v4.42 070603
c
c     SUM1GRD sums up the area emission of one species for a given group
c     in a given grid
c
c       07/19/02  --gwilson-- Added seperate source area map for each grids.
c
c     Input argument:
c        igroup            group ID
c        igrd              grid ID
c        idx               specie ID
c        ibeg              begining row of the domain
c        iend              ending row of the domain
c        emsgrd            the species emission in the grid
c
c     Output arguments:
c        emssum            emission summed over grid
c        emsbas            base emission
c        emsoth            "otherwise" emission
c        emslft            leftover emission
c        emstot            total emission
c        lemit             flag to determine if tracer class is emitted
c        
      include "camx.prm"
      include "grid.com"
      include "tracer.com"
      include "chmstry.com"
c
c
      dimension ibeg(MXROWA),iend(MXROWA)
      real      emssum(MXSPEC,MXTRSP)
      real      emsgrd(MXCOLA,MXROWA)
      real      emsbas(MXSPEC,MXTRSP), emsoth(MXSPEC,MXTRSP)
      real      emslft(MXCOL1,MXROW1,MXSPEC)
      real      emstot(MXCOL1,MXROW1,MXSPEC)
      logical   luse, lemit(MXALCLS)
c
c  --- make sure this species is used ---
c
      luse = .FALSE.
      do icls=1,ntrcls
        if( trspmap(idx,icls) .NE. 0.  .OR.
     &                         yratmap(idx,icls) .NE. 0. ) then
          if( trspmap(idx,icls) .NE. 0. ) lemit(icls) = .TRUE. 
          luse = .TRUE.
        endif
      enddo
      if( .NOT. luse ) goto 9999
c
c   --- sum up the emissions excluding the boundary for this grid ---
c
      do 40 j=2,nrow(igrd)-1
        istrt = 2
        ifin = ncol(igrd)-1
        if(igrd .EQ. 1) then
          if(ibeg(j) .EQ. -999) goto 40
          istrt = ibeg(j)
          ifin = iend(j)
        endif
        jcrs = (j - 2)/nmesh(igrd) + j1(igrd)
        if( igrd .eq. 1 ) jcrs = j
        do 50 i=istrt,ifin
c
c   --- calculate the coarse grid offset ---
c
           icrs = (i - 2)/nmesh(igrd) + i1(igrd)
           if( igrd .eq. 1 ) icrs = i
c
c   --- skip cell if this grid has a child in this cell ---
c
           ijcl = i + (j-1)*ncol(igrd)
           if( idfin(iptr2d(igrd)-1+ijcl) .NE. 0 ) goto 50
c
c  --- get the region for this cell from mapping array,
c      the grid cell should be the coarse grid  ----
c
           imap = igrmap(igrd,i,j)
           if( imap .LE. 0 .OR. imap .GT. nregin ) goto 50
c
c  --- calculate the index into the tracer species for this gruoup/region ---
c
           if( ngroup .GT. 0 ) then
c
c   --- if group is base emissions, add to "leftover" group ----
c
              if( igroup .EQ. 0 ) then
                if( leftovr ) then
                   do icls=1,ntrcls
                     if( trspmap(idx,icls) .NE. 0. .OR.
     &                                 yratmap(idx,icls) .NE. 0. ) then
                       ipt = iemcls(icls)-1 + imap+ngroup*nregin
                       emsbas(idx,ipt) = emsbas(idx,ipt) + emsgrd(i,j)
                     endif
                   enddo
                endif
                do icls=1,ntrcls
                  if( trspmap(idx,icls) .NE. 0. .OR.
     &                                 yratmap(idx,icls) .NE. 0. ) then
                      emstot(icrs,jcrs,idx) = 
     &                               emstot(icrs,jcrs,idx) + emsgrd(i,j)
                  endif
                enddo
c
c   --- otherwise, add to this group/region and subtract from "leftover" ---
c
              else
                do icls=1,ntrcls
                   if( trspmap(idx,icls) .NE. 0. .OR. 
     &                                 yratmap(idx,icls) .NE. 0. ) then
                      ipt = iemcls(icls)-1 + imap+(igroup-1)*nregin
                      emssum(idx,ipt) = emssum(idx,ipt) + emsgrd(i,j)
                      if( leftovr ) then
                         ipt = iemcls(icls)-1 + imap+ngroup*nregin
                         emsoth(idx,ipt) = emsoth(idx,ipt) + emsgrd(i,j)
                      endif
                    endif
                enddo
                do icls=1,ntrcls
                  if( trspmap(idx,icls) .NE. 0. .OR.
     &                                 yratmap(idx,icls) .NE. 0. ) then
                    emslft(icrs,jcrs,idx) = 
     &                               emslft(icrs,jcrs,idx) + emsgrd(i,j)
                  endif
                enddo
              endif
c
c   --- only using regular model emissions ---
c
           else
              do icls=1,ntrcls
                if( trspmap(idx,icls) .NE. 0. .OR.
     &                                 yratmap(idx,icls) .NE. 0. ) then
                   ipt = iemcls(icls) - 1 + imap
                   emssum(idx,ipt) = emssum(idx,ipt) + emsgrd(i,j)
                endif
              enddo
           endif
  50    continue
  40  continue
c
 9999 continue
      return
      end

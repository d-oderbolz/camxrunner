c*** GOATSA
c
      subroutine goatsa(igrid,icell,jcell,kcell,prdo3n,prdo3v,desto3,
     &                                                    delcon,dtime)
c
c----CAMx v4.51 080522
c
c-----------------------------------------------------------------------
c   Description:
c     This routine makes the "chemistry" adjustments to the tracer 
c     species.  The adjustments are based on the differences in 
c     concentrations of the regular model species before and after
c     the regular model chemistry.  This is essentially an adjustment
c     for production or decay. In GOAT (Geographic Ozone Apportionment
c     Technology), allocation of ozone production is based solely on 
c     location.
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       igrid   I  grid number
c       icell   I  the X grid location of current cell
c       jcell   I  the X grid location of current cell
c       kcell   I  the vertical grid location of current layer
c       prdo3n  R  ozone production attributed to NOx
c       prdo3v  R  ozone production attributed to VOC
c       desto3  R  ozone destruction
c       delcon  R  array of change in each species contrntrations
c       dtime   R  change in time for current time step 
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     05/22/97   --gwilson--  Now re-calculates the sum of VOC after
c                             adjustment using wtkoh.
c     07/19/02   --gwilson--  Added seperate source area map for each grids.
c     09/20/03   --gwilson--  Changed the individual species args to
c                             a vector
c     07/20/04   --gwilson--  Changed for OSAT2
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'grid.com'
      include 'chmstry.com'
      include 'tracer.com'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer   igrid
      integer   icell
      integer   jcell
      integer   kcell
      real      prdo3n
      real      prdo3v
      real      desto3
      real      delcon(5,MXSPEC)
      real      dtime
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 idxcel, idx, imap, iregn, i
      real      sumnox, sumkoh, sumo3, delno, delno2, delvoc
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- set the local variables for the individual species concs ---
c
      delno = delcon(1,kno)
      delno2 = delcon(1,kno2)
      delvoc = 0.
      do i=1,nspec
        if( lvocsp(i) ) delvoc = delvoc + delcon(1,i) * crbnum(i)
      enddo
c
c   --- calculate the source region for this cell ---
c
      imap = igrmap(igrid,icell,jcell)
      if( imap .LE. 0 .OR. imap .GT. nregin ) goto 9999
c
c   --- calculate the index of the cell in the grid ---
c
      idxcel =  ipsa3d(igrid)-1+ icell + ncol(igrid)*(jcell-1) + 
     &                               ncol(igrid)*nrow(igrid)*(kcell-1)
c
c   --- loop over the NOx tracer species, calculate total NOx tracer ---
c
      sumnox = 0.
      do i=iptcls(idxipt(ITRNOX)),nptcls(idxipt(ITRNOX))
         idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
         sumnox = sumnox + ptconc(idx)
      enddo
c
c   ---- apply the decay to each tracer species, apportioning by 
c        contribution to total ----
c
      if( sumnox .GT. 0. ) then
         do i=iptcls(idxipt(ITRNOX)),nptcls(idxipt(ITRNOX))
             idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
             ptconc(idx) = ptconc(idx) + 
     &                    (delno + delno2) * ptconc(idx) / sumnox
             ptconc(idx) = MAX(BNDLPT,ptconc(idx))
         enddo
      endif
c
c  --- adjust the sum of NOx species to account for change ---
c
      sumnox = sumnox + (delno + delno2)
c
c   --- loop over the VOC tracer species, calculate total VOC tracer ---
c
      sumkoh = 0.
      do i=iptcls(idxipt(ITRVOC)),nptcls(idxipt(ITRVOC))
         idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
         sumkoh = sumkoh + ptconc(idx) * wtkoh(i)
      enddo
c
c   ---- apply the decay to each tracer species, apportioning by 
c        contribution to total ----
c
      if( sumkoh .GT. 0. ) then
         do i=iptcls(idxipt(ITRVOC)),nptcls(idxipt(ITRVOC))
             idx = idxcel + ncol(igrid)*nrow(igrid)*nlay(igrid)*(i-1)
             ptconc(idx) = ptconc(idx) + 
     &               delvoc * ptconc(idx) * wtkoh(i) / sumkoh
             ptconc(idx) = MAX(BNDLPT,ptconc(idx))
         enddo
      endif
c
c   --- allocate any ozone destruction across all tracers ---
c
      if( desto3 .LT. 0. ) then
         sumo3 = 0.
         do i=iptcls(idxipt(ITRO3N)),nptcls(idxipt(ITRO3V))
            idx = idxcel + ncol(igrid)*nrow(igrid)* nlay(igrid)*(i-1)
            sumo3 = sumo3 + ptconc(idx)
         enddo
         if( sumo3 .GT. 0. ) then
            do i=iptcls(idxipt(ITRO3N)),nptcls(idxipt(ITRO3V))
               idx = idxcel + ncol(igrid)*nrow(igrid)* nlay(igrid)*(i-1)
               ptconc(idx) = ptconc(idx) +
     &                                desto3 * ptconc(idx) / sumo3
               ptconc(idx) = MAX(BNDLPT,ptconc(idx))
            enddo
         endif
      endif
c
c   --- ozone production, add to local ozone tracers,
c
      delo3 = prdo3n + prdo3v
      if( delo3 .GT. 0 ) then
c
c   --- loop over all the O3N and O3V species ---
c
         do 10 i=iptcls(idxipt(ITRO3N)),nptcls(idxipt(ITRO3V))
c
c   ---- the tracer names are of the the form O3N001rrr and
c        O3V001rrr, where rrr is the region number,  This
c        will match two times, once for O3N and once for O3V ---
c
            if( ptname(i)(7:8) .EQ. 'IC' ) goto 10
            if( ptname(i)(7:8) .EQ. 'BC' ) goto 10
            read(ptname(i)(7:9),'(I3)') iregn
            if( iregn .EQ. imap ) then
               idx = idxcel + ncol(igrid)*nrow(igrid)*
     &                                             nlay(igrid)*(i-1)
               ptconc(idx) = ptconc(idx) + delo3/2.0
               ptconc(idx) = MAX(BNDLPT,ptconc(idx))
            endif
 10      continue
      endif
c
c   --- decay the deacying tracer species ---
c
      if(ntrtim .GT. 0 ) then
         do i=ipttim+1,nsaspc,2
            idx = idxcel + ncol(igrid)*nrow(igrid)* nlay(igrid)*(i-1)
            ptconc(idx) = ptconc(idx) * 
     &                                     EXP( -0.08333333 * dtime )
            ptconc(idx) = MAX(BNDLPT,ptconc(idx))
         enddo
      endif
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

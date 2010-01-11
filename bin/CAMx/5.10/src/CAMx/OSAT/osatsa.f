c*** OSATSA
c
      subroutine osatsa(numcol,numrow,numlay,igrid,icell,jcell,kcell,
     &                         prdo3n,prdo3v,desto3,nspc,delcon,dtime)
      use grid
      use chmstry
      use tracer
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c   Description:
c     This routine makes the "chemistry" adjustments to the tracer 
c     species.  The adjustments are based on the differences in 
c     concentrations of the regular model species before and after
c     the regular model chemistry.  This is essentially an adjustment
c     for production or decay.
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       numcol  I number of columns in this slice
c       numrow  I number of rows in this slice
c       numlay  I number of layers in this slice
c       igrid   I  grid number
c       icell   I  the X grid location of current cell
c       jcell   I  the X grid location of current cell
c       kcell   I  the vertical grid location of current layer
c       prdo3n  R  ozone production attributed to NOx
c       prdo3v  R  ozone production attributed to VOC
c       desto3  R  ozone destruction
c       nspc    I  number of model species
c       delcon  R  array of change in each species contrntrations
c       dtime   R  change in time for current time step 
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     05/22/97   --gwilson--  Now re-calculates the sum of VOC after
c                             adjustment using wtkoh.
c     09/20/03   --gwilson--  Changed the individual species args to
c                             a vector
c     07/20/04   --gwilson--  Changed for OSAT2
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
      integer   numcol
      integer   numrow
      integer   numlay
      integer   igrid
      integer   icell
      integer   jcell
      integer   kcell
      real      prdo3n
      real      prdo3v
      real      desto3
      integer   nspc
      real      delcon(5,MXSPEC)
      real      dtime
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   idxcel, idx, jdx, ivoc, inox, i
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
c   --- calculate the index of the cell in the grid ---
c
      idxcel =  ipsa3d(igrid)-1+ icell + numcol * (jcell-1) + 
     &                                   numcol * numrow * (kcell-1)
c
c   --- loop over the NOx tracer species, calculate total NOx tracer ---
c
      sumnox = 0.
      do i=iptcls(idxipt(ITRNOX)),nptcls(idxipt(ITRNOX))
         idx = idxcel + numcol * numrow * numlay * (i-1)
         sumnox = sumnox + ptconc(idx)
      enddo
c
c   ---- apply the decay to each tracer species, apportioning by 
c        contribution to total ----
c
      if( sumnox .GT. 0. ) then
         do i=iptcls(idxipt(ITRNOX)),nptcls(idxipt(ITRNOX))
             idx = idxcel + numcol * numrow * numlay * (i-1)
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
         idx = idxcel + numcol * numrow * numlay * (i-1)
         sumkoh = sumkoh + ptconc(idx) * wtkoh(i)
      enddo
c
c   ---- apply the decay to each tracer species, apportioning by 
c        contribution to total ----
c
      if( sumkoh .GT. 0. ) then
         do i=iptcls(idxipt(ITRVOC)),nptcls(idxipt(ITRVOC))
             idx = idxcel + numcol * numrow * numlay * (i-1)
             ptconc(idx) = ptconc(idx) + 
     &               delvoc * ptconc(idx) * wtkoh(i) / sumkoh
             ptconc(idx) = MAX(BNDLPT,ptconc(idx))
         enddo
      endif
c
c  --- loop over the VOC tracer species, calculate total mir ---
c
      summir = 0.
      do i=iptcls(idxipt(ITRVOC)),nptcls(idxipt(ITRVOC))
         idx = idxcel + numcol * numrow * numlay * (i-1)
         summir = summir + ptconc(idx) * wtmir(i)
      enddo
c
c  --- allocate any ozone destruction across all tracers ---
c
      if( desto3 .LT. 0. ) then
         sumo3 = 0.
         do i=iptcls(idxipt(ITRO3N)),nptcls(idxipt(ITRO3V))
            idx = idxcel + numcol * numrow * numlay * (i-1)
            sumo3 = sumo3 + ptconc(idx)
         enddo
         if( sumo3 .GT. 0. ) then
            do i=iptcls(idxipt(ITRO3N)),nptcls(idxipt(ITRO3V))
               idx = idxcel + numcol * numrow * numlay * (i-1)
               ptconc(idx) = ptconc(idx) + desto3 * ptconc(idx) / sumo3
               ptconc(idx) = MAX(BNDLPT,ptconc(idx))
            enddo
         endif
      endif
c
c  --- allocate VOC sensitive ozone production,
c      increase the O3V tracers ---
c
      if( prdo3v .GT. 0 ) then
         ivoc = iptcls(idxipt(ITRVOC)) - 1
         if( summir .GT. 0 ) then
            do i=iptcls(idxipt(ITRO3V)),nptcls(idxipt(ITRO3V))
                idx = idxcel + numcol * numrow * numlay * (i-1)
                ivoc = ivoc + 1
                jdx = idxcel + numcol * numrow * numlay * (ivoc-1)
                ptconc(idx) = ptconc(idx) + prdo3v *
     &                       ptconc(jdx) * wtmir(ivoc) / summir
                ptconc(idx) = MAX(BNDLPT,ptconc(idx))
            enddo
         endif
      endif
c
c  --- allocate NOx sensitive ozone production,
c      increase the O3N tracers ---
c
      if( prdo3n .GT. 0 ) then
         inox = iptcls(idxipt(ITRNOX)) - 1
         if( sumnox .NE. 0. ) then
            do i=iptcls(idxipt(ITRO3N)),nptcls(idxipt(ITRO3N))
                idx = idxcel + numcol * numrow * numlay * (i-1)
                inox = inox + 1
                jdx = idxcel + numcol * numrow * numlay * (inox-1)
                ptconc(idx) = ptconc(idx) + prdo3n *
     &                                    ptconc(jdx) / sumnox
                ptconc(idx) = MAX(BNDLPT,ptconc(idx))
            enddo
         endif
      endif
c
c   --- decay the deacying tracer species ---
c
      if(ntrtim .GT. 0 ) then
         do i=ipttim+1,nsaspc,2
            idx = idxcel + numcol * numrow * numlay * (i-1)
            ptconc(idx) = ptconc(idx) * EXP( -0.08333333 * dtime )
            ptconc(idx) = MAX(BNDLPT,ptconc(idx))
         enddo
      endif
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

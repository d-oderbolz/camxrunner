c**** GRDTRP.F
c
      function grdtrp(stnx, stny, gridin, layer, nxcel, nycel, 
     &          orgx, orgy, delx, dely, idxwst, idxest, idxsth, idxnth)          
      real*4 grdtrp
c
c-----------------------------------------------------------------------
c
c   This function finds the model output value for a monitoring station
c   by interpolating between the closest four cells.  Thus, the 
c   terminology "bottom right", "bottom left", "top right", and "top
c   left" refer to the four cells surrounding the station. 
c     Return value:
c        The interpolated model output value.
c     Argument description:
c        Inputs:
c          stnx    R  The x coordinate for the station.
c          stny    R  The y coordinate for the station.
c          gridin  R  3-dimensional matrix of model output data.        
c          nxcel   I  number of cells in X direction
c          nycel   I  number of cells in Y direction
c          orgx    R  origin of domain in X direction
c          orgy    R  origin of domain in Y direction
c          delx    R  cell width in the X direction 
c          dely    R  cell width in the Y direction 
c          idxwst  I  index of first modeled cell on West edge
c          idxest  I  index of first modeled cell on East edge
c          idxsth  I  index of first modeled cell on South edge
c          idxnth  I  index of first modeled cell on North edge
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camxtrct.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      real*4    stnx
      real*4    stny
      real*4    gridin(MXCELL,MXCELL,MXLAYR)
      integer*4 nxcel
      integer*4 nycel
      real*4    orgx
      real*4    orgy
      real*4    delx
      real*4    dely
      integer*4 idxwst(MXCELL)
      integer*4 idxest(MXCELL)
      integer*4 idxsth(MXCELL)
      integer*4 idxnth(MXCELL)
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 icell, jcell, iup, idown, jup, jdown, layer
      real*4    xcent, ycent, frcdwn, frcup, frclft, frcrgt
      real*4    dist, val1, val2, val3, val4, val12, val34
      real*4    xdown, ydown, xup, yup
      integer*4 i,j
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c   --- finds the cell containing the point ---
c
      grdtrp = -9.0
      icell = INT( (stnx - orgx)/delx ) + 1
      jcell = INT( (stny - orgy)/dely ) + 1
      if( icell .LT. 1 .OR. icell .GT. nxcel ) goto 9999
      if( jcell .LT. 1 .OR. jcell .GT. nycel ) goto 9999
c
c   --- if outside domain, return missing value ----
c
      if( idxwst(jcell) .LE. 0 .OR. icell .LT. idxwst(jcell) ) goto 9999
      if( idxest(jcell) .LE. 0 .OR. icell .GT. idxest(jcell) ) goto 9999
      if( idxsth(icell) .LE. 0 .OR. jcell .LT. idxsth(icell) ) goto 9999
      if( idxnth(icell) .LE. 0 .OR. jcell .GT. idxnth(icell) ) goto 9999
c
c   --- if the station is in an edge cell return the cell value ---
c
      if( icell .EQ. idxwst(jcell) ) then
          grdtrp = gridin(icell,jcell,layer)
          goto 9999
      endif
      if( icell .EQ. idxest(jcell) ) then
          grdtrp = gridin(icell,jcell,layer)
          goto 9999
      endif
      if( jcell .EQ. idxsth(icell) ) then
          grdtrp = gridin(icell,jcell,layer)
          goto 9999
      endif
      if( jcell .EQ. idxnth(icell) ) then
          grdtrp = gridin(icell,jcell,layer)
          goto 9999
      endif
c
c   --- find the centroid of the cell containing the point ---
c
      xcent = (FLOAT( icell ) - 0.5) *delx + orgx
      ycent = (FLOAT( jcell ) - 0.5) *dely + orgy
c
c   --- find the 4-closest cells, for example, if distance to lower 
c       left edge is more than half a cell use upper right ---
c
      dist = stnx - ((icell-1)*delx + orgx)
      if( dist .GT. 0.5*delx ) then
          idown = icell
          iup = icell + 1
      else
          idown = icell - 1
          iup = icell
      endif
      dist = stny - ((jcell-1)*dely + orgy)
      if( dist .GT. 0.5*dely ) then
          jdown = jcell
          jup = jcell + 1
      else
          jdown = jcell - 1
          jup = jcell
      endif
c
c   --- find the coordinates of the cell centroids ---
c
      xdown = (FLOAT( idown ) - 0.5) *delx + orgx
      ydown = (FLOAT( jdown ) - 0.5) *dely + orgy
      xup = (FLOAT( iup ) - 0.5) *delx + orgx
      yup = (FLOAT( jup ) - 0.5) *dely + orgy
c
c   --- find the weighting fractions for each of four cells ----
c
      frcdwn = (stny - ydown)/dely
      frcup = 1.0 - frcdwn
      frclft = (stnx - xdown)/delx
      frcrgt = 1.0 - frclft
c
c   --- interpolate to find the correct value ----
c
      val1 = gridin(idown,jdown,layer)
      val2 = gridin(idown,jup,layer)
      val3 = gridin(iup,jdown,layer)
      val4 = gridin(iup,jup,layer)
c
      val12 = val1 * frcup + val2 * frcdwn
      val34 = val3 * frcup + val4 * frcdwn
c
c   --- set retuen value and return ---
c
      grdtrp = val12 * frcrgt + val34 * frclft
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

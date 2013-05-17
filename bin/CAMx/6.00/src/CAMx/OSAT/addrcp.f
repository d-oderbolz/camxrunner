
      subroutine addrcp(igrd,nox,noy,nspsa,saconc)
      use grid
      use tracer
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c   Description:
c     This routine adds the concentrations for each receptor and to
c     the running sum of concentrations.  This data will then be
c     averaged over all time steps in an hour to get hourly
c     average concentration at the receptor locations. 
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c         igrd   I  grid number for this grid
c         nox    I  number of X cells in grid
c         noy    I  number of Y cells in grid
c         nspsa  I  number of tracer species
c         saconc R  traxcer concentrations 
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c         
c    04/02/03  --gwilson--   Added grid number to recptors defined by
c                            cell index
c    05/07/12  --cemery--    Bug fix in interpolation algorithm
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
      real      dxcell
      real      dycell
      integer   nox
      integer   noy
      integer   nspsa
      real      saconc(nox,noy,nspsa)
      integer   igrd
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   ircp, i, j, icl, jcl
      integer   ixlow, ixhigh, iylow, iyhigh
      real      xnest, ynest
      real      xlow, xhigh, ylow, yhigh, xylow, xyhigh
      real      xcelwd, ycelwd, xcentr, ycentr
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- if no receptor file is provided, just return ---
c
      if( .NOT. lrcpfil ) goto 9999
c
c  --- loop over all receptors ---
c
      do 10 ircp=1,nrecep
c
c  --- skip receptor if it is not in this grid ---
c
         if( igrdrcp(ircp) .NE. igrd ) goto 10
c
c  --- if receptor is a single cell type just add the concentrations ---
c
          if( idrcp(ircp) .EQ. IDCEL ) then
              icl = irecep(ircp,1)
              jcl = jrecep(ircp,1)
              do i=1,ntotsp
                 conrcp(i,ircp) = conrcp(i,ircp) + saconc(icl,jcl,i)
              enddo
c
c   --- if receptor is a cell average type calculate the average of 
c       each tracer concentration ---
c
          else if( idrcp(ircp) .EQ. IDAVG ) then
              do j=1,nclrcp(ircp)
                 icl = irecep(ircp,j)
                 jcl = jrecep(ircp,j)
                 do i=1,ntotsp
                     conrcp(i,ircp) = conrcp(i,ircp) + 
     &                               saconc(icl,jcl,i) / nclrcp(ircp)
                 enddo
              enddo
c
c  --- else if receptor is a point type, perform bilinear interpolation
c      at the location and add to receptor total ----
c
          else if( idrcp(ircp) .EQ. IDPNT ) then
c
c   --- calculate the cell containing the receptor ---
c
              icl = irecep(ircp,1)
              jcl = jrecep(ircp,1)
c
c   --- if the cell is on the boundary, don't do interpolation ---
c       (why would you have a receptor in a bounday cell?)
c
              if( icl .LE. 1 .OR. icl .GE. nox .OR. 
     &                           jcl .LE. 1 .OR. jcl .GE. noy ) then
                 do i=1,ntotsp
                   conrcp(i,ircp) = conrcp(i,ircp) + saconc(icl,jcl,i)
                 enddo
                 goto 10
              endif
c
c   --- find the locations of centers surrounding the receptor ---
c
              dxcell = delx / meshold(igrd)
              dycell = dely / meshold(igrd)
              xnest = xorg + FLOAT(inst1(igrd)-1)*delx - dxcell
              ynest = yorg + FLOAT(jnst1(igrd)-1)*dely - dycell
              xcentr = (FLOAT(icl)-1.5)*dxcell + xnest
              if( recepx(ircp) - xcentr .LE. dxcell ) then
                 ixlow = icl-1
                 xlow = xcentr
                 ixhigh = icl
                 xhigh =  (FLOAT(icl)-0.5)*dxcell + xnest
              else
                 ixlow = icl
                 xlow =  (FLOAT(icl)-0.5)*dxcell + xnest
                 ixhigh = icl+1
                 xhigh =  (FLOAT(icl)+0.5)*dxcell + xnest
              endif
              ycentr = (FLOAT(jcl)-1.5)*dycell + ynest
              if( recepy(ircp) - ycentr .LE. dycell ) then
                 iylow = jcl-1
                 ylow = ycentr
                 iyhigh = jcl
                 yhigh =  (FLOAT(jcl)-0.5)*dycell + ynest
              else
                 iylow = jcl
                 ylow =  (FLOAT(jcl)-0.5)*dycell + ynest
                 iyhigh = jcl+1
                 yhigh =  (FLOAT(jcl)+0.5)*dycell + ynest
              endif
              xcelwd = xhigh - xlow
              ycelwd = yhigh - ylow
              do i=1,ntotsp
c
c  --- interpolate to the X position, at both Y positions ---
c
                  xylow = (recepx(ircp) - xlow) / xcelwd * 
     &                                           saconc(ixhigh,iylow,i)
                  xylow = xylow + (xhigh - recepx(ircp)) / xcelwd * 
     &                                          saconc(ixlow,iylow,i)
                  xyhigh = (recepx(ircp) - xlow) / dxcell * 
     &                                          saconc(ixhigh,iyhigh,i)
                  xyhigh = xyhigh + (xhigh - recepx(ircp)) / 
     &                                dxcell * saconc(ixlow,iyhigh,i)
c
c  --- interpolate to the exact position using the high/low X values ----
c
                 conrcp(i,ircp) = conrcp(i,ircp) + 
     &                     (recepy(ircp) - ylow)/ycelwd * xyhigh +
     &                            (yhigh - recepy(ircp))/ycelwd * xylow
              enddo
          endif
c
c  --- next receptor ----
c
   10 continue
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

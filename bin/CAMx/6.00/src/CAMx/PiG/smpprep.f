      subroutine smpprep(lrtrac,endtim,enddate)
      use camxcom
      use grid
      use chmstry
      use pigsty
      use filunit
      use tracer
      use rtracchm
c 
c----CAMx v6.00 130506
c 
c     SMPPREP writes headers to new SAMPLING GRID output files.
c 
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c           
c     Modifications: 
c        8/2/05       Generalized for regular model species or
c                     RTRAC species.
c        1/04/11      Revised for new header format
c 
c     Input arguments: 
c        endtim              model end time (HHMM)
c        enddate             model end date (YYJJJ)
c             
c     Output arguments: 
c        none
c             
c     Routines Called: 
c        none
c             
c     Called by: 
c        STARTUP 
c
      implicit none
      include 'camx.prm'
      include 'flags.inc'
c
      integer MXOUT
      parameter (MXOUT = MAX(MXSPEC,MXTRSP))
c
      logical lrtrac
      integer enddate
      real endtim
      integer idat1,idat2,n,iunit,iutm,l,nsg,izero,ione,nav
      integer iproj
      real tim1,tim2,orgx,orgy,dx,dy,zero
      real plon,plat,t1,t2
      character*4 ifile(10),note(60)
      character*10 avfil
c
      character*4 smpspec(10,MXOUT)
c
      data avfil /'AVERAGE   '/
      data izero,ione /0,1/
      data zero /0./
c
c-----Entry point
c
      idat1 = begdate
      idat2 = enddate
      tim1 = begtim/100.
      tim2 = endtim/100.
      read(runmsg(1:60),'(60a1)') (note(n),n=1,60)
      read(avfil,'(10a1)') (ifile(n),n=1,10)
c
c-----Loop over sampling grids; prep grid and species info
c
      do nsg = 1,nsample
        iunit = isample(nsg)
        if (lrtrac) iunit = iowsmp(nsg)
c
        iutm = 0
        plon = 0.
        plat = 0.
        t1   = 0.
        t2   = 0.
        if (llatlon) then
          iproj = 0
          orgx = xorgsmp(nsg)
          orgy = yorgsmp(nsg)
          dx = delx/meshsmp(nsg)
          dy = dely/meshsmp(nsg)
        else
          orgx = 1000.*xorgsmp(nsg)
          orgy = 1000.*yorgsmp(nsg)
          dx = 1000.*delx/meshsmp(nsg)
          dy = 1000.*dely/meshsmp(nsg)
          if (lutm) then
            iproj = 1
            iutm = iuzon
          elseif (lambrt) then
            iproj = 2
            plon = xlonc
            plat = ylatc
            t1   = tlat1 
            t2   = tlat2
          elseif (lpolar) then
            iproj = 3
            plon = polelon
            plat = polelat
          endif
        endif
c
        if (lrtrac) then
          nav = nrtrac
          do l = 1,nrtrac
            read(ptname(l),'(10a1)') (smpspec(n,l),n=1,10)
          enddo
        else
          nav = navspc
          do l = 1,navspc
            read(spname(lavmap(l)),'(10a1)') (smpspec(n,l),n=1,10)
          enddo
        endif
c
c-----Write header
c
        rewind(iunit)
        write(iunit) ifile,note,itzon,nav,idat1,tim1,idat2,tim2
        write(iunit) plon,plat,iutm,orgx,orgy,dx,dy,ncolsmp(nsg),
     &               nrowsmp(nsg),ione,iproj,izero,t1,t2,zero
        write(iunit) ione,ione,ncolsmp(nsg),nrowsmp(nsg)
        write(iunit) ((smpspec(n,l),n=1,10),l=1,nav)
      enddo
c
      return
      end

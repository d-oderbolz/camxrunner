      subroutine depprep(endtim,enddate)
      use camxcom
      use camxfld
      use filunit
      use grid
      use chmstry
c 
c----CAMx v6.00 130506
c 
c     DEPPREP generates new deposition output species names and writes 
c     headers to new DEPOSITION output files.
c 
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c           
c     Modifications: 
c        8/25/06   Dep output files now all UAM format, one file per grid
c        1/04/11   Revised for new header format
c 
c     Input arguments: 
c        endtim              model end time (HHMM)
c        enddate             model end date (YYJJJ)
c             
c     Output arguments: 
c        none
c             
c     Routines Called: 
c        ISTRLN
c             
c     Called by: 
c        STARTUP 
c
      include 'camx.prm'
      include 'flags.inc'
c
      character*4 ifile(10),note(60)
      integer enddate
      character*10 avfil,tmpnam
c
      character*4 dpspec(10,4*MXSPEC)
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
      iutm = 0
      plon = 0.
      plat = 0.
      t1   = 0.
      t2   = 0.
      if (llatlon) then
        iproj = 0
        orgx = xorg
        orgy = yorg
        dx = delx
        dy = dely
      else
        orgx = 1000.*xorg
        orgy = 1000.*yorg
        dx = 1000.*delx
        dy = 1000.*dely
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
      read(runmsg(1:60),'(60a1)') (note(n),n=1,60)
      read(avfil,'(10a1)') (ifile(n),n=1,10)
      do l = 1,ndepspc
        if( ldepmap(l) .GT. 0 ) then
           tmpnam = spname(ldepmap(l))
           ll = istrln(tmpnam)
           tmpnam(ll+1:ll+3) = '_DV'
           depsp(l) = tmpnam
           read(tmpnam,'(10a1)') (dpspec(n,l),n=1,10)
           tmpnam(ll+1:ll+3) = '_DD'
           depsp(ndepspc+l) = tmpnam
           read(tmpnam,'(10a1)') (dpspec(n,ndepspc+l),n=1,10)
           tmpnam(ll+1:ll+3) = '_WD'
           depsp(2*ndepspc+l) = tmpnam
           read(tmpnam,'(10a1)') (dpspec(n,2*ndepspc+l),n=1,10)
           tmpnam(ll+1:ll+3) = '_LC'
           depsp(3*ndepspc+l) = tmpnam
           read(tmpnam,'(10a1)') (dpspec(n,3*ndepspc+l),n=1,10)
        endif
      enddo
c
c-----Master grid header
c
      write(idep(1)) ifile,note,itzon,4*ndepspc,idat1,tim1,idat2,tim2
      write(idep(1)) plon,plat,iutm,orgx,orgy,dx,dy,ncol(1),nrow(1),
     &               ione,iproj,izero,t1,t2,zero
      write(idep(1)) ione,ione,ncol(1),nrow(1)
      write(idep(1)) ((dpspec(n,l),n=1,10),l=1,4*ndepspc)
      if( ngrid .EQ. 1 ) goto 9999
c
c-----Fine grid header
c
      do i = 2,ngrid
        dxf   = dx/float(meshold(i))
        dyf   = dy/float(meshold(i))
        orgxf = orgx + dx*(inst1(i)-1) - dxf
        orgyf = orgy + dy*(jnst1(i)-1) - dyf
        write(idep(i)) ifile,note,itzon,4*ndepspc,idat1,tim1,idat2,tim2
        write(idep(i)) plon,plat,iutm,orgxf,orgyf,dxf,dyf,ncol(i),
     &                 nrow(i),ione,iproj,izero,t1,t2,zero
        write(idep(i)) ione,ione,ncol(i),nrow(i)
        write(idep(i)) ((dpspec(n,l),n=1,10),l=1,4*ndepspc)
      enddo
c
 9999 continue
      return
      end

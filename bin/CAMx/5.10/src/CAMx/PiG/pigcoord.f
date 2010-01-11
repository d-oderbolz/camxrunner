C**** PIGCOORD
c
      subroutine pigcoord(xpig,ypig,iipig,jjpig,ingrd)
      use grid
      use bndary
      implicit none
c
c----CAMx v5.10 090918
c
c     PIGCOORD calculates the (I,J) and grid index location for a given
c     puff (X,Y) coordinate
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Modifications:
c        none
c
c     Input arguments:
c        xpig                x-coord of puff (km or deg)
c        ypig                y-coord of puff (km or deg)
c
c     Output arguments:
c        iipig               i-coord of puff
c        jjpig               j-coord of puff
c        ingrd               grid index containing puff coords
c
c     Subroutines Called:
c        none
c
c     Called by:
c        AVEPIG
c        PIGWALK
c        WALK1PUF
c        PIGDRIVE
c
      include "camx.prm"
c
      integer i,j,ingrd,ip,ic,igrd,ig,iipig,jjpig
      real xtmp,ytmp,xpig,ypig
c
c-----Entry point
c
      i = 1 + INT(xpig/delx)
      j = 1 + INT(ypig/dely)
c
      ingrd = 1
      do ip = 1,ngrid
        do ic = 1,nchdrn(ip)
          igrd = idchdrn(ic,ip)
          ig = mapgrd(igrd)
          if (i.ge.inst1(ig) .and. i.le.inst2(ig) .and.
     &        j.ge.jnst1(ig) .and. j.le.jnst2(ig))
     &      ingrd = igrd
        enddo
      enddo
c
      igrd = ingrd
      if (igrd.eq.1) then
        iipig = i
        jjpig = j
        if (j.le.1 .or. j.ge.nrow(1) .or. 
     &                      i.le.1 .or. i.ge.ncol(1)) ingrd = 0
      else
        xtmp = xpig - (inst1(igrd) - 1)*delx
        ytmp = ypig - (jnst1(igrd) - 1)*dely
        iipig = 2 + INT(xtmp/delx*FLOAT(meshold(igrd)))
        jjpig = 2 + INT(ytmp/dely*FLOAT(meshold(igrd)))
      endif
c
      return
      end

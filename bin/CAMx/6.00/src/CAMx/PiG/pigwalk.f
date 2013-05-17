      subroutine pigwalk(dt) 
      use grid
      use filunit
      use camxfld
      use pigsty
      implicit none
c
c----CAMx v6.00 130506
c
c     PIGWALK transports PiG puffs for the duration of one input timestep
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Modifications:
c        9/5/03              Revised to transport puff ends in "chained"
c                            puff approach
c
c     Input arguments:
c        dt                  time step (s)     
c
c     Output arguments:
c        none
c
c     Subroutines called:
c        PIGCOORD
c        WALK1PUF
c
c     Called by:
c        EMISTRNS
c
      include "camx.prm"
c
      real dt,dtcell,xdist,ydist,xlold,xlnew,xpig,ypig,dwdz,sigzold,
     &     axiszold,axisyold,dzbot,dztop,aa,bb,cc,
     &     zfactor,ztop
      integer n,kount,igrd,iipig,jjpig,ingrdf,ingrdb
      integer nflip
      logical lkick
c
c-----Entry point
c
c-----Loop over all PiGs; find active puffs to move
c
      do 20 n = 1,npig
        if (ingrd(n).eq.0) goto 20
c
        xpig = (xpigf(n) + xpigb(n))/2.
        ypig = (ypigf(n) + ypigb(n))/2.
        call pigcoord(xpig,ypig,iipig,jjpig,igrd)
        xdist = (xpigf(n) - xpigb(n))/
     &          delx*deltax(jjpig,igrd)*meshold(igrd)
        ydist = (ypigf(n) - ypigb(n))/
     &          dely*deltay(igrd)*meshold(igrd)
        xlold = sqrt(xdist**2 + ydist**2)
c
c-----Walk the leading (front) edge
c     New born PiGs walk for the duration of their age;
c     Older puffs walk for the duration of the current timestep
c
        call pigcoord(xpigf(n),ypigf(n),iipig,jjpig,ingrdf)
c
        if (lnewt(n)) then
          dtcell = agepigf(n)
        else
          dtcell = dt
          agepigf(n) = agepigf(n) + dt
        endif
c
        kount = 0
        nflip = 0
        lkick = .false.
  10    kount = kount + 1
        igrd = ingrdf
        call walk1puf(dtcell,igrd,ncol(igrd),nrow(igrd),nlay(igrd),
     &                iipig,jjpig,ingrdf,lkick,xpigf(n),ypigf(n),
     &                pufftop(n),puffbot(n),windu(iptr3d(igrd)),
     &                windv(iptr3d(igrd)),height(iptr3d(igrd)),
     &                press(iptr3d(igrd)),tempk(iptr3d(igrd)))
        if (igrd.ne.ingrdf) nflip = nflip + 1
        if (kount.gt.50 .and. dtcell.gt.0.) then
          if (nflip.ge.10) then
            nflip = 0
            lkick = .true.
            goto 10
          endif
          write(iout,'(//,a)') 'ERROR in PIGWALK:'
          write(iout,*) 'Number of steps > 50'
          write(iout,*) 'Front edge: Puff#,grid#,timestep left:'
          write(iout,*) n,ingrdf,dtcell
          write(iout,*) 'Location (i,j), puff top & bottom'
          write(iout,*) iipig,jjpig,pufftop(n),puffbot(n)
          call camxerr()
        endif
        if (dtcell.gt.0.) goto 10
        if (ingrdf.eq.0) then
          ingrd(n) = 0
          goto 20
        endif
c
c-----Walk the trailing (back) edge
c
        call pigcoord(xpigb(n),ypigb(n),iipig,jjpig,ingrdb)
c
        if (lnewt(n)) then
          dtcell = agepigb(n)
        else
          dtcell = dt
          agepigb(n) = agepigb(n) + dt
        endif
        if (dtcell.eq.0.) goto 30
c  
        kount = 0
        nflip = 0
        lkick = .false.
  11    kount = kount + 1
        igrd = ingrdb
        call walk1puf(dtcell,igrd,ncol(igrd),nrow(igrd),nlay(igrd),
     &                iipig,jjpig,ingrdb,lkick,xpigb(n),ypigb(n),
     &                pufftop(n),puffbot(n),windu(iptr3d(igrd)),
     &                windv(iptr3d(igrd)),height(iptr3d(igrd)),
     &                press(iptr3d(igrd)),tempk(iptr3d(igrd)))
        if (igrd.ne.ingrdb) nflip = nflip + 1
        if (kount.gt.50 .and. dtcell.gt.0.) then
          if (nflip.ge.10) then
            nflip = 0
            lkick = .true.
            goto 11
          endif
          write(iout,'(//,a)') 'ERROR in PIGWALK:'
          write(iout,*) 'Number of steps > 50'
          write(iout,*) 'Back edge: Puff#,grid#,timestep left:'
          write(iout,*) n,ingrdb,dtcell
          write(iout,*) 'Location (i,j), puff top & bottom'
          write(iout,*) iipig,jjpig,pufftop(n),puffbot(n)
          write(iout,*) 
          call camxerr()
        endif
        if (dtcell.gt.0.) goto 11
        if (ingrdb.eq.0) then
          ingrd(n) = 0
          goto 20
        endif
c
c-----Determine new puff center point coords, host grid, and puff length
c
 30     xpig = (xpigf(n) + xpigb(n))/2.
        ypig = (ypigf(n) + ypigb(n))/2.
        call pigcoord(xpig,ypig,iipig,jjpig,ingrd(n))
        igrd = ingrd(n)
        xdist = (xpigf(n) - xpigb(n))/
     &          delx*deltax(jjpig,igrd)*meshold(igrd)
        ydist = (ypigf(n) - ypigb(n))/
     &          dely*deltay(igrd)*meshold(igrd)
        xlnew = sqrt(xdist**2 + ydist**2)
        if (lnewt(n)) then
          lnewt(n) = .false.
          goto 20
        endif
c
c-----Calculate dw/dz through puff depth and adjust puff dimensions to
c     conserve volume
c
        call getdwdz(ncol(igrd),nrow(igrd),nlay(igrd),iipig,jjpig,
     &               pufftop(n),puffbot(n),deltax(jjpig,igrd),
     &               deltay(igrd),height(iptr3d(igrd)),
     &               press(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &               windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &               ztop,dwdz)
c
        dtcell = (agepigf(n) - agepigb(n))
        sigzold = sigz(n)
        axiszold = axisz(n)
        axisyold = axisy(n)
        zfactor = sqrt(amax1(0.1,1.+2.*dwdz*dtcell))

        sigz(n) = sigz(n)*zfactor
        dzbot = (zpig(n) - puffbot(n))*sigz(n)/sigzold
        dztop = (pufftop(n) - zpig(n))*sigz(n)/sigzold
        puffbot(n) = amax1(0.,  zpig(n) - dzbot)
        pufftop(n) = amin1(ztop,zpig(n) + dztop)
        zpig(n) = (pufftop(n) + puffbot(n))/2.
        axisz(n) = pufftop(n) - puffbot(n)
c
        aa = axisz(n)
        bb = axisz(n)*xlnew
        cc = -axisyold*axiszold*(xlold + axisyold)
        axisy(n) = (-bb + sqrt(bb*bb - 4.*aa*cc))/(2.*aa)
        sigy(n) = sigy(n)*axisy(n)/axisyold
  20  continue
c
      return
      end
c
c-------------------------------------------------------------------------------
c
      subroutine getdwdz(ncol,nrow,nlay,i,j,pufftop,puffbot,dx,dy,
     &                   height,press,tempk,windu,windv,ztop,dwdz)
c
c-----GETDWDZ calculates the bulk vertical wind shear across puff depth
c     
      implicit none
      include "camx.prm"
      integer ncol,nrow,nlay,i,j
      real pufftop,puffbot,dx,dy,height(ncol,nrow,nlay),
     &     press(ncol,nrow,nlay),tempk(ncol,nrow,nlay),
     &     windu(ncol,nrow,nlay),windv(ncol,nrow,nlay)
      integer kpb,kpt,kk
      real deplyr,sumwt,uup,uum,vvp,vvm,dwdz,ztop
      real wtfac(MXLAYER)
c 
c-----Entry point
c
      ztop = height(i,j,nlay)
c
c-----Find layers containing top and bottom of puff
c
      kpb = 1
      kpt = nlay
      do kk = 1,nlay
        if (height(i,j,kk) .GE. pufftop) then
          kpt = kk
          goto 26
        endif
      enddo
  26  continue
      do kk = nlay,1,-1
        if (height(i,j,kk) .LE. puffbot) then
          kpb = kk + 1
          goto 27
        endif
      enddo
 27   continue
c
c-----Determine layer density-weighted u,v wind components over puff depth
c
      if (kpb.eq.kpt) then
        wtfac(kpb) = 1.
      else
        sumwt = 0.
        deplyr = pufftop - height(i,j,kpt-1)
        wtfac(kpt) = deplyr*press(i,j,kpt)/tempk(i,j,kpt)
        sumwt = sumwt + wtfac(kpt)
c
        deplyr = height(i,j,kpb) - puffbot
        wtfac(kpb) = deplyr*press(i,j,kpb)/tempk(i,j,kpb)
        sumwt = sumwt + wtfac(kpb)
c
        do kk = kpb+1,kpt-1
          deplyr = height(i,j,kk) - height(i,j,kk-1)
          wtfac(kk) = deplyr*press(i,j,kk)/tempk(i,j,kk)
          sumwt = sumwt + wtfac(kk)
        enddo
c
        do kk = kpb,kpt
          wtfac(kk) = wtfac(kk)/sumwt
        enddo
      endif
c
      uup = 0.
      uum = 0.
      vvp = 0.
      vvm = 0.
      do kk = kpb,kpt
        uup = uup + wtfac(kk)*windu(i,j,kk)
        uum = uum + wtfac(kk)*windu(i-1,j,kk)
        vvp = vvp + wtfac(kk)*windv(i,j,kk)
        vvm = vvm + wtfac(kk)*windv(i,j-1,kk)
      enddo
      dwdz = -(uup - uum)/dx - (vvp - vvm)/dy
c
      return
      end

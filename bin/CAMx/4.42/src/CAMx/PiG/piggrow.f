      subroutine piggrow(n,nz,kpuff,dt,xlen,areamx,ztop,wind,temp1,
     &                   temp0,press1,press0,rkv,depth,z0,hshear,
     &                   vshear,delms,ldump)
c
c----CAMx v4.42 070603
c
c     PIGGROW performs the following functions:
c       (1) grows puffs
c       (2) calculates the mass to dump if horizontal growth restricted
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Modifications:
c        1/10/06         Small modification to vertical puff growth limiting
c                        in stable conditions
c        02/02/06        Renamed PIGGROW from IRONGROW
c
c     Input arguments:
c        n                   puff index
c        nz                  number of grid layers
c        kpuff               layer index in which puff resides
c        dt                  time step (s)
c        xlen                puff length (m)
c        areamx              maximum horizontal area allowed (m2)
c        ztop                model top height (m)
c        wind                layer 1 wind (m/s)
c        temp1               layer 1 temperature (K)
c        temp0               surface temperature (K)
c        press1              layer 1 pressure (mb)
c        press0              surface pressure (mb)
c        temp0               surface temperature (K)
c        rkv                 vertical diffusivity profile (m2/s)
c        depth               layer depth profile (m)
c        z0                  surface roughness (m)
c        hshear              grid-resolved horz shear of horz wind (1/s)
c        vshear              grid-resolved vert shear of horz wind (1/s)
c
c     Output arguments:
c        delms               mass fraction to dump for all species 
c        ldump               switch is set on if leakage has occurred
c
c     Subroutines called:
c        MICROMET
c        SIGGROW
c
c     Called by:
c        PIGDRIVE
c 
      implicit none
      include "camx.prm"
      include "pigsty.com"
      include "filunit.com"
c
      logical ldump,lstable
      integer n,nz,k,kpbl,kpuff
      real dt,areamx,wind,delms,ztop,temp1,temp0,press1,press0,
     &     hshear,vshear
      real rkv(nz),depth(nz),z0

      real sigzold,sigyold,fmsold,alimit,relsiz,
     &     pbl,zf,critk,ustar,wstar,psih,rkvavg,xlen,
     &     axisx,pufscl,pi,aa,bb,cc,dzbot,dztop
c
      data pi /3.1415927/
c
c-----Entry point
c
c-----Check for phantom puff
c
      if (fmspig(n).le.0.) then
        ingrd(n) = 0
        return
      endif
c
      delms   = 0.
      fmsold  = fmspig(n)
      sigyold = sigy(n)
      sigzold = sigz(n)
c
c-----Get the PBL parameters
c
      kpbl = 1
      pbl = depth(1)
      do k = 2,nz
        if (kpbl.ne.k-1) goto 102
        zf = depth(k)/depth(k-1)
        critk = 0.03*depth(k-1)*depth(k-1)*(1. + zf)/200.
        if (rkv(k-1).gt.critk) then
          kpbl = k
          pbl = pbl + depth(k)
        endif
      enddo
 102  continue

      call micromet(temp1,temp0,press1,press0,depth(1),wind,z0,pbl,
     &              ustar,psih,wstar,lstable)
c
c-----Grow puff spread
c
      rkvavg = rkv(kpuff) 
      if (kpuff.gt.1) rkvavg = (rkv(kpuff) + rkv(kpuff-1))/2.
      call siggrow(dt,zpig(n),pbl,ustar,wstar,lstable,rkvavg,
     &             htfms(n),htfmb(n),vtfms(n),vtfmb(n),hshear,vshear,
     &             sigy(n),sigz(n))
c
c-----Set vertical extent of puff; limit puff bottom to zero (ground),
c     and limit puff top to model top or to zero growth if puff extends
c     above PBL depth
c
      dzbot = (zpig(n) - puffbot(n))*sigz(n)/sigzold
      dztop = (pufftop(n) - zpig(n))*sigz(n)/sigzold

      puffbot(n) = amax1(0.,zpig(n) - dzbot)
      if (lstable .or. kpbl.eq.1 .or. pufftop(n).lt.pbl) then
        pufftop(n) = zpig(n) + dztop
        if (.not.lstable .and. kpbl.gt.1 .and. pufftop(n).gt.pbl)
     &     pufftop(n) = pbl
      endif
      pufftop(n) = amin1(ztop,pufftop(n))
      zpig(n) = (pufftop(n) + puffbot(n))/2.
      axisz(n) = pufftop(n) - puffbot(n)
      axisy(n) = axisy(n)*sigy(n)/sigyold
c
c-----Skip dumping for new puffs
c
      if (lnewg(n)) goto 900
c
c-----Limit puff horizontal dimensions by FLEAK*AREAMX
c     Dump excess fraction due to expansion in this time step
c     (FLEAK is set in the .prm file)
c
      alimit = FLEAK*areamx
      axisx = xlen + 3.*sigy(n)
      pufscl = pi*(axisx/2.)*(1.5*sigy(n))
      if (LEAKON .and. pufscl.gt.alimit) then
        relsiz = alimit/pufscl
        fmspig(n) = amin1(fmsold,relsiz)
      endif
c
c-----Artificially shrink puffs older than AGEMAX so that 10% of puff mass
c     is released each timestep
c
      if (agepigf(n).ge.AGEMAX) then
        fmspig(n) = amax1(0.,fmsold - 0.1)
      endif
c
c-----Get puff mass fraction lost and recalculate the reactor width parameter
c     AXISY to correct reactor volume and conserve concentration 
c
      delms = (fmsold - fmspig(n))/fmsold
      if (delms.gt.1.e-5) then
        aa = 1.
        bb = xlen
        cc = -(xlen*axisy(n) + axisy(n)*axisy(n))*(1. - delms)
        axisy(n) = (-bb + sqrt(bb*bb - 4.*aa*cc))/(2.*aa)
        ldump = .true.
      endif
c
c-----Check for errors
c
 900  if (delms.lt.0.) then
        write(iout,'(//,a)') 'ERROR in PIGGROW:'
        write(iout,*) 'Negative puff leakage:'
        write(iout,*) 'Puff#,axis_y/z:'
        write(iout,*)  n,axisy(n),axisz(n)
        write(iout,*) 'New and old mass fraction in puff:'
        write(iout,*)  fmspig(n),fmsold
        call camxerr()
      endif
c
      return
      end

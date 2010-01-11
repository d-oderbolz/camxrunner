      subroutine piginit(dt,pttrace,height,windu,windv,tempk,press)
c
c----CAMx v4.42 070603
c
c     PIGINIT initializes PiG puffs from flagged sources
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c          
c     Modifications: 
c        07/05/02    Added code for IRON-PiG
c        06/24/03    Removed thetapig
c        09/04/03    Removed adiabatic factor; improved plume rise
c                    entrainment; revamped puff initiation for "chained" puffs
c        11/12/03    Added RTRAC species injection
c        08/25/05    Added new PiG-OSAT pointers to allow different input
c                    point source lists across model restarts
c        02/02/06    Removed GREASD-PiG specific conditional code
c        12/20/06    Added plume height overrided
c
c     Input arguments:
c        dt                  time step size (s)
c        pttrace             point source emissions (mol/s)
c        height              layer height (m)
c        windu               wind speed in x-direction (m/s)
c        windv               wind speed in y-direction (m/s)
c        tempk               air temprature (deg.K)
c        press               air pressure (mb)
c
c     Output arguments:
c        none
c
c     Subroutines Called:
c        PLUMERIS
c
c     Called By:
c        EMISTRNS
c
      implicit none
      include  "camx.prm"
      include  "grid.com"
      include  "ptemiss.com"
      include  "pigsty.com"
      include  "chmstry.com"
      include  "filunit.com"
c
c======================== Source Apportion Begin =======================
c
      include  "tracer.com"
      include  "rtracchm.com"
c
c========================= Source Apportion End ========================
c
      real height(MXVEC3D),windu(MXVEC3D),
     &     windv(MXVEC3D),tempk(MXVEC3D),
     &     press(MXVEC3D),pttrace(MXPTSRC,MXSPEC)
      real hght1d(MXLAYA),wind1d(MXLAYA),tempk1d(MXLAYA),
     &     dtdz1d(MXLAYA),press1d(MXLAYA)
      real frctr(MXRECTR)
      real dt,gamma,p0,t1,t2,frsum,xtmp,ytmp,w2,dz,
     &     dtheta,dstktmp,zstk,dtpuff,dtime,grav,wp,tp,zrise,
     &     trise,sigma,pwidth,rip,fp,qp2,cq1,cq2,rkp,xlmax,
     &     tstktmp
      integer nr,lpt,m1,lsrc,n,i,j,k,igrd0,ip,ic,igrd,ig,
     &        ii,jj,kk,n3d,npuff,ipuff,m,m0,is,iempty
      real xerf
c
      data gamma /0.286/
      data p0 /1000./
      data grav /9.8/
      data cq1,cq2 /0.4,3.0/
c
c-----Entry point
c
c-----Divide initial mass among PiG reactors: (1) partition according to 
c     ERF function; (2) correct for truncation of xerf to conserve mass
c
      frsum = 0.
      do nr = 1,nreactr
        t1 = nr*sqrt(2.)/nreactr
        t2 = (nr-1)*sqrt(2.)/nreactr
        frctr(nr) = xerf(t1) - xerf(t2)
        frsum = frsum + frctr(nr)
      enddo
      do nr = 1,nreactr
        frctr(nr) = frctr(nr)/frsum
      enddo
c
c-----Loop over all elevated point sources in domain (master grid)
c
      m1 = 1
      do 50 lsrc = 1,nosrc(1)
        n = idsrc(lsrc,1)
        if (.not.lpiglet(n)) goto 50
        iempty = 0
        do lpt = 1,nptspc
          if (lptmap(lpt).gt.0 .and. pttrace(n,lpt).gt.0.) iempty = 1
        enddo
c
c======================== Source Apportion Begin =======================
c
        if( ltrace .AND. tectyp .EQ. RTRAC ) then
           do is = 1,nrtrac
             if (sapnts(n,is).gt.0.) iempty = 1
           enddo
        endif
c
c========================= Source Apportion End ========================
c
        if (iempty.eq.0) goto 50
c
c-----Found a PiG point source with non-zero emissions; load local 1-D
c     vectors from finest grid
c
        i = isrc(lsrc,1)
        j = jsrc(lsrc,1)
        igrd0 = 1
        do ip = 1,ngrid
          do ic = 1,nchdrn(ip)
            igrd = idchdrn(ic,ip)
            ig = mapgrd(igrd)
            if (i.ge.inst1(ig) .and. i.le.inst2(ig) .and.             
     &          j.ge.jnst1(ig) .and. j.le.jnst2(ig)) 
     &      igrd0 = igrd
          enddo
        enddo

        if (igrd0.eq.1) then
          ii = i
          jj = j
        else
          xtmp = xstk(n,1) - (inst1(igrd0) - 1)*delx
          ytmp = ystk(n,1) - (jnst1(igrd0) - 1)*dely
          ii = 2 + INT(xtmp/delx*meshold(igrd0))
          jj = 2 + INT(ytmp/dely*meshold(igrd0))
        endif

        do k = 1,nlay(igrd0)
          n3d = ii + ncol(igrd0)*(jj - 1) + 
     &          ncol(igrd0)*nrow(igrd0)*(k - 1)
          hght1d(k) = height(iptr3d(igrd0)-1+n3d)
          tempk1d(k) = tempk(iptr3d(igrd0)-1+n3d)
          press1d(k) = press(iptr3d(igrd0)-1+n3d)
          w2 = windu(iptr3d(igrd0)-1+n3d)*windu(iptr3d(igrd0)-1+n3d) +
     &         windv(iptr3d(igrd0)-1+n3d)*windv(iptr3d(igrd0)-1+n3d)
          wind1d(k) = amax1(sqrt(w2),0.1) 
        enddo
        do k = 1,nlay(igrd0)
          if (k.lt.nlay(igrd0)) then 
            dz = hght1d(k+1)/2.  
            if (k.gt.1) dz = (hght1d(k+1) - hght1d(k-1))/2.  
            dtheta = (tempk1d(k+1)*(p0/press1d(k+1))**gamma -  
     &                tempk1d(k)*(p0/press1d(k))**gamma)  
            dtdz1d(k) = dtheta/dz  
          else
            dtdz1d(k) = dtdz1d(k-1)  
          endif  
        enddo
c
c-----Calculate plume rise
c
        dstktmp = abs(dstk(n))
        if (effph(n) .lt. 0.) then
          zstk = abs(effph(n))
        else
          call plumeris(nlay(igrd0),hght1d,tempk1d,dtdz1d,wind1d,
     &                  hstk(n),dstktmp,tstk(n),vstk(n),zstk) 
        endif
        do kk = 1,nlay(igrd0)
          if (hght1d(kk).gt.zstk) goto 15
        enddo
        kk = nlay(igrd0)
        if (zstk.gt.hght1d(kk)) zstk = hght1d(kk)
  15    continue
c
c-----Divide the emissions into several puffs if the length exceeds half
c     the finest grid cell size
c
        xlmax = 1.e6
        do ip = 1,ngrid
          xlmax = amin1(xlmax,deltay(ip)/2.)
        enddo
        npuff = int(wind1d(kk)*dt/xlmax - 0.1) + 1
        dtpuff = dt/float(npuff)
        dtime = dt
        do 40 ipuff = 1,npuff
          dtime = dtime - dtpuff
c
c-----Calculate initial cross-puff sigma accounting for turbulent 
c     entrainment during release
c
          wp = amax1(0.1,vstk(n)/2.)
          tstktmp = amax1(tstk(n),tempk1d(kk)+1.)
          tp = (tempk1d(kk) + tstktmp)/2.
          zrise = zstk - hstk(n)
          trise = zrise/wp
          pwidth = sqrt(2.)*dstktmp
          sigma =  pwidth
          rip = grav*pwidth*abs(tp - tempk1d(kk))/(tempk1d(kk)*wp*wp)
          fp = 1. + 4.*rip
          qp2 = fp*wp*wp*(cq1 + cq2*wind1d(kk)*wind1d(kk)/
     &          (wind1d(kk)*wind1d(kk) + wp*wp))
          rkp = 0.15*pwidth*sqrt(qp2)
          sigma = sqrt(sigma*sigma + 2.*rkp*trise)
c
c-----Looking for a spot for the pig; add to the first empty location
c
          do m = m1,npig
            if (ingrd(m).eq.0) then
              m0 = m
              m1 = m0 + 1
              goto 20
            endif
          enddo
          npig = npig + 1
          m0 = npig
          m1 = npig + 1
  20      continue
          if (m0.gt.MXPIG) then
            write(iout,'(//,a)') 'ERROR in PIGINIT:'
            write(iout,*) 'PiG number exceeds maximum of ',MXPIG
            write(iout,*) 'Increase MXPIG and recompile'
            call camxerr()
          endif
c
c-----Load puff variables
c
          lnewt(m0) = .true.
          lnewg(m0) = .true.
          idpig(m0) = n
          ingrd(m0) = igrd0
          xpigf(m0) = xstk(n,1)
          xpigb(m0) = xstk(n,1)
          ypigf(m0) = ystk(n,1)
          ypigb(m0) = ystk(n,1)
          zpig(m0)  = zstk
          sigz(m0)  = amin1(sigma,amax1(zrise,1.))
          sigy(m0)  = sigma
          agepigf(m0) = dtime + dtpuff
          agepigb(m0) = dtime
          htfms(m0) = 0.
          htfmb(m0) = 0.
          vtfms(m0) = 0.
          vtfmb(m0) = 0.
c
c======================== Source Apportion Begin =======================
c
          if( ltrace .AND. tectyp .NE. RTRAC ) then
             ipufmap(m0) = ipigmap(n)
             ipufgrp(m0) = ipiggrp(n)
          endif
c
c========================= Source Apportion End ========================
c
c
c-----Calculate 3-D puff dimensions and fill with concentrations
c
          axisz(m0) = 3.*sigz(m0)
          axisy(m0) = 3.*sigy(m0)
          fmspig(m0) = 1.
          pufftop(m0) = zstk + 1.5*sigz(m0)
          puffbot(m0) = zstk - 1.5*sigz(m0)
          if (puffbot(m0).lt.0.) then
            axisz(m0) = axisz(m0) + puffbot(m0)
            puffbot(m0) = 0.
          endif
          if (pufftop(m0).gt.hght1d(nlay(igrd0))) then
            axisz(m0) = axisz(m0) - 
     &                  (pufftop(m0) - hght1d(nlay(igrd0)))
            pufftop(m0) = hght1d(nlay(igrd0))
          endif
c
c-----Initialize all PiG concentrations to zero, then fill emissions
c     and initialize radicals
c
          do nr = 1,nreactr
            do is = 1,nspec
              puffmass(is,nr,m0) = 0.
            enddo
            do lpt = 1,nptspc
              is = lptmap(lpt)
              if( is .GT. 0 )
     &           puffmass(is,nr,m0) = pttrace(n,lpt)*
     &                                dtpuff*1.e6*frctr(nr)
            enddo
            do is = 1,MXRADCL
              puffrad(is,nr,m0) = 1.e-9
            enddo
c
c======================== Source Apportion Begin =======================
c
            if( ltrace .AND. tectyp .EQ. RTRAC ) then
               do is = 1,nrtrac
                 puffrt(is,nr,m0) = sapnts(n,is)*
     &                              dtpuff*1.e6*frctr(nr)
               enddo
            endif
c
c========================= Source Apportion End ========================
c
          enddo

  40    continue
  50  continue
c
      return
      end

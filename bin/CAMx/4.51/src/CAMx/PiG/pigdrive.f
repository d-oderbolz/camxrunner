      subroutine pigdrive(igrd,iptr2d,ncol,nrow,nlay,itzon,dt,dx,dy,
     &                    delx,dely,meshfac,mapscl,height,rkv,tempk,
     &                    tsurf,press,water,windu,windv,cldtrns,fcloud,
     &                    cwc,pwr,pws,pwg,cph,cellat,cellon,fsurf,vdep,
     &                    conc,cncrad,pigdump,pgmserr,fluxes,depfld,
     &                    ipsa3d)
c
c----CAMx v4.51 080522
c
c     PIGDRIVE is the driver program for the PiG submodel.
c     It performs chemistry, puff growth, and mass dumping.
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c     Modifications:
c        02/02/06        Renamed PIGDRIVE from IRONDRIV
c        8/31/06         Added map scale factor
c        01/08/07        Added Stage 3 criteria for Mech6 (CB05)
c                        Added ETOH,MTBE,MBUT rxns to Stage 3 criteria for Mech5
c        06/22/07        Removed termination check for puffs extending to
c                        surface layer
c        02/21/08        Added coupling time for puff aerosol chemistry
c        03/10/08        Added avgcnc and acgrad to calls to IRONCHEM
c        03/31/08        Added average puff age when killed
c        05/14/08        Improved handling of puff PM increments
c
c     Input arguments:
c        igrd                grid index 
c        iptr2d              pointers into vectors for 2-D fields
c        ncol                number of columns
c        nrow                number of rows
c        nlay                number of layers
c        itzon               time zone 
c        dt                  time step (s)
c        dx                  cell size in x-direction (m)
c        dy                  cell size in y-direction (m)
c        delx                cell size in x-direction (km or deg)
c        dely                cell size in y-direction (km or deg)
c        meshfac             grid meshing factor relative to master grid
c        mapscl              map scale factor
c        height              gridded layer height (m)
c        rkv                 gridded vertical diffusivity (m2/s)
c        tempk               gridded temperature (K)
c        tsurf               gridded surface temperature (K)
c        press               gridded pressure (mb)
c        water               gridded water vapor concentration (ppm)
c        windu               gridded x-component windspeed (m/s)
c        windv               gridded y-component windspeed (m/s)
c        cldtrns             gridded energy transmission coefficient (fraction)
c        fcloud              gridded cloud coverage (fraction)
c        cwc                 gridded cloud water (g/m3)
c        pwr                 gridded precip rain (g/m3)
c        pws                 gridded precip snow (g/m3)
c        pwg                 gridded precip graupel (g/m3)
c        cph                 gridded cloud water pH
c        cellat              gridded cell centroid latitude (deg)
c        cellon              gridded cell centroid longitude (deg)
c        fsurf               gridded cell landuse fraction
c        vdep                gridded species deposition velocity (m/s)
c        conc                species concentration in the grid (umol/m3,ug/m3)
c        cncrad              radical concentrations (ppm)
c
c     Output arguments:
c        conc                species concentration in the grid (umol/m3,ug/m3)
c        cncrad              radical concentrations (ppm)
c        pigdump             PiG dumped mass (umol)
c        pgmserr             PiG dumping mass error (umol)
c        fluxes              boundary mass fluxes (umol, ug)
c        depfld              2-D array of wet deposited mass (mol/ha, g/ha)
c                            and surface liquid concentrations (mol/l, g/l)
c
c     Subroutines Called:
c        PIGCOORD
c        VIRTDUMP
c        KTHERM
c        GETZNTH
c        KPHOTO
c        KHETERO
c        NOXCHEM
c        IRONCHEM
c        CALDATE
c        PIGGROW 
c
c     Called by:
c        PIGEVOL
c
      implicit none
      include "camx.prm"
      include "camx.com"
      include "filunit.com"
      include "chmstry.com"
      include "ahomap.com"
      include "pigsty.com"
      include "deposit.com"
      include "flags.com"
c
c======================== Source Apportion Begin =======================
c
      include "tracer.com"
      include "rtracchm.com"
c
c========================= Source Apportion End ========================
c
      real*8 pigdump(MXSPEC),pgmserr(MXSPEC),flxdry(MXSPEC),
     &       flxwet(MXSPEC),fluxes(MXSPEC,11)
      logical ldarkp,ldump,lvdmp(MXPIG),lkill(MXPIG),lstage3
      integer ncol,nrow,nlay
      real height(ncol,nrow,nlay),rkv(ncol,nrow,nlay),
     &     tempk(ncol,nrow,nlay),water(ncol,nrow,nlay),
     &     press(ncol,nrow,nlay),windu(ncol,nrow,nlay),
     &     windv(ncol,nrow,nlay),cldtrns(ncol,nrow,nlay),
     &     fcloud(ncol,nrow,nlay),cwc(ncol,nrow,nlay),
     &     pwr(ncol,nrow,nlay),pws(ncol,nrow,nlay),pwg(ncol,nrow,nlay),
     &     cph(ncol,nrow,nlay),cellat(ncol,nrow),cellon(ncol,nrow),
     &     fsurf(ncol,nrow,NLU),vdep(ncol,nrow,nspec),
     &     conc(ncol,nrow,nlay,nspec),cncrad(ncol,nrow,nlay,MXRADCL),
     &     depfld(ncol,nrow,3*nspec),dx(nrow),tsurf(ncol,nrow),
     &     mapscl(ncol,nrow)
      real conpig(MXSPEC+1,MXRECTR),conback(MXSPEC+1),connew(MXSPEC+1),
     &     cradp(MXRADCL,MXRECTR),cradbck(MXRADCL),dumpmass(MXSPEC),
     &     pconc(MXLAYA),volvec(MXLAYA),wtfac(MXLAYA),
     &     pcvfac(MXLAYA),pcorrec(MXLAYA,MXSPEC),
     &     vpconc(MXCOLA,MXROWA,MXLAYA,MXSPEC),
     &     othrpuf(MXLAYA,MXSPEC),pbdnl(MXLAYA)
      real depth(MXLAYA),rkpig(MXLAYA),delx,dely,vd(MXSPEC),
     &     dpfl(2*MXSPEC)
      integer n,m,i,j,k,kk,kkk,l,ll,is,igrd,iptr2d,itzon,
     &        kpb,kpt,ij,iozon,ihaze,ialb,iabov,nr,idum,meshfac,
     &        idate,month,mbin,latbin,isesn,idxcel,noxcnt,
     &        hinox(MXRECTR),ierr1,ierr2,ierr3,aero_flag
      real dt,dy,areamx,pufscl,delt,dtpig,tpuff,wpuff,ppuff,sumwt,
     &     deplyr,
     &     hght,ctrns,fcld,zenith,convfac,volpuff,uu,vv,wind,press0,
     &     delms,rctdmp,volfac,tmass,tvol,resid,negconc,cldadj,z0sum,
     &     totland,z0,bmass,hshear,vshear,dudx,dudy,dudz,dvdx,
     &     dvdy,dvdz,avgup,avgum,avgvp,avgvm,dz,zzp,zzm,cwpuff,prpuff,
     &     pspuff,pgpuff,phpuff
      real xpig,ypig,xdist,ydist,xlen,axisx,pi,test,ch4
      real so4_tot,no3_tot,nhx_tot,clx_tot,frcsulf,frcpso4,frchno3,
     &     frcpno3,frcnh3,frcnh4,frchcl,frcpcl,so4_bck,no3_bck,
     &     nhx_bck,clx_bck
c
      common /pigdrivdat/ vpconc
c
c======================== Source Apportion Begin =======================
c
      integer ipsa3d,idx
      real    co3,radoh,radno3,rtcon(MXTRSP),rtdump(MXTRSP)
      real    delconc(MXLAYA),avgrad(MXRADCL),avgcnc(MXSPEC+1)
c
c========================= Source Apportion End ========================
c
      data pi /3.1415927/
      data CH4 /1.75/
c
c-----Entry point
c
      do i=1,MXRADCL
        avgrad(i) = 0.
      enddo
      do i=1,MXSPEC+1
        avgcnc(i) = 0.
      enddo
c
c-----Check for negative incoming CONC values and initialize puff overlap
c     array VPCONC
c
      do is = 1,nspec
        do k = 1,nlay
          do j = 1,nrow
            do i = 1,ncol
              vpconc(i,j,k,is) = 0.
              if (conc(i,j,k,is) .lt. 0.) then
                write(iout,'(/,a)') 
     &             'Start of PIGDRIVE ...negative value in grid conc'
                write(iout,*) 'species, grid, i, j, k: ',is,igrd,i,j,k
                call flush(iout)
                call camxerr()
              endif
            enddo
          enddo
        enddo
      enddo
c
c-----Perform dry deposition
c       
      if (ldry) then
        do 10 n = 1,npig
          if (ingrd(n).ne.igrd) goto 10
c
c-----Perform deposition once for new puffs over their age
c
          if (lnewg(n)) then
            delt = (agepigf(n) + agepigb(n))/2.
c
c-----Perform deposition for old puffs over current grid's timestep
c
          else
            delt = dt
          endif
c
          xpig = (xpigf(n) + xpigb(n))/2.
          ypig = (ypigf(n) + ypigb(n))/2.
          call pigcoord(xpig,ypig,i,j,idum)
          if (puffbot(n).ge.amax1(10.,height(i,j,1)/2.)) goto 10
          dz = height(i,j,1) - puffbot(n)
          do l = 1,nspec
            vd(l) = vdep(i,j,l)
          enddo
c
          call irondry(n,dx(j),dy,dz,mapscl(i,j),delt,vd,dpfl,flxdry)
c
          do 11 l = 1,nspec
            fluxes(l,11) = fluxes(l,11) + flxdry(l)
            do ll = 1,navspc
              if (l .eq. lavmap(ll)) then
                depfld(i,j,ll) = depfld(i,j,ll) + dpfl(l)
                goto 11
              endif
            enddo
 11       continue
 10     continue
      endif
c
c-----Perform chemistry and wet deposition
c
      if (.not.lchem .AND. .not.lwet) goto 101
c
c-----Aerosol timing parameters:
c     time_aero : time to call aerosol routine (HHMM)
c     date_aero : date to call aerosol routine (YYJJJ)
c
      aero_flag = 0
      if( lchem .AND. naero.GT.0 .AND. aeropt.EQ.'CF' ) then
         aero_flag = 1
         if ( (timec(igrd)-time_aero(igrd)) .ge. -0.01 .and.
     &        (datec(igrd) .eq. date_aero(igrd)) ) then
           aero_flag = 2
         endif
      endif
c
c-----Treat overlapping puffs via a virtual dump of "large" puffs
c 
      do 20 n = 1,npig

        lvdmp(n) = .false.
        lkill(n) = .false.
        if (ingrd(n).ne.igrd .or. .not.loverlap) goto 20
c
        xpig = (xpigf(n) + xpigb(n))/2.
        ypig = (ypigf(n) + ypigb(n))/2.
        call pigcoord(xpig,ypig,i,j,idum)
c
        xdist = (xpigf(n) - xpigb(n))/delx*dx(j)*meshfac
        ydist = (ypigf(n) - ypigb(n))/dely*dy*meshfac
        xlen  = sqrt(xdist**2 + ydist**2)
        axisx = xlen + 3.*sigy(n)
        pufscl = pi*(axisx/2.)*(1.5*sigy(n))
c
        if (DXYMAX .gt. 0.) then
          areamx = DXYMAX*DXYMAX
        else
          areamx = dx(j)*dy
        endif
        if (pufscl .ge. FLEAK*areamx) then
          lvdmp(n) = .true.
          call virtdump(n,ncol,nrow,nlay,nspec,i,j,dx(j),dy,
     &                  mapscl,height,tempk,press,pcorrec)
          do is = 1,nspec
            do kk = 1,nlay
              vpconc(i,j,kk,is) = vpconc(i,j,kk,is) + pcorrec(kk,is)
            enddo
          enddo
        endif
 20   continue
c
c-----CHEMISTRY + WET DEP loop
c
      do 100 n = 1,npig
c
c-----Skip puffs that are not in the current grid
c
        if (ingrd(n) .ne. igrd) goto 100
c
c-----Perform chemistry once for new puffs over their age
c
        if (lnewg(n)) then
          delt = (agepigf(n) + agepigb(n))/2.
c
c-----Perform chemistry for old puffs over current grid's timestep
c
        else
          delt = dt 
        endif 
        dtpig = min( (agepigf(n) + agepigb(n))/(2.*3600.),
     &                aero_dt(igrd)+(dt/3600.) )
c
c-----Locate the puff in the grid
c
        xpig = (xpigf(n) + xpigb(n))/2.
        ypig = (ypigf(n) + ypigb(n))/2.
        call pigcoord(xpig,ypig,i,j,idum)
        do k = 1,nlay
          if (height(i,j,k) .gt. zpig(n)) goto 25
        enddo
        k = nlay
c
  25    xdist = (xpigf(n) - xpigb(n))/delx*dx(j)*meshfac
        ydist = (ypigf(n) - ypigb(n))/dely*dy*meshfac
        xlen  = sqrt(xdist**2 + ydist**2)
        axisx = xlen + axisy(n)
        volpuff = pi*axisx*axisy(n)*axisz(n)/4.
c
c-----Find layers containing top and bottom of puff
c
        kpb = 1
        kpt = nlay
        do kk = 1,nlay
          if (height(i,j,kk) .GE. pufftop(n)) then
            kpt = kk
            goto 26
          endif
        enddo
  26    continue
        do kk = nlay,1,-1
          if (height(i,j,kk) .LE. puffbot(n)) then
            kpb = kk + 1
            goto 27
          endif
        enddo
  27    continue
c
c-----Get overlap vector for this puff if it contributed to the 
c     virtual dump above (subtract out its contribution to VPCONC).
c     Otherwise, do not apply any puff overlap
c
        if (lvdmp(n)) then
          call virtdump(n,ncol,nrow,nlay,nspec,i,j,dx(j),dy,
     &                  mapscl,height,tempk,press,pcorrec)
          do is = 1,nspec
            do kk = kpb,kpt
              othrpuf(kk,is) = vpconc(i,j,kk,is) - pcorrec(kk,is)
            enddo
          enddo
        else
          do is = 1,nspec
            do kk = kpb,kpt
              othrpuf(kk,is) = 0.
            enddo
          enddo
        endif
c 
c-----Average temp, press, water, and background concs (=overlap+gridded)
c     over layers contained in this puff; define layer weighting as the 
c     depth of puff coverage weighted by density
c
        if (kpb .eq. kpt) then
          wtfac(kpb) = 1.
        else
          sumwt = 0.
          deplyr = pufftop(n) - height(i,j,kpt-1)
          wtfac(kpt) = deplyr*press(i,j,kpt)/tempk(i,j,kpt)
          sumwt = sumwt + wtfac(kpt)
c
          deplyr = height(i,j,kpb) - puffbot(n)
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
        tpuff = 0.
        ppuff = 0.
        wpuff = 0.
        cwpuff = 0.
        prpuff = 0.
        pspuff = 0.
        pgpuff = 0.
        phpuff = 0.
        do is = 1,nspec+1
          conback(is) = 0.
        enddo
        do kk = kpb,kpt
          tpuff = tpuff + wtfac(kk)*tempk(i,j,kk)
          ppuff = ppuff + wtfac(kk)*press(i,j,kk)
          wpuff = wpuff + wtfac(kk)*water(i,j,kk)
          cwpuff = cwpuff + wtfac(kk)*cwc(i,j,kk)
          prpuff = prpuff + wtfac(kk)*pwr(i,j,kk)
          pspuff = pspuff + wtfac(kk)*pws(i,j,kk)
          pgpuff = pgpuff + wtfac(kk)*pwg(i,j,kk)
          phpuff = phpuff + wtfac(kk)*cph(i,j,kk)
          do is = 1,nspec
            conback(is) = conback(is) + wtfac(kk)*
     &                    (othrpuf(kk,is) + conc(i,j,kk,is))
          enddo
        enddo
        convfac = densfac*(273./tpuff)*(ppuff/1013.)
        do is = 1,nspec
          if (is.le.ngas) conback(is) = conback(is)/convfac
          conback(is) = amax1(bdnl(is),conback(is))
          connew(is) = conback(is)
        enddo
        connew(nspec+1) = 0.
c
c-----Add background concentrations (CONBACK) to puff increments in
c     each reactor to get total concentration (CONPIG).
c     Background radicals taken from nominal pig center (no averaging)
c     Puff radicals taken from last time step to use as initial guess
c
        do is = 1,nspec
          do nr = 1,nreactr
            conpig(is,nr) = puffmass(is,nr,n)/(volpuff/float(nreactr))
            if (is.le.ngas) conpig(is,nr) = conpig(is,nr)/convfac
            conpig(is,nr) = conback(is) + conpig(is,nr)
          enddo
        enddo
        do nr = 1,nreactr
          conpig(nspec+1,nr) = 0.
        enddo
        if (lchem) then
          do l = 1,nrad
            cradbck(l) = cncrad(i,j,k,l)
            do nr = 1,nreactr
              cradp(l,nr) = puffrad(l,nr,n)
            enddo
          enddo
        endif
c
c-----Perform wet deposition
c
        if (lwet .AND. (prpuff.ge.cwmin .or.
     &                  pspuff.ge.cwmin .or.
     &                  pgpuff.ge.cwmin)) then
c
          call ironwet(n,dx(j),dy,mapscl(i,j),cwpuff,prpuff,pspuff,
     &                 pgpuff,phpuff,tpuff,ppuff,volpuff,conpig,
     &                 axisz(n),delt,dpfl,flxwet)
c
          do 31 l = 1,nspec
            do nr = 1,nreactr
              conpig(l,nr) = puffmass(l,nr,n)/(volpuff/float(nreactr))
              if (l.le.ngas) conpig(l,nr) = conpig(l,nr)/convfac
              conpig(l,nr) = conback(l) + conpig(l,nr)
            enddo
            fluxes(l,11) = fluxes(l,11) + flxwet(l)
            do ll = 1,navspc
              if (l .eq. lavmap(ll)) then
                depfld(i,j,navspc+ll) = depfld(i,j,navspc+ll) + dpfl(l)
                depfld(i,j,2*navspc+ll) = depfld(i,j,2*navspc+ll) +
     &                                    dpfl(nspec+l)
                goto 31
              endif
            enddo
 31       continue
        endif
c
c-----Determine if any puff reactors have high NOx values
c
        if (.not.lchem) goto 100
        noxcnt = 0
        do nr = 1,nreactr
          hinox(nr) = 0
          if (conpig(kno,nr) + conpig(kno2,nr) .gt. 0.2) then
            noxcnt = noxcnt + 1
            hinox(nr) = 1
          endif
        enddo
c
c-----Load local values of ozone, haze, albedo and zenith angle
c
        ij = i + (j - 1)*ncol
        iozon = icdozn(iptr2d - 1 + ij)
        ihaze = icdhaz(iptr2d - 1 + ij)
        ialb  = icdalb(iptr2d - 1 + ij)
        if (lrdsno .and. icdsno(iptr2d-1+ij) .eq. 1) ialb = NALB

        hght = height(i,j,k)/2000.
        if (k.gt.1) hght = (height(i,j,k) + height(i,j,k-1))/2000.
        if (cldtrns(i,j,k) .ne. 1.) then
          iabov = 0
          ctrns = cldtrns(i,j,k)
          fcld = fcloud(i,j,k)
        else
          iabov = 1
          ctrns = cldtrns(i,j,1)
          fcld = fcloud(i,j,1)
        endif
        call getznth(cellat(i,j),cellon(i,j),timec(igrd),datec(igrd),
     &               itzon,zenith,ldarkp)
c
c-----Determine thermal rate constants
c
        call ktherm(tpuff,ppuff)
c
c-----Determine if any puff reactors are in Stage 3 chemistry
c
        lstage3 = .false.
        do nr = 1,nreactr
          if (conpig(kNO2,nr).le.0.) then
            test = 999.0
          elseif (idmech.eq.5) then
            test = (  rk(29)*conpig(kCO,nr) +
     &          rk(44)*conpig(kSO2,nr)   + rk(125)*conpig(kHCHO,nr) +
     &          rk(130)*conpig(kCCHO,nr) + rk(133)*conpig(kRCHO,nr) +
     &          rk(136)*conpig(kACET,nr) + rk(138)*conpig(kMEK,nr)  +
     &          rk(140)*conpig(kMEOH,nr) + rk(147)*conpig(kGLY,nr)  +
     &          rk(150)*conpig(kMGLY,nr) + rk(155)*conpig(kCRES,nr) +
     &          rk(161)*conpig(kMETH,nr) + rk(166)*conpig(kMVK,nr)  +
     &          rk(170)*conpig(kISPD,nr) + rk(178)*conpig(kDCB1,nr) +
     &          rk(180)*conpig(kDCB2,nr) + rk(182)*conpig(kDCB3,nr) +
     &          rk(184)*CH4              + rk(185)*conpig(kETHE,nr) +
     &          rk(189)*conpig(kISOP,nr) + rk(193)*conpig(kTERP,nr) +
     &          rk(197)*conpig(kALK1,nr) + rk(198)*conpig(kALK2,nr) +
     &          rk(199)*conpig(kALK3,nr) + rk(200)*conpig(kALK4,nr) +
     &          rk(201)*conpig(kALK5,nr) + rk(202)*conpig(kARO1,nr) +
     &          rk(203)*conpig(kARO2,nr) + rk(204)*conpig(kOLE1,nr) +
     &          rk(208)*conpig(kOLE2,nr) + rk(212)*conpig(kETOH,nr) +
     &          rk(213)*conpig(kMTBE,nr) + rk(214)*conpig(kMBUT,nr) ) /
     &          rk(25)*conpig(kNO2,nr)
          elseif (idmech.eq.6) then
            test = (  rk(63)*conpig(kSO2,nr) +
     &          rk(66)*conpig(kCO,nr)    + rk(67)*CH4               +
     &          rk(73)*conpig(kMEOH,nr)  + rk(74)*conpig(kFORM,nr)  +
     &          rk(85)*conpig(kALD2,nr)  + rk(100)*conpig(kALDX,nr) +
     &          rk(113)*conpig(kETHA,nr) + rk(114)*conpig(kETOH,nr) +
     &          rk(115)*conpig(kPAR,nr)  + rk(120)*conpig(kOLE,nr)  +
     &          rk(124)*conpig(kETH,nr)  + rk(128)*conpig(kIOLE,nr) +
     &          rk(131)*conpig(kTOL,nr)  + rk(134)*conpig(kCRES,nr) +
     &          rk(139)*conpig(kOPEN,nr) + rk(141)*conpig(kXYL,nr)  +
     &          rk(142)*conpig(kMGLY,nr) + rk(145)*conpig(kISOP,nr) +
     &          rk(149)*conpig(kISPD,nr) + rk(154)*conpig(kTERP,nr) ) /
     &          rk(28)*conpig(kNO2,nr)
          else
            test = (  rk(36)*conpig(kCO,nr) +
     &           rk(37)*conpig(kFORM,nr) + rk(43)*conpig(kALD2,nr) +
     &           rk(51)                  + rk(52)*conpig(kPAR,nr)  +
     &           rk(57)*conpig(kOLE,nr)  + rk(61)*conpig(kETH,nr)  +
     &           rk(63)*conpig(kTOL,nr)  + rk(66)*conpig(kCRES,nr) +
     &           rk(70)*conpig(kOPEN,nr) + rk(72)*conpig(kXYL,nr)  +
     &           rk(73)*conpig(kMGLY,nr) + rk(76)*conpig(kISOP,nr) +
     &           rk(82)*conpig(kSO2,nr)  + rk(84)*conpig(kMEOH,nr) +
     &           rk(85)*conpig(kETOH,nr) + rk(92)*conpig(kISPD,nr) ) /
     &           rk(26)*conpig(kNO2,nr) 
          endif
          if (test.gt.0.1) lstage3 = .true.
        enddo
        if (lstage3 .and. ipigflg.eq.GRESPIG) then
          lkill(n) = .true.
          nkill(5) = nkill(5) + 1
          goto 100
        endif
c
c-----Calculate the rate constant for heterogeneous hydrolysis of N2O5
c
        if (aero_flag.GT.0) call khetero(tpuff,wpuff,conback)
c
c-----Determine photolysis rates through interpolation of look-up table
c
        cldadj = 0.0
        call kphoto(iozon,ialb,ihaze,hght,zenith,fcld,ctrns,
     &              cldadj,ldarkp,iabov)
c
c-----Perform chemistry for CONBACK if total NOx (puff+background)
c     is less than 200 ppb.
c     All reactors use same background, save results in CONNEW
c
        igrdchm = -n
        ichm    = i
        jchm    = j
        kchm    = 0
        ierr1   = 0
        ierr2   = 0
        ierr3   = 0
        tchm = tpuff
        wchm = wpuff
        ldchm = ldarkp
        if (noxcnt .ne. nreactr) then
          call ironchem(delt,dtpig,aero_flag,wpuff,tpuff,ppuff,cwpuff,
     &                  phpuff,connew,cradbck,avgcnc,avgrad,convfac,
     &                  ierr1,ierr2,ierr3,.true.)
          if (ierr1 .ne. 0) then
            lkill(n) = .true.
            nkill(3) = nkill(3) + 1
c           write(idiag,*)'Dumping puff ',n,'due to bad background chem'
            goto 100
          endif
          if (ierr2.ne. 0) then
            lkill(n) = .true.
            nkill(6) = nkill(6) + 1
c           write(idiag,*)'Dumping puff ',n,'due to background AQ'
            goto 100
          endif
          if (ierr3.ne. 0) then
            lkill(n) = .true.
            nkill(8) = nkill(8) + 1
c           write(idiag,*)'Dumping puff ',n,'due to background PM'
            goto 100
          endif
        endif
c
c-----Loop over reactors, perform chemistry for CONPIG.
c     Check for NOx concentrations > 200 ppb; if found, use the 
c     reduced chemistry module
c
        do nr = 1,nreactr
          igrdchm = -n
          ichm    = i
          jchm    = j
          kchm    = -nr
          ierr1   = 0
          ierr2   = 0
          ierr3   = 0
          tchm = tpuff
          wchm = wpuff
          ldchm = ldarkp
c
c======================== Source Apportion Begin =======================
c
          if( ltrace .AND. tectyp .EQ. RTRAC ) then
             co3    = conpig(ko3,nr)
             radoh  = cradp(koh,nr)
             radno3 = cradp(kno3,nr)
          endif
c
c========================= Source Apportion End ========================
c
          if (hinox(nr) .eq. 1) then
            call noxchem(delt,bdnl(ko3),bdnl(kno),conpig(kno,nr),
     &                   conpig(kno2,nr),conpig(ko3,nr),rk(ipigrxn))
          else
            call ironchem(delt,dtpig,aero_flag,wpuff,tpuff,ppuff,cwpuff,
     &                    phpuff,conpig(1,nr),cradp(1,nr),avgcnc,avgrad,
     &                    convfac,ierr1,ierr2,ierr3,.false.)
            if (ierr1 .ne. 0) then
              lkill(n) = .true.
              nkill(4) = nkill(4) + 1
c             write(idiag,*)'Dumping puff ',n,'due to puff LSODE'
              goto 100
            endif
            if (ierr2 .ne. 0) then
              lkill(n) = .true.
              nkill(7) = nkill(7) + 1
c             write(idiag,*)'Dumping puff ',n,'due to puff AQ'
              goto 100
            endif
            if (ierr3 .ne. 0) then
              lkill(n) = .true.
              nkill(9) = nkill(9) + 1
c             write(idiag,*)'Dumping puff ',n,'due to puff PM'
              goto 100
            endif
          endif
c
c======================== Source Apportion Begin =======================
c
          if( ltrace .AND. tectyp .EQ. RTRAC .AND.
     &        nrtherm .GT. 0 .AND. nrtphot .GT. 0 ) then
             co3    = (co3 + conpig(ko3,nr))/2.
             radoh  = (radoh + cradp(koh,nr))/2.
             radno3 = (radno3 + cradp(kno3,nr))/2.
             do is = 1,nrtgas
               rtcon(is) = puffrt(is,nr,n)/(volpuff/float(nreactr))
               rtcon(is) = rtcon(is)/convfac
             enddo
             call pigchmrt(ppuff,tpuff,radoh,co3,radno3,delt/3600.,
     &                     rtcon)
             do is = 1,nrtgas
               rtcon(is) = rtcon(is)*convfac
               puffrt(is,nr,n) = rtcon(is)*volpuff/float(nreactr)
             enddo
          endif
c
c========================= Source Apportion End ========================
c
c-----Apply PM partitioning ratios to background mass
c
          if (aero_flag.GT.0) then
            so4_tot = conpig(ksulf,nr)*convfac + conpig(kpso4,nr)/96.
            no3_tot = conpig(khno3,nr)*convfac + conpig(kpno3,nr)/62.
            nhx_tot = conpig(knh3,nr)*convfac + conpig(kpnh4,nr)/18.
            clx_tot = conpig(khcl,nr)*convfac + conpig(kpcl,nr)/35.

            frcsulf = conpig(ksulf,nr)*convfac/so4_tot
            frcpso4 = conpig(kpso4,nr)/96./so4_tot
            frchno3 = conpig(khno3,nr)*convfac/no3_tot
            frcpno3 = conpig(kpno3,nr)/62./no3_tot
            frcnh3  = conpig(knh3,nr)*convfac/nhx_tot
            frcnh4  = conpig(kpnh4,nr)/18./nhx_tot
            frchcl  = conpig(khcl,nr)*convfac/clx_tot
            frcpcl  = conpig(kpcl,nr)/35./clx_tot

            so4_bck = connew(ksulf)*convfac + connew(kpso4)/96.
            no3_bck = connew(khno3)*convfac + connew(kpno3)/62.
            nhx_bck = connew(knh3)*convfac + connew(kpnh4)/18.
            clx_bck = connew(khcl)*convfac + connew(kpcl)/35.

            connew(ksulf) = amax1(so4_bck*frcsulf/convfac,bdnl(ksulf))
            connew(kpso4) = amax1(so4_bck*frcpso4*96.,bdnl(kpso4))
            connew(khno3) = amax1(no3_bck*frchno3/convfac,bdnl(khno3))
            connew(kpno3) = amax1(no3_bck*frcpno3*62.,bdnl(kpno3))
            connew(knh3)  = amax1(nhx_bck*frcnh3/convfac,bdnl(knh3))
            connew(kpnh4) = amax1(nhx_bck*frcnh4*18.,bdnl(kpnh4))
            connew(khcl)  = amax1(clx_bck*frchcl/convfac,bdnl(khcl))
            connew(kpcl)  = amax1(clx_bck*frcpcl*35.,bdnl(kpcl))
          endif
c
c-----Subtract off the new background to get new increment.
c     CONPIG and PUFFMASS can be negative again
c
          do is = 1,nspec
            if (hinox(nr).eq.1) then
              conpig(is,nr) = conpig(is,nr) - conback(is)
            else
              conpig(is,nr) = conpig(is,nr) - connew(is)
            endif
            if (is.le.ngas) conpig(is,nr) = conpig(is,nr)*convfac
            puffmass(is,nr,n) = conpig(is,nr)*volpuff/float(nreactr)
          enddo
          do l = 1,nrad
            puffrad(l,nr,n) = cradp(l,nr)
          enddo
        enddo
 100  continue
c
c-----End CHEMISTRY loop
c
c-----Start GROWTH and DUMPING loop
c
 101  continue
      do 200 n = 1,npig
c
c-----Skip puffs that are not in the current grid
c
        if (ingrd(n) .ne. igrd) goto 200
c
c-----Perform growth once for new puffs over their age
c
        if (lnewg(n)) then
          delt = (agepigf(n) + agepigb(n))/2.
c
c-----Perform growth for old puffs over current grid's timestep
c
        else
          delt = dt 
        endif 
c
c-----Locate the pig in the grid
c
        xpig = (xpigf(n) + xpigb(n))/2.
        ypig = (ypigf(n) + ypigb(n))/2.
        call pigcoord(xpig,ypig,i,j,idum)
        idxcel =  i + ncol*(j-1)
        do k = 1,nlay
          if (height(i,j,k) .gt. zpig(n)) goto 15
        enddo
        k = nlay
  15    xdist = (xpigf(n) - xpigb(n))/delx*dx(j)*meshfac
        ydist = (ypigf(n) - ypigb(n))/dely*dy*meshfac
        xlen  = sqrt(xdist**2 + ydist**2)
c
c-----Find layers containing top and bottom of puff
c
        kpb = 1
        kpt = nlay
        do kk = 1,nlay
          if (height(i,j,kk) .GE. pufftop(n)) then
            kpt = kk
            goto 151
          endif
        enddo
  151   continue
        do kk = nlay,1,-1
          if (height(i,j,kk) .LE. puffbot(n)) then
            kpb = kk + 1
            goto 152
          endif
        enddo
  152   continue
c
c-----Get the drivers on puff growth
c
c-----Resolved shear metrics: use deformation for net horizontal shear
c
        dudx = (windu(i,j,k) - windu(i-1,j,k))/dx(j)
        dvdy = (windv(i,j,k) - windv(i,j-1,k))/dy
        avgup = (windu(i,j,k) + windu(i,j+1,k) + windu(i-1,j,k) + 
     &           windu(i-1,j+1,k))/4.
        avgum = (windu(i,j,k) + windu(i,j-1,k) + windu(i-1,j,k) + 
     &           windu(i-1,j-1,k))/4.
        dudy = (avgup - avgum)/dy
        avgvp = (windv(i,j,k) + windv(i+1,j,k) + windv(i,j-1,k) + 
     &           windv(i+1,j-1,k))/4.
        avgvm = (windv(i,j,k) + windv(i-1,j,k) + windv(i,j-1,k) + 
     &           windv(i-1,j-1,k))/4.
        dvdx = (avgvp - avgvm)/dx(j)
        hshear = sqrt((dudy + dvdx)**2 + (dudx - dvdy)**2)
c
        dudz = 0.
        dvdz = 0.
        dz = height(i,j,k)
        if (k.gt.1) dz = height(i,j,k) - height(i,j,k-1)
        if (axisz(n) .gt. dz .and. kpt-kpb .ge. 1) then
          zzp = (height(i,j,kpt) + height(i,j,kpt-1))/2.
          zzm = height(i,j,kpb)/2.
          if (kpb.gt.1) zzm = (height(i,j,kpb) + height(i,j,kpb-1))/2.
          avgup = (windu(i,j,kpt) + windu(i-1,j,kpt))/2.
          avgum = (windu(i,j,kpb) + windu(i-1,j,kpb))/2.
          dudz = (avgup - avgum)/(zzp - zzm)
          avgvp = (windv(i,j,kpt) + windv(i,j-1,kpt))/2.
          avgvm = (windv(i,j,kpb) + windv(i,j-1,kpb))/2.
          dvdz = (avgvp - avgvm)/(zzp - zzm)
        endif
        vshear = sqrt(dudz**2 + dvdz**2)
c
c-----Surface layer met
c
        uu = 0.5*(windu(i,j,1) + windu(i-1,j,1))
        vv = 0.5*(windv(i,j,1) + windv(i,j-1,1))
        wind = sqrt(uu*uu + vv*vv)
        do kk = 1,nlay
          depth(kk) = height(i,j,kk)
          if (kk.gt.1) depth(kk) = height(i,j,kk) - height(i,j,kk-1)
          rkpig(kk) = rkv(i,j,kk)
        enddo
        press0 = press(i,j,1) - 
     &           depth(1)*(press(i,j,2) - press(i,j,1))/height(i,j,2)
c
c-----Season, landuse, and surface roughness
c
        idate = datec(igrd)
        call caldate(idate)
        month = (idate - 10000*int(idate/10000.))/100
        mbin = month
        if (cellat(i,j) .lt. 0.) then
          mbin = mod(month+6,12)
          if (mbin.eq.0) mbin = 12
        endif
        latbin = 1
        if (abs(cellat(i,j)) .gt. 20.) then
          latbin = 2
        elseif (abs(cellat(i,j)) .gt. 35.) then
          latbin = 3
        elseif (abs(cellat(i,j)) .gt. 50.) then
          latbin = 4
        elseif (abs(cellat(i,j)) .gt. 75.) then
          latbin = 5
        endif
        if ((cellat(i,j).gt.50. .and. cellat(i,j).lt.75.) .and.
     &      (cellon(i,j).gt.-15. .and. cellon(i,j).lt.15.)) latbin = 3
        isesn = iseason(latbin,mbin)
        if (lrdsno .and. icdsno(idxcel) .eq. 1) isesn = 4
        z0sum = 0.
        totland = 0.
        do m = 1,NLU
          if (fsurf(i,j,m) .ge. 0.01) then
            totland = totland + fsurf(i,j,m)
            z0 = z0lu(m,isesn)
            if (m.eq.7) z0 = amax1(z0,2.0e-6*wind**2.5)
            if (lrdruf(igrd) .and. icdruf(idxcel) .gt. 0) 
     &                                    z0 = ruflen(icdruf(idxcel))
            z0sum = z0sum + alog(z0)*fsurf(i,j,m)
          endif
        enddo
        z0 = exp(z0sum/totland)
c
c-----Calculate new puff size and fraction of mass that has leaked (DELMS)
c
        if (DXYMAX .gt. 0.) then
          areamx = DXYMAX*DXYMAX
        else
          areamx = dx(j)*dy
        endif
        ldump = .false.
        call piggrow(n,nlay,k,delt,xlen,areamx,height(i,j,nlay),wind,
     &               tempk(i,j,1),tsurf(i,j),press(i,j,1),press0,rkpig,
     &               depth,z0,hshear,vshear,delms,ldump)
c
c-----Skip dumping if it's a new puff
c
        if (lnewg(n) .and. .not.lkill(n)) goto 40
c
c-----Find layers containing top and bottom of puff
c
        kpb = 1       
        kpt = nlay
        do kk = 1,nlay
          if (height(i,j,kk) .GE. pufftop(n)) then
            kpt = kk     
            goto 16
          endif       
        enddo
  16    continue
        do kk = nlay,1,-1
          if (height(i,j,kk) .LE. puffbot(n)) then
            kpb = kk + 1
            goto 17
          endif
        enddo
  17    continue
c
c-----Revise vertical averaging to account for growth; define layer weighting 
c     as the depth of puff coverage weighted by density
c
        if (kpb .eq. kpt) then
          wtfac(kpb) = 1.
        else
          sumwt = 0.
          deplyr = pufftop(n) - height(i,j,kpt-1)
          wtfac(kpt) = deplyr*press(i,j,kpt)/tempk(i,j,kpt)
          sumwt = sumwt + wtfac(kpt)

          deplyr = height(i,j,kpb) - puffbot(n)
          wtfac(kpb)= deplyr*press(i,j,kpb)/tempk(i,j,kpb)
          sumwt = sumwt + wtfac(kpb)
c
          do kk = kpb+1,kpt-1
            deplyr = height(i,j,kk) - height(i,j,kk-1)
            wtfac(kk) = deplyr*press(i,j,kk)/tempk(i,j,kk)
            sumwt = sumwt + wtfac(kk)
          enddo

          do kk = kpb,kpt
            wtfac(kk) = wtfac(kk)/sumwt
          enddo
        endif
c
        do kk = 1,nlay
          deplyr = height(i,j,1)
          if (kk.gt.1) deplyr = height(i,j,kk) - height(i,j,kk-1)
          volvec(kk) = deplyr*dx(j)*dy/mapscl(i,j)**2
          pcvfac(kk) = densfac*(273./tempk(i,j,kk))*
     &                         (press(i,j,kk)/1013.)
        enddo
c
c-----First check for a full dump
c     Slaughter the puff if its horizontal size exceeds AREAMX
c     Slaughter the puff if remaining mass fraction < 0.1
c     Slaughter the puff according to chemistry flags
c
        axisx = xlen + 3.*sigy(n)
        pufscl = pi*(axisx/2.)*(1.5*sigy(n))
c
        if (lkill(n) .OR. pufscl.GT.areamx .OR. fmspig(n).lt.0.1) then
          if (.not.lkill(n)) then
            if (pufscl.gt.areamx) then
              nkill(1) = nkill(1) + 1
            elseif (fmspig(n).lt.0.1) then
              nkill(2) = nkill(2) + 1
            endif
          endif
          ingrd(n) = 0
          ldump = .true.
          do is = 1,nspec
            dumpmass(is) = 0.
            do nr = 1,nreactr
              dumpmass(is) = dumpmass(is) + puffmass(is,nr,n)
              puffmass(is,nr,n) = 0.
            enddo
          enddo
          pigage(igrd) = pigage(igrd) + agepigf(n)
          nage(igrd) = nage(igrd) + 1
c
c======================== Source Apportion Begin =======================
c
          if( ltrace .AND. tectyp .EQ. RTRAC ) then
             do is = 1,nrtrac
               rtdump(is) = 0.
               do nr = 1,nreactr
                 rtdump(is) = rtdump(is) + puffrt(is,nr,n)
                 puffrt(is,nr,n) = 0.
               enddo
             enddo
          endif
c
c========================= Source Apportion End ========================
c
          goto 60
        endif
c
c-----If no full dump, check for leakage
c     (LDUMP and DELMS were set in PIGGROW)
c
        if (ldump) then
          do is = 1,nspec
            dumpmass(is) = 0.
            do nr = 1,nreactr
              rctdmp = puffmass(is,nr,n)*delms
              dumpmass(is) = dumpmass(is) + rctdmp
              puffmass(is,nr,n) = puffmass(is,nr,n) - rctdmp
            enddo
          enddo
c
c======================== Source Apportion Begin =======================
c
          if( ltrace .AND. tectyp .EQ. RTRAC ) then
             do is = 1,nrtrac
               rtdump(is) = 0.
               do nr = 1,nreactr
                 rctdmp = puffrt(is,nr,n)*delms
                 rtdump(is) = rtdump(is) + rctdmp
                 puffrt(is,nr,n) = puffrt(is,nr,n) - rctdmp
               enddo
             enddo
          endif
c
c========================= Source Apportion End ========================
c
        endif
c
c-----Update total dumped mass 
c
 60     if (ldump) then
          do is = 1,nspec
            pigdump(is) = pigdump(is) + dumpmass(is)
          enddo
c
c-----Dump DUMPMASS into grid; allocate mass in proportion to layer densities
c     among layers contained within this puff
c
          do 300 is = 1,nspec
            do kk = kpb,kpt
              volfac = wtfac(kk)/volvec(kk)
              pconc(kk) = conc(i,j,kk,is) + dumpmass(is)*volfac
              if (is.le.ngas) then
                pbdnl(kk) = bdnl(is)*pcvfac(kk)
              else
                pbdnl(kk) = bdnl(is)
              endif
c
c-----Woops! DUMPMASS leads to negative grid concentration
c
              if (pconc(kk) .lt. pbdnl(kk)) then
                tmass = 0.
                bmass = 0.
                tvol = 0.
                do kkk = kpb,kpt
                  tmass = tmass + conc(i,j,kkk,is)*volvec(kkk)
                  tvol  = tvol + volvec(kkk)
                  if (is.le.ngas) then
                    pbdnl(kkk) = bdnl(is)*pcvfac(kkk)
                  else
                    pbdnl(kkk) = bdnl(is)
                  endif
                  bmass = bmass + pbdnl(kkk)*volvec(kkk)
                enddo
                resid = tmass + dumpmass(is)
c
c-----Try spreading any positive residual mass evenly over layers contained 
c     within puff depth (if there is enough grid mass)
c
                if (resid .ge. bmass) then
                  do kkk = kpb,kpt
                    pconc(kkk) = resid/tvol
                  enddo
                  goto 301
c
c-----Not enough grid mass for a postive residual. Set grid conc to lower
c     bound and add negative mass increment to tracking array
c
                else
                  negconc = pconc(kk) - pbdnl(kk)
                  pgmserr(is) = pgmserr(is) + negconc*volvec(kk)
                  pconc(kk) = pbdnl(kk)
                endif
              endif
            enddo
 301        do kk = kpb,kpt
              delconc(kk) = pconc(kk) - conc(i,j,kk,is)
              conc(i,j,kk,is) = pconc(kk)
            enddo
c
c======================== Source Apportion Begin =======================
c
c-----Call routine to update tracer concentration in the grid ---
c
            if( ltrace .AND. tectyp .NE. RTRAC ) then
               call pigdumpsa(ncol,nrow,nlay,ntotsp,
     &                       i,j,kpb,kpt,is,n,delconc,ptconc(ipsa3d) )
            endif
c
c======================== Source Apportion Begin =======================
c
 300      continue
c
c======================== Source Apportion Begin =======================
c
          if( ltrace .AND. tectyp .EQ. RTRAC ) then
            do is = 1,nrtrac
              do kk = kpb,kpt
                volfac = wtfac(kk)/volvec(kk)
                idx = ipsa3d - 1 + i + ncol*(j-1) + ncol*nrow*(kk-1) +
     &                ncol*nrow*nlay*(is-1)
                ptconc(idx) = ptconc(idx) + rtdump(is)*volfac
              enddo
            enddo
          endif
c
c========================= Source Apportion End ========================
c
        endif
c
c-----Reset ldump and set LNEWG to false
c
 40     continue
        ldump = .false.
        lnewg(n) = .false.
c
c-----End GROWTH and DUMPING loop
c
 200  continue
c
      return
      end

      subroutine drvtuv(igrd,ncol,nrow,nlay,nspc,time,date,itzon,
     &                  inptim,inpdate,idfin,
     &                  cellat,cellon,height,hnxt,press,pnxt,tsurf,
     &                  tsnxt,tempk,tnxt,water,wnxt,conc,cod,ialb,
     &                  cldtrns)
      use ahomap
      use chmstry
c
c----CAMx v5.41 121109
c
c     DRVTUV is the driver for the calculation of a cloud adjustment to
c     clear-sky photolysis rates.  A streamlined version of the TUV
c     radiative transfer model (based on v4.8) is called to calculate
c     clear and cloudy actinic fluxes through the grid column.  The ratio
c     of clear:cloud actinic flux in each grid cell is used to apply to
c     the clear-sky photolysis rates in KPHOTO.F.
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c     Modifications:
c        03/29/11    Support in-line TUV with aerosol optical depth
c
c     Input arguments:
c        igrd                grid index
c        ncol                number of columns
c        nrow                number of rows
c        nlay                number of layers
c        nspc                number of species
c        time                current time (HHMM)
c        date                current date (YYJJJ)
c        itzon               time zone
c        inptim              next met update time (HHMM)
c        inpdate             next met update date (YYJJJ)
c        idfin               map of nested grids in this grid
c        cellat              cell centroid latitude (deg)
c        cellon              cell centroid longitude (deg)
c        height              current layer interface height field (m)
c        hnxt                next layer interface height field (m)
c        press               current layer pressure field (mb)
c        pnxt                next layer pressure field (mb)
c        tsurf               current surface temperature field (K)
c        tsnxt               next surface temperature field (K)
c        tempk               current layer temperature field (K)
c        tnxt                next layer temperature field (K)
c        water               current layer water vapor field (mb)
c        wnxt                next layer water vapor field (K)
c        conc                concentration field (umol/m3, ug/m3)
c        cod                 layer cloud optical depth field (unitless)
c        ialb                column index to albedo lookup value
c
c     Output arguments:
c        cldtrns             Cloud adjustment factor field (unitless) 
c
c     Routines called:
c        GETZNTH
c        TUV
c
c     Called by:
c        TSTEP_INIT
c
      implicit none
      include 'camx.prm'
      include 'camx_aero.inc'
      include 'flags.inc'
c
c-----Arguments
c
      integer igrd, ncol, nrow, nlay, nspc, date, itzon, inpdate
      integer idfin(ncol,nrow), ialb(ncol,nrow)
      real time, inptim
      real cellat(ncol,nrow), cellon(ncol,nrow)
      real height(ncol,nrow,nlay), hnxt(ncol,nrow,nlay)
      real press(ncol,nrow,nlay), pnxt(ncol,nrow,nlay)
      real tsurf(ncol,nrow), tsnxt(ncol,nrow)
      real tempk(ncol,nrow,nlay), tnxt(ncol,nrow,nlay)
      real water(ncol,nrow,nlay), wnxt(ncol,nrow,nlay)
      real conc(ncol,nrow,nlay,nspc)
      real cod(ncol,nrow,nlay)
      real cldtrns(ncol,nrow,nlay)
c
c-----Local variables
c
      integer i, j, k, l
      integer iz, nz
      integer rh1d(MXLAYER)
      real sumcod,alb
      real prslev,tsrfav,tmplev
      real zen1, zen2, zen, coszen
      real dtdz, tavg
      real wvapor,qwatr,ev,es,rhfac
      real totext,totssa,totcon
      real temp1d(MXLAYER),pres1d(MXLAYER)
      real rafcld(MXLAYER+1)
      real z(MXLAYER+1),midhg1d(MXLAYER)
      real airlev(MXLAYER+1)
      real odcld(MXLAYER+1), omcld(MXLAYER+1), gcld(MXLAYER+1)
      real odaer(MXLAYER+1), omaer(MXLAYER+1), gaer(MXLAYER+1)
      real frh(100)
      logical ldark1, ldark2

      real RGAS
      parameter(RGAS = 1.3806503d-19) !cm3 mbar K-1 molec-1
c
c-----Aerosol humidity adjustment parameter as f(RH); FLAG (2000)
c
      data frh/1.,1.,1.,1.,1.,
     &         1.,1.,1.,1.,1.,
     &         1.,1.,1.,1.0001,1.0001,
     &         1.0004,1.0006,1.0024,1.0056,1.0089,
     &         1.0097,1.0105,1.0111,1.0115,1.0118,
     &         1.0122,1.0126,1.0130,1.0135,1.0139,
     &         1.0173,1.0206,1.0254,1.0315,1.0377,
     &         1.0486,1.0596,1.0751,1.0951,1.1151,
     &         1.1247,1.1343,1.1436,1.1525,1.1615,
     &         1.1724,1.1833,1.1955,1.2090,1.2224,
     &         1.2368,1.2512,1.2671,1.2844,1.3018,
     &         1.3234,1.3450,1.3695,1.3969,1.4243,
     &         1.4628,1.5014,1.5468,1.5992,1.6516,
     &         1.6991,1.7466,1.7985,1.8549,1.9113,
     &         1.9596,2.0080,2.0596,2.1146,2.1695,
     &         2.2630,2.3565,2.4692,2.6011,2.7330,
     &         2.8461,2.9592,3.0853,3.2245,3.3637,
     &         3.5743,3.7849,4.0466,4.3594,4.6721,
     &         5.3067,5.9412,6.9627,8.3710,9.7793,
     &         12.4288,15.0773,18.0590,21.3709,22. /
c
c-----Entry point
c
      nz = nlay + 1
c
c-----Loop over all vertical grid columns; skip columns containing nested grids
c
      do j = 2,nrow-1
        do i = 2,ncol-1
          do k = 1,nz
            rafcld(k) = 1.
          enddo
          if (idfin(i,j).gt.igrd) goto 100
c
c-----Get solar zenith angle
c
          call getznth(cellat(i,j),cellon(i,j),time,date,itzon,zen1,
     &                 ldark1)
          call getznth(cellat(i,j),cellon(i,j),inptim,inpdate,itzon,
     &                 zen2,ldark2)
          if (ldark1 .and. ldark2) goto 100
          zen = 0.5*(zen1 + zen2)
          coszen = COS(zen*3.1415/180.)
          if (zen .gt. 75. ) coszen = COS(75.*3.1415/180.)
c
c-----Check if this is a cloud-free column; adjustmnet ratio = 1
c
          sumcod = 0.
          do k = 1,nlay
            sumcod = sumcod + cod(i,j,k)
          enddo
          if (sumcod.eq.0. .AND. .not.ltuva) goto 100
c
c-----Prepare 1-D met variables for TUV
c
          alb = albcl(ialb(i,j))
          z(1) = 0.
          do k = 1,nlay
            z(k+1) = (height(i,j,k) + hnxt(i,j,k))*5.e-4
            temp1d(k) = (tempk(i,j,k) + tnxt(i,j,k))*0.5
            pres1d(k) = (press(i,j,k) + pnxt(i,j,k))*0.5
            wvapor    = (water(i,j,k) + wnxt(i,j,k))*0.5
            qwatr     = 1.e-6*wvapor*18./28.8
            ev        = qwatr*pres1d(k)/(qwatr + eps)
            es        = e0*exp((lv/rv)*(1./273. - 1./temp1d(k)))
            rh1d(k)   = nint(100.*max(0.01,min(0.95,ev/es)))
          enddo
          tsrfav = (tsurf(i,j) + tsnxt(i,j))*0.5
          do k = 1,nlay
            midhg1d(k) = (z(k) + z(k+1))*0.5
          enddo
c
c-----Interpolate temperature and pressure to layer interfaces, then
c     calculate air density
c
          prslev = pres1d(1) - (midhg1d(1)-z(1))*
     &             (pres1d(2)-pres1d(1))/(midhg1d(2)-midhg1d(1))
          airlev(1) = prslev/(Rgas*tsrfav)

          do iz = 2,nz-1
            prslev = pres1d(iz-1) + (z(iz)-midhg1d(iz-1))*
     &      (pres1d(iz)-pres1d(iz-1))/(midhg1d(iz)-midhg1d(iz-1))
            tmplev = temp1d(iz-1) + (z(iz)-midhg1d(iz-1))*
     &      (temp1d(iz)-temp1d(iz-1))/(midhg1d(iz)-midhg1d(iz-1))
            airlev(iz) = prslev/(Rgas*tmplev)
          enddo

          dtdz = (temp1d(nz-1) - temp1d(nz-2))/
     &           (midhg1d(nz-1) - midhg1d(nz-2))
          tmplev = temp1d(nz-1) + dtdz*(z(nz) - midhg1d(nz-1))
          tavg = (tmplev + temp1d(nz-1))/2.
          prslev = pres1d(nz-1)*
     &             exp(-9.8*(z(nz) - midhg1d(nz-1))/(2.*287.*tavg))
          airlev(nz) = prslev/(Rgas*tmplev)
c
c-----Set cloud params
c
          do iz = 1,nz-1
            odcld(iz) = cod(i,j,iz)
            omcld(iz) = 0.9999
            gcld(iz) = 0.85
            odaer(iz) = 0.
            omaer(iz) = 0.95
            gaer(iz) = 0.61
          enddo
c
c-----Set aerosol params: Bext and SSA are assumed to be applicable at 340 nm
c
          if (ltuva) then
            do iz = 1,nz-1
              totext = 0.
              totssa = 0.
              totcon = 0.
              do l = ngas+1,nspc
                rhfac = 1.
                if (rhadj(l).eq.1) rhfac = frh(rh1d(iz))
                totext = totext + bext(l)*rhfac*conc(i,j,iz,l)
                totssa = totssa + ssa(l)*conc(i,j,iz,l)
                totcon = totcon + conc(i,j,iz,l)
              enddo
              odaer(iz) = totext*(z(iz+1)-z(iz))*1000.
              omaer(iz) = max(0.20,min(0.99,totssa/totcon))
              gaer(iz) = 0.61
            enddo
          endif
c
c-----Call the tuv-cloud routine
c
          call tuv(nz,z,airlev,alb,coszen,odcld,omcld,gcld,
     &             odaer,omaer,gaer,rafcld)
c
c-----Load 3-D CLDTRNS array
c
 100      do k = 1,nlay
            cldtrns(i,j,k) = 0.5 * (rafcld(k) + rafcld(k+1))
          enddo
        enddo
      enddo
c
c-----Set boundary to be clear sky
c
      do j = 1,nrow
        do k = 1,nlay
          cldtrns(1,j,k) = 1.
          cldtrns(ncol,j,k) = 1.
        enddo
      enddo
      do i = 1,ncol
        do k = 1,nlay
          cldtrns(i,1,k) = 1.
          cldtrns(i,nrow,k) = 1.
        enddo
      enddo 
       
      return
      end

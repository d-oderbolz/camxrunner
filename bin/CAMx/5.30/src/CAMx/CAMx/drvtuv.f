      subroutine drvtuv(igrd,ncol,nrow,nlay,time,date,itzon,idfin,
     &                  cellat,cellon,height,hnxt,press,pnxt,tsurf,
     &                  tsnxt,tempk,tnxt,cod,ialb,cldtrns)
      use ahomap
c
c----CAMx v5.30 101223
c
c     DRVTUV is the driver for the calculation of a cloud adjustment to
c     clear-sky photolysis rates.  A streamlined version of the TUV
c     radiative transfer model (based on v4.8) is called to calculate
c     clear and cloudy actinic fluxes through the grid column.  The ratio
c     of clear:cloud actinic flux in each grid cell is used to apply to
c     the clear-sky photolysis rates in KPHOTO.F.
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c     Modifications:
c        None
c
c     Input arguments:
c        igrd                grid index
c        ncol                number of columns
c        nrow                number of rows
c        nlay                number of layers
c        time                current time (HHMM)
c        date                current date (YYJJJ)
c        itzon               time zone
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
c
c-----Arguments
c
      integer igrd, ncol, nrow, nlay, date, itzon
      integer idfin(ncol,nrow), ialb(ncol,nrow)
      real time
      real cellat(ncol,nrow), cellon(ncol,nrow)
      real height(ncol,nrow,nlay), hnxt(ncol,nrow,nlay)
      real press(ncol,nrow,nlay), pnxt(ncol,nrow,nlay)
      real tsurf(ncol,nrow), tsnxt(ncol,nrow)
      real tempk(ncol,nrow,nlay), tnxt(ncol,nrow,nlay)
      real cod(ncol,nrow,nlay)
      real cldtrns(ncol,nrow,nlay)
c
c-----Local variables
c
      integer i, j, k
      integer iz, nz
      real sumcod,alb
      real prslev,tsrfav,tmplev
      real zen, coszen
      real dtdz, tavg
      real temp1d(MXLAYER),pres1d(MXLAYER)
      real rafcld(MXLAYER+1)
      real z(MXLAYER+1),midhg1d(MXLAYER)
      real airlev(MXLAYER+1)
      real odcld(MXLAYER+1), omcld(MXLAYER+1), gcld(MXLAYER+1)
      real odaer(MXLAYER+1), omaer(MXLAYER+1), gaer(MXLAYER+1)
      logical ldark

      real RGAS
      parameter(RGAS = 1.3806503d-19) !cm3 mbar K-1 molec-1
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
          call getznth(cellat(i,j),cellon(i,j),time,date,itzon,zen,
     &                 ldark)
          if (zen .LE. 75.) then
            coszen = COS(zen*3.1415/180.)
          elseif (zen .LE. 95. ) then
            coszen = COS(75.*3.1415/180.)
          endif
c
c-----Check if this is a cloud-free column; adjustmnet ratio = 1
c
          sumcod = 0.
          do k = 1,nlay
            sumcod = sumcod + cod(i,j,k)
          enddo
          if (sumcod.eq.0.) goto 100
c
c-----This is a cloudy column; prepare 1-D variables for TUV
c
          alb = albcl(ialb(i,j))
          z(1) = 0.
          do k = 1,nlay
            z(k+1) = (height(i,j,k) + hnxt(i,j,k))*5.e-4
            temp1d(k) = (tempk(i,j,k) + tnxt(i,j,k))*0.5
            pres1d(k) = (press(i,j,k) + pnxt(i,j,k))*0.5
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
c-----Set clouds and aerosols (zero for now, placeholder)
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

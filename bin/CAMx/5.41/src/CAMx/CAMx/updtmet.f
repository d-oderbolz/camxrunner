      subroutine updtmet(m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon,
     &                   igrd,ncol,nrow,nlay,nspec,ngas,nrtspc,
     &                   densfac,deltat,phpt,height,depth,pppt,
     &                   press,pupt,windu,pvpt,windv,pspt,tsurf,
     &                   ptpt,tempk,pwpt,water,pkpt,rkv,conc,saconc,
     &                   adjwest,adjeast,adjsouth,adjnorth)
      use bndary
      use tracer
      use rtracchm
c 
c----CAMx v5.41 121109
c  
c     UPDTMET updates the time-varying vertical layer structure for the
c     current time step and current grid, based upon linear interpolation
c     between last input time and next input time.  Also performs
c     a similar interpolation for meteorological fields, and rescales
c     the master grid boundary concentrations for the changing met.
c 
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c 
c     Modifications:  
c       10/12/04   Water vapor and vertical diffusivity fields are now
c                  time-interpolated
c       10/04/05   Minimum water vapor of 1 ppm applied
c  
c     Input arguments:  
c        igrd                grid index 
c        ncol                number of columns
c        nrow                number of rows
c        nlay                number of layers
c        nspec               number of species
c        ngas                number of gas species
c        nrtspc              number of RTRAC species
c        densfac             density conversion factor (mol/m3)
c        deltat              timestep (s)
c        phpt                time-rate change of layer interface height (m/s)
c        height              layer interface height (m)
c        pppt                time-rate change of pressure (mb/s)
c        press               presssure (mb)
c        pupt                time-rate change of u-component wind (m/s2)
c        windu               u-component wind field (m/s)
c        pvpt                time-rate change of v-component wind (m/s2)
c        windv               v-component wind field (m/s)
c        pspt                time-rate change of surface temperature (K/s)
c        tsurf               surface temperature (K)
c        ptpt                time-rate change of 3-D temperature (K/s)
c        tempk               3-D temperature (K)
c        pwpt                time-rate change of 3-D water vapor (ppm/s)
c        water               3-D water vapor (ppm)
c        pkpt                time-rate change of 3-D diffusivity (m2/s2)
c        rkv                 3-D diffusivity (m2/s)
c        conc                concentration arrray (umol/m3)
c             
c     Output arguments:  
c        height              layer interface height (m)
c        depth               layer depth (m) 
c        press               presssure (mb)
c        windu               u-component wind field (m/s)
c        windv               v-component wind field (m/s)
c        tsurf               surface temperature (K)
c        tempk               3-D temperature (K)
c        water               3-D water vapor (ppm)
c        rkv                 3-D diffusivity (m2/s)
c        conc                concentration arrray (umol/m3)
c        saconc              RTRAC concentration arrray
c        adjwest             adjustment factors for the West boundary
c        adjeast             adjustment factors for the East boundary
c        adjsouth            adjustment factors for the South boundary
c        adjnorth            adjustment factors for the North boundary
c             
c     Routines Called:  
c        none 
c             
c     Called by:  
c        EMISTRNS  
c 
      include 'camx.prm'
c
      integer :: m1,m2,m3,i0,j0,ia,iz,ja,jz,ibcon
c
      real, dimension(m1,m2,m3) :: depth, height, phpt

      real, dimension(m1,m2,m3) :: press, pppt, windu, pupt, windv, 
     &                             pvpt, tempk, ptpt, pwpt, water, 
     &                             pkpt, rkv
      real conc(m1,m2,m3,nspec)
      real saconc(m1,m2,m3,nrtspc)
      real adjwest(MXCELLS,MXLAYER)
      real adjeast(MXCELLS,MXLAYER)
      real adjsouth(MXCELLS,MXLAYER)
      real adjnorth(MXCELLS,MXLAYER)
      real :: tsurf(m1,m2), pspt(m1,m2)
      logical, external :: isbound
c
c-----Entry point 
c
c-----Initialize the adjusting factors to 1.0 ---
c
      do i=1,MXCELLS
         do k=1,MXLAYER
           adjwest(i,k) = 1.0
           adjeast(i,k) = 1.0
           adjsouth(i,k) = 1.0
           adjnorth(i,k) = 1.0
         enddo
      enddo
c
c-----Convert master grid boundary concs from umol/m3 to ppm
c
      if (igrd.eq.1) then
         do j = 1,m2
            do 10 i = 1,m1
               if( i .EQ. 1 .AND. j .EQ. 1 ) goto 10
               if( i .EQ. 1 .AND. j .EQ. m2 ) goto 10
               if( i .EQ. m1 .AND. j .EQ. 1 ) goto 10
               if( i .EQ. m1 .AND. j .EQ. m2 ) goto 10
               if (isbound(ibcon,m1,m2,i,j) ) then
                  do k = 1,nlay
                     convfac = densfac*273./tempk(i,j,k)*press(i,j,k)/1013.
                     do l = 1,ngas
                        conc(i,j,k,l) = conc(i,j,k,l)/convfac
                     enddo
                     if( i .eq. 1 ) then
                        adjwest(j,k) = 1.0/convfac
                     endif
                     if( i .eq. m1 ) then
                        adjeast(j,k) = 1.0/convfac
                     endif
                     if( j .eq. 1 ) then
                        adjsouth(i,k) = 1.0/convfac
                     endif
                     if( j .eq. m2 ) then
                        adjnorth(i,k) = 1.0/convfac
                     endif
                     if( tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC ) then
                        do l = 1,nrtgas
                           saconc(i,j,k,l) = saconc(i,j,k,l)/convfac
                        enddo
                     endif
                  enddo
               endif
   10        continue
         enddo
      endif
c
c-----Update other parameters forward 1 timestep
c
      do 60 k = 1,nlay
        do 50 j = 1,m2  !nrow
          do 40 i = 1,m1    !ncol
            height(i,j,k) = height(i,j,k) + deltat*phpt(i,j,k)
            depth(i,j,k) = height(i,j,k)
            if (k.gt.1) depth(i,j,k) = 
     &                          height(i,j,k) - height(i,j,k-1)
            press(i,j,k) = press(i,j,k) + deltat*pppt(i,j,k)
            if (i+i0.lt.ncol .and. j+j0.lt.nrow) then
              windu(i,j,k) = windu(i,j,k) + deltat*pupt(i,j,k)
              windv(i,j,k) = windv(i,j,k) + deltat*pvpt(i,j,k)
            endif
            tempk(i,j,k) = tempk(i,j,k) + deltat*ptpt(i,j,k)
            water(i,j,k) = water(i,j,k) + deltat*pwpt(i,j,k)
            water(i,j,k) = amax1(water(i,j,k),1.)
            rkv(i,j,k)   = rkv(i,j,k) + deltat*pkpt(i,j,k)
            if (k.eq.1) tsurf(i,j) = tsurf(i,j) + deltat*pspt(i,j)
c
 40       continue
 50     continue
 60   continue
c
c-----Convert master grid boundary concs from ppm to umol/m3
c 
      if (igrd.eq.1) then
         do j = 1,m2
            do 20 i = 1,m1
               if( i .EQ. 1 .AND. j .EQ. 1 ) goto 20
               if( i .EQ. 1 .AND. j .EQ. m2 ) goto 20
               if( i .EQ. m1 .AND. j .EQ. 1 ) goto 20
               if( i .EQ. m1 .AND. j .EQ. m2 ) goto 20
               if (isbound(ibcon,m1,m2,i,j) ) then
                  do k = 1,nlay
                     convfac = densfac*273./tempk(i,j,k)*press(i,j,k)/1013.
                     do l = 1,ngas
                        conc(i,j,k,l) = conc(i,j,k,l)*convfac
                     enddo
                     if( i .eq. 1 ) then
                        adjwest(j,k) = adjwest(j,k) * convfac
                     endif
                     if( i .eq. m1 ) then
                        adjeast(j,k) = adjeast(j,k) * convfac
                     endif
                     if( j .eq. 1 ) then
                        adjsouth(i,k) = adjsouth(i,k) * convfac
                     endif
                     if( j .eq. m2 ) then
                        adjnorth(i,k) = adjnorth(i,k) * convfac
                     endif
                     if( tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) then
                        do l = 1,nrtgas
                           saconc(i,j,k,l) = saconc(i,j,k,l)*convfac
                        enddo
                     endif
                  enddo
               endif
   20        continue
         enddo
      endif
c
      return
      end

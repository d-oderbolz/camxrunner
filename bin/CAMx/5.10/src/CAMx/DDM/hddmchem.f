      subroutine hddmchem(hddmjac,ebirxn,ebirate,
     &                    nreact,nrxn,nspc,nrad,njac,
     &                    nfam,nrfam,nhfam,ipr,iph,
     &                    dt,H2O,atm,O2,CH4,H2,
     &                    rk,avgcnc,avgrad,sddm,sraddm,ierr)
c
c----CAMx v5.10 090918
c
c     HDDMCHEM advances the 1st and 2nd order sensitivty coefficients
c     one chemistry time step. This follows the CMAQ DDM-3D approach.
c
c     Copyright 1996-2009
c     ENVIRON International Corporation
c
c     History:
c        07/16/07   --bkoo--       Original development
c        06/11/08   --bkoo--       Added rate constant sensitivity
c
c     Input arguments:
c        hddmjac           name of subroutine to calc full Jacobian
c        ebirxn            name of subroutine to calculate reaction rates
c        ebirate           name of subroutine to calculate species rates
c        nreact            maximum number of reactions
c        nrxn              number of reactions
c        nspc              number of state species
c        nrad              number of radical species
c        njac              nrad + nspc
c        nfam              number of sensitivity families
c        nrfam             number of rate constant sensitivity families
c        nhfam             number of 2nd order sensitivity families
c        ipr               pointer array for rxns in rate constant groups;
c                            ipr(0,i) stores the number of rxns in the i-th group
c        iph               pointer array for 1st order sensitivity families
c                            to which 2nd order sensitivity is computed
c        dt                chemistry time step (hr)
c        H2O               water vapor concentration (ppm)
c        atm               total gas concentration (M) in ppm
c        O2                oxygen concentration (ppm)
c        CH4               methane concentration (ppm)
c        H2                hydrogen concentration (ppm)
c        rk                reaction rate constant (ppm/hr)
c        avgcnc            average state species concentrations (ppm)
c        avgrad            average radical species concentrations (ppm)
c        sddm              sensitivity matrix sddm (ppm)
c        sraddm            radical sensitivity matrix sraddm (ppm)
c
c     Output arguments:
c        sddm              updated sensitivity matrix
c        sraddm            updated radical sensitivity matrix
c        ierr              error flag from LU decomposition
c
c     Routines called:
c        HDDMJAC
c        EBIRXN
c        EBIRATE
c        SGEFA
c        SGESL
c
c     Called by:
c        CHEMDRIV
c
      implicit none

      integer  nreact,nrxn,nspc,nrad,njac,nfam,nrfam,nhfam,ierr
      integer  ipr(0:nreact,nrfam),iph(2,nhfam)
      real     dt,H2O,atm,O2,CH4,H2
      real     rk(nrxn),avgcnc(nspc+1),avgrad(nrad)
      real     sddm(nfam,nspc),sraddm(nfam,nrad)

      external hddmjac,ebirxn,ebirate

      real     eps
      parameter ( eps = 1.e-25 )

      real     ymid(njac+1),rjac(njac+1,njac+1)
      real     aa(njac,njac),a1(njac,njac),bb(njac)
      real     sen1d(njac+1),smid(nfam,njac)
      real     rrxn(nrxn),rtmp(nrxn)
      real     gain(njac+1),loss(njac+1),prod(njac)
      real     flg

      integer  ipvt(njac)
      integer  i,j,n,ip,ipz,ipx,ip1,ip2,iptmp1,iptmp2
c
c---  Entry point
c
      ierr = 0
c
c---  Load concentrations
c
      do i = 1, nrad
        ymid(i) = avgrad(i)
      enddo
      do i = 1, nspc
        ymid(i+nrad) = avgcnc(i)
      enddo
      ymid(njac+1) = 0.0
c
c---  Get the Jacobian
c
      flg = 1.0   ! keep (effective) 1st-order rxns
      rjac = 0.0  ! initialize the Jacobian
      call hddmjac(njac,nrxn,H2O,atm,O2,CH4,H2,flg,rjac,ymid,rk)
c
c---  Fill the coeff matrix
c
      do i = 1, njac
        do j = 1, njac
          aa(i,j) = -0.5 * dt * rjac(i,j)
          a1(i,j) = -aa(i,j)
        enddo
        aa(i,i) = aa(i,i) + 1.0
        a1(i,i) = a1(i,i) + 1.0
      enddo
c
c---  Factor the coeff matrix
c
      call sgefa(aa,njac,njac,ipvt,ierr)
      if (ierr.ne.0) goto 990 ! zero determinant
c
c---  Fill the rxn rate vector if rate const sens to be computed
c
      if ( nrfam.gt.0 ) then
        call ebirxn(njac,nrxn,ymid,H2O,atm,O2,CH4,H2,rk,rrxn)
      endif
c
c---  Solve for 1st order sensitivities
c
      ipz = nfam - nrfam - nhfam

      do ip = 1, nfam - nhfam

        do i = 1, nrad
          sen1d(i) = sraddm(ip,i)
        enddo
        do i = 1, nspc
          sen1d(i+nrad) = sddm(ip,i)
        enddo
        do i = 1, njac
          if ( ABS(sen1d(i)) .LT. eps ) sen1d(i) = 0.0
        enddo

        prod = 0.0
        if ( ip .gt. ipz ) then ! rate constant sens
          gain = 0.0
          loss = 0.0
          rtmp = 0.0
          ipx = ip - ipz
          do j = 1, ipr(0,ipx)
            rtmp( ipr(j,ipx) ) = rrxn( ipr(j,ipx) )
          enddo
          call ebirate(njac,nrxn,rtmp,gain,loss)
          do i = 1, njac
            prod(i) = ( gain(i) - loss(i) ) * dt
          enddo
        endif

        do i = 1, njac
          bb(i) = 0.0
          do j = 1, njac
            bb(i) = bb(i) + a1(i,j) * sen1d(j)
          enddo
          bb(i) = bb(i) + prod(i)
        enddo

        call sgesl(aa,njac,njac,ipvt,bb,0)

        do i = 1, njac
          if ( ABS(bb(i)) .LT. eps ) bb(i) = 0.0
          smid(ip,i) = 0.5 * (sen1d(i) + bb(i))
        enddo
        do i = 1, nrad
          sraddm(ip,i) = bb(i)
        enddo
        do i = 1, nspc
          sddm(ip,i) = bb(i+nrad)
        enddo

      enddo
c
c---  Solve for 2nd order sensitivities
c
      do ip = nfam - nhfam + 1, nfam

        ip1 = iph(1, ip - nfam + nhfam)
        ip2 = iph(2, ip - nfam + nhfam)

        prod = 0.0
        iptmp1 = ip1
        iptmp2 = ip2
        do n = 1, 2 ! loop for two 1st-order sens parameters
          if ( iptmp1 .gt. ipz ) then ! rate constant sens
            rtmp = 0.0
            ipx = iptmp1 - ipz
            do j = 1, ipr(0,ipx)
              rtmp( ipr(j,ipx) ) = rk( ipr(j,ipx) )
            enddo
            flg = 1.0   ! keep (effective) 1st-order rxns
            rjac = 0.0  ! initialize the Jacobian
            call hddmjac(njac,nrxn,H2O,atm,O2,CH4,H2,flg,rjac,ymid,rtmp)
            do i = 1, njac
              do j = 1, njac
                prod(i) = prod(i) + rjac(i,j) * smid(iptmp2,j)
              enddo
            enddo
            if ( iptmp1.eq.iptmp2 ) then
              prod = 2.0 * prod
              EXIT
            endif
          endif
          iptmp1 = ip2
          iptmp2 = ip1
        enddo

        do i = 1, njac
          sen1d(i) = smid(ip1,i)
        enddo
        sen1d(njac+1) = 0.0

        flg = 0.0   ! remove (effective) 1st-order rxns
        rjac = 0.0  ! initialize the Jacobian
        call hddmjac(njac,nrxn,H2O,atm,O2,CH4,H2,flg,rjac,sen1d,rk)

        do i = 1, nrad
          sen1d(i) = sraddm(ip,i)
        enddo
        do i = 1, nspc
          sen1d(i+nrad) = sddm(ip,i)
        enddo
        do i = 1, njac
          if ( ABS(sen1d(i)) .LT. eps ) sen1d(i) = 0.0
        enddo

        do i = 1, njac
          bb(i) = 0.0
          do j = 1, njac
            bb(i) = bb(i) + a1(i,j) * sen1d(j)
            prod(i) = prod(i) + rjac(i,j) * smid(ip2,j)
          enddo
          bb(i) = bb(i) + dt * prod(i)
        enddo

        call sgesl(aa,njac,njac,ipvt,bb,0)

        do i = 1, njac
          if ( ABS(bb(i)) .LT. eps ) bb(i) = 0.0
        enddo
        do i = 1, nrad
          sraddm(ip,i) = bb(i)
        enddo
        do i = 1, nspc
          sddm(ip,i) = bb(i+nrad)
        enddo

      enddo
c
c---  Return point
c
990   return
      end

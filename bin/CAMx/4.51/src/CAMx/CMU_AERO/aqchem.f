      subroutine aqchem(gas,aerosol,rh,press,temp,
     &                                lwc_c,t1,dt,cuts,ierr)
      include 'aerpar.inc'
      include 'droppar.inc'
      include 'dropcom.inc'
      real*4 gas(ngas_aq), aerosol(nsect,naers)   
      real*4 gasav(ngas_aq), aerosav(nsect,naers)   
      real*4 rh,temp,press,lwc_c,t0,t1,dt,p
      real*8 cuts(nsect+1)
      real*4 cut(nsect+1)
      logical lfrst
      save sulfbefppm,sulfbef0
      Data lfrst /.true./
c
c      iseed=43
c      do i=1,64
c        call randnumgen(iseed,randx)
c        call randnumgen(iseed,randx)
c      enddo
c
c SET MAIN PROGRAM VARIABLES
c
cgem      write(*,*) 'lwc, t1,dt :',lwc_c,t1,dt
      call flush(6)

      istep = 1                      ! print counter
      t0_s = t1 - dt
      t1_s = t1
cgem      t0_s=t1
cgem      t1_s=t1+dt
      istart = int(t0)                     ! beginning of simulation in min
      iend =  int(t1)                     ! end of simulation in min
      istart_s=istart
      iend_s=iend
      deltat= dt                    ! operator timestep in min
      p= press                         ! pressure in atm
      if ( lfrst ) then
       lfrst = .false.
        iaq = 1                        ! first call flag
        sulfbef0 = 0.0
        do i=1, nsect
          sulfbef0 = sulfbef0 + aerosol(i,na4)*32./96.
     &      + aerosol(i,nahso5)*32./113.+aerosol(i,nahmsa)*32./111.
        enddo
        sulfbef0 = sulfbef0 + gas(ngso2)*32.*p/(8.314e-5*temp)
        sulfbefppm = 0.0
        do i=1, nsect
          sulfbefppm= sulfbefppm+aerosol(i,na4)
        enddo
        sulfbefppm=sulfbefppm*0.0243113/96.+gas(ngso2)
c
        do isect=1,nsect+1
           cut(isect)=SNGL(cuts(isect))
        enddo
c
c  set up fdist and fdist2
c
        call aqdist(nsect,cut,fdist,fdist2) 
c
cgem        write(*,*) (cut(isect),isect=1,nsect+1)
cgem        write(*,*) (fdist(isect),isect=1,nsect)
cgem        write(*,*) (fdist2(isect),isect=1,nsect)
c
      endif
c
      do isect=1,nsect
        do isp=1,naers
          aerosav(isect,isp) = aerosol(isect,isp)
        enddo
      enddo
      do i=1,ngas_aq
        gasav(i) = gas(i)
      enddo
c
      so2init=gas(ngso2)
      h2o2init=gas(ngh2o2)
c
c     CALCULATION OF TOTAL SULFUR MASS BEFORE THE CALL
c
      sulfbef = 0.0
      do i=1, nsect
      sulfbef = sulfbef + aerosol(i,na4)*32./96.
     &  + aerosol(i,nahso5)*32./113.+aerosol(i,nahmsa)*32./111.
      enddo
      sulfbef = sulfbef + gas(ngso2)*32.*p/(8.314e-5*temp)
c
 560  call vsrm(gas, aerosol, lwc_c, t0, t1, deltat,
     &          temp, iaq, p, rh)
c
c     CALCULATION OF TOTAL SULFUR MASS AFTER THE CALL
c
      sulfaf = 0.0
      sulfafppm = 0.0
      do i=1, nsect
      sulfafppm= sulfafppm+aerosol(i,na4)
      sulfaf = sulfaf + aerosol(i,na4)* 32./96.
     &  + aerosol(i,nahso5)*32./113.+aerosol(i,nahmsa)*32./111.
      enddo
      sulfafppm=sulfafppm*0.0243113/96.+gas(ngso2)
      sulfaf = sulfaf+gas(ngso2)*32.*p/(8.314e-5*temp)
cgy
c try an S balance patch
c
      sbal = sulfaf/sulfbef
      sbalppm = sulfafppm/sulfbefppm
cgem      if (sulfbef.gt. 0.1 .and.
cgem     &    (sbal.lt.0.99 .or. sbal.gt.1.01) ) then
cgemcgy
cgem        do isect=1,nsect
cgem          do isp=1,naers
cgem            aerosol(isect,isp) = aerosav(isect,isp)
cgem          enddo
cgem        enddo
cgem        do i=1,ngas_aq
cgem          gas(i) = gasav(i)
cgem        enddo
cgem        t0=t0_s
cgem        t1=t1_s
cgem        istart = istart_s
cgem        iend = iend_s
cgem        deltat = deltat/2.
cgemcgem
cgem        write(*,*) ' Cutting dt ....',deltat
cgemcgem
cgem        goto 560
cgem      endif
c
      return
      end

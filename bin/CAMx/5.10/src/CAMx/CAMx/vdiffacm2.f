      subroutine vdiffacm2(ns,nz,deltat,temp0,press0,z0,delz,rkv,qv,
     &                     qc,temp,press,wind,vdep,con,fluxbot)
c
c----CAMx v5.10 090918
c
c     VDIFFACM2 performs vertical diffusion of concentrations using
c     the Asymmetric Convective Model v2 (ACM2; Pleim, 2007). 
c
c     ACM2 portion of code is based on: 
c         Models-3/CMAQ vdiffacm2.F,v 1.4 2006/07/26 19:39:10 
c         with patch from http://bugz.unc.edu/show_bug.cgi?id=2411
c
c     Copyright 2008
c     ENVIRON International Corporation
c
c     Modifications:
c       None 
c
c     Input arguments:
c        ns                number of species
c        nz                number of layers
c        deltat            timestep (s)
c        temp0             Surface temperature (K)
c        press0            Surface pressure (mb)
c        z0                Surface roughness (m)
c        delz              Layer depth (m)
c        rkv               Kv profile (m2/s)
c        qv                Humidity profile (mixing ratio) 
c        qc                Cloud water profile (mixing ratio)
c        temp              Temperature profile (K)
c        press             Pressure profile (mb)
c        wind              Wind speed profile
c        vdep              dry deposition velocity (m/s)
c        con               Species concentrations (umol/m3, ug/m3)
c
c     Output arguments:
c        con               Species concentrations (umol/m3, ug/m3)
c        fluxbot           Concentration removed by dry deposition (umol/m3)
c
c     Routines Called:
c        MICROMET
c        MATRIX
c        TRI
c
c     Called by:
c        DIFFUS
c
      implicit none
      include "camx.prm"
c
      integer ns,nz
      real deltat,temp0,press0,z0
      real delz(nz),rkv(nz),qv(nz),qc(nz),temp(nz),press(nz),
     &     wind(nz),vdep(ns),con(MXLAYER+MXLAYER*MXTRSP,ns)
      real*8 fluxbot(ns)
c
      real mu
      parameter (mu = 0.5)
      logical lconv,lstable
      integer k,kk,kpbl,nstep,n,l
      real gamma,vk
      real tv,pbl,zf,critk,ustar,el,psih,wstar,hoverl,
     &     dt,mbar,meddy,fconv,rz,dtacm,mfac,delc,xplus,xminus,
     &     lfac1,lfac2,dep,rhopbl,rhosum,roplus
      real zz(0:MXLAYER)
      real dz(MXLAYER),dzinv(MXLAYER),zm(MXLAYER),thetav(MXLAYER),
     &     rho(MXLAYER),dzm(MXLAYER),dzminv(MXLAYER),rkz(MXLAYER)
      real mbarks(MXLAYER),mdwn(MXLAYER)
      real aa(MXLAYER),bb(MXLAYER),cc(MXLAYER),ee(MXLAYER),dd(MXLAYER),
     &     uu(MXLAYER)
c
      data gamma /0.286/, vk /0.4/
c
c-----Entry point
c
c-----Determine vertical grid structure variables and thermodynamic profiles.
c
      zz(0) = 0.
      do k = 1,nz
        dz(k) = delz(k)
        zz(k) = zz(k-1) + dz(k)
        zm(k)  = (zz(k) + zz(k-1))/2.
        tv = temp(k)*(1. + 0.608*qv(k))
        thetav(k) = tv*(1000./press(k))**gamma
        rho(k) = press(k)/tv
        rkz(k) = rkv(k)
      enddo
      do k = 1,nz-1
        dzm(k) = zm(k+1) - zm(k)
      enddo
      dzm(nz) = 0.

      do l = 1,ns
        do k = 1,nz
          con(k,l) = con(k,l)/rho(k)
        enddo
      enddo
c
c-----Get the PBL parameters
c
      kpbl = 1
      pbl = dz(1)
      do k = 2,nz-1
        if (kpbl .ne. k-1) goto 100
        zf = dz(k)/dz(k-1)
        critk = 0.03*dz(k-1)*dzm(k)*(1. + zf)/200.
        if (rkv(k-1) .gt. critk) then
          kpbl = k
          pbl = pbl + dz(k)
        endif
      enddo
 100  continue

      call micromet(temp(1),temp0,press(1),press0,dz(1),wind(1),z0,pbl,
     &              ustar,el,psih,wstar,lstable)

      lconv = .false.
      hoverl = pbl/el
      if (((thetav(1) - thetav(2)) .gt. 1.e-8 ) .and.
     &    (hoverl .lt. -0.1) .and.
     &    (kpbl .gt. 3)) lconv = .true.
c
c-----Initialize ACM2 arrays and determine mixing sub-step
c
      dt = deltat
      do l = 1,ns
        dt = min(dt,0.5*dz(1)/vdep(l))
        fluxbot(l) = 0.
      enddo
      do k = 1,nz - 1
        dt = min(dt,0.75*dz(k)*dzm(k)/rkz(k))
        mbarks(k) = 0.
        mdwn(k) = 0.
      enddo
      mdwn(nz) = 0.
c
c-----Calculate ACM2 non-local mixing rates
c
      do k = 1,nz
        dz(k) = dz(k)*rho(k)
        dzinv(k) = 1./dz(k)
      enddo
      do k = 1,nz-1
        roplus = (rho(k)+rho(k+1))/2.
        dzm(k) = dzm(k)*roplus
        dzminv(k) = 1./dzm(k)
        rkz(k) = rkz(k)*roplus*roplus*dzminv(k)
      enddo
      dzminv(nz) = 0.

      mbar = 0.
      if (lconv) then
        rhopbl = 0.
        do k = 2,kpbl
          rhopbl = rhopbl + dz(k)
        enddo
        meddy = rkz(1)/rhopbl
        fconv = 1./(1. + ((vk/(-hoverl))**0.3333)/(0.72*vk))
        mbar = meddy*fconv
        do k = 1,kpbl-1
          rkz(k) = rkz(k)*(1. - fconv)
          rhosum = 0.
          do kk = k,kpbl
            rhosum = rhosum + dz(kk)
          enddo
          mbarks(k) = mbar
          mdwn(k) = mbar*rhosum*dzinv(k)
        enddo
        mbarks(kpbl) = mbar*dz(kpbl)*dzinv(kpbl)
        mdwn(kpbl) = mbarks(kpbl)
      else
        kpbl = 1
      endif
c
c-----Determine number of steps to take
c
      nstep = int(deltat/dt + 0.99)
      dt   = deltat/nstep
c
c-----Loop over species
c
      do 301 l = 1,ns
c
c-----Loop over sub-steps
c
        do 300 n = 1,nstep
c
c-----Remove dry deposited mass
c
          dep = con(1,l)*vdep(l)*dt/delz(1)
          con(1,l) =  con(1,l) - dep
          fluxbot(l) = fluxbot(l) - dep*rho(1)
c
c-----Initialize matrix elements
c
          do k = 1,nz
            aa(k) = 0.
            bb(k) = 0.
            cc(k) = 0.
            ee(k) = 0.
            dd(k) = 0.
            uu(k) = 0.
          enddo
c
c-----Load matrix elements for convective boundary layer
c
          if (lconv) then
            do k = 2,kpbl
              aa(k) = -mu*mbarks(k)*dt
              bb(k) = 1. + mu*mdwn(k)*dt
              ee(k-1) = -mu*mdwn(k)*dt*dz(k)*dzinv(k-1)
              mfac = dz(k+1)*dzinv(k)*mdwn(k+1)
              delc = dt*(mbarks(k)*con(1,l) 
     &               - mdwn(k)*con(k,l) 
     &               + mfac*con(k+1,l))
              dd(k) = con(k,l) + (1.-mu)*delc
            enddo
          endif
c
c-----Load matrix elements for K-theory
c
          aa(2) = aa(2) - rkz(1)*mu*dzinv(2)*dt
          ee(1) = ee(1) - rkz(1)*mu*dzinv(1)*dt
          do k = 2,nz
            if (k .gt. kpbl) then
              bb(k) = 1.
              dd(k) = con(k,l)
            endif
            xplus  = rkz(k)*dzinv(k)*dt
            xminus = rkz(k-1)*dzinv(k)*dt
            bb(k) = bb(k) + (xplus + xminus)*mu
            cc(k) = -xminus*mu
            ee(k) = ee(k) - xplus*mu
            if (k .eq. nz) then
              dd(k) = dd(k) - (1. - mu)*xminus*
     &                        (con(k,l) - con(k-1,l))
            else
              lfac1 = (1. - mu)*xplus
              lfac2 = (1. - mu)*xminus
              dd(k) = dd(k) + lfac1*(con(k+1,l) - con(k,l))
     &                      - lfac2*(con(k,l) - con(k-1,l))
            endif
          enddo
          bb(1) = 1.
          dd(1) = con(1,l)
c
c-----Load matrix elements for convective surface layer
c
          if (lconv) then
            lfac1 = rhopbl*dzinv(1)*dt
            lfac2 = (1. - mu)*mdwn(2)*dz(2)*dzinv(1)*dt
            bb(1) = bb(1) + mu*mbarks(1)*lfac1
            lfac1 = (1. - mu)*mbarks(1)*lfac1
            dd(1) = dd(1) - lfac1*con(1,l) + lfac2*con(2,l)
          endif
c
c-----Load matrix elements for K-theory surface layer
c
          bb(1) = bb(1) + mu*rkz(1)*dzinv(1)*dt
          lfac1 = (1. - mu)*rkz(1)*dzinv(1)*dt
          dd(1) = dd(1) + lfac1*(con(2,l) - con(1,l))
c
c-----Call solvers (MATRIX for ACM2, TRI for K-theory)
c
          if (lconv) then
            call matrix(nz,aa,bb,cc,dd,ee,uu)
          else
            call tri(nz,cc,bb,ee,dd,uu)
          endif
          do k = 1,nz
            con(k,l) = uu(k)
          enddo
              
300     continue

        do k = 1,nz
          con(k,l) = con(k,l)*rho(k)
        enddo

301   continue

      return
      end
c
c-------------------------------------------------------------------------------
c
      SUBROUTINE MATRIX ( nz, A, B, C, D, E, X )
c
c This routine taken from:
c Models-3/CMAQ matrix.F,v 1.4 2006/07/24 14:11:56
c
C---------------------------------------------------------
C-- Bordered band diagonal matrix solver for ACM2
C
C-- ACM2 Matrix is in this form:
C   B1 E1
C   A2 B2 E2
C   A3 C3 B3 E3
C   A4    C4 B4 E4
C   A5       C5 B5 E5
C   A6          C6 B6
C
C--Upper Matrix is
C  U11 U12
C      U22 U23
C          U33 U34
C              U44 U45
C                  U55 U56
C                      U66
C
C--Lower Matrix is:
C  1
C L21  1
C L31 L32  1
C L41 L42 L43  1
C L51 L52 L53 L54  1
C L61 L62 L63 L64 L65 1
C---------------------------------------------------------

      IMPLICIT NONE
      include "camx.prm"

C...Arguments

      integer nz
      REAL A( nz )
      REAL B( nz )
      REAL C( nz )
      REAL E( nz )
      REAL D( nz )
      REAL X( nz )

C...Locals

      INTEGER NLAYS
      REAL Y( MXLAYER )
      REAL L( MXLAYER,MXLAYER )
      REAL U( MXLAYER )
      REAL UP1( MXLAYER )
      REAL RU( MXLAYER )
      REAL DD, DD1, YSUM
      INTEGER I, J

      NLAYS = nz

C-- Define Upper and Lower matrices

      L( 1,1 ) = 1.0
      U( 1 ) = B( 1 )
      RU( 1 ) = 1.0 / B( 1 )

      DO I = 2, NLAYS
         L( I,I ) = 1.0
         L( I,1 ) = A( I ) / B( 1 )
         UP1( I-1 ) = E( I-1 )
      END DO

      DO I = 3, NLAYS
         DO J = 2, I - 2
            DD = B( J ) - L( J,J-1 ) * E( J-1 )
            L( I,J ) = - L( I,J-1 ) * E( J-1 ) / DD
         END DO
         J = I - 1
         DD = B( J ) - L( J,J-1 ) * E( J-1 )
         L( I,J ) = ( C( I ) - L( I,J-1 ) * E( J-1 ) ) / DD
      END DO

      DO I = 2, NLAYS
         U( I ) = B( I ) - L( I,I-1 ) * E( I-1 )
         RU( I ) = 1.0 / U( I )
      END DO

C-- Forward sub for Ly=d

      Y( 1 ) = D( 1 )
      DO I = 2, NLAYS
         YSUM = D( I )
         DO J = 1, I-1
            YSUM = YSUM - L( I,J ) * Y( J )
         END DO
         Y( I ) = YSUM
      END DO

C-- Back sub for Ux=y

      X( NLAYS ) = Y( NLAYS ) * RU( NLAYS )

      DO I = NLAYS - 1, 1, -1
         DD = RU( I )
         DD1 = UP1( I )
         X( I ) = ( Y( I ) - DD1 * X( I+1 ) ) * DD
      END DO

      RETURN
      END
c
c-------------------------------------------------------------------------------
c
      SUBROUTINE TRI ( nz, L, D, U, B, X )
c
c This routine taken from:
c Models-3/CMAQ tri.F,v 1.3 2006/07/18 17:58:44
c
C-----------------------------------------------------------------------
C  FUNCTION:
C    Solves tridiagonal system by Thomas algorithm.  Algorithm fails
C    ( M3ERR ) if first pivot is zero.  In that case, rewrite the
C    equation as a set of order KMAX-1, with X(2) trivially eliminated.
C The associated tri-diagonal system is stored in 3 arrays
C   D : diagonal
C   L : sub-diagonal
C   U : super-diagonal
C   B : right hand side function
C   X : return solution from tridiagonal solver
C
C     [ D(1) U(1) 0    0    0 ...       0     ]
C     [ L(2) D(2) U(2) 0    0 ...       .     ]
C     [ 0    L(3) D(3) U(3) 0 ...       .     ]
C     [ .       .     .     .           .     ] X(i) = B(i)
C     [ .             .     .     .     0     ]
C     [ .                   .     .     .     ]
C     [ 0                           L(n) D(n) ]
C
C   where n = NLAYS
C
C-----------------------------------------------------------------------

      IMPLICIT NONE
      include "camx.prm"

C Arguments:

      integer nz
      REAL        L( nz )               ! subdiagonal
      REAL        D( nz )               ! diagonal
      REAL        U( nz )               ! superdiagonal
      REAL        B( nz )               ! R.H. side
      REAL        X( nz )               ! solution

C Local Variables:

      INTEGER     NLAYS
      REAL        GAM( MXLAYER )
      REAL        BET
      INTEGER     K

      NLAYS = nz

C Decomposition and forward substitution:
      BET = 1.0 / D( 1 )
      X( 1 ) = BET * B( 1 )

      DO K = 2, NLAYS
         GAM( K ) = BET * U( K-1 )
         BET = 1.0 / ( D( K ) - L( K ) * GAM( K ) )
         X( K ) = BET * ( B( K ) - L( K ) * X( K-1 ) )
      END DO

C Back-substitution:

      DO K = NLAYS - 1, 1, -1
        X( K ) = X( K ) - GAM( K+1 ) * X( K+1 )
      END DO

      RETURN
      END

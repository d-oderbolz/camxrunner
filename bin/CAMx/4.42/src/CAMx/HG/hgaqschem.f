      SUBROUTINE HGAQSCHEM ( CHG0, CHG2, CO3, CSO2, COH, CHO2, CHCL,
     &                       CCL2, CPM, CPH, LWC, TEMP, PRES, DELT,
     &                       SO2H, SO2TMP, O3H, O3TMP )

C
c----CAMx v4.42 070603
C
C   HGAQSCHEM performs the Hg aqueous-phase chemistry calculations.
C
C     Copyright 2003
C     AER, Inc.
C 
C Revision History: 
C***********************************************************************
C   Version 1.0 written March 2003 by Prakash Karamchandani, AER       *
C   Code updated August 2003 to use Henry's Law and SO2 dissociation   *
C   constants consistent with those used in the CAMx aqueous-phase     *
C   chemistry module                                                   *
C*********************************************************************** 
C
C  Called by:  CHEMDRIV
C
C*********************************************************************** 

      IMPLICIT NONE

C Includes:

      INCLUDE 'hgaqschm.prm'  ! Hg aqueous-phase chemistry parameters

C Arguments:

      REAL  CHG0      ! Hg(0) concentration, ppm (Input/Output)
      REAL  CHG2      ! Hg(2) concentration, ppm (Input/Output)

      REAL  CO3       ! O3 concentration, ppm (Input only)
      REAL  CSO2      ! SO2 concentration, ppm (Input only)
      REAL  COH       ! OH concentration, ppm (Input only)
      REAL  CHO2      ! HO2 concentration, ppm (Input only)
      REAL  CHCL      ! HCL concentration, ppm (Input only)
      REAL  CCL2      ! CL2 concentration, ppm (Input only)
      REAL  CPM       ! Total PM concentration, ug/m3 (Input only)

      REAL  CPH       ! Cloudwater pH (Input only)
      REAL  LWC       ! Liquid water content, g/m3 (Input only)
      REAL  TEMP      ! Temperature, K (Input only)
      REAL  PRES      ! Pressure, mb (Input only)
      REAL  DELT      ! Chemistry time step, seconds (Input only)

      REAL SO2H       ! Henry's Law constant for SO2, M/atm (Input only)
      REAL O3H        ! Henry's Law constant for O3, M/atm (Input only)
      REAL SO2TMP     ! SO2 Henry's Law temperature dependence (Input only)
      REAL O3TMP      ! O3 Henry's Law temperature dependence (Input only)

! Local variables

! Standard atmosphere in mb
      REAL        STDATMMB
      PARAMETER ( STDATMMB = 1013.25 )

! Pressure in atmospheres
      REAL PRES_ATM

! Conv. factor from ppm to atmospheres
      REAL  CFACT

! Conversion factor (M to atm), liter-atm/mole
      REAL  XL

! Henry's Law constant for OH and HO2, M/atm
      REAL OHH, HO2H

! Temperature in K and Centigrade and inverse temperature (K-1)
      REAL  TEMPK, TEMPC, TEMPI

! Double precision local variables (for accuracy):

      DOUBLE PRECISION  Y00   ! Initial total Hg(0) partial pressure, atm
      DOUBLE PRECISION  Y20   ! Initial total Hg(2) partial pressure, atm

      DOUBLE PRECISION  Z0T   ! Final total Hg(0) partial pressure, atm
      DOUBLE PRECISION  Z2T   ! Final total Hg(2) partial pressure, atm

! --- Local concentration array
      DOUBLE PRECISION  Y( NUMSP )
!     Y(1)         [Hg(0)]-Gaseous (variable), atm
!     Y(2)         [Hg(2)]-Gaseous (variable), atm
!     Y(3)         [O3]-Gaseous (constant), atm
!     Y(4)         [Cl2]-Gaseous (constant), atm
!     Y(5)         [HCl]-Gaseous (constant), atm
!     Y(6)         [SO2]-Gaseous (constant), atm
!     Y(7)         [OH]-Aqueous (constant), M
!     Y(8)         [HO2]-Aqueous (constant), M

      DOUBLE PRECISION  PM  ! PM10 conc. (g/L)

! --- Intermediate variables for calculating Hg(2) complexes with SO2
! --- in solution and their reaction rates
      DOUBLE PRECISION  CON1, CON2, CON3, CON4, CON51, CON52

! --- Intermediate variables for calculating Hg(2) concentrations in
! --- solution as Hg(2)2+, HgCl2 and Hg(OH)2 and total Hg(2)
      DOUBLE PRECISION  CON5, CON6, CON7, TOTHG

! --- Aqueous concentrations (M) of dissociation products of Cl2,
! --- HOCL and OCL-
      DOUBLE PRECISION  CONHOCL, CONOCL

! --- Aqueous concentrations (M) of HCl and Cl2
      DOUBLE PRECISION  CONHCL, CONCL

! --- Intermediate variables for calculating Cl species distribution
! --- in solution
      DOUBLE PRECISION  THCL, TCL2, CA, CB, CC, CD, AA, AB, AC, AD

! --- Aqueous equilibrium constants for SO2 and HSO3
! --- SO2       <=>  H+ + HSO3-       (M)
! --- HSO3-     <=>  SO3-- + H+       (M)
      DOUBLE PRECISION  EKSO2, EKHSO3

! --- Henry's Law constants (M/atm)
      DOUBLE PRECISION  HNRHG0, HNRHGCL2, HNRHGOH2
      DOUBLE PRECISION  HNRSO2, HNRO3, HNRHCL, HNRCL2

! --- H+ concentration, OH- concentration
      DOUBLE PRECISION  PHCON, POHCON

      DOUBLE PRECISION  RLCONV  ! Conversion factor (M to atm)

! --- Chemistry parameters
      DOUBLE PRECISION  DT   ! Chemistry time step, sec
      DOUBLE PRECISION  ALPHA1, ALPHA2, ALPH12 ! Intermediate variables
      DOUBLE PRECISION  DENOM1, DENOM2

! --- Temperature-dependent rate for HGSO3 reaction
      DOUBLE PRECISION  RKHGSO3I

! --- Assign values to local variables

! --- Local copy of temperature (don't allow temperature to go below
! --- freezing)
      TEMPK = MAX( TEMP , REAL(TZERO) )

! --- Inverse temperature
      TEMPI = 1.0 / TEMPK

! --- Calculate partial pressures in atm.
      PRES_ATM = PRES / STDATMMB
      CFACT    = 1.E-6 * PRES_ATM

! --- XL is the conversion factor from M (moles/liter of water) to atm
      XL = ( LWC / H2ODENS ) * ( MOLVOL * TEMPK / TZERO )

      Y00 = CHG0 * CFACT
      Y20 = CHG2 * CFACT

      Y( KO3G  ) = CO3  * CFACT
      Y( KSO2G ) = CSO2 * CFACT
      Y( KHCLG ) = CHCL * CFACT
      Y( KCL2G ) = CCL2 * CFACT

! --- Calculate liquid-phase concs of OH and HO2 radicals

! --- Henry's Law constants for OH and HO2
! --- Values below are from Jacobson's book (1999)
      OHH  =   25. * EXP( 17.72 * ( 298.15 * TEMPI - 1.0 ) )
      HO2H = 2000. * EXP( 22.28 * ( 298.15 * TEMPI - 1.0 ) )

! --- Since liquid-phase chemistry of OH and HO2 radicals is not explicitly
! --- simulated in CAMx, assume that in-cloud processes remove 50% of OH and
! --- 90% of HO2
      Y( KOHAQ )  = COH  * CFACT * OHH  * 0.5 / ( 1.0 + OHH  * XL )
      Y( KHO2AQ ) = CHO2 * CFACT * HO2H * 0.1 / ( 1.0 + HO2H * XL )

! --- H+ and OH- aqueous concentrations in moles/liter
      PHCON  = 10. ** ( -CPH )
      POHCON = 10. ** ( CPH - 14 )

! --- concentration of PM in g/L of water
      PM = CPM * H2ODENS * 1.E-9 / LWC  ! No need to check if LWC = 0. since
                                        ! this routine is not called if
                                        ! LWC < LWMIN

! --- Assign some double precision variables

! --- Conversion factor from M to atm.
      RLCONV = XL

! --- Chemistry time step
      DT = DELT

! --- Henry's Law constants for SO2 and O3
      HNRO3  = O3H*exp(O3TMP*(1./298. - 1./tempk))
      HNRSO2 = SO2H*exp(SO2TMP*(1./298. - 1./tempk))
      EKSO2  = 10.**(853./tempk)/54950.
      EKHSO3 = 10.**(621.9/tempk)/1.897e+9

! --- Henry's Law constants for Hg(0), HgCl2, Hg(OH)2, HCl and Cl2
! --- (Not available from CAMx)
! --- Special expression for Hg(0) Henry's Law constant
      HNRHG0 = 54.78775913129319 * EXP( -55.7339 + 9540.36 * TEMPI +
     &                                 16.0477 * ALOG( TEMPK / 100. ) )
      HNRHGCL2 = 1.4E6
      HNRHGOH2 = 1.2E4

      HNRHCL   = 1.1

! --- Special expression for Cl2 Henry's Law constant
      TEMPC = TEMPK - TZERO
      HNRCL2 = 0.149 + TEMPC * ( -3.59E-03 +
     &                   TEMPC * ( 2.5E-05 + TEMPC * 5.67E-08 ) )

! --- Gas-liquid partitioning of reactant species
      Y( KO3G ) = Y( KO3G ) / ( 1.0 + RLCONV * HNRO3 )

! --- Calculate distribution of chlorine species
      IF ( RLCONV .GT. 0. ) THEN  ! Check for RLCONV not really required
                                  ! since this routine is only called for
                                  ! liquid water >= lwmin
         THCL = Y( KHCLG ) / RLCONV
         TCL2 = 2. * Y( KCL2G ) / RLCONV
C
C Calculate subcomponents
         CA = 2. * ( 1.0 + PHCON / EKHOCL )
         CB = 2. * ( 1.0 + 1.0 / ( HNRCL2 * RLCONV ) ) * PHCON**2. /
     &        EKCL2 / EKHOCL
         CC = 1.0 + PHCON / EKHCL * ( 1.0 + 1.0 / ( HNRHCL * RLCONV ) )
         CD = TCL2 * (1.0 + PHCON / EKHOCL )
C
C Solve quadratic equation
         AA = CB * CC
         AB = CA * CC - CB * THCL
         AC = -( CD + CA * THCL )
         AD = AB * AB - 4. * AA * AC

         CONCL = ( -AB + SQRT( AD ) ) / (2. * AA )
         CONHCL = CONCL * PHCON / EKHCL
         Y( KHCLG ) = CONHCL / HNRHCL
         Y( KCL2G ) = 0.5 * TCL2 / (1. / ( RLCONV ) + HNRCL2 *
     &                ( 1.0 + EKCL2 / ( EKHCL * HNRHCL * Y( KHCLG ) ) *
     &                ( 1.0 + EKHOCL / PHCON ) ) )

      END IF  ! ( RLCONV > 0. )

! --- Calculate partitioning of S(IV), assuming that mercury sulfites
! --- are negligible compared to other sulfite ions.
      Y( KSO2G ) = Y( KSO2G ) / ( 1.0 + RLCONV * HNRSO2 * ( 1.0 +
     &             EKSO2 / PHCON * ( 1.0 + EKHSO3 / PHCON ) ) )

! --- Calculate constants
      CON2 = EKHGSO3 * EKSO2 * EKHSO3 * HNRSO2 / ( PHCON * PHCON )
      CON3 = EKHGSO3 * EKHGSO32 * ( EKSO2 * EKSO2 ) * 
     &          ( EKHSO3 * EKHSO3 ) * ( HNRSO2 * HNRSO2 ) / ( PHCON**4 )
      CON5 = ( 1.0 + Y( KSO2G ) * ( CON2 + CON3 * Y( KSO2G ) ) )
      CON6 = ( ( EKHCL * HNRHCL * Y( KHCLG ) / PHCON )**2 ) / EKHGCL2
      CON7 = ( POHCON * POHCON ) / EKHGOH2
      CONHOCL = HNRCL2 * Y( KCL2G ) * EKCL2 / 
     &                                  ( EKHCL * HNRHCL * Y( KHCLG ) )
      CONOCL = CONHOCL * EKHOCL / PHCON

      CON51 = CON2 * Y( KSO2G )
      CON52 = CON3 * Y( KSO2G ) * Y( KSO2G )
      DENOM1 = ( 1.0 + RLCONV * HNRHG0 )
      TOTHG = CON5 + ( CON51 + CON52 ) * EKP * PM + ( CON6 + CON7 ) *
     2        ( 1.0 + EKP * PM )
      DENOM2 = CON6 / HNRHGCL2 + CON7 / HNRHGOH2 + RLCONV * TOTHG

! --- Hg aqueous chemistry calculations for time-step DT

! --- Calculate HGSO3 reaction rate (Van Loon et al., 2000)
      RKHGSO3I = RKHGSO3 * EXP( -105000. / 8.3 * ( TEMPI - TREFI ) )

      CON1 = RKHGSO32 * CON3
      CON4 = RKHGSO3I * CON2

      ALPHA1 = ( RLCONV * HNRHG0 * ( RKO3 * HNRO3 * Y( KO3G ) +
     &                               RKOH * Y( KOHAQ ) +
     &                               RKHOCL * CONHOCL +
     &                               RKOCL * CONOCL ) ) / DENOM1

      ALPHA2 =  RLCONV / DENOM2 * ( CON1 * ( Y( KSO2G ) * Y( KSO2G ) ) +
     &          CON4 * Y( KSO2G ) + RKHO2 * Y( KHO2AQ ) *
     &          ( CON5 + CON6 + CON7 ) )

      ALPH12 = ALPHA1 + ALPHA2

! --- Calculate analytical solution
      Z0T = ALPHA2 * ( Y00 + Y20 ) / ALPH12 *
     &      (1.0 - EXP( -ALPH12 * DT ) ) + Y00 * EXP( -ALPH12 * DT )
      Z2T = Y00 + Y20 - Z0T

      CHG0 = Z0T / CFACT
      CHG2 = Z2T / CFACT

      RETURN
      END

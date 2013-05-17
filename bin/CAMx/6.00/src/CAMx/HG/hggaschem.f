      SUBROUTINE HGGASCHEM ( CHG0, CHG2, CO3, CH2O2, COH, CBR,
     &                       CBRO, TEMP, PRES, DELT )
C
c----CAMx v6.00 130506
C
C   HGGASCHEM performs the Hg gas-phase chemistry calculations.
C
C     Copyright 2003
C     AER, Inc.
C 
C Revision History: 
C***********************************************************************
C   Version 1.0 written March 2003 by Prakash Karamchandani, AER       *
C                                                                      *
C   Updated May 2011 by PK, ENVIRON:                                   *
C      Add Hg(0)oxidation by bromine species (Br, BrO)                 *
C      Remove Cl2, HCl reactions (unimportant)                         *
C*********************************************************************** 
C
C  Called by:  CHEMDRIV
C
C*********************************************************************** 

      IMPLICIT NONE

C Includes:

      INCLUDE 'hggaschm.prm'  ! Hg gas-phase chemistry parameters

C Arguments:

      REAL  CHG0      ! Hg(0) concentration, ppm (Input/Output)
      REAL  CHG2      ! Hg(2) concentration, ppm (Input/Output)

      REAL  CO3       ! O3 concentration, ppm (Input only)
      REAL  CH2O2     ! H2O2 concentration, ppm (Input only)
      REAL  COH       ! OH concentration, ppm (Input only)
      REAL  CBR       ! Br concentration, ppm (Input only)
      REAL  CBRO      ! BrO concentration, ppm (Input only)

      REAL  TEMP      ! Temperature, K (Input only)
      REAL  PRES      ! Pressure, mb (Input only)
      REAL  DELT      ! Chemistry time step, seconds (Input only)

! Local variables

! Standard atmosphere in mb
      REAL, PARAMETER :: STDATMMB = 1013.25

! Pressure in atmospheres
      REAL PRES_ATM

! Conv. factor for rate constants (from (mol/cc)-1 s-1 to ppm-1 s-1)
      REAL  CFACT

! Local rate constants (ppm-1 s-1 units)
      REAL  RKO3L, RKOHL, RKH2O2L, RKBRL

! Local rate constants for individual bromine species reactions
      REAL*8 :: RK1, RK2, RK3, RK4, RK5

! Effective 1st order rate constant for Hg(0) -> Hg(2)
      REAL  RKHG0L

! Decrease in Hg(0) conc. due to chemistry
      REAL  DHG0
C-----------------------------------------------------------------------

! Convert 2nd-order rate constants from mol/cc-s units to ppm-s units
      PRES_ATM = PRES / STDATMMB
      CFACT    = COEF1 * PRES_ATM / TEMP

      RKO3L   = RKO3 * CFACT
      RKOHL   = RKOH * CFACT
      RKH2O2L = RKH2O2 * CFACT

! Rate constants for bromine reactions in ppm-sec units
      RK1 = RK1BR*PRES_ATM*(TEMP/298.0)**(-1.86) * CFACT
      RK2 = RK2BR*EXP(-8357.0/TEMP)
      RK3 = RK3BR*(TEMP/298.0)**(-0.57) * CFACT
      RK4 = RK4BR*(TEMP/298.0)**(-0.57) * CFACT
      RK5 = RKBRO * CFACT  ! No temperature dependence info available
C
C --- Calculate effective first-order rate constant for bromine reactions
      RKBRL = RK1*CBR * (RK3*CBR + RK4*COH) /
     &            (RK2 + RK3*CBR + RK4*COH) + RK5*CBRO

! Calculate effective 1st-order rate constant for Hg(0) to Hg(2) oxidation
! assuming that oxidant species concentrations are constant
      RKHG0L = (RKO3L*CO3 + RKOHL*COH + RKH2O2L*CH2O2 + RKBRL)

! Decrease in Hg(0) conc. due to gas-phase oxidation
      DHG0 = CHG0 * ( 1. - EXP( -RKHG0L * DELT ) )

      CHG0 = CHG0 - DHG0
      CHG2 = CHG2 + DHG0

      RETURN
      END

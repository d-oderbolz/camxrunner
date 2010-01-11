      SUBROUTINE HGGASCHEM ( CHG0, CHG2, CO3, CH2O2, COH, CHCL,
     &                       CCL2, TEMP, PRES, DELT )
C
c----CAMx v4.51 080522
C
C   HGGASCHEM performs the Hg gas-phase chemistry calculations.
C
C     Copyright 2003
C     AER, Inc.
C 
C Revision History: 
C***********************************************************************
C   Version 1.0 written March 2003 by Prakash Karamchandani, AER       *
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
      REAL  CHCL      ! HCL concentration, ppm (Input only)
      REAL  CCL2      ! CL2 concentration, ppm (Input only)

      REAL  TEMP      ! Temperature, K (Input only)
      REAL  PRES      ! Pressure, mb (Input only)
      REAL  DELT      ! Chemistry time step, seconds (Input only)

! Local variables

! Standard atmosphere in mb
      REAL        STDATMMB
      PARAMETER ( STDATMMB = 1013.25 )

! Pressure in atmospheres
      REAL PRES_ATM

! Conv. factor for rate constants (from (mol/cc)-1 s-1 to ppm-1 s-1)
      REAL  CFACT

! Local rate constants (ppm-1 s-1 units)
      REAL  RKO3L, RKOHL, RKH2O2L, RKCL2L, RKHCLL

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
      RKCL2L  = RKCL2 * CFACT
      RKHCLL  = RKHCL * CFACT

! Calculate effective 1st-order rate constant for Hg(0) to Hg(2) oxidation
! assuming that oxidant species concentrations are constant
      RKHG0L = ( RKO3L * CO3 + RKOHL * COH + RKH2O2L * CH2O2 +
     &           RKCL2L * CCL2 + RKHCLL * CHCL )

! Decrease in Hg(0) conc. due to gas-phase oxidation
      DHG0 = CHG0 * ( 1. - EXP( -RKHG0L * DELT ) )

      CHG0 = CHG0 - DHG0
      CHG2 = CHG2 + DHG0

      RETURN
      END

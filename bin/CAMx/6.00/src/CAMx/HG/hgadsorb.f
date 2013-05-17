      subroutine hgadsorb(temp,chg2,chgiip,chgiipc,fseas,
     &                    safine,sacoarse)
c
c----CAMx v6.00 130506
c
c    HGADSORB calculates adsorption of gas-phase Hg(2) on fine and coarse
c    primary PM
c
c     Copyright 2011 - 2013
c     ENVIRON International Corporation
c
c     Modifications:
c        06/02/11  Original development
c
      implicit none
c
c-----Arguments:
c
      real, intent( in    ) :: temp     ! Temperature (deg K)
      real, intent( inout ) :: chg2     ! Hg(2) concentration (ug/m3)
      real, intent( inout ) :: chgiip   ! HgIIP concentration (ug/m3)
      real, intent( inout ) :: chgiipc  ! HgIIPC concentration (ug/m3)
      real, intent( in    ) :: fseas    ! Seasalt fraction of fine PM
      real, intent( in    ) :: safine   ! Fine PM surface area (m2/m3)
      real, intent( in    ) :: sacoarse ! Coarse PM surface area (m2/m3)
c
c-----Constants:
c     Temperature dependence for adsorption coefficient
c
      real, parameter :: TEMPCOEF = 4250.
c
c-----Locals:
c
      real :: kadsorb1, kadsorb2     ! Adsorption coefficients
      real :: keff_fine, keff_coarse ! Effective adsorption coefficient
      real :: chg2t                  ! Total Hg(2) (gas+adsorbed)
c
c-----Calculate total Hg(2) (Combine gas-phase Hg(2) with previously adsorbed
c     Hg(2))
c     Note: In current implementation, gas-phase Hg(2) is actually total Hg(2)
c           and HGIIP/HGIIPC are set to zero in calling routine. Thus, no need
c           to calculate total Hg(2), but kept for flexibility
c
      chg2t = chg2 + chgiip + chgiipc
c
c-----Calculate adsorption coefficients
c
      kadsorb1 = 10.**(TEMPCOEF/temp - 10.)   ! Adsorption to urban PM
      kadsorb2 = kadsorb1 * 10.               ! Adsorption to seasalt
c
c-----Calculate adsorption using different coefficients for urban PM and seasalt PM
c     Effective coefficient
c
      keff_fine   = (kadsorb1*( 1.-fseas ) + kadsorb2*fseas)*safine
      keff_coarse =  kadsorb1 * sacoarse

      chgiip  = chg2t*keff_fine/(1.0+keff_fine+keff_coarse)
      chgiipc = chg2t*keff_coarse/(1.0+keff_fine+keff_coarse)
      chg2    = chg2t - (chgiip + chgiipc)

      return
      end

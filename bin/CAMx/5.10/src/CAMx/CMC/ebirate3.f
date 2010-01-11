      subroutine ebirate3(ny,nr,r,gain,loss)
      implicit none
c
c-----CAMx v5.10 090918
c
c     EBIRATE3 computes species production and loss
c     for the EBI solver
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c     Created by the CMC version 5.2
c
c --- Subroutines Called:
c        none
c
c --- Called by:
c        EBISOLV
c
c --- Argument definitions:
c        ny   - dimension of gain and loss
c        nr   - dimension of r
c        r    - reaction rates (hr-1)
c        gain - species production (ppm/hr)
c        loss - species destruction (ppm/hr)
c
c --- Includes:
      include "camx.prm"
      include "chmdat.com"
      include "ddmchm.com"
c
c --- Arguments:
      integer ny, nr, l
      real    loss(ny+1), gain(ny+1)
      real    r(nr)
c
c --- Entry Point:
c
c
c-----Calculate the species rates
c
c
c   NO2    NO     O    O3   NO3   O1D    OH   HO2  N2O5  HNO3
c  HONO   PNA  H2O2    CO  FORM  ALD2  C2O3   XO2   PAN   PAR
c  XO2N   ROR   NTR   OLE   ETH   TOL  CRES   TO2  OPEN   CRO
c  MGLY   XYL  ISOP  ISPD   SO2  SULF  MEOH  ETOH
c
        Loss(lNO2  )= +( 1.000)*r(  1)+( 1.000)*r(  4)+( 1.000)*r(  5)
     &                +( 1.000)*r(  7)+( 1.000)*r( 16)+( 1.000)*r( 17)
     &                +( 1.000)*r( 21)+( 1.000)*r( 26)+( 1.000)*r( 29)
     &                +( 1.000)*r( 47)+( 1.000)*r( 55)+( 1.000)*r( 68)
     &                +( 1.000)*r( 96)
c
        Gain(lNO2  )= +( 1.000)*r(  3)+( 1.000)*r(  6)+( 0.890)*r( 14)
     &                +( 2.000)*r( 15)+( 1.000)*r( 16)+( 1.000)*r( 19)
     &                +( 2.000)*r( 20)+( 1.000)*r( 24)+( 1.000)*r( 25)
     &                +( 1.000)*r( 28)+( 1.000)*r( 30)+( 1.000)*r( 31)
     &                +( 1.000)*r( 46)+( 1.000)*r( 48)+( 1.000)*r( 59)
     &                +( 0.900)*r( 64)+( 0.200)*r( 78)+( 1.000)*r( 79)
c
        Loss(lNO   )= +( 1.000)*r(  3)+( 1.000)*r(  6)+( 1.000)*r( 15)
     &                +( 2.000)*r( 20)+( 1.000)*r( 21)+( 1.000)*r( 22)
     &                +( 1.000)*r( 28)+( 1.000)*r( 46)+( 1.000)*r( 64)
     &                +( 1.000)*r( 79)+( 1.000)*r( 81)
c
        Gain(lNO   )= +( 1.000)*r(  1)+( 1.000)*r(  4)+( 0.110)*r( 14)
     &                +( 1.000)*r( 16)+( 1.000)*r( 23)+( 1.000)*r( 25)
     &                +( 0.200)*r( 96)
c
        Loss(lO    )= +( 1.000)*r(  2)+( 1.000)*r(  4)+( 1.000)*r(  5)
     &                +( 1.000)*r(  6)+( 1.000)*r( 40)+( 1.000)*r( 42)
     &                +( 1.000)*r( 56)+( 1.000)*r( 60)+( 1.000)*r( 75)
c
        Gain(lO    )= +( 1.000)*r(  1)+( 1.000)*r(  8)+( 1.000)*r( 10)
     &                +( 0.890)*r( 14)
c
        Loss(lO3   )= +( 1.000)*r(  3)+( 1.000)*r(  7)+( 1.000)*r(  8)
     &                +( 1.000)*r(  9)+( 1.000)*r( 12)+( 1.000)*r( 13)
     &                +( 1.000)*r( 58)+( 1.000)*r( 62)+( 1.000)*r( 71)
     &                +( 1.000)*r( 77)+( 1.000)*r( 93)
c
        Gain(lO3   )= +( 1.000)*r(  2)
c
        Loss(lNO3  )= +( 1.000)*r( 14)+( 1.000)*r( 15)+( 1.000)*r( 16)
     &                +( 1.000)*r( 17)+( 1.000)*r( 41)+( 1.000)*r( 44)
     &                +( 1.000)*r( 59)+( 1.000)*r( 67)+( 1.000)*r( 78)
     &                +( 1.000)*r( 94)
c
        Gain(lNO3  )= +( 1.000)*r(  5)+( 1.000)*r(  7)+( 1.000)*r( 19)
     &                +( 1.000)*r( 27)
c
        Loss(lO1D  )= +( 1.000)*r( 10)+( 1.000)*r( 11)
c
        Gain(lO1D  )= +( 1.000)*r(  9)
c
        Loss(lOH   )= +( 1.000)*r( 12)+( 1.000)*r( 22)+( 1.000)*r( 24)
     &                +( 1.000)*r( 26)+( 1.000)*r( 27)+( 1.000)*r( 31)
     &                +( 1.000)*r( 35)+( 1.000)*r( 36)+( 1.000)*r( 37)
     &                +( 1.000)*r( 43)+( 1.000)*r( 51)+( 1.000)*r( 52)
     &                +( 1.000)*r( 57)+( 1.000)*r( 61)+( 1.000)*r( 63)
     &                +( 1.000)*r( 66)+( 1.000)*r( 70)+( 1.000)*r( 72)
     &                +( 1.000)*r( 73)+( 1.000)*r( 76)+( 1.000)*r( 82)
     &                +( 1.000)*r( 84)+( 1.000)*r( 85)+( 1.000)*r( 90)
     &                +( 1.000)*r( 92)
c
        Gain(lOH   )= +( 2.000)*r( 11)+( 1.000)*r( 13)+( 1.000)*r( 23)
     &                +( 1.000)*r( 28)+( 2.000)*r( 34)+( 1.000)*r( 40)
     &                +( 1.000)*r( 42)+( 0.790)*r( 50)+( 0.200)*r( 56)
     &                +( 0.100)*r( 58)+( 0.300)*r( 60)+( 0.080)*r( 71)
     &                +( 0.266)*r( 77)+( 0.268)*r( 93)
c
        Loss(lHO2  )= +( 1.000)*r( 13)+( 1.000)*r( 28)+( 1.000)*r( 29)
     &                +( 2.000)*r( 32)+( 2.000)*r( 33)+( 1.000)*r( 50)
     &                +( 1.000)*r( 86)+( 1.000)*r( 87)+( 1.000)*r( 90)
c
        Gain(lHO2  )= +( 1.000)*r( 12)+( 1.000)*r( 30)+( 1.000)*r( 35)
     &                +( 1.000)*r( 36)+( 1.000)*r( 37)+( 2.000)*r( 38)
     &                +( 1.000)*r( 40)+( 1.000)*r( 41)+( 2.000)*r( 45)
     &                +( 1.000)*r( 46)+( 2.000)*r( 49)+( 0.790)*r( 50)
     &                +( 1.000)*r( 51)+( 0.110)*r( 52)+( 0.940)*r( 53)
     &                +( 1.000)*r( 54)+( 0.380)*r( 56)+( 1.000)*r( 57)
     &                +( 0.440)*r( 58)+( 1.700)*r( 60)+( 1.000)*r( 61)
     &                +( 0.120)*r( 62)+( 0.440)*r( 63)+( 0.900)*r( 64)
     &                +( 1.000)*r( 65)+( 0.600)*r( 66)+( 1.000)*r( 69)
        Gain(lHO2  ) = Gain(lHO2  )
     &                +( 2.000)*r( 70)+( 0.760)*r( 71)+( 0.700)*r( 72)
     &                +( 1.000)*r( 74)+( 0.250)*r( 75)+( 0.912)*r( 76)
     &                +( 0.066)*r( 77)+( 0.800)*r( 78)+( 1.000)*r( 82)
     &                +( 1.000)*r( 84)+( 1.000)*r( 85)+( 0.503)*r( 92)
     &                +( 0.154)*r( 93)+( 0.925)*r( 94)+( 1.033)*r( 95)
     &                +( 0.800)*r( 96)
c
        Loss(lN2O5 )= +( 1.000)*r( 18)+( 1.000)*r( 19)
c
        Gain(lN2O5 )= +( 1.000)*r( 17)
c
        Loss(lHNO3 )= +( 1.000)*r( 27)
c
        Gain(lHNO3 )= +( 2.000)*r( 18)+( 1.000)*r( 26)+( 1.000)*r( 41)
     &                +( 1.000)*r( 44)+( 1.000)*r( 67)+( 0.150)*r( 94)
c
        Loss(lHONO )= +( 1.000)*r( 23)+( 1.000)*r( 24)+( 2.000)*r( 25)
c
        Gain(lHONO )= +( 2.000)*r( 21)+( 1.000)*r( 22)
c
        Loss(lPNA  )= +( 1.000)*r( 30)+( 1.000)*r( 31)
c
        Gain(lPNA  )= +( 1.000)*r( 29)
c
        Loss(lH2O2 )= +( 1.000)*r( 34)+( 1.000)*r( 35)
c
        Gain(lH2O2 )= +( 1.000)*r( 32)+( 1.000)*r( 33)
c
        Loss(lCO   )= +( 1.000)*r( 36)
c
        Gain(lCO   )= +( 1.000)*r( 37)+( 1.000)*r( 38)+( 1.000)*r( 39)
     &                +( 1.000)*r( 40)+( 1.000)*r( 41)+( 1.000)*r( 45)
     &                +( 0.300)*r( 56)+( 0.330)*r( 58)+( 1.000)*r( 60)
     &                +( 0.420)*r( 62)+( 1.000)*r( 69)+( 2.000)*r( 70)
     &                +( 0.690)*r( 71)+( 1.000)*r( 74)+( 0.066)*r( 77)
     &                +( 0.334)*r( 92)+( 0.225)*r( 93)+( 0.643)*r( 94)
     &                +( 0.333)*r( 95)
c
        Loss(lFORM )= +( 1.000)*r( 37)+( 1.000)*r( 38)+( 1.000)*r( 39)
     &                +( 1.000)*r( 40)+( 1.000)*r( 41)
c
        Gain(lFORM )= +( 1.000)*r( 45)+( 1.000)*r( 46)+( 2.000)*r( 49)
     &                +( 0.790)*r( 50)+( 1.000)*r( 51)+( 0.200)*r( 56)
     &                +( 1.000)*r( 57)+( 0.740)*r( 58)+( 1.000)*r( 59)
     &                +( 1.000)*r( 60)+( 1.560)*r( 61)+( 1.000)*r( 62)
     &                +( 1.000)*r( 70)+( 0.700)*r( 71)+( 0.500)*r( 75)
     &                +( 0.629)*r( 76)+( 0.600)*r( 77)+( 1.000)*r( 84)
     &                +( 0.167)*r( 92)+( 0.150)*r( 93)+( 0.282)*r( 94)
     &                +( 0.900)*r( 95)
c
        Loss(lALD2 )= +( 1.000)*r( 42)+( 1.000)*r( 43)+( 1.000)*r( 44)
     &                +( 1.000)*r( 45)
c
        Gain(lALD2 )= +( 0.110)*r( 52)+( 1.100)*r( 53)+( 0.630)*r( 56)
     &                +( 1.000)*r( 57)+( 0.500)*r( 58)+( 1.000)*r( 59)
     &                +( 0.220)*r( 61)+( 0.030)*r( 71)+( 0.150)*r( 77)
     &                +( 0.800)*r( 78)+( 1.000)*r( 85)+( 0.273)*r( 92)
     &                +( 0.020)*r( 93)+( 0.357)*r( 94)+( 0.067)*r( 95)
     &                +( 0.800)*r( 96)
c
        Loss(lC2O3 )= +( 1.000)*r( 46)+( 1.000)*r( 47)+( 2.000)*r( 49)
     &                +( 1.000)*r( 50)
c
        Gain(lC2O3 )= +( 1.000)*r( 42)+( 1.000)*r( 43)+( 1.000)*r( 44)
     &                +( 1.000)*r( 48)+( 1.000)*r( 69)+( 1.000)*r( 70)
     &                +( 0.620)*r( 71)+( 1.000)*r( 73)+( 1.000)*r( 74)
     &                +( 0.250)*r( 75)+( 0.200)*r( 77)+( 0.498)*r( 92)
     &                +( 0.114)*r( 93)+( 0.075)*r( 94)+( 0.967)*r( 95)
c
        Loss(lXO2  )= +( 1.000)*r( 79)+( 2.000)*r( 80)+( 1.000)*r( 86)
     &                +( 1.000)*r( 89)
c
        Gain(lXO2  )= +( 1.000)*r( 45)+( 1.000)*r( 46)+( 2.000)*r( 49)
     &                +( 0.790)*r( 50)+( 1.000)*r( 51)+( 0.870)*r( 52)
     &                +( 0.960)*r( 53)+( 0.280)*r( 56)+( 1.000)*r( 57)
     &                +( 0.220)*r( 58)+( 0.910)*r( 59)+( 0.700)*r( 60)
     &                +( 1.000)*r( 61)+( 0.080)*r( 63)+( 0.600)*r( 66)
     &                +( 1.000)*r( 70)+( 0.030)*r( 71)+( 0.500)*r( 72)
     &                +( 1.000)*r( 73)+( 0.250)*r( 75)+( 0.991)*r( 76)
     &                +( 0.200)*r( 77)+( 1.000)*r( 78)+( 0.713)*r( 92)
     &                +( 0.064)*r( 93)+( 0.075)*r( 94)+( 0.700)*r( 95)
        Gain(lXO2  ) = Gain(lXO2  )
     &                +( 1.000)*r( 96)
c
        Loss(lPAN  )= +( 1.000)*r( 48)
c
        Gain(lPAN  )= +( 1.000)*r( 47)
c
        Loss(lPAR  )= +( 1.000)*r( 52)
c
        Gain(lPAR  )= +(-0.110)*r( 52)+(-2.100)*r( 53)+( 0.220)*r( 56)
     &                +(-1.000)*r( 57)+(-1.000)*r( 58)+(-1.000)*r( 59)
     &                +( 1.100)*r( 72)+( 0.250)*r( 75)+( 0.350)*r( 77)
     &                +( 2.400)*r( 78)+( 1.565)*r( 92)+( 0.360)*r( 93)
     &                +( 1.282)*r( 94)+( 0.832)*r( 95)+( 2.400)*r( 96)
c
        Loss(lXO2N )= +( 1.000)*r( 81)+( 1.000)*r( 87)+( 2.000)*r( 88)
     &                +( 1.000)*r( 89)
c
        Gain(lXO2N )= +( 0.130)*r( 52)+( 0.040)*r( 53)+( 0.020)*r( 56)
     &                +( 0.090)*r( 59)+( 0.088)*r( 76)
c
        Loss(lROR  )= +( 1.000)*r( 53)+( 1.000)*r( 54)+( 1.000)*r( 55)
c
        Gain(lROR  )= +( 0.760)*r( 52)
c
        Loss(lNTR  )= 0.0
c
        Gain(lNTR  )= +( 1.000)*r( 55)+( 0.100)*r( 64)+( 1.000)*r( 68)
     &                +( 0.800)*r( 78)+( 1.000)*r( 81)+( 0.850)*r( 94)
     &                +( 0.800)*r( 96)
c
        Loss(lOLE  )= +( 1.000)*r( 56)+( 1.000)*r( 57)+( 1.000)*r( 58)
     &                +( 1.000)*r( 59)
c
        Gain(lOLE  )= 0.0
c
        Loss(lETH  )= +( 1.000)*r( 60)+( 1.000)*r( 61)+( 1.000)*r( 62)
c
        Gain(lETH  )= 0.0
c
        Loss(lTOL  )= +( 1.000)*r( 63)
c
        Gain(lTOL  )= 0.0
c
        Loss(lCRES )= +( 1.000)*r( 66)+( 1.000)*r( 67)
c
        Gain(lCRES )= +( 0.360)*r( 63)+( 1.000)*r( 65)+( 0.200)*r( 72)
c
        Loss(lTO2  )= +( 1.000)*r( 64)+( 1.000)*r( 65)
c
        Gain(lTO2  )= +( 0.560)*r( 63)+( 0.300)*r( 72)
c
        Loss(lOPEN )= +( 1.000)*r( 69)+( 1.000)*r( 70)+( 1.000)*r( 71)
c
        Gain(lOPEN )= +( 0.900)*r( 64)+( 0.300)*r( 66)
c
        Loss(lCRO  )= +( 1.000)*r( 68)+( 1.000)*r( 91)
c
        Gain(lCRO  )= +( 0.400)*r( 66)+( 1.000)*r( 67)
c
        Loss(lMGLY )= +( 1.000)*r( 73)+( 1.000)*r( 74)
c
        Gain(lMGLY )= +( 0.200)*r( 71)+( 0.800)*r( 72)+( 0.168)*r( 92)
     &                +( 0.850)*r( 93)
c
        Loss(lXYL  )= +( 1.000)*r( 72)
c
        Gain(lXYL  )= 0.0
c
        Loss(lISOP )= +( 1.000)*r( 75)+( 1.000)*r( 76)+( 1.000)*r( 77)
     &                +( 1.000)*r( 78)+( 1.000)*r( 96)
c
        Gain(lISOP )= 0.0
c
        Loss(lISPD )= +( 1.000)*r( 92)+( 1.000)*r( 93)+( 1.000)*r( 94)
     &                +( 1.000)*r( 95)
c
        Gain(lISPD )= +( 0.750)*r( 75)+( 0.912)*r( 76)+( 0.650)*r( 77)
     &                +( 0.200)*r( 78)+( 0.200)*r( 96)
c
        Loss(lSO2  )= +( 1.000)*r( 82)+( 1.000)*r( 83)
c
        Gain(lSO2  )= 0.0
c
        Loss(lSULF )= 0.0
c
        Gain(lSULF )= +( 1.000)*r( 82)+( 1.000)*r( 83)
c
        Loss(lMEOH )= +( 1.000)*r( 84)
c
        Gain(lMEOH )= 0.0
c
        Loss(lETOH )= +( 1.000)*r( 85)
c
        Gain(lETOH )= 0.0
c
c
      return
      end


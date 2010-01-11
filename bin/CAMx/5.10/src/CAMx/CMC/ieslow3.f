      subroutine ieslow3(r,rate,gain,loss,nr,ny,n1,n2)
      implicit none
c
c-----CAMx v5.10 090918
c
c     IESLOW3 computes rates for IEH solver slow species
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c     Created by the CMC version 5.2
c
c     Routines Called:
c        none
c
c     Called by:
c        IEHSOLV
c
      include "camx.prm"
      include "chmdat.com"
      include "iehchem.com"
c
      integer ny, nr, n1, n2, l
      real loss(ny+1)
      real gain(ny+1)
      real rate(ny+1)
      real r(nr)
c
c --- Entry point
c
      do l = n1,n2
        Loss(l) = 0.0
        Gain(l) = 0.0
      enddo
c
c --- Calculate the species rates
c
c
c  HNO3  HONO   PNA  H2O2    CO  FORM  ALD2   PAR   NTR   OLE
c   ETH   TOL  CRES  OPEN  MGLY   XYL  ISOP   SO2  SULF  MEOH
c  ETOH  ISPD
c
        Loss(iHNO3 )= +( 1.000)*r( 27)
c
        Gain(iHNO3 )= +( 2.000)*r( 18)+( 1.000)*r( 26)+( 1.000)*r( 41)
     &                +( 1.000)*r( 44)+( 1.000)*r( 67)+( 0.150)*r( 94)
c
        Loss(iHONO )= +( 1.000)*r( 23)+( 1.000)*r( 24)+( 2.000)*r( 25)
c
        Gain(iHONO )= +( 2.000)*r( 21)+( 1.000)*r( 22)
c
        Loss(iPNA  )= +( 1.000)*r( 30)+( 1.000)*r( 31)
c
        Gain(iPNA  )= +( 1.000)*r( 29)
c
        Loss(iH2O2 )= +( 1.000)*r( 34)+( 1.000)*r( 35)
c
        Gain(iH2O2 )= +( 1.000)*r( 32)+( 1.000)*r( 33)
c
        Loss(iCO   )= +( 1.000)*r( 36)
c
        Gain(iCO   )= +( 1.000)*r( 37)+( 1.000)*r( 38)+( 1.000)*r( 39)
     &                +( 1.000)*r( 40)+( 1.000)*r( 41)+( 1.000)*r( 45)
     &                +( 0.300)*r( 56)+( 0.330)*r( 58)+( 1.000)*r( 60)
     &                +( 0.420)*r( 62)+( 1.000)*r( 69)+( 2.000)*r( 70)
     &                +( 0.690)*r( 71)+( 1.000)*r( 74)+( 0.066)*r( 77)
     &                +( 0.334)*r( 92)+( 0.225)*r( 93)+( 0.643)*r( 94)
     &                +( 0.333)*r( 95)
c
        Loss(iFORM )= +( 1.000)*r( 37)+( 1.000)*r( 38)+( 1.000)*r( 39)
     &                +( 1.000)*r( 40)+( 1.000)*r( 41)
c
        Gain(iFORM )= +( 1.000)*r( 45)+( 1.000)*r( 46)+( 2.000)*r( 49)
     &                +( 0.790)*r( 50)+( 1.000)*r( 51)+( 0.200)*r( 56)
     &                +( 1.000)*r( 57)+( 0.740)*r( 58)+( 1.000)*r( 59)
     &                +( 1.000)*r( 60)+( 1.560)*r( 61)+( 1.000)*r( 62)
     &                +( 1.000)*r( 70)+( 0.700)*r( 71)+( 0.500)*r( 75)
     &                +( 0.629)*r( 76)+( 0.600)*r( 77)+( 1.000)*r( 84)
     &                +( 0.167)*r( 92)+( 0.150)*r( 93)+( 0.282)*r( 94)
     &                +( 0.900)*r( 95)
c
        Loss(iALD2 )= +( 1.000)*r( 42)+( 1.000)*r( 43)+( 1.000)*r( 44)
     &                +( 1.000)*r( 45)
c
        Gain(iALD2 )= +( 0.110)*r( 52)+( 1.100)*r( 53)+( 0.630)*r( 56)
     &                +( 1.000)*r( 57)+( 0.500)*r( 58)+( 1.000)*r( 59)
     &                +( 0.220)*r( 61)+( 0.030)*r( 71)+( 0.150)*r( 77)
     &                +( 0.800)*r( 78)+( 1.000)*r( 85)+( 0.273)*r( 92)
     &                +( 0.020)*r( 93)+( 0.357)*r( 94)+( 0.067)*r( 95)
     &                +( 0.800)*r( 96)
c
        Loss(iPAR  )= +( 1.000)*r( 52)
c
        Gain(iPAR  )= +(-0.110)*r( 52)+(-2.100)*r( 53)+( 0.220)*r( 56)
     &                +(-1.000)*r( 57)+(-1.000)*r( 58)+(-1.000)*r( 59)
     &                +( 1.100)*r( 72)+( 0.250)*r( 75)+( 0.350)*r( 77)
     &                +( 2.400)*r( 78)+( 1.565)*r( 92)+( 0.360)*r( 93)
     &                +( 1.282)*r( 94)+( 0.832)*r( 95)+( 2.400)*r( 96)
c
        Loss(iNTR  )= 0.0
c
        Gain(iNTR  )= +( 1.000)*r( 55)+( 0.100)*r( 64)+( 1.000)*r( 68)
     &                +( 0.800)*r( 78)+( 1.000)*r( 81)+( 0.850)*r( 94)
     &                +( 0.800)*r( 96)
c
        Loss(iOLE  )= +( 1.000)*r( 56)+( 1.000)*r( 57)+( 1.000)*r( 58)
     &                +( 1.000)*r( 59)
c
        Gain(iOLE  )= 0.0
c
        Loss(iETH  )= +( 1.000)*r( 60)+( 1.000)*r( 61)+( 1.000)*r( 62)
c
        Gain(iETH  )= 0.0
c
        Loss(iTOL  )= +( 1.000)*r( 63)
c
        Gain(iTOL  )= 0.0
c
        Loss(iCRES )= +( 1.000)*r( 66)+( 1.000)*r( 67)
c
        Gain(iCRES )= +( 0.360)*r( 63)+( 1.000)*r( 65)+( 0.200)*r( 72)
c
        Loss(iOPEN )= +( 1.000)*r( 69)+( 1.000)*r( 70)+( 1.000)*r( 71)
c
        Gain(iOPEN )= +( 0.900)*r( 64)+( 0.300)*r( 66)
c
        Loss(iMGLY )= +( 1.000)*r( 73)+( 1.000)*r( 74)
c
        Gain(iMGLY )= +( 0.200)*r( 71)+( 0.800)*r( 72)+( 0.168)*r( 92)
     &                +( 0.850)*r( 93)
c
        Loss(iXYL  )= +( 1.000)*r( 72)
c
        Gain(iXYL  )= 0.0
c
        Loss(iISOP )= +( 1.000)*r( 75)+( 1.000)*r( 76)+( 1.000)*r( 77)
     &                +( 1.000)*r( 78)+( 1.000)*r( 96)
c
        Gain(iISOP )= 0.0
c
        Loss(iSO2  )= +( 1.000)*r( 82)+( 1.000)*r( 83)
c
        Gain(iSO2  )= 0.0
c
        Loss(iSULF )= 0.0
c
        Gain(iSULF )= +( 1.000)*r( 82)+( 1.000)*r( 83)
c
        Loss(iMEOH )= +( 1.000)*r( 84)
c
        Gain(iMEOH )= 0.0
c
        Loss(iETOH )= +( 1.000)*r( 85)
c
        Gain(iETOH )= 0.0
c
        Loss(iISPD )= +( 1.000)*r( 92)+( 1.000)*r( 93)+( 1.000)*r( 94)
     &                +( 1.000)*r( 95)
c
        Gain(iISPD )= +( 0.750)*r( 75)+( 0.912)*r( 76)+( 0.650)*r( 77)
     &                +( 0.200)*r( 78)+( 0.200)*r( 96)
c
c
      do l=n1,n2
        rate(l) = gain(l) -loss(l)
      enddo
c
      return
      end

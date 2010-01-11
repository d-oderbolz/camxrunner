      subroutine radslvr6(ldark,H2O,M,O2,CH4,H2,cncrad,conc,r,crold,
     &                    dt)
c
c----CAMx v4.51 080522
c
c     RADSLVR6 initializes radical concentrations
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c     Routines Called:
c        CPIVOT
c
c     Called by:
c        TRAP
c
      implicit none
      include "camx.prm"
      include "chmstry.com"
      include "filunit.com"
c
      logical ldark, lusecp, lsafe
      real conc(MXSPEC+1),cncrad(MXRADCL),r(MXRXN),rate(MXSPEC+1),
     &     loss(MXSPEC+1),gain(MXSPEC+1),jac(MXRADCL,MXRADCL),
     &     crold(MXRADCL)
      integer i, j, l, kount, nstrt, nend, nendcp, ierr
      real H2O, M, O2, CH4, H2
      real tol, weight, errbig, thresh, det, err, rlim, bnstrt, bnend
      real dt
      real gnNO3, lsNO3, lsN2O5, pdNO3, tpNO3, tpN2O5, fwd, bck, denom
      data tol/0.01/
c
c
c  new group of radicals
c
c   O1D
c
c
c  this radical is formed by photolysis only
c  solved by direct substitution
c
      if (ldark) then
        cncrad(kO1D) = 0.
      else
        Loss(kO1D  )= +( 1.000)*r( 10)+( 1.000)*r( 11)+( 1.000)*r( 38)
        Gain(kO1D  )= +( 1.000)*r(  9)
        cncrad(kO1D) = gain(kO1D)/loss(kO1D)*cncrad(kO1D)
        cncrad(kO1D) = amax1(bdlrad, cncrad(kO1D))
      r( 10) = rk( 10)*cncrad(kO1D)*M
      r( 11) = rk( 11)*cncrad(kO1D)*H2O
      r( 38) = rk( 38)*cncrad(kO1D)*H2
      endif
c
c  new group of radicals
c
c     O
c
c
c  this radical is formed by photolysis only
c  solved by direct substitution
c
      if (ldark) then
        cncrad(kO) = 0.
      else
        Loss(kO    )= +( 1.000)*r(  2)+( 1.000)*r(  4)+( 1.000)*r(  5)
     &                +( 1.000)*r(  6)+( 1.000)*r( 40)+( 1.000)*r( 44)
     &                +( 1.000)*r( 45)+( 1.000)*r( 46)+( 1.000)*r( 77)
     &                +( 1.000)*r( 84)+( 1.000)*r( 99)+( 1.000)*r(119)
     &                +( 1.000)*r(123)+( 1.000)*r(127)+( 1.000)*r(144)
     &                +( 1.000)*r(153)
        Gain(kO    )= +( 1.000)*r(  1)+( 1.000)*r(  8)+( 1.000)*r( 10)
     &                +( 1.000)*r( 14)+( 1.000)*r( 41)+( 0.500)*r(129)
        cncrad(kO) = gain(kO)/loss(kO)*cncrad(kO)
        cncrad(kO) = amax1(bdlrad, cncrad(kO))
      r(  2) = rk(  2)*cncrad(kO)*O2*M
      r(  4) = rk(  4)*cncrad(kO)*conc(kNO2)
      r(  5) = rk(  5)*cncrad(kO)*conc(kNO2)
      r(  6) = rk(  6)*cncrad(kO)*conc(kNO)
      r( 40) = rk( 40)*cncrad(kOH)*cncrad(kO)
      r( 44) = rk( 44)*cncrad(kHO2)*cncrad(kO)
      r( 45) = rk( 45)*conc(kH2O2)*cncrad(kO)
      r( 46) = rk( 46)*cncrad(kNO3)*cncrad(kO)
      r( 77) = rk( 77)*conc(kFORM)*cncrad(kO)
      r( 84) = rk( 84)*conc(kALD2)*cncrad(kO)
      r( 99) = rk( 99)*conc(kALDX)*cncrad(kO)
      r(119) = rk(119)*cncrad(kO)*conc(kOLE)
      r(123) = rk(123)*cncrad(kO)*conc(kETH)
      r(127) = rk(127)*conc(kIOLE)*cncrad(kO)
      r(144) = rk(144)*cncrad(kO)*conc(kISOP)
      r(153) = rk(153)*conc(kTERP)*cncrad(kO)
      endif
c
c  new group of radicals
c
c  N2O5   NO3
c
c  Explicitly solve N2O5 and NO3 using Hertel's solution
c  Note that NO3 self-reaction is hacked
c
        gnNO3  =      +( 1.000)*r(  5)
     &                +( 1.000)*r(  7)
     &                +( 1.000)*r( 29)
     &                +( 0.390)*r( 51)
        lsN2O5 =      +( 1.000)*rk( 19)*H2O
     &                +( 1.000)*rk( 20)*H2O*H2O
     &                +( 1.000)*rk( 21)
     &                +( 1.000)*rk( 53)
        lsNO3  =      +( 1.000)*rk( 14)
     &                +( 1.000)*rk( 15)
     &                +( 1.000)*rk( 16)*conc(kNO)
     &                +( 1.000)*rk( 17)*conc(kNO2)
     &                +( 1.000)*rk( 18)*conc(kNO2)
     &                +( 1.000)*rk( 46)*cncrad(kO)
     &                +( 1.000)*rk( 47)*cncrad(kOH)
     &                +( 1.000)*rk( 48)*cncrad(kHO2)
     &                +( 1.000)*rk( 49)*conc(kO3)
     &                +( 2.000)*rk( 50)*cncrad(kNO3)
     &                +( 1.000)*rk( 78)*conc(kFORM)
     &                +( 1.000)*rk( 86)*conc(kALD2)
     &                +( 1.000)*rk(101)*conc(kALDX)
     &                +( 1.000)*rk(122)*conc(kOLE)
     &                +( 1.000)*rk(126)*conc(kETH)
     &                +( 1.000)*rk(130)*conc(kIOLE)
     &                +( 1.000)*rk(135)*conc(kCRES)
     &                +( 1.000)*rk(147)*conc(kISOP)
     &                +( 1.000)*rk(151)*conc(kISPD)
     &                +( 1.000)*rk(156)*conc(kTERP)
      lsNO3 = 1.0 + dt*lsNO3
      lsN2O5 = 1.0 + dt*lsN2O5
      pdNO3 = crold(kNO3) + dt*gnNO3
      fwd = dt*(rk( 21)+rk( 53))
      bck = dt*rk( 18)*conc(kNO2)
      tpNO3 = lsN2O5*pdNO3 + fwd*crold(kN2O5)
      tpN2O5 = lsNO3*crold(kN2O5) + bck*pdNO3
      denom = (lsN2O5*lsNO3) - (fwd*bck)
      cncrad(kNO3) = tpNO3/denom
      cncrad(kN2O5) = tpN2O5/denom
      r( 14) = rk( 14)*cncrad(kNO3)
      r( 15) = rk( 15)*cncrad(kNO3)
      r( 16) = rk( 16)*cncrad(kNO3)*conc(kNO)
      r( 17) = rk( 17)*cncrad(kNO3)*conc(kNO2)
      r( 18) = rk( 18)*cncrad(kNO3)*conc(kNO2)
      r( 19) = rk( 19)*cncrad(kN2O5)*H2O
      r( 20) = rk( 20)*cncrad(kN2O5)*H2O*H2O
      r( 21) = rk( 21)*cncrad(kN2O5)
      r( 46) = rk( 46)*cncrad(kNO3)*cncrad(kO)
      r( 47) = rk( 47)*cncrad(kNO3)*cncrad(kOH)
      r( 48) = rk( 48)*cncrad(kNO3)*cncrad(kHO2)
      r( 49) = rk( 49)*cncrad(kNO3)*conc(kO3)
      r( 50) = rk( 50)*cncrad(kNO3)*cncrad(kNO3)
      r( 53) = rk( 53)*cncrad(kN2O5)
      r( 78) = rk( 78)*conc(kFORM)*cncrad(kNO3)
      r( 86) = rk( 86)*conc(kALD2)*cncrad(kNO3)
      r(101) = rk(101)*conc(kALDX)*cncrad(kNO3)
      r(122) = rk(122)*cncrad(kNO3)*conc(kOLE)
      r(126) = rk(126)*cncrad(kNO3)*conc(kETH)
      r(130) = rk(130)*conc(kIOLE)*cncrad(kNO3)
      r(135) = rk(135)*conc(kCRES)*cncrad(kNO3)
      r(147) = rk(147)*cncrad(kNO3)*conc(kISOP)
      r(151) = rk(151)*cncrad(kNO3)*conc(kISPD)
      r(156) = rk(156)*conc(kTERP)*cncrad(kNO3)
c
c  new group of radicals
c
c    OH   HO2  C2O3   XO2  XO2N  CXO3  MEO2   TO2   ROR  HCO3
c
c
      nstrt = kOH
      nend  = kHCO3
      weight = 1.0
      errbig = 1.0
      thresh = 1.0e-15
      lsafe = .false.
      kount = 0
  14  kount = kount + 1
      if (kount.gt.500) then
        write(iout,'(//,A,//)') 'ERROR in RADSLVR6:'
        write(iout,*) 'No Convergence in RADSLVR6, errbig = ', errbig
        write(iout,*) 'statement number = ', 14
        write(iout,*) 'igrd,i, j, k = ', igrdchm,ichm,jchm,kchm
        write(iout,*) 'LDARK is set ', ldark
        do l=1,ngas
          write(iout,'(i3,2x,a7,1pe10.3)') l,spname(l),conc(l)
        enddo
        write(iout,*) 'The radicals are: '
        do l= 1 , nrad
          write(iout,'(i3,2x,a7,1pe10.3)')
     &          l,nmrad(l),cncrad(l)
        enddo
        write(iout,*) 'Currently solving ', nstrt, ' to ',nendcp
        do l=nstrt,nendcp
          write(iout,'(i3,2x,a7,1p2e10.3)')
     &          l,nmrad(l),cncrad(l),abs(rate(l))/cncrad(l)
        enddo
        call camxerr()
      endif
      do i=nstrt,nend
        do j=nstrt,nend
          jac(i,j) = 0.
        enddo
      enddo
c
c  to reduce the matrix size, TO2 is solved first and substituted
c
        Loss(kTO2  )= +( 1.000)*r(132)+( 1.000)*r(133)
        Gain(kTO2  )= +( 0.560)*r(131)+( 0.300)*r(141)
c
c  first order method chosen for this radical
c  solved by direct substitution
c
      cncrad(kTO2) = gain(kTO2)/loss(kTO2)*cncrad(kTO2)
      cncrad(kTO2) = amax1(bdlrad, cncrad(kTO2))
      r(132) = rk(132)*cncrad(kTO2)*conc(kNO)
      r(133) = rk(133)*cncrad(kTO2)
c
c  to reduce the matrix size, ROR is solved first and substituted
c
        Loss(kROR  )= +( 1.000)*r(116)+( 1.000)*r(117)+( 1.000)*r(118)
        Gain(kROR  )= +( 0.760)*r(115)+( 0.020)*r(116)
c
c  first order method chosen for this radical
c  solved by direct substitution
c
      cncrad(kROR) = gain(kROR)/loss(kROR)*cncrad(kROR)
      cncrad(kROR) = amax1(bdlrad, cncrad(kROR))
      r(116) = rk(116)*cncrad(kROR)
      r(117) = rk(117)*cncrad(kROR)
      r(118) = rk(118)*cncrad(kROR)*conc(kNO2)
c
c  to reduce the matrix size, HCO3 is solved first and substituted
c
        Loss(kHCO3 )= +( 1.000)*r( 80)+( 1.000)*r( 81)+( 1.000)*r( 82)
        Gain(kHCO3 )= +( 1.000)*r( 79)
c
c  first order method chosen for this radical
c  solved by direct substitution
c
      cncrad(kHCO3) = gain(kHCO3)/loss(kHCO3)*cncrad(kHCO3)
      cncrad(kHCO3) = amax1(bdlrad, cncrad(kHCO3))
      r( 80) = rk( 80)*cncrad(kHCO3)
      r( 81) = rk( 81)*cncrad(kHCO3)*conc(kNO)
      r( 82) = rk( 82)*cncrad(kHCO3)*cncrad(kHO2)
        Loss(kOH   )= +( 1.000)*r( 12)+( 1.000)*r( 24)+( 1.000)*r( 26)
     &                +( 1.000)*r( 28)+( 1.000)*r( 29)+( 1.000)*r( 33)
     &                +( 1.000)*r( 37)+( 1.000)*r( 39)+( 1.000)*r( 40)
     &                +( 2.000)*r( 41)+( 2.000)*r( 42)+( 1.000)*r( 43)
     &                +( 1.000)*r( 47)+( 1.000)*r( 61)+( 1.000)*r( 63)
     &                +( 1.000)*r( 64)+( 1.000)*r( 66)+( 1.000)*r( 67)
     &                +( 1.000)*r( 71)+( 1.000)*r( 73)+( 1.000)*r( 74)
     &                +( 1.000)*r( 83)+( 1.000)*r( 85)+( 1.000)*r( 96)
     &                +( 1.000)*r( 98)+( 1.000)*r(100)+( 1.000)*r(107)
        Loss(kOH   ) = Loss(kOH   )
     &                +( 1.000)*r(113)+( 1.000)*r(114)+( 1.000)*r(115)
     &                +( 1.000)*r(120)+( 1.000)*r(124)+( 1.000)*r(128)
     &                +( 1.000)*r(131)+( 1.000)*r(134)+( 1.000)*r(139)
     &                +( 1.000)*r(141)+( 1.000)*r(142)+( 1.000)*r(145)
     &                +( 1.000)*r(149)+( 1.000)*r(154)
        Gain(kOH   )= +( 2.000)*r( 11)+( 1.000)*r( 13)+( 1.000)*r( 25)
     &                +( 1.000)*r( 30)+( 2.000)*r( 36)+( 1.000)*r( 38)
     &                +( 1.000)*r( 44)+( 1.000)*r( 45)+( 0.390)*r( 51)
     &                +( 1.000)*r( 52)+( 1.000)*r( 65)+( 1.000)*r( 72)
     &                +( 1.000)*r( 77)+( 1.000)*r( 84)+( 1.000)*r( 97)
     &                +( 1.000)*r( 99)+( 0.100)*r(119)+( 0.100)*r(121)
     &                +( 0.300)*r(123)+( 0.130)*r(125)+( 0.500)*r(129)
     &                +( 0.080)*r(140)+( 0.266)*r(146)+( 0.268)*r(150)
     &                +( 0.570)*r(155)
        Loss(kHO2  )= +( 1.000)*r( 13)+( 1.000)*r( 30)+( 1.000)*r( 31)
     &                +( 2.000)*r( 34)+( 2.000)*r( 35)+( 1.000)*r( 43)
     &                +( 1.000)*r( 44)+( 1.000)*r( 48)+( 1.000)*r( 56)
     &                +( 1.000)*r( 57)+( 1.000)*r( 69)+( 1.000)*r( 79)
     &                +( 1.000)*r( 82)+( 1.000)*r( 92)+( 1.000)*r(108)
     &                +( 1.000)*r(137)
        Gain(kHO2  )= +( 1.000)*r( 12)+( 1.000)*r( 32)+( 1.000)*r( 37)
     &                +( 1.000)*r( 38)+( 1.000)*r( 39)+( 1.000)*r( 40)
     &                +( 1.000)*r( 45)+( 1.000)*r( 47)+( 0.610)*r( 51)
     &                +( 1.000)*r( 61)+( 1.000)*r( 62)+( 1.000)*r( 63)
     &                +( 1.000)*r( 65)+( 1.000)*r( 66)+( 1.000)*r( 68)
     &                +( 0.740)*r( 70)+( 0.300)*r( 71)+( 1.000)*r( 72)
     &                +( 1.000)*r( 73)+( 1.000)*r( 74)+( 2.000)*r( 75)
     &                +( 1.000)*r( 77)+( 1.000)*r( 78)+( 1.000)*r( 80)
     &                +( 1.000)*r( 81)+( 1.000)*r( 83)+( 1.000)*r( 87)
        Gain(kHO2  ) = Gain(kHO2  )
     &                +( 0.900)*r( 93)+( 1.000)*r(102)+( 1.000)*r(103)
     &                +( 1.000)*r(109)+( 2.000)*r(111)+( 1.000)*r(112)
     &                +( 1.000)*r(113)+( 1.000)*r(114)+( 0.110)*r(115)
     &                +( 0.940)*r(116)+( 1.000)*r(117)+( 0.300)*r(119)
     &                +( 0.950)*r(120)+( 0.440)*r(121)+( 1.700)*r(123)
     &                +( 1.000)*r(124)+( 0.130)*r(125)+( 0.100)*r(127)
     &                +( 1.000)*r(128)+( 0.500)*r(129)+( 1.000)*r(130)
     &                +( 0.440)*r(131)+( 0.900)*r(132)+( 1.000)*r(133)
        Gain(kHO2  ) = Gain(kHO2  )
     &                +( 0.600)*r(134)+( 1.000)*r(138)+( 2.000)*r(139)
     &                +( 0.760)*r(140)+( 0.700)*r(141)+( 1.000)*r(143)
     &                +( 0.250)*r(144)+( 0.912)*r(145)+( 0.066)*r(146)
     &                +( 0.800)*r(147)+( 0.800)*r(148)+( 0.503)*r(149)
     &                +( 0.154)*r(150)+( 0.925)*r(151)+( 1.033)*r(152)
     &                +( 0.750)*r(154)+( 0.070)*r(155)+( 0.280)*r(156)
        Loss(kC2O3 )= +( 1.000)*r( 88)+( 1.000)*r( 89)+( 1.000)*r( 92)
     &                +( 1.000)*r( 93)+( 1.000)*r( 94)+( 2.000)*r( 95)
     &                +( 1.000)*r(112)
        Gain(kC2O3 )= +( 1.000)*r( 84)+( 1.000)*r( 85)+( 1.000)*r( 86)
     &                +( 1.000)*r( 90)+( 1.000)*r( 91)+( 1.000)*r( 96)
     &                +( 1.000)*r(138)+( 1.000)*r(139)+( 0.620)*r(140)
     &                +( 1.000)*r(142)+( 1.000)*r(143)+( 0.210)*r(149)
     &                +( 0.114)*r(150)+( 0.967)*r(152)
        Loss(kXO2  )= +( 1.000)*r( 54)+( 1.000)*r( 56)+( 2.000)*r( 58)
     &                +( 1.000)*r( 60)+( 1.000)*r( 94)+( 1.000)*r(110)
        Gain(kXO2  )= +( 1.000)*r( 64)+( 0.300)*r( 71)+( 1.000)*r(103)
     &                +( 0.900)*r(109)+( 2.000)*r(111)+( 1.000)*r(112)
     &                +( 0.991)*r(113)+( 0.100)*r(114)+( 0.870)*r(115)
     &                +( 0.960)*r(116)+( 0.200)*r(119)+( 0.800)*r(120)
     &                +( 0.220)*r(121)+( 0.910)*r(122)+( 0.700)*r(123)
     &                +( 1.000)*r(124)+( 1.000)*r(126)+( 0.100)*r(127)
     &                +( 1.000)*r(128)+( 0.080)*r(131)+( 0.600)*r(134)
     &                +( 1.000)*r(139)+( 0.030)*r(140)+( 0.500)*r(141)
     &                +( 1.000)*r(142)+( 0.250)*r(144)+( 0.991)*r(145)
        Gain(kXO2  ) = Gain(kXO2  )
     &                +( 0.200)*r(146)+( 1.000)*r(147)+( 1.000)*r(148)
     &                +( 0.713)*r(149)+( 0.064)*r(150)+( 0.075)*r(151)
     &                +( 0.700)*r(152)+( 1.250)*r(154)+( 0.760)*r(155)
     &                +( 1.030)*r(156)
        Loss(kXO2N )= +( 1.000)*r( 55)+( 1.000)*r( 57)+( 2.000)*r( 59)
     &                +( 1.000)*r( 60)
        Gain(kXO2N )= +( 0.009)*r(113)+( 0.130)*r(115)+( 0.040)*r(116)
     &                +( 0.010)*r(119)+( 0.090)*r(122)+( 0.088)*r(145)
     &                +( 0.250)*r(154)+( 0.180)*r(155)+( 0.250)*r(156)
        Loss(kCXO3 )= +( 1.000)*r(103)+( 1.000)*r(104)+( 1.000)*r(108)
     &                +( 1.000)*r(109)+( 1.000)*r(110)+( 2.000)*r(111)
     &                +( 1.000)*r(112)
        Gain(kCXO3 )= +( 1.000)*r( 99)+( 1.000)*r(100)+( 1.000)*r(101)
     &                +( 1.000)*r(105)+( 1.000)*r(106)+( 0.250)*r(144)
     &                +( 0.200)*r(146)+( 0.250)*r(149)+( 0.075)*r(151)
     &                +( 0.390)*r(155)
        Loss(kMEO2 )= +( 1.000)*r( 68)+( 1.000)*r( 69)+( 2.000)*r( 70)
     &                +( 1.000)*r( 93)+( 1.000)*r(109)
        Gain(kMEO2 )= +( 1.000)*r( 67)+( 0.700)*r( 71)+( 1.000)*r( 87)
     &                +( 1.000)*r( 88)+( 0.900)*r( 93)+( 0.900)*r( 94)
     &                +( 2.000)*r( 95)+( 1.000)*r( 97)+( 1.000)*r( 98)
     &                +( 1.000)*r(102)+( 1.000)*r(112)
        Loss(kTO2  )= +( 1.000)*r(132)+( 1.000)*r(133)
        Gain(kTO2  )= +( 0.560)*r(131)+( 0.300)*r(141)
        Loss(kROR  )= +( 1.000)*r(116)+( 1.000)*r(117)+( 1.000)*r(118)
        Gain(kROR  )= +( 0.760)*r(115)+( 0.020)*r(116)
        Loss(kHCO3 )= +( 1.000)*r( 80)+( 1.000)*r( 81)+( 1.000)*r( 82)
        Gain(kHCO3 )= +( 1.000)*r( 79)

          JAC(kOH  ,kOH  )= +( 1.000)*r( 12)+( 1.000)*r( 24)
     &                      +( 1.000)*r( 26)+( 1.000)*r( 28)
     &                      +( 1.000)*r( 29)+( 1.000)*r( 33)
     &                      +( 1.000)*r( 37)+( 1.000)*r( 39)
     &                      +( 1.000)*r( 40)+( 4.000)*r( 41)
     &                      +( 4.000)*r( 42)+( 1.000)*r( 43)
     &                      +( 1.000)*r( 47)+( 1.000)*r( 61)
     &                      +( 1.000)*r( 63)+( 1.000)*r( 64)
     &                      +( 1.000)*r( 66)+( 1.000)*r( 67)
          JAC(kOH  ,kOH  )=JAC(kOH  ,kOH  )
     &                      +( 1.000)*r( 71)+( 1.000)*r( 73)
     &                      +( 1.000)*r( 74)+( 1.000)*r( 83)
     &                      +( 1.000)*r( 85)+( 1.000)*r( 96)
     &                      +( 1.000)*r( 98)+( 1.000)*r(100)
     &                      +( 1.000)*r(107)+( 1.000)*r(113)
     &                      +( 1.000)*r(114)+( 1.000)*r(115)
     &                      +( 1.000)*r(120)+( 1.000)*r(124)
     &                      +( 1.000)*r(128)+( 1.000)*r(131)
          JAC(kOH  ,kOH  )=JAC(kOH  ,kOH  )
     &                      +( 1.000)*r(134)+( 1.000)*r(139)
     &                      +( 1.000)*r(141)+( 1.000)*r(142)
     &                      +( 1.000)*r(145)+( 1.000)*r(149)
     &                      +( 1.000)*r(154)
          JAC(kOH  ,kHO2 )= +(-1.000)*r( 13)+(-1.000)*r( 30)
     &                      +( 1.000)*r( 43)+(-1.000)*r( 44)
          JAC(kHO2 ,kOH  )= +(-1.000)*r( 12)+(-1.000)*r( 37)
     &                      +(-1.000)*r( 39)+(-1.000)*r( 40)
     &                      +( 1.000)*r( 43)+(-1.000)*r( 47)
     &                      +(-1.000)*r( 61)+(-1.000)*r( 63)
     &                      +(-1.000)*r( 66)+(-0.300)*r( 71)
     &                      +(-1.000)*r( 73)+(-1.000)*r( 74)
     &                      +(-1.000)*r( 83)+(-1.000)*r(113)
     &                      +(-1.000)*r(114)+(-0.110)*r(115)
     &                      +(-0.950)*r(120)+(-1.000)*r(124)
          JAC(kHO2 ,kOH  )=JAC(kHO2 ,kOH  )
     &                      +(-1.000)*r(128)+(-0.440)*r(131)
     &                      +(-0.600)*r(134)+(-2.000)*r(139)
     &                      +(-0.700)*r(141)+(-0.912)*r(145)
     &                      +(-0.503)*r(149)+(-0.750)*r(154)
          JAC(kHO2 ,kHO2 )= +( 1.000)*r( 13)+( 1.000)*r( 30)
     &                      +( 1.000)*r( 31)+( 4.000)*r( 34)
     &                      +( 4.000)*r( 35)+( 1.000)*r( 43)
     &                      +( 1.000)*r( 44)+( 1.000)*r( 48)
     &                      +( 1.000)*r( 56)+( 1.000)*r( 57)
     &                      +( 1.000)*r( 69)+( 1.000)*r( 79)
     &                      +( 1.000)*r( 82)+( 1.000)*r( 92)
     &                      +( 1.000)*r(108)+( 1.000)*r(137)
          JAC(kHO2 ,kC2O3)= +( 1.000)*r( 92)+(-0.900)*r( 93)
     &                      +(-1.000)*r(112)
          JAC(kHO2 ,kXO2 )= +( 1.000)*r( 56)
          JAC(kHO2 ,kXO2N)= +( 1.000)*r( 57)
          JAC(kHO2 ,kCXO3)= +(-1.000)*r(103)+( 1.000)*r(108)
     &                      +(-1.000)*r(109)+(-4.000)*r(111)
     &                      +(-1.000)*r(112)
          JAC(kHO2 ,kMEO2)= +(-1.000)*r( 68)+( 1.000)*r( 69)
     &                      +(-1.480)*r( 70)+(-0.900)*r( 93)
     &                      +(-1.000)*r(109)
          JAC(kHO2 ,kTO2 )= +(-0.900)*r(132)+(-1.000)*r(133)
          JAC(kHO2 ,kROR )= +(-0.940)*r(116)+(-1.000)*r(117)
          JAC(kHO2 ,kHCO3)= +(-1.000)*r( 80)+(-1.000)*r( 81)
     &                      +( 1.000)*r( 82)
          JAC(kC2O3,kOH  )= +(-1.000)*r( 85)+(-1.000)*r( 96)
     &                      +(-1.000)*r(139)+(-1.000)*r(142)
     &                      +(-0.210)*r(149)
          JAC(kC2O3,kHO2 )= +( 1.000)*r( 92)
          JAC(kC2O3,kC2O3)= +( 1.000)*r( 88)+( 1.000)*r( 89)
     &                      +( 1.000)*r( 92)+( 1.000)*r( 93)
     &                      +( 1.000)*r( 94)+( 4.000)*r( 95)
     &                      +( 1.000)*r(112)
          JAC(kC2O3,kXO2 )= +( 1.000)*r( 94)
          JAC(kC2O3,kCXO3)= +( 1.000)*r(112)
          JAC(kC2O3,kMEO2)= +( 1.000)*r( 93)
          JAC(kXO2 ,kOH  )= +(-1.000)*r( 64)+(-0.300)*r( 71)
     &                      +(-0.991)*r(113)+(-0.100)*r(114)
     &                      +(-0.870)*r(115)+(-0.800)*r(120)
     &                      +(-1.000)*r(124)+(-1.000)*r(128)
     &                      +(-0.080)*r(131)+(-0.600)*r(134)
     &                      +(-1.000)*r(139)+(-0.500)*r(141)
     &                      +(-1.000)*r(142)+(-0.991)*r(145)
     &                      +(-0.713)*r(149)+(-1.250)*r(154)
          JAC(kXO2 ,kHO2 )= +( 1.000)*r( 56)
          JAC(kXO2 ,kC2O3)= +( 1.000)*r( 94)+(-1.000)*r(112)
          JAC(kXO2 ,kXO2 )= +( 1.000)*r( 54)+( 1.000)*r( 56)
     &                      +( 4.000)*r( 58)+( 1.000)*r( 60)
     &                      +( 1.000)*r( 94)+( 1.000)*r(110)
          JAC(kXO2 ,kXO2N)= +( 1.000)*r( 60)
          JAC(kXO2 ,kCXO3)= +(-1.000)*r(103)+(-0.900)*r(109)
     &                      +( 1.000)*r(110)+(-4.000)*r(111)
     &                      +(-1.000)*r(112)
          JAC(kXO2 ,kMEO2)= +(-0.900)*r(109)
          JAC(kXO2 ,kROR )= +(-0.960)*r(116)
          JAC(kXO2N,kOH  )= +(-0.009)*r(113)+(-0.130)*r(115)
     &                      +(-0.088)*r(145)+(-0.250)*r(154)
          JAC(kXO2N,kHO2 )= +( 1.000)*r( 57)
          JAC(kXO2N,kXO2 )= +( 1.000)*r( 60)
          JAC(kXO2N,kXO2N)= +( 1.000)*r( 55)+( 1.000)*r( 57)
     &                      +( 4.000)*r( 59)+( 1.000)*r( 60)
          JAC(kXO2N,kROR )= +(-0.040)*r(116)
          JAC(kCXO3,kOH  )= +(-1.000)*r(100)+(-0.250)*r(149)
          JAC(kCXO3,kHO2 )= +( 1.000)*r(108)
          JAC(kCXO3,kC2O3)= +( 1.000)*r(112)
          JAC(kCXO3,kXO2 )= +( 1.000)*r(110)
          JAC(kCXO3,kCXO3)= +( 1.000)*r(103)+( 1.000)*r(104)
     &                      +( 1.000)*r(108)+( 1.000)*r(109)
     &                      +( 1.000)*r(110)+( 4.000)*r(111)
     &                      +( 1.000)*r(112)
          JAC(kCXO3,kMEO2)= +( 1.000)*r(109)
          JAC(kMEO2,kOH  )= +(-1.000)*r( 67)+(-0.700)*r( 71)
     &                      +(-1.000)*r( 98)
          JAC(kMEO2,kHO2 )= +( 1.000)*r( 69)
          JAC(kMEO2,kC2O3)= +(-1.000)*r( 88)+( 1.000)*r( 93)
     &                      +(-0.900)*r( 93)+(-0.900)*r( 94)
     &                      +(-4.000)*r( 95)+(-1.000)*r(112)
          JAC(kMEO2,kXO2 )= +(-0.900)*r( 94)
          JAC(kMEO2,kCXO3)= +( 1.000)*r(109)+(-1.000)*r(112)
          JAC(kMEO2,kMEO2)= +( 1.000)*r( 68)+( 1.000)*r( 69)
     &                      +( 4.000)*r( 70)+( 1.000)*r( 93)
     &                      +(-0.900)*r( 93)+( 1.000)*r(109)
          JAC(kTO2 ,kOH  )= +(-0.560)*r(131)+(-0.300)*r(141)
          JAC(kTO2 ,kTO2 )= +( 1.000)*r(132)+( 1.000)*r(133)
          JAC(kROR ,kOH  )= +(-0.760)*r(115)
          JAC(kROR ,kROR )= +( 1.000)*r(116)+(-0.020)*r(116)
     &                      +( 1.000)*r(117)+( 1.000)*r(118)
          JAC(kHCO3,kHO2 )= +(-1.000)*r( 79)+( 1.000)*r( 82)
          JAC(kHCO3,kHCO3)= +( 1.000)*r( 80)+( 1.000)*r( 81)
     &                      +( 1.000)*r( 82)
c
c  complete the rates and Jacobian
c
      do i=nstrt,nend
        crold(i) = cncrad(i)
        rate(i) = gain(i) - loss(i)
        do j=nstrt,nend
          jac(i,j) = jac(i,j)/cncrad(j)
        enddo
      enddo
c
c
c  Jacobian is modified due to substitution of TO2
c
c
c    OH   HO2  C2O3   XO2  XO2N  CXO3  MEO2   TO2   ROR  HCO3
c
          JAC(kHO2 ,kOH  ) = JAC(kHO2 ,kOH  )
     &         - JAC(kHO2 ,kTO2 )*JAC(kTO2 ,kOH  )/JAC(kTO2 ,kTO2 )
c
c  end of substitution of TO2
c
c
c  Jacobian is modified due to substitution of ROR
c
c
c    OH   HO2  C2O3   XO2  XO2N  CXO3  MEO2   TO2   ROR  HCO3
c
          JAC(kHO2 ,kOH  ) = JAC(kHO2 ,kOH  )
     &         - JAC(kHO2 ,kROR )*JAC(kROR ,kOH  )/JAC(kROR ,kROR )
          JAC(kXO2 ,kOH  ) = JAC(kXO2 ,kOH  )
     &         - JAC(kXO2 ,kROR )*JAC(kROR ,kOH  )/JAC(kROR ,kROR )
          JAC(kXO2N,kOH  ) = JAC(kXO2N,kOH  )
     &         - JAC(kXO2N,kROR )*JAC(kROR ,kOH  )/JAC(kROR ,kROR )
c
c  end of substitution of ROR
c
c
c  Jacobian is modified due to substitution of HCO3
c
c
c    OH   HO2  C2O3   XO2  XO2N  CXO3  MEO2   TO2   ROR  HCO3
c
          JAC(kHO2 ,kHO2 ) = JAC(kHO2 ,kHO2 )
     &         - JAC(kHO2 ,kHCO3)*JAC(kHCO3,kHO2 )/JAC(kHCO3,kHCO3)
c
c  end of substitution of HCO3
c
c
c  solve the matrix
c
      if (errbig.gt.500.0 .or. lsafe) then
        thresh = 1.0e+15
      else
        thresh = 1.0e-15
      endif
      lusecp = .false.
c
      nendcp=nend-3
      do j=nstrt,nendcp
        if (cncrad(j).le.thresh) then
          do i=nstrt,nendcp
            jac(i,j) = 0.
            jac(j,i) = 0.
          enddo
          jac(j,j) = 1.
      else
        lusecp = .true.
        endif
      enddo
c
      if (lusecp) then
        call cpivot(nstrt,nendcp,MXRADCL,jac,rate,ierr)
        if (ierr.ne.0) goto 900
      endif
c
c  update radical concentrations
c
      errbig = 0.
      weight = 1.
      rlim = 100.0
      if (kount.gt.10) then
        rlim = 10.0
        weight = 0.7
      elseif (kount.gt.50) then
        rlim = 3.0
        weight = 0.5
      endif
      do l=nstrt,nendcp
        if (cncrad(l) .le. thresh) then ! solve independantly
          cncrad(l) = crold(l)*(1.-weight) +
     &                         weight*gain(l)/loss(l)*crold(l)
          cncrad(l) = amax1(cncrad(l),crold(l)/rlim)
          cncrad(l) = amin1(cncrad(l),crold(l)*rlim)
          err = abs((cncrad(l)/crold(l))-1.0)
          err = amin1(err,thresh)
        else                            ! part of coupled solution
          rate(l) = amin1(rate(l), rlim*cncrad(l))
          rate(l) = amax1(rate(l), -cncrad(l)*(1.0-(1.0/rlim)))
          cncrad(l) = cncrad(l) + rate(l)*weight
          err = abs((cncrad(l)/crold(l))-1.0)
        endif
        cncrad(l) = amax1(cncrad(l), bdlrad)
c        cncrad(l) = amin1(cncrad(l), 10.0)
        errbig = amax1(errbig,err)
      enddo
c
      if (kount.eq.100) then
        lsafe = .true.
        write(iout,'(a)') 'WARNING:'
        write(iout,'(a)') 'Monitor slow convergence in RADSLVR6'
        write(iout,'(a3,30a9)')
     &     ' ', (nmrad(l),l=nstrt,nendcp),'rel err'
        do l=nstrt,nendcp
          cncrad(l) = 1.0e-12
        enddo
      endif
      if (kount.ge.100)
     &   write(iout,'(i3,1p30e9.2)')
     &     kount, (cncrad(l),l=nstrt,nendcp), errbig
c
      r( 12) = rk( 12)*conc(kO3)*cncrad(kOH)
      r( 13) = rk( 13)*conc(kO3)*cncrad(kHO2)
      r( 24) = rk( 24)*conc(kNO)*cncrad(kOH)
      r( 26) = rk( 26)*cncrad(kOH)*conc(kHONO)
      r( 28) = rk( 28)*conc(kNO2)*cncrad(kOH)
      r( 29) = rk( 29)*cncrad(kOH)*conc(kHNO3)
      r( 30) = rk( 30)*cncrad(kHO2)*conc(kNO)
      r( 31) = rk( 31)*cncrad(kHO2)*conc(kNO2)
      r( 33) = rk( 33)*cncrad(kOH)*conc(kPNA)
      r( 34) = rk( 34)*cncrad(kHO2)*cncrad(kHO2)
      r( 35) = rk( 35)*cncrad(kHO2)*cncrad(kHO2)*H2O
      r( 37) = rk( 37)*cncrad(kOH)*conc(kH2O2)
      r( 39) = rk( 39)*cncrad(kOH)*H2
      r( 40) = rk( 40)*cncrad(kOH)*cncrad(kO)
      r( 41) = rk( 41)*cncrad(kOH)*cncrad(kOH)
      r( 42) = rk( 42)*cncrad(kOH)*cncrad(kOH)
      r( 43) = rk( 43)*cncrad(kOH)*cncrad(kHO2)
      r( 44) = rk( 44)*cncrad(kHO2)*cncrad(kO)
      r( 47) = rk( 47)*cncrad(kNO3)*cncrad(kOH)
      r( 48) = rk( 48)*cncrad(kNO3)*cncrad(kHO2)
      r( 54) = rk( 54)*cncrad(kXO2)*conc(kNO)
      r( 55) = rk( 55)*cncrad(kXO2N)*conc(kNO)
      r( 56) = rk( 56)*cncrad(kXO2)*cncrad(kHO2)
      r( 57) = rk( 57)*cncrad(kXO2N)*cncrad(kHO2)
      r( 58) = rk( 58)*cncrad(kXO2)*cncrad(kXO2)
      r( 59) = rk( 59)*cncrad(kXO2N)*cncrad(kXO2N)
      r( 60) = rk( 60)*cncrad(kXO2)*cncrad(kXO2N)
      r( 61) = rk( 61)*conc(kNTR)*cncrad(kOH)
      r( 63) = rk( 63)*conc(kSO2)*cncrad(kOH)
      r( 64) = rk( 64)*conc(kROOH)*cncrad(kOH)
      r( 66) = rk( 66)*cncrad(kOH)*conc(kCO)
      r( 67) = rk( 67)*cncrad(kOH)*CH4
      r( 68) = rk( 68)*cncrad(kMEO2)*conc(kNO)
      r( 69) = rk( 69)*cncrad(kMEO2)*cncrad(kHO2)
      r( 70) = rk( 70)*cncrad(kMEO2)*cncrad(kMEO2)
      r( 71) = rk( 71)*conc(kMEPX)*cncrad(kOH)
      r( 73) = rk( 73)*conc(kMEOH)*cncrad(kOH)
      r( 74) = rk( 74)*conc(kFORM)*cncrad(kOH)
      r( 79) = rk( 79)*conc(kFORM)*cncrad(kHO2)
      r( 80) = rk( 80)*cncrad(kHCO3)
      r( 81) = rk( 81)*cncrad(kHCO3)*conc(kNO)
      r( 82) = rk( 82)*cncrad(kHCO3)*cncrad(kHO2)
      r( 83) = rk( 83)*conc(kFACD)*cncrad(kOH)
      r( 85) = rk( 85)*conc(kALD2)*cncrad(kOH)
      r( 88) = rk( 88)*cncrad(kC2O3)*conc(kNO)
      r( 89) = rk( 89)*cncrad(kC2O3)*conc(kNO2)
      r( 92) = rk( 92)*cncrad(kC2O3)*cncrad(kHO2)
      r( 93) = rk( 93)*cncrad(kC2O3)*cncrad(kMEO2)
      r( 94) = rk( 94)*cncrad(kC2O3)*cncrad(kXO2)
      r( 95) = rk( 95)*cncrad(kC2O3)*cncrad(kC2O3)
      r( 96) = rk( 96)*conc(kPACD)*cncrad(kOH)
      r( 98) = rk( 98)*conc(kAACD)*cncrad(kOH)
      r(100) = rk(100)*conc(kALDX)*cncrad(kOH)
      r(103) = rk(103)*cncrad(kCXO3)*conc(kNO)
      r(104) = rk(104)*cncrad(kCXO3)*conc(kNO2)
      r(107) = rk(107)*conc(kPANX)*cncrad(kOH)
      r(108) = rk(108)*cncrad(kCXO3)*cncrad(kHO2)
      r(109) = rk(109)*cncrad(kCXO3)*cncrad(kMEO2)
      r(110) = rk(110)*cncrad(kCXO3)*cncrad(kXO2)
      r(111) = rk(111)*cncrad(kCXO3)*cncrad(kCXO3)
      r(112) = rk(112)*cncrad(kCXO3)*cncrad(kC2O3)
      r(113) = rk(113)*cncrad(kOH)*conc(kETHA)
      r(114) = rk(114)*cncrad(kOH)*conc(kETOH)
      r(115) = rk(115)*conc(kPAR)*cncrad(kOH)
      r(116) = rk(116)*cncrad(kROR)
      r(117) = rk(117)*cncrad(kROR)
      r(118) = rk(118)*cncrad(kROR)*conc(kNO2)
      r(120) = rk(120)*cncrad(kOH)*conc(kOLE)
      r(124) = rk(124)*cncrad(kOH)*conc(kETH)
      r(128) = rk(128)*conc(kIOLE)*cncrad(kOH)
      r(131) = rk(131)*conc(kTOL)*cncrad(kOH)
      r(132) = rk(132)*cncrad(kTO2)*conc(kNO)
      r(133) = rk(133)*cncrad(kTO2)
      r(134) = rk(134)*cncrad(kOH)*conc(kCRES)
      r(137) = rk(137)*cncrad(kCRO)*cncrad(kHO2)
      r(139) = rk(139)*conc(kOPEN)*cncrad(kOH)
      r(141) = rk(141)*cncrad(kOH)*conc(kXYL)
      r(142) = rk(142)*cncrad(kOH)*conc(kMGLY)
      r(145) = rk(145)*cncrad(kOH)*conc(kISOP)
      r(149) = rk(149)*cncrad(kOH)*conc(kISPD)
      r(154) = rk(154)*conc(kTERP)*cncrad(kOH)
      if (errbig.gt.tol) goto 14
c
c  new group of radicals
c
c   TO2
c
        Loss(kTO2  )= +( 1.000)*r(132)+( 1.000)*r(133)
        Gain(kTO2  )= +( 0.560)*r(131)+( 0.300)*r(141)
c
c  first order method chosen for this radical
c  solved by direct substitution
c
      cncrad(kTO2) = gain(kTO2)/loss(kTO2)*cncrad(kTO2)
      cncrad(kTO2) = amax1(bdlrad, cncrad(kTO2))
      r(132) = rk(132)*cncrad(kTO2)*conc(kNO)
      r(133) = rk(133)*cncrad(kTO2)
c
c  new group of radicals
c
c   ROR
c
        Loss(kROR  )= +( 1.000)*r(116)+( 1.000)*r(117)+( 1.000)*r(118)
        Gain(kROR  )= +( 0.760)*r(115)+( 0.020)*r(116)
c
c  first order method chosen for this radical
c  solved by direct substitution
c
      cncrad(kROR) = gain(kROR)/loss(kROR)*cncrad(kROR)
      cncrad(kROR) = amax1(bdlrad, cncrad(kROR))
      r(116) = rk(116)*cncrad(kROR)
      r(117) = rk(117)*cncrad(kROR)
      r(118) = rk(118)*cncrad(kROR)*conc(kNO2)
c
c  new group of radicals
c
c  HCO3
c
        Loss(kHCO3 )= +( 1.000)*r( 80)+( 1.000)*r( 81)+( 1.000)*r( 82)
        Gain(kHCO3 )= +( 1.000)*r( 79)
c
c  first order method chosen for this radical
c  solved by direct substitution
c
      cncrad(kHCO3) = gain(kHCO3)/loss(kHCO3)*cncrad(kHCO3)
      cncrad(kHCO3) = amax1(bdlrad, cncrad(kHCO3))
      r( 80) = rk( 80)*cncrad(kHCO3)
      r( 81) = rk( 81)*cncrad(kHCO3)*conc(kNO)
      r( 82) = rk( 82)*cncrad(kHCO3)*cncrad(kHO2)
c
c  new group of radicals
c
c   CRO
c
        Loss(kCRO  )= +( 1.000)*r(136)+( 1.000)*r(137)
        Gain(kCRO  )= +( 0.400)*r(134)+( 1.000)*r(135)
c
c  first order method chosen for this radical
c  solved by direct substitution
c
      cncrad(kCRO) = gain(kCRO)/loss(kCRO)*cncrad(kCRO)
      cncrad(kCRO) = amax1(bdlrad, cncrad(kCRO))
      r(136) = rk(136)*cncrad(kCRO)*conc(kNO2)
      r(137) = rk(137)*cncrad(kCRO)*cncrad(kHO2)
c
      return
c
 900  continue
      write(iout,'(//,A,//)') 'ERROR in RADSLVR6:'
      write(iout,*) 'Zero determinant in CPIVOT at ', ierr
      write(iout,*) 'igrd,i, j, k = ', igrdchm,ichm,jchm,kchm
      write(iout,*) 'LDARK is set ', ldark
      do l=1,ngas
        write(iout,'(i3,2x,a7,1pe10.3)') l,spname(l),conc(l)
      enddo
      write(iout,*) 'The radicals are: '
      do l= 1 , nrad
        write(iout,'(i3,2x,a7,1pe10.3)')
     &        l,nmrad(l),cncrad(l)
      enddo
      write(iout,*) 'Currently solving ', nstrt, ' to ',nendcp
      do l=nstrt,nendcp
        write(iout,'(i3,2x,a7,1p2e10.3)')
     &        l,nmrad(l),cncrad(l),abs(rate(l))/cncrad(l)
      enddo
      call camxerr()
      end

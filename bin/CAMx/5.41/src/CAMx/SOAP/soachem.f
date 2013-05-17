      subroutine soachem(iout,noxin,nprin,nCG,tempk,press,dthr,
     &                        avgox,cprec,cCG)
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c   Description:
c     This routine solves the oxidation reactions for SOA precursors
c     assuming first-order decay and constant oxidant concentrations
c
c     Anthropogenic:
c
c        TOLA + OH  -> 0.044 CG1 + 0.085 CG2 ; 1.80e-12 @ -355.
c        XYLA + OH  -> 0.027 CG1 + 0.118 CG2 ; 1.70e-11 @ -116.
c       (m-XYL)
c
c     Biogenic:
c
c        ISP  + O   ->                       ; 3.60e-11
c        ISP  + OH  -> 0.015 CG3 + 0.12 CG4  ; 2.54e-11 @ -407.6
c        ISP  + O3  ->                       ; 7.86e-15 @ 1912.
c        ISP  + NO3 ->                       ; 3.03e-12 @ 448.
c
c        TRP  + O   -> 0.065 CG5 + 0.29 CG6  ; 2.79e-11
c        TRP  + OH  -> 0.065 CG5 + 0.29 CG6  ; 1.20e-11 @ -440.
c        TRP  + O3  -> 0.065 CG5 + 0.29 CG6  ; 6.30e-16 @ 580.
c        TRP  + NO3 -> 0.065 CG5 + 0.29 CG6  ; 1.20e-12 @ -490.
c       (a-pinene)
c
c        SQT  + OH  -> 0.85 CG7              ; 1.97e-10
c        SQT  + O3  -> 0.85 CG7              ; 1.16e-14
c        SQT  + NO3 -> 0.85 CG7              ; 1.90e-11
c       (b-caryophellene)
c
c     Rate constant:
c        k = A exp(-E/T)                     ; A [cm3/molecule-sec] @ E [K]
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c     Input arguments:
c        iout     standard output file unit
c        noxin    number of oxidants
c        nprin    number of precursors
c        nCG      number of CG species
c        tempk    cell temperature [K]
c        press    cell pressure [mb]
c        dthr     time duration to be integrated [hr]
c        avgox    average oxidant concentrations [ppm]
c                 1 - O; 2 - OH; 3 - O3, 4 - NO3
c        cprec    precursor concentrations [ppm]
c                 1 - TOLA; 2 - XYLA; 3 - ISP; 4 - TRP; 5 - SQT
c        cCG      CG species concentrations [ppm]
c                 1 - CG1; 2 - CG2; 3 - CG3; 4 - CG4; 5 - CG5; 6 - CG6; 7 - CG7
c
c     Output arguments:
c        cprec    precursor concentrations [ppm]
c        cCG      CG species concentrations [ppm]
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     03/22/06  --bkoo--  Original development
c     06/20/06  --bkoo--  2-product model for isoprene (Henze & Seinfeld, 2006)
c
c-----------------------------------------------------------------------
c
      include 'soap.inc'

      integer     noxin,nprin,nCG
      real        tempk,press,dthr,avgox(noxin),cprec(nprin),cCG(nCG)

      integer     nrxn, nox, nprec
      parameter ( nrxn  = 13 )           ! # of reactions
      parameter ( nox   = 4  )
      parameter ( nprec = 5  )

      real*8      rkpar(2,nrxn) ! rate constant parameters
c
      real        yval(2,nrxn) ! yield [ppm/ppm]
      integer     idCG(2,nrxn) ! CG ID for the oxidation products
      integer     mox(nprec),idox(nox,nprec),idrxn(nox,nprec)
      real*8      cf0, cfac
c                       A         E
      data  rkpar / 1.80d-12, -355.0d0,  ! rxn 1
     &              1.70d-11, -116.0d0,  ! rxn 2
     &              3.60d-11,    0.0d0,  ! rxn 3
     &              2.54d-11, -407.6d0,  ! rxn 4
     &              7.86d-15, 1912.0d0,  ! rxn 5
     &              3.03d-12,  448.0d0,  ! rxn 6
     &              2.79d-11,    0.0d0,  ! rxn 7
     &              1.20d-11, -440.0d0,  ! rxn 8
     &              6.30d-16,  580.0d0,  ! rxn 9
     &              1.20d-12, -490.0d0,  ! rxn 10
     &              1.97d-10,    0.0d0,  ! rxn 11
     &              1.16d-14,    0.0d0,  ! rxn 12
     &              1.90d-11,    0.0d0 / ! rxn 13

c                  Product 1    Product 2
      data  yval  / 0.044,       0.085,  ! rxn 1
     &              0.027,       0.118,  ! rxn 2
     &              0.0  ,       0.0  ,  ! rxn 3
     &              0.015,       0.12 ,  ! rxn 4
     &              0.0  ,       0.0  ,  ! rxn 5
     &              0.0  ,       0.0  ,  ! rxn 6
     &              0.065,       0.29 ,  ! rxn 7
     &              0.065,       0.29 ,  ! rxn 8
     &              0.065,       0.29 ,  ! rxn 9
     &              0.065,       0.29 ,  ! rxn 10
     &              0.85 ,       0.0  ,  ! rxn 11
     &              0.85 ,       0.0  ,  ! rxn 12
     &              0.85 ,       0.0   / ! rxn 13

c                  Product 1    Product 2
      data  idCG  / 1,           2,      ! rxn 1
     &              1,           2,      ! rxn 2
     &              3,           4,      ! rxn 3
     &              3,           4,      ! rxn 4
     &              3,           4,      ! rxn 5
     &              3,           4,      ! rxn 6
     &              5,           6,      ! rxn 7
     &              5,           6,      ! rxn 8
     &              5,           6,      ! rxn 9
     &              5,           6,      ! rxn 10
     &              7,           7,      ! rxn 11
     &              7,           7,      ! rxn 12
     &              7,           7 /     ! rxn 13

      data  mox   / 1,  1,  4,  4,  3 / ! # of oxidants to react
c                  Oxidant ID
      data  idox  / 2,  0,  0,  0,  ! TOLA
     &              2,  0,  0,  0,  ! XYLA
     &              1,  2,  3,  4,  ! ISP
     &              1,  2,  3,  4,  ! TRP
     &              2,  3,  4,  0 / ! SQT
c                  Reaction ID 
      data  idrxn / 1,  0,  0,  0,  ! TOLA
     &              2,  0,  0,  0,  ! XYLA
     &              3,  4,  5,  6,  ! ISP
     &              7,  8,  9, 10,  ! TRP
     &             11, 12, 13,  0 / ! SQT

c     [1/ppm-hr]=[cm3/molecule-sec]*3600*(6.022e23*1.e-12/8.314)*(P[Pa]/T[K])
      data  cf0 / 2.608d14 /      ! 3600*(6.022e23*1.e-12/8.314)

      real        rktmp(nrxn), rk1(nox), sumk, dprec, dfac
      integer     i, j, k, nchk

      save        rkpar, yval, idCG, mox, idox, idrxn, cf0
c
c-----Check consistency
c
      nchk = abs(noxin-nox) + abs(nprin-nprec) + abs(nCG-nsoap)
      if (nchk .ne. 0) then
        write(iout,'(//,a)') 'ERROR in SOACHEM:'
        write(iout,*)'Inconsistency in the number of species'
        write(iout,*)'noxin,nprin,nCG: ',noxin,nprin,nCG
        write(iout,*)'nox,nprec,nsoap: ',nox,nprec,nsoap
        call camxerr()
      endif
c
c-----Conversion factor for rate constants
c
      cfac = cf0 * DBLE(press*100./tempk) ! [1/ppm-hr] = cfac * [cm3/molecule-sec]
c
c-----Rate constants in [1/ppm-hr]
c
      do i = 1, nrxn
        rktmp(i) = SNGL( rkpar(1,i)*dexp(-rkpar(2,i)/DBLE(tempk))*cfac )
      enddo
c
c-----Calculate the amount of precursor reacted / CG produced
c
      do j = 1, nprec
        sumk = 0.0
        do k = 1, mox(j)
          rk1(k) = rktmp(idrxn(k,j)) * avgox(idox(k,j))
          sumk = sumk + rk1(k)
        enddo
        dprec = cprec(j) * ( 1. - exp(-sumk*dthr) )
        if (dprec.lt.1.e-12) cycle
        cprec(j) = cprec(j) - dprec
        do k = 1, mox(j)
          dfac = rk1(k) / sumk * dprec
          cCG(idCG(1,idrxn(k,j))) = cCG(idCG(1,idrxn(k,j)))       ! 1st product
     &                                + yval(1,idrxn(k,j)) * dfac
          cCG(idCG(2,idrxn(k,j))) = cCG(idCG(2,idrxn(k,j)))       ! 2nd product
     &                                + yval(2,idrxn(k,j)) * dfac
        enddo
      enddo

      return
      end


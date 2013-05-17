      subroutine readchm
      use filunit
      use grid
      use chmstry
      use tracer
      implicit none
c
c----CAMx v6.00 130506
c
c     READCHM reads the CAMx chemistry parameter file, which defines
c     the chemical system to be simulated
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c  
c     Modifications:
c        10/26/99  Added check for zero Henry's law constants to avoid a
c                  divide by 0 later in the code
c        10/20/00  Added CAMx version as first record on chemistry parameters
c                  file
c        1/9/02    Aerosol size cut points and density now defined on
c                  chemistry parameters file; removed conversion of aerosol
c                  BDNL values from ug/m3 to umol/m3
c        1/18/02   Added EC, PFIN, and PCRS to mechanism 4 species list
c        12/12/02  Expanded species list for Mechanism 4
c        3/26/03   Added surface resistance scaling factor to gas params
c        4/21/04   Incorporated sectional PM (Mech 4 CMU)
c        10/14/04  Added Mechanism 10 (user defined)
c        10/05/05  Split gas-phase mechanism ID and aerosol option string
c                  to separate records in the chemistry parameters file
c        12/29/06 -bkoo-     Expanded species list for the updated SOA scheme
c        01/08/07 -bkoo-     Added Mechanism 6 (CB05)
c                            Revised the code to re-order fast species (now use FASTS list)
c        01/10/07 -bkoo-     PM modules now linked to mechanisms 4 thru 6
c        07/04/07 -bkoo-     Added code to set pointer to hydrolysis of N2O5
c        12/15/08 -gwilson-  Added code to handle averaging of radicals
c        01/29/09 -bkoo-     De-activated mechanism 1
c        04/22/10 -gwilson-  Removed the code that eliminted PM species for
c                            SAPRC
c        11/20/10 -gwilson-  Consolidated radical and concentration arrays
c        12/21/10 -bkoo-     Added Mechanism 7 (CB6)
c        01/20/11 -gwilson-  De-activated Mechanisms 3 and 4
c        03/29/11 -cemery-   Revised to allow for inert PM with gas-phase
c                            chemistry and to support in-line TUV with
c                            aerosol optical depth
c        05/20/11 -bkoo-     Added Mechanisms 8 and 9 (CB05 and SAPRC99 extensions)
c        04/12/12 -cemery-   Added T and P adjustments to photolysis rates
c        07/20/12 -ou-       Added Mechanism 1 (CB6 with Iodine chemistry)
c        10/08/12 -jjung-    Added Mechanism 2 (CB6r1)
c        12/12/12 -bkoo-     Set I,IO,OIO as IEH steady-state radicals (mech1,8,9)
c        01/15/13 -cemery-   Replaced gas "diffrat" with "Molwt"; diffrat now
c                            calculated from Molwt when read
c
c     Input arguments: 
c        none 
c 
c     Output arguments: 
c        none 
c            
c     Routines called: 
c        ALLOC_CHMSTRY
c        AEROSET
c        EXPTBL
c        KTHERM
c            
c     Called by:
c        STARTUP
c
      include 'camx.prm'
      include 'flags.inc'
      include 'ddmchm.inc'
      include 'iehchem.inc'
      include 'lsbox.inc'
c
      integer istrln
c
      character*180    record, recpht1
      character*10     nametmp, splist(NSPNAM)
      character*10     fasts1(35), fasts2(29)
      character*10     fasts5(31), fasts6(21), fasts7(32)
      character*10     fasts8(36), fasts9(26)
      character*10     blank,camxv,camxvin
      character*10     tmpnam, tmpnam1
      integer          mchactv(10), mchgas(10), mchaero(10), mchrad(10)
      integer          mchrxn(10), mchphot(10), mchfast(10)
      integer          mchiessr(10)
      integer          jno2s99, jno2cb5, jno2cb6
      integer          jo3s99, jo3cb5, jo3cb6
      integer          jhcho1s99, jhcho1cb5, jhcho1cb6
      integer          jhcho2s99, jhcho2cb5, jhcho2cb6
      integer          jch3chos99, jch3chocb5, jch3chocb6
      integer          ipigsap, ipigcb05, ipigcb6
      integer          ihyds99, ihydcb05, ihydcb6
      integer          npar(7)
      integer          nsec_c,  nphot, nerr, nhit, iaero, ierr, iref
      integer          ncms, nppm, num, iunits, isec
      integer          i, j, l, nn, n, m, nl
      integer          ibin
      integer          rhadj_tmp
      real             tdum(3), pdum(3), tmp
      real             bdnl_tmp,roprt_tmp,bext_tmp,ssa_tmp,molwt
      real*8           dsec_i(MXSECT+1)
c
      double precision rxnpar(MXREACT,12)
      real             kdum(MXREACT,3)
      integer          rxntyp(MXREACT)
      integer          rxnord(MXREACT)
c
c-----Data that define the mechanism/solver options:
c
c  ********************* NOTE for the FASTS arrays *********************
c  *  The IEH fast species must be listed in the FASTS array for each  *
c  *  mechanism. The order of the species in each FASTS array must be  *
c  *  as follows:                                                      *
c  *   (1) IEH steady-state radicals                                   *
c  *   (2) CAMx radicals that do not belong to (1)                     *
c  *   (3) IEH fast species that do not belong to (2)                  *
c  *********************************************************************
c
      data camxv  /'VERSION6.0'/
      data blank  /'BLANK     '/
c
      data (splist(j),j=1,270)
     &            /'O1D       ','O         ','CL        ',
     &             'CLO       ','OH        ','HO2       ',
     &             'C2O3      ','XO2       ','XO2N      ',
     &             'CXO3      ','MEO2      ','TO2       ',
     &             'ROR       ','CRO       ','RO2R      ',
     &             'R2O2      ','RO2N      ','CCO3      ',
     &             'RCO3      ','MCO3      ','BZCO      ',
     &             'CXO2      ','HCO3      ','TBUO      ',
     &             'BZO       ','BZNO      ','I         ',
     &             'NO        ','NO2       ','O3        ',
     &             'PAN       ','PANX      ','CRES      ',
     &             'PAN2      ','MPAN      ','PBZN      ',
     &             'NPHE      ','RNO3      ','DCB2      ',
     &             'DCB3      ','HNO4      ','BZO2      ',
     &             'CAO2      ','CRN2      ','CRNO      ',
     &             'EPX2      ','ISO2      ','OPAN      ',
     &             'OPO3      ','RO2       ','XLO2      ',
     &             'XO2H      ','AACD      ','ACET      ',
     &             'ALD2      ','ALDX      ','ALK1      ',
     &             'ALK2      ','ALK3      ','ALK4      ',
     &             'ALK5      ','ARO1      ','ARO2      ',
     &             'BACL      ','BALD      ','BCL1      ',
     &             'BCL2      ','BENZ      ','BUTA      ',
     &             'C3H7I     ','CAT1      ','CCHO      ',
     &             'CCRS      ','CG1       ','CG2       ',
     &             'CG3       ','CG4       ','CG5       ',
     &             'CG6       ','CG7       ','CH2I2     ',
     &             'CH2IBr    ','CH2ICL    ','CH3I      ',
     &             'CL2       ','CO        ','CO2H      ',
     &             'CO3H      ','COOH      ','CPRM      ',
     &             'CRON      ','CRPX      ','DCB1      ',
     &             'EPOX      ','ETH       ','ETHA      ',
     &             'ETHE      ','ETHY      ','ETOH      ',
     &             'FACD      ','FCRS      ','FMCL      ',
     &             'FORM      ','FPRM      ','GLY       ',
     &             'GLYD      ','H2O2      ','HC2H      ',
     &             'HCHO      ','HCL       ','HG0       ',
     &             'HG2       ','HGP       ','HI        ',
     &             'HIO3      ','HNO3      ','HO2H      ',
     &             'HOCL      ','HOI       ','HOIO2     ',
     &             'HONO      ','HPLD      ','I2        ',
     &             'I2O2      ','IALK      ','IBAC      ',
     &             'IBUO      ','ICL1      ','ICL2      ',
     &             'INO       ','INO2      ','INO3      ',
     &             'INTR      ','IO        ','IOLE      ',
     &             'IONO2     ','ISOP      ','ISP       ',
     &             'ISPD      ','ISPX      ','IXOY      ',
     &             'KET       ','MBUT      ','MEK       ',
     &             'MEOH      ','MEPX      ','METH      ',
     &             'MGLY      ','MIB       ','MIC       ',
     &             'MI2       ','MTBE      ','MVK       ',
     &             'N2O5      ','N3CL      ','NA        ',
     &             'NBUI      ','NH3       ','NO3       ',
     &             'NTCL      ','NTR       ','OIO       ',
     &             'OLE       ','OLE1      ','OLE2      ',
     &             'OPEN      ','PACD      ','PAR       ',
     &             'PCL       ','PEC       ','PH2O      ',
     &             'PHEN      ','PNA       ','PNH4      ',
     &             'PNO3      ','POA       ','PROD      ',
     &             'PRPA      ','PSO4      ','RC2H      ',
     &             'RC3H      ','RCHO      ','ROOH      ',
     &             'SO2       ','SOA1      ','SOA2      ',
     &             'SOA3      ','SOA4      ','SOA5      ',
     &             'SOA6      ','SOA7      ','SOPA      ',
     &             'SOPB      ','SQT       ','SS        ',
     &             'SULF      ','TERP      ','TOL       ',
     &             'TOLA      ','TRP       ','XN        ',
     &             'XOPN      ','XYL       ','XYLA      ',
     &             'SOA1_1    ','SOA1_2    ','SOA1_3    ',
     &             'SOA1_4    ','SOA1_5    ','SOA1_6    ',
     &             'SOA1_7    ','SOA1_8    ','SOA1_9    ',
     &             'SOA1_10   ','SOA2_1    ','SOA2_2    ',
     &             'SOA2_3    ','SOA2_4    ','SOA2_5    ',
     &             'SOA2_6    ','SOA2_7    ','SOA2_8    ',
     &             'SOA2_9    ','SOA2_10   ','SOA3_1    ',
     &             'SOA3_2    ','SOA3_3    ','SOA3_4    ',
     &             'SOA3_5    ','SOA3_6    ','SOA3_7    ',
     &             'SOA3_8    ','SOA3_9    ','SOA3_10   ',
     &             'SOA4_1    ','SOA4_2    ','SOA4_3    ',
     &             'SOA4_4    ','SOA4_5    ','SOA4_6    ',
     &             'SOA4_7    ','SOA4_8    ','SOA4_9    ',
     &             'SOA4_10   ','SOA5_1    ','SOA5_2    ',
     &             'SOA5_3    ','SOA5_4    ','SOA5_5    ',
     &             'SOA5_6    ','SOA5_7    ','SOA5_8    ',
     &             'SOA5_9    ','SOA5_10   ','SOA6_1    ',
     &             'SOA6_2    ','SOA6_3    ','SOA6_4    ',
     &             'SOA6_5    ','SOA6_6    ','SOA6_7    ',
     &             'SOA6_8    ','SOA6_9    ','SOA6_10   ',
     &             'SOA7_1    ','SOA7_2    ','SOA7_3    ',
     &             'SOA7_4    ','SOA7_5    ','SOA7_6    '/
      data (splist(j),j=271,NSPNAM)
     &            /'SOA7_7    ','SOA7_8    ','SOA7_9    ',
     &             'SOA7_10   ','SOPA_1    ','SOPA_2    ',
     &             'SOPA_3    ','SOPA_4    ','SOPA_5    ',
     &             'SOPA_6    ','SOPA_7    ','SOPA_8    ',
     &             'SOPA_9    ','SOPA_10   ','SOPB_1    ',
     &             'SOPB_2    ','SOPB_3    ','SOPB_4    ',
     &             'SOPB_5    ','SOPB_6    ','SOPB_7    ',
     &             'SOPB_8    ','SOPB_9    ','SOPB_10   ',
     &             'POA_1     ','POA_2     ','POA_3     ',
     &             'POA_4     ','POA_5     ','POA_6     ',
     &             'POA_7     ','POA_8     ','POA_9     ',
     &             'POA_10    ','PEC_1     ','PEC_2     ',
     &             'PEC_3     ','PEC_4     ','PEC_5     ',
     &             'PEC_6     ','PEC_7     ','PEC_8     ',
     &             'PEC_9     ','PEC_10    ','CRST_1    ',
     &             'CRST_2    ','CRST_3    ','CRST_4    ',
     &             'CRST_5    ','CRST_6    ','CRST_7    ',
     &             'CRST_8    ','CRST_9    ','CRST_10   ',
     &             'PH2O_1    ','PH2O_2    ','PH2O_3    ',
     &             'PH2O_4    ','PH2O_5    ','PH2O_6    ',
     &             'PH2O_7    ','PH2O_8    ','PH2O_9    ',
     &             'PH2O_10   ','PCL_1     ','PCL_2     ',
     &             'PCL_3     ','PCL_4     ','PCL_5     ',
     &             'PCL_6     ','PCL_7     ','PCL_8     ',
     &             'PCL_9     ','PCL_10    ','NA_1      ',
     &             'NA_2      ','NA_3      ','NA_4      ',
     &             'NA_5      ','NA_6      ','NA_7      ',
     &             'NA_8      ','NA_9      ','NA_10     ',
     &             'PNH4_1    ','PNH4_2    ','PNH4_3    ',
     &             'PNH4_4    ','PNH4_5    ','PNH4_6    ',
     &             'PNH4_7    ','PNH4_8    ','PNH4_9    ',
     &             'PNH4_10   ','PNO3_1    ','PNO3_2    ',
     &             'PNO3_3    ','PNO3_4    ','PNO3_5    ',
     &             'PNO3_6    ','PNO3_7    ','PNO3_8    ',
     &             'PNO3_9    ','PNO3_10   ','PSO4_1    ',
     &             'PSO4_2    ','PSO4_3    ','PSO4_4    ',
     &             'PSO4_5    ','PSO4_6    ','PSO4_7    ',
     &             'PSO4_8    ','PSO4_9    ','PSO4_10   ',
     &             'HGIIP     ','HGIIPC    '/
c
      data fasts1 /'O1D       ','O         ','I         ','IO        ',
     &             'OIO       ','OH        ','HO2       ','C2O3      ',
     &             'XO2       ','XO2N      ','CXO3      ','MEO2      ',
     &             'TO2       ','ROR       ','HCO3      ','CRO       ',
     &             'BZO2      ','CAO2      ','CRN2      ','CRNO      ',
     &             'EPX2      ','ISO2      ','OPO3      ','RO2       ',
     &             'XLO2      ','XO2H      ','N2O5      ','NO3       ',
     &             'NO        ','NO2       ','O3        ','PAN       ',
     &             'PANX      ','PNA       ','OPAN      '/
      data fasts2 /'O1D       ','O         ','OH        ','HO2       ',
     &             'C2O3      ','XO2       ','XO2N      ','CXO3      ',
     &             'MEO2      ','TO2       ','ROR       ','HCO3      ',
     &             'CRO       ','BZO2      ','EPX2      ','ISO2      ',
     &             'OPO3      ','RO2       ','XLO2      ','XO2H      ',
     &             'N2O5      ','NO3       ','NO        ','NO2       ',
     &             'O3        ','PAN       ','PANX      ','PNA       ',
     &             'OPAN      '/
      data fasts5 /'O1D       ','O         ','OH        ','HO2       ',
     &             'RO2R      ','R2O2      ','RO2N      ','CCO3      ',
     &             'RCO3      ','MCO3      ','BZCO      ','CXO2      ',
     &             'HCO3      ','TBUO      ','BZO       ','BZNO      ',
     &             'N2O5      ','NO3       ','NO        ','NO2       ',
     &             'O3        ','PAN       ','CRES      ','PAN2      ',
     &             'MPAN      ','PBZN      ','NPHE      ','RNO3      ',
     &             'DCB2      ','DCB3      ','HNO4      '/
      data fasts6 /'O1D       ','O         ','OH        ','HO2       ',
     &             'C2O3      ','XO2       ','XO2N      ','CXO3      ',
     &             'MEO2      ','TO2       ','ROR       ','HCO3      ',
     &             'CRO       ','N2O5      ','NO3       ','NO        ',
     &             'NO2       ','O3        ','PAN       ','PANX      ',
     &             'PNA       '/
      data fasts7 /'O1D       ','O         ','OH        ','HO2       ',
     &             'C2O3      ','XO2       ','XO2N      ','CXO3      ',
     &             'MEO2      ','TO2       ','ROR       ','HCO3      ',
     &             'CRO       ','BZO2      ','CAO2      ','CRN2      ',
     &             'CRNO      ','EPX2      ','ISO2      ','OPO3      ',
     &             'RO2       ','XLO2      ','XO2H      ','N2O5      ',
     &             'NO3       ','NO        ','NO2       ','O3        ',
     &             'PAN       ','PANX      ','PNA       ','OPAN      '/
      data fasts8 /'O1D       ','O         ','CL        ','CLO       ',
     &             'I         ','IO        ','OIO       ','OH        ',
     &             'HO2       ','RO2R      ','R2O2      ','RO2N      ',
     &             'CCO3      ','RCO3      ','MCO3      ','BZCO      ',
     &             'CXO2      ','HCO3      ','TBUO      ','BZO       ',
     &             'BZNO      ','N2O5      ','NO3       ','NO        ',
     &             'NO2       ','O3        ','PAN       ','CRES      ',
     &             'PAN2      ','MPAN      ','PBZN      ','NPHE      ',
     &             'RNO3      ','DCB2      ','DCB3      ','HNO4      '/
      data fasts9 /'O1D       ','O         ','CL        ','CLO       ',
     &             'I         ','IO        ','OIO       ','OH        ',
     &             'HO2       ','C2O3      ','XO2       ','XO2N      ',
     &             'CXO3      ','MEO2      ','TO2       ','ROR       ',
     &             'HCO3      ','CRO       ','N2O5      ','NO3       ',
     &             'NO        ','NO2       ','O3        ','PAN       ',
     &             'PANX      ','PNA       '/
c
      data mchactv  /  1,  1,  0,  0,  1,  1,  1,  1,  1,  1 /
      data mchgas   /109, 89,  0,  0, 92, 67, 92,100, 75,999 /
      data mchaero  /  0, 22,  0,  0, 24, 24, 24,  0,  0,999 /
      data mchrad   / 26, 20,  0,  0, 16, 13, 23, 21, 18,  0 /
      data mchiessr /  5,  2,  0,  0,  2,  2,  2,  7,  7,  0 /
      data mchrxn   /251,213,  0,  0,217,156,218,288,222,  0 /
      data mchphot  / 39, 29,  0,  0, 30, 23, 28, 45, 38,  0 /
      data mchfast  / 35, 29,  0,  0, 31, 21, 32, 36, 26,  0 /
      data ipigsap,ipigcb05,ipigcb6 / 10, 22, 24 /
      data ihyds99,ihydcb05,ihydcb6 / 13, 19, 39 /
      data jno2s99,jno2cb5,jno2cb6          /  1,  1,  1/
      data jo3s99,jo3cb5,jo3cb6             / 18,  9,  9/
      data jhcho1s99,jhcho1cb5,jhcho1cb6    /123, 97, 75/
      data jhcho2s99,jhcho2cb5,jhcho2cb6    /124, 98, 76/
      data jch3chos99,jch3chocb5,jch3chocb6 /131,108, 87/
      data npar     /  1,  2,  4, 10,  5, 12,  8 /
      data tdum     / 298.,  298.,  310. /
      data pdum     / 1013., 506.5, 1013. /
c
c     Many rules about names, number and order of species are 
c     enforced here unless LCHEM is false.
c
c     Arrays MCHGAS, MCHAERO, MCHRXN and MCHPHOT allow for up to 10
c     gas-phase mechanisms to be called by RADDRIVR and CHEMDRIV.
c     The current mechansims are:
c      1 = CB6 + CH3I chemistry
c      2 = CB6r1
c      3 = Inactive
c      4 = Inactive
c      5 = SAPRC99 + ETOH, MTBE & MBUT (59 specs, 217 rxns, 30 phot rxns)
c      6 = CB05
c      7 = CB6
c      8 = SAPRC99 + NO2Cl & CH3I chemistry
c      9 = CB05 + NO2Cl & CH3I chemistry
c     10 = User-defined chemistry and species list
c
c     The model species will be re-ordered according to the orders defined
c     in FASTS lists.
c     Only the species that are named in SPLIST will be allowed.
c     Update CHMDAT.INC/DDMCHM.INC/IEHCHEM.INC if new species are added to SPLIST
c
c-----Entry point
c
c----CAMx v6.00 130506
c
      idmech = 0
      read(ichem,'(a)') record
      camxvin = record(21:30)
      call jstlft(camxvin)
      call toupper(camxvin)
      if (camxvin.ne.camxv) then
        write(iout,'(/,a)') ' CAMx version in CHEMPARAM file INVALID'
        write(iout,'(a,a)') ' Expecting: ',camxv
        write(iout,'(a,a)') '     Found: ',camxvin
        goto 910
      endif
c
c-----Mechanism ID
c
      read(ichem,'(a)') record
      read(record(21:80),'(I2)') idmech
      write(idiag,'(a,i4)') 'Using CHEMPARAM mechanism id ',idmech
      nrad = 0
      iessrad = 0
      if (lchem) then
        if (idmech.eq.10) then
          write(iout,'(/,2a)') ' You selected mechamism 10 ',
     &    'which requires a hard-coded subroutine called chem10.f'
        elseif( idmech .LT. 1 .OR. idmech .GT. 10 ) then
          write(iout,'(/,a)')  ' CHEMPARAM mechanism ID is INVALID'
          write(iout,'(a,i3)') ' You selected mechamism ID', idmech
          write(iout,'(a)')    ' The following mechanism IDs are valid:'
          do i = 1,10
            if (mchactv(i).eq.1) write(iout,'(40x,i3)') i
          enddo
          goto 910
        elseif (mchactv(idmech).ne.1) then
          write(iout,'(/,a)')  ' CHEMPARAM mechanism ID is INVALID'
          write(iout,'(a,i3)') ' You selected mechamism ID', idmech
          write(iout,'(a)')    ' The following mechanism IDs are valid:'
          do i = 1,10
            if (mchactv(i).eq.1) write(iout,'(40x,i3)') i
          enddo
          goto 910
        endif
        nrad = mchrad(idmech)      ! need to know NRAD for advection, etc.
        iessrad = mchiessr(idmech) ! need to know IESSRAD for IEHSOLV.
      endif
c
c-----There are four options for the aerosol treatment: 'NONE','INERT','CF','CMU'
c
      read(ichem,'(a)') record
      read(record(21:80),'(A)',ERR=111,END=111) aeropt
      call jstlft(aeropt)
 111  continue
      if (aeropt.NE.'CF'   .and. aeropt.NE.'CMU' .and.
     &    aeropt.NE.'NONE' .and. aeropt.NE.'INERT') then
        write(iout,'(/,3a)') ' Invalid option for Aerosol Scheme',
     &                       ' in CHEMPARAM input file: ',aeropt
        if( aeropt .EQ. ' ' ) then
           write(iout,'(2A)') ' You must specify the aerosol scheme.'
        endif
        write(iout,'(a,4(/,10x,a))') 'Acceptable options: ',
     &                            'NONE  - None (NAERO must = 0)',
     &                            'INERT - Inert PM',
     &                            'CF    - Static Coarse/Fine scheme',
     &                            'CMU   - Multi-section CMU scheme'
        goto 910
      endif
      if ((idmech.LT.5 .OR. idmech.GT.7) .and.
     &    (aeropt.EQ.'CF' .OR. aeropt.EQ.'CMU')) then
        write(iout,'(/,3a)') ' Invalid option for Aerosol Scheme',
     &                       ' in CHEMPARAM input file: ',aeropt
        write(iout,'(a,4(/,10x,a))') 'Acceptable options: ',
     &                          'NONE  - None (NAERO must = 0)',
     &                          'INERT - Inert PM'
      endif
      write(idiag,'(2a)') 'Using Aerosol Scheme - ',aeropt
c
      read(ichem,'(a)') record
      write(idiag,'(a)') record(:istrln(record))
c
c-----Number of species
c
      read(ichem,'(a)') record
      read(record(21:80),*) ngas
      read(ichem,'(a)') record
      read(record(21:80),*) naero
      nspec = ngas + naero
      if (nspec.lt.1) then
        write(iout,'(/,a,i5,a)')
     &    ' Number of GAS + AERO species on CHEMPARAM file =', nspec,
     &    ' is less than 1'
          goto 910
      endif
      if (naero.EQ.0 .AND. (aeropt.EQ.'CF' .OR. aeropt.EQ.'CMU'
     &                 .OR. aeropt.EQ.'INERT')) then
        write(iout,'(//A)')
     &      ' Number of aerosols = 0 for CF, CMU or INERT PM option'
        write(iout,'(a)') ' Set PM option to NONE'
        goto 910
      endif
      if (naero.GT.0) then
        if (aeropt.EQ.'NONE') then
          write(iout,'(//A)')
     &      ' Aerosol species are requested with aerosol option = NONE'
          write(iout,'(a,2(/,10x,a))') 'Acceptable aerosol options: ',
     &                            'INERT - Inert PM',
     &                            'CF    - Static Coarse/Fine scheme',
     &                            'CMU   - Multi-section CMU scheme'
          goto 910
        endif
        read(record(21:),*,ERR=903,END=903) naero,dtaero,nsec_c,
     &                      (dsec_i(i),i=1,nsec_c+1)
        nbin = nsec_c
        write(idiag,'(a,i5)') 'Number of Aerosol Species  : ',naero
        write(idiag,'(a,f5.0)') 'Aerosol Coupling Freq (min): ',dtaero
        write(idiag,'(a,i5)') 'Number of Aerosol Size bins: ',nbin
        if (aeropt.EQ.'CMU') nspec = ngas + naero*nsec_c
      endif
c
c-----Read number of reactions and check that the dimension parameter is large enough
c
      read(ichem,'(a)') record
      read(record(21:80),*) nreact
      if( nreact .GT. MXREACT ) then
         write(iout,'(//,A)') 'ERROR in READCHM:'
         write(iout,*) 'A parameter in the camx.prm is not ',
     &                                        'sufficiently large.'
         write(iout,*) 'Please change the value for parameter: ',MXREACT
         write(iout,*) 'It should be set to a value of at least: ',nreact
         goto 910
      endif
c
c-----Check number of species and number of reactions against chosen mechanism
c 
      if (lchem) then
        if (ngas.gt.mchgas(idmech)) then
          write(iout,'(/,a,i5,/,a,i5)')
     &     ' Number of GAS species in CHEMPARAM file =', ngas,
     &     ' greater than ngas for this mechanism =', mchgas(idmech)
          goto 910
        endif
c
        if (aeropt.NE.'NONE' .AND. aeropt.NE.'INERT' .AND.
     &      naero.gt.mchaero(idmech)) then
          write(iout,'(/,a,i5,/,a,i5)')
     &     ' Number of AERO species on CHEMPARAM file =', naero,
     &     ' greater than naero for this mechanism =', mchaero(idmech)
          goto 910
        endif
c
        if (nreact.ne.mchrxn(idmech)) then
          write(iout,'(/,a,i5,/,a,i5)')
     &     ' Number of reactions on CHEMPARAM file =',nreact,
     &     ' not equal to reactions for this mechanism =',mchrxn(idmech)
          goto 910
        endif
      endif
c
c-----Read primary photolysis ID record
c
      read(ichem,'(a)') recpht1
      read(recpht1(21:80),*) nphot1
c
c-----Read secondary (scaled) photolysis ID records
c
      read(ichem,'(a)') record
      read(record(21:80),*) nphot2
      if (nphot2.gt.0) then
        if (nphot1.eq.0) then
          write(iout,'(/,a)') 
     &     'Need at least one primary photolysis reaction'
          goto 910
        endif
      endif
      nphot = nphot1 + nphot2
c
c-----Call routine to allocate the arrays
c
      call alloc_chmstry(ngrid,nspec,nreact,nphot1,nphot2)
c
c-----Check photolysis ID records
c
      if( nphot .GT. 0 ) then
c
        if (nphot1.lt.1) then 
          write(iout,'(/,a)') 
     &      ' Need at least one primary photolysis reaction' 
          goto 910 
        endif 
c
        if (nphot.ne.mchphot(idmech)) then
          write(iout,'(/,a,i5,/,a,i5)')
     &     ' Chemistry mechanism requires', mchphot(idmech),
     &     ' photolysis reactions, but CHEMPARAM file has' , nphot
          goto 910
        endif
c
        if (nphot1.gt.MXPHT1) then
          write(iout,'(2a)')'Number of photolysis reactions is ',
     &                              'greater than internal dimensions.'
          write(iout,'(a,i3)') 'Number of reactions needed  : ',nphot1
          write(iout,'(a,i3)') 'Maximum dimension (MXPHT1)  : ',MXPHT1
          write(iout,'(2a)')'Increase the parameter MXPHT1 ',
     &                                   'in CAMx.PRM and recompile.'
          goto 910
        endif
c
        if (nphot2.gt.MXPHT2) then
          write(iout,'(/,a,i5)') 
          write(iout,'(2a)')'Number of photolysis reactions is ',
     &                              'greater than internal dimensions.'
          write(iout,'(a,i3)') 'Number of reactions needed  : ',nphot2
          write(iout,'(a,i3)') 'Maximum dimension (MXPHT2)  : ',MXPHT2
          write(iout,'(2a)')'Increase the parameter MXPHT2 ',
     &                                   'in CAMx.PRM and recompile.'
          goto 910
        endif
c
c-----Read primary photolysis ID record
c
        if (nphot1.gt.0) then
          read(recpht1(21:180),*) nphot1,(idphot1(n),n=1,nphot1)
          write(idiag,'(a)') 'The primary photolysis reactions are'
          write(idiag,'(i6)') (idphot1(n),n=1,nphot1)
        endif
c
c-----Read secondary (scaled) photolysis ID records
c
        if (nphot2.gt.0) then
          do n = 1,nphot2
            read(ichem,'(a)') record
            read(record(21:80),*) idphot2(n),idphot3(n),phtscl(n)
          enddo
          write(idiag,'(a)') 'The secondary photolysis reactions are'
          write(idiag,'(i6,a,i6,a,1pe10.3)')
     &      (idphot2(n),' =',idphot3(n),' *',phtscl(n),n=1,nphot2)
        endif
c
        nerr = 0
        do n = 1,nphot1
          if (idphot1(n).gt.nreact) nerr = 1
        enddo
c
        do n = 1,nphot2
          if (idphot2(n).gt.nreact) nerr = 1
          if (idphot3(n).gt.nreact) nerr = 1
          nhit = 0
          do nn = 1,nphot1
            if (idphot3(n).eq.idphot1(nn)) nhit = 1
          enddo
          if (nhit.eq.0) nerr = 1
        enddo
c
        if (nerr.eq.1) then
           write(iout,'(/,a)') 'ERROR in the CHEMPARAM file:'
           write(iout,'(a)') 
     &     ' Bad reaction number in one of the photolysis reaction IDs.'
          goto 910
        endif
      endif
c
c-----Species records, gases come first
c
      write(idiag,'(a)') 'The state species are'
      read(ichem,'(a)') record
      if (ngas.gt.0) then
        read(ichem,'(a)') record
        write(idiag,'(a)') record(:istrln(record))
        do l=1,ngas
          read(ichem,'(5x,a10,2e10.0,4f10.0)')
     &               spname(l),bdnl(l),henry0(l),tfact(l),
     &               molwt,f0(l),rscale(l)
          diffrat(l) = sqrt(molwt/18.)
          rscale(l) = amin1(1.,rscale(l))
          rscale(l) = amax1(0.,rscale(l))
          write(idiag,'(i3,2x,a10,2e10.2,4f10.2)')
     &               l,spname(l),bdnl(l),henry0(l),tfact(l),
     &               molwt,f0(l),rscale(l)
        enddo
      endif
c
c-----Check over the gas phase species for deposition calculations
c
      if ((ldry .or. lwet) .and. ngas.gt.0) then
        do l = 1,ngas
          if (henry0(l).eq.0.) then
            write(iout,'(/,a,i5)')
     &      'The Henry0 value must be non-zero for species ',l
            goto 910
          endif
        enddo
      endif
c
c-----Reorder fast species (NO, NO2, O3, and PAN etc) to come first
c
      if (lchem .and. ngas.gt.0 .and. idmech.NE.10) then
        nspfst=mchfast(idmech)
        if (nspfst.gt.0) then
          do i=1,nspfst
            ierr = 1
            do j=1,ngas
              if (idmech.EQ.1) then ! CB6 w/ CH3I 
                if (fasts1(i).NE.spname(j)) CYCLE
              elseif (idmech.EQ.2) then ! CB6r1
                if (fasts2(i).NE.spname(j)) CYCLE
              elseif (idmech.EQ.5) then ! SAPRC99
                if (fasts5(i).NE.spname(j)) CYCLE
              elseif (idmech.EQ.6) then ! CB05
                if (fasts6(i).NE.spname(j)) CYCLE
              elseif (idmech.EQ.7) then ! CB6
                if (fasts7(i).NE.spname(j)) CYCLE
              elseif (idmech.EQ.8) then ! SAPRC99 w/ NO2Cl & CH3I
                if (fasts8(i).NE.spname(j)) CYCLE
              elseif (idmech.EQ.9) then ! CB05 w/ NO2Cl & CH3I
                if (fasts9(i).NE.spname(j)) CYCLE
              else
                write(iout,'(/,a)') 
     &          'Dont know how to order fast species for mech #',idmech
                goto 910
              endif
              nametmp = spname(i)
              spname(i) = spname(j)
              spname(j) = nametmp
              tmp = bdnl(i)
              bdnl(i) = bdnl(j)
              bdnl(j) = tmp
              tmp = henry0(i)
              henry0(i) = henry0(j)
              henry0(j) = tmp
              tmp = tfact(i)
              tfact(i) = tfact(j)
              tfact(j) = tmp
              tmp = diffrat(i)
              diffrat(i) = diffrat(j)
              diffrat(j) = tmp
              tmp = f0(i)
              f0(i) = f0(j)
              f0(j) = tmp
              tmp = rscale(i)
              rscale(i) = rscale(j)
              rscale(j) = tmp
              ierr = 0
              EXIT
            enddo
            if (ierr.eq.1) then
              write(iout,'(/,2a)') 'You must include all of the ',
     &                'following fast species in CHEMPARAM file:'
              if (idmech.EQ.1) then
                write(iout,'(6a10)') (fasts1(l),l=1,mchfast(idmech))
              elseif (idmech.EQ.2) then
                write(iout,'(6a10)') (fasts2(l),l=1,mchfast(idmech))
              elseif (idmech.EQ.5) then
                write(iout,'(6a10)') (fasts5(l),l=1,mchfast(idmech))
              elseif (idmech.EQ.6) then
                write(iout,'(6a10)') (fasts6(l),l=1,mchfast(idmech))
              elseif (idmech.EQ.7) then
                write(iout,'(6a10)') (fasts7(l),l=1,mchfast(idmech))
              elseif (idmech.EQ.8) then
                write(iout,'(6a10)') (fasts8(l),l=1,mchfast(idmech))
              elseif (idmech.EQ.9) then
                write(iout,'(6a10)') (fasts9(l),l=1,mchfast(idmech))
              endif
              goto 910
            endif
          enddo
        endif
      endif
c
c-----Read the aero species, if any
c
      if (naero.ne.0) then
        read(ichem,'(a)') record
        write(idiag,'(a)') record(:istrln(record))
        if (aeropt.EQ.'CMU') then
          do iaero = 1,naero
            read(ichem,'(5x,a10,e10.0,2f10.0,i10,f10.0)')
     &           tmpnam,bdnl_tmp,roprt_tmp,bext_tmp,rhadj_tmp,ssa_tmp
            nl = istrln(tmpnam)
            do isec = 1,nsec_c
              l = ngas + (iaero-1)*nsec_c + isec
              if ( isec .ge. 10 ) then
                write(tmpnam1,'(a1,i2)') '_',isec
                spname(l)=tmpnam(1:nl)//tmpnam1(1:3)
              else
                write(tmpnam1,'(a1,i1)') '_',isec
                spname(l)=tmpnam(1:nl)//tmpnam1(1:2)
              endif
              bdnl(l) = bdnl_tmp
              roprt(l) = roprt_tmp
              bext(l) = bext_tmp
              rhadj(l) = rhadj_tmp
              ssa(l) = ssa_tmp
              dcut(l,1) = dsec_i(isec)
              dcut(l,2) = dsec_i(isec+1)
              write(idiag,'(i3,2x,a10,e10.2,2f10.2,i10,3f10.2)')
     &           l,spname(l),bdnl(l),roprt(l),bext(l),rhadj(l),
     &           ssa(l),(dcut(l,m),m=1,2)
              bext(l) = bext(l)*1.e-6
              roprt(l) = roprt(l)*1.e6
            enddo
          enddo
        else
          do l = ngas+1,nspec
            read(ichem,'(5x,a10,e10.0,2f10.0,i10,f10.0,i10)')
     &         spname(l),bdnl(l),roprt(l),bext(l),rhadj(l),ssa(l),ibin
            if (ibin .lt. 1 .OR. ibin.gt.nbin) then
              write(iout,'(/,3a)') 'PM size bin for species: ',
     &                             spname(l)(:istrln(spname(l))),
     &                         ' is outside range from CHEMPARAM header.'
              write(iout,'(a,i5)') 'PM size bin read:       ',ibin
              write(iout,'(a,i5)') 'Number of PM size bins: ',nbin
              goto 910
            endif
            dcut(l,1) = dsec_i(ibin)
            dcut(l,2) = dsec_i(ibin+1)
            write(idiag,'(i3,2x,a10,e10.2,2f10.2,i10,3f10.2)')
     &         l,spname(l),bdnl(l),roprt(l),bext(l),rhadj(l),
     &         ssa(l),(dcut(l,m),m=1,2)
            bext(l) = bext(l)*1.e-6
            roprt(l) = roprt(l)*1.e6
          enddo
        endif
      endif
c
c-----Map species names to the internal name list.  The default setting
c     to nspec+1 allows species that are in the chem solvers to be
c     omitted from the species list for this run.  Testing if a named
c     pointer is set to nspec+1 is used to identify species that are not
c     in the run.  Named pointers (e.g., kno) are set by equivalence
c     in CHMSTRY.COM
c
      if (lchem .and. idmech.NE.10) then
        do j = 1,NSPNAM
          kmap(j) = nspec+1
        enddo
c
        nn = nspec
        if (aeropt.eq.'INERT') nn = ngas
        do 10 j = 1,nn
          do i = 1,NSPNAM
            if (splist(i).eq.spname(j)) then
              kmap(i) = j
              goto 10
            endif
          enddo
          write(iout,'(//,a)') 'ERROR: Reading CHEMPARAM file.'
          write(iout,'(3a)') 'Species in chemparam file ',
     &     spname(j)(:istrln(spname(j))),' is not in the internal list.'
          write(iout,'(2a)') 'Please make sure you are using the ',
     &               'correct chemparam file for this version of CAMx.'
          goto 910
  10    continue
        spname(nspec+1) = blank
        bdnl(nspec+1) = 0.
        write(idiag,'(a)')'Internal species order is:'
        write(idiag,'(i4,1x,a10)') (i,spname(i),i=1,nspec)
c
c-----Check species list for consistency with CF and CMU options
c
        if (naero.GT.0 .and. aeropt.NE.'INERT') then
c
c-----Check mandatory gas species for PM chemistry
c
          if (ksulf.eq.nspec+1 .or. khno3.eq.nspec+1 .or.
     &         knh3.eq.nspec+1 .or.  kso2.eq.nspec+1 .or.
     &       (kh2o2.eq.nspec+1 .and. kho2h.eq.nspec+1) .or.
     &          ko3.eq.nspec+1 .or. kn2o5.eq.nspec+1 .or.
     &        ktola.eq.nspec+1 .or. kxyla.eq.nspec+1 .or.
     &         kisp.eq.nspec+1 .or.  ktrp.eq.nspec+1 .or.
     &         ksqt.eq.nspec+1 .or.  kcg1.eq.nspec+1 .or.
     &         kcg2.eq.nspec+1 .or.  kcg3.eq.nspec+1 .or.
     &         kcg4.eq.nspec+1 .or.  kcg5.eq.nspec+1 .or.
     &         kcg6.eq.nspec+1 .or.  kcg7.eq.nspec+1) then
            write(iout,'(/,2a)') ' You must have all of the',
     &               ' following gas species to use the CF/CMU options:'
            write(iout,'(a)')' SULF, HNO3, NH3, SO2, H2O2 (or HO2H),'
            write(iout,'(a)')' O3, N2O5, TOLA, XYLA, ISP, TRP, SQT,'
            write(iout,'(a)')' CG1, CG2, CG3, CG4, CG5, CG6, CG7.'
            goto 910
          endif

          if (aeropt.EQ.'CMU') then
c
c-----Check mandatory PM species for CMU mechanism
c
            if (kpso4_1.eq.nspec+1 .or. kpno3_1.eq.nspec+1 .or.
     &          kpnh4_1.eq.nspec+1 .or. ksoa1_1.eq.nspec+1 .or. 
     &          ksoa2_1.eq.nspec+1 .or. ksoa3_1.eq.nspec+1 .or.
     &          ksoa4_1.eq.nspec+1 .or. ksoa5_1.eq.nspec+1 .or.
     &          ksoa6_1.eq.nspec+1 .or. ksoa7_1.eq.nspec+1 .or.
     &          ksopa_1.eq.nspec+1 .or. ksopb_1.eq.nspec+1 .or.
     &           kpoa_1.eq.nspec+1 .or.  kpec_1.eq.nspec+1 .or.
     &          kcrst_1.eq.nspec+1 .or. kph2o_1.eq.nspec+1) then
              write(iout,'(/,2a)') ' You must have all of the',
     &                   ' following PM species to use the CMU option:'
              write(iout,'(a)')' PSO4, PNO3, PNH4, SOA1, SOA2, SOA3,'
              write(iout,'(a)')' SOA4, SOA5, SOA6, SOA7, SOPA, SOPB,'
              write(iout,'(a)')' POA, PEC, CRST, PH2O.'
              goto 910
            endif
c
c-----Check for sea salt species for CMU mechanism
c
            if ( khcl.eq.nspec+1 .or. kpcl_1.eq.nspec+1 .or.
     &          kna_1.eq.nspec+1) then
              write(iout,'(/,2a)') ' You must have all of the',
     &                   ' sea salt species to use the CMU option:'
              write(iout,'(a)')' HCL, PCL, NA.'
              goto 910
            endif

          elseif (aeropt.EQ.'CF') then
c
c-----Check mandatory PM species for CF mechanism
c
            if (kpso4.eq.nspec+1 .or. kpno3.eq.nspec+1 .or.
     &          kpnh4.eq.nspec+1 .or. ksoa1.eq.nspec+1 .or.
     &          ksoa2.eq.nspec+1 .or. ksoa3.eq.nspec+1 .or.
     &          ksoa4.eq.nspec+1 .or. ksoa5.eq.nspec+1 .or.
     &          ksoa6.eq.nspec+1 .or. ksoa7.eq.nspec+1 .or.
     &          ksopa.eq.nspec+1 .or. ksopb.eq.nspec+1 .or.
     &          kph2o.eq.nspec+1) then
              write(iout,'(/,2a)') ' You must have all of the',
     &                   ' following PM species to use the CF option:'
              write(iout,'(a)')' PSO4, PNO3, PNH4, SOA1, SOA2, SOA3,'
              write(iout,'(a)')' SOA4, SOA5, SOA6, SOA7, SOPA, SOPB,'
              write(iout,'(a)')' PH2O.'
              goto 910
            endif
c
c-----Check for a consistent set of sea salt species, or none
c
            if (kna.eq.nspec+1 .and. kpcl.eq.nspec+1 .and.
     &                               khcl.eq.nspec+1) then
              continue
            elseif (kna.lt.nspec+1 .and. kpcl.lt.nspec+1 .and.
     &                                   khcl.lt.nspec+1) then
              continue
            elseif (kna.eq.nspec+1 .and. kpcl.eq.nspec+1 .and.
     &                                   khcl.lt.nspec+1) then
              continue
            else
              write(iout,'(/,2a)') ' You must have all or none of the',
     &                   ' sea salt species to use the CF option:'
              write(iout,'(a)')' NA, PCL, HCL.'
              write(iout,'(a)')' Or, just HCL.'
              goto 910
            endif
          endif
        else
c
c-----Check for gas species for non-aerosol chemistry mechanisms
c
          if (ktola.lt.nspec+1 .or. kxyla.lt.nspec+1 .or.
     &         kisp.lt.nspec+1 .or.  ktrp.lt.nspec+1 .or.
     &         ksqt.lt.nspec+1 .or.  kcg1.lt.nspec+1 .or.
     &         kcg2.lt.nspec+1 .or.  kcg3.lt.nspec+1 .or.
     &         kcg4.lt.nspec+1 .or.  kcg5.lt.nspec+1 .or.
     &         kcg6.lt.nspec+1 .or.  kcg7.lt.nspec+1) then
            write(iout,'(/,a)') ' You must have none of the species'
            write(iout,'(a)')   ' TOLA,XYLA,ISP,TRP,SQT,CG1,CG2,CG3,'
            write(iout,'(a)')   ' CG4,CG5,CG6,CG7 with non-aerosol'
            write(iout,'(a)')   ' chemistry mechanisms.'
            goto 910
          endif
        endif
c
c-----Check for a consistent set of Mercury species, or none
c
        if (khg0.eq.nspec+1 .and. khg2.eq.nspec+1 .and.
     &      khgiip.eq.nspec+1 .and. khgiipc.eq.nspec+1) then
          continue
        elseif (khg0.lt.nspec+1 .and. khg2.lt.nspec+1 .and.
     &      khgiip.lt.nspec+1 .and. khgiipc.lt.nspec+1) then
          continue
        else
          write(iout,'(/,a)') ' You must have all or none of the '
          write(iout,'(a)')   ' Mercury gas-phase and adsorbed species '
          write(iout,'(a)')   ' HG0, HG2, HGIIP, HGIIPC'
          goto 910
        endif
        if (khg0.lt.nspec+1 .and. naero.lt.2 ) then
          write(iout,'(/,2a)') ' You must model PM10 to use the',
     &                         ' Mercury gas-phase chemistry.'
          write(iout,'(2a)')   ' HG0 and HG2 are included but',
     &          ' the number of aerosol species is less than 2'
          goto 910
        endif
        if (khg0.lt.nspec+1 .and. aeropt.eq.'CMU') then
          write(iout,'(/,2a)') ' Mercury is not currently available',
     &                         ' with the CMU option; use CF instead.'
          goto 910
        endif
        if (khg0.lt.nspec+1 .and. khcl.eq.nspec+1) then
          write(iout,'(/,2a)') ' Mercury chemistry requires HCl'
          goto 910
        endif
      endif
c
c-----Set up section diameters and check parameters for CF/CMU routines
c
      if (lchem .AND. (aeropt.EQ.'CMU' .OR. aeropt.EQ.'CF')) then
        ierr = 0
        call aeroset(dsec_i,ierr)
        if ( ierr .ne. 0 ) goto 910
      endif
c
c-----Reaction records
c
      if (lchem .AND. nreact.gt.0) then
        ncms = 0
        nppm = 0
        read(ichem,'(a)') record
        read(ichem,'(a)') record
        write(idiag,'(a)') 'The reaction rate parameters are'
        do i = 1,nreact
          read(ichem,'(a)') record
          read(record,*) num,rxntyp(i)
          if (rxntyp(i).lt.1 .or. rxntyp(i).gt.7) goto 902
          read(record,*,err=900) num,rxntyp(i),rxnord(i),
     &                           (rxnpar(i,j),j=1,npar(rxntyp(i)))
c
          if (rxntyp(i).eq.5) then
            iref = nint(rxnpar(i,1))
            if (i.le.iref) goto 901
          endif
          if (rxntyp(i).eq.2 .or. rxntyp(i).eq.3) then
            if (rxnpar(i,1).gt.1.0) then
              nppm = nppm +1
            else
              ncms = ncms +1
            endif
          endif
c
          write(idiag,'(3i3,1p12d12.4)')
     &       num,rxntyp(i),rxnord(i),(rxnpar(i,j),j=1,npar(rxntyp(i)))
        enddo
c
c-----Decide what units the rate constants are provided in
c
        if (nppm.eq.0 .and. ncms.eq.0) then
          write(iout,'(/,a)') ' Unable to determine rate constant units'
          write(iout,*)       ' ncms, nppm = ', ncms, nppm
          goto 910
        elseif (nppm.gt.ncms) then
          write(idiag,'(/,a)') 'Rate constants input in ppm units'
          write(idiag,*)       'ncms, nppm = ', ncms, nppm
          iunits = 1
        else
          write(idiag,'(/,a)') 'Rate constants input in cms units'
          write(idiag,*)       'ncms, nppm = ', ncms, nppm
          iunits = 2
        endif
c
c-----Populate rate constant lookup table
c
        call exptbl(iunits,rxntyp,rxnord,rxnpar)
c
c-----Provide diagnostic info for checking rate expressions
c
c       write(idiag,'(/,a,/,/,a)') 
c    &        'Diagnostic info for checking rate expressions',
c    &        'Rates at three temps and pressures in ppm-n min-1'
c       do i=1,3
c         call ktherm(tdum(i), pdum(i))
c         do j=1,nreact
c           kdum(j,i)=rk(j)/60.
c         enddo
c       enddo
c       write(idiag,'(a,3F12.1)') 'Temp= ', tdum
c       write(idiag,'(a,3F12.1)') 'Pres= ', pdum
c       write(idiag,'(a)') 'Rxn Type'
c       write(idiag,'(2i3,1p3e12.4)')
c    &       (j,rxntyp(j),kdum(j,1),
c    &        kdum(j,2),kdum(j,3),j=1,nreact)
c
c-----Set pointers for photolysis reactions to receive T,P adjustments
c
        if (idmech.eq.5 .OR. idmech.eq.8) then
          jno2rxn    = jno2s99
          jo3rxn     = jo3s99
          jhcho1rxn  = jhcho1s99
          jhcho2rxn  = jhcho2s99
          jch3chorxn = jch3chos99
        elseif (idmech.eq.6 .OR. idmech.eq.9) then
          jno2rxn    = jno2cb5
          jo3rxn     = jo3cb5
          jhcho1rxn  = jhcho1cb5
          jhcho2rxn  = jhcho2cb5
          jch3chorxn = jch3chocb5
        elseif (idmech.eq.1 .OR. idmech.eq.2 .OR. idmech.eq.7) then
          jno2rxn    = jno2cb6
          jo3rxn     = jo3cb6
          jhcho1rxn  = jhcho1cb6
          jhcho2rxn  = jhcho2cb6
          jch3chorxn = jch3chocb6
        else
          write(iout,'(/,a,i3)')
     &       'Must set T & P photolysis rxn indices for mech #', idmech
          goto 910
        endif
c
c-----Set pointer to hydrolysis of N2O5
c
        if(idmech.eq.5 .OR. idmech.eq.8)then
          ihydrxn = ihyds99
        elseif(idmech.eq.6 .OR. idmech.eq.9)then
          ihydrxn = ihydcb05
        elseif(idmech.eq.1 .OR. idmech.eq.2 .OR. idmech.eq.7)then
          ihydrxn = ihydcb6
        else
          write(iout,'(/,a,i3)')
     &       'Must set N2O5 hydrolysis rxn index for mech #', idmech
          goto 910
        endif
c
c-----Set reaction pointers for pig chemistry
c
        if (ipigflg .NE. 0) then
           if (naero.GT.0) then
             if (aeropt.EQ.'CMU') then
               write(iout,'(/,a)')'PiG cannot be run with CMU PM Option'
               goto 910
             endif
             if (aeropt.EQ.'CF' .AND. ipigflg .EQ. IRONPIG) then
               write(iout,'(/,a)')'IRON PiG cannot be run with PM CF'
               write(iout,'(/,2a)')'Either turn off PiG or change',
     &                            ' to GREASD'
               goto 910
             endif
           endif
           if(idmech.eq.5 .OR. idmech.eq.8)then
             ipigrxn = ipigsap
           elseif(idmech.eq.6 .OR. idmech.eq.9)then
             ipigrxn = ipigcb05
           elseif(idmech.eq.1 .OR. idmech.eq.2 .OR. idmech.eq.7)then
             ipigrxn = ipigcb6
           elseif (idmech.eq.10) then
             write(iout,'(/,a)')'PiG cannot be run with Mechanism 10'
             goto 910
           else
             write(iout,'(/,a,i3)') 
     &       'Dont know how to set pig rxns for mech #', idmech
             goto 910
           endif
        endif
      endif
c
c-----Set IEH solver species pointers via equivalences in iehchem.inc
c     IMAP order: fast species except for SSRAD; SSRAD; slow species; then
c                 IMAP(non-model spc) = NGAS + 1
c
      if (lchem .and. nspfst.gt.0) then
        do i = 1, nspnam
          if (kmap(i).gt.ngas) then
            imap(i) = ngas + 1
          elseif (kmap(i).gt.nspfst) then
            imap(i) = kmap(i)
          elseif (kmap(i).gt.iessrad) then
            imap(i) = kmap(i) - iessrad
          else
            imap(i) = kmap(i) + nspfst - iessrad
          endif
        enddo
      endif
c
c======================== DDM Begin =======================
c
c---  Set species pointers via equivalences in ddmchm.inc
c     LMAP(non-model spc) = NGAS + 1 while KMAP(non-model spc) = NSPEC + 1,
c     which allows shorter local arrays in the chemistry solver routines.
c
      if (lchem .and. nspfst.gt.0) then
        do i = 1, nspnam
          if (kmap(i).gt.ngas) then
            lmap(i) = ngas + 1
          else
            lmap(i) = kmap(i)
          endif
        enddo
      endif
c
c======================== DDM End   =======================
c
c  --- set the flag for determining if a species is gaseous ---
c
      do i=1,nrad
        lgas(i) = .FALSE.
      enddo
      do i=nrad+1,ngas
        lgas(i) = .TRUE.
      enddo
      do i=ngas+1,nspec
        lgas(i) = .FALSE.
      enddo
c
      write(idiag,*)
      call flush(idiag)
c
      return
c
 900  write(iout,'(//,a)') 'ERROR: Reading CHEMPARAM file record:'
      write(iout,'(a)') record
      write(iout,'(4(a,i4))')
     &  'reaction number', num, ' of type', rxntyp(i),
     &  ' should have', npar(rxntyp(i)), ' parameters'
      goto 910
c
 901  write(iout,'(//,a)') 'ERROR: Reading CHEMPARAM file record:'
      write(iout,'(a)') record
      write(iout,'(a)')  'For reaction type 5'
      write(iout,'(a,i4)') 'reference reaction # ', iref
      write(iout,'(a,i4)') 'must come before this reaction', i
      goto 910
c
 902  write(iout,'(//,a)') 'ERROR: Reading CHEMPARAM file record:'
      write(iout,'(a)') record
      write(iout,'(a)') 'Reaction type out of bounds (1-5)'
      write(iout,'(a,i4)') 'for reaction ', num
      write(iout,'(a,i4)') 'value was', rxntyp(i)
      write(iout,'(a)') 'Check that this is a CAMx5.4 chemparam file'
      goto 910
c
 903  write(iout,'(//,a)') 'ERROR: Reading CHEMPARAM file record:'
      write(iout,'(/,a,/)') record
      write(iout,'(2a)') 'If you include aerosol species you must ',
     &                                 'specify the size sections.'
      write(iout,'(a)') 'Check that this is a CAMx5.4 chemparam file'
      goto 910
c
 910  write(*,'(/,a)') 'ERROR in READCHM - see message in .out file'
      write(idiag,'(/,a)') 'ERROR in READCHM - see message in .out file'
      write(iout,'(//,a)') 'ERROR in READCHM.'
      call camxerr()
c
      end

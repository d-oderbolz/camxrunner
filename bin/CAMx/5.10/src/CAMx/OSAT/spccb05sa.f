c**** SPCCB05SA
c
      subroutine spccb05sa(namecls,numcls,coefcon,coeflux,
     &                                       nameyld,numyld,yield,molwt)
      use chmstry
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine initializes the species variables that will be used
c   to determine the species that will be included in each tracer
c   species class
c    Argument descriptions:
c     Input:  
c     Output:  
c       namecls  C  array of regular model species in each tracer class
c       numcls   I  the number of species contributing to this class
c       coefcon  R  2-D array of coefficients for making linear combo
c                   this one for concentrations and emissions
c       coeflux  R  2-D array of coefficients for making linear combo
c                   this one for fluxes
c       nameyld  C  array of regular model species for the yields of
c                   each tracer class
c       numyld   I  number of regular model species for the yields of
c                   each tracer class
c       yield    R  yield rates for each species in each class
c       molwt    R  molecular weight of model species
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     09/20/03   --gwilson--    Original development
c     01/08/07   --bkoo--       Revised for Mechanism 6 (CB05)
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'tracer.com'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      character*10 namecls(MXALCLS,MXSPEC)
      integer      numcls(MXALCLS)
      real         coefcon(MXALCLS,MXSPEC)
      real         coeflux(MXALCLS,MXSPEC)
      character*10 nameyld(MXALCLS,MXSPEC)
      integer      numyld(MXALCLS)
      real         yield(MXALCLS,MXSPEC)
      real         molwt(MXSPEC)
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c   NUMCB05   I   number of CB05 species used by PSAT treatment
c
      integer NUMCB05
c
      parameter( NUMCB05 = 58 )
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*10 cb05nam(NUMCB05)
      integer*4    i, j
      real         wtcb05(NUMCB05)
c
c-----------------------------------------------------------------------
c    Data statements:
c-----------------------------------------------------------------------
c
      data cb05nam /
     &         'NO        ', 'NO2       ', 'PAR       ', 'ETHA      ',
     &         'MEOH      ', 'ETOH      ', 'ETH       ', 'OLE       ',
     &         'IOLE      ', 'ISOP      ', 'TERP      ', 'FORM      ',
     &         'ALD2      ', 'ALDX      ', 'TOL       ', 'XYL       ',
     &         'SO2       ', 'HONO      ', 'NXOY      ', 'PAN       ',
     &         'PANX      ', 'PNA       ', 'NTR       ', 'HNO3      ',
     &         'NH3       ', 'TOLA      ', 'XYLA      ', 'ISP       ',
     &         'TRP       ', 'SQT       ', 'CG1       ', 'CG2       ',
     &         'CG3       ', 'CG4       ', 'CG5       ', 'CG6       ',
     &         'CG7       ', 'HG0       ', 'HG2       ', 'PSO4      ',
     &         'PNO3      ', 'PNH4      ', 'PEC       ', 'POA       ',
     &         'FCRS      ', 'FPRM      ', 'CCRS      ', 'CPRM      ',
     &         'SOA1      ', 'SOA2      ', 'SOA3      ', 'SOA4      ',
     &         'SOA5      ', 'SOA6      ', 'SOA7      ', 'SOPA      ',
     &         'SOPB      ', 'HGP       '/

      data wtcb05 /
     &          46.0       ,  46.0       ,  16.0       ,  32.0,
     &          16.0       ,  32.0       ,  32.0       ,  32.0,
     &          64.0       ,  80.0       , 160.0       ,  16.0,
     &          32.0       ,  32.0       , 112.0       , 128.0,
     &          64.0       ,  46.0       ,  46.0       ,  46.0,
     &          46.0       ,  46.0       ,  46.0       ,  63.0,
     &          17.0       ,  92.0       , 106.0       ,  68.0,
     &         136.0       , 204.0       , 150.0       , 150.0,
     &         130.0       , 130.0       , 180.0       , 180.0,
     &         210.0       , 200.6       , 200.6       ,   1.0,
     &           1.0       ,   1.0       ,   1.0       ,   1.0,
     &           1.0       ,   1.0       ,   1.0       ,   1.0,
     &           1.0       ,   1.0       ,   1.0       ,   1.0,
     &           1.0       ,   1.0       ,   1.0       ,   1.0,
     &           1.0       ,   1.0/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  ---- initialize all arrays ---
c
      do i=1,MXALCLS
        numcls(i) = 0
        numyld(i) = 0
        do j=1,MXSPEC
          namecls(i,j) = ' '
          nameyld(i,j) = ' '
          coefcon(i,j) = 0.0
          coeflux(i,j) = 0.0
          yield(i,j) = 0.0
        enddo
      enddo
c
c  --- set the molecular weight for each modeled species ---
c
      do i=1,nspec
        molwt(i) = 1.0
        do j=1,NUMCB05
           if( spname(i) .EQ. cb05nam(j) ) molwt(i) = wtcb05(j)
        enddo
      enddo
c
c  ---- NOx species ---
c
      if( lozone ) then
         numcls(ITRNOX) = 2
         namecls(ITRNOX,1) = 'NO'
         coefcon(ITRNOX,1) = 1.0
         coeflux(ITRNOX,1) = 1.0
         namecls(ITRNOX,2) = 'NO2'
         coefcon(ITRNOX,2) = 1.0
         coeflux(ITRNOX,2) = 1.0
c
c  ---- VOC species ---
c
         numcls(ITRVOC) = 14
         namecls(ITRVOC,1) = 'PAR'
         coefcon(ITRVOC,1) = 1.0
         coeflux(ITRVOC,1) = 1.0
         namecls(ITRVOC,2) = 'ETHA'
         coefcon(ITRVOC,2) = 2.0
         coeflux(ITRVOC,2) = 2.0
         namecls(ITRVOC,3) = 'MEOH'
         coefcon(ITRVOC,3) = 1.0
         coeflux(ITRVOC,3) = 1.0
         namecls(ITRVOC,4) = 'ETOH'
         coefcon(ITRVOC,4) = 2.0
         coeflux(ITRVOC,4) = 2.0
         namecls(ITRVOC,5) = 'ETH'
         coefcon(ITRVOC,5) = 2.0
         coeflux(ITRVOC,5) = 2.0
         namecls(ITRVOC,6) = 'OLE'
         coefcon(ITRVOC,6) = 2.0
         coeflux(ITRVOC,6) = 2.0
         namecls(ITRVOC,7) = 'IOLE'
         coefcon(ITRVOC,7) = 4.0
         coeflux(ITRVOC,7) = 4.0
         namecls(ITRVOC,8) = 'ISOP'
         coefcon(ITRVOC,8) = 5.0
         coeflux(ITRVOC,8) = 5.0
         namecls(ITRVOC,9) = 'TERP'
         coefcon(ITRVOC,9) = 10.0
         coeflux(ITRVOC,9) = 10.0
         namecls(ITRVOC,10) = 'FORM'
         coefcon(ITRVOC,10) = 1.0
         coeflux(ITRVOC,10) = 1.0
         namecls(ITRVOC,11) = 'ALD2'
         coefcon(ITRVOC,11) = 2.0
         coeflux(ITRVOC,11) = 2.0
         namecls(ITRVOC,12) = 'ALDX'
         coefcon(ITRVOC,12) = 2.0
         coeflux(ITRVOC,12) = 2.0
         namecls(ITRVOC,13) = 'TOL'
         coefcon(ITRVOC,13) = 7.0
         coeflux(ITRVOC,13) = 7.0
         namecls(ITRVOC,14) = 'XYL'
         coefcon(ITRVOC,14) = 8.0
         coeflux(ITRVOC,14) = 8.0
c
c  ---- O3-NOx species ---
c
         numcls(ITRO3N) = 1
         namecls(ITRO3N,1) = 'O3'
         coefcon(ITRO3N,1) = 0.5
         coeflux(ITRO3N,1) = 1.0
c
c  ---- O3-VOC species ---
c
         numcls(ITRO3V) = 1
         namecls(ITRO3V,1) = 'O3'
         coefcon(ITRO3V,1) = 0.5
         coeflux(ITRO3V,1) = 1.0
      endif
c
c   --- if doing the SULFATE species ---
c
      if( lsulfate ) then
c
c  ---- SO2 species ---
c
        numcls(ITRSO2) = 1
        namecls(ITRSO2,1) = 'SO2'
        coefcon(ITRSO2,1) = 1.0
        coeflux(ITRSO2,1) = 1.0
c
c  ---- PS4 species ---
c
        numcls(ITRPS4) = 1
        namecls(ITRPS4,1) = 'PSO4'
        coefcon(ITRPS4,1) = 1.0
        coeflux(ITRPS4,1) = 1.0
      endif
c
c   --- if doing the NITRATE species ---
c
      if( lnitrate ) then
c
c  ---- RGN species ---
c
          numcls(ITRRGN) = 4
          namecls(ITRRGN,1) = 'NO'
          coefcon(ITRRGN,1) = 1.0
          coeflux(ITRRGN,1) = 1.0
          namecls(ITRRGN,2) = 'NO2'
          coefcon(ITRRGN,2) = 1.0
          coeflux(ITRRGN,2) = 1.0
          namecls(ITRRGN,3) = 'HONO'
          coeflux(ITRRGN,3) = 1.0
          coefcon(ITRRGN,3) = 1.0
          namecls(ITRRGN,4) = 'NXOY'
          coefcon(ITRRGN,4) = 1.0
          coeflux(ITRRGN,4) = 1.0
c
c  ---- TPN species ---
c
          numcls(ITRTPN) = 3
          namecls(ITRTPN,1) = 'PAN'
          coefcon(ITRTPN,1) = 1.0
          coeflux(ITRTPN,1) = 1.0
          namecls(ITRTPN,2) = 'PANX'
          coefcon(ITRTPN,2) = 1.0
          coeflux(ITRTPN,2) = 1.0
          namecls(ITRTPN,3) = 'PNA'
          coefcon(ITRTPN,3) = 1.0
          coeflux(ITRTPN,3) = 1.0
c
c  ---- NTR species ---
c
          numcls(ITRNTR) = 1
          namecls(ITRNTR,1) = 'NTR'
          coefcon(ITRNTR,1) = 1.0
          coeflux(ITRNTR,1) = 1.0
c
c  ---- PN3 species ---
c
          numcls(ITRPN3) = 1
          namecls(ITRPN3,1) = 'PNO3'
          coefcon(ITRPN3,1) = 1.0
          coeflux(ITRPN3,1) = 1.0
c
c  ---- HN3 species ---
c
          numcls(ITRHN3) = 1
          namecls(ITRHN3,1) = 'HNO3'
          coefcon(ITRHN3,1) = 1.0
          coeflux(ITRHN3,1) = 1.0
c
c  ---- NH3 species ---
c
          numcls(ITRNH3) = 1
          namecls(ITRNH3,1) = 'NH3'
          coefcon(ITRNH3,1) = 1.0
          coeflux(ITRNH3,1) = 1.0
c
c  ---- PN4 species ---
c
          numcls(ITRPN4) = 1
          namecls(ITRPN4,1) = 'PNH4'
          coefcon(ITRPN4,1) = 1.0
          coeflux(ITRPN4,1) = 1.0
      endif
c
c   --- if doing the SOA species ---
c
      if( lsoa ) then
c
c  ---- ARO species ---
c
          numcls(ITRARO) = 2
          namecls(ITRARO,1) = 'TOLA'
          coefcon(ITRARO,1) = 1.0
          coeflux(ITRARO,1) = 1.0
          namecls(ITRARO,2) = 'XYLA'
          coefcon(ITRARO,2) = 1.0
          coeflux(ITRARO,2) = 1.0
c
c  ---- ISP species ---
c
          numcls(ITRISP) = 1
          namecls(ITRISP,1) = 'ISP'
          coefcon(ITRISP,1) = 1.0
          coeflux(ITRISP,1) = 1.0
c
c  ---- TRP species ---
c
          numcls(ITRTRP) = 1
          namecls(ITRTRP,1) = 'TRP'
          coefcon(ITRTRP,1) = 1.0
          coeflux(ITRTRP,1) = 1.0
c
c  ---- SQT species ---
c
          numcls(ITRSQT) = 1
          namecls(ITRSQT,1) = 'SQT'
          coefcon(ITRSQT,1) = 1.0
          coeflux(ITRSQT,1) = 1.0
c
c  ---- CG1 species ---
c
          numcls(ITRCG1) = 1
          namecls(ITRCG1,1) = 'CG1'
          coefcon(ITRCG1,1) = 1.0
          coeflux(ITRCG1,1) = 1.0
          numyld(ITRCG1) = 2
          nameyld(ITRCG1,1) = 'TOLA'
          yield(ITRCG1,1) = 0.044
          nameyld(ITRCG1,2) = 'XYLA'
          yield(ITRCG1,2) = 0.027
c
c  ---- CG2 species ---
c
          numcls(ITRCG2) = 1
          namecls(ITRCG2,1) = 'CG2'
          coefcon(ITRCG2,1) = 1.0
          coeflux(ITRCG2,1) = 1.0
          numyld(ITRCG2) = 2
          nameyld(ITRCG2,1) = 'TOLA'
          yield(ITRCG2,1) = 0.085
          nameyld(ITRCG2,2) = 'XYLA'
          yield(ITRCG2,2) = 0.118
c
c  ---- CG3 species ---
c
          numcls(ITRCG3) = 1
          namecls(ITRCG3,1) = 'CG3'
          coefcon(ITRCG3,1) = 1.0
          coeflux(ITRCG3,1) = 1.0
          numyld(ITRCG3) = 1
          nameyld(ITRCG3,1) = 'ISP'
          yield(ITRCG3,1) = 0.015
c
c  ---- CG4 species ---
c
          numcls(ITRCG4) = 1
          namecls(ITRCG4,1) = 'CG4'
          coefcon(ITRCG4,1) = 1.0
          coeflux(ITRCG4,1) = 1.0
          numyld(ITRCG4) = 1
          nameyld(ITRCG4,1) = 'ISP'
          yield(ITRCG4,1) = 0.12
c
c  ---- CG5 species ---
c
          numcls(ITRCG5) = 1
          namecls(ITRCG5,1) = 'CG5'
          coefcon(ITRCG5,1) = 1.0
          coeflux(ITRCG5,1) = 1.0
          numyld(ITRCG5) = 1
          nameyld(ITRCG5,1) = 'TRP'
          yield(ITRCG5,1) = 0.065
c
c  ---- CG6 species ---
c
          numcls(ITRCG6) = 1
          namecls(ITRCG6,1) = 'CG6'
          coefcon(ITRCG6,1) = 1.0
          coeflux(ITRCG6,1) = 1.0
          numyld(ITRCG6) = 1
          nameyld(ITRCG6,1) = 'TRP'
          yield(ITRCG6,1) = 0.29
c
c  ---- CG7 species ---
c
          numcls(ITRCG7) = 1
          namecls(ITRCG7,1) = 'CG7'
          coefcon(ITRCG7,1) = 1.0
          coeflux(ITRCG7,1) = 1.0
          numyld(ITRCG7) = 1
          nameyld(ITRCG7,1) = 'SQT'
          yield(ITRCG7,1) = 0.85
c
c  ---- PO1 species ---
c
          numcls(ITRPO1) = 1
          namecls(ITRPO1,1) = 'SOA1'
          coefcon(ITRPO1,1) = 1.0
          coeflux(ITRPO1,1) = 1.0
c
c  ---- PO2 species ---
c
          numcls(ITRPO2) = 1
          namecls(ITRPO2,1) = 'SOA2'
          coefcon(ITRPO2,1) = 1.0
          coeflux(ITRPO2,1) = 1.0
c
c  ---- PO3 species ---
c
          numcls(ITRPO3) = 1
          namecls(ITRPO3,1) = 'SOA3'
          coefcon(ITRPO3,1) = 1.0
          coeflux(ITRPO3,1) = 1.0
c
c  ---- PO4 species ---
c
          numcls(ITRPO4) = 1
          namecls(ITRPO4,1) = 'SOA4'
          coefcon(ITRPO4,1) = 1.0
          coeflux(ITRPO4,1) = 1.0
c
c  ---- PO5 species ---
c
          numcls(ITRPO5) = 1
          namecls(ITRPO5,1) = 'SOA5'
          coefcon(ITRPO5,1) = 1.0
          coeflux(ITRPO5,1) = 1.0
c
c  ---- PO6 species ---
c
          numcls(ITRPO6) = 1
          namecls(ITRPO6,1) = 'SOA6'
          coefcon(ITRPO6,1) = 1.0
          coeflux(ITRPO6,1) = 1.0
c
c  ---- PO7 species ---
c
          numcls(ITRPO7) = 1
          namecls(ITRPO7,1) = 'SOA7'
          coefcon(ITRPO7,1) = 1.0
          coeflux(ITRPO7,1) = 1.0
c
c  ---- PPA species ---
c
          numcls(ITRPPA) = 1
          namecls(ITRPPA,1) = 'SOPA'
          coefcon(ITRPPA,1) = 1.0
          coeflux(ITRPPA,1) = 1.0
c
c  ---- PPB species ---
c
          numcls(ITRPPB) = 1
          namecls(ITRPPB,1) = 'SOPB'
          coefcon(ITRPPB,1) = 1.0
          coeflux(ITRPPB,1) = 1.0
      endif
c
c   --- if doing the PRIMARY species ---
c
      if( lprimary ) then
c
c  ---- PEC species ---
c
          numcls(ITRPEC) = 1
          namecls(ITRPEC,1) = 'PEC'
          coefcon(ITRPEC,1) = 1.0
          coeflux(ITRPEC,1) = 1.0
c
c  ---- POA species ---
c
          numcls(ITRPOA) = 1
          namecls(ITRPOA,1) = 'POA'
          coefcon(ITRPOA,1) = 1.0
          coeflux(ITRPOA,1) = 1.0
c
c  ---- PFC species ---
c
          numcls(ITRPFC) = 1
          namecls(ITRPFC,1) = 'FCRS'
          coefcon(ITRPFC,1) = 1.0
          coeflux(ITRPFC,1) = 1.0
c
c  ---- PFN species ---
c
          numcls(ITRPFN) = 1
          namecls(ITRPFN,1) = 'FPRM'
          coeflux(ITRPFN,1) = 1.0
          coefcon(ITRPFN,1) = 1.0
c
c  ---- PCC species ---
c
          numcls(ITRPCC) = 1
          namecls(ITRPCC,1) = 'CCRS'
          coefcon(ITRPCC,1) = 1.0
          coeflux(ITRPCC,1) = 1.0
c
c  ---- PCS species ---
c
          numcls(ITRPCS) = 1
          namecls(ITRPCS,1) = 'CPRM'
          coefcon(ITRPCS,1) = 1.0
          coeflux(ITRPCS,1) = 1.0
      endif
c
c   --- if doing the PRIMARY species ---
c
      if( lmercury ) then
c       
c  ---- HG0 species ---
c
          numcls(ITRHG0) = 1
          namecls(ITRHG0,1) = 'HG0'
          coefcon(ITRHG0,1) = 1.0
          coeflux(ITRHG0,1) = 1.0
c       
c  ---- HG2 species ---
c
          numcls(ITRHG2) = 1
          namecls(ITRHG2,1) = 'HG2'
          coefcon(ITRHG2,1) = 1.0
          coeflux(ITRHG2,1) = 1.0
c       
c  ---- PHG species ---
c
          numcls(ITRPHG) = 1
          namecls(ITRPHG,1) = 'HGP'
          coefcon(ITRPHG,1) = 1.0
          coeflux(ITRPHG,1) = 1.0
      endif
c       
      goto 9999
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

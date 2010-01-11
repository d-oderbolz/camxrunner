c**** SPCSPRCSA
c
      subroutine spcsprcsa(namecls,numcls,coefcon,coeflux,
     &                                     nameyld,numyld,yield,molwt)
c
c----CAMx v4.42 070603
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
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     09/20/03   --gwilson--    Original development
c     08/20/06   --bkoo--       Added ETOH, MTBE & MBUT for updated SAPRC99
c                               Fixed wts99
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'chmstry.com'
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
c
c   NUMS99    I   number of CBIV species used by PSAT treatment
c
      integer NUMS99
c
      parameter( NUMS99 = 55 )
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*10 s99nam(NUMS99)
      integer*4    i, j
      real         wts99(NUMS99)
c
c-----------------------------------------------------------------------
c    Data statements:
c-----------------------------------------------------------------------
c
      data s99nam /
     &        'NO        ', 'NO2       ', 'ALK1      ', 'ALK2      ',
     &        'ALK3      ', 'ALK4      ', 'ALK5      ', 'ETHE      ',
     &        'OLE1      ', 'OLE2      ', 'TERP      ', 'ARO1      ',
     &        'ARO2      ', 'ISOP      ', 'HCHO      ', 'CCHO      ',
     &        'RCHO      ', 'MEOH      ', 'ETOH      ', 'MTBE      ',
     &        'MBUT      ', 'SO2       ', 'PSO4      ', 'PEC       ',
     &        'POA       ', 'FCRS      ', 'FPRM      ', 'CCRS      ',
     &        'CPRM      ', 'HGP       ', 'PNO3      ', 'PNH4      ',
     &        'CG1       ', 'CG2       ', 'CG3       ', 'CG4       ',
     &        'CG5       ', 'SOA1      ', 'SOA2      ', 'SOA3      ',
     &        'SOA4      ', 'SOA5      ', 'HONO      ', 'NXOY      ',
     &        'PAN       ', 'PNA       ', 'MPAN      ', 'PBZN      ',
     &        'HNO4      ', 'RNO3      ', 'HNO3      ', 'NH3       ',
     &        'CRES      ', 'HG0       ', 'HG2       '/

      data wts99 /
     &           46.01    ,    46.01    ,    46.81    ,     47.02   ,
     &           57.72    ,    73.23    ,   106.97    ,     28.05   ,
     &           61.68    ,    79.05    ,   136.24    ,    100.47   ,
     &          113.93    ,    68.12    ,    30.03    ,     44.05   ,
     &           67.32    ,    32.04    ,    46.07    ,     88.15   ,
     &           86.13    ,    64.06    ,     1.0     ,      1.0    ,
     &            1.0     ,     1.0     ,     1.0     ,      1.0    ,
     &            1.0     ,     1.0     ,     1.0     ,      1.0    ,
     &          150.0     ,   150.0     ,   150.0     ,    180.0    ,
     &          150.0     ,     1.0     ,     1.0     ,      1.0    ,
     &            1.0     ,     1.0     ,    46.01    ,     46.01   ,
     &           46.01    ,    46.01    ,    46.01    ,     46.01   ,
     &           46.01    ,    46.01    ,    63.01    ,     17.03   ,
     &          108.14    ,   200.59    ,   200.59    /
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
        do j=1,NUMS99
           if( spname(i) .EQ. s99nam(j) ) molwt(i) = wts99(j)
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
         numcls(ITRVOC) = 19
         namecls(ITRVOC,1) = 'ALK1'
         coefcon(ITRVOC,1) = 1.88
         coeflux(ITRVOC,1) = 1.88
         namecls(ITRVOC,2) = 'ALK2'
         coefcon(ITRVOC,2) = 2.65
         coeflux(ITRVOC,2) = 2.65
         namecls(ITRVOC,3) = 'ALK3'
         coefcon(ITRVOC,3) = 2.92
         coeflux(ITRVOC,3) = 2.92
         namecls(ITRVOC,4) = 'ALK4'
         coefcon(ITRVOC,4) = 4.36
         coeflux(ITRVOC,4) = 4.36
         namecls(ITRVOC,5) = 'ALK5'
         coefcon(ITRVOC,5) = 6.42
         coeflux(ITRVOC,5) = 6.42
         namecls(ITRVOC,6) = 'ETHE'
         coefcon(ITRVOC,6) = 2.0
         coeflux(ITRVOC,6) = 2.0
         namecls(ITRVOC,7) = 'OLE1'
         coefcon(ITRVOC,7) = 3.99
         coeflux(ITRVOC,7) = 3.99
         namecls(ITRVOC,8) = 'OLE2'
         coefcon(ITRVOC,8) = 5.67
         coeflux(ITRVOC,8) = 5.67
         namecls(ITRVOC,9) = 'TERP'
         coefcon(ITRVOC,9) = 10
         coeflux(ITRVOC,9) = 10
         namecls(ITRVOC,10) = 'ARO1'
         coefcon(ITRVOC,10) = 7.27
         coeflux(ITRVOC,10) = 7.27
         namecls(ITRVOC,11) = 'ARO2'
         coefcon(ITRVOC,11) = 8.58
         coeflux(ITRVOC,11) = 8.58
         namecls(ITRVOC,12) = 'ISOP'
         coefcon(ITRVOC,12) = 5.0
         coeflux(ITRVOC,12) = 5.0
         namecls(ITRVOC,13) = 'HCHO'
         coefcon(ITRVOC,13) = 1.0
         coeflux(ITRVOC,13) = 1.0
         namecls(ITRVOC,14) = 'CCHO'
         coefcon(ITRVOC,14) = 2.0
         coeflux(ITRVOC,14) = 2.0
         namecls(ITRVOC,15) = 'RCHO'
         coefcon(ITRVOC,15) = 3.66
         coeflux(ITRVOC,15) = 3.66
         namecls(ITRVOC,16) = 'MEOH'
         coefcon(ITRVOC,16) = 1.0
         coeflux(ITRVOC,16) = 1.0
         namecls(ITRVOC,17) = 'ETOH'
         coefcon(ITRVOC,17) = 2.0
         coeflux(ITRVOC,17) = 2.0
         namecls(ITRVOC,18) = 'MTBE'
         coefcon(ITRVOC,18) = 5.0
         coeflux(ITRVOC,18) = 5.0
         namecls(ITRVOC,19) = 'MBUT'
         coefcon(ITRVOC,19) = 5.0
         coeflux(ITRVOC,19) = 5.0
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
          numcls(ITRTPN) = 5
          namecls(ITRTPN,1) = 'PAN'
          coefcon(ITRTPN,1) = 1.0
          coeflux(ITRTPN,1) = 1.0
          namecls(ITRTPN,2) = 'PNA'
          coefcon(ITRTPN,2) = 1.0
          coeflux(ITRTPN,2) = 1.0
          namecls(ITRTPN,3) = 'MPAN'
          coefcon(ITRTPN,3) = 1.0
          coeflux(ITRTPN,3) = 1.0
          namecls(ITRTPN,4) = 'PBZN'
          coefcon(ITRTPN,4) = 1.0
          coeflux(ITRTPN,4) = 1.0
          namecls(ITRTPN,5) = 'HNO4'
          coefcon(ITRTPN,5) = 1.0
          coeflux(ITRTPN,5) = 1.0
c
c  ---- NTR species ---
c
          numcls(ITRNTR) = 1
          namecls(ITRNTR,1) = 'RNO3'
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
c  ---- ALK species ---
c
          numcls(ITRALK) = 2
          namecls(ITRALK,1) = 'ALK4'
          coefcon(ITRALK,1) = 1.0
          coeflux(ITRALK,1) = 1.0
          namecls(ITRALK,2) = 'ALK5'
          coefcon(ITRALK,2) = 1.0
          coeflux(ITRALK,2) = 1.0
c
c  ---- ARO species ---
c
          numcls(ITRARO) = 2
          namecls(ITRARO,1) = 'ARO1'
          coefcon(ITRARO,1) = 1.0
          coeflux(ITRARO,1) = 1.0
          namecls(ITRARO,2) = 'ARO2'
          coefcon(ITRARO,2) = 1.0
          coeflux(ITRARO,2) = 1.0
c
c  ---- CRE species ---
c
          numcls(ITRCRE) = 1
          namecls(ITRCRE,1) = 'CRES'
          coefcon(ITRCRE,1) = 1.0
          coeflux(ITRCRE,1) = 1.0
          numyld(ITRCRE) = 2
          nameyld(ITRCRE,1) = 'ARO1'
          yield(ITRCRE,1) = 0.207
          nameyld(ITRCRE,1) = 'ARO2'
          yield(ITRCRE,1) = 0.187
c
c  ---- TRP species ---
c
          numcls(ITRTRP) = 1
          namecls(ITRTRP,1) = 'TERP'
          coefcon(ITRTRP,1) = 1.0
          coeflux(ITRTRP,1) = 1.0
c
c  ---- CG1 species ---
c
          numcls(ITRCG1) = 1
          namecls(ITRCG1,1) = 'CG1'
          coefcon(ITRCG1,1) = 1.0
          coeflux(ITRCG1,1) = 1.0
          numyld(ITRCG1) = 2
          nameyld(ITRCG2,1) = 'ARO1'
          yield(ITRCG2,1) = 430.0
          nameyld(ITRCG2,2) = 'ARO2'
          yield(ITRCG2,2) = 268.0
c
c  ---- CG2 species ---
c
          numcls(ITRCG2) = 1
          namecls(ITRCG2,1) = 'CG2'
          coefcon(ITRCG2,1) = 1.0
          coeflux(ITRCG2,1) = 1.0
          numyld(ITRCG2) = 2
          nameyld(ITRCG2,1) = 'ARO1'
          yield(ITRCG2,1) = 836.0
          nameyld(ITRCG2,2) = 'ARO2'
          yield(ITRCG2,2) = 1178.0
c
c  ---- CG3 species ---
c
          numcls(ITRCG3) = 1
          namecls(ITRCG3,1) = 'CG3'
          coefcon(ITRCG3,1) = 1.0
          coeflux(ITRCG3,1) = 1.0
          numyld(ITRCG3) = 2
          nameyld(ITRCG3,1) = 'ALK4'
          yield(ITRCG3,1) = 0.0003
          nameyld(ITRCG3,2) = 'ALK5'
          yield(ITRCG3,2) = 0.016
c
c  ---- CG4 species ---
c
          numcls(ITRCG4) = 1
          namecls(ITRCG4,1) = 'CG4'
          coefcon(ITRCG4,1) = 1.0
          coeflux(ITRCG4,1) = 1.0
c
c  ---- CG5 species ---
c
          numcls(ITRCG5) = 1
          namecls(ITRCG5,1) = 'CG5'
          coefcon(ITRCG5,1) = 1.0
          coeflux(ITRCG5,1) = 1.0
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
          namecls(ITRPHG,1) = 'PHG'
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

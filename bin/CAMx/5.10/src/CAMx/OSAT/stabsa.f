c**** STABSA
c
      subroutine stabsa()
      use filunit
      use chmstry
      use tracer
      use rtracchm
      use grid
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine sets the flags that determine if each species in the
c   species list should be added to the VOC or NOx tracers.  The carbon
c   number for the VOC species is also put into the appropriate place
c   in the array.
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     05/27/96   --gwilson--    Original development
c     09/20/03   --gwilson--    Completely changed the way tracer classes
c                               are identified to deal with PSAT
c     08/20/06   --bkoo--       Added ETOH, MTBE & MBUT for updated SAPRC99
c                               Fixed rkohsprc & rmirsprc
c     09/01/06   --bkoo--       Updated rkohcbiv for ISOP, MEOH & ETOH
c     12/29/06   --bkoo--       Revised for the updated SOA scheme
c     01/08/06   --bkoo--       Added Mechanism 6 (CB05)
c     12/29/06   --bkoo--       Revised for the updated SOA scheme
c     01/08/07   --bkoo--       Added Mechanism 6 (CB05)
c     07/16/07   --bkoo--       Added HRVOC
c     10/25/07   --gyarwood-    Make MIR and kOH values per carbon
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Arguement declarations:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c   NSACBIV    I    the number of VOC species names to be checked (CBIV)
c   NSASPRC    I    the number of VOC species names to be checked (SAPRC)
c   NSACB05    I    the number of VOC species names to be checked (CB05)
c   NSASOAP    I    the number of VOC species names to be checked (SOAP)
c   MXSANAM    I    maximum size of the arrays
c   NHRVCBIV   I    the number of HRVOC species names (CBIV)
c   NHRVSPRC   I    the number of HRVOC species names (SAPRC)
c   NHRVCB05   I    the number of HRVOC species names (CB05)
c   MXHRVNAM   I    maximum size of the HRVOC arrays
c
      integer   NSACBIV, NSASPRC, NSACB05, NSASOAP, MXSANAM
      integer   NHRVCBIV, NHRVSPRC, NHRVCB05, MXHRVNAM
c
      parameter( NSACBIV = 10 )
      parameter( NSASPRC = 19 )
      parameter( NSACB05 = 14 )
      parameter( NSASOAP = 2 )
      parameter( MXSANAM = MAX(NSACBIV, NSASPRC, NSACB05) )
      parameter( NHRVCBIV = 2 )
      parameter( NHRVSPRC = 3 )
      parameter( NHRVCB05 = 3 )
      parameter( MXHRVNAM = MAX(NHRVCBIV, NHRVSPRC, NHRVCB05) )
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*10 spcnam(MXSANAM), hrvnam(MXHRVNAM)
      character*10 spccbiv(NSACBIV), spcsprc(NSASPRC), spccb05(NSACB05)
      character*10 hrvcbiv(NHRVCBIV),hrvsprc(NHRVSPRC),hrvcb05(NHRVCB05)
      character*10 spcsoap(NSASOAP)
      character*10 nameyld(MXALCLS,MXSPEC)
      character*10 nonam, no2nam, o3name, namecls(MXALCLS,MXSPEC)
      integer      numcls(MXALCLS), ispc, nnames, idxo3n, idxo3v, i
      integer      numyld(MXALCLS), nhrvoc
      real         carbon(MXSANAM), rkoh(MXSANAM), rmir(MXSANAM)
      real         carbcbiv(NSACBIV),rkohcbiv(NSACBIV),rmircbiv(NSACBIV)
      real         carbsprc(NSASPRC),rkohsprc(NSASPRC),rmirsprc(NSASPRC)
      real         carbcb05(NSACB05),rkohcb05(NSACB05),rmircb05(NSACB05)
      real         rkohsoap(NSASOAP)
      real         coefcon(MXALCLS,MXSPEC), coeflux(MXALCLS,MXSPEC)
      real         yield(MXALCLS,MXSPEC)
c
c-----------------------------------------------------------------------
c    Data statements:
c-----------------------------------------------------------------------
c
      data spccbiv /'PAR       ','OLE       ','ETH       ',
     &              'TOL       ','XYL       ','FORM      ',
     &              'ALD2      ','ISOP      ','MEOH      ',
     &              'ETOH      '/
c
      data carbcbiv /1.,            2.,           2.,
     &               7.,            8.,           1.,
     &               2.,            5.,           1.,
     &               2./
c
      data rkohcbiv  /1.20E3,       4.20E4,       1.19E4,
     &                9.15E3,       3.62E4,       1.50E4,
     &                2.40E4,       1.47E5,       1.35E3,
     &                4.71E3/
c
      data rmircbiv /0.346,       9.66,        4.66,
     &               3.60,        19.1,        5.77,
     &               6.01,        21.9,        0.410,
     &               1.33/
c
      data hrvcbiv /'ETH       ','OLE       '/
c
      data spcsprc /'ALK1      ','ALK2      ','ALK3      ',
     &              'ALK4      ','ALK5      ','ETHE      ',
     &              'OLE1      ','OLE2      ','TERP      ',
     &              'ARO1      ','ARO2      ','ISOP      ',
     &              'HCHO      ','CCHO      ','RCHO      ',
     &              'MEOH      ','ETOH      ','MTBE      ',
     &              'MBUT      '/
c
      data carbsprc /1.88,        2.65,         2.92,
     &               4.36,        6.42,         2.00,
     &               3.99,        5.67,        10.00,
     &               7.27,        8.58,         5.00,
     &               1.00,        2.00,         3.66,
     &               1.00,        2.00,         5.00,
     &               5.00/
c
      data rkohsprc  /3.75E2,     1.53E3,      3.51E3,
     &                6.49E3,     1.38E4,      1.26E4,
     &                4.77E4,     9.34E4,      1.22E5,
     &                8.79E3,     3.90E4,      1.45E5,
     &                1.36E4,     2.34E4,      2.95E4,
     &                1.35E3,     4.92E3,      4.34E3,
     &                9.35E4/
c
      data rmirsprc  /0.390,      0.803,       2.19,
     &                3.11,       4.95,        5.44,
     &                9.73,       11.0,        8.54,
     &                6.72,       15.4,        12.6,
     &                6.49,       5.79,        9.45,
     &                1.34,       1.62,        1.43,
     &                9.12/
c
      data hrvsprc /'ETHE      ','OLE1      ','OLE2      '/
c
      data spccb05 /'PAR       ','ETHA      ','MEOH      ',
     &              'ETOH      ','ETH       ','OLE       ',
     &              'IOLE      ','ISOP      ','TERP      ',
     &              'FORM      ','ALD2      ','ALDX      ',
     &              'TOL       ','XYL       '/
c
      data carbcb05 /1.,          2.,          1.,
     &               2.,          2.,          2.,
     &               4.,          5.,         10.,
     &               1.,          2.,          2.,
     &               7.,          8./
c
      data rkohcb05 /1.20E3,      3.54E2,      1.35E3,
     &               4.71E3,      1.20E4,      4.73E4,
     &               9.35E4,      1.47E5,      1.00E5,
     &               1.33E4,      2.05E4,      2.93E4,
     &               8.75E3,      3.71E4/
c
      data rmircb05 /0.319,       0.109,       0.361,
     &               1.04,        4.37,        8.24,
     &               13.1,        11.6,        8.82,
     &               4.50,        4.45,        6.81,
     &               2.94,        14.8/
c
      data hrvcb05 /'ETH       ','IOLE      ','OLE       '/
c
      data spcsoap /'TOLA      ','XYLA      '/
c
      data rkohsoap  / 8.75E3,  3.71E4 /
c
      data nonam  /'NO        '/
c
      data no2nam /'NO2       '/
c
      data o3name /'O3        '/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  ---- initialize the arrays for reactivity ---
c
      do i=1,nspec
         crbnum(i) = 0.
         rkohrt(i) = 0.
         rmirrt(i) = 0.
         lusespc(i) = .FALSE.
      enddo
c
c  --- calculate the beginning of the various tracer types ---
c      there will be (ngroup+1) if there is an extra group for the
c      "leftover" group  ----
c      
      if( lbndry ) then
          nbdic = 6 
      else
          nbdic = 2
      endif
      if( ngroup .EQ. 0 ) then
          ncount = nregin 
      else
          if( leftovr ) then
             ncount = (ngroup + 1) * nregin
          else
             ncount = ngroup * nregin
          endif
      endif
c
c  ---- load the local arrays based on the mechanism ---
c
      if( idmech .EQ. 5 ) then
         nnames = NSASPRC
         call spcsprcsa(namecls,numcls,coefcon,coeflux,
     &                                 nameyld,numyld,yield,mwspec)
         do i=1,nnames
            spcnam(i) = spcsprc(i)
            carbon(i) = carbsprc(i)
            rkoh(i) = rkohsprc(i)/carbon(i)
            rmir(i) = rmirsprc(i)/carbon(i)
         enddo
         nhrvoc = NHRVSPRC
         do i=1,nhrvoc
            hrvnam(i) = hrvsprc(i)
         enddo
      elseif( idmech .EQ. 6 ) then
         nnames = NSACB05
         call spccb05sa(namecls,numcls,coefcon,coeflux,
     &                                 nameyld,numyld,yield,mwspec)
         do i=1,nnames
            spcnam(i) = spccb05(i)
            carbon(i) = carbcb05(i)
            rkoh(i) = rkohcb05(i)/carbon(i)
            rmir(i) = rmircb05(i)/carbon(i)
         enddo
         nhrvoc = NHRVCB05
         do i=1,nhrvoc
            hrvnam(i) = hrvcb05(i)
         enddo
      else
         nnames = NSACBIV
         call spccbivsa(namecls,numcls,coefcon,coeflux,
     &                                nameyld,numyld,yield,mwspec)
         do i=1,nnames
            spcnam(i) = spccbiv(i)
            carbon(i) = carbcbiv(i)
            rkoh(i) = rkohcbiv(i)/carbon(i)
            rmir(i) = rmircbiv(i)/carbon(i)
         enddo
         nhrvoc = NHRVCBIV
         do i=1,nhrvoc
            hrvnam(i) = hrvcbiv(i)
         enddo
      endif
c
c  --- make sure there is enough space ---
c
      ntrcls = 0
      do i=1,MXALCLS
        if( numcls(i) .GT. 0 ) ntrcls = ntrcls + 1
      enddo
c
c  --- call routine to allocate arrays by class ---
c
      call alloc_tracer_class(nspec)
      call zeros(trspmap,nspec*ntrcls)
      call zeros(fluxmap,nspec*ntrcls)
      call zeros(yratmap,nspec*ntrcls)
c
c  --- initialize the pointers into the gridded conc array ---
c
      ntrcls = 0
      iptrbeg = 1
      idxo3n = 0
      idxo3v = 0
      do i=1,MXALCLS
        if( numcls(i) .GT. 0 ) then
           ntrcls = ntrcls + 1
           iptcls(ntrcls) = iptrbeg
           ipttrc(ntrcls) = iptrbeg
           iptrbeg = iptrbeg + ncount + nbdic
           nptcls(ntrcls) = iptrbeg - 1
           npttrc(ntrcls) = iptrbeg - 1
           iemcls(ntrcls) = iptcls(ntrcls) + nbdic
           idxcls(ntrcls) = i
           idxipt(i) = ntrcls
           if( i .EQ. ITRO3N ) idxo3n = ntrcls
           if( i .EQ. ITRO3V ) idxo3v = ntrcls
        endif
      enddo
      ipttim = iptrbeg
      iemtim = ipttim
c
c   --- adjust the pointers for O3N and O3V (together make up O3) ---
c
      if( idxo3n .GT. 0 .AND. idxo3v .GT. 0 ) then
        ipttrc(idxo3v) = 0
        npttrc(idxo3n) = npttrc(idxo3v)
        npttrc(idxo3v) = 0
      endif
c
c   --- calculate the number of tracers at first time step
c       (1 timing release is updated later) ---
c
      nreles = 0
      ntotsp = ipttim-1
      nsaspc = ipttim-1
c
c   --- call routine to allocate the arrays that depend on total
c       tracer species ---
c
      if( .NOT. lddm .AND. .NOT. lhddm ) then
         call alloc_tracer_specs(ngrid,ncol,nrow,nlay,tectyp)
         call alloc_tracer_vdep(ngrid,ncol,nrow,ntotsp)
      endif
c
      if( ntrtim .EQ. 0 ) npttim = 0
c
c   --- loop over the species in the chemparam file and 
c       intialize the flags ---
c
      do ispc=1,nspec
         lvocsp(ispc) = .FALSE.
         lnoxsp(ispc) = .FALSE.
         lo3sp(ispc) = .FALSE.
         lvocsoa(ispc) = .FALSE.
         lhrvoc(ispc) = .FALSE.
c
c   --- loop over the tracer classes ---
c
         do i=1,ntrcls
              itr = idxcls(i)
c
c   ---- loop over the model species making up this class ---
c
               do imod=1,numcls(itr)
c
c   --- check the name, if it matches load the global array ---
c
                 if( spname(ispc) .EQ. namecls(itr,imod) ) then
                     trspmap(ispc,i) = coefcon(itr,imod)
                     fluxmap(ispc,i) = coeflux(itr,imod)
                     lusespc(ispc) = .TRUE.
                 endif
               enddo
c
c   ---- loop over the model species for yields ---
c
               do imod=1,numyld(itr)
c
c   --- check the name, if it matches load the global array ---
c
                 if( spname(ispc) .EQ. nameyld(itr,imod) ) then
                     yratmap(ispc,i) = yield(itr,imod)
                 endif
               enddo
c
c   ---- next tracer class ---
c
         enddo
c
c   --- check for O3 species ----
c
         if( spname(ispc) .EQ. o3name ) then
              lo3sp(ispc) = .TRUE.
         endif
c
c   --- check for NOx species ----
c
         if( spname(ispc) .EQ. nonam .OR. spname(ispc) .EQ. no2nam) then
              lnoxsp(ispc) = .TRUE.
         endif
c
c   ---- check for reactive VOC species ---
c
         do i=1,nnames
            if( spname(ispc) .EQ. spcnam(i) ) then
               lvocsp(ispc) = .TRUE.
               crbnum(ispc) = carbon(i)
               rkohrt(ispc) = rkoh(i)
               rmirrt(ispc) = rmir(i)
            endif
         enddo
c
c   ---- check for SOA-producing VOC species ---
c   NOTE: 1. Carbon numbers and MIR values are irrelevant to PSAT
c         2. No need to weight tracers that have single model species
c
         do i=1,NSASOAP
            if( spname(ispc) .EQ. spcsoap(i) ) then
               lvocsoa(ispc) = .TRUE.
               rkohrt(ispc) = rkohsoap(i)
            endif
         enddo
c
c   ---- check for HRVOC species ---
c   NOTE: set for DDM
c
         do i=1,nhrvoc
            if( spname(ispc) .EQ. hrvnam(i) ) lhrvoc(ispc) = .TRUE.
         enddo
c
c  --- next simulation species ---
c
      enddo
c      
c  --- return to calling routine ---
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

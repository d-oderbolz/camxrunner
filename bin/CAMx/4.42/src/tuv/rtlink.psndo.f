      SUBROUTINE rtlnkdo(nz,z,
     $     iw, ag, zen,
     $     dsdh,nid,
     $     dtair, 
     $     dto3, 
     $     dto2,
     $     dtso2,
     $     dtno2, 
     $     dtcld, omcld, gcld,
     $     dtaer,omaer,gaer,
     $     edir, edn, eup, fdir, fdn, fup)

*bm  added sdir, sdn, sup (sine-weighted intensities); these values are calculated within
*bm  psndo.f - watch out for sindir, sinup, and sindn     

      IMPLICIT NONE

      INCLUDE 'params'

      REAL dr
      PARAMETER (dr = pi/180.)

      INTEGER MAXCLY, MAXULV, MAXUMU, MAXCMU, MAXPHI
      PARAMETER(MAXCLY=101,MAXULV=101)
      PARAMETER(MAXUMU=32,MAXCMU=32)
      PARAMETER(MAXPHI=3)

* input

      REAL dto3(kz,kw), dto2(kz,kw), dtso2(kz,kw), dtno2(kz,kw)
      REAL dtcld(kz,kw), omcld(kz,kw), gcld(kz,kw)
      REAL dtaer(kz,kw), omaer(kz,kw), gaer(kz,kw)
      REAL dtair(kz,kw)
      REAL dsdh(0:kz,kz)
      INTEGER nid(0:kz)

      REAL ag
      REAL zen

* altitude working grid:

      INTEGER nz
      REAL z(kz)

* output

      REAL edir(kz), edn(kz), eup(kz)
      REAL fdir(kz), fdn(kz), fup(kz)

*bm  added array LDIF for convenience (sky radiance)
      REAL ldif(MAXUMU, kz)

* internal

*bm  sine weighted intensity
      REAL sdir(kz), sdn(kz), sup(kz)
      REAL irrad

      CHARACTER  HEADER*127
      LOGICAL  DELTAM, LAMBER, PLANK, ONLYFL, PRNT(7), USRANG, USRTAU
      INTEGER  IBCND, NLYR,
     $     NUMU, NSTR, NPHI, NTAU
      REAL     ACCUR, ALBEDO, BTEMP, DTAUC( MAXCLY ), FBEAM, FISOT,
     $     HL( 0:MAXCMU ), PHI( MAXPHI ), PMOM( 0:MAXCMU, MAXCLY ),
     $     PHI0, SSALB( MAXCLY ), TEMPER( 0:MAXCLY ), TEMIS, TTEMP,
     $     WVNMLO, WVNMHI, UMU( MAXUMU ), CWT( MAXUMU ), UMU0, 
     $     UTAU( MAXULV )
      REAL     RFLDIR( MAXULV ), RFLDN( MAXULV ), FLUP( MAXULV ),
     $     UAVG( MAXULV ), DFDT( MAXULV ), U0U( MAXUMU, MAXULV ),
     $     UU( MAXUMU, MAXULV, MAXPHI ), ALBMED( MAXUMU ),
     $     TRNMED( MAXUMU ),
     $     uavgso( maxulv ), uavgup( maxulv ), uavgdn( maxulv ),
     $     sindir( maxulv ), sinup( maxulv ), sindn( maxulv )

      REAL m0
      REAL dt(kz), om(kz), g(kz)
      REAL om1
      REAL dtabs, dtsct
      REAL dscld, dacld, dsaer, daaer
      REAL pmcld, pmray, pmaer

      REAL precis
      INTEGER i, ii, iw, istr, iu

* Discrete ordinate constants:
* For pseudo-spherical DISORT, PLANK, USRTAU and USRANG must be .FALSE.;
* ONLYFL must be .TRUE.; FBEAM = 1.; FISOT = 0.; IBCND = 0

      data PRECIS /1.E-8/
      data LAMBER /.TRUE./
      data USRTAU /.FALSE./
      data PLANK /.FALSE./
      data USRANG /.FALSE./
      data ONLYFL /.TRUE./
      data PRNT /.FALSE.,.FALSE.,.FALSE.,.FALSE.,.FALSE.,
     $		 .FALSE.,.FALSE./
      data ACCUR /0.0001/
      data HEADER /' '/
      data NPHI /0/
      data IBCND /0/
      data FBEAM /1./
      data FISOT /0.0/
      data PHI0 /0.0/

* delat-M scaling option

      data DELTAM /.true./

* number of streams:

      data NSTR /8/

*_______________________________________________________________________

* initialize:

      do 5 i = 1, nz
         fdir(i) = 0.
         fup(i) = 0.
         fdn(i) = 0.
         edir(i) = 0.
         eup(i) = 0.
         edn(i) = 0.
         sdir(i) = 0.
         sup(i) = 0.
         sdn(i) = 0.
 5    continue

      m0 = COS(zen*dr)

*  set here any coefficients specific to rt scheme, 
* ----

      UMU0 = m0
      NLYR = nz - 1
      ALBEDO = ag

      do 10 i = 1, nz - 1

         dscld = dtcld(i,iw)*omcld(i,iw)
         dacld = dtcld(i,iw)*(1.-omcld(i,iw))

         dsaer = dtaer(i,iw)*omaer(i,iw)
         daaer = dtaer(i,iw)*(1.-omaer(i,iw))

         dtsct = dtair(i,iw) + dscld + dsaer
         dtabs = dtso2(i,iw) + dto2(i,iw) + dto3 (i,iw) +
     >           dtno2(i,iw) + dacld + daaer

 	 dtabs = amax1(dtabs,1./largest)
 	 dtsct = amax1(dtsct,1./largest)

* invert z-coordinate:

         ii = nz - i
         dt(ii) = dtsct + dtabs
         om(ii) = dtsct/(dtsct + dtabs)
           IF(dtsct .EQ. 1./largest) om(ii) = 1./largest
         g(ii) = (gcld(i,iw)*dscld + gaer(i,iw)*dsaer)/dtsct

* DISORD parameters

         OM1 = AMIN1(OM(ii),1.-PRECIS)
         SSALB( II ) = AMAX1(OM1,PRECIS)
         DTAUC( II ) = AMAX1(DT(ii),PRECIS)

*  phase function - assume Henyey-Greenstein for cloud and aerosol
*  and Rayleigh for molecular scattering

         PMOM(0,II) = 1.0
         DO 15 ISTR = 1, NSTR
            PMCLD = GCLD(i,iw)**(ISTR)
            PMAER = GAER(i,iw)**(ISTR)
               IF(ISTR .EQ. 2) THEN
                  PMRAY = 0.1
               ELSE
                  PMRAY = 0.
               ENDIF
               PMOM(ISTR,II) = (PMCLD*DSCLD + PMAER*DSAER + 
     $              PMRAY*DTAIR(i,iw)) / DTSCT
 15         CONTINUE

 10      CONTINUE

	CALL  PSNDO( dsdh, nid,
     $                NLYR, DTAUC, SSALB, PMOM, TEMPER, WVNMLO,
     $                WVNMHI, USRTAU, NTAU, UTAU, NSTR, USRANG,
     $                NUMU, UMU, CWT, NPHI, PHI, IBCND, FBEAM, UMU0,
     $                PHI0, FISOT, LAMBER, ALBEDO, HL, BTEMP,
     $                TTEMP, TEMIS, DELTAM, PLANK, ONLYFL,
     $                ACCUR, PRNT, HEADER, MAXCLY, MAXULV,
     $                MAXUMU, MAXCMU, MAXPHI, RFLDIR, RFLDN,
     $                FLUP, DFDT, UAVG, UU, U0U, ALBMED, TRNMED,
     $                uavgso, uavgup, uavgdn,
     $                sindir, sinup, sindn)

* output (invert z-coordinate)

         DO 20 i = 1, nz
		ii = nz - i + 1

* irradiances:

            edir(i) = RFLDIR(II)
            edn(i)  = RFLDN(II)
            eup(i)  = FLUP(II)

* actinic fluxes:

            fdir(i) = 4.* pi * uavgso(ii)
            fdn(i)  = 4.* pi * uavgdn(ii)
            fup(i)  = 4.* pi * uavgup(ii)

* sine-weighted intensity:

            sdir(i) = sindir(ii)
            sdn(i)  = sindn(ii)
            sup(i)  = sinup(ii)

*bm  azimutally averaged radiances at computational angles:
*bm  ldif(iu,i) is the radiance at level i and cosine of polar angle UMU(iu);
*bm  the polar angle is measured from the upward direction, implying that 
*bm  positive mu is upwelling and negative mu down-welling radiation.

            DO iu = 1, numu
               ldif(iu,i) = u0u (iu, ii)
            ENDDO

 20      continue

*bm  example output:
c         DO iu = 1, numu
c            WRITE (*,*) 'rad',iu,umu(iu),ldif(iu,1)
c         ENDDO

*bm  example for an integral, irradiance

c         irrad = 0.
c         DO iu = 1, NUMU/2
c            irrad = irrad + ldif(iu,1)*cwt(iu)*umu(iu)
c         ENDDO
c         irrad = irrad * 2. * pi

c         WRITE (*,*) edn(1),' = ',irrad,' ?'

      return
      end


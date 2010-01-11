      SUBROUTINE rtlnk2s(nz,z,
     $     iw, ag, zen, 
     $     dsdh, nid,
     $     dtrl, 
     $     dto3, 
     $     dto2, 
     $     dtso2, 
     $     dtno2, 
     $     dtcld, omcld, gcld,
     $     dtaer,omaer,gaer,
     $     edir, edn, eup, fdir, fdn, fup)
*_______________________________________________________________________

      IMPLICIT NONE

      INCLUDE 'params'

* input:

      INTEGER nz, iw
      REAL z(kz)
      REAL ag
      REAL zen
      REAL dtrl(kz,kw)
      REAL dto3(kz,kw), dto2(kz,kw), dtso2(kz,kw), dtno2(kz,kw)
      REAL dtcld(kz,kw), omcld(kz,kw), gcld(kz,kw)
      REAL dtaer(kz,kw), omaer(kz,kw), gaer(kz,kw)
      REAL dsdh(0:kz,kz)
      INTEGER nid(0:kz)


* output
      REAL edir(kz), edn(kz), eup(kz)
      REAL fdir(kz), fdn(kz), fup(kz)

* more program constants:
      REAL dr
      PARAMETER (dr = pi/180.)

* local:
      REAL dt(kz), om(kz), g(kz)
      REAL ediri(kz), edni(kz), eupi(kz)
      REAL fdiri(kz), fdni(kz), fupi(kz)
      REAL daaer, dtsct, dtabs, dsaer, dscld, dacld
      INTEGER i, ii
      LOGICAL delta

      DATA delta /.true./
*_______________________________________________________________________

* initialize:

      DO 5 i = 1, nz
         fdir(i) = 0.
         fup(i) = 0.
         fdn(i) = 0.
         edir(i) = 0.
         eup(i) = 0.
         edn(i) = 0.
 5    CONTINUE

*  set here any coefficients specific to rt scheme, 
* ----

      DO 10, i = 1, nz - 1

         dscld = dtcld(i,iw)*omcld(i,iw)
         dacld = dtcld(i,iw)*(1.-omcld(i,iw))

         dsaer = dtaer(i,iw)*omaer(i,iw)
         daaer = dtaer(i,iw)*(1.-omaer(i,iw))

         dtsct = dtrl(i,iw) + dscld + dsaer
         dtabs = dto3(i,iw) + dto2(i,iw) + dtso2(i,iw) + 
     >           dtno2(i,iw) + dacld + daaer

 	 dtabs = AMAX1(dtabs,1./largest)
 	 dtsct = AMAX1(dtsct,1./largest)

* invert z-coordinate:

         ii = nz - i
         dt(ii) = dtsct + dtabs
         om(ii) = dtsct/(dtsct + dtabs)
           IF(dtsct .EQ. 1./largest) om(ii) = 1./largest
         g(ii) = (gcld(i,iw)*dscld + gaer(i,iw)*dsaer)/dtsct

   10 CONTINUE

*  call rt routine:

      CALL ps2str(nz,zen,ag,dt,om,g,
     $         dsdh, nid, delta,
     $         fdiri, fupi, fdni, ediri, eupi, edni)

* put on upright z-coordinate

      DO 20, i = 1, nz
         ii = nz - i + 1
         fdir(i) = fdiri(ii)
         fup(i) = fupi(ii)
         fdn(i) = fdni(ii)
         edir(i) = ediri(ii)
         eup(i) = eupi(ii)
         edn(i) = edni(ii)
 20   CONTINUE
*_______________________________________________________________________

      RETURN
      END   

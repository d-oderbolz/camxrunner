      SUBROUTINE rdo3xs(mabs, nz,t,nw,wl, xs)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Read ozone molecular absorption cross section.  Re-grid data to match    =*
*=  specified wavelength working grid. Interpolate in temperature as needed  =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  MABS   - INTEGER, option for splicing different combinations of       (I)=*
*=           absorption cross secttions                                      =*
*=  NZ     - INTEGER, number of altitude levels or layers                 (I)=*
*=  T      - REAL, temperature of levels or layers                        (I)=*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid. In vacuum, nm                          =*
*=  XS     - REAL, molecular absoprtion cross section (cm^2) of O3 at     (O)=*
*=           each specified wavelength (WMO value at 273)                    =*
*-----------------------------------------------------------------------------*

      IMPLICIT NONE
      INCLUDE 'params'

* input: (altitude working grid)

      INTEGER iw, nw
      REAL wl(kw)

      INTEGER iz, nz
      REAL t(kz)

* internal

      INTEGER mabs
      REAL tc

* output:
* ozone absorption cross sections interpolated to 
*   working wavelength grid (iw)
*   working altitude grid (iz) for temperature of layer or level (specified in call)
* Units are cm2 molecule-1 in vacuum

      REAL xs(kz,kw)

* wavelength-interpolated values from different O3 data sources 
* also values of significant wavelengths, converted to vacuum

      REAL rei218(kw), rei228(kw), rei243(kw), rei295(kw)
      REAL v195, v345, v830

      REAL wmo203(kw), wmo273(kw)
      REAL v176, v850

      REAL jpl295(kw), jpl218(kw)
      REAL v186, v825

      REAL mol226(kw), mol263(kw), mol298(kw)
      REAL v185, v240, v350

      REAL c0(kw), c1(kw), c2(kw)
      REAL vb245, vb342

*_______________________________________________________________________
* read data from different sources
* rei = Reims group (Malicet et al., Brion et al.)
* jpl = JPL 2006 evaluation
* wmo = WMO 1985 O3 assessment
* mol = Molina and Molina
* bas = Bass et al.

      CALL o3_rei(nw,wl, rei218,rei228,rei243,rei295, v195,v345,v830)

      CALL o3_jpl(nw,wl, jpl218,jpl295, v186,v825)

      CALL o3_wmo(nw,wl, wmo203,wmo273, v176,v850)

      CALL o3_mol(nw,wl, mol226,mol263,mol298, v185,v240,v350)

      CALL o3_bas(nw,wl, c0,c1,c2, vb245,vb342)

****** option 1:

      IF(mabs. EQ. 1) THEN

* assign according to wavelength range:
*  175.439 - 185.185  1985WMO (203, 273 K)
*  185.185 - 195.00   2006JPL_O3 (218, 295 K)
*  195.00  - 345.00   Reims group (218, 228, 243, 295 K)
*  345.00  - 830.00   Reims group (295 K)
*  no extrapolations in temperature allowed

         DO 10 iw = 1, nw-1
         DO 20 iz = 1, nz

         IF(wl(iw) .LT. v185) THEN
            xs(iz,iw) = wmo203(iw) + 
     $           (wmo273(iw) - wmo203(iw))*(t(iz) - 203.)/(273. - 203.)
            IF (t(iz) .LE. 203.) xs(iz,iw) = wmo203(iw)
            IF (t(iz) .GE. 273.) xs(iz,iw) = wmo273(iw)
         ENDIF

         IF(wl(iw) .GE. v185 .AND. wl(iw) .LE. v195) THEN
            xs(iz,iw) = jpl218(iw) + 
     $           (jpl295(iw) - jpl218(iw))*(t(iz) - 218.)/(295. - 218.)
            IF (t(iz) .LE. 218.) xs(iz,iw) = jpl218(iw)
            IF (t(iz) .GE. 295.) xs(iz,iw) = jpl295(iw)
         ENDIF

         IF(wl(iw) .GE. v195 .AND. wl(iw) .LT. v345) THEN
            IF (t(iz) .GE. 218. .AND. t(iz) .LT. 228.) THEN
               xs(iz,iw) = rei218(iw) + 
     $              (t(iz)-218.)*(rei228(iw)-rei218(iw))/(228.-218.)
            ELSEIF (t(iz) .GE. 228. .AND. t(iz) .LT. 243.) THEN
               xs(iz,iw) = rei228(iw) +
     $              (t(iz)-228.)*(rei243(iw)-rei228(iw))/(243.-228.)
            ELSEIF (t(iz) .GE. 243. .AND. t(iz) .LT. 295.) THEN
               xs(iz,iw) = rei243(iw) +
     $              (t(iz)-243.)*(rei295(iw)-rei243(iw))/(295.-243.)
            ENDIF
            IF (t(iz) .LT. 218.) xs(iz,iw) = rei218(iw)
            IF (t(iz) .GE. 295.) xs(iz,iw) = rei295(iw)
         ENDIF

         IF(wl(iw) .GE. v345) THEN
            xs(iz,iw) = rei295(iw)
         ENDIF

 20      CONTINUE
 10      CONTINUE

      ELSEIF(mabs .EQ. 2) THEN

* use exclusively JPL-2006

         DO iw = 1, nw-1
         DO iz = 1, nz

            xs(iz,iw) = jpl218(iw) + 
     $           (jpl295(iw) - jpl218(iw))*(t(iz) - 218.)/(295. - 218.)
            IF (t(iz) .LE. 218.) xs(iz,iw) = jpl218(iw)
            IF (t(iz) .GE. 295.) xs(iz,iw) = jpl295(iw)

         ENDDO
         ENDDO

      ELSEIF(mabs .EQ. 3) THEN

* use exclusively Molina and Molina

         DO iw = 1, nw-1
         DO iz = 1, nz
            
            IF(wl(iw) .LT. v240) THEN
               xs(iz,iw) = mol226(iw) + 
     $              (t(iz)-226.)*(mol298(iw)-mol226(iw))/(298.-226.)
            ELSE
               IF(t(iz) .LT. 263.) THEN
                  xs(iz,iw) = mol226(iw) + 
     $                 (t(iz)-226.)*(mol263(iw)-mol226(iw))/(263.-226.)
               ELSE
                  xs(iz,iw) = mol263(iw) + 
     $                 (t(iz)-263.)*(mol298(iw)-mol263(iw))/(298.-263.)
               ENDIF
            ENDIF
            IF (t(iz) .LE. 226.) xs(iz,iw) = mol226(iw)
            IF (t(iz) .GE. 298.) xs(iz,iw) = mol298(iw)

         ENDDO
         ENDDO

      ELSEIF(mabs .EQ. 4) THEN

* use exclusively Bass et al.
* note limited wavelength range 245-342

         DO iw = 1, nw-1
         DO iz = 1, nz

            tc = t(iz) - 273.15
            xs(iz,iw) = c0(iw) + c1(iw)*tc + c2(iw)*tc*tc

         ENDDO
         ENDDO

      ELSE
         STOP 'mabs not set in rdxs.f'
      ENDIF

      RETURN
      END

*=============================================================================*

      SUBROUTINE o3_rei(nw,wl, 
     $     rei218,rei228,rei243,rei295, v195,v345,v830)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Read and interpolate the O3 cross section from Reims group               =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  REI218 - REAL, cross section (cm^2) for O3 at 218K                    (O)=*
*=  REI228 - REAL, cross section (cm^2) for O3 at 218K                    (O)=*
*=  REI243 - REAL, cross section (cm^2) for O3 at 218K                    (O)=*
*=  REI295 - REAL, cross section (cm^2) for O3 at 218K                    (O)=*
*=  V195   - REAL, exact wavelength in vacuum for data breaks             (O)=*
*=              e.g. start, stop, or other change                            =*
*=  V345   - REAL, exact wavelength in vacuum for data breaks             (O)=*
*=  V830   - REAL, exact wavelength in vacuum for data breaks             (O)=*
*-----------------------------------------------------------------------------*

      IMPLICIT NONE
      INCLUDE 'params'

*  input

      INTEGER nw, iw
      REAL wl(kw)

** internal

      INTEGER kdata
      PARAMETER (kdata = 70000)

      INTEGER n1, n2, n3, n4
      REAL x1(kdata), x2(kdata), x3(kdata), x4(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata), y4(kdata)

      INTEGER i
      INTEGER ierr

* used for air-to-vacuum wavelength conversion

      REAL refrac, ri(kdata)
      EXTERNAL refrac

* output:

      REAL rei218(kw), rei228(kw), rei243(kw), rei295(kw)
      REAL v195, v345, v830

* data from the Reims group:
*=  For Hartley and Huggins bands, use temperature-dependent values from     =*
*=  Malicet et al., J. Atmos. Chem.  v.21, pp.263-273, 1995.                 =*
*=  over 345.01 - 830.00, use values from Brion, room temperature only

      OPEN(UNIT=kin,FILE='DATAE1/O3/1995Malicet_O3.txt',STATUS='old')
      DO i = 1, 2
         READ(kin,*)
      ENDDO
      n1 = 15001
      n2 = 15001
      n3 = 15001
      n4 = 15001
      DO i = 1, n1
         READ(kin,*) x1(i), y1(i), y2(i), y3(i), y4(i)
         x2(i) = x1(i)
         x3(i) = x1(i)
         x4(i) = x1(i)
      ENDDO
      CLOSE (kin)

*=  over 345.01 - 830.00, use values from Brion, room temperature only
* skip datum at 345.00 because already read in from 1995Malicet

      OPEN(UNIT=kin,FILE='DATAE1/O3/1998Brion_295.txt',STATUS='old')
      DO i = 1, 15
         READ(kin,*)
      ENDDO
      DO i = 1, 48515-15
         n1 = n1 + 1
         READ(kin,*) x1(n1), y1(n1)
      ENDDO
      CLOSE (kin)

      DO i = 1, n1
         ri(i) = refrac(x1(i), 2.45E19)
      ENDDO
      DO i = 1, n1
         x1(i) = x1(i) * ri(i)
      ENDDO

      CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,               0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,            1.e+38,0.)
      CALL inter2(nw,wl,rei295,n1,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, 'O3 xsect - Reims 295K'
         STOP
      ENDIF

      DO i = 1, n2
         ri(i) = refrac(x2(i), 2.45E19)
      ENDDO
      DO i = 1, n2
         x2(i) = x2(i) * ri(i)
         x3(i) = x2(i)
         x4(i) = x2(i)
      ENDDO

      CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,               0.,0.)
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,            1.e+38,0.)
      CALL inter2(nw,wl,rei243,n2,x2,y2,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, 'O3 xsect - Reims 243K'
         STOP
      ENDIF

      CALL addpnt(x3,y3,kdata,n3,x3(1)*(1.-deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,               0.,0.)
      CALL addpnt(x3,y3,kdata,n3,x3(n3)*(1.+deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,            1.e+38,0.)
      CALL inter2(nw,wl,rei228,n3,x3,y3,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, 'O3 xsect - Reims 228K'
         STOP
      ENDIF

      CALL addpnt(x4,y4,kdata,n4,x4(1)*(1.-deltax),0.)
      CALL addpnt(x4,y4,kdata,n4,               0.,0.)
      CALL addpnt(x4,y4,kdata,n4,x4(n4)*(1.+deltax),0.)
      CALL addpnt(x4,y4,kdata,n4,            1.e+38,0.)
      CALL inter2(nw,wl,rei218,n4,x4,y4,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, 'O3 xsect - Reims 218K'
         STOP
      ENDIF

* wavelength breaks must be converted to vacuum:

      v195 = 195.00 * refrac(195.00, 2.45E19)
      v345 = 345.00 * refrac(345.00, 2.45E19)
      v830 = 830.00 * refrac(830.00, 2.45E19)

      RETURN
      END

*=============================================================================*

      SUBROUTINE o3_wmo(nw,wl, wmo203,wmo273, v176,v850)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Read and interpolate the O3 cross section                                =*
*=  data from WMO 85 Ozone Assessment                                        =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WMO203 - REAL, cross section (cm^2) for O3 at 203K                    (O)=*
*=  WMO273 - REAL, cross section (cm^2) for O3 at 273K                    (O)=*
*=  V176   - REAL, exact wavelength in vacuum for data breaks             (O)=*
*=              e.g. start, stop, or other change                            =*
*=  V850   - REAL, exact wavelength in vacuum for data breaks             (O)=*
*-----------------------------------------------------------------------------*

      IMPLICIT NONE
      INCLUDE 'params'

*  input

      INTEGER nw, iw
      REAL wl(kw)

* internal

      INTEGER kdata
      PARAMETER (kdata = 200)

      INTEGER n1, n2
      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), y2(kdata)

      INTEGER i, idum
      REAL a1, a2, dum
      INTEGER ierr

* used for air-to-vacuum wavelength conversion

      REAL refrac, ri(kdata)
      EXTERNAL refrac

* output

      REAL wmo203(kw), wmo273(kw)
      REAL v176, v850

*----------------------------------------------------------
* cross sections from WMO 1985 Ozone Assessment
* from 175.439 to 847.500 nm

      OPEN(UNIT=kin,FILE='DATAE1/wmo85',STATUS='old')
      DO i = 1, 3
         read(kin,*)
      ENDDO
      n1 = 158
      n2 = 158
      DO i = 1, n1
         READ(kin,*) idum, a1, a2, dum, dum, dum, y1(i), y2(i)
         x1(i) = (a1+a2)/2.
         x2(i) = (a1+a2)/2.
      ENDDO
      CLOSE (kin)

* convert wavelengths to vacuum

      DO i = 1, n1
         ri(i) = refrac(x1(i), 2.45E19)
      ENDDO
      DO i = 1, n1
         x1(i) = x1(i) * ri(i)
         x2(i) = x2(i) * ri(i)
      ENDDO

      CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,               0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
      CALL inter2(nw,wl,wmo203,n1,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, 'O3 cross section - WMO - 203K'
         STOP
      ENDIF

      CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,               0.,0.)
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,           1.e+38,0.)
      CALL inter2(nw,wl,wmo273,n2,x2,y2,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, 'O3 cross section - WMO - 273K'
         STOP
      ENDIF

* wavelength breaks must be converted to vacuum:
      
      a1 = (175.438 + 176.991) / 2.
      v176 = a1 * refrac(a1,2.45E19)

      a1 = (847.5 + 852.5) / 2.
      v850 = a1 * refrac(a1, 2.45E19)

      RETURN
      END

*=============================================================================*

      SUBROUTINE o3_jpl(nw,wl, jpl218,jpl295, v186,v825)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Read and interpolate the O3 cross section from JPL 2006                  =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  JPL218 - REAL, cross section (cm^2) for O3 at 218K                    (O)=*
*=  JPL295 - REAL, cross section (cm^2) for O3 at 295K                    (O)=*
*=  V186   - REAL, exact wavelength in vacuum for data breaks             (O)=*
*=              e.g. start, stop, or other change                            =*
*=  V825   - REAL, exact wavelength in vacuum for data breaks             (O)=*
*-----------------------------------------------------------------------------*

      IMPLICIT NONE
      INCLUDE 'params'

*  input

      INTEGER nw, iw
      REAL wl(kw)

* internal

      INTEGER kdata
      PARAMETER (kdata = 200)

      INTEGER n1, n2
      REAL x1(kdata), x2(kdata)
      REAL y1(kdata), y2(kdata)

      INTEGER i
      REAL dum
      INTEGER ierr

* used for air-to-vacuum wavelength conversion

      REAL refrac, ri(kdata)
      EXTERNAL refrac

* output

      REAL jpl295(kw), jpl218(kw)
      REAL v186, v825

***********

      OPEN(UNIT=kin,FILE='DATAE1/O3/2006JPL_O3.txt',STATUS='old')
      DO i = 1, 2
         read(kin,*)
      ENDDO
      n1 = 167
      n2 = 167
      DO i = 1, n1
         READ(kin,*) dum, dum, x1(i), y1(i), y2(i)
         y1(i) = y1(i) * 1.e-20
         y2(i) = y2(i) * 1.e-20
      ENDDO
      CLOSE (kin)

* convert wavelengths to vacuum

      DO i = 1, n1
         ri(i) = refrac(x1(i), 2.45E19)
      ENDDO
      DO i = 1, n1
         x1(i) = x1(i) * ri(i)
         x2(i) = x1(i)
      ENDDO

      CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,               0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,           1.e+38,0.)
      CALL inter2(nw,wl,jpl295,n1,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, 'O3 cross section - WMO - 295K'
         STOP
      ENDIF

      CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,               0.,0.)
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,           1.e+38,0.)
      CALL inter2(nw,wl,jpl218,n2,x2,y2,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, 'O3 cross section - WMO - 218K'
         STOP
      ENDIF

* wavelength breaks must be converted to vacuum:

      v186 = 186.051 * refrac(186.051, 2.45E19)
      v825 = 825.    * refrac(825.   , 2.45E19)


      RETURN
      END


*=============================================================================*

      SUBROUTINE o3_mol(nw,wl, mol226,mol263,mol298, v185,v240,v350)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Read and interpolate the O3 cross section from Molina and Molina 1986    =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  MOL226 - REAL, cross section (cm^2) for O3 at 226 K                   (O)=*
*=  MOL263 - REAL, cross section (cm^2) for O3 at 263 K                   (O)=*
*=  MOL298 - REAL, cross section (cm^2) for O3 at 298 K                   (O)=*
*=  V185   - REAL, exact wavelength in vacuum for data breaks             (O)=*
*=              e.g. start, stop, or other change                            =*
*=  V240   - REAL, exact wavelength in vacuum for data breaks             (O)=*
*=  V350   - REAL, exact wavelength in vacuum for data breaks             (O)=*
*-----------------------------------------------------------------------------*

      IMPLICIT NONE
      INCLUDE 'params'

*  input

      INTEGER nw, iw
      REAL wl(kw)

* internal

      INTEGER i
      INTEGER ierr

      INTEGER kdata
      PARAMETER (kdata = 335)
      INTEGER n1, n2, n3
      REAL x1(kdata), x2(kdata), x3(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata)

* used for air-to-vacuum wavelength conversion

      REAL refrac, ri(kdata)
      EXTERNAL refrac

* output

      REAL mol226(kw), mol263(kw), mol298(kw)
      REAL v185, v240, v350

*----------------------------------------------------------

      OPEN(UNIT=kin,FILE='DATAE1/O3/1986Molina.txt',STATUS='old')
      DO i = 1, 10
         READ(kin,*)
      ENDDO
      n1 = 0
      n2 = 0
      n3 = 0
      DO i = 1, 121-10
         n1 = n1 + 1
         n3 = n3 + 1
         READ(kin,*) x1(n1), y1(n1),  y3(n3)
         x3(n3) = x1(n1)
      ENDDO
      DO i = 1, 341-122
         n1 = n1 + 1
         n2 = n2 + 1
         n3 = n3 + 1
         READ(kin,*) x1(n1), y1(n1), y2(n2), y3(n3)
         x2(n2) = x1(n1)
         x3(n3) = x1(n1)
      ENDDO
      CLOSE (kin)

* convert all wavelengths from air to vacuum

      DO i = 1, n1
         ri(i) = refrac(x1(i), 2.45E19)
      ENDDO
      DO i = 1, n1
         x1(i) = x1(i) * ri(i)
      ENDDO

      DO i = 1, n2
         ri(i) = refrac(x2(i), 2.45E19)
      ENDDO
      DO i = 1, n2
         x2(i) = x2(i) * ri(i)
      ENDDO

      DO i = 1, n3
         ri(i) = refrac(x3(i), 2.45E19)
      ENDDO
      DO i = 1, n3
         x3(i) = x3(i) * ri(i)
      ENDDO

* convert wavelength breaks from air to vacuum

      v185 = 185.  * refrac(185. , 2.45E19)
      v240 = 240.5 * refrac(240.5, 2.45E19)
      v350 = 350.  * refrac(350. , 2.45E19)

* interpolate to working grid

      CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,               0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,            1.e+38,0.)
      CALL inter2(nw,wl,mol226,n1,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, 'O3 xsect - 226K Molina'
         STOP
      ENDIF

      CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,               0.,0.)
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,            1.e+38,0.)
      CALL inter2(nw,wl,mol263,n2,x2,y2,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, 'O3 xsect - 263K Molina'
         STOP
      ENDIF

      CALL addpnt(x3,y3,kdata,n3,x3(1)*(1.-deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,               0.,0.)
      CALL addpnt(x3,y3,kdata,n3,x3(n3)*(1.+deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,            1.e+38,0.)
      CALL inter2(nw,wl,mol298,n3,x3,y3,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, 'O3 xsect - 298K Molina'
         STOP
      ENDIF

      RETURN
      END

*=============================================================================*

      SUBROUTINE o3_bas(nw,wl, c0,c1,c2, vb245,vb342)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Read and interpolate the O3 cross section from Bass 1985                 =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  c0     - REAL, coefficint for polynomial fit to cross section (cm^2)  (O)=*
*=  c1     - REAL, coefficint for polynomial fit to cross section (cm^2)  (O)=*
*=  c2     - REAL, coefficint for polynomial fit to cross section (cm^2)  (O)=*
*=  Vb245   - REAL, exact wavelength in vacuum for data breaks            (O)=*
*=              e.g. start, stop, or other change                            =*
*=  Vb342   - REAL, exact wavelength in vacuum for data breaks            (O)=*
*-----------------------------------------------------------------------------*

      IMPLICIT NONE
      INCLUDE 'params'

* input:

      INTEGER nw, iw
      REAL wl(kw)

* internal:

      INTEGER kdata
      PARAMETER (kdata = 2000)

      INTEGER i
      INTEGER ierr

      INTEGER n1, n2, n3
      REAL x1(kdata), x2(kdata), x3(kdata)
      REAL y1(kdata), y2(kdata), y3(kdata)

* used for air-to-vacuum wavelength conversion

      REAL refrac, ri(kdata)
      EXTERNAL refrac

* output:

      REAL c0(kw), c1(kw), c2(kw)
      REAL vb245, vb342

*******************

      OPEN(UNIT=kin,FILE='DATAE1/O3/1985Bass_O3.txt',STATUS='old')
      DO i = 1, 8
         READ(kin,*)
      ENDDO
      n1 = 1915
      n2 = 1915
      n3 = 1915
      DO i = 1, n1
         READ(kin,*) x1(i), y1(i), y2(i), y3(i)
         y1(i) = 1.e-20 * y1(i)
         y2(i) = 1.e-20 * y2(i)
         y3(i) = 1.e-20 * y3(i)
      ENDDO
      CLOSE (kin)

* convert all wavelengths from air to vacuum

      DO i = 1, n1
         ri(i) = refrac(x1(i), 2.45E19)
      ENDDO
      DO i = 1, n1
         x1(i) = x1(i) * ri(i)
         x2(i) = x1(i)
         x3(i) = x1(i)
      ENDDO

* convert wavelength breaks to vacuum

      vb245 = 245.018 * refrac(245.018, 2.45E19)
      vb342 = 341.981 * refrac(341.981, 2.45E19)

* interpolate to working grid

      CALL addpnt(x1,y1,kdata,n1,x1(1)*(1.-deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,               0.,0.)
      CALL addpnt(x1,y1,kdata,n1,x1(n1)*(1.+deltax),0.)
      CALL addpnt(x1,y1,kdata,n1,            1.e+38,0.)
      CALL inter2(nw,wl,c0,n1,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, 'O3 xsect - c0 Bass'
         STOP
      ENDIF

      CALL addpnt(x2,y2,kdata,n2,x2(1)*(1.-deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,               0.,0.)
      CALL addpnt(x2,y2,kdata,n2,x2(n2)*(1.+deltax),0.)
      CALL addpnt(x2,y2,kdata,n2,            1.e+38,0.)
      CALL inter2(nw,wl,c1,n2,x2,y2,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, 'O3 xsect - c1 Bass'
         STOP
      ENDIF

      CALL addpnt(x3,y3,kdata,n3,x3(1)*(1.-deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,               0.,0.)
      CALL addpnt(x3,y3,kdata,n3,x3(n3)*(1.+deltax),0.)
      CALL addpnt(x3,y3,kdata,n3,            1.e+38,0.)
      CALL inter2(nw,wl,c2,n3,x3,y3,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, 'O3 xsect - c2 Bass'
         STOP
      ENDIF

      RETURN
      END


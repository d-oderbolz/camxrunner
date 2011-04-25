      SUBROUTINE seto2(nz,z,nw,wl,cz,zen,dto2,xso2)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Compute equivalent optical depths for O2 absorption, including absorption=*
*=  in SR bands and the Lyman-alpha line.                                    =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NZ      - INTEGER, number of specified altitude levels in the working (I)=*
*=            grid                                                           =*
*=  Z       - REAL, specified altitude working grid (km)                  (I)=*
*=  NW      - INTEGER, number of specified intervals + 1 in working       (I)=*
*=            wavelength grid                                                =*
*=  WL      - REAL, vector of lower limits of wavelength intervals in     (I)=*
*=            working wavelength grid                                        =*
*=  CZ      - REAL, number of air molecules per cm^2 at each specified    (I)=*
*=            altitude layer                                                 =*
*=  ZEN     - REAL, solar zenith angle                                    (I)=*
*=  DTO2    - REAL, optical depth due to O2 absorption at each specified  (O)=*
*=            vertical layer at each specified wavelength                    =*
*=  XSO2    - REAL, molecular absorption cross section in SR bands at     (O)=*
*=            each specified altitude and wavelength.  Includes Herzberg     =*
*=            continuum.                                                     =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  02/98  Included Lyman-alpha parameterization                             =*
*=  03/97  Fix dto2 problem at top level (nz)                                =*
*=  02/97  Changed offset for grid-end interpolation to relative number      =*
*=         (x * (1 +- deltax))                                               =*
*=  08/96  Modified for early exit, no redundant read of data and smaller    =*
*=         internal grid if possible;  internal grid uses user grid points   =*
*=         whenever possible                                                 =*
*=  07/96  Modified to work on internal grid and interpolate final values    =*
*=         onto the user-defined grid                                        =*
*-----------------------------------------------------------------------------*
*= This program is free software;  you can redistribute it and/or modify     =*
*= it under the terms of the GNU General Public License as published by the  =*
*= Free Software Foundation;  either version 2 of the license, or (at your   =*
*= option) any later version.                                                =*
*= The TUV package is distributed in the hope that it will be useful, but    =*
*= WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTIBI-  =*
*= LITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public     =*
*= License for more details.                                                 =*
*= To obtain a copy of the GNU General Public License, write to:             =*
*= Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.   =*
*-----------------------------------------------------------------------------*
*= To contact the authors, please mail to:                                   =*
*= Sasha Madronich, NCAR/ACD, P.O.Box 3000, Boulder, CO, 80307-3000, USA  or =*
*= send email to:  sasha@ucar.edu                                            =*
*-----------------------------------------------------------------------------*
*= Copyright (C) 1994,95,96  University Corporation for Atmospheric Research =*
*-----------------------------------------------------------------------------*

      IMPLICIT NONE
      INCLUDE 'params'

      REAL wl(kw)
      REAL z(kz), cz(kz)
      INTEGER nz, nw
      REAL zen
      REAL dto2(kz,kw), xso2(kz,kw)

* grid on which Kockarts' parameterization is defined
      INTEGER ngast
      PARAMETER (ngast = 17)
      REAL wlgast(ngast)
      SAVE wlgast
 
* O2 optical depth and equivalent cross section on Kockarts' grid
      REAL dto2k(kz,ngast-1), xso2k(kz,ngast-1)

* Lyman-alpha variables
      INTEGER nla
      PARAMETER (nla = 2)
      REAL wlla(nla)

* O2 optical depth and equivalent cross section in the Lyman-alpha region
      REAL dto2la(kz,nla-1), xso2la(kz,nla-1)

* internal grid and O2 cross section on internal grid
      INTEGER kdata
      PARAMETER (kdata = 200)
      REAL wlint(kdata), xso2int(kdata)
      SAVE wlint, xso2int
      INTEGER nwint
      SAVE nwint

* temporary one-dimensional storage for optical depth and cross section values
* XXtmp  - on internal grid
* XXuser - on user defined grid
      REAL dttmp(2*kw), xstmp(2*kw)
      REAL dtuser(kw), xsuser(kw)

      REAL o2col(kz)

      REAL secchi
      REAL fchap
      EXTERNAL fchap

* cross section data for use outside the SR-Bands (combined from
* Brasseur and Solomon and the JPL 1994 recommendation)
      INTEGER nosr
      PARAMETER (nosr = 105)
      REAL x1(nosr), y1(nosr)

* auxiliaries
      REAL x, y
      REAL dr
      PARAMETER (dr = pi/180.)
      REAL delO2
      INTEGER i, iw, igast, ierr, icount
      INTEGER iz
      INTEGER ifirst, n

      LOGICAL call1
      SAVE call1, icount
      DATA call1/.TRUE./

*-------------------------------------------------------------------------------


* check, whether user grid is in the O2 absorption band at all...
* if not, set cross section and optical depth values to zero and return

      IF (wl(1) .GT. 243.) THEN
         DO iw = 1, nw-1
           DO i = 1, nz
             dto2(i,iw) = 0.
             xso2(i,iw) = 0.
           ENDDO
         ENDDO
         RETURN
      ENDIF

* sec Xhi or Chapman calculation
      IF (zen .LE. 75.) THEN
         secchi = 1./COS(zen*dr)
      ELSEIF (zen .LE. 95. ) THEN
         secchi = fchap(zen)
      ELSE
         RETURN
      ENDIF

* O2 overhead columns calculation
      o2col(nz-1) = 0.2095 * cz(nz-1) * secchi
      DO i = nz-2, 1, -1
        o2col(i) = o2col(i+1) + 0.2095*cz(i)*secchi
      END DO

* read O2 cross section data outside SR-bands only in the very first call
      IF (call1) THEN
************* O2 absorption cross sections:
* from 116 nm to 245 nm, including Schumann-Runge continumm
* from Brasseur and Solomon 1986.

        OPEN(UNIT=kin,FILE='DATAE1/O2/O2_src.abs')
        READ(kin,*) ifirst, n
        DO i = 1, ifirst-2
          READ(kin,*)
        ENDDO
        icount = 0
        DO i = 1, n
          READ(kin,*) x, y
          IF (x .LT. 204.) THEN
            icount = icount+1
            x1(icount) = x
            y1(icount) = y
          ENDIF
        ENDDO
        CLOSE(kin)

* overwrite from 204 to 241 nm (Herzberg continuum)

        OPEN(UNIT=kin,FILE='DATAE1/O2/O2_jpl94.abs',STATUS='old')
        read(kin,*)
        n = 40
        DO i = 1, n
           icount = icount+1
           READ(kin,*) y
           y1(icount) = y*1E-24
           x1(icount) = 204. + FLOAT(i-1)
        END DO
        CLOSE (kin)

* set values to zero outside the wavelength range defined by the data files

        CALL addpnt(x1,y1,nosr,icount,     x1(1)-deltax,0.)
        CALL addpnt(x1,y1,nosr,icount,               0.,0.)
        CALL addpnt(x1,y1,nosr,icount,x1(icount)+deltax,0.)
        CALL addpnt(x1,y1,nosr,icount,            1.e38,0.)

* set up the internal grid, use full resolution of the cross section data
* outside the SR bands, use Kockarts' grid inside the SR bands

* define Kockarts' grid points
        OPEN(kin,FILE='DATAE1/GRIDS/kockarts.grid',STATUS='old')
        DO iw = 1, ngast-1
           READ(kin,*) wlgast(iw),wlgast(iw+1) 
        ENDDO
        CLOSE(kin)

* define the Lyman-Alpha grid
        wlla(1) = 121.4
        wlla(2) = 121.9

* put together the internal grid by "pasting" the Lyman-Alpha grid and 
* Kockarts' grid into the combination of Brasseur/Solomon and JPL grid
        nwint = 0
        DO iw = 1, 9
           nwint = nwint+1
           wlint(nwint) = x1(iw)
        ENDDO
        DO iw = 1, 2
           nwint = nwint+1
           wlint(nwint) = wlla(iw)
        ENDDO
        DO iw = 12, 47
           nwint = nwint + 1
           wlint(nwint) = x1(iw)
        ENDDO
        DO iw = 1, ngast
           nwint = nwint+1
           wlint(nwint) = wlgast(iw)
        ENDDO
        DO iw = 65, 105
           nwint = nwint+1
           wlint(nwint) = x1(iw)
        ENDDO


* interpolate Brasseur/Solomon and JPL data onto internal grid
        CALL inter2(nwint,wlint,xso2int, icount,x1,y1, ierr)

        IF (call1) call1 = .FALSE.

      ENDIF

* if necessary:
* do Kockarts' parameterization of the SR bands, output values of O2
* optical depth and O2 equivalent cross section are on his grid
      IF ((wl(1) .LT. wlgast(ngast)) .AND. 
     >    (wl(nw) .GT. wlgast(1))) THEN
        DO iw = 1, ngast-1
           CALL schu(nz,o2col,iw,secchi,dto2k,xso2k)
        ENDDO
      ENDIF

* if necessary: 
* do Lyman-Alpha parameterization, output values of O2 opticaldepth
* and O2 effective (equivalent) cross section
      IF ((wl(1) .LE. wlla(nla)) .AND. (wl(nw) .GE. wlla(1))) THEN
         CALL lymana(nz,o2col,secchi,dto2la,xso2la)
      ENDIF

* loop through the altitude levels 
      DO iz = 1, nz

         igast = 0
         delO2 = 0.2095 * cz(iz)    ! vertical O2 column

* loop through the internal wavelength grid
         DO iw = 1, nwint-1

* if outside Kockarts' grid and outside Lyman-Alpha, use the 
* JPL/Brasseur+Solomon data, if inside
* Kockarts' grid, use the parameterized values from the call to SCHU,
* if inside Lyman-Alpha, use the paraemterized values from call to LYMANA
           IF ((wlint(iw+1) .LE. wlgast(1)) .OR.
     >         (wlint(iw) .GE. wlgast(ngast))) THEN
             IF ((wlint(iw+1) .LE. wlla(1)) .OR.
     >           (wlint(iw) .GE. wlla(nla))) THEN
                IF (iz .EQ. nz) THEN
                  dttmp(iw) = 0.
                ELSE
                  dttmp(iw) = xso2int(iw) * delO2
                ENDIF
                xstmp(iw) = xso2int(iw)
             ELSE
                dttmp(iw) = dto2la(iz,1)
                xstmp(iw) = xso2la(iz,1)
             ENDIF
           ELSE
              igast = igast+1
              dttmp(iw) = dto2k(iz,igast)
              xstmp(iw) = xso2k(iz,igast)
           ENDIF

* compute the area in each bin (for correct interpolation purposes only!)
           dttmp(iw) = dttmp(iw) * (wlint(iw+1)-wlint(iw))
           xstmp(iw) = xstmp(iw) * (wlint(iw+1)-wlint(iw))

         ENDDO

* interpolate O2 optical depth from the internal grid onto the user grid
         CALL inter3(nw,wl,dtuser, nwint,wlint,dttmp, 0)
         DO iw = 1, nw-1
            dto2(iz,iw) = dtuser(iw)/(wl(iw+1)-wl(iw))
         ENDDO
      
* interpolate O2 cross section from the internal grid onto the user grid
         CALL inter3(nw,wl,xsuser, nwint,wlint,xstmp, 0)

         DO iw = 1, nw-1
            xso2(iz,iw) = xsuser(iw)/(wl(iw+1)-wl(iw))
         ENDDO

      ENDDO

      RETURN
      END

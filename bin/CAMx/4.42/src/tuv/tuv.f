      PROGRAM tuv
cgy
c Modified to write a CAMx format file
c Greg Yarwood, 6/4/99, 1/7/05 - gyarwood@envion.org
*_______________________________________________________________________
*     Tropospheric Ultraviolet-Visible (TUV) radiation model
*     version 4
*     Sept 98 by Madronich et al.
*_______________________________________________________________________
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

* include parameter file

      INCLUDE 'params'

* ___ SECTION 1: VARIABLES AND PARAMETERS ______________________________

* altitude grid

      INTEGER nz, iz
      REAL z(kz)

* solar zenith angle and azimuth

      REAL zen, azim

* slant path lengths in spherical geometry

      INTEGER nid(0:kz)
      REAL dsdh(0:kz,kz)

* wavelength grid:

      INTEGER nw, iw
      REAL wl(kw), wc(kw), wu(kw)

* extra terrestrial solar flux and earth-Sun distance ^-2

      REAL f(kw), etf(kw)
      REAL esfact

* ozone absorption cross section and ozone optical depth:

      REAL xso3(kw), s226(kw), s263(kw), s298(kw)

* O2 absorption cross section

      REAL xso2(kz,kw)

* SO2 absorption cross section
     
      REAL xsso2(kw)

* NO2 absorption cross section
     
      REAL xsno2(kw)

* atmospheric optical parameters:

      REAL tlev(kz), tlay(kz)
      REAL airlev(kz), colinc(kz)
      REAL dtrl(kz,kw)
      REAL dto3(kz,kw), dto2(kz,kw), dtso2(kz,kw), dtno2(kz,kw)
      REAL dtcld(kz,kw), omcld(kz,kw), gcld(kz,kw)
      REAL dtaer(kz,kw), omaer(kz,kw), gaer(kz,kw)
      REAL albedo(kw)

* spectral irradiance and actinic flux (scalar irradiance):

      REAL edir(kz), edn(kz), eup(kz)
      REAL fdir(kz), fdn(kz), fup(kz)

* spectral weighting functions and weighted radiation:

      INTEGER ns, is
      REAL sw(ks,kw), rate(ks), dose(ks)
      REAL sirrad, sprate 
      CHARACTER*40 label(ks)

*! j-values:

      INTEGER nj, ij
      REAL sj(kj,kz,kw), valj(kj,kz)
      REAL saflux, deltaj
      CHARACTER*40 jlabel(kj)

* new sea level pressure, surface dobson, etc.

      REAL pmbnew, dobnew, so2new, no2new

* Location and time

      REAL alat, along 
      INTEGER idate
      REAL dtime, ut, ut0

* Commonly used looping indices

      integer i, j
      integer  idat, idob, itime, izen

* Other user-defined variables here:


      INTEGER mabs
      REAL o3xs(kz,kw)
      integer nzm1


cgy camx variables

       integer  mxozn, mxalb, mxhaze, mxhgt, nzen, mxrxn
       parameter (mxozn=5,mxalb=5,mxhaze=3,nzen=10,mxhgt=40,mxrxn=40)
       real   prkn(nzen,mxrxn,mxhgt,mxhaze,mxalb,mxozn)
       real   camxozn(mxozn), camxhaze(mxhaze) 
       real   camxalb(mxalb), camxzen(nzen)
       real   camxhgt(mxhgt)
       real   albnew, hazenew
       integer  ijpoint(mxrxn)
       integer  nozn, nalb, nhaze, nhgt
       integer  iozn, ialb, ihaze, ihgt, irxn, iradtran, nrxn
       character*200 record, ifile
       character*20  zlabel
       character*12  oznlab
       character*8   alblab
       character*11  hazelab
       logical  ok
       data   camxzen /0., 10., 20., 30., 40., 50., 60., 70., 78., 86./
       data   zlabel  /"    km above surface"/
       data   oznlab  /" O3 Column ="/
       data   alblab  /" Albedo="/
       data   hazelab /" Haze O.D.="/ 
 
c**ENTRY POINT
 
       open (53, file ="tuv.out", status="unknown")

cgy input parameters for camx file
 
      write(*,*) 'TUV version 4.0 from Sasha Madronich at NCAR'
      write(*,*)
      write(*,*) 'I/O adapted to generate CAMx format photolysis'
      write(*,*) 'rate files by Greg Yarwood'
      write(*,*)
      write(*,*) 'The parameters for this run are:'
      write(*,*)

c open the user input file for reading

      open (10, file ="tuv.inp", status="old")
      read (10, '(20x,a180)') record
      open (20, file = record, status="unknown")
      write(*,*) 'Opened output file: '
      write(*,*) record

C Modified by Steven Lau 03/05/2003
C Read the Ozone, Albedo and Haze value directly from Ahomap output

      read (10, '(20x,a200)') ifile ! Read in the ahomap file
      open(unit=15,file=ifile,status='old')
      write(*,*) 
      write(*,*) 'Opened the CAMx AHOMAP file: '
      write(*,*) ifile
      read(10,'(20x,a200)') ifile
      read(ifile,*) nozn, nalb, nhaze ! No. of Ozone, Abledo, Haze

      if (nalb.gt.mxalb) then
        write(*,*) 'Requested # of albedos exceeds MXALB', mxalb
        stop
      endif
      if (nozn.gt.mxozn) then
        write(*,*) 'Requested # of ozone columns exceeds MXOZN', mxozn
        stop
      endif
      if (nhaze.gt.mxhaze) then
        write(*,*) 'Requested # of hazes exceeds MXHAZE', mxhaze
        stop
      endif
 
      read(15,*) ifile ! Read the header of the ahomap
      read(15,1501) ifile
      read(ifile,*) (camxalb(i),i=1,nalb)
      read(15,1501) ifile
      read(ifile,*) (camxhaze(i),i=1,nhaze)
      read(15,1501) ifile
      read(ifile,*) (camxozn(i),i=1,nozn)
1501  format(15x,a200)
      write (*,'(a,99f6.3)') '  Ozone Column  Values: ', 
     &                               (camxozn(i),i=1,nozn)
      write (*,'(a,99f6.3)') '  UV Albedo  Values:    ', 
     &                               (camxalb(i),i=1,nalb)
      write (*,'(a,99f6.3)') '  Haze OD Values:       ', 
     &                               (camxhaze(i),i=1,nhaze)
      write(*,*) 
      close(unit=15)
 
c get the requested CAMx height levels

      read (10, '(20x,a180)') record
      read (record, *) nhgt
      write (*,*) 'Number of CAMx vertical levels to be specified = ',
     &               nhgt
      if (nhgt.gt.mxhgt) then
        write(*,*) 'Requested # of levels exceeds MXHGT', mxhgt
        stop
      endif

      read (10, '(20x,a180)') record
      read (record, *) (camxhgt(ihgt),ihgt=1,nhgt)
      write(*,*) 'CAMx vertical levels specified: ' 
      do ihgt=1,nhgt
        write(*,*) 'Level ', ihgt, ' at', camxhgt(ihgt), ' km'
      enddo
      if (camxhgt(1).ne.0.0) then
        write(*,*) 'Error: The first level must be zero (ground level)'
        stop
      endif
      if (camxhgt(nhgt).gt.20.0) then
        write(*,*) 'Error: The top level is above 20 km which is not',
     &                ' reasonable for CAMx'
        stop
      endif
      call gridck(mxhgt,nhgt,camxhgt,ok)
      if (.not. ok) then
         write(*,*)'Error: These levels do not make sense'
         stop
      endif

c get the date to calculate the earth-sun distance

      read (10, '(20x,a180)') record
      read (record, *) idate
      write (*,*) 'Date: ', idate
      if (idate.lt.1.or.idate.gt.991231) then
        write(*,*) 'Date must be between 000001 and 991231'
        write(*,*) 'Enter year 2000 as 00, etc.'
        stop
      endif
 
c get the radiative transfer scheme option

      read (10, '(20x,a180)') record
      read (record, *) iradtran
      write (*,*) 'Radiative transfer scheme # ', iradtran
      if (iradtran.ne.1.and.iradtran.ne.2) then
        write(*,*) 'Must select scheme number 1 or 2'
        stop
      elseif (iradtran.eq.1) then
        write(*,*) 
     &    '  - pseudo-spherical two-stream delta-Eddington (ps2str.f)'
      elseif (iradtran.eq.2) then
        write(*,*) 
     &    '  - discrete ordinates method (psndo.f)'
      endif
 
c get the photolysis reactions to output

      read (10, '(20x,a180)') record
      read (record, *) nrxn
      write (*,*) 'Number of phot reactions: ', nrxn
      if (nrxn.lt.1) then
        write(*,*) 'Must have at least 1 reaction'
        stop
      elseif (nrxn.gt.mxrxn) then
        write(*,*) 'Requested # of reactions exceeds MXRXN', mxrxn
        stop
      endif
      read (10, '(20x,a180)') record
      read (record, *) (ijpoint(i),i=1,nrxn)
      write(*,*) 'Reaction numbers selected : ', 
     &           (ijpoint(i),i=1,nrxn)
      do i=1,nrxn
        if (ijpoint(i).gt.kj.or.ijpoint(i).lt.1) then
          write(*,*) 'Reaction specified: ', ijpoint(i)
          write(*,*) '    is outside the valid range of 1 to ', kj
          stop
        endif
      enddo

c done reading user input

      close(10)
      write(*,*)
      write(*,*) 'Successfully read input parameters'
      write(*,*)
cgy

***


* ___ SECTION 2: SET GRIDS _________________________________________________

* wavelengths

      CALL gridw(nw,wl,wc,wu)
      
* altitudes (in gridz can set the surface elevation, z(1), in km).

      CALL gridz(nz,z)

* ___ SECTION 3: SPECTRAL DATA ____________________________

* read (and grid) extra terrestrial flux data:
      
      CALL rdetfl(nw,wl,f)

* read cross section data for 
*    ozone (temperature-dependent)
*    SO2 
*    NO2

      CALL rdso2xs(nw,wl,xsso2)
      CALL rdno2xs(nw,wl,xsno2)

* ___ SECTION 4: SET MODEL ATMOSPHERE __________________________________

* temperature profile

      CALL settmp(nz,z,
     $     tlev,tlay)

*  air profile and Rayleigh optical depths

      pmbnew = -999.
      CALL setair(pmbnew,
     $     nz,z,nw,wl,
     $     airlev,dtrl,colinc)

* Photo-chemical and photo-biological weigting functions. 
* For pchem, need to know temperature and pressure profiles.
* Output:
* from pbiol:  s(ks,kw) - for each weigting function label(ks)
* from pchem:  sj(kj,kz,kw) - for each reaction jlabel(kj)

      is = 0
      CALL pbiol1(nw,wl,wc,is,sw,label)
      ns = is

      CALL pchem(nw,wl,nz,tlev,airlev,
     $     nj,sj,jlabel)

cgy
      write(kout,*) 
     &   ' Photo-biological weigting functions in the database are'
      do i = 1, ns
         write(kout,99) i, label(i)
      enddo
      write(kout,*) 
      write(kout,*) ' Photolysis reactions in the database are'
      do i = 1, nj
         write(kout,99) i, jlabel(i)
      enddo
      write(kout,*) 
     &  ' The following reactions will be written to the CAMx file'
      write(*,*) 
     &  ' The following reactions will be written to the CAMx file'
      do i = 1, nrxn
         ij=ijpoint(i)
         write(kout,99) ij, jlabel(ij)
         write(*,99) ij, jlabel(ij)
      enddo
      write(*,*) 

* ozone optical depths (must give temperature)

*sm
c      CALL rdo3xs(nw,wl,xso3,s226,s263,s298)

      mabs = 1
      nzm1 = nz-1
      call rdo3xs(mabs,nzm1,tlay,nw,wl, o3xs)


cgy begin CAMx ozone column loop

      DO 40 iozn = 1, nozn
         dobnew = camxozn(iozn) * 1000.0

cgy      dobnew = 300.

      CALL setozo(dobnew,
     $     nz,z,nw,wl,o3xs,
     $     dto3)



* SO2 optical depth (also has temperature correction)
* so2new = new column SO2, in Dobson Units

cgy      so2new =  0.
      so2new =  0.0000001
      CALL setso2(so2new,
     $     nz,z,nw,wl,
     $     xsso2, tlay,
     $     dtso2)

* NO2 optical depth (also has temperature correction)
* no2new = new column NO2, in Dobson Units

cgy      no2new =  0.
      no2new =  0.0000001
      CALL setno2(no2new,
     $     nz,z,nw,wl,
     $     xsno2, tlay,
     $     dtno2)

*  cloud and aerosol optical depths:

      CALL setcld(nz,z,nw,wl,
     $     dtcld,omcld,gcld)

cgy begin CAMx aerosol loop

      DO 50 ihaze = 1, nhaze

cgy hazenew now contains scaling factor for aerosol optical depths
        hazenew = camxhaze(ihaze)/0.38

cgy pass hazenew ro setaer
      CALL setaer(nz,z,nw,wl,
     $     dtaer,omaer,gaer,hazenew)
cgy     $     dtaer,omaer,gaer)

* surface albedo:

cgy begin CAMx aerosol loop

      DO 60 ialb = 1, nalb

cgy set albedo here
        do iw = 1, nw - 1
          albedo(iw) = camxalb(ialb)
        enddo
cgy     CALL setalb(nw,wl,albedo)

* ___ SECTION 5: TIME AND LOCATION _____________________________________

* specify date and compute earth-sun distance correction

cgy idate was read in above
cgy      idate = 940321

      CALL sundis(idate,esfact)
      WRITE(kout,*) 'idate = ', idate,' esfact = ', esfact
      do iw = 1, nw-1
         etf(iw) = f(iw) * esfact
      enddo

* specify latitude and longitude

      alat = 0.
      along = 0.
cgy      WRITE(kout,*)'lat = ',alat,' long = ',along

* below, can  chose between specific time (solar zenith angle is calculated) 
* or can set zenith angle to arbitrary value(s).  Loop DO 20 allows calculation 
* at multiple solar zenith angles  (or multiple times).

* Set starting time (ut = Universal Time, hrs.), and 
* time increment (dtime, in seconds)

C      ut0 = 0.
C      dtime = 3600./4.

* initalize time-integrated quantities

C      call zero1(dose,ks)

* Loop over time (alternatively, can loop over solar zenith angle)

C      DO 20, itime = 1, 96
C         ut = ut0 + (dtime/3600.) * FLOAT(itime-1)

* set time to local noon for this test case

cgy       ut = 12.

* solar zenith angle calculation:

cgy       CALL zenith(alat,along,idate,ut,azim,zen)
cgy       WRITE(kout,*) 'ut = ', ut, 'azimuth = ', azim, ' zen = ', zen

C         zen = 0.
cgy camx loop over zenith angle
       DO 20 izen = 1, nzen
         zen = camxzen(izen)

* ____ SECTION 6: CALCULATE ZENITH-ANGLE DEPENDENT QUANTITIES __________

* slant path lengths for spherical geometry

       CALL sphers(nz, z, zen, dsdh, nid)

* effective O2 optical depth (SR bands, must know zenith angle!)
* reassign O2 cross section to sj(1,*,*)

       CALL zero2(dto2,kz,kw)
       CALL zero2(xso2,kz,kw)
       CALL seto2(nz,z,nw,wl,colinc,zen,dto2,xso2)
       CALL sjo2(nz,nw,xso2,1,sj)

* ____ SECTION 7: WAVELENGTH LOOP ______________________________________

* initialize for wavelength integration

       call zero1(rate,ks)
       call zero2(valj,kj,kz)

** Main wavelength loop:

       DO 10, iw = 1, nw-1

** monochromatic radiative transfer:

cgy camx switch to select scheme
       if(iradtran.eq.2) then
         CALL rtlnkdo(nz,z,
     $        iw, albedo(iw), zen,
     $        dsdh,nid,
     $        dtrl,
     $        dto3,
     $        dto2,
     $        dtso2,
     $        dtno2,
     $        dtcld, omcld, gcld,
     $        dtaer,omaer,gaer,
     $        edir, edn, eup, fdir, fdn, fup)
       else
         CALL rtlnk2s(nz,z,
     $        iw, albedo(iw), zen,
     $        dsdh,nid,
     $        dtrl,
     $        dto3,
     $        dto2,
     $        dtso2,
     $        dtno2,
     $        dtcld, omcld, gcld,
     $        dtaer,omaer,gaer,
     $        edir, edn, eup, fdir, fdn, fup)
       endif

** surface irradiance and weighted radiation

         iz = 1 
         sirrad = etf(iw) * (edir(iz) + edn(iz))

         DO 15, is = 1, ns
            sprate = sirrad * sw(is,iw) 
            rate(is) = rate(is) + sprate * (wu(iw) - wl(iw))
 15      CONTINUE

** spherical irradiance (actinic flux)
* as a function of altitude
* convert to quanta s-1 nm-1 cm-2
* ( 1.e-4 * (wc*1e-9) / (hc = 6.62E-34 * 2.998E8) )

         DO 17 iz = 1, nz
            saflux = etf(iw)* 5.039e11 * wc(iw) *
     $           (fdir(iz) + fdn(iz) + fup(iz))

            DO 16, ij = 1, nj
               deltaj = saflux * sj(ij,iz,iw)
               valj(ij,iz) = valj(ij,iz) + deltaj * (wu(iw) - wl(iw))
 16         CONTINUE
 17      CONTINUE

 10   CONTINUE

** some examples of output:

* dose rates weighted by specific action spectra:

cgy      DO 35, is = 1, ns
cgy         WRITE(kout,99) is, label(is), rate(is)
cgy 35   CONTINUE

* photolysis rate coefficients (j-values) at surface

      iz = 1
cgy      DO 36, ij = 1, nj
cgy         WRITE(kout,99) ij, jlabel(ij), valj(ij,iz)
cgy 36   CONTINUE

 99   FORMAT(I4,1X,A40,1X,1PE10.3)

cgy fill the camx photolysis rate array

      DO ihgt=1,nhgt
         DO iz = 2, nz
            IF (z(iz).GT. camxhgt(ihgt) 
     &            .AND. z(iz-1).LE. camxhgt(ihgt)) THEN
               DO irxn = 1, nrxn
                  prkn(izen,irxn,ihgt,ihaze,ialb,iozn) =
     &                ( valj(ijpoint(irxn),iz-1)*60.*
     &                   (camxhgt(ihgt)-z(iz-1))/(z(iz)-z(iz-1)) ) +
     &                     ( valj(ijpoint(irxn),iz-1)*60.*
     &                        (z(iz)-camxhgt(ihgt))/(z(iz)-z(iz-1)) )
               ENDDO
            ENDIF
         ENDDO
      ENDDO

cgy end zenith loop
 20   continue

      write(*,*) ' Completed calculation',
     &          ((iozn-1)*15)+((ihaze-1)*5)+ialb,
     &          ' of ', nozn*nhaze*nalb

cgy end albedo loop
 60   continue

cgy end aerosol (haze) loop
 50   continue

cgy end ozone column loop
 40   continue

cgy write camx file
 
      do iozn = 1,nozn
        do ialb = 1,nalb
          do ihaze = 1,nhaze
            write(20,'(a12,f7.3,a8,f7.3,a11,f7.3)') 
     &          oznlab,camxozn(iozn),alblab,camxalb(ialb),
     &          hazelab,camxhaze(ihaze)
            do ihgt = 1,nhgt
              write(20,'(f7.3,a20)') camxhgt(ihgt), zlabel
              do irxn = 1,nrxn
                write(20,'(10(1PE12.3))') 
     &              (prkn(izen,irxn,ihgt,ihaze,ialb,iozn),
     &              izen=1,nzen)
              enddo
            enddo
          enddo
        enddo
      enddo


*_______________________________________________________________________

      END

      SUBROUTINE pbiol1(nw,wl,wc,j,s,label)

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Create or read various weighting functions, e.g. biological action       =*
*=  spectra, instrument responses etc.                                       =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  WC     - REAL, vector of central wavelength of wavelength intervals    I)=*
*=           in working wavelength grid                                      =*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  S      - REAL, value of each defined weighting function at each       (O)=*
*=           defined wavelength                                              =*
*=  LABEL  - CHARACTER*40, string identifier for each weighting function  (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  02/97  Changed offset for grid-end interpolation to relative number      =*
*=         (x * (1 +- deltax))                                               =*
*=  05/96  Renamed from LOADW1 to WSPEC1                                     =*
*=  03/95  Added vis+ and UV index                                           =*
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

      INTEGER kdata
      PARAMETER(kdata=1000)

* input:
      REAL wl(kw), wc(kw)
      INTEGER nw

* input/output:
      INTEGER j

* output: (weighting functions and labels)
      REAL s(ks,kw)
      CHARACTER*40 label(ks)

* internal:
      REAL x1(kdata)
      REAL y1(kdata)
      REAL yg(kw)

      REAL fery, futr
      EXTERNAL fery, futr
      INTEGER i, iw, n

      INTEGER ierr

      INTEGER idum
      REAL dum1, dum2
      REAL em, a, b, c

*_______________________________________________________________________

********* UV-B (280-315 nm)
 
      j = j + 1
      label(j) = 'UV-B, 280-315 nm'
      DO iw = 1, nw-1
         IF (wc(iw) .GT. 280. .AND. wc(iw) .LT. 315.) THEN
            s(j,iw) = 1.
         ELSE
            s(j,iw) = 0.
         ENDIF
      ENDDO

********* UV-B* (280-320 nm)
 
      j = j + 1
      label(j) = 'UV-B*, 280-320 nm'
      DO iw = 1, nw-1
         IF (wc(iw) .GT. 280. .AND. wc(iw) .LT. 320.) THEN
            s(j,iw) = 1.
         ELSE
            s(j,iw) = 0.
         ENDIF
      ENDDO

********* UV-A (315-400 nm)
 
      j = j + 1
      label(j) = 'UV-A, 315-400 nm'
      DO iw = 1, nw-1
         IF (wc(iw) .GT. 315. .AND. wc(iw) .LT. 400.) THEN
            s(j,iw) = 1.
         ELSE
            s(j,iw) = 0.
         ENDIF
      ENDDO

********* visible+ (> 400 nm)
 
      j = j + 1
      label(j) = 'vis+, > 400 nm'
      DO iw = 1, nw-1
         IF (wc(iw) .GT. 400.) THEN
            s(j,iw) = 1.
         ELSE
            s(j,iw) = 0.
         ENDIF
      ENDDO

********** unity raf constant slope:  

      j = j + 1
      label(j) = 'decay, 14 nm/10'
      DO iw = 1, nw-1
         s(j,iw) = 10.**(-(wc(iw) -300.)/14.)
      ENDDO

************ DNA damage action spectrum
* from: Setlow, R. B., The wavelengths in sunlight effective in 
*       producing skin cancer: a theoretical analysis, Proceedings 
*       of the National Academy of Science, 71, 3363 -3366, 1974.
* normalize to unity at 300 nm
* Data read from original hand-drawn plot by Setlow
* received from R. Setlow in May 1995
* data is per quantum (confirmed with R. Setlow in May 1995).  
* Therefore must put on energy basis if irradiance is is energy
* (rather than quanta) units.

      j = j + 1
      label(j) = 'Setlow dna.new'
      OPEN(UNIT=kin,FILE='DATAS1/dna.setlow.new',STATUS='old')
      do i = 1, 11
         read(kin,*)
      enddo
      n = 55
      DO i = 1, n
         READ(kin,*) x1(i), y1(i)
         y1(i) = y1(i) / 2.4E-02  *  x1(i)/300.
      ENDDO
      CLOSE (kin)

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),y1(1))
      CALL addpnt(x1,y1,kdata,n,          0.,y1(1))
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),   0.)
      CALL addpnt(x1,y1,kdata,n,      1.e+38,   0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, label(j)
         STOP
      ENDIF

      DO iw = 1, nw-1
         s(j,iw) = yg(iw)
      ENDDO

********* skin cancer in mice,  Utrecht/Phildelphia study
*from de Gruijl, F. R., H. J. C. M. Sterenborg, P. D. Forbes, 
*     R. E. Davies, C. Cole, G. Kelfkens, H. van Weelden, H. Slaper,
*     and J. C. van der Leun, Wavelength dependence of skin cancer 
*     induction by ultraviolet irradiation of albino hairless mice, 
*     Cancer Res., 53, 53-60, 1993.
* normalize at 300 nm.

      j = j + 1
      label(j) = 'SCUP-m'
      DO iw = 1, nw-1
         s(j,iw) =  futr(wc(iw)) / futr(300.)
      ENDDO
         
*********** Utrecht mice spectrum corrected for humans skin.

      j = j + 1
      label(j) = 'SCUP-h'
      OPEN(UNIT=kin,FILE='DATAS1/SCUP-h',STATUS='old')
      n = 28
      DO i = 1, n
         READ(kin,*) x1(i), y1(i)
      ENDDO

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),y1(1))
      CALL addpnt(x1,y1,kdata,n,          0.,y1(1))
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),   0.)
      CALL addpnt(x1,y1,kdata,n,      1.e+38,   0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, label(j)
         STOP
      ENDIF
            
      DO iw = 1, nw-1
         s(j,iw) = yg(iw)
      ENDDO
      CLOSE (kin)
      
***************** compute ery action spectrum
*from
* McKinlay, A. F., and B. L. Diffey, A reference action spectrum for 
* ultraviolet induced erythema in human skin, in Human Exposure to 
* Ultraviolet Radiation: Risks and Regulations, W. R. Passchler 
* and B. F. M. Bosnajokovic, (eds.), Elsevier, Amsterdam, 1987.

      j = j + 1
      label(j) = 'CIE hum erythema'
      DO iw = 1, nw-1
         s(j,iw) = fery(wc(iw))
      ENDDO

***************** UV index (Canadian - WMO/WHO)
* based on erythema

      j = j + 1
      label(j) = 'UV index'
      DO iw = 1, nw-1
         s(j,iw) = 40. * fery(wc(iw))
      ENDDO

************* erythema - Anders et al.
* for skin types II and III, from Anders et al., Photochem. and
* Photobiol., 61, 200-203, 1995. Units are J m-2.

      j = j + 1
      label(j) = 'ery.anders'
      OPEN(UNIT=kin,FILE='DATAS1/ery.anders',STATUS='old')
      do i = 1, 3
         read(kin,*)
      enddo
      n = 28
      DO i = 1, n
         READ(kin,*) x1(i), y1(i)
         y1(i) = 1./y1(i)
      ENDDO

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),y1(1))
      CALL addpnt(x1,y1,kdata,n,          0.,y1(1))
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),   0.)
      CALL addpnt(x1,y1,kdata,n,      1.e+38,   0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, label(j)
         STOP
      ENDIF
            
      DO iw = 1, nw-1
         s(j,iw) = yg(iw)
      ENDDO
      CLOSE (kin)

********* read 1991-92 acgih threshold limit values
* from
* ACGIH, 1991-1992 Threshold Limit Values, American Conference 
*  of Governmental and Industrial Hygienists, 1992.

      j = j + 1
      label(j) = 'ACGIH 1992 TLVs'
      OPEN(UNIT=kin,FILE='DATAS1/acgih.1992',STATUS='old')
      n = 56
      DO i = 1, n
         READ(kin,*) x1(i), y1(i)
         y1(i) = y1(i)
      ENDDO

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),y1(1))
      CALL addpnt(x1,y1,kdata,n,          0.,y1(1))
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),   0.)
      CALL addpnt(x1,y1,kdata,n,      1.e+38,   0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, label(j)
         STOP
      ENDIF
            
      DO iw = 1, nw-1
         s(j,iw) = yg(iw)
      ENDDO
      CLOSE (kin)

********** RB Meter, model 501
*  private communication, M. Morys (Solar Light Co.), 1994.
* From: morys@omni.voicenet.com (Marian Morys)
* Received: from acd.ucar.edu by sasha.acd.ucar.edu (AIX 3.2/UCB 5.64/4.03)
*          id AA17274; Wed, 21 Sep 1994 11:35:44 -0600

      j = j + 1
      label(j) = 'RB Meter, model 501'
      OPEN(UNIT=kin,FILE='DATAS1/rbm.501',STATUS='old')
      n = 57
      DO i = 1, n
         READ(kin,*) x1(i), y1(i)
      ENDDO
      
      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),y1(1))
      CALL addpnt(x1,y1,kdata,n,          0.,y1(1))
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),   0.)
      CALL addpnt(x1,y1,kdata,n,      1.e+38,   0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, label(j)
         STOP
      ENDIF
            
      DO iw = 1, nw-1
         s(j,iw) = yg(iw)
      ENDDO
      CLOSE (kin)

********* phytoplankton, Boucher et al. (1994) 
* from Boucher, N., Prezelin, B.B., Evens, T., Jovine, R., Kroon, B., Moline, M.A.,
* and Schofield, O., Icecolors '93: Biological weighting function for the ultraviolet
*  inhibition  of carbon fixation in a natural antarctic phytoplankton community, 
* Antarctic Journal, Review 1994, pp. 272-275, 1994.
* In original paper, value of b and m (em below are given as positive.  Correct values
* are negative. Also, limit to positive values.

      j = j + 1
      label(j) = 'phytoplankton, Boucher et al. (1994)'
      a = 112.5
      b = -6.223E-01
      c = 7.670E-04
      em = -3.17E-06
      DO iw = 1, nw-1
         IF (wc(iw) .GT. 290. .AND. wc(iw) .LT. 400.) THEN
            s(j,iw) = em + EXP(a+b*wc(iw)+c*wc(iw)*wc(iw))
         ELSE
            s(j,iw) = 0.
         ENDIF
         s(j,iw) = max(s(j,iw),0.)
      ENDDO

********* phytoplankton, Cullen et al.
* Cullen, J.J., Neale, P.J., and Lesser, M.P., Biological weighting function for the  
*  inhibition of phytoplankton photosynthesis by ultraviolet radiation, Science, 25,
*  646-649, 1992.
* phaeo

      j = j + 1
      label(j) = 'Cullen, phaeo'
      OPEN(UNIT=kin,FILE='DATAS1/phaeo.bio',STATUS='old')
      n = 106
      DO i = 1, n
         READ(kin,*) idum, dum1, dum2, y1(i)
         x1(i) = (dum1+dum2)/2.
      ENDDO

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),y1(1))
      CALL addpnt(x1,y1,kdata,n,          0.,y1(1))
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),   0.)
      CALL addpnt(x1,y1,kdata,n,      1.e+38,   0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, label(j)
         STOP
      ENDIF

      DO iw = 1, nw-1
         s(j,iw) = yg(iw)
      ENDDO
      CLOSE(kin)

* proro

      j = j + 1
      label(j) = 'Cullen, proro'
      OPEN(UNIT=kin,FILE='DATAS1/proro.bio',STATUS='old')
      n = 100
      DO i = 1, n
         READ(kin,*) idum, dum1, dum2, y1(i)
         x1(i) = (dum1+dum2)/2.
      ENDDO

      CALL addpnt(x1,y1,kdata,n,x1(1)*(1.-deltax),y1(1))
      CALL addpnt(x1,y1,kdata,n,          0.,y1(1))
      CALL addpnt(x1,y1,kdata,n,x1(n)*(1.+deltax),   0.)
      CALL addpnt(x1,y1,kdata,n,      1.e+38,   0.)
      CALL inter2(nw,wl,yg,n,x1,y1,ierr)
      IF (ierr .NE. 0) THEN
         WRITE(*,*) ierr, label(j)
         STOP
      ENDIF

      DO iw = 1, nw-1
         s(j,iw) = yg(iw)
      ENDDO
      CLOSE (kin)

****************************************************************
****************************************************************

*_______________________________________________________________________

      IF (j .GT. ks) STOP '1001'
*_______________________________________________________________________

      RETURN
      END

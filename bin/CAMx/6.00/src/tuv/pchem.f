      SUBROUTINE pchem(nw,wl,nz,tlev,airlev,
     $     j,sq,jlabel)
cgy modified for camx by greg yarwood, see comments below

*-----------------------------------------------------------------------------*
*=  PURPOSE:                                                                 =*
*=  Load various "weighting functions" (products of cross section and        =*
*=  quantum yield at each altitude and for wavelength).  The altitude        =*
*=  dependence is necessary to ensure the consideration of pressure and      =*
*=  temperature dependence of the cross sections or quantum yields.          =*
*=  The actual reading, evaluation and interpolation is done is separate     =*
*=  subroutines for ease of management and manipulation.  Please refer to    =*
*=  the inline documentation of the specific subroutines for detail          =*
*=  information.                                                             =*
*-----------------------------------------------------------------------------*
*=  PARAMETERS:                                                              =*
*=  NW     - INTEGER, number of specified intervals + 1 in working        (I)=*
*=           wavelength grid                                                 =*
*=  WL     - REAL, vector of lower limits of wavelength intervals in      (I)=*
*=           working wavelength grid                                         =*
*=  NZ     - INTEGER, number of altitude levels in working altitude grid  (I)=*
*=  TLEV   - REAL, temperature (K) at each specified altitude level       (I)=*
*=  AIRLEV - REAL, air density (molec/cc) at each altitude level          (I)=*
*=  J      - INTEGER, counter for number of weighting functions defined  (IO)=*
*=  SQ     - REAL, cross section x quantum yield (cm^2) for each          (O)=*
*=           photolysis reaction defined, at each defined wavelength and     =*
*=           at each defined altitude level                                  =*
*=  JLABEL - CHARACTER*40, string identifier for each photolysis reaction (O)=*
*=           defined                                                         =*
*-----------------------------------------------------------------------------*
*=  EDIT HISTORY:                                                            =*
*=  05/98  Original; adapted from the "old" JSPEC1 routine                   =*
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

* input

      INTEGER nw
      REAL wl(kw)
      
      INTEGER nz

      REAL tlev(kz)
      REAL airlev(kz)

* weighting functions

      CHARACTER*40 jlabel(kj)
      REAL sq(kj,kz,kw)

* input/output:
      INTEGER j

* local:
      REAL wc(kw), wu(kw)
      CHARACTER*7 sapver
      INTEGER iw, isap, nsap, nsap99, ncl, nio, niu, ncb6, icl
      parameter (nsap=23, nsap99=26, ncl=3, nio=10, niu=11, ncb6=23)
      character*12, saprc97(nsap), saprc99(nsap99)
      character*12, clreac(ncl), ioreac(nio)
      character*12, iupac04(niu), namsig, namphi
      character*12  cb6(ncb6)
      data saprc99    /'ACETONE.PHF ', 'ACROLEIN.PHF', 'BACL_ADJ.PHF',
     & 'BZCHO.PHF   ', 'C2CHO.PHF   ', 'CCHO_R.PHF  ', 'COOH.PHF    ',
     & 'GLY_ABS.PHF ', 'GLY_R.PHF   ', 'H2O2.PHF    ', 'HCHO_M.PHF  ',
     & 'HCHO_R.PHF  ', 'HNO3.PHF    ', 'HO2NO2.PHF  ', 'HONO-NO.PHF ',
     & 'HONO-NO2.PHF', 'IC3ONO2.PHF ', 'KETONE.PHF  ', 'MGLY_ABS.PHF',
     & 'MGLY_ADJ.PHF', 'NO2.PHF     ', 'NO3NO.PHF   ', 'NO3NO2.PHF  ',
     & 'O3O1D.PHF   ', 'O3O3P.PHF   ', 'ISPD.PHF    '/
      data clreac     /'Cl2.txt     ', 'HOCl.txt    ', 'HCOCl.txt   '/
      data ioreac     /'CH2I2.PHF   ', 'CH2IBR.PHF  ', 'CH2ICL.PHF  ',
     &                 'CH3I.PHF    ', 'HOI.PHF     ', 'I2.PHF      ',
     &                 'INO2.PHF    ', 'INO3.PHF    ', 'IO.PHF      ',
     &                 'OIO.PHF     '/
      data iupac04    /'O3_O1D.phf  ', 'O3_O3P.phf  ', 'HCHO_H.phf  ',
     &                 'HCHO_H2.phf ', 'HONO.phf    ', 'HO2NO2.phf  ',
     &                 'HNO3.phf    ', 'N2O5.phf    ', 'NTR.phf     ',
     &                 'PAN.phf     ', 'MGLY.phf    '/
      data cb6        /'NO2.PHF     ', 'O3O3P.PHF   ', 'O3O1D.PHF   ',
     &                 'H2O2.PHF    ', 'NO3_NO2.PHF ', 'NO3_NO.PHF  ',
     &                 'N2O5.PHF    ', 'HONO.PHF    ', 'HNO3.PHF    ',
     &                 'PNA.PHF     ', 'PAN.PHF     ', 'MEPX.PHF    ',
     &                 'NTR.PHF     ', 'FORM_R.PHF  ', 'FORM_M.PHF  ',
     &                 'ALD2_R.PHF  ', 'ALDX_R.PHF  ', 'KET.PHF     ',
     &                 'ACET.PHF    ', 'ISPD.PHF    ', 'GLYD.PHF    ',
     &                 'GLY_R.PHF   ', 'MGLY.PHF    ' /

*_______________________________________________________________________

* complete wavelength grid

      DO 5, iw = 1, nw - 1
         wc(iw) = (wl(iw) + wl(iw+1))/2.
         wu(iw) =  wl(iw+1)
 5    CONTINUE

*____________________________________________________________________________

C O2 + hv -> O + O
* reserve first position.  Cross section parameterization in Schumman-Runge and 
* Lyman-alpha regions are zenith-angle dependent, will be written in 
* subroutine sto2xs(nz,nw,xso2,nj,sj).
 
      j = 1
      jlabel(j) = 'O2 + hv -> O + O'

C O3 + hv ->  (both channels)
      CALL r1(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C NO2 + hv -> NO + O(3P)
      CALL r2(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C NO3 + hv ->  (both channels)
      CALL r3(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)
     
C N2O5 + hv -> (both channels)
      CALL r4(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C N2O + hv -> N2 + O(1D)
      CALL r44(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C HO2 + hv -> OH + O
      CALL r39(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C H2O2 + hv -> 2 OH
      CALL r8(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C HNO2 + hv -> OH + NO
      CALL r5(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C HNO3 + hv -> OH + NO2
      CALL r6(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C HNO4 + hv -> HO2 + NO2
      CALL r7(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CH2O + hv -> (both channels)
      CALL r10(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CH3CHO + hv -> (all three channels)
      CALL r11(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C C2H5CHO + hv -> C2H5 + HCO
      CALL r12(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CHOCHO + hv -> Products
      CALL r13(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CH3COCHO + hv -> Products
      CALL r14(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CH3COCH3 + hv -> Products
      CALL r15(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CH3OOH + hv -> CH3O + OH
      CALL r16(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CH3ONO2 + hv -> CH3O + NO2
      CALL r17(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C PAN + hv -> Products
      CALL r18(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C ClOO + hv -> Products
      CALL r31(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C ClONO2 + hv -> Products
      CALL r45(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CH3Cl + hv -> Products
      CALL r30(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CCl2O + hv -> Products
      CALL r19(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CCl4 + hv -> Products
      CALL r20(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CClFO + hv -> Products
      CALL r21(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CCF2O + hv -> Products
      CALL r22(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CF2ClCFCl2 (CFC-113) + hv -> Products
      CALL r23(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CF2ClCF2Cl (CFC-114) + hv -> Products
      CALL r24(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CF3CF2Cl (CFC-115) + hv -> Products
      CALL r25(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CCl3F (CFC-111) + hv -> Products
      CALL r26(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CCl2F2 (CFC-112) + hv -> Products
      CALL r27(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CH3CCl3 + hv -> Products
      CALL r29(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CF3CHCl2 (HCFC-123) + hv -> Products
      CALL r32(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CF3CHFCl (HCFC-124) + hv -> Products
      CALL r33(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CH3CFCl2 (HCFC-141b) + hv -> Products
      CALL r34(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CH3CF2Cl (HCFC-142b) + hv -> Products
      CALL r35(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CF3CF2CHCl2 (HCFC-225ca) + hv -> Products
      CALL r36(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CF2ClCF2CHFCl (HCFC-225cb) + hv -> Products
      CALL r37(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CHClF2 (HCFC-22) + hv -> Products
      CALL r38(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C BrONO2 + hv -> Products
      CALL r46(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CH3Br + hv -> Products
      CALL r28(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CHBr3 + hv -> Products
      CALL r9(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CF3Br (Halon-1301) + hv -> Products
      CALL r42(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CF2BrCF2Br (Halon-2402) + hv -> Products
      CALL r43(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CF2Br2 (Halon-1202) + hv -> Products
      CALL r40(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

C CF2BrCl (Halon-1211) + hv -> Products
      CALL r41(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

cgy
C ISPD + hv -> Products
      CALL r99(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel)

c SAPRC99 mechanism
      sapver = 'SAPRC99'
      do isap=1,nsap99
        CALL rsap(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel,
     &            saprc99(isap),sapver)
      enddo

c Chlorine mechanism
      do icl=1,ncl
        CALL rcl(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel,clreac(icl))
      enddo

c IUPAC 2002 data

c use riup to re-bin IUPAC data onto a common grid
c   the Huggins band data need gridw option 6
c

c O3 to O1D
c   Huggins band sigmas are binned (option 2 ==> inter3)
c      namsig = 'O3.sig1'
c      namphi = 'O3_O1D.phi'
c      call riup(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel,namsig,
c     &          namphi,2)
c   Hartley band sigmas are cell cenetered (option 1 ==> inter2)
c      namsig = 'O3.sig2'
c      namphi = 'O3_O1D.phi'
c      call riup(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel,namsig,
c     &          namphi,1)

c O3 to O3P
c   Huggins band sigmas are binned (option 2 ==> inter3)
c      namsig = 'O3.sig1'
c      namphi = 'O3_O3P.phi'
c      call riup(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel,namsig,
c     &          namphi,2)
c   Hartley band sigmas are cell cenetered (option 1 ==> inter2)
c      namsig = 'O3.sig2'
c      namphi = 'O3_O3P.phi'
c      call riup(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel,namsig,
c     &          namphi,1)

c HCHO sigmas are binned to 1 nm intervals
c   use gridw option 6 to get output every nm
c      namsig = 'HCHO.sig'
c      namphi = 'HCHO_H.phi'
c      call riup(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel,namsig,
c     &          namphi,2)
c
c      namsig = 'HCHO.sig'
c      namphi = 'HCHO_H2.phi'
c      call riup(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel,namsig,
c     &          namphi,2)
c
c use rsap to calculate rates from re-binned IUPAC data
      sapver = 'IUPAC04'
      do isap=1,niu
        CALL rsap(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel,
     &            iupac04(isap),sapver)
      enddo

c CB6 mechanism
      sapver = 'CB6'
      do isap=1,ncb6
        CALL rsap(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel,
     &            cb6(isap),sapver)
      enddo

c Iodine mechanism
      sapver = 'IODINE'
      do isap=1,nio
        CALL rsap(nw,wl,wc,nz,tlev,airlev,j,sq,jlabel,
     &            ioreac(isap),sapver)
      enddo

cgy
************************************************************************

      IF (j .GT. kj) then
        write(*,*) 'In RN j > kj',j, kj
        STOP 
      ENDIF
      RETURN
      END

      SUBROUTINE AVGCON (IOUT,IDONE)
C
C     PROGRAM READS A ASCII  AVERAGE FILE FROM AN AIRSHED SIM AND
C     WRITES IT OUT IN BINARY FORMAT.  THIS HAS BEEN WRITTEN
C     TO CONVERT THE 26-27 JUNE 1974 LA BASIN SIMS TO A UNFORMATTED
C     FILE TO INPUT TO THE ARB COMPUTER FOR A RUN TO BE SENT TO SAI.
C
C     UNITS FOR ALL CONCENTRATIONS ARE PPM
C
C     FILE 8: DIAGNOSTIC OUTPUT
C     FILE 7: INPUT, ASCII AIRSHED AVERAGE CONCENTRATION FILE
C     FILE 9: OUTPUT, BINARY AIRSHED AVERAGE CONCENTRATION FILE
C
      INCLUDE 'nampar.cmd'
      COMMON /LCM/ CONC(MXX,MXY),EMOB(MXX,MXY,29)
Ctcm  DIMENSION AVSNAM(10)
C
Ctcm  DATA AVSNAM /1HA,1HV,1HE,1HR,1HA,1HG,1HE,1H-,1HS,1H /
      character*4 AVSNAM(10)
C
      DATA AVSNAM /'A','V','E','R','A','G','E','-','S',' '/
C
C     ***  READ THE FOUR HEADER RECORDS
C
      IDONE=1
      READ (7,1000)IFILE,NOTE,NOSEG,NOSPEC,IDATB,TIMBEG,JDATE,TIMEND
      WRITE(9) IFILE,NOTE,NOSEG,NOSPEC,IDATB,TIMBEG,JDATE,TIMEND
      WRITE(IOUT,1001) IFILE,NOTE,IDATB,TIMBEG,JDATE,TIMEND
C
C  CHECK IF SHORT VERSION OF AVERAGE
C
      JMATCH = 0
      DO 10 I=1,10
        IF (IFILE(I) .EQ. AVSNAM(I)) JMATCH = JMATCH + 1
   10 CONTINUE
      IF (JMATCH .EQ. 10) THEN
        WRITE(*,*) 'Use file type AVERAGE-S to convert this file'
        STOP
      ENDIF
C
      READ (7,2001) XREF,YREF,IUTM,XORG,YORG,DELTAX,DELTAY,NOXG,
     $ NOYG,NOZG,NCELL1,NCELL2,SURFHT,HTMIN1,HTMIN2
      WRITE(9) XREF,YREF,IUTM,XORG,YORG,DELTAX,DELTAY,NOXG,NOYG,NOZG,
     $ NCELL1,NCELL2,SURFHT,HTMIN1,HTMIN2
C
      READ  (7,1002) ILOCX,ILOCY, NOX,NOY
      WRITE(9) ILOCX,ILOCY, NOX,NOY
C
      READ  (7,1003) ((MSPEC(I,J),I=1,10),J=1,NOSPEC)
      WRITE(9) ((MSPEC(I,J),I=1,10),J=1,NOSPEC)
C
      IF(NOSEG.NE.1) STOP 6
C     IF NOSEG>1 PROGRAM NEEDS TO LOOP OVER SEGMENTS.
C
C     ***  WRITE THE HEADER RECORDS FOR THE ASCII FILE
C
200   CONTINUE
C
C     ***  READ AND WRITE THE TIME VARYING DATA FOR 24 HRS
C
      IBHR = TIMBEG + 1
      IEHR = TIMEND
       DO 500 LL=1,9999
          READ(7,1005,END=999) IBGDAT, BEGTIM, IENDAT, ENDTIM
          WRITE(9) IBGDAT, BEGTIM, IENDAT, ENDTIM
          WRITE(IOUT,1005) IBGDAT,BEGTIM,IENDAT,ENDTIM
C
C   LOOP OVER SPECIES, ORDER OF SPECIES IS ESTABLISHED IN CHEMPARAM FILE
C
          DO 400 ISPEC=1,NOSPEC
C
C    LOOP OVER NUMBER OF VERTICAL LEVELS IN SIMULATION GRID
C
             DO 401 IZ=1,NOZG
                READ  (7,1006) ISEG, MNAME 
                READ  (7,1007) ((CONC(I,J),I=1,NOX),J=1,NOY)
                WRITE(9) ISEG, MNAME, ((CONC(I,J),I=1,NOX),J=1,NOY)
401          CONTINUE
400       CONTINUE
500    CONTINUE
C
C
  999 RETURN
 1000 FORMAT(10A1,60A1,/,I2,1X,I2,1X,I6,F6.0,I6,F6.0)
 1001 FORMAT(10X,10A1/5X,60A1/5X,I6,F6.0,I6,F6.0,//)
 1002 FORMAT(4I5)
 1003 FORMAT(10A1)
 1005 FORMAT(5X,2(I10,F10.2))
 1006 FORMAT(I4,10A1)
 1007 FORMAT(9E14.9)
 2001 FORMAT(F10.4,1X,F10.4,1X,I3,F10.4,1X,F10.4,1X,2F6.4,5I4,3F7.0)
      END

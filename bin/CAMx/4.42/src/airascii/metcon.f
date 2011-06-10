       SUBROUTINE METCON (IOUT,IDONE)
C
C
C      PROGRAM READS THE BINARY METSCALERS FILE
C      AND REFORMATS THIS FILE TO ASCII
C
      INCLUDE 'nampar.cmd'
Ctcm  DIMENSION  METTMP(10,6),SCAL(6)
      character*4  METTMP(10,6)
      DIMENSION  SCAL(6)
C
C
C
C   READ AND WRITE OUT FILE HEADER INFORMATION
C
      IDONE=1
      READ(7)IFILE,NOTE, NSEG, NSPECS, IDATE, BEGTIM, JDATE, ENDTIM
      WRITE(9,2100) IFILE,NOTE,NSEG,NSPECS,IDATE,BEGTIM,JDATE,ENDTIM
      WRITE (IOUT,1005) IDATE, BEGTIM, JDATE, ENDTIM
      READ(7) ORGX, ORGY, IZONE, UTMX, UTMY, DELTAX, DELTAY,
     $          NX, NY, NZ, NZLOWR, NZUPPR, HTSUR, HTLOW, HTUPP
      WRITE (9,2001) ORGX,ORGY,IZONE,UTMX,UTMY,DELTAX,DELTAY,NX,NY,
     $ NZ,NZLOWR,NZUPPR,HTSUR,HTLOW,HTUPP
      IF (NSPECS .GT. MXSPC) THEN
        WRITE(*,'(A,/,A)') ' PROGRAM ARRAY DIMENSIONS EXCEEDED',
     $                     ' CHECK GRID SIZE'
        STOP
      ENDIF
      IF (NSEG.EQ.0) GO TO 10
C  THIS WILL BE 0 FOR METSCL FILE
      READ (7) IX, IY, NXCLL, NYCLL
      WRITE (9,1002) IX,IY,NXCLL,NYCLL
  10  CONTINUE
      IF (NSPECS.EQ.0) GO TO 20
C  THIS WILL BE 0 FOR METSCL FILE
      READ  (7) ((MSPEC(M,L),M=1,10),L=1,NSPECS)
      WRITE (9,1003) ((MSPEC(I,J),I=1,10),J=1,NSPECS)
  20  CONTINUE
2100  FORMAT(10A1,60A1,/,I2,1X,I2,1X,I6,F6.0,I6,F6.0)
      IBHR=BEGTIM+1
      IEHR=ENDTIM
 2001  FORMAT(2(F16.5,1X),I3,1X,4(F16.5,1X),5I4,3F7.0)
 1002 FORMAT(4I5)
 1003  FORMAT(10A1)
C*
C
C    LARGE LOOP FOR 24 HOURS OF MET SCALERS DATA
C
      DO 1000 IH = 1,9999
          READ  (7,END=999) IBGDAT,BEGTIM,IENDAT,ENDTIM
          WRITE (9,1005) IBGDAT, BEGTIM, IENDAT, ENDTIM
          WRITE(IOUT,1005) IBGDAT,BEGTIM,IENDAT,ENDTIM
1005      FORMAT(5X,2(I10,F10.2))
C
C    READ IN METSCL DATA FOR THIS HOUR - 6 NAME - NUMBER PAIRS
C
C
C
          READ (7)      ((METTMP(I,J),I=1,10),SCAL(J),J=1,6)
          DO 23 J=1,6
            WRITE (9,1006) (METTMP(I,J),I=1,10),SCAL(J)
   23     CONTINUE
 1006     FORMAT(10A1,E14.7)
 1000 CONTINUE
  999 RETURN
       END

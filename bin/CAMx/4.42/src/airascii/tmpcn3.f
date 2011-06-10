      SUBROUTINE TMPCN3 (IOUT,IDONE)
C
C
C      PROGRAM READS THE 3D BINARY TEMPERATURE FILE THAT WILL INPUT INTO
C      THE SAI AIRSHED MODEL AND REFORMATS THIS FILE
C      THIS ROUTINE IS ALSO USED FOR 3D HUMIDITY FILE SINCE FORMAT OF
C      THAT FILE IS IDENTICAL TO 3D TEMPERATURE FILE FORMAT.
C
      INCLUDE 'nampar.cmd'
      COMMON /LCM/ EMOB(MXX,MXY)
C
C
C
C   READ AND WRITE OUT FILE HEADER INFORMATION
C
      IDONE=1
      READ(7) IFILE,NOTE,NSEG,NSPECS, IDATE, BEGTIM, JDATE, ENDTIM
      WRITE(9,2100) IFILE,NOTE,NSEG,NSPECS,IDATE,BEGTIM,JDATE,ENDTIM
      WRITE(IOUT,1005) IDATE, BEGTIM, JDATE, ENDTIM
      READ(7) ORGX, ORGY, IZONE, UTMX, UTMY, DELTAX, DELTAY,
     $          NX, NY, NZ, NZLOWR, NZUPPR, HTSUR, HTLOW, HTUPP
      WRITE (9,2001) ORGX,ORGY,IZONE,UTMX,UTMY,DELTAX,DELTAY,NX,NY,
     $          NZ,NZLOWR,NZUPPR,HTSUR,HTLOW,HTUPP
      IF (NSPECS .GT. MXSPC .OR. NX .GT. MXX .OR. NY .GT. MXY) THEN
        WRITE(*,'(A,/,A)') ' PROGRAM ARRAY DIMENSIONS EXCEEDED',
     $                     ' CHECK GRID SIZE'
        STOP
      ENDIF
      READ(7) IX, IY, NXCLL, NYCLL
      WRITE (9,1002) IX,IY,NXCLL,NYCLL
      IBHR=BEGTIM+1
      IEHR=ENDTIM
C*
C
C    LARGE LOOP FOR 24 HOURS OF TEMPERATURE MEASURMENTS
C
      DO 1000 IH = 1,100
          READ  (7,END=999) IBGDAT,BEGTIM,IENDAT,ENDTIM
          WRITE (9,1005) IBGDAT, BEGTIM, IENDAT, ENDTIM
          WRITE(IOUT,1005) IBGDAT,BEGTIM,IENDAT,ENDTIM
C
C    LOOP OVER LEVELS -  READ AND WRITE OUT IN ASCII
C
          DO 200 K=1,NZ
            READ (7) ISEG, (MNAME(M),M = 1,10), ((EMOB(I,J),
     $         I=1,NX),J=1,NY)
            WRITE (9,1006) ISEG, (MNAME(M),M=1,10)
            WRITE (9,1007) ((EMOB(I,J),I=1,NX),J=1,NY)
  200     CONTINUE
 1000 CONTINUE
  999 RETURN
C
 1002 FORMAT(4I5)
 1005 FORMAT(5X,2(I10,F10.2))
 1006 FORMAT(I4,10A1)
 1007 FORMAT(9E14.7)
 2001 FORMAT(2(F16.5,1X),I3,1X,4(F16.5,1X),5I4,3F7.0)
 2100 FORMAT(10A1,60A1,/,I2,1X,I2,1X,I6,F6.0,I6,F6.0)
      END

       SUBROUTINE PTSCON (IOUT,IDONE)
C
C
C      PROGRAM READS THE BINARY POINT SOURCE FILE THAT IS INPUT INTO
C      THE SAI AIRSHED MODEL AND REFORMATS THIS FILE
C
      INCLUDE 'nampar.cmd'
Ctcm  DIMENSION  METTMP(10,6),SCAL(6),ISEGIN(4,20)
Ctcm  DIMENSION  SPNAME(10,MXSPC),EMISS(20000)
      character*4  METTMP(10,6)
      DIMENSION  SCAL(6),ISEGIN(4,20)
      character*4  SPNAME(10,MXSPC)
      DIMENSION  EMISS(20000)
      REAL*4     DUMX(20000),DUMY(20000),IDUMX(20000),IDUMY(20000)
      REAL*8     DUM1,DUM2,DUM3,DUM4,DUM5,DUM6
      COMMON /BIG1/   DUMXX(20000),DUMYY(20000),
     $ ICELL(20000),JCELL(20000),FLOW(20000),PLUMHT(20000),
     $ KCELL(20000)
C
C
C   READ AND WRITE OUT FILE HEADER INFORMATION
C
      IDONE=1
      READ (7) IFILE,NOTE,NSEG,NSPECS,IDATE,BEGTIM,JDATE,ENDTIM
      WRITE (9,2100) IFILE,NOTE,NSEG,NSPECS,IDATE,BEGTIM,JDATE,ENDTIM
      WRITE (IOUT,1007) IDATE, BEGTIM, JDATE, ENDTIM
      READ  (7) ORGX,ORGY,IZONE,UTMX,UTMY,DELTAX,DELTAY,NX,NY,
     $ NZ,NZLOWR,NZUPPR,HTSUR,HTLOW,HTUPP
      IF (NSPECS .GT. MXSPC) THEN
        WRITE(*,'(A,/,A)') ' PROGRAM ARRAY DIMENSIONS EXCEEDED',
     $                     ' CHECK GRID SIZE'
        STOP
      ENDIF
      WRITE (9,2001) ORGX, ORGY, IZONE, UTMX, UTMY, DELTAX, DELTAY,
     $          NX, NY, NZ, NZLOWR, NZUPPR, HTSUR, HTLOW, HTUPP
      IF (NSEG.EQ.0) GO TO 11
C  SEGMENT ORIGIN AND SEGMENT SIZE
      DO 10 I=1,NSEG
      READ  (7) IX,IY,NXCLL,NYCLL
      WRITE (9,1002) IX, IY, NXCLL, NYCLL
      WRITE (IOUT,1002) IX,IY,NXCLL,NYCLL
  10  CONTINUE
  11  CONTINUE
      IF (NSPECS.LE.0) GO TO 21
C  NAMES OF ALL SPECIES PRESENT
      READ  (7) ((MSPEC(I,J),I=1,10),J=1,NSPECS)
      WRITE (9,1003) ((MSPEC(I,J),I=1,10),J=1,NSPECS)
      WRITE (IOUT,1013) ((MSPEC(I,J),I=1,10),J=1,NSPECS)
  20  CONTINUE
  21  CONTINUE
      IF (NSEG.LE.0) GO TO 31
C
C  TIME INVARIANT DATA
C
      DO 30 I=1,NSEG
      READ (7) ISEGM,NPMAX
      WRITE(9,1021) ISEGM,NPMAX
      IF (NPMAX.LE.0) GO TO 25
C
C  FOR EACH POINT SOURCE LOCATION AND STACK PARAMETERS
C
      if (npmax .gt. 20000) then
        write(*,*) npmax, ' is more than the max no. of pts'
        stop 
      endif
      READ (7) (DUMX(II),DUMY(II),IDUMX(II),IDUMY(II),DUMXX(II),
     &   DUMYY(II),II=1,NPMAX)
      DO 22 II=1,NPMAX
         DUM1=DUMX(II)
         DUM2=DUMY(II)
         DUM3=IDUMX(II)
         DUM4=IDUMY(II)
         DUM5=DUMXX(II)
         DUM6=DUMYY(II)
         WRITE (9,1005) DUM1,DUM2,DUM3,DUM4,DUM5,DUM6
  22  CONTINUE
C
  25  CONTINUE
  30  CONTINUE
  31  CONTINUE
      IBHR=BEGTIM+1
      IEHR=ENDTIM
C
C    LARGE LOOP FOR 24 HOURS OF PT SOURCE DATA
C
      DO 1000 IH = 1,9999
          READ  (7,END=999) IBGDAT, BEGTIM, IENDAT, ENDTIM
          WRITE (9,1007) IBGDAT,BEGTIM,IENDAT,ENDTIM
          WRITE(IOUT,1007) IBGDAT,BEGTIM,IENDAT,ENDTIM,IH
C
C    NOW GO THROUGH NUMBER OF SEGMENTS
C
          IF (NSEG.LE.0) GO TO 1000
          DO 990 J=1,NSEG
             READ (7) ISEGNM,NUMPTS
             WRITE(9,1004) ISEGNM,NUMPTS
             WRITE(IOUT,1004) ISEGNM,NUMPTS
             IF (NUMPTS.LE.0) GO TO 990
             READ(7) (ICELL(II),JCELL(II),KCELL(II),FLOW(II),
     &          PLUMHT(II),II=1,NUMPTS)
             WRITE(9,1008) (ICELL(II),JCELL(II),KCELL(II),FLOW(II),
     &          PLUMHT(II),II=1,NUMPTS)
             IF (NSPECS.LE.0) GO TO 990
             DO 980 K=1,NSPECS
                READ (7) ISEGNM,(SPNAME(II,K),II=1,10),(EMISS(II),
     &             II=1,NUMPTS)
                WRITE (9,1009) ISEGNM,(SPNAME(II,K),II=1,10)
                WRITE (9,1010) (EMISS(II),II=1,NUMPTS)
  980        CONTINUE
  990 CONTINUE
 1000 CONTINUE
  999 RETURN
 1002 FORMAT(4I5)
 1003 FORMAT(10A1)
 1004 FORMAT(4I10)
 1005 FORMAT(6D20.7)
 1006 FORMAT(10A1,E18.9)
 1007 FORMAT(2(I10,F10.2),I10)
 1008 FORMAT(3I12,2E18.9)
 1009 FORMAT(I10,10A1)
 1010 FORMAT(9E14.9)
 1013 FORMAT(1X,10A1)
 1021 FORMAT(2I10)
 2001 FORMAT(2(F16.5,1X),I3,1X,4(F16.5,1X),5I4,3F7.0)
 2100 FORMAT(10A1,60A1,/,I2,1X,I2,1X,I6,F6.0,I6,F6.0)
      END

      SUBROUTINE BNDCON (IOUT,IDONE)
C
C
C      PROGRAM READS THE ASCII BOUNDARY FILE THAT WAS SENT TO
C      SAI AND REFORMATS THIS FILE SO
C      IT CAN BE USED AS THE BINARY INPUT TO THE MODEL
C
      INCLUDE 'nampar.cmd'
Ctcm  DIMENSION  METTMP(10,6),SCAL(6),ISEGIN(4,20)
Ctcm  DIMENSION  SPNAME(10,30),BCONC(10,500)
      character*4  METTMP(10,6)
      DIMENSION  SCAL(6),ISEGIN(4,20)
      character*4  SPNAME(10,30)
      DIMENSION  BCONC(10,500)
      DIMENSION  IX(500),IY(500),NXCLL(500),NYCLL(500)
      DIMENSION  ILOC(4,500)
      IDONE=1
      READ (7) IFILE,NOTE,NSEG,NSPECS,IDATE,BEGTIM,JDATE,ENDTIM
      WRITE(9,2100)IFILE,NOTE, NSEG, NSPECS, IDATE,BEGTIM,JDATE,ENDTIM
      WRITE (IOUT,1007) IDATE, BEGTIM, JDATE, ENDTIM
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
C  SEGMENT ORIGIN AND SEGMENT SIZE
      READ(7) (IX(I),IY(I), NXCLL(I), NYCLL(I),I=1,NSEG)
      WRITE(IOUT,5002) (IX(I),IY(I), NXCLL(I), NYCLL(I),I=1,NSEG)
      WRITE (9,1002) (IX(I),IY(I),NXCLL(I),NYCLL(I),I=1,NSEG)
  10  CONTINUE
      IF (NSPECS.EQ.0) GO TO 20
C  NAMES OF ALL SPECIES PRESENT
      READ(7) ((MSPEC(I,J),I=1,10),J=1,NSPECS)
      WRITE (9,1003) ((MSPEC(I,J),I=1,10),J=1,NSPECS)
  20  CONTINUE
      IF (NSEG.EQ.0) GO TO 31
C
C  TIME INVARIANT DATA
C
      NRECS=4*NSEG
C
C  FOR EACH CELL FIND BOUNDARY INDEX AND LOCATION OF ADAJENT CELL
C
      DO 30 I=1,NRECS
         READ(7)ISEGM,IEDGE,NCELLS,((ILOC(II,J),II=1,4),J=1,NCELLS)
         WRITE(IOUT,5021)ISEGM,IEDGE,NCELLS
         WRITE(9,1021)      ISEGM,IEDGE,NCELLS,
     &      ((ILOC(II,J),II=1,4),J=1,NCELLS)
  30  CONTINUE
  31  CONTINUE
      IBHR=BEGTIM+1
      IEHR=ENDTIM
C*
C
C    LARGE LOOP FOR 24 HOURS OF BOUNDARY DATA
C
      DO 1000 IH = 1,9999
          READ(7,END=999) IBGDAT,BEGTIM,IENDAT,ENDTIM
          WRITE (9,1007) IBGDAT, BEGTIM, IENDAT, ENDTIM
          WRITE(IOUT,1007) IBGDAT,BEGTIM,IENDAT,ENDTIM
C
C    NOW GO THROUGH NUMBER OF SEGMENTS
C
          DO 990 J=1,NSEG
             IF (NSPECS.LE.0) GO TO 1000
C   FOR EACH SPECIES
             DO 985 K=1,NSPECS
                NCEL = NY
C   FOR EACH EDGE
                DO 980 I=1,4
                   IF (I.GE.3) NCEL= NX
                   READ(7) ISEGNM,(SPNAME(II,K),II=1,10),NEDGNO,
     &                ((BCONC(II,JJ),II=1,NZ),JJ=1,NCEL)
                   WRITE(9,1009)ISEGNM,(SPNAME(II,K),II=1,10),NEDGNO,
     &                ((BCONC(II,JJ),II=1,NZ),JJ=1,NCEL)
  980           CONTINUE
  985        CONTINUE
  990    CONTINUE
 1000 CONTINUE
  999 RETURN
 1002 FORMAT(4I5)
 1003 FORMAT(10A1)
 1004 FORMAT(4I10/)
 1005 FORMAT(4I10/)
 1006 FORMAT(10(10A1,E14.7/))
 1007 FORMAT(5X,2(I10,F10.2))
 1008 FORMAT(2I10,2E14.7/)
 1009 FORMAT(I10,10A1,I10/(9E14.7))
 1021 FORMAT(3I10/(9I14))
 2001 FORMAT(2(F16.5,1X),I3,1X,4(F16.5,1X),5I4,3F7.0)
 2100 FORMAT(10A1,60A1,/,I2,1X,I2,1X,I6,F6.0,I6,F6.0)
 5002 FORMAT(1X,4I5/)
 5021 FORMAT(3I10)
      END

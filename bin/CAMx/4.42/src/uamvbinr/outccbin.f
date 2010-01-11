c     output coarse concentration file

      subroutine outccbin

      include 'uamvbinr.inc'
c
      DIMENSION IFILE(10), MNOTE(60)
      character*4 ifile, mnote
      DIMENSION NOTE(60)
C
C   READ AND WRITE OUT FILE HEADER INFORMATION
C
      READ(InNum,2100)IFILE,(MNOTE(i),i=1,10),(NOTE(i),i=11,60),
     &           NSEG, NSPECS, IDATE, BEGTIM, JDATE, ENDTIM
      WRITE(OutNum)IFILE,(MNOTE(i),i=1,10),(NOTE(i),i=11,60),
     &             NSEG, NSPECS, IDATE, BEGTIM, JDATE,ENDTIM
      WRITE (ErrNum,1005) IDATE, BEGTIM, JDATE, ENDTIM
      READ (InNum,2001) ORGX,ORGY,IZONE,UTMX,UTMY,DELTAX,DELTAY,NX,NY,
     $ NZ,NZLOWR,NZUPPR,HTSUR,HTLOW,HTUPP
      WRITE(OutNum) ORGX, ORGY, IZONE, UTMX, UTMY, DELTAX, DELTAY,
     $          NX, NY, NZ, NZLOWR, NZUPPR, HTSUR, HTLOW, HTUPP
      WRITE (ErrNum,2001) ORGX,ORGY,IZONE,UTMX,UTMY,DELTAX,DELTAY,NX,NY,
     $ NZ,NZLOWR,NZUPPR,HTSUR,HTLOW,HTUPP
      READ (InNum,1002) IX,IY,NXCLL,NYCLL
      WRITE  (OutNum) IX, IY, NXCLL, NYCLL
      if (nx .gt. mxx .or. ny .gt. mxy) then
        write(*,*) 'Maximum region dimension exceeded.'
        write(*,*) 'File x by y size is ', nx, ' by ', ny
        write(*,*) 'Max x by y size is ', mxx, ' by ', mxy
        stop
      endif
      if (nspecs .gt. mxspc) then
        write(*,*) 'Maximum number of species exceeded.'
        write(*,*) 'No. of species on file = ', nspecs
        write(*,*) 'Maximum no. of species is ', mxspc
        stop
      endif
      IF (NSPECS.LE.0) GO TO 10
      READ (InNum,1003) ((MSPEC(I,J),I=1,10),J=1,NSPECS)
   10 IBHR=BEGTIM + 1
      IEHR=ENDTIM
      WRITE(OutNum) ((MSPEC(M,L),M=1,10),L=1,NSPECS)
C*
C    LARGE LOOP FOR 24 HOURS OF AIR QUALITY MEASURMENTS
C
      DO 1000 IH = 1,1000
          READ (InNum,1005,END=100,err=200) 
     $                       IBGDAT, BEGTIM, IENDAT, ENDTIM
          IF (ABS(BEGTIM-24.) .LT. .01 .AND.
     $        ABS(ENDTIM-0.) .LT. .01) THEN
            BEGTIM = BEGTIM - .5
            ENDTIM = ENDTIM + .5
          ENDIF
          WRITE  (OutNum,err=210) IBGDAT,BEGTIM,IENDAT,ENDTIM
          WRITE(ErrNum,1005) IBGDAT,BEGTIM,IENDAT,ENDTIM
C
C    LOOP ON INDIVIDUAL SPECIES -  READ AND WRITE OUT IN ASCII
C
          IF (NSPECS.LE.0) GO TO 1000
          DO 400 L = 1,NSPECS
             DO 350 K=1,NZ
                READ (InNum,1006,end=100,err=200) 
     $                                 ISEG,(MSPEC(M,L),M = 1,10)
                READ (InNum,1007,end=100,err=200) 
     $                                 ((EMOB(I,J),I=1,NX),J=1,NY)
                WRITE (OutNum,err=210)ISEG,(MSPEC(M,L),M=1,10),
     $            ((EMOB(I,J),I=1,NX),J=1,NY)
  350        CONTINUE
          WRITE (ErrNum,1002) L
          WRITE (ErrNum,1006) ISEG, (MSPEC(M,L),M = 1,10)
  400     CONTINUE
 1000 CONTINUE
 1002 FORMAT(4I5)
 1003  FORMAT(10A1)
 1005 FORMAT(5X,2(I10,F10.2))
 1006 FORMAT(I4,10A1)
 1007 FORMAT(9E14.7)
 2001 FORMAT(F10.1,1X,F10.1,1X,I3,F10.1,1X,F10.1,1X,2F6.0,5I4,3F7.0)
 2100 FORMAT(10A1,60A1,/,I2,1X,I2,1X,I6,F6.0,I6,F6.0)
C
  100 continue
      return

  200 continue
      write(ErrNum, 220) 'Error reading outcc file'
      write(*, 220) 'Error reading outcc file'
      stop

  210 continue
      write(ErrNum, 220) 'Error writing outcc file'
      write(*, 220) 'Error writing outcc file'
      stop

  220 format(A)

      end

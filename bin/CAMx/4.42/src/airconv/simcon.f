      SUBROUTINE SIMCON (IOUT,IDONE)
C
C
C      PROGRAM READS THE ASCII SIMCONTROL FILE THAT IS INPUT INTO
C      THE SAI AIRSHED MODEL FOR LA AND REFORMATS THIS FILE SO
C      IT CAN BE USED TO RUN THE MODEL.
C
      INCLUDE 'nampar.cmd'
Ctcm  DIMENSION  MECHID(60)
      character*4  MECHID(60)
Ctcm  DIMENSION TNAME(10,20),SPNAME(10,100),
Ctcm &   UBSS(100),LBINT(100),UBINT(100),STOINM(10,100)
      character*4 TNAME(10,20),SPNAME(10,100),STOINM(10,100)
      DIMENSION UBSS(100),LBINT(100),UBINT(100)
      LOGICAL*2 RATE(100),PHOTO(100),TEMP(100),ACTIV(100),REFTP(100)
      LOGICAL*2 REA(100),SSI(100),SSB(100),RES(100),LBSS(100),FLAGS(7)
Ctcm  DIMENSION COEF(100),SPECNM(10,100)
      DIMENSION COEF(100)
      character*4 SPECNM(10,100)
C
C   READ AND WRITE OUT FILE HEADER INFORMATION
C
      IDONE=1
      READ(7,2100)IFILE,NOTE,NSEG, NSPECS, IDATE, BEGTIM, JDATE, ENDTIM
      WRITE(9)     IFILE,NOTE,NSEG,NSPECS,IDATE,BEGTIM,JDATE,ENDTIM
      WRITE (IOUT,1005) IDATE, BEGTIM, JDATE, ENDTIM
      IBHR=BEGTIM +1
      IEHR=ENDTIM
      IF (NSPECS.LE. 0) GO TO 10
      READ(7,1003) ((MSPEC(M,L),M=1,10),L=1,NSPECS)
      WRITE (9)      ((MSPEC(I,J),I=1,10),J=1,NSPECS)
  10  CONTINUE
C*
C
C  TIME INVARIANT DATA
C
C
C  SIMULATION CONTROL DATA
C
      READ(7,1004) (MECHID(I),I=1,60),IBDATE,BHR,IEDATA,EHR,
     & (FLAGS(I),I=1,7),ROUGH,VEG,RMAX,NSTEP,SIZEMN,IMAXIT,ERRTOL,
     & DARK,TIMCN,TIMAV,IHIST,ICORE,NLVIC,NLVAC,ICPRNT,IPOPT

      WRITE(9)      (MECHID(I),I=1,60),IBDATE,BHR,IEDATA,EHR,
     & (FLAGS(I),I=1,7),ROUGH,VEG,RMAX,NSTEP,SIZEMN,IMAXIT,ERRTOL,
     & DARK,TIMCN,TIMAV,IHIST,ICORE,NLVIC,NLVAC,ICPRNT,IPOPT
  999 RETURN
 1002 FORMAT(4I5/)
 1003 FORMAT(10A1)
 1004 FORMAT(60A1,2(I10,E14.7)/7L4/3E14.7,I10,E14.7,I10/
     &  4E14.7,6I10)
 1005 FORMAT(10A1,8L4/)
 1006 FORMAT(5L4/)
 1007 FORMAT(10A1,E14.7/)
 1021 FORMAT(10A1/100(9E14.7/))
 2001 FORMAT(F10.1,1X,F10.1,1X,I3,F10.1,1X,F10.1,1X,2F6.0,5I4,3F7.0)
 2100 FORMAT(10A1,60A1,/,I2,1X,I2,1X,I6,F6.0,I6,F6.0)
       END

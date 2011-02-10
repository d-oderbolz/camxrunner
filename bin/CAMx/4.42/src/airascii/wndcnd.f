      SUBROUTINE WNDCND (IOUT,IDONE)
C
C     THIS SUBROUTINE PRINTS DIAGNOSTICS ON THE UAM WIND FILE
C
C
C     WIND COMPONENT UNITS ARE METERS/HOUR
C
      INCLUDE 'nampar.cmd'
      INCLUDE "winds.cmd"
Ctcm  DIMENSION  WINDX(10), WINDY(10)
      character*4  WINDX(10), WINDY(10)
      dimension wmxrem(20),wmnrem(20),wavrem(20)
C
C     READ WIND FILE HEADER INFO AND WRITE TO NEW FILE
C
      IDONE=1
      READ (7) IFILE, NOTE, NSEG, NSPECS, JDATE, BEGTIM, IDATE,
     $ ENDTIM
      READ(7) ORGX, ORGY, IZONE, UTMX, UTMY, DELTAX, DELTAY,
     1          NX, NY, NZ, NZLOWR, NZUPPR, HTSUR, HTLOW, HTUPP
 1001 FORMAT(F10.2,F10.2,I3,F10.2,F10.2,2F6.0,5I4,3F7.0)
      READ (7) IX, IY, NXCLL, NYCLL
C
C   CHECK RANGE OF ARRAYS
C
      IF (NX .GT. MXX .OR. NY .GT. MXY .OR. NZ .GT. MXZ) THEN
         WRITE(IOUT,*) 'ARRAY DIMENSIONS EXCEEDED'
         WRITE(IOUT,*) 'FILE DIMENSIONS: ', NX, NY, NZ
         WRITE(IOUT,*) 'PROG DIMENSIONS: ', MXX, MXY, MXZ
         STOP
      ENDIF
C
C  WRITE HEADER VALUES
C
      WRITE(IOUT,2000) IFILE,NOTE,NSEG,NSPECS,JDATE,BEGTIM, IDATE,
     $ ENDTIM
      WRITE(IOUT,2010) ORGX, ORGY, IZONE, UTMX, UTMY, DELTAX, DELTAY,
     $         NX,NY,NZ,NZLOWR,NZUPPR,HTSUR,HTLOW,HTUPP
      WRITE(IOUT,2020) IX, IY, NXCLL, NYCLL
C
C   LOOP OVER HOURS
C
      DO 600 N=1,999
         READ(7,END=999) IDT,TM1,IDT,TM2
         WRITE(IOUT,2030) IDT,TM1,IDT,TM2
         READ(7) ISEG,REF,HIX,HIY,AVXW,AVXE,AVYS,AVYN
         WRITE(IOUT,2040) ISEG,REF,HIX,HIY,AVXW,AVXE,AVYS,AVYN
         DO 410 K=1,NZ
            READ(7) ISEG,WINDX,((U(I,J,K),I=1,NX),J=1,NY)
            READ(7) ISEG,WINDY,((V(I,J,K),I=1,NX),J=1,NY)
  410    CONTINUE
         do 411 k=1,nz
            wavrem(k)=0.0
            wss=sqrt(u(2,2,k)*u(2,2,k)+v(2,2,k)*v(2,2,k))*2.78E-4
            wmxrem(k)=wss
            wmnrem(k)=wss
            do 412 j=2,ny-1
            do 412 i=2,nx-1
               wss=sqrt(u(i,j,k)*u(i,j,k)+v(i,j,k)*v(i,j,k))*2.78E-4
               wmxrem(k)=amax1(wmxrem(k),wss)
               wmnrem(k)=amin1(wmnrem(k),wss)
               wavrem(k)=wavrem(k)+wss
  412       continue
            wavrem(k)=wavrem(k)/((nx-2)*(ny-2))
            write(iout,2051) k,wmxrem(k),wmnrem(k),wavrem(k)
  411    continue
         CALL AVERGE(NX,NY,NZ)
         WRITE(IOUT,2050) ISEG,REF,HIX,HIY,AVXW,AVXE,AVYS,AVYN
         WRITE(IOUT,2060) WXLOW, WXHI, WYLOW, WYHI, WXMIN, WYMIN,       
     $                 SPMIN, IMIN, JMIN, KMIN, SPMAX, IMAX, 
     $                 JMAX, KMAX, NC5, NC10, NC20, NC30
  600 CONTINUE
  999 RETURN
 1000 FORMAT(10A1,2X,60A1,/,I2,1X,I2,1X,I6,1X,F6.2,1X,I6,1X,F6.2)
 1002 FORMAT(4I5)
 1003 FORMAT(I6,F7.0,I6,F7.0)
 1004 FORMAT(I4,7F8.1)
 1005 FORMAT(I4,10A1)
 1006 FORMAT(7D16.9)
 2000 FORMAT(' FILE HEADER VALUES:  ',//,1X,10A1,5X,60A1,/,
     $       1X,I10,I10,I10,F10.2,I10,F10.2)
 2010 FORMAT(/,1X,2F10.1,I10,/,2F10.1,5X,2F10.4,/,
     $       1X,5I10,/,1X,F10.1,5X,2F10.1)
 2020 FORMAT(/,1X,4I10)
 2030 FORMAT(/,1X,'DATE AND TIME:  ',I10,F10.1,I10,F10.1)
 2040 FORMAT(/,1X,'FROM DATA FILE: ',//,1X,'    ISEG      REF   ',/,
     $       1X,I10,F10.2,//,1X,'    HIX       HIY  ',/,1X,2F10.0,/,
     $       1X,'   AVXW      AVXE      AVYS      AVYN   ',/,
     $       1X,4F10.0)
 2050 FORMAT(/,1X,'CALCULATED: ',//,1X,'    ISEG      REF   ',/,
     $       1X,I10,F10.2,//,1X,'    HIX       HIY  ',/,1X,2F10.0,/,
     $       1X,'   AVXW      AVXE      AVYS      AVYN   ',/,
     $       1X,4F10.0)
 2051 FORMAT(1X,'MAX,MIN,AVG WS (M/S) FOR LAYER=',I5,3F10.2)
 2060 FORMAT(//,1X,'ADDITIONAL STATS: ',//,1X,'MINIMUM AND MAXIMUM X: ',     
     $       2F10.0,/,1X,'MINIMUM AND MAXIMUM Y: ',2F10.0,/,
     $       1X,'MINIMUM ABSOLUTE X AND Y: ',2(1PE12.4),/,
     $       1X,'MIN AND MAX SPEED: ',E12.4,' @ (',3I3,')',5X,
     $       E12.4,' @ (',3I3,')',
     $       //,10X,'SPEED   > 5 M/S  >10 M/S  >20 M/S  >30 M/S',/,
     $       1X,'NUMBER OF CELLS  ',I5,4X,I5,4X,I5,4X,I5)
 5000 FORMAT(1X, I5, 5X, F6.2, 5X, I5, 5X, F6.2)
      END

      SUBROUTINE AVGTIM (IOUT,IDONE)
C
C     PROGRAM READS A BINARY AVERAGE FILE FROM AN AIRSHED SIM AND
C     WRITES OUT TIME OF CONCENTRATION MAXIMUM (SURFACE ONLY).
C
C     UNITS FOR ALL CONCENTRATIONS ARE PPM
C
C     FILE 8: DIAGNOSTIC OUTPUT
C     FILE 5: INPUT, ASCII AIRSHED AVERAGE CONCENTRATION FILE
C     FILE 9: OUTPUT, ASCII FILE OF HOUR OF MAX CON
C
      include 'nampar.cmd'
      parameter (mxhrsp=1)
      COMMON /LCM/ CONC(MXX,MXY)
      common /maxtim/ jhrmax(mxx,mxy,mxhrsp), cmax(mxx,mxy,mxhrsp)
c
c
c   zero max concentration and hour of max
c
      do 30 l=1,mxhrsp
        do 25 j=1,mxy
          do 20 i=1,mxx
            cmax(i,j,l) = -1.e20
            jhrmax(i,j,l) = -9
   20     continue
   25   continue
   30 continue
C
C     ***  READ THE FOUR HEADER RECORDS
C
      IDONE=1
      READ (7)ifile,note,NOSEG,NOSPEC,IDATB,TIMBEG,JDATE,TIMEND
      WRITE(IOUT,1001) ifile,note,IDATB,TIMBEG,JDATE,TIMEND
C
      READ (7) XREF,YREF,IUTM,XORG,YORG,DELTAX,DELTAY,NOXG,
     $ NOYG,NOZG,NCELL1,NCELL2,SURFHT,HTMIN1,HTMIN2
C
      READ  (7) ILOCX,ILOCY, NOX,NOY
C
      IF (NOSPEC .GT. MXHRSP .OR. NOX .GT. MXX .OR. NOY .GT. MXY) THEN
        WRITE(*,'(A,/,A)') ' PROGRAM ARRAY DIMENSIONS EXCEEDED',
     $                     ' CHECK GRID SIZE'
        STOP
      ENDIF
C
      READ  (7) ((MSPEC(I,J),I=1,10),J=1,NOSPEC)
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
          READ(7,END=510) IBGDAT, BEGTIM, IENDAT, ENDTIM
          WRITE(IOUT,1005) IBGDAT,BEGTIM,IENDAT,ENDTIM
          IHOUR = INT(BEGTIM+.005) + 1
C
C   LOOP OVER SPECIES, ORDER OF SPECIES IS ESTABLISHED IN CHEMPARAM FILE
C
          DO 400 ISPEC=1,NOSPEC
C
C    LOOP OVER NUMBER OF VERTICAL LEVELS IN SIMULATION GRID
C
             DO 401 IZ=1,NOZG
                READ (7) ISEG, ifile, ((CONC(I,J),I=1,NOX),J=1,NOY)
                if (iz .eq. 1) then
                  do 350 j=1,noy
                    do 345 i=1,nox
                      if (conc(i,j) .ge. cmax(i,j,ispec)) then
                        cmax(i,j,ispec) = conc(i,j)
                        jhrmax(i,j,ispec) = ihour
                      endif
  345               continue
  350             continue
                endif
401          CONTINUE
400       CONTINUE
500    CONTINUE
c
c  write out hour of max for all cells
c
  510 do 560 l=1,nospec
        write(9,'(10a1)') (mspec(i,l),i=1,10)
        do 550 j=noy,1,-1
          write(9,'(40i3,/,1x,40i3,/,2x,40i3)') (jhrmax(i,j,l),i=1,nox)
  550   continue
  560 continue
      do 660 l=1,nospec
        write(61,'(10a1)') (mspec(i,l),i=1,10)
        do 650 j=noy,1,-1
          do 640 i=1,nox
            if (cmax(i,j,l) .lt. 100.) then
              jhrmax(i,j,l) = -9
            endif
  640     continue
          write(61,'(40i3,/,1x,40i3,/,2x,40i3)') (jhrmax(i,j,l),i=1,nox)
  650   continue
  660 continue
C
C
      RETURN
 1000 FORMAT(10A1,60A1,/,I2,1X,I2,1X,I6,F6.0,I6,F6.0)
 1001 FORMAT(10X,10A1/5X,60A1/5X,I10,F10.2,I10,F10.2//)
 1002 FORMAT(4I5)
 1003 FORMAT(10A1)
 1005 FORMAT(5X,2(I10,F10.2))
 1006 FORMAT(I4,10A1)
 1007 FORMAT(9E14.9)
 2001 FORMAT(F10.1,1X,F10.1,1X,I3,F10.1,1X,F10.1,1X,2F6.0,5I4,3F7.0)
      END

      SUBROUTINE AVGSRF (IOUT,IDONE)
C
C     PROGRAM READS A BINARY AVERAGE FILE FROM AN AIRSHED SIM AND
C     WRITES IT OUT IN SURFER FORMAT.
C
C     UNITS FOR ALL CONCENTRATIONS ARE PPM
C
C     FILE 8: DIAGNOSTIC OUTPUT
C     FILE 5: INPUT, ASCII AIRSHED AVERAGE CONCENTRATION FILE
C     FILE 9: OUTPUT, BINARY AIRSHED AVERAGE CONCENTRATION FILE
C
      include 'nampar.cmd'
      COMMON /LCM/ CONC(MXX,MXY)
c
      data ione /1/
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
      IF (NOSPEC .GT. MXSPC .OR. NOX .GT. MXX .OR. NOY .GT. MXY) THEN
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
      xutm1 = (xorg + 1.5*deltax)/1000.
      xutm2 = (xorg + (float(noxg)-1.5)*deltax)/1000.
      yutm1 = (yorg + 1.5*deltay)/1000.
      yutm2 = (yorg + (float(noyg)-1.5)*deltay)/1000.
      nxw = noxg-2
      nyw = noyg-2
       DO 500 LL=1,9999
          READ(7,END=999) IBGDAT, BEGTIM, IENDAT, ENDTIM
          WRITE(IOUT,1005) IBGDAT,BEGTIM,IENDAT,ENDTIM
C
C   LOOP OVER SPECIES, ORDER OF SPECIES IS ESTABLISHED IN CHEMPARAM FILE
C
          DO 400 ISPEC=1,NOSPEC
C
C    LOOP OVER NUMBER OF VERTICAL LEVELS IN SIMULATION GRID
C
             DO 401 IZ=1,NOZG
                WRITE(9,'(a)') 'DSAA'
                write(9,'(i3,1x,i3)') nxw, nyw
                write(9,'(f10.3,1x,f10.3)') xutm1, xutm2
                write(9,'(f10.3,1x,f10.3)') yutm1, yutm2
                READ (7) ISEG, ifile, ((CONC(I,J),I=1,NOX),J=1,NOY)
                do j=noy/2,1,-1
                  do i=nox/2,1,-1
                    if (conc(i,j) .eq. -999.) then
                      conc(i,j) = conc(i+1,j)
                    endif
                    if (conc(i,j) .eq. -999.) then
                      do ii=1,nox
                        if (conc(ii,j) .eq. -999.) 
     &                              conc(ii,j) = conc(ii,j+1)
                      enddo
                    endif
                  enddo
                  do i=nox/2,nox
                    if (conc(i,j) .eq. -999.) then
                      conc(i,j) = conc(i-1,j)
                    endif
                    if (conc(i,j) .eq. -999.) then
                      do ii=1,nox
                        if (conc(ii,j) .eq. -999.) 
     &                               conc(ii,j) = conc(ii,j+1)
                      enddo
                    endif
                  enddo
                enddo
                do j=noy/2,noy
                  do i=nox/2,1,-1
                    if (conc(i,j) .eq. -999.) then
                      conc(i,j) = conc(i+1,j)
                    endif
                    if (conc(i,j) .eq. -999.) then
                      do ii=1,nox
                        if (conc(ii,j) .eq. -999.) 
     &                            conc(ii,j) = conc(ii,j-1)
                      enddo
                    endif
                  enddo
                  do i=nox/2,nox
                    if (conc(i,j) .eq. -999.) then
                      conc(i,j) = conc(i-1,j)
                    endif
                    if (conc(i,j) .eq. -999.) then
                      do ii=1,nox
                        if (conc(ii,j) .eq. -999.) 
     &                            conc(i,j) = conc(i,j-1)
                      enddo
                    endif
                  enddo
                enddo
                call maxmin(mxx, mxy, nox, noy, conc, zmax, zmin)
                write(9,'(f10.2,1x,f10.2)') zmin, zmax
                do 399 j=2,noy-1
c
c    format for this write is set up as 200f10.2, which should work
c    fine for O3 in ppb
c
                  WRITE (9,1007) (CONC(I,J),I=2,NOX-1)
399             continue
401          CONTINUE
400       CONTINUE
500    CONTINUE
C
C
  999 RETURN
 1000 FORMAT(10A1,60A1,/,I2,1X,I2,1X,I6,F6.0,I6,F6.0)
 1001 FORMAT(10X,10A1/5X,60A1/5X,I10,F10.2,I10,F10.2//)
 1002 FORMAT(4I5)
 1003 FORMAT(10A1)
 1005 FORMAT(5X,2(I10,F10.2))
 1006 FORMAT(I4,10A1)
 1007 FORMAT(200f10.2)
 2001 FORMAT(F10.1,1X,F10.1,1X,I3,F10.1,1X,F10.1,1X,2F6.0,5I4,3F7.0)
      END
      subroutine maxmin(mxd,myd,nox,noy,conc,zmax,zmin)
c
c   find max and min value in array conc
c
      dimension conc(mxd,myd)
c
      zmax = -1.e20
      zmin = 1.e20
c
      do 50 j=2,noy-1
        do 40 i=2,nox-1
c         if (conc(i,j) .le. 0.) conc(i,j) = 0.
          zmax = amax1(zmax,conc(i,j))
          zmin = amin1(zmin,conc(i,j))
   40   continue
   50 continue
      return
      end

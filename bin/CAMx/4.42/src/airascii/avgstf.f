      SUBROUTINE AVGSTF (IOUT,IDONE,TOFF,AFAC,BFAC)
C
C     PROGRAM READS A BINARY AVERAGE FILE FROM AN AIRSHED SIM AND
C     WRITES IT OUT IN SIMPLE TEXT FILE FORMAT FOR SCIAN
C
C     UNITS FOR ALL CONCENTRATIONS ARE PPM
C
C     FILE 8: DIAGNOSTIC OUTPUT
C     FILE 5: INPUT, ASCII AIRSHED AVERAGE CONCENTRATION FILE
C     FILE 9: OUTPUT, BINARY AIRSHED AVERAGE CONCENTRATION FILE
C
      include 'nampar.cmd'
      COMMON /LCM/ CONC(MXX,MXY)
      character*2048 mout
      character*80 lunit
c
      data ione /1/
      data izero /0/
C
C     ***  READ THE FOUR HEADER RECORDS
C
      IDONE=1
      READ (7)ifile,note,NOSEG,NOSPEC,IDATB,TIMBEG,JDATE,TIMEND
      WRITE(IOUT,1001) ifile,note,IDATB,TIMBEG,JDATE,TIMEND
C
      READ (7) XREF,YREF,IUTM,XORG,YORG,DELTAX,DELTAY,NOXG,
     $ NOYG,NOZG,NCELL1,NCELL2,SURFHT,HTMIN1,HTMIN2
c
c  modify region definition if this is an "extract"ed file
c
c     WRITE(lunit,'(8a1)')(NOTE(I),I=1,8)
c     IF (LUNIT(1:2).eq.'XY') THEN
c       IS1=NOTE(11)
c       IS2=NOTE(12)
c       JS1=NOTE(13)
c       JS2=NOTE(14)
c       NOXG=IS2-IS1+1
c       NOYG=JS2-JS1+1
c       XORG=XORG+FLOAT(IS1-1)*DELTAX
c       YORG=YORG+FLOAT(JS1-1)*DELTAY
c     ENDIF
C
      READ  (7) ILOCX,ILOCY, NNOX,NNOY
C
      IF (NOSPEC .GT. MXSPC .OR. NOXG .GT. MXX .OR. NOYG .GT. MXY) THEN
        WRITE(*,'(A,/,A)') ' PROGRAM ARRAY DIMENSIONS EXCEEDED',
     $                     ' CHECK GRID SIZE'
        STOP
      ENDIF
C
      READ  (7) ((MSPEC(I,J),I=1,10),J=1,NOSPEC)
      l1 = 10
      do n=10,1,-1
        if (mspec(n,1) .eq. ' ') l1 = n-1
      enddo
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
             WRITE(9,'(a,60a1)') 'NAME ',(mspec(n,1),n=1,l1)
             WRITE(9,'(a,f10.0)') 'TIME ',BEGTIM+TOFF
             if (nozg .gt. 1) then
               write(9,'(a)') 'RANK 3'
               write(9,'(a,i3,1x,i3,1x,i3)') 
     &                  'DIMENSIONS ',nxw, nyw, nozg
               write(9,'(a,i3,1x,i3,1x,i3,1x,i3,1x,i3,1x,i3)') 
     &              'BOUNDS ', izero, nxw, izero, nyw, izero, nozg
             else
               write(9,'(a)') 'RANK 2'
               write(9,'(a,i3,1x,i3)') 'DIMENSIONS ',nxw, nyw
               write(9,'(a,i3,1x,i3,1x,i3,1x,i3)') 
     &              'BOUNDS ', izero, nxw, izero, nyw
             endif
             write(9,'(a)') 'SCALAR'
             write(9,'(a)') 'DATA'
             DO 401 IZ=1,NOZG
                READ (7) ISEG, ifile, ((CONC(I,J),I=1,NOXG),J=1,NOYG)
                call mxmn2(mxx, mxy, noxg, noyg, conc, zmax, zmin)
                do 399 j=2,noyg-1
c
c    format for this write is set up as 200f10.2, which should work
c    fine for O3 in ppb
c
                  do 398 i=2,noxg-1
                    if (conc(i,j) .ne. -999.)
     &                              conc(i,j) = afac*conc(i,j) + bfac
                    n2 = (i-1)*10
                    n1 = n2 - 9
                    if (conc(i,j) .eq. -999.) then
                      write(mout(n1:n2),'(a10)') '   MISSING'
                    else
                      WRITE (mout(n1:n2),'(1x,f9.2)') CONC(I,J)
                    endif
398               continue
                  write(9,'(a)') mout(1:n2)
399             continue
401          CONTINUE
             write(9,'(a)') 'END'
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
 2001 FORMAT(2(F16.5,1X),I3,1X,4(F16.5,1X),5I4,3F7.0)
      END
      subroutine mxmn2(mxd,myd,nox,noy,conc,zmax,zmin)
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
          if (conc(i,j) .ge. 0.) then
            zmax = amax1(zmax,conc(i,j))
            zmin = amin1(zmin,conc(i,j))
          endif
   40   continue
   50 continue
      return
      end

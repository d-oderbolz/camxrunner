C  THIS PROGRAM CONVERTS ALL BINARY AIRSHED FILES TO FORMATTED ASCII
C   FOR AIRSHED OR PARIS MODEL SIMULATIONS
C
C      INPUT ON UNIT 5
C      OUTPUT ON UNIT 9
C      DIAGNOSTICS ON UNIT 8
C
C   Slightly changed by Daniel Oderbolz for non-interactive operation
C
C   Call it like this: airascii source dest type
C   e.g:               airascii terrain_domain1_bx3_lucamx.bin terrain_domain1_bx3_lucamx.bin.asc TERRAIN

      character*255 source
      character*255 dest

      include 'nampar.cmd'
      COMMON /LCM/ EMOB (MXX,MXY)
      CHARACTER*255 IPATH1,IPATH2,IPATH3,IZERO
      CHARACTER*10 mfile
      IZERO='6'
      
c---  Read arguments   
c---  This is pgf77 specific code!     
      call getarg(1,IPATH1)
      call getarg(2,IPATH2)
      call getarg(3,mfile)
      
      
C      WRITE (*,101)
C      READ (*,105) mfile
C      WRITE (*,102)
C      READ (*,100) IPATH1
C
      IF (mfile.EQ. 'AVG-SURFER') THEN
C        WRITE (*,106)
C        READ (*,100) IPATH2
      ELSE IF (mfile.EQ. 'AVG-SCIAN ') THEN
        WRITE (*,107)
        READ (*,100) IPATH2
        II=INDEX(IPATH2,' ')
        READ(IPATH2(II:80),'(3F10.0)') TOFF,AFAC,BFAC
        if (ipath2(II+10:II+20) .eq. '          ') AFAC = 1.
        IPATH2 = IPATH2(1:II-1)
      ELSE
C        WRITE (*,103)
C        READ (*,100) IPATH2
      ENDIF
C
C      WRITE (*,104)
C      READ (*,100) IPATH3
C DCO: Send log to stdout
      IPATH3=IZERO

  100 FORMAT (A)
  101 FORMAT ('ENTER FILE TYPE TO BE CONVERTED (USE AIRSHED CONVENTION)
     1')
  102 FORMAT ('ENTER BINARY INPUT FILE TO BE CONVERTED')
  103 FORMAT ('ENTER NAME OF FORMATTED OUTPUT FILE')
  104 FORMAT ('ENTER NAME FOR DIAGNOSTIC FILE (6 FOR TTY OUTPUT)')
  105 FORMAT (A)
  106 FORMAT ('ENTER NAME OF FORMATTED OUTPUT (.grd) FILE')
  107 FORMAT ('ENTER NAME OF FORMATTED OUTPUT (.stf) FILE',/,
     &        'AND HOUR OFFSET IF DESIRED')
C
      OPEN (7,FILE=IPATH1,FORM='UNFORMATTED',STATUS='OLD')
      OPEN (9,FILE=IPATH2)
      IF (IPATH3.NE.IZERO) OPEN (8,FILE=IPATH3)
      IOUT=6
      IF (IPATH3.NE.IZERO) IOUT=8
C
      IDONE=0
      IF (mfile.EQ. 'AIRQUALITY') CALL AIRCON (IOUT,IDONE)
      IF (mfile.EQ. 'AVERAGE   ') CALL AVGCON (IOUT,IDONE)
      IF (mfile.EQ. 'AVG-SURFER') CALL AVGSRF (IOUT,IDONE)
      IF (mfile.EQ. 'AVG-SCIAN ') CALL AVGSTF (IOUT,IDONE,
     &                                         TOFF,AFAC,BFAC)
      IF (mfile.EQ. 'AVG-TIME  ') CALL AVGTIM (IOUT,IDONE)
      IF (mfile.EQ. 'AVERAGE-S ') CALL AVGCNS (IOUT,IDONE)
      IF (mfile.EQ. 'BOUNDARY  ') CALL BNDCON (IOUT,IDONE)
      IF (mfile.EQ. 'BOUNDFORM ') CALL BNDFRM (IOUT,IDONE)
      IF (mfile.EQ. 'CHEMPARAM ') CALL CHECON (IOUT,IDONE)
      IF (mfile.EQ. 'DIFFBREAK ') CALL DFBCON (IOUT,IDONE)
      IF (mfile.EQ. 'EMISSIONS ') CALL EMICON (IOUT,IDONE)
      IF (mfile.EQ. 'HUMIDITY  ') CALL TMPCN3 (IOUT,IDONE)
      IF (mfile.EQ. 'METSCALARS') CALL METCON (IOUT,IDONE)
      IF (mfile.EQ. 'PTSOURCE  ') CALL PTSCON (IOUT,IDONE)
      IF (mfile.EQ. 'REGIONTOP ') CALL REGCON (IOUT,IDONE)
      IF (mfile.EQ. 'SIMCONTROL') CALL SIMCON (IOUT,IDONE)
      IF (mfile.EQ. 'TEMPERATUR') CALL TMPCON (IOUT,IDONE)
      IF (mfile.EQ. 'TEMPER3D  ') CALL TMPCN3 (IOUT,IDONE)
      IF (mfile.EQ. 'TERRAIN   ') CALL TERCON (IOUT,IDONE)
      IF (mfile.EQ. 'TERRAINFRM') CALL TERFRM (IOUT,IDONE)
      IF (mfile.EQ. 'TOPCONC   ') CALL TPCCON (IOUT,IDONE)
      IF (mfile.EQ. 'WIND      ') CALL WNDCON (IOUT,IDONE)
      IF (mfile.EQ. 'WINDDIAG  ') CALL WNDCND (IOUT,IDONE)
      IF (IDONE.EQ.0) WRITE (*,1111) mfile
      IF (IDONE.EQ.0) WRITE (*,1112)
      IF (IDONE.NE.0) WRITE (*,1113) mfile
 1111 FORMAT ('CANNOT FIND FILE CALLED ',A)
 1112 FORMAT ('VALID FILE NAMES ARE:',/,
     1'    AIRQUALITY',/,
     2'    AVERAGE   ',/,
     3'    AVG-SURFER (Input to SURFER display software, .grd file)',/,
     4'    AVG-SCIAN (Input to SciAn display software, .stf file)',/,
     4'    AVG-TIME  (Write time of max concentration)',/,
     5'    AVERAGE-S (Shortened AVERAGE file)',/,
     6'    BOUNDARY  ',/,
     7'    CHEMPARAM ',/,
     8'    DIFFBREAK ',/,
     9'    EMISSIONS ',/,
     X'    HUMIDITY  ',/,
     1'    METSCALARS',/,
     2'    PTSOURCE  ',/,
     3'    REGIONTOP ',/,
     4'    SIMCONTROL',/,
     5'    TEMPERATUR',/,
     6'    TEMPER3D   (3D TEMPERATUR file)',/,
     7'    TERRAIN   ',/,
     8'    TERRAINFRM (Presentable version of TERRAIN)',/,
     9'    TOPCONC   ',/,
     X'    WIND      ',/,
     1'    WINDDIAG   (Diagnostic printout of WIND file)',/,
     2'RERUN THE PROGRAM WITH ONE OF THE ABOVE NAMES')
 1113 FORMAT ('COMPLETED CONVERSION FOR FILE ',A)
      STOP
      END

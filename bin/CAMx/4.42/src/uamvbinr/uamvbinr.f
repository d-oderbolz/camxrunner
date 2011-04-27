C      uamvbinr.f
c      based on uamvascii.f
c      CONVERTS ASCII UAM-V FILES TO BINARY
C
C   Slightly changed by Daniel Oderbolz for non-interactive operation
C
C   Call it like this: uamvbinr source dest type x-dim y-dim z-dim log
C   e.g:               uamvbinr terrain_domain1_bx3_lucamx.asc terrain_domain1_bx3_lucamx.asc TERRAIN 95 79 14 terrain_domain1_bx3_lucamx.log

      include 'uamvbinr.inc'

      CHARACTER*255 InName,OutName,ErrName
      CHARACTER*10 IFILE,cnox,cnoy,cnlayer
      
c---  Read arguments   
c---  This is pgf77 specific code!     
      call getarg(1,InName)
      call getarg(2,OutName)
      call getarg(3,IFILE)
      call getarg(4,cnox)
      call getarg(5,cnoy)
      call getarg(6,cnlayer)
      call getarg(7,ErrName)

c---  convert character data from the command line to integer

      READ (cnox,*) nox
      READ (cnoy,*) noy
      READ (cnlayer,*) nlayer

c      WRITE (*,100) 'ENTER FILE TYPE TO BE CONVERTED: '
c      READ (*,102) IFILE
c      WRITE (*,100) 'ENTER ASCII INPUT FILE TO BE CONVERTED: '
c      READ (*,102) InName
c      WRITE (*,100) 'ENTER NAME OF BINARY OUTPUT FILE: '
c      READ (*,102) OutName
c      WRITE (*,100) 'ENTER NAME FOR DIAGNOSTIC FILE: '
c      READ (*,102) ErrName
c
c      write(*,100) 'ENTER NOX, NOY, and NOZ: '
c      read(*,*) nox, noy, nlayer


      if (nox .gt. mxx .or. noy .gt. mxy) then
        write(*,*) 'Maximum region dimension exceeded.'
        write(*,*) 'Max x by y size is ', mxx, ' by ', mxy
        write(*,*) 'Specified size is ', nox, ' by ', noy, ' by ', nlayer
        stop
      endif


  100 FORMAT ($A)
  101 FORMAT (I4)
  102 FORMAT (A)
 
      OPEN (InNum,FILE=InName,STATUS='OLD')
      OPEN (OutNum,FILE=OutName,FORM='UNFORMATTED')
      OPEN (ErrNum,FILE=ErrName)
 
      IDONE=0
      IF (IFILE .EQ. 'WIND      ')      THEN 
          CALL MDFBIN 
      ELSE IF (IFILE .EQ. 'HEIGHT    ') THEN 
          CALL MIXHTBIN 
      ELSE IF (IFILE .EQ. 'TEMPERATUR') THEN 
          CALL ITEMPBIN 
      ELSE IF (IFILE .EQ. 'H2O       ') THEN 
          CALL IUH2OBIN 
      ELSE IF (IFILE .EQ. 'SURFACE   ') THEN 
          CALL ISURFBIN
      ELSE IF (IFILE .EQ. 'SURFACEN11') THEN 
          CALL ISURFNEW(11)
      ELSE IF (IFILE .EQ. 'SURFACEN26') THEN 
          CALL ISURFNEW(26)
      ELSE IF (IFILE .EQ. 'TERRAIN   ') THEN 
          CALL ITERRBIN 
      ELSE IF (IFILE .EQ. 'VDIFFUSION') THEN 
          CALL IKVBIN 
      ELSE IF (IFILE .EQ. 'INSTANT   ') THEN
          CALL OUTCCBIN
      ELSE IF (IFILE .EQ. 'FINSTANT  ') THEN
          CALL OUTFCBIN
      ELSE IF (IFILE .EQ. 'FAVERAGE  ') THEN
          CALL OUTFABIN
      ELSE
          WRITE (*,1111) IFILE
          WRITE (*,1112)
          STOP
      ENDIF

 1111 FORMAT ('UNKNOWN FILE TYPE ',A)
 1112 FORMAT ('VALID FILE TYPES ARE:',/,
     & 'WIND  ',/,
     & 'HEIGHT',/,
     & 'TEMPERATURE',/,
     & 'H2O',/,
     & 'SURFACE',/,
     & 'TERRAIN',/,
     & 'VDIFFUSION',/,
     & 'INSTANT',/,
     & 'FINSTANT',/,
     & 'FAVERAGE',/,
     X 'RERUN THE PROGRAM WITH ONE OF THE ABOVE NAMES')

      WRITE(*,1113) InName
 1113 FORMAT ('COMPLETED CONVERSION FOR FILE ',A)

      close(InNum)
      close(OutNum)
      close(ErrNum)

      STOP
      END

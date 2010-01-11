c*** RDOZMCH
c
      subroutine rdozmch(iinp, line, nline, lend)
      use filunit
      use rtcmcchm
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Read a chemical mechanism in OZIPM ASCII format used by CAMx CMC
c
c     Assumtions:
c     - species names must begin with a letter
c     - second lines of reactions are identified by charcter 19 being
c       non-numeric
c     - line length < 240
c
c    Copyright 1996-2007
c    ENVIRON International Corporation
c
c     Argument descriptions:
c
c      Inputs:
c       iinp         I      input unit number
c      Outputs:
c       line         C      current input line
c       nline        I      current line number within input file
c       lend         L      end of file found
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    07/06/07   --gyarwood--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer   iinp
      integer   nline
      logical   lend
      character*240 line
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer   istrln
      logical   is1alpha
      logical   is1num
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i, j, k, irxn, nl
      real      drate(MXRX)
      logical   lhit
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      write(idiag,'(//,A)')
     &                ' Reading the Equations section of the IMC file'
      write(idiag, '(A)') '     Note: Reactions expected in OZIP format'
c
c --- Set the names of fixed species
c
      nam_M   = 'M'
      nam_O2  = 'O2'
      nam_N2  = 'N2'
      nam_H2O = 'H2O'
      nam_H2  = 'H2'
      nam_CH4 = 'CH4'
c
c --- read the reactions
c
      irxn = 0
      do nl=1,9999
         read (iinp, '(A)', end=100, err=8000) line
         nline = nline + 1
         if ( istrln(line) .EQ. 0 ) goto 100
         call jstlft( line )
         call toupper( line )
         if ( line(1:8) .EQ. 'REACTION' ) goto 200
         if ( is1num( line(19:19) ) ) then
            irxn = irxn+1
            call rdozlin1 ( irxn, line )
         else
            call rdozlin2 ( irxn, line )
         endif
      enddo
c
 200  continue

      if( nl .gt. -1) then
        print*, 'get RDOZIP to read CAMx rate constants'
        stop
      endif

 100  lend = .true.
      nrxnrtc = irxn
      if( nrxnrtc .EQ. 0 ) goto 8005
      write(idiag,'(A,I5,A)') '     ', nrxnrtc,
     &      ' reactions were found'
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 8000 write(iout,'(//,A)') 
     &     ' Error in RDOZIP reading OZIP format input data'
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8001 write(iout,'(//,A)') 
     &     ' Error in RDOZIP: first reaction has no species'
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8005 write(iout,'(/,2(/,A))')
     &   ' ERROR in RDOZIP:',
     &   ' No reactions were specified'
      call camxerr()
c
      end

c*** RDOZLIN1
c
      subroutine rdozlin1( irxn, line)
      use rtcmcchm
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Read the first line of a reaction in OZIP ASCII format
c
c    Copyright 1996-2007
c    ENVIRON International Corporation
c
c     Argument descriptions:
c      Inputs:
c       irxn         I      number of the reaction
c       line         C      character string containing the reaction
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    07/06/07   --gyarwood--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer irxn
      character*240  line
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer   istrln
      logical   is1alpha
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer      i, j
      real         val
      character*12 param(12), noname
      data noname  /'            '/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- Parse input line to character strings
c
      read( line, '(2A6,A4,A3,A5,5A6,A11,A7)') (param(i),i=1,12)
      do i = 1,12
         call jstlft( param(i) ) 
         call toupper( param(i) )
      enddo
      lblrxn(irxn) = irxn
c
c --- Reactants
c
      nrct(irxn) = 0
      do i = 1,3
         namrct(irxn,i) = noname
         if( is1alpha( param(i) ) ) then
            nrct(irxn) = nrct(irxn)+1
            namrct(irxn,nrct(irxn)) = param(i)
         endif
      enddo
c
c --- Products
c
      nprd(irxn) = 0
      do j = 1,3
         i = (2*j)+4
         val = 0.
         namprd(irxn,j) = noname
         prdcoef(irxn,j) = 0.0D0
         if( is1alpha( param(i) ) ) then
            nprd(irxn) = nprd(irxn)+1
            namprd(irxn,nprd(irxn)) = param(i)
            if( istrln( param(i-1) ) .GT. 0 ) then
               read( param(i-1), * ) val
               prdcoef(irxn,nprd(irxn)) = DBLE(val)
            else
               prdcoef(irxn,nprd(irxn)) = 1.0D0
            endif
         endif
      enddo
c
c --- Initialize remaining products to null
c
      do j = 4,MXPRD
         namprd(irxn,j) = noname
         prdcoef(irxn,j) = 0.0D0
      enddo
c
c --- Rate coefficients
c
      ityprtc(irxn) = 1
      do i = 1,2
         if( istrln( param(i+10) ) .GT. 0 ) then
            read( param(i+10), * ) rkprmrtc(irxn,i)
            ityprtc(irxn) = i
            nrkprm(irxn) = i
         else
            rkprmrtc(irxn,i) = 0.0D0
         endif
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

c*** RDOZLIN2
c
      subroutine rdozlin2( irxn, line)
      use rtcmcchm
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c     Read the second line of a reaction in OZIP ASCII format
c
c    Copyright 1996-2007
c    ENVIRON International Corporation
c
c     Argument descriptions:
c      Inputs:
c       irxn         I      number of the reaction
c       line         C      character string containing the reaction
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    07/06/07   --gyarwood--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      implicit none
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer irxn
      character*240  line
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer   istrln
      logical   is1alpha
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer      i, j
      real         val
      character*12 param(2*(MXPRD-3)), noname
      data noname  /'            '/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- Parse input line to character strings
c     Note: line length of 240 accomodates up to 20 products (23 total)
c
      read( line, '(A5,99A6)') (param(i),i=1,2*(MXPRD-3))
      do i = 1,2*(MXPRD-3)
         call jstlft( param(i) ) 
         call toupper( param(i) )
      enddo
c
c --- Products
c
      do j = 4,MXPRD
         i = 2*(j-3)
         val = 0.
         namprd(irxn,j) = noname
         prdcoef(irxn,j) = 0.0D0
         if( is1alpha( param(i) ) ) then
            nprd(irxn) = nprd(irxn)+1
            namprd(irxn,nprd(irxn)) = param(i)
            if( istrln( param(i-1) ) .GT. 0 ) then
               read( param(i-1), * ) val
               prdcoef(irxn,nprd(irxn)) = DBLE(val)
            else
               prdcoef(irxn,nprd(irxn)) = 1.0D0
            endif
         endif
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

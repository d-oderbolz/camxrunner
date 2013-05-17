c*** RDSCHM
c
      subroutine rdschm(ldbg)
      use filunit
      use tracer
      use rtcmcchm
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     This routine reads a SCICHEM chemical mechanism input file
c     (IMC format file) that defines the chemistry for the RTCMC
c     probing tool.
c
c     Assumtions:
c     - Species names must begin with a letter
c     - Line length < 240
c
c    Copyright 1996 - 2012
c    ENVIRON International Corporation
c
c     Argument descriptions:
c      Outputs:    
c       ldbg         L      flag to turn on diagnostic statements
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
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      logical   ldbg
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer istrln
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i, j, k, nline
      logical   lhit, lend, lcont, lspec, ltabl, lequa
      character*1   letr
      character*8   word
      character*10  spec
      character*240 line
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ldbg = .true.
c
      write(idiag,'(//,A,//)',ERR=7009)
     &       ' ******* Reading RTCMC chemical definitions file.'
c
c  --- open the file ----
c
      open(unit=iorchm,file=chmfil,ERR=7010,status='OLD')
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
c --- Allocate RTCMC variables
c
      call alloc_rtcmc(MXTRSP,MXSPEC,MXRX,
     &                 MXKPRM,MXPHOT,MXZEN,MXRCT,MXPRD,
     &                 MXJACTRM,MXEQM,MXSLO)
c
c --- Identify and read sections of the IMC format input file
c
      lend  = .false.
      lcont = .false.
      lspec = .false.
      ltabl = .false.
      lequa = .false.
      nline = 1
      read (iorchm, '(A)', end=200, err=8000) line
      if( istrln(line) .EQ. 0 ) goto 200

 100  read (line, '(A1)') letr
      if( letr .NE. '#' ) goto 8002
      read (line, '(A1,A6)') letr, word
      call toupper( word )
      if( word .EQ. 'CONTRO  ' ) then
         call rdscctl(iorchm, line, nline, lend)
         lcont = .true.
      elseif( word .EQ. 'SPECIE  ' ) then
         call rdscspc(iorchm, line, nline, lend)
         lspec = .true.
      elseif( word .EQ. 'TABLE   ' ) then
         call rdsctbl(iorchm, line, nline, lend)
         ltabl = .true.
      elseif( word .EQ. 'EQUATI  ' ) then
         if( mtype .EQ. 2 ) then
            call rdscmch(iorchm, line, nline, lend)
         else
            call rdozmch(iorchm, line, nline, lend)
         endif
         lequa = .true.
      else 
         read (iorchm, '(A)', end=200, err=8000) line
         nline = nline + 1
      endif
      if( lend .OR. istrln(line) .EQ. 0 ) goto 200
      goto 100

 200  if( .NOT. lcont ) then
         word = 'Control '
         goto 8003
      elseif( .NOT. lspec ) then
         word = 'Species '
         goto 8003
      elseif( .NOT. ltabl ) then
         word = 'Table   '
         goto 8003
      elseif( .NOT. lequa ) then
         word = 'Equation'
         goto 8003
      endif
c
c --- Check the mechanism
c
c --- Renumber the photolysis table to sequential reaction numbering
c
      write(idiag,'(/,A)')
     & ' Checking the mechanism read by RDSCHM'
      if( njschm .GT. 0 ) then
         do i = 1,njschm
            do j = 1,nrxnrtc
               if( ijschm(i) .EQ. lblrxn(j) ) then
                  k = ijschm(i)
                  ijschm(i) = j
                  if( ityprtc(ijschm(i)) .NE. 100 ) goto 8006
               endif
           enddo
         enddo
         write(idiag,'(/,A)')
     & '    Photolysis reaction labels renumbered as follows:'
         write(idiag,'(2X,12I5)') (ijschm(i),i=1,njschm)
         write(idiag,'(2(/,A))') 
     & '    when the reactions were renumbered sequentially'
      endif
c
c --- Index the species
c
      if ( nrct(1) .GT. 0 ) then
         ngasrtc = 1
         spnmrt(1) = namrct(1,1)
      elseif ( nprd(1) .GT. 0 ) then
         ngasrtc = 1
         spnmrt(1) = namprd(1,1)
      else
         goto 8004
      endif
c   
      do i = 1, nrxnrtc
         if( nrct(i) .GT. 0 ) then
            do j = 1, nrct(i)
               lhit = .false.
               do k = 1, ngasrtc
                  if( namrct(i,j) .EQ. spnmrt(k) ) lhit = .true.
               enddo
               if ( .NOT. lhit ) then
                  ngasrtc = ngasrtc + 1
                  if( ngasrtc .GT. MXTRSP+MXSPEC ) goto 7002
                  spnmrt(ngasrtc) = namrct(i,j)
               endif
            enddo
         endif
         if( nprd(i) .GT. 0 ) then
            do j = 1, nprd(i)
               lhit = .false.
               do k = 1, ngasrtc
                  if( namprd(i,j) .EQ. spnmrt(k) ) lhit = .true.
               enddo
               if ( .NOT. lhit ) then
                  ngasrtc = ngasrtc + 1
                  if( ngasrtc .GT. MXTRSP+MXSPEC ) goto 7002
                  spnmrt(ngasrtc) = namprd(i,j)
               endif
            enddo
         endif
      enddo
c
c --- Set attributes of the species found in the mechanism
c
      do i = 1, ngasrtc
         do j = 1, ngasschm
            if( spnmschm(j) .EQ. nam_M
     &         .OR. spnmschm(j) .EQ. nam_O2
     &           .OR. spnmschm(j) .EQ. nam_N2
     &             .OR. spnmschm(j) .EQ. nam_H2O
     &               .OR. spnmschm(j) .EQ. nam_H2
     &                 .OR. spnmschm(j) .EQ. nam_CH4 ) then
               itypsp(i) = 4
               goto 300
            elseif( spnmrt(i) .EQ. spnmschm(j) ) then
               itypsp(i) = itypschm(j)
               goto 300
            elseif( j .EQ. ngasschm ) then
               spec = spnmrt(i)
               goto 8005
            endif
         enddo
 300     continue
      enddo
c
      if ( ldbg ) then
         write(idiag, '(/,A)') ' Species read by RDSCHM'
         write(idiag, '(/,A)') ' Rxn  Name        Type'
         do i = 1,ngasrtc
            write(idiag, '(I4,2X,A,2X,I2,2X,1PE10.3)') 
     &                            i, spnmrt(i), itypsp(i)
c     &                                             , conschm(i)
         enddo
         write(idiag, '(A)')
     &     '   Types are: 1=fast, 2=slow, 3=eqm, 4=fixed(ambient)'
      endif
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      write(idiag,'(//,A,//)',ERR=7009)
     &       ' ******* Finished RTCMC chemical definitions file.'
      close(iorchm)
      return
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7002 continue
      write(iout,'(//,A)') 'ERROR in RDSCHM:'
      write(iout,'(1X,A)') 'Number of RTRAC species exceeds max.'
      write(iout,'(1X,A,A)') 'Check the input reaction list,',
     &                       ' increase parameter MXTRSP and recompile.'
      call camxerr()
c
 7009 continue
      write(iout,'(//,A)') 'ERROR in RDSCHM:'
      write(iout,'(1X,A)') 'Cannot write to the diag file.'
      call camxerr()
c
 7010 continue
      write(iout,'(//,A)') 'ERROR in RDSCHM:'
      write(iout,'(1X,A)') 'Cannot open file: ',chmfil(:istrln(chmfil))
      call camxerr()
c
 8000 write(iout,'(//,A)') 
     &   ' ERROR in RDSCHM reading SCICHEM IMC format input file'
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8002 write(iout,'(/,4(/,A))') 
     &   ' ERROR in RDSCHM:',
     &   ' Reading the SCICHEM IMC format input file',
     &   ' Expecting this line to begin with the character # :', line
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8003 write(iout,'(/,3(/,A),A)') 
     &   ' ERROR in RDSCHM:',
     &   ' Reading the SCICHEM IMC format input file',
     &   ' Failed to find and read the following section: ', word
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8004 write(iout,'(//,A)')
     &     ' Error in RDSCHM: first reaction has no species'
      call camxerr()
c
 8005 write(iout,'(/,4(/,A))') 
     &   ' ERROR in RDSCHM:',
     &   ' The SCICHEM IMC file contains the following species',
     &   ' in the EQUATION section, but not the SPECIES section:',
     &     spec
      call camxerr()
c
 8006 write(iout,'(/,A,/,A,I5,/,A)') 
     &   ' ERROR in RDSCHM:',
     &   ' Photolysis rate provided for reaction number label:', k,
     &   ' The reaction provided with this label is not photolysis type'
      call camxerr()
c
      end
c
c*** RDSCCTL
c
      subroutine rdscctl(iinp, line, nline, lend)
      use filunit
      use rtcmcchm
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Read the CONTROL section of a SCICHEM chemical mechanism 
c     input file (IMC file).  
c
c     Look for the following SCICHEM keywords and input values:
c        rate_species_units:  ppm or molecules/cm3 (default)
c        rate_time_units:     hours, minutes, or seconds (default)
c     Look for several CAMx specific keywords:
c        solver:   DLS (default), SLS or ROS
c        jacobian: numeric (default) or algebraic
c     Ignore any other keywords
c
c     Assumtions:
c     - A line beginning with character "#" closes this section
c
c    Copyright 1996 - 2012
c    ENVIRON International Corporation
c
c     Argument descriptions:
c      Inputs:
c       iinp         I      input unit number
c      Outputs:
c       line         C      current input line
c       nline        I      currentline number within input file
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
c    External functions:
c-----------------------------------------------------------------------
c
      integer   istrln
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
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i
      character*1   letr
      character*6   word, tunit(3)
      character*14  cunit(2)
      character*24  slver(3), jcban(2)
      data tunit/'second','minute','hour  '/
      data cunit/'molecules/cm-3','ppm           '/
      data slver/'LSODE (double precision)',
     &           'LSODE (single precision)',
     &           'Rosenbrock              '/
      data jcban/'Numeric                 ',
     &           'Algebraic               '/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      write(idiag,'(/,3(/,A))')
     &     ' ----------------------------------------------',
     &     ' Diagnostic information for RTRAC CMC chemistry',
     &     ' ----------------------------------------------'
      write(idiag,'(/,A)') 
     &        ' Reading the Control section of the IMC file'
      write(idiag,'(/,A)') 
     &        '  Scan each line for recognized options'
c
c --- set default values for icunit, itunit, isolv, ijac,
c     rtolrtc, atolrtc
c
      icunit = 1
      itunit = 1
      isolv  = 1
      ijac   = 1
      mtype  = 2
      ktype  = 2
      rtolrtc = 1.0E-5
      atolrtc = 1.0E-18
c
c --- check for non-default values
c
      lend = .false.
 100  read (iinp, '(A)', end=8001, err=8000) line
      nline = nline + 1
      call jstlft( line )
      read (line, '(A6)') word
      call toupper( word )
      read (line, '(A1)') letr

      if( word .EQ. 'RATE_S' ) then
         do i=1,60
            read (line(i:i),'(A1)') letr
            if( letr .EQ. '=' ) then
               read (line(i+1:i+6),'(A6)') word
               call jstlft( word )
               call toupper( word )
               if( word(2:4) .EQ. 'PPM' ) then
                  icunit = 2
               elseif( word(2:4) .EQ. 'MOL' ) then
                  icunit = 1
               else
                  goto 8002
               endif
            endif
         enddo
         write(idiag,'(2A)') '    Recognised line: ',
     &                              line(1:istrln(line))
      elseif( word .EQ. 'RATE_T' ) then
         do i=1,60
            read (line(i:i),'(A1)') letr
            if( letr .EQ. '=' ) then
               read (line(i+1:i+6),'(A6)') word
               call jstlft( word )
               call toupper( word )
               if( word(2:4) .EQ. 'HOU' ) then
                  itunit = 3
               elseif( word(2:4) .EQ. 'MIN' ) then
                  itunit = 2
               elseif( word(2:4) .EQ. 'SEC' ) then
                  itunit = 1
               else
                  goto 8003
               endif
            endif
         enddo
         write(idiag,'(2A)') '    Recognised line: ',
     &                              line(1:istrln(line))
      elseif( word(1:4) .EQ. 'RTOL' ) then
         do i=1,60
            read (line(i:i),'(A1)') letr
            if( letr .EQ. '=' ) then
               read (line(i+1:i+16),*,err=8004) rtolrtc
            endif
         enddo
         write(idiag,'(2A)') '    Recognised line: ',
     &                              line(1:istrln(line))
      elseif( word(1:4) .EQ. 'ATOL' ) then
         do i=1,60
            read (line(i:i),'(A1)') letr
            if( letr .EQ. '=' ) then
               read (line(i+1:i+16),*,err=8005) atolrtc
            endif
         enddo
         write(idiag,'(2A)') '    Recognised line: ',
     &                              line(1:istrln(line))
      elseif( word .EQ. 'SOLVER' ) then
         do i=1,60
            read (line(i:i),'(A1)') letr
            if( letr .EQ. '=' ) then
               read (line(i+1:i+6),'(A6)') word
               call jstlft( word )
               call toupper( word )
               if( word(2:4) .EQ. 'DLS' ) then
                  isolv = 1
               elseif( word(2:4) .EQ. 'SLS' ) then
                  isolv = 2
               elseif( word(2:4) .EQ. 'ROS' ) then
                  isolv = 3
               endif
            endif
         enddo
         write(idiag,'(2A)') '    Recognised line: ',
     &                              line(1:istrln(line))
      elseif( word .EQ. 'JACOBI' ) then
         do i=1,60
            read (line(i:i),'(A1)') letr
            if( letr .EQ. '=' ) then
               read (line(i+1:i+6),'(A6)') word
               call jstlft( word )
               call toupper( word )
               if( word(2:4) .EQ. 'NUM' ) then
                  ijac = 1
               elseif( word(2:4) .EQ. 'ALG' ) then
                  ijac = 2
               endif
            endif
         enddo
         write(idiag,'(2A)') '    Recognised line: ',
     &                              line(1:istrln(line))
      elseif( word .EQ. 'EQUATI' ) then
         do i=1,60
            read (line(i:i),'(A1)') letr
            if( letr .EQ. '=' ) then
               read (line(i+1:i+6),'(A6)') word
               call jstlft( word )
               call toupper( word )
               if( word(2:4) .EQ. 'CAM' ) then
                  mtype  = 1
                  ktype  = 1
               endif
            endif
         enddo
         write(idiag,'(2A)') '    Recognised line: ',
     &                              line(1:istrln(line))
      elseif( letr .EQ. '#' ) then
         goto 200
      else
         write(idiag,'(2A)') '    Ignored line: ',
     &                              line(1:istrln(line))
      endif
      goto 100
c
c --- report options selected and then return
c
 200  write(idiag,'(/,2(2A,/))')
     &     '    The chemistry solver will be: ', slver(isolv),
     &     '    The Jacobian method will be:  ', jcban(ijac)
      write(idiag,'(2(A,1PE10.3,/))')
     &     '    The relative error tolerance will be: ', rtolrtc,
     &     '    The absolute error tolerance will be: ', atolrtc
      write(idiag,'(4A,/)')
     &     '    Rate constants must be input with units: ',
     &       cunit(icunit)(1:istrln(cunit(icunit))),'/',tunit(itunit)
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
     &   ' ERROR in RDSCCTL reading SCICHEM IMC format input file'
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8001 write(iout,'(/,2(/,A))')
     &   ' ERROR in RDSCCTL:',
     &   ' Unexpected end of SCICHEM IMC format input file'
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8002 write(iout,'(/,4(/,A))')
     &   ' ERROR in RDSCCTL:',
     &   ' Reading the SCICHEM IMC format input file',
     &   ' Failed to recognise the concentration units in: ', line
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8003 write(iout,'(/,4(/,A))')
     &   ' ERROR in RDSCCTL:',
     &   ' Reading the SCICHEM IMC format input file',
     &   ' Failed to recognise the time units in: ', line
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8004 write(iout,'(/,4(/,A))')
     &   ' ERROR in RDSCCTL:',
     &   ' Reading the SCICHEM IMC format input file',
     &   ' Failed to recognise value for RTOL in: ', line
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8005 write(iout,'(/,4(/,A))')
     &   ' ERROR in RDSCCTL:',
     &   ' Reading the SCICHEM IMC format input file',
     &   ' Failed to recognise value for ATOL in: ', line
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
      end

c*** RDSCSPC
c
      subroutine rdscspc(iinp, line, nline, lend)
      use filunit
      use rtcmcchm
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Read the SPECIES section of a SCICHEM chemical mechanism 
c     input file (IMC file)
c
c     Assumtions:
c     - Species names must begin with a letter
c     - A line beginning with character "#" closes this section
c
c    Copyright 1996 - 2012
c    ENVIRON International Corporation
c
c     Argument descriptions:
c      Inputs:
c       iinp         I      input unit number
c      Outputs:
c       line         C      current input line
c       nline        I      currentline number within input file
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
      include 'camx.prm'
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
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   nl, isp
      real atol
      character*1   letr
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      write(idiag,'(//,A)') 
     &            ' Reading the Species section of the IMC file'
c
      isp = 0
      lend = .false.

 100  read (iinp, '(A)', end=8001, err=8000) line
      nline = nline + 1
      call jstlft( line )
      read (line, '(A1)') letr
      if( letr .EQ. '#' ) then
         goto 200
      else
         isp = isp+1
         if( isp .GT. MXTRSP+MXSPEC ) goto 7002
         read(line, *) spnmschm(isp), letr, conschm(isp), atol,
     &                 depvschm(isp), wetschm(isp)
         if( letr .EQ. 'F' ) then
            itypschm(isp) = 1
         elseif( letr .EQ. 'S' ) then
            itypschm(isp) = 2
         elseif( letr .EQ. 'E' ) then
            itypschm(isp) = 3
         elseif( letr .EQ. 'A' ) then
            itypschm(isp) = 4
         else
            goto 8002
         endif
      endif
      goto 100

 200  ngasschm = isp
      if( ngasschm .EQ. 0 ) goto 8003
      write(idiag,'(A,I5,A)') '     ', ngasschm,
     &      ' species were found'
      do nl = 1,ngasschm
        write(idiag,'(a,a)') '     ',spnmschm(nl)
      enddo
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
 7002 continue
      write(iout,'(//,A)') 'ERROR in RDSCSPC:'
      write(iout,'(1X,A)') 'Number of RTRAC species exceeds max.'
      write(iout,'(1X,A,A)') 'Check the input species list,',
     &                       ' increase parameter MXTRSP and recompile.'
      call camxerr()
c
 8000 write(iout,'(//,A)')
     &   ' ERROR in RDSCSPC reading SCICHEM IMC format input file'
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8001 write(iout,'(/,2(/,A))')
     &   ' ERROR in RDSCSPC:',
     &   ' Unexpected end of SCICHEM IMC format input file'
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8002 write(iout,'(/,3(/,A))')
     &   ' ERROR in RDSCSPC:',
     &   ' Species type not recognized in: ', line
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8003 write(iout,'(/,2(/,A))')
     &   ' ERROR in RDSCSPC:',
     &   ' No species were specified'
      call camxerr()
c
      end

c*** RDSCTBL
c
      subroutine rdsctbl(iinp, line, nline, lend)
      use filunit
      use rtcmcchm
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c     Read the TABLE section of a SCICHEM chemical mechanism 
c     input file (IMC file) dealing with photolysis
c
c     Assumtions:
c     - A line beginning with character "#" closes this section
c
c    Copyright 1996 - 2012
c    ENVIRON International Corporation
c
c     Argument descriptions:
c      Inputs:
c       iinp         I  input unit number
c      Outputs:
c       line         C  current input line
c       nline        I  currentline number within input file
c       lend         L  end of file found
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
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i, j, k, idum, nrxn
      real      rdum(MXZEN)
      character*1   letr
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      write(idiag,'(//,A)')
     &         ' Reading the Table section of the IMC file'
      lend = .false.
c
c --- Read zenith angles
c     Determine how many zenith angles are being used
c     Code may be fragile because input file format is weakly defined 
c
      do i = 1,MXZEN
         zenschm(i) = 90.0
      enddo
      read (iinp, '(A)', end=8002, err=8000) line
      nline = nline + 1
      call jstlft( line )
      if( istrln(line) .EQ. 0 ) goto 8002
      read (line, * ) j
      if( j .NE. 0 ) goto 8002
      do i = 1,MXZEN-1
         read(line, * , err = 100, end = 100 ) 
     &                         idum, (rdum(k),k=1,i)
         j = i
      enddo
 100  nzschm = j
      read(line, *) idum, (zenschm(i),i=1,nzschm)
c
c --- Read photolysis rates
c
      nrxn = 0

 101  read (iinp, '(A)', end=200, err=8000) line
      nline = nline + 1
      call jstlft( line )
      if( istrln(line) .EQ. 0 ) goto 200
      read (line, '(A1)' ) letr
      if( letr .EQ. '#' ) goto 300
      nrxn = nrxn+1
      if( nrxn .GT. MXPHOT ) goto 8004
      read(line, *) ijschm(nrxn), (rjschm(i,nrxn),i=1,nzschm)
      if( ijschm(nrxn) .GT. MXRX ) goto 8003
      goto 101

 200  lend = .true.
 300  njschm = nrxn
c
      if( njschm .EQ. 0 ) then
         write(idiag,'(A)') 
     &      '    No photolysis rates were specified'
      else
         do i = 1,njschm-1
            k = i
            do j = i+1,njschm
               if( ijschm(i) .EQ. ijschm(j) ) goto 8005
            enddo
         enddo
c
         write(idiag,'(A,/,2X,10F6.2)') 
     & '    Photolysis rates specified at the following zenith angles:',
     &                                   (zenschm(i),i=1,nzschm)
         write(idiag,'(A)') 
     &      '    for the following reaction label numbers:'
         write(idiag,'(2X,12I5)') (ijschm(i),i=1,njschm)
         write(idiag,'(2(/,A))') 
     &      '    NOTE: These label numbers may change when the',
     &      '          mechanism reactions are renumbered sequentially'
      endif
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
     &   ' ERROR in RDSCTBL reading SCICHEM IMC format input file'
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8002 write(iout,'(/,2(/,A))')
     &   ' ERROR in RDSCTBL:',
     &   ' Failed to find the list of zenith angles for photolysis'
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8003 write(iout,'(/,2(/,A),I5,/,A)')
     &   ' ERROR in RDSCTBL:',
     &   ' Reaction number in photolysis table exceeds MXRX=', 
     &     MXRX, line
      write(iout,'(A,I5)') ' At input line ', nline
      write(iout,'(1X,A,A)') 'Check the input pholoysis list,',
     &                       ' increase parameter MXRX and recompile.'
      call camxerr()
c
 8004 write(iout,'(/,2(/,A),I5,/,A)')
     &   ' ERROR in RDSCTBL:',
     &   ' Number of photolysis reactions exceeds MXPHOT=', 
     &     MXPHOT, line
      write(iout,'(A,I5)') ' At input line ', nline
      write(iout,'(1X,A,A)') 'Check the input pholoysis list,',
     &                       ' increase parameter MXPHOT and recompile.'
      call camxerr()
c
 8005 write(iout,'(/,2(/,A),I5)')
     &   ' ERROR in RDSCTBL:',
     &   ' This photolysis reaction label is used more than once: ',
     &     ijschm(k)
      call camxerr()
c
      end

c*** RDSCMCH
c
      subroutine rdscmch(iinp, line, nline, lend)
      use filunit
      use rtcmcchm
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     Read the EQUATIONS section of a SCICHEM chemical mechanism 
c     input file (IMC file)
c
c     Assumtions:
c     - This is the last section of ythe IMC file
c     - End of file or blank line closes this section
c
c    Copyright 1996 - 2012
c    ENVIRON International Corporation
c
c     Argument descriptions:
c      Inputs:
c       iinp         I      input unit number
c     Outputs:
c       line         C      current input line
c       nline        I      currentline number within input file
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
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i, j, k, nsp, nrxn, l1, l2, l3
      integer   lr1(MXRCT), lr2(MXRCT), lp1(MXPRD), lp2(MXPRD)
      integer   lpc1(MXPRD), lpc2(MXPRD)
      real      tmp, rdum(MXKPRM)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      write(idiag,'(//,A)')
     &                ' Reading the Equations section of the IMC file'
c
      lend = .false.
      nrxn = 0

 101  read (iinp, '(A)', end=900, err=8000) line
      nline = nline + 1
      call jstlft( line )
      l3 = istrln( line )
      if( line(1:1) .EQ. '#' ) goto 1000
      if( l3 .EQ. 0 ) goto 900
      nrxn = nrxn + 1
      if( nrxn .GT. MXRX ) goto 8008
      read(line, *) lblrxn(nrxn)
c
c --- find reactants
c
      nsp = 0
      l1 = 0
      do i = 1,l3
         if( line(i:i) .EQ. '[' ) then
            nsp = nsp +1
            lr1(nsp) = i+1
         elseif( line(i:i) .EQ. ']' ) then
            lr2(nsp) = i-1
         elseif( line(i:i) .EQ. '>' ) then
            l1 = i-1
            goto 100
         endif
      enddo
 100  nrct(nrxn) = nsp
      if( l1 .EQ. 0 ) goto 8001
      if( nsp .GT. 0 ) then
         do i = 1,nsp
            read( line(lr1(i):lr2(i)), '(A)') namrct(nrxn,i)
         enddo
      endif
c
c --- find products and product coefficients
c
      nsp = 0
      l2 = 0
      lpc1(1) = 0
      do i = l1+1,l3
         if( line(i:i) .EQ. '[' ) then
            nsp = nsp +1
            lp1(nsp) = i+1
            lpc1(nsp+1) = 0
         elseif( line(i:i) .EQ. ']' ) then
            lp2(nsp) = i-1
         elseif( line(i:i) .EQ. '(' ) then
            lpc1(nsp+1) = i+1
         elseif( line(i:i) .EQ. ')' ) then
            lpc2(nsp+1) = i-1
         elseif( line(i:i) .EQ. ';' ) then
            l2 = i-1
            goto 200
         endif
      enddo
 200  nprd(nrxn) = nsp
      if( l2 .EQ. 0 ) goto 8002
      if( nsp .GT. 0 ) then
         do i = 1,nsp
            read( line(lp1(i):lp2(i)), '(A)' ) namprd(nrxn,i)
            call jstlft( namprd(nrxn,i) )
            if( lpc1(i) .GT. 0 ) then
               read(line(lpc1(i):lpc2(i)), *) tmp
               prdcoef(nrxn,i) = DBLE(tmp)
            else
               prdcoef(nrxn,i) = 1.0D0
            endif
         enddo
      endif
c
c --- Read rate constants 
c     Code may be fragile because input file format is weakly defined 
c     Determine how many parameters were input for each reaction
c
      j = 0
      do i = 1,MXKPRM
         rkprmrtc(nrxn,i) = 0.0
      enddo
      read( line(l2+2:l3), * ) ityprtc(nrxn)
      do i = 1,MXKPRM
         read( line(l2+2:l3), * , err = 300, end = 300 ) 
     &                         ityprtc(nrxn), (rdum(k),k=1,i)
         j = i
      enddo
 300  nrkprm(nrxn) = j
      read( line(l2+2:l3), * ) 
     &                ityprtc(nrxn), (rkprmrtc(nrxn,k),k=1,j)
      if( ityprtc(nrxn) .LT. 0 .OR. ityprtc(nrxn) .GT. 17) goto 8007
c
c --- Offset SCICHEM rate expression types by +100 
c
      ityprtc(nrxn) = 100 + ityprtc(nrxn)
c
c --- Express any reactants embedded into SCICHEM rate constants
c     and perform other changes or checks
c
      if( ityprtc(nrxn) .EQ. 104 ) then
         nrct(nrxn) = nrct(nrxn) + 1
         namrct(nrxn,nrct(nrxn)) = nam_H2O
      elseif( ityprtc(nrxn) .EQ. 105 ) then
         nrct(nrxn) = nrct(nrxn) + 1
         namrct(nrxn,nrct(nrxn)) = nam_M
      elseif( ityprtc(nrxn) .EQ. 109 
     &   .OR. ityprtc(nrxn) .EQ. 112 ) then
         nrct(nrxn) = nrct(nrxn) + 1
         namrct(nrxn,nrct(nrxn)) = nam_O2
      elseif( ityprtc(nrxn) .EQ. 110 ) then
         nrct(nrxn) = nrct(nrxn) + 1
         namrct(nrxn,nrct(nrxn)) = nam_N2
      elseif( ityprtc(nrxn) .EQ. 114 ) then
         nrct(nrxn) = nrct(nrxn) + 1
         namrct(nrxn,nrct(nrxn)) = nam_CH4
      elseif( ityprtc(nrxn) .EQ. 115 ) then
         nrct(nrxn) = nrct(nrxn) + 1
         namrct(nrxn,nrct(nrxn)) = nam_H2O
      elseif( ityprtc(nrxn) .EQ. 108 ) then
         if( NINT(rkprmrtc(nrxn,1)) .GE. nrxn ) goto 8003
      elseif( ityprtc(nrxn) .EQ. 106 ) then
         goto 8004
      endif
      goto 101
c
c --- End loop over input reactions
c
 900  lend = .true.
 1000 nrxnrtc = nrxn
      if( nrxnrtc .EQ. 0 ) goto 8005
      write(idiag,'(A,I5,A)') '     ', nrxnrtc,
     &      ' reactions were found'
c
c --- Check for duplicate reaction number labels
c
      do i = 1,nrxnrtc-1
         k = i
         do j = i+1,nrxnrtc
            if( lblrxn(i) .EQ. lblrxn(j) ) goto 8006
         enddo
      enddo
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
     &   ' ERROR in RDSCMCH reading SCICHEM IMC format input file'
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8001 write(iout,'(/,3(/,A))')
     &   ' ERROR in RDSCMCH reading SCICHEM IMC format input file',
     &   ' Failed to find the token > in reaction:', 
     &     line(1:istrln(line))
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8002 write(iout,'(/,3(/,A))')
     &   ' ERROR in RDSCMCH reading SCICHEM IMC format input file',
     &   ' Failed to find the token ; in reaction:',
     &     line(1:istrln(line))
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8003 write(iout,'(/,4(/,A))')
     &   ' ERROR in RDSCMCH reading SCICHEM IMC format input file',
     &   ' Equilibrium rate constant specified in reaction:',
     &     line(1:istrln(line)),
     &   ' illegaly points to a reaction later in the mechansim'
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8004 write(iout,'(/,3(/,A))')
     &   ' ERROR in RDSCMCH reading SCICHEM IMC format input file',
     &   ' Rate expression type 6 is illegal in:',
     &     line(1:istrln(line))
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8005 write(iout,'(/,2(/,A))')
     &   ' ERROR in RDSCMCH:',
     &   ' No reactions were specified'
      call camxerr()
c
 8006 write(iout,'(/,2(/,A),I5)')
     &   ' ERROR in RDSCMCH:',
     &   ' This reaction label number is used more than once: ',
     &     lblrxn(k)
      call camxerr()
c
 8007 write(iout,'(/,3(/,A))')
     &   ' ERROR in RDSCMCH reading SCICHEM IMC format input file',
     &   ' Rate expression type not in range 0 to 17 in:',
     &     line(1:istrln(line))
      write(iout,'(A,I5)') ' At input line ', nline
      call camxerr()
c
 8008 write(iout,'(/,2(/,A),I5,/,A)')
     &   ' ERROR in RDSCMCH:',
     &   ' Number of reactions exceeds MXRX=', 
     &     MXRX, line
      write(iout,'(A,I5)') ' At input line ', nline
      write(iout,'(1X,A,A)') 'Check the input equation list,',
     &                       ' increase parameter MXRX and recompile.'
      call camxerr()
c
      end

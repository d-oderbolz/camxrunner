c**** RDCHMRT
c
      subroutine rdchmrt(version)
      use filunit
      use grid
      use ptemiss
      use chmstry
      use pigsty
      use rtracchm
      use tracer
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c      This routine reads the chemical parameters file for RTRAC 
c      technology of the source apportionment algorithm.  
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c       Argument description:
c          Input:
c        version      C      model version character string
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     01/16/02   --gwilson--    Original development
c     11/12/03   --cemery--     Added secondary RTRAC species to primary
c                               RTRAC species
c     10/29/09   --cemery--     Added RTRAC surface model parameters
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
      include 'rtracsrf.inc'
c
c-----------------------------------------------------------------------
c    External functions:
c-----------------------------------------------------------------------
c
      integer istrln
c
c-----------------------------------------------------------------------
c    Argument declaration:
c-----------------------------------------------------------------------
c
      character*20 version
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer mvecedge
      real    rkoh, rko3, rkno3, adj
      character*200 line
      character*80  action
      character*10  camxv, camxvin, sptmp
c
      character*6 flag(MXTRSP)
      character*6 sectmp(MXTRSP)
c
c-----------------------------------------------------------------------
c    Data statements:
c-----------------------------------------------------------------------
c
      data camxv /'VERSION5.4'/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- initalize some variables ---
c
      lrtgas = .TRUE.      
      lrtaero = .TRUE.      
      write(idiag,'(//,A,//)',ERR=7009) 
     &       ' ******* Reading RTRAC chemical definitions file.'
c
c  --- open the file ----
c
      open(unit=iorchm,file=chmfil,ERR=7010,status='OLD')
c
c  --- read and verify the version number ---
c
      action = 'Reading version number in RTRAC Chemistry file.'
      read(iorchm,'(A)',ERR=7000,END=7001) line     
      camxvin = line(21:30)
      write(idiag,'(A)',ERR=7009) line(:istrln(line))
      call jstlft( camxvin )
      call toupper( camxvin )
      if (camxvin .NE. camxv ) then
        write(iout,'(//,A)') 'ERROR in RDCHMRT:'
        write(iout,'(/,A)') 
     &               ' CAMx version in RTRAC chemistry file INVALID'
        write(iout,'(2A)')  ' Expecting: ',camxv
        write(iout,'(2A)')  '     Found: ',camxvin
        write(iout,'(1X,3A)') 'Make sure you are using an RTRAC ',
     &                               'file designed for ',version
        call camxerr()
      endif
c
c  --- ignore the description record ----
c
      action = 'Reading description record in RTRAC Chemistry file.'
      read(iorchm,'(A)',ERR=7000,END=7001) line     
      write(idiag,'(A)',ERR=7009) line(:istrln(line))
c
c  --- read the number of gas species ----
c
      action = 'Reading number of gas species in RTRAC Chemistry file.'
      read(iorchm,'(A)',ERR=7000,END=7001) line     
      read(line,'(20X,I10)',ERR=7005) nrtgas
      write(idiag,'(A,I5)',ERR=7009) line(1:20), nrtgas
      if( nrtgas .LE. 0 ) lrtgas = .FALSE.
c
c  --- read the number of aero species ----
c
      action = 
     & 'Reading number of aerosol species in RTRAC Chemistry file.'
      read(iorchm,'(A)',ERR=7000,END=7001) line     
      read(line,'(20X,I10)',ERR=7005) nrtaero
      write(idiag,'(A,I5)',ERR=7009) line(1:20), nrtaero
      if (lparttn .AND. nrtaero.lt.nrtgas) then
         write(iout,'(A)') 'RTRAC gas-aerosol partitioning is selected.'
         write(iout,'(A,A)') 'You must list at least the same number',
     &                 ' of aerosol tracers as gas tracers.'
         write(iout,'(A,A)') 'NOTE: gas tracer 1 will partition to',
     &                       ' aerosol tracer 1, etc.'
         write(iout,'(A)') 'CAMx is stopping.'
         call camxerr()
      endif
      if( nrtaero .LE. 0 ) lrtaero = .FALSE.
c
c  --- read the number of photolysis reactions ----
c
      action = 
     & 'Reading number of photolysis reactions in RTRAC Chemistry file.'
      read(iorchm,'(A)',ERR=7000,END=7001) line     
      read(line,'(20X,I10)',ERR=7005) nrtphot
      write(idiag,'(A,I5)',ERR=7009) line(1:20), nrtphot
c
c  --- read the number of thermal reactions ----
c
      action = 
     & 'Reading number of thermal reactions in RTRAC Chemistry file.'
      read(iorchm,'(A)',ERR=7000,END=7001) line     
      read(line,'(20X,I10)',ERR=7005) nrtherm
      write(idiag,'(A,I5)',ERR=7009) line(1:20), nrtherm
c
c  --- make sure that there is enough array space ---
c
      ntotsp = nrtgas + nrtaero
      nrtrac = nrtgas + nrtaero
      nsaspc = ntotsp
      notimespc = ntotsp
c
c  --- make sure the arrays are allocated large enough ---
c
      if( ntotsp .GT. MXTRSP ) then
          write(iout,'(//,A)') 'ERROR in RDCHMRT:'
          write(iout,*) 'A parameter in the camx.prm is not ',
     &                                        'sufficiently large.'
          write(iout,*) 'Please change the value for parameter: MXTRSP'
          write(iout,*) 'It should be set to a value of at least: ',ntotsp
          call flush(iout)
          call camxerr()
      endif
c
c  --- call routine to allocate the arrays ---
c
      mvecedge = MAX(ncol(1),nrow(1))
      call alloc_tracer_specs(ngrid,ncol,nrow,nlay,tectyp,mvecedge)
      call alloc_rtracchm(ngrid,ncol,nrow,nspec,ntotsp)
      call alloc_tracer_vdep(ngrid,ncol,nrow,ntotsp)
c
c  --- if doing sampling grids, allocate the arrays ---
c
      if( lsmptrc ) 
     &  call alloc_tracer_sample(nsample,ncolsmp,nrowsmp,nrtsmpcels)

c  --- initialize the variables ---
c
      do i = 1,ntotsp
        jnum(i) = 0
        rtjfact(i) = 0.
        aoh(i) = 0.
        eaoh(i) = 0.
        boh(i) = 0.
        troh(i) = 0.
        ano3(i) = 0.
        eano3(i) = 0.
        bno3(i) = 0.
        trno3(i) = 0.
        ao3(i) = 0.
        eao3(i) = 0.
        bo3(i) = 0.
        tro3(i) = 0.
        flag(i) = ' '
        sectmp(i) = ' '
        eqkoa(i) = 0.
        khydro(i) = 0.
        kleach(i) = 0.
        kpen(i) = 0.
        kphot(i) = 0.
      enddo       
c
c  --- read the gas species identification records ---
c
      if( nrtgas .GT. 0 ) then
          action = 'Reading gas species identification'//
     &                       ' record in RTRAC Chemistry file.'
          read(iorchm,'(A)',ERR=7000,END=7001) line     
          write(idiag,'(A)',ERR=7009) line(:istrln(line))
          read(iorchm,'(A)',ERR=7000,END=7001) line     
          write(idiag,'(A)',ERR=7009) line(:istrln(line))
          do i = 1,nrtgas
            read(iorchm,'(A)',ERR=7000,END=7001) line     
c
c   --- parse the line ----
c
            sptmp = line(6:15)
            call jstlft( sptmp )
            flag(i) = line(16:21)
            call jstlft( flag(i) )
            call toupper( flag(i) )
            if( flag(i) .EQ. 'PRIM  ' ) then
               lsecnd(i) = .FALSE.
            else if( flag(i) .EQ.  'SEC   ' ) then
               lsecnd(i) = .TRUE.
               sectmp(i) = line(22:27)
               call jstlft( sectmp(i) )
               if( ipigflg .EQ. IRONPIG ) then
                 idx = 0
                 lreg(i) = .FALSE.
                 do ispc=1,i-1
                    if( ptname(ispc)(1:6) .EQ. sectmp(i) ) idx = ispc
                 enddo
                 if( idx .EQ. 0 ) goto 7011
                 ksec(i) = idx
               else
                 idx = 0
                 lreg(i) = .FALSE.
                 do ispc=1,nspec
                    if( spname(ispc)(1:6) .EQ. sectmp(i) ) then
                       idx = ispc
                       lreg(i) = .TRUE.
                       ksec(i) = idx
                    endif
                 enddo
                 if( .NOT. lreg(i) ) then
                    do ispc=1,i-1
                      if( ptname(ispc)(1:6) .EQ. sectmp(i) ) then
                         idx = ispc
                         ksec(i) = idx
                      endif
                    enddo
                 endif
                 if( idx .EQ. 0 ) goto 7004
               endif
            else
               goto 7003
            endif
c
c  ---- passes all checks, load into arrays ---
c
            ptname(i) = sptmp
            lsamap(i) = i
            lsagas(i) = .TRUE.
            loutsa(i) = .TRUE.
            read(line,'(27X,6F10.0)',ERR=7005) rtlbnd(i), 
     &        rthlaw(i), rttfact(i), rtdrate(i), rtreact(i), rtscale(i)
         enddo
c
c  --- echo the data to the diag file ----
c
          do i = 1,nrtgas
             write(idiag,'(I3,2X,A10,2A6,2E10.3,4F10.3)',ERR=7009) i,
     &              ptname(i),flag(i),sectmp(i),rtlbnd(i), rthlaw(i), 
     &                   rttfact(i), rtdrate(i),rtreact(i),rtscale(i)
          enddo
      endif
c
c  --- read the soil model data records
c
      if (lsrfmod) then
         action = 'Reading surface model species records'//
     &                       ' in RTRAC Chemistry file.'
         read(iorchm,'(A)',ERR=7000,END=7001) line
         write(idiag,'(A)',ERR=7009) line(:istrln(line))
         read(iorchm,'(A)',ERR=7000,END=7001) line
         write(idiag,'(A)',ERR=7009) line(:istrln(line))
         do i = 1,nrtgas
           read(iorchm,'(A)',ERR=7000,END=7001) line
           sptmp = line(6:15)
           call jstlft( sptmp )
           if (sptmp .NE. ptname(i)) goto 7020
           read(line,'(15X,5F10.0)',ERR=7005)
     &                     eqkoa(i),khydro(i),kleach(i),kpen(i),kphot(i)
           write(idiag,'(I3,2X,A10,5E10.3)',ERR=7009) i,
     &           ptname(i),eqkoa(i),khydro(i),kleach(i),kpen(i),kphot(i)
           if (eqkoa(i).lt.0. .or. khydro(i).lt.0. .or. kleach(i).lt.0.
     &         .or. kpen(i).lt.0. .or. kphot(i).lt.0.) then
             write(iout,'(A)') 'Rates for this species are <0!'
             write(iout,'(A)') 'Only positive rates are allowed --- Stopping'
             call camxerr() 
           endif
         enddo
      endif
c
c  --- read the aerosol species identification records ---
c
      if( nrtaero .GT. 0 ) then
         action = 'Reading aerosol species identification'//
     &                       ' record in RTRAC Chemistry file.'
         read(iorchm,'(A)',ERR=7000,END=7001) line     
         write(idiag,'(A)',ERR=7009) line(:istrln(line))
         read(iorchm,'(A)',ERR=7000,END=7001) line     
         write(idiag,'(A)',ERR=7009) line(:istrln(line))
         do j = 1,nrtaero
            i = nrtgas+j
            read(iorchm,'(A)',ERR=7000,END=7001) line     
c
c   --- parse the line ----
c
            sptmp = line(6:15)
            call jstlft( sptmp )
            ptname(i) = sptmp
            lsamap(i) = i
            lsagas(i) = .FALSE.
            loutsa(i) = .TRUE.
            read(line,'(16X,4F10.0)',ERR=7005) rtlbnd(i), 
     &                 rtdens(i), rtlcut(i), rtucut(i)
         enddo
c
c  --- echo the data to the diag file ----
c
         do j = 1,nrtaero
            i = nrtgas+j
             write(idiag,'(I3,2X,A10,E10.3,3F10.4)',ERR=7009) i,
     &              ptname(i), rtlbnd(i), rtdens(i), 
     &                                        rtlcut(i), rtucut(i)
             rtdens(i) = rtdens(i)*1.e6
         enddo
      endif
c
c  --- read the phololysis reactions ---
c
      if( nrtphot .GT. 0 ) then
         action = 'Reading photolysis reaction'//
     &                           ' record in RTRAC Chemistry file.'
         read(iorchm,'(A)',ERR=7000,END=7001) line     
         write(idiag,'(A)',ERR=7009) line(:istrln(line))
         read(iorchm,'(A)',ERR=7000,END=7001) line     
         write(idiag,'(A)',ERR=7009) line(:istrln(line))
         do irec=1,nrtphot
            read(iorchm,'(A)',ERR=7000,END=7001) line
            sptmp = line(1:10)
            call jstlft( sptmp )
            idx = 0
            do i = 1,nrtgas
               if( ptname(i) .EQ. sptmp ) idx = i
            enddo
            if( idx .EQ. 0 ) goto 7006
            read(line,'(10X,I10,F10.0)',ERR=7005) jnum(idx), 
     &                                               rtjfact(idx)
            iphot = 0
            do i = 1,nphot1
              if( jnum(idx) .EQ. idphot1(i) ) iphot = i
            enddo
            do i = 1,nphot2
              if( jnum(idx) .EQ. idphot2(i) .OR. 
     &                       jnum(idx) .EQ. idphot3(i) ) iphot = i
            enddo
            if( iphot .EQ. 0 ) goto 7007
         enddo
c
c   --- echo data to diag file ----
c
         do i = 1,nrtgas
            write(idiag,'(A10,I5,F10.4)',ERR=7009) ptname(i),
     &                                           jnum(i), rtjfact(i)
         enddo
      endif
c
c  --- read the thermal reactions ---
c
      if( nrtherm .GT. 0 ) then
         action = 'Reading thermal reaction'//
     &                       ' record in RTRAC Chemistry file.'
         read(iorchm,'(A)',ERR=7000,END=7001) line     
         write(idiag,'(A)',ERR=7009) line(:istrln(line))
         read(iorchm,'(A)',ERR=7000,END=7001) line     
         write(idiag,'(A)',ERR=7009) line(:istrln(line))
         do irec=1,nrtherm
            read(iorchm,'(A)',ERR=7000,END=7001) line     
            sptmp = line(1:10)
            call jstlft( sptmp )
            idx = 0
            do i = 1,nrtgas
               if( ptname(i) .EQ. sptmp ) idx = i
            enddo
            if( idx .EQ. 0 ) goto 7006
c
            sptmp = line(11:16)
            call jstlft( sptmp )
            if( sptmp .EQ.  'OH    ' ) then
                read(line,'(16X,2F12.0,2F10.0)',ERR=7005) aoh(idx), 
     &                                  eaoh(idx), boh(idx), troh(idx)
            else if( sptmp .EQ. 'NO3   ' ) then
                read(line,'(16x,2F12.0,2F10.0)',ERR=7005) ano3(idx), 
     &                               eano3(idx), bno3(idx), trno3(idx)
            else if( sptmp .EQ. 'O3    ' ) then
                read(line,'(16x,2F12.0,2F10.0)',ERR=7005) ao3(idx), 
     &                               eao3(idx), bo3(idx), tro3(idx)
            else
                goto 7008
            endif
         enddo
c
c   --- echo data to diag file ----
c
         do i = 1,nrtgas
            write(idiag,'(A10,A6,2E12.4,2F10.4)',ERR=7009) ptname(i),
     &                       ' OH  ',aoh(i), eaoh(i), boh(i), troh(i)
            write(idiag,'(A10,A6,2E12.4,2F10.4)',ERR=7009) ptname(i),
     &                    ' NO3 ',ano3(i), eano3(i), bno3(i), trno3(i)
            write(idiag,'(A10,A6,2E12.4,2F10.4)',ERR=7009) ptname(i),
     &                       ' O3  ',ao3(i), eao3(i), bo3(i), tro3(i)
         enddo
c
c-----Provide diagnostic info for checking rate expressions
c     See CHEMRT for explanation of function ARRHEN
c
        write(idiag,'(/,a)')
     &        'Diagnostic info for checking RTRAC rate expressions'
        write(idiag,'(/,a,/,a)')
     &        'Rates at 298 K and 1013 mbar in ppm-n min-1',
     &        '      Species   OH rate     O3 rate     NO3 rate'
        adj = 1.
        do i = 1,nrtgas
           rkoh  = arrhen(aoh(i),eaoh(i),boh(i),troh(i),298.)*adj
           rko3  = arrhen(ao3(i),eao3(i),bo3(i),tro3(i),298.)*adj
           rkno3 = arrhen(ano3(i),eano3(i),bno3(i),trno3(i),298.)*adj
           write(idiag,'(I5,1X,A10,1P3E12.4)') i, ptname(i),
     &                                           rkoh, rko3, rkno3
        enddo
        write(idiag,'(/,a,/,a)')
     &        'Rates at 273 K and 1013 mbar in ppm-n min-1',
     &        '      Species   OH rate     O3 rate     NO3 rate'
        adj = 298./273.
        do i = 1,nrtgas
           rkoh  = arrhen(aoh(i),eaoh(i),boh(i),troh(i),273.)*adj
           rko3  = arrhen(ao3(i),eao3(i),bo3(i),tro3(i),273.)*adj
           rkno3 = arrhen(ano3(i),eano3(i),bno3(i),trno3(i),273.)*adj
           write(idiag,'(I5,1X,A10,1P3E12.4)') i, ptname(i),
     &                                           rkoh, rko3, rkno3
        enddo
      endif
c
c  --- return to calling routine ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,A)') 'ERROR in RDCHMRT:'
      write(iout,'(1X,A)') action(:istrln(action))
      call camxerr()
c
 7001 continue
      write(iout,'(//,A)') 'ERROR in RDCHMRT:'
      write(iout,'(1X,A)') 'Premature end-of-file reached.'
      write(iout,'(1X,A)') action(:istrln(action))
      call camxerr()
c
 7003 continue
      write(iout,'(//,A)') 'ERROR in RDCHMRT:'
      write(iout,'(1X,A)') action(:istrln(action))
      write(iout,'(1X,2A)') 'Invalid keyword for species type: ',
     &                                                    line(16:21)
      write(iout,'(1X,A,/,A)') 'Line read: ',line(:istrln(line))
      call camxerr()
c
 7004 continue
      write(iout,'(//,A)') 'ERROR in RDCHMRT:'
      write(iout,'(1X,A)') action(:istrln(action))
      write(iout,'(1X,A)') 'Specified primary species: ',line(22:27),
     &                     ' is not in regular model species list'
      write(iout,'(1X,A)') 'nor in the RTRAC species list read so far.'
      write(iout,'(1X,A,/,A)') 'Line read: ',line(:istrln(line))
      write(iout,'(1X,A)') 'Any child RTRAC tracer must be listed after'
      write(iout,'(1X,A)') 'its parent RTRAC tracer.'
      call camxerr()
c
 7005 continue
      write(iout,'(//,A)') 'ERROR in RDCHMRT:'
      write(iout,'(1X,A)') action(:istrln(action))
      write(iout,'(1X,A,/,A)') 'Line read: ',line(:istrln(line))
      call camxerr()
c
 7006 continue
      write(iout,'(//,A)') 'ERROR in RDCHMRT:'
      write(iout,'(1X,A)') action(:istrln(action))
      write(iout,'(1X,3A)') 'Species listed not included in ',
     &               'list of gas species definitions.'
      write(iout,'(1X,A,/,A)') 'Line read: ',line(:istrln(line))
      call camxerr()
c
 7007 continue
      write(iout,'(//,A)') 'ERROR in RDCHMRT:'
      write(iout,'(1X,A)') action(:istrln(action))
      write(iout,'(1X,2A,I3)') 'Reaction # listed is not a ',
     &                    'photolysis reaction.  Rxn Id: ',jnum(idx)
      write(iout,'(1X,A,/,A)') 'Line read: ',line(:istrln(line))
      call camxerr()
c
 7008 continue
      write(iout,'(//,A)') 'ERROR in RDCHMRT:'
      write(iout,'(1X,A)') action(:istrln(action))
      write(iout,'(1X,3A)') 'Invalid species for reaction: ',
     &                                               line(11:16)
      write(iout,'(1X,A)') 'Must be one of the following: '
      write(iout,'(10X,A)') 'OH'
      write(iout,'(10X,A)') 'NO3'
      write(iout,'(10X,A)') 'O3'
      write(iout,'(1X,A,/,A)') 'Line read: ',line(:istrln(line))
      call camxerr()
c
 7009 continue
      write(iout,'(//,A)') 'ERROR in RDCHMRT:'
      write(iout,'(1X,A)') 'Cannot write to the diag file.'
      call camxerr()
c
 7010 continue
      write(iout,'(//,A)') 'ERROR in RDCHMRT:'
      write(iout,'(1X,A)') 'Cannot open file: ',chmfil(:istrln(chmfil))
      call camxerr()
 7011 continue
      write(iout,'(//,A)') 'ERROR in RDCHMRT:'
      write(iout,'(1X,A)') action(:istrln(action))
      write(iout,'(1X,A)') 'Specified primary species: ',line(22:27),
     &                  ' is not in the RTRAC species list read so far.'
      write(iout,'(1X,A,/,A)') 'Line read: ',line(:istrln(line))
      write(iout,'(1X,A)') 'Any child RTRAC tracer must be listed after'
      write(iout,'(1X,A)') 'its parent RTRAC tracer.'
      write(iout,'(1X,A)') 'Secondary RTRAC tracers can only map to',
     &                  ' other RTRAC tracers if IRON PiG is invoked.'
      call camxerr()
 7020 continue
      write(iout,'(//,A)') 'ERROR in RDCHMRT:'
      write(iout,'(1X,A)') action(:istrln(action))
      write(iout,'(1X,A,A,A)') 'Surface model gas species: ',sptmp,
     &                     ' does not match the RTRAC species list.'
      write(iout,'(1X,A,/,A)') 'Line read: ',line(:istrln(line))
      write(iout,'(1X,A,A)') 'Expecting species: ',ptname(i)
      write(iout,'(1X,A,A)') 'The surface model parameters must be',
     &        ' given in the same order as the RTRAC gas species list'
      call camxerr()
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
c
      write(idiag,'(//,A,//)',ERR=7009) 
     &       ' ******* Finished RTRAC chemical definitions file.'
      close(iorchm)
      return
      end

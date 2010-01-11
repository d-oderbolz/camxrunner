      subroutine openfils(ii,nopen)
c
c----CAMx v4.51 080522
c
c     OPENFILS opens Fortran I/O files
c                          
c     Copyright 1996-2008
c     ENVIRON International Corporation
c          
c     Modifications:
c        7/5/02    Changed to account for new type of PiG flag
c        8/30/02   Modified to read combined cloud/rain file, and now
c                  water vapor and cloud/rain files can be provided for each
c                  nest
c        01/30/02  Added code for RTRAC probing tool
c        1/10/03   Added open of deposition output file
c        4/2/03    Removed option for UAM-V type cloud file
c        3/3/04    Added checks for reading water files
c                  if either chemistry, dry dep or wet dep is on
c        3/19/04   Added checks for reading AHO file if
c                  if either chemistry or dry dep is on
c        10/4/04   Restructured for namelist input
c        6/21/05   Cloud/rain header modified for new structure
c        8/02/05   Added open of general PiG sampling grid files
c        8/25/05   PiG restart file is now optional
c        5/25/06   Output average files not opened if HDF = true
c        8/23/06   Instantaneous restart files reduced to 1 per grid type
c        8/25/06   Average and deposition output files now all UAM format,
c                  one file per grid
c
c     Input arguments:
c        ii                  Length of fileroot string 
c        nopen               number of files opened
c
c     Output arguments:
c        none
c
c     Routines Called:
c        none
c
c     Called by:
c        READNML
c
      implicit none
      include 'camx.prm'
      include 'camx.com'
      include 'camxfld.com'
      include 'filunit.com'
      include 'chmstry.com'
      include 'ahomap.com'
      include 'grid.com'
      include 'flags.com'
      include 'pigsty.com'
c
c======================== Probing Tool Begin ===========================
c
      include 'tracer.com'
      include 'procan.com'
c
c======================== Probing Tool End   ===========================
c
      include 'namelist.com'
c
      integer istrln
      character*200 filtmp
      character*80  action
      character*20  cldhdr
      logical       lexist
      integer       nopen,nfils,ii,n,nxcl,nycl,nzcl
c
c-----Entry point
c
      filtmp = filroot
      write(iout,9000) 'Output OUT message file              (unit):',
     &                                                             iout
      write(iout,9003) filtmp(1:ii),'.out'
      write(iout,9000) 'Output DIAG diagnostic file          (unit):',
     &                                                             idiag
      write(iout,9003) filtmp(1:ii),'.diag'
      write(iout,9000) 'Output MASS summary file             (unit):',
     &                                                             imass
      write(iout,9003) filtmp(1:ii),'.mass'
c
c-----Open output files for instantaneous concentration
c
      filtmp(ii+1:) = '.inst'
      nopen = nopen + 1
      call getunit(iconc)
      action = 'Opening output INST file for master grid.'
      open(unit=iconc,file=filtmp(1:ii+5),form='UNFORMATTED',
     &                                    status= 'UNKNOWN',ERR=7000)
      write(iout,9000) 'Output INST file for master grid     (unit):',
     &                                                           iconc
      write(iout,9002) filtmp(1:ii+5)
c
c-----Open optional output concentration files
c
      nfils = 4
      if (ngrid.gt.1) then
        filtmp(ii+1:) = '.finst'
        nopen = nopen + 1
        call getunit(ifconc)
        action = 'Opening output INST file for nest grids.'
        open(unit=ifconc,file=filtmp(1:ii+6),form='UNFORMATTED',
     &                                    status= 'UNKNOWN',ERR=7000)
        write(iout,9000) 'Output INST file for nest grids      (unit):',
     &                                                            ifconc
        write(iout,9002) filtmp(1:ii+6)
        nfils = nfils + 1
      endif
c
      if (navspc.gt.0 .and. .not.lhdfout) then
        filtmp(ii+1:) = '.avrg'
        do n = 1,ngrid
          write(filtmp(ii+6:),'(a,i2.2)') '.grd',n
          nopen = nopen + 1
          call getunit(iavg(n))
          action = 'Opening output AVERAGE file'
          open(unit=iavg(n),file=filtmp(1:ii+11),form='UNFORMATTED',
     &                                    status='UNKNOWN',ERR=7000)
          write(iout,9000) 
     &        'Output AVERAGE file                  (unit):',iavg(n)
          write(iout,9002) filtmp(1:ii+11)
          nfils = nfils + 1
        enddo
      endif
c
      if (navspc.gt.0 .and. (ldry .or. lwet)) then
        filtmp(ii+1:) = '.depn'
        do n = 1,ngrid
          write(filtmp(ii+6:),'(a,i2.2)') '.grd',n
          nopen = nopen + 1
          call getunit(idep(n))
          action = 'Opening output DEPOSITION file'
          open(unit=idep(n),file=filtmp(1:ii+11),form='UNFORMATTED',
     &                                   status= 'UNKNOWN',ERR=7000)
          write(iout,9000) 
     &        'Output DEPOSITION file               (unit):',idep(n)
          write(iout,9002) filtmp(1:ii+11)
          nfils = nfils + 1
        enddo
      endif
c
      if( ipigflg .NE. 0 ) then
        filtmp(ii+1:) = '.pig'
        nopen = nopen + 1
        call getunit(ipig)
        action = 'Opening output PiG file.'
        open(unit=ipig,file=filtmp(1:ii+4),form='UNFORMATTED',
     &                             status= 'UNKNOWN',ERR=7000)
        write(iout,9000) 'Output PiG file                      (unit):',
     &                                                              ipig
        write(iout,9002) filtmp(1:ii+4)
        nfils = nfils + 1
      endif
c
      if (lsample .and. .not.lhdfout) then
         filtmp(ii+1:) = '.smp'
         do n = 1,nsample
           write(filtmp(ii+5:),'(i2.2)') n
           nopen = nopen + 1
           call getunit(isample(n))
           action = 'Opening output PiG sampling grid file.'
           open(unit=isample(n),file=filtmp(1:ii+6),form='UNFORMATTED',
     &          status='UNKNOWN',ERR=7000)
           write(iout,9000)
     &                   'PiG Sampling Grid output file        (unit):',
     &                                                        isample(n)
           write(iout,9002) filtmp(1:ii+6)
         enddo
      endif
c
c-----Read and open input file names
c
      filtmp = Chemistry_Parameters
      action = 'Opening Chemistry Parameters file.'
      inquire(file=filtmp,exist=lexist)
      if( .NOT. lexist ) goto 7002
      nopen = nopen + 1
      call getunit(ichem)
      open(unit=ichem,file=filtmp,status='OLD',ERR=7000)
      write(iout,9000) 'Chemistry Parameters file            (unit):',
     &                                                           ichem
      write(iout,9002) filtmp(:istrln(filtmp))
      nfils = nfils + 1
c
      if( lchem ) then
        filtmp = Photolyis_Rates
        action = 'Opening Photolysis Rates file.'
        inquire(file=filtmp,exist=lexist)
        if( .NOT. lexist ) goto 7002
        nopen = nopen + 1
        call getunit(iphot)
        open(unit=iphot,file=filtmp,status='OLD',ERR=7000) 
        write(iout,9000) 'Photolysis Rates file                (unit):',
     &                                                             iphot
        write(iout,9002) filtmp(:istrln(filtmp))
        nfils = nfils + 1
      else
        iphot = 0
        write(iout,9000) 'Photolysis Rates file                      :'
        write(iout,9002) '   Ignored.' 
      endif
c
      if( .NOT. lrstrt ) then
        filtmp = Initial_Conditions
        action = 'Opening Initial Conditions file.'
        inquire(file=filtmp,exist=lexist)
        if( .NOT. lexist ) goto 7002
        nopen = nopen + 1
        call getunit(iic)
        open(unit=iic,file=filtmp,form='UNFORMATTED',
     &                                            status='OLD',ERR=7000)
        write(iout,9000) 'Initial Conditions file              (unit):',
     &                                                               iic
        write(iout,9002) filtmp(:istrln(filtmp))
        nfils = nfils + 1
      else
        iic = 0
        write(iout,9000) 'Initial Conditions file                    :'
        write(iout,9002) '   Ignored.'
      endif
c
      filtmp = Boundary_Conditions
      action = 'Opening Boundary Conditions file.'
      inquire(file=filtmp,exist=lexist)
      if( .NOT. lexist ) goto 7002
      nopen = nopen + 1
      call getunit(ibc)
      open(unit=ibc,file=filtmp,form='UNFORMATTED',
     &                                         status='OLD',ERR=7000)
      write(iout,9000) 'Boundary Conditions file             (unit):',
     &                                                             ibc
      write(iout,9002) filtmp(:istrln(filtmp))
      nfils = nfils + 1
c
      filtmp = Top_Concentrations
      action = 'Opening Top Concentrations file.'
      inquire(file=filtmp,exist=lexist)
      if( .NOT. lexist ) goto 7002
      nopen = nopen + 1
      call getunit(itopc)
      open(unit=itopc,file=filtmp,status='OLD',ERR=7000)
      write(iout,9000) 'Top Concentration file               (unit):',
     &                                                           itopc
      write(iout,9002) filtmp(:istrln(filtmp))
      nfils = nfils + 1
c
      if( lchem .or. ldry ) then
        filtmp = Albedo_Haze_Ozone
        action = 'Opening Albedo/Haze/Ozone file.'
        inquire(file=filtmp,exist=lexist)
        if( .NOT. lexist ) goto 7002
        nopen = nopen + 1
        call getunit(iaho)
        open(unit=iaho,file=filtmp,status='OLD',ERR=7000)
        write(iout,9000) 'Albedo/Haze/Ozone file               (unit):',
     &                                                              iaho
        write(iout,9002) filtmp(:istrln(filtmp))
        nfils = nfils + 1
      else
        iaho = 0
        write(iout,9000) 'Albedo/Haze/Ozone file                     :'
        write(iout,9002) '   Ignored.'
      endif
c
      if( lptsrc ) then
        filtmp = Point_Sources
        action = 'Opening Point Source Emissions file.'
        inquire(file=filtmp,exist=lexist)
        if( .NOT. lexist ) goto 7002
        nopen = nopen + 1
        call getunit(iptem)
        open(unit=iptem,file=filtmp,form='UNFORMATTED',
     &                                           status='OLD',ERR=7000)
        write(iout,9000) 'Point Source Emissions file          (unit):',
     &                                                             iptem
        write(iout,9002) filtmp(:istrln(filtmp))
        nfils = nfils + 1
      else
        iptem = 0
        write(iout,9000) 'Point Source Emissions file                :'
        write(iout,9002) '   Ignored.'
      endif
c
      if( lrstrt ) then
        filtmp = Master_Grid_Restart
        action = 'Opening Restart file for master grid.'
        inquire(file=filtmp,exist=lexist)
        if( .NOT. lexist ) goto 7002
        nopen = nopen + 1
        call getunit(irstc)
        open(unit=irstc,file=filtmp,form='UNFORMATTED',
     &                                       status='OLD',ERR=7005)
        write(iout,9000) 'Master grid Restart file             (unit):',
     &                                                             irstc
        write(iout,9002) filtmp(:istrln(filtmp))
        nfils = nfils + 1
c
        if( ngrid .GT. 1 ) then
          filtmp = Nested_Grid_Restart
          if( filtmp .EQ. ' ' ) then
             irstf = 0
             write(iout,9001)
     &                  'Nest grid Restart file                     :'
             write(iout,9002) '   Ignored.'
          else
             action = 'Opening Restart file for nest grids.'
             inquire(file=filtmp,exist=lexist)
             if( .NOT. lexist ) goto 7002
             nopen = nopen + 1
             call getunit(irstf)
             open(unit=irstf,file=filtmp,form='UNFORMATTED',
     &                                         status='OLD',ERR=7005)
             write(iout,9000)
     &              'Nest grid Restart file               (unit):',irstf
             write(iout,9002) filtmp(:istrln(filtmp))
             nfils = nfils + 1
          endif
        endif
c
        if( ipigflg .NE. 0 ) then
          filtmp = PiG_Restart
          if( filtmp .EQ. ' ' ) then
             irstp = 0
             write(iout,9001)
     &                   'PiG Restart file                           :'
             write(iout,9002) '   Ignored.'
          else
             action = 'Opening PiG Restart file.'
             inquire(file=filtmp,exist=lexist)
             if( .NOT. lexist ) goto 7002
             nopen = nopen + 1
             call getunit(irstp)
             open(unit=irstp,file=filtmp,form='UNFORMATTED',
     &                                status='OLD',ERR=7005)
             write(iout,9000)
     &         'PiG Restart file                     (unit):',irstp
             write(iout,9002) filtmp(:istrln(filtmp))
             nfils = nfils + 1
          endif
        endif
      endif
c
      if( ldry .OR. ipigflg .NE. 0 )then
        filtmp = Landuse_Grid(1)
        write(action,'(A,I4)') 'Opening Landuse file for grid:',1
        inquire(file=filtmp,exist=lexist)
        if( .NOT. lexist ) goto 7002
        nopen = nopen + 1
        call getunit(isurf(1))
        open(unit=isurf(1),file=filtmp,form='UNFORMATTED',
     &                                          status='OLD',ERR=7000)
        write(iout,9001)
     &        'Landuse file for grid # ',1,'           (unit):',isurf(1)
        write(iout,9002) filtmp(:istrln(filtmp))
        nfils = nfils + 1
c
        do n = 2,ngrid
          filtmp = Landuse_Grid(n)
          if( filtmp .EQ. ' ')  then
             isurf(n) = 0
             write(iout,9001)
     &                 'Landuse file for grid # ',n,'                 :'
             write(iout,9002) '   Ignored.'
          else
             write(action,'(A,I4)') 'Opening Landuse file for grid:',n
             inquire(file=filtmp,exist=lexist)
             if( .NOT. lexist ) goto 7002
             nopen = nopen + 1
             call getunit(isurf(n))
             open(unit=isurf(n),file=filtmp,form='UNFORMATTED',
     &                                          status='OLD',ERR=7000)
             write(iout,9001)
     &             'Landuse file for grid # ',n,'           (unit):',
     &                                                         isurf(n)
             write(iout,9002) filtmp(:istrln(filtmp))
             nfils = nfils + 1
          endif
        enddo
c
      else
        do n = 1,ngrid
          isurf(n) = 0
          write(iout,9001)
     &                'Landuse file for grid # ',n,'                 :'
          write(iout,9002) '   Ignored.'
        enddo
      endif
c
      filtmp = ZP_Grid(1)
      write(action,'(A,I4)') 'Opening Height/Pressure file for grid:',1
      inquire(file=filtmp,exist=lexist)
      if( .NOT. lexist ) goto 7002
      nopen = nopen + 1
      call getunit(ihtp(1))
      open(unit=ihtp(1),file=filtmp,form='UNFORMATTED',
     &                                           status='OLD',ERR=7000)
      write(iout,9001)'Height/Pressure file for grid # ',1,'   (unit):',
     &                                                           ihtp(1)
      write(iout,9002) filtmp(:istrln(filtmp))
      nfils = nfils + 1
c
      do n = 2,ngrid
        filtmp = ZP_Grid(n)
        if( filtmp .EQ. ' ' ) then
          ihtp(n) = 0
          write(iout,9001)
     &    'Height/Pressure file for grid # ',n,'         :'
          write(iout,9002) '   Ignored.'
        else
          write(action,'(A,I4)')
     &                       'Opening Height/Pressure file for grid:',n
          inquire(file=filtmp,exist=lexist)
          if( .NOT. lexist ) goto 7002
          nopen = nopen + 1
          call getunit(ihtp(n))
          open(unit=ihtp(n),file=filtmp,form='UNFORMATTED',
     &                                           status='OLD',ERR=7000)
          write(iout,9001)
     &              'Height/Pressure file for grid # ',n,'   (unit):',
     &                                                           ihtp(n)
          write(iout,9002) filtmp(:istrln(filtmp))
          nfils = nfils + 1
        endif
      enddo
c
      filtmp = Wind_Grid(1)
      write(action,'(A,I4)') 'Opening Wind file for grid:',1
      inquire(file=filtmp,exist=lexist)
      if( .NOT. lexist ) goto 7002
      nopen = nopen + 1
      call getunit(iwind(1))
      open(unit=iwind(1),file=filtmp,form='UNFORMATTED',
     &                                          status='OLD',ERR=7000)
      write(iout,9001)
     &              'Wind file for grid # ',1,'              (unit):',
     &                                                          iwind(1)
      write(iout,9002) filtmp(:istrln(filtmp))
      nfils = nfils + 1
c
      do n = 2,ngrid
        filtmp = Wind_Grid(n)
        if( filtmp .EQ. ' ' ) then
           iwind(n) = 0
           write(iout,9001)
     &                'Wind file for grid # ',n,'                    :'
           write(iout,9002) '   Ignored.'
         else
           write(action,'(A,I4)') 'Opening Wind file for grid:',n
           inquire(file=filtmp,exist=lexist)
           if( .NOT. lexist ) goto 7002
           nopen = nopen + 1
           call getunit(iwind(n))
           open(unit=iwind(n),file=filtmp,form='UNFORMATTED',
     &                                            status='OLD',ERR=7000)
           write(iout,9001)
     &               'Wind file for grid # ',n,'              (unit):',
     &                                                          iwind(n)
           write(iout,9002) filtmp(:istrln(filtmp))
           nfils = nfils + 1
         endif
      enddo
c
      filtmp = Temp_Grid(1)
      write(action,'(A,I4)') 'Opening Temperature file for grid:',1
      inquire(file=filtmp,exist=lexist)
      if( .NOT. lexist ) goto 7002
      nopen = nopen + 1
      call getunit(itemp(1))
      open(unit=itemp(1),file=filtmp,form='UNFORMATTED',
     &                                            status='OLD',ERR=7000)
      write(iout,9001)'Temperature file for grid # ',1,'       (unit):',
     &                                                          itemp(1)
      write(iout,9002) filtmp(:istrln(filtmp))
      nfils = nfils + 1
c
      do n = 2,ngrid
        filtmp = Temp_Grid(n)
        if( filtmp .EQ. ' ' ) then
           itemp(n) = 0
           write(iout,9001)
     &                'Temperature file for grid # ',n,'             :'
           write(iout,9002) '   Ignored.'
        else
           write(action,'(A,I4)')
     &               'Opening Temperature file for grid:',n
           inquire(file=filtmp,exist=lexist)
           if( .NOT. lexist ) goto 7002
           nopen = nopen + 1
           call getunit(itemp(n))
           open(unit=itemp(n),file=filtmp,form='UNFORMATTED',
     &                                         status='OLD',ERR=7000)
           write(iout,9001)
     &               'Temperature file for grid # ',n,'       (unit):',
     &                                                          itemp(n)
           write(iout,9002) filtmp(:istrln(filtmp))
           nfils = nfils + 1
        endif
      enddo
c
      if( lchem .or. lwet .or. ldry ) then
        filtmp = Vapor_Grid(1)
        write(action,'(A,I4)') 'Opening Water Vapor file for grid:',1
        inquire(file=filtmp,exist=lexist)
        if( .NOT. lexist ) goto 7002
        nopen = nopen + 1
        call getunit(ih2o(1))
        open(unit=ih2o(1),file=filtmp,form='UNFORMATTED',
     &                                         status='OLD',ERR=7000)
        write(iout,9001) 
     &               'Water Vapor file for grid # ',1,'       (unit):',
     &                                                           ih2o(1)
        write(iout,9002) filtmp(:istrln(filtmp))
        nfils = nfils + 1
c
        do n = 2,ngrid
          filtmp = Vapor_Grid(n)
          if( filtmp .EQ. ' ' ) then
             ih2o(n) = 0
             write(iout,9001)
     &       'Water Vapor file for grid # ',n,'             :'
             write(iout,9002) '   Ignored.'
          else
             write(action,'(A,I4)')
     &       'Opening Water Vapor file for grid:',n
             inquire(file=filtmp,exist=lexist)
             if( .NOT. lexist ) goto 7002
             nopen = nopen + 1
             call getunit(ih2o(n))
             open(unit=ih2o(n),file=filtmp,form='UNFORMATTED',
     &                                          status='OLD',ERR=7000)
             write(iout,9001)
     &              'Water Vapor file for grid # ',n,'       (unit):',
     &                                                           ih2o(n)
             write(iout,9002) filtmp(:istrln(filtmp))
             nfils = nfils + 1
          endif
        enddo
c
      else
        do n = 1,ngrid
          ih2o(n) = 0
          write(iout,9001)
     &             'Water Vapor file for grid # ',n,'             :'
          write(iout,9002) '   Ignored.'
        enddo
      endif
c
      if( lwet ) then
         filtmp = Cloud_Grid(1)
         write(action,'(A,I4)') 'Opening Cloud/Rain file for grid:',1
         inquire(file=filtmp,exist=lexist)
         if( .NOT. lexist ) goto 7002
         nopen = nopen + 1
         call getunit(icld(1))
         open(unit=icld(1),file=filtmp,form='UNFORMATTED',
     &                                            status='OLD',ERR=7000)
         write(iout,9001)
     &               'Cloud/Rain file for grid # ',1,'        (unit):',
     &                                                           icld(1)
         write(iout,9002) filtmp(:istrln(filtmp))
         nfils = nfils + 1
c
         read(icld(1),ERR=7008) cldhdr
         call jstlft( cldhdr )
         call toupper( cldhdr )
         if( cldhdr .EQ. 'CAMX_V4.3 CLOUD_RAIN' ) then
            backspace(icld(1))
            n = 1
            read(icld(1),ERR=7008) cldhdr,nxcl,nycl,nzcl
            if (nxcl.ne.ncol(1) .or. nycl.ne.nrow(1) .or.
     &                               nzcl.ne.nlay(1)) goto 7007
         else
           goto 7006 
         endif
c
         do n = 2,ngrid
           filtmp = Cloud_Grid(n)
           if( filtmp .EQ. ' ' ) then
              icld(n) = 0
              write(iout,9001)
     &                 'Cloud/Rain file for grid # ',n,'              :'
              write(iout,9002) '   Ignored.'
           else
              write(action,'(A,I4)')
     &        'Opening Cloud/Rain file for grid:',n
              inquire(file=filtmp,exist=lexist)
              if( .NOT. lexist ) goto 7002
              nopen = nopen + 1
              call getunit(icld(n))
              open(unit=icld(n),file=filtmp,form='UNFORMATTED',
     &                                           status='OLD',ERR=7000)
              write(iout,9001)
     &               'Cloud/Rain file for grid # ',n,'        (unit):',
     &                                                           icld(n)
              write(iout,9002) filtmp(:istrln(filtmp))
              nfils = nfils + 1
c
              read(icld(n),ERR=7008) cldhdr
              call jstlft( cldhdr )
              call toupper( cldhdr )
              if( cldhdr .EQ. 'CAMX_V4.3 CLOUD_RAIN' ) then
                 backspace(icld(n))
                 read(icld(n),ERR=7008) cldhdr,nxcl,nycl,nzcl
                 if (nxcl.ne.ncol(n) .or. nycl.ne.nrow(n) .or.
     &                                    nzcl.ne.nlay(n)) goto 7007
              else
                 goto 7006 
              endif
           endif
         enddo
c
      else
         do n = 1,ngrid
           filtmp = Cloud_Grid(n)
           if( filtmp .EQ. ' ' ) then
              icld(n) = 0
              write(iout,9001)
     &                 'Cloud/Rain file for grid # ',n,'              :'
              write(iout,9002) '   Ignored.' 
           else
              write(action,'(A,I4)')
     &                          'Opening Cloud/Rain file for grid:',n
              inquire(file=filtmp,exist=lexist)
              if( .NOT. lexist ) goto 7002
              nopen = nopen + 1
              call getunit(icld(n))
              open(unit=icld(n),file=filtmp,form='UNFORMATTED',
     &                                          status='OLD',ERR=7000)
              write(iout,9001)
     &              'Cloud/Rain file for grid # ',n,'        (unit):',
     &                                                           icld(n)
              write(iout,9002) filtmp(:istrln(filtmp))
              nfils = nfils + 1
c
              read(icld(n),ERR=7008) cldhdr
              call jstlft( cldhdr )
              call toupper( cldhdr )
              if( cldhdr .EQ. 'CAMX_V4.3 CLOUD_RAIN' ) then
                 backspace(icld(n))
                 read(icld(n),ERR=7008) cldhdr,nxcl,nycl,nzcl
                 if (nxcl.ne.ncol(n) .or. nycl.ne.nrow(n) .or.
     &                                    nzcl.ne.nlay(n)) goto 7007
              else
                 goto 7006 
              endif
           endif
         enddo
      endif
c
      filtmp = Kv_Grid(1)
      write(action,'(A,I4)') 
     &                'Opening Vertical Diffusivity file for grid:',1
      inquire(file=filtmp,exist=lexist)
      if( .NOT. lexist ) goto 7002
      nopen = nopen + 1
      call getunit(ikv(1))
      open(unit=ikv(1),file=filtmp,form='UNFORMATTED',
     &                                        status='OLD',ERR=7000)
      write(iout,9001)
     &              'Kv file for grid # ',1,'                (unit):',
     &                                                           ikv(1)
      write(iout,9002) filtmp(:istrln(filtmp))
      nfils = nfils + 1
c
      do n = 2,ngrid
        filtmp = Kv_Grid(n)
        if( filtmp .EQ. ' ' ) then
           ikv(n) = 0
           write(iout,9001)
     &               'Kv file for grid # ',n,'                      :'
           write(iout,9002) '   Ignored.'
        else
           write(action,'(A,I4)')
     &              'Opening Vertical Diffusivity file for grid:',n
           inquire(file=filtmp,exist=lexist)
           if( .NOT. lexist ) goto 7002
           nopen = nopen + 1
           call getunit(ikv(n))
           open(unit=ikv(n),file=filtmp,form='UNFORMATTED',
     &                                         status='OLD',ERR=7000)
           write(iout,9001)
     &               'Kv file for grid # ',n,'                (unit):',
     &                                                            ikv(n)
           write(iout,9002) filtmp(:istrln(filtmp))
           nfils = nfils + 1
        endif
      enddo
c
      if( larsrc ) then
         filtmp = Emiss_Grid(1)
         write(action,'(A,I4)') 
     &                     'Opening Gridded Emissions file for grid:',1
         inquire(file=filtmp,exist=lexist)
         if( .NOT. lexist ) goto 7002
         nopen = nopen + 1
         call getunit(iarem(1))
         open(unit=iarem(1),file=filtmp,form='UNFORMATTED',
     &                                          status='OLD',ERR=7000)
         write(iout,9001)
     &              'Gridded Emissions file for grid # ',1,' (unit):',
     &                                                         iarem(1)
         write(iout,9002) filtmp(:istrln(filtmp))
         nfils = nfils + 1
c
         do n = 2,ngrid
           filtmp = Emiss_Grid(n)
           if( filtmp .EQ. ' ' ) then
              iarem(n) = 0
              write(iout,9001)
     &                'Gridded Emissions file for grid # ',n,'       :'
              write(iout,9002) '   Ignored.' 
           else
              write(action,'(A,I4)') 
     &                     'Opening Gridded Emissions file for grid:',n
              inquire(file=filtmp,exist=lexist)
              if( .NOT. lexist ) goto 7002
              nopen = nopen + 1
              call getunit(iarem(n))
              open(unit=iarem(n),file=filtmp,form='UNFORMATTED',
     &                                          status='OLD',ERR=7000)
              write(iout,9001)
     &                'Gridded Emissions file for grid # ',n,' (unit):',
     &                                                          iarem(n)
              write(iout,9002) filtmp(:istrln(filtmp))
              nfils = nfils + 1
           endif
         enddo
c
      else
        do n = 1,ngrid
          iarem(n) = 0
          write(iout,9001)
     &              'Gridded Emissions file for grid # ',n,'       :'
          write(iout,9002) '   Ignored.' 
        enddo
      endif
c
      goto 9999
c
c  --- Format statements ---
c
 9000 format(/,A,I2)
 9001 format(/,A,I2,A,I2)
 9002 format(2A)
 9003 format(3A)
c
c  --- Error messages
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in OPENFILS:'
      write(iout,'(A)') action(:istrln(action))
      write(iout,'(2A)') 'Could not open file: ',
     &                                   filtmp(:istrln(filtmp))
      call camxerr()
c
 7002 continue
      write(iout,'(//,a)') 'ERROR in OPENFILS:'
      write(iout,'(A)') action(:istrln(action))
      if( filtmp .EQ. ' ' ) then
         write(iout,'(2A,I2)') 'Blank filename provided in control file'
      else
         write(iout,'(2A)') 'Input file does not exist: ',
     &                                       filtmp(:istrln(filtmp))
      endif
      call camxerr()
c
 7005 continue
      write(iout,'(//,a)') 'ERROR in OPENFILS:'
      write(iout,'(A)') action(:istrln(action))
      write(iout,'(2A)') 'Could not open file: ',
     &                                   filtmp(:istrln(filtmp))
      write(iout,'(10X,2A)') 'Make sure the names of restart files ',
     &                       'are for the previous simulation period.'
      call camxerr()
c
 7006 continue
      write(iout,'(//,a)') 'ERROR in OPENFILS:'
      write(iout,'(A)') 'CAMx cloud/rain file header is incorrect.'
      write(iout,'(A)') 'Expecting: CAMx_V4.3 CLOUD_RAIN'
      write(iout,'(2A)')'Read     : ',cldhdr
      call camxerr()
c
 7007 continue
      write(iout,'(//,a)') 'ERROR in OPENFILS:'
      write(iout,'(2A,i5)')'Cloud/rain file dimensions do not',
     &                     ' match dimensions of grid ',n
      write(iout,'(a,3i5)') 'Cloud file: ',nxcl,nycl,nzcl
      write(iout,'(a,3i5)') 'CAMx      : ',ncol(n),nrow(n),nlay(n)
      call camxerr()
c
 7008 continue
      write(iout,'(//,a)') 'ERROR in OPENFILS:'
      write(iout,'(A)')'Could not read first record of cloud/rain file.'
      write(iout,'(A)') filtmp(:istrln(filtmp))
      call camxerr()
c
 9999 continue
      return
      end

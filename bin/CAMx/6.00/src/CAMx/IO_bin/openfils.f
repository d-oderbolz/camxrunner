      subroutine openfils(ii,nopen)
      use camxcom
      use camxfld
      use filunit
      use chmstry
      use o3colmap
      use grid
      use pigsty
      use tracer
      use procan
      implicit none
c
c----CAMx v6.00 130506
c
c     OPENFILS opens Fortran I/O files
c                          
c     Copyright 1996 - 2013
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
c        11/4/09   Removed input top concentrations
c        1/04/11   Revised for new met input format
c        04/02/12  Removed drought stress and snow flag; AHO
c                  file is now just ozone column
c        05/07/12  Added flexi-nesting flag
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
      include 'camx.prm'
      include 'flags.inc'
      include 'namelist.inc'
c
      integer istrln
      character*200 filtmp
      character*80  action
      logical       lexist
      integer       nopen,nfils,ii,n
c
c-----Entry point
c
c-----Open text output files
c
      filtmp = ' '
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
c-----Open master grid instantaneous concentration output file
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
c-----Open fine grid instantaneous concentration output file
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
c-----Open average concentration output file(s)
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
c-----Open deposition output file(s)
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
c-----Open PiG output file
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
c
c-----Open PiG sampling grid output file(s)
c
        if (lsample .and. .not.lhdfout) then
          filtmp(ii+1:) = '.smp'
          do n = 1,nsample
            write(filtmp(ii+5:),'(i2.2)') n
            nopen = nopen + 1
            call getunit(isample(n))
            action = 'Opening output PiG sampling grid file.'
            open(unit=isample(n),file=filtmp(1:ii+6),form='UNFORMATTED',
     &           status='UNKNOWN',ERR=7000)
            write(iout,9000)
     &                   'PiG Sampling Grid output file        (unit):',
     &                                                        isample(n)
            write(iout,9002) filtmp(1:ii+6)
          enddo
        endif
      endif
c
c-----Open chemistry parameters input file
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
c-----Open photoloysis rates input file
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
c-----Open initial conditions input file
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
c-----Open boundary conditions input file
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
c-----Open ozone column input file
c
      filtmp = Ozone_Column
      if( filtmp .NE. ' ' ) then
        action = 'Opening ozone column file.'
        inquire(file=filtmp,exist=lexist)
        if( .NOT. lexist ) goto 7002
        nopen = nopen + 1
        call getunit(io3col)
        open(unit=io3col,file=filtmp,status='OLD',ERR=7000)
        write(iout,9000) 'Ozone column file                    (unit):',
     &                                                            io3col
        write(iout,9002) filtmp(:istrln(filtmp))
        nfils = nfils + 1
      else
        io3col = 0
        if( lchem ) goto 7010
        lrdocn = .FALSE.
        write(iout,9000) 'Ozone column file                          :'
        write(iout,9002) '   Ignored.'
      endif
c
c-----Open point source emissions input file
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
c-----Open master grid restart input file
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
c-----Open fine grid restart input file
c
        if( ngrid .GT. 1 ) then
          filtmp = Nested_Grid_Restart
          action = 'Opening Restart file for nest grids.'
          if( filtmp .EQ. ' ' ) then
             if (lflexi) then
               irstf = 0
               write(iout,9001)
     &                  'Nest grid Restart file                     :'
               write(iout,9002) '   Ignored.'
             else
               goto 7002
             endif
          else
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
c-----Open PiG restart input file
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
c-----Open master grid 2D Surface input file
c
      filtmp = Surface_Grid(1)
      write(action,'(A,I4)') 'Opening 2D Surface file for grid:',1
      inquire(file=filtmp,exist=lexist)
      if( .NOT. lexist ) goto 7002
      nopen = nopen + 1
      call getunit(isurf(1))
      open(unit=isurf(1),file=filtmp,form='UNFORMATTED',
     &                                          status='OLD',ERR=7000)
      write(iout,9001)
     &        '2D Surface file for grid # ',1,'        (unit):',isurf(1)
      write(iout,9002) filtmp(:istrln(filtmp))
      nfils = nfils + 1
c
c-----Open nested grid 2D Surface input file(s)
c
      do n = 2,ngrid
        filtmp = Surface_Grid(n)
        write(action,'(A,I4)') 'Opening 2D Surface file for grid:',n
        if( filtmp .EQ. ' ')  then
           if (lflexi) then
             isurf(n) = 0
             write(iout,9001)
     &                 '2D Surface file for grid # ',n,'              :'
             write(iout,9002) '   Ignored.'
           else
             goto 7002
           endif
        else
           inquire(file=filtmp,exist=lexist)
           if( .NOT. lexist ) goto 7002
           nopen = nopen + 1
           call getunit(isurf(n))
           open(unit=isurf(n),file=filtmp,form='UNFORMATTED',
     &                                          status='OLD',ERR=7000)
           write(iout,9001)
     &             '2D Surface file for grid # ',n,'        (unit):',
     &                                                         isurf(n)
           write(iout,9002) filtmp(:istrln(filtmp))
           nfils = nfils + 1
        endif
      enddo
c
c-----Open master grid 3D Met input file
c
      filtmp = Met3d_Grid(1)
      write(action,'(A,I4)') 'Opening 3D Met file for grid:',1
      inquire(file=filtmp,exist=lexist)
      if( .NOT. lexist ) goto 7002
      nopen = nopen + 1
      call getunit(i3dmet(1))
      open(unit=i3dmet(1),file=filtmp,form='UNFORMATTED',
     &                                           status='OLD',ERR=7000)
      write(iout,9001)'3D Met file for grid # ',1,'            (unit):',
     &                                                         i3dmet(1)
      write(iout,9002) filtmp(:istrln(filtmp))
      nfils = nfils + 1
c
c-----Open nested grid 3D Met input file(s)
c
      do n = 2,ngrid
        filtmp = Met3D_Grid(n)
        write(action,'(A,I4)') 'Opening 3D Met file for grid:',n
        if( filtmp .EQ. ' ' ) then
          if (lflexi) then
            i3dmet(n) = 0
            write(iout,9001)
     &                 '3D Met file for grid # ',n,'                  :'
            write(iout,9002) '   Ignored.'
          else
            goto 7002
          endif
        else
          inquire(file=filtmp,exist=lexist)
          if( .NOT. lexist ) goto 7002
          nopen = nopen + 1
          call getunit(i3dmet(n))
          open(unit=i3dmet(n),file=filtmp,form='UNFORMATTED',
     &                                           status='OLD',ERR=7000)
          write(iout,9001)
     &                '3D Met file for grid # ',n,'            (unit):',
     &                                                         i3dmet(n)
          write(iout,9002) filtmp(:istrln(filtmp))
          nfils = nfils + 1
        endif
      enddo
c
c-----Open master grid 2D Met input file
c
      filtmp = Met2D_Grid(1)
      write(action,'(A,I4)') 'Opening 2D Met file for grid:',1
      inquire(file=filtmp,exist=lexist)
      if( .NOT. lexist ) goto 7002
      nopen = nopen + 1
      call getunit(i2dmet(1))
      open(unit=i2dmet(1),file=filtmp,form='UNFORMATTED',
     &                                          status='OLD',ERR=7000)
      write(iout,9001)
     &              '2D Met file for grid # ',1,'            (unit):',
     &                                                       i2dmet(1)
      write(iout,9002) filtmp(:istrln(filtmp))
      nfils = nfils + 1
c
c-----Open nested grid 2D Met input file(s)
c
      do n = 2,ngrid
        filtmp = Met2D_Grid(n)
        write(action,'(A,I4)') 'Opening 2D Met file for grid:',n
        if( filtmp .EQ. ' ' ) then
           if (lflexi) then
             i2dmet(n) = 0
             write(iout,9001)
     &                 '2D Met file for grid # ',n,'                  :'
             write(iout,9002) '   Ignored.'
           else
             goto 7002
           endif
         else
           inquire(file=filtmp,exist=lexist)
           if( .NOT. lexist ) goto 7002
           nopen = nopen + 1
           call getunit(i2dmet(n))
           open(unit=i2dmet(n),file=filtmp,form='UNFORMATTED',
     &                                            status='OLD',ERR=7000)
           write(iout,9001)
     &               '2D Met file for grid # ',n,'            (unit):',
     &                                                        i2dmet(n)
           write(iout,9002) filtmp(:istrln(filtmp))
           nfils = nfils + 1
         endif
      enddo
c
c-----Open master grid 3D VDiff input file
c
      filtmp = Vdiff_Grid(1)
      write(action,'(A,I4)')
     &                'Opening 3D Vertical Diffusivity file for grid:',1
      inquire(file=filtmp,exist=lexist)
      if( .NOT. lexist ) goto 7002
      nopen = nopen + 1
      call getunit(ikv(1))
      open(unit=ikv(1),file=filtmp,form='UNFORMATTED',
     &                                        status='OLD',ERR=7000)
      write(iout,9001)
     &              '3D VDiff file for grid # ',1,'             (unit):',
     &                                                          ikv(1)
      write(iout,9002) filtmp(:istrln(filtmp))
      nfils = nfils + 1
c
c-----Open nested grid 3D VDiff input file(s)
c
      do n = 2,ngrid
        filtmp = Vdiff_Grid(n)
        write(action,'(A,I4)')
     &                'Opening 3D Vertical Diffusivity file for grid:',n
        if( filtmp .EQ. ' ' ) then
           if (lflexi) then
             ikv(n) = 0
             write(iout,9001)
     &                 '3D VDiff file for grid # ',n,'                   :'
             write(iout,9002) '   Ignored.'
           else
             goto 7002
           endif
        else
           inquire(file=filtmp,exist=lexist)
           if( .NOT. lexist ) goto 7002
           nopen = nopen + 1
           call getunit(ikv(n))
           open(unit=ikv(n),file=filtmp,form='UNFORMATTED',
     &                                         status='OLD',ERR=7000)
           write(iout,9001)
     &               '3D VDiff file for grid # ',n,'             (unit):',
     &                                                           ikv(n)
           write(iout,9002) filtmp(:istrln(filtmp))
           nfils = nfils + 1
        endif
      enddo
c
c-----Open master grid 3D Cloud/rain input file
c
      filtmp = Cloud_Grid(1)
      write(action,'(A,I4)') 'Opening 3D Cloud/Rain file for grid:',1
      if( filtmp .EQ. ' ' ) then
         if( lwet ) goto 7002
         icld(1) = 0
         write(iout,9001)
     &                 '3D Cloud/Rain file for grid # ',1,'           :'
         write(iout,9002) '   Ignored.' 
      else
         inquire(file=filtmp,exist=lexist)
         if( .NOT. lexist ) goto 7002
         nopen = nopen + 1
         call getunit(icld(1))
         open(unit=icld(1),file=filtmp,form='UNFORMATTED',
     &                                            status='OLD',ERR=7000)
         write(iout,9001)
     &               '3D Cloud/Rain file for grid # ',1,'     (unit):',
     &                                                          icld(1)
         write(iout,9002) filtmp(:istrln(filtmp))
         nfils = nfils + 1
c
c-----Open nested grid 3D Cloud/rain input file(s)
c
         do n = 2,ngrid
           filtmp = Cloud_Grid(n)
           write(action,'(A,I4)')
     &                          'Opening 3D Cloud/Rain file for grid:',n
           if( filtmp .EQ. ' ' ) then
              if (lflexi .or. .not.lwet) then
                icld(n) = 0
                write(iout,9001)
     &                 '3D Cloud/Rain file for grid # ',n,'           :'
                write(iout,9002) '   Ignored.'
              else
                goto 7002
              endif
           else
              inquire(file=filtmp,exist=lexist)
              if( .NOT. lexist ) goto 7002
              nopen = nopen + 1
              call getunit(icld(n))
              open(unit=icld(n),file=filtmp,form='UNFORMATTED',
     &                                           status='OLD',ERR=7000)
              write(iout,9001)
     &               '3D Cloud/Rain file for grid # ',n,'     (unit):',
     &                                                          icld(n)
              write(iout,9002) filtmp(:istrln(filtmp))
              nfils = nfils + 1
c
           endif
         enddo
      endif
c
c-----Open master grid 2D Emissions input file
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
c-----Open nested grid 2D Emissions input file(s)
c
         do n = 2,ngrid
           filtmp = Emiss_Grid(n)
           write(action,'(A,I4)') 
     &                     'Opening Gridded Emissions file for grid:',n
           if( filtmp .EQ. ' ' ) then
              if (lflexi) then
                iarem(n) = 0
                write(iout,9001)
     &                'Gridded Emissions file for grid # ',n,'       :'
                write(iout,9002) '   Ignored.' 
              else
                goto 7002
              endif
           else
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
         write(iout,'(A)') 'Blank filename provided in control file'
         write(iout,'(2A)') 'If this file is for a nested grid, set',
     &                      ' the namelist variable '
         write(iout,'(5X,A,/,A)')  'Flexi_Nest = .true.', 'to ignore this file.'
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
 7010 continue
      write(iout,'(//,a)') 'ERROR in OPENFILS:'
      write(iout,'(2A)') 'The chemistry flag is set but no ',
     &                         'Ozone Column file is supplied.'
      write(iout,'(2A)')'Either supply an Ozone Column file ',
     &                         'or set the Chemistry flag to false.'
      write(iout,'(2A)')'If using Mechanism 10 just supply ',
     &                  'a dummy file for the Ozone Column file.'
      call camxerr()
c
 9999 continue
      return
      end

c**** STARTRT
c
      subroutine startrt(iout,nopen)
      use grid
      use pigsty
      use procan
      use tracer
      use rtracchm
      implicit none
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     This routine loads all of the files needed for the reactive 
c     tracer algorithm (RTRAC)
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Argument description:
c        Inputs:
c          iout     I   unit number for output
c        Outputs:   
c          nopen    I   number of files opened
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     01/16/02   --gwilson--   Original development
c     11/10/03   --cemery--    Added sampling grid I/O
c     10/06/04   --cemery--    Restructured for namelist input
c      8/02/05   --cemery--    Included specific option flag for Rtrac
c                              sampling grid
c      5/25/06   --cemery--    Average sfc and sampling grid files not
c                              opened if HDF = true
c      8/23/06   --cemery--    Instantaneous restart files reduced to 1
c                              per grid type
c      8/25/06   --cemery--    Surface output files now all UAM format,
c                              one file per grid
c     10/29/09   --cemery--    Added optional RTRAC surface model
c      11/4/09   --cemery--    Removed input top concentrations
c     05/07/12   --cemery--    Added flexi-nesting flag
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
      include 'namelist.inc'
      include 'chmdat.inc'
      include 'rtracsrf.inc'
c
c-----------------------------------------------------------------------
c    Argument declaration:
c-----------------------------------------------------------------------
c
      integer   iout
      integer   nopen
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
      character*200 fname
      character*80 action
      integer      i, j, n
      logical      lexist
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- if option is turned off, just return ---
c
      if( .NOT. ltrace .OR. 
     &         (tectyp .NE. RTRAC .AND. tectyp .NE. RTCMC) ) goto 9999
      nchar = istrln( flrtsa )
c
c   --- only one emission group for RTRAC ----
c
      ngroup = 1
c
c   --- call routine to allocate all of the data ---
c
      call alloc_tracer(ngroup,ngrid,ncol,nrow,nlay,nspec)
      call alloc_tracer_full(ngrid,ncol,nrow)
      call alloc_rtrac_cel(ngrid,ncol,nrow,nlay)
c
c   --- intialize first time through to true ---
c
      lfirst = .TRUE.
      write(iout,'(/,A,/)') 
     &            '           **** Source Apportionment Files ****'
c
c   --- chemistry parameters file ---
c
      action = 'Opening RTRAC Chemistry Parameters file.'
      chmfil = RT_Chemistry_Parameters
      fname = chmfil
      inquire(file=chmfil,exist=lexist)
      if( .NOT. lexist ) goto 7000
      nopen =  nopen + 1
      call getunit(iorchm)
      write(iout,9000) 'RTRAC Chemistry Parameters file      (unit):',
     &                                                          iorchm
      write(iout,9002) chmfil(:istrln(chmfil))
c
c   --- receptor definition file ----
c
      rcpfil = RT_Receptor_Definitions
      if( rcpfil .NE. ' ' ) then
         action = 'Opening RTRAC Receptor Definition file.'
         fname = rcpfil
         inquire(file=rcpfil,exist=lexist)
         if( .NOT. lexist ) goto 7000
         nopen =  nopen + 1
         call getunit(iorrcp)
         write(iout,9000) 
     &         'RTRAC Receptor Definition file       (unit):',iorrcp
         write(iout,9002) rcpfil(:istrln(rcpfil))
         lrcpfil = .TRUE.
      else
         write(iout,9000) 'No RTRAC Receptor Definition file provided.'
         lrcpfil = .FALSE.
      endif
c
c   --- instantaneous file used for initialization ---
c
      if( lrstrt ) then
         action = 'Opening RTRAC master grid Restart file.'
         inifil(IDXCRS) = RT_Master_Restart
         fname = inifil(IDXCRS)
         inquire(file=inifil(IDXCRS),exist=lexist)
         if( .NOT. lexist ) goto 7000
         nopen =  nopen + 1
         call getunit(iorini(IDXCRS))
         write(iout,9000)'RTRAC master grid Restart file       (unit):',
     &                                                    iorini(IDXCRS)
         write(iout,9002) inifil(IDXCRS)(:istrln(inifil(IDXCRS)))
c
         if( ngrid .GT. 1 ) then
            action = 'Opening RTRAC nested grid Restart file.'
            inifil(IDXFIN) = RT_Nested_Restart
            if( inifil(IDXFIN) .NE. ' ' ) then
               fname = inifil(IDXFIN)
               inquire(file=inifil(IDXFIN),exist=lexist)
               if( .NOT. lexist ) goto 7000
               nopen =  nopen + 1
               call getunit(iorini(IDXFIN))
               write(iout,9000)
     &                 'RTRAC nested grid Restart file       (unit):',
     &                                                    iorini(IDXFIN)
               write(iout,9002) inifil(IDXFIN)(:istrln(inifil(IDXFIN)))
            elseif (.not.lflexi) then
               goto 7003
            endif
         endif
      else
c
c   --- initial conditions file ---	
c
         action = 'Opening RTRAC Initial Conditions file.'
         icfil = RT_Initial_Conditions
         fname = icfil
         if( fname .EQ. ' ' ) then
            licfil = .FALSE.
            write(iout,9003)
     &              'RTRAC Initial Conditions file        (unit):',
     &                                             ' Not supplied'
         else
            inquire(file=icfil,exist=lexist)
            if( .NOT. lexist ) goto 7000
            nopen =  nopen + 1
            call getunit(ioric)
            write(iout,9000)
     &              'RTRAC Initial Conditions file        (unit):',ioric
            write(iout,9002) icfil(:istrln(icfil))
            licfil = .TRUE.
         endif
      endif
c
c   --- boundary conditions file ---
c
      action = 'Opening RTRAC Boundary Conditions file.'
      bcfil = RT_Boundary_Conditions
      fname = bcfil
      if( fname .EQ. ' ' ) then
         lbcfil = .FALSE.
         write(iout,9003)'RTRAC Boundary Conditions file       (unit):',
     &                                                  ' Not supplied'
      else
         inquire(file=bcfil,exist=lexist)
         if( .NOT. lexist ) goto 7000
         nopen =  nopen + 1
         call getunit(iorbc)
         write(iout,9000)'RTRAC Boundary Conditions file       (unit):',
     &                                                             iorbc
         write(iout,9002) bcfil(:istrln(bcfil))
         lbcfil = .TRUE.
      endif
c
c   --- surface emissions file --- 
c
      do 10 j = 1,ngrid
          action = 'Opening RTRAC Gridded Emissions file.'
          temfil(j,1) = RT_Emiss_Grid(j)
          fname = temfil(j,1)
          if( fname .EQ. ' ') then
             if (lflexi) then
               ltemfl(j,1) = .FALSE.
               write(iout,9004)
     &               'Gridded Emissions for grid # ',j,'           :',
     &                                                ' Not supplied'
             else
               goto 7004
             endif
          else
             ltemfl(j,1) = .TRUE.
             inquire(file=temfil(j,1),exist=lexist)
             if( .NOT. lexist ) goto 7000
             nopen = nopen + 1
             call getunit(iortem(j,1))
             open(unit=iortem(j,1),file=temfil(j,1),ERR=7001,
     &                             form='UNFORMATTED',status='UNKNOWN')
             write(iout,9005)
     &               'Gridded Emissions for grid # ',j,'      (unit):',
     &                                                      iortem(j,1)
             write(iout,9002) temfil(j,1)(:istrln(temfil(j,1)))
          endif
   10 continue
c
c   --- elevated point source emissions ----
c
      tptfil(1) = RT_Point_Sources
      fname = tptfil(1)
      action = 'Opening RTRAC Point Source file.'
      if( fname .EQ. ' ' ) then
         ltptfl(1) = .FALSE.
         write(iout,9003)
     &                   'Point Source Emissions                     :',
     &                                                  ' Not supplied'
      else
         ltptfl(1) = .TRUE.
         inquire(file=tptfil(1),exist=lexist)
         if( .NOT. lexist ) goto 7000
         nopen = nopen + 1
         call getunit(iortpt(1))
         open(unit=iortpt(1),file=tptfil(1),ERR=7001,
     &                        form='UNFORMATTED',status='UNKNOWN')
         write(iout,9005)'Point Source Emissions               (unit):',
     &                                                         iortpt(1)
         write(iout,9002) tptfil(1)(:istrln(tptfil(1)))
      endif
c
c   --- input surface model mass file ---
c
      if (lsrfmod) then
        do j = 1,ngrid
          action = 'Opening input RTRAC Surface Model file.'
          rtsrfin(j) = RT_Srfmod_Grid(j)
          fname = rtsrfin(j)
          if( fname .EQ. ' ' ) then
             write(iout,9004)
     &               'RTRAC Surface Mass for grid # ',j,'           :',
     &                                                ' Not supplied'
             write(iout,*) 'This file is required when',
     &                     ' RT_Surface_Model = T'
             call camxerr()
          else
             inquire(file=rtsrfin(j),exist=lexist)
             if( .NOT. lexist ) goto 7000
             nopen = nopen + 1
             call getunit(iorrtsrf(j))
             write(iout,9005)
     &               'RTRAC Surface Mass for grid # ',j,'      (unit):',
     &                                                     iorrtsrf(j)
             write(iout,9002) rtsrfin(j)(:istrln(rtsrfin(j)))
          endif
        enddo
      endif
c
c   --- automatically output average concs for RTRAC ---
c
      lsfcfl = .TRUE.
c
c   --- output filenames ---
c
      do 20 i=IDXCRS,IDXFIN
         if( i .EQ. IDXFIN .AND. ngrid .EQ. 1 ) goto 20
c
c   --- output instantaneous file ---
c
         confil(i) = flrtsa
         if( i .EQ. IDXCRS ) then
            confil(i)(nchar+1:) = '.rt.inst'
         else
            confil(i)(nchar+1:) = '.rt.finst'
         endif
         fname = confil(i)
         nopen = nopen + 1
         call getunit(iowcon(i))
         open(unit=iowcon(i),file=confil(i),ERR=7002,
     &                            form='UNFORMATTED',status='UNKNOWN')
         if( i .EQ. IDXCRS ) then
            write(iout,9000)
     &                  'RTRAC INST file for master grid      (unit):',
     &                                                        iowcon(i)
            write(iout,9002) confil(i)(:istrln(confil(i)))
         else
            write(iout,9000)
     &                  'RTRAC INST file for nested grids     (unit):',
     &                                                        iowcon(i)
            write(iout,9002) confil(i)(:istrln(confil(i)))
         endif
   20 continue
c
c    ---- surface concentrations file ----
c
      if (.not.lhdfout) then
         do n = 1,ngrid
            sfcfil(n) = ' '
            fname = flrtsa
            write(fname(nchar+1:),'(a,i2.2)') '.rt.grd',n
            sfcfil(n) = fname
            nopen = nopen + 1
            call getunit(iowsfc(n))
            open(unit=iowsfc(n),file=sfcfil(n),ERR=7002,
     &                            form='UNFORMATTED',status='UNKNOWN')
            write(iout,9000)
     &                  'RTRAC output Surface file            (unit):',
     &                                                        iowsfc(n)
            write(iout,9002) sfcfil(n)(:istrln(sfcfil(n)))
         enddo
      endif
c
c    ---- surface model mass file ----
c
      if (lsrfmod) then
         do n = 1,ngrid
            rtsrfout(n) = ' '
            fname = flrtsa
            write(fname(nchar+1:),'(a,i2.2)') '.rt.srf',n
            rtsrfout(n) = fname
            nopen = nopen + 1
            call getunit(iowrtsrf(n))
            open(unit=iowrtsrf(n),file=rtsrfout(n),ERR=7002,
     &                            form='UNFORMATTED',status='UNKNOWN')
            write(iout,9000)
     &                  'RTRAC output Surface Mass file       (unit):',
     &                                                      iowrtsrf(n)
            write(iout,9002) rtsrfout(n)(:istrln(rtsrfout(n)))
         enddo
      endif
c
c    ---- receptor output file ----
c
      if( lrcpfil ) then
         avgfil(1:) = flrtsa
         avgfil(nchar+1:) = '.rt.receptor'
         fname = avgfil
         nopen = nopen + 1
         call getunit(iowrcp)
         open(unit=iowrcp,file=avgfil,ERR=7002,form='FORMATTED',
     &                                               status='UNKNOWN')
         write(iout,9000) 
     &          'RTRAC Receptor decay rate file       (unit):',iowrcp
         write(iout,9002) avgfil(:istrln(avgfil))
      endif
c
c    ---- sampling grid concentration file ----
c
      if( lsample .AND. lsmptrc .AND. .NOT. lhdfout ) then
         do n = 1,nsample
           smpfil(n)(1:) = flrtsa
           smpfil(n)(nchar+1:) = '.rt.smp'
           write(smpfil(n)(nchar+8:),'(i2.2)') n
           fname = smpfil(n)
           nopen = nopen + 1
           call getunit(iowsmp(n))
           open(unit=iowsmp(n),file=smpfil(n),ERR=7002,
     &                          form='UNFORMATTED',status='UNKNOWN')
           write(iout,9000)
     &                 'RTRAC/PiG Sampling Grid output file  (unit):',
     &                                                       iowsmp(n)
           write(iout,9002) smpfil(n)(:istrln(smpfil(n)))
         enddo
      endif
c
c  --- return to calling routine ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format(/,A,I2)
 9002 format(2A)
 9003 format(/,2A)
 9004 format(/,A,I3,2A)
 9005 format(/,A,I3,A,I3)
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,A)') 'ERROR in STARTRT:'
      write(iout,'(A)') action(:istrln(action))
      write(iout,'(/,1X,2A)') 'Input file does not exist: ',
     &                                           fname(:istrln(fname))
      call camxerr()
c
 7001 continue
      write(iout,'(//,A)') 'ERROR in STARTRT:'
      write(iout,'(A)') action(:istrln(action))
      write(iout,'(/,1X,2A)') 'Cannot open input file: ',
     &                                           fname(:istrln(fname))
      call camxerr()
c
 7002 continue
      write(iout,'(//,A)') 'ERROR in STARTRT:'
      write(iout,'(/,1X,2A)') 'Cannot open output file: ',
     &                                           fname(:istrln(fname))
      call camxerr()
c
 7003 continue
      write(iout,'(//,A)') 'ERROR in STARTRT:'
       write(iout,'(/,1X,3A,/,A)') 'Nested grid restart file for ',
     &                     tectyp(:istrln(tectyp)),' not supplied.',
     &                  ' Set Flexi_Nest = .true. to ignore this file.'
      call camxerr()
c
 7004 continue
      write(iout,'(//,A)') 'ERROR in STARTRT:'
       write(iout,'(/,1X,3A,/,A)') 'Nested grid emissions file for ',
     &                     tectyp(:istrln(tectyp)),' not supplied.',
     &                  ' Set Flexi_Nest = .true. to ignore this file.'
      call camxerr()
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

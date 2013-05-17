c**** STARTSA
c
      subroutine startsa(iout,nopen)
      use grid
      use chmstry
      use ptemiss
      use procan
      use tracer
      implicit none
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     This routine loads all of the files needed for the source 
c     apportionment algorithm.  The output files are opened, the input
c     files are opened as needed.
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
c     12/16/99   --gwilson--   Fixed a bug which caused the model to try
c                              and read a restart file for each find grid
c     07/19/02   --gwilson--   Added code for source area map for nests
c     10/06/04   --cemery --   Restructured for namelist input
c      5/25/06   --cemery--    Average sfc grid files not opened if
c                              HDF = true
c      8/23/06   --cemery--    Instantaneous restart files reduced to 1
c                              per grid type
c      8/25/06   --cemery--    Surface output files now all UAM format,
c                              one file per grid
c     03/15/09   --gwilson--   Added code for deposition output for tracers
c     05/07/12   --cemery--    Added flexi-nesting flag
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
      include 'namelist.inc'
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
      integer      igrd, i, j, n
      logical      lexist
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- if option is turned off, just return ---
c
      if( .NOT. ltrace ) goto 9999
      nchar = istrln( flrtsa )
c
c  -- call routine to allocate arrays ----
c
      call alloc_tracer(ngroup,ngrid,ncol,nrow,nlay,nspec)
      call alloc_tracer_full(ngrid,ncol,nrow)
c
c   --- intialize first time through to true ---
c
      lfirst = .TRUE.
      write(iout,'(/,A,/)') 
     &            '           **** Source Apportionment Files ****'
c
c   --- source area mapping file ---
c
      if( nregin .GT. 0 ) then
         do 10 igrd = 1,ngrid
            write(action,'(A,I2)')
     &                        'Opening Source Area Map for grid: ',igrd
            mapfil(igrd) = SA_Source_Area_Map(igrd)
            fname = mapfil(igrd)
            if( fname .EQ. ' ' ) then
               if( igrd .EQ. 1 ) goto 7004
               lmapfl(i grd) = .FALSE.
               write(iout,9000)
     &               'No Source Area Map file for Grid #         :',igrd
               goto 10
            endif
            inquire(file=mapfil(igrd),exist=lexist)
            if( .NOT. lexist ) goto 7000
            nopen =  nopen + 1
            call getunit(iormap(igrd))
            write(iout,9001)
     &            'Source area map file for grid # ',igrd,'   (unit):',
     &                                                      iormap(igrd)
            write(iout,9002) mapfil(igrd)(:istrln(mapfil(igrd)))
            lmapfl(igrd) = .TRUE.
   10    continue
      endif
c
c   --- receptor definition file ----
c
      rcpfil = SA_Receptor_Definitions
      if( rcpfil .NE. ' ' ) then
         action = 'Opening SA Receptor Definition file.'
         fname = rcpfil
         inquire(file=rcpfil,exist=lexist)
         if( .NOT. lexist ) goto 7000
         nopen =  nopen + 1
         call getunit(iorrcp)
         write(iout,9000)'SA Receptor definition file          (unit):',
     &                                                          iorrcp
         write(iout,9002) rcpfil(:istrln(rcpfil))
         lrcpfil = .TRUE.
      else
         write(iout,9000)'No SA Receptor definition file provided.'
         lrcpfil = .FALSE.
      endif
c
c   --- instantaneous file used for initialization ---
c
      if( lrstrt ) then
         action = 'Opening SA master grid Restart file.'
         inifil(IDXCRS) = SA_Master_Restart
         fname = inifil(IDXCRS)
         inquire(file=inifil(IDXCRS),exist=lexist)
         if( .NOT. lexist ) goto 7000
         nopen =  nopen + 1
         call getunit(iorini(IDXCRS))
         write(iout,9000)
     &                'SA master grid Restart file          (unit):',
     &                                                  iorini(IDXCRS)
         write(iout,9002) inifil(IDXCRS)(:istrln(inifil(IDXCRS)))
c
         if( ngrid .GT. 1 ) then
            action = 'Opening SA nested grid Restart file.'
            inifil(IDXFIN) = SA_Nested_Restart
            if( inifil(IDXFIN) .NE. ' ' ) then
                fname = inifil(IDXFIN)
                inquire(file=inifil(IDXFIN),exist=lexist)
                if( .NOT. lexist ) goto 7000
                nopen =  nopen + 1
                call getunit(iorini(IDXFIN))
                write(iout,9000)
     &                  'SA nested grid Restart file          (unit):',
     &                                                    iorini(IDXFIN)
                write(iout,9002) inifil(IDXFIN)(:istrln(inifil(IDXFIN)))
            elseif (.not.lflexi) then
                goto 7003
            endif
         endif
      endif
c
c   --- emissions files for the source groupings ---
c
      do 20 i = 1,ngroup
c
c   --- surface emissions file --- 
c
         do 30 j = 1,ngrid
            action = 'Opening SA Gridded Emissions file.'
            temfil(j,i) = SA_Emiss_Group_Grid(i,j)
            fname = temfil(j,i)
            if( fname .EQ. ' ' ) then
               ltemfl(j,i) = .FALSE.
               write(iout,9003)
     &                 'Gridded Emissions file for grid#/group#    :',
     &                                               j,i,' Not supplied'
            else
               ltemfl(j,i) = .TRUE.
               inquire(file=temfil(j,i),exist=lexist)
               if( .NOT. lexist ) goto 7000
               nopen = nopen + 1
               call getunit(iortem(j,i))
               open(unit=iortem(j,i),file=temfil(j,i),
     &                     ERR=7001,form='UNFORMATTED',status='UNKNOWN')
               write(iout,9005)
     &                  'Gridded Emissions for grid#/group#   (unit):',
     &                                                   j,i,iortem(j,i)
               write(iout,9002) temfil(j,i)(:istrln(temfil(j,i)))
            endif
   30    continue
c
c   --- elevated point source emissions ----
c
         tptfil(i) = SA_Points_Group(i)
         fname = tptfil(i)
         action = 'Opening SA Point Source file.'
         if( fname .EQ. ' ' ) then
            ltptfl(i) = .FALSE.
            write(iout,9004)
     &               'Point Source Emissions file for group#     :',
     &                                                 i,' Not supplied'
         else
            ltptfl(i) = .TRUE.
            inquire(file=tptfil(i),exist=lexist)
            if( .NOT. lexist ) goto 7000
            nopen = nopen + 1
            call getunit(iortpt(i))
            open(unit=iortpt(i),file=tptfil(i),ERR=7001,
     &                              form='UNFORMATTED',status='UNKNOWN')
            write(iout,9005)
     &                 'Point Source Emissions for group#    (unit):',
     &                                                       i,iortpt(i)
            write(iout,9002) tptfil(i)(:istrln(tptfil(i)))
         endif
   20 continue
c
c   --- output filenames ---
c
      do 40 i=IDXCRS,IDXFIN
         if( i .EQ. IDXFIN .AND. ngrid .EQ. 1 ) goto 40
c
c   --- output instantaneous file ---
c
         confil(i) = flrtsa
         if( i .EQ. IDXCRS ) then
            confil(i)(nchar+1:) = '.sa.inst'
         else
            confil(i)(nchar+1:) = '.sa.finst'
         endif
         fname = confil(i)
         nopen = nopen + 1
         call getunit(iowcon(i))
         open(unit=iowcon(i),file=confil(i),ERR=7002,
     &                           form='UNFORMATTED',status='UNKNOWN')
         if( i .EQ. IDXCRS ) then
            write(iout,9000)
     &                 'SA INST file for master grid         (unit):',
     &                                                         iowcon(i)
            write(iout,9002) confil(i)(:istrln(confil(i)))
         else
            write(iout,9000)
     &                 'SA INST file for nested grids        (unit):',
     &                                                         iowcon(i)
            write(iout,9002) confil(i)(:istrln(confil(i)))
         endif
   40 continue
c
c    ---- surface concentrations file ----
c
      if( .NOT. lsfcfl ) then
         write(iout,9006)
     &                 'SA Surface file                      (unit):',
     &                                                  ' Not supplied'
      elseif( .NOT. lhdfout ) then
         do n = 1,ngrid
            fname = flrtsa
            write(fname(nchar+1:),'(a,i2.2)') '.sa.grd',n
            sfcfil(n) = fname
            nopen = nopen + 1
            call getunit(iowsfc(n))
            open(unit=iowsfc(n),file=sfcfil(n),ERR=7002,
     &                            form='UNFORMATTED',status='UNKNOWN')
            write(iout,9000)
     &                  'SA Surface file                      (unit):',
     &                                                         iowsfc(n)
            write(iout,9002) sfcfil(n)(:istrln(sfcfil(n)))
         enddo
c
c  --- if doing deposition output, open the files ---
c
         if( lptdepout ) then
            do n = 1,ngrid
               fname = flrtsa
               write(fname(nchar+1:),'(a,i2.2)') '.sa.depn.grd',n
               ptdepfil(n) = fname
               nopen = nopen + 1
               call getunit(iowptdep(n))
               open(unit=iowptdep(n),file=ptdepfil(n),ERR=7002,
     &                            form='UNFORMATTED',status='UNKNOWN')
               write(iout,9000)
     &                  'SA Deposition file                   (unit):',
     &                                                      iowptdep(n)
               write(iout,9002) ptdepfil(n)(:istrln(ptdepfil(n)))
            enddo
         endif
      endif
c
c    ---- tracer receptor file ----
c
      if( lrcpfil ) then
         avgfil(1:) = flrtsa
         avgfil(nchar+1:) = '.sa.receptor'
         fname = avgfil
         nopen = nopen + 1
         call getunit(iowrcp)
         open(unit=iowrcp,file=avgfil,ERR=7002,form='FORMATTED',
     &                                               status='UNKNOWN')
         write(iout,9000)
     &            'SA Receptor concentration file       (unit):',iowrcp
         write(iout,9002) avgfil(:istrln(avgfil))
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
 9001 format(/,A,I2,A,I2)
 9002 format(2A)
 9003 format(/,A,I2,1X,I2,A)
 9004 format(/,A,I2,A)
 9005 format(/,A,I2,1X,I2,1X,I2)
 9006 format(/,2A)
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,A)') 'ERROR in STARTSA:'
      write(iout,'(A)') action(:istrln(action))
      write(iout,'(/,1X,2A)') 'Input file does not exist: ',
     &                                             fname(:istrln(fname))
      call camxerr()
c
 7001 continue
      write(iout,'(//,A)') 'ERROR in STARTSA:'
      write(iout,'(A)') action(:istrln(action))
      write(iout,'(/,1X,2A)') 'Cannot open input file: ',
     &                                             fname(:istrln(fname))
      call camxerr()
c
 7002 continue
      write(iout,'(//,A)') 'ERROR in STARTSA:'
      write(iout,'(/,1X,2A)') 'Cannot open output file: ',
     &                                             fname(:istrln(fname))
      call camxerr()
c
 7003 continue
      write(iout,'(//,A)') 'ERROR in STARTSA:'
      write(iout,'(/,1X,3A,/,A)') 'Nested grid restart file for ',
     &                     tectyp(:istrln(tectyp)),' not supplied.',
     &                  ' Set Flexi_Nest = .true. to ignore this file.'
      call camxerr()
c
 7004 continue
      write(iout,'(//,A)') 'ERROR in STARTSA:'
      write(iout,'(/,2A)') 'A source area mapping file must be ',
     &                'supplied for the master grid, Grid #1'
      call camxerr()
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

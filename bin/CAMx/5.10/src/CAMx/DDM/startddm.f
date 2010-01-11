c**** STARTDDM.F
c
      subroutine startddm(iout,nopen)
      use grid
      use chmstry
      use procan
      use tracer
      implicit none
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c     This routine loads all of the files needed for the source 
c     apportionment algorithm.  The output files are opened, the input
c     files are opened as needed.
c
c     Copyright 1996 - 2009
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
c     07/19/02   --gwilson--   Added code for source area map for nests
c     10/06/04   --cemery --   Restructured for namelist input
c      5/25/06   --cemery--    Average sfc grid files not opened if
c                              HDF = true
c      8/23/06   --cemery--    Instantaneous restart files reduced to 1
c                              per grid type
c      8/25/06   --cemery--    Surface output files now all UAM format,
c                              one file per grid
c     07/16/07   --bkoo--      Added check for HDDM
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.com'
      include 'namelist.com'
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
      integer      i, j, igrd, n
      logical      lexist
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- if option is turned off, just return ---
c
      if( .NOT. lddm .AND. .NOT. lhddm ) goto 9999
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
     &            '           **** DDM Files ****'
c
c   --- source area mapping file ---
c
      do 10 igrd = 1,ngrid
         write(action,'(A,I2)') 
     &   'Opening Source Area Map for grid: ',igrd
                                mapfil(igrd) = DDM_Source_Area_Map(igrd)
         fname = ' '
         fname = mapfil(igrd)
         if( nregin .LE. 0 ) then
            lmapfl(igrd) = .FALSE.
            goto 10
         endif 
         if( fname .EQ. ' ' ) then
            if( igrd .EQ. 1 ) goto 7004
            lmapfl(igrd) = .FALSE.
            write(iout,9001)
     &              'No Source Area Map file for Grid #         :',igrd
            goto 10
         endif
         inquire(file=mapfil(igrd),exist=lexist)
         if( .NOT. lexist ) goto 7000
         nopen =  nopen + 1
         call getunit(iormap(igrd))
         write(iout,9001)  'Source area map file for grid # ',igrd,
     &                                        '   (unit):',iormap(igrd)
         write(iout,9002) mapfil(igrd)(:istrln(mapfil(igrd)))
         lmapfl(igrd) = .TRUE.
   10 continue
c
c   --- receptor definition file ----
c
      rcpfil = ' '
      rcpfil = DDM_Receptor_Definitions
      if( rcpfil .NE. ' ' ) then
         action = 'Opening DDM Receptor Definition file.'
         fname = rcpfil
         inquire(file=rcpfil,exist=lexist)
         if( .NOT. lexist ) goto 7000
         nopen =  nopen + 1
         call getunit(iorrcp)
         write(iout,9000) 
     &       'DDM Receptor definition file         (unit):',iorrcp
         write(iout,9002) rcpfil(:istrln(rcpfil))
         lrcpfil = .TRUE.
      else
         write(iout,9000) 'No DDM Receptor definition file provided.'
         lrcpfil = .FALSE.
      endif
c
c   --- instantaneous file used for initialization ---
c
      if( lrstrt ) then
         action = 'Opening DDM master grid Restart file.'
         inifil(IDXCRS) = ' '
         inifil(IDXCRS) = DDM_Master_Restart
         fname = inifil(IDXCRS)
         inquire(file=inifil(IDXCRS),exist=lexist)
         if( .NOT. lexist ) goto 7000
         nopen =  nopen + 1
         call getunit(iorini(IDXCRS))
         write(iout,9000)'DDM master grid Restart file         (unit):',
     &                                                    iorini(IDXCRS)
         write(iout,9002) inifil(IDXCRS)(:istrln(inifil(IDXCRS)))
c
         if( ngrid .GT. 1 ) then
            action = 'Opening DDM nested grid Restart file.'
            inifil(IDXFIN) = ' '
            inifil(IDXFIN) = DDM_Nested_Restart
            if( inifil(IDXFIN) .NE. ' ' ) then
               fname = inifil(IDXFIN)
               inquire(file=inifil(IDXFIN),exist=lexist)
               if( .NOT. lexist ) goto 7000
               nopen =  nopen + 1
               call getunit(iorini(IDXFIN))
               write(iout,9001)
     &                'DDM nested grid Restart file         (unit):',
     &                                                    iorini(IDXFIN)
               write(iout,9002) inifil(IDXFIN)(:istrln(inifil(IDXFIN)))
            endif
         endif
      endif
c
c   --- IC file for DDM ---
c
      if( nicddm .GT. 0 .AND. .NOT. lrstrt ) then
         action = 'Opening DDM Initial Conditions file.'
         icfil = ' '
         icfil = DDM_Initial_Conditions
         fname = icfil
         inquire(file=icfil,exist=lexist)
         if( .NOT. lexist ) goto 7000
         nopen =  nopen + 1
         call getunit(ioric)
         write(iout,9000)'DDM Initial Conditions file          (unit):',
     &                                                             ioric
         write(iout,9002) icfil(:istrln(icfil))
      endif
c
c   --- BC file and TOPCONC file for DDM ---
c
      if( nbcddm .GT. 0 ) then
         action = 'Opening DDM Boundary Conditions file.'
         bcfil = ' '
         bcfil = DDM_Boundary_Conditions
         fname = bcfil
         inquire(file=bcfil,exist=lexist)
         if( .NOT. lexist ) goto 7000
         nopen =  nopen + 1
         call getunit(iorbc)
         write(iout,9000)'DDM Boundary Conditions file         (unit):',
     &                                                             iorbc
         write(iout,9002) bcfil(:istrln(bcfil))
c
         action = 'Opening DDM Top Concentration file.'
         tcfil = ' '
         tcfil = DDM_Top_Concentrations
         fname = tcfil
         inquire(file=tcfil,exist=lexist)
         if( .NOT. lexist ) goto 7000
         nopen =  nopen + 1
         call getunit(iortc)
         write(iout,9000)'DDM Top Concentrations file         (unit):',
     &                                                            iortc
         write(iout,9002) tcfil(:istrln(tcfil))
      endif
c
c   --- emissions files for the source groupings,
c       skip if not doing any emissions DDM species ---
c
      if( nemddm .EQ. 0 ) goto 111
      do 20 i=1,ngroup
c
c   --- surface emissions file --- 
c
         do 30 j=1,ngrid
            action = 'Opening DDM Gridded Emissions file.'
            temfil(j,i) = ' '
            temfil(j,i) = DDM_Emiss_Group_Grid(i,j)
            fname = temfil(j,i)
            if( fname .EQ. ' ' ) then
               ltemfl(j,i) = .FALSE.
               write(iout,9003)
     &         'Gridded emissions file for grid#/group#    :',
     &                                     j,i,' Not supplied'
            else
               ltemfl(j,i) = .TRUE.
               inquire(file=temfil(j,i),exist=lexist)
               if( .NOT. lexist ) goto 7000
               nopen = nopen + 1
               call getunit(iortem(j,i))
               open(unit=iortem(j,i),file=temfil(j,i),
     &                    ERR=7001,form='UNFORMATTED',status='UNKNOWN')
               write(iout,9005)
     &         'Gridded emissions for grid#/group#   (unit):',
     &                                                 j,i,iortem(j,i)
               write(iout,9002) temfil(j,i)(:istrln(temfil(j,i)))
            endif
   30    continue
c
c   --- elevated point source emissions ----
c
         tptfil(i) = ' '
         tptfil(i) = DDM_Points_Group(i)
         fname = tptfil(i)
         action = 'Opening DDM Point Source file.'
         if( fname .EQ. ' ' ) then
            ltptfl(i) = .FALSE.
            write(iout,9004)
     &      'Point Source Emissions file for group#     :',
     &                                    i,' Not supplied'
         else
            ltptfl(i) = .TRUE.
            inquire(file=tptfil(i),exist=lexist)
            if( .NOT. lexist ) goto 7000
            nopen = nopen + 1
            call getunit(iortpt(i))
            open(unit=iortpt(i),file=tptfil(i),ERR=7001,
     &                        form='UNFORMATTED',status='UNKNOWN')
            write(iout,9005)
     &                 'Point Source Emissions for group#    (unit):',
     &                                                      i,iortpt(i)
            write(iout,9002) tptfil(i)(:istrln(tptfil(i)))
         endif
   20 continue
c
c   --- output filenames ---
c
  111 continue
      do 40 i=IDXCRS,IDXFIN
         if( i .EQ. IDXFIN .AND. ngrid .EQ. 1 ) goto 40
c
c   --- output instantaneous file ---
c
         confil(i)(1:) =  ' '
         confil(i)(1:) = flrtsa(1:nchar)
         if( i .EQ. IDXCRS ) then
            confil(i)(nchar+1:) = '.ddm.inst'
         else
            confil(i)(nchar+1:) = '.ddm.finst'
         endif
         fname = confil(i)
         nopen = nopen + 1
         call getunit(iowcon(i))
         open(unit=iowcon(i),file=confil(i),ERR=7002,
     &                              form='UNFORMATTED',status='UNKNOWN')
         if( i .EQ. IDXCRS ) then
            write(iout,9000)
     &          'DDM INST file for master grid        (unit):',iowcon(i)
            write(iout,9002) confil(i)(:istrln(confil(i)))
         else
            write(iout,9000)
     &          'DDM INST file for nested grids       (unit):',iowcon(i)
            write(iout,9002) confil(i)(:istrln(confil(i)))
         endif
   40 continue
c
c    ---- surface concentrations file ----
c
      if( .not.lsfcfl ) then
         write(iout,9006)
     &              'DDM Surface file                     (unit):',
     &                                                ' Not supplied'
      elseif (.not.lhdfout) then
         do n = 1,ngrid
            sfcfil(n)(1:nchar+10) = ' '
            fname = flrtsa(1:nchar)
            write(fname(nchar+1:),'(a,i2.2)') '.ddm.grd',n
            sfcfil(n)(1:nchar+10) = fname
            nopen = nopen + 1
            call getunit(iowsfc(n))
            open(unit=iowsfc(n),file=sfcfil(n),ERR=7002,
     &                           form='UNFORMATTED',status='UNKNOWN')
            write(iout,9000)
     &                'DDM Surface file                     (unit):',
     &                                                      iowsfc(n)
            write(iout,9002) sfcfil(n)(:istrln(sfcfil(n)))
         enddo
      endif
c
c    ---- receptor file ----
c
      if( lrcpfil ) then
         avgfil(1:) = ' '
         avgfil(1:) = flrtsa(1:nchar)
         avgfil(nchar+1:) = '.ddm.receptor'
         fname = avgfil
         nopen = nopen + 1
         call getunit(iowrcp)
         open(unit=iowrcp,file=avgfil,ERR=7002,form='FORMATTED',
     &                                      status='UNKNOWN')
         write(iout,9000) 
     &     'DDM Receptor concentration file       (unit):',iowrcp
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
      write(iout,'(//,A)') 'ERROR in STARTDDM:'
      write(iout,'(A)') action(:istrln(action))
      write(iout,'(/,1X,2A)') 'Input file does not exist: ',fname
      call camxerr()
c
 7001 continue
      write(iout,'(//,A)') 'ERROR in STARTDDM:'
      write(iout,'(A)') action(:istrln(action))
      write(iout,'(/,1X,2A)') 'Cannot open input file: ',fname
      call camxerr()
c
 7002 continue
      write(iout,'(//,A)') 'ERROR in STARTDDM:'
      write(iout,'(/,1X,2A)') 'Cannot open output file: ',fname
      call camxerr()
c
 7004 continue
      write(iout,'(//,A)') 'ERROR in STARTDDM:'
      write(iout,'(/,A)') 'A source area mapping file must be ',
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

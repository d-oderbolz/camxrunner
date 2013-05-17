      subroutine readnml(version,enddate,endtim,numprocs)
      use filunit
      use grid
      use chmstry
      use o3colmap
      use camxfld
      use camxcom
      use pigsty
      use procan
      use tracer
      use node_mod
      use rtracchm
c
      implicit none
c
c----CAMx v6.00 130506
c
c     READNML opens and reads the CAMx input namelist file called "CAMx.in"
c     that defines user inputs.  All namelist variables are mapped to 
c     internal variables and checked for appropriate/consistent values.
c                          
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c          
c     Modifications:
c        7/20/05       Moved PiG sampling grid inputs to main namelist, added
c                      new options for sampling standard species
c        7/11/07       Added new RTCMC Probing Tool option
c        07/16/07 -bkoo-     Revised for HDDM
c        04/24/08 -gyarwood- Added EBI chemistry solver option
c        06/11/08 -bkoo-     Added rate constant sensitivity
c        10/09/08      Added ACM2 option
c        07/16/08 -bkoo-     Added DDM turn-off flag
c        01/30/09 -bkoo-     Removed the CMC fast solver
c        10/29/09      Added code for RTRAC surface model
c        07/14/10      Added in-line TUV option
c        03/29/11      Added option for inert PM with gas-phase chemistry
c                      and support in-line TUV with aerosol optical depth
c        04/02/12      Removed RADM cloud adjustment option, cloud/aerosol
c                      adjustments now always done with in-line TUV; AHO
c                      file is now just ozone column; number of output
c                      species now determined internally from user list
c        05/07/12      Added flexi-nesting flag
c
c     Input arguments:
c        version             model version character string
c
c     Output arguments:
c        none
c
c     Routines Called:
c        CVTDATE
c        JSTLFT
c        ISTRLN
c        TOUPPER
c        INIPTR
c        OPENFILS
c
c     Called by:
c        STARTUP
c
      include 'camx.prm'
      include 'flags.inc'
      include 'namelist.inc'
      include 'deposit.inc'
      include 'mpif.h'
      include 'rtracsrf.inc'
c
      logical   Dry_Deposition
      namelist /CAMx_Control/
     & Run_Message,Time_Zone,Restart,Start_Date_Hour,End_Date_Hour,
     & Maximum_Timestep,Met_Input_Frequency,Ems_Input_Frequency,
     & Output_Frequency,Map_Projection,UTM_Zone,POLAR_Longitude_Pole,
     & POLAR_Latitude_Pole,LAMBERT_Central_Meridian,
     & LAMBERT_Center_Longitude,LAMBERT_Center_Latitude,
     & LAMBERT_True_Latitude1,LAMBERT_True_Latitude2,Number_of_Grids,
     & Master_Origin_XCoord,Master_Origin_YCoord,Master_Cell_XSize,
     & Master_Cell_YSize,Master_Grid_Columns,Master_Grid_Rows,
     & Number_of_Layers,Nest_Meshing_Factor,Nest_Beg_I_Index,
     & Nest_End_I_Index,Nest_Beg_J_Index,Nest_End_J_Index,
     & Diagnostic_Error_Check,Advection_Solver,Chemistry_Solver,
     & Drydep_Model,PiG_Submodel,Probing_Tool,Chemistry,Super_Stepping,
     & Wet_Deposition,ACM2_Diffusion,Gridded_Emissions,Point_Emissions,
     & Ignore_Emission_Dates,Root_Output_Name,
     & Average_Output_3D,Output_3D_Grid,HDF_Format_Output,HDF_File_Root,
     & Output_Species_Names,PiG_Sampling_Grid,
     & Sample_Background,Number_of_Sampling_Grids,SG_Beg_I_Index,
     & SG_End_I_Index,SG_Beg_J_Index,SG_End_J_Index,SG_Mesh_Factor,
     & Flexi_Nest,Chemistry_Parameters,Photolyis_Rates,
     & Initial_Conditions,Boundary_Conditions,Top_Concentrations,
     & Ozone_Column,Point_Sources,Master_Grid_Restart,
     & Nested_Grid_Restart,PiG_Restart,Surface_Grid,Met2D_Grid,
     & Met3D_Grid,Cloud_Grid,Vdiff_Grid,Emiss_Grid,
     & Dry_Deposition
c
c======================== Probing Tool Begin ===========================
c
      namelist /SA_Control/
     & SA_File_Root,SA_Master_Sfc_Output,SA_Nested_Sfc_Output,
     & SA_Deposition_Output,
     & SA_Stratify_Boundary,SA_Number_of_Source_Regions,
     & SA_Number_of_Source_Groups,Use_Leftover_Group,
     & Number_of_Timing_Releases,SA_Receptor_Definitions,
     & SA_Source_Area_Map,SA_Master_Restart,SA_Nested_Restart,
     & SA_Points_Group,SA_Emiss_Group_Grid,SA_Summary_Output,
     & PSAT_Treat_SULFATE_Class,PSAT_Treat_NITRATE_Class,
     & PSAT_Treat_SOA_Class,PSAT_Treat_PRIMARY_Class,
     & PSAT_Treat_MERCURY_Class,PSAT_Treat_OZONE_Class,PSAT_Use_APCA
c
      namelist /DDM_Control/
     & DDM_File_Root,DDM_Master_Sfc_Output,DDM_Nested_Sfc_Output,
     & DDM_Stratify_Boundary,DDM_Number_of_Source_Regions,
     & DDM_Number_of_Source_Groups,Number_of_IC_Species_Groups,
     & IC_Species_Groups,Number_of_BC_Species_Groups,BC_Species_Groups,
     & Number_of_EM_Species_Groups,Emis_Species_Groups,
     & Number_of_Rate_Const_Groups,Rate_Const_Groups,
     & Number_of_HDDM_Sens_Groups,HDDM_parameters,
     & DDM_Receptor_Definitions,DDM_Source_Area_Map,
     & DDM_Initial_Conditions,DDM_Boundary_Conditions,
     & DDM_Top_Concentrations,DDM_Master_Restart,DDM_Nested_Restart,
     & DDM_Points_Group,DDM_Emiss_Group_Grid,DDM_Calc_Grid
c
      namelist /RT_Control/
     & RT_File_Root,RT_Initial_Conditions,RT_Boundary_Conditions,
     & RT_Top_Concentrations,RT_Master_Restart,RT_Nested_Restart,
     & RT_Chemistry_Parameters,RT_Receptor_Definitions,RT_Point_Sources,
     & RT_Emiss_Grid,RT_PiG_Sample,RT_Surface_Model,RT_Partitioning,
     & RT_Srfmod_Grid
c
      namelist /PA_Control/
     & PA_File_Root,Number_of_PA_Domains,Within_CAMx_Grid,
     & PA_Beg_I_Index,PA_End_I_Index,PA_Beg_J_Index,PA_End_J_Index,
     & PA_Beg_K_Index,PA_End_K_Index
c
c======================== Probing Tool End =============================
c
      character*20  version
      integer       enddate
      real          endtim
      integer       numprocs
c      
      integer istrln,i,jj,l,n,ibyr,ibmo,ibdy,ieyr,iemo,iedy
      integer ng
      character*200 ctlfil,filtmp
      character*100 action
      character*30  keyword
      character*20  namegrp
      integer   nemiss,cbdate,cedate,inp,ii,iifroot,nopen
      logical   lexist, lacross
c
      data inp /3/
      data ctlfil /'CAMx.in'/
c
c-----Entry point
c
c-----Open user control file and read core model parameters
c
      Dry_Deposition = .FALSE.
      inquire(file=ctlfil,exist=lexist)
      if( .NOT. lexist ) goto 7007
      open(unit=inp,file=ctlfil,STATUS='UNKNOWN',ERR=7005)
      namegrp = 'CAMx_Control'
      action = ' '
      read(inp,CAMx_Control,END=7100,ERR=7102)
c
      runmsg = Run_Message
      filroot = Root_Output_Name
      call jstlft( filroot )
      ii = istrln( filroot )
      if (filroot .EQ. ' ') goto 7006
c
c-----Open ASCII output, diagnostic, and mass reporting files
c
      filtmp = filroot
      filtmp(ii+1:) = '.out'
      call getunit(iout)
      action = 'Opening OUT message file.'
      open(unit=iout,file=filtmp(1:ii+4),status='UNKNOWN',ERR=7000)
c
      filtmp(ii+1:) = '.diag'
      call getunit(idiag)
      action = 'Opening DIAG diagnostic file.'
      open(unit=idiag,file=filtmp(1:ii+5),status='UNKNOWN',ERR=7000)
c
      filtmp(ii+1:) = '.mass'
      call getunit(imass)
      action = 'Opening MASS summary file.'
      open(unit=imass,file=filtmp(1:ii+5),status='UNKNOWN',ERR=7000)
      iifroot = ii
      nopen = 3
c
c-----Write model version to output and diagnostic files
c
      write(iout,8000) version(:istrln(version))
      write(idiag,8000) version(:istrln(version))
 8000 format(//,30x,20('*'),/,30x,a,/,30x,20('*'),//) 
      write(iout,8001) runmsg(:istrln(runmsg))
      write(idiag,8001) runmsg(:istrln(runmsg))
 8001 format(/,a,/)
c
c-----Set internal variables from namelist parameters and check inputs
c
c-----Clock management
c
      itzon = Time_Zone
      ibyr = Start_Date_Hour(1)
      ibmo = Start_Date_Hour(2)
      ibdy = Start_Date_Hour(3)
      begtim = float(Start_Date_hour(4))
      if (ibyr.eq.0 .or. ibmo.eq.0 .or. ibdy.eq.0) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(a)') 'Simulation start date contains zeros '
        write(iout,'(a,i5)') 'Year:  ',ibyr
        write(iout,'(a,i5)') 'Month: ',ibmo
        write(iout,'(a,i5)') 'Day:   ',ibdy
        call camxerr()
      endif
      call cvtdate(ibyr,ibmo,ibdy,begtim,cbdate,begdate)
c
      ieyr = End_Date_Hour(1)
      iemo = End_Date_Hour(2)
      iedy = End_Date_Hour(3)
      endtim = float(End_Date_Hour(4))
      if (ieyr.eq.0 .or. iemo.eq.0 .or. iedy.eq.0)  then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(a)') 'Simulation end date contains zeros '
        write(iout,'(a,i5)') 'Year:  ',ieyr
        write(iout,'(a,i5)') 'Month: ',iemo
        write(iout,'(a,i5)') 'Day:   ',iedy
        call camxerr()
      endif
      call cvtdate(ieyr,iemo,iedy,endtim,cedate,enddate)
c
      if( enddate .LT. begdate ) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(a,a)') 'Simulation end date is less than ',
     &                                 'simulation start date.'
        write(iout,'(a,i10.5)') 'Simulation start: ',begdate
        write(iout,'(a,i10.5)') 'Simulation end:   ',enddate
        call camxerr()
      elseif( enddate .EQ. begdate ) then
        if( endtim .LE. begtim ) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(a,a)') 'Simulation end time is less than ',
     &                                   'simulation start time.'
          write(iout,'(a,f10.0)') 'Simulation start: ',begtim
          write(iout,'(a,f10.0)') 'Simulation end:   ',endtim
          call camxerr()
        endif
      endif
c
c-----Max timestep and I/O frequencies
c
      dtmax = Maximum_Timestep
      dtinp = Met_Input_Frequency
      dtems = Ems_Input_Frequency
      dtout = Output_Frequency
c
      if( dtmax .LE. 1 .OR. dtinp .LE. 1. .or. dtems .LE. 1.
     &                                    .or. dtout .LE. 1.) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(/,a,/)')
     &            'I/O frequencies must be greater than 1 minute'
        write(iout,'(a,f6.3)')'Maximum time step   (DTMAX): ',dtmax
        write(iout,'(a,f6.3)')'Input interval      (DTINP): ',dtinp
        write(iout,'(a,f6.3)')'Emissions interval  (DTEMS): ',dtems
        write(iout,'(a,f6.3)')'Output interval     (DTOUT): ',dtout
        call camxerr()
      endif
      if( dtmax .GT. MAXDT ) then
        write(iout,'(//,A)') 'WARNING in READNML:'
        write(iout,'(A)') 
     &               'Invalid value specified for maximum time step.'
        write(iout,'(A)') 'A default value will be used instead.'
        write(iout,'(a,f6.0)')'Maximum time step (DTMAX)     : ',dtmax
        write(iout,'(a,f6.0)')'Default value to be used      : ',MAXDT
        dtmax = MAXDT
      endif
      if( (dtmax .GT. 60. .AND. amod(dtmax,60.) .GT. 0. ) .OR.
     &    (dtinp .GT. 60. .AND. amod(dtinp,60.) .GT. 0. ) .OR.
     &    (dtems .GT. 60. .AND. amod(dtems,60.) .GT. 0. ) .OR.
     &    (dtout .GT. 60. .AND. amod(dtout,60.) .GT. 0. )) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(a)')'An Input/Output interval is > 60 minutes.'
        write(iout,'(a)')'It must be an integer multiple of 60 minutes.'
        write(iout,'(a,f6.0)')'Maximum time step   (DTMAX): ',dtmax
        write(iout,'(a,f6.0)')'Input interval      (DTINP): ',dtinp
        write(iout,'(a,f6.0)')'Emissions interval  (DTEMS): ',dtems
        write(iout,'(a,f6.0)')'Output interval     (DTOUT): ',dtout
        call camxerr()
      endif
      if( (dtmax .LT. 60. .AND. amod(60.,dtmax) .GT. 0. ) .OR.
     &    (dtinp .LT. 60. .AND. amod(60.,dtinp) .GT. 0. ) .OR.
     &    (dtems .LT. 60. .AND. amod(60.,dtems) .GT. 0. ) .OR.
     &    (dtout .LT. 60. .AND. amod(60.,dtout) .GT. 0. )) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(a)')'An Input/Output interval is < 60 minutes.'
        write(iout,'(a)')'It must divide 60 minutes evenly.'
        write(iout,'(a,f6.0)')'Maximum time step   (DTMAX): ',dtmax
        write(iout,'(a,f6.0)')'Input interval      (DTINP): ',dtinp
        write(iout,'(a,f6.0)')'Emissions interval  (DTEMS): ',dtems
        write(iout,'(a,f6.0)')'Output interval     (DTOUT): ',dtout
        call camxerr()
      endif
      if( amod(amax1(dtinp,dtems),amin1(dtinp,dtems)) .GT. 0. .OR.
     &    amod(amax1(dtinp,dtout),amin1(dtinp,dtout)) .GT. 0. .OR.
     &    amod(amax1(dtems,dtout),amin1(dtems,dtout)) .GT. 0. ) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(a)')'Input/Output intervals must be even multiples'
        write(iout,'(a)')'of each other.'
        write(iout,'(a,f6.0)')'Input interval      (DTINP): ',dtinp
        write(iout,'(a,f6.0)')'Emissions interval  (DTEMS): ',dtems
        write(iout,'(a,f6.0)')'Output interval     (DTOUT): ',dtout
        call camxerr()
      endif
c
c-----Projection parameters
c
      llatlon = .FALSE.
      lutm    = .FALSE.
      lpolar  = .FALSE.
      lambrt  = .FALSE.
      call jstlft( Map_Projection )
      call toupper( Map_Projection )
      if( Map_Projection .EQ. 'LATLON    ' ) then
         llatlon = .TRUE.
      elseif( Map_Projection .EQ. 'UTM       ' ) then
         lutm = .TRUE.
      elseif( Map_Projection .EQ. 'POLAR     ' ) then
        lpolar = .TRUE.
      elseif( Map_Projection .EQ. 'LAMBERT   ' ) then
        lambrt = .TRUE.
      else
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(3A)') 'Incorrect coordinate ID specified in ',
     &                     'control file: ',Map_Projection
        write(iout,'(1X,A)') 'Acceptable options are:'
        write(iout,'(10X,A)') 'LATLON'
        write(iout,'(10X,A)') 'UTM'
        write(iout,'(10X,A)') 'POLAR'
        write(iout,'(10X,A)') 'LAMBERT'
        call camxerr()
      endif
c
      if( lutm ) then
         iuzon = UTM_Zone
         if( iuzon .eq. 0 ) then
           write(iout,'(//,A)')'ERROR in READNML:'
           write(iout,'(A)')   '  The UTM zone can not be set to zero'
           write(iout,'(2A)')  '  Use +60 for the northern or -60 for',
     &                         ' the southern hemisphere'
           call camxerr()
         endif
      elseif( lpolar ) then
         polelon = POLAR_Longitude_Pole
         polelat = POLAR_Latitude_Pole
      elseif( lambrt) then
         xlonc = LAMBERT_Center_Longitude
         ylatc = LAMBERT_Center_Latitude
         tlat1 = LAMBERT_True_Latitude1
         tlat2 = LAMBERT_True_Latitude2
      endif
c
c-----Number of Grids
c
      ngrid = Number_of_Grids
c
c----Call routine to allocate arrays for file units ---
c
      call alloc_filunit(ngrid)
c
      nnest = ngrid - 1
      if( ngrid .LT. 1 ) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(a)') 'Number of grids must be 1 or more'
        call camxerr()
      endif
c
c----Call routine to allocate arrays for grid definitions ---
c
      call alloc_grid()
c
c-----Master grid parameters
c
      xorg     = Master_Origin_XCoord
      yorg     = Master_Origin_YCoord
      delx     = Master_Cell_XSize
      dely     = Master_Cell_YSize
      ncol(1)  = Master_Grid_Columns
      nrow(1)  = Master_Grid_Rows
      nlay(1)  = Number_of_Layers
      inst1(1) = 2
      inst2(1) = ncol(1)-1
      jnst1(1) = 2
      jnst2(1) = nrow(1)-1
c
c-----Nested grid parameters
c
      if (ngrid.gt.1) then
        do n = 2,ngrid
          meshold(n) = Nest_Meshing_Factor(n)
          inst1(n)   = Nest_Beg_I_Index(n)
          inst2(n)   = Nest_End_I_Index(n)
          jnst1(n)   = Nest_Beg_J_Index(n)
          jnst2(n)   = Nest_End_J_Index(n)
          nlay(n)    = Number_of_Layers
        enddo
c
        do n = 2,ngrid
          if( inst1(n) .LE. 1 .OR. inst1(n) .GE. ncol(1) .OR.
     &        inst2(n) .LE. 1 .OR. inst2(n) .GE. ncol(1) .OR.
     &        jnst1(n) .LE. 1 .OR. jnst1(n) .GE. nrow(1) .OR.
     &        jnst2(n) .LE. 1 .OR. jnst2(n) .GE. nrow(1) ) then
              write(iout,'(//,a)') 'ERROR in READNML:'
              write(iout,'(a,i5)') 'For grid # ',n
              write(iout,'(2a)') 'Starting/ending indices exceed ',
     &                           'extent of coarse grid.'
              write(iout,'(a,2i5)') 'Nest column range   :',
     &                              inst1(n),inst2(n)
              write(iout,'(a,2i5)') 'Coarse column range :',
     &                              1,ncol(1)
              write(iout,'(a,2i5)') 'Nest row range      :',
     &                              jnst1(n),jnst2(n)
              write(iout,'(a,2i5)') 'Coarse row range    :',
     &                              1,nrow(1)
              call camxerr()
          endif
c
c-----Calculate dimensions for the grid and check for array overflow
c
          ncol(n) = (inst2(n) - inst1(n) + 1)*meshold(n) + 2
          nrow(n) = (jnst2(n) - jnst1(n) + 1)*meshold(n) + 2
        enddo
      endif
c
c-----Model options/flags
c
      call jstlft( Advection_Solver )
      call toupper( Advection_Solver )
      if( Advection_Solver .EQ. 'BOTT      ' ) then
        iadvct = 2
      elseif( Advection_Solver .EQ. 'PPM       ' ) then
        iadvct = 3
      else
        write(iout,'(//,a)') 'ERROR in READNML:' 
        write(iout,'(3A)') 'Incorrect horizontal advection solver ',
     &                'specified in control file: ',Advection_Solver
        write(iout,'(1X,A)')'Acceptable options are:'
        write(iout,'(10X,A)') 'BOTT'
        write(iout,'(10X,A)') 'PPM'
        call camxerr()
      endif
c
      call jstlft( Chemistry_Solver )
      call toupper( Chemistry_Solver )
      if( Chemistry_Solver .EQ. CDCMC ) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(/,1X,2A)')
     &                      'The CMC solver is no longer supported. ',
     &                      'Use the EBI solver instead.'
          call camxerr()
      else if( Chemistry_Solver .EQ. CDEBI ) then
          idsolv = IDEBI
      else if( Chemistry_Solver .EQ. CDIEH ) then
          idsolv = IDIEH
      else if( Chemistry_Solver .EQ. CDLSOD ) then
          idsolv = IDLSOD
      else
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(/,1X,6A)') 'Invalid chemistry solver specified ',
     &                              'in control file: ',Chemistry_Solver
          write(iout,'(1X,A)') 'Acceptable options are: '
          write(iout,'(10X,A)') CDEBI
          write(iout,'(10X,A)') CDIEH
          write(iout,'(10X,A)') CDLSOD
          call camxerr()
      endif
      if( Dry_Deposition ) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(2A)') 'NOTE: This version uses a different ',
     &               'designation for the dry deposition option.'
          write(iout,'(6X,2A)') 'It is now a character ',
     &                         'variable called: Drydep_Model'
          write(iout,'(10X,A)') 'Acceptable options are: '
          write(iout,'(15X,A)') 'NONE'
          write(iout,'(15X,A)') 'WESELY89 - original scheme'
          write(iout,'(15X,A)') 'ZHANG03  - new scheme'
          write(iout,'(//)')
          call camxerr()
      endif
      call jstlft( Drydep_Model )
      call toupper( Drydep_Model )
      nlu = NLUZ03
      if( Drydep_Model .EQ. 'ZHANG03   ') then
          idrydep = 2
          ldry = .true.
      else if( Drydep_Model .EQ. 'WESELY89  ') then
          idrydep = 1
          ldry = .true.
          nlu = NLUW89
      else if( Drydep_Model .EQ. 'NONE      ' ) then
          idrydep = 0
          ldry = .false.
      else
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(2A)') 'NOTE: This version uses a different ',
     &               'designation for the dry deposition option.'
          write(iout,'(6X,2A)') 'It is now a character ',
     &                         'variable called: Drydep_Model'
          write(iout,'(/,1X,6A)') 'Invalid dry deposition option ',
     &                            'specified in control file: ',
     &                            Drydep_Model
          write(iout,'(1X,A)') 'Acceptable options are: '
          write(iout,'(10X,A)') 'NONE'
          write(iout,'(10X,A)') 'ZHANG03'
          write(iout,'(10X,A)') 'WESELY89'
          call camxerr()
      endif
c
      call jstlft( PiG_Submodel )
      call toupper( PiG_Submodel )
      if (PiG_Submodel .EQ. 'GREASD    ') then
        ipigflg = GRESPIG
      elseif (PiG_Submodel .EQ. 'IRON      ') then
        ipigflg = IRONPIG
      elseif (PiG_Submodel .EQ. 'NONE      ') then
        ipigflg = 0
      else
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(3A)') 'Incorrect PiG option specified in ',
     &                             'control file: ',PiG_Submodel
        write(iout,'(1X,A)')'Acceptable options are: '
        write(iout,'(10X,A)') 'NONE'
        write(iout,'(10X,A)') 'GREASD'
        write(iout,'(10X,A)') 'IRON'
        call camxerr()
      endif
c
c======================== Probing Tool Begin ===========================
c
      call jstlft( Probing_Tool )
      call toupper( Probing_Tool )
      if( Probing_Tool .NE. 'NONE      ' .AND.
     &    Probing_Tool .NE. OSAT   .AND.
     &    Probing_Tool .NE. GOAT   .AND.
     &    Probing_Tool .NE. APCA   .AND.
     &    Probing_Tool .NE. PSAT   .AND.
     &    Probing_Tool .NE. RTRAC  .AND.
     &    Probing_Tool .NE. RTCMC  .AND.
     &    Probing_Tool .NE. DDM    .AND.
     &    Probing_Tool .NE. HDDM   .AND.
     &    Probing_Tool .NE. STRPA  .AND.
     &    Probing_Tool .NE. STRIPR .AND.
     &    Probing_Tool .NE. STRIRR) then
          write(iout,'(//,A)') 'ERROR in READNML:'
          write(iout,'(/,1X,6A)') 'Invalid technology type ',
     &             'specified in control file: ',Probing_Tool
          write(iout,'(1X,A)') 'Acceptable options are: '
          write(iout,'(10X,A)')'NONE'
          write(iout,'(10X,A)') OSAT
          write(iout,'(10X,A)') GOAT
          write(iout,'(10X,A)') APCA
          write(iout,'(10X,A)') PSAT
          write(iout,'(10X,A)') RTRAC
          write(iout,'(10X,A)') RTCMC
          write(iout,'(10X,A)') DDM
          write(iout,'(10X,A)') HDDM
          write(iout,'(10X,A)') STRPA
          write(iout,'(10X,A)') STRIPR
          write(iout,'(10X,A)') STRIRR
          call camxerr()
       endif
c
c  --- check that switches are compatible ---
c
       if( ipigflg .EQ. GRESPIG .AND. 
     &   Probing_Tool .NE. 'NONE' .AND. Probing_Tool .NE. OSAT  .AND.
     &   Probing_Tool .NE. GOAT   .AND. Probing_Tool .NE. APCA  .AND.
     &   Probing_Tool .NE. PSAT   .AND. Probing_Tool .NE. STRPA .AND.
     &   Probing_Tool .NE. STRIPR .AND. Probing_Tool .NE. STRIRR ) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(2a)') 'GREASD PiG only works with the following',
     &                       ' Probing Tools:'
          write(iout,'(a)') 'OSAT,GOAT,APCA,PSAT,PA,IPR,IRR'
          write(iout,'(a)') 'Turn off PiG switch and try again.'
          call camxerr()
       endif
       if( ipigflg .EQ. IRONPIG .AND.
     &     Probing_Tool .NE. 'NONE' .AND. Probing_Tool .NE. RTRAC .AND.
     &     Probing_Tool .NE. RTCMC) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(2a)') 'IRON PiG only works with the following',
     &                       ' Probing Tools:'
          write(iout,'(a)') 'RTRAC'
          write(iout,'(a)') 'RTCMC'
          write(iout,'(a)') 'Turn off PiG switch and try again.'
          call camxerr()
       endif
       if( ipigflg .NE. 0 .AND. Probing_Tool .NE. 'NONE' 
     &                                          .AND. LVISPIG ) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(a)') 'You are running PiG with Probing Tools.'
          write(iout,'(2a)') 'PiG visualization in the average file ',
     &                          'is not supported with Probing Tools.' 
          write(iout,'(2a,/,a)') 'Please set the LVISPIG parameter in ',
     &                        'the camx.prm file to FALSE to run PiG ',
     &                                             'with Probing Tools.'
          write(iout,'(2a)') 'See the CAMx Users Guide for a ',
     &                  'description of the PiG visualization feature.'
          call camxerr()
        endif
cbk       if( iadvct .ne. 2 .AND. Probing_Tool .EQ. DDM ) then
cbk          write(iout,'(//,a)') 'ERROR in READNML:'
cbk          write(iout,'(2a)') 'Advection solver used with DDM ',
cbk     &                                          'must be BOTT.'
cbk          write(iout,*)'Change the control file and try again.'
cbk          call camxerr()
cbk       endif
       if( idsolv .NE. IDEBI .AND. Probing_Tool .EQ. DDM ) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(2a)') 'Chemistry solver used with DDM ',
     &                                           'must be EBI.'
          write(iout,*)'Change the control file and try again.'
          call camxerr()
       endif
       if( ACM2_Diffusion .AND. ( Probing_Tool .EQ. DDM .OR.
     &                            Probing_Tool .EQ. HDDM )) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(a)') 'DDM does not work with ACM2 diffusion.'
          write(iout,*)'Change the control file and try again.'
          call camxerr()
       endif
       if( ACM2_Diffusion .AND. ( Probing_Tool .EQ. STRIPR .OR.
     &                            Probing_Tool .EQ. STRPA )) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(a)') 'PA/IPR does not work with ACM2 diffusion.'
          write(iout,*)'Change the control file and try again.'
          call camxerr()
       endif
c
c======================== Probing Tool End =============================
c 
      lflexi = Flexi_Nest
      ldiag  = Diagnostic_Error_Check
      lrstrt = Restart
      lchem  = Chemistry
      lwet   = Wet_Deposition
      lacm2  = ACM2_Diffusion
      le1day = Ignore_Emission_Dates
      larsrc = Gridded_Emissions
      lptsrc = Point_Emissions
      lsuper = Super_Stepping
      if( ipigflg .NE. 0 .AND. .NOT.lptsrc ) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(a)')
     &         'Point source emissions are required for PiG module.'
        write(iout,'(2a)')'Either turn off PiG module or turn on ',
     &                                    'point sources treatment.'
        call camxerr()
      endif
c
c-----Output options and species
c
      do i=1,ngrid
         l3davg(i) = Average_Output_3D
         if( Output_3D_Grid(i) ) l3davg(i) = .TRUE.
      enddo
      lhdfout = HDF_Format_Output
      hdfroot = HDF_File_Root
      call jstlft( hdfroot )
      if (lhdfout .and. hdfroot .EQ. ' ') goto 7008
      if (lhdfout .and. llatlon) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(//,a)') 'HDF file output format is not supported'
        write(iout,'(//,a)') 'for Lat/Lon coordinates.'
        call camxerr()
      endif
      do n = MXNAM,1,-1
        if (Output_Species_Names(n).ne.' ') then
          navspc = n
          call alloc_chmstry_avg(navspc)
          do l = 1,navspc
            if (Output_Species_Names(n).eq.' ') then
              write(iout,'(//,a)') 'ERROR in READNML:'
              write(iout,'(a)')'An output species name is blank'
              write(iout,'(a,i5)')'at species number: ',l
              call camxerr()
            endif 
            spavg(l) = Output_Species_Names(l)
          enddo
          goto 50
        endif
      enddo
      write(iout,'(//,a)') 'ERROR in READNML:'
      write(iout,'(a)')'Number of average species is zero.'
      write(iout,'(a)')'At least 1 species needs to be output.'
      call camxerr()
 50   continue
c
c-----PiG sampling grids
c
      nsample = 0
      lsample = PiG_Sampling_Grid
      if( .NOT. lsample ) goto 100
      if( ipigflg .EQ. 0 ) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(/,1X,A,A)')'PiG flag must be turned on',
     &                           ' to use sampling grids.'
        call camxerr()
      endif
      lbckgrd = Sample_Background
      nsample = Number_of_Sampling_Grids
      if (nsample .EQ. 0) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(/,1X,A,A)')'Sampling grids are turned on ',
     &                'but number of sampling grids is set to zero'
        call camxerr()
      endif
c
c----Call routine to allocate arrays for sampling file units ---
c
      call alloc_filunit_sample(nsample)
c
c   --- allocate the arrays that depend on number of samples ---
c
      call alloc_pigsty_sample()
c
      do n = 1,nsample
        ismp1(n)   = SG_Beg_I_Index(n)
        ismp2(n)   = SG_End_I_Index(n)
        jsmp1(n)   = SG_Beg_J_Index(n)
        jsmp2(n)   = SG_End_J_Index(n)
        meshsmp(n) = SG_Mesh_Factor(n)
      enddo
      do n = 1,nsample
        if (ismp1(n) .LE. 1 .OR. ismp1(n) .GE. ncol(1) .OR.
     &      ismp2(n) .LE. 1 .OR. ismp2(n) .GE. ncol(1) .OR.
     &      jsmp1(n) .LE. 1 .OR. jsmp1(n) .GE. nrow(1) .OR.
     &      jsmp2(n) .LE. 1 .OR. jsmp2(n) .GE. nrow(1)) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(a,i3)') 'For sampling grid # ',n
          write(iout,'(2a)') 'Starting/ending indices exceed extent',
     &                       ' of coarse grid.'
          write(iout,'(a,2i5)')
     &                   'Sample column range   :',ismp1(n),ismp2(n)
          write(iout,'(a,2i5)')
     &                   'Sample row range      :',jsmp1(n),jsmp2(n)
          write(iout,'(a,2i5)') 'Coarse column range   :',1,ncol(1)
          write(iout,'(a,2i5)') 'Coarse row range      :',1,nrow(1)
          call camxerr()
        endif
        ncolsmp(n) = (ismp2(n) - ismp1(n) + 1)*meshsmp(n)
        nrowsmp(n) = (jsmp2(n) - jsmp1(n) + 1)*meshsmp(n)
        xorgsmp(n) = xorg + (ismp1(n) - 1)*delx
        yorgsmp(n) = yorg + (jsmp1(n) - 1)*dely
      enddo
      do n = 1,nsample
        ismpgrd(n) = 1
        do 101 ng = 2,ngrid
c
c  --- check for across the seam of this grid ---
c
          lacross = .FALSE.
          if( ismp1(n) .LT. inst1(ng) .AND. 
     &                    ismp2(n) .GT. inst1(ng) ) lacross = .TRUE. 
          if( ismp1(n) .LT. inst2(ng) .AND. 
     &                    ismp2(n) .GT. inst2(ng) ) lacross = .TRUE. 
          if( jsmp1(n) .LT. jnst1(ng) .AND. 
     &                    jsmp2(n) .GT. jnst1(ng) ) lacross = .TRUE. 
          if( jsmp1(n) .LT. jnst2(ng) .AND. 
     &                    jsmp2(n) .GT. jnst2(ng) ) lacross = .TRUE. 
          if( lacross ) then
             write(iout,'(//,a)') 'ERROR in READNML:'
             write(iout,'(a,i3)') 'For sampling grid # ',n
             write(iout,'(2a,I3)') 'Sampling grid crosses the seam of ',
     &                       'grid # ',ng
             write(iout,'(a,2i5)')
     &                   'Sample column range   :',ismp1(n),ismp2(n)
             write(iout,'(a,2i5)')
     &                   'Sample row range      :',jsmp1(n),jsmp2(n)
             write(iout,'(a,2i5)') 'Grid column range   :',
     &                                             inst1(ng),inst2(ng)
             write(iout,'(a,2i5)') 'Grid row range      :',
     &                                             jnst1(ng),jnst2(ng)
             call camxerr()
          endif
          if (inst1(ng).le.ismp1(n) .and. inst2(ng).ge.ismp2(n) .and.
     &        jnst1(ng).le.jsmp1(n) .and. jnst2(ng).ge.jsmp2(n)) then
            ismpgrd(n) = ng
c
c  --- make sure the sampling grid is finer than current grid ---
c
            if( meshold(ng) .GT. meshsmp(n) ) then
             write(iout,'(//,a,/)') 'ERROR in READNML:'
             write(iout,'(a,i3)') 'For sampling grid # ',n
             write(iout,'(2a)') 'The modeling grid is finer than the ',
     &                                                   'sampling grid.'
             write(iout,'(2a,/)') 'Aggregation for sampling grids is ',
     &                                                  'not supported.'
             write(iout,'(a,i3,a,i3)') 'Sampling grid: ',n,
     &                               ' is contained in modeling grid: ',ng
             write(iout,'(a,i3,a,i3)') 'Sampling grid: ',n,
     &                               ' has meshing factor: ',meshsmp(n)
             write(iout,'(a,i3,a,i3)') 'Modeling grid: ',ng,
     &                               ' has meshing factor: ',meshold(ng)
             write(iout,'(2a,/,a)') 'Set the meshing factor for your ',
     &                              'sampling grid to be at least as ',
     &                             'large as the value for the modeling grid.'
             call camxerr()
            endif
          endif
 101    continue
      enddo
c
c-----Make sure the TOPCON file has been elininated
c
 100  continue
      action = 'Top Concentrations file.'
      keyword = '"Top_Concentrations"'
      if( Top_Concentrations .NE. ' ' ) goto 7200
c
c-----Echo run control parameters
c
      write(idiag,'(A,F10.0,I10.6,I10.5)')
     &         'Simulation start time/date : ',begtim,cbdate,begdate
      write(idiag,'(A,F10.0,I10.6,I10.5)')
     &         'Simulation end time/date   : ',endtim,cedate,enddate
      write(idiag,'(A,I10)')
     &         'Time zone                  : ',itzon
      write(idiag,'(A,F10.0)')
     &         'Max timestep (min)         : ',dtmax
      write(idiag,'(A,F10.0)')
     &         'Met Input interval (min)   : ',dtinp
      write(idiag,'(A,F10.0)')
     &         'Emiss Input interval (min) : ',dtems
      write(idiag,'(A,F10.0)')
     &         'Output interval (min)      : ',dtout
      write(idiag,'(A,A)')
     &         'Grid Projection Type       : ',Map_Projection
      if (lutm) then
        write(idiag,'(A,I10)')
     &         'UTM: zone number           : ',iuzon
      elseif (lpolar) then
        write(idiag,'(A,2F10.3)')
     &         'POLAR: pole lon/lat        : ',polelon,polelat
      elseif (lambrt) then
        write(idiag,'(A,4F10.3)')
     &         'LAMBERT: center lon/lat    : ',xlonc,ylatc
        write(idiag,'(A,4F10.3)')
     &         'LAMBERT: true latitudes    : ',tlat1,tlat2
      endif
      write(idiag,'(A,4F10.3)')
     &         'Master Grid X/Y Origin     : ',xorg,yorg
      write(idiag,'(A,4F10.3)')
     &         'Master Grid cell size      : ',delx,dely
      write(idiag,'(A,3I10)')
     &         'Master Grid NCOL NROW NLAY : ',ncol(1),nrow(1),nlay(1)
      write(idiag,*)
      write(idiag,'(A)')'CAMx control flags'
      write(idiag,'(A,L10)') 
     &         'Stop after diagnostic check: ',ldiag
      write(idiag,'(A,A)')
     &         'Advection Solver           : ',Advection_Solver
      write(idiag,'(A,A)')
     &         'Chemistry Solver           : ',Chemistry_Solver
      write(idiag,'(A,A)')
     &         'Dry Deposition             : ',Drydep_Model
      write(idiag,'(A,A)')
     &         'PiG submodel               : ',PiG_Submodel
      write(idiag,'(A,L10)')
     &         'PiG Sampling Grid          : ',PiG_Sampling_Grid
      write(idiag,'(A,A)')
     &         'Probing Tools              : ',Probing_Tool
      write(idiag,'(A,L10)') 
     &         'Flexi-Nest                 : ',lflexi 
      write(idiag,'(A,L10)') 
     &         'Restart                    : ',lrstrt 
      write(idiag,'(A,L10)') 
     &         'Chemistry                  : ',lchem
      write(idiag,'(A,L10)') 
     &         'Dry deposition             : ',ldry 
      write(idiag,'(A,L10)') 
     &         'Wet deposition             : ',lwet 
      write(idiag,'(A,L10)')
     &         'ACM2 Vertical Diffusion    : ',lacm2
      write(idiag,'(A,L10)') 
     &         'Super stepping advection   : ',lsuper
      write(idiag,'(A,L10)') 
     &         'Area sources               : ',larsrc
      write(idiag,'(A,L10)') 
     &         'Point sources              : ',lptsrc
      write(idiag,'(A,L10)') 
     &         'Date-insensitive emissions : ',le1day
      do i=1,ngrid
         write(idiag,'(A,I3,L10)') 
     &         '3-D average file - Grid #  : ',i,l3davg(i)
      enddo
      write(idiag,'(A,L10)') 
     &         'HDF output format          : ',lhdfout
      if( navspc .GT. 0 ) then
        write(idiag,*)
        write(idiag,'(A,I3)')
     &         'Number of output species   : ',navspc
        do l = 1,navspc
          write(idiag,'(24x,i3,'': '',A)') l,spavg(l)
        enddo
      else
        write(idiag,*)
        write(idiag,'(29X,A)')
     &            'Average concentrations will not be output.'
      endif
      write(idiag,*)
c
      if( nnest .GT. 0 ) then
        write(idiag,'(A,I3)')'Number of nested fine grids: ',nnest
        write(idiag,*)' Nest        x-range       ncol        y-range',
     &                '       nrow   mesh factor  nlay'      
        do n = 2,ngrid
          write(idiag,'(I5,2(5X,I5,1X,I5,5X,I5),5X,I5,5X,I5)')
     &          n,inst1(n),inst2(n),ncol(n),jnst1(n),jnst2(n),
     &          nrow(n),meshold(n),nlay(n)
        enddo
        write(idiag,*)
        write(idiag,*) '|',('-',i=1,74),'|'
        write(idiag,*) '|',(' ',i=1,74),'|'
        write(idiag,'(2A)') ' | NOTE:  The nest order listed above ',
     &       'is the original order specified in    |'
        write(idiag,'(2A)') ' |        the CAMx.in file.           ',
     &       '                                      |'
        write(idiag,'(2A)') ' |        CAMx may re-order the nests.',
     &      ' See the internal nest order provided |'
        write(idiag,'(2A)') ' |        in the table below.         ',
     &       '                                      |'
        write(idiag,*) '|',(' ',i=1,74),'|'
        write(idiag,*) '|',('-',i=1,74),'|'
      else
        write(idiag,*)'Fine grid nests are not specified.'
      endif
      write(idiag,*)
c
      if( lsample ) then
        write(idiag,*)
        write(idiag,'(A,L10)') 
     &         'Sample grid includes bckgrd: ',lbckgrd
        write(idiag,'(A,I3)')'Number PiG Sampling grids  : ',nsample
        write(idiag,*) ' ID     x-range     ncol     y-range',
     &                '     nrow   mesh factor  delx    dely'
        do n = 1,nsample
          write(idiag,'(1X,I2,2(3X,I4,3X,I4,3X,I4),6X,I4,4X,
     &                  2(2X,F6.3))')
     &          n,ismp1(n),ismp2(n),ncolsmp(n),jsmp1(n),jsmp2(n),
     &          nrowsmp(n),meshsmp(n),delx/float(meshsmp(n)),
     &          dely/float(meshsmp(n))
        enddo
        write(idiag,*)
      endif
c
c-----Open the remaining I/O files
c
      call openfils(iifroot,nopen)
c
c-----Read chemistry parameters
c
      time = begtim
      date = begdate
      call readchm()
c
c   --- allocate the arrays for PiG --
c
      if( ipigflg .NE. 0 .AND. .NOT. lrstrt )
     &    call alloc_pigsty(nspec,nreactr,ngrid,
     &                                  MAX(1,numprocs-1),ipigflg)
      if( ipigflg .NE. 0 ) 
     &         call alloc_pigsty_vpconc(ncol,nrow,nlay,nspec,ngrid)
c
c  --- setup some pointers that depend on number of species ---
c
      if( lsample ) then
        ipsmp(1) = 1
        do i = 2,nsample
          ipsmp(i) = ipsmp(i-1) + ncolsmp(i-1)*nrowsmp(i-1)*navspc
        enddo
c
c   --- allocate the arrays for sampling that depend on species ---
c
         call alloc_pigsty_smpgrd(navspc,nsample,
     &                                    nrowsmp,ncolsmp,nsmpcels)
      endif
c
c======================== Probing Tool Begin ===========================
c
c  --- set Probing Tool switches according to technology type ---
c
      ltrace = .FALSE.
      lddm   = .FALSE.
      lhddm   = .FALSE.
      lproca = .FALSE.
      lipr   = .FALSE.
      lirr   = .FALSE.
      lozone = .FALSE.
      lpsat_apca = .FALSE.
      lptdepout = .FALSE.
      tectyp = Probing_Tool
      if( tectyp .EQ. 'NONE      '  ) then
c
c---- call routines to allocate the arrays that need a pointer ---
c
         call alloc_tracer_null(nspec,lirr,ngrid,ncol,nrow)
         call alloc_procan_null()
         goto 8888
      elseif( tectyp .EQ. OSAT  ) then
         ltrace = .true.
         verson = VEROSAT
         lozone = .TRUE.
         call alloc_procan_null()
      elseif( tectyp .EQ. GOAT  ) then
         ltrace = .true.
         verson = VERGOAT
         lozone = .TRUE.
         call alloc_procan_null()
      elseif( tectyp .EQ. APCA  ) then
         ltrace = .true.
         verson = VERAPCA
         lozone = .TRUE.
         call alloc_procan_null()
      elseif( tectyp .EQ. PSAT  ) then
         ltrace = .true.
         verson = VERPSAT
         call alloc_procan_null()
      elseif( tectyp .EQ. RTRAC ) then
         ltrace = .true.
         verson = VERRTRAC
         call alloc_procan_null()
      elseif( tectyp .EQ. RTCMC ) then
         ltrace = .true.
         verson = VERRTCMC
         call alloc_procan_null()
      elseif( tectyp .EQ. DDM  ) then
         lddm = .TRUE.
         verson = VERDDM
         call alloc_procan_null()
      elseif( tectyp .EQ. HDDM  ) then
         lhddm = .TRUE.
         verson = VERHDDM
         call alloc_procan_null()
      elseif( tectyp .EQ. STRPA ) then
         lproca = .TRUE.
         lipr = .TRUE.
         lirr = .TRUE.
         lsfcfl = .TRUE.
         verson = VERPA
         call alloc_procan_init(ngrid,MXCPA)
         call alloc_ptwet_null()
      elseif( tectyp .EQ. STRIPR ) then
         lproca = .TRUE.
         lipr = .TRUE.
         verson = VERPA
         call alloc_tracer_null(nspec,lirr,ngrid,ncol,nrow)
         call alloc_procan_init(ngrid,MXCPA)
         call alloc_ptwet_null()
      elseif( tectyp .EQ. STRIRR ) then
         lproca = .TRUE.
         lirr = .TRUE.
         lsfcfl = .TRUE.
         verson = VERPA
         call alloc_procan_init(ngrid,MXCPA)
         call alloc_ptwet_null()
      endif
c
c  --- CMU chemistry and probing tools is not allowed ---
c
      if( (aeropt.EQ.'INERT' .OR. aeropt.EQ.'CMU') .AND. ltrace ) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(/,2A)') ' The source apportionment option is ',
     &             'not supported with the INERT or CMU Aerosol Scheme.'
          write(iout,'(2A)')' Either turn off source apportionment or ',
     &                                   'choose the CF Aerosol Scheme.'
          call camxerr()
      endif
c
c  --- read Probing Tool namelists ---
c
      if( ltrace ) then
         if( tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC ) then
            namegrp = 'RT_Control'
            action =  'You must have the '//namegrp(:istrln(namegrp))//
     &          ' namelist group when probing tools is set to '//
     &                                    tectyp(:istrln(tectyp))//'.'
            read(inp,RT_Control,END=7100,ERR=7101)
            action = 'Top Concentrations file for RTRAC.'
            keyword = '"RT_Top_Concentrations"'
            if( RT_Top_Concentrations .NE. ' ' ) goto 7200
            flrtsa = RT_File_Root
         else
            namegrp = 'SA_Control'
            action =  'You must have the '//namegrp(:istrln(namegrp))//
     &          ' namelist group when probing tools is set to '//
     &                                    tectyp(:istrln(tectyp))//'.'
            read(inp,SA_Control,END=7100,ERR=7101)
            flrtsa = SA_File_Root
            lptdepout = SA_Deposition_Output
            if( .NOT. ldry .AND. .NOT. lwet ) lptdepout = .FALSE.
         endif 
      elseif( lddm .OR. lhddm ) then
         namegrp = 'DDM_Control'
         action =  'You must have the '//namegrp(:istrln(namegrp))//
     &          ' namelist group when probing tools is set to '//
     &                                    tectyp(:istrln(tectyp))//'.'
         read(inp,DDM_Control,END=7100,ERR=7101)
         action = 'Top Concentrations file for DDM.'
         keyword = '"DDM_Top_Concentrations"'
         if( DDM_Top_Concentrations .NE. ' ' ) goto 7200
         flrtsa = DDM_File_Root
      elseif( lproca ) then
         namegrp = 'PA_Control'
         action =  'You must have the '//namegrp(:istrln(namegrp))//
     &          ' namelist group when probing tools is set to '//
     &                                    tectyp(:istrln(tectyp))//'.'
         read(inp,PA_Control,END=7100,ERR=7101)
         flrtsa = PA_File_Root
      endif
c
c======================== Probing Tool End =============================
c 
c
c======================== Source Apportion Begin =======================
c
      if( ltrace ) then
c
c  --- call routines to get the rest of the options ---
c
c
         if( tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) then
            lsrfmod = RT_Surface_Model
            lparttn = RT_Partitioning
            if (lsrfmod .AND. idrydep .NE. 1) then
              write(iout,*)'The RTRAC Surface Model must be used',
     &                     ' with the WESELY89 deposition'
              write(iout,*)'Stopping'
              call camxerr()
            endif
            if (lsrfmod .AND. ipigflg.ne.0) then
              write(iout,*)'The RTRAC Surface Model cannot be used',
     &                     ' with the PiG model'
              write(iout,*)'Stopping'
              call camxerr()
            endif
            if (lparttn .AND. .not.lsrfmod) then
              write(iout,*)'The RTRAC Surface Model must be used',
     &                     ' if gas-aerosol partitioning is requested.'
              write(iout,*)'Set RT_Surface_Model to true'
              write(iout,*)'Stopping'
              call camxerr()
            endif
            if (lparttn .AND. aeropt.ne.'CF') then
              write(iout,*)'RTRAC gas-aerosol partitioning must',
     &                     ' use the CF aerosol scheme'
              write(iout,*)'Stopping'
              call camxerr()
            endif
            write(idiag,*)
            write(idiag,'(A,L10)')
     &                  'RTRAC Surface Model        : ',lsrfmod
            write(idiag,'(A,L10)')
     &                  'RTRAC Gas-PM Partitioning  : ',lparttn
c
            if( lsample ) then
               lsmptrc = RT_PiG_Sample
               write(idiag,*)
               write(idiag,'(A,L10)')
     &                  'Sample grid includes RTRAC : ',lsmptrc
               if( lsmptrc ) then
c
c  --- call routine to allocate the variables for RTRAC sampling ---
c
                  call alloc_tracer_sample_io(nsample,0)
                  iprtsmp(1) = 1
                  do i=2,nsample
                    iprtsmp(i) = iprtsmp(i-1) +
     &                           ncolsmp(i-1)*nrowsmp(i-1)*ntotsp
                  enddo
               endif
            endif
            write(idiag,*)
            call startrt(iout,nopen)
         else
            call rdoptsa()
            nemiss = ngroup
            if( leftovr ) nemiss = ngroup + 1
            write(idiag,'(A,L10)') 
     &               'Output Deposition Fields   : ',lptdepout
            write(idiag,'(A,L10)') 
     &               'Stratify Boundary          : ',lbndry
            write(idiag,'(A,I10)') 
     &               'Number of source areas     : ',nregin
            write(idiag,'(A,I10)') 
     &               'Number of source groups    : ',nemiss
            write(idiag,'(A,L10)') 
     &               'Use Leftover group         : ',leftovr
            write(idiag,'(A,I10)') 
     &               'Number of time releases    : ',ntrtim
            write(idiag,*)
            call startsa(iout,nopen)
         endif
      endif
c
c======================== Source Apportion End =========================
c
c
c======================== DDM Begin ====================================
c
      if( lddm .OR. lhddm ) then
         call alloc_ddm_species(nspec)
         call rdoptddm()
         write(idiag,'(A,I10)') 'Number of IC groups        : ',nicddm
         if( nicddm .GT. 0 ) then
            do i = 1,nicddm
              write(idiag,'(24X,i3,'': '',A)') i,icddmsp(i)
            enddo
         else
            write(idiag,'(29X,A)') 'No IC species modeled.'
         endif
         write(idiag,'(A,I10)') 'Number of BC groups        : ',nbcddm
         if( nbcddm .GT. 0 ) then
            do i = 1,nbcddm
              write(idiag,'(24X,i3,'': '',A)') i,bcddmsp(i)
            enddo
         else
            write(idiag,'(29X,A)') 'No BC species modeled.'
         endif
         write(idiag,'(A,I10)') 'Number of EM groups        : ',nemddm
         if( nemddm .GT. 0 ) then
            do i = 1,nemddm
              write(idiag,'(24X,i3,'': '',A)') i,emddmsp(i)
            enddo
         else
            write(idiag,'(29X,A)') 'No emission species modeled.'
         endif
         write(idiag,'(A,I10)') 'Number of rate const groups: ',nrateddm
         if( nrateddm .GT. 0 ) then
            if( lddm ) then
              write(iout,'(//,a)') 'ERROR in READNML:'
              write(iout,'(2a)') 'Rate constant sensitivity cannot ',
     &                                           'be used with DDM.'
              call camxerr()
            endif
            do i = 1,nrateddm
              write(idiag,'(24X,i3,'': '',A)') i,rateddm(i)
              write(idiag,'(29X,10I4)') (iprate(n,i),n=1,iprate(0,i))
            enddo
         else
            write(idiag,'(29X,A)') 'No rate constant sens modeled.'
         endif
         write(idiag,'(A,I10)') 'Number of HDDM sens groups : ',nhddm
         if( nhddm .GT. 0 ) then
            do i = 1,nhddm
              write(idiag,'(24X,i3,'': '',A,'', '',A)') i,hddmsp(1,i),
     &                                                    hddmsp(2,i)
            enddo
         else
            write(idiag,'(29X,A)') 'No HDDM sensitivity modeled.'
         endif
         write(idiag,'(2A)') 'DDM turn-off flags are shown below ',
     &                       'after the nested-grid maps.'
         write(idiag,*)
         call startddm(iout,nopen)
      endif
c
c========================= DDM End =====================================
c
c
c======================== Process Analysis Begin =======================
c
c   --- echo the irmb specific flags ---
c
      if( lproca ) then
         call rdoptpa
         write(idiag,'(A,L10)') 'Integrated Process Rates   : ',lipr
         write(idiag,'(A,L10)') 'Integrated Reaction Rates  : ',lirr
         write(idiag,*)
      endif
c
      if( lipr ) then
         jj = istrln(flrtsa)
         call getunit(ipr_unit)
         filtmp = flrtsa
         filtmp(jj+1:) = '.ipr'
         nopen = nopen + 1
         open(unit=ipr_unit,file=filtmp(1:jj+4),form='UNFORMATTED',
     &                                  status= 'UNKNOWN',ERR=7000)
         write(iout,9000)
     &   'Cell Specific Process output         (unit):',ipr_unit
         write(iout,9002) filtmp(1:jj+4)
      endif
c
      if( lirr ) then
         jj = istrln(flrtsa)
         call getunit(irr_unit)
         filtmp = flrtsa
         filtmp(jj+1:) = '.irr'
         nopen = nopen + 1
         open(unit=irr_unit,file=filtmp(1:jj+4),form='UNFORMATTED',
     &                                  status= 'UNKNOWN',ERR=7000)
         write(iout,9000)
     &   'Cell Specific Rates output           (unit):',irr_unit
         write(iout,9002) filtmp(1:jj+4)
c
         if( lsfcfl ) then
            do n = 1,ngrid
               filtmp = flrtsa
               write(filtmp(jj+1:),'(a,i2.2)') '.cpa.grd',n
               sfcfil(n) = filtmp
               nopen = nopen + 1
               call getunit(iowsfc(n))
               open(unit=iowsfc(n),file=sfcfil(n),
     &                  form='UNFORMATTED',status= 'UNKNOWN',ERR=7000)
               write(iout,9000)
     &               'Gridded CPA file                     (unit):',
     &                                                     iowsfc(n)
               write(iout,9002) filtmp(1:jj+10)
            enddo
         endif
      endif
c
c========================= Process Analysis End ========================
c
c-----Everything worked correctly, return to calling routine
c
 8888 continue
      write(iout,*)
      write(iout,*)'Finished reading control file.'
      write(iout,'(A,I2)')'Number of input files opened: ',nopen
      write(iout,*)
      write(iout,*)
      if( ntrtim .GT. 0 ) then
        write(iout,*) '   ---------------------------------------------'
        write(iout,*) '   |                                           |'
        write(iout,*) '   |    NOTE:                                  |'
        write(iout,*) '   |    You have chosen to include timing      |'
        write(iout,*) '   |    tracers. Be aware that this will       |'
        write(iout,*) '   |    add a significant amount of memory     |'
        write(iout,*) '   |    to your application. The memory        |'
        write(iout,*) '   |    requirements may exceed available      |'
        write(iout,*) '   |    resources.                             |'
        write(iout,*) '   |                                           |'
        write(iout,*) '   ---------------------------------------------'
        write(iout,*)
      endif
      call flush(iout)
      call flush(idiag)
      return
c
 9000 format(/,A,I2)
 9002 format(2A)
c
c-----Error messages
c
 7000 continue
      write(*,'(//,A)') 'ERROR in READNML:'
      write(*,'(A)') action(:istrln(action))
      write(*,'(2A,//)') ' Could not open file: ',
     &                                 filtmp(:istrln(filtmp))
      call exit(1)
      stop
c
 7005 continue
      write(*,'(//,A)') 'ERROR in READNML:'
      write(*,'(2A,//)') ' Could not open control file: ',
     &                                   ctlfil(:istrln(ctlfil))
      call exit(1)
      stop
c
 7006 continue
      write(*,'(//,A)') 'ERROR in READNML:'
      write(*,'(A)') ' Root output file name is blank.'
      write(*,'(A)') ' A valid path/filename must be provided in '
      write(*,'(A,//)') ' the namelist variable: Root_Output_Name'
      call exit(1)
      stop
c
 7007 continue
      write(*,'(//,a)') 'ERROR in READNML:'
      write(*,'(2A,//)') ' Cannot find control file: ',
     &                     ctlfil(:istrln(ctlfil))
      call exit(1)
      stop
c
 7008 continue
      write(*,'(//,A)') 'ERROR in READNML:'
      write(*,'(A)') ' HDF root output file name is blank.'
      write(*,'(A)') ' A valid path/filename must be provided in '
      write(*,'(A,//)') ' the namelist variable: HDF_File_Root'
      call exit(1)
      stop
c
 7100 continue
      write(*,'(//,a)') 'ERROR in READNML:'
      write(*,'(2A)') ' Cannot find namelist group: ',
     &                                  namegrp(:istrln(namegrp))
      write(*,'(1X,A)') action(:istrln(action))
      write(*,'(3A)') ' Please make sure the CAMx control file has ',
     &        'a section beginning with &',namegrp(:istrln(namegrp))
      write(*,'(A,//)') ' and terminated with a & character.'
      call exit(1)
      stop
c
 7101 continue
      write(*,'(//,a)') 'ERROR in READNML:'
      write(*,'(2A)') ' Error reading the namelist group: ',
     &                                  namegrp(:istrln(namegrp))
      write(*,'(2A)') ' It is probably a problem with ',
     &                         'syntax.  Please see the user''s'
      write(*,'(2A)') ' guide for examples ',
     &                'of possible problems with namelist syntax.'
      write(*,'(A,//)') ' Then review your namelist carefully.'
      call exit(1)
      stop
c
 7102 continue
      write(*,'(//,a)') 'ERROR in READNML:'
      write(*,'(2A)') ' Error reading the namelist group: ',
     &                                  namegrp(:istrln(namegrp))
      write(*,'(2A)') ' It is probably a problem with ',
     &                         'syntax.  Please see the user''s'
      write(*,'(2A)') ' guide for examples ',
     &                'of possible problems with namelist syntax.'
      write(*,'(A,/)') ' Then review your namelist carefully.'
      write(*,'(A,/,A)') ' Be aware that in this version some of the namelist variables ',
     &                         ' have changed, including: '
      write(*,'(A)') '        Number_of_Output_Species is eliminated.'
      write(*,'(A)') '        Staggered_Winds is eliminated.'
      write(*,'(A)') '        TUV_Cloud_Adjust is eliminated.'
      write(*,'(A)') '        TUV_Aero_Adjust is eliminated.'
      write(*,'(A)') '        Number_of_Layers is now a scalar applied to all grids.'
      write(*,'(A)') '        Albedo_Haze_Ozone is now Ozone_Column.'
      write(*,'(/,A)') ' Some input filename variables have changed. These are no longer used: '
      write(*,'(A)') '        Landuse_Grid'
      write(*,'(A)') '        ZP_Grid'
      write(*,'(A)') '        Wind_Grid'
      write(*,'(A)') '        Temp_Grid'
      write(*,'(A)') '        Vapor_Grid'
      write(*,'(A)') '        Kv_Grid'
      write(*,'(/,A,/,A,//)') ' Please refer to the Release Notes and the template included ',
     &                                 ' in the source code distribution.'
      call exit(1)
      stop
c
 7200 continue
      write(iout,'(//,a)') 'ERROR in READNML:'
      write(iout,'(2A)') ' Error reading the namelist group: ',
     &                                  namegrp(:istrln(namegrp))
      write(iout,'(2A)') ' You supplied a ',action(:istrln(action))
      write(iout,'(A)') ' This version of CAMx no longer uses that file.'
      write(iout,'(3A,//)') ' Please remove the variable ',
     &               keyword(:istrln(keyword)),' from the namelist.'
      call camxerr()
c
c-----Return point
c
      end

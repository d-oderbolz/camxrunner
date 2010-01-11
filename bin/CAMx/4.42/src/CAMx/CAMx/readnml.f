      subroutine readnml(version,enddate,endtim)
c
c----CAMx v4.42 070603
c
c     READNML opens and reads the CAMx input namelist file called "CAMx.in"
c     that defines user inputs.  All namelist variables are mapped to 
c     internal variables and checked for appropriate/consistent values.
c                          
c     Copyright 1996-2007
c     ENVIRON International Corporation
c          
c     Modifications:
c        7/20/05       Moved PiG sampling grid inputs to main namelist, added
c                      new options for sampling standard species
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
      implicit none
      include 'camx.prm'
      include 'camx.com'
      include 'camxfld.com'
      include 'filunit.com'
      include 'chmstry.com'
      include 'ahomap.com'
      include 'grid.com'
      include 'pigsty.com'
      include 'flags.com'
c
c======================== Probing Tool Begin ===========================
c
      include 'tracer.com'
      include 'procan.com'
c
c======================== Probing Tool End =============================
c
      include 'namelist.com'
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
     & PiG_Submodel,Probing_Tool,Chemistry,
     & Dry_Deposition,Wet_Deposition,Staggered_Winds,Gridded_Emissions,
     & Point_Emissions,Ignore_Emission_Dates,Root_Output_Name,
     & Average_Output_3D,HDF_Format_Output,HDF_File_Root,
     & Number_of_Output_Species,Output_Species_Names,PiG_Sampling_Grid,
     & Sample_Background,Number_of_Sampling_Grids,SG_Beg_I_Index,
     & SG_End_I_Index,SG_Beg_J_Index,SG_End_J_Index,SG_Mesh_Factor,
     & Chemistry_Parameters,Photolyis_Rates,
     & Initial_Conditions,Boundary_Conditions,Top_Concentrations,
     & Albedo_Haze_Ozone,Point_Sources,Master_Grid_Restart,
     & Nested_Grid_Restart,PiG_Restart,Landuse_Grid,ZP_Grid,
     & Wind_Grid,Temp_Grid,Vapor_Grid,Cloud_Grid,Kv_Grid,Emiss_Grid
c
c======================== Probing Tool Begin ===========================
c
      namelist /SA_Control/
     & SA_File_Root,SA_Master_Sfc_Output,SA_Nested_Sfc_Output,
     & SA_Stratify_Boundary,SA_Number_of_Source_Regions,
     & SA_Number_of_Source_Groups,Use_Leftover_Group,
     & Number_of_Timing_Releases,SA_Receptor_Definitions,
     & SA_Source_Area_Map,SA_Master_Restart,SA_Nested_Restart,
     & SA_Points_Group,SA_Emiss_Group_Grid,SA_Summary_Output,
     & PSAT_Treat_SULFATE_Class,PSAT_Treat_NITRATE_Class,
     & PSAT_Treat_SOA_Class,PSAT_Treat_PRIMARY_Class,
     & PSAT_Treat_MERCURY_Class,PSAT_Treat_OZONE_Class
c
      namelist /DDM_Control/
     & DDM_File_Root,DDM_Master_Sfc_Output,DDM_Nested_Sfc_Output,
     & DDM_Stratify_Boundary,DDM_Number_of_Source_Regions,
     & DDM_Number_of_Source_Groups,Number_of_IC_Species_Groups,
     & IC_Species_Groups,Number_of_BC_Species_Groups,BC_Species_Groups,
     & Number_of_EM_Species_Groups,Emis_Species_Groups,
     & DDM_Receptor_Definitions,DDM_Source_Area_Map,
     & DDM_Initial_Conditions,DDM_Boundary_Conditions,
     & DDM_Top_Concentrations,DDM_Master_Restart,DDM_Nested_Restart,
     & DDM_Points_Group,DDM_Emiss_Group_Grid
c
      namelist /RT_Control/
     & RT_File_Root,RT_Initial_Conditions,RT_Boundary_Conditions,
     & RT_Top_Concentrations,RT_Master_Restart,RT_Nested_Restart,
     & RT_Chemistry_Parameters,RT_Receptor_Definitions,RT_Point_Sources,
     & RT_Emiss_Grid,RT_PiG_Sample
c
      namelist /PA_Control/
     & PA_File_Root,Number_of_PA_Domains,Within_CAMx_Grid,
     & PA_Beg_I_Index,PA_End_I_Index,PA_Beg_J_Index,PA_End_J_Index,
     & PA_Beg_K_Index,PA_End_K_Index
c
c======================== Probing Tool End =============================
c
      integer enddate,istrln,i,jj,l,m,n,ibyr,ibmo,ibdy,ieyr,iemo,iedy
      integer minmesh,ng
      real endtim
      character*200 ctlfil,filtmp
      character*100 action
      character*20  version, namegrp
      integer   nemiss,cbdate,cedate,inp,ii,iifroot,nopen
      logical   lexist
c
      data inp /3/
      data ctlfil /'CAMx.in'/
c
c-----Entry point
c     Initialize namelist variables to default values
c
      Restart                = .false.
      Chemistry              = .false.
      Dry_Deposition         = .false.
      Wet_Deposition         = .false.
      Staggered_Winds        = .false.
      Gridded_Emissions      = .false.
      Point_Emissions        = .false.
      Ignore_Emission_Dates  = .false.
      Diagnostic_Error_Check = .false.
      Average_Output_3D      = .false.
      HDF_Format_Output      = .false.
      PiG_Sampling_Grid      = .false.
      Sample_Background      = .false.

      Root_Output_Name     = ' '
      HDF_File_Root        = ' '
      Chemistry_Parameters = ' '
      Photolyis_Rates      = ' '
      Initial_Conditions   = ' '
      Boundary_Conditions  = ' '
      Top_Concentrations   = ' '
      Albedo_Haze_Ozone    = ' '
      Point_Sources        = ' '
      Master_Grid_Restart  = ' '
      Nested_Grid_Restart  = ' '
      PiG_Restart          = ' '
      do i = 1,MXNAM
        Landuse_Grid(i)    = ' '
        ZP_Grid(i)         = ' '
        Wind_Grid(i)       = ' '
        Temp_Grid(i)       = ' '
        Vapor_Grid(i)      = ' '
        Cloud_Grid(i)      = ' '
        Kv_Grid(i)         = ' '
        Emiss_Grid(i)      = ' '
      enddo
      Run_Message          = ' '
      Map_Projection       = ' '
      Advection_Solver     = ' '
      Chemistry_Solver     = ' '
      PiG_Submodel         = ' '
      Probing_Tool         = ' '
      do i = 1,MXNAM
        Output_Species_Names(i) = ' '
      enddo

      Time_Zone = 0
      do i = 1,4
        Start_Date_Hour(i)     = 0
        End_Date_Hour(i)       = 0
      enddo
      UTM_Zone                 = 0
      Number_of_Grids          = 0
      Master_Grid_Columns      = 0
      Master_Grid_Rows         = 0
      do i = 1,MXNAM
        Number_of_Layers(i)    = 0
        Nest_Meshing_Factor(i) = 0
        Nest_Beg_I_Index(i)    = 0
        Nest_End_I_Index(i)    = 0
        Nest_Beg_J_Index(i)    = 0
        Nest_End_J_Index(i)    = 0
      enddo
      Number_of_Output_Species = 0

      Maximum_Timestep         = 15.
      Met_Input_Frequency      = 60.
      Ems_Input_Frequency      = 60.
      Output_Frequency         = 60.
      POLAR_Longitude_Pole     = 0.
      POLAR_Latitude_Pole      = 0.
      LAMBERT_Central_Meridian = 0.
      LAMBERT_Center_Longitude = 0.
      LAMBERT_Center_Latitude  = 0.
      LAMBERT_True_Latitude1   = 0.
      LAMBERT_True_Latitude2   = 0.
      Master_Origin_XCoord     = 0.
      Master_Origin_YCoord     = 0.
      Master_Cell_XSize        = 0.
      Master_Cell_YSize        = 0.

      Number_of_Sampling_Grids     = 0
      do n = 1,MXNAM
        SG_Beg_I_Index(n)          = 0
        SG_End_I_Index(n)          = 0
        SG_Beg_J_Index(n)          = 0
        SG_End_J_Index(n)          = 0
        SG_Mesh_Factor(n)          = 0.
      enddo
c
c======================== Probing Tool Begin ===========================
c
      SA_Master_Sfc_Output  = .false.
      SA_Nested_Sfc_Output  = .false.
      SA_Stratify_Boundary  = .false.
      Use_Leftover_Group    = .false.
      SA_Summary_Output     = .false.

      DDM_Master_Sfc_Output = .false.
      DDM_Nested_Sfc_Output = .false.
      DDM_Stratify_Boundary = .false.

      SA_File_Root                  = ' '
      SA_Receptor_Definitions       = ' '
      do n = 1,MXNAM
        SA_Source_Area_Map(n)       = ' '
      enddo
      SA_Master_Restart             = ' '
      SA_Nested_Restart             = ' '
      do n = 1,MXNAM
        SA_Points_Group(n)          = ' '
        do m = 1,MXNAM
          SA_Emiss_Group_Grid(m,n)  = ' '
        enddo
      enddo
      PSAT_Treat_SULFATE_Class = .false.
      PSAT_Treat_NITRATE_Class = .false.
      PSAT_Treat_SOA_Class     = .false.
      PSAT_Treat_PRIMARY_Class = .false.
      PSAT_Treat_MERCURY_Class = .false.
      PSAT_Treat_OZONE_Class   = .false.
 
      DDM_File_Root                 = ' '
      DDM_Receptor_Definitions      = ' '
      do n = 1,MXNAM
        DDM_Source_Area_Map(n)      = ' '
      enddo
      DDM_Initial_Conditions        = ' '
      DDM_Boundary_Conditions       = ' '
      DDM_Top_Concentrations        = ' '
      DDM_Master_Restart            = ' '
      DDM_Nested_Restart            = ' '
      do n = 1,MXNAM
        DDM_Points_Group(n)         = ' '
        do m = 1,MXNAM
          DDM_Emiss_Group_Grid(m,n) = ' '
        enddo
        IC_Species_Groups(n)        = ' '
        BC_Species_Groups(n)        = ' '
        Emis_Species_Groups(n)      = ' '
      enddo

      RT_File_Root                  = ' '
      RT_Initial_Conditions         = ' '
      RT_Boundary_Conditions        = ' '
      RT_Top_Concentrations         = ' '
      RT_Master_Restart             = ' '
      RT_Nested_Restart             = ' '
      RT_Chemistry_Parameters       = ' '
      RT_Receptor_Definitions       = ' '
      RT_Point_Sources              = ' '
      do m = 1,MXNAM
        RT_Emiss_Grid(m)            = ' '
      enddo
      RT_PiG_Sample                 = .false.

      PA_File_Root                  = ' '

      SA_Number_of_Source_Regions  = 0
      SA_Number_of_Source_Groups   = 0
      Number_of_Timing_Releases    = 0

      DDM_Number_of_Source_Regions = 0
      DDM_Number_of_Source_Groups  = 0
      Number_of_IC_Species_Groups  = 0
      Number_of_BC_Species_Groups  = 0
      Number_of_EM_Species_Groups  = 0

      Number_of_PA_Domains         = 0
      do n = 1,MXNAM
        Within_CAMx_Grid(n)        = 0
        PA_Beg_I_Index(n)          = 0
        PA_End_I_Index(n)          = 0
        PA_Beg_J_Index(n)          = 0
        PA_End_J_Index(n)          = 0
        PA_Beg_K_Index(n)          = 0
        PA_End_K_Index(n)          = 0
      enddo
c
c======================== Probing Tool End =============================
c
c
c-----Open user control file and read core model parameters
c
      inquire(file=ctlfil,exist=lexist)
      if( .NOT. lexist ) goto 7007
      open(unit=inp,file=ctlfil,STATUS='UNKNOWN',ERR=7005)
      namegrp = 'CAMx_Control'
      action = ' '
      read(inp,CAMx_Control,END=7100,ERR=7101)
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
      filtmp(ii+1:ii+5) = '.mass'
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
      call cvtdate(ibyr,ibmo,ibdy,begtim,cbdate,begdate,ly2k)
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
      call cvtdate(ieyr,iemo,iedy,endtim,cedate,enddate,ly2k)
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
      nnest = ngrid - 1
      if( ngrid .LT. 1 ) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(a)') 'Number of grids must be 1 or more'
        call camxerr()
      elseif( ngrid .GT. 1 ) then
        if( ngrid .GT. MXGRID ) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(a)') 
     &            'Number of grids exceeds internal dimension.'
          write(iout,'(a,i2)') 'Number of grids to be run:  ',ngrid
          write(iout,'(a,i2)') 'Maximum dimension (MXGRID): ',MXGRID
          write(iout,'(a,a)')'Increase MXGRID and expand the grid',
     &                       ' dimension lists in CAMx.PRM'
          write(iout,'(/,a,a)')'Besides MXGRID, CAMx is coded to treat',
     &                         ' up to 4 generations of nested grids'
          write(iout,'(a,a)')'You will need to modify NESTING.F to',
     &                       ' increase this capacity'
          call camxerr()
        endif
      endif
c
c-----Master grid parameters
c
      xorg     = Master_Origin_XCoord
      yorg     = Master_Origin_YCoord
      delx     = Master_Cell_XSize
      dely     = Master_Cell_YSize
      ncol(1)  = Master_Grid_Columns
      nrow(1)  = Master_Grid_Rows
      nlay(1)  = Number_of_Layers(1)
      inst1(1) = 2
      inst2(1) = ncol(1)-1
      jnst1(1) = 2
      jnst2(1) = nrow(1)-1
c
      if( ncol(1) .GT. MXCOLA .OR. nrow(1) .GT. MXROWA .OR.
     &                             nlay(1) .GT. MXLAYA ) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(2a)') 'Input grid specifications greater than ',
     &                     'internal model dimensions.'
        write(iout,'(10X,A,5X,A)') '   Parameters','   Grid Definition'
        write(iout,'(A16,2X,I5,10X,I5)') 'Rows    (MXCOLA):',MXROWA,nrow(1)
        write(iout,'(A16,2X,I5,10X,I5)') 'Columns (MXROWA):',MXCOLA,ncol(1)
        write(iout,'(A16,2X,I5,10X,I5)') 'Layers  (MXLAYA):',MXLAYA,nlay(1)
        call camxerr()
      endif
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
          nlay(n)    = Number_of_Layers(n)
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
          if( ncol(n) .GT. MXCOLA .OR. nrow(n) .GT. MXROWA .OR.
     &                                 nlay(n) .GT. MXLAYA ) then
            write(iout,'(//,a)') 'ERROR in READNML:'
            write(iout,'(a,i5)') 'For grid # ',n
            write(iout,'(2a)') 'Input grid specifications greater ',
     &                         'than internal model dimensions.'
            write(iout,'(10X,A,5X,A)') 'Parameters','Grid Definition'
            write(iout,'(A10,5X,I5,10X,I5)') 'Rows    :',MXROWA,nrow(n)
            write(iout,'(A10,5X,I5,10X,I5)') 'Columns :',MXCOLA,ncol(n)
            write(iout,'(A10,5X,I5,10X,I5)') 'Layers  :',MXLAYA,nlay(n)
            call camxerr()
          endif
        enddo
      endif
c
c-----Call routine to set up the pointers for grid vectors
c
      call iniptr()
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
          idsolv = IDCMC
      else if( Chemistry_Solver .EQ. CDIEH ) then
          idsolv = IDIEH
      else if( Chemistry_Solver .EQ. CDLSOD ) then
          idsolv = IDLSOD
      else
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(/,1X,6A)') 'Invalid chemistry solver specified ',
     &                              'in control file: ',Chemistry_Solver
          write(iout,'(1X,A)') 'Acceptable options are: '
          write(iout,'(10X,A)') CDCMC
          write(iout,'(10X,A)') CDIEH
          write(iout,'(10X,A)') CDLSOD
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
     &    Probing_Tool .NE. DDM    .AND.
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
          write(iout,'(10X,A)') DDM
          write(iout,'(10X,A)') STRPA
          write(iout,'(10X,A)') STRIPR
          write(iout,'(10X,A)') STRIRR
          call camxerr()
       endif
c
c  --- check that switches are compatible ---
c
       if (ipigflg .EQ. GRESPIG .AND. 
     &     Probing_Tool .NE. 'NONE' .AND. Probing_Tool .NE. OSAT .AND.
     &     Probing_Tool .NE. GOAT .AND. Probing_Tool .NE. APCA .AND.
     &     Probing_Tool .NE. PSAT) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(2a)') 'GREASD PiG only works with the following',
     &                       ' Probing Tools:'
          write(iout,'(a)') 'OSAT,GOAT,APCA,PSAT'
          write(iout,'(a)') 'Turn off PiG switch and try again.'
          call camxerr()
       endif
       if (ipigflg .EQ. IRONPIG .AND.
     &     Probing_Tool .NE. 'NONE' .AND. Probing_Tool .NE. RTRAC) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(2a)') 'IRON PiG only works with the following',
     &                       ' Probing Tools:'
          write(iout,'(a)') 'RTRAC'
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
       if( iadvct .ne. 2 .AND. Probing_Tool .EQ. DDM ) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(2a)') 'Advection solver used with DDM ',
     &                                          'must be BOTT.'
          write(iout,*)'Change the control file and try again.'
          call camxerr()
       endif
c
c======================== Probing Tool End =============================
c 
      ldiag  = Diagnostic_Error_Check
      lrstrt = Restart
      lchem  = Chemistry
      ldry   = Dry_Deposition
      lwet   = Wet_Deposition
      lstagw = Staggered_Winds
      le1day = Ignore_Emission_Dates
      larsrc = Gridded_Emissions
      lptsrc = Point_Emissions
      if (ipigflg .NE. 0 .AND. .NOT.lptsrc) then
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
      l3davg = Average_Output_3D
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
      navspc = Number_of_Output_Species
      if (navspc.gt.MXSPEC) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(2a)')'Number of average species to be output ',
     &                    'is greater than internal dimensions'
        write(iout,'(a,i2)') 'Number of output species  : ',navspc
        write(iout,'(a,i2)') 'Maximum dimension (MXSPEC): ',MXSPEC
        write(iout,'(2a)')'Either increase the parameter MXSPEC ',
     &                              'in CAMx.PRM and recompile,'
        write(iout,'(a)')'or reduce the number of output species.'
        call camxerr()
      endif
      if( navspc .GT. 0 ) then
         do l = 1,navspc
           spavg(l) = Output_Species_Names(l)
         enddo
      else
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(a)')'Number of average species is zero.'
        write(iout,'(a)')'At least 1 species needs to be output.'
        call camxerr()
      endif
c
c-----PiG sampling grids
c
      nsample = 0
      lsample = PiG_Sampling_Grid
      if (.NOT.lsample) goto 100
      if (ipigflg.EQ.0) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(/,1X,A,A)')'PiG flag must be turned on',
     &                           ' to use sampling grids.'
        call camxerr()
      endif
      lbckgrd = Sample_Background
      nsample = Number_of_Sampling_Grids
      if (nsample .GT. MXSAMPLE) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(/,1X,A,I4,A)')'Number of sampling grids ',
     &               nsample,' exceeds maximum.  Increase MXSAMPLE.'
        call camxerr()
      endif
      if (nsample .EQ. 0) then
        write(iout,'(//,a)') 'ERROR in READNML:'
        write(iout,'(/,1X,A,A)')'Sampling grids are turned on ',
     &                'but number of sampling grids is set to zero'
        call camxerr()
      endif
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
        ncols(n) = (ismp2(n) - ismp1(n) + 1)*meshsmp(n)
        nrows(n) = (jsmp2(n) - jsmp1(n) + 1)*meshsmp(n)
        if (ncols(n) .GT. MXCOLS .OR. nrows(n) .GT. MXROWS) then
          write(iout,'(//,a)') 'ERROR in READNML:'
          write(iout,'(a,i3)') 'For sampling grid # ',n
          write(iout,'(2a)') 'Input grid specifications greater than ',
     &                       'internal model dimensions.'
          write(iout,'(10X,A,5X,A)') 'Dimensions','User Definition'
          write(iout,'(A10,5X,I5,10X,I5)') 'Rows    :',MXROWS,nrows(n)
          write(iout,'(A10,5X,I5,10X,I5)') 'Columns :',MXCOLS,ncols(n)
          call camxerr()
        endif
        xorgs(n) = xorg + (ismp1(n) - 1)*delx
        yorgs(n) = yorg + (jsmp1(n) - 1)*dely
      enddo
      do n = 1,nsample
        ismpgrd(n) = 1
        minmesh = 1
        do 101 ng = 2,ngrid
          if (minmesh.ge.meshold(ng)) goto 101
          if (inst1(ng).le.ismp1(n) .and. inst2(ng).ge.ismp2(n) .and.
     &        jnst1(ng).le.jsmp1(n) .and. jnst2(ng).ge.jsmp2(n)) then
            ismpgrd(n) = ng
            minmesh = meshold(ng)
          endif
 101    continue
      enddo
      ipsmp(1) = 1
      do i = 2,nsample
        ipsmp(i) = ipsmp(i-1) + ncols(i-1)*nrows(i-1)*MXSPEC
      enddo
c
c-----Echo run control parameters
c
 100  write(idiag,'(A,F10.0,I10.6,I10.5)')
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
     &         'PiG submodel               : ',PiG_Submodel
      write(idiag,'(A,L10)')
     &         'PiG Sampling Grid          : ',PiG_Sampling_Grid
      write(idiag,'(A,A)')
     &         'Probing Tools              : ',Probing_Tool
      write(idiag,'(A,L10)') 
     &         'Restart                    : ',lrstrt 
      write(idiag,'(A,L10)') 
     &         'Chemistry                  : ',lchem
      write(idiag,'(A,L10)') 
     &         'Dry deposition             : ',ldry 
      write(idiag,'(A,L10)') 
     &         'Wet deposition             : ',lwet 
      write(idiag,'(A,L10)') 
     &         'Staggered winds            : ',lstagw
      write(idiag,'(A,L10)') 
     &         'Area sources               : ',larsrc
      write(idiag,'(A,L10)') 
     &         'Point sources              : ',lptsrc
      write(idiag,'(A,L10)') 
     &         'Date-insensitive emissions : ',le1day
      write(idiag,'(A,L10)') 
     &         '3-D average file           : ',l3davg
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
      if (lsample) then
        write(idiag,*)
        write(idiag,'(A,L10)') 
     &         'Sample grid includes bckgrd: ',lbckgrd
        write(idiag,'(A,I3)')'Number PiG Sampling grids  : ',nsample
        write(idiag,*) ' ID     x-range     ncol     y-range',
     &                '     nrow   mesh factor  delx    dely'
        do n = 1,nsample
          write(idiag,'(1X,I2,2(3X,I4,3X,I4,3X,I4),6X,I4,4X,
     &                  2(2X,F6.3))')
     &          n,ismp1(n),ismp2(n),ncols(n),jsmp1(n),jsmp2(n),
     &          nrows(n),meshsmp(n),delx/float(meshsmp(n)),
     &          dely/float(meshsmp(n))
        enddo
        write(idiag,*)
      endif
c
c-----Open the remaining I/O files
c
      call openfils(iifroot,nopen)
c
c======================== Probing Tool Begin ===========================
c
c  --- set Probing Tool switches according to technology type ---
c
      ltrace = .FALSE.
      lddm   = .FALSE.
      lproca = .FALSE.
      lipr   = .FALSE.
      lirr   = .FALSE.
      lozone = .FALSE.
      tectyp = Probing_Tool
      if( tectyp .EQ. 'NONE      '  ) then
         goto 8888
      elseif( tectyp .EQ. OSAT  ) then
         ltrace = .true.
         verson = VEROSAT
         lozone = .TRUE.
      elseif( tectyp .EQ. GOAT  ) then
         ltrace = .true.
         verson = VERGOAT
         lozone = .TRUE.
      elseif( tectyp .EQ. APCA  ) then
         ltrace = .true.
         verson = VERAPCA
         lozone = .TRUE.
      elseif( tectyp .EQ. PSAT  ) then
         ltrace = .true.
         verson = VERPSAT
      elseif( tectyp .EQ. RTRAC ) then
         ltrace = .true.
         verson = VERRTRAC
      elseif( tectyp .EQ. DDM  ) then
         lddm = .TRUE.
         verson = VERDDM
      elseif( tectyp .EQ. STRPA ) then
         lproca = .TRUE.
         lipr = .TRUE.
         lirr = .TRUE.
         lsfcfl = .TRUE.
         verson = VERPA
      elseif( tectyp .EQ. STRIPR ) then
         lproca = .TRUE.
         lipr = .TRUE.
         verson = VERPA
      elseif( tectyp .EQ. STRIRR ) then
         lproca = .TRUE.
         lirr = .TRUE.
         lsfcfl = .TRUE.
         verson = VERPA
      endif
c
c  --- read Probing Tool namelists ---
c
      if( ltrace ) then
         if( tectyp .EQ. RTRAC ) then
            namegrp = 'RT_Control'
            action =  'You must have the '//namegrp(:istrln(namegrp))//
     &          ' namelist group when probing tools is set to '//
     &                                    tectyp(:istrln(tectyp))//'.'
            read(inp,RT_Control,END=7100,ERR=7101)
            flrtsa = RT_File_Root
         else
            namegrp = 'SA_Control'
            action =  'You must have the '//namegrp(:istrln(namegrp))//
     &          ' namelist group when probing tools is set to '//
     &                                    tectyp(:istrln(tectyp))//'.'
            read(inp,SA_Control,END=7100,ERR=7101)
            flrtsa = SA_File_Root
         endif 
      elseif( lddm ) then
         namegrp = 'DDM_Control'
         action =  'You must have the '//namegrp(:istrln(namegrp))//
     &          ' namelist group when probing tools is set to '//
     &                                    tectyp(:istrln(tectyp))//'.'
         read(inp,DDM_Control,END=7100,ERR=7101)
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
         if( tectyp .EQ. RTRAC ) then
            if( lsample ) then
               lsmptrc = RT_PiG_Sample
               write(idiag,*)
               write(idiag,'(A,L10)')
     &                  'Sample grid includes RTRAC : ',lsmptrc
               if( lsmptrc ) then
                  iprtsmp(1) = 1
                  do i=2,nsample
                    iprtsmp(i) = iprtsmp(i-1) + 
     &                           ncols(i-1)*nrows(i-1)*MXTRSP
                  enddo
               endif
            endif
            write(idiag,*)
            call startrt(iout,nopen)
         else
            call rdoptsa
            nemiss = ngroup
            if( leftovr ) nemiss = ngroup + 1
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
      if( lddm ) then
         call rdoptddm
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
               sfcfil(n)(1:jj+10) = filtmp
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
      stop
c
 7005 continue
      write(*,'(//,A)') 'ERROR in READNML:'
      write(*,'(2A,//)') ' Could not open control file: ',
     &                                   ctlfil(:istrln(ctlfil))
      stop
c
 7006 continue
      write(*,'(//,A)') 'ERROR in READNML:'
      write(*,'(A)') ' Root output file name is blank.'
      write(*,'(A)') ' A valid path/filename must be provided in '
      write(*,'(A,//)') ' the namelist variable: Root_Output_Name'
      stop
c
 7007 continue
      write(*,'(//,a)') 'ERROR in READNML:'
      write(*,'(2A,//)') ' Cannot find control file: ',
     &                     ctlfil(:istrln(ctlfil))
      stop
c
 7008 continue
      write(*,'(//,A)') 'ERROR in READNML:'
      write(*,'(A)') ' HDF root output file name is blank.'
      write(*,'(A)') ' A valid path/filename must be provided in '
      write(*,'(A,//)') ' the namelist variable: HDF_File_Root'
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
      stop
c
c-----Return point
c
      end

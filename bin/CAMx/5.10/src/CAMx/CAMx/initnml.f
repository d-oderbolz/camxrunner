      subroutine initnml()
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use ahomap
      use camxfld
      use camxcom
      use pigsty
c
      implicit none
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c         This routine initializes the namelist variables
c         to default values.
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       STARTUP
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Modifications:
c       03/15/09     Added code for deposition output for tracers 
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
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
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: i
      integer :: n
      integer :: m
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      Restart                = .false.
      Chemistry              = .false.
      Dry_Deposition         = .false.
      Wet_Deposition         = .false.
      ACM2_Diffusion         = .false.
      Staggered_Winds        = .false.
      Gridded_Emissions      = .false.
      Point_Emissions        = .false.
      Ignore_Emission_Dates  = .false.
      Diagnostic_Error_Check = .false.
      HDF_Format_Output      = .false.
      PiG_Sampling_Grid      = .false.
      Sample_Background      = .false.
      Average_Output_3D      = .false.
      Super_Stepping         = .true.
      do i = 1,MXNAM
        Output_3D_Grid(i)    = .false.
      enddo

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
      SA_Deposition_Output  = .false.
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
          HDDM_parameters(m,n)      = ' '
        enddo
        IC_Species_Groups(n)        = ' '
        BC_Species_Groups(n)        = ' '
        Emis_Species_Groups(n)      = ' '
        Rate_Const_Groups(n)        = ' '
        DDM_Calc_Grid(n)            = .TRUE.
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
      Number_of_Rate_Const_Groups  = 0
      Number_of_HDDM_Sens_Groups   = 0

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
      end

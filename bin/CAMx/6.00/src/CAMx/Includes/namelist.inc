c
c----CAMx v6.00 130506
c
c     NAMELIST.COM contains all input namelist variables, for both
c     the core model and Probing Tools
c                           
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c           
c     Modifications: 
c        7/20/05       Moved PiG sampling grid inputs to main namelist, added
c                      new options for sampling standard species
c       03/15/09       Added code for deposition output for tracers
c        5/25/06       Added HDF output flag
c        07/16/07 -bkoo-     Added variables for HDDM
c        06/11/08 -bkoo-     Added variables for rate constant sensitivity
c        07/16/08 -bkoo-     Added DDM turn-off flag
c        10/10/08 -cemery-   Added ACM2 option
c        10/29/09 -cemery-   Added RTRAC surface model option
c        10/10/08       Added ACM2 option
c        7/14/10        Added in-line TUV option
c        1/04/11        Revised for new met input format
c        03/29/11       Support in-line TUV with aerosol optical depth
c        08/31/11  -gwilson- Added easter egg parameter for APCA treatment
c                            with PSAT
c        04/02/12      Removed RADM cloud adjustment option, cloud/aerosol
c                      adjustments now always done with in-line TUV; AHO
c                      file is now just ozone column; number of output
c                      species now determined internally from user list
c        05/07/12      Added flexi-nesting flag
c
c-----------------------------------------------------------------------
c
      integer MXNAM
      parameter( MXNAM = 99 )
c
      logical       Restart
      logical       Flexi_Nest
      logical       Chemistry
      logical       Wet_Deposition
      logical       Gridded_Emissions
      logical       Point_Emissions
      logical       Ignore_Emission_Dates
      logical       Diagnostic_Error_Check
      logical       Average_Output_3D
      logical       Output_3D_Grid(MXNAM)
      logical       HDF_Format_Output
      logical       PiG_Sampling_Grid
      logical       Sample_Background
      logical       Super_Stepping
      logical       ACM2_Diffusion
c
      character*200 Root_Output_Name
      character*200 Chemistry_Parameters
      character*200 Photolyis_Rates
      character*200 Initial_Conditions
      character*200 Boundary_Conditions
      character*200 Top_Concentrations
      character*200 Ozone_Column
      character*200 Point_Sources
      character*200 Master_Grid_Restart
      character*200 Nested_Grid_Restart
      character*200 PiG_Restart
      character*200 Surface_Grid(MXNAM)
      character*200 Met2D_Grid(MXNAM)
      character*200 Met3D_Grid(MXNAM)
      character*200 Cloud_Grid(MXNAM)
      character*200 Vdiff_Grid(MXNAM)
      character*200 Emiss_Grid(MXNAM)
      character*200 HDF_File_Root
c
      character*60  Run_Message
c
      character*10  Map_Projection
      character*10  Advection_Solver
      character*10  Chemistry_Solver
      character*10  Drydep_Model
      character*10  PiG_Submodel
      character*10  Probing_Tool
      character*10  Output_Species_Names(MXNAM)
c
      integer       Time_Zone
      integer       Start_Date_Hour(4)
      integer       End_Date_Hour(4)
      integer       UTM_Zone
      integer       Number_of_Grids
      integer       Master_Grid_Columns
      integer       Master_Grid_Rows
      integer       Number_of_Layers
      integer       Nest_Meshing_Factor(MXNAM)
      integer       Nest_Beg_I_Index(MXNAM)
      integer       Nest_End_I_Index(MXNAM)
      integer       Nest_Beg_J_Index(MXNAM)
      integer       Nest_End_J_Index(MXNAM)
      integer       Number_of_Sampling_Grids
      integer       SG_Beg_I_Index(MXNAM)
      integer       SG_End_I_Index(MXNAM)
      integer       SG_Beg_J_Index(MXNAM)
      integer       SG_End_J_Index(MXNAM)
c
      real          Maximum_Timestep
      real          Met_Input_Frequency
      real          Ems_Input_Frequency
      real          Output_Frequency
      real          POLAR_Longitude_Pole
      real          POLAR_Latitude_Pole
      real          LAMBERT_Central_Meridian
      real          LAMBERT_Center_Longitude
      real          LAMBERT_Center_Latitude
      real          LAMBERT_True_Latitude1
      real          LAMBERT_True_Latitude2
      real          Master_Origin_XCoord
      real          Master_Origin_YCoord
      real          Master_Cell_XSize
      real          Master_Cell_YSize
      real          SG_Mesh_Factor(MXNAM)
c
      common /chr_camx_cntrl/ 
     &             Root_Output_Name, Chemistry_Parameters,
     &             Photolyis_Rates, Initial_Conditions,
     &             Boundary_Conditions, Top_Concentrations,
     &             Ozone_Column, Point_Sources,Master_Grid_Restart,
     &             Nested_Grid_Restart, PiG_Restart,
     &             Surface_Grid, Met2D_Grid, Met3D_Grid,
     &             Cloud_Grid, Vdiff_Grid, Emiss_Grid,
     &             Run_Message, Map_Projection, Advection_Solver,
     &             Chemistry_Solver, Drydep_Model, PiG_Submodel,
     &             Probing_Tool, Output_Species_Names, HDF_File_Root
c
      common /dat_camx_cntrl/ 
     &      Restart, Flexi_Nest, Chemistry, Wet_Deposition,
     &      Gridded_Emissions, Point_Emissions,
     &      Ignore_Emission_Dates, Diagnostic_Error_Check,
     &      Average_Output_3D, Output_3D_Grid, HDF_Format_Output,
     &      Super_Stepping, ACM2_Diffusion,
     &      Time_Zone, Start_Date_Hour, End_Date_Hour,
     &      UTM_Zone, Number_of_Grids, Master_Grid_Columns, 
     &      Master_Grid_Rows,Number_of_Layers, Nest_Meshing_Factor,
     &      Nest_Beg_I_Index, Nest_End_I_Index,
     &      Nest_Beg_J_Index, Nest_End_J_Index,
     &      Maximum_Timestep,
     &      Met_Input_Frequency, Ems_Input_Frequency,
     &      Output_Frequency, POLAR_Longitude_Pole, POLAR_Latitude_Pole,
     &      LAMBERT_Central_Meridian, LAMBERT_Center_Longitude,
     &      LAMBERT_Center_Latitude, LAMBERT_True_Latitude1,
     &      LAMBERT_True_Latitude2, Master_Origin_XCoord,
     &      Master_Origin_YCoord, Master_Cell_XSize, Master_Cell_YSize,
     &      PiG_Sampling_Grid, Sample_Background, Sampling_Grid, 
     &      Number_of_Sampling_Grids, 
     &      SG_Beg_I_Index, SG_End_I_Index, SG_Beg_J_Index, 
     &      SG_End_J_Index, SG_Mesh_Factor
c
c======================== Probing Tool Begin ===========================
c
      logical       SA_Master_Sfc_Output
      logical       SA_Nested_Sfc_Output
      logical       SA_Deposition_Output
      logical       SA_Stratify_Boundary
      logical       Use_Leftover_Group
      logical       SA_Summary_Output
      logical       DDM_Master_Sfc_Output
      logical       DDM_Nested_Sfc_Output
      logical       DDM_Stratify_Boundary
      logical       DDM_Calc_Grid(MXNAM)
      logical       Sampling_Grid
      logical       PSAT_Treat_SULFATE_Class
      logical       PSAT_Treat_NITRATE_Class
      logical       PSAT_Treat_SOA_Class
      logical       PSAT_Treat_PRIMARY_Class
      logical       PSAT_Treat_MERCURY_Class
      logical       PSAT_Treat_OZONE_Class
      logical       PSAT_Use_APCA
      logical       RT_Surface_Model
      logical       RT_Partitioning
c
      character*200 SA_File_Root
      character*200 SA_Receptor_Definitions
      character*200 SA_Source_Area_Map(MXNAM)
      character*200 SA_Master_Restart
      character*200 SA_Nested_Restart
      character*200 SA_Points_Group(MXNAM)
      character*200 SA_Emiss_Group_Grid(MXNAM,MXNAM)
      character*200 DDM_File_Root
      character*200 DDM_Receptor_Definitions
      character*200 DDM_Source_Area_Map(MXNAM)
      character*200 DDM_Initial_Conditions
      character*200 DDM_Boundary_Conditions
      character*200 DDM_Top_Concentrations
      character*200 DDM_Master_Restart
      character*200 DDM_Nested_Restart
      character*200 DDM_Points_Group(MXNAM)
      character*200 DDM_Emiss_Group_Grid(MXNAM,MXNAM)
      character*200 RT_File_Root
      character*200 RT_Initial_Conditions
      character*200 RT_Boundary_Conditions
      character*200 RT_Top_Concentrations
      character*200 RT_Master_Restart
      character*200 RT_Nested_Restart
      character*200 RT_Chemistry_Parameters
      character*200 RT_Receptor_Definitions
      character*200 RT_Point_Sources
      character*200 RT_Emiss_Grid(MXNAM)
      character*200 RT_Srfmod_Grid(MXNAM)
      character*200 PA_File_Root
      character*200 Rate_Const_Groups(MXNAM)
c
      character*10  IC_Species_Groups(MXNAM)
      character*10  BC_Species_Groups(MXNAM)
      character*10  Emis_Species_Groups(MXNAM)
      character*10  HDDM_parameters(MXNAM,MXNAM)
c
      integer       SA_Number_of_Source_Regions
      integer       SA_Number_of_Source_Groups
      integer       Number_of_Timing_Releases
      integer       DDM_Number_of_Source_Regions
      integer       DDM_Number_of_Source_Groups
      integer       Number_of_IC_Species_Groups
      integer       Number_of_BC_Species_Groups
      integer       Number_of_EM_Species_Groups
      integer       Number_of_HDDM_Sens_Groups
      integer       Number_of_Rate_Const_Groups
      integer       Number_of_PA_Domains
      integer       Within_CAMx_Grid(MXNAM)
      integer       PA_Beg_I_Index(MXNAM)
      integer       PA_End_I_Index(MXNAM)
      integer       PA_Beg_J_Index(MXNAM)
      integer       PA_End_J_Index(MXNAM)
      integer       PA_Beg_K_Index(MXNAM)
      integer       PA_End_K_Index(MXNAM)
      logical       RT_PiG_Sample
c
      common /chr_sa_cntrl/
     &        SA_File_Root, SA_Receptor_Definitions, SA_Source_Area_Map,
     &        SA_Master_Restart, SA_Nested_Restart, SA_Points_Group,
     &        SA_Emiss_Group_Grid
c
      common /dat_sa_cntrl/
     &        SA_Master_Sfc_Output, SA_Nested_Sfc_Output,
     &        SA_Deposition_Output,
     &        SA_Stratify_Boundary, Use_Leftover_Group,
     &        SA_Summary_Output, PSAT_Treat_SULFATE_Class,
     &        PSAT_Treat_NITRATE_Class, PSAT_Treat_SOA_Class,
     &        PSAT_Treat_PRIMARY_Class, PSAT_Treat_MERCURY_Class,
     &        PSAT_Treat_OZONE_Class, SA_Number_of_Source_Regions,
     &        SA_Number_of_Source_Groups, Number_of_Timing_Releases,
     &        PSAT_Use_APCA
c
      common /chr_ddm_cntrl/
     &        DDM_File_Root, DDM_Receptor_Definitions,
     &        DDM_Source_Area_Map, DDM_Initial_Conditions,
     &        DDM_Boundary_Conditions, DDM_Top_Concentrations,
     &        DDM_Master_Restart, DDM_Nested_Restart,
     &        DDM_Points_Group, DDM_Emiss_Group_Grid,
     &        IC_Species_Groups, BC_Species_Groups, Emis_Species_Groups,
     &        HDDM_parameters,Rate_Const_Groups
c
      common /dat_ddm_cntrl/
     &        DDM_Master_Sfc_Output, DDM_Nested_Sfc_Output,
     &        DDM_Stratify_Boundary, DDM_Number_of_Source_Regions,
     &        DDM_Number_of_Source_Groups, Number_of_IC_Species_Groups,
     &        Number_of_BC_Species_Groups, Number_of_EM_Species_Groups,
     &        Number_of_HDDM_Sens_Groups,Number_of_Rate_Const_Groups,
     &        DDM_Calc_Grid
c
      common /chr_rt_cntrl/
     &        RT_File_Root, RT_Initial_Conditions,
     &        RT_Boundary_Conditions, RT_Top_Concentrations,
     &        RT_Master_Restart, RT_Nested_Restart,
     &        RT_Chemistry_Parameters, RT_Receptor_Definitions,
     &        RT_Point_Sources, RT_Emiss_Grid, RT_PiG_Sample,
     &        RT_Surface_Model,RT_Partitioning,
     &        RT_Srfmod_Grid
c
      common /chr_pa_cntrl/
     &        PA_File_Root
c
      common /dat_pa_cntrl/
     &        Number_of_PA_Domains, Within_CAMx_Grid,
     &        PA_Beg_I_Index, PA_End_I_Index,
     &        PA_Beg_J_Index, PA_End_J_Index,
     &        PA_Beg_K_Index, PA_End_K_Index
c
c======================== Probing Tool End ===========================
c

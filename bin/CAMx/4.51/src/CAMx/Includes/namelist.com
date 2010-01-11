c
c-----CAMx v4.51 080522
c
c     NAMELIST.COM contains all input namelist variables, for both
c     the core model and Probing Tools
c                           
c     Copyright 1996-2008
c     ENVIRON International Corporation
c           
c     Modifications: 
c        7/20/05       Moved PiG sampling grid inputs to main namelist, added
c                      new options for sampling standard species
c
c-----------------------------------------------------------------------
c
      integer MXNAM
      parameter( MXNAM = 99 )
c
      logical Restart,Chemistry,Dry_Deposition,Wet_Deposition,
     &        Staggered_Winds,Gridded_Emissions,Point_Emissions,
     &        Ignore_Emission_Dates,Diagnostic_Error_Check,
     &        Average_Output_3D,HDF_Format_Output
      logical PiG_Sampling_Grid,Sample_Background
      character*200 Root_Output_Name,Chemistry_Parameters,
     &              Photolyis_Rates,Initial_Conditions,
     &              Boundary_Conditions,Top_Concentrations,
     &              Albedo_Haze_Ozone,Point_Sources,Master_Grid_Restart,
     &              Nested_Grid_Restart,PiG_Restart,
     &              Landuse_Grid(MXNAM),ZP_Grid(MXNAM),
     &              Wind_Grid(MXNAM),Temp_Grid(MXNAM),
     &              Vapor_Grid(MXNAM),Cloud_Grid(MXNAM),
     &              Kv_Grid(MXNAM),Emiss_Grid(MXNAM),
     &              HDF_File_Root
      character*60 Run_Message
      character*10 Map_Projection,Advection_Solver,Chemistry_Solver,
     &             PiG_Submodel,Probing_Tool,Output_Species_Names(MXNAM)
      integer Time_Zone,Start_Date_Hour(4),End_Date_Hour(4),UTM_Zone,
     &        Number_of_Grids,Master_Grid_Columns,Master_Grid_Rows,
     &        Number_of_Layers(MXNAM),Nest_Meshing_Factor(MXNAM),
     &        Nest_Beg_I_Index(MXNAM),Nest_End_I_Index(MXNAM),
     &        Nest_Beg_J_Index(MXNAM),Nest_End_J_Index(MXNAM),
     &        Number_of_Output_Species
      integer Number_of_Sampling_Grids,SG_Beg_I_Index(MXNAM),
     &        SG_End_I_Index(MXNAM),SG_Beg_J_Index(MXNAM),
     &        SG_End_J_Index(MXNAM)
      real Maximum_Timestep,Met_Input_Frequency,Ems_Input_Frequency,
     &     Output_Frequency,POLAR_Longitude_Pole,POLAR_Latitude_Pole,
     &     LAMBERT_Central_Meridian,LAMBERT_Center_Longitude,
     &     LAMBERT_Center_Latitude,LAMBERT_True_Latitude1,
     &     LAMBERT_True_Latitude2,Master_Origin_XCoord,
     &     Master_Origin_YCoord,Master_Cell_XSize,Master_Cell_YSize
      real SG_Mesh_Factor(MXNAM)
c
      common /chr_camx_cntrl/ 
     &              Root_Output_Name,Chemistry_Parameters,
     &              Photolyis_Rates,Initial_Conditions,
     &              Boundary_Conditions,Top_Concentrations,
     &              Albedo_Haze_Ozone,Point_Sources,Master_Grid_Restart,
     &              Nested_Grid_Restart,PiG_Restart,
     &              Landuse_Grid,ZP_Grid,Wind_Grid,Temp_Grid,
     &              Vapor_Grid,Cloud_Grid,Kv_Grid,Emiss_Grid,
     &              Run_Message,Map_Projection,Advection_Solver,
     &              Chemistry_Solver,PiG_Submodel,
     &              Probing_Tool,Output_Species_Names,HDF_File_Root
c
      common /dat_camx_cntrl/ 
     &       Restart,Chemistry,Dry_Deposition,Wet_Deposition,
     &       Staggered_Winds,Gridded_Emissions,Point_Emissions,
     &       Ignore_Emission_Dates,Diagnostic_Error_Check,
     &       Average_Output_3D,HDF_Format_Output,
     &       Time_Zone,Start_Date_Hour,End_Date_Hour,UTM_Zone,
     &       Number_of_Grids,Master_Grid_Columns,Master_Grid_Rows,
     &       Number_of_Layers,Nest_Meshing_Factor,
     &       Nest_Beg_I_Index,Nest_End_I_Index,
     &       Nest_Beg_J_Index,Nest_End_J_Index,
     &       Number_of_Output_Species,Maximum_Timestep,
     &       Met_Input_Frequency,Ems_Input_Frequency,
     &       Output_Frequency,POLAR_Longitude_Pole,POLAR_Latitude_Pole,
     &       LAMBERT_Central_Meridian,LAMBERT_Center_Longitude,
     &       LAMBERT_Center_Latitude,LAMBERT_True_Latitude1,
     &       LAMBERT_True_Latitude2,Master_Origin_XCoord,
     &       Master_Origin_YCoord,Master_Cell_XSize,Master_Cell_YSize,
     &       PiG_Sampling_Grid,Sample_Background,
     &       Number_of_Sampling_Grids,SG_Beg_I_Index,
     &       SG_End_I_Index,SG_Beg_J_Index,SG_End_J_Index,
     &       SG_Mesh_Factor
c
c
c======================== Probing Tool Begin ===========================
c
      logical SA_Master_Sfc_Output,SA_Nested_Sfc_Output,
     &        SA_Stratify_Boundary,Use_Leftover_Group,
     &        SA_Summary_Output
      logical DDM_Master_Sfc_Output,DDM_Nested_Sfc_Output,
     &        DDM_Stratify_Boundary
      logical PSAT_Treat_SULFATE_Class
      logical PSAT_Treat_NITRATE_Class
      logical PSAT_Treat_SOA_Class
      logical PSAT_Treat_PRIMARY_Class
      logical PSAT_Treat_MERCURY_Class
      logical PSAT_Treat_OZONE_Class
      logical RT_PiG_Sample
      character*200 SA_File_Root,SA_Receptor_Definitions,
     &              SA_Source_Area_Map(MXNAM),SA_Master_Restart,
     &              SA_Nested_Restart,SA_Points_Group(MXNAM),
     &              SA_Emiss_Group_Grid(MXNAM,MXNAM)
      character*200 DDM_File_Root,DDM_Receptor_Definitions,
     &              DDM_Source_Area_Map(MXNAM),DDM_Initial_Conditions,
     &              DDM_Boundary_Conditions,DDM_Top_Concentrations,
     &              DDM_Master_Restart,DDM_Nested_Restart,
     &              DDM_Points_Group(MXNAM),
     &              DDM_Emiss_Group_Grid(MXNAM,MXNAM)
      character*200 RT_File_Root,RT_Initial_Conditions,
     &              RT_Boundary_Conditions,RT_Top_Concentrations,
     &              RT_Master_Restart,RT_Nested_Restart,
     &              RT_Chemistry_Parameters,RT_Receptor_Definitions,
     &              RT_Point_Sources,RT_Emiss_Grid(MXNAM)
      character*200 PA_File_Root
      character*10 IC_Species_Groups(MXNAM),
     &             BC_Species_Groups(MXNAM),
     &             Emis_Species_Groups(MXNAM)
      integer SA_Number_of_Source_Regions,SA_Number_of_Source_Groups,
     &        Number_of_Timing_Releases
      integer DDM_Number_of_Source_Regions,DDM_Number_of_Source_Groups,
     &        Number_of_IC_Species_Groups,Number_of_BC_Species_Groups,
     &        Number_of_EM_Species_Groups
      integer Number_of_PA_Domains,Within_CAMx_Grid(MXNAM),
     &        PA_Beg_I_Index(MXNAM),PA_End_I_Index(MXNAM),
     &        PA_Beg_J_Index(MXNAM),PA_End_J_Index(MXNAM),
     &        PA_Beg_K_Index(MXNAM),PA_End_K_Index(MXNAM)
c
      common /chr_sa_cntrl/
     &        SA_File_Root,SA_Receptor_Definitions,SA_Source_Area_Map,
     &        SA_Master_Restart,SA_Nested_Restart,SA_Points_Group,
     &        SA_Emiss_Group_Grid
c
      common /dat_sa_cntrl/
     &        SA_Master_Sfc_Output,SA_Nested_Sfc_Output,
     &        SA_Stratify_Boundary,Use_Leftover_Group,
     &        SA_Summary_Output,PSAT_Treat_SULFATE_Class,
     &        PSAT_Treat_NITRATE_Class,PSAT_Treat_SOA_Class,
     &        PSAT_Treat_PRIMARY_Class,PSAT_Treat_MERCURY_Class,
     &        PSAT_Treat_OZONE_Class,SA_Number_of_Source_Regions,
     &        SA_Number_of_Source_Groups,Number_of_Timing_Releases
c
      common /chr_ddm_cntrl/
     &        DDM_File_Root,DDM_Receptor_Definitions,
     &        DDM_Source_Area_Map,DDM_Initial_Conditions,
     &        DDM_Boundary_Conditions,DDM_Top_Concentrations,
     &        DDM_Master_Restart,DDM_Nested_Restart,
     &        DDM_Points_Group,DDM_Emiss_Group_Grid,
     &        IC_Species_Groups,BC_Species_Groups,Emis_Species_Groups

      common /dat_ddm_cntrl/
     &        DDM_Master_Sfc_Output,DDM_Nested_Sfc_Output,
     &        DDM_Stratify_Boundary,DDM_Number_of_Source_Regions,
     &        DDM_Number_of_Source_Groups,Number_of_IC_Species_Groups,
     &        Number_of_BC_Species_Groups,Number_of_EM_Species_Groups
c
      common /chr_rt_cntrl/
     &        RT_File_Root,RT_Initial_Conditions,
     &        RT_Boundary_Conditions,RT_Top_Concentrations,
     &        RT_Master_Restart,RT_Nested_Restart,
     &        RT_Chemistry_Parameters,RT_Receptor_Definitions,
     &        RT_Point_Sources,RT_Emiss_Grid
      common /dat_rt_cntrl/ RT_PiG_Sample
c
      common /chr_pa_cntrl/
     &        PA_File_Root

      common /dat_pa_cntrl/
     &        Number_of_PA_Domains,Within_CAMx_Grid,
     &        PA_Beg_I_Index,PA_End_I_Index,
     &        PA_Beg_J_Index,PA_End_J_Index,
     &        PA_Beg_K_Index,PA_End_K_Index
c
c======================== Probing Tool End ===========================
c

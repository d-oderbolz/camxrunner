c----CAMx v5.10 090918
c 
c     FILUNIT.COM contains all model I/O unit numbers
c                           
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c           
c     Modifications: 
c        8/30/02    Cloud file now contains rain record, rain unit removed.
c                   Cloud/rain and water vapor files can be read in for all
c                   nests
c        1/10/03    Added deposition file units
c        10/12/04   Added file root name as common variable
c        8/23/06    Instantaneous restart files reduced to 1 per grid type
c        8/25/06    Average and deposition output files now one file per grid
c
c-----------------------------------------------------------------------
c     File units for ASCII and Fortran binary I/O files:
c
c     icur_unit -- unit number of the current file being opened
c     iout      -- message output file
c     idiag     -- diagnostic output file
c     imass     -- mass summary output file
c     iconc     -- coarse grid instantaneous concentration output file
c     ifconc    -- fine grid instantaneous concentration output file
c     iavg      -- average concentration output file
c     idep      -- deposition output file
c     ipig      -- PiG output file
c     ichem     -- chemistry parameters input file
c     iphot     -- photolysis lookup input file
c     ih2o      -- water vapor concentration input file
c     icld      -- cloud/rain input file
c     iic       -- initial conditions input file
c     ibc       -- boundary conditions input file
c     itopc     -- top conditions input file
c     iaho      -- albedo/haze/ozone input file
c     iptem     -- point source input file
c     ihtp      -- layer height/pressure input file
c     iarem     -- area emission input file
c     isurf     -- landuse input file
c     iwind     -- wind input file
c     itemp     -- temperature input file
c     ikv       -- vertical diffusivity input file
c     irstc     -- coarse grid restart input file
c     irstf     -- fine grid restart input file
c     irstp     -- PiG restart file
c     filroot   -- Root output file name
c     hdfroot   -- HDF root output file name
c-----------------------------------------------------------------------
c
      character*200 filroot
      character*200 hdfroot
      integer       icur_unit
      integer       iout
      integer       idiag
      integer       imass
      integer       iconc
      integer       ifconc
      integer       ipig
      integer       ichem
      integer       iphot
      integer       iic
      integer       ibc
      integer       itopc
      integer       iaho
      integer       iptem
      integer       irstc
      integer       irstf
      integer       irstp
c
      common /funit/ icur_unit, iout, idiag, imass, iconc, ifconc,
     &               ipig, ichem, iphot, iic, ibc, 
     &               itopc, iaho, iptem, irstc, irstf, irstp
      common /fuchr/ filroot, hdfroot
c
c========================= Process Analysis Begin ==============================
c
c     ipr_unit  -- unit number of Integrated Process Rates output file
c     irr_unit  -- unit number of Integrated Reaction Rates output file
c
      integer ipr_unit
      integer irr_unit
c
      common /paunit/ ipr_unit, irr_unit
c
c========================= Process Analysis End ==============================
c
c
c-----------------------------------------------------------------------
c     File units for ASCII and Fortran binary I/O files:
c     These are dynamically allocated.
c
c     iarem     -- area emission input file
c     isurf     -- landuse input file
c     ihtp      -- layer height/pressure input file
c     iwind     -- wind input file
c     itemp     -- temperature input file
c     ikv       -- vertical diffusivity input file
c     ih2o      -- water vapor concentration input file
c     icld      -- cloud/rain input file
c-----------------------------------------------------------------------
c
      integer, allocatable, dimension(:) :: iavg
      integer, allocatable, dimension(:) :: idep
      integer, allocatable, dimension(:) :: iarem
      integer, allocatable, dimension(:) :: isurf
      integer, allocatable, dimension(:) :: ihtp
      integer, allocatable, dimension(:) :: iwind
      integer, allocatable, dimension(:) :: itemp
      integer, allocatable, dimension(:) :: ikv
      integer, allocatable, dimension(:) :: ih2o
      integer, allocatable, dimension(:) :: icld
      integer, allocatable, dimension(:) :: isample
c
      integer itag
      common /tagdat/ itag

c**** RTCMCCHM
c
c----CAMx v5.41 121109
c
c----------------------------------------------------------------------
c
c    Include file for parameters and data structures used by the 
c    RTRAC Chemical Mechanism Compiler (RTCMC)
c
c    Copyright 1996 - 2012
c    ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    07/06/07   -gyarwood-  original development
c    12/08/08   -cemery-    memory management modified for v5
c
c-----------------------------------------------------------------------
c    Parameters for RTCMC:
c-----------------------------------------------------------------------
c
c  MXRX     I  maximum number of RTCMC reactions
c  MXKPRM   I  maximum number of rate constant parameters
c  MXPHOT   I  maximum number of photolysis reactions
c  MXZEN    I  maximum number of photolysis reaction zenith angles
c  MXRCT    I  maximum number of reactants in each reaction
c  MXPRD    I  maximum number of products in each reaction
c  MXJACTRM I  maximum number of Jacobian terms (an estimate)
c  MXEQM    I  maximum number of equilibrium species
c  MXSLO    I  maximum number of slow species
c
      integer   MXRX
      integer   MXKPRM
      integer   MXPHOT
      integer   MXZEN
      integer   MXRCT
      integer   MXPRD
      integer   MXJACTRM
      integer   MXEQM
      integer   MXSLO
c
      parameter( MXRX   = 20)
      parameter( MXKPRM = 12)
      parameter( MXPHOT = 10)
      parameter( MXZEN  = 15)
      parameter( MXRCT  = 03)
      parameter( MXPRD  = 20)
      parameter( MXJACTRM = MXRX*MXPRD/2 )
      parameter( MXEQM  = 05)
      parameter( MXSLO  = 25)
c
c-----------------------------------------------------------------------
c   Control variables 
c-----------------------------------------------------------------------
c
c   isolv     I   solver (1=DLSODE, 2=SLSODE, 3=Rosenbrock)
c   ijac      I   solver (1=numeric, 2=algebraic)
c   rtolcmc   r   relative error tolerance
c   atolcmc   r   absolute error tolerance
c
      integer      isolv
      integer      ijac
      real         rtolrtc
      real         atolrtc
c
      common /rtccn1/ isolv, ijac
      common /rtccn3/ rtolrtc, atolrtc
c
c-----------------------------------------------------------------------
c   Variables defining the RTCMC rate constants:
c-----------------------------------------------------------------------
c
c   ktype     I   type of rate expressions (1=CAMx, 2=SCICHEM)
c   nrxnrtc   I   number of gas phase reactions
c   icunit    I   concentration unit type (1=molecules, 2=ppm)
c   itunit    I   time unit type (1=sec, 2=min, 3=hrs)
c   ityprtc   I   rate expression type 
c   nrkprm    I   number of parameters entered for each reaction
c   rkprmrtc  R   parameters entered for each reaction
c   srkrtc    R   computed rate constants for a specific T and P
c   rkrtc     D   computed rate constants for a specific T and P
c
      integer      ktype
      integer      nrxnrtc
      integer      icunit
      integer      itunit
      integer, allocatable, dimension(:) ::   ityprtc
      integer, allocatable, dimension(:) ::   nrkprm
      real,    allocatable, dimension(:,:) :: rkprmrtc
c
      common /rtcrx1/ ktype, nrxnrtc, icunit, itunit
c
      real   srkrtc(MXRX)
      real*8 rkrtc(MXRX)
      common /rtrtcdat/ srkrtc, rkrtc
c$omp threadprivate(/rtrtcdat/)
c
c-----------------------------------------------------------------------
c   Variables defining the SCICHEM IMC file photolysis rates 
c-----------------------------------------------------------------------
c
c   njschm    I   number of photolysis reactions
c   nzschm    I   number of photolysis reaction zenith angles
c   ijschm    I   indices of photolysis reactions
c   zenschm   R   zenith angles for photolysis rates
c   rjschm    R   values of photolysis rates
c
      integer      njschm
      integer      nzschm
      integer, allocatable, dimension(:) ::   ijschm
      real,    allocatable, dimension(:) ::   zenschm
      real,    allocatable, dimension(:,:) :: rjschm
c
      common /rtcjj1/ njschm, nzschm
c
c-----------------------------------------------------------------------
c   Variables defining the RTCMC reactions and species:
c-----------------------------------------------------------------------
c
c   mtype     I   mechanism input format (1=OZIP/CMC, 2=SCICHEM)
c   ngasrtc   I   number of gas phase species - total
c   nfstrtc   I   number of gas phase species - fast
c   nslortc   I   number of gas phase species - slow
c   neqmrtc   I   number of gas phase species - equilibrium
c   nfixrtc   I   number of gas phase species - fixed
c   ngasschm  I   number of gas phase species declared in SCICHEM IMC file
c   itypsp    I   type of each gas phase species (fast, slow, eqm, fixed)
c   itypschm  I   type of each gas phase species requested in IMC file
c   lblrxn    I   number of reaction read from input file (reaction label)
c   nrct      I   number of reactants in each reactant (the order)
c   nprd      I   number of products in each reaction
c   idxrct    I   species index of each reactant in each reaction
c   idxprd    I   species index of each product in each reaction
c   conschm   R   background concentrations provided in SCICHEM IMC file
c   depvschm  R   dry deposition velocities (m/s) provided in SCICHEM IMC file
c   wetschm   R   wet scavenging rates (1/s) provided in SCICHEM IMC file
c   spdcoef   R   product coefficients (yields) for each reaction
c   dryrtc    R   dry deposition velocities (m/s) in final RTRAC gas list
c   wetrtc    R   wet scavinging rates (1/s) in final RTRAC gas list
c   prdcoef   D   product coefficients (yields) for each reaction
c   spnmrt    C   names of gas phase species found in mechanism
c   spnmschm  C   names of gas phase species declared in SCICHEM IMC file
c   namrct    C   name of each reactant in each reaction
c   namprd    C   name of each product in each reaction
c
      integer      mtype
      integer      ngasrtc
      integer      nfstrtc
      integer      nslortc
      integer      neqmrtc
      integer      nfixrtc
      integer      ngasschm
      integer, allocatable, dimension(:) ::   itypsp
      integer, allocatable, dimension(:) ::   itypschm
      integer, allocatable, dimension(:) ::   lblrxn
      integer, allocatable, dimension(:) ::   nrct
      integer, allocatable, dimension(:) ::   nprd
      integer, allocatable, dimension(:,:) :: idxrct
      integer, allocatable, dimension(:,:) :: idxprd
      real,    allocatable, dimension(:) ::   conschm
      real,    allocatable, dimension(:) ::   depvschm
      real,    allocatable, dimension(:) ::   wetschm
      real,    allocatable, dimension(:,:) :: spdcoef
      real,    allocatable, dimension(:) ::   dryrtc
      real,    allocatable, dimension(:) ::   wetrtc
      real*8,  allocatable, dimension(:,:) :: prdcoef
      character*10, allocatable, dimension(:) ::   spnmrt
      character*10, allocatable, dimension(:) ::   spnmschm
      character*10, allocatable, dimension(:,:) :: namrct
      character*10, allocatable, dimension(:,:) :: namprd
c
      common /rtcsp1/ mtype, ngasrtc, nfstrtc, nslortc, neqmrtc, 
     &                nfixrtc, ngasschm
c
c-----------------------------------------------------------------------
c   Variables used by RATECMC to fill the rates, ydot=(dy/dt)
c   Only include fast species in ydot
c-----------------------------------------------------------------------
c
c   nrctfst   I   number of fast reactants
c   nprdfst   I   number of fast products
c   idxrctfst I   species index array for fast reactants
c   idxprdfst I   species index array for fast products
c   prdcofst  D   fast product coefficients (yields) for each reaction
c
      integer, allocatable, dimension(:) ::   nrctfst
      integer, allocatable, dimension(:) ::   nprdfst
      integer, allocatable, dimension(:,:) :: idxrctfst
      integer, allocatable, dimension(:,:) :: idxprdfst
      real*8,  allocatable, dimension(:,:) ::  prdcofst
c
c-----------------------------------------------------------------------
c   Variables used by JACCMC to fill the Jacobian, pd=(df/dy)
c   Only include fast species in pd
c-----------------------------------------------------------------------
c
c   njactrm   I   number of terms to complete the Jacobian
c   ipd       I   first index into pd for each term
c   jpd       I   second index into pd for each term
c   idrxjac   I   reaction number for each term
c   nspjac    I   number of species appearing in each term
c   idspjac   I   indices of species appearing in each term
c   scoefjac  R   coefficient for each term
c   coefjac   D   coefficient for each term
c
      integer      njactrm
      integer, allocatable, dimension(:) ::   ipd
      integer, allocatable, dimension(:) ::   jpd
      integer, allocatable, dimension(:) ::   idrxjac
      integer, allocatable, dimension(:) ::   nspjac
      integer, allocatable, dimension(:,:) :: idspjac
      real,    allocatable, dimension(:) ::   scoefjac
      real*8,  allocatable, dimension(:) ::   coefjac
c
      common /rtcjac1/ njactrm
c
c-----------------------------------------------------------------------
c   Variables used by EQMCMC to solve equilibrium (steady state) species
c-----------------------------------------------------------------------
c
c   idslo     I   indices of slow species
c   nslgain   I   number of reactions that produce each eqm species
c   nslloss   I   number of reactions that destroy each eqm species
c   islgain   I   list of reactions that produce each eqm species
c   islloss   I   list of reactions that destroy each eqm species
c   spdcoslo  R   product coefficients for slow species, by reaction
c   prdcoslo  D   product coefficients for slow species, by reaction
c
      integer, allocatable, dimension(:) ::   idslo
      integer, allocatable, dimension(:) ::   nslgain
      integer, allocatable, dimension(:) ::   nslloss
      integer, allocatable, dimension(:,:) :: islgain
      integer, allocatable, dimension(:,:) :: islloss
      real*8,  allocatable, dimension(:,:) :: spdcoslo
      real*8,  allocatable, dimension(:,:) :: prdcoslo
c
c-----------------------------------------------------------------------
c   Variables used by EQMCMC to solve equilibrium (steady state) species
c-----------------------------------------------------------------------
c
c   ideqm     I   indices of equilibrium species
c   nrxgain   I   number of reactions that produce each eqm species
c   nrxloss   I   number of reactions that destroy each eqm species
c   irxgain   I   list of reactions that produce each eqm species
c   irxloss   I   list of reactions that destroy each eqm species
c   nrxupdt   I   number of reactions that depend upon an eqm species
c   irxupdt   I   list of reactions that depend upon an eqm species
c   spdcoeqm  R   product coefficients for eqm species, by reaction
c   prdcoeqm  D   product coefficients for eqm species, by reaction
c
      integer      nrxupdt
      integer, allocatable, dimension(:) ::   ideqm
      integer, allocatable, dimension(:) ::   nrxgain
      integer, allocatable, dimension(:) ::   nrxloss
      integer, allocatable, dimension(:,:) :: irxgain
      integer, allocatable, dimension(:,:) :: irxloss
      integer, allocatable, dimension(:) ::   irxupdt
      real,    allocatable, dimension(:,:) :: spdcoeqm
      real*8,  allocatable, dimension(:,:) :: prdcoeqm
c
      common /rtceqm1/ nrxupdt
c
c-----------------------------------------------------------------------
c   Variables used for fixed species (called ambient in SCICHEM)
c-----------------------------------------------------------------------
c
c   idxfix    I   pointer used to fill concentrations of fixed species
c   nam_M     C   name of the fixed species M (air)
c   nam_O2    C   name of the fixed species O2
c   nam_N2    C   name of the fixed species N2
c   nam_H2O   C   name of the fixed species H2O
c   nam_H2    C   name of the fixed species H2
c   nam_CH4   C   name of the fixed species CH4
c
      integer, allocatable, dimension(:) :: idxfix
      character*10   nam_M
      character*10   nam_O2
      character*10   nam_N2
      character*10   nam_H2O
      character*10   nam_H2
      character*10   nam_CH4
c
      common /rtcfix2/ nam_M, nam_O2, nam_N2, nam_H2O, nam_H2, nam_CH4
c
c-----------------------------------------------------------------------

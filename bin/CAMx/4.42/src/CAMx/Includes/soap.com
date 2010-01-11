c-----CAMx v4.42 070514
c
c     SOAP.COM contains common parameters and variables for SOAP
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Modifications:
c
c-----------------------------------------------------------------------
c     Parameters
c
c     NSOAP   -- number of CG/SOA species pairs
c-----------------------------------------------------------------------
c
      integer      NSOAP
c
      parameter  ( NSOAP = 5 )
c
      integer      PFLAG       ! bkoo (03/09/03)
c
      parameter  ( PFLAG = 1 ) ! 1 if there is pre-existing organic aerosol; 0 if not
c
c-----------------------------------------------------------------------
c     Variables that are initialized in soapdat.f
c
c     mwsoap  -- molecular weights of CG/SOA species (g/mol)
c     csat    -- saturation concentrations of CG/SOA species (ug/m3)
c     cstemp  -- temperatures corresponding to saturation concentrations
c                of CG/SOA species (K)
c     deltah  -- enthalpy of vaporization of CG/SOA species (J/mol)
c     flagsoap-- set to 1 if CG/SOA species forms solutions; 0 if not
c-----------------------------------------------------------------------
c
      REAL         mwsoap(NSOAP)
      REAL         csat(NSOAP)
      REAL         cstemp(NSOAP)
      REAL         deltah(NSOAP)
      INTEGER      flagsoap(NSOAP)
c
      common /soapx/ mwsoap, csat, cstemp, deltah, flagsoap
c
c-----------------------------------------------------------------------

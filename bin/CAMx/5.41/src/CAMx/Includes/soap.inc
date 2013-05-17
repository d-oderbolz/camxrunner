c----CAMx v5.41 121109
c
c     SOAP.COM contains common parameters and variables for SOAP
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c     Modifications:
c
c-----------------------------------------------------------------------
c     Parameters
c
c     NSOAP   -- number of CG/SOA species pairs
c     NASOA   -- number of anthropogenic SOA species
c     *** Notes for polymerization
c         1. anthro SOAs must be ordered before biogenic SOAs
c         2. SOA from SQT must be ordered last
c-----------------------------------------------------------------------
c
      integer      NSOAP, NASOA
c
      parameter  ( NSOAP = 7 )
      parameter  ( NASOA = 2 )
c
      integer      PFLAG       ! bkoo (03/09/03)
c
      parameter  ( PFLAG = 1 ) ! 1 if there is pre-existing organic aerosol; 0 if not
c
c-----------------------------------------------------------------------
c     Variables that are initialized in soapdat.f
c
c     mwsoap  -- molecular weights of CG/SOA species (g/mol)
c     mwsopa  -- molecular weights of SOPA (g/mol)
c     mwsopb  -- molecular weights of SOPB (g/mol)
c     csat    -- saturation concentrations of CG/SOA species (ug/m3)
c     cstemp  -- temperatures corresponding to saturation concentrations
c                of CG/SOA species (K)
c     deltah  -- enthalpy of vaporization of CG/SOA species (J/mol)
c     fpoly0  -- polymerized mass fraction per hour
c     flagsoap-- set to 1 if CG/SOA species forms solutions; 0 if not
c-----------------------------------------------------------------------
c
      REAL         mwsoap(NSOAP)
      REAL         mwsopa,mwsopb
      REAL         csat(NSOAP)
      REAL         cstemp(NSOAP)
      REAL         deltah(NSOAP)
      REAL         fpoly0
      INTEGER      flagsoap(NSOAP)
c
      common /soapx/ mwsoap, mwsopa, mwsopb, csat, cstemp, deltah,
     &               fpoly0, flagsoap
c
c-----------------------------------------------------------------------

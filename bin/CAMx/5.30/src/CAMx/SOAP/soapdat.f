      Block Data soapdat
c
c-----------------------------------------------------------------------
c     Initialize common block variables for SOAP
c
c     mwsoap   - molecular weights of CG/SOA species (g/mol)
c     mwsopa   - molecular weights of SOPA (g/mol)
c     mwsopb   - molecular weights of SOPB (g/mol)
c     csat     - saturation concentrations of SOA species (ug/m3)
c     cstemp   - temperatures corresponding to saturation concentrations
c                of CG/SOA species (K)
c     deltah   - enthalpy of vaporization of CG/SOA species (J/mol)
c     fpoly0   - polymerized mass fraction per hour
c     flagsoap - 1 if SOA species forms solutions; 0 if not
c-----------------------------------------------------------------------
c
      include 'soap.inc'
c
      data mwsoap   /150., 150., 130., 130., 180., 180., 210./
      data mwsopa, mwsopb /220., 220./
      data csat     /7.82, 227., 0.726, 136, 3.92, 55.8, 0.0/
      data cstemp   /7*298.0/
      data deltah   /66800.,66800.,42000.,42000.,75500.,75500.,0.0/
      data fpoly0   /0.025/
      data flagsoap /7*1/
c
      end

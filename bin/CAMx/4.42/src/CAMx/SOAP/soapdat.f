      Block Data soapdat
c
c-----------------------------------------------------------------------
c     Initialize common block variables for SOAP
c
c     mwsoap   - molecular weights of CG/SOA species (g/mol)
c     csat     - saturation concentrations of SOA species (ug/m3)
c     cstemp   - temperatures corresponding to saturation concentrations
c                of CG/SOA species (K)
c     deltah   - enthalpy of vaporization of CG/SOA species (J/mol)
c     flagsoap - 1 if SOA species forms solutions; 0 if not
c-----------------------------------------------------------------------
c
      include 'soap.com'
c
      data mwsoap   /150., 150., 150., 180., 150./
      data csat     /1.9, 56., 0.007, 0.008, 0.007/
      data cstemp   /5*298.0/
      data deltah   /156250., 156250., 0., 0., 0./
      data flagsoap /5*1/
c
      end

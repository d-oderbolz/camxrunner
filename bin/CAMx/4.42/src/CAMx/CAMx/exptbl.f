      subroutine exptbl(iunits,rxntyp,rxnord,rxnpar)
c 
c----CAMx v4.42 070603
c 
c     EXPTBL calculates a lookup table for temperature/pressure-dependent
c     rate constants using parameters from the chemistry parameters file.
c     The pressure and temperature dependance of the conversion factor from
c     cm3 molec-1 units to ppm units is accounted for.
c     The expression types supported are:
c       Type 1 Temperature independent
c       Type 2 UAM/OZIPM format Arrhenius
c       Type 3 Generalized temperature dependent
c       Type 4 Troe teperature/pressure dependent
c       Type 5 Equilibrium constant ratio to another reaction
c       Type 6 Lindemann-Hinshelwood used for OH + HNO3
c       Type 7 k = k1 + k2[M] used for OH + CO and HO2 self-reaction
c     During the model run rate constants are always interpolated from 
c     the lookup table.  The output rate constants in RKTBL have units
c     ppm-n hour-1.
c 
c     Copyright 1996-2007
c     ENVIRON International Corporation
c           
c     Modifications: 
c        09/15/04            Accept rate constants in cms units
c        10/20/05            Restructured the definition of the bins in
c                            the temperature and pressure lookup table
c 
c     Input arguments: 
c        iunits              input units for rate expressions 
c                              1 = ppm and minutes (ppm)
c                              2 = molec/cm3 and sec (cms)
c        rxntyp              rate constant expression type
c        rxnord              order of reaction
c        rxnpar              rate constant expression parameters
c 
c     Output arguments: 
c        none
c             
c     Routines Called:
c        none
c             
c     Called by: 
c        READCHM
c
      include "camx.prm"
      include "chmstry.com"
c
c-----Conversions from molecule cm-3 to ppm and from hr to min to sec
c       cf1 from gas law: (n/V) = NP/RT
c       cf1 = (6.022e23 * 1.013e5 * 1e-12 ) / (8.314 * 298.0)
c     The cf1 conversion is at 298 K and 1013 mb and the
c     adjustment "factor" below generalises to other conditions
c
      double precision darren
      double precision cf1, m2h, s2m
      parameter (cf1 = 2.462d13)
      parameter (m2h = 60.d0)
      parameter (s2m = 60.d0)
c
      double precision rxnpar(MXRXN,12)
      integer rxntyp(MXRXN), rxnord(MXRXN)
      double precision cf, cfm, rktmp, temp, ppmpres, factor
      integer i, j, k, iref
      real dtemp, dpres
c
c-----Entry point
c
      dtemp = (TEMPHI - TEMPLO)/(NTEMPR - 1)
      dpres = (PRESHI - PRESLO)/(NPRESR - 1)
      do j = 1,NTEMPR
        tempr(j) = TEMPLO + (j-1)*dtemp
      enddo
      do k = 1,NPRESR
        presr(k) = PRESLO + (k-1)*dpres
      enddo
c
c-----Fill lookup table
c
      do i = 1,nreact
        if (iunits .EQ. 1) then
          cf  = 1.d0
          cfm = 1.d0
        else
          cf  = s2m * cf1**(rxnord(i)-1)
          cfm = s2m * cf1**(rxnord(i))
        endif
        do j = 1,NTEMPR
          temp = tempr(j)
          do k = 1,NPRESR
            ppmpres = 1.d6*presr(k)/1013.d0
            factor = ( (298.d0/temp)*(presr(k)/1013.d0) )**(rxnord(i)-1)
            if (rxntyp(i).eq.1) then
              rktbl(i,j,k) = rxnpar(i,1)*cf*m2h
            elseif (rxntyp(i).eq.2) then
              rktmp = rxnpar(i,2)*(1.d0/298.d0 - 1.d0/temp)
              rktbl(i,j,k) = rxnpar(i,1)*dexp(rktmp)*cf*m2h
            elseif (rxntyp(i).eq.3) then
              rktbl(i,j,k) = darren(rxnpar(i,1),rxnpar(i,2),
     &                       rxnpar(i,3),rxnpar(i,4),temp)*cf*m2h
            elseif (rxntyp(i).eq.4) then
              rk0 = darren(rxnpar(i,1),rxnpar(i,2),
     &              rxnpar(i,3),rxnpar(i,4),temp)*cfm*ppmpres
              ratio = rk0/(darren(rxnpar(i,5),rxnpar(i,6),
     &              rxnpar(i,7),rxnpar(i,8),temp)*cf)
              rktbl(i,j,k) = 
     &          (rk0/(1.d0+ratio))*rxnpar(i,9)**(1.d0/(1.d0+
     &          (0.43429d0*log(ratio)/rxnpar(i,10))**2))*m2h
            elseif (rxntyp(i).eq.5) then
              factor = 1.d0
              iref = nint(rxnpar(i,1))
              if (iunits .EQ. 2) 
     &            cf  = cf1**(rxnord(iref)-rxnord(i))
              rktmp = darren(rxnpar(i,2),rxnpar(i,3),
     &                rxnpar(i,4),rxnpar(i,5),temp)*cf
              rktbl(i,j,k) = rktbl(iref,j,k) / rktmp 
            elseif (rxntyp(i).eq.6) then
              rk1 = darren(rxnpar(i,1),rxnpar(i,2),
     &              rxnpar(i,3),rxnpar(i,4),temp)*cf
              rk2 = darren(rxnpar(i,5),rxnpar(i,6),
     &              rxnpar(i,7),rxnpar(i,8),temp)*cf
              rk3 = darren(rxnpar(i,9),rxnpar(i,10),
     &              rxnpar(i,11),rxnpar(i,12),temp)*cfm
              rktbl(i,j,k) = ( rk1 + (rk3*ppmpres /
     &              (1.d0 + (rk3*ppmpres / rk2) ) ) )*m2h
            elseif (rxntyp(i).eq.7) then
              rk1 = darren(rxnpar(i,1),rxnpar(i,2),
     &              rxnpar(i,3),rxnpar(i,4),temp)*cf        
              rk2 = darren(rxnpar(i,5),rxnpar(i,6),
     &              rxnpar(i,7),rxnpar(i,8),temp)*cfm
              rktbl(i,j,k) = ( rk1 + rk2*ppmpres )*m2h
            endif
            rktbl(i,j,k) = rktbl(i,j,k)*factor
          enddo
        enddo
      enddo
c
      return
      end
c
      function darren(a,ea,b,tref,temp)
c
c --- Calculate a rate constant using k = A*(T/Tref)^B*exp(-Ea/T)
c
      double precision darren
      double precision a,ea,b,tref,temp
c
      darren = a*((temp/tref)**b)*dexp(-ea/temp)
c
      end

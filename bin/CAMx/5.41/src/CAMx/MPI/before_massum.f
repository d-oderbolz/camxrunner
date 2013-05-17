      subroutine before_massum(ip,icode)
      use filunit
      use grid
      use chmstry
      use camxfld
      use camxcom
      use procan
      use tracer
      use node_mod
c
c----CAMx v5.41 121109
c
c     This routine does the mass budget calculations for the 
c     slices when doing the nest feedback process. This routine
c     will do the calculations before feedback is performed and
c     store the values in temporary arrays.
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c          
c     Modifications:
c
c     Input arguments:
c        ip                  parent grid index
c        icode               flag to select when to sum mass on parent grid
c                              0 = sum both before and after aggregation
c                              1 = sum before aggregation
c                              2 = sum after aggregation
c                             >2 = do not sum
c
c     Output arguments:
c        none
c
c     Subroutine called:
c        MASSUM
c        AGGREG
c
c     Called by:
c        NESTING
c
      include "camx.prm"
c
c-----Entry point
c
      if( icode .GT. 2 ) return
c
      if (icode.le.1) then
        call massum(mmxp(ip),mmyp(ip),mmzp(ip),mi0(ip),mj0(ip),
     &             mia(ip),miz(ip),mja(ip),mjz(ip),mibcon(ip),
     &             ip,nspec,ncol(ip),nrow(ip),nlay(ip),deltax(1,ip),
     &             deltay(ip),depth(iptr3d(ip)),mapscl(iptr2d(ip)),
     &             conc(iptr4d(ip)),xmstmp(1,ip))
      endif
c
      return
      end

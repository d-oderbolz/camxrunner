      subroutine initipr(linit,iproc_id,igrid,nospec,ncol,nrow,nlay,conc)
      use chmstry
      use bndary
      use procan
      use node_mod
c
c----CAMx v5.41 121109
c
c     Loads the initial values for the Integrated Process 
c     Analysis (IPR) data for the Process Anaylsis algorithm.  
c     For each sub-domain cell, either the array element for 
c     initial value or final value (depneding on the input flag
c     LINIT) is loaded from the gridded concentration array to
c     the process analysis array.
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c     Modifications:
c        none
c
c     Input arguments:
c        igrid               grid index
c        iproc_id            process ID (MPI)
c        nospec              number of species
c        ncol                number of columns
c        nrow                number of rows
c        nlay                number of layers
c        conc                concentration field (umol/m3)
c
c     Output arguments:
c
c
c     Routines called:
c        none
c
c     Called by:
c        CAMx
c
      include "camx.prm"
c
      logical linit
      real    conc(ncol,nrow,nlay,nospec)
c
c-----Entry point
c
c  --- loop over all all species ---
c
      do ispc=1,nospec
c
c --- skip if not in this grid, otherwise get cell indexes ---
c
          do 10 icel=1,npa_cels
            if( ipanst(icel) .NE. igrid ) goto 10
            irx = ipax(icel)
            iry = ipay(icel)
            irz = ipaz(icel)
c
c   --- skip if this cell is not in the computational domain ---
c
            if( iproc_id .GT. 0 )  then
               irx = irx - mi0(igrid)
               iry = iry - mj0(igrid)
               if( irx .LT. mia(igrid) .OR. irx .GT. miz(igrid) .OR. 
     &           iry .LT. mja(igrid) .OR. iry .GT. mjz(igrid) ) goto 10
            endif
c
c  --- if initializing, load into initial position ---
c
            if (linit) then
                cipr(IPR_INIT,icel,ispc) = conc(irx,iry,irz,ispc)
c
c  --- otherwise, load into final position ---
c
            else
                cipr(IPR_FINAL,icel,ispc) = conc(irx,iry,irz,ispc)
            endif
   10     continue
c
c  --- next species ---
c
      enddo
c
      return
      end

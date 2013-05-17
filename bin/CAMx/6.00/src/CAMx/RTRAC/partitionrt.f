      subroutine partitionrt(numcol,numrow,numlay,nspc,igrd,icell,jcell,
     &                       kcell,conc,convfac)
      use filunit
      use chmstry
      use grid
      use rtracchm
      use tracer
c
c----CAMx v6.00 130506
c 
c-----------------------------------------------------------------------
c   Description:
c     This routine performs gas-aerosol partitioning of RTRAC gas species 
c     using Koa theory.
c 
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c           
c   Argument descriptions:
c     Inputs:
c       numcol  I number of columns in this slice
c       numrow  I number of rows in this slice
c       numlay  I number of layers in this slice
c       nspc    I number of host model species
c       igrd    I  umber of the grid containing the cell
c       icell   I  X index of the cell
c       jcell   I  Y index of the cell
c       kcell   I  Z index of the cell
c       conc    R  host model species concentrations (ppm)
c       convfac R  conversion factor: umol/m3 = ppm * convfac
c
c     Routines called: 
c             
c     Called by: 
c        CHEMDRIV
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     10/14/09  --jjung--   Original development
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c 
      implicit none
c     include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer numcol
      integer numrow
      integer numlay
      integer nspc
      integer igrd
      integer icell
      integer jcell
      integer kcell
      real    conc(nspc)
      real    convfac
      real    eps
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   i
      integer   ispc
      integer   idx
      integer   idxaer
      integer   idxcel

      real daero
      real Mom,fom,Kp,TSP
      real MwH2O,Mw,gas,aero
      real total
c
      data MwH2O /18.0/   !Mw of H2O (g/mol)
      data eps /1.0e-20/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- Calculate array pointers for the current grid cell ---
c
      idxcel =  ipsa3d(igrd)-1 + icell + numcol*(jcell-1) +
     &                              numcol*numrow*(kcell-1)
c
c --- Calculate organic and total PM mass, and OM/PM ratio
c
      TSP = 0.0
      do ispc = ngas+1,nspc
         TSP = TSP + conc(ispc)
      enddo
      Mom = conc(ksoa1) + conc(ksoa2) + conc(ksoa3) + conc(ksoa4) +
     &      conc(ksoa5) + conc(ksoa6) + conc(ksoa7) + conc(ksopa) +
     &      conc(ksopb) + conc(kpoa)
      if (TSP.gt.0.0) then
         fom = Mom/TSP
      else
         TSP = eps
         fom = 0.2 ! Set 20%
      endif
      if (fom.lt.1.e-5) fom = 1.e-5 ! Bound fom bigger than 0.001%
c
c-----Loop over GAS species, and calculate partitioning for this cell
c
      do i = 1,nrtgas
         idx    = idxcel + numcol*numrow*numlay*(i-1)
         idxaer = idxcel + numcol*numrow*numlay*(nrtgas+i-1)
         Mw = MwH2O*rtdrate(i)**2.
         gas = ptconc(idx)*convfac*Mw                   ! ppm -> ug/m3
         Kp = 1.0e-9/820.0*eqkoa(i)*fom
         total = gas + ptconc(idxaer)
         aero = total*Kp*TSP/(Kp*TSP+1.)
         daero = aero - ptconc(idxaer)
         if (daero.ge.0.0) then                         ! aerosol increase
            if (daero.ge.gas) then                      ! Negative gas
              write(iout,*)'Error in PARTITIONRT:',
     &                     ' Partitioning more than available gas.'
              write(iout,*)'Available gas (ug/m3)',gas
              write(iout,*)'Mass subtracted (ug/m3)',daero
              call camxerr() 
            endif
         else                                           ! aerosol decrease
            if (abs(daero).ge.ptconc(idxaer)) then      ! Negative aerosol
              write(iout,*)'Error in PARTITIONRT:',
     &                     ' Partitioning more than available aerosol.'
              write(iout,*)'Available aerosol (ug/m3)',ptconc(idxaer)
              write(iout,*)'Mass subtracted (ug/m3)',abs(daero)
              write(iout,*)'gas,ptconc(idxaer),Kp,TSP,eqkoa,fom'
              write(iout,*)gas,ptconc(idxaer),Kp,TSP,eqkoa(i),fom
              call camxerr() 
            endif
         endif
         ptconc(idx)    = ptconc(idx)    - daero/(convfac*Mw) ! Update gas conc
         ptconc(idxaer) = ptconc(idxaer) + daero              ! Update aerosol conc
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end

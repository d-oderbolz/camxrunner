      subroutine fgavrg(iproc_id,igrd)
      use grid
      use chmstry
      use camxfld
      use camxcom
      use pigsty
      use procan
      use rtracchm
      use tracer
      use node_mod
c
c----CAMx v5.30 101223
c
c     FGAVRG passes arrays from common blocks to AVERAGE
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c  
c     Modifications:
c        11/10/03  Added RTRAC/PiG sampling grid
c        07/29/05  Added sampling grid for regular model species
c        12/15/08  Added code to handle averaging of radicals
c        07/16/07 -bkoo-     Added check for HDDM
c        07/16/08 -bkoo-     Added DDM turn-off flag
c        09/27/10 --gwilson-- the calculation for process analysis 
c                             conversion factors is now done by a separate
c                             routine
c
c
c     Input arguments: 
c       iproc_id    process ID for this slice (MPI)
c       igrd        current grid index
c
c     Output arguments: 
c        none
c      
c     Routines called: 
c        AVERAGE
c      
c     Called by: 
c        NESTING
c
      include 'camx.prm'
      include 'flags.inc'
c
c-----Entry point
c
      integer nlayav
c
c-----Update cumulative time for this fine grid 
c
      nlayav = nlay(igrd)
      if( .NOT. l3davg(igrd) ) nlayav = 1

      call average(mxp,myp,mzp,.FALSE.,igrd,deltat(igrd)/2.0,
     &             ncol(igrd),nrow(igrd),nlay(igrd),nlayav,
     &             navspc,nspec,nrad,lavmap,lradmap,lgas,
     &             tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &             conc(iptr4d(igrd)),cncrad(iptrad(igrd)),
     &             avcnc(iptrav(igrd)) )
c
c========================= Process Analysis Begin ==============================
c
c  --- call routine to calculate conversion factors for PA
c
      if( lipr ) then
          call paconv(mxp,myp,mzp,deltat(igrd)/2.0,ncol(igrd),
     &                nrow(igrd),nlay(igrd),navspc,nspec,lavmap,
     &                lradmap,lgas,tempk(iptr3d(igrd)),
     &                press(iptr3d(igrd)),ipacl_3d(iptr3d_full(igrd)) )
      endif
c
c========================= Process Analysis End ==============================
c

c
c-----Update PiG contribution
c
      if (ipigflg .NE. 0 .and. LVISPIG) then
         call avepig(iproc_id,igrd,deltat(igrd)/2.0,mxp,myp,mzp,
     &               i0,j0,ncol(igrd),nrow(igrd),nlay(igrd),nlayav,
     &               deltax(1,igrd),deltay(igrd),mapscl(iptr2d(igrd)),
     &               height(iptr3d_full(igrd)),
     &               navspc,nspec,lavmap,
     &               tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &               avcnc(iptrav(igrd)))
      endif
c
c-----Update running average of average specs on sampling grids
c
      if (lsample) then
        do lgrd = 1,nsample
          if (ismpgrd(lgrd).eq.igrd) then
            call pigsampl(mxp,myp,mzp,i0,j0,.FALSE.,lgrd,ngrid,nspec,
     &                    navspc,ncolsmp(lgrd),nrowsmp(lgrd),ncol(igrd),
     &                    nrow(igrd),nlay(igrd),meshold(igrd),
     &                    inst1(igrd),jnst1(igrd),deltat(igrd)/2.0,
     &                    delx,dely,deltax(1,igrd),deltay(igrd),
     &                    height(iptr3d_full(igrd)),tempk(iptr3d(igrd)),
     &                    press(iptr3d(igrd)),conc(iptr4d(igrd)),
     &                    smpcnc(ipsmp(lgrd)))
          endif
        enddo
      endif
c
c======================== Source Apportion Begin =======================
c
c   --- call routine to update the running averages ---
c
      if( ltrace .OR. ((lddm .OR. lhddm) .AND. lddmcalc(igrd)) ) then
         call average(mxp,myp,mzp,.TRUE.,igrd,deltat(igrd)/2.0,
     &                ncol(igrd),nrow(igrd),nlay(igrd),1,
     &                ntotsp,ntotsp,nrad,lsamap,lmaprad,lsagas,
     &                tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &                ptconc(ipsa3d(igrd)),cncrad(iptrad(igrd)),
     &                ptavrg(ipsa2d(igrd)) )
c
         if ((tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) .AND.
     &        lsample .AND. lsmptrc ) then
            do lgrd = 1,nsample
              if (ismpgrd(lgrd).eq.igrd) then
                call pigsampl(mxp,myp,mzp,i0,j0,.TRUE.,lgrd,ngrid,nrtrac,
     &                        nrtrac,ncolsmp(lgrd),nrowsmp(lgrd),
     &                        ncol(igrd),nrow(igrd),nlay(igrd),
     &                        meshold(igrd),inst1(igrd),jnst1(igrd),
     &                        deltat(igrd)/2.0,delx,dely,deltax(1,igrd),
     &                        deltay(igrd),height(iptr3d_full(igrd)),
     &                        tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &                        ptconc(ipsa3d(igrd)),rtsmpcnc(iprtsmp(lgrd)))
              endif
            enddo
         endif
      endif
c
c========================= Source Apportion End ========================
c
      return
      end

      subroutine avgall(iproc_id)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use camxcom
      use grid
      use camxfld
      use chmstry
      use pigsty
      use tracer
      use rtracchm
      use procan
      use node_mod
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c         Calls all of the routines that perform the averaging for 
c         the various concentation fields: regular model, probing tools,
c         probing tool receptors and PiG sampling
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c       iproc_id    process ID for this slice (MPI)
c     Output:  
c
c    Called by:
c       CAMX
c    Subroutines called:
c       NEWGRID
c       AVERAGE
c       AVEPIG
c       PIGSAMPL
c       AVGWAL
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     09/27/10 --gwilson-- the calculation for process analysis
c                          conversion factors is now done by a separate
c                          routine
c     11/06/12 --gwilson-- fixed Wall of Cells receptors for MPI
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      implicit none
      include 'camx.prm'
      include 'flags.inc'
c
c-----------------------------------------------------------------------
c    Argument delarations:
c-----------------------------------------------------------------------
c
      integer :: iproc_id
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: igrd
      integer :: nlayav
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      call newgrid(1)
      nlayav = nlay(1)
      if( .NOT. l3davg(1) ) nlayav = 1
      call average(mxp,myp,mzp,.FALSE.,1,deltat(1)/2.0,
     &             ncol(1),nrow(1),nlay(1),nlayav,navspc,nspec,
     &                lavmap,lgas,tempk(1),press(1),conc(1),avcnc(1) )
c
c========================= Process Analysis Begin ==============================
c
c  --- call routine to calculate conversion factors for PA
c
      if( lipr ) then
          call paconv(mxp,myp,mzp,deltat(1)/2.0,
     &             ncol(1),nrow(1),nlay(1),navspc,nspec,lavmap,
     &                           lgas,tempk(1),press(1),ipacl_3d(1))
      endif
c
c========================= Process Analysis End ==============================
c
c  --- Add PiG masses to average ---
c
      if (ipigflg .NE. 0 .AND. LVISPIG) then
         call avepig(iproc_id,1,deltat(1)/2.0,mxp,myp,mzp,i0,j0,
     &               ncol(1),nrow(1),nlay(1),nlayav,deltax(1,1),
     &               deltay(1),mapscl(1),height(1),navspc,nspec,
     &                               lavmap,tempk(1),press(1),avcnc(1))
      endif
c
c  --- Update running average of average specs on sampling grids ---
c
      if (lsample) then
         do igrd = 1,nsample
            if( ismpgrd(igrd) .EQ. 1 ) then
               call pigsampl(mxp,myp,mzp,i0,j0,.FALSE.,igrd,ngrid,nspec,
     &                       navspc,ncolsmp(igrd),nrowsmp(igrd),ncol(1),
     &                       nrow(1),nlay(1),meshold(1),inst1(1),
     &                       jnst1(1),deltat(1)/2.0,delx,dely,
     &                       deltax(1,1),deltay(1),height(1),tempk(1),
     &                       press(1),conc(1),smpcnc(ipsmp(igrd)) )
            endif
         enddo
      endif
c
c======================== Source Apportion Begin =======================
c
c  --- call routine to update the running averages ---
c
      if( ltrace .OR. lddm .OR. lhddm ) then
         if( ltrace .OR. lddmcalc(1) ) then
         call average(mxp,myp,mzp,.TRUE.,1,deltat(1)/2.0,
     &                ncol(1),nrow(1),nlay(1),1,ntotsp,ntotsp,
     &                      lsamap,lsagas,tempk(1),press(1),
     &                                       ptconc(1),ptavrg(1) )
         endif
c
c  --- if WALL OF CELLS receptors exist, add averages ---
c
         if( lwalls ) then
            do igrd=1,ngrid
               if( .NOT.( ltrace .OR. lddmcalc(igrd) ) ) cycle
               call newgrid(igrd)
               call avgwal(igrd,mxp,myp,mzp,
     &                     ncol(igrd),i0,j0,ntotsp,deltat(igrd)/2.0,
     &                     deltax(1,igrd),deltay(igrd),depth(iptr3d(igrd)),
     &                     mapscl(iptr2d(igrd)),tempk(iptr3d(igrd)),
     &                     press(iptr3d(igrd)),ptconc(ipsa3d(igrd))     )
            enddo
         endif
c
c  --- Update running average on sampling grids ---
c
         if ((tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) .AND.
     &        lsample .AND. lsmptrc) then
            do igrd = 1,nsample 
               if( ismpgrd(igrd) .EQ. 1 ) then
                  call pigsampl(mxp,myp,mzp,i0,j0,.TRUE.,igrd,ngrid,nrtrac,
     &                          nrtrac,ncolsmp(igrd),nrowsmp(igrd),ncol(1),
     &                          nrow(1),nlay(1),meshold(1),inst1(1),
     &                          jnst1(1),deltat(1)/2.0,delx,dely,
     &                          deltax(1,1),deltay(1),height(1),tempk(1),
     &                          press(1),ptconc(1),rtsmpcnc(iprtsmp(igrd)) )
               endif
            enddo
         endif
      endif
c
c========================= Source Apportion End ========================
c
      return
      end

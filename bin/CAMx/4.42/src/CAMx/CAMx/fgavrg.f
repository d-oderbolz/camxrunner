      subroutine fgavrg(igrd)
c
c----CAMx v4.42 070603
c
c     FGAVRG passes arrays from common blocks to AVERAGE
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c  
c     Modifications:
c        11/10/03  Added RTRAC/PiG sampling grid
c        07/29/05  Added sampling grid for regular model species
c
c     Input arguments: 
c        igrd                current grid index
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
      include 'camx.com'
      include 'camxfld.com'
      include 'grid.com'
      include 'chmstry.com'
      include 'flags.com'
      include 'pigsty.com'
c
c======================== Source Apportion Begin =======================
c
      include 'tracer.com'
      include 'rtracchm.com'
c
c========================= Source Apportion End ========================
c
c
c======================== Process Analysis Begin =======================
c
      include 'procan.com'
c
c========================= Process Analysis End ========================
c
c-----Entry point
c
c-----Update cumulative time for this fine grid 
c
      call average(.FALSE.,igrd,deltat(igrd)/2.0,
     &             ncol(igrd),nrow(igrd),nlay(igrd),nlay(igrd),
     &             navspc,nspec,lavmap,lgas,tempk(iptr3d(igrd)),
     &             press(iptr3d(igrd)),conc(iptr4d(igrd)),
     &             avcnc(iptr4d(igrd)),ipacl_3d(iptr3d(igrd)) )
c
c-----Update PiG contribution
c
      if (ipigflg .NE. 0 .and. LVISPIG) then
         call avepig(igrd,deltat(igrd)/2.0,ncol(igrd),nrow(igrd),
     &               nlay(igrd),deltax(1,igrd),deltay(igrd),
     &               mapscl(iptr2d(igrd)),height(iptr3d(igrd)),navspc,
     &               nspec,lavmap,tempk(iptr3d(igrd)),
     &               press(iptr3d(igrd)),avcnc(iptr4d(igrd)))
      endif
c
c-----Update running average of average specs on sampling grids
c
      if (lsample) then
        do lgrd = 1,nsample
          if (ismpgrd(lgrd).eq.igrd) then
            call pigsampl(.false.,lgrd,nspec,navspc,ncols(lgrd),
     &                    nrows(lgrd),ncol(igrd),nrow(igrd),nlay(igrd),
     &                    meshold(igrd),inst1(igrd),jnst1(igrd),
     &                    deltat(igrd)/2.0,delx,dely,
     &                    deltax(1,igrd),deltay(igrd),
     &                    height(iptr3d(igrd)),
     &                    tempk(iptr3d(igrd)),
     &                    press(iptr3d(igrd)),
     &                    conc(iptr4d(igrd)),
     &                    smpcnc(ipsmp(lgrd)))
          endif
        enddo
      endif
c
c======================== Source Apportion Begin =======================
c
c   --- call routine to update the running averages ---
c
      if( ltrace .OR. lddm ) then
         call average(.TRUE.,igrd,deltat(igrd)/2.0,
     &                ncol(igrd),nrow(igrd),nlay(igrd),1,
     &                ntotsp,ntotsp,lsamap,lsagas,tempk(iptr3d(igrd)),
     &                press(iptr3d(igrd)),ptconc(ipsa3d(igrd)),
     &                ptavrg(ipsa2d(igrd)),ipacl_3d(iptr3d(igrd)))
c
         if (tectyp .EQ. RTRAC .AND. lsample .AND. lsmptrc) then
            do lgrd = 1,nsample
              if (ismpgrd(lgrd).eq.igrd) then
                call pigsampl(.true.,lgrd,nrtrac,nrtrac,ncols(lgrd),
     &                        nrows(lgrd),ncol(igrd),nrow(igrd),
     &                        nlay(igrd),meshold(igrd),inst1(igrd),
     &                        jnst1(igrd),deltat(igrd)/2.0,delx,
     &                        dely,deltax(1,igrd),deltay(igrd),
     &                        height(iptr3d(igrd)),
     &                        tempk(iptr3d(igrd)),
     &                        press(iptr3d(igrd)),
     &                        ptconc(ipsa3d(igrd)),
     &                        rtsmpcnc(iprtsmp(lgrd)))
              endif
            enddo
         endif
      endif
c
c========================= Source Apportion End ========================
c
      return
      end

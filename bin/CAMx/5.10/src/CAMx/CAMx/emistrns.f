      subroutine emistrns(igrd,chtime,chdate,numprocs,iproc_id)
      use filunit
      use grid
      use grid_nodes
      use chmstry
      use bndary
      use ahomap
      use camxfld
      use camxcom
      use pigsty
      use ptemiss
      use procan
      use rtracchm
      use rtcmcchm
      use tracer
c
      use master_mod
      use node_mod 
c
c----CAMx v5.10 090918
c
c     EMISTRNS performs the following tasks for one grid:
c        1.  Injects new PiG puffs and moves all puffs
c        2.  Determines mass-conserving vertical velocity parameters
c        3.  Updates concentrations due to emissions
c        4.  Performs 3-D transport
c        5.  Performs wet scavenging 
c        6.  Updates met fields to current time step
c        7.  Performs 3-D diffusion
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c            
c     Modifications:  
c        01/30/02   Added code for RTRAC probing tool
c        10/12/04   Water vapor and vertical diffusivity fields are now
c                   time-interpolated
c        10/12/04   PIGINIT and PIGWALK moved into this routine, and
c                   IRONDRIV+GRESDRIV moved out to NESTING and CAMx
c        05/07/07   Added calls to subroutines to update Probing Tools
c                   boundary conditions after met is updated
c       03/15/09    Added code for deposition output for tracers
c       07/16/07 -bkoo-     Added check for HDDM
c       07/16/08 -bkoo-     Added DDM turn-off flag
c
c     Input arguments:
c        igrd                grid index
c        chtime              simulation time ('HH:MM:SS')
c        chdate              simulation date ('YY/MM/DD')
c
c     Output arguments:
c        none
c
c     Routines called:
c        PIGINIT
c        PIGWALK
c        ZRATES
c        EMISS
c        XYADVEC
c        ZADVEC
c        WETDEP
c        UPDTMET
c        DIFFUS
c
c     Called by:
c        CAMx
c        NESTING
c
      include 'camx.prm'
      include 'flags.com'
      include 'tracerpts.com'
      include 'mpif.h'
c
      integer     ierr
      character*8 chtime, chdate
      integer     numprocs, iproc_id
      integer     ncola, nrowa  
c
c======================== Source Apportion Begin =======================
c
      real*8 ardum(MXTRSP)
      real*8 ptdum(MXTRSP)
      real*8 fluxdum(MXTRSP*11)
      real*8 depdum(MXCELLS,MXCELLS,3*MXSPEC)
      real adjwest(MXCELLS,MXLAYER)
      real adjeast(MXCELLS,MXLAYER)
      real adjsouth(MXCELLS,MXLAYER)
      real adjnorth(MXCELLS,MXLAYER)
c
      common /comemistrns/ depdum, ptdum, ardum, fluxdum
c
c========================= Source Apportion End ========================
c
c-----Entry point
c
      if( .NOT. lmpi .OR. (iproc_id .eq. 1) ) then
         write(*,'(/,a,i3)')           ' Processing grid: ',igrd
         write(*,'(a,f7.1)')           '    Timestep (s): ',deltat(igrd)
         write(*,'(a,2x,a8,1X,a8,/)')  '  Grid Date/Time: ',
     &                                                     chdate,chtime
         call flush(6)
      endif
      if( .not. LMPI .or. (iproc_id .gt. 0) ) then  !cbwmpi
         write(iout,'(/,a,i3)')          ' Processing grid: ',igrd
         write(iout,'(a,f7.1)')          '        Timestep: ',
     &                                                     deltat(igrd)
         write(iout,'(a,2x,a8,1x,a8,/)') '  Grid Date/Time: ',
     &                                                     chdate,chtime
      endif
c
c-----Initialize new pig puffs
c
      if (ipigflg .NE. 0 .AND. ipigint .EQ. igrd) then
        if( iproc_id .eq. 0 ) then
           if( .NOT. lmpi ) write(*,'(a20,$)') 'piginit ......'
           write(iout,'(a20,$)') 'piginit ......'
           call piginit(deltat(igrd),nptsrc,nspec,ptemis,height,windu,
     &                                              windv,tempk,press)
           tcpu = dtime(tarray2)
           if( .NOT. lmpi ) write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
           call flush(6)
           write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
           call flush(iout)
c
c-----Update PiG locations 
c
           if( .NOT. lmpi ) write(*,'(a20,$)') 'pigwalk ......'
           write(iout,'(a20,$)') 'pigwalk ......'
           call pigwalk(deltat(igrd))
           tcpu = dtime(tarray2)
           if( .NOT. lmpi ) write(*,'(a,f10.3)') 
     &                                   '   CPU = ', tarray2(1)
           call flush(6)
           write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
           call flush(iout)
        end if 
c
        if( lmpi ) call nodes_met_pig(numprocs,iproc_id)
c       
      endif
c
c-----Inject emissions
c
      if( .NOT. lmpi .OR. (iproc_id .EQ. 1) ) then
        write(*,'(a20,$)') 'emiss ......'
      endif
      if( .NOT. lmpi .OR. (iproc_id .GT. 0) ) then
        write(iout,'(a20,$)') 'emiss ......'
        tcpu = dtime(tarray2)
        call emiss(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &           igrd,nspec,larmap(1,igrd),
     &           lptmap,nosrc(igrd),idsrc(1,igrd),isrc(1,igrd),
     &           jsrc(1,igrd),ncol(igrd),nrow(igrd),
     &           nlay(igrd),
     &           deltat(igrd),deltax(1,igrd),deltay(igrd),
     &           mapscl(iptr2d(igrd)),
     &           height(iptr3d_full(igrd)),depth(iptr3d_full(igrd)),
     &           windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &           tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &           aremis(iptrem(igrd)),ptemis,MAX(nptsrc,1),
     &           armass(1,igrd),ptmass(1,igrd),
     &           conc(iptr4d(igrd)),ipacl_3d(iptr3d_full(igrd)))
c
c======================== Source Apportion Begin =======================
c
c  --- call routine with tracer species arrays, send dummy
c      arguemnts for the total mass arrays ---
c
c
        if( ltrace .OR. ((lddm .OR. lhddm) .AND. lddmcalc(igrd)) ) then
           call emiss(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &              igrd,ntotsp,lsamap,lsamap,
     &              nosrc(igrd),idsrc(1,igrd),isrc(1,igrd),jsrc(1,igrd),
     &              ncol(igrd),nrow(igrd),nlay(igrd),deltat(igrd),
     &              deltax(1,igrd),deltay(igrd),mapscl(iptr2d(igrd)),
     &              height(iptr3d_full(igrd)),
     &              depth(iptr3d_full(igrd)),windu(iptr3d(igrd)),
     &              windv(iptr3d(igrd)),
     &              tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &              saemis(ipsa2d(igrd)),sapnts,MAX(MXPTSRC,1),
     &              ardum,ptdum,ptconc(ipsa3d(igrd)),
     &              ipacl_3d(iptr3d_full(igrd)) )
        endif
c
c========================= Source Apportion End ========================
c
        tcpu = dtime(tarray2)

        if( .NOT. lmpi .OR. (iproc_id .EQ. 1) ) then
          write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
          call flush(6)
        endif

        write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
        call flush(iout)
c
c-----Determine mass-conserving vertical velocity parameters
c
        if( .NOT. lmpi .OR. (iproc_id .EQ. 1) ) then
          write(*,'(a20,$)') 'zrates ......'
        endif
        write(iout,'(a20,$)') 'zrates ......'
      endif

      if( iproc_id .gt. 0 ) then
          call node_send_lbc(iproc_id, nspec,igrd)
          if( ltrace .OR. lddm .OR. lhddm )
     &        call node_send_lbc_pt(iproc_id, ntotsp, igrd)
      endif
      if( .NOT. lmpi .OR. (iproc_id .GT. 0) ) then
        call zrates(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &            igrd,xyordr,ncol(igrd),nrow(igrd),nlay(igrd),
     &            nadv(1,igrd),deltat(igrd),deltax(1,igrd),
     &            deltay(igrd),depth(iptr3d_full(igrd)),
     &            phpt(iptr3d_full(igrd)),pppt(iptr3d(igrd)),
     &            ptpt(iptr3d(igrd)),windu(iptr3d(igrd)),
     &            windv(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &            press(iptr3d(igrd)),mapscl(iptr2d(igrd)),
     &            dilut(iptr3d(igrd)),entrn(iptr3d(igrd)) )
      endif
      if( iproc_id .GT. 0 ) then
          call node_get_lbc(iproc_id, nspec,igrd)
          if( ltrace .OR. lddm .OR. lhddm )
     &         call node_get_lbc_pt(iproc_id, ntotsp, igrd)
      endif
      if( .NOT. lmpi .OR. (iproc_id .GT. 0) ) then
          tcpu = dtime(tarray2)
          if( .NOT. lmpi .OR. (iproc_id .EQ. 1) ) then
            write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
            call flush(6)
          endif
          write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
          call flush(iout)
c
c-----Perform 2-D transport
c
          call xyadvec(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &             igrd,xyordr,ncol(igrd),nrow(igrd),
     &             nlay(igrd),nspec,nadv(1,igrd),
     &             deltat(igrd),deltax(1,igrd),deltay(igrd),
     &             windu(iptr3d(igrd)),
     &             windv(iptr3d(igrd)),depth(iptr3d_full(igrd)),
     &             mapscl(iptr2d(igrd)),conc(iptr4d(igrd)),
     &             fluxes(1,igrd),tarray2,ipsa3d(igrd),
     &             ipacl_3d(iptr3d_full(igrd)),iproc_id )
          call flush(6)
          call flush(iout)
c
c-----Perform vertical transport
c
          call zadvec(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &             .FALSE.,igrd,ncol(igrd),nrow(igrd),
     &             nlay(igrd),nspec,
     &             MAX(1,ntotsp),deltat(igrd),deltax(1,igrd),
     &             deltay(igrd),
     &             densfac,idfin(iptr2d(igrd)),caloft,
     &             depth(iptr3d_full(igrd)),entrn(iptr3d(igrd)),
     &             dilut(iptr3d(igrd)),
     &             tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &             spname,conc(iptr4d(igrd)),
     &             fluxes(1,igrd),ptloft,ipsa3d(igrd),
     &             tarray2,ipacl_2d(iptr2d_full(igrd)),
     &             ipacl_3d(iptr3d_full(igrd)),iproc_id )
          call flush(6)
          call flush(iout)
c
c======================== Source Apportion Begin =======================
c
          if( ltrace ) then
c
              if( tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC ) then
                 call xyadvec(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &             igrd,xyordr,ncol(igrd),nrow(igrd),nlay(igrd),
     &             ntotsp,nadv(1,igrd),deltat(igrd),deltax(1,igrd),
     &             deltay(igrd),windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &             depth(iptr3d_full(igrd)),mapscl(iptr2d(igrd)),
     &             ptconc(ipsa3d(igrd)),fluxdum,tarray2,ipsa3d(igrd),
     &             ipacl_3d(iptr3d_full(igrd)),iproc_id )
              endif
c
c----- call routine to advect the timing tracers horizontally ---
c
             if( npttim .GT. 0 ) call timadv(mxp,myp,mzp,i0,j0,ia,
     &             iz,ja,jz,ibcon,
     &             igrd,xyordr,ncol(igrd),
     &             nrow(igrd),nlay(igrd),nspec,
     &             deltat(igrd),deltax(1,igrd),deltay(igrd),
     &             windu(iptr3d(igrd)),
     &             windv(iptr3d(igrd)),depth(iptr3d_full(igrd)),
     &             mapscl(iptr2d(igrd)),ptconc(ipsa3d(igrd)))
c
c  --- call routine with tracer species arrays, send dummy
c      arguments for the total mass arrays ---
c
             call zadvec(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &                .TRUE.,igrd,ncol(igrd),nrow(igrd),nlay(igrd),
     &                ntotsp,1,deltat(igrd),deltax(1,igrd),deltay(igrd),
     &                densfac,idfin(iptr2d(igrd)),ptloft,
     &                depth(iptr3d_full(igrd)),entrn(iptr3d(igrd)),
     &                dilut(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &                press(iptr3d(igrd)),ptname, ptconc(ipsa3d(igrd)),
     &                fluxdum,ptloft,ipsa3d(igrd),tarray2,
     &                ipacl_2d(iptr2d_full(igrd)),
     &                          ipacl_3d(iptr3d_full(igrd)),iproc_id )
          endif
      endif
c
c========================= Source Apportion End ========================
c
c-----Update vertical grid and environmental fields for this timestep
c
      if( .NOT. lmpi .OR. (iproc_id .EQ. 1) ) then
         write(*,'(a20,$)') 'updtmet ......'
      endif
      write(iout,'(a20,$)') 'updtmet ......'
      call updtmet(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &             igrd,ncol(igrd),nrow(igrd),nlay(igrd),
     &             nspec,ngas,ntotsp,densfac,deltat(igrd),
     &             phpt(iptr3d_full(igrd)),
     &             height(iptr3d_full(igrd)),
     &             depth(iptr3d_full(igrd)),
     &             pppt(iptr3d(igrd)),press(iptr3d(igrd)),
     &             pupt(iptr3d(igrd)),windu(iptr3d(igrd)),
     &             pvpt(iptr3d(igrd)),
     &             windv(iptr3d(igrd)),pspt(iptr2d(igrd)),
     &             tsurf(iptr2d(igrd)),
     &             ptpt(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &             pwpt(iptr3d(igrd)),water(iptr3d(igrd)),
     &             pkpt(iptr3d(igrd)),rkv(iptr3d(igrd)),
     &             conc(iptr4d(igrd)),ptconc(ipsa3d(igrd)),
     &             adjwest,adjeast,adjsouth,adjnorth)
      call srfruf(mxp,myp,mzp,ncol(igrd),nrow(igrd),nlay(igrd),
     &            datec(igrd),cellat(iptr2d(igrd)),cellon(iptr2d(igrd)),
     &            windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &            fsurf(iptrlu(igrd)),lrdruf(igrd),icdruf(iptr2d(igrd)),
     &            ruflen(1),lrdsno,icdsno(iptr2d(igrd)),
     &            sfcz0(iptr2d(igrd)))
      tcpu = dtime(tarray2)

      if( .NOT. lmpi .OR. (iproc_id .EQ. 1) ) then
         write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
      endif
      write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
      call flush(6)
      call flush(iout)
c
c-----Calculate dry deposition rates
c
      if( .NOT. lmpi .OR. (iproc_id .GT. 0) ) then
          if (ldry) then
            if( .NOT. lmpi .OR. iproc_id .EQ. 1 ) then
               write(*,'(a20,$)') 'drydep ......'
            endif
            write(iout,'(a20,$)') 'drydep ......'
            call drydep(mxp,myp,mzp,i0,j0,
     &               ncol(igrd),nrow(igrd),nlay(igrd),
     &               itzon,tsurf(iptr2d(igrd)),cellat(iptr2d(igrd)),
     &               cellon(iptr2d(igrd)),pwr(iptr3d(igrd)),
     &               cwc(iptr3d(igrd)),height(iptr3d_full(igrd)),
     &               press(iptr3d(igrd)),
     &               windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &               fcloud(iptr3d(igrd)),cldtrns(iptr3d(igrd)),
     &               water(iptr3d(igrd)),fsurf(iptrlu(igrd)),
     &               tempk(iptr3d(igrd)),lrdruf(igrd),
     &               icdruf(iptr2d(igrd)),ruflen(1),lrddrt(igrd),
     &               icddrt(iptr2d(igrd)),lrdsno,icdsno(iptr2d(igrd)),
     &               conc(iptr4d(igrd)),vdep(iptrem(igrd)))
            call depsmry(igrd,mxp,myp,nspec,vdep(iptrem(igrd)) )
c
c======================== Source Apportion Begin =======================
c
            if( ltrace .AND. tectyp .EQ. RTRAC ) then
               call drydeprt(mxp,myp,mzp,i0,j0,
     &                  ncol(igrd),nrow(igrd),nlay(igrd),nrtrac,itzon,                 
     &                  tsurf(iptr2d(igrd)),cellat(iptr2d(igrd)),
     &                  cellon(iptr2d(igrd)),pwr(iptr3d(igrd)),
     &                  cwc(iptr3d(igrd)),height(iptr3d_full(igrd)),
     &                  press(iptr3d(igrd)),
     &                  windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &                  fcloud(iptr3d(igrd)),cldtrns(iptr3d(igrd)),
     &                  water(iptr3d(igrd)),fsurf(iptrlu(igrd)),
     &                  tempk(iptr3d(igrd)),lrdruf(igrd),
     &                  icdruf(iptr2d(igrd)),ruflen(1),lrddrt(igrd),
     &                  icddrt(iptr2d(igrd)),lrdsno,
     &                  icdsno(iptr2d(igrd)),vdeprt(ipsa2d(igrd)))
            endif
c
c========================= Source Apportion End ========================
c
            tcpu = dtime(tarray2)
            if( .NOT. lmpi .OR. iproc_id .EQ. 1 ) then
              write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
            endif
            write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
            call flush(6)
            call flush(iout)
          else
            call zeros(vdep(iptrem(igrd)),mxp*myp*nspec)
          endif
c
c========================= Source Apportion Begin ========================
c
c
c  --- if this is the master grid, update the boundary
c      conditions for the tracer species ---
c
          if( ltrace .and. igrd .EQ. 1 .AND. tectyp .NE. RTRAC .AND.
     &                                       tectyp .NE. RTCMC ) then
             call clrbdysa(1,mmxp(1),mmyp(1),mmzp(1),ntotsp,ptconc(1))
             call filbdysa(1,mmxp(1),mmyp(1),mmzp(1),nspec,ntotsp,
     &                                                  conc(1),ptconc(1))
          endif
c
c======================== DDM Begin =======================
c
      if( (lddm.OR.lhddm) .and. nbcddm .GT. 0 .and. igrd .EQ. 1 ) then
          call adjbcddm(adjwest,adjeast,adjsouth,adjnorth,mmxp(1),
     &                              mmyp(1),mmzp(1),ntotsp,ptconc(1))
      endif
c
c======================== DDM End =======================
c
c
c========================= Source Apportion End ========================
c
c-----Perform 3-D Diffusion
c
          call diffus(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,.FALSE.,
     &          igrd,ncol(igrd),nrow(igrd),nlay(igrd),nspec,ndepspc*3,
     &          deltat(igrd),deltax(1,igrd),deltay(igrd),
     &          idfin(iptr2d(igrd)),vdep(iptrem(igrd)),
     &          rkx(iptr3d(igrd)),rky(iptr3d(igrd)),
     &          rkv(iptr3d(igrd)),depth(iptr3d_full(igrd)),
     &          tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &          windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &          water(iptr3d(igrd)),cwc(iptr3d(igrd)),
     &          tsurf(iptr2d(igrd)),mapscl(iptr2d(igrd)),
     &          sfcz0(iptr2d(igrd)),conc(iptr4d(igrd)),
     &          fluxes(1,igrd),depfld(iptrdp(igrd)),
     &          ipsa3d(igrd),tarray2,iproc_id,
     &          ipacl_2d(iptr2d_full(igrd)),
     &          ipacl_3d(iptr3d_full(igrd)),
     &          '  z diffusion ......','     x/y diff ......')
          call flush(6)
          call flush(iout)
c
c======================== Source Apportion Begin =======================
c
          if( ltrace ) then
c
c   --- call routine to calculate the depostion velocities 
c       for the tracer species ---
c
             mxp_rt = 1
             myp_rt = 1
             nsp_rt = 1
             if( tectyp .EQ. 'RTRAC' .OR. tectyp .EQ. 'RTCMC' ) then
                mxp_rt = mxp
                myp_rt = myp
                nsp_rt = ntotsp
             endif
             if( ldry ) call filvdsa(mxp,myp,mzp,nspec,ntotsp,
     &                     mxp_rt,myp_rt,nsp_rt,conc(iptr4d(igrd)),
     &                                     vdep(iptrem(igrd)),ptvdep,
     &                                             vdeprt(ipsa2d(igrd)))
c
c  --- call routine with tracer species arrays, send dummy
c      arguments for the total mass arrays ---
c
               if( lptdepout ) then
                   call diffus(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &                  .TRUE.,igrd,ncol(igrd),nrow(igrd),nlay(igrd),
     &                  ntotsp,ndepspc*3,deltat(igrd),deltax(1,igrd),
     &                  deltay(igrd),idfin(iptr2d(igrd)),ptvdep,
     &                  rkx(iptr3d(igrd)),rky(iptr3d(igrd)),
     &                  rkv(iptr3d(igrd)),depth(iptr3d_full(igrd)),
     &                  tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &                  windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &                  water(iptr3d(igrd)),cwc(iptr3d(igrd)),
     &                  tsurf(iptr2d(igrd)),mapscl(iptr2d(igrd)),
     &                  sfcz0(iptr2d(igrd)),ptconc(ipsa3d(igrd)),
     &                  fluxdum,ptdryfld(ipsa2d(igrd)),ipsa3d(igrd),
     &                  tarray2,iproc_id,ipacl_2d(iptr2d_full(igrd)),
     &                  ipacl_3d(iptr3d_full(igrd)),
     &                  '  SA z diffus ......','  SA x/y diff ......')
               else
                   call diffus(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &                 .TRUE.,igrd,ncol(igrd),nrow(igrd),nlay(igrd),
     &                  ntotsp,ndepspc*3,deltat(igrd),deltax(1,igrd),
     &                  deltay(igrd),idfin(iptr2d(igrd)),ptvdep,
     &                  rkx(iptr3d(igrd)),rky(iptr3d(igrd)),
     &                  rkv(iptr3d(igrd)),depth(iptr3d_full(igrd)),
     &                  tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &                  windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &                  water(iptr3d(igrd)),cwc(iptr3d(igrd)),
     &                  tsurf(iptr2d(igrd)),mapscl(iptr2d(igrd)),
     &                  sfcz0(iptr2d(igrd)),ptconc(ipsa3d(igrd)),
     &                  fluxdum,depdum,ipsa3d(igrd),
     &                  tarray2,iproc_id,ipacl_2d(iptr2d_full(igrd)),
     &                  ipacl_3d(iptr3d_full(igrd)),
     &                  '  SA z diffus ......','  SA x/y diff ......')
               endif
          endif
c
c-----Perform wet scavenging
c
          if (lwet) then
            if( .NOT. lmpi .OR. iproc_id .EQ. 1 ) then
              write(*,'(a20,$)') 'wetdep  ......'
            endif
            write(iout,'(a20,$)') 'wetdep  ......'
            call wetdep(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &              igrd,ncol(igrd),nrow(igrd),nlay(igrd),nspec,
     &              ndepspc*3,deltat(igrd),deltax(1,igrd),deltay(igrd),
     &              mapscl(iptr2d(igrd)),depth(iptr3d_full(igrd)),
     &              tempk(iptr3d(igrd)),
     &              press(iptr3d(igrd)),cwc(iptr3d(igrd)),
     &              pwr(iptr3d(igrd)),pws(iptr3d(igrd)),
     &              pwg(iptr3d(igrd)),cph(iptr3d(igrd)),
     &              densfac,idfin(iptr2d(igrd)),
     &              conc(iptr4d(igrd)),fluxes(1,igrd),
     &              depfld(iptrdp(igrd)),ptwetfld(MAX(ipsa2d(igrd),1)),
     &              dtout,ipacl_3d(iptr3d_full(igrd)),ipsa3d(igrd) )
            tcpu = dtime(tarray2)
            if( .NOT. lmpi .OR. iproc_id .EQ. 1 ) then
               write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
            endif
            write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
            call flush(6)
            call flush(iout)
c
c======================== Source Apportion Begin =======================
c
            if( ltrace .AND. tectyp .EQ. RTRAC ) then
               call wetdeprt(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &              igrd,ncol(igrd),nrow(igrd),nlay(igrd),nrtrac,
     &              deltat(igrd),deltax(1,igrd),deltay(igrd),
     &              mapscl(iptr2d(igrd)),depth(iptr3d_full(igrd)),
     &              tempk(iptr3d(igrd)),
     &              press(iptr3d(igrd)),cwc(iptr3d(igrd)),
     &              pwr(iptr3d(igrd)),pws(iptr3d(igrd)),
     &              pwg(iptr3d(igrd)),cph(iptr3d(igrd)),
     &              densfac,idfin(iptr2d(igrd)),
     &              ptconc(ipsa3d(igrd)))
            endif
c
c======================== Source Apportion End =======================
c
         endif      !lwet
      endif         !end of if( .not. LMPI .or. (iproc_id .gt. 0) ) on line332
      return
      end

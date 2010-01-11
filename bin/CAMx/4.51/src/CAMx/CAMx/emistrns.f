      subroutine emistrns(igrd,chtime,chdate)
c
c----CAMx v4.51 080522
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
c     Copyright 1996-2008
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
      include 'camx.com'
      include 'camxfld.com'
      include 'chmstry.com'
      include 'grid.com'
      include 'flags.com'
      include 'filunit.com'
      include 'ptemiss.com'
      include 'bndary.com'
      include 'pigsty.com'
c
      character*8 chtime, chdate
c
c======================== Process Analysis Begin =======================
c
      include "procan.com"
c
c========================= Process Analysis End ========================
c
c
c======================== Source Apportion Begin =======================
c
      include 'tracer.com'
      include 'rtracchm.com'
c
      real*8 ardum(MXTRSP), ptdum(MXTRSP), fluxdum(MXTRSP*11)
      real depdum(MXCOLA,MXROWA,3*MXSPEC)
c
      common /comemistrns/ depdum, ptdum, ardum, fluxdum
c
c========================= Source Apportion End ========================
c
c-----Entry point
c
      write(*,'(/,a,i3)')           ' Processing grid: ',igrd
      write(*,'(a,f7.1)')           '    Timestep (s): ',deltat(igrd)
      write(*,'(a,2x,a8,1X,a8,/)')  '  Grid Date/Time: ',chdate,chtime
      write(iout,'(/,a,i3)')          ' Processing grid: ',igrd
      write(iout,'(a,f7.1)')          '        Timestep: ',deltat(igrd)
      write(iout,'(a,2x,a8,1x,a8,/)') '  Grid Date/Time: ',chdate,chtime
c
c-----Initialize new pig puffs
c
      if (ipigflg .NE. 0 .AND. ipigint .EQ. igrd) then
        write(*,'(a20,$)') 'piginit ......'
        write(iout,'(a20,$)') 'piginit ......'
        call piginit(deltat(igrd),ptemis,height,windu,windv,tempk,press)
c       call pigdiag(idiag,time,date,igrd,'After piginit       ')
        tcpu = dtime(tarray2)
        write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
        write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
        call flush(6)
        call flush(iout)
c
c-----Update PiG locations 
c
        write(*,'(a20,$)') 'pigwalk ......'
        write(iout,'(a20,$)') 'pigwalk ......'
        call pigwalk(deltat(igrd))
c       call pigdiag(idiag,time,date,igrd,'After pigwalk       ')
        tcpu = dtime(tarray2)
        write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
        write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
        call flush(6)
        call flush(iout)
      endif
c
c-----Determine mass-conserving vertical velocity parameters
c
      write(*,'(a20,$)') 'zrates ......'
      write(iout,'(a20,$)') 'zrates ......'
      call zrates(igrd,xyordr,ncol(igrd),nrow(igrd),nlay(igrd),
     &            nadv(1,igrd),deltat(igrd),deltax(1,igrd),
     &            deltay(igrd),depth(iptr3d(igrd)),
     &            phpt(iptr3d(igrd)),pppt(iptr3d(igrd)),
     &            ptpt(iptr3d(igrd)),windu(iptr3d(igrd)),
     &            windv(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &            press(iptr3d(igrd)),mapscl(iptr2d(igrd)),dilut,entrn)
      tcpu = dtime(tarray2)
      write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
      write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
      call flush(6)
      call flush(iout)
c
c-----Inject emissions
c
      write(*,'(a20,$)') 'emiss ......'
      write(iout,'(a20,$)') 'emiss ......'
      tcpu = dtime(tarray2)
      call emiss(igrd,nspec,narspc(igrd),nptspc,larmap(1,igrd),
     &           lptmap,nosrc(igrd),idsrc(1,igrd),isrc(1,igrd),
     &           jsrc(1,igrd),ncol(igrd),nrow(igrd),nlay(igrd),
     &           deltat(igrd),deltax(1,igrd),deltay(igrd),
     &           mapscl(iptr2d(igrd)),
     &           height(iptr3d(igrd)),depth(iptr3d(igrd)),
     &           windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &           tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &           aremis(iptrem(igrd)),ptemis,
     &           armass(1,igrd),ptmass(1,igrd),
     &           conc(iptr4d(igrd)),ipacl_3d(iptr3d(igrd)) )
c
c======================== Source Apportion Begin =======================
c
c  --- call routine with tracer species arrays, send dummy
c      arguemnts for the total mass arrays ---
c
c
      if( ltrace .OR. lddm ) then
         call emiss(igrd,ntotsp,ntotsp,ntotsp,lsamap,lsamap,
     &              nosrc(igrd),idsrc(1,igrd),isrc(1,igrd),jsrc(1,igrd),
     &              ncol(igrd),nrow(igrd),nlay(igrd),deltat(igrd),
     &              deltax(1,igrd),deltay(igrd),mapscl(iptr2d(igrd)),
     &              height(iptr3d(igrd)),
     &              depth(iptr3d(igrd)),windu(iptr3d(igrd)),
     &              windv(iptr3d(igrd)),
     &              tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &              saemis(ipsa2d(igrd)),
     &              sapnts,ardum,ptdum,
     &              ptconc(ipsa3d(igrd)),ipacl_3d(iptr3d(igrd)) )
      endif
c
c========================= Source Apportion End ========================
c
      tcpu = dtime(tarray2)
      write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
      write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
      call flush(6)
      call flush(iout)
c
c-----Perform 2-D transport
c
      call xyadvec(igrd,xyordr,ncol(igrd),nrow(igrd),nlay(igrd),nspec,
     &             MAX(1,ntotsp),nadv(1,igrd),
     &             deltat(igrd),deltax(1,igrd),deltay(igrd),
     &             windu(iptr3d(igrd)),
     &             windv(iptr3d(igrd)),depth(iptr3d(igrd)),
     &             mapscl(iptr2d(igrd)),conc(iptr4d(igrd)),
     &             fluxes(1,igrd),ptconc(MAX(1,ipsa3d(igrd))),
     &             tarray2,ipsa3d(igrd),ipacl_3d(iptr3d(igrd)) )
      call flush(6)
      call flush(iout)
c
c-----Perform vertical transport
c
      call zadvec(.FALSE.,igrd,ncol(igrd),nrow(igrd),nlay(igrd),nspec,
     &             MAX(1,ntotsp),deltat(igrd),deltax(1,igrd),
     &             deltay(igrd),
     &             densfac,idfin(iptr2d(igrd)),caloft,
     &             depth(iptr3d(igrd)),entrn,dilut,
     &             tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &             spname,conc(iptr4d(igrd)),
     &             fluxes(1,igrd),ptloft,ptconc(MAX(ipsa3d(igrd),1)),
     &             tarray2,ipacl_2d(iptr2d(igrd)),
     &                                        ipacl_3d(iptr3d(igrd)) )
      call flush(6)
      call flush(iout)
c
c
c======================== Source Apportion Begin =======================
c
      if( ltrace ) then
c
         if( tectyp .EQ. RTRAC ) then
           call xyadvec(igrd,xyordr,ncol(igrd),nrow(igrd),nlay(igrd),
     &             ntotsp,1,nadv(1,igrd),deltat(igrd),deltax(1,igrd),
     &             deltay(igrd),windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &             depth(iptr3d(igrd)),mapscl(iptr2d(igrd)),
     &             ptconc(ipsa3d(igrd)),fluxdum,ptconc(1),
     &             tarray2,ipsa3d(igrd),ipacl_3d(iptr3d(igrd)) )
         endif
c
c----- call routine to advect the timing tracers horizontally ---
c
         if( npttim .GT. 0 ) call timadv(igrd,xyordr,ncol(igrd),
     &             nrow(igrd),nlay(igrd),nspec,
     &             deltat(igrd),deltax(1,igrd),deltay(igrd),
     &             windu(iptr3d(igrd)),
     &             windv(iptr3d(igrd)),depth(iptr3d(igrd)),
     &             mapscl(iptr2d(igrd)),ptconc(ipsa3d(igrd)))
c
c  --- call routine with tracer species arrays, send dummy
c      arguments for the total mass arrays ---
c
         call zadvec(.TRUE.,igrd,ncol(igrd),nrow(igrd),nlay(igrd),
     &                ntotsp,1,deltat(igrd),deltax(1,igrd),deltay(igrd),
     &                densfac,idfin(iptr2d(igrd)),ptloft,
     &                depth(iptr3d(igrd)),entrn,dilut,
     &                tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &                ptname, ptconc(ipsa3d(igrd)),
     &                fluxdum,ptloft,ptconc(1),
     &                tarray2,ipacl_2d(iptr2d(igrd)),
     &                ipacl_3d(iptr3d(igrd)) )
      endif
c
c========================= Source Apportion End ========================
c
c-----Perform wet scavenging
c
      if (lwet) then
        write(*,'(a20,$)') 'wetdep  ......'
        write(iout,'(a20,$)') 'wetdep  ......'
        call wetdep(igrd,ncol(igrd),nrow(igrd),nlay(igrd),nspec,
     &              deltat(igrd),deltax(1,igrd),deltay(igrd),
     &              mapscl(iptr2d(igrd)),depth(iptr3d(igrd)),
     &              tempk(iptr3d(igrd)),
     &              press(iptr3d(igrd)),cwc(iptr3d(igrd)),
     &              pwr(iptr3d(igrd)),pws(iptr3d(igrd)),
     &              pwg(iptr3d(igrd)),cph(iptr3d(igrd)),
     &              densfac,idfin(iptr2d(igrd)),
     &              conc(iptr4d(igrd)),fluxes(1,igrd),
     &              depfld(iptrdp(igrd)),dtout,ipacl_3d(iptr3d(igrd)),
     &              ipsa3d(igrd) )
        tcpu = dtime(tarray2)
        write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
        write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
        call flush(6)
        call flush(iout)
c
c======================== Source Apportion Begin =======================
c
         if( ltrace .AND. tectyp .EQ. RTRAC ) then
           call wetdeprt(igrd,ncol(igrd),nrow(igrd),nlay(igrd),nrtrac,
     &              deltat(igrd),deltax(1,igrd),deltay(igrd),
     &              mapscl(iptr2d(igrd)),depth(iptr3d(igrd)),
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
      endif
c
c-----Update vertical grid and environmental fields for this timestep
c
      write(*,'(a20,$)') 'updtmet ......'
      write(iout,'(a20,$)') 'updtmet ......'
      call updtmet(igrd,ncol(igrd),nrow(igrd),nlay(igrd),
     &             ngas,densfac,deltat(igrd),phpt(iptr3d(igrd)),
     &             height(iptr3d(igrd)),depth(iptr3d(igrd)),
     &             pppt(iptr3d(igrd)),press(iptr3d(igrd)),
     &             pupt(iptr3d(igrd)),windu(iptr3d(igrd)),
     &             pvpt(iptr3d(igrd)),
     &             windv(iptr3d(igrd)),pspt(iptr2d(igrd)),
     &             tsurf(iptr2d(igrd)),
     &             ptpt(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &             pwpt(iptr3d(igrd)),water(iptr3d(igrd)),
     &             pkpt(iptr3d(igrd)),rkv(iptr3d(igrd)),
     &             conc(iptr4d(igrd)) )
      tcpu = dtime(tarray2)
      write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
      write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
      call flush(6)
      call flush(iout)
c
c  --- if this is the master grid, update the boundary 
c      conditions for the tracer species ---
c
      if( ltrace .and. tectyp .NE. RTRAC .and. igrd .EQ. 1 ) then
          call clrbdysa(1,ncol(1),nrow(1),nlay(1),ntotsp,ptconc(1))
          call filbdysa(1,ncol(1),nrow(1),nlay(1),nspec,ntotsp,
     &                                           conc(1),ptconc(1))
      endif
c
c-----Perform 3-D Diffusion
c
      call diffus(igrd,ncol(igrd),nrow(igrd),nlay(igrd),nspec,
     &          MAX(1,ntotsp),deltat(igrd),deltax(1,igrd),deltay(igrd),
     &          idfin(iptr2d(igrd)),vdep(iptrem(igrd)),
     &          rkx(iptr3d(igrd)),rky(iptr3d(igrd)),
     &          rkv(iptr3d(igrd)),depth(iptr3d(igrd)),
     &          tempk(iptr3d(igrd)),press(iptr3d(igrd)),
     &          mapscl(iptr2d(igrd)),conc(iptr4d(igrd)),
     &          fluxes(1,igrd),depfld(iptrdp(igrd)),
     &          ptconc(MAX(1,ipsa3d(igrd))),tarray2,
     &          '  z diffusion ......','     x/y diff ......',
     &          ipacl_2d(iptr2d(igrd)),ipacl_3d(iptr3d(igrd)) )
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
         call filvdsa(ncol(igrd),nrow(igrd),nlay(igrd),nspec,ntotsp,
     &                     conc(iptr4d(igrd)),vdep(iptrem(igrd)),ptvdep,
     &                                             vdeprt(ipsa2d(igrd)))
c
c  --- call routine with tracer species arrays, send dummy
c      arguemnts for the total mass arrays ---
c
         call diffus(igrd,ncol(igrd),nrow(igrd),nlay(igrd),ntotsp,1,
     &               deltat(igrd),deltax(1,igrd),deltay(igrd),
     &               idfin(iptr2d(igrd)),ptvdep,rkx(iptr3d(igrd)),
     &               rky(iptr3d(igrd)),rkv(iptr3d(igrd)),
     &               depth(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &               press(iptr3d(igrd)),mapscl(iptr2d(igrd)),
     &               ptconc(ipsa3d(igrd)),fluxdum,depdum,ptconc(1),
     &               tarray2,
     &               '  SA z diffus ......','  SA x/y diff ......',
     &               ipacl_2d(iptr2d(igrd)),ipacl_3d(iptr3d(igrd)) )
      endif
c
c========================= Source Apportion End ========================
c
      return
      end

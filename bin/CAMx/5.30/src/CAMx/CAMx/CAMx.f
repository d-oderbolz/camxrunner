      program CAMx
      use filunit
      use grid
      use chmstry
      use ahomap
      use bndary
      use camxfld
      use camxcom
      use ptemiss
      use pigsty
      use procan
      use rtracchm
      use tracer
c
      use master_mod  
      use node_mod   
c
c***********************************************************************
c
c                  CCCCCCC      AA      MM      MM                       
c                 CC          AA  AA    MMM    MMM   xx   xx  
c                 CC         AA    AA   MM MMMM MM    xx xx  
c                 CC         AAAAAAAA   MM  MM  MM     xxx  
c                 CC         AA    AA   MM      MM    xx xx
c                  CCCCCCC   AA    AA   MM      MM   xx   xx
c
c        C O M P R E H E N S I V E   A I R   Q U A L I T Y   M O D E L
c        -                           -                       - 
c                         with   E X T E N S I O N S
c                                  -
c
c                          VERSION  5.30  12-23-10
c
c                           Copyright 1996 - 2010
c                     ENVIRON International Corporation 
c                       773 San Marin Dr, Suite 2115
c                             Novato, CA  94998
c                             (415) 899 - 0700
c                                www.camx.com
c
c     Revision History:
c       12/23/10   Version 5.30 released (see Release_notes_5.30)
c       07/01/10   Version 5.20.1 released (see Release_notes_5.20.1)
c       04/02/10   Version 5.20 released (see Release_notes_5.20)
c       09/09/09   Version 5.10 released (see Release_notes_5.10)
c       04/06/09   Version 5.01 released (see Release_notes_5.01)
c       05/22/08   Version 4.51 released (see Release_notes_4.51)
c       10/25/06   Version 4.40 released (see Release_notes_4.40)
c       03/29/06   Version 4.31 released (see Release_notes_4.31)
c       02/15/06   Version 4.30 released (see Release_notes_4.30)
c       06/15/05   Version 4.20 released (see Release_notes_4.20)
c       12/06/04   Version 4.11.s released (see Release_notes_4.11.s)
c       08/01/04   Version 4.10.s released (see Release_notes_4.10.s)
c       12/05/03   Version 4.03 released (see Release_notes_4.03)
c       07/09/03   Version 4.02 released (see Release_notes_4.02)
c       06/17/03   Version 4.01 released (see Release_notes_4.01)
c       05/31/03   Version 4.00 released (see Release_notes_4.00)
c       11/15/01   Version 3.10 released (see Release_notes_3.10)
c        4/25/01   Version 3.01 released (see Release_notes_3.01)
c       12/30/00   Version 3.00 released (see Release_notes_3.00) 
c       12/30/99   Version 2.03 released (see Release_notes_2.03) 
c        9/13/99   Version 2.02 released (see Release_notes_2.02) 
c        5/10/99   Version 2.01 released (see Release_notes_2.01) 
c       12/30/98   Version 2.00 released, original development
c
c***********************************************************************
c  
      include 'camx.prm'
      include 'flags.inc'
      include 'mpif.h'
c
c======================== Probing Tool Begin ===========================
c
c     linit   --  logical, if true,   initialize cipr(IPR_INIT ,*,*,*)
c                          otherwise, initialize cipr(IPR_FINAL,*,*,*)
c
      logical linit
c
c========================= Probing Tool End ============================
c
      integer inpdate,emsdate,hazdate,ozndate,bnddate,wrtdate,enddate 
      integer snodate, iproc_id, nlayav
      real inptim,emstim,haztim,ozntim,bndtim,wrttim,endtim 
      real snotim
      character*20 version
      character*10 name
      character*8 chtime, chdate
      logical lupdtdep(MXGRID)
c
      data version /'CAMx v5.30, 12-23-10'/
c
c-----Entry point
c
c----Initialize the MPI mechanism
c
      lmpi = .TRUE.
      call MPI_INIT( ierr )
      if( ierr .NE. 0 ) then
         write(*,*) 'MPI is not active.'
         iproc_id = 0
         numprocs = 1
      endif
      if( lmpi ) then
         call MPI_COMM_SIZE( MPI_COMM_WORLD, numprocs, ierr )
         call MPI_COMM_RANK( MPI_COMM_WORLD, iproc_id, ierr )
      endif
      if( numprocs .EQ. 1 ) lmpi = .FALSE.
      nsteps = 0
      itag = 1
c
c-----Start model simulation
c
      tcpu = dtime(tarray2)
      call sim_init(version,inptim,inpdate,emstim,emsdate,haztim,
     &             hazdate,ozntim,ozndate,snotim,snodate,
     &             bndtim,bnddate,wrttim,wrtdate,endtim,enddate,
     &                                            numprocs,iproc_id) 
      nlayav = nlay(1)
      if( .NOT. l3davg(1) ) nlayav = 1
c
      write(iout,*)
      write(iout,'(a,f10.3)') 
     &       'Elapsed time (s) for model startup: ',tcpu
      write(iout,*)
      write(idiag,*)
      write(iout,'(a)')'BEGIN SIMULATION'
      write(idiag,'(a)')'BEGIN SIMULATION'
      write(iout,'(a)') 'Starting main integration loop...'
      write(idiag,*)
      call flush(iout)
      call flush(idiag)
c
c--------------------  Main time-integration loop  ---------------------
c
  100 continue
      if( lmpi ) then
        call nodes_pass(date,1,MPI_INTEGER,itag,numprocs,iproc_id)
        call nodes_pass(time,1,MPI_REAL,itag,numprocs,iproc_id)
      endif
      call chrtime(time,date,chtime,chdate)
      if( iproc_id .EQ. 0 ) then
         nsteps = nsteps + 1
         xyordr = mod(nsteps,2)
      endif
      if( (lmpi .AND. iproc_id .EQ. 1) .OR. .NOT. lmpi ) then
         write(*,*)
         write(*,'(a,a8,1x,a8)') 'Date/time: ',chdate,chtime
         write(*,*)
      endif
c
c-----Check if time-varying grid/met data are to be read
c
      if( lmpi ) then
        call MPI_Barrier(MPI_COMM_WORLD, ierr)
        call nodes_pass(bnddate,1,MPI_INTEGER,itag,numprocs,iproc_id)
        call nodes_pass(bndtim,1,MPI_REAL,itag,numprocs,iproc_id)
        call nodes_pass(inpdate,1,MPI_INTEGER,itag,numprocs,iproc_id)
        call nodes_pass(inptim,1,MPI_REAL,itag,numprocs,iproc_id)
        call nodes_pass(emsdate,1,MPI_INTEGER,itag,numprocs,iproc_id)
        call nodes_pass(emstim,1,MPI_REAL,itag,numprocs,iproc_id)
        call nodes_pass(nsteps,1,MPI_REAL,itag,numprocs,iproc_id)
        call nodes_pass(xyordr,1,MPI_REAL,itag,numprocs,iproc_id)
      endif
c
      write(iout,*)
      write(iout,'(a,a8,1x,a8)') 'Date/time: ',chdate,chtime
      write(iout,*)
c
      if( date .EQ. inpdate .AND. ABS(time-inptim) .LT. 0.01) then
          do i=1,ngrid
             lupdtdep(i) = .TRUE.
          enddo
          call nodes_pass(lupdtdep,ngrid,MPI_INTEGER,
     &                                        itag,numprocs,iproc_id)
          call tstep_init(inptim,inpdate,nsteps,numprocs,iproc_id)
      endif
c
c-----Check if emissions data are to be read
c
      if( lmpi ) call MPI_Barrier(MPI_COMM_WORLD, ierr)
      if( date .EQ. emsdate .AND. ABS(time-emstim) .LT. 0.01)
     &                 call emiss_updt(emstim,emsdate,numprocs,iproc_id)
c
c-----Check if master grid boundary data are to be read
c
      call bndry_updt(mmxp,mmyp,mmzp,bndtim,bnddate,nsteps,
     &                                              numprocs,iproc_id)
c
c-----Check if haze data are to be read
c
      if( lchem .OR. ldry ) call ahoz_updt(hazdate,haztim,ozndate,
     &                         ozntim,snodate,snotim,numprocs,iproc_id)
      call newgrid(1)
c
c-----At this point we have a successful model startup
c     Stop model now if startup diagnostic flag is set
c
      if ( ldiag .and. nsteps.eq.1) then
        if( iproc_id .EQ. 0 ) then
            write(*,'(/,a,a8,1x,a8,/)')'Date/time: ',chdate,chtime
            write(*,'(a)')'SUCCESSFUL MODEL STARTUP'
            write(*,'(a)')'Diagnostic Initialization Test Complete'
        endif
        write(iout,'(/,a,a8,1x,a8,/)') 'Date/time: ',chdate,chtime
        write(iout,'(a)')'SUCCESSFUL MODEL STARTUP'
        write(iout,'(a)')'Diagnostic Initialization Test Complete'
        call flush(6)
        call MPI_Finalize(ierr)
        stop
      endif
c
c-----Update date/time for this timestep
c
      do ip=1,ngrid
        timec(ip) = time
        datec(ip) = date
      enddo
      call uptime(time,date,deltat(1))
c
c-----Initialize radical concentrations
c
      if( iproc_id .EQ. 0 .AND. lchem .and. nsteps.eq.1 
     &                                     .and. nrad.gt.0) then
        write(*,'(a20,$)') 'raddrivr ......'
        write(iout,'(a20,$)') 'raddrivr ......'
        call raddrivr()
        tcpu = dtime(tarray2)
        write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
        write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
        call flush(6)
        call flush(iout)
      endif
      call chrtime(timec(1),datec(1),chtime,chdate)
c
c-----Master grid emission and transport
c
c  --- call routine to load all of the input data to
c      compute node processes ---
c
      if( lmpi .AND. nsteps .EQ. 1 ) then
         call MPI_Barrier(MPI_COMM_WORLD, ierr)
         call nodes_tstep(numprocs,iproc_id)
      endif
c
      if( .NOT. lmpi .OR. iproc_id .GT. 0 ) call avgall(iproc_id)
c
      call newgrid(1)
c
      call emistrns(1,chtime,chdate,numprocs,iproc_id,lupdtdep(1))
c
c-----Computation for nested grids
c
      if( ngrid .GT. 1 ) call nesting(numprocs,iproc_id,lupdtdep)
c
      if( .NOT. lmpi .OR. iproc_id .GT. 0 ) then
c
         call uptime(timec(1),datec(1),deltat(1))
         call chrtime(timec(1),datec(1),chtime,chdate)
c
c-----Evolve PiG puffs on master grid
c
         if( ipigflg .NE. 0 ) call pigevol(1,iproc_id)
      endif
      if( ipigflg .NE. 0 .AND. lmpi )
     &                 call nodes_pig_pass(1,numprocs,iproc_id)
c     
c-----Master grid chemical reactions
c
      if( .NOT. lmpi .OR. iproc_id .GT. 0 ) then
         call chemrxn(1,iproc_id)
c
c-----Update average concentrations
c
         call avgall(iproc_id)
      endif
c
c-----Check if concentration fields are to be written
c
      if( lmpi ) then
         if( date .EQ. wrtdate .AND. ABS(time-wrttim) .LT. 0.01 ) then
             call MPI_Barrier(MPI_COMM_WORLD, ierr)
             call master_update(numprocs, iproc_id)
             call MPI_Barrier(MPI_COMM_WORLD, ierr)
          endif
      endif
      call chrtime(time,date,chtime,chdate)
      if( iproc_id .EQ. 0 ) then
         if( date .EQ. wrtdate .AND. ABS(time-wrttim) .LT. 0.01 ) then
c
c-----Regular model grid files
c
           write(*,'(a20,$)') 'wrtcon ......'
           write(iout,'(a20,$)') 'wrtcon ......'

           if( lhdfout ) then
             call hdf5_file_out('averag',date,time)
           else
             do igrd = 1,ngrid
               nlayav = nlay(igrd)
               if( .NOT. l3davg(igrd) ) nlayav = 1
               call wrtcon(0,time,date,iavg(igrd),igrd,ncol(igrd),
     &                     nrow(igrd),nlayav,navspc,avcnc(iptrav(igrd)))
             enddo
           endif

           if( ldry .OR. lwet ) then
             do igrd = 1,ngrid
               call wrtdep(time,date,idep(igrd),ncol(igrd),nrow(igrd),
     &                               3*ndepspc,nspec,vdep(iptrem(igrd)),
     &                                            depfld(iptrdp(igrd)))
             enddo
c
c======================== Source Apportion Begin =======================
c
             if( lptdepout ) then
                do igrd = 1,ngrid
                  call wrtdepsa(time,date,iowptdep(igrd),ncol(igrd),
     &                     nrow(igrd),notimespc,ptdryfld(ipsadep(igrd)),
     &                                         ptwetfld(ipsadep(igrd)))
                enddo
             endif
c
c======================== Source Apportion End =======================
c
c
           endif
c
           do igrd = 1,ngrid
             nlayav = nlay(igrd)
             if( .NOT. l3davg(igrd) ) nlayav = 1
             nodes = ncol(igrd)*nrow(igrd)*nlayav*navspc
             call zeros(avcnc(iptrav(igrd)),nodes)
             nodes = ncol(igrd)*nrow(igrd)*3*ndepspc
             call zeros(depfld(iptrdp(igrd)),nodes)
             if( lptdepout ) then
                nodes = ncol(igrd)*nrow(igrd)*notimespc
                call zeros(ptdryfld(ipsadep(igrd)),nodes)
                call zeros(ptwetfld(ipsadep(igrd)),nodes)
             endif
           enddo
c
c-----Write the PiG sampling grid output
c
           if( lsample ) then
             if( lhdfout ) then
               call hdf5_file_out('sample',date,time)
             else
               do igrd = 1,nsample
                  call wrtsmp(.FALSE.,time,date,isample(igrd),
     &                         ncolsmp(igrd),nrowsmp(igrd),1,
     &                         navspc,smpcnc(ipsmp(igrd)))
               enddo
             endif
             do igrd = 1,nsample
               nodes = ncolsmp(igrd)*nrowsmp(igrd)*navspc
               call zeros(smpcnc(ipsmp(igrd)),nodes)
             enddo
           endif
c
c======================== Source Apportion Begin =======================
c
           if( ltrace .OR. lddm .OR. lhddm ) then
               if( tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) then
                  call wrrcprt(date,time)
               else
c
c   --- call routine to get the averages at the receptors and
c       write the receptor average file ----
c
                  do igrd=1,ngrid
                     if( .NOT.( ltrace .OR. lddmcalc(igrd) ) ) cycle
                     call addrcp(igrd,ncol(igrd),nrow(igrd),
     &                                   ntotsp,ptavrg(ipsa2d(igrd)))
                  enddo
                  if( ltrace ) call avgrcp(date,time)
                  if( lddm .OR. lhddm ) call avgrcpddm(date,time)
               endif
c
c
c   --- call routine to write the tracer gridded surface concentrations ---
c
              do igrd=1,ngrid
                if( .NOT.( ltrace .OR. lddmcalc(igrd) ) ) cycle
                call wsfcsa(igrd,date,time,ncol(igrd),nrow(igrd),ntotsp,
     &                   ptavrg(ipsa2d(igrd)))
              enddo
c
c   --- call routine to re-initialize the running averages ---
c
              do igrd=1,ngrid
                 if( .NOT.( ltrace .OR. lddmcalc(igrd) ) ) cycle
                 nodes=ncol(igrd)*nrow(igrd)*ntotsp
                 call zeros(ptavrg(ipsa2d(igrd)),nodes)
              enddo
c
c   --- call routine to write the RTRAC/PiG sampling grid output
c
              if ((tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) .AND.
     &             lsample .AND. lsmptrc ) then
                 if( .NOT. lhdfout) then
                    do igrd = 1,nsample
                       call wrtsmp(.true.,time,date,iowsmp(igrd),
     &                          ncolsmp(igrd),nrowsmp(igrd),1,nrtrac,
     &                          rtsmpcnc(iprtsmp(igrd)))
                    enddo
                 endif
                 do igrd = 1,nsample
                   nodes = ncolsmp(igrd)*nrowsmp(igrd)*nrtrac
                   call zeros(rtsmpcnc(iprtsmp(igrd)),nodes)
                 enddo
              endif
           endif
c
c========================= Source Apportion End ========================
c
c========================= Process Analysis Begin ======================
c
c-----Get final concentration
c
           if( lipr .OR. lirr ) then
              if( lipr ) then
                linit = .FALSE.
                do igrd = 1,ngrid
                   call initipr(linit,iproc_id,igrd,nspec,ncol(igrd),
     &                        nrow(igrd), nlay(igrd),conc(iptr4d(igrd)))
                enddo
              endif
c
c-----Write PA results
c
              if( lipr ) call wrtipr(date,time)
              if( lirr ) call wrtirr(date,time)
c
c   --- call routine to zero out all Process Analysis data structures ---
c
              call pazero()
c
c-----Get initial concentration for next loop
c
              if( lipr ) then
                 linit = .TRUE.
                 do igrd = 1,ngrid
                    call initipr(linit,iproc_id,igrd,nspec,ncol(igrd),
     &                        nrow(igrd),nlay(igrd),conc(iptr4d(igrd)))
                 enddo
              endif
           endif
c
c========================= Process Analysis End ========================
c
           tcpu = dtime(tarray2)
           write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
           write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
           call flush(6)
           call flush(iout)
c
c-----Write PiG restart file and diagnostics
c
           if( ipigflg .NE. 0 ) then
             write(*,'(a20,$)') 'wrtpig ......'
             write(iout,'(a20,$)') 'wrtpig ......'
             call wrtpig(date,time,begdate,begtim)
c            call pigdiag(idiag,chtime,chdate,1,'                    ')
             call pigmscl(nspec,ngrid,chtime,chdate,idiag,pigdump,pgmserr)
             tcpu = dtime(tarray2)
             write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
             write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
             call flush(6)
             call flush(iout)
           endif
c
c-----Write model mass
c
           do igrd = 1,ngrid
             call wrtmass(igrd,chdate,chtime,1)
           enddo
c
c-----Flush file units
c
           call flush(iout)
           call flush(idiag)
           call flush(imass)
         endif
      endif
c
      if( lmpi .AND. iproc_id .GT. 0 ) then
         if( date .EQ. wrtdate .AND. ABS(time-wrttim) .LT. 0.01 ) then
             do igrd = 1,ngrid
               call newgrid(igrd)
               nlayav = nlay(igrd)
               if( .NOT. l3davg(igrd) ) nlayav = 1
               nodes = mxp*myp*nlayav*navspc
               call zeros(avcnc(iptrav(igrd)),nodes)
               nodes = mxp*myp*3*ndepspc
               call zeros(depfld(iptrdp(igrd)),nodes)
               if( lptdepout ) then
                  nodes = mxp*myp*notimespc
                  call zeros(ptdryfld(ipsadep(igrd)),nodes)
                  call zeros(ptwetfld(ipsadep(igrd)),nodes)
               endif
               call nodemass(igrd)
c
c========================= Source Apportion Begin ========================
c
               if( ltrace .OR. lddm .OR. lhddm ) then
                 nodes=mmxp(igrd)*mmyp(igrd)*ntotsp
                 call zeros(ptavrg(ipsa2d(igrd)),nodes)
               endif
c
c========================= Source Apportion End ========================
c

             enddo 
             if( ipigflg .NE. 0 .AND. lsample ) then
                 do ismp=1,nsample
                   nodes = ncolsmp(ismp)*nrowsmp(ismp)*navspc
                   call zeros(smpcnc(ipsmp(ismp)),nodes)
                   if( (tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC)
     &                                           .AND. lsmptrc ) then
                       nodes = ncolsmp(ismp)*nrowsmp(ismp)*nrtrac
                       call zeros(rtsmpcnc(iprtsmp(ismp)),nodes)
                   endif
                 enddo
             endif
         endif
      endif
c
      if( date .EQ. wrtdate .AND. ABS(time-wrttim) .LT. 0.01 ) then
         whr = aint(wrttim/100.)
         wmn = amod(wrttim,100.)
         wrttim = 100.*(whr + aint((wmn + dtout)/60.)) + 
     &                                     amod((wmn + dtout),60.)
         if (wrttim.ge.2400.) then
          wrttim = wrttim - 2400.
          wrtdate = wrtdate + 1
          if( MOD(wrtdate,1000) .GT. 365 ) then
              if( MOD(INT(wrtdate/1000),4) .EQ. 0 ) then
                 if( MOD(wrtdate,1000) .EQ. 367 )
     &                     wrtdate = (INT(wrtdate/1000)+1)*1000 + 1
              else
                 wrtdate = (INT(wrtdate/1000)+1)*1000 + 1
              endif
           endif
         endif
      endif
c
c-----Check for end of simulation
c
      tcpu = etime(tarray)
      write(iout,'(a,f10.3)') 'Accumulative CPU time:  ',tcpu 
      call flush(iout)
      if( (lmpi .AND. iproc_id .EQ. 1) .OR. .NOT. lmpi ) then
        write(*,'(a,f10.3)') 'Accumulative CPU time:  ',tcpu
        call flush(6)
      endif
      if (date.lt.enddate) goto 100
      if (date.eq.enddate .and. time.lt.endtim - 0.01) goto 100
c
c------------------  End main time-integration loop  -------------------
c
c-----Write final instantaneous restart files
c
      if( iproc_id .EQ. 0 ) then
          call wrtcon(1,time,date,iconc,1,ncol(1),nrow(1),nlay(1),
     &                                                   nspec,conc(1))
          if( ngrid .GT. 1) call wrfgcon(date,time)
c
c======================== Source Apportion Begin =======================
c
          if( ltrace .OR. lddm .OR. lhddm ) then
             call wconsa(date,time,ncol(1),nrow(1),nlay(1),ntotsp,ptconc(1))
             if( ngrid .GT. 1 ) call wfconsa(date,time)
          endif
      endif
c
c========================= Source Apportion End ========================
c
      call MPI_Barrier(MPI_COMM_WORLD, ierr)
      write(iout,'(/,a,a8,1x,a8,/)') 'Date/time: ',chdate,chtime
      write(iout,'(a)')'END SIMULATION'
      write(iout,'(a,i10)') 'TOTAL MASTER GRID TIME STEPS: ', nsteps
      if( iproc_id .EQ. 0 ) then
         write(*,'(/,a,a8,1x,a8,/)')'Date/time: ',chdate,chtime
         write(*,'(a)')'END SIMULATION'
         write(*,'(a,i10)') 'TOTAL MASTER GRID TIME STEPS: ', nsteps
      end if
c
      if (lmpi) call MPI_Finalize(ierr)
      stop
      end


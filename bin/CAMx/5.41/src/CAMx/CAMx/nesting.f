      subroutine nesting(numprocs,iproc_id,lupdtdep)
      use grid
      use grid_nodes
      use camxcom
      use camxfld
      use tracer
      use procan
c
c----CAMx v5.41 121109
c
c     NESTING is the driver for grid nesting algorithm.  It does the 
c     following tasks in a recursive order:
c       1. determines boundary conditions for children grids
c       2. calls EMISTRNS for each grid
c       3. call PIGEVOL for each grid
c       4. calls CHEMRXN for each grid 
c       5. aggregates concentrations on children grids to parent grid
c     Chemistry and transport are performed for each grid on their own
c     time step. Up to 4 generations of grid nesting are currently allowed.
c                          
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c          
c     Modifications:
c        10/12/04     New routine PIGEVOL added for PiG growth/chemistry
c
c     Input arguments:
c        iproc_id    process ID for this processor
c
c     Output arguments:
c        none
c
c     Subroutines Called:
c        SETBC
c        EMISTRNS
c        PIGEVOL
c        CHEMRXN
c        AGGR00
c        FGAVRG
c        CHRTIME
c
c     Called by:
c        CAMx
c
      include "camx.prm"
      include "flags.inc"
c
      integer     numprocs,iproc_id
      logical     lupdtdep(*)
      character*8 chtime, chdate
c
      integer iparnt(20),igrd0(20)
c
c-----Entry point
c
c-----Computation for children grids
c
      do igen=1,20
        iparnt(igen)=0
        igrd0(igen)=0
      enddo
c
c-----Generation 2
c
      igen=1
      iparnt(igen+1)=1
      if( lmpi .AND. iproc_id .GT. 0 ) then
        do ngr=1,nchdrn(iparnt(igen+1))
          call node_send_nbc(idchdrn(ngr,iparnt(igen+1)),
     &                                   iparnt(igen+1))
          call node_get_nbc (idchdrn(ngr,iparnt(igen+1)),
     &                                   iparnt(igen+1))
          if( ltrace .OR. lddm .OR. lhddm ) then
              call node_send_nbc_pt(idchdrn(ngr,iparnt(igen+1)),
     &                                          iparnt(igen+1))
              call node_get_nbc_pt (idchdrn(ngr,iparnt(igen+1)),
     &                                          iparnt(igen+1))
          endif
        enddo
      else if (.not. lmpi) then
        call setbc(iparnt(igen+1))
      endif
      mch2=nchdrn(iparnt(igen+1))
      do 100 ic2=1,mch2
          igen=2
          igrd0(igen)=idchdrn(ic2,iparnt(igen))
          igrd = igrd0(igen)
          call newgrid(igrd)
c
c-----Perform emissions and transport for generation 2
c
          do 99 it2=1,ntim(igrd)
            if( .NOT. lmpi .OR. iproc_id .GT. 0 ) 
     &                                  call fgavrg(iproc_id,igrd)
            call chrtime(timec(igrd),datec(igrd),chtime,chdate)
            call emistrns(igrd,chtime,chdate,numprocs,
     &                                     iproc_id,lupdtdep(igrd))
c
c-----Generation 3
c
            iparnt(igen+1)=igrd0(igen)
            if( lmpi .AND. iproc_id .GT. 0 ) then
              do ngr=1,nchdrn(iparnt(igen+1))
                 call node_send_nbc(idchdrn(ngr,iparnt(igen+1)),
     &                                         iparnt(igen+1))
                 call node_get_nbc (idchdrn(ngr,iparnt(igen+1)),
     &                                         iparnt(igen+1))
                 if( ltrace .OR. lddm .OR. lhddm ) then
                   call node_send_nbc_pt(idchdrn(ngr,iparnt(igen+1)),
     &                                                 iparnt(igen+1))
                   call node_get_nbc_pt(idchdrn(ngr,iparnt(igen+1)),
     &                                                  iparnt(igen+1))
                 endif
              enddo
            else if (.not. lmpi) then
              call setbc(iparnt(igen+1))
            endif
            mch3=nchdrn(iparnt(igen+1))
            do 90 ic3=1,mch3
              igen=3
              igrd0(igen)=idchdrn(ic3,iparnt(igen))
              igrd = igrd0(igen)
              call newgrid(igrd)
c
c-----Perform emissions and transport for generation 3
c
              do 89 it3=1,ntim(igrd)
                if( .NOT. lmpi .OR. iproc_id .GT. 0 ) 
     &                                     call fgavrg(iproc_id,igrd)
                call chrtime(timec(igrd),datec(igrd),
     &                     chtime,chdate)
                call emistrns(igrd,chtime,chdate,numprocs,
     &                                         iproc_id,lupdtdep(igrd))
c
c-----Generation 4
c
                iparnt(igen+1)=igrd0(igen)
                if( lmpi .AND. iproc_id .GT. 0 ) then
                  do ngr=1,nchdrn(iparnt(igen+1))
                    call node_send_nbc(idchdrn(ngr,iparnt(igen+1)),
     &                                         iparnt(igen+1))
                    call node_get_nbc (idchdrn(ngr,iparnt(igen+1)),
     &                                         iparnt(igen+1))
                     if( ltrace .OR. lddm .OR. lhddm ) then
                         call node_send_nbc_pt(
     &                      idchdrn(ngr,iparnt(igen+1)),iparnt(igen+1))
                         call node_get_nbc_pt(
     &                      idchdrn(ngr,iparnt(igen+1)),iparnt(igen+1))
                     endif
                  enddo
                else if (.not. lmpi) then
                  call setbc(iparnt(igen+1))
                endif
                mch4=nchdrn(iparnt(igen+1))
                do 80 ic4=1,mch4
                  igen=4
                  igrd0(igen)=idchdrn(ic4,iparnt(igen))
                  igrd = igrd0(igen)
                  call newgrid(igrd)
c
c-----Perform emissions and transport for generation 4
c
                  do 79 it4=1,ntim(igrd)
                    if( .NOT. lmpi .OR. iproc_id .GT. 0 ) 
     &                                     call fgavrg(iproc_id,igrd)
                    call chrtime(timec(igrd),datec(igrd),
     &                                              chtime,chdate)
                    call emistrns(igrd,chtime,chdate,numprocs,
     &                                     iproc_id,lupdtdep(igrd))
c
c-----Generation X: more generations would be added here
c
c-----Perform chemistry for generation 4
c
                    igen=4
                    igrd = igrd0(igen)
                    call newgrid(igrd)
                    call uptime(timec(igrd),datec(igrd),deltat(igrd))
                    if( ipigflg .NE. 0 .AND. (.NOT. lmpi 
     &                                  .OR. iproc_id .GT. 0) ) 
     &                                       call pigevol(igrd,iproc_id)
                    if( ipigflg .NE. 0 .AND. lmpi ) then
                         call nodes_pig_pass(igrd,numprocs,iproc_id)
                    endif 
                    if( .NOT. lmpi .OR. iproc_id .GT. 0 ) then
                       call chemrxn(igrd,iproc_id)
                       call fgavrg(iproc_id,igrd)
                    endif
  79              continue
                  icode = 3
                  if (mch4.eq.1) then
                    icode = 0
                  elseif (ic4.eq.1) then
                    icode = 1
                  elseif (ic4.eq.mch4) then
                    icode = 2
                  endif
                  if( lmpi .AND. iproc_id .GT. 0 ) then
                    call mpi_feedback(igrd0(igen),iparnt(igen),icode)
                  else if( .NOT. lmpi) then
                    call aggr00(igrd0(igen),iparnt(igen),icode)
                  endif
c
  80            continue
c
c-----Perform chemistry for generation 3
c
                igen=3
                igrd = igrd0(igen)
                call newgrid(igrd)
                call uptime(timec(igrd),datec(igrd),deltat(igrd))
                if( ipigflg .NE. 0 .AND. (.NOT. lmpi 
     &                                  .OR. iproc_id .GT. 0) ) 
     &                                       call pigevol(igrd,iproc_id)
                if( ipigflg .NE. 0 .AND. lmpi ) then
                     call nodes_pig_pass(igrd,numprocs,iproc_id)
                endif 
                if( .NOT. lmpi .OR. iproc_id .GT. 0 ) then
                   call chemrxn(igrd,iproc_id)
                   call fgavrg(iproc_id,igrd)
                endif
  89          continue
              icode = 3
              if (mch3.eq.1) then
                icode = 0
              elseif (ic3.eq.1) then
                icode = 1
              elseif (ic3.eq.mch3) then
                icode = 2
              endif
              if( lmpi .AND. iproc_id .GT. 0 ) then
                 call mpi_feedback(igrd0(igen),iparnt(igen),icode)
              else if( .NOT. lmpi) then
                 call aggr00(igrd0(igen),iparnt(igen),icode)
              endif
c
  90        continue
c
c-----Perform chemistry for generation 2
c
            igen=2
            igrd = igrd0(igen)
            call newgrid(igrd)
            call uptime(timec(igrd),datec(igrd),deltat(igrd))
            if( ipigflg .NE. 0 .AND. (.NOT. lmpi .OR. iproc_id .GT. 0) ) 
     &                                       call pigevol(igrd,iproc_id)
            if (ipigflg .NE. 0 .AND. lmpi) then
                 call nodes_pig_pass(igrd,numprocs,iproc_id)
            endif 
            if( .NOT. lmpi .OR. iproc_id .GT. 0 ) then
               call chemrxn(igrd,iproc_id)
               call fgavrg(iproc_id,igrd)
            endif
  99      continue
          icode = 3
          if (mch2.eq.1) then
            icode = 0
          elseif (ic2.eq.1) then
            icode = 1
          elseif (ic2.eq.mch2) then
            icode = 2
          endif
          if( lmpi .AND. iproc_id .GT. 0 ) then
             call mpi_feedback(igrd0(igen),iparnt(igen),icode)
          else if( .NOT. lmpi) then
             call aggr00(igrd0(igen),iparnt(igen),icode)
          endif
c
 100  continue
c
      call newgrid(1)
c
      return
      end

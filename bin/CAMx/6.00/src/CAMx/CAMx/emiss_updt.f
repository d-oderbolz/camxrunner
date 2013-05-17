      subroutine emiss_updt(emstim,emsdate,numprocs,iproc_id)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use filunit
      use camxcom
      use camxfld
      use grid
      use chmstry
      use tracer
c
      implicit none
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    This routine reads is reponsible for updating the emissions arrays.
c    It will call the routines needed to read the emissions files for
c    the regular model as well as any Probing Tools. If MPI is activated,
c    it will then call the routines to pass the data to the computational 
c    slices.
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       CAMX
c    Subroutines called:
c       READPT
c       RDPTRT
c       READPTSA
c       RDPTDDM
c       FLUSH
c       READAR
c       EMASSIGN
c       RDARRT
c       READARSA
c       RDARDDM
c       MPI_BARRIER
c       NODES_EMISS
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
      include 'mpif.h'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real    :: emstim
c
      integer :: emsdate
      integer :: numprocs
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: iproc_id
      integer :: ierr
      integer :: igrd
      integer :: ip
      integer :: ic
c
      real    :: whr
      real    :: wmn
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      if (iproc_id .EQ. 0) then
         if (lptsrc) then
            write(*,'(a20,$)') 'readpt ......'
            call readpt()
c
c======================== Source Apportion Begin =======================
c
c  --- call routine to read the points source emissions files
c      and load the tracer emission arrays ---
c
            if (ltrace) then
               if (tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) then
                  call rdptrt(date,time)
               else
                  call readptsa(date,time)
               endif
            endif
c
c========================= Source Apportion End ========================
c
c============================= DDM Begin ===============================
c
c  --- call routine to read the points source emissions files
c      and load the tracer emission arrays ---
c
            if( lddm .OR. lhddm ) then
               call rdptddm(date,time)
            endif
c
c============================= DDM End =================================
c
            write(*,'(a)') '   Done'
            call flush(6)
         endif
      endif
c
      if (iproc_id .EQ. 0) then
         if (larsrc) then
c
c  --- Read area emissions if data is available ---
c
            write(*,'(a20,$)') 'readar ......'
            do igrd = 1,ngrid
               if (iarem(igrd) .GT. 0) then
                  call readar(igrd,ncol(igrd),nrow(igrd),iarem(igrd),
     &                    iout,aremis(iptrem(igrd)),narspc(igrd),nspec)
               else
c
c  --- Otherwise assign values from the parent ---
c
                  do ip=1,ngrid
                     do ic = 1,nchdrn(ip)
                        if (igrd .EQ. idchdrn(ic,ip)) then
                           write(iout,'(a40,f7.0,i8.5,a,i3)')
     &                          'Assigning emissions from parent grid',
     &                                           time, date,' grid',igrd
                           call emassign(ncol(ip),nrow(ip),i1(igrd),
     &                                  j1(igrd),nmesh(igrd),ncol(igrd),
     &                                  nrow(igrd),nspec,
     &                                  aremis(iptrem(ip)),
     &                                  aremis(iptrem(igrd))           )
                        endif
                     enddo
                  enddo
               endif
            enddo
c
c======================== Source Apportion Begin =======================
c
c  --- call routine to read the emissions files and load
c      the tracer emission arrays ---
c
            if (ltrace) then
               do igrd=1,ngrid
                  if (tectyp .EQ. RTRAC .OR. tectyp .EQ. RTCMC) then
                     if (ltemfl(igrd,1)) then
                        call rdarrt(igrd,date,time,ncol(igrd),
     &                       nrow(igrd),ntotsp,saemis(ipsa2d(igrd)))
                     else
c
c  --- assign values from the parent ---
c
                        do ip=1,ngrid
                           do ic = 1,nchdrn(ip)
                              if (igrd .EQ. idchdrn(ic,ip)) then
                                 write(iout,'(a40,f7.0,i8.5,a,i3)')
     &                           'Assigning RTRAC emissions from parent grid',
     &                                                  time, date,' grid',igrd
                                 call emassign(ncol(ip),nrow(ip),i1(igrd),
     &                                         j1(igrd),nmesh(igrd),ncol(igrd),
     &                                         nrow(igrd),ntotsp,
     &                                         saemis(ipsa2d(ip)),
     &                                         saemis(ipsa2d(igrd))            )
                              endif
                           enddo
                        enddo
                     endif
                  else
                     call readarsa(igrd,date,time,ncol(igrd),nrow(igrd),
     &                             ntotsp,deltax(1,igrd),deltay(igrd),
     &                             saemis(ipsa2d(igrd))                 )
                  endif 
               enddo
            endif
c
c========================= Source Apportion End ========================
c
c
c============================= DDM Begin ===============================
c
c  --- call routine to read the emissions files and load
c      the DDM tracer emission arrays ---
c
          if( lddm .OR. lhddm ) then
               do igrd=1,ngrid
                  if( .NOT. lddmcalc(igrd) ) cycle
                  call rdarddm(igrd,date,time,ncol(igrd),nrow(igrd),
     &                         ntotsp,saemis(ipsa2d(igrd))          )
               enddo
            endif
c
c============================= DDM End =================================
c
            write(*,'(a)') '   Done'
            call flush(6)
         endif
      endif
c
c  --- send message to computes nodes to sync up ---
c
      if (iproc_id .EQ. 0) then
         whr = aint(emstim/100.)
         wmn = amod(emstim,100.)
         emstim = 100.*(whr + aint((wmn + dtems)/60.)) + amod((wmn+dtems),60.)
         if (emstim .ge. 2400.) then
            emstim = emstim - 2400.
            emsdate = emsdate + 1
            if (MOD(emsdate,1000) .GT. 365) then
               if (MOD(INT(emsdate/1000),4) .EQ. 0) then
                  if (MOD(emsdate,1000) .EQ. 367)
     &               emsdate = (INT(emsdate/1000)+1)*1000 + 1
               else
                  emsdate = (INT(emsdate/1000)+1)*1000 + 1
               endif
            endif
         endif
      endif
c
c  --- call the routine to pass the emissions values to the nodes ---
c
      if (lmpi) then
         call MPI_Barrier( MPI_COMM_WORLD, ierr)
         call nodes_emiss(numprocs,iproc_id)
      endif
c
      end

      subroutine ahoz_updt(hazdate,haztim,ozndate,ozntim,
     &                     snodate,snotim,numprocs,iproc_id)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use filunit
      use ahomap
      use camxcom
      use chmstry
      use grid
c
      implicit none
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c     Output:  
c
c    Called by:
c       CAMX
c    Subroutines called:
c       READAHO
c       FLUSH
c       IASSGN2D
c       NODES_AHOZ
c
c     Copyright 1996 - 2009
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
      include 'flags.com'
      include 'mpif.h'
c
c========================= Probing Tool End ============================
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      real    :: haztim
      real    :: ozntim
      real    :: snotim
c
      integer :: hazdate
      integer :: ozndate
      integer :: snodate
      integer :: numprocs
      integer :: iproc_id
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: inpdate
      integer :: emsdate
      integer :: bnddate
      integer :: wrtdate
      integer :: enddate
      integer :: ip
      integer :: ic
      integer :: igrd
c
      real    :: inptim
      real    :: emstim
      real    :: bndtim
      real    :: wrttim
      real    :: endtim
      real    :: tcpu
      real    :: dtime
c
      character*20 :: version
      character*10 :: name
      character*8  :: chtime
      character*8  :: chdate
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- Do we need haze and ozone ---
c
      if (iproc_id .EQ. 0) then
         if (lchem .AND. idmech .NE. 10) then
c
c  --- Check if haze data are to be read ---
c
            if (date.eq.hazdate .and. abs(time-haztim) .lt. 0.01) then
               name = 'HAZE      '
               write(*,'(a20,$)') 'readaho (haze)'
               call readaho(ncol(1),nrow(1),time,date,ly2k,
     &                      name,haztim,hazdate,icdhaz     )
               tcpu = dtime(tarray2)
               write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
               call flush(6)
            endif
c
c  --- Check if ozone column data are to be read ---
c
            if (date .EQ. ozndate .and. abs(time-ozntim) .lt. 0.01) then
               name = 'OZONE COL '
               write(*,'(a20,$)') 'readaho (o3)..'
               call readaho(ncol(1),nrow(1),time,date,ly2k,
     &                      name,ozntim,ozndate,icdozn     )
               tcpu = dtime(tarray2)
               write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
               call flush(6)
            endif
         endif
c
c  --- Check if snow cover data are to be read ---
c
         if ((lchem .OR. ldry) .AND. idmech .NE. 10) then
            if (lrdsno .AND. date .EQ. snodate .AND. 
     &                                         ABS(time-snotim) .LT. 0.01) then
               name = 'SNOW      '
               write(*,'(a20,$)') 'readaho (snow)'
               call readaho(ncol(1),nrow(1),time,date,ly2k,
     &                      name,snotim,snodate,icdsno     )
               tcpu = dtime(tarray2)
               write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
               call flush(6)
            endif
c
c  --- Assign haze, ozone column, snow values for fine grids ---
c
            do ip = 1,ngrid
               do ic = 1,nchdrn(ip)
                  igrd = idchdrn(ic,ip)
                  call iassgn2d(ncol(ip),nrow(ip),i1(igrd),j1(igrd),
     &                          nmesh(igrd),ncol(igrd),nrow(igrd),
     &                          icdhaz(iptr2d(ip)),icdhaz(iptr2d(igrd)))
                  call iassgn2d(ncol(ip),nrow(ip),i1(igrd),j1(igrd),
     &                          nmesh(igrd),ncol(igrd),nrow(igrd),
     &                          icdozn(iptr2d(ip)),icdozn(iptr2d(igrd)))
                  call iassgn2d(ncol(ip),nrow(ip),i1(igrd),j1(igrd),
     &                          nmesh(igrd),ncol(igrd),nrow(igrd),
     &                          icdsno(iptr2d(ip)),icdsno(iptr2d(igrd)))
               enddo
            enddo
         endif
      endif
c
c  --- call the routine to send the data to the nodes ---
c
      if (lmpi) then
         call nodes_ahoz(numprocs,iproc_id)
      endif
c
      end

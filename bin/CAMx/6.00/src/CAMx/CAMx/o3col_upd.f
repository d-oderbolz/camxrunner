      subroutine o3col_updt(ozndate,ozntim,numprocs,iproc_id)
c
c-----------------------------------------------------------------------
c    Modules used:
c-----------------------------------------------------------------------
c
      use filunit
      use o3colmap
      use camxcom
      use chmstry
      use grid
c
      implicit none
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Argument descriptions:
c     Input:  
c       ozndate       Date to search in ozone column file
c       ozntim        Time to search in ozone column file
c       numprocs      Number of MPI slices
c       iproc_id      MPI process ID
c     Output:  
c       none
c
c    Called by:
c       CAMX
c    Subroutines called:
c       READO3COL
c       IASSGN2D
c       NODES_O3COL
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c        02/11/11   Moved snow panel to 2D met file
c        03/31/12   Reads only time-varying ozone column
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
      real    :: ozntim
      integer :: ozndate
      integer :: numprocs
      integer :: iproc_id
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer :: ip
      integer :: ic
      integer :: igrd
c
      character*10 :: name
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- Do we need ozone ---
c
      if (lchem .AND. idmech .NE. 10) then
c
c  --- Check if ozone column data are to be read ---
c
         if (date .EQ. ozndate .and. abs(time-ozntim) .lt. 0.01) then
            if (iproc_id .EQ. 0) then
               name = 'OZONE COL '
               write(*,'(a20,$)') '    reado3col ......'
               call reado3col(ncol(1),nrow(1),time,date,
     &                        name,ozntim,ozndate,icdozn     )
               write(*,'(a)') '   Done'
               call flush(6)
c
               do ip = 1,ngrid
                 do ic = 1,nchdrn(ip)
                   igrd = idchdrn(ic,ip)
                   call iassgn2d(ncol(ip),nrow(ip),i1(igrd),j1(igrd),
     &                          nmesh(igrd),ncol(igrd),nrow(igrd),
     &                          icdozn(iptr2d(ip)),icdozn(iptr2d(igrd)))
                 enddo
               enddo
            endif
c
c  --- call the routine to send the data to the nodes ---
c
            if (lmpi) then
                 call nodes_o3col(numprocs,iproc_id)
             endif
         endif
      endif
c
      end

      subroutine getdepth(nvar,ncols,nrows,nlays,ibgdhp,idtnow,btimhp,
     &                    timnow,iunit,height,depth)
      use filunit
c
c----CAMx v6.00 130506
c
c     GETDEPTH reads the 3D met file until to the current hour,
c     calculates layer depth, and passes back the data for OSAT calculations
c 
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c           
c     Modifications: 
c        11/06/01  Added Y2K flag
c        01/04/11  Revised for new met input format
c 
c     Input arguments: 
c        nvar                number of variables on 3D met file
c        ncols               number of columns
c        nrows               number of rows
c        nlays               number of layers
c        idtnow              current date (YYJJJ)
c        timnow              current time (HH.HH)
c        iunit               height/pressure file unit number
c             
c     Output arguments: 
c        ibgdhp              date from height/pressure file (YYJJJ)
c        btimhp              time from height/pressure file (HH.HH) 
c        height              array of gridded layer heights (m)
c        depth               array of gridded layer depths (m)
c             
c     Routines Called: 
c        None
c             
c     Called by: 
c        RDSUMBC
c
      implicit none
      include "camx.prm"
c
      integer nvar,ncols,nrows,nlays,ibgdhp,idtnow,iunit
      real btimhp,timnow
      real height(ncols,nrows,nlays),depth(ncols,nrows,nlays)
c
      character*4 namvar(10)
      character*10 namevar
      integer idum,n,k,i,j,m
      real arr3d(MXCELLS,MXCELLS,MXLAYER)
      logical lfound
c
c-----Read Height pressure file to current time/date
c
  333 continue
      if( (ibgdhp .EQ. idtnow .AND. btimhp .LT. timnow) .OR.
     &     ibgdhp .LT. idtnow ) then
         lfound = .false.
         read(iunit,end=7001,err=7001) ibgdhp,btimhp
         do n = 1,nvar
           do k = 1,nlays
             read(iunit,ERR=7001) idum,(namvar(m),m=1,10),
     &                            ((arr3d(i,j,k),i=1,ncols),j=1,nrows)
           enddo
           write(namevar,'(10a1)') (namvar(m),m=1,10)
           if (namevar.eq.'ZGRID_M') then
             lfound = .true.
             do k = 1,nlays
               do j = 1,nrows
                 do i = 1,ncols
                   height(i,j,k) = arr3d(i,j,k)
                 enddo
               enddo
             enddo
           endif
         enddo
         if( idtnow .EQ. ibgdhp .AND. btimhp .LT. timnow ) goto 333
         if( ibgdhp .LT. idtnow ) goto 333
c
c-----Calculate the depths from the heights
c
         if (.not.lfound) goto 7002
         do k = 1,nlays
           do j = 1,nrows
             do i = 1,ncols
               depth(i,j,k) = height(i,j,k)
               if( k .GT. 1 ) 
     &            depth(i,j,k) = height(i,j,k) - height(i,j,k-1)
             enddo
           enddo
         enddo
      endif
c
      return
c
 7001 continue
      write(iout,'(//,a)') 'ERROR in GETDEPTH:'
      write(iout,'(/,1X,A)') 'Reading 3D Met file'
      call camxerr()
c
 7002 continue
      write(iout,'(//,a)') 'ERROR in GETDEPTH:'
      write(iout,'(/,1X,A)') 'Did not find ZGRID_M in 3D met file'
      call camxerr()
      end

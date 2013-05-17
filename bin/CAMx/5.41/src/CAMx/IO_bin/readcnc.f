      subroutine readcnc
      use filunit
      use grid
      use chmstry
      use bndary
      use camxfld
      use camxcom
c 
c     
c----CAMx v5.41 121109
c
c     READCNC operates in two modes:
c        1) when LRSTRT = F, reads and cycles through the AIRQUALITY file
c           to current time/date, initializes coarse grid concentrations,
c           and maps concentrations to any nested grids
c        2) when LRSTRT = T, reads and cycles through the coarse and
c           fine grid INSTANT files (from previous run) to current
c           time/date, and initializes all grid concentrations directly
c 
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c           
c     Modifications: 
c        1/7/99    Added conditional to call rdfgcon for only if there are nests
c      10/31/01    Improved assignment of coarse grid IC's based on whether
c                  this is a restart or not
c      04/17/03    Changed the logic so it no longer requires such a large 
c                  local array. It was causing problems with stack size on the 
c                  latest PGI compiler.
c      04/20/04    Fixed bug in date/time loop
c      11/4/09     Removed input top concentrations
c  
c     Input arguments: 
c        none
c             
c     Output arguments: 
c        none
c             
c     Routines Called: 
c        INTRPCNC
c        RDFGCON
c             
c     Called by: 
c        CAMx
c
      include 'camx.prm'
      include 'flags.inc'
c
      character*4  icspec(10)
c
      real cinit(MXCELLS,MXCELLS)
c
c-----Entry point
c
      iunit = iic
      if (lrstrt) iunit = irstc
      nx = ncol(1)
      ny = nrow(1)
      nz = nlay(1)
c
c-----Read through coarse grid concentration records until current time/date
c
 100  read(iunit,end=900) idat1,tim1,idat2,tim2
      tim1 = 100.*tim1
      tim2 = 100.*tim2
      write(iout,'(a40,2(f7.0,i8.5))') 
     &      'Read initial condition file at ',tim1,idat1,tim2,idat2
      call flush(iout)
c
      if ((idat1.lt.date .or. (idat1.eq.date .and. tim1.le.time)) .and.
     &    (idat2.gt.date .or. (idat2.eq.date .and. tim2.ge.time))) then
        do lread = 1,nicspc
          do k = 1,nz
            read(iunit) idum,(icspec(n),n=1,10), 
     &                  ((cinit(i,j),i=1,nx),j=1,ny) 
            do 90 lmod = 1,nspec
              lic = licmap(lmod,1)
              if( lic .NE. lread ) goto 90
              do j = 1,ny
                do i = 1,nx
                  n3d = i + nx*(j - 1) + nx*ny*(k - 1)
                  n4d = n3d + nx*ny*nz*(lmod - 1)
                  if (lrstrt) then
                    conc(n4d) = cinit(i,j)
                  else
                    conc(n4d) = bdnl(lmod)
                    conc(n4d) = amax1(conc(n4d),cinit(i,j))
                  endif
                enddo
              enddo
  90        continue
          enddo
        enddo
      else
        do lread = 1,nicspc
          do k = 1,nz
            read(iunit)
          enddo
        enddo
        goto 100
      endif
c
c-----If this is not a restart, interpolate coarse grid concentrations
c     to all fine grids
c
      if (.not.lrstrt) then
        if (ngrid.gt.1) then
          do ip = 1,ngrid
            do ic = 1,nchdrn(ip)
              ig = idchdrn(ic,ip)
              call intrpcnc(nspec,ncol(ip),nrow(ip),nlay(ip),i1(ig),
     &                      j1(ig),nmesh(ig),nmshv(1,ig),ncol(ig),
     &                      nrow(ig),nlay(ig),conc(iptr4d(ip)),
     &                      conc(iptr4d(ig)) )
            enddo
          enddo
        endif
c
c-----Convert from ppm to umol/m3
c
        do igrd = 1,ngrid
          nx = ncol(igrd)
          ny = nrow(igrd)
          nz = nlay(igrd)
          do l = 1,nspec
            do k = 1,nz
              do j = 1,ny
                do i = 1,nx
                  n3d = i + nx*(j - 1) + nx*ny*(k - 1)
                  n4d = n3d + nx*ny*nz*(l - 1)
                  if (l.le.ngas) then
                    convfac = densfac*273./tempk(iptr3d(igrd)-1+n3d)*
     &                        press(iptr3d(igrd)-1+n3d)/1013.
                  else
                    convfac = 1.
                  endif
                  conc(iptr4d(igrd)-1+n4d) = convfac*
     &                     AMAX1( bdnl(l), conc(iptr4d(igrd)-1+n4d) )
                enddo
              enddo
            enddo
          enddo
        enddo
      else
c
c-----Otherwise, read fine grid concentrations from fine grid restart file
c
        if( ngrid .GT. 1 ) call rdfgcon(idat1,tim1)
      endif
      goto 999
c
c-----End of IC file reached
c
 900  write(iout,'(//,a)') 'ERROR in READCNC:'
      write(iout,*)'End of IC file'
      write(iout,*)'Make sure initial condition file contains the ',
     &                                   'simulation beginning hour.'
      call camxerr()
c
 999  continue
c
      return
      end

      subroutine wrtsmp(lrtrac,tim2,idat2,iunit,nox,noy,noz,nsamp,
     &                  cncfld)
      use camxcom
      use chmstry
      use tracer

c
c----CAMx v5.10 090918
c 
c     WRTSMP writes average concentration fields from a PiG sampling grid
c 
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c           
c     Modifications: 
c        8/2/05       Generalized to output regular model species or
c                     RTRAC species.
c 
c     Input arguments:
c        tim2                output time (HHMM)
c        idat2               output date (YYJJJ)
c        iunit               output unit
c        nox                 number of cells in x-direction
c        noy                 number of cells in y-direction
c        noz                 number of layers
c        nsamp               number of species
c        cncfld              concentration field to output (ppm or ug/m3)
c             
c     Output arguments: 
c        none
c             
c     Routines Called: 
c        none
c             
c     Called by: 
c        CAMx
c 
      implicit none
      include 'camx.prm'
c
      integer MXOUT
      parameter (MXOUT = MAX(MXSPEC,MXTRSP))
c
      logical lrtrac
      integer idat2,iunit,nox,noy,noz,nsamp
      real tim2,cncfld(nox,noy,noz,nsamp)
      integer nseg,idat1,i,j,k,l,n
      real etim,btim
      character*4 ispec(10,MXOUT)
c
      data nseg /1/
c
c-----Entry point
c
c-----Determine time/date range
c
      idat1 = idat2 
      etim = AINT(ANINT(tim2)/100.) + amod(ANINT(tim2),100.)/60.
      btim = ANINT( 1000*(etim - ANINT(dtout)/60.) )/1000.
      if (btim.lt.0.) then 
        btim = btim + 24. 
        idat1 = idat1 - 1 
      endif 
      write(iunit) idat1,btim,idat2,etim
c
c-----Load species names and write date stamp
c
      if (lrtrac) then
        do l = 1,nsamp
          read(ptname(l),'(10a1)') (ispec(n,l),n=1,10)
        enddo
      else
        do l = 1,nsamp
          read(spname(lavmap(l)),'(10a1)') (ispec(n,l),n=1,10)
        enddo
      endif
c
c-----Write gridded concentration field
c
      do l = 1,nsamp
        do k = 1,noz
          write(iunit) nseg,(ispec(n,l),n=1,10),
     &                 ((cncfld(i,j,k,l),i=1,nox),j=1,noy)
        enddo
      enddo
      call flush(iunit)
      return
      end

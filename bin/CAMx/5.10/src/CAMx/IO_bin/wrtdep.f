      subroutine wrtdep(tim2,idat2,iunit,nox,noy,nsptmp,nspdry,
     &                  vdep,depfld)
      use grid
      use chmstry
      use camxcom
c
c----CAMx v5.10 090918
c 
c     WRTDEP writes deposition fields.
c 
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c           
c     Modifications: 
c        none
c 
c     Input arguments:
c        tim2                output time (HHMM)
c        idat2               output date (YYJJJ)
c        iunit               output unit
c        nox                 number of cells in x-direction
c        noy                 number of cells in y-direction
c        nsptmp              number of dep field species
c        nspdry              number of dry dep species
c        vdep                dry deposition velocities
c        depfld              deposition field to output
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
      include 'camx.prm'
c
      real depfld(nox,noy,nsptmp),vdep(nox,noy,nspdry)
c
      character*4 ispec(10,4*MXSPEC)
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
c
c-----Write gridded deposition field
c
      do l = 1,4*ndepspc
        read(depsp(l),'(10a1)') (ispec(n,l),n=1,10)
      enddo
      write(iunit) idat1,btim,idat2,etim
      do l = 1,ndepspc
        ll = ldepmap(l)
        write(iunit) nseg,(ispec(n,l),n=1,10),
     &               ((vdep(i,j,ll),i=1,nox),j=1,noy)
      enddo
      do l = 1,nsptmp
        ll = l + ndepspc
        write(iunit) nseg,(ispec(n,ll),n=1,10),
     &               ((depfld(i,j,l),i=1,nox),j=1,noy)
      enddo
c
      return
      end

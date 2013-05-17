      subroutine wrtsrfrt(tim1,idat1,iunit,nox,noy,nspdep,solmas,vegmas)
      use grid
      use chmstry
      use camxcom
      use tracer
c
c----CAMx v6.00 130506
c 
c     WRTSRFRT writes surface mass fields from RTRAC
c 
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c           
c     Modifications: 
c        none
c 
c     Input arguments:
c        tim1                output time (HHMM)
c        idat1               output date (YYJJJ)
c        iunit               output unit
c        nox                 number of cells in x-direction
c        noy                 number of cells in y-direction
c        nspdep              number of dep field species
c        solmas              surface soil mass field to output
c        vegmas              surface veg mass field to output
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
      real solmas(nox,noy,nspdep)
      real vegmas(nox,noy,nspdep)
c
      character*4 ispec(10)
c
      data nseg /1/
c
c-----Entry point
c
c-----Determine time/date range
c
      idat2 = idat1
      tim2 = tim1 + 10.0
      btim = AINT(ANINT(tim1)/100.) + amod(ANINT(tim1),100.)/60.
      etim = AINT(ANINT(tim2)/100.) + amod(ANINT(tim2),100.)/60.
      if (etim.gt.24.) then
        etim = etim - 24.
        idat2 = idat2 + 1
        if( MOD(idat2,1000) .GT. 365 ) then
            if( MOD(INT(idat2/1000),4) .EQ. 0 ) then
               if( MOD(idat2,1000) .EQ. 367 )
     &                     idat2 = (INT(idat2/1000)+1)*1000 + 1
            else
               idat2 = (INT(idat2/1000)+1)*1000 + 1
            endif
        endif
      endif 
c
c-----Write the time stamp ---
c
      write(iunit) idat1,btim,idat2,etim
c
c-----Write gridded surface mass field
c
      do l = 1,ntotsp
        ispec(1) = 'S'
        read(ptname(l)(1:9),'(9a1)') (ispec(n),n=2,10)
        write(iunit) nseg,(ispec(n),n=1,10),
     &               ((solmas(i,j,l),i=1,nox),j=1,noy)
      enddo
      do l = 1,ntotsp
        ispec(1) = 'V'
        read(ptname(l)(1:9),'(9a1)') (ispec(n),n=2,10)
        write(iunit) nseg,(ispec(n),n=1,10),
     &               ((vegmas(i,j,l),i=1,nox),j=1,noy)
      enddo
c
      return
      end

      subroutine wrtdepsa(tim2,idat2,iunit,nox,noy,nspdep,
     &                                               dryfld,wetfld)
      use grid
      use chmstry
      use camxcom
      use tracer
c
c----CAMx v5.10 090918
c 
c     WRTDEPSA writes deposition fields for the Source Apportionment
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
c        nspdep              number of dep field species
c        dryfld              dry deposition field to output
c        wetfld              dry deposition field to output
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
      real dryfld(nox,noy,nspdep)
      real wetfld(nox,noy,nspdep)
c
      character*4 ispec(10,MXTRSP)
      character*1 dstring, wstring, underscore
c
      data nseg /1/
      data dstring /'D'/
      data wstring /'W'/
      data underscore /'_'/
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
c-----Write the time stamp ---
c
      write(iunit) idat1,btim,idat2,etim
c
c-----Write gridded dry deposition field
c
      do l = 1,ntotsp
        do n=1,9
          if( ptname(l)(n:n) .NE. ' ' ) then
             read(ptname(l)(n:n),'(a1)') ispec(n,l)
          else
             read(underscore,'(a1)') ispec(n,l)
          endif
        enddo
        read(dstring,'(a1)') ispec(10,l)
      enddo
      do l = 1,ntotsp
        if( loutsa(l) ) then
           write(iunit) nseg,(ispec(n,l),n=1,10),
     &               ((dryfld(i,j,l),i=1,nox),j=1,noy)
        endif
      enddo
c
c-----Write gridded wet deposition field
c
      do l = 1,ntotsp
        do n=1,9
          if( ptname(l)(n:n) .NE. ' ' ) then
             read(ptname(l)(n:n),'(a1)') ispec(n,l)
          else
             read(underscore,'(a1)') ispec(n,l)
          endif
        enddo
        read(wstring,'(a1)') ispec(10,l)
      enddo
      do l = 1,ntotsp
        if( loutsa(l) ) then
           write(iunit) nseg,(ispec(n,l),n=1,10),
     &               ((wetfld(i,j,l),i=1,nox),j=1,noy)
        endif
      enddo
c
      return
      end

      subroutine rdtmpr(idate,itime,ncol,nrow,nlay)
c
c----------------------------------------------------------------------- 
c 
c     RDTMPR reads the temperature file for the coarse grid until the 
c     current time/date has been reached. It then interpolates coarse
c     grid temperature to all fine grids.
c     Argument description: 
c      Inputs:
c        idate  I  date
c        itime  I  time
c        ncol   I  number of columns
c        nrow   I  number of rows
c        nlay   I  number of layers
c      Outputs: 
c                          
c----------------------------------------------------------------------- 
c   Include files: 
c----------------------------------------------------------------------- 
c 
      include 'camxtrct.inc' 
c 
c----------------------------------------------------------------------- 
c   Argument declaration: 
c----------------------------------------------------------------------- 
c 
      integer*4 idate,itime,ncol,nrow,nlay 
c 
c----------------------------------------------------------------------- 
c   External functions: 
c----------------------------------------------------------------------- 
c 
c----------------------------------------------------------------------- 
c    Local variables: 
c----------------------------------------------------------------------- 
c 
      integer*4 ihr, idt
      integer*4 i, j, k, igrd
      real*4 hr
c 
c----------------------------------------------------------------------- 
c    Entry point:  
c----------------------------------------------------------------------- 
c
c  --- Read temperature file for coarse grid ---
c
 100  continue
      read(IORTMP) hr,idt
      do k = 1,nlay
        read(IORTMP) hr,idt,((temp(i,j,k,0),i=1,ncol),j=1,nrow)
      enddo
      ihr = int(hr/100.)
      if (idt.gt.100000) call juldate(idt)
      write(IOWSTD,'(1X,A,T30,2I10)') 'Read temperature file:',idt,ihr
c             
      if (idt.lt.idate .or. (idt.eq.idate .and. ihr.lt.itime)) goto 100 
      if (idt.gt.idate .or. (idt.eq.idate .and. ihr.gt.itime)) then 
        write(IOWSTD,*)'Past current time/date ',itime,idate 
        stop  
      endif 
c
c  --- Interpolate temperature data to each fine grid
c
      do igrd = 1,ngrid
        call interp2d(nxgrid(0),nygrid(0),nlgrid(0),iclbeg(igrd),
     &                jclbeg(igrd),mshfac(igrd),nxgrid(igrd),
     &                nygrid(igrd),temp(1,1,1,0),temp(1,1,1,igrd)) 
        call interpv(nxgrid(0),nygrid(0),nlgrid(0),nxgrid(igrd), 
     &               nygrid(igrd),nlgrid(igrd),mshfac(igrd), 
     &               nmshv(1,igrd),iclbeg(igrd),jclbeg(igrd), 
     &               height(1,1,1,0),height(1,1,1,igrd),
     &               temp(1,1,1,igrd))
      enddo 
c
c----------------------------------------------------------------------- 
c   Return point: 
c----------------------------------------------------------------------- 
c 
 9999 continue 
      return 
      end 

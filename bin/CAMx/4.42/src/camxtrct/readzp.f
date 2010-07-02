      subroutine readzp(igrd,lfirst,idate,itime,ncol,nrow,nlay)
c 
c----------------------------------------------------------------------- 
c 
c   READZP reads the height-pressure file for the given grid until the 
c   current time/date has been reached
c     Argument description: 
c      Inputs:
c        igrd   I  grid number
c        lfirst L  flag to indicate first hour of extraction
c        idate  I  date to read
c        itime  I  hour to read
c        ncol   I  number of cells in x-direction
c        nrow   I  number of cells in y-direction
c        nlay   I  number of layers
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
      integer*4 igrd, idate, itime, ncol, nrow, nlay
      logical*4 lfirst
c 
c----------------------------------------------------------------------- 
c    Local variables: 
c----------------------------------------------------------------------- 
c 
      integer*4 idt,ihr
      integer*4 i, j, k,ihtp,ic,jc,kold,kc,kf
      real*4 diff,hr
c 
c----------------------------------------------------------------------- 
c    Entry point: 
c----------------------------------------------------------------------- 
c
      ihtp = IORZP + igrd
 100  do k = 1,nlay
        read(ihtp) hr,idt,((height(i,j,k,igrd),i=1,ncol),j=1,nrow)
        read(ihtp) hr,idt,((press(i,j,k,igrd),i=1,ncol),j=1,nrow)
      enddo
      ihr = int(hr/100.)
      if ( idt .GT. 100000. ) call juldate (idt)
      write(IOWSTD,'(1X,A,T30,2I10)') 
     &         'Read height/pressure file:',idt,ihr
c
      if (idt.lt.idate .or. (idt.eq.idate .and. ihr.lt.itime)) goto 100
      if (idt.gt.idate .or. (idt.eq.idate .and. ihr.gt.itime)) then
        write(IOWSTD,*)'Past current time/date ',itime,idate
        stop
      endif
c
c  --- calculate layer depth ---
c
      do k = 1,nlay
        do j = 1,nrow
          do i = 1,ncol
            if (k.eq.1) then
              depth(i,j,k,igrd) = height(i,j,k,igrd) 
            else
              depth(i,j,k,igrd) = height(i,j,k,igrd) - 
     &                            height(i,j,k-1,igrd)
            endif
          enddo
        enddo
      enddo
c
c  ---- determine vertical meshing factor ---
c
      if (igrd.eq.0 .or. .not.lfirst) goto 9999
c
      ic = iclbeg(igrd)
      jc = jclbeg(igrd)
      kold = 0
      write(IOWSTD,'(/,A,i3)')'VERTICAL MESHING for GRID',igrd
      do 10 kc = 1,nlgrid(0)
        do kf = kold+1,nlgrid(igrd)
          diff = abs(height(2,2,kf,igrd) - height(ic,jc,kc,0))/
     &           height(ic,jc,kc,0)
          if (diff.lt.0.05) then
            nmshv(kc,igrd) = kf - kold
            write(IOWSTD,'(A,I3,A,I3,A,I3)')'COARSE Layer',kc,
     &                   ' includes FINE Layers',kold+1,' to',kf
            kold = kf
            goto 10
          endif
        enddo
 10   continue
      write(IOWSTD,*)
c 
c----------------------------------------------------------------------- 
c   Return point: 
c----------------------------------------------------------------------- 
c 
 9999 continue 
      return 
      end 

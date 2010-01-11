c     output fine concentration

      subroutine outfcbin

      include 'uamvbinr.inc'

      read(InNum,5) msg
      read(InNum,6) numfin, nspc
      read(InNum,7) ((mspec(i,l),i=1,10),l=1,nspc)

      write(OutNum) msg
      write(OutNum) numfin, nspc
      write(OutNum) ((mspec(i,l),i=1,10),l=1,nspc)
    5 format(a80)
    6 format(2i10)
    7 format(60a1)
      if (nspc .gt. mxspc) then
        write(*,*) 'Maximum number of species exceeded.'
        write(*,*) 'No. of species on file = ', nspc
        write(*,*) 'Maximum no. of species is ', mxspc
        stop
      endif
c
c   loop over fine grids, reading & writing grid definition
c
      do 4881 ifine=1,numfin
        read(InNum,8) ixfb(ifine), jyfb(ifine),
     $               ixfe(ifine), jyfe(ifine), nhf(ifine),
     $               nvf(ifine), nxf(ifine), nyf(ifine), 
     $               nzf(ifine), ifgptr(ifine), ifglvl(ifine)
        if (nxf(ifine) .gt. mxx .or. nyf(ifine) .gt. mxy) then
          write(*,*) 'Maximum region dimension exceeded.'
          write(*,*) 'File x by y size is ', nxf(ifine),
     $               ' by ', nyf(ifine)
          write(*,*) 'Max x by y size is ', mxx, ' by ', mxy
          stop
        endif
        write(OutNum) ixfb(ifine), jyfb(ifine),
     $               ixfe(ifine), jyfe(ifine), nhf(ifine),
     $               nvf(ifine), nxf(ifine), nyf(ifine), 
     $               nzf(ifine), ifgptr(ifine), ifglvl(ifine)
    8   format(6i10)
 4881 continue

   10 continue
      read(InNum,20,end=100,err=200) time,ndate
      write(OutNum,err=210) time,ndate
      write(ErrNum,20) time,ndate
   20 format(F10.3,1X,I10)
      do 60 ifine=1,numfin
        do 50 l = 1, nspc
          do 40 k = 1, nzf(ifine)
              read(InNum,30,end=100,err=200) 
     &            ((emob(i,j), i=1,nxf(ifine)),j=1,nyf(ifine))
              write(OutNum,err=210)  
     &            ((emob(i,j), i=1,nxf(ifine)),j=1,nyf(ifine))
   30         format(9E14.7)
   40     continue
   50   continue
   60 continue
      goto 10

  100 continue
      return

  200 continue
      write(ErrNum, 220) 'Error reading outfc file'
      write(*, 220) 'Error reading outfc file'
      stop

  210 continue
      write(ErrNum, 220) 'Error writing outfc file'
      write(*, 220) 'Error writing outfc file'
      stop

  220 format(A)

      end

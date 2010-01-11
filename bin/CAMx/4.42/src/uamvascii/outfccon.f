c     output fine concentration

      subroutine outfccon

      include 'uamvascii.inc'

      read(InNum) msg
      read(InNum) numfin, navspc
      read(InNum) ((mspec(i,l),i=1,10),l=1,navspc)

      write(OutNum,5) msg
      write(OutNum,6) numfin, navspc
      write(OutNum,7) ((mspec(i,l),i=1,10),l=1,navspc)
    5 format(a80)
    6 format(2i10)
    7 format(60a1)
      if (navspc .gt. mxspc) then
        write(*,*) 'Maximum number of species exceeded.'
        write(*,*) 'No. of species on file = ', navspc
        write(*,*) 'Maximum no. of species is ', mxspc
        stop
      endif
c
c   loop over fine grids, reading & writing grid definition
c
      do 4881 ifine=1,numfin
        read(InNum) ixfb(ifine), jyfb(ifine),
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
        write(OutNum,8) ixfb(ifine), jyfb(ifine),
     $               ixfe(ifine), jyfe(ifine), nhf(ifine),
     $               nvf(ifine), nxf(ifine), nyf(ifine), 
     $               nzf(ifine), ifgptr(ifine), ifglvl(ifine)
    8   format(6i10)
 4881 continue

   10 continue
      read(InNum,end=100,err=200) time,ndate
      write(OutNum,20,err=210) time,ndate
      write(ErrNum,20,err=210) time,ndate
   20 format(F10.3,1X,I10)
      do 60 ifine=1,numfin
        do 50 l = 1, navspc
          do 40 k = 1, nzf(ifine)
              read(InNum,end=100,err=200) 
     &            ((emob(i,j), i=1,nxf(ifine)),j=1,nyf(ifine))
              write(OutNum,30,err=210)  
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

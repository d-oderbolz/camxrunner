c     output fine average
     
      subroutine outfabin

      include 'uamvbinr.inc'

      read(InNum,5) msg
      read(InNum,6) numfin, navspc
      read(InNum,7) (mavspc(l),l=1,navspc)

      write(OutNum) msg
      write(OutNum) numfin, navspc
      write(OutNum) (mavspc(l),l=1,navspc)
    5 format(a80)
    6 format(2i10)
    7 format(6a10)
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
      write(ErrNum,*) ' Time/date:',time, ndate
      write(OutNum) time,ndate
   20 format(F10.4,1X,I10)
      do 50 ifine=1,numfin
        do 40 l = 1, navspc
          read(InNum,30,end=100,err=200) 
     &        ((emob(i,j), i=1,nxf(ifine)),j=1,nyf(ifine))
          write(OutNum,err=210)  
     &        ((emob(i,j), i=1,nxf(ifine)),j=1,nyf(ifine))
   30     format(9E14.7)
   40   continue
   50 continue
      goto 10

  100 continue
      return

  200 continue
      write(ErrNum, 220) 'Error reading outfa file'
      write(*, 220) 'Error reading outfa file'
      stop

  210 continue
      write(ErrNum, 220) 'Error writing outfa file'
      write(*, 220) 'Error writing outfa file'
      stop

  220 format(A)

      end

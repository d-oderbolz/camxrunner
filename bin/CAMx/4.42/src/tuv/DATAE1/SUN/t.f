      open(unit=10, file='demerj.flx',status='old')

      nhead = 22
      do i = 1, nhead
         read(10,*)
      enddo

      do i = 1, 48
         read(10,*) x1, x2, y1
         write(20,*) x1, y1/(x2-x1)
         write(20,*) x2, y1/(x2-x1)
      enddo

      end

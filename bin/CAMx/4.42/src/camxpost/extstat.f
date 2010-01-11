      program extstat

c-----Quicky hack to pull out important stats from the output of CAMxSTAT

      integer ihrb(20)
      real uppa(20),appa(20),eppa(20),bnavg(20),enavg(20)
      character*80 ipath

c-----Read and open I/O files

      read(*,'(20x,a)') ipath
      open(11,file=ipath)
      write(*,*) 'Opened extraction file: ', ipath
      read(*,'(20x,i10)') nday

      do m = 1,nday
        read(*,'(20x,a)') ipath
        open(10,file=ipath,status='old')
        write(*,*) 'Opened input STAT file: ', ipath

c-----Read header of Peak Prediction stats

        do n = 1,10
          read(10,*)
        enddo
        read(10,9030) uppa(m)
 9030   format(55x,f10.1)
        do n = 1,5
          read(10,*)
        enddo
 100    read(10,'(a)') ipath
        if (ipath(1:28).eq.'   Peak Bias (unpaired time)') then
          read(ipath,'(60x,f10.1,i10)') appa(m),ihrb(m)
          read(10,'(60x,f10.1,i10)') eppa(m)
          do n = 1,14
            read(10,*)
          enddo
          goto 200
        else
          goto 100
        endif

c-----Read overall stats

 200    read(10,'(26x,f6.1)') bnavg(m)
        read(10,*)
        read(10,*)
        read(10,'(26x,f6.1)') enavg(m)
        close(10)
      enddo

c-----Write important stats to temporary file

      write(11,'(20f8.1)') (uppa(m),m=1,nday)
      write(11,'(20f8.1)') (appa(m),m=1,nday)
      write(11,'(20f8.1)') (eppa(m),m=1,nday)
      write(11,'(20i8)')   (ihrb(m),m=1,nday)
      write(11,'(20f8.1)') (bnavg(m),m=1,nday)
      write(11,'(20f8.1)') (enavg(m),m=1,nday)

      stop 
      end

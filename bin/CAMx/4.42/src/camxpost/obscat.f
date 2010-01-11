      program obscat

c-----OBSCAT concatenates a series of OBS files into a single file
c
c     NOTE: 
c         *assumes all input OBS files contain identical number of 
c          hour records and identical stations

      parameter(mxstn=100,mxhr=24,mxspc=10)
      character*4 mspec(10,mxspc)
      real xutm(mxstn),yutm(mxstn),oin(mxspc)
      character*80 ipath, statmsg, dummsg
      character*20 sitnam(mxstn),sitmax
      character*10 site(mxstn),atmp

      data zero /0./

c-----Read and open I/O files

      read(*,'(20x,a)') ipath
      open(9,file=ipath)
      write(*,*) 'Opened output OBS file: ', ipath
      read(*,'(20x,a)') statmsg
      write(*,*) statmsg
      read(*,'(20x,i10)') nfils

      do nf = 1,nfils
        read(*,'(20x,a)') ipath
        open(10+nf,file=ipath,status='old',err=901)
        write(*,*) 'Opened input OBS file: ', ipath
      enddo

c-----Read OBS files and write to new file; assume they contain
c     identical hour records and identical stations

      do nf = 1,nfils
        read(10+nf,'(a)') dummsg
        if (nf.eq.1) write(9,'(a)') statmsg
        read(10+nf,'(i5,1x,10a1)') nspec,((mspec(m,n),m=1,10),n=1,nspec)
        if (nf.eq.1) write(9,'(i5,1x,10a1)') 
     &               nspec,((mspec(m,n),m=1,10),n=1,nspec)
        read(10+nf,'(i5)') nhr
        if (nf.eq.1) write(9,'(i5)') nfils*nhr
        read(10+nf,'(i5)') nsite
        if (nf.eq.1) write(9,'(i5)') nsite
        do 5 ns = 1,nsite
          read(10+nf,'(a10,a20,2f10.0)') site(ns),sitnam(ns),
     &                                   xutm(ns),yutm(ns)
          if (nf.eq.1) write(9,'(a10,a20,2f10.3)') site(ns),sitnam(ns),
     &                                             xutm(ns),yutm(ns)
 5      continue
      enddo

      do ns = 1,nsite
        do nf = 1,nfils
          do nh = 1,nhr
            read(10+nf,8030) atmp,idat1,itim1,(oin(m),m=1,nspec)
            write(9,8031) atmp,idat1,itim1,(oin(m),m=1,nspec)
          enddo
        enddo
      enddo
 8030 format(a10,2x,i6,i2,10f10.0)
 8031 format(a10,2x,i6,i2.2,10f10.2)
      stop

 901  continue 
      write(*,*) 'OBS file not found' 
      write(*,'(a)') ipath 
      stop 
      end

      program extstat

c-----Quicky hack to pull out important stats from the output of CAMxSTAT
c     9/28/09.  v2 update extracts the peak observed and peak
c      paired predicted (paired in space, not in time)

      parameter (mxday = 40)
      integer ihrb(mxday)
      real uppa(mxday),appa(mxday),eppa(mxday),bnavg(mxday),enavg(mxday)
      real pobs(mxday),ppred(mxday)
      character*100 ipath
      character*20 site,pobssite(mxday)
      character*10 runname
      integer jdate,idd(mxday),imm(mxday)

c-----Read and open I/O files

      read(*,'(20x,a)') ipath
      open(11,file=ipath)
      write(*,*) 'Opened extraction file: ', ipath

      read(*,'(20x,a)') runname
      write(*,*) 'Processing run: ', runname

      read(*,'(20x,i10)') nday
      do m = 1,nday
        ppred(m) = -999.

        read(*,'(20x,a)') ipath
        open(10,file=ipath,status='old')
        write(*,*) 'Opened input STAT file: ', ipath

c-----Read header of Peak Prediction stats

        do n = 1,8
          read(10,*)
        enddo
        read(10,'(17x,a20,i6,12x,f10.1)') pobssite(m),jdate,pobs(m)
        read(10,*)
        read(10,'(55x,f10.1)') uppa(m)

        call caldate(jdate)
        idd(m) = mod(jdate,100)
        iyy = int(jdate/10000)
        imm(m) = int((jdate - iyy*10000) / 100)
        do n = 1,5
          read(10,*)
        enddo

 100    read(10,'(a)') ipath
        if (ipath(1:28).eq.'   Peak Bias (unpaired time)') then
c---------read/store daily max stats using all sites
          read(ipath,'(60x,f10.1,i10)') appa(m),ihrb(m)
          read(10,'(60x,f10.1,i10)') eppa(m)
          do n = 1,14
            read(10,*)
          enddo
          goto 200
        else
c---------read/store daily max stats from individual sites
          read(ipath,'(a20,f10.1,5x,f10.1)') site,pred,obs
          if (site .eq. pobssite(m)) then
            ppred(m) = pred
          endif
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
      write(11,'(2a,1x,40(3x,i2.2,a1,i2.2))')   'Date     ',
     +  'mm/dd     ',((imm(m),'/',idd(m)),m=1,nday)
      write(11,'(2a,1x,40f8.1)') 'PeakObs  ','Observed  ',
     +  (pobs(m),m=1,nday)
      write(11,'(2a,1x,40f8.1)') 'PairPred ',runname,(ppred(m),m=1,nday)
      write(11,'(2a,1x,40f8.1)') 'UPPA     ',runname,(uppa(m),m=1,nday)
      write(11,'(2a,1x,40f8.1)') 'APPA     ',runname,(appa(m),m=1,nday)
      write(11,'(2a,1x,40f8.1)') 'EPPA     ',runname,(eppa(m),m=1,nday)
      write(11,'(2a,1x,40i8)')   'PTB      ',runname,(ihrb(m),m=1,nday)
      write(11,'(2a,1x,40f8.1)') 'NB       ',runname,(bnavg(m),m=1,nday)
      write(11,'(2a,1x,40f8.1)') 'NE       ',runname,(enavg(m),m=1,nday)

      stop 
      end

      subroutine caldate(idate)
c
c----CAMx v5.10 090918
c
c     CALDATE converts date from Julian (YYJJJ) format to calender
c     (YYMMDD) format
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Modifications:
c        none
c
c     Input arguments:
c        idate               julian date (YYJJJ)
c
c     Output arguments:
c        idate               calender date (YYMMDD)
c
      integer nday(12)
      data nday/31,28,31,30,31,30,31,31,30,31,30,31/
c
c-----Entry point
c
c-----If it is already in calender date, return
c
      if (idate.gt.100000) goto 9999
      iyear = idate/1000
      jday = idate - iyear*1000
c
      nday(2) = 28
      if (mod(iyear,4).eq.0) nday(2) = 29
      mday = 0
      do 10 imonth = 1,12
        mday = mday + nday(imonth)
        if (mday.ge.jday) go to 20
 10   continue
 20   iday = jday - (mday - nday(imonth))
      idate = iyear*10000 + imonth*100 + iday
c
 9999 return
      end

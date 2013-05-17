      subroutine chrtime(time,date,chtime,chdate)
      implicit none
c
c----CAMx v6.00 130506
c
c     CHRTIME translates date/time variables to character strings for output
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Modifications:
c        none
c
c     Input arguments:
c        time            current simulation time (HHMMM)
c        date            current simulation date (YYJJJ)
c
c     Output arguments:
c        chtime          current simulation time ('HH:MM:SS')
c        chdate          current simulation date (YY/MM/DD)
c
c     Routines called:
c        none
c
c     Called by:
c        CAMx
c        NESTING
c
      integer date,ndate,iyr,imo,idy,ihr,imn,isc
      real time,rmn
      character*8 chtime,chdate
c
c-----Entry point
c
      ihr = int(time/100.)
      rmn = amod(time,100.)
      imn = int(rmn)
      isc = nint(60.*(rmn - float(imn)))
      if (isc.eq.60) then
        isc = 0
        imn = imn + 1
        if (imn.eq.60) then
          imn = 0
          ihr = ihr + 1
        endif
      endif
      write(chtime,'(i2.2,a,i2.2,a,i2.2)') ihr,':',imn,':',isc
c
      ndate = date
      call caldate(ndate)
      iyr = ndate/10000
      imo = mod(ndate,10000)/100
      idy = mod(ndate,100)
      write(chdate,'(i2.2,a,i2.2,a,i2.2)') iyr,'/',imo,'/',idy
c
      return
      end

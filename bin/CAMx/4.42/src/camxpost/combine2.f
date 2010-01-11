      program combine

c-----COMBINE reads individual PRED/OBS files generated from CAMxPOST
c     and combines them into a new PRED/OBS file suitable for plotting
c     their time series together
c
c     NOTE: 
c         *assumes all input PRED/OBS files contain identical hour records
c          and identical stations
c         *writes out only the first species (if multiple species are on
c          input files)

      parameter(mxstn=100,mxfile=20,mxrun=3)
      integer nhrin(mxrun,mxfile),nsite(mxrun,mxfile),
     &        n1(mxfile),n2(mxfile)
      real uin1(mxrun)
      character*80 ipath, ipathout, statmsg, dummsg, ipthoutn
      character*40 sitnam(mxrun,mxfile,mxstn)
      character*10 mspec(mxrun,mxfile),site(mxrun,mxfile,mxstn),
     &             atmp,label1(4),label(mxrun),units
      logical lmaxmin,lobs

      data zero /0./
      data label1 /'  Observed',' Predicted','   Maximum','   Minimum'/
      data lobs /.true./

c-----Do program I/O

      read(*,'(20x,a)') ipathout
      read(*,'(20x,a)') statmsg
      write(*,*) statmsg
      read(*,'(20x,a)') units
      write(*,*) units
      read(*,'(20x,i10)') nruns
      if (nruns.eq.1) then
        ncol = 2
        read(*,'(20x,l10)') lmaxmin
        if (lmaxmin) ncol = 4
      else
        read(*,'(20x,l10)') lobs
        ncol = nruns + 1
        do nr = 1,nruns
          read(*,'(20x,a)') label(nr)
          write(*,*) label(nr)
        enddo
      endif
      read(*,'(20x,a)') ipath
      if (ipath.eq.'ALL') then
        nsites = mxstn
      else
        read(ipath,*) nsites
      endif
      write(*,*) nsites
      read(*,'(20x,i10)') nfils

      if (nfils.gt.mxfile .or. nruns.gt.mxrun) then
        write(*,*) 'Increase parameter MXFILES or MXRUN'
        stop
      endif

      do nf = 1,nfils
        do nr = 1,nruns
          read(*,'(20x,a)') ipath
          iunit = nr + 10 + mxrun*(nf-1)
          open(iunit,file=ipath,status='old',err=901)
          write(*,*) 'Opened input PRED/OBS file: ',iunit,ipath
        enddo
      enddo

c-----Read all PRED/OBS file headers; check to ensure they contain identical
c     stations and number of hours

      nhr = 0
      do nf = 1,nfils
        do nr = 1,nruns
          iunit = nr + 10 + mxrun*(nf-1)
          read(iunit,'(a)') dummsg
          read(iunit,'(6x,a10)') mspec(nr,nf)
          if (mspec(nr,nf).ne.mspec(1,1)) then
            write(*,*) 'Different species on input file'
            write(*,*) 'Run  # ',nr
            write(*,*) 'File # ',nf
            write(*,*) mspec(1,1),mspec(nr,nf)
            stop
          endif
          read(iunit,'(i5)') nhrin(nr,nf)
          if (nr.eq.1) nhr = nhr+nhrin(nr,nf)
          if (nhrin(nr,nf).ne.nhrin(1,nf)) then
            write(*,*) 'Different number of hours on input file'
            write(*,*) 'Run  # ',nr
            write(*,*) 'File # ',nf
            write(*,*) nhrin(1,nf),nhrin(nr,nf)
            stop
          endif
          read(iunit,'(i5)') nsite(nr,nf)
          if (nsite(nr,nf).gt.mxstn) then
            write(*,*)'Increase parameter MXSTN to ',nsite(nr,nf)
            stop
          endif
          if (nsite(nr,nf).ne.nsite(1,1)) then
            write(*,*) 'Different number of sites on input file'
            write(*,*) 'Run  # ',nr
            write(*,*) 'File # ',nf
            write(*,*) nsite(1,1),nsite(nr,nf)
            stop
          endif
        enddo
      enddo
      if (nsites.eq.mxstn) nsites = nsite(1,1)
      if (mod(nsite(1,1),nsites).eq.0) then
        nn = nsite(1,1)/nsites
      else
        nn = nsite(1,1)/nsites + 1
      endif

c-----Open output files and write header info

      nstart = 1
      do n = 1,nn
        if (nsite(1,1).eq.nsites) then
          open(80+n,file=ipathout)
          write(*,*) 'Opened output PRED/OBS file: ',ipathout
        else
          il = istrln(ipathout)
          write(ipthoutn,'(a,a,i2.2)') ipathout(1:il),'.part',n
          open(80+n,file=ipthoutn)
          write(*,*) 'Opened output PRED/OBS file: ',ipthoutn
        endif
        write(80+n,'(a)') statmsg
        write(80+n,3000) mspec(1,1), units
 3000   format(a10,',',a10)
        write(80+n,'(i5)') nhr
        if (n.ne.nn) then
          write(80+n,'(i5)') nsites
        else
          if (mod(nsite(1,1),nsites).ne.0) then
            write(80+n,'(i5)') mod(nsite(1,1),nsites)
          else
            write(80+n,'(i5)') nsites
          endif
        endif
        n1(n) = nstart
        n2(n) = min(nsite(1,1),nstart+nsites-1)
        write(*,'(a,i3,a,i4,a,i4,/)')
     &        'Output file',n,' receives sites',n1(n),' to',n2(n)
        nstart = n2(n) + 1
      enddo

c-----Read station info

      do nf = 1,nfils
        do nr = 1,nruns
          iunit = nr + 10 + mxrun*(nf-1)
          do ns = 1,nsite(1,1)
            read(iunit,'(a10,a40)') site(nr,nf,ns),sitnam(nr,nf,ns)
            if (site(nr,nf,ns).ne.site(1,1,ns)) then
              write(*,*) 'Different site order on input file'
              write(*,*) 'Run  # ',nr
              write(*,*) 'File # ',nf
              write(*,*) 'Site # ',ns
              write(*,*) site(1,1,ns),site(nr,nf,ns)
              stop
            endif
          enddo
          read(iunit,*)
          read(iunit,*)
        enddo
      enddo

c-----Write station info and column labels

      do n = 1,nn
        do i = n1(n),n2(n)
          write(80+n,1000) site(1,1,i),sitnam(1,1,i)
 1000     format(a10,',',a40)
        enddo
        if (nruns.eq.1) then
          write(80+n,2000) ncol,(label1(m),m=1,ncol)
        else 
          write(80+n,2000) ncol,label1(1),(label(m),m=1,nruns)
        endif
 2000   format(i4,'      ,        ,  ,',10(a10,','))
      enddo

c-----Read all PRED/OBS time-varying data and write to new files

      do n = 1,nn
        do ns = n1(n),n2(n)
          do nf = 1,nfils
            do nh = 1,nhrin(1,nf)
              do nr = 1,nruns
                iunit = nr + 10 + mxrun*(nf-1)
                read(iunit,8030) atmp,idat1,itim1,idat2,itim2,
     &                           oin,uin1(nr),uin2,uin3
              enddo
              if (.not.lobs) then
                if (nf.eq.1 .and. nh .eq. 1) then
                  oin = 0.
                else
                  oin = -999.
                endif
              else 
                if (nf.eq.1 .and. nh .eq. 1 .and. oin .eq. -999. )
     &              oin = 0.
              endif
              call caldate(idat1)
              iyr = idat1/10000
              imo = mod(idat1,10000)/100
              idy = mod(idat1,100)
              if (nruns.eq.1) then
                if (lmaxmin) then
                   write(80+n,8031) atmp,imo,idy,iyr,itim1,
     &                              oin,uin1(1),uin2,uin3
                 else
                   write(80+n,8031) atmp,imo,idy,iyr,itim1,oin,uin1(1)
                 endif
              else
                 write(80+n,8031) atmp,imo,idy,iyr,itim1,
     &                         oin,(uin1(m),m=1,nruns)
              endif
            enddo
          enddo
        enddo
      enddo
 8030 format(a10,2x,i6,i2,2x,i6,i2,10f10.0)
 8031 format(a10,',',i2,'/',i2,'/',i2,',',i2.2,','10(f10.2,','))
      stop

 901  continue 
      write(*,*) 'PRED/OBS file not found' 
      write(*,'(a)') ipath 
      stop 
      end
c
      subroutine caldate(idate)
c  
c-----CAMx v2.03 991230
c  
c     CALDATE converts date from Julian (YYJJJ) format to calender
c     (YYMMDD) format
c                            
c     Copyright 1996, 1997, 1998, 1999 ENVIRON International Corporation  
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
c     Routines Called:  
c        none
c              
c     Called by:  
c        AREAPREP  
c        BNDPREP
c        CNCPREP
c        DRYDEP
c        PIGPREP
c        RDPTHDR
c        READZP
c        STARTUP
c
      dimension nday(12)
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

C**** ISTRLN
c
c-----CAMx v2.03 991230
c
c
      function istrln( string )
      integer   istrln
c
c-----------------------------------------------------------------------
c
c     This routine returns the non-blank length of a string.
c
c   Arguments:
c     Inputs:
c       string   C   string for determining length
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*(*) string
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer   i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- initialize length to zero ----
c
      istrln = 0
      do 10 i=LEN( string ),1,-1
         if( string(i:i) .NE. ' ' ) then
             istrln = i
             goto 9999
         endif
   10 continue
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

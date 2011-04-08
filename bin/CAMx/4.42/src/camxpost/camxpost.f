      program camxpost

c-----Reads: 
c       1) an hourly CAMx coarse grid or UAM average output file
c       2) an hourly observation file (optional)
c       3) user inputs for
c          a) species to process
c          b) hour span for n-hour averaging
c          c) period to process
c     Produces for the simulation period:
c       1) a running n-hourly average output file for plotting (optional)
c       2) a maximum n-hourly average output file for plotting (optional)
c       3) a running n-hourly paired prediction-observation file for
c          further processing of stats and plotting of time-series (optional)
c       4) a file of maximum observations at each site (optional)
c
c     NOTES: only layer 1 is processed
c            only 1 species is processed (but it may be the sum of more than one species)
c
c     -Optional output is selected by supplying a valid file name on input
c     -If the observational file name is blank, obs-related processing and
c      output will not be performed
c     -If the gridded output file names are not supplied, they will not be
c      generated
c
      parameter (mxx=300,mxy=300,mxspc=150,mxhr=48,mxsit=100,mxobs=200,
     & mxspecsum=10)
      integer ibgdat(mxhr),ndate(mxobs),nhour(mxobs),
     &        nuse(mxhr),nmax(mxsit),iostatus
      real cread(mxx,mxy),xutm(mxsit),yutm(mxsit),
     &     begtim(mxhr),obsmx(mxsit)
      real prdconc1(mxx,mxy,mxhr),prdconc2(mxx,mxy,mxhr),
     &     prdconc3(mxhr,mxsit),prdconc4(mxx,mxy),
     &     obsconc1(mxobs,mxsit),obsconc2(mxobs,mxsit),
     &     grdmax(mxhr,mxsit),grdmin(mxhr,mxsit),cfactor,deltax,deltay,
     &     conv2metre, decimali, decimalj
      character*200 ipath,statmsg,obsmsg
      character*20 sitnam(mxsit),sitmax
      character*10 atmp,site(mxsit)
      character*4 filnam(10),fileid(60),mspec(10,mxspc),
     &            obspec(10)
      character*10 mspec1(mxspecsum)
      character*2 atim1,atim2,aotmmx1,aotmmx2,autmmx1,autmmx2
      character*7 projection
      data ione /1/

c-----Read and open I/O files; get user-specified inputs

      lout11 = .true.
      lout15 = .true.
      lobs = .true.

      read(*,'(20x,a)') ipath
      open(10,file=ipath,form='unformatted',status='old',err=902)
      write(*,*) 'Opened input AVERAGE file: ', ipath

      read(*,'(20x,a)') ipath
      if (ipath.eq.' ') then
        lobs = .false.
      else
        open(12,file=ipath,status='old',err=903)
        write(*,*) 'Opened input OBSERVATION file: ', ipath
      endif

      read(*,'(20x,a)') ipath
      if (ipath.eq.' ') then
        lout11 = .false.
      else
        open(11,file=ipath,form='unformatted')
        write(*,*) 'Opened output AVERAGE file: ', ipath
      endif

      read(*,'(20x,a)') ipath
      if (ipath.eq.' ') then
        lout15 = .false.
      else
        open(15,file=ipath,form='unformatted')
        write(*,*) 'Opened output MAX AVERAGE file: ', ipath
      endif
 
      read(*,'(20x,a)') ipath
      if (lobs) then
        open(13,file=ipath)
        write(*,*) 'Opened output PRED-OBS file: ', ipath
      endif

      read(*,'(20x,a)') ipath
      if (lobs) then
        open(14,file=ipath)
        write(*,*) 'Opened output MAX OBSERVATION file: ', ipath
      endif
 
      read(*,'(20x,i5)') navhour
      write(*,*) 'n-hour averaging span: ',navhour
      read(*,'(20x,i5,i2,1x,i5,i2)') jday1,ihr1,jday2,ihr2
      write(*,'(1x,a,i10,i5,i10,i5)') 
     &     'Period to process: ',jday1,ihr1,jday2,ihr2
      hr1 = float(ihr1)
      hr2 = float(ihr2)
      
      read(*,'(20x,f5.4)') radmax
      write(*,*) 'Search radius for max prediction (km/deg): ',radmax
      read(*,'(20x,4i5)') isub1,isub2,jsub1,jsub2
      write(*,'(1x,a,4i5)') 'Sub-domain to search for peak: ',
     &                      isub1,isub2,jsub1,jsub2
      read(*,'(20x,a)') statmsg
      write(*,*) statmsg
      
c-----Optional items
      read(*,'(20x,a)',IOSTAT=iostatus) projection
      if (iostatus.ne.0) then
        projection='UTM'
       endif
       
      write(*,*) 'Projection: ', projection
      
      read(*,'(20x,f6.4)',IOSTAT=iostatus) cfactor
      if (iostatus.ne.0) then
        cfactor=1.0
       endif
      write(*,*) 'Conversion factor: ', cfactor
      
      write(*,*)
      

c-----Read and write the four header records
      read(10) filnam,fileid,noseg,nospec,idat1,tim1,idat2,tim2
      if (tim2.eq.0.) then
         tim2 = 24.
         idat2 = idat2 - 1
      endif
      write(*,'(1x,a,i10,f5.0,i10,f5.0)')
     &     'Time span on input AVERAGE file: ',idat1,tim1,idat2,tim2
      if (nospec.gt.mxspc) then
        write(*,*)'File SPEC dimensions exceed CAMxPOST dimensions'
        write(*,*)'FILE: ',nospec
        write(*,*)'CAMxPOST: ',mxspc
        write(*,*)'Increase MXSPC and recompile'
        stop
      endif

      hr2 = hr2 + float(navhour) - 1
      if (hr2.gt.24.) then
        hr2 = hr2 - 24.
        jday2 = jday2 + 1
      endif
      t2 = hr2 + 1
      id2 = jday2
      if (t2.gt.24.) then
        t2 = t2 - 24.
        id2 = id2 + 1
      endif
      if (id2.gt.idat2 .or.
     &    id2.eq.idat2 .and. t2.gt.tim2) then
        t2 = tim2
        id2 = idat2
      endif
      if (lout11) then
        write(*,'(1x,a,i10,f5.0,i10,f5.0)')
     &       'Time span on output AVERAGE file: ',jday1,hr1,id2,t2
        write(11) filnam,fileid,noseg,ione,jday1,hr1,id2,t2
      endif
      if (lout15) then
        write(*,'(1x,a,i10,f5.0,i10,f5.0)')
     &       'Time span on output MAX AVERAGE file: ',jday1,hr1,id2,t2
        write(15) filnam,fileid,noseg,ione,jday1,hr1,id2,t2
      endif

      read(10) xref,yref,iutm,xorg,yorg,deltax,deltay,nx,ny,nozg,
     &         ncell1,ncell2,surfht,htmin1,htmin2
      if (nx.gt.mxx .or. ny.gt.mxy) then
        write(*,*)'File X/Y dimensions exceed CAMxPOST dimensions'
        write(*,*)'FILE: ',nx,ny
        write(*,*)'CAMxPOST: ',mxx,mxy
        write(*,*)'Increase MXX and MXY and recompile'
        stop
      endif

      if (lout11)
     &  write(11) xref,yref,iutm,xorg,yorg,deltax,deltay,nx,ny,ione,
     &            ncell1,ncell2,surfht,htmin1,htmin2
      if (lout15)
     &  write(15) xref,yref,iutm,xorg,yorg,deltax,deltay,nx,ny,ione,
     &          ncell1,ncell2,surfht,htmin1,htmin2

      read(10) ilocx,ilocy,nox,noy
      if (nox.ne.nx .or. noy.ne.ny) then
        write(*,*)'Inconsistency in AVERAGE file header'
        write(*,*)'NX and NY:   ',nx,ny
        write(*,*)'NOX and NOY: ',nox,noy
        stop
      endif
      
c     dco The original code assumes that coordinates are in UTM
c     dco and therefore, multiplies distances by 1000
c     dco This Hack allows us to keep the rest of the code intact
c     dco In the future We may also allow 'INDEX', then we use the coordinates (must be ints)
c     dco directly as indices
      if (projection.eq.'LATLON') then
        conv2metre = 1.0
      else
        conv2metre = 1000.
      endif

      if (lout11) write(11) ilocx,ilocy,nox,noy
      if (lout15) write(15) ilocx,ilocy,nox,noy

      read(10) ((mspec(i,j),i=1,10),j=1,nospec)

      if (lout11) write(11) (mspec1(i),i=1,10)
      if (lout15) write(15) (mspec1(i),i=1,10)
 
c-----Identify species desired
 
      do 10 i = 1,nospec
        isp1 = i
        do 11 m = 1,10
          if (mspec(m,i).ne.mspec1(m)) goto 10
 11     continue
        goto 20
 10   continue
      write(*,9000) (mspec1(m),m=1,10)
      write(*,9010) 
      write(*,9020) ((mspec(m,j),m=1,10),j=1,nospec)
9000  format ('Error could not match species: ', 10a1)
9010  format ('Species on file are:')
9020  format (8(10a1))
      stop

c-----Loop over hours on file; read time record

 20   nprdhr = 0
 500  continue
      nn = nprdhr + 1
      read(10,end=901) ibgdat(nn),begtim(nn),iendat,endtim
      if (nn.eq.1 .and. (ibgdat(nn).gt.jday1 .or.
     &    ibgdat(nn).eq.jday1 .and. begtim(nn).gt.hr1)) then
        write(*,*) 'AVG file starts later than first hour to process'
        stop
      endif
      if (ibgdat(nn).gt.jday2 .or.
     &    ibgdat(nn).eq.jday2 .and. begtim(nn).gt.hr2) goto 901
 
c-----Loop over species and number of vertical levels in simulation grid
c     Load hourly concentrations for layer 1 and given species and apply conversion factor
 
      do 30 ispec = 1,nospec
        do 31 iz = 1,nozg
          read(10) iseg,filnam,((cread(i,j),i=1,nox),j=1,noy)
          if (iz.ne.1 .or. ispec.ne.isp1) goto 31
          do 40 j = 1,noy
            do 41 i = 1,nox
              prdconc1(i,j,nn) = cread(i,j) * cfactor
 41         continue
 40       continue
 31     continue
 30   continue
      write(*,'(a,i5,i10,f5.0,i10,f5.0)') 'Read AVG file: ',
     &                nn,ibgdat(nn),begtim(nn),iendat,endtim
      if (ibgdat(nn).gt.jday1 .or.
     &    ibgdat(nn).eq.jday1 .and. begtim(nn).ge.hr1) nprdhr = nn
      if (nprdhr.gt.mxhr) then
        write(*,*)'Number of hours extracted exceeds',
     &            ' CAMxPOST dimensions'
        write(*,*)'FILE:     ',nprdhr
        write(*,*)'CAMxPOST: ',mxhr
        write(*,*)'Increase MXHR and recompile'
        stop
      endif
      goto 500

 901  continue

c-----Read header of observation file and check for species match

      if (lobs) then
        read(12,'(a)') obsmsg
        read(12,'(6x,5a1)') (obspec(m),m=1,5)
        read(12,*) nobhrs
        read(12,*) nsite
        if (nobhrs.gt.mxobs) then
           write(*,*)'Number of hours in OBS file exceed',
     &               ' CAMxPOST dimensions'
           write(*,*)'FILE:     ',nobhrs
           write(*,*)'CAMxPOST: ',mxobs
           write(*,*)'Increase MXOBS and recompile'
           stop
        endif
        if (nsite.gt.mxsit) then
           write(*,*)'Number of sites in OBS file exceed',
     &               ' CAMxPOST dimensions'
           write(*,*)'FILE:     ',nsite
           write(*,*)'CAMxPOST: ',mxsit
           write(*,*)'Increase MXSIT and recompile'
           stop
        endif
        do m = 1,5
          if (obspec(m).ne.mspec1(m)) then
            write(*,9030) (mspec1(n),n=1,10)
            write(*,9010) 
            write(*,9020) (obspec(n),n=1,5)
 9030       format('Error could not match OBS species: ', 10a1)
            stop
          endif
        enddo

c-----Loop over sites to get locations

 60     do 65 ns = 1,nsite
          read(12,'(a10,a20,2f10.0)') site(ns),sitnam(ns),
     &                                xutm(ns),yutm(ns)
 65     continue

c-----Loop over hours in observation file and load hourly concentrations
c     for given species

        do 70 ns = 1,nsite
          do 71 nh = 1,nobhrs
            read(12,8010,end=904) atmp,ndate(nh),nhour(nh),obread
            if (atmp.ne.site(ns)) then
              write(*,*)'In OBS file, expecting site: ',site(ns)
              write(*,*)'Found: ',atmp,nh,ns
              stop
            endif
            obsconc1(nh,ns) = obread
 71       continue
 70     continue
 8010   format(a10,i8,i2,10f10.0)
      endif

c-----Calculate running n-hourly averages for gridded concentrations 

      do 80 j = 1,noy
        do 81 i = 1,nox 
          prdconc4(i,j) = 0.
          do 82 nh = 1,nprdhr-navhour+1
            prdconc2(i,j,nh) = 0.
            do 83 nn = 1,navhour 
              prdconc2(i,j,nh) = prdconc2(i,j,nh) + 
     &                           prdconc1(i,j,nh+nn-1)
 83         continue
            prdconc2(i,j,nh) = prdconc2(i,j,nh)/float(navhour)
            prdconc4(i,j) = amax1(prdconc4(i,j),prdconc2(i,j,nh))
 82       continue
 81     continue
 80   continue

c-----Write n-hourly fields to new average file

      if (lout11) then
        do 90 nh = 1,nprdhr-navhour+1
          endtim = anint(begtim(nh)) + float(navhour)
          iendat = ibgdat(nh) 
          if (endtim.gt.24.) then
            endtim = endtim - 24.
            iendat = ibgdat(nh) + 1 
          endif
          write(11) ibgdat(nh),anint(begtim(nh)),iendat,endtim 
          write(*,'(1x,a,i10,f5.0,i10,f5.0)')
     &          'Writing n-hour AVERAGE file: ',
     &           ibgdat(nh),anint(begtim(nh)),iendat,endtim 
          write(11) ione,mspec1,((prdconc2(i,j,nh),i=1,nox),j=1,noy) 
 90     continue
      endif
      if (lout15) then
        write(15) jday1,hr1,id2,t2
        write(*,'(1x,a,i10,f5.0,i10,f5.0)')
     &        'Writing MAX AVERAGE file: ',jday1,hr1,id2,t2
        write(15) ione,mspec1,((prdconc4(i,j),i=1,nox),j=1,noy) 
      endif

c-----Calculate running n-hour averages for observations
c     -- data completeness threshold is 75%

      if (lobs) then
        ntst = int(navhour*0.74 + 1)
        do 110 ns = 1,nsite
          do 111 nh = 1,nobhrs-navhour+1 
            obsconc2(nh,ns) = 0.
            nvalid = 0
            do 112 nn = 1,navhour 
              if (obsconc1(nh+nn-1,ns).eq.-999.) goto 112
              nvalid = nvalid + 1
              obsconc2(nh,ns) = obsconc2(nh,ns) + obsconc1(nh+nn-1,ns)
 112        continue
            if (nvalid.ge.ntst) then
              obsconc2(nh,ns) = obsconc2(nh,ns)/float(nvalid) 
            else
              obsconc2(nh,ns) = -999.
            endif
 111      continue
 110    continue

c-----Write header of new PRED/OBS file

        write(13,'(a)') statmsg
        write(13,'(i5,1x,10a1)') ione,(mspec1(m),m=1,10)
        write(13,'(i5)') nprdhr-navhour+1
        write(13,'(i5)') nsite

c-----Determine predicted n-hourly concentrations at observation sites using
c     bi-linear interpolation
c     Also determine peak observation over all sites and hours, and
c     max/min predictions in 9 surrounding grid cells

        obsmax = 0.
        do 120 ns = 1,nsite
          obsmx(ns) = 0.
          nmax(ns) = 0
          write(13,'(a10,a20,2f10.3)') site(ns),sitnam(ns),
     &                                 xutm(ns),yutm(ns)
          distx = conv2metre*xutm(ns) - xorg
          disty = conv2metre*yutm(ns) - yorg
          
c----- Print out the "decimal index" of the stations
          decimali = (distx/deltax) + 1
          decimalj = (disty/deltay) + 1
          
          write(*,*) 'Site ',sitnam(ns),' corresponds to cell ',
     &               decimali,decimalj
          
          ii = int(distx/deltax + 0.5)
          jj = int(disty/deltay + 0.5)
          iii = int(distx/deltax + 1.)
          jjj = int(disty/deltay + 1.)
          dx = distx - deltax*(float(ii) - 0.5)
          dy = disty - deltay*(float(jj) - 0.5)
          do 130 nh = 1,nprdhr-navhour+1
            if (ii.ge.2 .and. ii.le.nox-2 .and.
     &          jj.ge.2 .and. jj.le.noy-2) then
              cc1 = prdconc2(ii,jj,nh) + (dx/deltax)*
     &              (prdconc2(ii+1,jj,nh) - prdconc2(ii,jj,nh))
              cc2 = prdconc2(ii,jj+1,nh) + (dx/deltax)*
     &              (prdconc2(ii+1,jj+1,nh) - prdconc2(ii,jj+1,nh))
              prdconc3(nh,ns) = cc1 + (dy/deltay)*(cc2 - cc1)
              grdmax(nh,ns) = 0.
              grdmin(nh,ns) = 1.e10
              do i = iii-1,iii+1
                do j = jjj-1,jjj+1
                  grdmax(nh,ns) = amax1(grdmax(nh,ns),prdconc2(i,j,nh))
                  grdmin(nh,ns) = amin1(grdmin(nh,ns),prdconc2(i,j,nh))
                enddo
              enddo
            else
              prdconc3(nh,ns) = -999.
              grdmax(nh,ns) = -999.
              grdmin(nh,ns) = -999.
            endif

            if (ns.eq.1) then
              do 140 nn = 1,nobhrs
                if (ndate(nn).eq.ibgdat(nh) .and. 
     &              nhour(nn).eq.nint(begtim(nh))) then
                  nuse(nh) = nn
                  goto 150
                endif
 140          continue
              write(*,*) 'Could not synchronize PRED and OBS files'
              write(*,*) 'PRD time : ',ibgdat(nh),anint(begtim(nh))
              write(*,*) 'OBS start: ',ndate(1),nhour(1)
              write(*,*) 'OBS end  : ',ndate(nobhrs),nhour(nobhrs)
              stop
            endif

 150        if (obsconc2(nuse(nh),ns).ne.-999.) then
              obsmax = amax1(obsmax,obsconc2(nuse(nh),ns))
              if (obsmax.eq.obsconc2(nuse(nh),ns)) then
                sitmax = sitnam(ns)
                iodtmx1 = ibgdat(nh)
                iotmmx1 = nint(begtim(nh))
                nsmax = ns
              endif
              obsmx(ns) = amax1(obsmx(ns),obsconc2(nuse(nh),ns))
              nmax(ns) = nmax(ns) + 1
            endif
 130      continue
 120    continue

c-----Find peak gridded concentrations over all hours
c     within a user-defined sub-domain and radius from max ob site
c     CT 9/12/02 Change max jsub2 from noy-2 to noy-1 

        if (isub1.lt.1 .or. jsub1.lt.1) then
          isub1 = 2
          isub2 = nox - 1
          jsub1 = 2
          jsub2 = noy - 1
        else
          isub1 = max(2,isub1)
          isub2 = min(nox-1,isub2)
          jsub1 = max(2,jsub1)
          jsub2 = min(noy-1,jsub2)
        endif
        prdmax = 0.
        do j = jsub1,jsub2
          do i = isub1,isub2
            xloc = (xorg + (i-0.5)*deltax)/conv2metre
            yloc = (yorg + (j-0.5)*deltay)/conv2metre
            distx = (xloc - xutm(nsmax))
            disty = (yloc - yutm(nsmax))
            dist = sqrt(distx**2 + disty**2)
            if (dist.le.radmax) then
              do nh = 1,nprdhr-navhour+1
                prdmax = amax1(prdmax,prdconc2(i,j,nh))
                if (prdmax.eq.prdconc2(i,j,nh)) then
                  iudtmx1 = ibgdat(nh)
                  iutmmx1 = nint(begtim(nh))
                  imax = i
                  jmax = j
                endif
              enddo
            endif
          enddo
        enddo

c-----Write to new PRED/OBS file

        iutmmx2 = iutmmx1 + navhour
        iotmmx2 = iotmmx1 + navhour
        iudtmx2 = iudtmx1
        iodtmx2 = iodtmx1
        if (iutmmx2.gt.24) then
          iutmmx2 = iutmmx2 - 24
          iudtmx2 = iudtmx2 + 1 
        endif
        if (iotmmx2.gt.24) then
          iotmmx2 = iotmmx2 - 24
          iodtmx2 = iodtmx2 + 1 
        endif

        write(aotmmx1,'(i2.2)') iotmmx1
        write(aotmmx2,'(i2.2)') iotmmx2
        write(autmmx1,'(i2.2)') iutmmx1
        write(autmmx2,'(i2.2)') iutmmx2
        write(13,9040) sitmax,iodtmx1,aotmmx1,iodtmx2,aotmmx2,obsmax
        write(13,9050) imax,jmax,iudtmx1,autmmx1,iudtmx2,autmmx2,prdmax
 9040   format('Peak Observed     : ',a20,i6,a2,' -',i6,a2,f10.4)
 9050   format('Peak Predicted    : Cell (',i3,',',i3,')',6x,i6,a2,
     &         ' -',i6,a2,f10.4)

cPeak Observed     : East Charleston      yyjjjhh - yyjjjhh      10.1
cPeak Predicted    : Cell (iii,jjj)       yyjjjhh - yyjjjhh      10.1

        do 160 ns = 1,nsite
          do 170 nh = 1,nprdhr-navhour+1
            idat1 = ibgdat(nh)
            itim1 = nint(begtim(nh))
            write(atim1,'(i2.2)') itim1
            itim2 = itim1 + navhour
            idat2 = idat1
            if (itim2.gt.24) then
              itim2 = itim2 - 24
              idat2 = idat2 + 1 
            endif
            write(atim2,'(i2.2)') itim2
            write(13,9060) site(ns),idat1,atim1,idat2,atim2,
     &                     obsconc2(nuse(nh),ns),prdconc3(nh,ns),
     &                     grdmax(nh,ns),grdmin(nh,ns)
 170      continue
 160    continue
 9060   format(a10,2x,i6,a2,' -',i6,a2,10f10.4)

c-----Write peak observations to separate file

        do 180 ns = 1,nsite
          if (nmax(ns).eq.0) obsmx(ns) = -999.
          write(14,'(a10,2f10.3,i10)') site(ns),xutm(ns),yutm(ns),
     &                                 nint(obsmx(ns))
 180    continue
      endif

      stop

 902  continue
      write(*,*) 'Average file not found'
      write(*,'(a)') ipath
      stop
 903  continue
      write(*,*) 'Observation file not found'
      write(*,'(a)') ipath
      stop
 904  continue
      write(*,*) 'Unexpected end of observation data'
      stop
      end

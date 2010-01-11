      program camxstat

c-----CAMxSTAT reads an n-hourly PRED/OBS file generated from CAMxPOST
c     and calculates the following statistics:
c     1) unpaired (time and space) peak prediction accuracy
c     2) space-paired, time-unpaired peak prediction accuracy by site
c     3) space-paired, time-unpaired peak bias and error over all sites
c     4) space-paired, time-paired peak bias and error over all sites
c     5) mean prediction
c     6) mean observation
c     7) difference and normalized difference in mean pred and mean obs
c     8) absolute, normalized, and fractional bias
c     9) absolute, normalized, and fractional error
c    10) root mean square error

      parameter(mxstn=12,mxhr=120,mxspc=10)
      integer idat1(mxhr),itim1(mxhr),idat2(mxhr),itim2(mxhr),
     &        mspec(10,mxspc)
      real xutm(mxstn),yutm(mxstn),obs(mxhr,mxstn),prd(mxhr,mxstn)
      real oin(mxspc),uin(mxspc)
      character*80 ipath, statmsg
      character*20 sitnam(mxstn),sitmax
      character*10 site(mxstn),atmp
      character*4 units

c-----Read and open I/O files

      read(*,'(20x,a)') ipath
      open(10,file=ipath,status='old',err=901)
      write(*,*) 'Opened input PRED/OBS file: ', ipath
      read(*,'(20x,a)') ipath
      open(11,file=ipath)
      write(*,*) 'Opened output STAT file: ', ipath
      read(*,'(20x,f10.0)') cmin
      write(*,*) 'Minimum concentration to calculate stats: ',cmin
      read(*,'(20x,f10.0)') sclfact
      write(*,*) 'Predictions scaled by ',sclfact
      read(*,'(20x,a4)') units
      write(*,*) 'Pred/obs units are ',units

c-----Read entire PRED/OBS file

      read(10,'(a)') statmsg
      read(10,'(i5,1x,10a1)') nspec,((mspec(m,n),m=1,10),n=1,nspec)
      read(10,'(i5)') nhr
      read(10,'(i5)') nsite
      do 5 ns = 1,nsite
        read(10,'(a10,a20,2f10.0)') site(ns),sitnam(ns),
     &                              xutm(ns),yutm(ns)
 5    continue
      read(10,8010) sitmax,iodtmx1,iotmmx1,iodtmx2,iotmmx2,obsmax
      read(10,8020) imax,jmax,iudtmx1,iutmmx1,iudtmx2,iutmmx2,prdmax
      prdmax = prdmax*sclfact
 8010 format(20x,a20,i6,i2,2x,i6,i2,f10.1)
 8020 format(26x,i3,1x,i3,7x,i6,i2,2x,i6,i2,f10.1)
      do 10 ns = 1,nsite
        do 20 nh = 1,nhr
          read(10,8030) atmp,idat1(nh),itim1(nh),idat2(nh),itim2(nh),
     &                   (oin(n),uin(n),n=1,nspec)
          if (atmp.ne.site(ns)) then
            write(*,*)'In OBS file, expecting site: ',site(ns) 
            write(*,*)'Found: ',atmp 
            stop 
          endif
          obs(nh,ns) = oin(1)
          prd(nh,ns) = uin(1)*sclfact
 20     continue
 10   continue
 8030 format(a10,2x,i6,i2,2x,i6,i2,10f10.0)

c-----Write header for Peak Prediction stats

      write(11,*)
      write(11,'(a)') 'MODEL PERFORMANCE STATISTICS'
      write(11,*)
      write(11,'(a)') statmsg

      do 500 l = 1,nspec
      write(11,*)
      write(11,'(a,10a1)')'Peak Prediction Statistics for ',
     &                     (mspec(m,l),m=1,10)
      write(11,*) 'Predictions scaled by: ',sclfact
      write(11,'(80a)') ('-',m=1,80)
      write(11,9010) sitmax,iodtmx1,iotmmx1,iodtmx2,iotmmx2,
     &               obsmax,units
      write(11,9020) imax,jmax,iudtmx1,iutmmx1,iudtmx2,iutmmx2,
     &               prdmax,units
 9010 format('Peak Observed  : ',a20,i6,i2,' -',i6,i2,f10.1,1x,a4)
 9020 format('Peak Predicted : Cell (',i3,',',i3,')',6x,i6,i2,
     &       ' -',i6,i2,f10.1,1x,a4)
      uppa = 100.*(prdmax - obsmax)/obsmax
      write(11,9030) uppa
 9030 format('Unpaired Peak Prediction Accuracy :',20x,f10.1,' %')
      write(11,*)
      write(11,'(a,f5.1,1x,a4)')'For Concentrations above ',cmin,units
      write(11,'(a)')'Peak Predicted/Observed by Site:'
      write(11,'(a,a)')'                          Predicted',
     &           '       Observed          Error      Time Diff'
      write(11,'(a,a)')'       Site              conc    hr     conc',
     &           '    hr     conc        %       hours'
c                        Predicted       Observed          Error       Time Diff
c     Site              conc    hr     conc    hr     conc        %        hours
caaaaaaaaaaaaaaaaaaaffffffff.fhhhhhffffffff.fhhhhhffffffff.feeeeeeee.ehhhhhhhhhh

c-----Calculate stats over all sites and hours

      oavg = 0.
      uavg = 0.
      bavg = 0.
      bnavg = 0.
      bfavg = 0.
      eavg = 0.
      enavg = 0.
      efavg = 0.
      ravg = 0.
      appa = 0.
      appat = 0.
      eppa = 0.
      eppat = 0.
      ppb = 0.
      ppbt = 0.
      ppe = 0.
      ppet = 0.
      ihrb = 0
      ihre = 0
      nvalid = 0
      nvalida = 0
      nvalidp = 0
      do 30 ns = 1,nsite
        omax = -999.
        umax = -999.
        do 40 nh = 1,nhr
          if (obs(nh,ns).ge.cmin) omax = amax1(omax,obs(nh,ns))
          if (prd(nh,ns).ne.-999.) umax = amax1(umax,prd(nh,ns))
          if (omax.eq.obs(nh,ns)) then
            iohrmx = nh
          endif
          if (umax.eq.prd(nh,ns)) then
            iuhrmx = nh
          endif
          if (obs(nh,ns).ne.-999. .and. prd(nh,ns).ne.-999.) then
            nvalida = nvalida + 1
            uavg = uavg + prd(nh,ns)
            oavg = oavg + obs(nh,ns)
          endif
          if (obs(nh,ns).ge.cmin .and. prd(nh,ns).ne.-999.) then
            nvalid = nvalid + 1
            diff = prd(nh,ns) - obs(nh,ns)
            bavg = bavg + diff
            bnavg = bnavg + diff/obs(nh,ns)
            bfavg = bfavg + diff/(prd(nh,ns) + obs(nh,ns))
            eavg = eavg + abs(diff)
            enavg = enavg + abs(diff)/obs(nh,ns)
            efavg = efavg + abs(diff)/(prd(nh,ns) + obs(nh,ns))
            ravg = ravg + diff**2
          endif
 40     continue
        if (umax.eq.-999. .or. omax.eq.-999.) then
          itmaxu = itim1(iuhrmx)
          itmaxo = itim1(iohrmx)
          if (umax.eq.-999.) itmaxu = -999
          if (omax.eq.-999.) itmaxo = -999
          ihrdif = -999
          ppa = -999.
          write(11,9040) sitnam(ns),umax,itmaxu,omax,
     &                   itmaxo,ppa,ppa,ihrdif
        else
          nvalidp = nvalidp + 1
          ihrdif = 24*(idat1(iuhrmx) - idat1(iohrmx)) +
     &             itim1(iuhrmx) - itim1(iohrmx)
          pdiff = umax - omax
          ppa = 100.*pdiff/omax
          appa = appa + ppa
          appat = appat + 100.*(prd(iohrmx,ns)-omax)/omax
          eppa = eppa + abs(ppa)
          eppat = eppat + 100.*abs(prd(iohrmx,ns)-omax)/omax
          ppb = ppb + pdiff
          ppbt = ppbt + (prd(iohrmx,ns)-omax)
          ppe = ppe + abs(pdiff)
          ppet = ppet + abs(prd(iohrmx,ns)-omax)
          ihrb = ihrb + ihrdif
          ihre = ihre + abs(ihrdif)
          write(11,9040) sitnam(ns),umax,itim1(iuhrmx),omax,
     &                   itim1(iohrmx),pdiff,ppa,ihrdif
        endif
 9040   format(a20,f10.1,i5,f10.1,i5,2f10.1,i10)
 30   continue

c-----Calculate average Peak stats and write to output

      if (nvalidp.gt.0) then
        ppb = ppb/float(nvalidp)
        ppbt = ppbt/float(nvalidp)
        ppe = ppe/float(nvalidp)
        ppet = ppet/float(nvalidp)
        appa = appa/float(nvalidp)
        appat = appat/float(nvalidp)
        eppa = eppa/float(nvalidp)
        eppat = eppat/float(nvalidp)
        ihrb = nint(float(ihrb)/float(nvalidp))
        ihre = nint(float(ihre)/float(nvalidp))
      else
        ppb  = -999.
        ppbt = -999.
        ppe  = -999.
        ppet = -999.
        appa = -999.
        appat= -999.
        eppa = -999.
        eppat= -999.
        ihrb = -999
        ihre = -999
      endif
      write(11,9050) ppb,appa,ihrb
      write(11,9051) ppe,eppa,ihre
      write(11,9052) ppbt,appat
      write(11,9053) ppet,eppat
      write(11,9060) nsite,nvalidp
 9050 format(3x,'Peak Bias (unpaired time) :',20x,2f10.1,i10)
 9051 format(3x,'Peak Error (unpaired time):',20x,2f10.1,i10)
 9052 format(3x,'Peak Bias (paired time)   :',20x,2f10.1)
 9053 format(3x,'Peak Error (paired time)  :',20x,2f10.1)
 9060 format('Number of Stations:         ',i3,/,
     &       'Number of valid peak pairs: ',i3)
      write(11,'(80a)') ('-',m=1,80)

c-----Calculate average stats and write to output

      npos = nsite*nhr
      if (nvalida.gt.0) then
        uavg = uavg/float(nvalida) 
        oavg = oavg/float(nvalida)
        davg = uavg - oavg
        dnavg = 100.*davg/oavg
      else
        uavg = -999.
        oavg = -999.
        davg = -999.
        dnavg = -999.
      endif
      if (nvalid.gt.0) then
        bavg = bavg/float(nvalid)
        bnavg = 100.*bnavg/float(nvalid)
        bfavg = 200.*bfavg/float(nvalid)
        eavg = eavg/float(nvalid)
        enavg = 100.*enavg/float(nvalid)
        efavg = 200.*efavg/float(nvalid)
        ravg = sqrt(ravg/float(nvalid))
      else
        bavg = -999.
        bnavg = -999.
        bfavg = -999.
        eavg = -999.
        enavg = -999.
        efavg = -999.
        ravg = -999.
      endif

      write(11,*)
      write(11,'(a,10a1)')'Overall Statistics for ',(mspec(m,l),m=1,10)
      write(11,'(80a)') ('-',m=1,80)
      write(11,'(a,f6.1,1x,a)')'Average Prediction       :',uavg,units
      write(11,'(a,f6.1,1x,a)')'Average Observation      :',oavg,units
      write(11,'(a,f6.1,1x,a)')'Difference in Averages   :',davg,units
      write(11,'(a,f6.1,1x,a)')'                         :',dnavg,'%'
      write(11,'(a,f6.1,1x,a)')'For Concentrations above :',cmin,units
      write(11,'(a,f6.1,1x,a)')'Bias          (absolute) :',bavg,units
      write(11,'(a,f6.1,1x,a)')'            (normalized) :',bnavg,'%'
      write(11,'(a,f6.1,1x,a)')'            (fractional) :',bfavg,'%'
      write(11,'(a,f6.1,1x,a)')'Error         (absolute) :',eavg,units
      write(11,'(a,f6.1,1x,a)')'            (normalized) :',enavg,'%'
      write(11,'(a,f6.1,1x,a)')'            (fractional) :',efavg,'%'
      write(11,'(a,f6.1,1x,a)')'RMS Error                :',ravg,units
      write(11,9070) npos,nvalid,100.*float(nvalid)/float(npos)
 9070 format('Number of total pairs: ',i4,/,
     &       'Number of valid pairs: ',i4,/,
     &       '                   (%) ',f5.1)
c     write(11,'(80a)') ('-',m=1,80)
 500  continue

c-----Write EPA definitions

c     write(11,*)
c     write(11,*)'EPA Definitions:'
c     write(11,*)'Bias                   = Bias '
c     write(11,*)'Normalized Bias        = Bias (%)'
c     write(11,*)'Gross Error            = Error '
c     write(11,*)'Normalized Gross Error = Error (%)'
      stop

 901  continue 
      write(*,*) 'PRED/OBS file not found' 
      write(*,'(a)') ipath 
      stop 
      end

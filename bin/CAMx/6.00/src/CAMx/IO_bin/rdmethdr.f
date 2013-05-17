      subroutine rdmethdr(iunit,filtype,igrid,begtim,begdate,endtim,
     &                    enddate,ncl,nrw,nly,xomod,yomod,
     &                    dxmod,dymod,iout,nvar)
      use grid
c 
c----CAMx v6.00 130506
c 
c     RDMETHDR reads the header of binary meteorological files,
c     and checks that all projection/grid information and variable lists
c     are consistent with the current run.
c                           
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c           
c     Modifications: 
c        None
c 
c     Input arguments: 
c        iunit               input file unit
c        filtype             File type label
c        igrid               grid index
c        begtim              model start time (HHMM) 
c        begdate             model start date (YYJJJ) 
c        endtim              model end time (HHMM) 
c        enddate             model end date (YYJJJ)
c        ncl                 number of grid columns
c        nrw                 number of grid rows
c        nly                 number of grid layers
c        xomod               X-origin for grid (km or deg)
c        yomod               Y-origin for grid (km or deg)
c        dxmod               X-grid size (km or deg)
c        dymod               Y-grid size (km or deg)
c        iout                output message file unit
c             
c     Output arguments: 
c        nvar                Number of variables on file
c             
c     Routines Called: 
c        none
c             
c     Called by: 
c        SRFPREP
c        METINIT
c 
      implicit none
      include 'camx.prm'
      include 'flags.inc'
c
      character*200 filname
      character*10 filtype
      integer iunit,iout,igrid,begdate,enddate,ncl,nrw,nly
      integer nvar
      real begtim,endtim
      real xomod,yomod,dxmod,dymod
c
      character*4 fname(10),note(60),varnam(10,MXVAR)
      character*10 namsrf(NLUZ03),
     &             nam3d(NUM3DMET),
     &             nam2d(NUM2DMET),
     &             namkv(NUMKVMET),
     &             namcld(NUMCLDMET)
      character*10 filnote,varname(MXVAR)
      integer itz,idat1,idat2,iutm,nx,ny,nz,iproj,istag
      integer ical
      integer l,ll,lll,n
      real tim1,tim2,plon,plat,orgx,orgy,dxin,dyin,t1,t2
c
      integer istrln
c
      data namsrf   /'WATER     ',
     &               'ICE       ',
     &               'LAKE      ',
     &               'ENEEDL    ',
     &               'EBROAD    ',
     &               'DNEEDL    ',
     &               'DBROAD    ',
     &               'TBROAD    ',
     &               'DDECID    ',
     &               'ESHRUB    ',
     &               'DSHRUB    ',
     &               'TSHRUB    ',
     &               'SGRASS    ',
     &               'LGRASS    ',
     &               'CROPS     ',
     &               'RICE      ',
     &               'SUGAR     ',
     &               'MAIZE     ',
     &               'COTTON    ',
     &               'ICROPS    ',
     &               'URBAN     ',
     &               'TUNDRA    ',
     &               'SWAMP     ',
     &               'DESERT    ',
     &               'MWOOD     ',
     &               'TFOREST   '/
      data nam3d    /'ZGRID_M   ',
     &               'PRESS_MB  ',
     &               'TEMP_K    ',
     &               'HUMID_PPM ',
     &               'UWIND_MpS ',
     &               'VWIND_MpS '/
      data nam2d    /'TSURF_K   '/
      data namkv    /'KV_M2pS   '/
      data namcld   /'CLODW_GpM3',
     &               'RAINW_GpM3',
     &               'SNOWW_GpM3',
     &               'GRPLW_GpM3',
     &               'CLOUDOD   '/
c
c-----Entry point
c
c
c-----Get name of file attached to this unitnumber ---
c
      inquire(unit=iunit,name=filname)
c
c-----Read 1st header record and check inputs 
c             
      rewind(iunit)
      read(iunit,ERR=300) fname,note,itz,nvar,idat1,tim1,idat2,tim2
      write(filnote,'(10a1)') (note(n),n=1,10) 
c
      do n=1,10
        if( ichar(filnote(n:n)) .EQ. 0 ) filnote(n:n) = ' '
      enddo
      if (filnote.ne.filtype) then
        write(iout,'(//,a)') 'ERROR in RDMETHDR:'
        write(iout,'(a,i10)') 'grid #: ',igrid
        write(iout,*) 'Mismatch in file type.'
        write(iout,*) 'File contains: ',filnote
        write(iout,*) 'Expecting: ',filtype
        write(iout,*) 'Filename is: '
        write(iout,*) filname(:istrln(filname))
        write(iout,*)
        write(iout,*) 'NOTE: Some file formats changed with this version.'
        write(iout,*) 'Make sure you are using the new format.'
        call camxerr()
      endif   
      if (filnote.ne.'SURFACE   ' .and. itz.ne.itzon) then
        write(iout,'(//,a)') 'ERROR in RDMETHDR:'
        write(iout,'(a,i10)') 'grid #: ',igrid
        write(iout,'(a,i10)') 'Mismatch in time zone'
        write(iout,*) 'File contains: ',itz
        write(iout,*) 'Expecting: ',itzon
        write(iout,*) 'Filename is: '
        write(iout,*) filname(:istrln(filname))
        write(iout,*)
        call camxerr()
      endif
c
c  --- make sure that endtime check passes for cloud file ---
c
      if( filnote .EQ. 'CLDMET    ' ) then
          if( INT(tim2) .EQ. 24 ) then
            idat2 = idat2 + 1 
            tim2 = 0.
            if( MOD(idat2,1000) .GT. 365 ) then
               if( MOD(INT(idat2/1000),4) .EQ. 0 ) then
                  if( MOD(idat2,1000) .EQ. 367 )
     &                    idat2 = (INT(idat2/1000)+1)*1000 + 1
               else
                  idat2 = (INT(idat2/1000)+1)*1000 + 1
               endif
            endif
          endif
          tim2 = tim2 + 1.
      endif
      if( INT(tim2) .EQ. 24 ) then
         idat2 = idat2 + 1
         tim2 = 0.
         if( MOD(idat2,1000) .GT. 365 ) then 
            if( MOD(INT(idat2/1000),4) .EQ. 0 ) then 
               if( MOD(idat2,1000) .EQ. 367 )
     &                    idat2 = (INT(idat2/1000)+1)*1000 + 1
            else
               idat2 = (INT(idat2/1000)+1)*1000 + 1 
            endif
         endif
      endif
      tim1 = 100.*tim1
      tim2 = 100.*tim2
      if (idat1.gt.begdate) then 
        write(iout,'(//,a)') 'ERROR in RDMETHDR:'
        write(iout,'(a,i10)')  '    grid #: ',igrid
        write(iout,'(a,a)')    '   filtype: ',filtype
        write(iout,'(a,a)') filnote,'start date > simulation start date'
        ical = idat1
        call caldate(ical)
        write(iout,'(a,a,2i10)') filnote,':',idat1,ical
        ical = begdate
        call caldate(ical)
        write(iout,'(a,2i10)')  'Sim start :',begdate,ical
        write(iout,*) 'Filename is: '
        write(iout,*) filname(:istrln(filname))
        write(iout,*)
        call camxerr()
      elseif (idat1.eq.begdate .and. tim1.gt.begtim) then 
        write(iout,'(//,a)') 'ERROR in RDMETHDR:'
        write(iout,'(a,i10)')  '    grid #: ',igrid
        write(iout,'(a,a)')    '   filtype: ',filtype
        write(iout,'(a,a)') filnote,'start time > simulation start time'
        write(iout,'(a,a,i10)') filnote,':',tim1
        write(iout,'(a,i10)')  'Sim start :',begtim
        write(iout,*) 'Filename is: '
        write(iout,*) filname(:istrln(filname))
        write(iout,*)
        call camxerr()
      elseif (idat2.lt.enddate) then 
        write(iout,'(//,a)') 'ERROR in RDMETHDR:'
        write(iout,'(a,i10)')  '    grid #: ',igrid
        write(iout,'(a,a)')    '   filtype: ',filtype
        write(iout,'(a,a)') filnote,'end date < simulation end date'
        ical = idat2
        call caldate(ical)
        write(iout,'(a,a,i10)') filnote,':',idat2,ical
        ical = begdate
        call caldate(ical)
        write(iout,'(a,i10)')  '  Sim end :',enddate,ical
        write(iout,*) 'Filename is: '
        write(iout,*) filname(:istrln(filname))
        write(iout,*)
        call camxerr()
      elseif (idat2.eq.enddate .and. tim2.lt.endtim) then 
        write(iout,'(//,a)') 'ERROR in RDMETHDR:'
        write(iout,'(a,i10)')  '    grid #: ',igrid
        write(iout,'(a,a)')    '   filtype: ',filtype
        write(iout,'(a,a)') filnote,'end time < simulation end time'
        write(iout,'(a,a,i10)') filnote,':',tim2
        write(iout,'(a,i10)')  '  Sim end :',endtim
        write(iout,*) 'Filename is: '
        write(iout,*) filname(:istrln(filname))
        write(iout,*)
        call camxerr()
      endif 
c
      if (nvar.gt.MXVAR) then
        write(iout,'(//,a)') 'ERROR in RDMETHDR:'
        write(iout,'(a,i10)') ' grid #: ',igrid
        write(iout,'(a,a)')   'filtype: ',filtype
        write(iout,*) 'Filename is: '
        write(iout,*) filname(:istrln(filname))
        write(iout,'(a,i10)') 'Number of variables exceeds:',MXVAR
        write(iout,'(a,i10)') 'Number of variables read   :',nvar
        write(iout,'(2a)') 'Increase parameter MXVAR in RDMETHDR.F ',
     &                                               'and re-compile'
        write(iout,*)
        call camxerr()
      endif
c 
c-----Read 2nd header record and check inputs 
c 
      read(iunit) plon,plat,iutm,orgx,orgy,dxin,dyin,nx,ny,nz,iproj,
     &            istag,t1,t2
      if (.NOT.llatlon) then
        orgx = orgx/1000.
        orgy = orgy/1000.
        dxin = dxin/1000.
        dyin = dyin/1000.
      endif
c
      if (filtype.eq.'3DMET') then
        if (igrid.eq.1) then
          lstagw = .true.
          if (istag.eq.0) lstagw = .false.
        else
          if ((istag.eq.0 .and. lstagw) .or.
     &        (istag.eq.1 .and. .not.lstagw)) then
            write(iout,'(//,a)') 'ERROR in RDMETHDR:'
            write(iout,'(a,i10)') '   grid #: ',igrid
            write(iout,'(a,a)')   '  File type: ',filnote
            write(iout,*) 
     &        'Mismatch in staggered wind flag with master grid'
            write(iout,*) '         Master grid stagger is: ',lstagw
            write(iout,*) 'Flag on 3D met file is (1=true): ',istag
            write(iout,*) 'Filename is: '
            write(iout,*) filname(:istrln(filname))
            write(iout,*)
            call camxerr()
          endif
        endif
      endif
c
      if (llatlon    .and. iproj.ne.0) then
        write(iout,'(//,a)') 'ERROR in RDMETHDR:'
        write(iout,'(a,i10)') '   grid #: ',igrid
        write(iout,'(a,a)')   '  File type: ',filnote
        write(iout,*) 'CAMx grid is lat/lon, met projection should be 0'
        write(iout,*) 'Met file projection is: ',iproj
        write(iout,*) 'Filename is: '
        write(iout,*) filname(:istrln(filname))
        write(iout,*)
        call camxerr()
      elseif (lutm   .and. iproj.ne.1) then
        write(iout,'(//,a)') 'ERROR in RDMETHDR:'
        write(iout,'(a,i10)') '   grid #: ',igrid
        write(iout,'(a,a)')   '  File type: ',filnote
        write(iout,*) 'CAMx grid is UTM, met projection should be 1'
        write(iout,*) 'Met file projection is: ',iproj
        write(iout,*) 'Filename is: '
        write(iout,*) filname(:istrln(filname))
        write(iout,*)
        call camxerr()
      elseif (lambrt .and. iproj.ne.2) then
        write(iout,'(//,a)') 'ERROR in RDMETHDR:'
        write(iout,'(a,i10)') '   grid #: ',igrid
        write(iout,'(a,a)')   '  File type: ',filnote
        write(iout,*) 'CAMx grid is LCP, met projection should be 2'
        write(iout,*) 'Met file projection is: ',iproj
        write(iout,*) 'Filename is: '
        write(iout,*) filname(:istrln(filname))
        write(iout,*)
        call camxerr()
      elseif (lpolar .and. iproj.ne.3) then
        write(iout,'(//,a)') 'ERROR in RDMETHDR:'
        write(iout,'(a,i10)') '   grid #: ',igrid
        write(iout,'(a,a)')   '  File type: ',filnote
        write(iout,*) 'CAMx grid is polar, met projection should be 3'
        write(iout,*) 'Met file projection is: ',iproj
        write(iout,*) 'Filename is: '
        write(iout,*) filname(:istrln(filname))
        write(iout,*)
        call camxerr()
      endif
c
      if (iproj .eq. 1 .and. iutm .ne. iuzon) then
        write(iout,'(//,a)') 'ERROR in RDMETHDR:'
        write(iout,'(a,i10)') '   grid #: ',igrid
        write(iout,'(a,a)')   '  File type: ',filnote
        write(iout,*) 'CAMx UTM zone does not match zone from met file'
        write(iout,*) 'Met file UTM zone is: ',iutm
        write(iout,*) '    CAMx UTM zone is: ',iuzon
        write(iout,*) 'Filename is: '
        write(iout,*) filname(:istrln(filname))
        write(iout,*)
        call camxerr()
      endif
      if (iproj.eq.2) then
        if (abs(plon-xlonc).gt.0.001 .or. abs(plat-ylatc).gt.0.001) then
          write(iout,'(//,a)') 'ERROR in RDMETHDR:'
          write(iout,'(a,i10)') '   grid #: ',igrid
          write(iout,'(a,a)')   '  File type: ',filnote
          write(iout,*) 'CAMx LCP does not match met file'
          write(iout,*) 'Met file pole is: ',plon,plat
          write(iout,*) '    CAMx pole is: ',xlonc,ylatc
          write(iout,*) 'Filename is: '
          write(iout,*) filname(:istrln(filname))
          write(iout,*)
          call camxerr()
        endif
        if (abs(t1-tlat1).gt.0.001 .or. abs(t2-tlat2).gt.0.001) then
          if (abs(t1-tlat2).gt.0.001 .or. abs(t2-tlat1).gt.0.001) then
            write(iout,'(//,a)') 'ERROR in RDMETHDR:'
            write(iout,'(a,i10)') '   grid #: ',igrid
            write(iout,'(a,a)')   '  File type: ',filnote
            write(iout,*) 'CAMx LCP does not match met file'
            write(iout,*) 'Met file true lats are: ',t1,t2
            write(iout,*) '    CAMx true lats are: ',tlat1,tlat2
            write(iout,*) 'Filename is: '
            write(iout,*) filname(:istrln(filname))
            write(iout,*)
            call camxerr()
          endif
        endif
      endif
      if (iproj.eq.3) then
        if (abs(plon-polelon).gt.0.001 .or. 
     &      abs(plat-polelat).gt.0.001) then
          write(iout,'(//,a)') 'ERROR in RDMETHDR:'
          write(iout,'(a,i10)') '   grid #: ',igrid
          write(iout,'(a,a)')   '  File type: ',filnote
          write(iout,*) 'CAMx pole does not match pole from met file'
          write(iout,*) 'Met file pole is: ',plon,plat
          write(iout,*) '    CAMx pole is: ',polelon,polelat
          write(iout,*) 'Filename is: '
          write(iout,*) filname(:istrln(filname))
          write(iout,*)
          call camxerr()
        endif
      endif
c
      if (abs(orgx-xomod).gt.0.001 .or. abs(orgy-yomod).gt.0.001) then
        write(iout,'(//,a)') 'ERROR in RDMETHDR:'
        write(iout,*)'Met origin not equal to model origin'
        write(iout,'(a,i10)')   '   grid #: ',igrid
        write(iout,'(a,a)')   '  File type: ',filnote
        write(iout,'(a,2f12.4)')' Met file: ',orgx,orgy
        write(iout,'(a,2f12.4)')'    model: ',xomod,yomod
        write(iout,*) 'Filename is: '
        write(iout,*) filname(:istrln(filname))
        write(iout,*)
        call camxerr()
      endif
      if (abs(dxin-dxmod).gt.0.001 .or. abs(dyin-dymod).gt.0.001) then
        write(iout,'(//,a)') 'ERROR in RDMETHDR:'
        write(iout,*)'Met cell size not equal to model cell size'
        write(iout,'(a,i10)')   '   grid #: ',igrid
        write(iout,'(a,a)')   '  File type: ',filnote
        write(iout,'(a,2f10.4)')' Met file: ',dxin,dyin
        write(iout,'(a,2f10.4)')'    model: ',dxmod,dymod
        write(iout,*) 'Filename is: '
        write(iout,*) filname(:istrln(filname))
        write(iout,*)
        call camxerr()
      endif
      if (filnote.eq.'2DMET' .or. filnote.eq.'SURFACE') then
        if (nx.ne.ncl .or. ny.ne.nrw) then
          write(iout,'(//,a)') 'ERROR in RDMETHDR:'
          write(iout,*)'Met grid size not equal to model grid size '
          write(iout,*)       '   grid #: ',igrid
          write(iout,'(a,a)')   '  File type: ',filnote
          write(iout,*)' Met file: ',nx,ny
          write(iout,*)'    model: ',ncl,nrw
          write(iout,*) 'Filename is: '
          write(iout,*) filname(:istrln(filname))
          write(iout,*)
          call camxerr()
        endif 
      else
        if (nx.ne.ncl .or. ny.ne.nrw .or. nz.ne.nly) then
          write(iout,'(//,a)') 'ERROR in RDMETHDR:'
          write(iout,*)'Met grid size not equal to model grid size '
          write(iout,*)       '   grid #: ',igrid
          write(iout,'(a,a)')   '  File type: ',filnote
          write(iout,*)' Met file: ',nx,ny,nz
          write(iout,*)'    model: ',ncl,nrw,nly
          write(iout,*) 'Filename is: '
          write(iout,*) filname(:istrln(filname))
          write(iout,*)
          call camxerr()
        endif 
      endif
c
c-----Read 3rd & 4th header 
c 
      read(iunit)
      read(iunit) ((varnam(n,l),n=1,10),l=1,nvar) 
      do l = 1,nvar
        write(varname(l),'(10a1)') (varnam(n,l),n=1,10)
      enddo
c 
c-----Check variable names against required fields
c 
      if (filnote.eq.'3DMET') then
        do 100 l = 1,NUM3DMET
          do ll = 1,nvar
            if (varname(ll).eq.nam3d(l)) goto 100
          enddo
          write(iout,'(//,a)') 'ERROR in RDMETHDR:'
          write(iout,*)'3D Met file does not have the required field:'
          write(iout,*)'   grid #: ',igrid
          write(iout,*)' Variable: ',nam3d(l)
          write(iout,*) 'Filename is: '
          write(iout,*) filname(:istrln(filname))
          write(iout,*)
          call camxerr()
 100    continue
      endif
      if (filnote.eq.'2DMET') then
        do 101 l = 1,NUM2DMET
          do ll = 1,nvar
            if (varname(ll).eq.nam2d(l)) goto 101
          enddo
          write(iout,'(//,a)') 'ERROR in RDMETHDR:'
          write(iout,*)'2D Met file does not have the required field:'
          write(iout,*)'   grid #: ',igrid
          write(iout,*)' Variable: ',nam2d(l)
          write(iout,*) 'Filename is: '
          write(iout,*) filname(:istrln(filname))
          write(iout,*)
          call camxerr()
 101    continue
      endif
      if (filnote.eq.'KVMET') then
        do 102 l = 1,NUMKVMET
          do ll = 1,nvar
            if (varname(ll).eq.namkv(l)) goto 102
          enddo
          write(iout,'(//,a)') 'ERROR in RDMETHDR:'
          write(iout,*)'VDiff file does not have the required field:'
          write(iout,*)'   grid #: ',igrid
          write(iout,*)' Variable: ',namkv(ll)
          write(iout,*) 'Filename is: '
          write(iout,*) filname(:istrln(filname))
          write(iout,*)
          call camxerr()
 102    continue
      endif
      if (filnote.eq.'CLDMET') then
        do 103 l = 1,NUMCLDMET
          do ll = 1,nvar
            if (varname(ll).eq.namcld(l)) goto 103
          enddo
          write(iout,'(//,a)') 'ERROR in RDMETHDR:'
          write(iout,*)'Cloud file does not have the required field:'
          write(iout,*)'   grid #: ',igrid
          write(iout,*)' Variable: ',namcld(ll)
          write(iout,*) 'Filename is: '
          write(iout,*) filname(:istrln(filname))
          write(iout,*)
          call camxerr()
 103    continue
      endif
      if (filnote.eq.'SURFACE') then
        do 105 l = 1,NLUZ03
          do ll = 1,nvar
            if (varname(ll).eq.namsrf(l)) goto 105
          enddo
          goto 200
 105    continue
      endif
c
      return
c
 200  continue
      write(iout,'(//,a)') 'ERROR in RDMETHDR:'
      write(iout,*)'   grid #: ',igrid
      write(iout,*) 'Filename is: '
      write(iout,*) filname(:istrln(filname))
      write(iout,*)'Surface file does not have the required field:'
      write(iout,*) namsrf(l)
      write(iout,*)'The surface file needs 26 landuse types:'
      do lll = 1,NLUZ03
        write(iout,*) namsrf(lll)
      enddo
      call camxerr()
c
 300  continue
      write(iout,'(//,a)') 'ERROR in RDMETHDR:'
      write(iout,*)'Could not read header of met file.'
      write(iout,*)'   grid #: ',igrid
      write(iout,*) 'Expecting: ',filtype
      write(iout,*) 'Filename is: '
      write(iout,*) filname(:istrln(filname))
      write(iout,*)
      write(iout,*) 'NOTE: Some file formats changed with this version.'
      write(iout,*) 'Make sure you are using the new format.'
      call camxerr()
c
      end

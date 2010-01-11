      subroutine cncprep(endtim,enddate)
      use camxcom
      use camxfld
      use filunit
      use grid
      use chmstry
c 
c----CAMx v5.10 090918
c 
c     CNCPREP reads the AIRQUALITY or INSTANT file header (depending on
c     whether this is a restart or not) and maps file species names to
c     the internal CAMx species list. The routine then writes headers
c     to new AVERAGE files
c 
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c           
c     Modifications: 
c        1/20/99   Grid cell size on file should be meters for all cartesian
c                  projections (UTM, LCP, PSP)
c        10/24/01  Removed BSWAP and converted integer strings to character*4
c        10/31/01  Added logic to ensure that a cold start reads an AIRQUALITY
c                  file and a restart reads an INSTANT file
c        11/06/01  Input dates are now Julian
c        8/25/06   Average output files now all UAM format, one file per grid
c        11/16/06  Now rewinds the files before writing the header
c 
c     Input arguments: 
c        endtim              model end time (HHMM)
c        enddate             model end date (YYJJJ)
c             
c     Output arguments: 
c        none
c             
c     Routines Called: 
c        none
c             
c     Called by: 
c        STARTUP 
c
      include 'camx.prm'
      include 'flags.com'
c
      character*4 ifile(10),note(60)
      character*4 icspec(10,MXSPEC+MXRADCL)
      integer enddate
      character*10 aqfil,infil,cnfil,avfil,icspc
c
      data aqfil /'AIRQUALITY'/
      data cnfil /'INSTANT   '/
      data avfil /'AVERAGE   '/
      data nseg,izero,ione /1,0,1/
      data zero /0./
c
c-----Entry point
c
      lairqul = .false.
      iunit = iic
      if (lrstrt) iunit = irstc
c
c-----Read 1st IC header record and check inputs
c
      read(iunit,ERR=7000,END=7000) ifile,note,iseg,nicspc,idat1,
     &                                             tim1,idat2,tim2
      if (INT(tim2) .EQ. 24 ) then
        idat2 = idat2 + 1
        tim2 = 0.
        if( MOD(idat2,1000) .GT. 365 ) then
           if( MOD(INT(idat2/1000),4) .EQ. 0 ) then
              if( MOD(idat2,1000) .EQ. 367 )
     &                     idat2 = (INT(idat2/1000)+1)*1000 + 1
           else
              idat2 = (INT(idat2/1000)+1)*1000 + 1
           endif
        endif
      endif
      write(infil,'(10a1)') (ifile(n),n=1,10)
      if (.not.lrstrt .and. infil.ne.aqfil) then
        write(iout,'(//,a)') 'ERROR in CNCPREP:'
        write(iout,*)'This is a cold start from Initial Conditions'
        write(iout,*)'IC input file is not labelled AIRQUALITY'
        call camxerr()
      endif
      if (lrstrt .and. infil.ne.cnfil) then
        write(iout,'(//,a)') 'ERROR in CNCPREP:'
        write(iout,*)'This is a restart from a Restart File'
        write(iout,*)'IC input file is not labelled INSTANT'
        call camxerr()
      endif
      if (infil.eq.aqfil) lairqul = .true.
      tim1 = 100.*tim1
      tim2 = 100.*tim2
      if (idat1.gt.begdate) then
        write(iout,'(//,a)') 'ERROR in CNCPREP:'
        write(iout,*)'IC start date > simulation start date'
        write(iout,*)'  IC file: ',idat1
        write(iout,*)'Sim start: ',begdate
        call camxerr()
      elseif (idat1.eq.begdate .and. tim1.gt.begtim) then
        write(iout,'(//,a)') 'ERROR in CNCPREP:'
        write(iout,*)'IC start time > simulation start time'
        write(iout,*)'  IC file: ',tim1
        write(iout,*)'Sim start: ',begtim
        call camxerr()
      endif
c
c-----Read 2nd IC header record and check inputs
c
      read(iunit,ERR=7001) orgx,orgy,izone,utmx,utmy,dx,dy,nx,ny,nz
      if (.NOT.llatlon) then
        dx = dx/1000.
        dy = dy/1000.
      endif
      if (abs(dx-delx).gt.0.001 .or. abs(dy-dely).gt.0.001) then
        write(iout,'(//,a)') 'WARNING in CNCPREP:'
        write(iout,*)'IC cell size not equal to model cell size'
        write(iout,'(a,2f10.4)')'  IC file: ',dx,dy
        write(iout,'(a,2f10.4)')'    model: ',delx,dely
        write(iout,*)
      elseif (nx.ne.ncol(1) .or. ny.ne.nrow(1) 
     &                          .or. nz.ne.nlay(1)) then
        write(iout,'(//,a)') 'ERROR in CNCPREP:'
        write(iout,*)'IC grid size not equal to model grid size'
        write(iout,*)'IC file: ',nx,ny,nz
        write(iout,*)'  model: ',ncol(1),nrow(1),nlay(1)
        write(iout,*)
        call camxerr()
      endif 
c
c-----Read 3rd & 4th IC header 
c
      read(iunit,ERR=7001) (idum,idum,idum,idum,n=1,iseg)
      read(iunit,ERR=7001) ((icspec(n,l),n=1,10),l=1,nicspc)
c
c-----Map IC species to model species
c
      do 20 l = 1,nspec
        licmap(l,1) = 0
        do 15 lic = 1,nicspc
          write(icspc,'(10a1)') (icspec(n,lic),n=1,10)
          if (icspc.eq.'HNO2      ') icspc = 'HONO      '
          if (icspc.eq.'HCHO      ' .and. kHCHO.eq.nspec+1)
     &                                        icspc = 'FORM      '
          if (icspc.eq.spname(l)) then
            licmap(l,1) = lic
            write(idiag,'(2(a,i5,2x,a))')
     &                   'Initial species ',lic,icspc,
     &                   ' mapped to model species ',l,spname(l)
            goto 20
          endif
 15     continue
        write(idiag,*)'Did not find species: ',spname(l),' on IC file'
        if (.not.lairqul) then
          write(iout,'(//,a)') 'ERROR in CNCPREP:'
          write(iout,*)'The INSTANT file must contain the same ',
     &                 'species as specified in the CHEMPARAM file'
          call camxerr()
        endif
 20   continue
      write(idiag,*)
c 
c-----Write average output concentration file headers
c
      idat1 = begdate
      idat2 = enddate
      tim1 = begtim/100.
      tim2 = endtim/100.
      if (.NOT.llatlon) then
        orgx = 1000.*xorg
        orgy = 1000.*yorg
        dx = 1000.*delx
        dy = 1000.*dely
        izone = 0
        if (lutm) izone = iuzon
      else
        orgx = xorg
        orgy = yorg
        dx = delx
        dy = dely
        izone = 0
      endif
      read(runmsg(1:60),'(60a1)') (note(n),n=1,60)
      read(avfil,'(10a1)') (ifile(n),n=1,10)
      do l = 1,navspc
        if(lavmap(l) .GT. 0 ) then
           read(spname(lavmap(l)),'(10a1)') (icspec(n,l),n=1,10)
        else if(lradmap(l) .GT. 0 ) then
           read(nmrad(lradmap(l)),'(10a1)') (icspec(n,l),n=1,10)
        endif
      enddo
      if( l3davg(1) ) then
        nlayer = nlay(1)
      else
        nlayer = 1 
      endif
c
c-----Master grid average header
c
      if (lhdfout) goto 9999 
      rewind(iavg(1))
      write(iavg(1)) ifile,note,nseg,navspc,idat1,tim1,idat2,tim2
      write(iavg(1)) zero,zero,izone,orgx,orgy,dx,dy,nx,ny,nlayer,
     &               izero,izero,zero,zero,zero
      write(iavg(1)) ione,ione,nx,ny
      write(iavg(1)) ((icspec(n,l),n=1,10),l=1,navspc)
      if( ngrid .EQ. 1 ) goto 9999
c
c-----Fine grid average headers
c
      do i = 2,ngrid 
        dxf   = dx/float(meshold(i))
        dyf   = dy/float(meshold(i))
        orgxf = orgx + dx*(inst1(i)-1) - dxf
        orgyf = orgy + dy*(jnst1(i)-1) - dyf
        if( l3davg(i) ) then
          nlayer = nlay(i)
        else
          nlayer = 1
        endif
        rewind(iavg(i))
        write(iavg(i)) ifile,note,nseg,navspc,idat1,tim1,idat2,tim2
        write(iavg(i)) zero,zero,izone,orgxf,orgyf,dxf,dyf,ncol(i),
     &                 nrow(i),nlayer,izero,izero,zero,zero,zero
        write(iavg(i)) ione,ione,ncol(i),nrow(i)
        write(iavg(i)) ((icspec(n,l),n=1,10),l=1,navspc)
      enddo 
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in CNCPREP:'
      write(iout,'(A)',ERR=9999)'Reading restart file for coarse grid.'
      write(iout,'(2A)',ERR=9999)'Make sure the filename is specified ',
     &          'correctly and the previous day finished correctly.'
      call camxerr()
c
 7001 continue
      write(iout,'(//,a)') 'ERROR in CNCPREP:'
      write(iout,'(2A)',ERR=9999)'Reading the header of restart file ',
     &                                  'for coarse grid.'      
      call camxerr()
c
 9999 continue
      return
      end

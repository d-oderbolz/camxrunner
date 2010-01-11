      subroutine startup(version,inptim,inpdate,emstim,emsdate,haztim,
     &                   hazdate,ozntim,ozndate,snotim,snodate,
     &                   bndtim,bnddate,wrttim,wrtdate,endtim,enddate)
c
c----CAMx v4.42 070603
c
c     STARTUP is the main initialization and setup routine for CAMx.
c     It performs the following tasks:
c        - initializes certain vector/array and scalar variables 
c        - reads and checks the user input file
c        - sets model simulation clock
c        - opens all I/O files
c        - reads all time-invariant files and look-up tables
c        - reads/writes headers from/to UAM-formatted I/O files
c        - calculates grid parameters
c        - initializes the PiG submodel
c                          
c     Copyright 1996-2007
c     ENVIRON International Corporation
c          
c     Modifications:
c        1/29/99   Added diagnostic printout of layer nesting, and error
c                  messages if layer nesting calculation is invalid
c        2/12/99   Removed assignment of negative longitude for xorg
c        4/26/99   Added Piecewise Parabolic Method for horizontal advection
c        5/24/99   Fixed bug in filling idfin array
c        10/20/00  Added CAMx version as first record on control file
c        11/06/01  Added Y2K flag; conversion of simulation date to Julian 
c                   now done immediately after reading from control file
c        1/21/02   Added RTRAC tech type to OSAT
c        1/25/02   Revised input I/O frequencies and max time step to minutes,
c                  and improved checks on values
c        7/5/02    Added code to handle new IRON-PiG option
c        1/10/03   Added prep of deposition output files 
c        03/21/03  Removed the OSAT technology type OPPAT
c        05/01/03  Added snow cover and other options to AHO file
c        11/10/03  Added sampling grid setup for RTRAC+IRONPIG
c        10/06/04  Restructured for namelist input
c        10/12/04  Added read of water vapor and vertical diffusivity as
c                  time-interpolated variables
c        10/14/04  Special checks added for mechanism 10
c        7/29/05   Added sampling grid setup for IRONPIG for average concs
c
c     Input arguments:
c        version             model version character string
c
c     Output arguments:
c        inptim              next time to read environmental fields (HHMM)
c        inpdate             next date to read environmental fields (YYJJJ)
c        emstim              next time to read emissions (HHMM)
c        emsdate             next date to read emissions (YYJJJ)
c        haztim              next update time for haze map (HHMM)  
c        hazdate             next update date for haze map (YYJJJ)  
c        ozntim              next update time for ozone map (HHMM)  
c        ozndate             next update date for ozone map (YYJJJ) 
c        snotim              next update time for snow cover (HHMM)
c        snodate             next update date for snow cover (YYJJJ)
c        bndtim              next update time for boundary conditions (HHMM)  
c        bnddate             next update date for boundary conditions (YYJJJ) 
c        wrttim              next time to output concentrations (HHMM)
c        wrtdate             next date to output concentrations (YYJJJ)
c        endtim              model end time (HHMM)
c        enddate             model end date (YYJJJ)
c
c     Routines Called:
c        READNML,  READCHM,  READPHT,  AHOPREP,  BNDPREP,  GRDPREP,
c        NSTPREP,  IASSGN2D, INTERP2D, METINIT,  VNMSHCAL, SRFPREP, 
c        LUASSGN,  PNTPREP,  AREAPREP, CNCPREP,  PIGPREP,  INTERPV, 
c        FINWIND   RASSGN3D, DEPPREP
c
c     Called by:
c        CAMx
c
      include 'camx.prm'
      include 'camx.com'
      include 'camxfld.com'
      include 'filunit.com'
      include 'chmstry.com'
      include 'ahomap.com'
      include 'grid.com'
      include 'pigsty.com'
      include 'flags.com'
c
c======================== Probing Tools Begin ==========================
c
      include 'tracer.com'
      include 'procan.com'
c
c======================== Probing Tools End ============================
c
      integer inpdate,emsdate,wrtdate,enddate,hazdate,ozndate,
     &        bnddate,snodate
      real inptim,emstim,wrttim,endtim,haztim,ozntim,bndtim,snotim
      character*20 version
c
c-----Entry point
c
      icur_unit = 6
      call readnml(version,enddate,endtim)
c
c-----Initialize simulation clock
c
      time = begtim
      date = begdate
c
c-----Read chemistry parameters
c
      call readchm
c
c-----Read photolysis rates lookup table
c
      if (lchem .and. idmech.ne.10) call readpht
c
c-----Read albedo/haze/ozone file header
c
      if ((lchem .or. ldry) .and. idmech.ne.10) call ahoprep
c
c-----Check number of average output species 
c             
      if (navspc.gt.nspec) then 
        write(iout,'(//,a)') 'ERROR in STARTUP:'
        write(iout,*)'Number of average species to be output ', 
     &               'is greater than number of species to model' 
        write(iout,*)'NAVSPC,NSPEC: ',navspc,nspec 
        call camxerr()
      endif
c  
c-----Map average species to model species
c  
      do 46 lav = 1,navspc
c
c---- make sure every species name has been assigned
c
        if( spavg(lav) .EQ. ' ' ) then
            write(iout,'(//,A)') 'ERROR in STARTUP:'
            write(iout,'(A,I2,A)')'Average species number: ',lav,
     &                                   ' has not been initalized.'
            write(iout,'(2A)')'Check that the number of output ',
     &                 'species and name of each species is correctly '
            write(iout,'(A)')'assigned in your CAMx control file.'
            call camxerr()
        endif
c
        do l = 1,nspec  
          if (spavg(lav).eq.spname(l)) then  
            lavmap(lav) = l
            write(idiag,'(2(a,i5,2x,a))')
     &                   'Average species ',lav,spavg(lav),
     &                   ' mapped to modeled species ',l,spname(l)
            goto 46  
          endif  
        enddo  
        write(iout,'(//,a)') 'ERROR in STARTUP:'
        write(iout,*)'Did not find average species: ',
     &            spavg(lav)(:istrln(spavg(lav))),' in species list.'
        write(iout,*)'Either remove this species from the list of ',
     &                                     'average species in your'
        write(iout,*)'CAMx control file, or use the appropriate ',
     &                                               'chemparam file.'
        if( aeropt .EQ. 'CMU' ) then
             write(iout,*) 'If you are using the PM sectional model ',
     &                                     'you have to include the '
              write(iout,*) 'section number in the species name.'
        endif
        call camxerr()
 46   continue
      write(idiag,*)
c
c-----Call routine to write the headers of all sampling grids
c
      if (lsample .and. .not.lhdfout) then
        call smpprep(.false.,endtim,enddate)
      endif
c
c======================== Source Apportion Begin =======================
c
c  --- check whether probing tool allowed with this mechanism ---
c
      if( idmech.eq.10 .and. (ltrace .or. lddm .or. lirr)) then
        write(iout,'(//,A)') 'ERROR in STARTUP:'
        write(iout,'(A)') 'Probing tool incompatible with mechanism'
        write(iout,'(A,A10)') 'Technology type     :',tectyp
        write(iout,'(A,i10)') 'Chemical mechanism  :',idmech
        call camxerr()
      endif
c
      if( ltrace ) then
c
c   --- call routine to initialize the source apportionment
c       data structures ---
c
          call initsa(version,ncol,nrow,
     &                             begdate,begtim,enddate,endtim)
c
c   --- if this is a restart, call routine to read the instantaneous files ---
c
          if( lrstrt ) then
             call rdinstsa(begdate,time,ncol(1),nrow(1),nlay(1),
     &                     ntotsp,ptconc(1))
          endif
c
c  ---- call routine to calculate the average reactivity
c       of boundary conditions ---
c
          if( tectyp .NE. RTRAC ) then
              call clcbwt(begdate,begtim,enddate,endtim,
     &                                        ncol(1),nrow(1),nlay(1))
          endif
c
c   --- call routine to write the header of the coarse grid
c       average surface tracer concentrations file ---
c
          if( lsfcfl .and. .not.lhdfout) then
             call hdrwsa(iowsfc,sfcfil,'AVERAGE   ',
     &                   ntotsp,1,begdate,begtim,enddate,endtim)
          endif
c
c   --- if doing nitrate species in PSAT, initialize the
c       number of IRR reactions ---
c
          if( lozone .OR. lnitrate ) nirrrxn =  nreact
c
c   --- call routine to write the headers of all sampling grids
c
          if (tectyp .EQ. RTRAC .AND. lsample .AND. lsmptrc .and.
     &        .not.lhdfout) then
             call smpprep(.true.,endtim,enddate)
          endif
      endif
c
c========================= Source Apportion End ========================
c
c
c======================== DDM Begin ====================================
c
      if( lddm ) then
c
c  --- check that switches are compatible ---
c
         if( naero .GT. 0 ) then
             write(iout,'(//,a)') 'ERROR in STARTUP:'
             write(iout,*) 'Cannot use DDM with Aerosol chemistry.'
             write(iout,*) 
     &          '  Turn off DDM or use a different chemical mechanism.'
             call camxerr()
         endif
c
c  --- check that chemical mechanism is valid ---
c
         if( idmech .LT. 3 .OR. idmech .GT. 5 ) then
            write(iout,'(//,a)') 'ERROR in STARTUP:'
            write(iout,*) 'Chemical mechanism is invalid for ',
     &                                               'for use with DDM.'
            write(iout,'(A,I2)') ' Mechanism ID: ',idmech
            write(iout,*) '  Please supply a DDM compatible chemistry ',
     &                                             'file and try again.'
            call camxerr()
         endif
c
c  --- check that advection solver is correct ---
c
         if( iadvct .ne. 2 ) then
            write(iout,'(//,a)') 'ERROR in STARTUP:'
            write(iout,*) 'Advection solver used with DDM ',
     &                                                   'must be BOTT.'
            write(iout,*)'Please change the control file and try again.'
            call camxerr()
          endif
c
c   --- call routine to initialize the source apportionment
c       data structures ---
c
         call initsa(version,ncol,nrow,begdate,begtim,enddate,endtim)
c
c   --- if this is a restart, call routine to read the instantaneous files ---
c
         if( lrstrt ) then
             call rdinstsa(begdate,time,ncol(1),nrow(1),nlay(1),
     &                     ntotsp,ptconc(1))
         endif
c
c   --- call routine to write the header of the coarse grid
c       average surface tracer concentrations file ---
c
         if( lsfcfl .and. .not.lhdfout) then
             call hdrwsa(iowsfc,sfcfil,'AVERAGE   ',
     &                        ntotsp,1,begdate,begtim,enddate,endtim)
         endif
      endif
c
c======================== DDM End ====================================
c
c-----Read BC file header and set irregular boundary cells
c
      call bndprep(begtim,begdate,endtim,enddate)
c
c-----Calculate grid parameters for coarse grid
c
      call grdprep(ncol(1),nrow(1),cellon(1),cellat(1),mapscl(1))
c
c-----Calculate nested grid mapping parameters
c
      if (ngrid.gt.1) then
        call nstprep
      else
        do j=1,nrow(1)
          do i=1,ncol(1)
            n = (j-1)*ncol(1) + i
            idfin(n) = 0
          enddo
        enddo
        mapgrd(1) = 1
        nchdrn(1) = 0
        meshold(1) = 1
        nmesh(1) = 1
      endif
c
c======================== Source Apportion Begin =======================
c
      if( ltrace ) then
          if( tectyp .EQ. RTRAC ) then
c
c   --- get each emissions file to the correct place ----
c
              do i=1,ngrid
                if( ltemfl(i,1) .OR. ltptfl(1) ) 
     &             call empreprt(begdate,begtim,i)
              enddo
c
c   ---- call routine to read the receptor definition file ---
c
              call rdrcprt()
c
c  --- call routine to write the header of receptor average file ---
c
              call hdrcprt(begdate,begtim,enddate,endtim)
c
c  ---- call routine to calculate the average reactivity
c       of initial conditions ---
c
          else
              call clciwt(begdate,begtim,enddate,endtim,ly2k,
     &                ncol(1),nrow(1),nlay(1))
c
c   --- get each emissions files to the correct place ----
c
              do i=1,ngrid
                  call emprepsa(begdate,begtim,i)
              enddo
c
c  ---- call routine to calculate the average reactivity
c       of emissions ---
c
              call clcewt(enddate,endtim)
c
c   --- get each emissions files BACK to the correct place ----
c
              do i=1,ngrid
                  call emprepsa(begdate,begtim,i)
              enddo
c
c   ---- call routine to read the receptor definition file ---
c
              call rercp()
c
c  --- call routine to write the header of receptor average file ---
c
              call hdrrcp(begdate,begtim,enddate,endtim)
          endif
      endif
c
c========================= Source Apportion End ========================
c
c
c======================== DDM Begin ====================================
c
          if( lddm ) then
c
c   ---- call routine to read the receptor definition file ---
c
              call rercp()
c
c  --- call routine to write the header of receptor average file ---
c
              call hdrrcpddm(begdate,begtim,enddate,endtim)
          endif
c
c======================== DDM End ====================================
c
c
c=================== Process Analysis Begin ==========================
c
c-----Call routine to initialize Process Analysis subdomains
c
      if( lproca ) then
         call pagrids()
c
c-----Call routine to write the header to the otuput files ---
c
         if( lipr ) call wrtiprhdr(begdate,begtim,enddate,endtim)
         if( lirr ) then
            call pasetup
            call wrtirrhdr(begdate,begtim,enddate,endtim)
            if( lsfcfl ) then
                if( l3davg ) then
                    nlayer = nlay(1) 
                else
                    nlayer = 1
                endif
                call hdrwsa(iowsfc,sfcfil,'AVERAGE   ',
     &                      ntotsp,nlayer,begdate,begtim,enddate,endtim)
            endif
         endif
      endif
c
c===================== Process Analysis End ============================
c
c-----Assign albedo values for fine grids
c
      do ip = 1,ngrid
        do ic = 1,nchdrn(ip)
          igrd = idchdrn(ic,ip)
          if( .NOT. lrdalb(igrd) ) call iassgn2d(
     &                  ncol(ip),nrow(ip),i1(igrd),j1(igrd),
     &                  nmesh(igrd),ncol(igrd),nrow(igrd),
     &                  icdalb(iptr2d(ip)),icdalb(iptr2d(igrd)))
          if( .NOT. lrdocn(igrd) ) call iassgn2d(
     &                  ncol(ip),nrow(ip),i1(igrd),j1(igrd),
     &                  nmesh(igrd),ncol(igrd),nrow(igrd),
     &                  icdocn(iptr2d(ip)),icdocn(iptr2d(igrd)))
          if( .NOT. lrddrt(igrd) ) call iassgn2d(
     &                  ncol(ip),nrow(ip),i1(igrd),j1(igrd),
     &                  nmesh(igrd),ncol(igrd),nrow(igrd),
     &                  icddrt(iptr2d(ip)),icddrt(iptr2d(igrd)))
          if( .NOT. lrdruf(igrd) ) call iassgn2d(
     &                  ncol(ip),nrow(ip),i1(igrd),j1(igrd),
     &                  nmesh(igrd),ncol(igrd),nrow(igrd),
     &                  icdruf(iptr2d(ip)),icdruf(iptr2d(igrd)))
        enddo
      enddo
c
c------Calculate grid parameters for fine grids
c
      do ip = 1,ngrid
        do ic = 1,nchdrn(ip)
          igrd = idchdrn(ic,ip)
          call interp2d(ncol(ip),nrow(ip),1,i1(igrd),j1(igrd),
     &                  nmesh(igrd),ncol(igrd),nrow(igrd),
     &                  cellat(iptr2d(ip)),cellat(iptr2d(igrd)) )
          call interp2d(ncol(ip),nrow(ip),1,i1(igrd),j1(igrd),
     &                  nmesh(igrd),ncol(igrd),nrow(igrd),
     &                  cellon(iptr2d(ip)),cellon(iptr2d(igrd)) )
          call interp2d(ncol(ip),nrow(ip),1,i1(igrd),j1(igrd),
     &                  nmesh(igrd),ncol(igrd),nrow(igrd),
     &                  mapscl(iptr2d(ip)),mapscl(iptr2d(igrd)) )
        enddo
      enddo
c
c-----Read met fields that are to be time-interpolated, for all grids,
c     to current time
c
      do igrd=1,ngrid
        call metinit(igrd,ihtp(igrd),iwind(igrd),itemp(igrd),
     &               ih2o(igrd),ikv(igrd),iout,
     &               ncol(igrd),nrow(igrd),nlay(igrd),
     &               height(iptr3d(igrd)),press(iptr3d(igrd)),
     &               depth(iptr3d(igrd)),windu(iptr3d(igrd)),
     &               windv(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &               tsurf(iptr2d(igrd)),water(iptr3d(igrd)),
     &               rkv(iptr3d(igrd)))
      enddo
c
c-----Determine vertical meshing number
c
      do ip = 1,ngrid
        do ic = 1,nchdrn(ip)
          ig = idchdrn(ic,ip)
c
c-----If ZP file is not provided, make sure that there is
c     no vertical meshing----
c
          if( ihtp(ig) .LE. 0 ) then
              if( nlay(ig) .NE. nlay(ip) ) then
                 write(iout,'(//,a)') 'ERROR in STARTUP:'
                 write(iout,*) 'A height/pressure file is required ',
     &                                          'for vertical nesting.'
                 write(iout,'(1X,A,I2,A)') 
     &                 'Set the number of layers for grid:',ig,
     &                        ' to the same value as its parent grid.'
                 write(iout,*) 'OR supply a height/pressure file.'
                 call camxerr()
               endif
          endif
c
          call vnmshcal(ig,ncol(ip),nrow(ip),nlay(ip),i1(ig),j1(ig),
     &                  ncol(ig),nrow(ig),nlay(ig),height(iptr3d(ip)),
     &                  height(iptr3d(ig)),nmshv(1,ig))
c
c-----If no ZP file is supplied call routines to 
c     interpolate ZP from parent grid---
c
          if( ihtp(ig) .LE. 0 ) then
            write(iout,'(a40,f7.0,i8.5,a,i3)')
     &                 'Assigning heights from parent grid',
     &                             time, date,' grid',ig
            call rassgn3d(ncol(ip),nrow(ip),nlay(ip),
     &           i1(ig),j1(ig),nmesh(ig),ncol(ig),nrow(ig),
     &                          height(iptr3d(ip)),height(iptr3d(ig)) )
            call rassgn3d(ncol(ip),nrow(ip),nlay(ip),
     &           i1(ig),j1(ig),nmesh(ig),ncol(ig),nrow(ig),
     &                          depth(iptr3d(ip)),depth(iptr3d(ig)) )
            write(iout,'(a40,f7.0,i8.5,a,i3)')
     &                 'Interpolating pressure from parent grid',
     &                             time, date,' grid',ig
            call interp2d(ncol(ip),nrow(ip),nlay(ip),
     &             i1(ig),j1(ig),nmesh(ig),ncol(ig),nrow(ig),
     &                            press(iptr3d(ip)),press(iptr3d(ig)) )
            call interpv(ncol(ip),nrow(ip),nlay(ip),ncol(ig),
     &                   nrow(ig),nlay(ig),nmesh(ig),
     &                   nmshv(1,ig),i1(ig),j1(ig),
     &                   height(iptr3d(ip)),
     &                   height(iptr3d(ig)),press(iptr3d(ig)) )
          endif
c
          write(idiag,*)
          write(idiag,'(a,i2)')'Vertical meshing factors for grid: ',ig
          kg1 = 1
          do kp = 1,nlay(ip)
            if (nmshv(kp,ig).lt.1) then
              write(iout,'(//,a)') 'ERROR in STARTUP:'
              write(iout,*) 'Vertical meshing factor < 1!'
              write(iout,*) 'In parent layer ',kp
              call camxerr()
            endif
            do kg = kg1,kg1+nmshv(kp,ig)-1
              if (nmshv(kp,ig).gt.1) then
                write(idiag,'(a,i2,a,i2)') 'Layer ',kg,
     &                                     '   is in parent layer ',kp
              else
                write(idiag,'(a,i2,a,i2)') 'Layer ',kg,
     &                                     ' matches parent layer ',kp
              endif
            enddo
            kg1 = kg1 + nmshv(kp,ig)
          enddo
          write(idiag,*)
        enddo
      enddo
c
c-----Interpolate wind fields that were not read to fine grids
c
      do ip = 1,ngrid 
        do ic = 1,nchdrn(ip) 
          igrd = idchdrn(ic,ip)
          iunit = iwind(igrd) 
          call finwind(iunit,ncol(ip),nrow(ip),nlay(ip),i1(igrd),
     &                 j1(igrd),nmesh(igrd),nmshv(1,igrd),ncol(igrd),
     &                 nrow(igrd),nlay(igrd),
     &                 windu(iptr3d(ip)),windv(iptr3d(ip)),
     &                 windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &                 igrd,date,time,iout ) 
c 
c-----Interpolate temperature fields that were not read to each grid
c 
          if (itemp(igrd).eq. 0) then
            call interp2d(ncol(ip),nrow(ip),nlay(ip),i1(igrd),j1(igrd),
     &                    nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    tempk(iptr3d(ip)),
     &                    tempk(iptr3d(igrd)) )
            call interpv(ncol(ip),nrow(ip),nlay(ip),ncol(igrd),
     &                   nrow(igrd),nlay(igrd),nmesh(igrd),
     &                   nmshv(1,igrd),i1(igrd),j1(igrd),
     &                   height(iptr3d(ip)),
     &                   height(iptr3d(igrd)),tempk(iptr3d(igrd)))
            call interp2d(ncol(ip),nrow(ip),1,i1(igrd),j1(igrd), 
     &                    nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    tsurf(iptr2d(ip)),tsurf(iptr2d(igrd)) ) 
          endif
c
c-----Interpolate water vapor fields that were not read to each grid
c
          if (ih2o(igrd).eq. 0) then
            call interp2d(ncol(ip),nrow(ip),nlay(ip),i1(igrd),j1(igrd),
     &                    nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    water(iptr3d(ip)),
     &                    water(iptr3d(igrd)) )
            call interpv(ncol(ip),nrow(ip),nlay(ip),ncol(igrd),
     &                   nrow(igrd),nlay(igrd),nmesh(igrd),
     &                   nmshv(1,igrd),i1(igrd),j1(igrd),
     &                   height(iptr3d(ip)),
     &                   height(iptr3d(igrd)),water(iptr3d(igrd)))
          endif
c
c-----Interpolate diffusivity fields that were not read to each grid
c
          if (ikv(igrd).eq. 0) then
            call interp2d(ncol(ip),nrow(ip),nlay(ip),i1(igrd),j1(igrd),
     &                    nmesh(igrd),ncol(igrd),nrow(igrd),
     &                    rkv(iptr3d(ip)),rkv(iptr3d(igrd)) )
            call interpv(ncol(ip),nrow(ip),nlay(ip),ncol(igrd),
     &                   nrow(igrd),nlay(igrd),nmesh(igrd),
     &                   nmshv(1,igrd),i1(igrd),j1(igrd),
     &                   height(iptr3d(ip)),
     &                   height(iptr3d(igrd)),rkv(iptr3d(igrd)))
          endif
        enddo
      enddo
c
c-----Read surface files and initialize arrays
c
      if (ldry .OR. ipigflg .NE. 0) then
        do igrd=1,ngrid
          call srfprep(igrd,ncol(igrd),nrow(igrd),fsurf(iptrlu(igrd)),
     &                 topo(iptr2d(igrd)))
        enddo
      endif
c
c-----Assign fine grid landuse fractions
c
      do ip = 1,ngrid
        do ic = 1,nchdrn(ip)
          igrd = idchdrn(ic,ip)
          if (isurf(igrd).eq.0) then
            call luassgn(ncol(ip),nrow(ip),NLU,i1(igrd),j1(igrd),
     &                   nmesh(igrd),ncol(igrd),nrow(igrd),
     &                   fsurf(iptrlu(ip)),fsurf(iptrlu(igrd)) )
          endif
        enddo
      enddo
c
c-----Read emission file headers
c
      if (lptsrc) call pntprep(begtim,begdate,endtim,enddate)
      if (larsrc) then
        do igrd = 1,ngrid
          dxmod = delx
          dymod = dely
          if (igrd.gt.1) then
            dxmod = delx/meshold(igrd)
            dymod = dely/meshold(igrd)
          endif
          call areaprep(igrd,begtim,begdate,endtim,enddate,
     &                  iarem(igrd),iout,idiag,dxmod,dymod)
        enddo
      endif
c
c======================== Source Apportion Begin =======================
c
c
c   --- if this is a restart, call routine to read the
c       instantaneous files for fine grids ---
c
      if( (ltrace .OR. lddm) .AND. lrstrt .AND. ngrid .GT. 1 )
     &                               call rdfgsa(begdate,time)
c
c======================== Source Apportion End =======================
c
c
c======================== DDM Begin ====================================
c
      if( lddm ) then
c
c   --- get each emissions files to the correct place,
c       NOTE:  Done here for DDM because we need the point locations
c       from regular model ----
c
          do i=1,ngrid
              call emprepsa(begdate,begtim,i)
          enddo
      endif
c
c======================== DDM End ====================================
c
c-----Read IC or restart files headers and write output concentration
c     file headers 
c 
      call cncprep(endtim,enddate) 
c
c-----Write deposition output file headers
c
      if (ldry .or. lwet) call depprep(endtim,enddate)
c
c-----Initialize PiG submodel
c
      if (ipigflg .NE. 0) call pigprep(begdate,begtim)
c
c-----Determine times and dates for next inputs/emissions/write
c
      inptim = time
      inpdate = date
      emstim = time 
      emsdate = date
      bndtim = time 
      bnddate = date
      haztim = time 
      hazdate = date
      ozntim = time 
      ozndate = date
      snotim = time
      snodate = date

      whr = aint(time/100.)
      wmn = amod(time,100.)
      wrttim = 100.*(whr + aint((wmn + dtout)/60.)) +
     &             amod((wmn + dtout),60.)
      wrtdate = date
      if (wrttim.ge.2400.) then 
        wrttim = wrttim - 2400. 
        wrtdate = wrtdate + 1 
        if( MOD(wrtdate,1000) .GT. 365 ) then
           if( MOD(INT(wrtdate/1000),4) .EQ. 0 ) then
              if( MOD(wrtdate,1000) .EQ. 367 )
     &                   wrtdate = (INT(wrtdate/1000)+1)*1000 + 1
           else
              wrtdate = (INT(wrtdate/1000)+1)*1000 + 1
           endif
        endif
      endif
c
c-----Everything worked correctly, return to calling routine
c
      call flush(iout)
      call flush(idiag)
      return
c
c-----Return point
c
      end

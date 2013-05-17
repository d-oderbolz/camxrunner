      subroutine areaprep(igrid,begtim,begdate,endtim,enddate,iarem,
     &                    iout,idiag,dxmod,dymod)
      use grid
      use chmstry
c 
c----CAMx v6.00 130506
c 
c     AREAPREP reads the header of binary area source emissions file,
c     and maps the area source species list to the internal CAMx species list
c                           
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c           
c     Modifications: 
c        1/20/99   Grid cell size from file should be meters for all cartesian
c                  projections (UTM, LCP, PSP)
c        10/24/01  Removed BSWAP and converted integer strings to character*4
c        11/06/01  Input dates are now Julian
c        02/09/02  Added code to handle end of year dates
c 
c     Input arguments: 
c        igrid               grid index
c        begtim              model start time (HHMM) 
c        begdate             model start date (YYJJJ) 
c        endtim              model end time (HHMM) 
c        enddate             model end date (YYJJJ)
c        iarem               area emissions file unit
c        iout                output message file unit
c        idiag               output diagnostic file unit
c        dxmod               model grid size in x-direction (deg or km) 
c        dymod               model grid size in y-direction (deg or km)
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
      include 'flags.inc'
c
      character*4 ifile(10),note(60)
      integer begdate,enddate
      character*10 arfil,infil,arspc 
c
      character*4 arspec(10,MXSPEC)
c
      data arfil /'EMISSIONS '/
c
c-----Entry point
c
c-----Initialize the mapping array ---
c
      do l=1,nspec
        larmap(l,igrid) = 0
      enddo
c
c-----If the emissions file was not supplied, set variables
c     based on the parent ---
c
      if( iarem .EQ. 0 ) then
         do ip=1,ngrid
            do ic = 1,nchdrn(ip)
              if( igrid .EQ. idchdrn(ic,ip) ) then
                 narspc(igrid) = narspc(ip)
                 do l = 1,nspec
                    larmap(l,igrid) = larmap(l,ip)
                 enddo
              endif
            enddo
         enddo
         goto 9999
      endif
c
c-----Read 1st AREA header record and check inputs 
c             
      rewind(iarem)
      read(iarem) ifile,note,nseg,narspc(igrid),idat1,tim1,idat2,tim2
c             
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
      write(infil,'(10a1)') (ifile(n),n=1,10) 
      if (infil.ne.arfil) then 
        write(iout,'(//,a)') 'ERROR in AREAPREP:'
        write(iout,*)'AREA input file is not labelled EMISSIONS' 
        call camxerr()
      endif   
      tim1 = 100.*tim1
      tim2 = 100.*tim2
      if (idat1.gt.begdate) then 
        write(iout,'(//,a)') 'WARNING in AREAPREP:'
        write(iout,*)'AREA start date > simulation start date' 
        write(iout,*)'AREA file: ',idat1
        write(iout,*)'Sim start: ',begdate 
        if (.not.le1day) then
          write(iout,*)'CAMx expecting day-specific emissions: Stopping'
          call camxerr()
        endif
      elseif (idat1.eq.begdate .and. tim1.gt.begtim) then 
        write(iout,'(//,a)') 'ERROR in AREAPREP:'
        write(iout,*)'AREA start time > simulation start time' 
        write(iout,*)'AREA file: ',tim1
        write(iout,*)'Sim start: ',begtim 
        call camxerr()
      elseif (idat2.lt.enddate) then 
        write(iout,'(//,a)') 'WARNING in AREAPREP:'
        write(iout,*)'AREA end date < simulation end date' 
        write(iout,*)'AREA file: ',idat2
        write(iout,*)'  Sim end: ',enddate 
        if (.not.le1day) then
          write(iout,*)'CAMx expecting day-specific emissions: Stopping'
          call camxerr()
        endif
      elseif (idat2.eq.enddate .and. tim2.lt.endtim) then 
        write(iout,'(//,a)') 'WARNING in AREAPREP:'
        write(iout,*)'AREA end time < simulation end time' 
        write(iout,*)'AREA file: ',tim2
        write(iout,*)'  Sim end: ',endtim 
        call camxerr()
      endif 
c 
c-----Read 2nd AREA header record and check inputs 
c 
      read(iarem) orgx,orgy,izone,utmx,utmy,dx,dy,nx,ny,nz 
      if (.NOT.llatlon) then
        dx = dx/1000.
        dy = dy/1000.
      endif
      if (abs(dx-dxmod).gt.0.001 .or. abs(dy-dymod).gt.0.001) then
        write(iout,'(//,a)') 'WARNING in AREAPREP:'
        write(iout,*)'AREA cell size not equal to model cell size'
        write(iout,'(a,2f10.4)')'AREA file: ',dx,dy
        write(iout,'(a,2f10.4)')'    model: ',dxmod,dymod
        write(iout,*)
      elseif (nx.ne.ncol(igrid) .or. ny.ne.nrow(igrid)) then
        write(iout,'(//,a)') 'ERROR in AREAPREP:'
        write(iout,*)'AREA grid size not equal to model grid size '
        write(iout,*)'   grid #: ',igrid
        write(iout,*)'AREA file: ',nx,ny,nz
        write(iout,*)'    model: ',ncol(igrid),nrow(igrid),nlay(igrid)
        write(iout,*)
        call camxerr()
      endif 
c 
c-----Read 3rd & 4th AREA header 
c 
      read(iarem) (idum,idum,idum,idum,n=1,nseg) 
      read(iarem) ((arspec(n,l),n=1,10),l=1,narspc(igrid)) 
c 
c-----Map AREA species to model species 
c 
      do 15 lar = 1,narspc(igrid)
        write(arspc,'(10a1)') (arspec(n,lar),n=1,10) 
        if (arspc.eq.'HNO2      ') arspc = 'HONO      '
        if (arspc.eq.'HCHO      ' .and. kHCHO.eq.nspec+1)
     &                                        arspc = 'FORM      '
        do 20 l = 1,nspec 
          if (arspc.eq.spname(l)) then 
            larmap(l,igrid) = lar
            write(idiag,'(2(a,i5,2x,a))')
     &                   'Area source species ',lar,arspc, 
     &                   ' mapped to model species ',l,spname(l) 
            goto 15 
          endif 
 20     continue 
        write(idiag,*)'AREA species: ',arspc,' not modeled'
 15   continue
      write(idiag,*)
c
 9999 continue
c             
      return
      end

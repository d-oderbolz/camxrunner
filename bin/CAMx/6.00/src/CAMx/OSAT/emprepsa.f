*** EMPREPSA
c
      subroutine emprepsa(idate,btim,igrid)
      use filunit
      use grid
      use chmstry
      use ptemiss
      use tracer
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine reads the source group emissions files and moves the 
c   file pointer to the first hour to be used in this run.  Each point
c   source and surace emissions files respresenting a source group is
c   read.
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c        10/24/01  Removed BSWAP and converted integer strings to character*4
c        11/06/01  Input dates are now Julian
c        07/05/02  Changed to account for new type of the PiG flag
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
c
c-----------------------------------------------------------------------
c    Arguement declarations:
c-----------------------------------------------------------------------
c
      integer   idate
      real      btim
      integer   igrid
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
      integer MXOUT
      parameter (MXOUT = MAX(MXSPEC,MXTRSP))
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*200 fname
      character*10  namein
      character*4   ifname(10), ifnote(60)
      integer       ifile, nseg, nspect
      integer       ispc, ibgdat, iendat
      integer       idateb, idatee, nxcelt, nycelt, nzcelt
      integer       idum, izonet, iounit, isegm, npoint
      integer       i, j, ndate, seg4(4)
      real          tutmx, tutmy, tdxcel, tdycel
      real          tbegti, tendti, begtim, endtim, dum
      real          txorig, tyorig, ttime
c
      character*4 iname(10,MXOUT)
      real        txloc (MXPTSRC)
      real        tyloc (MXPTSRC)
      real        tdiam(MXPTSRC)
c
      common /comemprepsa/ txloc, tyloc, tdiam
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- set the date and time ---
c
      ndate = idate
      ttime = btim/100
      if( ttime .EQ. 24.0 ) then
         ndate = ndate + 1
         ttime = 0.
         if( MOD(ndate,1000) .GT. 365 ) then
            if( MOD(INT(ndate/1000),4) .EQ. 0 ) then
               if( MOD(ndate,1000) .EQ. 367 )
     &                        ndate = (INT(ndate/1000)+1)*1000 + 1
            else
               ndate = (INT(ndate/1000)+1)*1000 + 1
            endif
         endif
      endif
c
c   --- initialize the flag for PiG sources ---
c
      if( igrid .EQ. 1 ) then
         do i=1,nptsrc
            lpigsa(i) = .FALSE.
         enddo
      endif 
c
c   --- initialize postion zero, which is for the model emissions ----
c
      if( larsrc .AND. ltrace ) then
          ltemfl(igrid,0) = .TRUE.
      else
          ltemfl(igrid,0) = .FALSE.
      endif
      if( lptsrc .AND. ltrace ) then
          ltptfl(0) = .TRUE.
      else
          ltptfl(0) = .FALSE.
      endif
c
c   --- loop over the source grouping files ----
c
      do 10 ifile=0,ngroup
c
c   --- only read if surface emissions file is supplied ---
c
          if( ltemfl(igrid,ifile) .AND. larsrc ) then
             if( ifile .EQ. 0 ) then
                iounit = iarem(igrid)
                write(fname,'(A,I3)') 'EMISSIONS -- UNIT ',
     &                                                iarem(igrid)
             else
                iounit = iortem(igrid,ifile)
                fname = temfil(igrid,ifile)
             endif
c
c   --- rewind the file ----
c
             rewind(iounit,ERR=7005)
c
c   --- read the header of the emissions file ---
c
             read(iounit,ERR=7000) ifname, ifnote, nseg, nspect, 
     &                                 idateb, tbegti, idatee, tendti
             if( nspect .GT. MXTRSP ) then
               write(iout,'(//,A)') 'ERROR in EMPREPSA:'
               write(iout,*) 'A parameter in the camx.prm is not ',
     &                                        'sufficiently large.'
               write(iout,*) 
     &                    'Please change the value for parameter: MXTRSP'
               write(iout,*) 'It should be set to a value of at least: ',
     &                                                             nspect
               call flush(iout)
               call camxerr()
             endif
c
             read(iounit,ERR=7000) tutmx, tutmy, izonet, txorig, 
     &                 tyorig, tdxcel, tdycel, nxcelt, nycelt, nzcelt
             read(iounit,ERR=7000) seg4
             read(iounit,ERR=7000) 
     &                   ((iname(i,ispc),i=1,10),ispc=1,nspect)
c
c   --- set up index for species names ---
c
             nspcem(igrid,ifile) = nspect
             do 20 ispc=1,nspcem(igrid,ifile)
                write(namein,'(10A1)') (iname(i,ispc),i=1,10)
                if(namein .EQ.'HNO2      ') namein = 'HONO      '
                if(namein .EQ.'HCHO      ' .AND. kHCHO .eq. nspec+1 )
     &                                            namein = 'FORM      '
                do j=1,nspec
                  if(namein .EQ. spname(j) ) then
                      idxems(igrid,ifile,ispc) = j
                      goto 20
                   endif
                enddo
  20         continue
c
c   --- read the data until the correct position in the file is reached ---
c
  111        continue
             read(iounit,ERR=7002,END=7003) ibgdat, begtim, 
     &                                              iendat, endtim
             if(NINT(endtim) .EQ. 0) then
               endtim = 24.
               iendat = iendat - 1
             endif
             if( le1day ) then
                if( INT(begtim*100) .EQ. INT(ttime*100) ) goto 222
             else
                if( ibgdat .EQ. ndate .AND. 
     &                INT(begtim*100) .EQ. INT(ttime*100) ) goto 222
             endif
             do ispc=1,nspcem(igrid,ifile)
                 read(iounit,ERR=7002,END=7003) idum, 
     &              (dum,i=1,10),((dum,i=1,ncol(igrid)),j=1,nrow(igrid))
             enddo
             goto 111
  222        continue
             backspace(iounit)
c
          endif
c
c   --- point sources are only for the coarse grid -- grid #1 ---
c
          if( igrid .GT. 1 ) goto 10
c
c   --- only read if elevated point source emissions file is supplied ---
c
          if( ltptfl(ifile) .AND. lptsrc ) then
             if( ifile .EQ. 0 ) then
                iounit = iptem
                write(fname,'(A,I3)') 'PTSOURCE -- UNIT ',iptem
             else
                iounit = iortpt(ifile)
                fname = tptfil(ifile)
             endif
c
c   --- rewind the file ----
c
             rewind(iounit,ERR=7005)
c
c   --- read the header of the emissions file ---
c
             read(iounit,ERR=7000) ifname, ifnote, nseg, nspect, 
     &                                 idateb, tbegti, idatee, tendti
             if( nspect .GT. MXTRSP ) then
               write(iout,'(//,A)') 'ERROR in EMPREPSA:'
               write(iout,*) 'A parameter in the camx.prm is not ',
     &                                        'sufficiently large.'
               write(iout,*)
     &                    'Please change the value for parameter: MXTRSP'
               write(iout,*) 'It should be set to a value of at least: ',
     &                                                             nspect
               call flush(iout)
               call camxerr()
             endif
c
             read(iounit,ERR=7000) tutmx, tutmy, izonet, txorig, 
     &                 tyorig, tdxcel, tdycel, nxcelt, nycelt, nzcelt
             read(iounit,ERR=7000) seg4
             read(iounit,ERR=7000) 
     &                   ((iname(i,ispc),i=1,10),ispc=1,nspect)
c
c   --- set up index for species names ---
c
             nspcpt(ifile) = nspect
             do 30 ispc=1,nspcpt(ifile)
                write(namein,'(10A1)') (iname(i,ispc),i=1,10)
                if(namein .EQ.'HNO2      ') namein = 'HONO      '
                if(namein .EQ.'HCHO      ' .AND. kHCHO .eq. nspec+1 )
     &                                            namein = 'FORM      '
                do j=1,nspec
                  if(namein .EQ. spname(j) ) then
                      idxpts(ifile,ispc) = j
                      goto 30
                   endif
                 enddo
  30         continue
             read(iounit,ERR=7000) isegm, npoint
             if( npoint .GT. MXPTSRC ) then
              write(iout,'(//,A)') 'ERROR in RDPTDDM:'
              write(iout,*) 'A parameter in the camx.prm is not ',
     &                                        'sufficiently large.'
              write(iout,*) 
     &               'Please change the value for parameter: MXPTSRC'
              write(iout,*) 'It should be set to a value of at least: ',
     &                                                            npoint
              call flush(iout)
              call camxerr()
             endif
c
             read(iounit,ERR=7002) (txloc(i),tyloc(i),idum,tdiam(i),
     &                                           dum,dum,i=1,npoint)
             do i=1,npoint
                if(ifile .EQ. 0 .OR. ((lddm .OR. lhddm) 
     &                                    .and. ifile .eq. 1) ) then
                    xlocpt(i) = txloc(i)
                    ylocpt(i) = tyloc(i)
                endif
                if( txloc(i) .NE. xlocpt(i) .OR. 
     &                             tyloc(i) .NE. ylocpt(i) ) goto 7004
                if( ipigflg .EQ. GRESPIG .AND. tdiam(i) .LT. 0. ) 
     &             lpigsa(i) = .TRUE.
             enddo
c
c   --- read the data until the correct position in the file is reached ---
c
  333        continue
             read(iounit,ERR=7002,END=7003) ibgdat, begtim, 
     &                                              iendat, endtim
             if(NINT(endtim) .EQ. 0) then
               endtim = 24.
               iendat = iendat - 1
             endif
             if( le1day ) then
                if( INT(begtim*100) .EQ. INT(ttime*100) ) goto 444
             else
                if( ibgdat .EQ. ndate .AND. 
     &                INT(begtim*100) .EQ. INT(ttime*100) ) goto 444
             endif
             read(iounit,ERR=7002) isegm, npoint
             read(iounit,ERR=7002) (idum,idum,idum,dum,dum,i=1,npoint)
             do ispc=1,nspcpt(ifile)
                 read(iounit,ERR=7002,END=7003) idum,(dum,i=1,10),
     &                                                (dum,i=1,npoint)
             enddo
             goto 333
  444        continue
             backspace(iounit)
c
         endif
   10 continue
c
c  ---- return to calling routine ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,A)') 'ERROR in EMPREPSA:'
      write(iout,'(1X,3A)') 'Reading header of ',
     &                     'emissions file: ',fname(:istrln(fname))
      call camxerr()
c
 7002 continue
      write(iout,'(//,A)') 'ERROR in EMPREPSA:'
      write(iout,'(1X,2A)') 'Reading emissions grid in file: ',
     &                                        fname(:istrln(fname))
      call camxerr()
c
 7003 continue
      write(iout,'(//,A)') 'ERROR in EMPREPSA:'
      write(iout,'(1X,3A)') 'Premature end-of-file reading ',
     &                     'emissions file: ',fname(:istrln(fname))
      write(iout,'(1X,A)') 'Make sure date/time matches the simulation '
      write(iout,'(1X,A)') 'or turn on single emissions flag.'
      call camxerr()
c
 7004 continue
      write(iout,'(//,A)') 'ERROR in EMPREPSA:'
      write(iout,'(1X,A,I10,2A)') 'Location for point: ',i,
     &     ' is not consistent with regular emissions in file: ',
     &                                        fname(:istrln(fname))
      call camxerr()
c
 7005 continue
      if( ifile .EQ. 0 ) then
         if( iarem(igrid) .GT. 0 ) then
            write(iout,'(//,A)') 'ERROR in EMPREPSA:'
            write(iout,'(1X,2A)') 'Cannot access emissions file',
     &                             ' provided for OSAT processing. '
            write(iout,'(1X,2A)') 'If it is the same as the file ',
     &            'used for the regular model, you may have to make a '
            write(iout,'(1X,A)')'copy and specify the name of the copy.'
         else
            write(iout,'(//,A)') 'ERROR in EMPREPSA:'
            write(iout,'(1X,2A)') 'Flexi-nesting the emissions inputs',
     &              ' is not supported with a '
            write(iout,'(1X,2A)') 'Probing Tools application.'
            write(iout,'(1X,2A)') 'You must supply an emissions file ',
     &                                                 'for each grid.'
         endif
      else
         write(iout,'(//,A)') 'ERROR in EMPREPSA:'
         write(iout,'(1X,3A)') 'Cannot access emissions file',
     &     ' provided for OSAT processing: ',fname(:istrln(fname))
      endif
      call camxerr()
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

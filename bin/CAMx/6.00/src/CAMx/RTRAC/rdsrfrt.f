c**** RDSRFRT.F
c
      subroutine rdsrfrt(igrd,nox,noy,nspsa,solmas,vegmas)
      use filunit
      use grid
      use chmstry
      use bndary
      use camxcom
      use rtracchm
      use tracer
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine fils one hour of surface mass for the RTRAC
c   species. It then places these mass fields in the appropriate 
c   place in the gridded array used for RTRAC soil model.  
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c      Argument description:
c       Outputs:
c           solmas   R  surface soil mass (mol/ha, g/ha)
c           vegmas   R  surface veg mass (mol/ha, g/ha)
c       Inputs:
c           igrd     I  grid index
c           nox      I  number of X cells in the grid
c           noy      I  number of Y cells in the grid
c           nspsa    I  number of tracer species
c       
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     10/29/09   --cemery--    Original development
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
      integer   igrd
      integer   nox
      integer   noy
      integer   nspsa
      real      solmas(nox,noy,nspsa)
      real      vegmas(nox,noy,nspsa)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*10 aqfil, cfile, cspec
      character*4  ifile(10), note(60), ispec(10)
      integer      ispc, i, j, n
      integer      iseg, nspc, idat1, idat2
      integer      izone, nx, ny, nz, idum, imod
      real         tim1, tim2, orgx, orgy, utmx, utmy, dx, dy
      logical      lexist, luse
c
      real srfin(MXCELLS,MXCELLS)
c
c-----------------------------------------------------------------------
c    Data statements:
c-----------------------------------------------------------------------
c
      data aqfil /'AVERAGE'/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      iunit = iorrtsrf(igrd)
c
c  --- initailize to lower bound ---
c
      do ispc=1,nspsa
         do j=1,noy
            do i=1,nox
               solmas(i,j,ispc) = 0.
               vegmas(i,j,ispc) = 0.
            enddo
         enddo
      enddo
c
c  --- open the file for this grid ---
c
      inquire(file=rtsrfin(igrd),exist=lexist)
      if( .NOT. lexist ) goto 7000
      open(file=rtsrfin(igrd),unit=iunit,status='UNKNOWN',
     &                                   form='UNFORMATTED',ERR=7001)
c
c  --- read 1st header record and check inputs ---
c
      read(iunit,ERR=7002) ifile,note,iseg,nspc,idat1,tim1,idat2,tim2
      if( INT(tim2) .EQ. 24 ) then
        idat2 = idat2 + 1
        tim2 = 0.
      endif
c     write(cfile,'(10A1)') (ifile(i),i=1,10)
c     if( cfile .NE. aqfil ) goto 7003
      tim1 = 100.*tim1
      tim2 = 100.*tim2
      if( idat1 .GT. begdate .AND. .NOT. le1day ) goto 7004
      if( idat1 .EQ. begdate .AND. tim1 .GT. begtim
     &                              .AND. .NOT. le1day ) goto 7004
c
c  --- read 2nd header record and check inputs ---
c
      read(iunit,ERR=7002) orgx,orgy,izone,utmx,utmy,dx,dy,nx,ny,nz
      if( nx .NE. ncol(igrd) .OR. ny .NE. nrow(igrd) 
     &                                .OR. nz .NE. 1 ) goto 7005
c
c  --- skip the next 2 records ---
c
      read(iunit,ERR=7002)
      read(iunit,ERR=7002) 
c
c  --- read the date and time and make sure it is the correct hour ---
c
  111 continue
      read(iunit,ERR=7007,END=7006) idat1, tim1, idat2, tim2
      tim1 = 100*tim1
      tim2 = 100*tim2
      if( INT(tim2) .EQ. 24 ) then
        idat2 = idat2 + 1
        tim2 = 0.
        if( MOD(idat2,1000) .GT. 365 ) then
           if( MOD(INT(idat2/1000),4) .EQ. 0 ) then
              if( MOD(idat2,1000) .EQ. 367 )
     &                       idat2 = (INT(idat2/1000)+1)*1000 + 1
           else
              idat2 = (INT(idat2/1000)+1)*1000 + 1
           endif
        endif
      endif
      if( (idat1 .LT. begdate .OR. 
     &       (idat1 .EQ. begdate .AND. tim1 .LE. begtim)) .AND.
     &            (idat2 .GT. begdate .OR. 
     &                (idat2 .EQ. begdate .AND. tim2 .GT. begtim))) then
c
c  --- read the concentrations for each species ---
c
         do ispc=1,nspc
c
           read(iunit) idum,(ispec(n),n=1,10),
     &                                 ((srfin(i,j),i=1,nx),j=1,ny)
           write(cspec,'(10A1)') ispec
c
c  --- find this species in the RTRAC species list ---
c
           luse = .FALSE.
           do imod=1,ntotsp
              if( cspec(2:) .EQ. ptname(imod) ) then
                  luse = .TRUE.
                  do 20 j=2,ny-1
                     do i=2,nx-1
                        if( srfin(i,j) .GE. 0. ) then
                           if (cspec(1:1) .EQ. 'S') then
                              solmas(i,j,imod) = srfin(i,j)
                           elseif (cspec(1:1) .EQ. 'V') then
                              vegmas(i,j,imod) = srfin(i,j)
                           endif
                        endif
                     enddo
   20             continue
              endif
c
c  --- check the next modeled species ---
c
           enddo
c
c  --- if not found in model species list, write a message ----
c
           if( .NOT. luse ) then
               write(iout,'(1X,4A)') 'Species in RTRAC ',
     &              'surface mass file: ',cspec(:istrln(cspec)),
     &              ' not found in RTRAC species list ... Stopping.'
               call camxerr()
           endif
c
c  --- read next species worth of data ---
c
         enddo
c
c  --- if not the right hour, read through it ---
c
      else
         do ispc=1,nspc
            read(iunit,ERR=7007) 
         enddo
         goto 111
      endif
c
c  --- close the file and return to the calling routine ---
c
      close(iunit)
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue 
      write(iout,'(//,A)') 'ERROR in RDSRFRT:'
      write(iout,*) 
     &   'ERROR:  RTRAC surface mass file does not exist: ',
     &                             rtsrfin(igrd)(:istrln(rtsrfin(igrd)))
      call camxerr()
c
 7001 continue 
      write(iout,'(//,A)') 'ERROR in RDSRFRT:'
      write(iout,*) 'ERROR:  Opening RTRAC surface mass file: ',
     &                             rtsrfin(igrd)(:istrln(rtsrfin(igrd)))
      call camxerr()
c
 7002 continue
      write(iout,'(//,A)') 'ERROR in RDSRFRT:'
      write(iout,*)'ERROR: Reading header of RTRAC surface mass file: ',
     &                             rtsrfin(igrd)(:istrln(rtsrfin(igrd)))
      call camxerr()
c
 7003 continue
      write(iout,'(//,A)') 'ERROR in RDSRFRT:'
      write(iout,*) 'ERROR:  RTRAC surface mass file is not labelled ',
     &                                                 'AIRQUALITY.'
      call camxerr()
c
 7004 continue
      write(iout,'(//,A)') 'ERROR in RDSRFRT:'
      write(iout,*) 'ERROR:  RTRAC surface mass input file is not for ',
     &                                          'correct time period.'
      write(iout,*) '   ***  Episode ***'
      write(iout,'(a,i10.5)') 'Date   : ',begdate
      write(iout,'(a,f10.1)') 'Time   : ',begtim
      write(iout,*) '   ***  RTRAC File ***'
      write(iout,'(a,i10.5)') 'Date   : ',idat1
      write(iout,'(a,f10.1)') 'Time   : ',tim1
      call camxerr()
c
 7005 continue
      write(iout,'(//,A)') 'ERROR in RDSRFRT:'
      write(iout,*) 'ERROR: RTRTAC surface mass input file does not',
     &              ' appear to be for the correct grid.' 
      write(iout,*) '   ***  Grid ***'
      write(iout,*) 'No. of Cells : (',ncol(igrd),',',nrow(igrd),')'
      write(iout,*) 'No. of layers:  ',1
      write(iout,*) '   ***  RTRAC File ***'
      write(iout,*) 'No. of Cells : (',nx,',',ny,')'
      write(iout,*) 'No. of layers:  ',nz
      call camxerr()
c
 7006 continue
      write(iout,'(//,A)') 'ERROR in RDSRFRT:'
      write(iout,*) 'ERROR:  Premature end-of-file reached in ',
     &                                  'RTRAC surface mass input file.'
      write(iout,*) 'Make sure the file contains the correct date/time.'
      call camxerr()
c
 7007 continue
      write(iout,'(//,A)') 'ERROR in RDSRFRT:'
      write(iout,*) 'ERROR:  Reading RTRAC surface mass input file: ',
     &                             rtsrfin(igrd)(:istrln(rtsrfin(igrd)))
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
c
      return
      end

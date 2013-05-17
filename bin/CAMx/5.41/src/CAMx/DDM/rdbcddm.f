c**** RDBCDDM.F
c
      subroutine rdbcddm(nox,noy,noz,nspsa,saconc,tpgrd,prgrd)
      use filunit
      use grid
      use chmstry
      use bndary
      use camxcom
      use tracer
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine fills one hour of boundary conditions for the DDM
c   species. It then places these concentrations in the  appropriate 
c   place in the gridded array used for tracer concentrations.  
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c      Argument description:
c       Outputs:
c           saconc   R  tracer concentrations
c       Inputs:
c           nox      I  number of X cells in the grid
c           noy      I  number of Y cells in the grid
c           noz      I  number of layers in the grid
c           nspsa    I  number of tracer species
c           tpgrd    I  3-D temperature field
c           prgrd    I  3-D pressure field
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     03/31/99   --gwilson--    Original development
c     10/24/01  Removed BSWAP and converted integer strings to character*4
c     07/16/07   --bkoo--       Added HRVOC
c     11/4/09   -cemery-        Removed input top concentrations
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
      integer   nox
      integer   noy
      integer   noz
      integer   nspsa
      real      saconc(nox,noy,noz,nspsa)
      real      tpgrd(nox,noy,noz)
      real      prgrd(nox,noy,noz)
c
c-----------------------------------------------------------------------
c    Local variables:
c----------------------------------------------------------------------
c
      character*10 bnfil, cfile, cspec
      character*4  ifile(10), note(60), ispec(10)
      integer      ispc, iedge
      integer      iseg, nspc, idat1, idat2
      integer      izone, nx, ny, nz, imod
      integer      iddm, iptr, i, j, k, nedge
      real         tim1, tim2, orgx, orgy, utmx, utmy, dx, dy
      logical      lexist, luse
c
      real conwst(MXCELLS,MXCELLS)
      real conest(MXCELLS,MXCELLS)
      real consth(MXCELLS,MXCELLS)
      real connth(MXCELLS,MXCELLS)
c
c-----------------------------------------------------------------------
c    Data statements:
c-----------------------------------------------------------------------
c
      data bnfil /'BOUNDARY'/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- set the number of BC edges --
c
      if( lbndry ) then
        nedge = 5
      else
        nedge = 1
      endif
c
c  --- open the BC ---
c
      inquire(file=bcfil,exist=lexist)
      if( .NOT. lexist ) goto 7000
      open(file=bcfil,unit=iorbc,status='UNKNOWN',
     &                                   form='UNFORMATTED',ERR=7001)
c
c  --- read 1st BC header record and check inputs ---
c
      read(iorbc,ERR=7002) ifile,note,iseg,nspc,idat1,tim1,idat2,tim2
      if( INT(tim2) .EQ. 24 ) then
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
      write(cfile,'(10A1)') (ifile(i),i=1,10)
      if( cfile .NE. bnfil ) goto 7003
      tim1 = 100.*tim1
      tim2 = 100.*tim2
      if( idat1 .GT. date .AND. .NOT. le1day ) goto 7004
      if( idat1 .EQ. date .AND. tim1 .GT. time
     &                              .AND. .NOT. le1day ) goto 7004
c
c  --- read 2nd BC header record and check inputs ---
c
      read(iorbc,ERR=7002) orgx,orgy,izone,utmx,utmy,dx,dy,nx,ny,nz
      if( .NOT. llatlon ) then
        dx = dx/1000.
        dy = dy/1000.
      endif
      if( nx .NE. ncol(1) .OR. ny .NE. nrow(1) 
     &                                .OR. nz .NE. nlay(1) ) goto 7005
c
c  --- skip the next 6 records ---
c
      read(iorbc,ERR=7002)
      read(iorbc,ERR=7002) 
      do i=1,4
        read(iorbc,ERR=7002) 
      enddo
c
c  --- read the date and time and make sure it is the correct hour ---
c
  111 continue
      read(iorbc,ERR=7007,END=7006) idat1, tim1, idat2, tim2
      tim1 = tim1*100
      tim2 = tim2*100
      if( INT(tim2) .EQ. 24 ) then
        tim2 = 0.
        idat2 = idat2 + 1
        if( MOD(idat2,1000) .GT. 365 ) then
           if( MOD(INT(idat2/1000),4) .EQ. 0 ) then
              if( MOD(idat2,1000) .EQ. 367 )
     &                     idat2 = (INT(idat2/1000)+1)*1000 + 1
           else
              idat2 = (INT(idat2/1000)+1)*1000 + 1
           endif
        endif
      endif
      if( (idat1 .LT. date .OR. 
     &       (idat1 .EQ. date .AND. tim1 .LE. time)) .AND.
     &            (idat2 .GT. date .OR. 
     &                (idat2 .EQ. date .AND. tim2 .GT. time))) then
c
c  --- read the concentrations for each species ---
c
         do ispc=1,nspc
            read(iorbc,ERR=7007,END=7006) iseg, (ispec(i),i=1,10),iedge,
     &                                     ((conwst(k,j),k=1,nz),j=1,ny)
            read(iorbc,ERR=7007,END=7006) iseg, (ispec(i),i=1,10),iedge,
     &                                     ((conest(k,j),k=1,nz),j=1,ny)
            read(iorbc,ERR=7007,END=7006) iseg, (ispec(i),i=1,10),iedge,
     &                                     ((consth(k,i),k=1,nz),i=1,nx)
            read(iorbc,ERR=7007,END=7006) iseg, (ispec(i),i=1,10),iedge,
     &                                     ((connth(k,i),k=1,nz),i=1,nx)
            write(cspec,'(10A1)') ispec
            if( cspec .EQ. 'HNO2      ') cspec = 'HONO      '
            if( cspec .EQ. 'HCHO      ' .and. kHCHO.eq.nspec+1 ) 
     &                                             cspec = 'FORM      '
c
c  --- find this species in the modeled species list ---
c
            do imod=1,nspec
               if( cspec .EQ. spname(imod) ) then
                   do iddm=1,nbcddm
                      luse = .FALSE.
                      if( bcddmsp(iddm) .EQ. cspec ) luse = .TRUE. 
                      if( bcddmsp(iddm) .EQ. NAMALL  ) luse = .TRUE. 
                      if( bcddmsp(iddm) .EQ. NAMVOC 
     &                               .AND. lvocsp(imod) ) luse = .TRUE. 
                      if( bcddmsp(iddm) .EQ. NAMNOX 
     &                               .AND. lnoxsp(imod) ) luse = .TRUE. 
                      if( bcddmsp(iddm) .EQ. NAMHRV
     &                               .AND. lhrvoc(imod) ) luse = .TRUE.
c
c  --- if this DDM species matches this modeled species, load it ---
c
                      if( luse ) then
                         do k=1,nz
                            do j=1,ny
c
c  --- get the gas conversion factor to umol/m3 ---
c
                                  if( imod .LE. ngas ) then
                                   convfac = densfac*273./
     &                               tpgrd(1,j,k)*prgrd(1,j,k)/1013.
                                  else
                                   convfac = 1.
                                  endif
c
c  --- load the west boundary ---
c
                                  if( lbndry ) then
                                     ioff = IDXBWS
                                  else
                                     ioff = 1
                                  endif
                                  iptr = iptddm(imod) + (iddm-1)*nedge +
     &                                                 nicddm + ioff - 1
                                  if( conwst(k,j) .GT. bdnl(imod) )
     &                               saconc(1,j,k,iptr) = 
     &                                           conwst(k,j) * convfac
c
c  --- load the east boundary ---
c
c  --- get the gas conversion factor to umol/m3 ---
c
                                  if( imod .LE. ngas ) then
                                   convfac = densfac*273./
     &                               tpgrd(nx,j,k)*prgrd(nx,j,k)/1013.
                                  else
                                   convfac = 1.
                                  endif
                                 if( lbndry ) then
                                     ioff = IDXBES
                                  else
                                     ioff = 1
                                  endif
                                  iptr = iptddm(imod) + (iddm-1)*nedge +
     &                                                 nicddm + ioff - 1
                                  if( conest(k,j) .GT. bdnl(imod) )
     &                               saconc(nx,j,k,iptr) = 
     &                                           conest(k,j) * convfac
                            enddo
c
c  --- load the south boundary ---
c
                            do i=1,nx
c
c  --- get the gas conversion factor to umol/m3 ---
c
                                 if( imod .LE. ngas ) then
                                   convfac = densfac*273./
     &                               tpgrd(i,1,k)*prgrd(i,1,k)/1013.
                                 else
                                   convfac = 1.
                                 endif
                                 if( lbndry ) then
                                     ioff = IDXBST
                                  else
                                     ioff = 1
                                  endif
                                  iptr = iptddm(imod) + (iddm-1)*nedge +
     &                                                 nicddm + ioff - 1
                                  if( consth(k,i) .GT. bdnl(imod) )
     &                               saconc(i,1,k,iptr) = 
     &                                           consth(k,i) * convfac
c
c  --- load the east boundary ---
c
c
c  --- get the gas conversion factor to umol/m3 ---
c
                                 if( imod .LE. ngas ) then
                                   convfac = densfac*273./
     &                               tpgrd(i,ny,k)*prgrd(i,ny,k)/1013.
                                 else
                                   convfac = 1.
                                 endif
                                 if( lbndry ) then
                                     ioff = IDXBNT
                                  else
                                     ioff = 1
                                  endif
                                  iptr = iptddm(imod) + (iddm-1)*nedge +
     &                                                 nicddm + ioff - 1
                                  if( connth(k,i) .GT. bdnl(imod) )
     &                               saconc(i,ny,k,iptr) = 
     &                                           connth(k,i) * convfac
                            enddo
                         enddo
                      endif
c
c  --- check the next BC DDM species ---
c
                   enddo
c
c  --- check the next modeled species ---
c
               endif
            enddo
c
c  --- read next species worth of data ---
c
         enddo
c
c  --- if not the right hour, read through it ---
c
      else
         do ispc=1,nspc
            do i=1,4
               read(iorbc,ERR=7007) 
            enddo
         enddo
         goto 111
      endif
c
c  --- finally got the right hour, close the file ---
c
      close(iorbc)
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue 
      write(iout,'(//,A)') 'ERROR in RDBCDDM:' 
      write(iout,*) 'ERROR:  BC file for DDM does not exist: ',
     &                                             bcfil(:istrln(bcfil))
      call camxerr()
c
 7001 continue 
      write(iout,'(//,A)') 'ERROR in RDBCDDM:' 
      write(iout,*) 'ERROR:  Opening BC file for DDM: ',
     &                                             bcfil(:istrln(bcfil))
      call camxerr()
c
 7002 continue
      write(iout,'(//,A)') 'ERROR in RDBCDDM:' 
      write(iout,*) 'ERROR:  Reading header of BC file for DDM: ',
     &                                             bcfil(:istrln(bcfil))
      call camxerr()
c
 7003 continue
      write(iout,'(//,A)') 'ERROR in RDBCDDM:' 
      write(iout,*) 'ERROR:  BC input file for DDM is not labelled ',
     &                                                 'BOUNDARY.'
      call camxerr()
c
 7004 continue
      write(iout,'(//,A)') 'ERROR in RDBCDDM:' 
      write(iout,*) 'ERROR:  BC input file for DDM is not for correct ',
     &                                                   'time period.'
      write(iout,*) '   ***  Episode ***'
      write(iout,'(a,i10.5)') 'Date   : ',date
      write(iout,'(a,f10.1)') 'Time   : ',time
      write(iout,*) '   ***  BC File ***'
      write(iout,'(a,i10.5)') 'Date   : ',idat1
      write(iout,'(a,f10.1)') 'Time   : ',tim1
      call camxerr()
c
 7005 continue
      write(iout,'(//,A)') 'ERROR in RDBCDDM:' 
      write(iout,*) 'ERROR:  BC input file does not appear to be for ',
     &                ' the correct grid.' 
      write(iout,*) '   ***  Coarse Grid ***'
      write(iout,*) 'No. of Cells : (',ncol(1),',',nrow(1),')'
      write(iout,*) 'No. of layers:  ',nlay(1)
      write(iout,*) '   ***  BC File ***'
      write(iout,*) 'No. of Cells : (',nx,',',ny,')'
      write(iout,*) 'No. of layers:  ',nz
      call camxerr()
c
 7006 continue
      write(iout,'(//,A)') 'ERROR in RDBCDDM:' 
      write(iout,*) 'ERROR:  Premature end-of-file reached in BC file ',
     &                                                      'for DDM.'
      write(iout,*) 'Make sure the file contains the correct date/time.'
      call camxerr()
c
 7007 continue
      write(iout,'(//,A)') 'ERROR in RDBCDDM:' 
      write(iout,*) 'ERROR:  Reading BC file for DDM: ',
     &                                             bcfil(:istrln(bcfil))
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

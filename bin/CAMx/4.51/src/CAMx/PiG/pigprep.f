      subroutine pigprep(begdat,begtim)
c
c----CAMx v4.51 080522
c
c     PIGPREP prepares the PiG submodel
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c          
c     Modifications:
c        6/27/00 - Added some diagnostics to ensure that the point source PiG
c                  list is consistent on a restart with that used in previous 
c                  run.
c                - Now writing PiG point source information to PiG restart
c                  file, and echoing this information to diagnostic file on 
c                  model startup.
c       11/06/01   Input dates are now Julian
c        7/05/02   Added code to accommodate IRON-PiG
c       06/24/03   Removed thetapig
c       02/16/05   Added code to accomodate RTRAC mass
c       08/24/05   Removed diagnostics that ensure consistent PiG lists on
c                  restarts; removed read/write of PiG header containing 
c                  point source parameters; read of GREASD restart file now 
c                  includes OSAT pointer to region and group
c        2/02/06   Removed GREASD-PiG specific file formats; both PiG options
c                  use the IRON file formats
c
c     Input arguments:
c        begdat              model beginning date (YYJJJ)
c        begtim              model beginning time
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
      implicit none
      include "camx.prm"
      include "flags.com"
      include "pigsty.com"
      include "filunit.com"
      include "ptemiss.com"
      include "chmstry.com"
      include "camxfld.com"
c
c======================== Source Apportion Begin =======================
c
      include "tracer.com"
      include "rtracchm.com"
c
c========================= Source Apportion End ========================
c
      integer begdat
      real begtim
      integer idatpig,nsrc,irec,n,i,nr,is,m
      real timpig,xx(MXPTSRC),yy(MXPTSRC),hh(MXPTSRC),dd(MXPTSRC),
     &     tt(MXPTSRC),vv(MXPTSRC)
      common /pigsrc/ xx,yy,hh,dd,tt,vv
c
c-----Entry point
c
      nkill(1) = 0
      nkill(2) = 0
      nkill(3) = 0
      nkill(4) = 0
      nkill(5) = 0
      nkill(6) = 0
      nkill(7) = 0
      nkill(8) = 0
      nkill(9) = 0
      npig = 0
      nreactr = 1
      loverlap = .false.
      if (ipigflg .EQ. IRONPIG) then
        nreactr = MXRECTR
        loverlap = OVERLAP 
      endif
      do n = 1,MXGRID
        nage(n) = 0
        pigage(n) = 0.
        do is = 1,MXSPEC
          pgmserr(is,n) = 0.
        enddo
      enddo
c
c-----Check FLEAK param
c
      if ((OVERLAP .or. LEAKON) .and. FLEAK .GT. 1.) then
        write(iout,'(//,a)') 'ERROR in PIGPREP:'
        write(iout,*) 'FLEAK must be <= 1.0'
        write(iout,*) 'FLEAK = ',FLEAK
        call camxerr()
      endif
c 
c-----Read PiG information if it is a restart run and if PiG file is
c     provided 
c
      if (lrstrt .AND. irstp.ne.0) then
c
c-----Read hourly data to current date/time
c
        irec = 0
 100    continue
        irec = irec + 1
        read(irstp,ERR=7000,END=7001) idatpig,timpig,npig,nreactr
        irec = irec + 1
        if (ltrace .AND. tectyp .EQ. RTRAC) then
          read(irstp,ERR=7002,END=7001)
     &           (ingrd(n),idpig(n),xpigf(n),xpigb(n),ypigf(n),
     &           ypigb(n),zpig(n),axisy(n),axisz(n),sigy(n),
     &           sigz(n),pufftop(n),puffbot(n),htfms(n),htfmb(n),
     &           vtfms(n),vtfmb(n),agepigf(n),agepigb(n),fmspig(n),
     &           ((puffrad(i,nr,n),i=1,nrad),nr=1,nreactr),
     &           ((puffmass(i,nr,n),i=1,nspec),nr=1,nreactr),n=1,npig),
     &           (((puffrt(i,nr,n),i=1,nrtrac),nr=1,nreactr),n=1,npig)
        else
          read(irstp,ERR=7002,END=7001)
     &           (ingrd(n),idpig(n),xpigf(n),xpigb(n),ypigf(n),
     &           ypigb(n),zpig(n),axisy(n),axisz(n),sigy(n),
     &           sigz(n),pufftop(n),puffbot(n),htfms(n),htfmb(n),
     &           vtfms(n),vtfmb(n),agepigf(n),agepigb(n),fmspig(n),
     &           ipufmap(n),ipufgrp(n),
     &           ((puffrad(i,nr,n),i=1,nrad),nr=1,nreactr),
     &           ((puffmass(i,nr,n),i=1,nspec),nr=1,nreactr),n=1,npig)
        endif
        if (timpig.ge.2400.) then
          timpig = timpig - 2400.
          idatpig = idatpig + 1
          if (MOD(idatpig,1000) .GT. 365) then
            if (MOD(INT(idatpig/1000),4) .EQ. 0) then
              if (MOD(idatpig,1000) .EQ. 367)
     &          idatpig = (INT(idatpig/1000)+1)*1000 + 1
            else
              idatpig = (INT(idatpig/1000)+1)*1000 + 1
            endif
          endif
        endif
        write(iout,'(a,F10.1,i10.5,i10)') 'Read PiG file at  ',
     &                                timpig,idatpig,npig
        if (idatpig.lt.begdat .or. 
     &     (idatpig.eq.begdat .and. timpig.lt.begtim - 0.01)) goto 100
        if (idatpig.gt.begdat .or. 
     &     (idatpig.eq.begdat .and. timpig.gt.begtim + 0.01)) then
           write(iout,'(//,a)') 'ERROR in PIGPREP:'
           write(iout,*) 'Date or time in PiG file > beginning time'
           write(iout,'(2i10.5)') idatpig, timpig
           call camxerr()
        endif
      endif
c
c-----Echo PiG point source information to diagnostic file
c
      write(idiag,'(//,a)') 'PiG source information'
      write(idiag,'(80a)') ('-',m=1,80)
      write(idiag,'(a,a)') ' Pig Src   Pt Src     Xloc      Yloc   ',
     &                     ' Height   Diameter  Temperature  Velocity'
      write(idiag,'(a,a)') '    #         #        (km or deg)  ',
     &                '     (m)       (m)         (K)       (m/hr)'
      write(idiag,'(80a)') ('-',m=1,80)
      rewind(iptem)
      do n = 1,5
        read(iptem)
      enddo
      read(iptem) (xx(n),yy(n),hh(n),dd(n),tt(n),vv(n),n=1,nptsrc)
      nsrc = 0
      do n = 1,nptsrc
        if (dd(n).lt.0.) then
          nsrc = nsrc + 1
          write(idiag,'(i5,2x,i10,2f10.3,f9.1,f9.2,f12.1,f13.0)')
     &      nsrc,n,xx(n)/1000.,yy(n)/1000.,hh(n),dd(n),tt(n),vv(n)
        endif
      enddo
c
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in PIGPREP:'
      write(iout,*) 'Cannot read PiG restart file at record: ',irec
      call camxerr()
c
 7001 continue
      write(iout,'(//,a)') 'ERROR in PIGPREP:'
      write(iout,*) 'Premature end-of-file found in PiG ',
     &              'restart file at record: ',irec
      call camxerr()
c
 7002 continue
      write(iout,'(//,a)') 'ERROR in PIGPREP:'
      write(iout,'(a,i10.5,f10.1)') 
     &      'Cannot read data in PiG restart file at hour: ',
     &      idatpig,timpig
      call camxerr()
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

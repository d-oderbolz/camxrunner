      subroutine wrtpig(idatpig,timpig)
c
c----CAMx v4.51 080522
c
c     WRTPIG writes pig parameters and state variables for restart 
c                          
c     Copyright 1996-2008
c     ENVIRON International Corporation
c          
c     Modifications:
c        07/05/02   Added code to accomodate IRON-PiG
c        02/16/05   Added code to accomodate RTRAC mass
c        08/25/05   Added write of GREASD-OSAT region/group pointers
c        02/02/06   Removed GREASD-PiG specific file formats; both PiG options
c                   use the IRON file formats
c
c     Input arguments:
c        idatpig             model date (YYJJJ)
c        timpig              model time
c
c     Output arguments:
c        none
c
c     Subroutines called:
c        none
c
c     Called by:
c        CAMx
c
      implicit none
      include "camx.prm"
      include "filunit.com"
      include "pigsty.com"
      include "chmstry.com"
c
c======================== Source Apportion Begin =======================
c
      include "tracer.com"
      include "rtracchm.com"
c
c========================= Source Apportion End ========================
c
      integer idatpig,n,i,nr
      real timpig
c
c-----Entry point
c
      write(ipig) idatpig,timpig,npig,nreactr
c
      if (ltrace .AND. tectyp .EQ. RTRAC) then
        write(ipig) (ingrd(n),idpig(n),xpigf(n),xpigb(n),ypigf(n),
     &            ypigb(n),zpig(n),axisy(n),axisz(n),sigy(n),
     &            sigz(n),pufftop(n),puffbot(n),htfms(n),htfmb(n),
     &            vtfms(n),vtfmb(n),agepigf(n),agepigb(n),fmspig(n),
     &            ((puffrad(i,nr,n),i=1,nrad),nr=1,nreactr),
     &            ((puffmass(i,nr,n),i=1,nspec),nr=1,nreactr),n=1,npig),
     &            (((puffrt(i,nr,n),i=1,nrtrac),nr=1,nreactr),n=1,npig)
      else
          write(ipig) (ingrd(n),idpig(n),xpigf(n),xpigb(n),ypigf(n),
     &             ypigb(n),zpig(n),axisy(n),axisz(n),sigy(n),
     &             sigz(n),pufftop(n),puffbot(n),htfms(n),htfmb(n),
     &             vtfms(n),vtfmb(n),agepigf(n),agepigb(n),fmspig(n),
     &             ipufmap(n),ipufgrp(n),
     &             ((puffrad(i,nr,n),i=1,nrad),nr=1,nreactr),
     &             ((puffmass(i,nr,n),i=1,nspec),nr=1,nreactr),n=1,npig)
      endif
c
      return
      end

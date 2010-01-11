      subroutine pigevol(igrd)
c
c----CAMx v4.51 080522
c
c     PIGEVOL calls the PiG driver routine for puff chemistry, growth,
c     and dumping
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c            
c     Modifications:  
c        2/02/06             Removed GRESDRIV, IRONDRIV renamed PIGDRIVE
c
c     Input arguments:
c        igrd                grid index
c
c     Output arguments:
c        none
c
c     Routines called:
c        PIGDRIVE
c
c     Called by:
c        CAMx
c        NESTING
c
      include 'camx.prm'
      include 'camx.com'
      include 'camxfld.com'
      include 'grid.com'
      include 'filunit.com'
c
c======================== Process Analysis Begin =======================
c
      include "procan.com"
c
c========================= Process Analysis End ========================
c
c-----Perform PiG evolution
c
      write(*,'(a20,$)') 'pigdrive ......'
      write(iout,'(a20,$)') 'pigdrive ......'
      call pigdrive(igrd,iptr2d(igrd),ncol(igrd),nrow(igrd),
     &              nlay(igrd),itzon,deltat(igrd),deltax(1,igrd),
     &              deltay(igrd),delx,dely,meshold(igrd),
     &              mapscl(iptr2d(igrd)),
     &              height(iptr3d(igrd)),rkv(iptr3d(igrd)),
     &              tempk(iptr3d(igrd)),tsurf(iptr2d(igrd)),
     &              press(iptr3d(igrd)),water(iptr3d(igrd)),
     &              windu(iptr3d(igrd)),windv(iptr3d(igrd)),
     &              cldtrns(iptr3d(igrd)),fcloud(iptr3d(igrd)),
     &              cwc(iptr3d(igrd)),pwr(iptr3d(igrd)),
     &              pws(iptr3d(igrd)),pwg(iptr3d(igrd)),
     &              cph(iptr3d(igrd)),cellat(iptr2d(igrd)),
     &              cellon(iptr2d(igrd)),fsurf(iptrlu(igrd)),
     &              vdep(iptrem(igrd)),conc(iptr4d(igrd)),
     &              cncrad(iptrad(igrd)),pigdump(1,igrd),
     &              pgmserr(1,igrd),fluxes(1,igrd),
     &              depfld(iptrdp(igrd)),ipsa3d(igrd))
c     call pigdiag(idiag,time,date,igrd,'After pigdrive      ')
      tcpu = dtime(tarray2)
      write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
      write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
      call flush(6)
      call flush(iout)
c
      return
      end

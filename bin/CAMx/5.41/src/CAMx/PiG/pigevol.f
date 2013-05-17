      subroutine pigevol(igrd,iproc_id)
      use filunit
      use grid
      use camxfld
      use camxcom
      use procan
      use chmstry
      use node_mod
c
c----CAMx v5.41 121109
c
c
c     PIGEVOL calls the PiG driver routine for puff chemistry, growth,
c     and dumping
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c     Modifications:
c        2/02/06             Removed GRESDRIV, IRONDRIV renamed PIGDRIVE
c
c     Input arguments:
c        igrd                grid index
c        iproc_id            process ID for this slice
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
      include 'flags.inc'
c
c-----Perform PiG evolution
c
      if( iproc_id .LE. 1 ) then
        write(*,'(a20,$)') 'pigdrive ......'
        call flush(6)
      endif
      write(iout,'(a20,$)') 'pigdrive ......'
      call pigdrive(mmxp(igrd),mmyp(igrd),mmzp(igrd),
     &              mi0(igrd),mj0(igrd),
     &              igrd,iptr2d(igrd),
     &              ncol(igrd),nrow(igrd),nlay(igrd),
     &              nspec,ndepspc*3,itzon,deltat(igrd),deltax(1,igrd),
     &              deltay(igrd),delx,dely,meshold(igrd),
     &              mapscl(iptr2d(igrd)),
     &              height(iptr3d(igrd)),
     &              rkv(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &              tsurf(iptr2d(igrd)),press(iptr3d(igrd)),
     &              water(iptr3d(igrd)),windu(iptr3d(igrd)),
     &              windv(iptr3d(igrd)),cldtrns(iptr3d(igrd)),
     &              fcloud(iptr3d(igrd)),
     &              cwc(iptr3d(igrd)),pwr(iptr3d(igrd)),
     &              pws(iptr3d(igrd)),pwg(iptr3d(igrd)),
     &              cph(iptr3d(igrd)),cellat(iptr2d(igrd)),
     &              cellon(iptr2d(igrd)),sfcz0(iptr2d(igrd)),
     &              vdep(iptrem(igrd)),conc(iptr4d(igrd)),
     &              pigdump(1,igrd),
     &              pgmserr(1,igrd),fluxes(1,igrd),
     &              depfld(iptrdp(igrd)),ipsa2d(igrd),ipsa3d(igrd),
     &              ipsadep(igrd),ipacl_3d(iptr3d_full(igrd)),iproc_id )
      tcpu = dtime(tarray2)
      if( iproc_id .LE. 1 ) then
         write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
         call flush(6)
       endif
       write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
       call flush(iout)
c
      return
      end

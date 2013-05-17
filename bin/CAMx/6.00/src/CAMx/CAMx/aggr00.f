      subroutine aggr00(igrd,ip,icode)
      use filunit
      use grid
      use chmstry
      use camxfld
      use camxcom
      use procan
      use tracer
      use node_mod
      use rtracchm
c
c----CAMx v6.00 130506
c
c     AGGR00 passes arrays from the common blocks to AGGREG
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c          
c     Modifications:
c        1/15/03     Added aggregation of deposition fields
c       03/15/09     Added code for deposition output for tracers
c       07/16/07 -bkoo-     Added check for HDDM
c       07/16/08 -bkoo-     Added DDM turn-off flag
c       10/29/09 -cemery-   Added code for RTRAC surface mass
c
c     Input arguments:
c        igrd                grid index
c        ip                  parent grid index
c        icode               flag to select when to sum mass on parent grid
c                              0 = sum both before and after aggregation
c                              1 = sum before aggregation
c                              2 = sum after aggregation
c                             >2 = do not sum
c
c     Output arguments:
c        none
c
c     Subroutine called:
c        MASSUM
c        AGGREG
c
c     Called by:
c        NESTING
c
      include "camx.prm"
      include "rtracsrf.inc"
c
c-----Entry point
c
      write(*,'(a20,$)') 'aggrg ......'
      write(iout,'(a20,$)') 'aggrg ......'
c
      if (icode.le.1) then
        call newgrid(ip)
        call massum(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &             ip,nspec,ncol(ip),nrow(ip),nlay(ip),deltax(1,ip),
     &             deltay(ip),depth(iptr3d(ip)),mapscl(iptr2d(ip)),
     &             conc(iptr4d(ip)),xmstmp(1,ip))
      endif
c
      call newgrid(igrd)
c
c  --- regular model species ----
c
      call aggreg(ncol(igrd),nrow(igrd),nlay(igrd),ncol(ip),nrow(ip),
     &            nlay(ip),nspec,i1(igrd),j1(igrd),i2(igrd),j2(igrd),
     &            nmesh(igrd),deltax(1,igrd),deltay(igrd),
     &            depth(iptr3d(igrd)),deltax(1,ip),deltay(ip),
     &            depth(iptr3d(ip)),conc(iptr4d(igrd)),
     &            conc(iptr4d(ip)) )
c
      call aggdep(ncol(igrd),nrow(igrd),ncol(ip),nrow(ip),ndepspc*3,
     &            i1(igrd),j1(igrd),i2(igrd),j2(igrd),nmesh(igrd),
     &            deltax(1,igrd),deltay(igrd),depfld(iptrdp(igrd)),
     &            deltax(1,ip),deltay(ip),depfld(iptrdp(ip)) )
c
c======================== Source Apportion Begin =======================
c
      if( ltrace .OR. ((lddm .OR. lhddm) .AND. lddmcalc(ip)) ) then
         call aggreg(ncol(igrd),nrow(igrd),nlay(igrd),ncol(ip),nrow(ip),
     &            nlay(ip),ntotsp,i1(igrd),j1(igrd),i2(igrd),j2(igrd),
     &            nmesh(igrd),deltax(1,igrd),deltay(igrd),
     &            depth(iptr3d(igrd)),deltax(1,ip),deltay(ip),
     &            depth(iptr3d(ip)),ptconc(ipsa3d(igrd)),
     &            ptconc(ipsa3d(ip)) )
         if( lptdepout ) then
            call aggdep(ncol(igrd),nrow(igrd),ncol(ip),nrow(ip),
     &            notimespc,
     &            i1(igrd),j1(igrd),i2(igrd),j2(igrd),nmesh(igrd),
     &            deltax(1,igrd),deltay(igrd),ptdryfld(ipsadep(igrd)),
     &            deltax(1,ip),deltay(ip),ptdryfld(ipsadep(ip)) )
            call aggdep(ncol(igrd),nrow(igrd),ncol(ip),nrow(ip),
     &            notimespc,
     &            i1(igrd),j1(igrd),i2(igrd),j2(igrd),nmesh(igrd),
     &            deltax(1,igrd),deltay(igrd),ptwetfld(ipsadep(igrd)),
     &            deltax(1,ip),deltay(ip),ptwetfld(ipsadep(ip)) )
         endif
         if( lsrfmod ) then
            call aggdep(ncol(igrd),nrow(igrd),ncol(ip),nrow(ip),ntotsp,
     &            i1(igrd),j1(igrd),i2(igrd),j2(igrd),nmesh(igrd),
     &            deltax(1,igrd),deltay(igrd),rtsolmas(ipsa2d(igrd)),
     &            deltax(1,ip),deltay(ip),rtsolmas(ipsa2d(ip)) )
            call aggdep(ncol(igrd),nrow(igrd),ncol(ip),nrow(ip),ntotsp,
     &            i1(igrd),j1(igrd),i2(igrd),j2(igrd),nmesh(igrd),
     &            deltax(1,igrd),deltay(igrd),rtvegmas(ipsa2d(igrd)),
     &            deltax(1,ip),deltay(ip),rtvegmas(ipsa2d(ip)) )
         endif
      endif
c
c========================= Source Apportion End ========================
c
c
c========================= Process Analysis Begin ==============================
c
      if( lirr ) then
         call aggreg(ncol(igrd),nrow(igrd),nlay(igrd),ncol(ip),nrow(ip),
     &            nlay(ip),ntotsp,i1(igrd),j1(igrd),i2(igrd),j2(igrd),
     &            nmesh(igrd),deltax(1,igrd),deltay(igrd),
     &            depth(iptr3d(igrd)),deltax(1,ip),deltay(ip),
     &            depth(iptr3d(ip)),ptconc(ipsa3d(igrd)),
     &            ptconc(ipsa3d(ip)) )
      endif
c
c========================= Process Analysis End ==============================
c
      if (icode.eq.0 .or. icode.eq.2) then
        call newgrid(ip)
        call massum(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &              ip,nspec,ncol(ip),nrow(ip),nlay(ip),deltax(1,ip),
     &              deltay(ip),depth(iptr3d(ip)),mapscl(iptr2d(ip)),
     &              conc(iptr4d(ip)),xmass(1,ip))
        do l = 1,nspec 
          xmsfin(l,ip) = xmsfin(l,ip) + xmass(l,ip) - xmstmp(l,ip) 
        enddo
      endif
c
      write(*,'(a)') '   Done'
      write(iout,'(a)') '   Done'
      call flush(6)
      call flush(iout)
c
      return
      end

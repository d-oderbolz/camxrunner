      subroutine setbc(ip)
      use grid
      use chmstry
      use camxfld
      use tracer
c
c----CAMx v6.00 130506
c
c     SETBC sets up boundary conditions for children grids
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c          
c     Modifications:
c        01/22/02     Now only sets OSAT to lower bound if not in RTRAC
c        01/30/02     Added code for RTRAC probing tool
c        03/15/09     Added code for deposition output for tracers
c
c     Input arguments:
c        ip                  parent grid index 
c
c     Output arguments:
c        none
c
c     Routines Called:
c        BC1GRD
c        BCMODFY
c
c     Called By:
c        NESTING 
c
      include "camx.prm"
c
      integer mvsa4d
c
c-----Entry point
c
c-----When children grids are not connected
c
      do ic = 1,nchdrn(ip)
        ig = idchdrn(ic,ip)
        call bc1grd(nspec,ncol(ip),nrow(ip),nlay(ip),ncol(ig),nrow(ig),
     &              nlay(ig),i1(ig),j1(ig),nmesh(ig),
     &              conc(iptr4d(ip)),conc(iptr4d(ig)) )
c
c======================== Source Apportion Begin =======================
c
        if( ltrace .OR. ((lddm .OR. lhddm) .AND. lddmcalc(ip)) ) then
            call bc1grd(ntotsp,ncol(ip),nrow(ip),nlay(ip),ncol(ig),
     &              nrow(ig),nlay(ig),i1(ig),j1(ig),nmesh(ig),
     &              ptconc(ipsa3d(ip)),ptconc(ipsa3d(ig)) )
            if( ltrace .AND. tectyp .NE. RTRAC .AND.
     &                                         tectyp .NE. RTCMC ) then
               mvsa4d = ncol(ig)*nrow(ig)*nlay(ig)*ntotsp
               do i=0,mvsa4d-1
                  ptconc(ipsa3d(ig)+i) = AMAX1( ptconc(ipsa3d(ig)+i),BNDLPT )
               enddo
            endif
        endif
c
c========================= Source Apportion End ========================
c
      enddo
c
c-----Modify BC's if some children are attached to each other
c
      do ic1 = 1,nchdrn(ip)
        ig1 = idchdrn(ic1,ip)
        do ic2 = 1,nchdrn(ip)
          ig2 = idchdrn(ic2,ip)
          call bcmodfy(nspec,ncol(ig1),nrow(ig1),nlay(ig1),ncol(ig2),
     &                 nrow(ig2),nlay(ig1),i1(ig1),j1(ig1),i2(ig1),
     &                 j2(ig1),i1(ig2),j1(ig2),i2(ig2),j2(ig2),
     &                 nmesh(ig1),nmesh(ig2),conc(iptr4d(ig1)),
     &                 conc(iptr4d(ig2)) )
c
c======================== Source Apportion Begin =======================
c
           if( ltrace .OR. ((lddm .OR. lhddm) .AND.
     &                      lddmcalc(ig1) .AND. lddmcalc(ig2)) ) then
               call bcmodfy(ntotsp,ncol(ig1),nrow(ig1),nlay(ig1),
     &                ncol(ig2),nrow(ig2),nlay(ig1),i1(ig1),j1(ig1),
     &                i2(ig1),j2(ig1),i1(ig2),j1(ig2),i2(ig2),j2(ig2),
     &                nmesh(ig1),nmesh(ig2),ptconc(ipsa3d(ig1)),
     &                ptconc(ipsa3d(ig2)) )
               if( ltrace .AND. tectyp .NE. RTRAC .AND.
     &                          tectyp .NE. RTCMC ) then
                   mvsa4d = ncol(ig1)*nrow(ig1)*nlay(ig1)*ntotsp
                   do i=0,mvsa4d-1
                      ptconc(ipsa3d(ig1)+i) = 
     &                           AMAX1( ptconc(ipsa3d(ig1)+i),BNDLPT )
                   enddo
                   mvsa4d = ncol(ig2)*nrow(ig2)*nlay(ig2)*ntotsp
                   do i=0,mvsa4d-1
                      ptconc(ipsa3d(ig2)+i) = 
     &                           AMAX1( ptconc(ipsa3d(ig2)+i),BNDLPT )
                   enddo
               endif
           endif
c
c========================= Source Apportion End ========================
c
        enddo
      enddo
c
      return
      end

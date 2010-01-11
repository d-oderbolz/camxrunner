      subroutine chemrxn(igrd,iproc_id)
      use filunit
      use grid
      use grid_nodes
      use chmstry
      use camxfld
      use camxcom
      use procan
c
      use master_mod
      use node_mod
c
c----CAMx v5.10 090918
c
c     CHEMRXN passes arrays from common blocks to CHEMDRIV
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c            
c     Modifications:  
c        none
c
c     Input arguments:
c        igrd                grid index
c
c     Output arguments:
c        none
c
c     Routines called:
c        CHEMDRIV
c        MASSUM
c
c     Called by:
c        CAMx
c        NESTING
c
      include 'camx.prm'
      include 'flags.com'
c
      integer iii, jjj, kkk !bwang: for debug
c
c-----Entry point
c
      call massum(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &           igrd,nspec,ncol(igrd),nrow(igrd),nlay(igrd),
     &           deltax(1,igrd),deltay(igrd),depth(iptr3d_full(igrd)),
     &           mapscl(iptr2d(igrd)),conc(iptr4d(igrd)),xmstmp(1,igrd))
c
      if (lchem) then
        if( iproc_id .LE. 1 ) then
          write(*,'(a20,$)') 'chemdriv ......'
        endif
        write(iout,'(a20,$)') 'chemdriv ......'
        call flush(6)
        call chemdriv(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &                igrd,ncol(igrd),nrow(igrd),nlay(igrd),
     &                deltat(igrd),itzon,idfin(iptr2d(igrd)),
     &                fcloud(iptr3d(igrd)),cldtrns(iptr3d(igrd)),
     &                water(iptr3d(igrd)),tempk(iptr3d(igrd)),
     &                press(iptr3d(igrd)),height(iptr3d_full(igrd)),
     &                cwc(iptr3d(igrd)),cph(iptr3d(igrd)),
     &                conc(iptr4d(igrd)),cncrad(iptrad(igrd)),
     &                cellat(iptr2d(igrd)),cellon(iptr2d(igrd)),
     &                ldark(iptr2d(igrd)),l3davg(igrd),iptr2d(igrd),
     &                ipsa3d(igrd),ipsa3d_rad(igrd),
     &                ipacl_3d(iptr3d_full(igrd)),iproc_id)
        tcpu = dtime(tarray2)
        if( iproc_id .LE. 1 ) then
           write(*,'(a,f10.3)') '   CPU = ', tarray2(1)
        endif
        write(iout,'(a,f10.3)') '   CPU = ', tarray2(1)
        call flush(6)
        call flush(iout)
      endif
c
      call massum(mxp,myp,mzp,i0,j0,ia,iz,ja,jz,ibcon,
     &            igrd,nspec,ncol(igrd),nrow(igrd),nlay(igrd),
     &            deltax(1,igrd),deltay(igrd),depth(iptr3d_full(igrd)),
     &            mapscl(iptr2d(igrd)),conc(iptr4d(igrd)),xmass(1,igrd))
c
      do l = 1,nspec 
        xmschem(l,igrd) = xmschem(l,igrd) + xmass(l,igrd) - 
     &                    xmstmp(l,igrd) 
      enddo
c
      return
      end

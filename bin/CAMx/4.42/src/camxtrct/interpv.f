      subroutine interpv(ncol,nrow,nlay,ncolf,nrowf,nlayf,mesh,mshv,
     &                   i1,j1,htcrs,htfin,fval)
c
c     INTERPV vertically interpolates coarse layer fields to a fine 
c     layer structure
c
c     Modifications:
c        none
c
c     Input arguments:
c        ncol              number of columns in coarse grid
c        nrow              number of rows in coarse grid
c        nlay              number of layers in coarse grid
c        ncolf             number of columns in fine grid
c        nrowf             number of rows in fine grid
c        nlayf             number of layers in fine grid
c        mesh              meshing factor for fine grid
c        mshv              vertical mesh number
c        i1,j1             starting indices for fine grid
c        htcrs             coarse grid layer interface height field (m)
c        htfin             fine grid layer interface height field (m)
c        fval              layer-centered field on coarse structure
c
c     Output arguments:
c        fval              layer-centered field on fine structure
c
c     Subroutine called:
c        none
c
      include 'camxtrct.inc'
c
      real*4 htcrs(MXCELL,MXCELL,MXLAYR),htfin(MXCELL,MXCELL,MXLAYR),
     &       fval(MXCELL,MXCELL,MXLAYR),mshv(MXLAYR)
      real*4 dtdz(MXLAYR+1),dum(MXLAYR),zmid(MXLAYR)
      real*4 zmidf
      integer*4 mesh,ncol,nrow,nlay,ncolf,nrowf,nlayf,i1,j1,j,i,ii,jj,
     &          kp,kg,kfin,kgrad 
c
c-----Entry Point
c
      do j = 1,nrowf
        do i = 1,ncolf
          ii = i1 + (i-2)/mesh
          jj = j1 + (j-2)/mesh
          do kp = 1,nlay
            zmid(1) = htcrs(ii,jj,kp)/2.
            dum(kp) = fval(i,j,kp)
            if (kp.gt.1) 
     &        zmid(kp) = (htcrs(ii,jj,kp) + htcrs(ii,jj,kp-1))/2.
          enddo
          dtdz(1) = (dum(2) - dum(1))/(zmid(2) - zmid(1))
          dtdz(nlay+1) = (dum(nlay) - dum(nlay-1))/
     &                   (zmid(nlay) - zmid(nlay-1))
          do kp = 2,nlay
            dtdz(kp) = (dum(kp) - dum(kp-1))/(zmid(kp) - zmid(kp-1))
          enddo
c
          kg = 1
          do 10 kp = 1,nlay
            if (mshv(kp).eq.1) then
              fval(i,j,kg) = dum(kp)
              kg = kg + 1
              goto 10
            else
              do kfin = kg,kg+mshv(kp)-1
                zmidf = htfin(i,j,kfin)/2.
                if (kfin.gt.1) 
     &            zmidf = (htfin(i,j,kfin) + htfin(i,j,kfin-1))/2.
                kgrad = kp
                if (zmidf.ge.zmid(kp)) kgrad = kp + 1
                fval(i,j,kfin) = dum(kp) + 
     &                           dtdz(kgrad)*(zmidf - zmid(kp))
              enddo
              kg = kg + mshv(kp)
            endif
 10       continue
        enddo
      enddo
c
      return
      end

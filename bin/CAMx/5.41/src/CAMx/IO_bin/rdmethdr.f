      subroutine rdmethdr(iunit,filtype,igrid,begtim,begdate,endtim,
     &                    enddate,ncol,nrow,nlay,xomod,yomod,
     &                    dxmod,dymod,iout,nvar)
      use grid
c 
      implicit none
      include 'camx.prm'
      include 'flags.inc'
c
      character*10 filtype
      integer iunit,iout,igrid,begdate,enddate,ncol,nrow,nlay
      integer nvar
      real begtim,endtim
      real xomod,yomod,dxmod,dymod
c
      return
c
      end

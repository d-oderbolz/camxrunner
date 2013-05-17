c**** ADJBCDDM.F
c
      subroutine adjbcddm(adjwest,adjeast,adjsouth,adjnorth,
     &                                     nx,ny,nz,nspc,saconc)
      use bndary
      use chmstry
      use tracer
      use node_mod
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine adjusts the boundary conditions for the DDM
c   species. This is necessary because the boundary conditions
c   change through time to reflect the interpolated met.
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c      Argument description:
c       Inputs:
c           adjwest  R  adjustment factors for West boundary
c           adjeast  R  adjustment factors for East boundary
c           adjsouth R  adjustment factors for South boundary
c           adjnorth R  adjustment factors for North boundary
c           nx       I  number of columns in field
c           ny       I  number of rows in field
c           nz       I  number of layers in field
c           nspc     I  number of species in the field
c       Inputs:
c           saconc   R  field of sensitivies
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     08/23/07   --gwilson--    Original development
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
      real    adjwest(MXCELLS,MXLAYER)
      real    adjeast(MXCELLS,MXLAYER)
      real    adjsouth(MXCELLS,MXLAYER)
      real    adjnorth(MXCELLS,MXLAYER)
      integer nx
      integer ny
      integer nz
      integer nspc
      real    saconc(nx,ny,nz,nspc)
c
c-----------------------------------------------------------------------
c    External functions:
c----------------------------------------------------------------------
c
      logical, external :: isbound
c
c-----------------------------------------------------------------------
c    Local variables:
c----------------------------------------------------------------------
c
      integer iddm, imod , iptr, ioff, nedge, i, j, k
c
c-----------------------------------------------------------------------
c    Data statements:
c-----------------------------------------------------------------------
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
c  --- loop over all boundary condition sensitivities ---
c
      do iddm = 1, nbcddm
c
c  --- loop over all model species ---
c
         do imod = 1, nspec
c
c  --- loop over all layers ---
c
           do k = 1, nz
c
c  --- do West and East edges ---
c
              do j = 2, ny-1
c
c  --- load the west boundary ---
c
                 if( lbndry ) then
                    ioff = IDXBWS
                 else
                    ioff = 1
                 endif
                 iptr = iptddm(imod) + (iddm-1)*nedge +
     &                                              nicddm + ioff - 1
                 if( .NOT. lmpi .OR.
     &                isbound( mibcon(1),mmxp(1),mmyp(1),1,0 ) ) then
                     saconc(1,j,k,iptr) = saconc(1,j,k,iptr) * adjwest(j,k)
                 endif
c
c  --- load the east boundary ---
c
                 if( lbndry ) then
                     ioff = IDXBES
                 else
                     ioff = 1
                 endif
                 iptr = iptddm(imod) + (iddm-1)*nedge +
     &                                              nicddm + ioff - 1
                 if( .NOT. lmpi .OR.
     &              isbound( mibcon(1),mmxp(1),mmyp(1),mmxp(1),0 ) ) then
                       saconc(nx,j,k,iptr) = saconc(nx,j,k,iptr) * adjeast(j,k)
                 endif
              enddo
c
c  --- do South and North edges ---
c
              do i = 2, nx-1
c
c  --- load the south boundary ---
c
                if( lbndry ) then
                    ioff = IDXBST
                 else
                    ioff = 1
                 endif
                 iptr = iptddm(imod) + (iddm-1)*nedge +
     &                                              nicddm + ioff - 1
                 if( .NOT. lmpi .OR.
     &                   isbound( mibcon(1),mmxp(1),mmyp(1),0,1 ) ) then
                      saconc(i,1,k,iptr) = saconc(i,1,k,iptr) * adjsouth(i,k)
                 endif
c
c  --- load the east boundary ---
c
                 if( lbndry ) then
                     ioff = IDXBNT
                 else
                     ioff = 1
                 endif
                 iptr = iptddm(imod) + (iddm-1)*nedge +
     &                                               nicddm + ioff - 1
                 if( .NOT. lmpi .OR.
     &               isbound( mibcon(1),mmxp(1),mmyp(1),0,mmyp(1) ) ) then
                      saconc(i,ny,k,iptr) = saconc(i,ny,k,iptr) * adjnorth(i,k)
                 endif
              enddo
c
c  --- get the next layer ---
c
           enddo
c
c  --- get the next model species ---
c
         enddo
c
c  --- Get the next DDM boundary sensitivity ---
c
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

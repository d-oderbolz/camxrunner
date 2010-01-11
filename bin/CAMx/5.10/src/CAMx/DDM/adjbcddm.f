c**** ADJBCDDM.F
c
      subroutine adjbcddm(adjwest,adjeast,adjsouth,adjnorth,
     &                                     nx,ny,nz,nspc,saconc)
      use bndary
      use chmstry
      use tracer
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine adjusts the boundary conditions for the DDM
c   species. This is necessary because the boundary conditions
c   change through time to reflect the interpolated met.
c
c     Copyright 1996-2009
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
                 if (ibeg(j).eq.-999) CYCLE
c
c  --- load the west boundary ---
c
                 i = ibeg(j) - 1 
                 if( lbndry ) then
                    ioff = IDXBWS
                 else
                    ioff = 1
                 endif
                 iptr = iptddm(imod) + (iddm-1)*nedge +
     &                                              nicddm + ioff - 1
                 saconc(i,j,k,iptr) = saconc(i,j,k,iptr) * adjwest(j,k)
c
c  --- load the east boundary ---
c
                 i = iend(j) + 1
                 if( lbndry ) then
                     ioff = IDXBES
                 else
                     ioff = 1
                 endif
                 iptr = iptddm(imod) + (iddm-1)*nedge +
     &                                              nicddm + ioff - 1
                 saconc(i,j,k,iptr) = saconc(i,j,k,iptr) * adjeast(j,k)
              enddo
c
c  --- do South and North edges ---
c
              do i = 2, nx-1
                if (jbeg(i).eq.-999) CYCLE
c
c  --- load the south boundary ---
c
                j = jbeg(i) - 1 
                if( lbndry ) then
                    ioff = IDXBST
                 else
                    ioff = 1
                 endif
                 iptr = iptddm(imod) + (iddm-1)*nedge +
     &                                              nicddm + ioff - 1
                 saconc(i,j,k,iptr) = saconc(i,j,k,iptr) * adjsouth(i,k)
c
c  --- load the east boundary ---
c
                 j = jend(i) + 1
                 if( lbndry ) then
                     ioff = IDXBNT
                 else
                     ioff = 1
                 endif
                 iptr = iptddm(imod) + (iddm-1)*nedge +
     &                                               nicddm + ioff - 1
                 saconc(i,j,k,iptr) = saconc(i,j,k,iptr) * adjnorth(i,k)
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

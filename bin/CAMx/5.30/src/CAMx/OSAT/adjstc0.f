c*** ADJSTC0
c
      subroutine adjstc0(numcol,numrow,numlay,igrd,icl,jcl,kcl,delcls,
     &                 concls,cytcls,c0cls,tmtrac,cellvol,rainvol,levap)
      use grid
      use tracer
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c   Description:
c     This routine makes the wet dep adjustments to the tracer 
c     species.  The adjustments are based on the relative fluxes between 
c     cell and rain for the regular model species due to wet dep.
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c       numcol  I number of columns in this slice
c       numrow  I number of rows in this slice
c       numlay  I number of layers in this slice
c       igrd    I grid number
c       icl     I the X grid location of current cell
c       jcl     I the X grid location of current cell
c       kcl     I the vertical grid location of current layer
c       delcls  R change in cell concentration for each tracer class
c       concls  R current cell concentration for each tracer class
c       cytcls  R amount of tracer cycled between air and rain for
c                 eachtracer class
c       c0cls   R current rain concentration for each tracer class
c       cellvol R cell volume (m3)
c       rainvol R volume of rain water (m3)
c       levap   L true if the rain totally evaporates in this cell                                                         
c     Outputs 
c       tmtrac  R total rain mass for tracers (umol/m3)
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c       04/08/03  --gwilson--   Original development
c       08/15/03  --gyarwood--  Revise adjustments
c       09/20/03  --gwilson--   changes for PSAT
c       10/10/05  --gyarwood--  Revise adjustments
c       12/22/09  --cemery--    Improved treatment of "c0"
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      implicit none
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer numcol
      integer numrow
      integer numlay
      integer igrd
      integer icl
      integer jcl
      integer kcl
      integer icls
      real    delcls(*)
      real    concls(*)
      real    cytcls(*)
      real    c0cls(*)
      real    tmtrac(*)
      real    cellvol
      real    rainvol
      logical levap
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer idxcel, idx, i
      real    delrn, delpt
      real    trgas, trpcp, clsgas, clspcp, sumcls
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- calculate the index of the cell in the grid ---
c

      idxcel = icl + numcol*(jcl-1) + 
     &                      numcol*numrow*(kcl-1)
c
c   --- if levap is true, all the tracers are returned to the cell ---
c
      if( levap) then
        do i=iptcls(1),nptcls(ntrcls)
          idx = ipsa3d(igrd)-1+idxcel + numcol * numrow * numlay * (i-1)
          ptconc(idx) = ptconc(idx) + tmtrac(i) / cellvol
          tmtrac(i) = 0.
        enddo
        goto 9999
      endif
c
c   --- adjust the gas each tracer class ----
c
      do icls=1,ntrcls
         delrn = MAX(delcls(icls),0.0)
         delpt = -MIN(delcls(icls),0.0)
         do 20 i=iptcls(icls),nptcls(icls)
            if( .NOT. lsagas(i) ) goto 20
            idx = ipsa3d(igrd)-1+idxcel + 
     &                              numcol * numrow * numlay * (i-1)
            trgas = ptconc(idx)
            trpcp = tmtrac(i)/cellvol
            clsgas = concls(icls)
            clspcp = c0cls(icls)
            clspcp = MAX(clspcp,1E-20)
            ptconc(idx) = trgas
     &                    + delpt*trpcp / clspcp
     &                      - delrn*trgas / clsgas
     &                        - cytcls(icls)*trgas / clsgas
     &                          + cytcls(icls)*(trgas + trpcp) /
     &                                             ( clsgas + clspcp )
            ptconc(idx) = MAX(BNDLPT,ptconc(idx))
            tmtrac(i) = cellvol*( trpcp
     &                    - delpt*trpcp / clspcp
     &                      + delrn*trgas / clsgas
     &                        - cytcls(icls)*trpcp / clspcp
     &                          + cytcls(icls)*(trgas + trpcp) /
     &                                            ( clsgas + clspcp) )
            tmtrac(i) = MAX(0.0, tmtrac(i))
   20    continue
      enddo
c
c   --- for particulates, make adjustment based on 
c       change in regular model concentration ---
c
      do icls=1,ntrcls
         sumcls = 0.
         do i=iptcls(icls),nptcls(icls)
            idx = ipsa3d(igrd)-1+idxcel + 
     &                              numcol * numrow * numlay * (i-1)
            sumcls = sumcls + ptconc(idx)
         enddo
         if( sumcls .GT. 0 ) then
           do 30 i=iptcls(icls),nptcls(icls)
             if( lsagas(i) ) goto 30
             idx = ipsa3d(igrd)-1+idxcel + 
     &                             numcol * numrow * numlay * (i-1)
             tmtrac(i) = tmtrac(i) + cellvol * delcls(icls) * 
     &                                           ptconc(idx) / sumcls
             ptconc(idx) = ptconc(idx) - 
     &                           delcls(icls) * ptconc(idx) / sumcls
   30      continue
         endif
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 return
      end

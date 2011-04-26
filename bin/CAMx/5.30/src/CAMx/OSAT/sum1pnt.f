      subroutine sum1pnt(numcols,numrows,nspmod,nsptrac,igroup,idx,
     &    nptsrc,emsbas,emsoth,emslft,emstot,emspnt,emssum,izcel,lemit)
      use grid
      use chmstry
      use pigsty
      use filunit
      use tracer
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation
c
c
c----CAMx v5.30 101223
c
c     SUM1PNT sums up the point emission of one species for a given group
c
c       07/19/02  --gwilson-- Added seperate source area map for each grid.
c       08/25/05  --cemery--  Revamped PiG pointer arrays for source group
c                             and region
c       11/16/06  --gwilson-- fixed bug in point source override for PiG sources
c       11/27/06  --gwilson-- fixed bug in calculating emissions table
c       10/28/09  --gwilson-- Changed dimension of variables to accomodate 
c                             the dynamic memory allocation
c
c     Input argument:
c        numcols           max number of columns in any grid
c        numrows           max number of columns in any grid
c        nspmod            number of model species
c        nsptrac           number of tracer species
c        igroup            group ID
c        idx               specie ID
c        emspnt            the species emission for the group
c
c     Output arguments:
c        emssum            emission summed over grid
c        emsbas            base emission
c        emsoth            "otherwise" emission
c        emslft            leftover emission
c        emstot            total emission
c        lemit             flag to determine if tracer class is emitted
c
      include "camx.prm"
      include "flags.inc"
c
      character*200 fname
      integer   numcols
      integer   numrows
      integer   nspmod
      integer   nsptrac
      integer igroup
      integer idx
      integer nptsrc
      real    emsbas(nspmod,nsptrac)
      real    emsoth(nspmod,nsptrac)
      real    emslft(numcols,numrows,nspmod)
      real    emstot(numcols,numrows,nspmod)
      real    emspnt(MXPTSRC)
      real    emssum(nspmod,nsptrac)
      integer izcel(*)
      logical lemit(*)
      logical luse
c
      real, allocatable, dimension(:) :: xloctmp
      real, allocatable, dimension(:) :: yloctmp
c
c  --- allocate the local arrays ---
c
      allocate( xloctmp(nptsrc) )
      allocate( yloctmp(nptsrc) )
c
c   --- set the file name for elevated points emissions file ---
c
      if( igroup .EQ. 0 ) then
        write(fname,'(A,I3)') 'PTSOURCE -- UNIT ',iptem
      else
        fname = tptfil(igroup)
      endif
c
c  --- make sure this species is needed ---
c
      luse = .FALSE.
      do icls=1,ntrcls
        if( trspmap(idx,icls) .NE. 0.  .OR.
     &                         yratmap(idx,icls) .NE. 0. ) then
          if( trspmap(idx,icls) .NE. 0. ) lemit(icls) = .TRUE.
          luse = .TRUE.
        endif
      enddo
      if( .NOT. luse ) goto 9999
c
c   --- sum up the emissions for each point ---
c
        if (llatlon) then
          do n = 1,nptsrc
            xloctmp(n) = xlocpt(n) - xorg
            yloctmp(n) = ylocpt(n) - yorg
          enddo
        else
          do n = 1,nptsrc
            xloctmp(n) = xlocpt(n)/1000. - xorg
            yloctmp(n) = ylocpt(n)/1000. - yorg
          enddo
        endif
c
      do 70 i=1,nptsrc
         icel = 1 + INT( xloctmp(i)/delx )
         jcel = 1 + INT( yloctmp(i)/dely )
         if(icel .LE. 0 .OR. icel .GT. ncol(1)) goto 70
         if(jcel .LE. 0 .OR. jcel .GT. nrow(1)) goto 70
         icrs = icel
         jcrs = jcel
c
c   --- find out if a nest contains this source  ---
c
         igrd = 1
         do ig = 2,ngrid
           xlocnst = xloctmp(i) - (inst1(ig)-1)*delx
           ylocnst = yloctmp(i) - (jnst1(ig)-1)*dely
           ii = 2 + INT( xlocnst/delx * FLOAT( meshold(ig) ) )
           jj = 2 + INT( ylocnst/dely * FLOAT( meshold(ig) ) )
           if( ii .GT. 1 .AND. jj .GT. 1 .AND. ii .LT. ncol(ig) .AND.
     &                                           jj .LT. nrow(ig) ) then
              igrd = ig
              icel = ii
              jcel = jj
            endif
         enddo
c
c  --- get the region for this cell from mapping array ----
c
         imap = igrmap(igrd,icel,jcel)
c
c  --- change the region if the override is set ---
c
         if( izcel(i) .LT. 0 ) imap = ABS( izcel(i) )
         if( imap .LE. 0 .OR. imap .GT. nregin ) then
            write(iout,'(//,a)') 'ERROR in SUM1PNT:'
            write(iout,'(/,1X,A,A,I4)') 'Invalid region found in',
     &             ' point source override when reading point source file.'
            write(iout,'(1X,A,I4)') 'Region code      : ',imap
            write(iout,'(1X,A,I4)') 'Number of regions: ',nregin
            write(iout,'(10X,A,/,A)') 'Point source filename: ',
     &                                                 fname(:istrln(fname))
            write(iout,'(1X,2A)') 'Check the values in the point ',
     &                                                     'source overide.'
            call camxerr()
         endif
c
c  --- calculate the index into the tracer species for this group/region ---
c
         if( ngroup .GT. 0 ) then
c
c   --- if group is base emissions, add to "leftover" group ----
c
            if( igroup .EQ. 0 ) then
               if( leftovr ) then
                  do icls=1,ntrcls
                     if( trspmap(idx,icls) .NE. 0.  .OR.
     &                                 yratmap(idx,icls) .NE. 0. ) then
                         ipt = iemcls(icls)-1 + imap+ngroup*nregin
                         emsbas(idx,ipt) = emsbas(idx,ipt) + emspnt(i)
                     endif
                  enddo
               endif
c
c  --- if doing PiG and source is a PIG source, set the PiG map and group
c      pointers ---
c
               if( lpigsa(i) .AND. emspnt(i) .GT. 0. ) then
                  ipigmap(i) = imap
                  ipiggrp(i) = ngroup
               endif
c
               do icls=1,ntrcls
                  if( trspmap(idx,icls) .NE. 0.  .OR.
     &                                 yratmap(idx,icls) .NE. 0. ) then
                     emstot(icrs,jcrs,idx) = 
     &                              emstot(icrs,jcrs,idx) +  emspnt(i)
                  endif
               enddo
c
c   --- otherwise, add to this group/region and subtract from "leftover" ---
c
            else
c
c  --- if doing PiG and source is a PIG source, set the PiG map and group
c      pointers ---
c
               if( lpigsa(i) .AND. emspnt(i) .GT. 0. ) then
                  ipigmap(i) = imap
                  ipiggrp(i) = igroup-1
               endif
c
               do icls=1,ntrcls
                  if( trspmap(idx,icls) .NE. 0.  .OR.
     &                                 yratmap(idx,icls) .NE. 0. ) then
                     ipt = iemcls(icls)-1 + imap +(igroup-1)*nregin
                     emssum(idx,ipt) = emssum(idx,ipt) + emspnt(i)
                     if( leftovr ) then
                       ipt = iemcls(icls)-1 + imap+ngroup*nregin
                       emsoth(idx,ipt) = emsoth(idx,ipt) + emspnt(i)
                     endif
                  endif
               enddo
               do icls=1,ntrcls
                   if( trspmap(idx,icls) .NE. 0.  .OR.
     &                                 yratmap(idx,icls) .NE. 0. ) then
                     emslft(icrs,jcrs,idx) = 
     &                         emslft(icrs,jcrs,idx) + emspnt(i)
                  endif
               enddo
            endif
c
c   --- only using regular model emissions ---
c
         else
            do icls=1,ntrcls
               if( trspmap(idx,icls) .NE. 0.  .OR.
     &                                 yratmap(idx,icls) .NE. 0. ) then
                  ipt = iemcls(icls) - 1 + imap
                  emssum(idx,ipt) = emssum(idx,ipt) + emspnt(i)
               endif
            enddo
c
c  --- if doing PiG and source is a PIG source, set the PiG map and group
c      pointers ---
c
            if( lpigsa(i) .AND. emspnt(i) .GT. 0. ) then
               ipigmap(i) = imap
               ipiggrp(i) = 0
            endif
         endif
  70  continue
c
 9999 continue
c
c  --- deallocate the local arrays ---
c
      deallocate( xloctmp )
      deallocate( yloctmp )
c
      return
      end

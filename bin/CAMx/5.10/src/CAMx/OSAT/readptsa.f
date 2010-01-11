c**** READPTSA
c
      subroutine readptsa(idate,btim)
      use filunit
      use grid
      use camxcom
      use ptemiss
      use tracer
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine reads one hour of emissions from each of the elevated
c   emissions files and calculates the emissions levels.  It then
c   places these emissions in the appropriate place in the point source
c   array used for tracer emissions.  The tracer to which the emissions
c   is assigned depends on the source group and the source region.
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Inputs:
c          idate  I  date of current hour (YYJJJ)
c          btim   R  time of current hour
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     06/06/96   --gwilson--    Original development
c     12/10/96   --gwilson--    Added switch to skip emissions if PiG
c     02/01/97   --gwilson--    Put fuzz factor of 0.1 ppb to catch if
c                               emissions of leftover group is negative.
c     02/03/97   --gwilson--    Put code to ignore emissions on boundary.
c     01/20/99   --cemery---    Grid cell size from file should be meters
c                               for all cartesian projections (UTM, LCP,
c                               PSP)
c     11/06/01   --cemery--     Input dates are now Julian
c     07/19/02   --gwilson--    Added seperate source area map for each grids.
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.com'
      include 'tracerpts.com'
c
c-----------------------------------------------------------------------
c    Arguement declarations:
c-----------------------------------------------------------------------
c
      integer   idate
      real      btim
c
c-----------------------------------------------------------------------
c    Local parameters:
c-----------------------------------------------------------------------
c
c   FUZZ   R    fuzz factor used to determine if emissions are truly < 0
c
      real   FUZZ
c
      parameter( FUZZ = -0.0001 )
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*200 fname
      integer imap, i, j, ii, jj
      integer ndate, icel, jcel, xloccrs, yloccrs
      real    xloctmp, yloctmp, ttime
c
      integer izcel(MXPTSRC)
      real    lefcls(MXTRCLS,MXPTSRC)
      real    emscls(MXTRCLS,MXPTSRC)
c
c
c-----------------------------------------------------------------------
c    Common blocks:
c-----------------------------------------------------------------------
c
      common /comreadptsa/ emscls, lefcls, izcel
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- set the date and times ---
c
      ndate = idate
      ttime = btim/100.0
c
c   --- initialize emissions to zero ---
c
      do i=1,nptsrc
         do j=1,nsaspc
            sapnts(i,j) = 0.
         enddo
         do j=1,ntrcls
            emscls(j,i) = 0. 
         enddo
      enddo
c
c  --- loop over groups ---
c
      do igroup = 0,ngroup
c
c   --- set the file name for elevated points emissions file ---
c
         if( igroup .EQ. 0 ) then
             write(fname,'(A,I3)') 'PTSOURCE -- UNIT ',iptem
         else
             fname = tptfil(igroup)
         endif
c
c  --- for each group, call routine to read the emissions for
c      the group ----
c
         call rdptgrp(ndate,ttime,igroup,ntrcls,emscls,izcel)
c
c  --- if doing source groups calculate the 
c      left-over and put emissions into proper place in the emissions
c      array ---
c
         if( ngroup .GT. 0 ) then
            do 10 i=1,nptsrc
c
c  --- get the region for this cell from mapping array ----
c
               if( .NOT.llatlon ) then
                  icel = INT( (xlocpt(i)/1000. - xorg) / delx ) + 1
                  jcel = INT( (ylocpt(i)/1000. - yorg) / dely ) + 1
                  xloccrs = xlocpt(i)/1000. - xorg
                  yloccrs = ylocpt(i)/1000. - yorg
               else
                  icel = INT( (xlocpt(i) - xorg) / delx ) + 1
                  jcel = INT( (ylocpt(i) - yorg) / dely ) + 1
                  xloccrs = xlocpt(i) - xorg
                  yloccrs = ylocpt(i) - yorg
               endif
               if(icel .LE. 0 .OR. icel .GT. ncol(1)) goto 10
               if(jcel .LE. 0 .OR. jcel .GT. nrow(1)) goto 10
c
c   --- find out if a nest contains this source  ---
c
               igrd = 1
               do ig = 2,ngrid
                 xloctmp = xloccrs - (inst1(ig)-1)*delx
                 yloctmp = yloccrs - (jnst1(ig)-1)*dely
                 if( xloctmp .GT. 0. .AND. yloctmp .GT. 0. ) then
                    ii = 2 + INT( xloctmp/delx * FLOAT( meshold(ig) ) )
                    jj = 2 + INT( yloctmp/dely * FLOAT( meshold(ig) ) )
                    if( ii .GT. 1 .AND. jj .GT. 1 .AND. ii .LT. ncol(ig)
     &                                    .AND. jj .LT. nrow(ig) ) then
                       igrd = ig
                       icel = ii
                       jcel = jj
                    endif
                 endif
               enddo
               imap = igrmap(igrd,icel,jcel)
               if( izcel(i) .LT. 0 ) imap = ABS( izcel(i) )
               if( imap .LE. 0 .OR. imap .GT. nregin ) then
                  write(iout,'(//,a)') 'ERROR in READPTSA:'
                  write(iout,'(/,1X,A,A,I4)') 'Invalid region found in',
     &                           ' point source override when reading ',
     &                                              'point source file.'
                  write(iout,'(1X,A,I4)') 'Region code      : ',imap
                  write(iout,'(1X,A,I4)') 'Number of regions: ',nregin
                  write(iout,'(10X,A,/,A)') 'Point source filename: ',
     &                                             fname(:istrln(fname))
                  write(iout,'(1X,2A)') 'Check the values in the ',
     &                                           'point source overide.'
                  call camxerr()
               endif
c
c  --- calculate the leftover emissions to use in last source group ---
c
               do icls=1,ntrcls
                  if( igroup .EQ. 0 ) then
                      lefcls(icls,i) = emscls(icls,i)
                  else
                      lefcls(icls,i) = lefcls(icls,i) - emscls(icls,i)
c
c  --- put emissions into position in gridded tracer emissions array ---
c
                      ipos = iemcls(icls) - 1 + imap + (igroup-1)*nregin
                      sapnts(i,ipos) = emscls(icls,i)
                  endif
               enddo
c
c  --- next point source ----
c
   10      continue
c
c  --- only one group, just load emissions into arrays from
c      position 0 in gridded array ----
c
         else
            do 20 i=1,nptsrc
c
c  --- get the region for this cell from mapping array ----
c
               if( .NOT.llatlon ) then
                  icel = INT( (xlocpt(i)/1000. - xorg) / delx ) + 1
                  jcel = INT( (ylocpt(i)/1000. - yorg) / dely ) + 1
                  xloctmp = xlocpt(i)/1000. - xorg
                  yloctmp = ylocpt(i)/1000. - yorg
               else
                  icel = INT( (xlocpt(i) - xorg) / delx ) + 1
                  jcel = INT( (ylocpt(i) - yorg) / dely ) + 1
                  xloctmp = xlocpt(i) - xorg
                  yloctmp = ylocpt(i) - yorg
               endif
               if(icel .LE. 0 .OR. icel .GT. ncol(1)) goto 20
               if(jcel .LE. 0 .OR. jcel .GT. nrow(1)) goto 20
c
c   --- find out if a nest contains this source  ---
c
               igrd = 1
               do ig = 2,ngrid
                 xloctmp = xloctmp - (inst1(ig)-1)*delx
                 yloctmp = yloctmp - (jnst1(ig)-1)*dely
                 ii = 2 + INT( xloctmp/delx * FLOAT( meshold(ig) ) )
                 jj = 2 + INT( yloctmp/dely * FLOAT( meshold(ig) ) )
                 if( ii .GT. 1 .AND. jj .GT. 1 .AND. 
     &                   ii .LT. ncol(ig) .AND. jj .LT. nrow(ig) ) then
                    igrd = ig
                    icel = ii
                    jcel = jj
                  endif
               enddo
               imap = igrmap(igrd,icel,jcel)
               if( izcel(i) .LT. 0 ) imap = ABS( izcel(i) )
               if( imap .LE. 0 .OR. imap .GT. nregin ) then
                  write(iout,'(//,a)') 'ERROR in READPTSA:'
                  write(iout,'(/,1X,A,A,I4)') 'Invalid region found in',
     &                           ' point source override when reading ',
     &                                              'point source file.'
                  write(iout,'(1X,A,I4)') 'Region code      : ',imap
                  write(iout,'(1X,A,I4)') 'Number of regions: ',nregin
                  write(iout,'(10X,A,/,A)') 'Point source filename: ',
     &                                             fname(:istrln(fname))
                  write(iout,'(1X,2A)') 'Check the values in the ',
     &                                           'point source overide.'
                  call camxerr()
               endif
c
c   --- put emissions in array at correct offset ---
c
               do icls=1,ntrcls
                  ipos = iemcls(icls) - 1 + imap
                  sapnts(i,ipos) = emscls(icls,i)
               enddo
c
c  --- next point source ----
c
   20       continue
         endif
c
c  --- get emissions from the next group ----
c
      enddo
c
c   --- all groups read, load the leftover group ---
c
      if( ngroup .GT. 0 .AND. leftovr ) then
         do 30 i=1,nptsrc
c
c  --- get the region for this cell from mapping array ----
c
            if( .NOT.llatlon ) then
               icel = INT( (xlocpt(i)/1000. - xorg) / delx ) + 1
               jcel = INT( (ylocpt(i)/1000. - yorg) / dely ) + 1
               xloctmp = xlocpt(i)/1000. - xorg
               yloctmp = ylocpt(i)/1000. - yorg
            else
               icel = INT( (xlocpt(i) - xorg) / delx ) + 1
               jcel = INT( (ylocpt(i) - yorg) / dely ) + 1
               xloctmp = xlocpt(i) - xorg
               yloctmp = ylocpt(i) - yorg
            endif
            if(icel .LE. 0 .OR. icel .GT. ncol(1)) goto 30
            if(jcel .LE. 0 .OR. jcel .GT. nrow(1)) goto 30
c
c   --- find out if a nest contains this source  ---
c
            igrd = 1
            do ig = 2,ngrid
              xloctmp = xloctmp - (inst1(ig)-1)*delx
              yloctmp = yloctmp - (jnst1(ig)-1)*dely
              ii = 2 + INT( xloctmp/delx * FLOAT( meshold(ig) ) )
              jj = 2 + INT( yloctmp/dely * FLOAT( meshold(ig) ) )
              if( ii .GT. 1 .AND. jj .GT. 1 .AND.
     &                ii .LT. ncol(ig) .AND. jj .LT. nrow(ig) ) then
                 igrd = ig
                 icel = ii
                 jcel = jj
               endif
            enddo
            imap = igrmap(igrd,icel,jcel)
            if( izcel(i) .LT. 0 ) imap = ABS( izcel(i) )
            if( imap .LE. 0 .OR. imap .GT. nregin ) then
               write(iout,'(//,a)') 'ERROR in READPTSA:'
               write(iout,'(/,1X,A,A,I4)') 'Invalid region found in',
     &          ' point source override when reading point source file.'
               write(iout,'(1X,A,I4)') 'Region code      : ',imap
               write(iout,'(1X,A,I4)') 'Number of regions: ',nregin
               write(iout,'(10X,A,/,A)') 'Point source filename: ',
     &                                             fname(:istrln(fname))
               write(iout,'(1X,2A)') 'Check the values in the point ',
     &                                                 'source overide.'
               call camxerr()
            endif
c
c   ---- make sure leftover group is not negative ---
c
            do icls=1,ntrcls
               if( lefcls(icls,i) .LT. 0. ) then
                  if( lefcls(icls,i) .GT. FUZZ ) then
                    lefcls(icls,i) = 0.
                  else
                    write(iout,'(//,A)') 'ERROR in READPTSA:'
                    write(iout,'(/,4A,I5,A)')
     &                     'Negative ',clsnam(idxcls(icls)),
     &                       ' emissions calculated in leftover group ',
     &                                        'in point source: ',i,' :'
                    write(iout,'(A,F20.6)')'   Value = ',lefcls(icls,i)
                    call camxerr()
                  endif
               endif
               ipos = iemcls(icls) - 1 + imap + ngroup * nregin
               sapnts(i,ipos) = lefcls(icls,i)
            enddo
   30    continue
      endif
c
c  --- all files read ---
c
      return
      end

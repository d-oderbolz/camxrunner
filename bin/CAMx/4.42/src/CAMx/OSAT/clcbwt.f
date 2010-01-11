c**** CLCBWT
c
      subroutine clcbwt(idate,btim,jdate,etim,ncolx,nrowy,nlays)
c
c----CAMx v4.42 070603
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine calculates the weighted reactivity factor for VOC
c   species for the boundary conditions.  The mass is weighted by layer
c   thickness giving the weighted average for the cell.  The average
c   over all cells is then calculated.  The averages are calculated for
c   the entire bounday and for each boundary seperately.
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c     Argument declarations:
c        idate   I   beginning date of simulation (YYJJJ)
c        btim    R   beginning time of simulation
c        jdate   I   ending date of simulation (YYJJJ)
c        etim    R   ending time of simulation
c        ncolx    I   number of columns in coarse grid
c        nrowy    I   number of rows in coarse grid
c        nlays    I   number of layers in coarse grid
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     05/29/96   --gwilson--    Original development
c     11/06/01   --cemery--     Input dates are now Julian
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'camx.com'
      include 'filunit.com'
      include 'chmstry.com'
      include 'bndary.com'
      include 'tracer.com'
c
c-----------------------------------------------------------------------
c    Arguement declarations:
c-----------------------------------------------------------------------
c
      integer   idate
      real      btim
      integer   jdate
      real      etim
      integer   ncolx
      integer   nrowy
      integer   nlays
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*10 tpspc
      integer      ndate, jdlast, i, j
      integer      idtnow
      real         ttime, ttlast, timnow
      real         sumvoc(MXTRCLS), sumkoh(MXTRCLS), summir(MXTRCLS)
      real         sumyld(MXTRCLS), yldvoc(MXTRCLS)
      real         consum(MXSPEC,0:IDXBTP), ctin, contop(MXSPEC)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- call routine to get the file pointers to the proper place ---
c
      call bndprep(btim,idate,etim,jdate)
      do i=1,MXSPEC
         contop(i) = bdnl(i)
      enddo
c
c   --- read the top concentration file and store in species array ---
c
      rewind(itopc)
  111 continue
      read(itopc,8000,ERR=7000,END=222) tpspc, ctin
      do i=1,nspec
        if( tpspc .eq. spname(i) ) then
          contop(i) = ctin
          goto 111
        endif
      enddo
      goto 111
c
c   --- set ending date and time to be consistent with how time is 
c       counted here ----
c      
  222 continue
      ndate = idate
      ttime = btim/100.0
      jdlast = jdate
      ttlast = etim/100.0
      if( ttlast .EQ. 0. ) then
         jdlast = jdlast - 1
         ttlast = 24.0
      endif
c
c  --- initialize the dates and times ---
c
      idtnow = ndate
      timnow = ttime
c
      call rdsumbc(idtnow,timnow,jdlast,ttlast,ncolx,nrowy,nlays,
     &             contop,consum)
c
c   --- calculate the fractions ---
c
      if( lbndry ) then
         do j=1,IDXBTP
            do icls=1,ntrcls
               sumvoc(icls) = 0.
               sumkoh(icls) = 0.
               summir(icls) = 0.
               sumyld(icls) = 0.
               yldvoc(icls) = 0.
            enddo
            do i=1,nspec
               if( consum(i,j) .GT. 0. ) then
                   do icls=1,ntrcls
                      sumvoc(icls) = sumvoc(icls) + 
     &                                     consum(i,j) * trspmap(i,icls)
                      sumkoh(icls) = sumkoh(icls) + 
     &                        consum(i,j) * rkohrt(i)  * trspmap(i,icls)
                      summir(icls) = summir(icls) + 
     &                        consum(i,j) * rmirrt(i)  * trspmap(i,icls)
                      if( yratmap(i,icls) .GT. 0. ) then
                          sumyld(icls) = sumyld(icls) + consum(i,j)
                          yldvoc(icls) = yldvoc(icls) + 
     &                                     consum(i,j) * yratmap(i,icls)
                      endif
                   enddo
               endif
            enddo
            do icls=1,ntrcls
               if( sumvoc(icls) .GT. 0. ) then
                   wtkoh(iptcls(icls)+j) = sumkoh(icls) / sumvoc(icls)
                   wtmir(iptcls(icls)+j) = wtmir(icls) / sumvoc(icls)
               else
                   wtkoh(iptcls(icls)+j) = 0.
                   wtmir(iptcls(icls)+j) = 0.
               endif
               if( sumyld(icls) .GT. 0. ) then
                   yrates(iptcls(icls)+j) = yldvoc(icls) / sumyld(icls)
               else
                   yrates(iptcls(icls)+j) = 0.
               endif
            enddo
         enddo
      else
         do icls=1,ntrcls
            sumvoc(icls) = 0.
            sumkoh(icls) = 0.
            summir(icls) = 0.
            sumyld(icls) = 0.
            yldvoc(icls) = 0.
         enddo
         do i=1,nspec
            if( consum(i,0) .GT. 0. ) then
                do icls=1,ntrcls
                    sumvoc(icls) = sumvoc(icls) + 
     &                                     consum(i,0) * trspmap(i,icls)
                    sumkoh(icls) = sumkoh(icls) + 
     &                        consum(i,0) * rkohrt(i)  * trspmap(i,icls)
                    summir(icls) = summir(icls) + 
     &                        consum(i,0) * rmirrt(i)  * trspmap(i,icls)
                    if( yratmap(i,icls) .GT. 0. ) then
                       sumyld(icls) = sumyld(icls) + consum(i,0)
                       yldvoc(icls) = yldvoc(icls) + 
     &                                     consum(i,0) * yratmap(i,icls)
                    endif
                enddo
            endif
         enddo
         do icls=1,ntrcls
            if( sumvoc(icls) .GT. 0. ) then
               wtkoh(iptcls(icls)+1) = sumkoh(icls) / sumvoc(icls)
               wtmir(iptcls(icls)+1) = summir(icls) / sumvoc(icls)
            else
               wtkoh(iptcls(icls)+1) = 0.
               wtmir(iptcls(icls)+1) = 0.
            endif
            if( sumyld(icls) .GT. 0. ) then
               yrates(iptcls(icls)+1) = yldvoc(icls) / sumyld(icls)
            else
               yrates(iptcls(icls)+1) = 0.
            endif
         enddo
      endif
c
c  --- fill the global arrays of top concentrations for tracer species ---
c
      if( lbndry ) then
         ioff = IDXBTP
      else
         ioff = 1
      endif
      do i=1,nspec
         do icls=1,ntrcls
            ptloft(iptcls(icls)+ioff) = ptloft(iptcls(icls)+ioff) + 
     &                             contop(i) * trspmap(i,icls)
         enddo
      enddo
      do i=1,ntotsp
         ptloft(i) = MAX( ptloft(i), BNDLPT )  
      enddo
c
c  --- return to the calling routine ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in CLCBWT:'
      write(iout,'(/,1X,A)') 'Reading top concentrations file.'
      call camxerr()
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 8000 format(A10,F10.0)
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

c*** WSRFSA
c
      subroutine wsfcsa(ngrd, iendat, endtim, nox, noy, nspsa, saavrg)
      use camxcom
      use tracer
      use filunit
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c   Description:
c     This routine writes the surface concentrations for the tracer
c     species.
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c    Argument description:
c      ngrd    I  grid index
c      iendat  I  ending date for this hour
c      endtim  R  ending time for this hour
c      nox     I  number of X cells in domain
c      noy     I  number of Y cells in domain
c      nspsa   I  number of species
c      saavrg  R  array of surface concentrations (average)
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'flags.inc'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
       integer   ngrd
       integer   iendat
       real      endtim
       integer   nox
       integer   noy
       integer   nspsa
       real      saavrg(nox,noy,nspsa)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer       idate, jdate
      integer       isegmt, i, j, l, nspcout
      character*4   ispec(10,MXTRSP)
      real          btim, etim
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   ---- put species names into integer array ----
c 
      isegmt = 1
      nspcout = 0
      do 10 j=1,ntotsp
          if( loutsa(j) ) then
             nspcout = nspcout + 1
             read(ptname(j),'(10A1)') (ispec(i,j),i=1,10)
          endif
   10 continue
c
c  --- write the surface concentration file if requested ----
c
      if( lsfcfl .and. .not.lhdfout) then
c
c   --- set up the dates and times for the surface file ---
c
          jdate = iendat
          etim = AINT(ANINT(endtim)/100.) + amod(ANINT(endtim),100.)/60.
          idate = jdate
          btim = ANINT( 1000*(etim - ANINT(dtout)/60.) )/1000.
          if( btim .LT. 0. ) then
              btim = btim + 24.0
              idate = idate - 1
              if (MOD(idate,1000) .EQ. 0 ) then
                if ( MOD(INT(idate/1000)-1,4) .EQ. 0 ) then
                  idate = (INT(idate/1000)-1)*1000 + 366
                else
                  idate = (INT(idate/1000)-1)*1000 + 365
                end if
              endif
          endif
          write(iowsfc(ngrd),ERR=7000) idate, btim, jdate, etim
          do 40 l=1,nsaspc
            if( loutsa(l) ) write(iowsfc(ngrd)) isegmt, 
     &           (ispec(i,l),i=1,10),((saavrg(i,j,l),i=1,nox),j=1,noy)
   40     continue
          if( ntrtim .GT. 0 ) then
             do 50 l=nsaspc+1,ntotsp
               write(iowsfc(ngrd)) isegmt, (ispec(i,l),i=1,10), 
     &                                        ((0.0,i=1,nox),j=1,noy)
   50        continue
          endif
      endif
c
c  --- return to calling routine ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,A)') 'ERROR in WSFCSA:'
      write(iout,9000,ERR=9999)'Writing output tracer file: ',
     &                           sfcfil(ngrd)(:istrln(sfcfil(ngrd)))
      call camxerr()
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format(/,1X,2A)
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

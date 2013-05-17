c*** WCONSA
c
      subroutine wconsa(iendat, endtim, nox, noy, noz, nspsa, saconc)
      use camxcom
      use tracer
      use filunit
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c   Description:
c     This routine writes the instantaneous file for the tracer
c     species.  
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c    Argument description:
c      iendat  I  ending date for this hour
c      endtim  R  ending time for this hour
c      nox     I  number of X cells in domain
c      noy     I  number of Y cells in domain
c      noz     I  number of layers in domain
c      nspsa   I  number of species
c      saconc  R  array of concentrations (instantaneous)
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c      10/24/01  Removed BSWAP and converted integer strings to character*4
c      11/06/01  Removed calls to JULDATE and CALDATE (all dates in Julian)
c      08/23/06  Revised to just write instant fields at end of simulation
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
       integer   iendat
       real      endtim
       integer   nox
       integer   noy
       integer   noz
       integer   nspsa
       real      saconc(nox,noy,noz,nspsa)
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer       idate, jdate
      integer       isegmt, i, j, k, l
      character*4   ispec(10,MXTRSP)
      real          btim, etim
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- set up the dates and times for instantaneous files ---
c
      jdate = iendat
      btim = endtim 
      etim = endtim + 10.0
      idate = jdate
      if( etim .EQ. 24.0 ) then
         jdate = jdate + 1
         etim = 0.0
         if( MOD(jdate,1000) .GT. 365 ) then
            if( MOD(INT(jdate/1000),4) .EQ. 0 ) then
               if( MOD(jdate,1000) .EQ. 367 )
     &                     jdate = (INT(jdate/1000)+1)*1000 + 1
            else
               jdate = (INT(jdate/1000)+1)*1000 + 1
            endif
         endif
      endif
c
c   ---- put species names into integer array ----
c 
      do 10 j=1,ntotsp
          read(ptname(j),'(10A1)') (ispec(i,j),i=1,10)
   10 continue
c
c  --- call routine to write the header ---
c
      call hdrwsa( iowcon(IDXCRS), confil(IDXCRS), 'AIRQUALITY', 
     &                           nsaspc, noz, idate, btim, jdate, etim )
c
c   --- write the data for this hour ----
c
      isegmt = 1
      btim = AINT(ANINT(btim)/100.) + amod(ANINT(btim),100.)/60.
      etim = AINT(ANINT(etim)/100.) + amod(ANINT(etim),100.)/60.
      write(iowcon(IDXCRS),ERR=7000) idate, btim, jdate, etim 
      do 20 l=1,nsaspc
         do 30 k=1,noz
            write(iowcon(IDXCRS)) isegmt, (ispec(i,l),i=1,10), 
     &                              ((saconc(i,j,k,l),i=1,nox),j=1,noy)
   30    continue
   20 continue
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
      write(iout,'(//,A)') 'ERROR in WCONSA:'
      write(iout,9000,ERR=9999)'Writing output tracer file: ',
     &                           confil(IDXCRS)(:istrln(confil(IDXCRS)))
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

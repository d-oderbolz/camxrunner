c*** WRTCPA
c
      subroutine wrtcpa(ngrd, iendat, endtim, igrd, nox, noy, noz, 
     &                                                  nspsa, saconc)
      use camxcom
      use tracer
      use filunit
      use grid
c
c----CAMx v5.30 101223
c
c-----------------------------------------------------------------------
c   Description:
c     This routine writes the outptut file for the process analysis
c     gridded reaction rate data (CPA data).  This routine is writes 
c     the data for the coarse grid.  The format is the same as the 
c     coarse grid average concentration file for the regular model 
c     species.
c
c     Copyright 1996 - 2010
c     ENVIRON International Corporation 
c
c    Argument description:
c      ngrd    I  grid index
c      iendat  I  ending date for this hour
c      endtim  R  ending time for this hour
c      igrd    I  grid number
c      nox     I  number of X cells in domain
c      noy     I  number of Y cells in domain
c      noz     I  number of layers in domain
c      nspsa   I  number of species
c      saconc  R  array of concentrations (instantaneous)
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
       integer   igrd
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
      character*4   ispec(10)
      real          btim, etim
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- set layers based on the output flag ---
c
      nlayer = noz
      if( .NOT. l3davg(igrd) ) nlayer = 1 
      isegmt = 1
c
c   --- set up the dates and times ----
c
      jdate = iendat
      etim = ANINT(endtim)/100.
      idate = jdate
      if( dtout .GE. 60.0 ) then
          btim = ANINT( 1000*(etim - ANINT(dtout)/60.) )/1000.
      else
          btim = ANINT( 1000*(etim - ANINT(dtout)/100.) )/1000.
      endif
      if( btim .LT. 0. ) then
          btim = btim + 24.0
          idate = idate - 1
      endif
      write(iowsfc(ngrd),ERR=7000) idate, btim, jdate, etim
      do l=1,nsaspc
        do k = 1,nlayer
c
c   --- fill the integer array with process analysis "species" ---
c
          read(ptname(l),'(10A1)') (ispec(i),i=1,10)
c
c   --- write concentration field ---
c
          write(iowsfc(ngrd)) isegmt,(ispec(i),i=1,10),
     &                             ((saconc(i,j,k,l),i=1,nox),j=1,noy)
        enddo
      enddo
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
      write(iout,'(//,A)') 'ERROR in WRTCPA'
      write(iout,9000,ERR=9999) 'Writing gridded output chemical ',
     &                'process analysis (.cpa) file.'
      call camxerr()
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format(/,1X,3A)
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

c*** AVGRCPDDM
c
      subroutine avgrcpddm(idate,etime)
c
c----CAMx v4.42 070603
c
c-----------------------------------------------------------------------
c   Description:
c     This routine writes the average sensitivities to the 
c     DDM receptor file.  
c
c     Copyright 1996-2007
c     ENVIRON International Corporation
c
c   Argument descriptions:
c     Inputs:
c        idate   I  ending date of this time interval
c        etime   R  ending time of this time interval
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c    07/18/01  --gyarwood--  Original development from AVGRCP for OSAT
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'camx.com'
      include 'tracer.com'
      include 'filunit.com'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer   idate 
      real      etime 
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer      idatbg, idatnd, ircp, i, l
      real         timbg, timnd
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- if no receptor file is provided, just return ---
c
      if( .NOT. lrcpfil ) goto 9999
c
c --- set the date and time ---
c
      idatnd = idate
      timnd = ANINT(etime)/100.
      if( timnd .EQ. 0. ) then
         timnd = 24.0
         idatnd = idatnd - 1
      endif
      idatbg = idatnd
      if( dtout .GE. 60. ) then
          timbg = AINT( 1000*(timnd - ANINT(dtout)/60.) )/1000.
      else
          timbg = AINT( 1000*(timnd - ANINT(dtout)/100.) )/1000.
      endif
      if(timbg .LT. -0.01 ) then
        timbg = timbg + 24.
        idatbg = idatbg - 1
      endif
c
c ---  write the header for this hour ---
c
      write(iowrcp,9000,ERR=7000)
      write(iowrcp,9000,ERR=7000) 'Data for Period',
     &                                    idatbg, timbg, idatnd, timnd
c
c --- loop over all receptors ---
c
      do ircp=1,nrecep
        write(iowrcp,9001,ERR=7000) ircp
        do i=1,ntotsp
          if(loutsa(i)) write(iowrcp,9002,ERR=7000) 
     &                        conrcp(i,ircp)
        enddo
        write(iowrcp,*,ERR=7000)
      enddo
      call flush(iowrcp)
c
c --- re-initialize the averages to zero ---
c
      do l=1,MXTRSP
        do i=1,MXRECP
           conrcp(l,i) = 0.        
        enddo
      enddo
c
c --- return to calling routine ---
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in AVGRCPDDM:'
      write(iout,9010,ERR=9999) 'Writing DDM average file at: ',
     &                           idatbg,timbg,idatnd,timnd
      call camxerr()
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format(A,',',2(I10,',',F10.2,','))
 9001 format(I5,',',$)
 9002 format(:,1p1e11.4,',',$)
 9010 format(/,1X,A,',',2(I10,',',F10.2,','))
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

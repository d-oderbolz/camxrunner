c*** WRRCPRT
c
      subroutine wrrcprt(idate,etime)
      use filunit
      use camxcom
      use rtracchm
      use chmstry
      use tracer
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c   Description:
c     This routine writes the decay rates for the RTRAC species
c     at the receptor locations.
c
c     Copyright 1996 - 2012
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
      integer   idate 
      real      etime 
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   idatbg, idatnd, ircp, i, l
      real      timbg, timnd
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- if no receptor file is provided, just return ---
c
      if( .NOT. lrcpfil ) goto 9999
c
c  --- if no receptors requested, just exit ----
c
      if( nrcprt .LE. 0 ) goto 9999
c
c  ---- set the date and time ---
c
      idatnd = idate
      timnd = AINT(ANINT(etime)/100.) + amod(ANINT(etime),100.)/60.
      if( timnd .EQ. 0. ) then
         timnd = 24.0
         idatnd = idatnd - 1
      endif
      idatbg = idatnd
      timbg = ANINT( 1000*(timnd - ANINT(dtout)/60.) )/1000.
      if(timbg .LT. 0. ) then
        timbg = timbg + 24.
        idatbg = idatbg - 1
      endif
c
c  ---  write the header for this hour ---
c
      write(iowrcp,9000,ERR=7000)
      write(iowrcp,9000,ERR=7000) 'Data for Period',
     &                                    idatbg, timbg, idatnd, timnd
c
c  --- loop over all receptors ----
c
      do ircp=1,nrcprt
c
c  --- write the header for this receptor ----
c
          write(iowrcp,9001,ERR=7000) 'Receptor',ircp,idatbg,
     &                                timbg,(rcpdcy(ircp,i),i=1,nrtgas)
c
c  --- next receptor ----
c
      enddo
      call flush(iowrcp)
c
c  ---- re-initialize the averages to zero ---
c
      do l=1,MXRTCEL
         do i=1,nspec
            rcpdcy(l,i) = 0.        
         enddo
      enddo
c
c  --- return to calling routine ---
c
      call flush(iowrcp)
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in AVGRCP:'
      write(iout,9010,ERR=9999) 'Writing Tracer average file at: ',
     &                           idatbg,timbg,idatnd,timnd
      call camxerr()
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format(A,',',2(I10.5,',',F10.2,','))
 9001 format(A8,I5,',',I10.5,',',F10.2,',',500(:,1p1e10.4,',',1X))
 9010 format(/,1X,A,',',2(I10.5,',',F10.2,','))
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

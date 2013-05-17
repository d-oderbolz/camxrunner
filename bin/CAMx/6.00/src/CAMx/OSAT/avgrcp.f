c*** AVGRCP
c
      subroutine avgrcp(idate,etime)
      use filunit
      use camxcom
      use tracer
c
c----CAMx v6.00 130506
c
c-----------------------------------------------------------------------
c   Description:
c     This routine writes the average concentrations to the 
c     receptor average file.  
c
c     Copyright 1996 - 2013
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
c    12/11/2000 --gwilson-- Now handles the case where peak cell could
c                           not be found.  This could happen if not
c                           outputting ozone as an average species.
c     04/02/03  --gwilson-- Added grid number to recptors defined by
c                           cell index
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
c  ---- set the date and time ---
c
      idatnd = idate
      timnd = ANINT(etime)/100.
      if( timnd .EQ. 0. ) then
         timnd = 24.0
         idatnd = idatnd - 1
      endif
      idatbg = idatnd
      if( dtout .GE. 60.0 ) then
          timbg = ANINT( 1000*(timnd - ANINT(dtout)/60.) )/1000.
      else
          timbg = ANINT( 1000*(timnd - ANINT(dtout)/100.) )/1000.
      endif
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
c  --- loop over all receptors ---
c
      do ircp=1,nrecep
c
c  --- write the header for this receptor ----
c
          write(iowrcp,9001,ERR=7000) 'Receptor',ircp
          do icls=1,ntrcls
             write(iowrcp,9002,ERR=7000) (conrcp(i,ircp),
     &                                     i=iptcls(icls),nptcls(icls))
          enddo
c
          if( ntrtim .GT. 0 ) then
              write(iowrcp,9002,ERR=7000) (conrcp(i,ircp),
     &               i=ipttim,nsaspc-1,2),(BNDLPT,i=nsaspc+1,npttim-1,2)
              write(iowrcp,9002,ERR=7000) (conrcp(i,ircp),
     &                 i=ipttim+1,nsaspc,2),(BNDLPT,i=nsaspc+2,npttim,2)
          endif
c
c  --- next receptor ----
c
      enddo
      call flush(iowrcp)
c
c  ---- re-initialize the averages to zero ---
c
      do l=1,ntotsp
         do i=1,nrecep
            conrcp(l,i) = 0.        
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
 9001 format(A10,',',I5,',')
 9002 format(500(:,1p1e10.4,',',1X))
 9010 format(/,1X,A,',',2(I10.5,',',F10.2,','))
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

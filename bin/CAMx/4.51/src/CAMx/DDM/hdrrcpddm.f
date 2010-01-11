c*** HDRRCPDDM
c
      subroutine hdrrcpddm(idate,btim,jdate,etim)
c
c----CAMx v4.51 080522
c
c-----------------------------------------------------------------------
c   Description:
c     This routine writes the header information for the receptor
c     output file for DDM.
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c     Argument declarations:
c        idate   I   beginning date of simulation (YYJJJ)
c        btim    R   beginning time of simulation
c        jdate   I   ending date of simulation (YYJJJ)
c        etim    R   ending time of simulation
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c    07/18/01  --gyarwood--  Original development from HDRRCP for OSAT
c    04/02/03  --gwilson--   Added grid number to recptors defined by
c                            cell index
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
      include 'camx.com'
      include 'chmstry.com'
      include 'tracer.com'
      include 'filunit.com'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer   idate
      real      btim
      integer   jdate
      real      etim
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*40 cdate
      integer      ndate, ndlast, itype, i, j, nbound, noutsen
      real         ttime, ttlast
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c --- if no receptor file is provided, just return ---
c
      if( .NOT. lrcpfil ) goto 9999
c
c  --- set up the date and time ---
c
      ndate = idate
      ttime = btim/100.0
      ndlast = jdate
      ttlast = etim/100.0
      if( ttlast .EQ. 0. ) then
          ttlast = 24.0
          ndlast = ndlast - 1
      endif
c
c  --- get the date and time and write header records ---
c
      call fdate( cdate )
      write(iowrcp,9010,ERR=7000) 'CAMx', runmsg, verson
      write(iowrcp,'(A)',ERR=7000) cdate(1:24)
c
c  --- write the dates and times of simuation ---
c
      write(iowrcp,9000,ERR=7000)
      write(iowrcp,9001,ERR=7000) ndate, ttime, ndlast, ttlast
      write(iowrcp,9008,ERR=7000)'Average Interval ',ANINT(dtout)/60.
c
c  --- write summaries of sensitivities requested ---
c
      write(iowrcp,9000,ERR=7000)
      write(iowrcp,9002,ERR=7000) 'Number of IC influencing species   ',
     &                                                            nicddm
      write(iowrcp,9003,ERR=7000) 'IC influencing species             '
      if (nicddm.gt.0)
     &        write(iowrcp,9003,ERR=7000)  (icddmsp(i),i=1,nicddm)
      write(iowrcp,9000,ERR=7000)
      write(iowrcp,9002,ERR=7000) 'Number of IC sensitivities         ',
     &                                                      nspec*nicddm
      write(iowrcp,9000,ERR=7000)
c
      nbound = 1
      if (lbndry) nbound = 5
      if (nbcddm.EQ.0) nbound = 0
      write(iowrcp,9002,ERR=7000) 'Number of BC influencing species   ',
     &                                                            nbcddm
      write(iowrcp,9003,ERR=7000) 'BC influencing species             '
      if (nicddm.gt.0)
     &        write(iowrcp,9003,ERR=7000)  (bcddmsp(i),i=1,nbcddm)
      write(iowrcp,9000,ERR=7000)
      write(iowrcp,9002,ERR=7000) 'Number of boundary groups          ',
     &                                                            nbound
      write(iowrcp,9002,ERR=7000) 'Number of BC sensitivities         ',
     &                                               nspec*nbound*nbcddm
      write(iowrcp,9000,ERR=7000)
c
      write(iowrcp,9002,ERR=7000) 'Number of emiss influencing species',
     &                                                            nemddm
      write(iowrcp,9003,ERR=7000) 'Emissions influencing species      '
      if (nemddm.gt.0)
     &        write(iowrcp,9003,ERR=7000)  (emddmsp(i),i=1,nemddm)
      write(iowrcp,9000,ERR=7000)
      write(iowrcp,9002,ERR=7000) 'Number of emission source areas    ',
     &                                                            nregin
      write(iowrcp,9002,ERR=7000) 'Number of emission categories      ',
     &                                                            ngroup
      write(iowrcp,9002,ERR=7000) 'Number of emission sensitivities   ',
     &                                        nspec*ngroup*nregin*nemddm
      write(iowrcp,9000,ERR=7000)
c
c   --- write the sensitivity names to output ---
c
      write(iowrcp,9002,ERR=7000) 'Total number of sensitivities      ',
     &                                                            ntotsp
      noutsen = 0
      do i = 1,ntotsp
        if (loutsa(i)) 
     &    noutsen = noutsen + 1
      enddo
      write(iowrcp,9002,ERR=7000) 'Number of sensitivities output     ',
     &                                                           noutsen
      if (noutsen.EQ.0) goto 7001
      write(iowrcp,9000,ERR=7000)
      write(iowrcp,9003,ERR=7000) 'Short Sensitivity Names'
      do i = 1,ntotsp
        if (loutsa(i)) 
     &      write(iowrcp,9003,ERR=7000) ptname(i)
      enddo
      write(iowrcp,9000,ERR=7000)
      write(iowrcp,9003,ERR=7000) 'Long Sensitivity Names'
      do i = 1,ntotsp
        if (loutsa(i)) 
     &      write(iowrcp,9003,ERR=7000) ptlong(i)
      enddo
      write(iowrcp,9000,ERR=7000)
      write(iowrcp,9000,ERR=7000)
c
c   --- write the receptor definitions to output ---
c
      write(iowrcp,9002,ERR=7000) 'Number of receptors ',nrecep
      write(iowrcp,9004,ERR=7000)
c
c   --- do all receptors ---
c
      do i=1,nrecep
         if( idrcp(i) .EQ. IDPNT ) then
            itype = 0
            write(iowrcp,9005,ERR=7000) i, rcpnam(i), itype, 
     &                                              recepx(i), recepy(i)
         else if( idrcp(i) .EQ. IDCEL ) then
            itype = 1
            write(iowrcp,9006,ERR=7000) i, rcpnam(i), itype, igrdrcp(i),
     &                                          irecep(i,1), jrecep(i,1)
         else if( idrcp(i) .EQ. IDAVG ) then
            itype = nclrcp(i)
            write(iowrcp,9006,ERR=7000) i, rcpnam(i), itype, igrdrcp(i),
     &                                          irecep(i,1), jrecep(i,1)
            do j=2,nclrcp(i)
                 write(iowrcp,9011,ERR=7000) irecep(i,j), jrecep(i,j)
            enddo
         else if( idrcp(i) .EQ. IDWAL ) then
            itype = 3
            write(iowrcp,9006,ERR=7000) i, rcpnam(i), itype, igrdrcp(i),
     &                                          iwalbg(i), iwalnd(i)
            write(iowrcp,9011,ERR=7000) jwalbg(i), jwalnd(i)
            write(iowrcp,9011,ERR=7000) kwalbg(i), kwalnd(i)
         endif
      enddo
c
c  --- write the header for rest of data ---
c
      write(iowrcp,9000,ERR=7000)
      write(iowrcp,9000,ERR=7000) 'Time Varying Sensitivity Data'
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
      write(iout,'(//,a)') 'ERROR in HDRRCPDDM:'
      write(iout,9020) 'Writing header of DDM receptor file.'
      call camxerr()
c
 7001 continue
      write(iout,'(//,a)') 'ERROR in HDRRCPDDM:'
      write(iout,9020) 'Number of sensitivities requested'//
     &    'for output is zero'
      call camxerr()
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format(2(:,A,','))
 9001 format(' File Duration  ,',2(I10.5,',',F10.2,','))
 9002 format(A,',',I10)
 9003 format(:,A,',',$)
 9004 format('  No,   Name,  ',2x,'Grid#,   Type,    Xloc,      Yloc,')
 9005 format(I4,',',1X,A10,',',1X,I4,', ,',2(F10.1,',',1X))
 9006 format(I4,',',1X,A10,',',1X,I4,',',1X,I4,',',2(I10,',',1X))
 9008 format(A,',',F10.4)
 9010 format(3(A,','))
 9011 format(20X,',,,',2(I10,',',1X))
 9020 format(/,1X,2(A,','))
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

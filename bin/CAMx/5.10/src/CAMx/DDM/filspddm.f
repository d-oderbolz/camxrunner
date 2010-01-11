c**** FILSPDDM
c
      subroutine filspddm(iaffec,caffec,cinflu,srcnam,
     &                                    iptfam,ngrps,nregs,lout )
      use filunit
      use chmstry
      use tracer
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine fills the array of species names for DDM species.  It
c   loads all of the names for a given family.  The argument variables
c   determine which family is to be filled.
c     Argument definitions:
c      Outputs:
c         iptfam   I  pointer into array of current species name
c      Inputs:
c         iaffec   I  value indicating order of affected species   
c         caffec   C  name of affected species 
c         cinflu   C  name of influencing species
c         srcnam   C  name of the source of the influence
c         ngrps    I  number of groups to use (0 for IC/BC)
c         nregs    I  number of regions to use (0 for IC/BC)
c         lout     L  flag for determing if the affected species is
c                     to be output to average file
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     03/23/99   --gwilson--    Original development
c     07/16/07   --bkoo--       Revised for HDDM
c                               Added HRVOC
c     06/11/08   --bkoo--       Added rate constant sensitivity
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
       integer      iaffec
       character*10 caffec
       character*10 cinflu
       character*10 srcnam
       integer      iptfam
       integer      ngrps
       integer      nregs
       logical      lout 
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
       integer iinflu, igrp, ireg
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- HDDM sensitivity names
c
      if ( srcnam .EQ. 'HDDM' ) then
         iptfam = iptfam + 1
         lsamap(iptfam) = iptfam
         ptlong(iptfam)(1:4) = caffec(1:4)
         ptlong(iptfam)(5:8) = srcnam(1:4)

         ! Here, ngrps passes the index of the current HDDM sens group
         iphddm(1,ngrps) = 0
         iphddm(2,ngrps) = 0
         do i = 1, nddmsp - nhddm
            if (ptlong(i)(5:14).EQ.hddmsp(1,ngrps)) iphddm(1,ngrps) = i
            if (ptlong(i)(5:14).EQ.hddmsp(2,ngrps)) iphddm(2,ngrps) = i
         enddo
         if (iphddm(1,ngrps).EQ.0 .OR. iphddm(2,ngrps).EQ.0 ) goto 7001

         write(ptlong(iptfam)( 9:11),'(I3.3)') iphddm(1,ngrps)
         write(ptlong(iptfam)(12:14),'(I3.3)') iphddm(2,ngrps)

         write(ptname(iptfam)(1:2),'(I2.2)') iaffec
         ptname(iptfam)(3:6) = srcnam(1:4)
         ptname(iptfam)(7:8) = ptlong(iptfam)(10:11)
         ptname(iptfam)(9:10)= ptlong(iptfam)(13:14)
c
c  --- set the flag for outputing this species to average file ---
c
         loutsa(iptfam) = lout
c
c  --- replace all blanks with underscores ---
c
         do i=1,14
            if( ptlong(iptfam)(i:i) .EQ. ' ' )
     &                                   ptlong(iptfam)(i:i) = '_'
         enddo
         do i=1,10
            if( ptname(iptfam)(i:i) .EQ. ' ' )
     &                                   ptname(iptfam)(i:i) = '_'
         enddo
         write(idiag,9002) caffec, hddmsp(1,ngrps),'&',hddmsp(2,ngrps),
     &                                  ptlong(iptfam), ptname(iptfam)

         goto 9999
      endif
c
c  --- Rate constant sensitivity names
c
      if ( srcnam .EQ. 'RATE' ) then
         iptfam = iptfam + 1
         lsamap(iptfam) = iptfam
         ptlong(iptfam)(1:4) = caffec(1:4)
         ptlong(iptfam)(5:10) = srcnam(1:4)
         ptlong(iptfam)(11:14) = cinflu(1:4)
         write(ptname(iptfam)(1:2),'(I2.2)') iaffec
         ptname(iptfam)(3:7) = srcnam(1:4)
         ! Here, ngrps passes the index of the current Rate constant sens group
         write(ptname(iptfam)(8:10),'(I3.3)') ngrps

         do i = 1, iprate(0,ngrps)
           if ( iprate(i,ngrps) .LE. 0 .OR.
     &          iprate(i,ngrps) .GT. nreact ) goto 7002
         enddo
c
c  --- set the flag for outputing this species to average file ---
c
         loutsa(iptfam) = lout
c
c  --- replace all blanks with underscores ---
c
         do i=1,14
            if( ptlong(iptfam)(i:i) .EQ. ' ' )
     &                                   ptlong(iptfam)(i:i) = '_'
         enddo
         do i=1,10
            if( ptname(iptfam)(i:i) .EQ. ' ' )
     &                                   ptname(iptfam)(i:i) = '_'
         enddo
         write(idiag,9001) caffec, cinflu, srcnam, 'N/A', 'N/A', 
     &                                  ptlong(iptfam), ptname(iptfam)

         goto 9999
      endif
c
c  --- find the integer value for this influencing species ---
c
      iinflu = 0
      do i=1,ngas
         if( cinflu .EQ. spname(i) ) iinflu = i
      enddo
      if( cinflu .EQ. NAMALL ) then
          iinflu = IDALL
      else if( cinflu .EQ. NAMVOC ) then
          iinflu = IDVOC
      else if( cinflu .EQ. NAMNOX ) then
          iinflu = IDNOX
      else if( cinflu .EQ. NAMHRV ) then
          iinflu = IDHRV
      else if( iinflu .EQ. 0 ) then
          goto 7000
      endif
c
c  --- if there are groups loop over them --- 
c
      if( ngrps .GT. 0 .OR. nregs .GT. 0 ) then
         do ireg = 1,nregs
            do igrp = 1,ngrps
c
c  --- incrament the counter for position of species ---
c
                iptfam = iptfam + 1
c
c  --- set the flag for outputing this species to average file ---
c
                loutsa(iptfam) = lout
c
c  --- fill in the parts that depend on region ---
c
                lsamap(iptfam) = iptfam
                ptlong(iptfam)(1:4) = caffec(1:4)
                ptlong(iptfam)(5:6) = srcnam(1:2)
                write(ptlong(iptfam)(7:8),'(I2.2)') igrp
                write(ptlong(iptfam)(9:10),'(I2.2)') ireg
                ptlong(iptfam)(11:14) = cinflu(1:4)
                write(ptname(iptfam)(1:2),'(I2.2)') iaffec
                ptname(iptfam)(3:3) = srcnam(1:1)
                write(ptname(iptfam)(4:5),'(I2.2)') igrp
                write(ptname(iptfam)(6:7),'(I2.2)') ireg
                if( iinflu .GT. 0 ) then
                    write(ptname(iptfam)(8:10),'(I3.3)') iinflu
                else
                    ptname(iptfam)(8:10) = cinflu(1:3)
                endif
c
c  --- replace all blanks with underscores ---
c
                do i=1,14
                   if( ptlong(iptfam)(i:i) .EQ. ' ' ) 
     &                                   ptlong(iptfam)(i:i) = '_'
                enddo
                do i=1,10
                   if( ptname(iptfam)(i:i) .EQ. ' ' ) 
     &                                   ptname(iptfam)(i:i) = '_'
                enddo
c
c  --- echo the information to the output file ---
c
                write(idiag,9000) caffec, cinflu, srcnam, igrp, ireg, 
     &                                  ptlong(iptfam), ptname(iptfam)
            enddo
         enddo
c
c   --- if there are no groups or regions treat it differently ---
c
      else
         iptfam = iptfam + 1 
         lsamap(iptfam) = iptfam
         ptlong(iptfam)(1:4) = caffec(1:4)
         ptlong(iptfam)(5:10) = srcnam(1:5)
         ptlong(iptfam)(11:14) = cinflu(1:4)
         write(ptname(iptfam)(1:2),'(I2.2)') iaffec
         ptname(iptfam)(3:3) = srcnam(1:1)
         ptname(iptfam)(4:7) = srcnam(3:6)
         if( iinflu .GT. 0 ) then
             write(ptname(iptfam)(8:10),'(I3.3)') iinflu
         else
             ptname(iptfam)(8:10) = cinflu(1:3)
         endif
c
c  --- set the flag for outputing this species to average file ---
c
         loutsa(iptfam) = lout
c
c  --- replace all blanks with underscores ---
c
         do i=1,14
            if( ptlong(iptfam)(i:i) .EQ. ' ' ) 
     &                                   ptlong(iptfam)(i:i) = '_'
         enddo
         do i=1,10
            if( ptname(iptfam)(i:i) .EQ. ' ' ) 
     &                                   ptname(iptfam)(i:i) = '_'
         enddo
         write(idiag,9001) caffec, cinflu, srcnam, 'N/A', 'N/A', 
     &                                  ptlong(iptfam), ptname(iptfam)
      endif
      goto 9999
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format(1X,A,3X,A,4X,A,I5,2X,I5,4X,A,2X,A)
 9001 format(1X,A,3X,A,4X,A,A5,2X,A5,4X,A,2X,A)
 9002 format(1X,A,3X,A,1X,A1,1X,A,17X,A,2X,A)
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue 
      write(iout,'(//,A)') 'ERROR in FILSPDDM:' 
      write(iout,'(/,1X,3A,/,2A)') 
     &          'ERROR: The species name: /',cinflu,'/',
     &          ' was used as a DDM species name but does not match',
     &          ' any modeled species.'
      call camxerr()
 7001 continue
      write(iout,'(//,A)') 'ERROR in FILSPDDM:'
      write(iout,'(/,1X,2A,/,5X,A,/,5X,A)')
     &           'ERROR: HDDM requires both of the following 1st order',
     &           ' sensitivity parameters:',
     &           hddmsp(1,ngrps),hddmsp(2,ngrps)
      call camxerr()
c
 7002 continue
      write(iout,'(//,A)') 'ERROR in FILSPDDM:'
      write(iout,'(/,1X,A,I2,A,I5)')
     &           'ERROR: Rate_Const_Groups(',ngrps,
     &           ') includes out-of-range rxn index - ',iprate(i,ngrps)
      call camxerr()
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

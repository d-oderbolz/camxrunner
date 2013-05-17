      subroutine readpht
      use filunit
      use chmstry
      use o3colmap
      implicit none
c
c----CAMx v6.00 130506
c
c     RDPHOT reads photolysis reaction rates as a function
c     of NZEN zenith angles, NHGHT altitudes above ground, NTRN terrain
c     height classes, NALB surface albedo classes, and NOZN ozone column
c     classes. Dimensions for all five of these parameters are set in the 
c     Inc/CAMx.prm file.
c
c     Copyright 1996 - 2013
c     ENVIRON International Corporation
c
c     Modifications:
c        11/30/04      Revised to read altitude from file, instead
c                      of relying on data statement of heights
c        04/02/12      Removed RADM cloud adjustment option, cloud/aerosol
c                      adjustments now always done with in-line TUV; AHO
c                      file is now just ozone column; replaced haze
c                      dimension with terrain height in photo file
c
c     Input arguments:
c        none
c
c     Output arguments:
c        none
c
c     Routines called:
c        none
c
c     Called by:
c        STARTUP
c
      include 'camx.prm'
      include 'flags.inc'
c
      character*80 record
      character*12 tuvlabl
      real         tmp3(3)
      integer      line, iozn, ialb, itrn, kount
      integer      i, k, k1, ihght, irxn, izen
c
      data tuvlabl /'TUV4.8CAMx6 '/
c
c-----Entry Point
c
c-----Read photolysis rates for the primary photolysis reactions
c     declared in the CHEMPARAM file.
c       NPHOT1 is the number of primary photolysis reactions
c
c     The zenith angles are set in CHEMDAT
c
      line = 1
      read(iphot,'(a)',err=7000,end=7001) record
      if (record(1:12).ne.tuvlabl) goto 7004

      do iozn = 1,NOZN
        do ialb = 1,NALB
          do itrn = 1,NTRN
            line = line + 1
            read(iphot,'(a)',err=7000,end=7001) record
            kount = 0
            k1 = 1
            do 10 k=k1,80
              if (kount.eq.3) goto 11
              if(record(k:k).eq.'=') then
                kount = kount + 1
                k1 = k + 1
                read(record(k1:80),*,err=7002) tmp3(kount)
                goto 10
              endif                
  10        continue
  11        continue
            if (kount.ne.3) goto 7003
            ozcl(iozn) = tmp3(1)
            albcl(ialb) = tmp3(2)
            trncl(itrn) = tmp3(3)
c
            do ihght = 1,NHGHT
              line = line + 1
              read(iphot,*,err=7000,end=7001) htint(ihght)
              do irxn = 1,nphot1
                line = line + 1
                read(iphot,'(1x,10f12.0)',err=7000,end=7001) 
     &              (prkn(izen,irxn,ihght,itrn,ialb,iozn),izen=1,NZEN)
              enddo
            enddo
          enddo
        enddo
      enddo
c
c-----Echo photolysis dimensions
c
      write(idiag,'(/,a)') 'Completed reading Photolysis file'
      write(idiag,'(a15,99f7.3)') 'Ozone classes :',(ozcl(i),i=1,NOZN)
      write(idiag,'(a15,99f7.3)') 'Albedo classes:',(albcl(i),i=1,NALB)
      write(idiag,'(a15,99f7.3)') 'Trn Ht classes:',(trncl(i),i=1,NTRN)
      write(idiag,'(a15,99f7.3)') 'Altitudes (km):',(htint(i),i=1,NHGHT)
      write(idiag,'(a15,99f7.3)') 'Zenith angles :',(zenint(i),i=1,NZEN)
      write(idiag,'(//)')
c
c-----Convert rates from per min to per hour
c
      do iozn = 1,NOZN
        do ialb = 1,NALB
          do itrn = 1,NTRN
            do ihght = 1,NHGHT
              do irxn = 1,nphot1
                do izen=1,NZEN
                  prkn(izen,irxn,ihght,itrn,ialb,iozn) =
     &            prkn(izen,irxn,ihght,itrn,ialb,iozn)*60.
                enddo
              enddo
            enddo
          enddo
        enddo
      enddo
c
      close(iphot)
      goto 9999
c
c----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,A)') 'ERROR in READPHT:'
      write(iout,'(/,2A,i5)') ' ERROR: Reading photolysis rates',
     &                  ' file at line: ', line
      write(iout,'(/,2A)') ' Does the rate file have the correct',
     &                 ' number of reactions?'
      call camxerr()
c
 7001 continue
      write(iout,'(//,A)') 'ERROR in READPHT:'
      write(iout,'(/,A,/,A,i5)') ' ERROR: Reading photolysis rates',
     &                  ' End of file at line: ', line
      call camxerr()
c
 7002 continue
      write(iout,'(//,A)') 'ERROR in READPHT:'
      write(iout,'(/,A,/,A,i5,/,A)') 
     &            ' ERROR: Reading photolysis rates',
     &                  ' Reading header record at line: ', line,
     &                       record
      call camxerr()
c
 7003 continue
      write(iout,'(//,A)') 'ERROR in READPHT:'
      write(iout,'(/,A,/,A,i5,/,A)') 
     &            ' ERROR: Reading photolysis rates',
     &                  ' Reading header record at line: ', line,
     &                       record
      write(iout,*)'Did not find three = signs in header. Found ',kount
      call camxerr()
c
 7004 continue
      write(iout,'(//,A)') 'ERROR in READPHT:'
      write(iout,'(/,A,/,A,i5,/,A)') 
     &            ' ERROR: Reading photolysis rates',
     &                  ' Reading header record at line: ', line
      write(iout,'(/,a)') ' Photolysis label record is INVALID'
      write(iout,'(a,a)') ' Expecting: ',tuvlabl
      write(iout,'(a,a)') '     Found: ',record(1:12)
      write(iout,'(2a,/,a)') ' Make sure you are uusing the correct ',
     &                      'version of the TUV processor for this',
     &                       ' version of CAMx.'
      call camxerr()
c
c----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

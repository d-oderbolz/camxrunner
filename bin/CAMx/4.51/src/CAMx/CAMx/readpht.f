      subroutine readpht
c
c----CAMx v4.51 080522
c
c     RDPHOT reads photolysis reaction rates as a function
c     of NZEN zenith angles, NHGHT altitudes above ground, NHAZE turbidity
c     classes, NALB surface albedo classes, and NOZN ozone column classes.
c     Dimensions for all five of these parameters are set in the 
c     Inc/CAMx.prm file.
c
c     Copyright 1996-2008
c     ENVIRON International Corporation
c
c     Modifications:
c        113004              Revised to read altitude from file, instead
c                            of relying on data statement of heights
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
      include 'chmstry.com'
      include 'filunit.com'
      include 'ahomap.com'
c
      character*80 record
      dimension tmp3(3)
c
c-----Entry Point
c
c-----Read photolysis rates for the primary photolysis reactions
c     declared in the CHEMPARAM file.
c       NPHOT1 is the number of primary photolysis reactions
c
c     The reference heights and zenith angles are set in CHEMDAT
c
      line = 0
      do iozn = 1,NOZN
        do ialb = 1,NALB
          do ihaze = 1,NHAZE
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
            hazcl(ihaze) = tmp3(3)
c
            do ihght = 1,NHGHT
              line = line + 1
              read(iphot,*,err=7000,end=7001) htint(ihght)
              do irxn = 1,nphot1
c               write(iout,*) iozn,ialb,ihaze,ihght,irxn
                line = line + 1
                read(iphot,'(1x,10f12.0)',err=7000,end=7001) 
     &              (prkn(izen,irxn,ihght,ihaze,ialb,iozn),izen=1,NZEN)
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
      write(idiag,'(a15,99f7.3)') 'Haze classes  :',(hazcl(i),i=1,NHAZE)
      write(idiag,'(a15,99f7.3)') 'Altitudes (km):',(htint(i),i=1,NHGHT)
      write(idiag,'(a15,99f7.3)') 'Zenith angles :',(zenint(i),i=1,NZEN)
      write(idiag,'(//)')
c
c-----Convert rates from per min to per hour
c
      do iozn = 1,NOZN
        do ialb = 1,NALB
          do ihaze = 1,NHAZE
            do ihght = 1,NHGHT
              do irxn = 1,nphot1
                do izen=1,NZEN
                  prkn(izen,irxn,ihght,ihaze,ialb,iozn) =
     &            prkn(izen,irxn,ihght,ihaze,ialb,iozn)*60.
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
      write(iout,*)'Did not find three = signs in header.'
      call camxerr()
c
c----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

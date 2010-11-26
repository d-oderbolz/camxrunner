      program psat_combine

c     This program reads a PSAT receptor file and combines the output into 24 
c     hour averages.  
c     Program includes the ability to shift data to local time (if needed)
c     This program also merges multiple receptor files of differing species for 
c     the same time period.
c
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
c Copyright (C) 2009  ENVIRON 
c  
c This program is free software; you can redistribute it and/or 
c modify it under the terms of the GNU General Public License 
c as published by the Free Software Foundation; either version 2 
c of the License, or (at your option) any later version. 
c  
c This program is distributed in the hope that it will be useful, 
c but WITHOUT ANY WARRANTY; without even the implied warranty of 
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the 
c GNU General Public License for more details. 
c!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

      parameter (mxrec = 15, mxspec = 32, mxfmrg = 3, mxtrac = 100)

      character*1000 linell
      character*65 line,line1
      character*120 infile,outfile
      real conc(mxrec,mxspec,mxfmrg,mxtrac),
     +  avg(mxrec,mxspec,mxfmrg,mxtrac)

      logical lpartial
      integer nspec(mxfmrg)
      integer ntrac(mxfmrg),ntracsp(mxspec,mxfmrg)
      character*3 spec(mxspec,mxfmrg)

c     READ INPUT/OUTPUTS
      read (*,'(20x,a)') outfile
      open (20, file=outfile,status='unknown')
      print *, 'creating output file: ', outfile

      read (*,'(20x,i10)') ishift
      print *, 'shifting time stamp by ', ishift, ' hours'

      read (*,'(20x,i10)') juldate
      print *, 'Generate 24-hour average for ', juldate

      read (*,'(20x,l10)') lpartial
      if (lpartial) then
        print *, 'Allow non-24 hour averages to be computed if not ',
     +    'all hours are available'
      endif

      read (*,'(20x,i10)') nftim
      print *, 'Number of receptor files to concatenate ', nftim

      read (*,'(20x,i10)') nfmrg
      print *, 'Number of input receptor files to merge ', nfmrg
       
c     MAIN LOOP TO READ IN FILES
      do iftim=1,nftim
c       LOOP TO READ IN FILES OF THE SAME TIME, BUT WITH DIFFERENT SPECIES
        do k=1,nfmrg
          read (*,'(20x,a)') infile 
          open (10+k,file=infile,status='old')
          print *, 'File ', k, ' to merge: ', infile

c         READ/WRITE HEADER 
          read (10+k,'(a)') linell
          if (iftim .eq. 1 .and. k .eq. 1) write (20,'(a)') 
     +      linell(1:167)

          do nl = 1,9999
            read (10+k,'(a)') line
            if (line(1:14) .eq. ' File Duration') then
              read (line(20:),*) jbeg,hrbeg,jend,hrend

c             CHECK TO MAKE SURE TIMES MATCH          
              if (k .eq. 1) then
                jbegin = jbeg
                jendin = jend
                hrbegin = hrbeg
                hrendin = hrend
                if (iftim .eq. 1) then
                  write (20,'(a,2(i10.5,a1,f10.2,a1)') line(1:17),
     +              juldate, ',', 0.0, ',', juldate, ',', 24.0, ','
                endif
              else
                if (jbeg .ne. jbegin .or. jend .ne. jendin .or.
     +            hrbegin .ne. hrbeg .or. hrendin .ne. hrend) then
                  print *, 'Times of the files to be merged together ',
     +              ' do not match'
                  stop
                endif
              endif
            elseif (line(1:16) .eq. 'Average Interval') then
              read (line(19:),*) timint
              if (k .eq. 1 .and. iftim .eq. 1) then
                write (20,'(a,f10.2)') line(1:20),24.
                timint1 = timint
              else
                if (timintin .ne. timin) then
                  print *, 'time intervals between files do not match'
                  stop
                endif
              endif
              goto 90
            else
              if (k .eq. 1 .and .iftim .eq. 1) write (20,'(a)') line
            endif
          enddo
90        continue
        enddo

c       READ/WRITE NUMBER OF SOURCES, GROUPS, ETC.
        do k=1,nfmrg
          do nl = 1,999
            read (10+k,'(a)')line
            if (k .eq. 1 .and .iftim .eq. 1) write (20,'(a)')line(1:40)

            if (line(1:16) .eq. 'Number of timing') then
              if (k .eq. 1) then
                read (line(31:),*) ntimper1
              else
                read (line(31:),*) ntimper
                if (ntimper1 .ne. ntimper) then
                  print *, 'number of timing periods do not match'
                  stop
                endif
              endif
            elseif (line(1:16) .eq. 'Number of source') then
              if (k .eq. 1) then
                read (line(31:),*) nsrc1
              else
                read (line(31:),*) nsrc
                if (nsrc1 .ne. nsrc) then
                  print *, 'number of source areas do not match'
                  stop
                endif
              endif
            elseif (line(1:28) .eq. 'Number of emission groupings') then
              if (k .eq. 1) then
                read (line(31:),*) nemis1
              else
                read (line(31:),*) nemis
                if (nemis1 .ne. nemis) then
                  print *, 'number of emission groups do not match'
                  stop
                endif
              endif
              goto 100
            endif
          enddo
100       continue
        enddo

c       READ IN NUMBER OF TRACER SPECIES 
        do k=1,nfmrg
          read (10+k,'(a)') line
          if (line(1:24) .eq. 'Number of tracer species') then
            read (line(31:),*) ntrac(k)
            itottrac = itottrac + ntrac(k)
          else
            print *, 'program should be reading the # tracers'
            print *, 'instead, it is reading : ', line
            stop
          endif
        enddo
        if (iftim .eq.1 ) write (20,'(a30,i10)') line(1:30),itottrac

c       READ/WRITE NUMBER OF TRACERS FOR INDIVIDUAL SPECIES
        do k=1,nfmrg
          icnt = 0
          nspec(k) = 0
          do is = 1,mxspec
            read (10+k,'(a)') line
              read (line(11:13),'(a3)') spec(is,k)
              read (line(31:),*) ntracsp(is,k)
              maxch = max0(maxch,ntracsp(is,k)*10)
              maxch2 = max0(maxch,ntracsp(is,k)*11)
              if (maxch .gt. 1000) then
                print *, ' '
                print *, 'INCREASE THE CHARACTER LENGTH OF LINELL'
	        print *, 'to ', maxch2
                stop
              endif
              nspec(k) = nspec(k) + 1
              if (iftim .eq. 1) write (20,'(a)') line(1:40)
              icnt = icnt + ntracsp(is,k)
              if (icnt .eq. ntrac(k) ) goto 110
          enddo

c         IF THIS IS REACHED, CHECK NUMBER OF TRACERS!
          print *, 'number of tracers do not add up to the total'
          stop

110       continue
        enddo

        do k=1,nfmrg
          do nl = 1, 999
            read (10+k,'(a)') line
            if (k .eq. 1 .and. iftim .eq. 1) write (20,'(a)') line(1:40)
            if (line(1:12) .eq. 'Tracer Names') goto 120
          enddo
120       continue
        enddo

c       READ/WRITE TRACER NAMES
        do k=1,nfmrg
          do isp = 1,nspec(k)
            read (10+k,'(a)') linell
            if (linell(1:3) .ne. spec(isp,k)) then
              print *, 'Tracer name, ', linell(1:3), ' does not match', 
     +          ' the species name ', spec(isp,k), isp,k
              stop
            endif
            if (iftim .eq. 1) write (20,'(a)') linell(1:maxch)
          enddo             
        enddo             

c       READ/WRITE RECEPTOR LOCATIONS
        do nl = 1,999
          do k=1,nfmrg
            read (10+k,'(a)') line
         
            if (k .eq. 1 .and .iftim .eq. 1) then
              write (20,'(a)') line(1:52)
            endif

            if (line(1:20) .eq. 'Number of receptors ') then
              if (k .eq. 1) then
                read (line(22:),*) nrec1
              else
                read (line(22:),*) nrec
                if (nrec1 .ne. nrec) then
                  print *, 'Number of receptors do not match'
                  stop
                endif
              endif
            endif

            if (k .eq. 1) then
              line1 = line
            else
              if (line1(1:20) .ne. line(1:20)) then
                print *, 'Receptor parameters do not match for ',
     +            'receptor', ir
                stop
              endif
            endif
              
            if (line(1:24) .eq. 'Time Varying Tracer Data' .and. 
     +        k .eq. nfmrg) goto 150
          enddo
        enddo
150     continue
 
                
c       MAIN LOOP OF CONC DATA
        do ihr = 1,99
          do k=1,nfmrg
c           READ TIME STAMP
            read (10+k,*,end=800)
	    if (k .eq. 1 .and. ihr .eq. 1 .and. iftim .eq. 1) 
     +        write (20,*)

            read (10+k,'(a)') line
            if (line(1:15) .ne. 'Data for Period') then
              print *, 'Program isn''t reading in the time stamp',
     +          ' in file ', k
              stop
            endif
           
            if (k .eq. 1 ) then
              read (line(17:),*) jbeg1,hrbeg1,jend1,hrend1
              if (iftim .eq. 1 .and. ihr .eq. 1) 
     +          write (20,'(a16,2(i10.5,a1,f10.2,a1)') 
     +          line(1:16),juldate,',', 0.,',', juldate,',', 24.,','
            else
              read (line(17:),*) jbeg,hrbeg,jend,hrend
              if (jbeg1 .ne. jbeg .or. hrbeg1 .ne. hrbeg
     +          .or. jend1 .ne. jend .or. hrend1 .ne. hrend) then
                print *, ' times do not match'
                stop
              endif
            endif

c           ADJUST TO LOCAL TIME
            if (k .eq. 1) then
              jbegloc = jbeg1
	      jendloc = jend1
              hrbegloc = hrbeg1 + real(ishift)
              hrendloc = hrend1 + real(ishift)
              if (hrbegloc .lt. 0.) then
                hrbegloc = hrbegloc + 24.
                jbegloc  = jbegloc  - 1
              elseif (hrbegloc .ge. 24.) then
                hrbegloc = hrbegloc - 24.
                jbegloc  = jbegloc  + 1
              endif
        
              if (hrendloc .le. 0.) then
                hrendloc = hrendloc + 24.
                jendloc  = jendloc  - 1
              elseif (hrendloc .gt. 24.) then
                hrendloc = hrendloc - 24.
                jendloc  = jendloc  + 1
              endif

	      write(*,'(a,i5.5,1x,i2)') 'processing local date,hour ', 
     +          jbegloc, nint(hrbegloc)
            endif

            do irec = 1,nrec1
              read (10+k,'(a)') line
              read (line(12:),*) ii
              if (ii .ne. irec) then
		print *
                print *, 'receptor number does not agree with counter'
                stop
              endif
            
              do isp = 1,nspec(k)
                read (10+k,'(a)') linell
                if (jbegloc .eq. juldate ) then
                  read (linell,*) 
     +              (conc(irec,isp,k,itrac),itrac=1,ntracsp(isp,k))

                  do itrac=1,ntracsp(isp,k)
                    avg(irec,isp,k,itrac) = avg(irec,isp,k,itrac) + 
     +                conc(irec,isp,k,itrac)
                  enddo
	       
                  if (isp .eq. 1 .and. k .eq. 1 .and. irec .eq. 1)
     +              sumtim = sumtim + hrendloc-hrbegloc + 
     +                real(jendloc-jbegloc)*24.

                elseif (jbegloc .gt. juldate) then
		  print *
                  print *, 'reading data for date: ', jbegloc
                  print *, 'which is beyond the desired date ', juldate
                  goto 810
                endif
              enddo
            enddo
          enddo

          if (nint(sumtim) .eq. 24) then
            do irec = 1,nrec1
              write (20,'(a,i5,a1)') ' Receptor,', irec, ','
              do k=1,nfmrg
                do isp = 1,nspec(k)
                  do itrac=1,ntracsp(isp,k)
                    avg(irec,isp,k,itrac) =avg(irec,isp,k,itrac)/sumtim
                  enddo
                  write(20,'(100(e10.4,a2))') ((avg(irec,isp,k,itrac),
     +              ', '),itrac=1,ntracsp(isp,k))
                enddo
              enddo
            enddo
            goto 900
          endif

        enddo

800     do k=1,nfmrg
          close(10+k)
        enddo
      enddo
810   continue

c     IF PROGRAM GOES HERE, ALL 24 HOURS WERE NOT FOUND.
      write (*,'(/,i2,a,i5.5,a)') nint(sumtim), ' hours of date ', 
     +  juldate, ' were found. '
      if (lpartial) then
        write (*,'(/,a,i2,a)')  'This date will contain a ', 
     +    nint(sumtim), '-hr average' 

c       COMPUTE NON-24HOUR AVERAGE
        if (nint(sumtim) .eq. 0) then
          print *
          print *, 'no data was available for date: ', juldate
          stop
        else
          do irec = 1,nrec1
            write (20,'(a,i5,a1)') ' Receptor,', irec, ','
            do k=1,nfmrg
              do isp = 1,nspec(k)
                do itrac=1,ntracsp(isp,k)
                  avg(irec,isp,k,itrac) =avg(irec,isp,k,itrac)/sumtim
                enddo
                write(20,'(100(e10.4,a2))') ((avg(irec,isp,k,itrac),
     +            ', '),itrac=1,ntracsp(isp,k))
              enddo
            enddo
          enddo
        endif
      else
        print *
        print *, 'Check dates and times of input files and try again'
        print *, 'or change the flag to output non-24 hour averages'
      endif

900   continue
      stop
      end

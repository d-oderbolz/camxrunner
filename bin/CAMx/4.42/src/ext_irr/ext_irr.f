      program ext_irr
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c Copyright (C) 2001  ENVIRON
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
c
c To obtain a copy of the GNU General Public License
c write to the Free Software Foundation, Inc.,
c 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c-----------------------------------------------------------------------
c    This program reads the output file from a CAMx/IRR run and
c    writes the data to an ASCII output file.  The header data is
c    written for completeness, but the data records are written
c    in a form to be imported into a spreadsheet.
c
c-----------------------------------------------------------------------
c   Local parameters:
c-----------------------------------------------------------------------
c
c    IORIRR  I  unit number of the input IRR file
c    IOWASC  I  unit number of the output ASCII file
c    IOWBIN  I  unit number of the output 
c    MXRXN   I  maximum number of processes in the file
c    MXHOUR  I  maximum number of hours in simulation
c    MXCELL  I  maximum number of cells in modeling grid
c    MXLAY   I  maxumum number of layers
c    MXFILE  I  maximum number of files to read
c    CONFAC  I  conversion factor from micromoles/m^3 to moles/m^3
c
      integer     IORIRR
      integer     IOWASC
      integer     IOWBIN
      integer     MXRXN 
      integer     MXHOUR
      integer     MXCELL
      integer     MXLAY
      integer     MXFILE
      real*8      CONFAC
c
      parameter( IORIRR = 10 )
      parameter( IOWASC = 20 )
      parameter( IOWBIN = 21 )
      parameter( MXRXN  = 156 )
      parameter( MXHOUR = 50 )
      parameter( MXCELL = 100 )
      parameter( MXLAY  = 20 )
      parameter( MXFILE = 99 )
      parameter( CONFAC = 1E-6 )

c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      character*200 fname, infile(MXFILE)
      character*80  runmsg, string
      character*20  keywrd
      character*10  species, ftype
      character*4   inote(60), iftype(10), ispec(10,MXRXN)
      character*3   ounits
      character*1   comma
      integer       irec, ibgdat, iendat, ngrid, nspec
      integer       ncol(100), nrow(100), iutm(100)
      integer       nirmbdom, imbgrd, i_sw, i_ne, j_sw, j_ne
      integer       b_lay, t_lay, nirrrxn, imbdom, imbnst
      integer       imbx, imby, imbz, idomext, idatcnt, idxspc
      integer       jrec, nfiles, ifile, idtlst
      integer       iclbeg, iclend, jclbeg, jclend, kclbeg, kclend
      integer       iutmsim, nhours, i, j, ibegsim, iendsim
      integer       ibegdt, ienddt, ihr, ispc
      integer       nxsim, nysim, instdim, numhrs
      real*4        begtim, endtim
      real*4        orgx(100), orgy(100), xsize(100), ysize(100)
      real*4        cirr(MXRXN), begsim, endsim, xorgsim, yorgsim
      real*4        dxsim, dysim, beghr, endhr
      real*4        abs_error, rel_error, timlst
      real*4        grdval(MXCELL,MXCELL,MXLAY,MXRXN)
      logical       lexist, lascii, lbinary
c
c-----------------------------------------------------------------------
c   Data statements:
c-----------------------------------------------------------------------
c
      data comma /','/
      data ftype /'AVERAGE'/
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   --- get the filenames from the user and open the files ---
c
      nfiles = 0
 111  continue      
      write(*,'(1X,A)') 'Enter the name of the input file: '
      write(*,'(10X,A)') '/END/ means finished with input files.'
      read(*,'(20X,A)') fname
      keywrd = fname(1:20)
      call jstlft( keywrd )
      call toupper( keywrd )
      if( keywrd(1:5) .NE. '/END/' ) then
         call jstlft( fname )
         inquire(file=fname,exist=lexist)
         if( .NOT. lexist ) goto 7000
         nfiles = nfiles + 1
         if( nfiles .GT. MXFILE ) goto 7010
         infile(nfiles) = fname
         goto 111
      endif
c
      write(*,'(1X,A)') 'Enter the name of the ASCII output file: '
      read(*,'(20X,A)') fname
      call jstlft( fname )
      if( fname .NE. ' ' ) then
         open(unit=IOWASC,file=fname,status='UNKNOWN',ERR=7002)
         lascii = .TRUE.
      else
         lascii = .FALSE.
      endif
c
      write(*,'(1X,A)') 'Enter the name of the Binary output file: '
      read(*,'(20X,A)') fname
      call jstlft( fname )
      if( fname .NE. ' ' ) then
         open(unit=IOWBIN,file=fname,status='UNKNOWN',
     &                              form='UNFORMATTED',ERR=7002)
         lbinary = .TRUE.
      else
         lbinary = .FALSE.
      endif
c
      write(*,'(1X,A)') 'Enter the sub-domain to extract: '
      read(*,'(20X,A)') string
      read(string,*) idomext
c
      write(*,'(1X,A)') 'Enter the X-range (beginning and ending):'
      read(*,'(20X,A)') string
      read(string,*) iclbeg, iclend
c
      write(*,'(1X,A)') 'Enter the Y-range (beginning and ending):'
      read(*,'(20X,A)') string
      read(string,*) jclbeg, jclend
c
      write(*,'(1X,A)') 'Enter the Z-range (beginning and ending):'
      read(*,'(20X,A)') string
      read(string,*) kclbeg, kclend
c
      if( lbinary ) then
        write(*,'(1X,A)') 'Enter the total number of hours to process: '
        read(*,'(20X,A)') string
        read(string,*) numhrs
      endif
c
c  --- read and write the header information ---
c
      irec = 0
      idtlst = 0
      timlst = 0.0
      idatcnt = 0
      nhours = 0
c
c  --- the simulation message ---
c
      do ifile = 1,nfiles
         open(unit=IORIRR,file=infile(ifile),form='UNFORMATTED',
     &                                  status='UNKNOWN',ERR=7001)
         irec = 1
         read(IORIRR,ERR=7003) runmsg
         if( ifile .EQ. 1 ) then
             jrec = 1
             if( lascii ) then
                write(IOWASC,'(3A)',ERR=7004)
     &               'CAMX IRR data extraction by EXT_IRR',comma, runmsg
                write(IOWASC,'(a)',err=7004)
     &               'The header from the first CAMx IRR file follows: '
             endif
         endif
c
c   --- the simulation date/time ---
c
         irec = irec + 1
         read(IORIRR,ERR=7003) ibgdat,begtim,iendat,endtim 
         if( ifile .EQ. 1 ) then
            jrec = jrec + 1
            if( lascii ) write(IOWASC,'(2(I10,A,F10.2,A))',ERR=7004) 
     &                  ibgdat,comma,begtim,comma,iendat,comma,endtim
            ibegsim = ibgdat
            begsim = begtim
         endif
         iendsim = iendat
         endsim = endtim
c
c   --- calculate the date and time for binary file ---
c
         if( lbinary ) then
            ibegdt = ibegsim
            beghr = begsim
            ienddt = MAX( ibegdt,ibegdt+INT(numhrs/24) )
            endhr = MOD(numhrs,24)
         endif
c
c   --- the grid information ---
c
         irec = irec + 1
         read(IORIRR,ERR=7003) ngrid
         if( ifile .EQ. 1 ) then
            jrec = jrec + 1
            if( lascii ) write(IOWASC,'(2A,I10)',ERR=7004) 
     &                          'Number of grids: ',comma,ngrid
            jrec = jrec + 1
            if( lascii ) write(IOWASC,'(11A)',ERR=7004) 
     &           'x-origin',comma,'y-origin',comma,'# cols',comma,
     &                     '# rows',comma,'x-width',comma,'y-width'
         endif
         do i=1,ngrid
           irec = irec + 1
           read(IORIRR,ERR=7003) orgx(i),orgy(i),ncol(i),nrow(i),
     &                                       xsize(i),ysize(i),iutm(i)
           if( ifile .EQ. 1 ) then
               jrec = jrec + 1
               if( lascii ) write(IOWASC,
     &              '(2(F10.4,A),2(I10,A),2(F10.4,A),I10)',ERR=7004)
     &                  orgx(i),comma,orgy(i),comma,ncol(i),comma,
     &                          nrow(i),comma,xsize(i),
     &                                     comma,ysize(i),comma,iutm(i)
           endif
         enddo
c
c  --- the sub-domain definitions ----
c
         irec = irec + 1
         read(IORIRR,ERR=7003) nirmbdom
c
c   --- check to make sure the extraction sub-domain is in the file ---
c
         if( idomext .LE. 0 .OR. idomext .GT. nirmbdom ) goto 7008
         if( ifile .EQ. 1 ) then
            jrec = jrec + 1
            if( lascii ) write(IOWASC,'(2A,I10)',ERR=7004) 
     &                      'Number of sub-domains: ',comma,nirmbdom
            jrec = jrec + 1
            if( lascii ) write(IOWASC,'(13A)',ERR=7004) 
     &               'grid #',comma,'I-begin',comma,'I-end',comma,
     &                     'J-begin',comma,'J-end',comma,
     &                                'Layer-begin',comma,'Layer-end'
         endif
         do i=1,nirmbdom
            irec = irec + 1
            read(IORIRR,ERR=7003) imbgrd, i_sw, i_ne, j_sw, j_ne, 
     &                                                 b_lay, t_lay
            if( ifile .EQ. 1 ) then
              jrec = jrec + 1
              if( lascii ) write(IOWASC,'(7(I10,A))',ERR=7004) 
     &          imbgrd, comma, i_sw,comma, i_ne, comma, j_sw, 
     &                        comma, j_ne,comma, b_lay, comma, t_lay
              if( i .EQ. idomext ) then
                  xorgsim = orgx(imbgrd)
                  yorgsim = orgy(imbgrd)
                  dxsim = xsize(imbgrd)
                  dysim = ysize(imbgrd)
                  nxsim = ncol(imbgrd)
                  nysim = nrow(imbgrd)
                  iutmsim = iutm(imbgrd)
                  if( iutmsim .NE. 0 ) then
                     xorgsim = xorgsim * 1000.0
                     yorgsim = yorgsim * 1000.0
                     dxsim = dxsim * 1000.0
                     dysim = dysim * 1000.0
                  endif
c
c  --- reset the extraction grid range so it makes sense 
c      with this sub-domain ---
c
                 if( iclbeg .LT. i_sw  ) iclbeg = i_sw
                 if( iclend .GT. i_ne  ) iclend = i_ne
                 if( jclbeg .LT. j_sw  ) jclbeg = j_sw
                 if( jclend .GT. j_ne  ) jclend = j_ne
                 if( kclbeg .LT. b_lay ) kclbeg = b_lay
                 if( kclend .GT. t_lay ) kclend = t_lay
c
c   --- recalculate the domain definition for the extraction region ---
c
                 if( lbinary ) then
                    xorgsim = xorgsim + FLOAT(iclbeg-1) * dxsim
                    yorgsim = yorgsim + FLOAT(jclbeg-1) * dysim
                    nxsim = (iclend - iclbeg) + 1
                    nysim = (jclend - jclbeg) + 1
                    if( nxsim .GT. MXCELL .OR. 
     &                             nysim .GT. MXCELL ) goto 7015
                    nlaysim = (kclend - kclbeg) + 1
                    if( nlaysim .GT. MXLAY ) goto 7016
                 endif
c
c  --- check for invalid domain specifications ---
c
                 if( iclbeg .GT. i_ne .OR. jclbeg .GT. j_ne 
     &            .OR. kclbeg .GT. t_lay .OR. iclend .LT. i_sw .OR. 
     &               jclend .LT. j_sw .OR. kclend .LT. b_lay ) goto 7011
              endif
            endif
         enddo
c
c  --- number of processes ---
c
         irec = irec + 1
         read(IORIRR,ERR=7003) nirrrxn
         if( nirrrxn .GT. MXRXN) goto 7005
         if( ifile .EQ. 1 ) then
            jrec = jrec + 1
            if( lascii ) write(IOWASC,'(2A,I10)',ERR=7004) 
     &                       'Number of processes: ',comma,nirrrxn
         endif
c
c  --- read and write the data records ---
c
  222    continue
         irec = irec + 1
         idatcnt = idatcnt + 1
         read(IORIRR,ERR=7006,END=333) iendat, begtim, 
     &          imbdom, imbnst, imbx, imby, imbz, (cirr(i),i=1,nirrrxn)
c
c --- if this is a new hour and not the first record in the file,
c     output the totals ---
c
         if( iendat .NE. idtlst .OR. begtim .NE. timlst ) then
c
c   --- write a message that new hour was found ----
c
            nhours = nhours + 1
            if( lbinary ) then
c
c     --- write the header to the binary file ---
c
               if( nhours .EQ. 1 ) then
                 do i=1,60
                     inote(i) = runmsg(i:i)
                 enddo
                 do i=1,10
                     iftype(i) = ftype(i:i)
                 enddo
                 write(IOWBIN,ERR=7013) iftype, inote, 1, nirrrxn, 
     &                             ibegdt, beghr, ienddt, endhr
                 write(IOWBIN,ERR=7013) 0.0, 0.0, iutmsim, xorgsim, 
     &                          yorgsim, dxsim, dysim, nxsim, nysim, 
     &                                  nlaysim, 1, nlaysim, 0., 0., 0.
                 write(IOWBIN,ERR=7013) 1, 1, nxsim, nysim
                 do i=1,nirrrxn
                   write(species,'(A,I3.3)') 'Rxn_',i
                   read(species,'(10A1)') (ispec(j,i),j=1,10)
                 enddo
                 write(IOWBIN,ERR=7013)((ispec(i,j),i=1,10),j=1,nirrrxn)
               else
                 ienddt = idtlst
                 endhr = timlst/100.0
                 if( endhr .EQ. 0.0 ) then
                    endhr = 24.0
                    ienddt = ienddt - 1
                 endif
                 ibegdt = ienddt
                 beghr = endhr - 1.0
                 if( beghr .LT. 0.0 ) then
                    beghr = 0.0
                    ibegdt = ibegdt - 1
                 endif
                 write(IOWBIN,ERR=7013) ibegdt, beghr, ienddt, endhr
                 write(*,'(A,2(I10,F10.2))') 'Writing binary file: ',
     &                                  ibegdt, beghr, ienddt, endhr
                 do ispc=1,nirrrxn
                    do k=1,nlaysim
                      write(IOWBIN,ERR=7013) 1, (ispec(i,ispc),i=1,10),
     &                       ((grdval(i,j,k,ispc),i=1,nxsim),j=1,nysim)
                    enddo
                 enddo
                 do i=1,nxsim
                   do j=1,nysim
                      do k=1,nlaysim
                        do ispc=1,nirrrxn
                           grdval(i,j,k,ispc) = 0.
                        enddo
                      enddo
                   enddo
                 enddo
               endif
            endif
            write(*,'(1X,A,I6,1X,F5.0)') 'Read: ',iendat, begtim
            idtlst = iendat
            timlst = begtim
         endif
c
c  --- skip if not the extraction domain ----
c
         if( imbdom .NE. idomext ) goto 222
c
c   --- skip if this cell is not in the extraction subgrid ----
c
         if( imbx .LT. iclbeg ) goto  222
         if( imbx .GT. iclend ) goto  222
         if( imby .LT. jclbeg ) goto  222
         if( imby .GT. jclend ) goto  222
         if( imbz .LT. kclbeg ) goto  222
         if( imbz .GT. kclend ) goto  222
c
c   --- write out the data ---
c
         if( lascii ) write(IOWASC,9000,ERR=7007) idtlst, comma, timlst, 
     &                comma, imbdom, comma, imbnst, comma, imbx, comma, 
     &                    imby, comma, imbz, (comma,cirr(i),i=1,nirrrxn)
c
c   --- load into gridded array ---
c
         if( lbinary ) then
            do i=1,nirrrxn
               grdval(imbx-iclbeg+1,imby-jclbeg+1,
     &                                   imbz-kclbeg+1,i) = cirr(i)
            enddo
         endif
         goto 222
c
c   --- entire file read, get the next file ---
c
 333     continue
         close(IORIRR)
      enddo
c
c  --- write last hours worth to binary file ---
c
      ienddt = idtlst
      endhr = timlst/100.0
      if( endhr .EQ. 0.0 ) then
         endhr = 24.0
         ienddt = ienddt - 1
      endif
      ibegdt = ienddt
      beghr = endhr - 1.0
      if( beghr .LT. 0.0 ) then
         beghr = 0.0
         ibegdt = ibegdt - 1      
      endif
      write(IOWBIN,ERR=7013) ibegdt, beghr, ienddt, endhr
      write(*,'(A,2(I10,F10.2))') 'Writing binary file: ',
     &                                  ibegdt, beghr, ienddt, endhr
      do ispc=1,nirrrxn
         do k=1,nlaysim
           write(IOWBIN,ERR=7013) 1, (ispec(i,ispc),i=1,10),
     &                             ((grdval(i,j,k,ispc),i=1,nxsim),
     &                                                    j=1,nysim)
         enddo
      enddo
c
      close(IOWASC)
      close(IOWBIN)
      write(*,*) ' Successful completion.'
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(1X,2A)') 'ERROR:  Input file does not exist: ',fname
      goto 9999
c
 7001 continue
      write(*,'(1X,2A)') 'ERROR:  Cannot open input file: ',fname
      goto 9999
c
 7002 continue
      write(*,'(1X,2A)') 'ERROR:  Cannot open output file: ',fname
      goto 9999
c
 7003 continue
      write(*,'(1X,2A,I5)') 'ERROR:  Reading header of IRR file ',
     &                                            'at record: ',irec
      goto 9999
c
 7004 continue
      write(*,'(1X,2A,I5)') 'ERROR:  Writing header of ASCII file.'
      goto 9999
c
 7005 continue
      write(*,'(1X,A,/,10X,A)') 
     &                 'ERROR:  Number of IRR reactions exceeds max.',
     &                                     'Increase parameter MXRXN.'
      goto 9999
c
 7006 continue
      write(*,'(1X,2A,I8)') 'ERROR:  Reading data record of IRR file ',
     &                                            'at record: ',irec
      goto 9999
c
 7007 continue
      write(*,'(1X,2A,I8)') 
     &             'ERROR:  Writing data record of ASCII file ',
     &                                            'at record: ',jrec
      goto 9999
c
 7008 continue
      write(*,'(1X,2A,I2)') 'ERROR:  Sub-domain requested for ',
     &                    'extraction is not in the file: ',idomext
      goto 9999
c
 7010 continue
      write(*,'(1X,A,/,10X,A)') 
     &                 'ERROR:  Number of input files exceeds max.',
     &                                     'Increase parameter MXFILE.'
      goto 9999
c
 7011 continue
      write(*,'(1X,A)') 'ERROR: Invalid extraction domain specified. '
      write(*,'(10X,A)') 'Please specify a domain within the following.'
      write(*,'(20X,A,2I10)') 'I-cell',i_sw,i_ne
      write(*,'(20X,A,2I10)') 'J-cell',j_sw,j_ne
      write(*,'(20X,A,2I10)') 'K-cell',b_lay,t_lay
      goto 9999
c
 7013 continue
      write(*,'(1X,2A)') 'ERROR: Writing the header to the binary ',
     &             'output file.'     
      goto 9999
c
 7014 continue
      write(*,'(1X,A,/,10X,A)') 
     &        'ERROR:  Number of hours in the simulation exceeds max.',
     &                                     'Increase parameter MXHOURS.'
      goto 9999
c
 7015 continue
      write(*,'(1X,A)') 
     &      'ERROR:  Number of cells in extraction region exceeds max.'
      write(*,'(10X,A)') 'Increase parameter MXCELL.'
      write(*,'(20X,2(A,I3))') 'I-cell: ',nxsim,'   Max: ',MXCELL
      write(*,'(20X,2(A,I3))') 'J-cell: ',nysim,'   Max: ',MXCELL
      goto 9999
c
 7016 continue
      write(*,'(1X,A)') 
     &      'ERROR:  Number of lyaers in extraction region exceeds max.'
      write(*,'(10X,A)') 'Increase parameter MXLAY.'
      write(*,'(20X,2(A,I3))') 'Layers: ',nlaysim,'   Max: ',MXLAY
      goto 9999
c
c-----------------------------------------------------------------------
c   Format statements:
c-----------------------------------------------------------------------
c
 9000 format(I10,A,F10.2,5(A,I10),500(A,E15.8))
c
c-----------------------------------------------------------------------
c   Exist point:
c-----------------------------------------------------------------------
c
 9999 continue
      end
      subroutine jstlft( string )
c
c-----CAMx v2.03 991230
c
c
c-----------------------------------------------------------------------
c
c   Description:
c     Left justifies a string
c
c   Arguments:
c     Inputs/Outputs: (the string arguments serves as both input and
c                      output)
c       string   C   string to left justify
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c     08/10/98  -gmw-  original development
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      character*(*) string
c
c-----------------------------------------------------------------------
c   Local variables:
c-----------------------------------------------------------------------
c
      integer ibeg, i
c
c-----------------------------------------------------------------------
c   Entry point:
c-----------------------------------------------------------------------
c
c   ---- it may already be left-justified ---
c
      if( string(1:1) .NE. ' ' ) goto 9999
c
c   ---- find the first non-blank character ---
c
      do 10 i=1,LEN( string )
         if( string(i:i) .NE. ' ' ) then
             ibeg = i
             goto 111
         endif
   10 continue
c
c   --- no non-blanks found, it's a blank string, nothing to do ----
c
      goto 9999
c
c   ---- move the string over, 2 char at a time ---
c
  111 continue
      do 20 i=1,LEN( string )-ibeg+1
         string(i:i) = string(i+ibeg-1:i+ibeg-1)
         string(i+ibeg-1:i+ibeg-1) = ' '
   20 continue
      goto 9999
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
c**** TOUPPER
c
      subroutine toupper(string)
c
c-----CAMx v2.03 991230
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c      Converts the string to upper case.
c
c     Copyright 1996, 1997, 1998, 1999 ENVIRON International Corporation 
c
c       Argument description:
c         Inputs:
c           string  C  string to convert
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     08/16/98   --gwilson--    Original development
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
c    Argument declaration:
c-----------------------------------------------------------------------
c
      character*(*) string
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*26 lower, upper
      integer idx, i
c
c-----------------------------------------------------------------------
c    Data statments:
c-----------------------------------------------------------------------
c
      data lower /'abcdefghijklmnopqrstuvwxyz'/
      data upper /'ABCDEFGHIJKLMNOPQRSTUVWXYZ'/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      do i=1,LEN( string )
         idx = INDEX( lower, string(i:i) )
         if( idx .GT. 0 ) string(i:i) = upper(idx:idx)
      enddo
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
      return
      end
      subroutine bswap (instring,num1,num2)
c
c-----CAMx v2.03 991230
c
c     BSWAP converts big-endian character strings stored as integers
c     to little endian character strings stored as integers
c
c     Copyright 1996, 1997, 1998, 1999 ENVIRON International Corporation
c
c     Modifications:
c        none
c
c     Input arguments:
c        instring            integer string array to be converted 
c        num1                number of elements in array along 1st dimension
c        num2                number of elements along second dimension
c
c     Output arguments:
c        instring            converted integer string array 
c
      logical*1 instring(*), tmp1, tmp2
      integer   num1,num2
      integer   i,j,k
c
c-----Entry point
c
c-----Process in row major order
c
      if (num2 .ne. 0) goto 200
c
c-----Single dimension array
c
      do 150 i = 1,num1
        j = (i-1)*4+1
        tmp1 = instring(j)
        instring(j) = instring(j+3)
        instring(j+3) = tmp1
        tmp2 = instring(j+1)
        instring(j+1) = instring(j+2)
        instring(j+2) = tmp2
 150  continue

      goto 300
c
c-----Double dimension array
c
 200  continue
      do 250 i = 1,num2
        do 240 j = 1,num1
          k = (i-1)*(num1*4)+(j-1)*4+1
          tmp1 = instring(k)
          instring(k) = instring(k+3)
          instring(k+3) = tmp1
          tmp2 = instring(k+1)
          instring(k+1) = instring(k+2)
          instring(k+2) = tmp2
 240    continue
 250  continue

 300  return
c
      end

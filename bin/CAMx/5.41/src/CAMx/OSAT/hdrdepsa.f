c*** HDRDEPSA
c
      subroutine hdrdepsa(iounit,fname,nspcs,idate,btim,jdate,etim)
      use filunit
      use grid
      use camxcom
      use tracer
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c   Description:
c     This routine writes the header information to the output tracer
c     deposition files.
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c    Argument description:
c      iounit  I  unit number of file to write
c      fname   C  name of file being written
c      nspcs   I  number of species to write to file
c      idate   I   beginning date of simulation (YYJJJ)
c      btim    R   beginning time of simulation
c      jdate   I   ending date of simulation (YYJJJ)
c      etim    R   ending time of simulation
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c      1/20/99   Grid cell size on file should be meters for all
c                cartesian projections (UTM, LCP, PSP)
c      10/24/01  Removed BSWAP and converted integer strings to character*4
c      11/06/01  Input dates are now Julian
c      8/25/06   Surface output files now all UAM format, one file per grid
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
      integer      iounit(*)
      character*80 fname(*)
      integer      nspcs
      integer      idate
      real         btim
      integer      jdate
      real         etim
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      character*10 avgstring
      character*4  ifile(10), note(60), ispec(10,MXTRSP*2)
      character*1  dstring, wstring, underscore
      integer      i, j, nseg, ndate, ndlast, izero, ione, nspcout
      real         ttime, ttlast, zero, factr
      real         xorgf, yorgf, dxf, dyf
c
c-----------------------------------------------------------------------
c    Data statements:
c-----------------------------------------------------------------------
c
      data zero /0.0/
      data izero /0/
      data ione  /1/
      data dstring /'D'/
      data wstring /'W'/
      data underscore /'_'/
      data avgstring /'AVERAGE   '/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
      ndate = idate
      ndlast = jdate
      ttime  = AINT(ANINT(btim)/100.) + amod(ANINT(btim),100.)/60.
      ttlast = AINT(ANINT(etim)/100.) + amod(ANINT(etim),100.)/60.
c
c   --- set scaling factor for coordinates ---
c
      if( .NOT.llatlon ) then
         factr = 1000.0
      else
         factr = 1.0
      endif
c
c   ---- put species names into integer array ----
c 
      nspcout = 0
      do j=1,nspcs
        if( loutsa(j) ) then
          nspcout = nspcout + 1
          do i=1,9
             if( ptname(j)(i:i) .NE. ' ' ) then
               read(ptname(j)(i:i),'(A1)') ispec(i,nspcout)
             else
               read(underscore,'(A1)') ispec(i,nspcout)
             endif
          enddo
          read(dstring,'(A1)') ispec(i,nspcout)
        endif
      enddo
      do j=1,nspcs
        if( loutsa(j) ) then
          nspcout = nspcout + 1
          do i=1,9
             if( ptname(j)(i:i) .NE. ' ' ) then
               read(ptname(j)(i:i),'(A1)') ispec(i,nspcout)
             else
               read(underscore,'(A1)') ispec(i,nspcout)
             endif
          enddo
          read(wstring,'(A1)') ispec(i,nspcout)
        endif
      enddo
c
c   --- master grid file header ---
c
      read(avgstring,'(10A1)') (ifile(i),i=1,10)
      read(runmsg(1:60),'(60A1)') (note(i),i=1,60)
      nseg = 1
      n = 1
      nz = 1
      write(iounit(1),ERR=7000) ifile,note,nseg,nspcout,ndate,ttime,
     &                                                 ndlast,ttlast
      write(iounit(1),ERR=7000) zero,zero,iuzon,xorg*factr,yorg*factr, 
     &                          delx*factr,dely*factr,ncol(1),nrow(1), 
     &                          nz,izero,izero,zero,zero,zero
      write(iounit(1),ERR=7000) ione,ione,ncol(1),nrow(1)
      write(iounit(1),ERR=7000) ((ispec(i,j),i=1,10),j=1,nspcout)
c
c   --- nested grid headers ---
c
      do n = 2,ngrid
         dxf   = delx/float(meshold(n))
         dyf   = dely/float(meshold(n))
         xorgf = xorg + delx*(inst1(n)-1) - dxf
         yorgf = yorg + dely*(jnst1(n)-1) - dyf
         write(iounit(n),ERR=7000) ifile,note,nseg,nspcout,ndate,ttime,
     &                                                    ndlast,ttlast
         write(iounit(n),ERR=7000) zero,zero,iuzon,xorgf*factr,
     &                             yorgf*factr,dxf*factr,dyf*factr,
     &                             ncol(n),nrow(n),nz,izero,izero,
     &                             zero,zero,zero
         write(iounit(n),ERR=7000) ione,ione,ncol(n),nrow(n)
         write(iounit(n),ERR=7000) ((ispec(i,j),i=1,10),j=1,nspcout)
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
      write(iout,'(//,A)') 'ERROR in HDRWSA:'
      write(iout,9000,ERR=9999)'Writing output tracer ',
     &                  'deposition file: ',fname(n)(:istrln(fname(n)))
      call camxerr()
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format(/,1X,2A,/,A)
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
c

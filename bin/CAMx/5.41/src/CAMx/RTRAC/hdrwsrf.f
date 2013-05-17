c*** HDRWSRF
c
      subroutine hdrwsrf(iounit,fname,nspcs,idate,btim,jdate,etim)
      use filunit
      use grid
      use camxcom
      use tracer
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c   Description:
c     This routine writes the header information to the output RTRAC
c     surface model file.
c     The data is written to the unit number in the argument list.
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
c
c      10/29/09  Original development
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
      character*4  ifile(10), note(60), ispec(10,MXTRSP)
      integer      i, j, nseg, ndate, ndlast, izero, ione
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
      data ifile/'A','V','E','R','A','G','E',' ',' ',' '/
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
      do j=1,nspcs
         ispec(1,j) = 'S'
         read(ptname(j)(1:9),'(9A1)') (ispec(i,j),i=2,10)
      enddo
      do j=1,nspcs
         ispec(1,j+nspcs) = 'V'
         read(ptname(j)(1:9),'(9A1)') (ispec(i,j+nspcs),i=2,10)
      enddo
c
c   --- master grid file header ---
c
      read(runmsg(1:60),'(60A1)') (note(i),i=1,60)
      nseg = 1
      nz = 1
      n = 1
      write(iounit(n),ERR=7000) ifile,note,nseg,2*nspcs,ndate,ttime,
     &                                                ndlast,ttlast
      write(iounit(n),ERR=7000) zero,zero,iuzon,xorg*factr,yorg*factr, 
     &                          delx*factr,dely*factr,ncol(n),nrow(n), 
     &                          nz,izero,izero,zero,zero,zero
      write(iounit(n),ERR=7000) ione,ione,ncol(n),nrow(n)
      write(iounit(n),ERR=7000) ((ispec(i,j),i=1,10),j=1,2*nspcs)
c
c   --- nested grid headers ---
c
      do n = 2,ngrid
         dxf   = delx/float(meshold(n))
         dyf   = dely/float(meshold(n))
         xorgf = xorg + delx*(inst1(n)-1) - dxf
         yorgf = yorg + dely*(jnst1(n)-1) - dyf
         write(iounit(n),ERR=7000) ifile,note,nseg,2*nspcs,ndate,ttime,
     &                                                   ndlast,ttlast
         write(iounit(n),ERR=7000) zero,zero,iuzon,xorgf*factr,
     &                             yorgf*factr,dxf*factr,dyf*factr,
     &                             ncol(n),nrow(n),nz,izero,izero,
     &                             zero,zero,zero
         write(iounit(n),ERR=7000) ione,ione,ncol(n),nrow(n)
         write(iounit(n),ERR=7000) ((ispec(i,j),i=1,10),j=1,2*nspcs)
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
      write(iout,9000,ERR=9999)
     &                    'Writing output RTRAC surface model file: ',
     &                                     fname(n)(:istrln(fname(n)))
      call camxerr()
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format(/,1X,2A)
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
c

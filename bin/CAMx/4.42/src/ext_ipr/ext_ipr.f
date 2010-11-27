      program ext_ipr
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
c   This program reads the binary output file from a CAMx/IPR run and
c   writes the data to an ASCII output file. The header data is
c   written for completeness, but the data records are written
c   in a form to be imported into a spreadsheet.
c   
c-----------------------------------------------------------------------
c  Local parameters:
c-----------------------------------------------------------------------
c
c  IORIPR    I  unit number of the input IPR file
c  IOWASC    I  unit number of the output ASCII file
c  MXPROC    I  maximum number of processes in the file
c  MXSPEC    I  maximum number of species allowed
c  MXFILE    I  maximum number of files to read
c  CONFAC    I  conversion factor from micromoles/m^3 to moles/m^3
c  CDPPB     C  character string for output in ppb units
c
      integer      IORIPR
      integer      IOWASC
      integer      MXPROC
      integer      MXSPEC
      integer      MXFILE
      real*8       CONFAC
      character*3  CDPPB
c
c
      parameter(  IORIPR   = 10 )
      parameter(  IOWASC   = 20 )
      parameter(  MXPROC   = 99 )
      parameter(  MXSPEC   = 100 )
      parameter(  MXFILE   = 99 )
      parameter(  CONFAC   = 1E-6 )
      parameter(  CDPPB = "PPB" )

c
c     IPR_INIT          initial concentration of each IPR interval
c     IPR_CHEM          concentration change from chemistry
c     IPR_AEMIS         concentration change from area emissions
c     IPR_PTEMIS        concentration change from point source emissions
c     IPR_PIGEMIS       concentration change from plume-in-grid (PiG)
c     IPR_WADV          concentration change due to advection at
c                       west boundary of grid cell
c     IPR_EADV          concentration change due to advection at
c                       east boundary of grid cell
c     IPR_SADV          concentration change due to advection at
c                       south boundary of grid cell
c     IPR_NADV          concentration change due to advection at
c                       north boundary of grid cell
c     IPR_BADV          concentration change due to entrainment
c                       from lower grid cell
c     IPR_TADV          concentration change due to entrainment
c                       from upper grid cell
c     IPR_DADV          concentration change due to diffusion of
c                       current grid cell
c     IPR_WDIF          concentration change due to diffusion at
c                       west boundary of grid cell
c     IPR_EDIF          concentration change due to diffusion at
c                       east boundary of grid cell
c     IPR_SDIF          concentration change due to diffusion at
c                       south boundary of grid cell
c     IPR_NDIF          concentration change due to diffusion at
c                       north boundary of grid cell
c     IPR_BDIF          concentration change due to diffusion at
c                       bottom boundary of grid cell
c     IPR_TDIF          concentration change due to diffusion at
c                       top boundary of grid cell
c     IPR_DDEP          concentration change from dry deposition
c     IPR_WDEP          concentration change from wet deposition
c     IPR_IAERO         concentration change from inorganic aerosol chemistry
c     IPR_OAERO         concentration change from organic aerosol chemistry
c     IPR_AQCHEM        concentration change from aqueous chemistry
c     IPR_FINAL         final concentration of each IPR interval
c     IPR_CONV          umole/m3 <=> ppm conversion
c     IPR_VOL           average volume of the cell
c
      integer      IPR_INIT
      integer      IPR_CHEM
      integer      IPR_AEMIS
      integer      IPR_PTEMIS
      integer      IPR_PIGEMIS
      integer      IPR_WADV
      integer      IPR_EADV
      integer      IPR_SADV
      integer      IPR_NADV
      integer      IPR_BADV
      integer      IPR_TADV
      integer      IPR_DADV
      integer      IPR_WDIF
      integer      IPR_EDIF
      integer      IPR_SDIF
      integer      IPR_NDIF
      integer      IPR_BDIF
      integer      IPR_TDIF
      integer      IPR_DDEP
      integer      IPR_WDEP
      integer      IPR_IAERO
      integer      IPR_OAERO
      integer      IPR_AQCHEM
      integer      IPR_FINAL
      integer      IPR_CONV
      integer      IPR_VOL

      parameter    ( IPR_INIT      =    1 )
      parameter    ( IPR_CHEM      =    2 )
      parameter    ( IPR_AEMIS     =    3 )
      parameter    ( IPR_PTEMIS    =    4 )
      parameter    ( IPR_PIGEMIS   =    5 )
      parameter    ( IPR_WADV      =    6 )
      parameter    ( IPR_EADV      =    7 )
      parameter    ( IPR_SADV      =    8 )
      parameter    ( IPR_NADV      =    9 )
      parameter    ( IPR_BADV      =   10 )
      parameter    ( IPR_TADV      =   11 )
      parameter    ( IPR_DADV      =   12 )
      parameter    ( IPR_WDIF      =   13 )
      parameter    ( IPR_EDIF      =   14 )
      parameter    ( IPR_SDIF      =   15 )
      parameter    ( IPR_NDIF      =   16 )
      parameter    ( IPR_BDIF      =   17 )
      parameter    ( IPR_TDIF      =   18 )
      parameter    ( IPR_DDEP      =   19 )
      parameter    ( IPR_WDEP      =   20 )
      parameter    ( IPR_IAERO     =   21 )
      parameter    ( IPR_OAERO     =   22 )
      parameter    ( IPR_AQCHEM    =   23 )
      parameter    ( IPR_FINAL     =   24 )
      parameter    ( IPR_CONV      =   25 )
      parameter    ( IPR_VOL       =   26 )
c
c -----------------------------------------------------------------------
c    Local variables:
c -----------------------------------------------------------------------
c
      character*240    fname, infile(MXFILE)
      character*80     runmsg, string
      character*25     prcnam(MXPROC)
      character*20     keywrd
      character*10     spname, species(MXSPEC)
      character*3      ounits
      character*1      comma
      integer       irec, ibgdat,iendat,ngrid,nspec,ncol,nrow
      integer       niprdom, iprgrd, i_sw, i_ne, j_sw, j_ne
      integer       b_lay, t_lay, niprprc, iprdom, iprnst
      integer       iprx, ipry, iprz, idomext, idatcnt, idxspc
      integer       jrec, nfiles, ifile, idtlst
      integer       iclbeg, iclend, jclbeg, jclend, kclbeg, kclend
      integer       i, j
      real          begtim, endtim
      real*4        orgx, orgy, xsize, ysize
      real*4        cipr(MXPROC), ciprtot(MXPROC,MXSPEC)
      real*4        abs_error, rel_error, timlst
      logical       lexist, lppb
c
c-----------------------------------------------------------------------
c    Data statements:
c-----------------------------------------------------------------------
c
      data comma /','/
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c ---  get the filenames from the user and open the files      ---
c
      nfiles = 0
111   continue
      write(*,'(1X,A,$)') 'Enter the name of the input file: '
      write(*,'(10X,A)') '/END/ means finished with input files.'
      read(*,'(20X,A)') fname
      keywrd = fname(1:20)
      call jstlft( keywrd)
      call toupper( keywrd)
      if( keywrd(1:5) .NE. '/END/' ) then
         call jstlft( fname )
         inquire(file=fname,exist=lexist)
         if(.NOT. lexist ) goto 7000
         nfiles = nfiles + 1
         if( nfiles .GT. MXFILE ) goto 7010
         infile(nfiles) = fname
         write(*,'(A)') fname
         goto 111
      endif
c
      write(*,'(1X,A,$)') 'Enter the name of the output file: '
      read(*,'(20X,A)') fname
      call jstlft( fname )
      write(*,'(A)') fname
      if( fname .EQ. '' ) then
        write(*,'(1X,A)') 'Output file name appears to have zero length'
        goto 7002
      endif
      open(unit=IOWASC,file=fname,
     &           status='UNKNOWN',ERR=7002)
c
      write(*,'(1X,A)') 'Output units, choose moles or ppb: '
      read(*,'(20X,A)') ounits
      call jstlft( ounits)
      call toupper( ounits )
      if( ounits .EQ. CDPPB ) then
         lppb = .TRUE.
         write(*,'(10X,A)') 'Output units will be PPB.'
      else
         lppb = .FALSE.
         write(*,'(10X,A)') 'Output units will be moles.'
      endif
c
      write(*,'(1X,A,$)') 'Enter the sub-domain to extract: '
      read(*,'(20X,A)') string
      write(*,'(A)') string
      read(string,*) idomext
c
      write(*,'(1X,A,$)') 'Enter the X-range (beginning and ending):'
      read(*,'(20X,A)') string
      write(*,'(A)') string
      read(string,*) iclbeg, iclend

      write(*,'(1X,A,$)') 'Enter the Y-range (beginning and ending):'           
      read(*,'(20X,A)') string
      write(*,'(A)') string
      read(string,*) jclbeg, jclend
c
      write(*,'(1X,A,$)') 'Enter the Z-range (beginning and ending):'
      read(*,'(20X,A)') string
      write(*,'(A)') string
      read(string,*) kclbeg, kclend
c
c --- read and write the header information ---
c
      irec = 0
      idtlst = 0
      timlst = 0.0
      idatcnt = 0
c
c --- the simulation message ---
c
      do ifile = 1,nfiles
         open(unit=IORIPR,file=infile(ifile),form='UNFORMATTED',
     &                                  status='UNKNOWN',ERR=7001)
         irec = 1
         read(IORIPR,ERR=7003) runmsg
         if( ifile .EQ. 1 ) then
           write(*,'(//,1X,3A)')
     &            'CAMX IPR data extraction by EXT_IPR',comma, runmsg
           jrec = 1
           write(IOWASC,'(3A)',ERR=7004)
     &               'CAMX IPR data extraction by EXT_IPR',comma, runmsg
           write(IOWASC,'(a)',err=7004) 
     &               'The header from the first CAMx IPR file follows: '
         endif
c
c    --- the simulation date/time ---
c
         irec = irec + 1
         read(IORIPR,ERR=7003) ibgdat,begtim,iendat,endtim
         if( ifile .EQ. 1 ) then
           jrec = jrec + 1
           write(IOWASC,'(2(I10,A,F10.2,A))',ERR=7004) ibgdat,comma,
     &                                 begtim,comma,iendat,comma,endtim
         endif
c
c    --- the grid information ---
c
         irec = irec + 1
         read(IORIPR,ERR=7003) ngrid
         if( ifile .EQ. 1 ) then
           write(*,'(/,1X,A)') 'CAMx Grid Definition'
           write(*,'(1X,2A,I10)') 'Number of grids: ',comma,ngrid
           write(*,'(1X,11A)') 
     &           'x-origin',comma,'y-origin',comma,'# cols',comma,
     &                     '# rows',comma,'x-width',comma,'y-width'
           jrec = jrec + 1
           write(IOWASC,'(2A,I10)',ERR=7004) 'Number of grids: ',
     &            comma,ngrid
           jrec = jrec + 1
           write(IOWASC,'(11A)',ERR=7004) 'x-origin',comma,
     &            'y-origin',comma,'# cols',comma,'# rows',comma,
     &            'x-width',comma,'y-width'
          endif
          do i=1,ngrid
            irec = irec + 1
            read(IORIPR,ERR=7003) orgx,orgy,ncol,nrow,xsize,ysize
            if( ifile .EQ. 1 ) then
               write(*,'(2(F10.4,A),2(I10,A),2(F10.4,A),I10)')
     &                           orgx,comma,orgy,comma,ncol,comma,
     &                           nrow,comma,xsize,comma,ysize
               jrec = jrec + 1
               write(IOWASC,'(2(F10.4,A),2(I10,A),2(F10.4,A))',ERR=7004)
     &                           orgx,comma,orgy,comma,ncol,comma,
     &                           nrow,comma,xsize,comma,ysize
            endif
          enddo
c
c    --- the model species in the file ---
c
         irec = irec + 1
         read(IORIPR,ERR=7003) nspec
         if( nspec .GT. MXSPEC) goto 7009
c
         if( ifile .EQ. 1 ) then
             jrec = jrec + 1
             write(IOWASC,'(2A,I10)',ERR=7004) 'Number of species: ',
     &                                                 comma,nspec
         endif
         do i=1,nspec
           irec = irec + 1
           read(IORIPR,ERR=7003) spname
           if( ifile .EQ. 1) then
             jrec = jrec + 1
             write(IOWASC,'(A)',ERR=7004) spname
             species(i) = spname
           endif
         enddo
c
c  --- the sub-domain definitions ----
c
         irec = irec + 1
         read(IORIPR,ERR=7003) niprdom
c
c  --- check to make sure the extraction sub-domain is in the file ---
c
         if( idomext .LE. 0 .OR. idomext .GT. niprdom ) goto 7008
         if( ifile .EQ. 1 ) then
            write(*,'(/,1X,A)') 'Process Analysis Sub-Domains'
            write(*,'(1X,2A,I10)') 'Number of sub-domains: ',
     &                                                comma,niprdom
            write(*,'(1X,13A)') 'grid #',comma,'I-begin',
     &                comma,'I-end',comma,'J-begin',comma,'J-end',comma,
     &                                   'Layer-begin',comma,'Layer-end'
            jrec = jrec + 1
            write(IOWASC,'(2A,I10)',ERR=7004) 'Number of sub-domains: ',
     &                                                comma,niprdom
            jrec = jrec + 1
            write(IOWASC,'(13A)',ERR=7004) 'grid #',comma,'I-begin',
     &                comma,'I-end',comma,'J-begin',comma,'J-end',comma,
     &                                   'Layer-begin',comma,'Layer-end'
         endif
         do i=1,niprdom
            irec = irec + 1
            read(IORIPR,ERR=7003) iprgrd, i_sw, i_ne, j_sw, j_ne,
     &                                                 b_lay, t_lay
            if( ifile .EQ. 1 ) then
              write(*,'(7(1X,I10,A))') iprgrd, comma, i_sw,
     &                      comma, i_ne, comma, j_sw, comma, j_ne,
     &                                        comma, b_lay, comma, t_lay
              jrec = jrec + 1
              write(IOWASC,'(7(I10,A))',ERR=7004) iprgrd, comma, i_sw,
     &                      comma, i_ne, comma, j_sw, comma, j_ne,
     &                                        comma, b_lay, comma, t_lay
              if( i .EQ. idomext) then
		iprgrdext = iprgrd
c 
c  --- reset the extraction grid range so it makes sense
c      with this  sub-domain ---
c
                if(  iclbeg  .LT.  i_sw  )  iclbeg  =  i_sw
                if(  iclend  .GT.  i_ne  )  iclend  =  i_ne
                if(  jclbeg  .LT.  j_sw  )  jclbeg  =  j_sw
                if(  jclend  .GT.  j_ne  )  jclend  =  j_ne
                if(  kclbeg  .LT.  b_lay )  kclbeg  =  b_lay
                if(  kclend  .GT.  t_lay )  kclend  =  t_lay
c 
c  --- check for invalid domain specifications ---
c
                if( iclbeg .GT. i_ne .OR. jclbeg .GT. j_ne
     &              .OR. kclbeg .GT. t_lay .OR. iclend .LT. i_sw .OR.
     &               jclend .LT. j_sw .OR. kclend .LT. b_lay ) goto 7011
              endif
           endif
         enddo
c
c  --- report the region being extracted
c
         if( ifile .EQ. 1 ) then
            write(*,'(//,1X,A)') 'The extraction region is:'
            write(*,'(1X,A,i4)') 'Sub-domain number', idomext
            write(*,'(1X,A,i4,A,i4)') 
     &                    'X-cell range',iclbeg,' to ',iclend
            write(*,'(1X,A,i4,A,i4)') 
     &                    'Y-cell range',jclbeg,' to ',jclend
            write(*,'(1X,A,i4,A,i4,//)')
     &                    'Z-cell range',kclbeg,' to ',kclend
         endif
c
c  --- number of processes ---
c
         irec = irec + 1
         read(IORIPR,ERR=7003) niprprc
         if( niprprc .GT. MXPROC) goto 7005
         if( ifile .EQ. 1 ) then
            jrec = jrec + 1
            write(IOWASC,'(2A,I10)',ERR=7004) 'Number of processes: ',
     &                                                comma,niprprc
         endif
         do i=1,niprprc
           irec = irec + 1
           read(IORIPR,ERR=7003) prcnam(i)
           if( ifile .EQ. 1 ) then
             jrec = jrec + 1
             write(IOWASC,'(A)',ERR=7004) prcnam(i)
           endif
         enddo
c
c  --- List subdomain extracted
c
         if( ifile .EQ. 1 ) then
           write(IOWASC,'(a)',err=7004) 
     &                 'End of the CAMx IPR file header.'
           write(IOWASC,'(a)',err=7004) 
     &        'EXT_IPR extracted data for the following cells:  '
           write(IOWASC,'(13A)',ERR=7004) 'grid #',comma,'I-begin',
     &                comma,'I-end',comma,'J-begin',comma,'J-end',comma,
     &                                   'Layer-begin',comma,'Layer-end'
           write(IOWASC,'(7(I10,A))',ERR=7004) iprgrdext, comma, iclbeg,
     &                comma, iclend, comma, jclbeg, comma, jclend,
     &                                   comma, kclbeg, comma, kclend
           write(IOWASC,'(a)') 'The extracted data follow: '
         endif
c        
c  --- change the name of the cell volume --
c
         if( ifile .EQ. 1 ) then
           jrec = jrec + 1
           prcnam(IPR_VOL) = 'Total Volume'
           write(IOWASC,'(100A)',ERR=7004) 'Date',comma,'Time',comma,
     &              'Species',(comma,prcnam(i),i=1,niprprc),
     &                comma,'Absolute Error',comma,'Relative Error'
         endif
c
c --- read and write the data records ---
c
 222     continue
         irec = irec + 1
         idatcnt = idatcnt + 1
         read(IORIPR,ERR=7006,END=333) iendat, endtim, spname,
     &          iprdom, iprnst, iprx, ipry, iprz, (cipr(i),i=1,niprprc)
c
c  --- find the species index in the array ---
c
         idxspc  = 0
         do i=1,nspec
            if( spname .EQ. species(i) ) idxspc = i
         enddo
         if( idxspc .LE. 0 ) goto 222
c
c --- if this is a new hour and not the first record in the file,
c     output the totals ---
c
         if( iendat .NE. idtlst .OR. endtim .NE. timlst ) then
            if( idatcnt .GT. 1 ) then
              do j=1,nspec
                 if( ciprtot(IPR_VOL,j) .GT. 0.0 ) then
                     ciprtot(IPR_CONV,j) =
     +                        ciprtot(IPR_CONV,j) / ciprtot(IPR_VOL,j)
                 else
                     ciprtot(IPR_CONV,j) = 1.0
                 endif
                 abs_error = ciprtot(IPR_FINAL,j)
                 abs_error = abs_error - ciprtot(IPR_INIT,j)
                 abs_error = abs_error - ciprtot(IPR_CHEM,j)
                 abs_error = abs_error - ciprtot(IPR_AEMIS,j)
                 abs_error = abs_error - ciprtot(IPR_PTEMIS,j)
                 abs_error = abs_error - ciprtot(IPR_PIGEMIS,j)
                 abs_error = abs_error - ciprtot(IPR_WADV,j)
                 abs_error = abs_error - ciprtot(IPR_EADV,j)
                 abs_error = abs_error - ciprtot(IPR_SADV,j)
                 abs_error = abs_error - ciprtot(IPR_NADV,j)
                 abs_error = abs_error - ciprtot(IPR_BADV,j)
                 abs_error = abs_error - ciprtot(IPR_TADV,j)
                 abs_error = abs_error - ciprtot(IPR_DADV,j)
                 abs_error = abs_error - ciprtot(IPR_WDIF,j)
                 abs_error = abs_error - ciprtot(IPR_EDIF,j)
                 abs_error = abs_error - ciprtot(IPR_SDIF,j)
                 abs_error = abs_error - ciprtot(IPR_NDIF,j)
                 abs_error = abs_error - ciprtot(IPR_BDIF,j)
                 abs_error = abs_error - ciprtot(IPR_TDIF,j)
                 abs_error = abs_error - ciprtot(IPR_DDEP,j)
                 abs_error = abs_error - ciprtot(IPR_WDEP,j)
                 abs_error = abs_error - ciprtot(IPR_IAERO,j)
                 abs_error = abs_error - ciprtot(IPR_OAERO,j)
                 abs_error = abs_error - ciprtot(IPR_AQCHEM,j)
                 denom=0.0
                 do jj = IPR_INIT, IPR_FINAL-1
                   denom = max(abs(ciprtot(jj,j)),denom)
                 enddo
                 rel_error = abs(abs_error / denom)
                 jrec = jrec + 1
c
c --- convert units if output requested in ppb ---
c
                 if (lppb) then
                 if (ciprtot(IPR_CONV,j).lt.0.999) then ! gases: [mole]->[ppb]
                   abs_error = abs_error*1.e9*ciprtot(IPR_CONV,j)
     &                                           / ciprtot(IPR_VOL,j)
                   do i=1,niprprc-2
                      ciprtot(i,j) =
     &                      ciprtot(i,j)*1.e9*ciprtot(IPR_CONV,j)
     &                                           / ciprtot(IPR_VOL,j)
                   enddo
                 else                                   ! aerosols: [g]->[ug/m3]
                   abs_error = abs_error * 1.e6 / ciprtot(IPR_VOL,j)
                   do i=1,niprprc-2
                      ciprtot(i,j) =
     &                      ciprtot(i,j) * 1.e6 / ciprtot(IPR_VOL,j)
                   enddo
                 endif
                 endif
c

                 write(IOWASC,9000,ERR=7007) idtlst, comma, timlst,
     &             comma, species(j),(comma,ciprtot(i,j),i=1,niprprc),
     &                                  comma,abs_error,comma,rel_error
              enddo
           endif
c
C  --- initialize the totals to zero ---
c
           do j=1,nspec
              do i=1,niprprc
                 ciprtot(i,j) = 0.
              enddo
           enddo
c
c  --- write a message that new hour was found ----
c
           write(*,'(1X,A,I6,1X,F5.0)') 'Read: ',iendat, endtim
           idtlst = iendat
           timlst = endtim
        endif
c
c  --- skip if not the extraction domain ----
c
        if(  iprdom .NE. idomext ) goto 222
c
c  ---  skip if this cell is not in the extraction subgrid ----
c
        if( iprx .LT. iclbeg ) goto 222
        if( iprx .GT. iclend ) goto 222
        if( ipry .LT. jclbeg ) goto 222
        if( ipry .GT. jclend ) goto 222
        if( iprz .LT. kclbeg ) goto 222
        if( iprz .GT. kclend ) goto 222
c
c  --- add totals for the initial concentrations ---
c
        ciprtot(IPR_INIT,idxspc) = ciprtot(IPR_INIT,idxspc) +
     &                      cipr(IPR_INIT) * cipr(IPR_VOL) * CONFAC
c
c  --- add totals for the chemistry process ---
c
        ciprtot(IPR_CHEM,idxspc) = ciprtot(IPR_CHEM,idxspc) +
     &                      cipr(IPR_CHEM) * cipr(IPR_VOL) * CONFAC
c
c  --- add totals for the surface source emissions ----
c

          ciprtot(IPR_AEMIS,idxspc) = ciprtot(IPR_AEMIS,idxspc) +
     &                        cipr(IPR_AEMIS) * cipr(IPR_VOL) * CONFAC
c
c  --- add totals for the point source emissions ----
c
          ciprtot(IPR_PTEMIS,idxspc) = ciprtot(IPR_PTEMIS,idxspc) +
     &                        cipr(IPR_PTEMIS) * cipr(IPR_VOL) *  CONFAC
c
c  --- add totals for the PiGged emissions ---
c
          ciprtot(IPR_PIGEMIS,idxspc) = ciprtot(IPR_PIGEMIS,idxspc) +
     &                        cipr(IPR_PIGEMIS) * cipr(IPR_VOL) * CONFAC
c
c  --- add totals for the dry deposition ----
c
          ciprtot(IPR_DDEP,idxspc) = ciprtot(IPR_DDEP,idxspc) +
     &                        cipr(IPR_DDEP) * cipr(IPR_VOL) * CONFAC
c
c  --- add totals for the wet deposition ----
c
          ciprtot(IPR_WDEP,idxspc) = ciprtot(IPR_WDEP,idxspc) +
     &                        cipr(IPR_WDEP) * cipr(IPR_VOL) * CONFAC
c
c  --- add totals for the inorganic aerosol chemistry ----
c
          ciprtot(IPR_IAERO,idxspc) = ciprtot(IPR_IAERO,idxspc) +
     &                        cipr(IPR_IAERO) * cipr(IPR_VOL) * CONFAC
c
c  --- add totals for the organic aerosol chemistry ----
c
          ciprtot(IPR_OAERO,idxspc) = ciprtot(IPR_OAERO,idxspc) +
     &                        cipr(IPR_OAERO) * cipr(IPR_VOL) * CONFAC
c
c  --- add totals for the aqueous chemistry ----
c
          ciprtot(IPR_AQCHEM,idxspc) = ciprtot(IPR_AQCHEM,idxspc) +
     &                        cipr(IPR_AQCHEM) * cipr(IPR_VOL) * CONFAC
c
c  --- add totals for the average conversion factor ----
c
          ciprtot(IPR_CONV,idxspc) = ciprtot(IPR_CONV,idxspc) +
     &                        cipr(IPR_CONV) * cipr(IPR_VOL)
c
c  --- add totals for the final concentrations ---
c
          ciprtot(IPR_FINAL,idxspc) = ciprtot(IPR_FINAL,idxspc) +
     &                        cipr(IPR_FINAL) * cipr(IPR_VOL) * CONFAC
c
c  --- add totals for the volume ---
c
          ciprtot(IPR_VOL,idxspc) = ciprtot(IPR_VOL,idxspc) +
     &                                                   cipr(IPR_VOL)
c   
c  --- add totals for the advection fluxes and diffusion ---
c
          if( iprx .EQ. iclbeg ) then
            ciprtot(IPR_WADV,idxspc) = ciprtot(IPR_WADV,idxspc) +
     &                         cipr(IPR_WADV) * cipr(IPR_VOL) * CONFAC
            ciprtot(IPR_WDIF,idxspc) = ciprtot(IPR_WDIF,idxspc) +
     &                         cipr(IPR_WDIF) * cipr(IPR_VOL) * CONFAC
          endif

          if( iprx .EQ. iclend ) then
            ciprtot(IPR_EADV,idxspc) = ciprtot(IPR_EADV,idxspc) +
     &                         cipr(IPR_EADV) * cipr(IPR_VOL) * CONFAC
            ciprtot(IPR_EDIF,idxspc) = ciprtot(IPR_EDIF,idxspc) +
     &                         cipr(IPR_EDIF) * cipr(IPR_VOL) * CONFAC
          endif
c
          if( ipry .EQ. jclbeg ) then
            ciprtot(IPR_SADV,idxspc) = ciprtot(IPR_SADV,idxspc) +
     &                         cipr(IPR_SADV) * cipr(IPR_VOL) * CONFAC
            ciprtot(IPR_SDIF,idxspc) = ciprtot(IPR_SDIF,idxspc) +
     &                         cipr(IPR_SDIF) * cipr(IPR_VOL) * CONFAC
          endif
c
          if( ipry .EQ. jclend ) then
            ciprtot(IPR_NADV,idxspc) = ciprtot(IPR_NADV,idxspc) +
     &                         cipr(IPR_NADV) * cipr(IPR_VOL) * CONFAC
            ciprtot(IPR_NDIF,idxspc) = ciprtot(IPR_NDIF,idxspc) +
     &                         cipr(IPR_NDIF) * cipr(IPR_VOL) * CONFAC
          endif
c
          if( iprz .EQ. kclbeg ) then
            ciprtot(IPR_BADV,idxspc) = ciprtot(IPR_BADV,idxspc) +
     &                         cipr(IPR_BADV) * cipr(IPR_VOL) * CONFAC
            ciprtot(IPR_BDIF,idxspc) = ciprtot(IPR_BDIF,idxspc) +
     &                         cipr(IPR_BDIF) * cipr(IPR_VOL) * CONFAC
          endif
c
          if( iprz .EQ. kclend ) then
            ciprtot(IPR_TADV,idxspc) = ciprtot(IPR_TADV,idxspc) +
     &                         cipr(IPR_TADV) * cipr(IPR_VOL) * CONFAC
            ciprtot(IPR_TDIF,idxspc) = ciprtot(IPR_TDIF,idxspc) +
     &                         cipr(IPR_TDIF) * cipr(IPR_VOL) * CONFAC
          endif
c
          goto 222
c
c   --- entire file read, get the next file ---
c
333       continue
          close(IORIPR)
       enddo
c
c  --- write out totals for last hour ---
c
      do j=1,nspec
          if( ciprtot(IPR_VOL,j) .GT. 0.0 ) then
            ciprtot(IPR_CONV,J) = ciprtot(IPR_CONV,J) / 
     &                                            ciprtot(IPR_VOL,j)
          else
            ciprtot(IPR_CONV,J) = 1.0
          endif
          abs_error  =  ciprtot(IPR_FINAL,j)
          abs_error  =  abs_error   - ciprtot(IPR_INIT,j)
          abs_error  =  abs_error    -ciprtot(IPR_CHEM,j)
          abs_error  =  abs_error    -ciprtot(IPR_AEMIS,j)
          abs_error  =  abs_error    -ciprtot(IPR_PTEMIS,j)
          abs_error  =  abs_error    -ciprtot(IPR_PIGEMIS,j)
          abs_error  =  abs_error    -ciprtot(IPR_WADV,j)
          abs_error  =  abs_error    -ciprtot(IPR_EADV,j)
          abs_error  =  abs_error    -ciprtot(IPR_SADV,j)
          abs_error  =  abs_error    -ciprtot(IPR_NADV,j)
          abs_error  =  abs_error    -ciprtot(IPR_BADV,j)
          abs_error  =  abs_error    -ciprtot(IPR_TADV,j)
          abs_error  =  abs_error    -ciprtot(IPR_DADV,j)
          abs_error  =  abs_error    -ciprtot(IPR_WDIF,j)
          abs_error  =  abs_error    -ciprtot(IPR_EDIF,j)
          abs_error  =  abs_error    -ciprtot(IPR_SDIF,j)
          abs_error  =  abs_error    -ciprtot(IPR_NDIF,j)
          abs_error  =  abs_error    -ciprtot(IPR_BDIF,j)
          abs_error  =  abs_error    -ciprtot(IPR_TDIF,j)
          abs_error  =  abs_error    -ciprtot(IPR_DDEP,j) 
          abs_error  =  abs_error    -ciprtot(IPR_WDEP,j)
          abs_error  =  abs_error    -ciprtot(IPR_IAERO,j)
          abs_error =   abs_error    -ciprtot(IPR_OAERO,j)
          abs_error =   abs_error    -ciprtot(IPR_AQCHEM,j)
          denom=0.0
          do jj = IPR_INIT, IPR_FINAL-1
            denom = max(abs(ciprtot(jj,j)),denom)
          enddo
          rel_error = abs(abs_error / denom)
          jrec = jrec + 1
c
c  --- convert units if output requested in ppb ---
c
          if (lppb) then
          if (ciprtot(IPR_CONV,j).lt.0.999) then ! gases: [mole]->[ppb]
            abs_error = abs_error*1.e9*ciprtot(IPR_CONV,j)
     &                          / ciprtot(IPR_VOL,j)
            do i=1,niprprc-2
              ciprtot(i,j) = ciprtot(i,j)*1.e9*ciprtot(IPR_CONV,j)
     &                          / ciprtot(IPR_VOL,j)
            enddo
          else                                   ! aerosols: [g]->[ug/m3]
            abs_error = abs_error * 1.e6 / ciprtot(IPR_VOL,j)
            do i=1,niprprc-2
              ciprtot(i,j) = ciprtot(i,j) * 1.e6 / ciprtot(IPR_VOL,j)
            enddo
          endif
          endif
c
          write(IOWASC,9000,ERR=7007) idtlst, comma, timlst,
     &             comma, species(j),(comma,ciprtot(i,j),i=1,niprprc),
     &                               comma,abs_error, comma, rel_error
      enddo
      close(IORIPR)
      close(IOWASC)
      write(*,*) ' Successful completion.'
      goto 9999
c
c-----------------------------------------------------------------------
c  Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(*,'(1X,2A)') 'ERROR: Input file does not exist: ',fname
      goto 9999
c    
 7001 continue
      write(*,'(1X,2A)') 'ERROR: Cannot open input file: ',fname
      goto 9999
c
 7002 continue
      write(*,'(1X,2A)') 'ERROR: Cannot open output file: ',fname
      goto 9999
c
 7003 continue
      write(*,'(1X,2A,I5)') 'ERROR: Reading header of IPR file ',
     &                                            'at record: ',irec
      goto 9999
c
 7004 continue
      write(*,'(1X,2A,I5)') 'ERROR: Writing header of ASCII file.'
      goto 9999
c
 7005 continue
      write(*,'(1X,A,/,10X,A)')
     &                'ERROR: Number of IPR processes exceeds max.',
     &                                   'Increase parameter MXPROC.'
      goto 9999
c
 7006 continue
      write(*,'(1X,2A,I8)') 'ERROR: Reading data record of IPR file ',
     &                                             'at record: ',irec
      goto 9999
c
 7007 continue
      write(*,'(1X,2A,I8)')
     &               'ERROR: Writing data record of ASCII file ',
     &                                             'at record: ',jrec
      goto 9999
c
 7008 continue
      write(*,'(1X,2A,I2)') 'ERROR: Sub-domain requested for ',
     &                      'extraction is not in the file: ',idomext
      goto 9999
c
 7009 continue
      write(*,'(1X,A,/,10X,A)')
     &              'ERROR: Number of species in file exceeds max.',
     &                                  'Increase parameter MXSPEC.'
      goto 9999
c
 7010 continue
      write(*,'(1X,A,/,10X,A)')
     &              'ERROR: Number of input files exceeds max.',
     &                                 'Increase parameter MXFILE.'
      goto 9999
c
 7011 continue

      write(*,'(1X,A)') 'ERROR: Invalid extraction domain specified. '
      write(*,'(10X,A)') 'Please specify a domain within the following.'
      write(*,'(20X,A,2I20)') 'I-cell',i_sw,i_ne
      write(*,'(20X,A,2I20)') 'J-cell',j_sw,j_ne
      write(*,'(20X,A,2I20)') 'K-cell',b_lay,t_lay
      goto 9999
c
c-----------------------------------------------------------------------
c
c  Format statements:
c-----------------------------------------------------------------------
c
 9000 format(I10,A,F10.2,3A,100(E15.8,A))
c
c-----------------------------------------------------------------------
c  Exist point:
c-----------------------------------------------------------------------
c
 9999 continue
      stop
      end

      subroutine jstlft( string)
c
c----- CAMX v2.03 991230
c
c
c-----------------------------------------------------------------------
c
c  Description:
c     Left justifies a string
c   
c  Arguments:
c    Inputs/Outputs: (the string arguments serves as both input and
c                      output)
c       string   C    string to left justify
c
c-----------------------------------------------------------------------
c  LOG:
c-----------------------------------------------------------------------
c
c    08/10/98 -gmw- original development
c
c-----------------------------------------------------------------------
c  Argument declaration:
c-----------------------------------------------------------------------
c
      character*(*) string
c
c-----------------------------------------------------------------------
c  Local variables:
c-----------------------------------------------------------------------
c
      integer ibeg, i
c
c-----------------------------------------------------------------------
c  Entry point:
c-----------------------------------------------------------------------
c
c  ---- it may already be left-justified    ---
c
      if( string(1:1) .NE. ' ') goto 9999
c
c  ---- find the first non-blank character ---
c
      do 10 i=1,LEN( string)
         if( string(i:i) .NE. ' ') then
              ibeg = i
              goto 111
         endif
   10 continue
c
c    --- no non-blanks found, it's a blank string, nothing to do ----
c
      goto 9999
c
c    ---- move the string over, 2 char at a time ---
c
111   continue
      do 20 i=1,LEN( string )-ibeg+1
         string(i:i) = string(i+ibeg-1:i+ibeg-1)
         string(i+ibeg-1:i+ibeg-1) = ' '
20    continue
      goto 9999
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end
           
c**** TOUPPER
c
      subroutine toupper(string)
c
c----- CAMX v2.03 991230
c
C-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c    Converts the string to upper case.
c
c    Copyright 1996, 1997, 1998, 1999 ENVIRON International Corporation
c
c     Argument description:
c        Inputs:
c          string C string to convert
c
C-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c    08/16/98   --gwilson--     Original development
c
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
c   Entry point:
c-----------------------------------------------------------------------
c
      do i=1,LEN( string )
         idx = INDEX( lower, string(i:i))
         if( idx .GT. 0 ) string(i:i) = upper(idx:idx)
      enddo
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
      return
      end


c**** RESMAP
c
      subroutine resmap()
      use filunit
      use grid
      use tracer
c
c----CAMx v5.10 090918
c
c-----------------------------------------------------------------------
c    Description:
c-----------------------------------------------------------------------
c
c   This routine reads the mapping file and sets the source region
c   for each grid cell.  The mapping file is an ASCII grid which
c   contains the source area number for each grid. 
c
c     Copyright 1996 - 2009
c     ENVIRON International Corporation
c
c     Argument description:
c      Inputs:
c
c-----------------------------------------------------------------------
c    LOG:
c-----------------------------------------------------------------------
c
c     05/20/96   --gwilson--    Original development
c     07/19/02   --gwilson--    Added source area map for nests
c     07/16/07   --bkoo--       Added check for HDDM
c
c-----------------------------------------------------------------------
c    Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Arguement declarations:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer   irec, irow, ierr, igrd, ncola, i
c
      integer irowvl(MXCELLS)
c
c-----------------------------------------------------------------------
c    Enxternal functions:
c-----------------------------------------------------------------------
c
      integer istrln
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c  --- loop over grids ---
c
      do 10 igrd=1,ngrid
c
c  ---- initialize the array to zero ---
c
         do i=1,ncol(igrd)
            do j=1,nrow(igrd)
               igrmap(igrd,i,j) = 0
            enddo
         enddo
c
c   --- if mapping file is not supplied, go to next grid ---
c
         if( .NOT. lmapfl(igrd) ) goto 10
c
c  ---- open the file ---
c
         open(unit=iormap(igrd),file=mapfil(igrd),status='UNKNOWN')
c
c  ---- loop over rows (Y direction), go backwards since the ASCII map
c       is a grid image ---
c
         irec = 0
         do 20 irow=nycell(igrd),1,-1
           irec = irec + 1
           read(iormap(igrd),'(500(:,I3))',IOSTAT=ierr) 
     &                                (irowvl(i),i=1,nxcell(igrd))
           if( ierr .NE. 0 ) then
               write(iout,'(//,a)') 'ERROR in RESMAP:'
               write(iout,'(/,1X,2A,I4)') 'Reading the source ',
     &                                     'map file at line: ',irec
               write(iout,'(10X,A,/,A)') 'Source map filename: ',
     &                              mapfil(igrd)(:istrln(mapfil(igrd)))
              call camxerr()
           endif
c
c  --- check each mapping value for validity and load data 
c      into the global array ---
c
           do 30 i=1,nxcell(igrd)
              if( (irowvl(i) .LE. 0 .AND. .NOT. (lddm.OR.lhddm)) .OR.
     &                                     irowvl(i) .GT. nregin ) then
                  write(iout,'(//,a)') 'ERROR in RESMAP:'
                  write(iout,'(/,1X,A,A,I4)') 'Mapping value read',
     &                      ' from map file is out of range: ',irowvl(i)
                  write(iout,'(10X,A,/,A)') 'Source map filename: ',
     &                               mapfil(igrd)(:istrln(mapfil(igrd)))
                  call camxerr()
              endif
              igrmap(igrd,i,irow) = irowvl(i)
   30      continue
c
c  --- read the next row ---
c
   20   continue
c
c  --- close file and return to calling routine ---
c
        close(iormap(igrd))
c
c   --- next nest ---
c
   10 continue
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
c
      return
      end

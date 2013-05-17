c*** WFCONSA
c
      subroutine wfconsa(iendat,endtim)
      use camxcom
      use grid
      use tracer
      use filunit
c
c----CAMx v5.41 121109
c
c-----------------------------------------------------------------------
c   Description:
c     This routine writes the instantaneous file for the tracer
c     species for the fine grids.  
c
c     Copyright 1996 - 2012
c     ENVIRON International Corporation
c
c    Argument description:
c     Outputs:
c     Inputs:
c        iendat  I   current date
c        endtim  R   current time
c
c-----------------------------------------------------------------------
c   LOG:
c-----------------------------------------------------------------------
c
c      08/23/06  Revised to just write instant fields at end of simulation
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camx.prm'
c
c-----------------------------------------------------------------------
c    Argument declarations:
c-----------------------------------------------------------------------
c
      integer   iendat
      real      endtim
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer       ifgptr(MXGRID), ifglvl(MXGRID)
      integer       igrd, idx, i, j, k, l
      real          cnctmp(MXCELLS,MXCELLS)
c
c-----------------------------------------------------------------------
c    Entry point:
c-----------------------------------------------------------------------
c
c   --- set the fine grid pointers ---
c
      do i=1,ngrid
         ifglvl(i) = 0
      enddo
      do i=1,ngrid
         do j=1,nchdrn(i)
            idch = idchdrn(j,i)
            ifgptr(idch) = i - 1 
            ifglvl(idch) = ifglvl(idch) + 1
            do k=1,nchdrn(idch)
               ifglvl(idchdrn(k,idch)) = ifglvl(idchdrn(k,idch)) + 1
            enddo
         enddo
      enddo
c
c   --- write the file description header ---
c
      write(iowcon(IDXFIN),ERR=7000) runmsg
      write(iowcon(IDXFIN),ERR=7000) ngrid-1, ntotsp
c
c  ---- species list ---
c
      write(iowcon(IDXFIN),ERR=7000) (ptname(i),i=1,ntotsp)
c
c  --- write the fine grid descriptions 
c
      do 20 igrd=2,ngrid
          write(iowcon(IDXFIN),ERR=7000) inst1(igrd), jnst1(igrd), 
     &                                   inst2(igrd), jnst2(igrd), 
     &             meshold(igrd), meshold(igrd), ncol(igrd), nrow(igrd), 
     &                            nlay(igrd), ifgptr(igrd), ifglvl(igrd)
   20 continue
c
c  --- write the time span ---
c
      write(iowcon(IDXFIN),ERR=7000) endtim, iendat
c
c   --- write the data for this hour ----
c
      do 50 igrd=2,ngrid
         do 60 l=1,ntotsp
            do 70 k=1,nlay(igrd)
               do 80 j=1,nrow(igrd)
                  do 90 i=1,ncol(igrd)
                     idx =  i + ncol(igrd)*(j-1) + 
     &                      ncol(igrd)*nrow(igrd)*(k-1) +
     &                           ncol(igrd)*nrow(igrd)*nlay(igrd)*(l-1)
                     cnctmp(i,j) = ptconc(ipsa3d(igrd)-1+idx)
   90             continue
   80          continue
               write(iowcon(IDXFIN)) ((cnctmp(i,j),i=1,ncol(igrd)),
     &                                               j=1,nrow(igrd))
   70       continue
   60    continue
   50 continue
c
      goto 9999
c
c-----------------------------------------------------------------------
c    Error messages:
c-----------------------------------------------------------------------
c
 7000 continue
      write(iout,'(//,a)') 'ERROR in WFCONSA:'
      write(iout,9000,ERR=9999)'Writing output tracer file: ',
     &                           confil(IDXFIN)(:istrln(confil(IDXFIN)))
      call camxerr()
c
c-----------------------------------------------------------------------
c    Format statements:
c-----------------------------------------------------------------------
c
 9000 format(/,1X,6A)
c
c-----------------------------------------------------------------------
c    Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

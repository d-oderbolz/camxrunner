c**** RDHRFG.F
c
      subroutine rdhrfg(ierr,cncget,idate,ibeghr,igrid)          
c
c-----------------------------------------------------------------------
c
c   This routine reads one hour of data from the FINE grid average file
c   and outputs the applicable species.  
c     Argument description:
c      Outputs:
c        ierr    I  error code
c        cncget  R  gridded array of requested species concentrations
c        idate   I  current date read
c        ibeghr  I  current hour read
c      Inputs:
c        igrid   I  grid number of current grid
c
c-----------------------------------------------------------------------
c   Include files:
c-----------------------------------------------------------------------
c
      include 'camxtrct.inc'
c
c-----------------------------------------------------------------------
c   Argument declaration:
c-----------------------------------------------------------------------
c
      integer*4 ierr
      integer*4 idate
      integer*4 ibeghr
      real*4    cncget(MXCELL,MXCELL,MXLAYR)
      integer*4 igrid
c
c-----------------------------------------------------------------------
c   External functions:
c-----------------------------------------------------------------------
c
c
c-----------------------------------------------------------------------
c    Local variables:
c-----------------------------------------------------------------------
c
      integer*4 i, j, k, ispct, layct
      real*4    beghr
      real*4    cncden(MXCELL,MXCELL,MXLAYR)
      real*4    cncnum(MXCELL,MXCELL,MXLAYR)
      real*4    cnctmp(MXCELL,MXCELL,MXLAYR)
c
c-----------------------------------------------------------------------
c    Entry point: 
c-----------------------------------------------------------------------
c
c  --- initialize the error code ---
c
      ierr = IFAIL
c
c  --- initialize the arrays to zero ----
c
      call zeros(cncden,MXCELL*MXCELL*MXLAYR)
      call zeros(cncnum,MXCELL*MXCELL*MXLAYR)
      call zeros(cnctmp,MXCELL*MXCELL*MXLAYR)
c
c  --- read the date/time stamp if this is the first grid ----
c
      if( igrid .EQ. 1 ) then
          read(IORFAV,ERR=7000,END=111) beghr, idate
          if( beghr .EQ. 0. ) then
             beghr = 23.0
             idate = idate - 1
             ibeghr = INT( beghr )
          else
             ibeghr = MAX(0,NINT(beghr)/100 - 1)
          endif
      endif
c
c  --- loop over all of the species ---
c      
      do ispct=1,nspec
c
c  --- loop over the layers ---
c
          do layct=1,nlgrid(igrid)
              read(IORFAV, ERR=7000) ((cnctmp(i,j,layct),
     &                         i=1,nxgrid(igrid)),j=1,nygrid(igrid))
          enddo
c
c  --- if species is in the denominator, add to cumulative array ---
c
          if( lnumer(ispct) ) then
            call operate(cncnum,cnctmp,cncnum,"ADD",facnum(ispct))
          endif
c
c  --- if species is in the denominator, add to cumulative array ---
c
          if( ldenom(ispct) ) then
            call operate(cncden,cnctmp,cncden,"ADD",facnum(ispct))
          endif
c
c  --- get the next species ----
c
      enddo
c
c  ---- if doing a ratio of species, call routine to perform
c       the division operation and put result in output array ----
c
      if (lratio) then
        call operate(cncget,cncnum,cncden,"DIV",1.0)
c
c  ---- if not doing a ratio of species, call routine to essentially
c       put the numerator into the output array ----
c
      else
        call operate(cncget,cncnum,cncden,"ADD",1.0)
      endif 
c
c  --- set error code and return ---
c
      ierr = ISUCES
      goto 9999
c
c  --- end of file encountered set error code appropariately ---
c
  111 continue
      ierr = IEOF
      goto 9999
c
c-----------------------------------------------------------------------
c   Error messages:
c-----------------------------------------------------------------------
c
 7000 continue 
      write(IOWSTD,'(2A)',ERR=9999) 'ERROR: Reading fine grid ',
     &                                                 'average file. '
      goto 9999 
c
c-----------------------------------------------------------------------
c   Return point:
c-----------------------------------------------------------------------
c
 9999 continue
      return
      end

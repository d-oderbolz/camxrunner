c**** RDHRCG.F
c
      subroutine rdhrcg(ierr,cncget,idate,ibeghr)          
c
c-----------------------------------------------------------------------
c
c   This routine reads one hour of data from the COARSE grid average 
c   file and outputs the applicable species.  
c     Argument description:
c      Outputs:
c        ierr    I  error code
c        cncget  R  gridded array of requested species concentrations
c        idate   I  current date read
c        ibeghr  I  current hour read
c      Inputs:
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
      integer*4 i, j, k, n, ispct, layct, nseg, jdate, mspec(10)
      real*4    beghr, endhr 
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
c  --- initialize the cumulative arrays to zero ---
c
      do k=1,nlayer
        do j=1,nycell
          do i=1,nxcell
            cncden(i,j,k) = 0
            cncnum(i,j,k) = 0
          enddo 
        enddo 
      enddo  
c
c  --- read the date/time stamp --
c
      read(IORCAV,ERR=7000,END=111) idate, beghr, jdate, endhr
      ibeghr = NINT(beghr)
c
c  --- loop over all of the species ---
c      
      do ispct=1,nspec
c
c  --- loop over the layers ---
c
          do layct=1,nlayer 
              read(IORCAV, ERR=7000) nseg, (mspec(n),n=1,10),
     &                       ((cnctmp(i,j,layct),i=1,nxcell),j=1,nycell)
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
      write(IOWSTD,'(2A)',ERR=9999) 'ERROR: Reading coarse grid ',
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
